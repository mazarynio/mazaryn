-module(ipfs_media).
-author("Zaryn Technologies").
-export([
    upload_media/1, 
    upload_media/2, 
    get_media/1, 
    get_media/2, 
    get_media_binary/1, 
    get_media_binary/2, 
    merge_options/2, 
    ipfs_get_file/2
]).

-define(RPC_API, "http://localhost:5001/api").
-define(DEFAULT_CHUNK_SIZE, 1048576). % 1MB chunks 

-define(DEFAULT_ADD_OPTS, [
    {pin, false},                
    {cid_version, 1},
    {wrap_with_directory, false},
    {chunker, "size-" ++ integer_to_list(?DEFAULT_CHUNK_SIZE)}
]).

-define(DEFAULT_GET_OPTS, [
    {output, undefined},
    {archive, true},             
    {compress, true},           
    {progress, true}
]).

-define(LARGE_FILE_THRESHOLD, 536870912). % 512MB threshold 
-define(MAX_PARALLEL_UPLOADS, 5).    

%%% Enhanced Upload Functions %%%

upload_media(FilePath) ->
    Filename = filename:basename(FilePath),
    upload_media(FilePath, Filename).

upload_media(FilePath, CustomFilename) ->
    case filelib:file_size(FilePath) of
        {error, Reason} -> 
            {error, {file_size_error, Reason}};
        Size when Size > ?LARGE_FILE_THRESHOLD ->
            case parallel_chunked_upload(FilePath, CustomFilename, Size) of
                {ok, CID} -> binary_to_list(CID);
                Error -> Error
            end;
        Size when Size > ?DEFAULT_CHUNK_SIZE ->
            case sequential_chunked_upload(FilePath, CustomFilename, Size) of
                {ok, CID} -> binary_to_list(CID);
                Error -> Error
            end;
        _ ->
            single_upload(FilePath, CustomFilename)
    end.

single_upload(FilePath, CustomFilename) ->
    case file:read_file(FilePath) of
        {ok, MediaData} ->
            case ipfs_add_file(CustomFilename, MediaData, ?DEFAULT_ADD_OPTS) of
                {ok, CID} -> 
                    case ipfs_cluster:pin_to_cluster(CID) of
                        {ok, _} -> binary_to_list(CID);
                        Error -> Error
                    end;
                Error -> Error
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

sequential_chunked_upload(FilePath, CustomFilename, FileSize) ->
    case file:open(FilePath, [read, binary, raw]) of
        {ok, Fd} ->
            try
                upload_chunks(Fd, CustomFilename, ?DEFAULT_CHUNK_SIZE, 0, FileSize, [])
            after
                file:close(Fd)
            end;
        {error, Reason} ->
            {error, {file_open_error, Reason}}
    end.

parallel_chunked_upload(FilePath, CustomFilename, FileSize) ->
    ChunkSize = optimal_chunk_size(FileSize),
    TotalChunks = ceiling(FileSize / ChunkSize),
    MonitorPid = spawn(fun() -> upload_monitor(TotalChunks) end),
    
    case file:open(FilePath, [read, binary, raw]) of
        {ok, Fd} ->
            try
                CoordinatorPid = self(),
                Workers = lists:map(
                    fun(_) -> 
                        spawn_link(fun() -> 
                            upload_worker(Fd, CustomFilename, ChunkSize, TotalChunks, CoordinatorPid) 
                        end)
                    end, lists:seq(1, ?MAX_PARALLEL_UPLOADS)),
                
                collect_results(TotalChunks, [], Workers, MonitorPid)
            after
                file:close(Fd)
            end;
        {error, Reason} ->
            {error, {file_open_error, Reason}}
    end.

%%% GET MEDIA FUNCTIONS (FILE-BASED) %%%

get_media(CID) ->
    get_media(CID, []).

get_media(CID, Options) ->
    NormalizedCID = case is_binary(CID) of
        true -> binary_to_list(CID);
        false -> CID
    end,
    
    OutputPath = determine_output_path(NormalizedCID, Options),
    
    case ipfs_get_file_local(NormalizedCID, OutputPath) of
        {ok, LocalPath, Size} -> 
            {ok, LocalPath, Size};
        {error, _} ->
            case ipfs_cluster:get_pin_status(NormalizedCID) of
                {ok, _} ->
                    case ipfs_get_file_cluster(NormalizedCID, OutputPath) of
                        {ok, ClusterPath, ClusterSize} -> 
                            {ok, ClusterPath, ClusterSize};
                        ClusterError -> 
                            ClusterError
                    end;
                StatusError -> 
                    StatusError
            end
    end.

%% @doc Get media as binary from IPFS using CID with default options
get_media_binary(CID) ->
    get_media_binary(CID, []).

get_media_binary(CID, _Options) ->
    NormalizedCID = case is_binary(CID) of
        true -> binary_to_list(CID);
        false -> CID
    end,
    
    case ipfs_get_binary_local(NormalizedCID) of
        {ok, Binary} -> 
            Binary;
        {error, _} ->
            case ipfs_cluster:get_pin_status(NormalizedCID) of
                {ok, _} ->
                    case ipfs_cluster:recover_pin(NormalizedCID) of
                        {ok, _} ->
                            timer:sleep(1000),  
                            ipfs_get_binary_local(NormalizedCID);
                        Error -> 
                            Error
                    end;
                StatusError -> 
                    StatusError
            end
    end.

ipfs_get_binary_local(CID) ->
    QueryString = build_query_string([
        {arg, CID},
        {archive, false},
        {compress, false}
    ]),
    Url = ?RPC_API ++ "/v0/cat" ++ QueryString,
    
    case httpc:request(post, 
                     {Url, [], "application/json", ""},
                     [{timeout, 30000}],
                     [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        {ok, {{_, Status, _}, _, Body}} ->
            {error, {status_code, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

determine_output_path(CID, Options) ->
    case proplists:get_value(output, Options) of
        undefined ->
            case proplists:get_value(filename, Options) of
                undefined -> 
                    "./" ++ CID;
                Filename -> 
                    "./" ++ Filename
            end;
        Path -> 
            Path
    end.

ipfs_get_file_local(CID, OutputPath) ->
    file:delete(OutputPath),
    
    BaseName = filename:basename(OutputPath),
    QueryString = build_query_string([
        {arg, CID},
        {output, BaseName},
        {archive, false},
        {compress, false}
    ]),
    Url = ?RPC_API ++ "/v0/get" ++ QueryString,
    
    case httpc:request(post, 
                     {Url, [], "application/json", ""},
                     [{timeout, 30000}],
                     [
                         {body_format, binary},
                         {stream, OutputPath}
                     ]) of
        {ok, saved_to_file} ->
            verify_download(OutputPath);
        {ok, {{_, Status, _}, _, Body}} ->
            {error, {status_code, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

ipfs_get_file_cluster(CID, OutputPath) ->
    case ipfs_cluster:recover_pin(CID) of
        {ok, _} ->
            timer:sleep(1000),
            ipfs_get_file_local(CID, OutputPath);
        Error ->
            Error
    end.

verify_download(Path) ->
    case filelib:file_size(Path) of
        0 -> {error, zero_length_file};
        Size -> {ok, Path, Size}
    end.

ipfs_add_file(Filename, Data, Opts) ->
    case ipfs_client_1:add_file(Filename, Data, Opts) of
        {ok, CID} ->
            case ipfs_cluster:pin_to_cluster(CID) of
                {ok, _} -> {ok, CID};
                Error -> Error
            end;
        Error -> Error
    end.

ipfs_get_file(CID, Options) ->
    MergedOpts = merge_options(Options, ?DEFAULT_GET_OPTS),
    QueryString = build_query_string([{arg, CID}|MergedOpts]),
    Url = ?RPC_API ++ "/v0/get" ++ QueryString,
    
    OutputPath = case proplists:get_value(output, Options) of
        undefined -> filename:join(["./", CID]);
        Path -> Path
    end,
    
    case httpc:request(post, 
                     {Url, [], "application/json", ""},
                     [],
                     [
                         {body_format, binary},
                         {stream, OutputPath},
                         {timeout, 3600000} 
                     ]) of
        {ok, saved_to_file} -> verify_download(OutputPath);
        {ok, {{_, Status, _}, _, Body}} -> {error, {status_code, Status, Body}};
        {error, Reason} -> {error, Reason}
    end.

upload_chunks(Fd, Filename, ChunkSize, Offset, TotalSize, Acc) ->
    case file:pread(Fd, Offset, ChunkSize) of
        {ok, Data} ->
            case ipfs_add_file(Filename, Data, ?DEFAULT_ADD_OPTS) of
                {ok, CID} ->
                    upload_chunks(Fd, Filename, ChunkSize, Offset + byte_size(Data), TotalSize, [CID|Acc]);
                Error ->
                    Error
            end;
        eof ->
            create_manifest(Acc, TotalSize);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

upload_worker(Fd, Filename, ChunkSize, TotalChunks, CoordinatorPid) ->
    case get_next_chunk(Fd, ChunkSize, TotalChunks) of
        {ok, Offset, Data} ->
            case ipfs_add_file(Filename, Data, ?DEFAULT_ADD_OPTS) of
                {ok, CID} -> 
                    CoordinatorPid ! {chunk_complete, Offset, CID};
                Error -> 
                    CoordinatorPid ! {chunk_error, Offset, Error}
            end;
        eof ->
            CoordinatorPid ! worker_done;
        Error ->
            CoordinatorPid ! Error
    end,
    upload_worker(Fd, Filename, ChunkSize, TotalChunks, CoordinatorPid).

collect_results(TotalChunks, Acc, Workers, MonitorPid) ->
    case length(Acc) of
        TotalChunks ->
            MonitorPid ! complete,
            create_manifest(Acc, TotalChunks * ?DEFAULT_CHUNK_SIZE);
        _ ->
            receive
                {chunk_complete, _Offset, CID} ->
                    MonitorPid ! {progress, length(Acc) + 1},
                    collect_results(TotalChunks, [CID|Acc], Workers, MonitorPid);
                {chunk_error, _Offset, Error} ->
                    MonitorPid ! cancel,
                    Error;
                worker_done ->
                    case Workers -- [self()] of
                        [] -> 
                            MonitorPid ! complete,
                            create_manifest(Acc, TotalChunks * ?DEFAULT_CHUNK_SIZE);
                        RemainingWorkers ->
                            collect_results(TotalChunks, Acc, RemainingWorkers, MonitorPid)
                    end
            after
                3600000 -> % 1 hour timeout
                    MonitorPid ! cancel,
                    {error, upload_timeout}
            end
    end.

create_manifest(ChunkCIDs, TotalSize) ->
    Manifest = #{
        chunks => lists:reverse(ChunkCIDs),
        total_size => TotalSize,
        timestamp => erlang:system_time(seconds),
        chunk_size => ?DEFAULT_CHUNK_SIZE,
        chunk_count => length(ChunkCIDs)
    },
    ManifestBinary = jsx:encode(Manifest),
    case ipfs_add_file("manifest.json", ManifestBinary, ?DEFAULT_ADD_OPTS) of
        {ok, ManifestCID} -> 
            ipfs_cluster:pin_to_cluster(ManifestCID),
            {ok, ManifestCID};
        Error -> Error
    end.

optimal_chunk_size(FileSize) ->
    MinChunk = ?DEFAULT_CHUNK_SIZE,
    MaxChunk = 8388608, % 8MB
    DesiredChunks = 100,
    ChunkSize = FileSize div DesiredChunks,
    min(max(MinChunk, ChunkSize), MaxChunk).

ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

upload_monitor(TotalChunks) ->
    receive
        {progress, _N} ->
            upload_monitor(TotalChunks);
        complete -> ok;
        cancel -> ok
    after
        3600000 -> ok 
    end.

get_next_chunk(Fd, ChunkSize, TotalChunks) ->
    case get(fd_offset) of
        undefined -> put(fd_offset, 0);
        Offset when Offset >= TotalChunks * ChunkSize -> eof;
        Offset ->
            put(fd_offset, Offset + ChunkSize),
            case file:pread(Fd, Offset, ChunkSize) of
                {ok, Data} -> {ok, Offset, Data};
                eof -> eof;
                Error -> Error
            end
    end.

merge_options(Options, Defaults) ->
    lists:ukeymerge(1, lists:sort(Options), lists:sort(Defaults)).

build_query_string(Params) ->
    "?" ++ string:join(
        lists:map(
            fun({K, V}) -> 
                io_lib:format("~s=~s", [K, uri_encode(V)]) 
            end, 
            Params),
        "&").

uri_encode(Term) when is_binary(Term) ->
    uri_encode(binary_to_list(Term));
uri_encode(Term) when is_list(Term) ->
    uri_string:quote(Term);
uri_encode(Term) when is_integer(Term) ->
    integer_to_list(Term);
uri_encode(Term) when is_atom(Term) ->
    uri_encode(atom_to_list(Term));
uri_encode(Term) ->
    Term.