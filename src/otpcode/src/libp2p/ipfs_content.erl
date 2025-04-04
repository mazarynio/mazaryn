-module(ipfs_content).
-author("Zaryn Technologies").
-export([upload_text/1, upload_text/2, get_text/1, get_text/2]).

-define(RPC_API, "http://localhost:5001/api").
-define(DEFAULT_CHUNK_SIZE, 262144). % 256KB chunks 
-define(LARGE_TEXT_THRESHOLD, 1048576). % 1MB
-define(MAX_PARALLEL_UPLOADS, 5).

upload_text(Text) ->
    upload_text(Text, "text.txt").

upload_text(Text, Filename) when is_binary(Text) ->
    upload_text(binary_to_list(Text), Filename);

upload_text(Text, Filename) ->
    case iolist_size(Text) > ?LARGE_TEXT_THRESHOLD of
        true ->
            chunked_text_upload(Text, Filename);
        false ->
            single_text_upload(Text, Filename)
    end.

single_text_upload(Text, Filename) ->
    case ipfs_add_file(Filename, Text, [
            {pin, false}, 
            {cid_version, 1},
            {chunker, "size-" ++ integer_to_list(?DEFAULT_CHUNK_SIZE)}
        ]) of
        {ok, CID} ->
            cluster_pin(CID);
        Error -> Error
    end.

chunked_text_upload(Text, Filename) ->
    ChunkSize = ?DEFAULT_CHUNK_SIZE,
    TotalSize = iolist_size(Text),
    TotalChunks = ceiling(TotalSize / ChunkSize),
    
    CoordinatorPid = self(),
    MonitorPid = spawn(fun() -> upload_monitor(TotalChunks) end),
    
    Workers = lists:map(
        fun(I) ->
            spawn_link(fun() ->
                Start = I * ChunkSize,
                End = min(Start + ChunkSize, TotalSize),
                Chunk = string:slice(Text, Start, End - Start),
                case ipfs_add_file(Filename, Chunk, []) of
                    {ok, CID} -> CoordinatorPid ! {chunk_complete, I, CID};
                    Error -> CoordinatorPid ! {chunk_error, I, Error}
                end
            end)
        end, lists:seq(0, ?MAX_PARALLEL_UPLOADS - 1)),
    
    case collect_text_chunks(TotalChunks, [], Workers, MonitorPid) of
        {ok, ChunkCIDs} ->
            create_text_manifest(ChunkCIDs, TotalSize, Filename);
        Error -> Error
    end.


get_text(CID) ->
    get_text(CID, []).

get_text(CID, Options) ->
    case ipfs_get_file(CID, Options) of
        {ok, Path, _Size} ->
            case file:read_file(Path) of
                {ok, Content} -> 
                    {ok, Content};
                {error, Reason} -> 
                    {error, {file_read_error, Reason}}
            end;
        Error -> Error
    end.

cluster_pin(CID) ->
    case ipfs_cluster:pin_to_cluster(CID) of
        {ok, _} -> {ok, CID};
        Error -> Error
    end.

ipfs_add_file(Filename, Data, Opts) ->
    QueryString = build_query_string([{arg, Filename}|Opts]),
    Url = ?RPC_API ++ "/v0/add" ++ QueryString,
    
    Boundary = generate_boundary(),
    ContentType = "multipart/form-data; boundary=" ++ Boundary,
    
    FormData = [
        "--", Boundary, "\r\n",
        "Content-Disposition: form-data; name=\"file\"; filename=\"", Filename, "\"\r\n",
        "Content-Type: text/plain\r\n\r\n",
        Data, "\r\n",
        "--", Boundary, "--\r\n"
    ],
    
    case httpc:request(post, 
                     {Url, [], ContentType, iolist_to_binary(FormData)},
                     [{timeout, 30000}],
                     [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            case jsx:decode(Body, [return_maps]) of
                #{<<"Hash">> := CID} -> {ok, CID};
                _ -> {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, Body}} ->
            {error, {status_code, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

ipfs_get_file(CID, Options) ->
    OutputPath = proplists:get_value(output, Options, "./" ++ CID),
    file:delete(OutputPath), 
    
    QueryString = build_query_string([{arg, CID}]),
    Url = ?RPC_API ++ "/v0/cat" ++ QueryString,
    
    case httpc:request(get, 
                     {Url, [], [], []},
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

create_text_manifest(ChunkCIDs, TotalSize, OriginalFilename) ->
    Manifest = #{
        original_name => OriginalFilename,
        chunks => ChunkCIDs,
        total_size => TotalSize,
        chunk_size => ?DEFAULT_CHUNK_SIZE,
        timestamp => erlang:system_time(seconds)
    },
    case ipfs_add_file("manifest.json", jsx:encode(Manifest), []) of
        {ok, ManifestCID} -> cluster_pin(ManifestCID);
        Error -> Error
    end.

collect_text_chunks(TotalChunks, Acc, Workers, MonitorPid) ->
    case length(Acc) of
        TotalChunks ->
            MonitorPid ! complete,
            {ok, lists:sort(Acc)};
        _ ->
            receive
                {chunk_complete, Index, CID} ->
                    MonitorPid ! {progress, length(Acc) + 1},
                    collect_text_chunks(TotalChunks, [{Index, CID}|Acc], Workers, MonitorPid);
                {chunk_error, _Index, Error} ->
                    MonitorPid ! cancel,
                    Error;
                worker_done ->
                    collect_text_chunks(TotalChunks, Acc, Workers -- [self()], MonitorPid)
            after
                300000 -> % 5 minute timeout
                    MonitorPid ! cancel,
                    {error, upload_timeout}
            end
    end.

upload_monitor(TotalChunks) ->
    receive
        {progress, N} ->
            io:format("Upload progress: ~.1f%~n", [N*100/TotalChunks]),
            upload_monitor(TotalChunks);
        complete -> ok;
        cancel -> ok
    after
        3600000 -> io:format("Upload monitor timeout~n")
    end.

generate_boundary() ->
    "----" ++ integer_to_list(erlang:unique_integer([positive])).

ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

verify_download(Path) ->
    case filelib:file_size(Path) of
        0 -> {error, zero_length_file};
        Size -> {ok, Path, Size}
    end.

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