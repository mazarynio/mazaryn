-module(ipfs_dataset).
-author("Zaryn Technologies").
-export([
    upload_dataset/1,
    upload_dataset/2
]).

-define(RPC_API, "http://localhost:5001/api").
-define(DEFAULT_CHUNK_SIZE, 1048576). % 1MB chunks
-define(DEFAULT_ADD_OPTS, [
    {pin, false},
    {cid_version, 1},
    {wrap_with_directory, false},
    {chunker, "size-" ++ integer_to_list(?DEFAULT_CHUNK_SIZE)}
]).
-define(LARGE_FILE_THRESHOLD, 536870912). % 512MB
-define(MAX_PARALLEL_UPLOADS, 5).

upload_dataset(Data) when is_binary(Data) ->
    upload_dataset_from_binary(Data, "dataset.dat");
upload_dataset(FilePath) when is_list(FilePath) ->
    upload_dataset_from_file(FilePath, filename:basename(FilePath)).

upload_dataset(Data, CustomFilename) when is_binary(Data) ->
    upload_dataset_from_binary(Data, CustomFilename);
upload_dataset(FilePath, CustomFilename) when is_list(FilePath) ->
    upload_dataset_from_file(FilePath, CustomFilename).

upload_dataset_from_binary(Data, CustomFilename) ->
    Size = byte_size(Data),
    if
        Size > ?LARGE_FILE_THRESHOLD ->
            upload_large_binary_parallel(Data, CustomFilename, Size);
        true ->
            upload_single_binary(Data, CustomFilename)
    end.

upload_dataset_from_file(FilePath, CustomFilename) ->
    case filelib:file_size(FilePath) of
        {error, Reason} ->
            {error, {file_size_error, Reason}};
        Size when Size > ?LARGE_FILE_THRESHOLD ->
            upload_large_file_parallel(FilePath, CustomFilename, Size);
        _ ->
            upload_single_file(FilePath, CustomFilename)
    end.

upload_single_binary(Data, CustomFilename) ->
    case ipfs_add_file(CustomFilename, Data, ?DEFAULT_ADD_OPTS) of
        {ok, CID} ->
            case ipfs_cluster:pin_to_cluster(CID) of
                {ok, _} -> binary_to_list(CID);
                Error -> Error
            end;
        Error -> Error
    end.

upload_single_file(FilePath, CustomFilename) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            upload_single_binary(Data, CustomFilename);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

upload_large_binary_parallel(Data, CustomFilename, DataSize) ->
    ChunkSize = optimal_chunk_size(DataSize),
    TotalChunks = ceiling(DataSize / ChunkSize),
    MonitorPid = spawn(fun() -> upload_monitor(TotalChunks) end),
    CoordinatorPid = self(),
    Workers = lists:map(
        fun(I) ->
            spawn_link(fun() ->
                upload_binary_chunk_worker(Data, CustomFilename, ChunkSize, TotalChunks, I - 1, CoordinatorPid)
            end)
        end, lists:seq(1, ?MAX_PARALLEL_UPLOADS)),
    collect_results(TotalChunks, [], Workers, MonitorPid).

upload_large_file_parallel(FilePath, CustomFilename, FileSize) ->
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
                            upload_file_chunk_worker(Fd, CustomFilename, ChunkSize, TotalChunks, CoordinatorPid)
                        end)
                    end, lists:seq(1, ?MAX_PARALLEL_UPLOADS)),
                collect_results(TotalChunks, [], Workers, MonitorPid)
            after
                file:close(Fd)
            end;
        {error, Reason} ->
            {error, {file_open_error, Reason}}
    end.

upload_binary_chunk_worker(Data, Filename, ChunkSize, TotalChunks, ChunkIndex, CoordinatorPid) ->
    Offset = ChunkIndex * ChunkSize,
    if
        Offset >= byte_size(Data) ->
            CoordinatorPid ! worker_done;
        true ->
            End = min(Offset + ChunkSize, byte_size(Data)),
            Chunk = binary:part(Data, Offset, End - Offset),
            case ipfs_add_file(Filename, Chunk, ?DEFAULT_ADD_OPTS) of
                {ok, CID} ->
                    CoordinatorPid ! {chunk_complete, Offset, CID};
                Error ->
                    CoordinatorPid ! {chunk_error, Offset, Error}
            end
    end.

upload_file_chunk_worker(Fd, Filename, ChunkSize, _TotalChunks, CoordinatorPid) ->
    case get_next_file_chunk(Fd, ChunkSize) of
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
    upload_file_chunk_worker(Fd, Filename, ChunkSize, _TotalChunks, CoordinatorPid).

get_next_file_chunk(Fd, ChunkSize) ->
    case get(fd_offset) of
        undefined ->
            put(fd_offset, 0),
            get_next_file_chunk(Fd, ChunkSize);
        Offset ->
            case file:pread(Fd, Offset, ChunkSize) of
                {ok, Data} ->
                    put(fd_offset, Offset + byte_size(Data)),
                    {ok, Offset, Data};
                eof ->
                    eof;
                Error ->
                    Error
            end
    end.

collect_results(TotalChunks, Acc, Workers, MonitorPid) ->
    case length(Acc) of
        TotalChunks ->
            MonitorPid ! complete,
            create_manifest(Acc, TotalChunks * ?DEFAULT_CHUNK_SIZE);
        _ ->
            receive
                {chunk_complete, _Offset, CID} ->
                    MonitorPid ! {progress, length(Acc) + 1},
                    collect_results(TotalChunks, [CID | Acc], Workers, MonitorPid);
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
                3600000 ->
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
    MaxChunk = 8388608,
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

ipfs_add_file(Filename, Data, Opts) ->
    case ipfs_client_1:add_file(Filename, Data, Opts) of
        {ok, CID} ->
            case ipfs_cluster:pin_to_cluster(CID) of
                {ok, _} -> {ok, CID};
                Error -> Error
            end;
        Error -> Error
    end.
