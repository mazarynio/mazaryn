-module(ipfs_video).
-author("Zaryn Technologies").
-export([
    upload_video/1,
    upload_video/2,
    upload_video_from_file/2,
    upload_video_from_binary/2,
    get_video/1,
    get_video_binary/1
]).

-define(RPC_API, "http://localhost:5001/api").
-define(DEFAULT_CHUNK_SIZE, 1048576).  % 1MB
-define(DEFAULT_ADD_OPTS, [
    {pin, false},
    {cid_version, 1},
    {wrap_with_directory, false},
    {chunker, "size-" ++ integer_to_list(?DEFAULT_CHUNK_SIZE)}
]).
-define(LARGE_FILE_THRESHOLD, 104857600).  % 100MB
-define(MAX_PARALLEL_UPLOADS, 8).

upload_video(Data) when is_binary(Data) ->
    upload_video_from_binary(Data, "video.webm");
upload_video(FilePath) when is_list(FilePath) ->
    case filelib:is_file(FilePath) of
        true -> upload_video_from_file(FilePath, filename:basename(FilePath));
        false -> {error, file_not_found}
    end.

upload_video(Data, CustomFilename) when is_binary(Data) ->
    upload_video_from_binary(Data, CustomFilename);
upload_video(FilePath, CustomFilename) when is_list(FilePath) ->
    case filelib:is_file(FilePath) of
        true -> upload_video_from_file(FilePath, CustomFilename);
        false -> {error, file_not_found}
    end.

upload_video_from_binary(Data, CustomFilename) ->
    Size = byte_size(Data),
    if
        Size > ?LARGE_FILE_THRESHOLD ->
            upload_large_binary_parallel(Data, CustomFilename, Size);
        true ->
            upload_single_binary(Data, CustomFilename)
    end.

upload_video_from_file(FilePath, CustomFilename) ->
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
    ChunkSize = ?DEFAULT_CHUNK_SIZE,
    TotalChunks = ceiling(DataSize / ChunkSize),
    MonitorPid = spawn(fun() -> upload_monitor(TotalChunks) end),
    CoordinatorPid = self(),
    Workers = lists:map(
        fun(I) ->
            spawn_link(fun() ->
                upload_binary_chunk_worker(Data, CustomFilename, ChunkSize, TotalChunks, I - 1, CoordinatorPid)
            end)
        end, lists:seq(1, min(?MAX_PARALLEL_UPLOADS, TotalChunks))),
    collect_results(TotalChunks, [], Workers, MonitorPid, DataSize).

upload_large_file_parallel(FilePath, CustomFilename, FileSize) ->
    ChunkSize = ?DEFAULT_CHUNK_SIZE,
    TotalChunks = ceiling(FileSize / ChunkSize),
    MonitorPid = spawn(fun() -> upload_monitor(TotalChunks) end),
    case file:open(FilePath, [read, binary, raw]) of
        {ok, Fd} ->
            try
                CoordinatorPid = self(),
                Workers = lists:map(
                    fun(_) ->
                        spawn_link(fun() ->
                            upload_file_chunk_worker(Fd, CustomFilename, ChunkSize, CoordinatorPid)
                        end)
                    end, lists:seq(1, ?MAX_PARALLEL_UPLOADS)),
                collect_results(TotalChunks, [], Workers, MonitorPid, FileSize)
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
            ChunkData = binary:part(Data, Offset, End - Offset),
            case ipfs_add_file(Filename, ChunkData, ?DEFAULT_ADD_OPTS) of
                {ok, CID} ->
                    CoordinatorPid ! {chunk_complete, Offset, CID};
                Error ->
                    CoordinatorPid ! {chunk_error, Offset, Error}
            end,
            upload_binary_chunk_worker(Data, Filename, ChunkSize, TotalChunks, ChunkIndex + ?MAX_PARALLEL_UPLOADS, CoordinatorPid)
    end.

upload_file_chunk_worker(Fd, Filename, ChunkSize, CoordinatorPid) ->
    case get_next_file_chunk(Fd, ChunkSize) of
        {ok, Offset, Data} ->
            case ipfs_add_file(Filename, Data, ?DEFAULT_ADD_OPTS) of
                {ok, CID} ->
                    CoordinatorPid ! {chunk_complete, Offset, CID};
                Error ->
                    CoordinatorPid ! {chunk_error, Offset, Error}
            end,
            upload_file_chunk_worker(Fd, Filename, ChunkSize, CoordinatorPid);
        eof ->
            CoordinatorPid ! worker_done;
        Error ->
            CoordinatorPid ! Error
    end.

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

collect_results(TotalChunks, Acc, Workers, MonitorPid, TotalSize) ->
    case length(Acc) of
        TotalChunks ->
            MonitorPid ! complete,
            create_manifest(Acc, TotalSize);
        _ ->
            receive
                {chunk_complete, _Offset, CID} ->
                    collect_results(TotalChunks, [CID | Acc], Workers, MonitorPid, TotalSize);
                {chunk_error, _Offset, Error} ->
                    MonitorPid ! cancel,
                    Error;
                worker_done ->
                    case Workers -- [self()] of
                        [] ->
                            MonitorPid ! complete,
                            create_manifest(Acc, TotalSize);
                        RemainingWorkers ->
                            collect_results(TotalChunks, Acc, RemainingWorkers, MonitorPid, TotalSize)
                    end
            after
                3600000 ->
                    MonitorPid ! cancel,
                    {error, upload_timeout}
            end
    end.

create_manifest(ChunkCIDs, TotalSize) ->
    Manifest = #{
        <<"type">> => <<"chunked_video">>,
        <<"chunks">> => lists:reverse(ChunkCIDs),
        <<"total_size">> => TotalSize,
        <<"timestamp">> => erlang:system_time(seconds),
        <<"chunk_size">> => ?DEFAULT_CHUNK_SIZE,
        <<"chunk_count">> => length(ChunkCIDs)
    },
    ManifestBinary = jsx:encode(Manifest),
    case ipfs_add_file("video_manifest.json", ManifestBinary, ?DEFAULT_ADD_OPTS) of
        {ok, ManifestCID} ->
            ipfs_cluster:pin_to_cluster(ManifestCID),
            binary_to_list(ManifestCID);
        Error -> Error
    end.

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

get_video(CID) when is_binary(CID) ->
    get_video(binary_to_list(CID));
get_video(CID) when is_list(CID) ->
    case ipfs_client_1:cat(CID) of
        {ok, Binary} when is_binary(Binary) ->
            case is_manifest(Binary) of
                true -> reconstruct_from_manifest(Binary);
                false -> {ok, Binary}
            end;
        Binary when is_binary(Binary) ->
            case is_manifest(Binary) of
                true -> reconstruct_from_manifest(Binary);
                false -> {ok, Binary}
            end;
        {error, Reason} ->
            {error, {ipfs_cat_error, Reason}};
        Other ->
            {error, {unexpected_response, Other}}
    end.

get_video_binary(CID) ->
    case get_video(CID) of
        {ok, Binary} -> Binary;
        Error -> Error
    end.

is_manifest(Binary) ->
    try
        jsx:decode(Binary, [return_maps]) of
            #{<<"type">> := <<"chunked_video">>, <<"chunks">> := _} -> true;
            #{<<"chunks">> := _} -> true;
            _ -> false
    catch
        _:_ -> false
    end.

reconstruct_from_manifest(Binary) ->
    try
        Manifest = jsx:decode(Binary, [return_maps]),
        Chunks = maps:get(<<"chunks">>, Manifest, []),
        TotalSize = maps:get(<<"total_size">>, Manifest, 0),
        ChunkList = case Chunks of
            [] -> [];
            _ when is_list(Chunks) -> Chunks;
            _ -> []
        end,
        Result = reconstruct_chunks(ChunkList, <<>>),
        case byte_size(Result) of
            TotalSize -> {ok, Result};
            _ -> {ok, Result}
        end
    catch
        _:_ -> {error, invalid_manifest}
    end.

reconstruct_chunks([], Acc) ->
    Acc;
reconstruct_chunks([CID | Rest], Acc) when is_binary(CID) ->
    case ipfs_client_1:cat(binary_to_list(CID)) of
        {ok, Data} when is_binary(Data) ->
            reconstruct_chunks(Rest, <<Acc/binary, Data/binary>>);
        Data when is_binary(Data) ->
            reconstruct_chunks(Rest, <<Acc/binary, Data/binary>>);
        _ ->
            reconstruct_chunks(Rest, Acc)
    end;
reconstruct_chunks([CID | Rest], Acc) when is_list(CID) ->
    case ipfs_client_1:cat(CID) of
        {ok, Data} when is_binary(Data) ->
            reconstruct_chunks(Rest, <<Acc/binary, Data/binary>>);
        Data when is_binary(Data) ->
            reconstruct_chunks(Rest, <<Acc/binary, Data/binary>>);
        _ ->
            reconstruct_chunks(Rest, Acc)
    end;
reconstruct_chunks([_ | Rest], Acc) ->
    reconstruct_chunks(Rest, Acc).
