-module(datasetdb). 
-author("Zaryn Technologies").
-export([insert/4, get_dataset_by_id/1, get_datasets_by_user_id/1, 
         get_dataset_content_by_id/1, update_dataset/2, delete_dataset/1, 
         get_datasets/0, get_all_datasets_from_date/4, 
         pin_dataset/1, get_dataset_ipns_by_id/1, get_dataset_ipfs_by_ipns/1, display_media/1]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(DEFAULT_CONCURRENCY, 5).
-define(DEFAULT_TIMEOUT, 30000).

insert(UserID, FileData, Title, Description) ->  
    Fun = fun() ->
        Id = nanoid:gen(),
        Date = calendar:universal_time(),
        
        ContentToCache = if
            is_binary(FileData) -> binary_to_list(FileData);
            true -> FileData
        end,
        
        ok = content_cache:set(Id, ContentToCache),
        
        PlaceholderContent = Id,
        
        mnesia:write(#dataset{
            id = Id,
            title = Title,
            description = Description,
            creator_id = UserID,
            content_cid = PlaceholderContent,
            date_created = Date,
            downloads = 0
        }),
        
        case mnesia:read({user, UserID}) of
            [User] ->
                Datasets = User#user.datasets,
                mnesia:write(User#user{datasets = [Id | Datasets]});
            [] -> ok
        end,
        
        update_activity(UserID, Date),
        
        {ok, Id}
    end,
  
    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} -> 
            spawn(fun() ->
                ContentToUse = content_cache:get(Id),
                CIDString = case ContentToUse of
                    "" -> ""; 
                    _ -> 
                        try
                            ipfs_media:upload_media(ContentToUse)
                        catch
                            Exception:Error:Stacktrace ->
                                error_logger:error_msg(
                                    "Exception while uploading dataset ~p to IPFS: ~p:~p~n~p", 
                                    [Id, Exception, Error, Stacktrace]
                                ),
                                undefined
                        end
                end,
                
                UpdateF = fun() ->
                    case mnesia:read({dataset, Id}) of
                        [Dataset] ->
                            UpdatedDataset = Dataset#dataset{
                                content_cid = CIDString
                            },
                            mnesia:write(UpdatedDataset);
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),
                
                content_cache:delete(Id),
                
                spawn(fun() ->
                    timer:sleep(15000),
                    
                    case CIDString of
                        undefined -> 
                            error_logger:info_msg("No content to publish to IPNS for dataset ~p", [Id]);
                        "" -> 
                            error_logger:info_msg("Empty content, skipping IPNS publish for dataset ~p", [Id]);
                        _ ->
                            try
                                {ok, #{id := _KeyID, name := _BinID}} = ipfs_client_4:key_gen(Id),
                                
                                PublishOptions = [
                                        {key, Id},
                                        {resolve, false},     
                                        {lifetime, "168h0m0s"},  
                                        {ttl, "15m0s"},         
                                        {v1compat, true},        
                                        {ipns_base, "base36"},   
                                        {quieter, true},        
                                        {'allow-offline', true}  
                                    ],
                                
                                case ipfs_client_5:name_publish(
                                    "/ipfs/" ++ CIDString,
                                    PublishOptions
                                ) of
                                    {ok, #{name := IPNSKey}} ->
                                        update_dataset_ipns(Id, IPNSKey);
                                    {error, Reason} ->
                                        error_logger:error_msg("IPNS publish failed for dataset ~p: ~p", [Id, Reason]),
                                        err 
                                end
                            catch
                                _Exception:_Error:_Stacktrace ->
                                    error_logger:error_msg(
                                        "Exception while publishing to IPNS for dataset ~p: ~p:~p~n~p", 
                                        [Id, _Exception, _Error, _Stacktrace]
                                    )
                            end
                    end
                end)
            end),
            
            Id;
        {atomic, {error, Reason}} -> 
            {error, Reason};
        {aborted, Reason} -> 
            {error, {transaction_failed, Reason}}
    end.

update_dataset_ipns(DatasetId, IPNSKey) ->
    UpdateF = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [Dataset] ->
                UpdatedDataset = Dataset#dataset{ipns = IPNSKey},
                mnesia:write(UpdatedDataset),
                ok;
            [] ->
                {error, not_found}
        end
    end,
    
    case mnesia:transaction(UpdateF) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> 
            error_logger:error_msg("Failed to update dataset ~p with IPNS: ~p", [DatasetId, Reason]),
            {error, Reason};
        {aborted, Reason} ->
            error_logger:error_msg("Transaction aborted while updating dataset ~p with IPNS: ~p", [DatasetId, Reason]),
            {error, {transaction_failed, Reason}}
    end.

get_dataset_by_id(Id) ->
    Fun = fun() ->
        case mnesia:read({dataset, Id}) of
            [Dataset] -> Dataset;
            [] -> {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_dataset_content_by_id(PostID) ->
    get_dataset_content_by_id(PostID, ?DEFAULT_CONCURRENCY).

get_dataset_content_by_id(PostID, MaxConcurrent) when is_integer(MaxConcurrent), MaxConcurrent > 0 ->
    case get_media_manifest(PostID) of
        {error, Reason} -> 
            {error, Reason};
        ManifestBinary when is_binary(ManifestBinary) -> 
            case parse_and_retrieve_media_async(ManifestBinary, MaxConcurrent) of
                {ok, DatasetBinary} -> DatasetBinary;
                Error -> Error
            end;
        Other -> 
            Other
    end.
get_media_manifest(PostID) ->
    Fun = fun() ->
        case mnesia:read({dataset, PostID}) of
            [] -> {error, post_not_found};
            [Post] -> 
                Dataset = Post#dataset.content_cid,
                case Dataset of
                    {dataset, ID} when ID =:= PostID -> 
                        case content_cache:get({dataset, ID}) of
                            undefined -> {error, dataset_not_ready};
                            "" -> {ok, ""}; 
                            CachedDataset -> {ok, CachedDataset}
                        end;
                    nil -> {ok, nil};
                    "" -> {ok, ""};
                    _ ->
                        try
                            ActualDataset = ipfs_media:get_media_binary(Dataset),
                            {ok, ActualDataset}
                        catch
                            _:Error -> {error, Error}
                        end
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Dataset}} -> Dataset;
        {atomic, {error, Reason}} -> {error, Reason};
        Error -> Error
    end.
parse_and_retrieve_media_async(ManifestBinary, MaxConcurrent) ->
    try
        Manifest = jsx:decode(ManifestBinary, [return_maps]),
        case maps:get(<<"chunks">>, Manifest, undefined) of
            undefined -> 
                {error, invalid_manifest};
            [] ->
                {ok, <<>>};
            Chunks when is_list(Chunks) ->
                retrieve_chunks_with_limit(Chunks, MaxConcurrent)
        end
    catch
        _:_ -> 
            {ok, ManifestBinary}
    end.

retrieve_chunks_with_limit(Chunks, MaxConcurrent) ->
    Parent = self(),
    ChunksWithIndex = lists:zip(Chunks, lists:seq(1, length(Chunks))),
    
    WorkerPids = start_chunk_workers(min(MaxConcurrent, length(Chunks)), Parent),
    
    Queue = queue:from_list(ChunksWithIndex),
    
    {InitialQueue, WorkerState} = assign_initial_work(Queue, WorkerPids, #{}),
    
    Result = process_chunk_results(InitialQueue, WorkerState, length(Chunks), #{}, ?DEFAULT_TIMEOUT),
    
    lists:foreach(fun(Pid) -> exit(Pid, normal) end, WorkerPids),
    
    Result.

start_chunk_workers(Count, Parent) ->
    [spawn_link(fun() -> chunk_worker_loop(Parent) end) || _ <- lists:seq(1, Count)].

chunk_worker_loop(Parent) ->
    receive
        {retrieve_chunk, Ref, ChunkCID, Index} ->
            Result = retrieve_chunk(ChunkCID, Index),
            Parent ! {chunk_result, Ref, self(), Result},
            chunk_worker_loop(Parent);
        stop ->
            ok
    end.

retrieve_chunk(ChunkCID, Index) ->
    case ipfs_media:get_media_binary(ChunkCID) of
        {error, Reason} ->
            {error, {chunk_retrieve_error, ChunkCID, Reason}};
        ChunkBinary when is_binary(ChunkBinary) ->
            {ok, {Index, ChunkBinary}}
    end.

assign_initial_work(Queue, [WorkerPid|Workers], WorkerState) ->
    case queue:out(Queue) of
        {{value, {ChunkCID, Index}}, NewQueue} ->
            Ref = make_ref(),
            WorkerPid ! {retrieve_chunk, Ref, ChunkCID, Index},
            assign_initial_work(NewQueue, Workers, maps:put(Ref, {WorkerPid, Index}, WorkerState));
        {empty, Queue} ->
            {Queue, WorkerState}
    end;
assign_initial_work(Queue, [], WorkerState) ->
    {Queue, WorkerState}.
process_chunk_results(_Queue, _WorkerState, TotalChunks, Results, _Timeout) when map_size(Results) =:= TotalChunks ->
    try
        SortedChunks = lists:sort(
            fun({IndexA, _}, {IndexB, _}) -> IndexA =< IndexB end,
            maps:values(Results)
        ),
        
        CombinedBinary = lists:foldl(
            fun({_, ChunkBinary}, AccBin) ->
                <<AccBin/binary, ChunkBinary/binary>>
            end,
            <<>>,
            SortedChunks
        ),
        {ok, CombinedBinary}
    catch
        _:Error ->
            {error, {combine_error, Error}}
    end;

process_chunk_results(Queue, WorkerState, TotalChunks, Results, Timeout) ->
    receive
        {chunk_result, Ref, WorkerPid, {ok, {Index, ChunkBinary}}} ->
            NewResults = maps:put(Index, {Index, ChunkBinary}, Results),
            
            case queue:out(Queue) of
                {{value, {NextChunkCID, NextIndex}}, NewQueue} ->
                    NextRef = make_ref(),
                    WorkerPid ! {retrieve_chunk, NextRef, NextChunkCID, NextIndex},
                    NewWorkerState = maps:remove(Ref, WorkerState),
                    NewWorkerState2 = maps:put(NextRef, {WorkerPid, NextIndex}, NewWorkerState),
                    process_chunk_results(NewQueue, NewWorkerState2, TotalChunks, NewResults, Timeout);
                {empty, NewQueue} ->
                    NewWorkerState = maps:remove(Ref, WorkerState),
                    process_chunk_results(NewQueue, NewWorkerState, TotalChunks, NewResults, Timeout)
            end;
        
        {chunk_result, _Ref, _WorkerPid, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
        {error, {chunk_timeout, map_size(WorkerState), TotalChunks - map_size(Results)}}
    end.

display_media(MediaBinary) when is_binary(MediaBinary) ->
    FileType = determine_file_type(MediaBinary),
    TempFilePath = generate_temp_filepath(FileType),
    ok = file:write_file(TempFilePath, MediaBinary),
    open_file_with_viewer(TempFilePath),
    
    {ok, TempFilePath}.

determine_file_type(<<16#25, 16#50, 16#44, 16#46, _/binary>>) -> 
    "pdf";
determine_file_type(<<16#49, 16#49, 16#2A, 16#00, _/binary>>) -> 
    "tiff"; 
determine_file_type(<<16#4D, 16#4D, 16#00, 16#2A, _/binary>>) -> 
    "tiff";
determine_file_type(<<16#50, 16#4B, 16#03, 16#04, _/binary>>) -> 
    "zip";  % ZIP file signature (PK\003\004)
determine_file_type(<<16#50, 16#4B, 16#05, 16#06, _/binary>>) -> 
    "zip";  % ZIP end of central directory record
determine_file_type(<<16#50, 16#4B, 16#07, 16#08, _/binary>>) -> 
    "zip";  % ZIP spanning marker
determine_file_type(_) -> 
    "bin".

generate_temp_filepath(Extension) ->
    TempDir = case os:type() of
        {unix, _} -> "/tmp";
        {win32, _} -> 
            case os:getenv("TEMP") of
                false -> "C:/Windows/Temp";
                TempPath -> TempPath
            end
    end,
    
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    Timestamp = lists:flatten(io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B", 
                                          [Y, M, D, H, Min, S])),
    Random = integer_to_list(rand:uniform(1000000)),
    Filename = "media_" ++ Timestamp ++ "_" ++ Random ++ "." ++ Extension,
    
    filename:join(TempDir, Filename).

open_file_with_viewer(FilePath) ->
    Command = case os:type() of
        {unix, darwin} -> 
            "open \"" ++ FilePath ++ "\"";
        {unix, _} -> 
            "xdg-open \"" ++ FilePath ++ "\"";
        {win32, _} -> 
            "start \"\" \"" ++ FilePath ++ "\""
    end,
    
    os:cmd(Command),
    ok.

get_datasets_by_user_id(UserId) ->
    Fun = fun() ->
        Query = qlc:q([D || D <- mnesia:table(dataset), 
                            D#dataset.creator_id =:= UserId]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Datasets} -> Datasets;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_dataset(Id, UpdateAttrs) ->
    Fun = fun() ->
        case mnesia:read({dataset, Id}) of
            [Dataset] ->
                UpdatedDataset = apply_updates(Dataset, UpdateAttrs),
                mnesia:write(UpdatedDataset),
                {ok, UpdatedDataset};
            [] -> {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

apply_updates(Dataset, UpdateAttrs) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                title -> Acc#dataset{title = Value};
                description -> Acc#dataset{description = Value};
                download_count -> Acc#dataset{downloads = Value};
                _ -> Acc
            end
        end, Dataset, UpdateAttrs).

delete_dataset(Id) ->
    Fun = fun() ->
        case mnesia:read({dataset, Id}) of
            [Dataset] ->
                UserID = Dataset#dataset.creator_id,
                case mnesia:read({user, UserID}) of
                    [User] ->
                        UpdatedDatasetsList = lists:delete(Id, User#user.datasets),
                        mnesia:write(User#user{datasets = UpdatedDatasetsList});
                    [] -> ok
                end,
                mnesia:delete({dataset, Id}),
                ok;
            [] -> {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_datasets() ->
    Fun = fun() ->
        Query = qlc:q([D || D <- mnesia:table(dataset)]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Datasets} -> Datasets;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_all_datasets_from_date(Year, Month, Day, Limit) ->
    Fun = fun() ->
        StartDate = {{Year, Month, Day}, {0, 0, 0}},
        Query = qlc:q([D || D <- mnesia:table(dataset),
                            D#dataset.date_created >= StartDate]),
        
        SortedQuery = qlc:sort(Query, [{order, descending}]),
        case Limit of
            all -> qlc:e(SortedQuery);
            _ when is_integer(Limit), Limit > 0 ->
                Cursor = qlc:cursor(SortedQuery),
                Result = qlc:next_answers(Cursor, Limit),
                qlc:delete_cursor(Cursor),
                Result
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Datasets} -> Datasets;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

pin_dataset(Id) ->
    case get_dataset_by_id(Id) of
        {error, Reason} -> {error, Reason};
        Dataset ->
            ContentCID = Dataset#dataset.content_cid,
            case ContentCID of
                undefined -> {error, content_not_available};
                "" -> {error, empty_content};
                _ -> 
                    try
                        ipfs_client_5:pin_add(ContentCID)
                    catch
                        _:_ -> {error, pin_failed}
                    end
            end
    end.

get_dataset_ipns_by_id(Id) ->
    case get_dataset_by_id(Id) of
        {error, Reason} -> {error, Reason};
        Dataset ->
            case Dataset#dataset.ipns of
                undefined -> {error, ipns_not_available};
                "" -> {error, empty_ipns};
                IPNS -> {ok, IPNS}
            end
    end.

get_dataset_ipfs_by_ipns(IPNS) ->
    try
        case ipfs_client_5:name_resolve(IPNS) of
            {ok, #{path := Path}} ->
                CID = string:substr(Path, 7),
                {ok, CID};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ -> {error, resolve_failed}
    end.

update_activity(UserID, Date) ->
    Fun = fun() ->
        case mnesia:read({user, UserID}) of
            [User] ->
                mnesia:write(User#user{last_activity = Date});
            [] -> ok
        end
    end,
    mnesia:transaction(Fun).