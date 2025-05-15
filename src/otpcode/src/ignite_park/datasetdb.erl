-module(datasetdb). 
-author("Zaryn Technologies").
-export([insert/4, get_dataset_by_id/1, get_datasets_by_user_id/1, 
         get_dataset_content_by_id/1, update_dataset/2, delete_dataset/1, 
         get_datasets/0, get_all_datasets_from_date/4, 
         pin_dataset/1, get_dataset_ipns_by_id/1, get_dataset_ipfs_by_ipns/1]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

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

get_dataset_content_by_id(Id) ->
    case get_dataset_by_id(Id) of
        {error, Reason} -> {error, Reason};
        Dataset ->
            ContentCID = Dataset#dataset.content_cid,
            case ContentCID of
                undefined -> {error, content_not_available};
                "" -> {error, empty_content};
                _ -> 
                    try
                        {ok, ipfs_content:download_content(ContentCID)}
                    catch
                        _:_ -> {error, ipfs_download_failed}
                    end
            end
    end.

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