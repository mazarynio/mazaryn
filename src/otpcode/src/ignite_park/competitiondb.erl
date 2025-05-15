-module(competitiondb).
-author("Zaryn Technologies").
-export([insert/6, get_competition_by_id/1, update_competition_status/2]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% StartTime = {{2025, 5, 15}, {9, 0, 0}}.
%% EndTime = {{2025, 6, 15}, {18, 0, 0}}.
insert(UserID, Title, Description, DatasetIDs, StartTime, EndTime) ->
    NormalizedDatasetIDs = case is_list(DatasetIDs) andalso not is_integer(hd(DatasetIDs)) of
        true -> DatasetIDs; 
        false -> [DatasetIDs] 
    end,
    
    Fun = fun() ->
        Id = nanoid:gen(),
        DateNow = calendar:universal_time(),
        
        {DatasetCIDs, DatasetIPNS} = lists:foldl(
            fun(DatasetID, {CIDsAcc, IPNSAcc}) ->
                case mnesia:read({dataset, DatasetID}) of
                    [Dataset] -> 
                        CID = case Dataset#dataset.content_cid of
                            undefined -> "";
                            "" -> "";
                            Val -> Val
                        end,
                        
                        IPNS = case Dataset#dataset.ipns of
                            undefined -> "";
                            "" -> "";
                            IPNSVal -> IPNSVal
                        end,
                        
                        {[CID | CIDsAcc], [IPNS | IPNSAcc]};
                    [] -> 
                        {CIDsAcc, IPNSAcc}
                end
            end, {[], []}, NormalizedDatasetIDs),
        
        FinalCIDs = lists:reverse(DatasetCIDs),
        FinalIPNS = lists:reverse(DatasetIPNS),
        
        mnesia:write(#competition{
            id = Id,
            title = Title,
            description = Description,
            creator_id = UserID,
            dataset_id = NormalizedDatasetIDs,
            dataset_cids = FinalCIDs,
            dataset_ipns = FinalIPNS,
            start_time = StartTime,
            end_time = EndTime,
            reward_type = undefined,
            reward_value = undefined,
            rules = #{},
            evaluation_metric = undefined,
            submission_count_limit = infinity,
            team_size_limit = infinity,
            status = 
                if
                    StartTime =< DateNow, EndTime > DateNow -> active;
                    StartTime > DateNow -> draft;
                    true -> ended
                end,
            participants = [],
            date_created = DateNow,
            date_updated = DateNow
        }),
        
        case mnesia:read({user, UserID}) of
            [User] ->
                Competitions = User#user.competitions,
                mnesia:write(User#user{competitions = [Id | Competitions]});
            [] -> ok
        end,
        
        lists:foreach(
            fun(DatasetID) ->
                case mnesia:read({dataset, DatasetID}) of
                    [Dataset] ->
                        CompetitionList = Dataset#dataset.competitions,
                        UpdatedDataset = Dataset#dataset{
                            competitions = [Id | CompetitionList]
                        },
                        mnesia:write(UpdatedDataset);
                    [] -> ok
                end
            end, NormalizedDatasetIDs),
        
        update_activity(UserID, DateNow),
        
        {ok, Id}
    end,
    
    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} -> 
            schedule_competition_status_updates(Id, StartTime, EndTime),
            
            spawn(fun() ->
                timer:sleep(20000),  
                
                UpdateF = fun() ->
                    case mnesia:read({competition, Id}) of
                        [Competition] ->
                            {UpdatedCIDs, UpdatedIPNS} = lists:foldl(
                                fun(DatasetID, {CidsAcc, IpnsAcc}) ->
                                    case mnesia:read({dataset, DatasetID}) of
                                        [Dataset] -> 
                                            CID = case Dataset#dataset.content_cid of
                                                undefined -> "";
                                                "" -> "";
                                                Val -> Val
                                            end,
                                            
                                            IPNS = case Dataset#dataset.ipns of
                                                undefined -> "";
                                                "" -> "";
                                                IPNSVal -> IPNSVal
                                            end,
                                            
                                            {[CID | CidsAcc], [IPNS | IpnsAcc]};
                                        [] -> 
                                            {CidsAcc, IpnsAcc}
                                    end
                                end, {[], []}, Competition#competition.dataset_id),
                                
                            FinalUpdatedCIDs = lists:reverse(UpdatedCIDs),
                            FinalUpdatedIPNS = lists:reverse(UpdatedIPNS),
                            
                            error_logger:info_msg("Updating competition ~p with dataset CIDs: ~p", [Id, FinalUpdatedCIDs]),
                            error_logger:info_msg("Updating competition ~p with dataset IPNS: ~p", [Id, FinalUpdatedIPNS]),
                            
                            UpdatedCompetition = Competition#competition{
                                dataset_cids = FinalUpdatedCIDs,
                                dataset_ipns = FinalUpdatedIPNS,
                                date_updated = calendar:universal_time()
                            },
                            mnesia:write(UpdatedCompetition);
                        [] -> ok
                    end
                end,
                
                case mnesia:transaction(UpdateF) of
                    {atomic, _} -> 
                        error_logger:info_msg("Successfully updated competition ~p with latest dataset CIDs and IPNS", [Id]);
                    {aborted, Reason} ->
                        error_logger:error_msg("Failed to update competition ~p with latest CIDs: ~p", [Id, Reason])
                end
            end),
            
            Id;
        {atomic, {error, Reason}} -> 
            {error, Reason};
        {aborted, Reason} -> 
            {error, {transaction_failed, Reason}}
    end.

update_activity(UserID, Timestamp) ->
    case mnesia:read({user, UserID}) of
        [User] ->
            mnesia:write(User#user{last_activity = Timestamp});
        [] -> ok
    end.

get_competition_by_id(Id) ->
    Fun = fun() ->
        case mnesia:read({competition, Id}) of
            [Competition] -> Competition;
            [] -> {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_competition_status(CompetitionId, NewStatus) ->
    UpdateF = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [Competition] ->
                UpdatedCompetition = Competition#competition{
                    status = NewStatus,
                    date_updated = calendar:universal_time()
                },
                mnesia:write(UpdatedCompetition),
                ok;
            [] ->
                {error, not_found}
        end
    end,
    
    case mnesia:transaction(UpdateF) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> 
            error_logger:error_msg("Failed to update competition ~p status: ~p", 
                                  [CompetitionId, Reason]),
            {error, Reason};
        {aborted, Reason} ->
            error_logger:error_msg("Transaction aborted while updating competition ~p status: ~p", 
                                  [CompetitionId, Reason]),
            {error, {transaction_failed, Reason}}
    end.

schedule_competition_status_updates(CompetitionId, StartTime, EndTime) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    StartSecs = calendar:datetime_to_gregorian_seconds(StartTime),
    EndSecs = calendar:datetime_to_gregorian_seconds(EndTime),
    
    TimeToStart = (StartSecs - NowSecs) * 1000,
    if
        TimeToStart > 0 ->
            spawn(fun() ->
                timer:sleep(TimeToStart),
                update_competition_status(CompetitionId, active)
            end);
        true -> ok
    end,
    
    TimeToEnd = (EndSecs - NowSecs) * 1000,
    if
        TimeToEnd > 0 ->
            spawn(fun() ->
                timer:sleep(TimeToEnd),
                update_competition_status(CompetitionId, ended)
            end);
        true -> ok
    end,
    ok.