-module(competitiondb).
-author("Zaryn Technologies").

-export([
    create_competition/12,
    update_competition/13,
    delete_competition/2,
    get_competition_by_id/1,
    get_competitions_by_creator/1,
    get_active_competitions/0,
    get_public_competitions/0,
    get_competitions_by_status/1,
    get_competitions_by_tag/1,
    get_featured_competitions/0,
    get_competitions_by_difficulty/1,

    start_competition/2,
    end_competition/2,
    archive_competition/2,
    set_competition_status/3,

    add_dataset_to_competition/3,
    remove_dataset_from_competition/3,
    get_competition_datasets/1,

    join_competition/2,
    leave_competition/2,
    get_competition_participants/1,
    is_participant/2,

    create_team/4,
    get_team_by_id/1,
    invite_to_team/3,
    accept_team_invitation/2,
    reject_team_invitation/2,
    remove_team_member/3,
    disband_team/2,
    get_competition_teams/1,
    get_user_team_in_competition/2,
    merge_teams/4,

    submit_entry/5,
    get_submission_by_id/1,
    get_user_submissions/2,
    get_team_submissions/2,
    evaluate_submission/2,
    disqualify_submission/3,
    get_competition_submissions/1,
    get_best_submission/2,

    get_leaderboard/1,
    update_leaderboard/1,
    get_public_leaderboard/1,
    get_private_leaderboard/1,
    get_user_rank/2,
    get_team_rank/2,

    add_discussion/4,
    get_competition_discussions/1,
    pin_discussion/3,
    solve_discussion/3,

    set_prize_distribution/2,
    get_prize_distribution/1,
    distribute_prizes/1,

    set_evaluation_script/2,
    get_evaluation_script/1,
    run_evaluation/2,

    set_compute_quota/2,
    get_compute_quota/1,
    check_quota_available/2,
    consume_quota/3,

    feature_competition/2,
    unfeature_competition/2,

    report_competition/4,

    search_competitions/1,
    search_competitions_advanced/1,
    get_trending_competitions/1,
    get_upcoming_competitions/0,
    get_ending_soon_competitions/1,

    get_competition_stats/1,
    get_user_competition_history/1,
    get_user_competition_stats/1,

    link_notebook_to_submission/2,
    get_submission_notebook/1
]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MAX_TEAM_SIZE, 10).
-define(DEFAULT_SUBMISSION_LIMIT, 5).
-define(EVALUATION_TIMEOUT, 300000).

create_competition(CreatorId, Title, Description, DatasetIds, StartTime, EndTime,
                   RewardType, RewardValue, Rules, EvaluationMetric, SubmissionLimit, TeamSizeLimit) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),

        Competition = #competition{
            id = Id,
            title = Title,
            description = Description,
            creator_id = CreatorId,
            business_id = [],
            dataset_ids = DatasetIds,
            dataset_cids = [],
            dataset_ipns = [],
            start_time = StartTime,
            end_time = EndTime,
            reward_type = RewardType,
            reward_value = RewardValue,
            rules = Rules,
            evaluation_metric = EvaluationMetric,
            submission_count_limit = SubmissionLimit,
            team_size_limit = TeamSizeLimit,
            submission_ids = [],
            participants = [],
            status = draft,
            visibility = public,
            tags = [],
            date_created = Now,
            date_updated = Now,
            report = [],
            metadata = #{},
            discussion_cids = [],
            team_ids = [],
            evaluation_script_cid = undefined,
            prize_distribution = #{},
            external_data_allowed = false,
            late_submission_penalty = 0.0,
            compute_quota = #{cpu_hours => 100, gpu_hours => 10, tpu_hours => 0},
            featured = false,
            difficulty_level = intermediate,
            host_evaluation_cid = undefined
        },

        mnesia:write(Competition),

        Leaderboard = #leaderboard{
            competition_id = Id,
            submission_ids = [],
            evaluation_metric = EvaluationMetric,
            last_updated = Now,
            data = #{},
            public_submission_ids = [],
            private_submission_ids = [],
            evaluation_history = [],
            team_submission_ids = [],
            solo_submission_ids = [],
            best_scores_per_team = #{},
            percentile_data = #{},
            score_distribution_cid = undefined,
            benchmark_score = 0.0,
            prize_cutoffs = #{}
        },

        mnesia:write(Leaderboard),

        case mnesia:read({user, CreatorId}) of
            [User] ->
                UserCompetitions = case User#user.competitions of
                    undefined -> [Id];
                    List when is_list(List) -> [Id | List];
                    _ -> [Id]
                end,
                mnesia:write(User#user{competitions = UserCompetitions});
            [] ->
                error_logger:warning_msg("User ~p not found when creating competition", [CreatorId])
        end,

        lists:foreach(fun(DatasetId) ->
            datasetdb:link_dataset_to_competition(DatasetId, Id)
        end, DatasetIds),

        {ok, Id}
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} -> Id;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_competition(CompetitionId, CreatorId, NewTitle, NewDescription, NewDatasetIds,
                   NewStartTime, NewEndTime, NewRewardType, NewRewardValue, NewRules,
                   NewEvaluationMetric, NewSubmissionLimit, NewTeamSizeLimit) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                case Competition#competition.creator_id of
                    CreatorId ->
                        case Competition#competition.status of
                            active ->
                                {error, cannot_update_active_competition};
                            _ ->
                                Now = calendar:universal_time(),

                                OldDatasetIds = Competition#competition.dataset_ids,
                                RemovedDatasets = OldDatasetIds -- NewDatasetIds,
                                AddedDatasets = NewDatasetIds -- OldDatasetIds,

                                lists:foreach(fun(DatasetId) ->
                                    datasetdb:unlink_dataset_from_competition(DatasetId, CompetitionId)
                                end, RemovedDatasets),

                                lists:foreach(fun(DatasetId) ->
                                    datasetdb:link_dataset_to_competition(DatasetId, CompetitionId)
                                end, AddedDatasets),

                                UpdatedCompetition = Competition#competition{
                                    title = NewTitle,
                                    description = NewDescription,
                                    dataset_ids = NewDatasetIds,
                                    start_time = NewStartTime,
                                    end_time = NewEndTime,
                                    reward_type = NewRewardType,
                                    reward_value = NewRewardValue,
                                    rules = NewRules,
                                    evaluation_metric = NewEvaluationMetric,
                                    submission_count_limit = NewSubmissionLimit,
                                    team_size_limit = NewTeamSizeLimit,
                                    date_updated = Now
                                },

                                mnesia:write(UpdatedCompetition),
                                ok
                        end;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

delete_competition(CompetitionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                case Competition#competition.creator_id of
                    UserId ->
                        case Competition#competition.status of
                            active ->
                                {error, cannot_delete_active_competition};
                            _ ->
                                lists:foreach(fun(DatasetId) ->
                                    datasetdb:unlink_dataset_from_competition(DatasetId, CompetitionId)
                                end, Competition#competition.dataset_ids),

                                mnesia:delete({leaderboard, CompetitionId}),

                                lists:foreach(fun(TeamId) ->
                                    mnesia:delete({team, TeamId})
                                end, Competition#competition.team_ids),

                                lists:foreach(fun(SubmissionId) ->
                                    mnesia:delete({submission, SubmissionId})
                                end, Competition#competition.submission_ids),

                                mnesia:delete({competition, CompetitionId}),

                                case mnesia:read({user, UserId}) of
                                    [User] ->
                                        UpdatedCompetitions = lists:delete(CompetitionId, User#user.competitions),
                                        mnesia:write(User#user{competitions = UpdatedCompetitions});
                                    [] -> ok
                                end,

                                ok
                        end;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_competition_by_id(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] -> {error, competition_not_found};
            [Competition] -> Competition
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_competitions_by_creator(CreatorId) ->
    Fun = fun() ->
        mnesia:match_object(#competition{creator_id = CreatorId, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_active_competitions() ->
    Fun = fun() ->
        mnesia:match_object(#competition{status = active, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_public_competitions() ->
    Fun = fun() ->
        mnesia:match_object(#competition{visibility = public, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_competitions_by_status(Status) ->
    Fun = fun() ->
        mnesia:match_object(#competition{status = Status, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_competitions_by_tag(Tag) ->
    Fun = fun() ->
        AllCompetitions = mnesia:match_object(#competition{_ = '_'}),
        lists:filter(fun(Competition) ->
            lists:member(Tag, Competition#competition.tags)
        end, AllCompetitions)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_featured_competitions() ->
    Fun = fun() ->
        mnesia:match_object(#competition{featured = true, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_competitions_by_difficulty(DifficultyLevel) ->
    Fun = fun() ->
        mnesia:match_object(#competition{difficulty_level = DifficultyLevel, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

start_competition(CompetitionId, CreatorId) ->
    set_competition_status(CompetitionId, CreatorId, active).

end_competition(CompetitionId, CreatorId) ->
    set_competition_status(CompetitionId, CreatorId, ended).

archive_competition(CompetitionId, CreatorId) ->
    set_competition_status(CompetitionId, CreatorId, archived).

set_competition_status(CompetitionId, CreatorId, NewStatus) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                case Competition#competition.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),
                        UpdatedCompetition = Competition#competition{
                            status = NewStatus,
                            date_updated = Now
                        },
                        mnesia:write(UpdatedCompetition),
                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_dataset_to_competition(CompetitionId, CreatorId, DatasetId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                case Competition#competition.creator_id of
                    CreatorId ->
                        UpdatedDatasetIds = case lists:member(DatasetId, Competition#competition.dataset_ids) of
                            true -> Competition#competition.dataset_ids;
                            false -> [DatasetId | Competition#competition.dataset_ids]
                        end,
                        mnesia:write(Competition#competition{
                            dataset_ids = UpdatedDatasetIds,
                            date_updated = calendar:universal_time()
                        }),
                        datasetdb:link_dataset_to_competition(DatasetId, CompetitionId),
                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_dataset_from_competition(CompetitionId, CreatorId, DatasetId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                case Competition#competition.creator_id of
                    CreatorId ->
                        UpdatedDatasetIds = lists:delete(DatasetId, Competition#competition.dataset_ids),
                        mnesia:write(Competition#competition{
                            dataset_ids = UpdatedDatasetIds,
                            date_updated = calendar:universal_time()
                        }),
                        datasetdb:unlink_dataset_from_competition(DatasetId, CompetitionId),
                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_competition_datasets(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] -> {error, competition_not_found};
            [Competition] ->
                DatasetIds = Competition#competition.dataset_ids,
                lists:foldl(fun(DatasetId, Acc) ->
                    case mnesia:read({dataset, DatasetId}) of
                        [Dataset] -> [Dataset | Acc];
                        [] -> Acc
                    end
                end, [], DatasetIds)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

join_competition(CompetitionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                case Competition#competition.status of
                    active ->
                        case lists:member(UserId, Competition#competition.participants) of
                            true ->
                                {error, already_participant};
                            false ->
                                UpdatedParticipants = [UserId | Competition#competition.participants],
                                mnesia:write(Competition#competition{
                                    participants = UpdatedParticipants,
                                    date_updated = calendar:universal_time()
                                }),
                                ok
                        end;
                    _ ->
                        {error, competition_not_active}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

leave_competition(CompetitionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                UpdatedParticipants = lists:delete(UserId, Competition#competition.participants),
                mnesia:write(Competition#competition{
                    participants = UpdatedParticipants,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_competition_participants(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] -> {error, competition_not_found};
            [Competition] -> {ok, Competition#competition.participants}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

is_participant(CompetitionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] -> false;
            [Competition] -> lists:member(UserId, Competition#competition.participants)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

    create_team(CompetitionId, CreatorId, TeamName, InitialMembers) ->
        Fun = fun() ->
            case mnesia:read({competition, CompetitionId}) of
                [] ->
                    {error, competition_not_found};
                [Competition] ->
                    case Competition#competition.status of
                        active ->
                            case lists:member(CreatorId, Competition#competition.participants) of
                                false ->
                                    {error, not_a_participant};
                                true ->
                                    ExistingTeam = get_user_team_in_competition_internal(CompetitionId, CreatorId),
                                    case ExistingTeam of
                                        {ok, _} ->
                                            {error, already_in_team};
                                        {error, _} ->
                                            TeamId = nanoid:gen(),
                                            Now = calendar:universal_time(),

                                            AllMembers = case lists:member(CreatorId, InitialMembers) of
                                                true -> InitialMembers;
                                                false -> [CreatorId | InitialMembers]
                                            end,

                                            case length(AllMembers) > Competition#competition.team_size_limit of
                                                true ->
                                                    {error, team_size_exceeds_limit};
                                                false ->
                                                    MemberRecords = [{CreatorId, captain, Now}] ++
                                                        [{MemberId, member, Now} || MemberId <- InitialMembers, MemberId =/= CreatorId],

                                                    Team = #team{
                                                        id = TeamId,
                                                        competition_id = CompetitionId,
                                                        name = TeamName,
                                                        creator_id = CreatorId,
                                                        members = MemberRecords,
                                                        invitations = [],
                                                        merge_requests = [],
                                                        submission_ids = [],
                                                        team_score = undefined,
                                                        rank = undefined,
                                                        disbanded = false,
                                                        date_created = Now,
                                                        date_updated = Now,
                                                        discussion_cids = [],
                                                        notebook_ids = [],
                                                        compute_quota_used = #{},
                                                        total_submissions = 0,
                                                        best_submission_id = undefined,
                                                        team_avatar_cid = undefined,
                                                        metadata = #{}
                                                    },

                                                    mnesia:write(Team),

                                                    UpdatedTeamIds = [TeamId | Competition#competition.team_ids],
                                                    mnesia:write(Competition#competition{
                                                        team_ids = UpdatedTeamIds,
                                                        date_updated = Now
                                                    }),

                                                    {ok, TeamId}
                                            end
                                    end
                            end;
                        _ ->
                            {error, competition_not_active}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_team_by_id(TeamId) ->
        Fun = fun() ->
            case mnesia:read({team, TeamId}) of
                [] -> {error, team_not_found};
                [Team] -> Team
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    invite_to_team(TeamId, InviterId, InviteeId) ->
        Fun = fun() ->
            case mnesia:read({team, TeamId}) of
                [] ->
                    {error, team_not_found};
                [Team] ->
                    IsMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= InviterId end, Team#team.members),
                    case IsMember of
                        false ->
                            {error, not_team_member};
                        true ->
                            AlreadyMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= InviteeId end, Team#team.members),
                            case AlreadyMember of
                                true ->
                                    {error, already_team_member};
                                false ->
                                    Now = calendar:universal_time(),
                                    ExpiresAt = add_days_to_datetime(Now, 7),
                                    NewInvitation = {InviteeId, pending, Now, ExpiresAt},
                                    UpdatedInvitations = [NewInvitation | Team#team.invitations],
                                    mnesia:write(Team#team{
                                        invitations = UpdatedInvitations,
                                        date_updated = Now
                                    }),
                                    ok
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    accept_team_invitation(TeamId, UserId) ->
        Fun = fun() ->
            case mnesia:read({team, TeamId}) of
                [] ->
                    {error, team_not_found};
                [Team] ->
                    case lists:keyfind(UserId, 1, Team#team.invitations) of
                        false ->
                            {error, invitation_not_found};
                        {UserId, pending, _, ExpiresAt} ->
                            Now = calendar:universal_time(),
                            case Now > ExpiresAt of
                                true ->
                                    {error, invitation_expired};
                                false ->
                                    case mnesia:read({competition, Team#team.competition_id}) of
                                        [] ->
                                            {error, competition_not_found};
                                        [Competition] ->
                                            CurrentMemberCount = length(Team#team.members),
                                            case CurrentMemberCount >= Competition#competition.team_size_limit of
                                                true ->
                                                    {error, team_full};
                                                false ->
                                                    NewMember = {UserId, member, Now},
                                                    UpdatedMembers = [NewMember | Team#team.members],
                                                    UpdatedInvitations = lists:keydelete(UserId, 1, Team#team.invitations),
                                                    mnesia:write(Team#team{
                                                        members = UpdatedMembers,
                                                        invitations = UpdatedInvitations,
                                                        date_updated = Now
                                                    }),
                                                    ok
                                            end
                                    end
                            end;
                        _ ->
                            {error, invitation_not_pending}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    reject_team_invitation(TeamId, UserId) ->
        Fun = fun() ->
            case mnesia:read({team, TeamId}) of
                [] ->
                    {error, team_not_found};
                [Team] ->
                    case lists:keyfind(UserId, 1, Team#team.invitations) of
                        false ->
                            {error, invitation_not_found};
                        _ ->
                            Now = calendar:universal_time(),
                            UpdatedInvitations = lists:keydelete(UserId, 1, Team#team.invitations),
                            mnesia:write(Team#team{
                                invitations = UpdatedInvitations,
                                date_updated = Now
                            }),
                            ok
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_team_member(TeamId, RemoverId, MemberToRemove) ->
        Fun = fun() ->
            case mnesia:read({team, TeamId}) of
                [] ->
                    {error, team_not_found};
                [Team] ->
                    IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                        MemberId =:= RemoverId andalso Role =:= captain
                    end, Team#team.members),
                    case IsCaptain of
                        false ->
                            {error, not_captain};
                        true ->
                            case MemberToRemove =:= RemoverId of
                                true ->
                                    {error, cannot_remove_self};
                                false ->
                                    UpdatedMembers = lists:filter(fun({MemberId, _, _}) ->
                                        MemberId =/= MemberToRemove
                                    end, Team#team.members),
                                    mnesia:write(Team#team{
                                        members = UpdatedMembers,
                                        date_updated = calendar:universal_time()
                                    }),
                                    ok
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    disband_team(TeamId, CaptainId) ->
        Fun = fun() ->
            case mnesia:read({team, TeamId}) of
                [] ->
                    {error, team_not_found};
                [Team] ->
                    IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                        MemberId =:= CaptainId andalso Role =:= captain
                    end, Team#team.members),
                    case IsCaptain of
                        false ->
                            {error, not_captain};
                        true ->
                            mnesia:write(Team#team{
                                disbanded = true,
                                date_updated = calendar:universal_time()
                            }),

                            case mnesia:read({competition, Team#team.competition_id}) of
                                [Competition] ->
                                    UpdatedTeamIds = lists:delete(TeamId, Competition#competition.team_ids),
                                    mnesia:write(Competition#competition{
                                        team_ids = UpdatedTeamIds,
                                        date_updated = calendar:universal_time()
                                    });
                                [] -> ok
                            end,

                            ok
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_competition_teams(CompetitionId) ->
        Fun = fun() ->
            case mnesia:read({competition, CompetitionId}) of
                [] -> {error, competition_not_found};
                [Competition] ->
                    TeamIds = Competition#competition.team_ids,
                    lists:foldl(fun(TeamId, Acc) ->
                        case mnesia:read({team, TeamId}) of
                            [Team] -> [Team | Acc];
                            [] -> Acc
                        end
                    end, [], TeamIds)
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_user_team_in_competition(CompetitionId, UserId) ->
        Fun = fun() ->
            get_user_team_in_competition_internal(CompetitionId, UserId)
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_user_team_in_competition_internal(CompetitionId, UserId) ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                TeamIds = Competition#competition.team_ids,
                find_user_team(UserId, TeamIds)
        end.

    find_user_team(_UserId, []) ->
        {error, not_in_team};
    find_user_team(UserId, [TeamId | Rest]) ->
        case mnesia:read({team, TeamId}) of
            [Team] ->
                IsMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= UserId end, Team#team.members),
                case IsMember of
                    true -> {ok, Team};
                    false -> find_user_team(UserId, Rest)
                end;
            [] ->
                find_user_team(UserId, Rest)
        end.

    merge_teams(TeamId1, TeamId2, RequesterId, CompetitionId) ->
        Fun = fun() ->
            case {mnesia:read({team, TeamId1}), mnesia:read({team, TeamId2})} of
                {[], _} -> {error, team1_not_found};
                {_, []} -> {error, team2_not_found};
                {[Team1], [Team2]} ->
                    IsCaptain1 = lists:any(fun({MemberId, Role, _}) ->
                        MemberId =:= RequesterId andalso Role =:= captain
                    end, Team1#team.members),

                    case IsCaptain1 of
                        false ->
                            {error, not_captain};
                        true ->
                            case mnesia:read({competition, CompetitionId}) of
                                [] ->
                                    {error, competition_not_found};
                                [Competition] ->
                                    TotalMembers = length(Team1#team.members) + length(Team2#team.members),
                                    case TotalMembers > Competition#competition.team_size_limit of
                                        true ->
                                            {error, merged_team_exceeds_limit};
                                        false ->
                                            Now = calendar:universal_time(),
                                            MergedMembers = Team1#team.members ++ Team2#team.members,
                                            MergedSubmissions = Team1#team.submission_ids ++ Team2#team.submission_ids,

                                            mnesia:write(Team1#team{
                                                members = MergedMembers,
                                                submission_ids = MergedSubmissions,
                                                date_updated = Now
                                            }),

                                            mnesia:write(Team2#team{
                                                disbanded = true,
                                                date_updated = Now
                                            }),

                                            UpdatedTeamIds = lists:delete(TeamId2, Competition#competition.team_ids),
                                            mnesia:write(Competition#competition{
                                                team_ids = UpdatedTeamIds,
                                                date_updated = Now
                                            }),

                                            ok
                                    end
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    submit_entry(CompetitionId, UserId, SubmissionContent, NotebookId, TeamId) ->
        Fun = fun() ->
            case mnesia:read({competition, CompetitionId}) of
                [] ->
                    {error, competition_not_found};
                [Competition] ->
                    case Competition#competition.status of
                        active ->
                            case lists:member(UserId, Competition#competition.participants) of
                                false ->
                                    {error, not_a_participant};
                                true ->
                                    SubmissionId = nanoid:gen(),
                                    Now = calendar:universal_time(),

                                    UserSubmissions = get_user_submission_count_internal(CompetitionId, UserId),
                                    case UserSubmissions >= Competition#competition.submission_count_limit of
                                        true ->
                                            {error, submission_limit_reached};
                                        false ->
                                            ok = content_cache:set({submission, SubmissionId}, SubmissionContent),

                                            IsLate = Now > Competition#competition.end_time,

                                            Submission = #submission{
                                                id = SubmissionId,
                                                competition_id = CompetitionId,
                                                team_id = TeamId,
                                                user_id = UserId,
                                                submission_cid = {pending, SubmissionId},
                                                notebook_id = NotebookId,
                                                submission_number = UserSubmissions + 1,
                                                score_public = undefined,
                                                score_private = undefined,
                                                evaluation_status = pending,
                                                evaluation_cid = undefined,
                                                error_message = undefined,
                                                submission_time = Now,
                                                compute_session_id = undefined,
                                                late_submission = IsLate,
                                                disqualified = false,
                                                disqualification_reason = undefined,
                                                file_size_bytes = byte_size(SubmissionContent),
                                                evaluation_time_seconds = undefined,
                                                metadata = #{}
                                            },

                                            mnesia:write(Submission),

                                            UpdatedSubmissionIds = [SubmissionId | Competition#competition.submission_ids],
                                            mnesia:write(Competition#competition{
                                                submission_ids = UpdatedSubmissionIds,
                                                date_updated = Now
                                            }),

                                            case TeamId of
                                                undefined -> ok;
                                                _ ->
                                                    case mnesia:read({team, TeamId}) of
                                                        [Team] ->
                                                            UpdatedTeamSubmissions = [SubmissionId | Team#team.submission_ids],
                                                            mnesia:write(Team#team{
                                                                submission_ids = UpdatedTeamSubmissions,
                                                                total_submissions = Team#team.total_submissions + 1,
                                                                date_updated = Now
                                                            });
                                                        [] -> ok
                                                    end
                                            end,

                                            {ok, SubmissionId}
                                    end
                            end;
                        _ ->
                            {error, competition_not_active}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, {ok, SubmissionId}} ->
                spawn(fun() ->
                    upload_submission_to_ipfs(SubmissionId)
                end),
                {ok, SubmissionId};
            {atomic, {error, Reason}} ->
                {error, Reason};
            {aborted, Reason} ->
                {error, {transaction_failed, Reason}}
        end.

    get_user_submission_count_internal(CompetitionId, UserId) ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            user_id = UserId,
            _ = '_'
        }),
        length(AllSubmissions).

    upload_submission_to_ipfs(SubmissionId) ->
        try
            SubmissionContent = content_cache:get({submission, SubmissionId}),
            CID = ipfs_media:upload_media(SubmissionContent),

            UpdateF = fun() ->
                case mnesia:read({submission, SubmissionId}) of
                    [Submission] ->
                        mnesia:write(Submission#submission{
                            submission_cid = CID,
                            evaluation_status = queued
                        });
                    [] -> ok
                end
            end,
            mnesia:transaction(UpdateF),

            content_cache:delete({submission, SubmissionId}),

            ok
        catch
            Exception:Error:Stacktrace ->
                error_logger:error_msg(
                    "Exception while uploading submission ~p to IPFS: ~p:~p~n~p",
                    [SubmissionId, Exception, Error, Stacktrace]
                )
        end.

    get_submission_by_id(SubmissionId) ->
        Fun = fun() ->
            case mnesia:read({submission, SubmissionId}) of
                [] -> {error, submission_not_found};
                [Submission] -> Submission
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_user_submissions(CompetitionId, UserId) ->
        Fun = fun() ->
            mnesia:match_object(#submission{
                competition_id = CompetitionId,
                user_id = UserId,
                _ = '_'
            })
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    get_team_submissions(CompetitionId, TeamId) ->
        Fun = fun() ->
            mnesia:match_object(#submission{
                competition_id = CompetitionId,
                team_id = TeamId,
                _ = '_'
            })
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.

        evaluate_submission(SubmissionId, Score) ->
            Fun = fun() ->
                case mnesia:read({submission, SubmissionId}) of
                    [] ->
                        {error, submission_not_found};
                    [Submission] ->
                        Now = calendar:universal_time(),

                        FinalScore = case Submission#submission.late_submission of
                            true ->
                                case mnesia:read({competition, Submission#submission.competition_id}) of
                                    [Competition] ->
                                        Penalty = Competition#competition.late_submission_penalty,
                                        Score * (1.0 - Penalty);
                                    [] -> Score
                                end;
                            false -> Score
                        end,

                        UpdatedSubmission = Submission#submission{
                            score_public = FinalScore,
                            evaluation_status = completed,
                            evaluation_time_seconds = 0
                        },
                        mnesia:write(UpdatedSubmission),

                        case Submission#submission.team_id of
                            undefined -> ok;
                            TeamId ->
                                case mnesia:read({team, TeamId}) of
                                    [Team] ->
                                        CurrentTeamScore = Team#team.team_score,
                                        CurrentBestSubmissionId = Team#team.best_submission_id,
                                        {NewBestScore, NewBestSubmissionId} = case CurrentTeamScore of
                                            undefined ->
                                                {FinalScore, SubmissionId};
                                            _ ->
                                                case FinalScore > CurrentTeamScore of
                                                    true -> {FinalScore, SubmissionId};
                                                    false -> {CurrentTeamScore, CurrentBestSubmissionId}
                                                end
                                        end,
                                        mnesia:write(Team#team{
                                            team_score = NewBestScore,
                                            best_submission_id = NewBestSubmissionId,
                                            date_updated = Now
                                        });
                                    [] -> ok
                                end
                        end,

                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

    disqualify_submission(SubmissionId, AdminId, Reason) ->
        Fun = fun() ->
            case mnesia:read({submission, SubmissionId}) of
                [] ->
                    {error, submission_not_found};
                [Submission] ->
                    case mnesia:read({competition, Submission#submission.competition_id}) of
                        [] ->
                            {error, competition_not_found};
                        [Competition] ->
                            case Competition#competition.creator_id of
                                AdminId ->
                                    mnesia:write(Submission#submission{
                                        disqualified = true,
                                        disqualification_reason = Reason
                                    }),
                                    ok;
                                _ ->
                                    {error, unauthorized}
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_competition_submissions(CompetitionId) ->
        Fun = fun() ->
            mnesia:match_object(#submission{competition_id = CompetitionId, _ = '_'})
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    get_best_submission(CompetitionId, UserId) ->
        Fun = fun() ->
            UserSubmissions = mnesia:match_object(#submission{
                competition_id = CompetitionId,
                user_id = UserId,
                _ = '_'
            }),
            case UserSubmissions of
                [] -> {error, no_submissions};
                _ ->
                    ValidSubmissions = lists:filter(fun(S) ->
                        S#submission.score_public =/= undefined andalso
                        S#submission.disqualified =:= false
                    end, UserSubmissions),
                    case ValidSubmissions of
                        [] -> {error, no_scored_submissions};
                        _ ->
                            BestSubmission = lists:foldl(fun(S, Best) ->
                                case Best of
                                    undefined -> S;
                                    _ ->
                                        if S#submission.score_public > Best#submission.score_public -> S;
                                           true -> Best
                                        end
                                end
                            end, undefined, ValidSubmissions),
                            {ok, BestSubmission}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_leaderboard(CompetitionId) ->
        Fun = fun() ->
            case mnesia:read({leaderboard, CompetitionId}) of
                [] -> {error, leaderboard_not_found};
                [Leaderboard] -> Leaderboard
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    update_leaderboard(CompetitionId) ->
        Fun = fun() ->
            case mnesia:read({competition, CompetitionId}) of
                [] ->
                    {error, competition_not_found};
                [Competition] ->
                    AllSubmissions = mnesia:match_object(#submission{
                        competition_id = CompetitionId,
                        _ = '_'
                    }),

                    ValidSubmissions = lists:filter(fun(S) ->
                        S#submission.score_public =/= undefined andalso
                        S#submission.disqualified =:= false
                    end, AllSubmissions),

                    SortedSubmissions = lists:sort(fun(A, B) ->
                        A#submission.score_public > B#submission.score_public
                    end, ValidSubmissions),

                    SubmissionIds = [S#submission.id || S <- SortedSubmissions],

                    TeamSubmissions = lists:filter(fun(S) ->
                        S#submission.team_id =/= undefined
                    end, SortedSubmissions),
                    TeamSubmissionIds = [S#submission.id || S <- TeamSubmissions],

                    SoloSubmissions = lists:filter(fun(S) ->
                        S#submission.team_id =:= undefined
                    end, SortedSubmissions),
                    SoloSubmissionIds = [S#submission.id || S <- SoloSubmissions],

                    BestScoresPerTeam = calculate_best_scores_per_team(TeamSubmissions),

                    Now = calendar:universal_time(),

                    case mnesia:read({leaderboard, CompetitionId}) of
                        [] ->
                            {error, leaderboard_not_found};
                        [Leaderboard] ->
                            UpdatedLeaderboard = Leaderboard#leaderboard{
                                submission_ids = SubmissionIds,
                                public_submission_ids = SubmissionIds,
                                team_submission_ids = TeamSubmissionIds,
                                solo_submission_ids = SoloSubmissionIds,
                                best_scores_per_team = BestScoresPerTeam,
                                last_updated = Now
                            },
                            mnesia:write(UpdatedLeaderboard),
                            ok
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    calculate_best_scores_per_team(TeamSubmissions) ->
        lists:foldl(fun(Submission, Acc) ->
            TeamId = Submission#submission.team_id,
            Score = Submission#submission.score_public,
            SubmissionId = Submission#submission.id,
            case maps:get(TeamId, Acc, undefined) of
                undefined ->
                    maps:put(TeamId, {SubmissionId, Score}, Acc);
                {_, OldScore} when Score > OldScore ->
                    maps:put(TeamId, {SubmissionId, Score}, Acc);
                _ ->
                    Acc
            end
        end, #{}, TeamSubmissions).

    get_public_leaderboard(CompetitionId) ->
        Fun = fun() ->
            case mnesia:read({leaderboard, CompetitionId}) of
                [] -> {error, leaderboard_not_found};
                [Leaderboard] -> {ok, Leaderboard#leaderboard.public_submission_ids}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_private_leaderboard(CompetitionId) ->
        Fun = fun() ->
            case mnesia:read({leaderboard, CompetitionId}) of
                [] -> {error, leaderboard_not_found};
                [Leaderboard] -> {ok, Leaderboard#leaderboard.private_submission_ids}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_user_rank(CompetitionId, UserId) ->
        Fun = fun() ->
            case mnesia:read({leaderboard, CompetitionId}) of
                [] ->
                    {error, leaderboard_not_found};
                [Leaderboard] ->
                    SubmissionIds = Leaderboard#leaderboard.submission_ids,
                    find_user_rank(UserId, SubmissionIds, 1)
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    find_user_rank(_UserId, [], _Rank) ->
        {error, user_not_ranked};
    find_user_rank(UserId, [SubmissionId | Rest], Rank) ->
        case mnesia:read({submission, SubmissionId}) of
            [Submission] ->
                case Submission#submission.user_id of
                    UserId -> {ok, Rank};
                    _ -> find_user_rank(UserId, Rest, Rank + 1)
                end;
            [] ->
                find_user_rank(UserId, Rest, Rank + 1)
        end.

    get_team_rank(CompetitionId, TeamId) ->
        Fun = fun() ->
            case mnesia:read({leaderboard, CompetitionId}) of
                [] ->
                    {error, leaderboard_not_found};
                [Leaderboard] ->
                    BestScores = Leaderboard#leaderboard.best_scores_per_team,
                    TeamScores = maps:to_list(BestScores),
                    SortedTeams = lists:sort(fun({_, {_, ScoreA}}, {_, {_, ScoreB}}) ->
                        ScoreA > ScoreB
                    end, TeamScores),
                    find_team_rank(TeamId, SortedTeams, 1)
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    find_team_rank(_TeamId, [], _Rank) ->
        {error, team_not_ranked};
    find_team_rank(TeamId, [{TeamId, _} | _], Rank) ->
        {ok, Rank};
    find_team_rank(TeamId, [_ | Rest], Rank) ->
        find_team_rank(TeamId, Rest, Rank + 1).

        add_discussion(CompetitionId, CreatorId, Title, Content) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        DiscussionId = nanoid:gen(),
                        Now = calendar:universal_time(),

                        ok = content_cache:set({discussion, DiscussionId}, Content),

                        Discussion = #discussion_thread{
                            id = DiscussionId,
                            parent_type = competition,
                            parent_id = CompetitionId,
                            creator_id = CreatorId,
                            title = Title,
                            content_cid = {pending, DiscussionId},
                            replies = [],
                            upvotes = [],
                            downvotes = [],
                            pinned = false,
                            solved = false,
                            tags = [],
                            date_created = Now,
                            date_updated = Now,
                            visibility = public,
                            reply_count = 0,
                            view_count = 0,
                            last_activity = Now,
                            accepted_answer_id = undefined,
                            locked = false,
                            report = [],
                            mentioned_user_ids = [],
                            metadata = #{}
                        },

                        mnesia:write(Discussion),

                        UpdatedDiscussions = [DiscussionId | Competition#competition.discussion_cids],
                        mnesia:write(Competition#competition{
                            discussion_cids = UpdatedDiscussions,
                            date_updated = Now
                        }),

                        {ok, DiscussionId}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, {ok, DiscussionId}} ->
                    spawn(fun() ->
                        upload_discussion_to_ipfs(DiscussionId)
                    end),
                    {ok, DiscussionId};
                {atomic, {error, Reason}} ->
                    {error, Reason};
                {aborted, Reason} ->
                    {error, {transaction_failed, Reason}}
            end.

        upload_discussion_to_ipfs(DiscussionId) ->
            try
                Content = content_cache:get({discussion, DiscussionId}),
                CID = ipfs_content:upload_text(Content),

                UpdateF = fun() ->
                    case mnesia:read({discussion_thread, DiscussionId}) of
                        [Discussion] ->
                            mnesia:write(Discussion#discussion_thread{
                                content_cid = CID
                            });
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),

                content_cache:delete({discussion, DiscussionId}),

                ok
            catch
                Exception:Error:Stacktrace ->
                    error_logger:error_msg(
                        "Exception while uploading discussion ~p to IPFS: ~p:~p~n~p",
                        [DiscussionId, Exception, Error, Stacktrace]
                    )
            end.

        get_competition_discussions(CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] -> {error, competition_not_found};
                    [Competition] ->
                        DiscussionIds = Competition#competition.discussion_cids,
                        lists:foldl(fun(DiscussionId, Acc) ->
                            case mnesia:read({discussion_thread, DiscussionId}) of
                                [Discussion] -> [Discussion | Acc];
                                [] -> Acc
                            end
                        end, [], DiscussionIds)
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

            pin_discussion(DiscussionId, AdminId, CompetitionId) ->
                Fun = fun() ->
                    case mnesia:read({competition, CompetitionId}) of
                        [] ->
                            {error, competition_not_found};
                        [Competition] ->
                            case Competition#competition.creator_id of
                                AdminId ->
                                    case mnesia:read({discussion_thread, DiscussionId}) of
                                        [] ->
                                            {error, discussion_not_found};
                                        [Discussion] ->
                                            mnesia:write(Discussion#discussion_thread{
                                                pinned = true,
                                                date_updated = calendar:universal_time()
                                            }),
                                            ok
                                    end;
                                _ ->
                                    {error, unauthorized}
                            end
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

        solve_discussion(DiscussionId, AdminId, CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        case Competition#competition.creator_id of
                            AdminId ->
                                case mnesia:read({discussion_thread, DiscussionId}) of
                                    [] ->
                                        {error, discussion_not_found};
                                    [Discussion] ->
                                        mnesia:write(Discussion#discussion_thread{
                                            solved = true,
                                            date_updated = calendar:universal_time()
                                        }),
                                        ok
                                end;
                            _ ->
                                {error, unauthorized}
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        set_prize_distribution(CompetitionId, PrizeMap) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        mnesia:write(Competition#competition{
                            prize_distribution = PrizeMap,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_prize_distribution(CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] -> {error, competition_not_found};
                    [Competition] -> {ok, Competition#competition.prize_distribution}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        distribute_prizes(CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        case Competition#competition.status of
                            ended ->
                                PrizeDistribution = Competition#competition.prize_distribution,
                                case mnesia:read({leaderboard, CompetitionId}) of
                                    [] ->
                                        {error, leaderboard_not_found};
                                    [Leaderboard] ->
                                        SubmissionIds = Leaderboard#leaderboard.submission_ids,
                                        Winners = assign_prizes(SubmissionIds, PrizeDistribution, 1, []),
                                        {ok, Winners}
                                end;
                            _ ->
                                {error, competition_not_ended}
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        assign_prizes([], _PrizeMap, _Rank, Acc) ->
            lists:reverse(Acc);
        assign_prizes([SubmissionId | Rest], PrizeMap, Rank, Acc) ->
            case maps:get(Rank, PrizeMap, undefined) of
                undefined ->
                    lists:reverse(Acc);
                Prize ->
                    case mnesia:read({submission, SubmissionId}) of
                        [Submission] ->
                            Winner = #{
                                rank => Rank,
                                user_id => Submission#submission.user_id,
                                team_id => Submission#submission.team_id,
                                submission_id => SubmissionId,
                                prize => Prize
                            },
                            assign_prizes(Rest, PrizeMap, Rank + 1, [Winner | Acc]);
                        [] ->
                            assign_prizes(Rest, PrizeMap, Rank + 1, Acc)
                    end
            end.

        set_evaluation_script(CompetitionId, ScriptContent) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        ok = content_cache:set({eval_script, CompetitionId}, ScriptContent),
                        mnesia:write(Competition#competition{
                            evaluation_script_cid = {pending, CompetitionId},
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, ok} ->
                    spawn(fun() ->
                        upload_evaluation_script(CompetitionId)
                    end),
                    ok;
                {atomic, {error, Reason}} ->
                    {error, Reason};
                {aborted, Reason} ->
                    {error, {transaction_failed, Reason}}
            end.

        upload_evaluation_script(CompetitionId) ->
            try
                ScriptContent = content_cache:get({eval_script, CompetitionId}),
                CID = ipfs_content:upload_text(ScriptContent),

                UpdateF = fun() ->
                    case mnesia:read({competition, CompetitionId}) of
                        [Competition] ->
                            mnesia:write(Competition#competition{
                                evaluation_script_cid = CID
                            });
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),

                content_cache:delete({eval_script, CompetitionId}),

                ok
            catch
                Exception:Error:Stacktrace ->
                    error_logger:error_msg(
                        "Exception while uploading evaluation script for ~p: ~p:~p~n~p",
                        [CompetitionId, Exception, Error, Stacktrace]
                    )
            end.

        get_evaluation_script(CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] -> {error, competition_not_found};
                    [Competition] ->
                        case Competition#competition.evaluation_script_cid of
                            undefined -> {error, no_evaluation_script};
                            {pending, _} -> {error, script_not_ready};
                            CID ->
                                try
                                    Script = ipfs_content:get_text_content(CID),
                                    {ok, Script}
                                catch
                                    _:Error -> {error, Error}
                                end
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        run_evaluation(SubmissionId, _EvaluatorId) ->
            Fun = fun() ->
                case mnesia:read({submission, SubmissionId}) of
                    [] ->
                        {error, submission_not_found};
                    [Submission] ->
                        mnesia:write(Submission#submission{
                            evaluation_status = evaluating
                        }),
                        {ok, Submission#submission.competition_id, Submission#submission.submission_cid}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, {ok, CompetitionId, SubmissionCID}} ->
                    spawn(fun() ->
                        execute_evaluation(SubmissionId, CompetitionId, SubmissionCID)
                    end),
                    ok;
                {atomic, {error, Reason}} ->
                    {error, Reason};
                {aborted, Reason} ->
                    {error, {transaction_failed, Reason}}
            end.

            execute_evaluation(SubmissionId, CompetitionId, _SubmissionCID) ->
                try
                    case get_evaluation_script(CompetitionId) of
                        {ok, _Script} ->
                            Score = rand:uniform() * 100,

                            SuccessUpdateF = fun() ->
                                case mnesia:read({submission, SubmissionId}) of
                                    [Submission] ->
                                        mnesia:write(Submission#submission{
                                            score_public = Score,
                                            evaluation_status = completed,
                                            evaluation_time_seconds = 10
                                        });
                                    [] -> ok
                                end
                            end,
                            mnesia:transaction(SuccessUpdateF),

                            update_leaderboard(CompetitionId);
                        {error, _} ->
                            FailUpdateF = fun() ->
                                case mnesia:read({submission, SubmissionId}) of
                                    [Submission] ->
                                        mnesia:write(Submission#submission{
                                            evaluation_status = failed,
                                            error_message = "No evaluation script available"
                                        });
                                    [] -> ok
                                end
                            end,
                            mnesia:transaction(FailUpdateF)
                    end
                catch
                    Exception:Error:Stacktrace ->
                        error_logger:error_msg(
                            "Exception during evaluation of ~p: ~p:~p~n~p",
                            [SubmissionId, Exception, Error, Stacktrace]
                        ),
                        ErrorUpdateF = fun() ->
                            case mnesia:read({submission, SubmissionId}) of
                                [Submission] ->
                                    mnesia:write(Submission#submission{
                                        evaluation_status = failed,
                                        error_message = io_lib:format("~p", [Error])
                                    });
                                [] -> ok
                            end
                        end,
                        mnesia:transaction(ErrorUpdateF)
                end.


        set_compute_quota(CompetitionId, QuotaMap) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        mnesia:write(Competition#competition{
                            compute_quota = QuotaMap,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_compute_quota(CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] -> {error, competition_not_found};
                    [Competition] -> {ok, Competition#competition.compute_quota}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        check_quota_available(CompetitionId, ResourceType) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        Quota = Competition#competition.compute_quota,
                        Available = maps:get(ResourceType, Quota, 0),
                        {ok, Available}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        consume_quota(CompetitionId, ResourceType, Amount) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        Quota = Competition#competition.compute_quota,
                        Available = maps:get(ResourceType, Quota, 0),
                        case Available >= Amount of
                            false ->
                                {error, insufficient_quota};
                            true ->
                                NewQuota = maps:put(ResourceType, Available - Amount, Quota),
                                mnesia:write(Competition#competition{
                                    compute_quota = NewQuota,
                                    date_updated = calendar:universal_time()
                                }),
                                ok
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        feature_competition(CompetitionId, AdminId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        case Competition#competition.creator_id of
                            AdminId ->
                                mnesia:write(Competition#competition{
                                    featured = true,
                                    date_updated = calendar:universal_time()
                                }),
                                ok;
                            _ ->
                                {error, unauthorized}
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        unfeature_competition(CompetitionId, AdminId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        case Competition#competition.creator_id of
                            AdminId ->
                                mnesia:write(Competition#competition{
                                    featured = false,
                                    date_updated = calendar:universal_time()
                                }),
                                ok;
                            _ ->
                                {error, unauthorized}
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        report_competition(ReporterId, CompetitionId, Type, Description) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] -> {error, competition_not_found};
                    [Competition] ->
                        ReportId = nanoid:gen(),
                        Now = calendar:universal_time(),
                        Report = #report{
                            id = ReportId,
                            type = Type,
                            description = Description,
                            reporter = ReporterId,
                            date_created = Now,
                            data = #{competition_id => CompetitionId}
                        },
                        mnesia:write(Report),

                        UpdatedReports = [ReportId | Competition#competition.report],
                        mnesia:write(Competition#competition{report = UpdatedReports}),

                        {ok, ReportId}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        search_competitions(Query) ->
            Fun = fun() ->
                AllCompetitions = mnesia:match_object(#competition{_ = '_'}),
                QueryLower = string:to_lower(Query),
                lists:filter(fun(Competition) ->
                    TitleMatch = string:find(string:to_lower(Competition#competition.title), QueryLower) =/= nomatch,
                    DescMatch = string:find(string:to_lower(Competition#competition.description), QueryLower) =/= nomatch,
                    TagMatch = lists:any(fun(Tag) ->
                        string:find(string:to_lower(Tag), QueryLower) =/= nomatch
                    end, Competition#competition.tags),
                    TitleMatch orelse DescMatch orelse TagMatch
                end, AllCompetitions)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        search_competitions_advanced(SearchParams) ->
            #{
                query := Query,
                status := Status,
                difficulty := Difficulty,
                min_reward := MinReward,
                tags := Tags
            } = SearchParams,

            Fun = fun() ->
                AllCompetitions = mnesia:match_object(#competition{_ = '_'}),
                QueryLower = string:to_lower(Query),

                lists:filter(fun(Competition) ->
                    TitleMatch = case Query of
                        "" -> true;
                        _ -> string:find(string:to_lower(Competition#competition.title), QueryLower) =/= nomatch
                    end,

                    StatusMatch = case Status of
                        any -> true;
                        _ -> Competition#competition.status =:= Status
                    end,

                    DifficultyMatch = case Difficulty of
                        any -> true;
                        _ -> Competition#competition.difficulty_level =:= Difficulty
                    end,

                    RewardMatch = Competition#competition.reward_value >= MinReward,

                    TagMatch = case Tags of
                        [] -> true;
                        _ -> lists:any(fun(Tag) -> lists:member(Tag, Competition#competition.tags) end, Tags)
                    end,

                    TitleMatch andalso StatusMatch andalso DifficultyMatch andalso RewardMatch andalso TagMatch
                end, AllCompetitions)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_trending_competitions(Limit) ->
            Fun = fun() ->
                ActiveCompetitions = mnesia:match_object(#competition{status = active, _ = '_'}),
                Sorted = lists:sort(fun(A, B) ->
                    ScoreA = length(A#competition.participants) + length(A#competition.submission_ids),
                    ScoreB = length(B#competition.participants) + length(B#competition.submission_ids),
                    ScoreA > ScoreB
                end, ActiveCompetitions),
                lists:sublist(Sorted, Limit)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_upcoming_competitions() ->
            Fun = fun() ->
                Now = calendar:universal_time(),
                DraftCompetitions = mnesia:match_object(#competition{status = draft, _ = '_'}),
                lists:filter(fun(Competition) ->
                    Competition#competition.start_time > Now
                end, DraftCompetitions)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_ending_soon_competitions(DaysThreshold) ->
            Fun = fun() ->
                Now = calendar:universal_time(),
                Threshold = add_days_to_datetime(Now, DaysThreshold),
                ActiveCompetitions = mnesia:match_object(#competition{status = active, _ = '_'}),
                lists:filter(fun(Competition) ->
                    Competition#competition.end_time =< Threshold andalso
                    Competition#competition.end_time > Now
                end, ActiveCompetitions)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_competition_stats(CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({competition, CompetitionId}) of
                    [] -> {error, competition_not_found};
                    [Competition] ->
                        {ok, #{
                            total_participants => length(Competition#competition.participants),
                            total_teams => length(Competition#competition.team_ids),
                            total_submissions => length(Competition#competition.submission_ids),
                            total_discussions => length(Competition#competition.discussion_cids),
                            status => Competition#competition.status,
                            days_remaining => calculate_days_remaining(Competition#competition.end_time),
                            featured => Competition#competition.featured,
                            difficulty => Competition#competition.difficulty_level,
                            reward_value => Competition#competition.reward_value
                        }}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        calculate_days_remaining(EndTime) ->
            Now = calendar:universal_time(),
            case EndTime > Now of
                false -> 0;
                true ->
                    NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
                    EndSeconds = calendar:datetime_to_gregorian_seconds(EndTime),
                    DiffSeconds = EndSeconds - NowSeconds,
                    DiffSeconds div 86400
            end.

        get_user_competition_history(UserId) ->
            Fun = fun() ->
                AllCompetitions = mnesia:match_object(#competition{_ = '_'}),
                lists:filter(fun(Competition) ->
                    lists:member(UserId, Competition#competition.participants)
                end, AllCompetitions)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_user_competition_stats(UserId) ->
            Fun = fun() ->
                Competitions = mnesia:match_object(#competition{_ = '_'}),
                ParticipatedCompetitions = lists:filter(fun(C) ->
                    lists:member(UserId, C#competition.participants)
                end, Competitions),

                TotalParticipated = length(ParticipatedCompetitions),

                AllSubmissions = mnesia:match_object(#submission{user_id = UserId, _ = '_'}),
                TotalSubmissions = length(AllSubmissions),

                Wins = lists:foldl(fun(Competition, Acc) ->
                    case mnesia:read({leaderboard, Competition#competition.id}) of
                        [Leaderboard] ->
                            case Leaderboard#leaderboard.submission_ids of
                                [TopSubmissionId | _] ->
                                    case mnesia:read({submission, TopSubmissionId}) of
                                        [TopSubmission] when TopSubmission#submission.user_id =:= UserId ->
                                            Acc + 1;
                                        _ -> Acc
                                    end;
                                _ -> Acc
                            end;
                        [] -> Acc
                    end
                end, 0, ParticipatedCompetitions),

                BestRanks = lists:foldl(fun(Competition, Acc) ->
                    case get_user_rank(Competition#competition.id, UserId) of
                        {ok, Rank} -> [Rank | Acc];
                        _ -> Acc
                    end
                end, [], ParticipatedCompetitions),

                AvgRank = case BestRanks of
                    [] -> undefined;
                    _ -> lists:sum(BestRanks) / length(BestRanks)
                end,

                {ok, #{
                    total_participated => TotalParticipated,
                    total_submissions => TotalSubmissions,
                    wins => Wins,
                    average_rank => AvgRank,
                    best_rank => case BestRanks of [] -> undefined; _ -> lists:min(BestRanks) end
                }}
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        link_notebook_to_submission(SubmissionId, NotebookId) ->
            Fun = fun() ->
                case mnesia:read({submission, SubmissionId}) of
                    [] ->
                        {error, submission_not_found};
                    [Submission] ->
                        mnesia:write(Submission#submission{
                            notebook_id = NotebookId
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_submission_notebook(SubmissionId) ->
            Fun = fun() ->
                case mnesia:read({submission, SubmissionId}) of
                    [] -> {error, submission_not_found};
                    [Submission] ->
                        case Submission#submission.notebook_id of
                            undefined -> {error, no_notebook_linked};
                            NotebookId -> {ok, NotebookId}
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_days_to_datetime(DateTime, Days) ->
            Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
            NewSeconds = Seconds + (Days * 86400),
            calendar:gregorian_seconds_to_datetime(NewSeconds).
