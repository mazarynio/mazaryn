-module(leaderboarddb).
-author("Zaryn Technologies").

-export([
    create_leaderboard/2,
    delete_leaderboard/1,
    get_leaderboard_by_competition/1,

    update_leaderboard/1,
    update_leaderboard_async/1,
    rebuild_leaderboard/1,

    add_submission_to_leaderboard/2,
    remove_submission_from_leaderboard/2,

    get_public_leaderboard/1,
    get_private_leaderboard/1,
    get_full_leaderboard/1,

    get_top_n/2,
    get_top_n_teams/2,
    get_top_n_solo/2,

    get_user_rank/2,
    get_team_rank/2,
    get_submission_rank/2,

    get_user_best_submission/2,
    get_team_best_submission/2,

    filter_by_team/1,
    filter_by_solo/1,

    calculate_percentile_ranks/1,
    get_percentile_score/2,
    get_score_distribution/1,

    get_leaderboard_statistics/1,
    get_rank_changes/2,
    get_historical_rankings/1,

    freeze_leaderboard/1,
    unfreeze_leaderboard/1,
    is_frozen/1,

    set_benchmark_score/2,
    get_benchmark_score/1,
    compare_to_benchmark/2,

    set_prize_cutoffs/2,
    get_prize_cutoffs/1,
    get_prize_tier/2,

    export_leaderboard/1,
    export_leaderboard_csv/1,

    get_leaderboard_snapshot/1,
    restore_leaderboard_snapshot/2,

    get_evaluation_history/1,
    add_evaluation_to_history/3,

    calculate_team_scores/1,
    update_team_scores/1,

    get_score_timeline/2,
    get_improvement_rate/2,

    get_near_ranks/3,
    get_competitors_around/3,

    merge_leaderboards/2,
    split_leaderboard/2,

    invalidate_cache/1,
    warm_cache/1
]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(DEFAULT_TOP_N, 100).
-define(CACHE_TTL, 300).
-define(MAX_HISTORY, 1000).

create_leaderboard(CompetitionId, EvaluationMetric) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [_Competition] ->
                Now = calendar:universal_time(),

                Leaderboard = #leaderboard{
                    competition_id = CompetitionId,
                    submission_ids = [],
                    evaluation_metric = EvaluationMetric,
                    last_updated = Now,
                    data = #{
                        frozen => false,
                        snapshot_history => []
                    },
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
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

delete_leaderboard(CompetitionId) ->
    Fun = fun() ->
        mnesia:delete({leaderboard, CompetitionId})
    end,

    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_leaderboard_by_competition(CompetitionId) ->
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
        case mnesia:read({leaderboard, CompetitionId}) of
            [] ->
                {error, leaderboard_not_found};
            [Leaderboard] ->
                case is_frozen_internal(Leaderboard) of
                    true ->
                        {error, leaderboard_frozen};
                    false ->
                        UpdatedLeaderboard = recalculate_leaderboard(CompetitionId, Leaderboard),
                        mnesia:write(UpdatedLeaderboard),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_leaderboard_async(CompetitionId) ->
    spawn(fun() ->
        update_leaderboard(CompetitionId)
    end),
    ok.

rebuild_leaderboard(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] ->
                {error, leaderboard_not_found};
            [Leaderboard] ->
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

                BestScoresPerTeam = calculate_best_scores_per_team_internal(TeamSubmissions),
                PercentileData = calculate_percentiles(SortedSubmissions),

                Now = calendar:universal_time(),

                UpdatedLeaderboard = Leaderboard#leaderboard{
                    submission_ids = SubmissionIds,
                    public_submission_ids = SubmissionIds,
                    team_submission_ids = TeamSubmissionIds,
                    solo_submission_ids = SoloSubmissionIds,
                    best_scores_per_team = BestScoresPerTeam,
                    percentile_data = PercentileData,
                    last_updated = Now
                },

                mnesia:write(UpdatedLeaderboard),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

recalculate_leaderboard(CompetitionId, Leaderboard) ->
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

    BestScoresPerTeam = calculate_best_scores_per_team_internal(TeamSubmissions),
    PercentileData = calculate_percentiles(SortedSubmissions),

    Now = calendar:universal_time(),

    Leaderboard#leaderboard{
        submission_ids = SubmissionIds,
        public_submission_ids = SubmissionIds,
        team_submission_ids = TeamSubmissionIds,
        solo_submission_ids = SoloSubmissionIds,
        best_scores_per_team = BestScoresPerTeam,
        percentile_data = PercentileData,
        last_updated = Now
    }.

add_submission_to_leaderboard(CompetitionId, SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] ->
                {error, leaderboard_not_found};
            [Leaderboard] ->
                case mnesia:read({submission, SubmissionId}) of
                    [] ->
                        {error, submission_not_found};
                    [Submission] ->
                        case Submission#submission.score_public of
                            undefined ->
                                {error, submission_not_scored};
                            _Score ->
                                UpdatedLeaderboard = recalculate_leaderboard(CompetitionId, Leaderboard),
                                mnesia:write(UpdatedLeaderboard),
                                ok
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_submission_from_leaderboard(CompetitionId, SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] ->
                {error, leaderboard_not_found};
            [Leaderboard] ->
                UpdatedSubmissionIds = lists:delete(SubmissionId, Leaderboard#leaderboard.submission_ids),
                UpdatedPublicIds = lists:delete(SubmissionId, Leaderboard#leaderboard.public_submission_ids),
                UpdatedTeamIds = lists:delete(SubmissionId, Leaderboard#leaderboard.team_submission_ids),
                UpdatedSoloIds = lists:delete(SubmissionId, Leaderboard#leaderboard.solo_submission_ids),

                UpdatedLeaderboard = Leaderboard#leaderboard{
                    submission_ids = UpdatedSubmissionIds,
                    public_submission_ids = UpdatedPublicIds,
                    team_submission_ids = UpdatedTeamIds,
                    solo_submission_ids = UpdatedSoloIds,
                    last_updated = calendar:universal_time()
                },

                mnesia:write(UpdatedLeaderboard),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_public_leaderboard(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = Leaderboard#leaderboard.public_submission_ids,
                Submissions = lists:foldl(fun(Id, Acc) ->
                    case mnesia:read({submission, Id}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], SubmissionIds),
                {ok, lists:reverse(Submissions)}
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
            [Leaderboard] ->
                SubmissionIds = Leaderboard#leaderboard.private_submission_ids,
                case SubmissionIds of
                    [] ->
                        {ok, []};
                    _ ->
                        Submissions = lists:foldl(fun(Id, Acc) ->
                            case mnesia:read({submission, Id}) of
                                [Sub] -> [Sub | Acc];
                                [] -> Acc
                            end
                        end, [], SubmissionIds),
                        {ok, lists:reverse(Submissions)}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_full_leaderboard(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = Leaderboard#leaderboard.submission_ids,
                Submissions = lists:foldl(fun(Id, Acc) ->
                    case mnesia:read({submission, Id}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], SubmissionIds),
                {ok, lists:reverse(Submissions)}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_top_n(CompetitionId, N) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = lists:sublist(Leaderboard#leaderboard.submission_ids, N),
                Submissions = lists:foldl(fun(Id, Acc) ->
                    case mnesia:read({submission, Id}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], SubmissionIds),
                {ok, lists:reverse(Submissions)}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_top_n_teams(CompetitionId, N) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                BestScores = Leaderboard#leaderboard.best_scores_per_team,
                TeamScores = maps:to_list(BestScores),
                SortedTeams = lists:sort(fun({_, {_, ScoreA}}, {_, {_, ScoreB}}) ->
                    ScoreA > ScoreB
                end, TeamScores),
                TopTeams = lists:sublist(SortedTeams, N),
                {ok, TopTeams}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_top_n_solo(CompetitionId, N) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = lists:sublist(Leaderboard#leaderboard.solo_submission_ids, N),
                Submissions = lists:foldl(fun(Id, Acc) ->
                    case mnesia:read({submission, Id}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], SubmissionIds),
                {ok, lists:reverse(Submissions)}
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
                find_user_rank_internal(UserId, SubmissionIds, 1)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

find_user_rank_internal(_UserId, [], _Rank) ->
    {error, user_not_ranked};
find_user_rank_internal(UserId, [SubmissionId | Rest], Rank) ->
    case mnesia:read({submission, SubmissionId}) of
        [Submission] ->
            case Submission#submission.user_id of
                UserId -> {ok, Rank, Submission#submission.score_public};
                _ -> find_user_rank_internal(UserId, Rest, Rank + 1)
            end;
        [] ->
            find_user_rank_internal(UserId, Rest, Rank + 1)
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
                find_team_rank_internal(TeamId, SortedTeams, 1)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

find_team_rank_internal(_TeamId, [], _Rank) ->
    {error, team_not_ranked};
find_team_rank_internal(TeamId, [{TeamId, {_SubmissionId, Score}} | _], Rank) ->
    {ok, Rank, Score};
find_team_rank_internal(TeamId, [_ | Rest], Rank) ->
    find_team_rank_internal(TeamId, Rest, Rank + 1).

get_submission_rank(CompetitionId, SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] ->
                {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = Leaderboard#leaderboard.submission_ids,
                case lists:member(SubmissionId, SubmissionIds) of
                    false ->
                        {error, submission_not_in_leaderboard};
                    true ->
                        Rank = find_submission_rank_internal(SubmissionId, SubmissionIds, 1),
                        case mnesia:read({submission, SubmissionId}) of
                            [Submission] ->
                                {ok, Rank, Submission#submission.score_public};
                            [] ->
                                {error, submission_not_found}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

find_submission_rank_internal(SubmissionId, [SubmissionId | _], Rank) ->
    Rank;
find_submission_rank_internal(SubmissionId, [_ | Rest], Rank) ->
    find_submission_rank_internal(SubmissionId, Rest, Rank + 1);
find_submission_rank_internal(_, [], _) ->
    undefined.

get_user_best_submission(CompetitionId, UserId) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            user_id = UserId,
            _ = '_'
        }),

        ValidSubmissions = lists:filter(fun(S) ->
            S#submission.score_public =/= undefined andalso
            S#submission.disqualified =:= false
        end, AllSubmissions),

        case ValidSubmissions of
            [] -> {error, no_valid_submissions};
            _ ->
                Best = lists:foldl(fun(S, Acc) ->
                    case Acc of
                        undefined -> S;
                        _ ->
                            if S#submission.score_public > Acc#submission.score_public -> S;
                               true -> Acc
                            end
                    end
                end, undefined, ValidSubmissions),
                {ok, Best}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_best_submission(CompetitionId, TeamId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                BestScores = Leaderboard#leaderboard.best_scores_per_team,
                case maps:get(TeamId, BestScores, undefined) of
                    undefined ->
                        {error, team_not_found};
                    {SubmissionId, Score} ->
                        case mnesia:read({submission, SubmissionId}) of
                            [Submission] -> {ok, Submission, Score};
                            [] -> {error, submission_not_found}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

filter_by_team(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = Leaderboard#leaderboard.team_submission_ids,
                Submissions = lists:foldl(fun(Id, Acc) ->
                    case mnesia:read({submission, Id}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], SubmissionIds),
                {ok, lists:reverse(Submissions)}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

filter_by_solo(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = Leaderboard#leaderboard.solo_submission_ids,
                Submissions = lists:foldl(fun(Id, Acc) ->
                    case mnesia:read({submission, Id}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], SubmissionIds),
                {ok, lists:reverse(Submissions)}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

calculate_percentile_ranks(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                AllSubmissions = mnesia:match_object(#submission{
                    competition_id = CompetitionId,
                    _ = '_'
                }),

                ValidSubmissions = lists:filter(fun(S) ->
                    S#submission.score_public =/= undefined andalso
                    S#submission.disqualified =:= false
                end, AllSubmissions),

                PercentileData = calculate_percentiles(ValidSubmissions),

                UpdatedLeaderboard = Leaderboard#leaderboard{
                    percentile_data = PercentileData,
                    last_updated = calendar:universal_time()
                },
                mnesia:write(UpdatedLeaderboard),
                {ok, PercentileData}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

calculate_percentiles(Submissions) ->
    Sorted = lists:sort(fun(A, B) ->
        A#submission.score_public > B#submission.score_public
    end, Submissions),

    Total = length(Sorted),

    Percentiles = [10, 25, 50, 75, 90, 95, 99],

    lists:foldl(fun(Percentile, Acc) ->
        Index = max(1, round((Percentile / 100.0) * Total)),
        Submission = lists:nth(Index, Sorted),
        maps:put(Percentile, Submission#submission.score_public, Acc)
    end, #{}, Percentiles).

get_percentile_score(CompetitionId, Percentile) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                PercentileData = Leaderboard#leaderboard.percentile_data,
                case maps:get(Percentile, PercentileData, undefined) of
                    undefined -> {error, percentile_not_calculated};
                    Score -> {ok, Score}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_score_distribution(CompetitionId) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            _ = '_'
        }),

        ValidSubmissions = lists:filter(fun(S) ->
            S#submission.score_public =/= undefined andalso
            S#submission.disqualified =:= false
        end, AllSubmissions),

        Scores = [S#submission.score_public || S <- ValidSubmissions],

        case Scores of
            [] -> {ok, #{count => 0}};
            _ ->
                Min = lists:min(Scores),
                Max = lists:max(Scores),
                Mean = lists:sum(Scores) / length(Scores),

                SortedScores = lists:sort(Scores),
                Median = case length(SortedScores) rem 2 of
                    0 ->
                        Mid = length(SortedScores) div 2,
                        (lists:nth(Mid, SortedScores) + lists:nth(Mid + 1, SortedScores)) / 2;
                    _ ->
                        lists:nth((length(SortedScores) + 1) div 2, SortedScores)
                end,

                Variance = lists:sum([math:pow(S - Mean, 2) || S <- Scores]) / length(Scores),
                StdDev = math:sqrt(Variance),

                Distribution = #{
                    count => length(Scores),
                    min => Min,
                    max => Max,
                    mean => Mean,
                    median => Median,
                    std_dev => StdDev,
                    range => Max - Min
                },

                {ok, Distribution}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_leaderboard_statistics(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                TotalSubmissions = length(Leaderboard#leaderboard.submission_ids),
                TeamSubmissions = length(Leaderboard#leaderboard.team_submission_ids),
                SoloSubmissions = length(Leaderboard#leaderboard.solo_submission_ids),
                TotalTeams = maps:size(Leaderboard#leaderboard.best_scores_per_team),

                Stats = #{
                    total_submissions => TotalSubmissions,
                    team_submissions => TeamSubmissions,
                    solo_submissions => SoloSubmissions,
                    total_teams => TotalTeams,
                    last_updated => Leaderboard#leaderboard.last_updated,
                    evaluation_metric => Leaderboard#leaderboard.evaluation_metric,
                    benchmark_score => Leaderboard#leaderboard.benchmark_score
                },

                {ok, Stats}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_rank_changes(CompetitionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                History = Leaderboard#leaderboard.evaluation_history,
                UserHistory = lists:filter(fun({_Time, SubId, _Score}) ->
                    case mnesia:read({submission, SubId}) of
                        [Sub] -> Sub#submission.user_id =:= UserId;
                        [] -> false
                    end
                end, History),

                {ok, UserHistory}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_historical_rankings(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] -> {ok, Leaderboard#leaderboard.evaluation_history}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

freeze_leaderboard(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                DataMap = Leaderboard#leaderboard.data,
                UpdatedDataMap = maps:put(frozen, true, DataMap),
                mnesia:write(Leaderboard#leaderboard{data = UpdatedDataMap}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unfreeze_leaderboard(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                DataMap = Leaderboard#leaderboard.data,
                UpdatedDataMap = maps:put(frozen, false, DataMap),
                mnesia:write(Leaderboard#leaderboard{data = UpdatedDataMap}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

is_frozen(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> false;
            [Leaderboard] -> is_frozen_internal(Leaderboard)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

is_frozen_internal(Leaderboard) ->
    DataMap = Leaderboard#leaderboard.data,
    maps:get(frozen, DataMap, false).

set_benchmark_score(CompetitionId, BenchmarkScore) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                mnesia:write(Leaderboard#leaderboard{benchmark_score = BenchmarkScore}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_benchmark_score(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] -> {ok, Leaderboard#leaderboard.benchmark_score}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

compare_to_benchmark(CompetitionId, SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                BenchmarkScore = Leaderboard#leaderboard.benchmark_score,
                case mnesia:read({submission, SubmissionId}) of
                    [] -> {error, submission_not_found};
                    [Submission] ->
                        case Submission#submission.score_public of
                            undefined -> {error, submission_not_scored};
                            Score ->
                                Diff = Score - BenchmarkScore,
                                PercentImprovement = (Diff / BenchmarkScore) * 100,
                                {ok, #{
                                    submission_score => Score,
                                    benchmark_score => BenchmarkScore,
                                    difference => Diff,
                                    percent_improvement => PercentImprovement,
                                    beats_benchmark => Score > BenchmarkScore
                                }}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_prize_cutoffs(CompetitionId, PrizeCutoffs) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                mnesia:write(Leaderboard#leaderboard{prize_cutoffs = PrizeCutoffs}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_prize_cutoffs(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] -> {ok, Leaderboard#leaderboard.prize_cutoffs}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_prize_tier(CompetitionId, Rank) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                PrizeCutoffs = Leaderboard#leaderboard.prize_cutoffs,
                find_prize_tier(Rank, maps:to_list(PrizeCutoffs))
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

find_prize_tier(_Rank, []) ->
    {ok, no_prize};
find_prize_tier(Rank, [{RankRange, Prize} | Rest]) ->
    case is_in_range(Rank, RankRange) of
        true -> {ok, Prize};
        false -> find_prize_tier(Rank, Rest)
    end.

is_in_range(Rank, {Min, Max}) ->
    Rank >= Min andalso Rank =< Max;
is_in_range(Rank, SingleRank) when is_integer(SingleRank) ->
    Rank =:= SingleRank.

export_leaderboard(CompetitionId) ->
    case get_full_leaderboard(CompetitionId) of
        {ok, Submissions} ->
            LeaderboardData = lists:map(fun(Sub) ->
                #{
                    rank => find_submission_rank_internal(Sub#submission.id,
                        [S#submission.id || S <- Submissions], 1),
                    submission_id => Sub#submission.id,
                    user_id => Sub#submission.user_id,
                    team_id => Sub#submission.team_id,
                    score => Sub#submission.score_public,
                    submission_time => Sub#submission.submission_time
                }
            end, Submissions),
            {ok, LeaderboardData};
        Error -> Error
    end.

export_leaderboard_csv(CompetitionId) ->
    case export_leaderboard(CompetitionId) of
        {ok, Data} ->
            Header = "Rank,SubmissionID,UserID,TeamID,Score,SubmissionTime\n",
            Rows = lists:map(fun(Entry) ->
                lists:flatten(io_lib:format("~p,~s,~s,~s,~p,~p\n", [
                    maps:get(rank, Entry),
                    maps:get(submission_id, Entry),
                    maps:get(user_id, Entry),
                    format_optional(maps:get(team_id, Entry)),
                    maps:get(score, Entry),
                    format_datetime(maps:get(submission_time, Entry))
                ]))
            end, Data),
            CSV = Header ++ lists:flatten(Rows),
            {ok, CSV};
        Error -> Error
    end.

format_optional(undefined) -> "N/A";
format_optional(Value) -> Value.

format_datetime({{Y, M, D}, {H, Min, S}}) ->
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Y, M, D, H, Min, S])).

get_leaderboard_snapshot(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                Snapshot = #{
                    submission_ids => Leaderboard#leaderboard.submission_ids,
                    public_submission_ids => Leaderboard#leaderboard.public_submission_ids,
                    team_submission_ids => Leaderboard#leaderboard.team_submission_ids,
                    solo_submission_ids => Leaderboard#leaderboard.solo_submission_ids,
                    best_scores_per_team => Leaderboard#leaderboard.best_scores_per_team,
                    timestamp => calendar:universal_time()
                },

                DataMap = Leaderboard#leaderboard.data,
                SnapshotHistory = maps:get(snapshot_history, DataMap, []),
                UpdatedHistory = [Snapshot | lists:sublist(SnapshotHistory, 99)],
                UpdatedDataMap = maps:put(snapshot_history, UpdatedHistory, DataMap),

                mnesia:write(Leaderboard#leaderboard{data = UpdatedDataMap}),
                {ok, Snapshot}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

restore_leaderboard_snapshot(CompetitionId, Snapshot) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                RestoredLeaderboard = Leaderboard#leaderboard{
                    submission_ids = maps:get(submission_ids, Snapshot),
                    public_submission_ids = maps:get(public_submission_ids, Snapshot),
                    team_submission_ids = maps:get(team_submission_ids, Snapshot),
                    solo_submission_ids = maps:get(solo_submission_ids, Snapshot),
                    best_scores_per_team = maps:get(best_scores_per_team, Snapshot),
                    last_updated = calendar:universal_time()
                },
                mnesia:write(RestoredLeaderboard),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_evaluation_history(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] -> {ok, Leaderboard#leaderboard.evaluation_history}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_evaluation_to_history(CompetitionId, SubmissionId, Score) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                Now = calendar:universal_time(),
                Entry = {Now, SubmissionId, Score},

                History = Leaderboard#leaderboard.evaluation_history,
                UpdatedHistory = [Entry | lists:sublist(History, ?MAX_HISTORY - 1)],

                mnesia:write(Leaderboard#leaderboard{evaluation_history = UpdatedHistory}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

calculate_team_scores(CompetitionId) ->
    Fun = fun() ->
        TeamSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            team_id = '_',
            _ = '_'
        }),

        ValidTeamSubmissions = lists:filter(fun(S) ->
            S#submission.team_id =/= undefined andalso
            S#submission.score_public =/= undefined andalso
            S#submission.disqualified =:= false
        end, TeamSubmissions),

        BestScores = calculate_best_scores_per_team_internal(ValidTeamSubmissions),
        {ok, BestScores}
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

calculate_best_scores_per_team_internal(TeamSubmissions) ->
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

update_team_scores(CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                {ok, BestScores} = calculate_team_scores(CompetitionId),
                mnesia:write(Leaderboard#leaderboard{best_scores_per_team = BestScores}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_score_timeline(CompetitionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                History = Leaderboard#leaderboard.evaluation_history,
                UserTimeline = lists:filter(fun({_Time, SubId, _Score}) ->
                    case mnesia:read({submission, SubId}) of
                        [Sub] -> Sub#submission.user_id =:= UserId;
                        [] -> false
                    end
                end, History),

                SortedTimeline = lists:sort(fun({TimeA, _, _}, {TimeB, _, _}) ->
                    TimeA < TimeB
                end, UserTimeline),

                {ok, SortedTimeline}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_improvement_rate(CompetitionId, UserId) ->
    case get_score_timeline(CompetitionId, UserId) of
        {ok, Timeline} when length(Timeline) >= 2 ->
            [{_, _, FirstScore} | _] = Timeline,
            {_, _, LastScore} = lists:last(Timeline),

            Improvement = LastScore - FirstScore,
            SubmissionCount = length(Timeline),
            Rate = Improvement / SubmissionCount,

            {ok, #{
                first_score => FirstScore,
                last_score => LastScore,
                total_improvement => Improvement,
                submission_count => SubmissionCount,
                improvement_rate => Rate
            }};
        {ok, _} -> {error, insufficient_data};
        Error -> Error
    end.

get_near_ranks(CompetitionId, Rank, Range) ->
    Fun = fun() ->
        case mnesia:read({leaderboard, CompetitionId}) of
            [] -> {error, leaderboard_not_found};
            [Leaderboard] ->
                SubmissionIds = Leaderboard#leaderboard.submission_ids,
                StartIdx = max(1, Rank - Range),
                EndIdx = min(length(SubmissionIds), Rank + Range),

                NearIds = lists:sublist(SubmissionIds, StartIdx, EndIdx - StartIdx + 1),

                Submissions = lists:foldl(fun(Id, Acc) ->
                    case mnesia:read({submission, Id}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], NearIds),

                {ok, lists:reverse(Submissions)}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_competitors_around(CompetitionId, SubmissionId, Range) ->
    case get_submission_rank(CompetitionId, SubmissionId) of
        {ok, Rank, _Score} ->
            get_near_ranks(CompetitionId, Rank, Range);
        Error -> Error
    end.

merge_leaderboards(CompetitionId1, CompetitionId2) ->
    Fun = fun() ->
        case {mnesia:read({leaderboard, CompetitionId1}),
              mnesia:read({leaderboard, CompetitionId2})} of
            {[], _} -> {error, leaderboard1_not_found};
            {_, []} -> {error, leaderboard2_not_found};
            {[Board1], [Board2]} ->
                MergedIds = lists:usort(Board1#leaderboard.submission_ids ++
                                       Board2#leaderboard.submission_ids),

                UpdatedBoard1 = Board1#leaderboard{
                    submission_ids = MergedIds,
                    last_updated = calendar:universal_time()
                },

                mnesia:write(UpdatedBoard1),
                UpdatedBoard1 = recalculate_leaderboard(CompetitionId1, UpdatedBoard1),
                mnesia:write(UpdatedBoard1),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    split_leaderboard(CompetitionId, SplitDate) ->
        Fun = fun() ->
            case mnesia:read({leaderboard, CompetitionId}) of
                [] -> {error, leaderboard_not_found};
                [Leaderboard] ->
                    AllIds = Leaderboard#leaderboard.submission_ids,

                    {BeforeIds, AfterIds} = lists:partition(fun(SubId) ->
                        case mnesia:read({submission, SubId}) of
                            [Sub] -> Sub#submission.submission_time < SplitDate;
                            [] -> false
                        end
                    end, AllIds),

                    {ok, #{'before' => BeforeIds, 'after' => AfterIds}}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    invalidate_cache(_CompetitionId) ->
        ok.

warm_cache(CompetitionId) ->
    spawn(fun() ->
        get_public_leaderboard(CompetitionId),
        get_top_n(CompetitionId, 100),
        get_leaderboard_statistics(CompetitionId)
    end),
    ok.
