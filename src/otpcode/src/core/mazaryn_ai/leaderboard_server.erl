-module(leaderboard_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-export([
    start_link/0,
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 100, worker_pool = []}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
create_leaderboard(CompetitionId, EvaluationMetric) ->
    gen_server:call({global, ?MODULE}, {create_leaderboard, CompetitionId, EvaluationMetric}).
delete_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {delete_leaderboard, CompetitionId}).
get_leaderboard_by_competition(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_leaderboard_by_competition, CompetitionId}).
update_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {update_leaderboard, CompetitionId}).
update_leaderboard_async(CompetitionId) ->
    gen_server:cast({global, ?MODULE}, {update_leaderboard_async, CompetitionId}).
rebuild_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {rebuild_leaderboard, CompetitionId}).
add_submission_to_leaderboard(CompetitionId, SubmissionId) ->
    gen_server:call({global, ?MODULE}, {add_submission_to_leaderboard, CompetitionId, SubmissionId}).
remove_submission_from_leaderboard(CompetitionId, SubmissionId) ->
    gen_server:call({global, ?MODULE}, {remove_submission_from_leaderboard, CompetitionId, SubmissionId}).
get_public_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_public_leaderboard, CompetitionId}).
get_private_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_private_leaderboard, CompetitionId}).
get_full_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_full_leaderboard, CompetitionId}).
get_top_n(CompetitionId, N) ->
    gen_server:call({global, ?MODULE}, {get_top_n, CompetitionId, N}).
get_top_n_teams(CompetitionId, N) ->
    gen_server:call({global, ?MODULE}, {get_top_n_teams, CompetitionId, N}).
get_top_n_solo(CompetitionId, N) ->
    gen_server:call({global, ?MODULE}, {get_top_n_solo, CompetitionId, N}).
get_user_rank(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_rank, CompetitionId, UserId}).
get_team_rank(CompetitionId, TeamId) ->
    gen_server:call({global, ?MODULE}, {get_team_rank, CompetitionId, TeamId}).
get_submission_rank(CompetitionId, SubmissionId) ->
    gen_server:call({global, ?MODULE}, {get_submission_rank, CompetitionId, SubmissionId}).
get_user_best_submission(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_best_submission, CompetitionId, UserId}).
get_team_best_submission(CompetitionId, TeamId) ->
    gen_server:call({global, ?MODULE}, {get_team_best_submission, CompetitionId, TeamId}).
filter_by_team(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {filter_by_team, CompetitionId}).
filter_by_solo(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {filter_by_solo, CompetitionId}).
calculate_percentile_ranks(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {calculate_percentile_ranks, CompetitionId}).
get_percentile_score(CompetitionId, Percentile) ->
    gen_server:call({global, ?MODULE}, {get_percentile_score, CompetitionId, Percentile}).
get_score_distribution(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_score_distribution, CompetitionId}).
get_leaderboard_statistics(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_leaderboard_statistics, CompetitionId}).
get_rank_changes(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_rank_changes, CompetitionId, UserId}).
get_historical_rankings(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_historical_rankings, CompetitionId}).
freeze_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {freeze_leaderboard, CompetitionId}).
unfreeze_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {unfreeze_leaderboard, CompetitionId}).
is_frozen(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {is_frozen, CompetitionId}).
set_benchmark_score(CompetitionId, BenchmarkScore) ->
    gen_server:call({global, ?MODULE}, {set_benchmark_score, CompetitionId, BenchmarkScore}).
get_benchmark_score(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_benchmark_score, CompetitionId}).
compare_to_benchmark(CompetitionId, SubmissionId) ->
    gen_server:call({global, ?MODULE}, {compare_to_benchmark, CompetitionId, SubmissionId}).
set_prize_cutoffs(CompetitionId, PrizeCutoffs) ->
    gen_server:call({global, ?MODULE}, {set_prize_cutoffs, CompetitionId, PrizeCutoffs}).
get_prize_cutoffs(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_prize_cutoffs, CompetitionId}).
get_prize_tier(CompetitionId, Rank) ->
    gen_server:call({global, ?MODULE}, {get_prize_tier, CompetitionId, Rank}).
export_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {export_leaderboard, CompetitionId}).
export_leaderboard_csv(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {export_leaderboard_csv, CompetitionId}).
get_leaderboard_snapshot(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_leaderboard_snapshot, CompetitionId}).
restore_leaderboard_snapshot(CompetitionId, Snapshot) ->
    gen_server:call({global, ?MODULE}, {restore_leaderboard_snapshot, CompetitionId, Snapshot}).
get_evaluation_history(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_evaluation_history, CompetitionId}).
add_evaluation_to_history(CompetitionId, SubmissionId, Score) ->
    gen_server:call({global, ?MODULE}, {add_evaluation_to_history, CompetitionId, SubmissionId, Score}).
calculate_team_scores(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {calculate_team_scores, CompetitionId}).
update_team_scores(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {update_team_scores, CompetitionId}).
get_score_timeline(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_score_timeline, CompetitionId, UserId}).
get_improvement_rate(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_improvement_rate, CompetitionId, UserId}).
get_near_ranks(CompetitionId, Rank, Range) ->
    gen_server:call({global, ?MODULE}, {get_near_ranks, CompetitionId, Rank, Range}).
get_competitors_around(CompetitionId, SubmissionId, Range) ->
    gen_server:call({global, ?MODULE}, {get_competitors_around, CompetitionId, SubmissionId, Range}).
merge_leaderboards(CompetitionId1, CompetitionId2) ->
    gen_server:call({global, ?MODULE}, {merge_leaderboards, CompetitionId1, CompetitionId2}).
split_leaderboard(CompetitionId, SplitDate) ->
    gen_server:call({global, ?MODULE}, {split_leaderboard, CompetitionId, SplitDate}).
invalidate_cache(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {invalidate_cache, CompetitionId}).
warm_cache(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {warm_cache, CompetitionId}).
init([]) ->
    ?LOG_NOTICE("Leaderboard server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, leaderboard_worker_pool_size, 100),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.
initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].
worker_loop() ->
    receive
        {rebuild_leaderboard, CompetitionId, From} ->
            Result = leaderboarddb:rebuild_leaderboard(CompetitionId),
            gen_server:reply(From, Result),
            worker_loop();
        {update_leaderboard, CompetitionId, From} ->
            Result = leaderboarddb:update_leaderboard(CompetitionId),
            gen_server:reply(From, Result),
            worker_loop();
        {calculate_percentile_ranks, CompetitionId, From} ->
            Result = leaderboarddb:calculate_percentile_ranks(CompetitionId),
            gen_server:reply(From, Result),
            worker_loop();
        {calculate_team_scores, CompetitionId, From} ->
            Result = leaderboarddb:calculate_team_scores(CompetitionId),
            gen_server:reply(From, Result),
            worker_loop();
        {update_team_scores, CompetitionId, From} ->
            Result = leaderboarddb:update_team_scores(CompetitionId),
            gen_server:reply(From, Result),
            worker_loop();
        {add_submission_to_leaderboard, CompetitionId, SubmissionId, From} ->
            Result = leaderboarddb:add_submission_to_leaderboard(CompetitionId, SubmissionId),
            gen_server:reply(From, Result),
            worker_loop();
        {remove_submission_from_leaderboard, CompetitionId, SubmissionId, From} ->
            Result = leaderboarddb:remove_submission_from_leaderboard(CompetitionId, SubmissionId),
            gen_server:reply(From, Result),
            worker_loop();
        {merge_leaderboards, CompetitionId1, CompetitionId2, From} ->
            Result = leaderboarddb:merge_leaderboards(CompetitionId1, CompetitionId2),
            gen_server:reply(From, Result),
            worker_loop();
        {split_leaderboard, CompetitionId, SplitDate, From} ->
            Result = leaderboarddb:split_leaderboard(CompetitionId, SplitDate),
            gen_server:reply(From, Result),
            worker_loop();
        {stop, From} ->
            From ! {stopped, self()};
        Other ->
            ?LOG_WARNING("Worker received unknown message: ~p", [Other]),
            worker_loop()
    end.
handle_call({create_leaderboard, CompetitionId, EvaluationMetric}, _From, State) ->
    Res = leaderboarddb:create_leaderboard(CompetitionId, EvaluationMetric),
    {reply, Res, State};
handle_call({delete_leaderboard, CompetitionId}, _From, State) ->
    Res = leaderboarddb:delete_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({get_leaderboard_by_competition, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_leaderboard_by_competition(CompetitionId),
    {reply, Res, State};
handle_call({update_leaderboard, CompetitionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {update_leaderboard, CompetitionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({rebuild_leaderboard, CompetitionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {rebuild_leaderboard, CompetitionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({add_submission_to_leaderboard, CompetitionId, SubmissionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {add_submission_to_leaderboard, CompetitionId, SubmissionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({remove_submission_from_leaderboard, CompetitionId, SubmissionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {remove_submission_from_leaderboard, CompetitionId, SubmissionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_public_leaderboard, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_public_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({get_private_leaderboard, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_private_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({get_full_leaderboard, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_full_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({get_top_n, CompetitionId, N}, _From, State) ->
    Res = leaderboarddb:get_top_n(CompetitionId, N),
    {reply, Res, State};
handle_call({get_top_n_teams, CompetitionId, N}, _From, State) ->
    Res = leaderboarddb:get_top_n_teams(CompetitionId, N),
    {reply, Res, State};
handle_call({get_top_n_solo, CompetitionId, N}, _From, State) ->
    Res = leaderboarddb:get_top_n_solo(CompetitionId, N),
    {reply, Res, State};
handle_call({get_user_rank, CompetitionId, UserId}, _From, State) ->
    Res = leaderboarddb:get_user_rank(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_team_rank, CompetitionId, TeamId}, _From, State) ->
    Res = leaderboarddb:get_team_rank(CompetitionId, TeamId),
    {reply, Res, State};
handle_call({get_submission_rank, CompetitionId, SubmissionId}, _From, State) ->
    Res = leaderboarddb:get_submission_rank(CompetitionId, SubmissionId),
    {reply, Res, State};
handle_call({get_user_best_submission, CompetitionId, UserId}, _From, State) ->
    Res = leaderboarddb:get_user_best_submission(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_team_best_submission, CompetitionId, TeamId}, _From, State) ->
    Res = leaderboarddb:get_team_best_submission(CompetitionId, TeamId),
    {reply, Res, State};
handle_call({filter_by_team, CompetitionId}, _From, State) ->
    Res = leaderboarddb:filter_by_team(CompetitionId),
    {reply, Res, State};
handle_call({filter_by_solo, CompetitionId}, _From, State) ->
    Res = leaderboarddb:filter_by_solo(CompetitionId),
    {reply, Res, State};
handle_call({calculate_percentile_ranks, CompetitionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {calculate_percentile_ranks, CompetitionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_percentile_score, CompetitionId, Percentile}, _From, State) ->
    Res = leaderboarddb:get_percentile_score(CompetitionId, Percentile),
    {reply, Res, State};
handle_call({get_score_distribution, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_score_distribution(CompetitionId),
    {reply, Res, State};
handle_call({get_leaderboard_statistics, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_leaderboard_statistics(CompetitionId),
    {reply, Res, State};
handle_call({get_rank_changes, CompetitionId, UserId}, _From, State) ->
    Res = leaderboarddb:get_rank_changes(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_historical_rankings, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_historical_rankings(CompetitionId),
    {reply, Res, State};
handle_call({freeze_leaderboard, CompetitionId}, _From, State) ->
    Res = leaderboarddb:freeze_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({unfreeze_leaderboard, CompetitionId}, _From, State) ->
    Res = leaderboarddb:unfreeze_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({is_frozen, CompetitionId}, _From, State) ->
    Res = leaderboarddb:is_frozen(CompetitionId),
    {reply, Res, State};
handle_call({set_benchmark_score, CompetitionId, BenchmarkScore}, _From, State) ->
    Res = leaderboarddb:set_benchmark_score(CompetitionId, BenchmarkScore),
    {reply, Res, State};
handle_call({get_benchmark_score, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_benchmark_score(CompetitionId),
    {reply, Res, State};
handle_call({compare_to_benchmark, CompetitionId, SubmissionId}, _From, State) ->
    Res = leaderboarddb:compare_to_benchmark(CompetitionId, SubmissionId),
    {reply, Res, State};
handle_call({set_prize_cutoffs, CompetitionId, PrizeCutoffs}, _From, State) ->
    Res = leaderboarddb:set_prize_cutoffs(CompetitionId, PrizeCutoffs),
    {reply, Res, State};
handle_call({get_prize_cutoffs, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_prize_cutoffs(CompetitionId),
    {reply, Res, State};
handle_call({get_prize_tier, CompetitionId, Rank}, _From, State) ->
    Res = leaderboarddb:get_prize_tier(CompetitionId, Rank),
    {reply, Res, State};
handle_call({export_leaderboard, CompetitionId}, _From, State) ->
    Res = leaderboarddb:export_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({export_leaderboard_csv, CompetitionId}, _From, State) ->
    Res = leaderboarddb:export_leaderboard_csv(CompetitionId),
    {reply, Res, State};
handle_call({get_leaderboard_snapshot, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_leaderboard_snapshot(CompetitionId),
    {reply, Res, State};
handle_call({restore_leaderboard_snapshot, CompetitionId, Snapshot}, _From, State) ->
    Res = leaderboarddb:restore_leaderboard_snapshot(CompetitionId, Snapshot),
    {reply, Res, State};
handle_call({get_evaluation_history, CompetitionId}, _From, State) ->
    Res = leaderboarddb:get_evaluation_history(CompetitionId),
    {reply, Res, State};
handle_call({add_evaluation_to_history, CompetitionId, SubmissionId, Score}, _From, State) ->
    Res = leaderboarddb:add_evaluation_to_history(CompetitionId, SubmissionId, Score),
    {reply, Res, State};
handle_call({calculate_team_scores, CompetitionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {calculate_team_scores, CompetitionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({update_team_scores, CompetitionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {update_team_scores, CompetitionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_score_timeline, CompetitionId, UserId}, _From, State) ->
    Res = leaderboarddb:get_score_timeline(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_improvement_rate, CompetitionId, UserId}, _From, State) ->
    Res = leaderboarddb:get_improvement_rate(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_near_ranks, CompetitionId, Rank, Range}, _From, State) ->
    Res = leaderboarddb:get_near_ranks(CompetitionId, Rank, Range),
    {reply, Res, State};
handle_call({get_competitors_around, CompetitionId, SubmissionId, Range}, _From, State) ->
    Res = leaderboarddb:get_competitors_around(CompetitionId, SubmissionId, Range),
    {reply, Res, State};
handle_call({merge_leaderboards, CompetitionId1, CompetitionId2}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {merge_leaderboards, CompetitionId1, CompetitionId2, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({split_leaderboard, CompetitionId, SplitDate}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {split_leaderboard, CompetitionId, SplitDate, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({invalidate_cache, CompetitionId}, _From, State) ->
    Res = leaderboarddb:invalidate_cache(CompetitionId),
    {reply, Res, State};
handle_call({warm_cache, CompetitionId}, _From, State) ->
    Res = leaderboarddb:warm_cache(CompetitionId),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.
handle_cast({update_leaderboard_async, CompetitionId}, State) ->
    spawn(fun() ->
        leaderboarddb:update_leaderboard_async(CompetitionId)
    end),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info({'EXIT', Pid, Reason}, State = #state{worker_pool = Workers}) ->
    case lists:member(Pid, Workers) of
        true ->
            ?LOG_WARNING("Worker ~p crashed with reason: ~p. Replacing.", [Pid, Reason]),
            NewWorker = spawn_link(fun() -> worker_loop() end),
            NewWorkers = lists:delete(Pid, Workers) ++ [NewWorker],
            {noreply, State#state{worker_pool = NewWorkers}};
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, #state{worker_pool = Workers}) ->
    [Worker ! {stop, self()} || Worker <- Workers],
    [receive {stopped, _W} -> ok after 1000 -> ok end || _ <- Workers],
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
