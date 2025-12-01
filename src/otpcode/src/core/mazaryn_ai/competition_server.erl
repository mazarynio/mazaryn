-module(competition_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-export([
    start_link/0,
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
    get_submission_notebook/1,
    download_competition_datasets/2,
    download_competition_datasets/3,
    bulk_download_competition_datasets/2,
    get_competition_download_progress/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 100, worker_pool = []}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
create_competition(CreatorId, Title, Description, DatasetIds, StartTime, EndTime, RewardType, RewardValue, Rules, EvaluationMetric, SubmissionLimit, TeamSizeLimit) ->
    gen_server:call({global, ?MODULE}, {create_competition, CreatorId, Title, Description, DatasetIds, StartTime, EndTime, RewardType, RewardValue, Rules, EvaluationMetric, SubmissionLimit, TeamSizeLimit}).
update_competition(CompetitionId, CreatorId, NewTitle, NewDescription, NewDatasetIds, NewStartTime, NewEndTime, NewRewardType, NewRewardValue, NewRules, NewEvaluationMetric, NewSubmissionLimit, NewTeamSizeLimit) ->
    gen_server:call({global, ?MODULE}, {update_competition, CompetitionId, CreatorId, NewTitle, NewDescription, NewDatasetIds, NewStartTime, NewEndTime, NewRewardType, NewRewardValue, NewRules, NewEvaluationMetric, NewSubmissionLimit, NewTeamSizeLimit}).
delete_competition(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {delete_competition, CompetitionId, UserId}).
get_competition_by_id(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_by_id, CompetitionId}).
get_competitions_by_creator(CreatorId) ->
    gen_server:call({global, ?MODULE}, {get_competitions_by_creator, CreatorId}).
get_active_competitions() ->
    gen_server:call({global, ?MODULE}, {get_active_competitions}).
get_public_competitions() ->
    gen_server:call({global, ?MODULE}, {get_public_competitions}).
get_competitions_by_status(Status) ->
    gen_server:call({global, ?MODULE}, {get_competitions_by_status, Status}).
get_competitions_by_tag(Tag) ->
    gen_server:call({global, ?MODULE}, {get_competitions_by_tag, Tag}).
get_featured_competitions() ->
    gen_server:call({global, ?MODULE}, {get_featured_competitions}).
get_competitions_by_difficulty(DifficultyLevel) ->
    gen_server:call({global, ?MODULE}, {get_competitions_by_difficulty, DifficultyLevel}).
start_competition(CompetitionId, CreatorId) ->
    gen_server:call({global, ?MODULE}, {start_competition, CompetitionId, CreatorId}).
end_competition(CompetitionId, CreatorId) ->
    gen_server:call({global, ?MODULE}, {end_competition, CompetitionId, CreatorId}).
archive_competition(CompetitionId, CreatorId) ->
    gen_server:call({global, ?MODULE}, {archive_competition, CompetitionId, CreatorId}).
set_competition_status(CompetitionId, CreatorId, NewStatus) ->
    gen_server:call({global, ?MODULE}, {set_competition_status, CompetitionId, CreatorId, NewStatus}).
add_dataset_to_competition(CompetitionId, CreatorId, DatasetId) ->
    gen_server:call({global, ?MODULE}, {add_dataset_to_competition, CompetitionId, CreatorId, DatasetId}).
remove_dataset_from_competition(CompetitionId, CreatorId, DatasetId) ->
    gen_server:call({global, ?MODULE}, {remove_dataset_from_competition, CompetitionId, CreatorId, DatasetId}).
get_competition_datasets(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_datasets, CompetitionId}).
join_competition(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {join_competition, CompetitionId, UserId}).
leave_competition(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {leave_competition, CompetitionId, UserId}).
get_competition_participants(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_participants, CompetitionId}).
is_participant(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {is_participant, CompetitionId, UserId}).
create_team(CompetitionId, CreatorId, TeamName, InitialMembers) ->
    gen_server:call({global, ?MODULE}, {create_team, CompetitionId, CreatorId, TeamName, InitialMembers}).
get_team_by_id(TeamId) ->
    gen_server:call({global, ?MODULE}, {get_team_by_id, TeamId}).
invite_to_team(TeamId, InviterId, InviteeId) ->
    gen_server:call({global, ?MODULE}, {invite_to_team, TeamId, InviterId, InviteeId}).
accept_team_invitation(TeamId, UserId) ->
    gen_server:call({global, ?MODULE}, {accept_team_invitation, TeamId, UserId}).
reject_team_invitation(TeamId, UserId) ->
    gen_server:call({global, ?MODULE}, {reject_team_invitation, TeamId, UserId}).
remove_team_member(TeamId, RemoverId, MemberToRemove) ->
    gen_server:call({global, ?MODULE}, {remove_team_member, TeamId, RemoverId, MemberToRemove}).
disband_team(TeamId, CaptainId) ->
    gen_server:call({global, ?MODULE}, {disband_team, TeamId, CaptainId}).
get_competition_teams(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_teams, CompetitionId}).
get_user_team_in_competition(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_team_in_competition, CompetitionId, UserId}).
merge_teams(TeamId1, TeamId2, RequesterId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {merge_teams, TeamId1, TeamId2, RequesterId, CompetitionId}).
submit_entry(CompetitionId, UserId, SubmissionContent, NotebookId, TeamId) ->
    gen_server:call({global, ?MODULE}, {submit_entry, CompetitionId, UserId, SubmissionContent, NotebookId, TeamId}, 60000).
get_submission_by_id(SubmissionId) ->
    gen_server:call({global, ?MODULE}, {get_submission_by_id, SubmissionId}).
get_user_submissions(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_submissions, CompetitionId, UserId}).
get_team_submissions(CompetitionId, TeamId) ->
    gen_server:call({global, ?MODULE}, {get_team_submissions, CompetitionId, TeamId}).
evaluate_submission(SubmissionId, Score) ->
    gen_server:call({global, ?MODULE}, {evaluate_submission, SubmissionId, Score}).
disqualify_submission(SubmissionId, AdminId, Reason) ->
    gen_server:call({global, ?MODULE}, {disqualify_submission, SubmissionId, AdminId, Reason}).
get_competition_submissions(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_submissions, CompetitionId}).
get_best_submission(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_best_submission, CompetitionId, UserId}).
get_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_leaderboard, CompetitionId}).
update_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {update_leaderboard, CompetitionId}).
get_public_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_public_leaderboard, CompetitionId}).
get_private_leaderboard(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_private_leaderboard, CompetitionId}).
get_user_rank(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_rank, CompetitionId, UserId}).
get_team_rank(CompetitionId, TeamId) ->
    gen_server:call({global, ?MODULE}, {get_team_rank, CompetitionId, TeamId}).
add_discussion(CompetitionId, CreatorId, Title, Content) ->
    gen_server:call({global, ?MODULE}, {add_discussion, CompetitionId, CreatorId, Title, Content}, 30000).
get_competition_discussions(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_discussions, CompetitionId}).
pin_discussion(DiscussionId, AdminId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {pin_discussion, DiscussionId, AdminId, CompetitionId}).
solve_discussion(DiscussionId, AdminId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {solve_discussion, DiscussionId, AdminId, CompetitionId}).
set_prize_distribution(CompetitionId, PrizeMap) ->
    gen_server:call({global, ?MODULE}, {set_prize_distribution, CompetitionId, PrizeMap}).
get_prize_distribution(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_prize_distribution, CompetitionId}).
distribute_prizes(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {distribute_prizes, CompetitionId}).
set_evaluation_script(CompetitionId, ScriptContent) ->
    gen_server:call({global, ?MODULE}, {set_evaluation_script, CompetitionId, ScriptContent}, 30000).
get_evaluation_script(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_evaluation_script, CompetitionId}).
run_evaluation(SubmissionId, EvaluatorId) ->
    gen_server:call({global, ?MODULE}, {run_evaluation, SubmissionId, EvaluatorId}, 60000).
set_compute_quota(CompetitionId, QuotaMap) ->
    gen_server:call({global, ?MODULE}, {set_compute_quota, CompetitionId, QuotaMap}).
get_compute_quota(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_compute_quota, CompetitionId}).
check_quota_available(CompetitionId, ResourceType) ->
    gen_server:call({global, ?MODULE}, {check_quota_available, CompetitionId, ResourceType}).
consume_quota(CompetitionId, ResourceType, Amount) ->
    gen_server:call({global, ?MODULE}, {consume_quota, CompetitionId, ResourceType, Amount}).
feature_competition(CompetitionId, AdminId) ->
    gen_server:call({global, ?MODULE}, {feature_competition, CompetitionId, AdminId}).
unfeature_competition(CompetitionId, AdminId) ->
    gen_server:call({global, ?MODULE}, {unfeature_competition, CompetitionId, AdminId}).
report_competition(ReporterId, CompetitionId, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_competition, ReporterId, CompetitionId, Type, Description}).
search_competitions(Query) ->
    gen_server:call({global, ?MODULE}, {search_competitions, Query}).
search_competitions_advanced(SearchParams) ->
    gen_server:call({global, ?MODULE}, {search_competitions_advanced, SearchParams}).
get_trending_competitions(Limit) ->
    gen_server:call({global, ?MODULE}, {get_trending_competitions, Limit}).
get_upcoming_competitions() ->
    gen_server:call({global, ?MODULE}, {get_upcoming_competitions}).
get_ending_soon_competitions(DaysThreshold) ->
    gen_server:call({global, ?MODULE}, {get_ending_soon_competitions, DaysThreshold}).
get_competition_stats(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_stats, CompetitionId}).
get_user_competition_history(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_competition_history, UserId}).
get_user_competition_stats(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_competition_stats, UserId}).
link_notebook_to_submission(SubmissionId, NotebookId) ->
    gen_server:call({global, ?MODULE}, {link_notebook_to_submission, SubmissionId, NotebookId}).
get_submission_notebook(SubmissionId) ->
    gen_server:call({global, ?MODULE}, {get_submission_notebook, SubmissionId}).
download_competition_datasets(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {download_competition_datasets, CompetitionId, UserId}, 60000).
download_competition_datasets(CompetitionId, UserId, Options) ->
    gen_server:call({global, ?MODULE}, {download_competition_datasets, CompetitionId, UserId, Options}, 60000).
bulk_download_competition_datasets(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {bulk_download_competition_datasets, CompetitionId, UserId}, 60000).
get_competition_download_progress(CompetitionId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_competition_download_progress, CompetitionId, UserId}).
init([]) ->
    ?LOG_NOTICE("Competition server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, competition_worker_pool_size, 100),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.
initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].
worker_loop() ->
    receive
        {create_competition, CreatorId, Title, Description, DatasetIds, StartTime, EndTime, RewardType, RewardValue, Rules, EvaluationMetric, SubmissionLimit, TeamSizeLimit, From} ->
            Result = competitiondb:create_competition(CreatorId, Title, Description, DatasetIds, StartTime, EndTime, RewardType, RewardValue, Rules, EvaluationMetric, SubmissionLimit, TeamSizeLimit),
            gen_server:reply(From, Result),
            worker_loop();
        {update_competition, CompetitionId, CreatorId, NewTitle, NewDescription, NewDatasetIds, NewStartTime, NewEndTime, NewRewardType, NewRewardValue, NewRules, NewEvaluationMetric, NewSubmissionLimit, NewTeamSizeLimit, From} ->
            Result = competitiondb:update_competition(CompetitionId, CreatorId, NewTitle, NewDescription, NewDatasetIds, NewStartTime, NewEndTime, NewRewardType, NewRewardValue, NewRules, NewEvaluationMetric, NewSubmissionLimit, NewTeamSizeLimit),
            gen_server:reply(From, Result),
            worker_loop();
        {submit_entry, CompetitionId, UserId, SubmissionContent, NotebookId, TeamId, From} ->
            Result = competitiondb:submit_entry(CompetitionId, UserId, SubmissionContent, NotebookId, TeamId),
            gen_server:reply(From, Result),
            worker_loop();
        {set_evaluation_script, CompetitionId, ScriptContent, From} ->
            Result = competitiondb:set_evaluation_script(CompetitionId, ScriptContent),
            gen_server:reply(From, Result),
            worker_loop();
        {run_evaluation, SubmissionId, EvaluatorId, From} ->
            Result = competitiondb:run_evaluation(SubmissionId, EvaluatorId),
            gen_server:reply(From, Result),
            worker_loop();
        {download_competition_datasets, CompetitionId, UserId, Options, From} ->
            Result = competitiondb:download_competition_datasets(CompetitionId, UserId, Options),
            gen_server:reply(From, Result),
            worker_loop();
        {bulk_download_competition_datasets, CompetitionId, UserId, From} ->
            Result = competitiondb:bulk_download_competition_datasets(CompetitionId, UserId),
            gen_server:reply(From, Result),
            worker_loop();
        {add_discussion, CompetitionId, CreatorId, Title, Content, From} ->
            Result = competitiondb:add_discussion(CompetitionId, CreatorId, Title, Content),
            gen_server:reply(From, Result),
            worker_loop();
        {stop, From} ->
            From ! {stopped, self()};
        Other ->
            ?LOG_WARNING("Worker received unknown message: ~p", [Other]),
            worker_loop()
    end.
handle_call({create_competition, CreatorId, Title, Description, DatasetIds, StartTime, EndTime, RewardType, RewardValue, Rules, EvaluationMetric, SubmissionLimit, TeamSizeLimit}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_competition, CreatorId, Title, Description, DatasetIds, StartTime, EndTime, RewardType, RewardValue, Rules, EvaluationMetric, SubmissionLimit, TeamSizeLimit, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({update_competition, CompetitionId, CreatorId, NewTitle, NewDescription, NewDatasetIds, NewStartTime, NewEndTime, NewRewardType, NewRewardValue, NewRules, NewEvaluationMetric, NewSubmissionLimit, NewTeamSizeLimit}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {update_competition, CompetitionId, CreatorId, NewTitle, NewDescription, NewDatasetIds, NewStartTime, NewEndTime, NewRewardType, NewRewardValue, NewRules, NewEvaluationMetric, NewSubmissionLimit, NewTeamSizeLimit, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({delete_competition, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:delete_competition(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_competition_by_id, CompetitionId}, _From, State) ->
    Res = competitiondb:get_competition_by_id(CompetitionId),
    {reply, Res, State};
handle_call({get_competitions_by_creator, CreatorId}, _From, State) ->
    Res = competitiondb:get_competitions_by_creator(CreatorId),
    {reply, Res, State};
handle_call({get_active_competitions}, _From, State) ->
    Res = competitiondb:get_active_competitions(),
    {reply, Res, State};
handle_call({get_public_competitions}, _From, State) ->
    Res = competitiondb:get_public_competitions(),
    {reply, Res, State};
handle_call({get_competitions_by_status, Status}, _From, State) ->
    Res = competitiondb:get_competitions_by_status(Status),
    {reply, Res, State};
handle_call({get_competitions_by_tag, Tag}, _From, State) ->
    Res = competitiondb:get_competitions_by_tag(Tag),
    {reply, Res, State};
handle_call({get_featured_competitions}, _From, State) ->
    Res = competitiondb:get_featured_competitions(),
    {reply, Res, State};
handle_call({get_competitions_by_difficulty, DifficultyLevel}, _From, State) ->
    Res = competitiondb:get_competitions_by_difficulty(DifficultyLevel),
    {reply, Res, State};
handle_call({start_competition, CompetitionId, CreatorId}, _From, State) ->
    Res = competitiondb:start_competition(CompetitionId, CreatorId),
    {reply, Res, State};
handle_call({end_competition, CompetitionId, CreatorId}, _From, State) ->
    Res = competitiondb:end_competition(CompetitionId, CreatorId),
    {reply, Res, State};
handle_call({archive_competition, CompetitionId, CreatorId}, _From, State) ->
    Res = competitiondb:archive_competition(CompetitionId, CreatorId),
    {reply, Res, State};
handle_call({set_competition_status, CompetitionId, CreatorId, NewStatus}, _From, State) ->
    Res = competitiondb:set_competition_status(CompetitionId, CreatorId, NewStatus),
    {reply, Res, State};
handle_call({add_dataset_to_competition, CompetitionId, CreatorId, DatasetId}, _From, State) ->
    Res = competitiondb:add_dataset_to_competition(CompetitionId, CreatorId, DatasetId),
    {reply, Res, State};
handle_call({remove_dataset_from_competition, CompetitionId, CreatorId, DatasetId}, _From, State) ->
    Res = competitiondb:remove_dataset_from_competition(CompetitionId, CreatorId, DatasetId),
    {reply, Res, State};
handle_call({get_competition_datasets, CompetitionId}, _From, State) ->
    Res = competitiondb:get_competition_datasets(CompetitionId),
    {reply, Res, State};
handle_call({join_competition, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:join_competition(CompetitionId, UserId),
    {reply, Res, State};
handle_call({leave_competition, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:leave_competition(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_competition_participants, CompetitionId}, _From, State) ->
    Res = competitiondb:get_competition_participants(CompetitionId),
    {reply, Res, State};
handle_call({is_participant, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:is_participant(CompetitionId, UserId),
    {reply, Res, State};
handle_call({create_team, CompetitionId, CreatorId, TeamName, InitialMembers}, _From, State) ->
    Res = competitiondb:create_team(CompetitionId, CreatorId, TeamName, InitialMembers),
    {reply, Res, State};
handle_call({get_team_by_id, TeamId}, _From, State) ->
    Res = competitiondb:get_team_by_id(TeamId),
    {reply, Res, State};
handle_call({invite_to_team, TeamId, InviterId, InviteeId}, _From, State) ->
    Res = competitiondb:invite_to_team(TeamId, InviterId, InviteeId),
    {reply, Res, State};
handle_call({accept_team_invitation, TeamId, UserId}, _From, State) ->
    Res = competitiondb:accept_team_invitation(TeamId, UserId),
    {reply, Res, State};
handle_call({reject_team_invitation, TeamId, UserId}, _From, State) ->
    Res = competitiondb:reject_team_invitation(TeamId, UserId),
    {reply, Res, State};
handle_call({remove_team_member, TeamId, RemoverId, MemberToRemove}, _From, State) ->
    Res = competitiondb:remove_team_member(TeamId, RemoverId, MemberToRemove),
    {reply, Res, State};
handle_call({disband_team, TeamId, CaptainId}, _From, State) ->
    Res = competitiondb:disband_team(TeamId, CaptainId),
    {reply, Res, State};
handle_call({get_competition_teams, CompetitionId}, _From, State) ->
    Res = competitiondb:get_competition_teams(CompetitionId),
    {reply, Res, State};
handle_call({get_user_team_in_competition, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:get_user_team_in_competition(CompetitionId, UserId),
    {reply, Res, State};
handle_call({merge_teams, TeamId1, TeamId2, RequesterId, CompetitionId}, _From, State) ->
    Res = competitiondb:merge_teams(TeamId1, TeamId2, RequesterId, CompetitionId),
    {reply, Res, State};
handle_call({submit_entry, CompetitionId, UserId, SubmissionContent, NotebookId, TeamId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {submit_entry, CompetitionId, UserId, SubmissionContent, NotebookId, TeamId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_submission_by_id, SubmissionId}, _From, State) ->
    Res = competitiondb:get_submission_by_id(SubmissionId),
    {reply, Res, State};
handle_call({get_user_submissions, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:get_user_submissions(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_team_submissions, CompetitionId, TeamId}, _From, State) ->
    Res = competitiondb:get_team_submissions(CompetitionId, TeamId),
    {reply, Res, State};
handle_call({evaluate_submission, SubmissionId, Score}, _From, State) ->
    Res = competitiondb:evaluate_submission(SubmissionId, Score),
    {reply, Res, State};
handle_call({disqualify_submission, SubmissionId, AdminId, Reason}, _From, State) ->
    Res = competitiondb:disqualify_submission(SubmissionId, AdminId, Reason),
    {reply, Res, State};
handle_call({get_competition_submissions, CompetitionId}, _From, State) ->
    Res = competitiondb:get_competition_submissions(CompetitionId),
    {reply, Res, State};
handle_call({get_best_submission, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:get_best_submission(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_leaderboard, CompetitionId}, _From, State) ->
    Res = competitiondb:get_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({update_leaderboard, CompetitionId}, _From, State) ->
    Res = competitiondb:update_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({get_public_leaderboard, CompetitionId}, _From, State) ->
    Res = competitiondb:get_public_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({get_private_leaderboard, CompetitionId}, _From, State) ->
    Res = competitiondb:get_private_leaderboard(CompetitionId),
    {reply, Res, State};
handle_call({get_user_rank, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:get_user_rank(CompetitionId, UserId),
    {reply, Res, State};
handle_call({get_team_rank, CompetitionId, TeamId}, _From, State) ->
    Res = competitiondb:get_team_rank(CompetitionId, TeamId),
    {reply, Res, State};
handle_call({add_discussion, CompetitionId, CreatorId, Title, Content}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {add_discussion, CompetitionId, CreatorId, Title, Content, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_competition_discussions, CompetitionId}, _From, State) ->
    Res = competitiondb:get_competition_discussions(CompetitionId),
    {reply, Res, State};
handle_call({pin_discussion, DiscussionId, AdminId, CompetitionId}, _From, State) ->
    Res = competitiondb:pin_discussion(DiscussionId, AdminId, CompetitionId),
    {reply, Res, State};
handle_call({solve_discussion, DiscussionId, AdminId, CompetitionId}, _From, State) ->
    Res = competitiondb:solve_discussion(DiscussionId, AdminId, CompetitionId),
    {reply, Res, State};
handle_call({set_prize_distribution, CompetitionId, PrizeMap}, _From, State) ->
    Res = competitiondb:set_prize_distribution(CompetitionId, PrizeMap),
    {reply, Res, State};
handle_call({get_prize_distribution, CompetitionId}, _From, State) ->
    Res = competitiondb:get_prize_distribution(CompetitionId),
    {reply, Res, State};
handle_call({distribute_prizes, CompetitionId}, _From, State) ->
    Res = competitiondb:distribute_prizes(CompetitionId),
    {reply, Res, State};
handle_call({set_evaluation_script, CompetitionId, ScriptContent}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {set_evaluation_script, CompetitionId, ScriptContent, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_evaluation_script, CompetitionId}, _From, State) ->
    Res = competitiondb:get_evaluation_script(CompetitionId),
    {reply, Res, State};
handle_call({run_evaluation, SubmissionId, EvaluatorId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {run_evaluation, SubmissionId, EvaluatorId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({set_compute_quota, CompetitionId, QuotaMap}, _From, State) ->
    Res = competitiondb:set_compute_quota(CompetitionId, QuotaMap),
    {reply, Res, State};
handle_call({get_compute_quota, CompetitionId}, _From, State) ->
    Res = competitiondb:get_compute_quota(CompetitionId),
    {reply, Res, State};
handle_call({check_quota_available, CompetitionId, ResourceType}, _From, State) ->
    Res = competitiondb:check_quota_available(CompetitionId, ResourceType),
    {reply, Res, State};
handle_call({consume_quota, CompetitionId, ResourceType, Amount}, _From, State) ->
    Res = competitiondb:consume_quota(CompetitionId, ResourceType, Amount),
    {reply, Res, State};
handle_call({feature_competition, CompetitionId, AdminId}, _From, State) ->
    Res = competitiondb:feature_competition(CompetitionId, AdminId),
    {reply, Res, State};
handle_call({unfeature_competition, CompetitionId, AdminId}, _From, State) ->
    Res = competitiondb:unfeature_competition(CompetitionId, AdminId),
    {reply, Res, State};
handle_call({report_competition, ReporterId, CompetitionId, Type, Description}, _From, State) ->
    Res = competitiondb:report_competition(ReporterId, CompetitionId, Type, Description),
    {reply, Res, State};
handle_call({search_competitions, Query}, _From, State) ->
    Res = competitiondb:search_competitions(Query),
    {reply, Res, State};
handle_call({search_competitions_advanced, SearchParams}, _From, State) ->
    Res = competitiondb:search_competitions_advanced(SearchParams),
    {reply, Res, State};
handle_call({get_trending_competitions, Limit}, _From, State) ->
    Res = competitiondb:get_trending_competitions(Limit),
    {reply, Res, State};
handle_call({get_upcoming_competitions}, _From, State) ->
    Res = competitiondb:get_upcoming_competitions(),
    {reply, Res, State};
handle_call({get_ending_soon_competitions, DaysThreshold}, _From, State) ->
    Res = competitiondb:get_ending_soon_competitions(DaysThreshold),
    {reply, Res, State};
handle_call({get_competition_stats, CompetitionId}, _From, State) ->
    Res = competitiondb:get_competition_stats(CompetitionId),
    {reply, Res, State};
handle_call({get_user_competition_history, UserId}, _From, State) ->
    Res = competitiondb:get_user_competition_history(UserId),
    {reply, Res, State};
handle_call({get_user_competition_stats, UserId}, _From, State) ->
    Res = competitiondb:get_user_competition_stats(UserId),
    {reply, Res, State};
handle_call({link_notebook_to_submission, SubmissionId, NotebookId}, _From, State) ->
    Res = competitiondb:link_notebook_to_submission(SubmissionId, NotebookId),
    {reply, Res, State};
handle_call({get_submission_notebook, SubmissionId}, _From, State) ->
    Res = competitiondb:get_submission_notebook(SubmissionId),
    {reply, Res, State};
handle_call({download_competition_datasets, CompetitionId, UserId, Options}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {download_competition_datasets, CompetitionId, UserId, Options, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({bulk_download_competition_datasets, CompetitionId, UserId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {bulk_download_competition_datasets, CompetitionId, UserId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_competition_download_progress, CompetitionId, UserId}, _From, State) ->
    Res = competitiondb:get_competition_download_progress(CompetitionId, UserId),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.
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
