-module(submissiondb).
-author("Zaryn Technologies").

-export([
    create_submission/6,
    create_submission_concurrent/6,
    create_submission_from_file/5,
    update_submission/3,
    delete_submission/2,

    get_submission_by_id/1,
    get_submissions_by_user/2,
    get_submissions_by_team/2,
    get_submissions_by_competition/1,
    get_user_submission_count/2,
    get_team_submission_count/2,
    get_latest_submission/2,
    get_best_submission/2,

    get_submission_file/1,
    download_submission/2,
    get_submission_content/1,

    evaluate_submission/2,
    evaluate_submission_async/2,
    set_evaluation_score/3,
    set_evaluation_status/2,
    set_evaluation_error/2,
    get_evaluation_status/1,
    get_evaluation_score/1,
    retry_evaluation/1,

    disqualify_submission/3,
    reinstate_submission/2,
    is_disqualified/1,

    mark_as_late/1,
    is_late_submission/1,
    apply_late_penalty/2,

    link_notebook/2,
    unlink_notebook/1,
    get_linked_notebook/1,

    link_compute_session/2,
    get_compute_session/1,

    set_public_score/2,
    set_private_score/2,
    get_public_score/1,
    get_private_score/1,
    has_private_score/1,

    compare_submissions/2,
    get_submission_rank/2,
    get_submissions_better_than/2,
    get_submissions_worse_than/2,

    validate_submission_file/2,
    check_submission_limit/2,
    can_submit/2,

    get_submission_metadata/1,
    update_submission_metadata/2,
    add_metadata_field/3,

    get_evaluation_logs/1,
    store_evaluation_logs/2,

    clone_submission/2,

    get_submissions_by_status/2,
    get_pending_submissions/1,
    get_failed_submissions/1,
    get_completed_submissions/1,

    search_submissions/2,
    get_recent_submissions/2,
    get_top_submissions/2,

    calculate_percentile/2,
    get_submission_statistics/1,

    batch_evaluate/2,
    batch_update_status/2,

    export_submission_data/1,
    import_submission_data/2
]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-define(MAX_FILE_SIZE, 5368709120).
-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).
-define(EVALUATION_TIMEOUT, 300000).
-define(CHUNK_SIZE, 10485760).
-define(BATCH_SIZE, 50).

create_submission(CompetitionId, UserId, TeamId, SubmissionContent, NotebookId, Metadata) ->
    Fun = fun() ->
        case can_submit_internal(CompetitionId, UserId) of
            {error, Reason} ->
                {error, Reason};
            ok ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [Competition] ->
                        Id = nanoid:gen(),
                        Now = calendar:universal_time(),

                        FileContent = if
                            is_binary(SubmissionContent) -> SubmissionContent;
                            is_list(SubmissionContent) -> list_to_binary(SubmissionContent);
                            true -> <<>>
                        end,

                        ok = content_cache:set({submission, Id}, FileContent),
                        SizeBytes = byte_size(FileContent),

                        UserSubmissionCount = get_user_submission_count_internal(CompetitionId, UserId),
                        IsLate = Now > Competition#competition.end_time,

                        Submission = #submission{
                            id = Id,
                            competition_id = CompetitionId,
                            team_id = TeamId,
                            user_id = UserId,
                            submission_cid = {pending, Id},
                            notebook_id = NotebookId,
                            submission_number = UserSubmissionCount + 1,
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
                            file_size_bytes = SizeBytes,
                            evaluation_time_seconds = undefined,
                            metadata = Metadata
                        },

                        mnesia:write(Submission),

                        UpdatedSubmissionIds = [Id | Competition#competition.submission_ids],
                        mnesia:write(Competition#competition{
                            submission_ids = UpdatedSubmissionIds,
                            date_updated = Now
                        }),

                        case TeamId of
                            undefined -> ok;
                            _ ->
                                case mnesia:read({team, TeamId}) of
                                    [Team] ->
                                        UpdatedTeamSubmissions = [Id | Team#team.submission_ids],
                                        mnesia:write(Team#team{
                                            submission_ids = UpdatedTeamSubmissions,
                                            total_submissions = Team#team.total_submissions + 1,
                                            date_updated = Now
                                        });
                                    [] -> ok
                                end
                        end,

                        {ok, Id}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} ->
            spawn(fun() ->
                upload_submission_to_ipfs(Id)
            end),
            {ok, Id};
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

create_submission_concurrent(CompetitionId, UserId, TeamId, SubmissionContent, NotebookId, Metadata) ->
    IdFuture = spawn_monitor(fun() -> exit({result, nanoid:gen()}) end),
    Id = receive_result(IdFuture),

    Now = calendar:universal_time(),

    FileContent = if
        is_binary(SubmissionContent) -> SubmissionContent;
        is_list(SubmissionContent) -> list_to_binary(SubmissionContent);
        true -> <<>>
    end,

    CacheFuture = spawn_monitor(fun() ->
        ok = content_cache:set({submission, Id}, FileContent),
        exit({result, ok})
    end),

    SizeFuture = spawn_monitor(fun() ->
        exit({result, byte_size(FileContent)})
    end),

    receive_result(CacheFuture),
    SizeBytes = receive_result(SizeFuture),

    case can_submit(CompetitionId, UserId) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case mnesia:dirty_read({competition, CompetitionId}) of
                [] ->
                    {error, competition_not_found};
                [Competition] ->
                    UserSubmissionCount = get_user_submission_count(CompetitionId, UserId),
                    IsLate = Now > Competition#competition.end_time,

                    Submission = #submission{
                        id = Id,
                        competition_id = CompetitionId,
                        team_id = TeamId,
                        user_id = UserId,
                        submission_cid = {pending, Id},
                        notebook_id = NotebookId,
                        submission_number = UserSubmissionCount + 1,
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
                        file_size_bytes = SizeBytes,
                        evaluation_time_seconds = undefined,
                        metadata = Metadata
                    },

                    case write_submission_with_retry(Submission, CompetitionId, TeamId, ?MAX_RETRIES) of
                        ok ->
                            spawn(fun() ->
                                upload_submission_to_ipfs(Id)
                            end),
                            {ok, Id};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

create_submission_from_file(CompetitionId, UserId, TeamId, FilePath, NotebookId) ->
    Path = case is_binary(FilePath) of
        true -> binary_to_list(FilePath);
        false -> FilePath
    end,

    case validate_submission_file(CompetitionId, Path) of
        {error, Reason} ->
            {error, Reason};
        {ok, _FileInfo} ->
            case file:read_file(Path) of
                {ok, FileContent} ->
                    Metadata = extract_file_metadata(Path),
                    create_submission(CompetitionId, UserId, TeamId, FileContent, NotebookId, Metadata);
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end
    end.

update_submission(SubmissionId, UserId, Updates) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] ->
                {error, submission_not_found};
            [Submission] ->
                case Submission#submission.user_id of
                    UserId ->
                        case Submission#submission.evaluation_status of
                            evaluating ->
                                {error, cannot_update_during_evaluation};
                            completed ->
                                {error, cannot_update_completed_submission};
                            _ ->
                                UpdatedSubmission = apply_updates(Submission, Updates),
                                mnesia:write(UpdatedSubmission),
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

delete_submission(SubmissionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] ->
                {error, submission_not_found};
            [Submission] ->
                case Submission#submission.user_id of
                    UserId ->
                        case Submission#submission.evaluation_status of
                            evaluating ->
                                {error, cannot_delete_during_evaluation};
                            completed ->
                                {error, cannot_delete_completed_submission};
                            _ ->
                                mnesia:delete({submission, SubmissionId}),

                                case mnesia:read({competition, Submission#submission.competition_id}) of
                                    [Competition] ->
                                        UpdatedSubmissions = lists:delete(SubmissionId, Competition#competition.submission_ids),
                                        mnesia:write(Competition#competition{submission_ids = UpdatedSubmissions});
                                    [] -> ok
                                end,

                                case Submission#submission.team_id of
                                    undefined -> ok;
                                    TeamId ->
                                        case mnesia:read({team, TeamId}) of
                                            [Team] ->
                                                UpdatedTeamSubmissions = lists:delete(SubmissionId, Team#team.submission_ids),
                                                mnesia:write(Team#team{submission_ids = UpdatedTeamSubmissions});
                                            [] -> ok
                                        end
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

get_submissions_by_user(CompetitionId, UserId) ->
    Fun = fun() ->
        mnesia:match_object(#submission{
            competition_id = CompetitionId,
            user_id = UserId,
            _ = '_'
        })
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_submissions_by_team(CompetitionId, TeamId) ->
    Fun = fun() ->
        mnesia:match_object(#submission{
            competition_id = CompetitionId,
            team_id = TeamId,
            _ = '_'
        })
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_submissions_by_competition(CompetitionId) ->
    Fun = fun() ->
        mnesia:match_object(#submission{competition_id = CompetitionId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_user_submission_count(CompetitionId, UserId) ->
    Submissions = get_submissions_by_user(CompetitionId, UserId),
    length(Submissions).

get_user_submission_count_internal(CompetitionId, UserId) ->
    AllSubmissions = mnesia:match_object(#submission{
        competition_id = CompetitionId,
        user_id = UserId,
        _ = '_'
    }),
    length(AllSubmissions).

get_team_submission_count(CompetitionId, TeamId) ->
    Submissions = get_submissions_by_team(CompetitionId, TeamId),
    length(Submissions).

get_latest_submission(CompetitionId, UserId) ->
    Fun = fun() ->
        UserSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            user_id = UserId,
            _ = '_'
        }),
        case UserSubmissions of
            [] -> {error, no_submissions};
            _ ->
                Sorted = lists:sort(fun(A, B) ->
                    A#submission.submission_time > B#submission.submission_time
                end, UserSubmissions),
                {ok, hd(Sorted)}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

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
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_submission_file(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                SubmissionCID = Submission#submission.submission_cid,
                case SubmissionCID of
                    {pending, Id} when Id =:= SubmissionId ->
                        case content_cache:get({submission, Id}) of
                            undefined -> {error, file_not_ready};
                            CachedFile -> {ok, CachedFile}
                        end;
                    _ ->
                        try
                            ActualFile = ipfs_media:get_media_binary(SubmissionCID),
                            {ok, ActualFile}
                        catch
                            _:Error -> {error, Error}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, File}} -> File;
        {atomic, {error, Reason}} -> {error, Reason};
        Error -> Error
    end.

download_submission(SubmissionId, _UserId) ->
    get_submission_file(SubmissionId).

get_submission_content(SubmissionId) ->
    get_submission_file(SubmissionId).

evaluate_submission(SubmissionId, EvaluatorId) ->
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
                            EvaluatorId ->
                                mnesia:write(Submission#submission{
                                    evaluation_status = evaluating
                                }),
                                {ok, Submission};
                            _ ->
                                {error, unauthorized}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Submission}} ->
            execute_evaluation(Submission),
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

evaluate_submission_async(SubmissionId, EvaluatorId) ->
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
                            EvaluatorId ->
                                mnesia:write(Submission#submission{
                                    evaluation_status = queued
                                }),
                                {ok, Submission};
                            _ ->
                                {error, unauthorized}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Submission}} ->
            spawn(fun() ->
                set_evaluation_status(SubmissionId, evaluating),
                execute_evaluation(Submission)
            end),
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

execute_evaluation(Submission) ->
    StartTime = erlang:system_time(millisecond),

    try
        CompetitionId = Submission#submission.competition_id,
        SubmissionId = Submission#submission.id,

        case mnesia:dirty_read({competition, CompetitionId}) of
            [] ->
                set_evaluation_error(SubmissionId, "Competition not found");
            [Competition] ->
                case Competition#competition.evaluation_script_cid of
                    undefined ->
                        Score = rand:uniform() * 100,

                        EndTime = erlang:system_time(millisecond),
                        EvalTime = (EndTime - StartTime) div 1000,

                        set_evaluation_score(SubmissionId, Score, EvalTime),
                        competitiondb:update_leaderboard(CompetitionId);
                    _ScriptCID ->
                        Score = rand:uniform() * 100,

                        EndTime = erlang:system_time(millisecond),
                        EvalTime = (EndTime - StartTime) div 1000,

                        set_evaluation_score(SubmissionId, Score, EvalTime),
                        competitiondb:update_leaderboard(CompetitionId)
                end
        end
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception during evaluation of ~p: ~p:~p~n~p",
                [Submission#submission.id, Exception, Error, Stacktrace]
            ),
            set_evaluation_error(Submission#submission.id, io_lib:format("~p", [Error]))
    end.

set_evaluation_score(SubmissionId, Score, EvaluationTime) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
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
                    evaluation_time_seconds = EvaluationTime
                },
                mnesia:write(UpdatedSubmission),

                case Submission#submission.team_id of
                    undefined -> ok;
                    TeamId ->
                        update_team_best_score(TeamId, SubmissionId, FinalScore)
                end,

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_team_best_score(TeamId, SubmissionId, Score) ->
    case mnesia:read({team, TeamId}) of
        [Team] ->
            CurrentScore = Team#team.team_score,
            CurrentBestId = Team#team.best_submission_id,

            {NewScore, NewBestId} = case CurrentScore of
                undefined ->
                    {Score, SubmissionId};
                _ ->
                    if Score > CurrentScore ->
                        {Score, SubmissionId};
                       true ->
                        {CurrentScore, CurrentBestId}
                    end
            end,

            mnesia:write(Team#team{
                team_score = NewScore,
                best_submission_id = NewBestId,
                date_updated = calendar:universal_time()
            });
        [] -> ok
    end.

set_evaluation_status(SubmissionId, Status) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{evaluation_status = Status}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_evaluation_error(SubmissionId, ErrorMessage) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{
                    evaluation_status = failed,
                    error_message = ErrorMessage
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_evaluation_status(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] -> {ok, Submission#submission.evaluation_status}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_evaluation_score(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                case Submission#submission.score_public of
                    undefined -> {error, not_evaluated};
                    Score -> {ok, Score}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

retry_evaluation(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                case Submission#submission.evaluation_status of
                    failed ->
                        mnesia:write(Submission#submission{
                            evaluation_status = pending,
                            error_message = undefined
                        }),
                        {ok, Submission};
                    _ ->
                        {error, not_failed}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Submission}} ->
            spawn(fun() ->
                set_evaluation_status(SubmissionId, evaluating),
                execute_evaluation(Submission)
            end),
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
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

reinstate_submission(SubmissionId, AdminId) ->
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
                                    disqualified = false,
                                    disqualification_reason = undefined
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

is_disqualified(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] -> Submission#submission.disqualified
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

mark_as_late(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{late_submission = true}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

is_late_submission(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] -> Submission#submission.late_submission
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

apply_late_penalty(SubmissionId, PenaltyPercent) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                case Submission#submission.score_public of
                    undefined -> {error, not_scored_yet};
                    Score ->
                        NewScore = Score * (1.0 - PenaltyPercent),
                        mnesia:write(Submission#submission{score_public = NewScore}),
                        {ok, NewScore}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

link_notebook(SubmissionId, NotebookId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{notebook_id = NotebookId}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unlink_notebook(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{notebook_id = undefined}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_linked_notebook(SubmissionId) ->
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

link_compute_session(SubmissionId, SessionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{compute_session_id = SessionId}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_compute_session(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] -> {ok, Submission#submission.compute_session_id}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_public_score(SubmissionId, Score) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{score_public = Score}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_private_score(SubmissionId, Score) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{score_private = Score}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_public_score(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] -> {ok, Submission#submission.score_public}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_private_score(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] -> {ok, Submission#submission.score_private}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

has_private_score(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> false;
            [Submission] -> Submission#submission.score_private =/= undefined
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

compare_submissions(SubmissionId1, SubmissionId2) ->
    Fun = fun() ->
        case {mnesia:read({submission, SubmissionId1}), mnesia:read({submission, SubmissionId2})} of
            {[], _} -> {error, submission1_not_found};
            {_, []} -> {error, submission2_not_found};
            {[Sub1], [Sub2]} ->
                Score1 = Sub1#submission.score_public,
                Score2 = Sub2#submission.score_public,
                case {Score1, Score2} of
                    {undefined, _} -> {error, submission1_not_scored};
                    {_, undefined} -> {error, submission2_not_scored};
                    _ ->
                        if
                            Score1 > Score2 -> {ok, greater, Score1 - Score2};
                            Score1 < Score2 -> {ok, less, Score2 - Score1};
                            true -> {ok, equal, 0}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_submission_rank(CompetitionId, SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                AllSubmissions = mnesia:match_object(#submission{
                    competition_id = CompetitionId,
                    _ = '_'
                }),

                ValidSubmissions = lists:filter(fun(S) ->
                    S#submission.score_public =/= undefined andalso
                    S#submission.disqualified =:= false
                end, AllSubmissions),

                Sorted = lists:sort(fun(A, B) ->
                    A#submission.score_public > B#submission.score_public
                end, ValidSubmissions),

                case lists:keyfind(SubmissionId, #submission.id, Sorted) of
                    false -> {error, submission_not_ranked};
                    _ ->
                        Rank = find_rank(SubmissionId, Sorted, 1),
                        {ok, Rank}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

find_rank(SubmissionId, [#submission{id = SubmissionId} | _], Rank) ->
    Rank;
find_rank(SubmissionId, [_ | Rest], Rank) ->
    find_rank(SubmissionId, Rest, Rank + 1);
find_rank(_, [], _) ->
    undefined.

get_submissions_better_than(CompetitionId, Score) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            _ = '_'
        }),

        lists:filter(fun(S) ->
            S#submission.score_public =/= undefined andalso
            S#submission.score_public > Score andalso
            S#submission.disqualified =:= false
        end, AllSubmissions)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_submissions_worse_than(CompetitionId, Score) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            _ = '_'
        }),

        lists:filter(fun(S) ->
            S#submission.score_public =/= undefined andalso
            S#submission.score_public < Score andalso
            S#submission.disqualified =:= false
        end, AllSubmissions)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

validate_submission_file(CompetitionId, FilePath) ->
    case filelib:is_file(FilePath) of
        false ->
            {error, file_not_found};
        true ->
            case file:read_file_info(FilePath) of
                {ok, FileInfo} ->
                    Size = FileInfo#file_info.size,
                    if
                        Size > ?MAX_FILE_SIZE ->
                            {error, {file_too_large, Size, ?MAX_FILE_SIZE}};
                        Size == 0 ->
                            {error, empty_file};
                        true ->
                            {ok, FileInfo}
                    end;
                {error, Reason} ->
                    {error, {file_info_error, Reason}}
            end
    end.

check_submission_limit(CompetitionId, UserId) ->
    case mnesia:dirty_read({competition, CompetitionId}) of
        [] ->
            {error, competition_not_found};
        [Competition] ->
            UserSubmissionCount = get_user_submission_count(CompetitionId, UserId),
            Limit = Competition#competition.submission_count_limit,
            if
                UserSubmissionCount >= Limit ->
                    {error, submission_limit_reached};
                true ->
                    {ok, Limit - UserSubmissionCount}
            end
    end.

can_submit(CompetitionId, UserId) ->
    case mnesia:dirty_read({competition, CompetitionId}) of
        [] ->
            {error, competition_not_found};
        [Competition] ->
            case Competition#competition.status of
                active ->
                    case lists:member(UserId, Competition#competition.participants) of
                        false ->
                            {error, not_a_participant};
                        true ->
                            UserSubmissionCount = get_user_submission_count(CompetitionId, UserId),
                            case UserSubmissionCount >= Competition#competition.submission_count_limit of
                                true ->
                                    {error, submission_limit_reached};
                                false ->
                                    ok
                            end
                    end;
                _ ->
                    {error, competition_not_active}
            end
    end.

can_submit_internal(CompetitionId, UserId) ->
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
                            UserSubmissionCount = get_user_submission_count_internal(CompetitionId, UserId),
                            case UserSubmissionCount >= Competition#competition.submission_count_limit of
                                true ->
                                    {error, submission_limit_reached};
                                false ->
                                    ok
                            end
                    end;
                _ ->
                    {error, competition_not_active}
            end
    end.

get_submission_metadata(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] -> {ok, Submission#submission.metadata}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_submission_metadata(SubmissionId, NewMetadata) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                mnesia:write(Submission#submission{metadata = NewMetadata}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_metadata_field(SubmissionId, Key, Value) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                UpdatedMetadata = maps:put(Key, Value, Submission#submission.metadata),
                mnesia:write(Submission#submission{metadata = UpdatedMetadata}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_evaluation_logs(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                case Submission#submission.evaluation_cid of
                    undefined -> {error, no_logs_available};
                    LogsCID ->
                        try
                            Logs = ipfs_content:get_text_content(LogsCID),
                            {ok, Logs}
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

store_evaluation_logs(SubmissionId, Logs) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                LogsCID = ipfs_content:upload_text(Logs),
                mnesia:write(Submission#submission{evaluation_cid = LogsCID}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

clone_submission(SubmissionId, NewUserId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [OriginalSubmission] ->
                NewId = nanoid:gen(),
                Now = calendar:universal_time(),

                NewSubmission = OriginalSubmission#submission{
                    id = NewId,
                    user_id = NewUserId,
                    submission_number = 1,
                    score_public = undefined,
                    score_private = undefined,
                    evaluation_status = pending,
                    evaluation_cid = undefined,
                    error_message = undefined,
                    submission_time = Now,
                    disqualified = false,
                    disqualification_reason = undefined
                },

                mnesia:write(NewSubmission),
                {ok, NewId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_submissions_by_status(CompetitionId, Status) ->
    Fun = fun() ->
        mnesia:match_object(#submission{
            competition_id = CompetitionId,
            evaluation_status = Status,
            _ = '_'
        })
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_pending_submissions(CompetitionId) ->
    get_submissions_by_status(CompetitionId, pending).

get_failed_submissions(CompetitionId) ->
    get_submissions_by_status(CompetitionId, failed).

get_completed_submissions(CompetitionId) ->
    get_submissions_by_status(CompetitionId, completed).

search_submissions(CompetitionId, Query) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            _ = '_'
        }),

        QueryLower = string:to_lower(Query),

        lists:filter(fun(Submission) ->
            UserIdMatch = string:find(string:to_lower(Submission#submission.user_id), QueryLower) =/= nomatch,
            UserIdMatch
        end, AllSubmissions)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_recent_submissions(CompetitionId, Limit) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            _ = '_'
        }),

        Sorted = lists:sort(fun(A, B) ->
            A#submission.submission_time > B#submission.submission_time
        end, AllSubmissions),

        lists:sublist(Sorted, Limit)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_top_submissions(CompetitionId, Limit) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            _ = '_'
        }),

        ValidSubmissions = lists:filter(fun(S) ->
            S#submission.score_public =/= undefined andalso
            S#submission.disqualified =:= false
        end, AllSubmissions),

        Sorted = lists:sort(fun(A, B) ->
            A#submission.score_public > B#submission.score_public
        end, ValidSubmissions),

        lists:sublist(Sorted, Limit)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

calculate_percentile(CompetitionId, SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                case Submission#submission.score_public of
                    undefined -> {error, not_scored};
                    Score ->
                        AllSubmissions = mnesia:match_object(#submission{
                            competition_id = CompetitionId,
                            _ = '_'
                        }),

                        ScoredSubmissions = lists:filter(fun(S) ->
                            S#submission.score_public =/= undefined andalso
                            S#submission.disqualified =:= false
                        end, AllSubmissions),

                        Total = length(ScoredSubmissions),
                        Better = length(lists:filter(fun(S) ->
                            S#submission.score_public > Score
                        end, ScoredSubmissions)),

                        Percentile = case Total of
                            0 -> 0;
                            _ -> (1 - (Better / Total)) * 100
                        end,

                        {ok, Percentile}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_submission_statistics(CompetitionId) ->
    Fun = fun() ->
        AllSubmissions = mnesia:match_object(#submission{
            competition_id = CompetitionId,
            _ = '_'
        }),

        Total = length(AllSubmissions),

        Pending = length(lists:filter(fun(S) -> S#submission.evaluation_status =:= pending end, AllSubmissions)),
        Evaluating = length(lists:filter(fun(S) -> S#submission.evaluation_status =:= evaluating end, AllSubmissions)),
        Completed = length(lists:filter(fun(S) -> S#submission.evaluation_status =:= completed end, AllSubmissions)),
        Failed = length(lists:filter(fun(S) -> S#submission.evaluation_status =:= failed end, AllSubmissions)),

        Disqualified = length(lists:filter(fun(S) -> S#submission.disqualified =:= true end, AllSubmissions)),
        Late = length(lists:filter(fun(S) -> S#submission.late_submission =:= true end, AllSubmissions)),

        ScoredSubmissions = lists:filter(fun(S) ->
            S#submission.score_public =/= undefined
        end, AllSubmissions),

        {AvgScore, MinScore, MaxScore} = case ScoredSubmissions of
            [] -> {0, 0, 0};
            _ ->
                Scores = [S#submission.score_public || S <- ScoredSubmissions],
                Avg = lists:sum(Scores) / length(Scores),
                Min = lists:min(Scores),
                Max = lists:max(Scores),
                {Avg, Min, Max}
        end,

        {ok, #{
            total => Total,
            pending => Pending,
            evaluating => Evaluating,
            completed => Completed,
            failed => Failed,
            disqualified => Disqualified,
            late => Late,
            avg_score => AvgScore,
            min_score => MinScore,
            max_score => MaxScore
        }}
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

batch_evaluate(SubmissionIds, EvaluatorId) ->
    Results = lists:map(fun(SubmissionId) ->
        case evaluate_submission_async(SubmissionId, EvaluatorId) of
            ok -> {SubmissionId, ok};
            {error, Reason} -> {SubmissionId, {error, Reason}}
        end
    end, SubmissionIds),

    {ok, Results}.

batch_update_status(SubmissionIds, NewStatus) ->
    Results = lists:map(fun(SubmissionId) ->
        case set_evaluation_status(SubmissionId, NewStatus) of
            ok -> {SubmissionId, ok};
            {error, Reason} -> {SubmissionId, {error, Reason}}
        end
    end, SubmissionIds),

    {ok, Results}.

export_submission_data(SubmissionId) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                Data = #{
                    id => Submission#submission.id,
                    competition_id => Submission#submission.competition_id,
                    user_id => Submission#submission.user_id,
                    team_id => Submission#submission.team_id,
                    submission_number => Submission#submission.submission_number,
                    score_public => Submission#submission.score_public,
                    score_private => Submission#submission.score_private,
                    evaluation_status => Submission#submission.evaluation_status,
                    submission_time => Submission#submission.submission_time,
                    late_submission => Submission#submission.late_submission,
                    disqualified => Submission#submission.disqualified,
                    file_size_bytes => Submission#submission.file_size_bytes,
                    evaluation_time_seconds => Submission#submission.evaluation_time_seconds,
                    metadata => Submission#submission.metadata
                },
                {ok, Data}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

import_submission_data(SubmissionId, Data) ->
    Fun = fun() ->
        case mnesia:read({submission, SubmissionId}) of
            [] -> {error, submission_not_found};
            [Submission] ->
                UpdatedSubmission = Submission#submission{
                    score_public = maps:get(score_public, Data, Submission#submission.score_public),
                    score_private = maps:get(score_private, Data, Submission#submission.score_private),
                    evaluation_status = maps:get(evaluation_status, Data, Submission#submission.evaluation_status),
                    metadata = maps:get(metadata, Data, Submission#submission.metadata)
                },
                mnesia:write(UpdatedSubmission),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

upload_submission_to_ipfs(SubmissionId) ->
    try
        FileContent = content_cache:get({submission, SubmissionId}),

        CID = case byte_size(FileContent) of
            Size when Size > ?CHUNK_SIZE ->
                upload_large_file_chunked(FileContent);
            _ ->
                ipfs_media:upload_media(FileContent)
        end,

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

upload_large_file_chunked(BinaryContent) ->
    Chunks = split_into_chunks(BinaryContent, ?CHUNK_SIZE),
    ChunkCIDs = [ipfs_media:upload_media(Chunk) || Chunk <- Chunks],

    Manifest = #{
        <<"type">> => <<"chunked">>,
        <<"chunks">> => ChunkCIDs,
        <<"total_size">> => byte_size(BinaryContent)
    },

    ManifestJSON = jsx:encode(Manifest),
    ipfs_content:upload_text(binary_to_list(ManifestJSON)).

split_into_chunks(Binary, ChunkSize) ->
    split_into_chunks(Binary, ChunkSize, []).

split_into_chunks(<<>>, _ChunkSize, Acc) ->
    lists:reverse(Acc);
split_into_chunks(Binary, ChunkSize, Acc) when byte_size(Binary) =< ChunkSize ->
    lists:reverse([Binary | Acc]);
split_into_chunks(Binary, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = Binary,
    split_into_chunks(Rest, ChunkSize, [Chunk | Acc]).

extract_file_metadata(FilePath) ->
    FileName = filename:basename(FilePath),
    Extension = filename:extension(FilePath),

    #{
        filename => FileName,
        extension => Extension,
        uploaded_at => calendar:universal_time()
    }.

apply_updates(Submission, Updates) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        case Key of
            metadata -> Acc#submission{metadata = Value};
            notebook_id -> Acc#submission{notebook_id = Value};
            _ -> Acc
        end
    end, Submission, Updates).

receive_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, process, Pid, {result, Result}} ->
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            error_logger:error_msg("Process ~p failed: ~p", [Pid, Reason]),
            exit({concurrent_operation_failed, Reason})
    after 30000 ->
        exit(Pid, kill),
        exit(timeout)
    end.

write_submission_with_retry(Submission, CompetitionId, TeamId, RetriesLeft) when RetriesLeft > 0 ->
    Fun = fun() ->
        mnesia:write(Submission),

        case mnesia:read({competition, CompetitionId}) of
            [Competition] ->
                UpdatedSubmissionIds = [Submission#submission.id | Competition#competition.submission_ids],
                mnesia:write(Competition#competition{
                    submission_ids = UpdatedSubmissionIds,
                    date_updated = calendar:universal_time()
                });
            [] ->
                error_logger:warning_msg("Competition ~p not found", [CompetitionId])
        end,

        case TeamId of
            undefined -> ok;
            _ ->
                case mnesia:read({team, TeamId}) of
                    [Team] ->
                        UpdatedTeamSubmissions = [Submission#submission.id | Team#team.submission_ids],
                        mnesia:write(Team#team{
                            submission_ids = UpdatedTeamSubmissions,
                            total_submissions = Team#team.total_submissions + 1,
                            date_updated = calendar:universal_time()
                        });
                    [] -> ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, _} ->
            ok;
        {aborted, Reason} ->
            error_logger:warning_msg("Submission write failed (retries left: ~p): ~p",
                                   [RetriesLeft, Reason]),
            timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
            write_submission_with_retry(Submission, CompetitionId, TeamId, RetriesLeft - 1)
    end;
write_submission_with_retry(_Submission, _CompetitionId, _TeamId, 0) ->
    {error, max_retries_exceeded}.
