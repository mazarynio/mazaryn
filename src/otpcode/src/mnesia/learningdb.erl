-module(learningdb).
-author("Zaryn Technologies").
-export([
    create_learning_path/15, update_learning_path/16, delete_learning_path/2,
    create_learning_resource/18, update_learning_resource/19, delete_learning_resource/2,
    create_module/11, update_module/12, delete_module/2,
    create_lesson/18, update_lesson/19, delete_lesson/2,
    create_quiz/13, update_quiz/14, delete_quiz/2,
    create_exercise/14, update_exercise/15, delete_exercise/2,
    create_project/15, update_project/17, delete_project/2,
    approve_content/4, reject_content/5, flag_content/5, unflag_content/3,
    get_pending_approvals/1, get_flagged_content/1, get_creator_content/2,
    get_learning_path/1, get_learning_resource/1, get_module/1, get_lesson/1,
    get_quiz/1, get_exercise/1, get_project/1,
    get_all_learning_paths/0, get_approved_learning_paths/0,
    get_paths_by_level/1, get_paths_by_track/1, get_paths_by_category/2,
    get_featured_paths/0, get_trending_paths/1, get_bestseller_paths/1,
    get_resources_by_path/1, get_modules_by_path/1, get_lessons_by_module/1,
    enroll_user/2, unenroll_user/2, get_user_enrollments/1,
    get_user_progress/2, update_progress/3,
    complete_resource/2, complete_lesson/2, complete_module/2, complete_path/2,
    submit_quiz/3, get_quiz_attempts/2, get_best_quiz_score/2,
    submit_exercise/4, get_exercise_submissions/2,
    submit_project/4, get_project_submission/2, grade_project/4,
    award_certificate/3, get_user_certificates/1, verify_certificate/1,
    award_badge/2, get_user_badges/1,
    create_discussion_post/6, reply_to_post/3, get_resource_discussions/1,
    track_time_spent/3, track_content_time/4,
    add_bookmark/2, remove_bookmark/2, get_user_bookmarks/1,
    rate_resource/3, get_resource_rating/1,
    search_learning_content/1, get_recommended_paths/1,
    get_prerequisite_paths/1, get_next_paths/1,
    create_study_group/5, join_study_group/2, leave_study_group/2, get_user_study_groups/1,
    schedule_mentor_session/6, complete_mentor_session/2, get_mentor_sessions/1,
    create_live_class/9, register_for_live_class/2, get_upcoming_live_classes/1,
    get_learning_analytics/1, get_path_analytics/1, get_completion_stats/1,
    get_leaderboard/2, get_user_rank/2,
    create_instructor_profile/12, update_instructor_profile/13,
    verify_instructor/3, get_instructor_profile/1,
    log_admin_action/7, get_admin_actions/2, reverse_admin_action/3,
    approve_path/2, reject_path/3, get_paths_by_status/1,
    get_lessons_by_path/1, get_module_with_lessons/1, get_path_structure/1, get_lesson_with_details/1,
    create_lesson_progress/2, update_lesson_progress/3, get_lesson_progress/2,
    create_module_progress/2, update_module_progress/3, get_module_progress/2,
    get_user_progress_detailed/2, update_video_position/3,
    add_comment/5, get_comments/2, add_reaction/4, get_reactions/2, remove_reaction/3,
    ask_question/5, answer_question/4, accept_answer/2, get_lesson_questions/1,
    upvote_question/2, upvote_answer/2,
    create_path_review/8, get_path_reviews/1, mark_review_helpful/2,
    create_upload_session/4, update_upload_progress/3, complete_upload/2, get_upload_session/1,
    record_content_view/3, get_content_analytics/2, get_instructor_analytics/2,
    increment_view_count/2, increment_completion_count/2,
    create_notification/7, get_user_notifications/1, mark_notification_read/1,
    enroll_user_enhanced/2, get_next_lesson/2, unlock_next_module/2,
    get_learning_statistics/1, get_popular_paths/1, get_completion_leaderboard/2
]).
-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(CHUNK_SIZE, 10485760).

is_admin(Username) ->
    admindb:is_admin(Username).

can_create_content(UserId) ->
    instructordb:is_verified_instructor(UserId).

create_learning_path(CreatorId, CreatorName, CreatorFamily, Title, Description, DifficultyLevel, EstimatedDuration,
                     Track, Category, Subcategory, Tags, Price, Currency, Language, ThumbnailData) ->
    case can_create_content(CreatorId) of
        false -> {error, not_verified_instructor};
        true ->
            Fun = fun() ->
                Id = nanoid:gen(),
                Now = calendar:universal_time(),
                ok = content_cache:set({path_description, Id}, Description),
                ok = content_cache:set({path_thumbnail, Id}, ThumbnailData),
                Path = #learning_path{
                    id = Id, title = Title, description = {pending, Id},
                    creator_id = CreatorId, creator_name = CreatorName, creator_family = CreatorFamily,
                    approval_status = approved, difficulty_level = DifficultyLevel,
                    estimated_duration = EstimatedDuration, track = Track, category = Category,
                    subcategory = Subcategory, tags = Tags, price = Price, currency = Currency,
                    language = Language, thumbnail_cid = {pending, Id}, visibility = public,
                    date_created = Now, date_updated = Now
                },
                mnesia:write(Path),
                {ok, Id}
            end,
            case mnesia:transaction(Fun) of
                {atomic, {ok, Id}} ->
                    spawn(fun() ->
                        DescContent = content_cache:get({path_description, Id}),
                        DescCID = ipfs_content:upload_text(DescContent),
                        ThumbData = content_cache:get({path_thumbnail, Id}),
                        ThumbCID = case ThumbData of undefined -> undefined; _ -> ipfs_media:upload_media(ThumbData) end,
                        UpdateF = fun() ->
                            case mnesia:read({learning_path, Id}) of
                                [P] -> mnesia:write(P#learning_path{description = DescCID, thumbnail_cid = ThumbCID});
                                [] -> ok
                            end
                        end,
                        mnesia:transaction(UpdateF),
                        content_cache:delete({path_description, Id}),
                        content_cache:delete({path_thumbnail, Id})
                    end),
                    Id;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end
    end.

update_learning_path(PathId, CreatorId, NewTitle, NewDescription, NewDifficultyLevel, NewEstimatedDuration,
                     NewTrack, NewCategory, NewSubcategory, NewTags, NewPrice, NewCurrency,
                     NewLanguage, NewThumbnailData, NewPreviewVideoData, NewSyllabusPdfData) ->
    Fun = fun() ->
        case mnesia:read({learning_path, PathId}) of
            [] -> {error, not_found};
            [Path] ->
                case Path#learning_path.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),
                        DescCID = if NewDescription =/= undefined -> content_cache:set({path_desc_upd, PathId}, NewDescription), {pending_update, PathId}; true -> Path#learning_path.description end,
                        ThumbCID = if NewThumbnailData =/= undefined -> content_cache:set({path_thumb_upd, PathId}, NewThumbnailData), {pending_update, PathId}; true -> Path#learning_path.thumbnail_cid end,
                        PreviewCID = if NewPreviewVideoData =/= undefined -> content_cache:set({path_prev_upd, PathId}, NewPreviewVideoData), {pending_update, PathId}; true -> Path#learning_path.preview_video_cid end,
                        SyllabusCID = if NewSyllabusPdfData =/= undefined -> content_cache:set({path_syll_upd, PathId}, NewSyllabusPdfData), {pending_update, PathId}; true -> Path#learning_path.syllabus_pdf_cid end,
                        Updated = Path#learning_path{title = NewTitle, description = DescCID, difficulty_level = NewDifficultyLevel,
                            estimated_duration = NewEstimatedDuration, track = NewTrack, category = NewCategory, subcategory = NewSubcategory,
                            tags = NewTags, price = NewPrice, currency = NewCurrency, language = NewLanguage, thumbnail_cid = ThumbCID,
                            preview_video_cid = PreviewCID, syllabus_pdf_cid = SyllabusCID,
                            date_updated = Now},
                        mnesia:write(Updated),
                        spawn(fun() ->
                            if NewDescription =/= undefined -> DC = content_cache:get({path_desc_upd, PathId}), DCID = ipfs_content:upload_text(DC), update_path_field(PathId, description, DCID), content_cache:delete({path_desc_upd, PathId}); true -> ok end,
                            if NewThumbnailData =/= undefined -> TD = content_cache:get({path_thumb_upd, PathId}), TCID = ipfs_media:upload_media(TD), update_path_field(PathId, thumbnail_cid, TCID), content_cache:delete({path_thumb_upd, PathId}); true -> ok end,
                            if NewPreviewVideoData =/= undefined -> PD = content_cache:get({path_prev_upd, PathId}), PCID = ipfs_video:upload_video(PD), update_path_field(PathId, preview_video_cid, PCID), content_cache:delete({path_prev_upd, PathId}); true -> ok end,
                            if NewSyllabusPdfData =/= undefined -> SD = content_cache:get({path_syll_upd, PathId}), SCID = ipfs_media:upload_media(SD), update_path_field(PathId, syllabus_pdf_cid, SCID), content_cache:delete({path_syll_upd, PathId}); true -> ok end
                        end),
                        ok;
                    _ -> {error, unauthorized}
                end
        end
    end,
    case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

update_path_field(PathId, Field, Value) ->
    F = fun() ->
        case mnesia:read({learning_path, PathId}) of
            [P] ->
                Updated = case Field of description -> P#learning_path{description = Value}; thumbnail_cid -> P#learning_path{thumbnail_cid = Value};
                    preview_video_cid -> P#learning_path{preview_video_cid = Value}; syllabus_pdf_cid -> P#learning_path{syllabus_pdf_cid = Value}; ipns -> P#learning_path{ipns = Value} end,
                mnesia:write(Updated);
            [] -> ok
        end
    end,
    mnesia:transaction(F).

delete_learning_path(PathId, UserId) ->
    Fun = fun() ->
        case mnesia:read({learning_path, PathId}) of
            [] -> {error, not_found};
            [Path] -> case Path#learning_path.creator_id of UserId -> mnesia:delete({learning_path, PathId}), ok; _ -> {error, unauthorized} end
        end
    end,
    case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

get_lessons_by_path(PathId) ->
    Fun = fun() ->
        Modules = qlc:e(qlc:q([M || M <- mnesia:table(learning_module),
                                     M#learning_module.path_id =:= PathId])),
        ModuleIds = [M#learning_module.id || M <- Modules],
        qlc:e(qlc:q([L || L <- mnesia:table(lesson),
                          lists:member(L#lesson.module_id, ModuleIds)]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, Lessons} -> Lessons;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_module_with_lessons(ModuleId) ->
    Fun = fun() ->
        case mnesia:read({learning_module, ModuleId}) of
            [] -> {error, not_found};
            [Module] ->
                Lessons = qlc:e(qlc:q([L || L <- mnesia:table(lesson),
                                             L#lesson.module_id =:= ModuleId])),
                SortedLessons = lists:sort(
                    fun(A, B) -> A#lesson.lesson_number =< B#lesson.lesson_number end,
                    Lessons
                ),
                #{module => Module, lessons => SortedLessons}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_path_structure(PathId) ->
    Fun = fun() ->
        case mnesia:read({learning_path, PathId}) of
            [] -> {error, not_found};
            [Path] ->
                Modules = qlc:e(qlc:q([M || M <- mnesia:table(learning_module),
                                             M#learning_module.path_id =:= PathId])),
                SortedModules = lists:sort(
                    fun(A, B) -> A#learning_module.module_number =< B#learning_module.module_number end,
                    Modules
                ),
                ModulesWithLessons = lists:map(fun(Module) ->
                    Lessons = qlc:e(qlc:q([L || L <- mnesia:table(lesson),
                                                 L#lesson.module_id =:= Module#learning_module.id])),
                    SortedLessons = lists:sort(
                        fun(A, B) -> A#lesson.lesson_number =< B#lesson.lesson_number end,
                        Lessons
                    ),
                    #{module => Module, lessons => SortedLessons}
                end, SortedModules),
                #{path => Path, modules => ModulesWithLessons}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_lesson_with_details(LessonId) ->
    Fun = fun() ->
        case mnesia:read({lesson, LessonId}) of
            [] -> {error, not_found};
            [Lesson] ->
                Comments = qlc:e(qlc:q([C || C <- mnesia:table(content_comment),
                                              C#content_comment.content_type =:= lesson,
                                              C#content_comment.content_id =:= LessonId])),
                Questions = qlc:e(qlc:q([Q || Q <- mnesia:table(student_question),
                                               Q#student_question.lesson_id =:= LessonId])),
                #{lesson => Lesson, comments => Comments, questions => Questions}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_lesson_progress(UserId, LessonId) ->
    Fun = fun() ->
        case mnesia:read({lesson, LessonId}) of
            [] -> {error, lesson_not_found};
            [Lesson] ->
                PathId = case mnesia:read({learning_module, Lesson#lesson.module_id}) of
                    [Module] -> Module#learning_module.path_id;
                    [] -> undefined
                end,
                Id = nanoid:gen(),
                Now = calendar:universal_time(),
                Progress = #lesson_progress{
                    id = Id,
                    user_id = UserId,
                    lesson_id = LessonId,
                    module_id = Lesson#lesson.module_id,
                    path_id = PathId,
                    status = in_progress,
                    last_accessed = Now
                },
                mnesia:write(Progress),
                {ok, Id}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_lesson_progress(UserId, LessonId, Updates) ->
    Fun = fun() ->
        Existing = qlc:e(qlc:q([P || P <- mnesia:table(lesson_progress),
                                      P#lesson_progress.user_id =:= UserId,
                                      P#lesson_progress.lesson_id =:= LessonId])),
        case Existing of
            [] -> {error, not_found};
            [Progress] ->
                Now = calendar:universal_time(),
                Updated = Progress#lesson_progress{
                    progress_percentage = maps:get(progress_percentage, Updates, Progress#lesson_progress.progress_percentage),
                    video_position_seconds = maps:get(video_position, Updates, Progress#lesson_progress.video_position_seconds),
                    status = maps:get(status, Updates, Progress#lesson_progress.status),
                    time_spent_seconds = maps:get(time_spent, Updates, Progress#lesson_progress.time_spent_seconds),
                    completed_at = maps:get(completed_at, Updates, Progress#lesson_progress.completed_at),
                    last_accessed = Now
                },
                mnesia:write(Updated),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_lesson_progress(UserId, LessonId) ->
    Fun = fun() ->
        qlc:e(qlc:q([P || P <- mnesia:table(lesson_progress),
                          P#lesson_progress.user_id =:= UserId,
                          P#lesson_progress.lesson_id =:= LessonId]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, [Progress]} -> Progress;
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_video_position(UserId, LessonId, PositionSeconds) ->
    update_lesson_progress(UserId, LessonId, #{video_position => PositionSeconds}).

create_module_progress(UserId, ModuleId) ->
    Fun = fun() ->
        case mnesia:read({learning_module, ModuleId}) of
            [] -> {error, module_not_found};
            [Module] ->
                Id = nanoid:gen(),
                Now = calendar:universal_time(),
                Progress = #module_progress{
                    id = Id,
                    user_id = UserId,
                    module_id = ModuleId,
                    path_id = Module#learning_module.path_id,
                    status = in_progress,
                    started_at = Now
                },
                mnesia:write(Progress),
                {ok, Id}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_module_progress(UserId, ModuleId, Updates) ->
    Fun = fun() ->
        Existing = qlc:e(qlc:q([P || P <- mnesia:table(module_progress),
                                      P#module_progress.user_id =:= UserId,
                                      P#module_progress.module_id =:= ModuleId])),
        case Existing of
            [] -> {error, not_found};
            [Progress] ->
                Updated = Progress#module_progress{
                    progress_percentage = maps:get(progress_percentage, Updates, Progress#module_progress.progress_percentage),
                    status = maps:get(status, Updates, Progress#module_progress.status),
                    completed_lessons = maps:get(completed_lessons, Updates, Progress#module_progress.completed_lessons),
                    completed_at = maps:get(completed_at, Updates, Progress#module_progress.completed_at)
                },
                mnesia:write(Updated),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_module_progress(UserId, ModuleId) ->
    Fun = fun() ->
        qlc:e(qlc:q([P || P <- mnesia:table(module_progress),
                          P#module_progress.user_id =:= UserId,
                          P#module_progress.module_id =:= ModuleId]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, [Progress]} -> Progress;
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_user_progress_detailed(UserId, PathId) ->
    Fun = fun() ->
        Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment),
                                         E#enrollment.user_id =:= UserId,
                                         E#enrollment.path_id =:= PathId])),
        ModuleProgress = qlc:e(qlc:q([M || M <- mnesia:table(module_progress),
                                            M#module_progress.user_id =:= UserId,
                                            M#module_progress.path_id =:= PathId])),
        LessonProgress = qlc:e(qlc:q([L || L <- mnesia:table(lesson_progress),
                                            L#lesson_progress.user_id =:= UserId,
                                            L#lesson_progress.path_id =:= PathId])),
        case Enrollments of
            [] -> {error, not_enrolled};
            [Enrollment] ->
                #{
                    enrollment => Enrollment,
                    module_progress => ModuleProgress,
                    lesson_progress => LessonProgress,
                    overall_progress => Enrollment#enrollment.progress_percentage
                }
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_comment(ContentType, ContentId, UserId, Username, CommentText) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),
        Comment = #content_comment{
            id = Id,
            content_type = ContentType,
            content_id = ContentId,
            user_id = UserId,
            username = Username,
            comment_text = CommentText,
            timestamp = Now
        },
        mnesia:write(Comment),
        {ok, Id}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_comments(ContentType, ContentId) ->
    Fun = fun() ->
        qlc:e(qlc:q([C || C <- mnesia:table(content_comment),
                          C#content_comment.content_type =:= ContentType,
                          C#content_comment.content_id =:= ContentId]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, Comments} -> Comments;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_reaction(ContentType, ContentId, UserId, ReactionType) ->
    Fun = fun() ->
        Existing = qlc:e(qlc:q([R || R <- mnesia:table(content_reaction),
                                      R#content_reaction.content_type =:= ContentType,
                                      R#content_reaction.content_id =:= ContentId,
                                      R#content_reaction.user_id =:= UserId])),
        case Existing of
            [OldReaction] ->
                Updated = OldReaction#content_reaction{
                    reaction_type = ReactionType,
                    timestamp = calendar:universal_time()
                },
                mnesia:write(Updated),
                {ok, updated};
            [] ->
                Id = nanoid:gen(),
                Reaction = #content_reaction{
                    id = Id,
                    content_type = ContentType,
                    content_id = ContentId,
                    user_id = UserId,
                    reaction_type = ReactionType,
                    timestamp = calendar:universal_time()
                },
                mnesia:write(Reaction),
                {ok, Id}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_reactions(ContentType, ContentId) ->
    Fun = fun() ->
        Reactions = qlc:e(qlc:q([R || R <- mnesia:table(content_reaction),
                                       R#content_reaction.content_type =:= ContentType,
                                       R#content_reaction.content_id =:= ContentId])),
        lists:foldl(fun(R, Acc) ->
            Type = R#content_reaction.reaction_type,
            Count = maps:get(Type, Acc, 0),
            maps:put(Type, Count + 1, Acc)
        end, #{}, Reactions)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_reaction(ContentType, ContentId, UserId) ->
    Fun = fun() ->
        Reactions = qlc:e(qlc:q([R || R <- mnesia:table(content_reaction),
                                       R#content_reaction.content_type =:= ContentType,
                                       R#content_reaction.content_id =:= ContentId,
                                       R#content_reaction.user_id =:= UserId])),
        case Reactions of
            [Reaction] -> mnesia:delete({content_reaction, Reaction#content_reaction.id}), ok;
            [] -> {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

ask_question(LessonId, StudentId, QuestionText, VideoTimestamp, Tags) ->
    Fun = fun() ->
        case mnesia:read({lesson, LessonId}) of
            [] -> {error, lesson_not_found};
            [Lesson] ->
                PathId = case mnesia:read({learning_module, Lesson#lesson.module_id}) of
                    [Module] -> Module#learning_module.path_id;
                    [] -> undefined
                end,
                Id = nanoid:gen(),
                Now = calendar:universal_time(),
                Question = #student_question{
                    id = Id,
                    lesson_id = LessonId,
                    module_id = Lesson#lesson.module_id,
                    path_id = PathId,
                    student_id = StudentId,
                    question_text = QuestionText,
                    question_timestamp_seconds = VideoTimestamp,
                    tags = Tags,
                    created_at = Now,
                    updated_at = Now
                },
                mnesia:write(Question),
                {ok, Id}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

answer_question(QuestionId, ResponderId, ResponderType, AnswerText) ->
    Fun = fun() ->
        case mnesia:read({student_question, QuestionId}) of
            [] -> {error, question_not_found};
            [Question] ->
                AnswerId = nanoid:gen(),
                Now = calendar:universal_time(),
                Answer = #question_answer{
                    id = AnswerId,
                    question_id = QuestionId,
                    responder_id = ResponderId,
                    responder_type = ResponderType,
                    answer_text = AnswerText,
                    created_at = Now
                },
                mnesia:write(Answer),
                UpdatedQuestion = Question#student_question{
                    answers = [AnswerId | Question#student_question.answers],
                    status = answered,
                    updated_at = Now
                },
                mnesia:write(UpdatedQuestion),
                {ok, AnswerId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

accept_answer(QuestionId, AnswerId) ->
    Fun = fun() ->
        case {mnesia:read({student_question, QuestionId}), mnesia:read({question_answer, AnswerId})} of
            {[Question], [Answer]} ->
                UpdatedQuestion = Question#student_question{
                    accepted_answer_id = AnswerId,
                    status = closed,
                    updated_at = calendar:universal_time()
                },
                UpdatedAnswer = Answer#question_answer{accepted = true},
                mnesia:write(UpdatedQuestion),
                mnesia:write(UpdatedAnswer),
                ok;
            _ -> {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_lesson_questions(LessonId) ->
    Fun = fun() ->
        qlc:e(qlc:q([Q || Q <- mnesia:table(student_question),
                          Q#student_question.lesson_id =:= LessonId]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, Questions} -> Questions;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

upvote_question(QuestionId, UserId) ->
    Fun = fun() ->
        case mnesia:read({student_question, QuestionId}) of
            [] -> {error, not_found};
            [Question] ->
                case lists:member(UserId, Question#student_question.upvotes) of
                    true -> {error, already_upvoted};
                    false ->
                        Updated = Question#student_question{
                            upvotes = [UserId | Question#student_question.upvotes]
                        },
                        mnesia:write(Updated),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

upvote_answer(AnswerId, UserId) ->
    Fun = fun() ->
        case mnesia:read({question_answer, AnswerId}) of
            [] -> {error, not_found};
            [Answer] ->
                case lists:member(UserId, Answer#question_answer.upvotes) of
                    true -> {error, already_upvoted};
                    false ->
                        Updated = Answer#question_answer{
                            upvotes = [UserId | Answer#question_answer.upvotes]
                        },
                        mnesia:write(Updated),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_path_review(PathId, UserId, Username, Rating, ReviewTitle, ReviewText, Pros, Cons) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),
        Progress = case get_user_progress(UserId, PathId) of
            #{progress_percentage := Percentage} -> Percentage;
            _ -> 0.0
        end,
        Review = #path_review{
            id = Id,
            path_id = PathId,
            user_id = UserId,
            username = Username,
            rating = Rating,
            review_title = ReviewTitle,
            review_text = ReviewText,
            pros = Pros,
            cons = Cons,
            completion_percentage = Progress,
            created_at = Now,
            updated_at = Now
        },
        mnesia:write(Review),
        {ok, Id}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_path_reviews(PathId) ->
    Fun = fun() ->
        qlc:e(qlc:q([R || R <- mnesia:table(path_review),
                          R#path_review.path_id =:= PathId]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, Reviews} -> Reviews;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    mark_review_helpful(ReviewId, _UserId) ->
        Fun = fun() ->
            case mnesia:read({path_review, ReviewId}) of
                [] -> {error, not_found};
                [Review] ->
                    Updated = Review#path_review{
                        helpful_votes = Review#path_review.helpful_votes + 1
                    },
                    mnesia:write(Updated),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    create_upload_session(LessonId, UserId, Filename, TotalSizeBytes) ->
        Fun = fun() ->
            Id = nanoid:gen(),
            Now = calendar:universal_time(),
            ChunksTotal = erlang:ceil(TotalSizeBytes / ?CHUNK_SIZE),
            Session = #video_upload_session{
                id = Id,
                lesson_id = LessonId,
                user_id = UserId,
                filename = Filename,
                total_size_bytes = TotalSizeBytes,
                chunks_total = ChunksTotal,
                started_at = Now
            },
            mnesia:write(Session),
            {ok, Id}
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

update_upload_progress(SessionId, ChunkNumber, UploadedBytes) ->
    Fun = fun() ->
        case mnesia:read({video_upload_session, SessionId}) of
            [] -> {error, not_found};
            [Session] ->
                Updated = Session#video_upload_session{
                    uploaded_bytes = Session#video_upload_session.uploaded_bytes + UploadedBytes,
                    chunks_uploaded = [ChunkNumber | Session#video_upload_session.chunks_uploaded]
                },
                mnesia:write(Updated),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

complete_upload(SessionId, FinalCID) ->
    Fun = fun() ->
        case mnesia:read({video_upload_session, SessionId}) of
            [] -> {error, not_found};
            [Session] ->
                Now = calendar:universal_time(),
                Updated = Session#video_upload_session{
                    status = completed,
                    final_cid = FinalCID,
                    completed_at = Now
                },
                mnesia:write(Updated),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_upload_session(SessionId) ->
    Fun = fun() -> mnesia:read({video_upload_session, SessionId}) end,
    case mnesia:transaction(Fun) of
        {atomic, [Session]} -> Session;
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    record_content_view(_UserId, ContentType, ContentId) ->
        Fun = fun() ->
            Now = calendar:universal_time(),
            Date = element(1, Now),
            Analytics = case qlc:e(qlc:q([A || A <- mnesia:table(content_analytics),
                                               A#content_analytics.content_type =:= ContentType,
                                               A#content_analytics.content_id =:= ContentId,
                                               A#content_analytics.date =:= Date])) of
                [Existing] ->
                    Existing#content_analytics{
                        views = Existing#content_analytics.views + 1
                    };
                [] ->
                    #content_analytics{
                        content_type = ContentType,
                        content_id = ContentId,
                        date = Date,
                        views = 1
                    }
            end,
            mnesia:write(Analytics),
            ok
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

get_content_analytics(ContentType, ContentId) ->
    Fun = fun() ->
        qlc:e(qlc:q([A || A <- mnesia:table(content_analytics),
                          A#content_analytics.content_type =:= ContentType,
                          A#content_analytics.content_id =:= ContentId]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, Analytics} -> Analytics;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_instructor_analytics(InstructorId, Date) ->
    Fun = fun() ->
        qlc:e(qlc:q([A || A <- mnesia:table(instructor_analytics),
                          A#instructor_analytics.instructor_id =:= InstructorId,
                          A#instructor_analytics.date =:= Date]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, [Analytics]} -> Analytics;
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

increment_view_count(ContentType, ContentId) ->
    Fun = fun() ->
        case ContentType of
            lesson ->
                case mnesia:read({lesson, ContentId}) of
                    [L] -> mnesia:write(L#lesson{view_count = L#lesson.view_count + 1}), ok;
                    [] -> {error, not_found}
                end;
            _ -> {error, unsupported_content_type}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

increment_completion_count(ContentType, ContentId) ->
    Fun = fun() ->
        case ContentType of
            lesson ->
                case mnesia:read({lesson, ContentId}) of
                    [L] -> mnesia:write(L#lesson{completion_count = L#lesson.completion_count + 1}), ok;
                    [] -> {error, not_found}
                end;
            learning_module ->
                case mnesia:read({learning_module, ContentId}) of
                    [M] -> mnesia:write(M#learning_module{completion_count = M#learning_module.completion_count + 1}), ok;
                    [] -> {error, not_found}
                end;
            learning_path ->
                case mnesia:read({learning_path, ContentId}) of
                    [P] -> mnesia:write(P#learning_path{completion_count = P#learning_path.completion_count + 1}), ok;
                    [] -> {error, not_found}
                end;
            _ -> {error, unsupported_content_type}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_notification(UserId, Type, Title, Message, LinkType, LinkId, Priority) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),
        Notification = #learning_notification{
            id = Id,
            user_id = UserId,
            type = Type,
            title = Title,
            message = Message,
            link_type = LinkType,
            link_id = LinkId,
            priority = Priority,
            created_at = Now
        },
        mnesia:write(Notification),
        {ok, Id}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_user_notifications(UserId) ->
    Fun = fun() ->
        qlc:e(qlc:q([N || N <- mnesia:table(learning_notification),
                          N#learning_notification.user_id =:= UserId]))
    end,
    case mnesia:transaction(Fun) of
        {atomic, Notifications} -> lists:sort(fun(A, B) ->
            A#learning_notification.created_at > B#learning_notification.created_at
        end, Notifications);
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

mark_notification_read(NotificationId) ->
    Fun = fun() ->
        case mnesia:read({learning_notification, NotificationId}) of
            [] -> {error, not_found};
            [Notification] ->
                Updated = Notification#learning_notification{
                    read = true,
                    read_at = calendar:universal_time()
                },
                mnesia:write(Updated),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    enroll_user_enhanced(UserId, PathId) ->
        Fun = fun() ->
            EnrollmentId = enroll_user(UserId, PathId),
            _ProgressId = nanoid:gen(),
            Now = calendar:universal_time(),
            Progress = #user_learning_progress{
                user_id = UserId,
                path_id = PathId,
                enrollment_date = Now,
                status = enrolled
            },
            mnesia:write(Progress),
            {ok, EnrollmentId}
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

        get_next_lesson(_UserId, CurrentLessonId) ->
            Fun = fun() ->
                case mnesia:read({lesson, CurrentLessonId}) of
                    [] -> {error, not_found};
                    [Lesson] ->
                        case Lesson#lesson.next_lesson_id of
                            undefined ->
                                AllLessons = qlc:e(qlc:q([L || L <- mnesia:table(lesson),
                                                                L#lesson.module_id =:= Lesson#lesson.module_id])),
                                Sorted = lists:sort(fun(A, B) -> A#lesson.lesson_number < B#lesson.lesson_number end, AllLessons),
                                case lists:dropwhile(fun(L) -> L#lesson.id =/= CurrentLessonId end, Sorted) of
                                    [_, NextLesson | _] -> NextLesson;
                                    _ -> {error, no_next_lesson}
                                end;
                            NextId ->
                                case mnesia:read({lesson, NextId}) of
                                    [NextLesson] -> NextLesson;
                                    [] -> {error, not_found}
                                end
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

unlock_next_module(UserId, CurrentModuleId) ->
    Fun = fun() ->
        case mnesia:read({learning_module, CurrentModuleId}) of
            [] -> {error, not_found};
            [Module] ->
                AllModules = qlc:e(qlc:q([M || M <- mnesia:table(learning_module),
                                               M#learning_module.path_id =:= Module#learning_module.path_id])),
                Sorted = lists:sort(fun(A, B) -> A#learning_module.module_number < B#learning_module.module_number end, AllModules),
                case lists:dropwhile(fun(M) -> M#learning_module.id =/= CurrentModuleId end, Sorted) of
                    [_, NextModule | _] ->
                        case create_module_progress(UserId, NextModule#learning_module.id) of
                            {ok, _} -> {ok, NextModule};
                            Error -> Error
                        end;
                    _ -> {error, no_next_module}
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_learning_statistics(UserId) ->
    Fun = fun() ->
        Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId])),
        LessonProgress = qlc:e(qlc:q([L || L <- mnesia:table(lesson_progress), L#lesson_progress.user_id =:= UserId])),
        Completed = length([L || L <- LessonProgress, L#lesson_progress.status =:= completed]),
        TotalTime = lists:foldl(fun(L, Sum) -> Sum + L#lesson_progress.time_spent_seconds end, 0, LessonProgress),
        #{
            total_enrollments => length(Enrollments),
            lessons_completed => Completed,
            total_time_seconds => TotalTime,
            average_progress => case Enrollments of
                [] -> 0.0;
                _ -> lists:sum([E#enrollment.progress_percentage || E <- Enrollments]) / length(Enrollments)
            end
        }
    end,
    case mnesia:transaction(Fun) of
        {atomic, Stats} -> Stats;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_popular_paths(Limit) ->
    Fun = fun() ->
        AllPaths = qlc:e(qlc:q([P || P <- mnesia:table(learning_path),
                                      P#learning_path.approval_status =:= approved])),
        Sorted = lists:sort(fun(A, B) ->
            A#learning_path.enrollment_count > B#learning_path.enrollment_count
        end, AllPaths),
        lists:sublist(Sorted, Limit)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Paths} -> Paths;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_completion_leaderboard(PathId, Limit) ->
    Fun = fun() ->
        LessonProgress = qlc:e(qlc:q([L || L <- mnesia:table(lesson_progress),
                                            L#lesson_progress.path_id =:= PathId])),
        UserCompletions = lists:foldl(fun(Progress, Acc) ->
            UserId = Progress#lesson_progress.user_id,
            Count = maps:get(UserId, Acc, 0),
            maps:put(UserId, Count + 1, Acc)
        end, #{}, LessonProgress),
        Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, maps:to_list(UserCompletions)),
        lists:sublist(Sorted, Limit)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Leaderboard} -> Leaderboard;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    create_learning_resource(CreatorId, CreatorName, CreatorFamily, Title, Description, Type, Content,
                             Level, Duration, Tags, LearningPathId, ModuleNumber, LessonNumber,
                             VideoData, AudioData, SlidesData, _ImagesData, TextContentData) ->
        case can_create_content(CreatorId) of
            false -> {error, not_verified_instructor};
            true ->
                Fun = fun() ->
                    Id = nanoid:gen(), Now = calendar:universal_time(),
                    ok = content_cache:set({resource_content, Id}, Content),
                    if VideoData =/= undefined -> ok = content_cache:set({resource_video, Id}, VideoData); true -> ok end,
                    if AudioData =/= undefined -> ok = content_cache:set({resource_audio, Id}, AudioData); true -> ok end,
                    if SlidesData =/= undefined -> ok = content_cache:set({resource_slides, Id}, SlidesData); true -> ok end,
                    if TextContentData =/= undefined -> ok = content_cache:set({resource_text, Id}, TextContentData); true -> ok end,
                    Resource = #learning_resource{id = Id, creator_id = CreatorId, creator_name = CreatorName, creator_family = CreatorFamily,
                        title = Title, description = Description, type = Type, content_cid = {pending, Id}, level = Level, duration = Duration,
                        tags = Tags, learning_path_id = LearningPathId, module_number = ModuleNumber, lesson_number = LessonNumber,
                        video_lectures_cid = if VideoData =/= undefined -> {pending, Id}; true -> undefined end,
                        audio_lectures_cid = if AudioData =/= undefined -> {pending, Id}; true -> undefined end,
                        slides_cid = if SlidesData =/= undefined -> {pending, Id}; true -> undefined end,
                        text_content_cid = if TextContentData =/= undefined -> {pending, Id}; true -> undefined end,
                        approval_status = approved, visibility = public, date_created = Now, date_updated = Now},
                    mnesia:write(Resource), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> spawn(fun() -> upload_resource_content(Id, VideoData, AudioData, SlidesData, TextContentData) end), Id;
                    {atomic, {error, Reason}} -> {error, Reason}; {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end
        end.

    upload_resource_content(Id, VideoData, AudioData, SlidesData, TextContentData) ->
        Content = content_cache:get({resource_content, Id}), ContentCID = ipfs_content:upload_text(Content),
        VideoCID = if VideoData =/= undefined -> VD = content_cache:get({resource_video, Id}), ipfs_video:upload_video(VD); true -> undefined end,
        AudioCID = if AudioData =/= undefined -> AD = content_cache:get({resource_audio, Id}), ipfs_media:upload_media(AD); true -> undefined end,
        SlidesCID = if SlidesData =/= undefined -> SD = content_cache:get({resource_slides, Id}), ipfs_media:upload_media(SD); true -> undefined end,
        TextCID = if TextContentData =/= undefined -> TD = content_cache:get({resource_text, Id}), ipfs_content:upload_text(TD); true -> undefined end,
        UpdateF = fun() -> case mnesia:read({learning_resource, Id}) of
            [R] -> mnesia:write(R#learning_resource{content_cid = ContentCID, video_lectures_cid = VideoCID, audio_lectures_cid = AudioCID, slides_cid = SlidesCID, text_content_cid = TextCID}); [] -> ok end end,
        mnesia:transaction(UpdateF),
        content_cache:delete({resource_content, Id}), content_cache:delete({resource_video, Id}),
        content_cache:delete({resource_audio, Id}), content_cache:delete({resource_slides, Id}), content_cache:delete({resource_text, Id}).

    update_learning_resource(ResourceId, CreatorId, NewTitle, NewDescription, NewType, NewContent,
                             NewLevel, NewDuration, NewTags, NewLearningPathId, NewModuleNumber, NewLessonNumber,
                             NewVideoData, NewAudioData, NewSlidesData, _NewImagesData, NewTextContentData,
                             NewPrerequisites, NewLearningObjectives) ->
        Fun = fun() ->
            case mnesia:read({learning_resource, ResourceId}) of
                [] -> {error, not_found};
                [Resource] -> case Resource#learning_resource.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),
                        ContentCID = if NewContent =/= undefined -> content_cache:set({res_cont_upd, ResourceId}, NewContent), {pending_update, ResourceId}; true -> Resource#learning_resource.content_cid end,
                        VideoCID = if NewVideoData =/= undefined -> content_cache:set({res_vid_upd, ResourceId}, NewVideoData), {pending_update, ResourceId}; true -> Resource#learning_resource.video_lectures_cid end,
                        AudioCID = if NewAudioData =/= undefined -> content_cache:set({res_aud_upd, ResourceId}, NewAudioData), {pending_update, ResourceId}; true -> Resource#learning_resource.audio_lectures_cid end,
                        SlidesCID = if NewSlidesData =/= undefined -> content_cache:set({res_sld_upd, ResourceId}, NewSlidesData), {pending_update, ResourceId}; true -> Resource#learning_resource.slides_cid end,
                        TextCID = if NewTextContentData =/= undefined -> content_cache:set({res_txt_upd, ResourceId}, NewTextContentData), {pending_update, ResourceId}; true -> Resource#learning_resource.text_content_cid end,
                        Updated = Resource#learning_resource{title = NewTitle, description = NewDescription, type = NewType, content_cid = ContentCID,
                            level = NewLevel, duration = NewDuration, tags = NewTags, learning_path_id = NewLearningPathId, module_number = NewModuleNumber,
                            lesson_number = NewLessonNumber, video_lectures_cid = VideoCID, audio_lectures_cid = AudioCID, slides_cid = SlidesCID,
                            text_content_cid = TextCID, prerequisites = NewPrerequisites, learning_objectives = NewLearningObjectives,
                            date_updated = Now},
                        mnesia:write(Updated),
                        spawn(fun() ->
                            if NewContent =/= undefined -> C = content_cache:get({res_cont_upd, ResourceId}), CCID = ipfs_content:upload_text(C), update_resource_field(ResourceId, content_cid, CCID), content_cache:delete({res_cont_upd, ResourceId}); true -> ok end,
                            if NewVideoData =/= undefined -> V = content_cache:get({res_vid_upd, ResourceId}), VCID = ipfs_video:upload_video(V), update_resource_field(ResourceId, video_lectures_cid, VCID), content_cache:delete({res_vid_upd, ResourceId}); true -> ok end,
                            if NewAudioData =/= undefined -> A = content_cache:get({res_aud_upd, ResourceId}), ACID = ipfs_media:upload_media(A), update_resource_field(ResourceId, audio_lectures_cid, ACID), content_cache:delete({res_aud_upd, ResourceId}); true -> ok end,
                            if NewSlidesData =/= undefined -> S = content_cache:get({res_sld_upd, ResourceId}), SCID = ipfs_media:upload_media(S), update_resource_field(ResourceId, slides_cid, SCID), content_cache:delete({res_sld_upd, ResourceId}); true -> ok end,
                            if NewTextContentData =/= undefined -> T = content_cache:get({res_txt_upd, ResourceId}), TCID = ipfs_content:upload_text(T), update_resource_field(ResourceId, text_content_cid, TCID), content_cache:delete({res_txt_upd, ResourceId}); true -> ok end
                        end), ok;
                    _ -> {error, unauthorized}
                end
            end
        end,
        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

    update_resource_field(ResourceId, Field, Value) ->
        F = fun() -> case mnesia:read({learning_resource, ResourceId}) of
            [R] -> Updated = case Field of content_cid -> R#learning_resource{content_cid = Value}; video_lectures_cid -> R#learning_resource{video_lectures_cid = Value};
                audio_lectures_cid -> R#learning_resource{audio_lectures_cid = Value}; slides_cid -> R#learning_resource{slides_cid = Value};
                text_content_cid -> R#learning_resource{text_content_cid = Value}; ipns -> R#learning_resource{ipns = Value} end,
                mnesia:write(Updated); [] -> ok end end,
        mnesia:transaction(F).

    delete_learning_resource(ResourceId, UserId) ->
        Fun = fun() -> case mnesia:read({learning_resource, ResourceId}) of
            [] -> {error, not_found};
            [Resource] -> case Resource#learning_resource.creator_id of UserId -> mnesia:delete({learning_resource, ResourceId}), ok; _ -> {error, unauthorized} end end end,
        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

    create_module(PathId, CreatorId, CreatorName, CreatorFamily, Title, Description, ModuleNumber,
                  EstimatedHours, Prerequisites, LearningObjectives, IntroVideoData) ->
        case can_create_content(CreatorId) of
            false -> {error, not_verified_instructor};
            true ->
                Fun = fun() ->
                    Id = nanoid:gen(), Now = calendar:universal_time(),
                    ok = content_cache:set({module_description, Id}, Description),
                    if IntroVideoData =/= undefined -> ok = content_cache:set({module_intro_video, Id}, IntroVideoData); true -> ok end,
                    Module = #learning_module{id = Id, path_id = PathId, creator_id = CreatorId, creator_name = CreatorName, creator_family = CreatorFamily,
                        title = Title, description = {pending, Id}, module_number = ModuleNumber, estimated_hours = EstimatedHours,
                        prerequisites = Prerequisites, learning_objectives = LearningObjectives,
                        intro_video_cid = if IntroVideoData =/= undefined -> {pending, Id}; true -> undefined end,
                        approval_status = approved, visibility = public, date_created = Now, date_updated = Now},
                    mnesia:write(Module), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} ->
                        spawn(fun() ->
                            Desc = content_cache:get({module_description, Id}), DescCID = ipfs_content:upload_text(Desc),
                            IntroCID = if IntroVideoData =/= undefined -> IV = content_cache:get({module_intro_video, Id}), ipfs_video:upload_video(IV); true -> undefined end,
                            UpdateF = fun() -> case mnesia:read({learning_module, Id}) of
                                [M] -> mnesia:write(M#learning_module{description = DescCID, intro_video_cid = IntroCID}); [] -> ok end end,
                            mnesia:transaction(UpdateF),
                            content_cache:delete({module_description, Id}), content_cache:delete({module_intro_video, Id})
                        end), Id;
                    {atomic, {error, Reason}} -> {error, Reason}; {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end
        end.

    update_module(ModuleId, CreatorId, NewTitle, NewDescription, NewModuleNumber, NewEstimatedHours,
                  NewPrerequisites, NewLearningObjectives, NewIntroVideoData, NewThumbnailData,
                  _NewResourcesData, NewAssessmentId) ->
        Fun = fun() -> case mnesia:read({learning_module, ModuleId}) of
            [] -> {error, not_found};
            [Module] -> case Module#learning_module.creator_id of
                CreatorId ->
                    Now = calendar:universal_time(),
                    DescCID = if NewDescription =/= undefined -> content_cache:set({mod_desc_upd, ModuleId}, NewDescription), {pending_update, ModuleId}; true -> Module#learning_module.description end,
                    IntroCID = if NewIntroVideoData =/= undefined -> content_cache:set({mod_intro_upd, ModuleId}, NewIntroVideoData), {pending_update, ModuleId}; true -> Module#learning_module.intro_video_cid end,
                    ThumbCID = if NewThumbnailData =/= undefined -> content_cache:set({mod_thumb_upd, ModuleId}, NewThumbnailData), {pending_update, ModuleId}; true -> Module#learning_module.thumbnail_cid end,
                    Updated = Module#learning_module{title = NewTitle, description = DescCID, module_number = NewModuleNumber,
                        estimated_hours = NewEstimatedHours, prerequisites = NewPrerequisites, learning_objectives = NewLearningObjectives,
                        intro_video_cid = IntroCID, thumbnail_cid = ThumbCID, assessment_id = NewAssessmentId,
                        date_updated = Now},
                    mnesia:write(Updated),
                    spawn(fun() ->
                        if NewDescription =/= undefined -> D = content_cache:get({mod_desc_upd, ModuleId}), DCID = ipfs_content:upload_text(D), update_module_field(ModuleId, description, DCID), content_cache:delete({mod_desc_upd, ModuleId}); true -> ok end,
                        if NewIntroVideoData =/= undefined -> I = content_cache:get({mod_intro_upd, ModuleId}), ICID = ipfs_video:upload_video(I), update_module_field(ModuleId, intro_video_cid, ICID), content_cache:delete({mod_intro_upd, ModuleId}); true -> ok end,
                        if NewThumbnailData =/= undefined -> T = content_cache:get({mod_thumb_upd, ModuleId}), TCID = ipfs_media:upload_media(T), update_module_field(ModuleId, thumbnail_cid, TCID), content_cache:delete({mod_thumb_upd, ModuleId}); true -> ok end
                    end), ok;
                _ -> {error, unauthorized}
            end end end,
        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

    update_module_field(ModuleId, Field, Value) ->
        F = fun() -> case mnesia:read({learning_module, ModuleId}) of
            [M] -> Updated = case Field of description -> M#learning_module{description = Value}; intro_video_cid -> M#learning_module{intro_video_cid = Value};
                thumbnail_cid -> M#learning_module{thumbnail_cid = Value}; ipns -> M#learning_module{ipns = Value} end,
                mnesia:write(Updated); [] -> ok end end,
        mnesia:transaction(F).

    delete_module(ModuleId, UserId) ->
        Fun = fun() -> case mnesia:read({learning_module, ModuleId}) of
            [] -> {error, not_found};
            [Module] -> case Module#learning_module.creator_id of UserId -> mnesia:delete({learning_module, ModuleId}), ok; _ -> {error, unauthorized} end end end,
        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

    create_lesson(ModuleId, CreatorId, CreatorName, CreatorFamily, Title, Description, LessonNumber,
                  ContentType, Duration, VideoData, AudioData, SlidesData, TextContentData,
                  TranscriptData, CodeSamples, Prerequisites, QuizId, CompletionCriteria) ->
        case can_create_content(CreatorId) of
            false -> {error, not_verified_instructor};
            true ->
                Fun = fun() ->
                    PathId = case mnesia:read({learning_module, ModuleId}) of
                        [Module] -> Module#learning_module.path_id;
                        [] -> undefined
                    end,
                    Id = nanoid:gen(), Now = calendar:universal_time(),
                    ok = content_cache:set({lesson_description, Id}, Description),
                    if VideoData =/= undefined -> ok = content_cache:set({lesson_video, Id}, VideoData); true -> ok end,
                    if AudioData =/= undefined -> ok = content_cache:set({lesson_audio, Id}, AudioData); true -> ok end,
                    if SlidesData =/= undefined -> ok = content_cache:set({lesson_slides, Id}, SlidesData); true -> ok end,
                    if TextContentData =/= undefined -> ok = content_cache:set({lesson_text, Id}, TextContentData); true -> ok end,
                    if TranscriptData =/= undefined -> ok = content_cache:set({lesson_transcript, Id}, TranscriptData); true -> ok end,
                    Lesson = #lesson{id = Id, module_id = ModuleId, path_id = PathId, creator_id = CreatorId, creator_name = CreatorName, creator_family = CreatorFamily,
                        title = Title, description = {pending, Id}, lesson_number = LessonNumber, content_type = ContentType, content_cid = {pending, Id},
                        duration_minutes = Duration, video_cid = if VideoData =/= undefined -> {pending, Id}; true -> undefined end,
                        audio_cid = if AudioData =/= undefined -> {pending, Id}; true -> undefined end,
                        slides_cid = if SlidesData =/= undefined -> {pending, Id}; true -> undefined end,
                        text_content_cid = if TextContentData =/= undefined -> {pending, Id}; true -> undefined end,
                        transcript_cid = if TranscriptData =/= undefined -> {pending, Id}; true -> undefined end,
                        code_samples = CodeSamples, quiz_id = QuizId, prerequisites = Prerequisites, completion_criteria = CompletionCriteria,
                        approval_status = approved, visibility = public, content_status = processing, date_created = Now, date_updated = Now},
                    mnesia:write(Lesson), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> spawn(fun() -> upload_lesson_content(Id, VideoData, AudioData, SlidesData, TextContentData, TranscriptData) end), Id;
                    {atomic, {error, Reason}} -> {error, Reason}; {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end
        end.

    upload_lesson_content(Id, VideoData, AudioData, SlidesData, TextContentData, TranscriptData) ->
        Desc = content_cache:get({lesson_description, Id}), DescCID = ipfs_content:upload_text(Desc),
        VideoCID = if VideoData =/= undefined -> V = content_cache:get({lesson_video, Id}), ipfs_video:upload_video(V); true -> undefined end,
        AudioCID = if AudioData =/= undefined -> A = content_cache:get({lesson_audio, Id}), ipfs_media:upload_media(A); true -> undefined end,
        SlidesCID = if SlidesData =/= undefined -> S = content_cache:get({lesson_slides, Id}), ipfs_media:upload_media(S); true -> undefined end,
        TextCID = if TextContentData =/= undefined -> T = content_cache:get({lesson_text, Id}), ipfs_content:upload_text(T); true -> undefined end,
        TranscriptCID = if TranscriptData =/= undefined -> TR = content_cache:get({lesson_transcript, Id}), ipfs_content:upload_text(TR); true -> undefined end,
        UpdateF = fun() -> case mnesia:read({lesson, Id}) of
            [L] -> mnesia:write(L#lesson{description = DescCID, content_cid = DescCID, video_cid = VideoCID, audio_cid = AudioCID,
                slides_cid = SlidesCID, text_content_cid = TextCID, transcript_cid = TranscriptCID, content_status = ready}); [] -> ok end end,
        mnesia:transaction(UpdateF),
        content_cache:delete({lesson_description, Id}), content_cache:delete({lesson_video, Id}), content_cache:delete({lesson_audio, Id}),
        content_cache:delete({lesson_slides, Id}), content_cache:delete({lesson_text, Id}), content_cache:delete({lesson_transcript, Id}).

    update_lesson(LessonId, CreatorId, NewTitle, NewDescription, NewLessonNumber, NewContentType, NewDuration,
                  NewVideoData, NewAudioData, NewSlidesData, NewTextContentData, NewTranscriptData,
                  NewCodeSamples, NewPrerequisites, NewQuizId, NewCompletionCriteria, NewNextLessonId,
                  NewPrevLessonId, NewThumbnailData) ->
        Fun = fun() -> case mnesia:read({lesson, LessonId}) of
            [] -> {error, not_found};
            [Lesson] -> case Lesson#lesson.creator_id of
                CreatorId ->
                    Now = calendar:universal_time(),
                    DescCID = if NewDescription =/= undefined -> content_cache:set({les_desc_upd, LessonId}, NewDescription), {pending_update, LessonId}; true -> Lesson#lesson.description end,
                    VideoCID = if NewVideoData =/= undefined -> content_cache:set({les_vid_upd, LessonId}, NewVideoData), {pending_update, LessonId}; true -> Lesson#lesson.video_cid end,
                    AudioCID = if NewAudioData =/= undefined -> content_cache:set({les_aud_upd, LessonId}, NewAudioData), {pending_update, LessonId}; true -> Lesson#lesson.audio_cid end,
                    SlidesCID = if NewSlidesData =/= undefined -> content_cache:set({les_sld_upd, LessonId}, NewSlidesData), {pending_update, LessonId}; true -> Lesson#lesson.slides_cid end,
                    TextCID = if NewTextContentData =/= undefined -> content_cache:set({les_txt_upd, LessonId}, NewTextContentData), {pending_update, LessonId}; true -> Lesson#lesson.text_content_cid end,
                    TransCID = if NewTranscriptData =/= undefined -> content_cache:set({les_trans_upd, LessonId}, NewTranscriptData), {pending_update, LessonId}; true -> Lesson#lesson.transcript_cid end,
                    ThumbCID = if NewThumbnailData =/= undefined -> content_cache:set({les_thumb_upd, LessonId}, NewThumbnailData), {pending_update, LessonId}; true -> Lesson#lesson.thumbnail_cid end,
                    Updated = Lesson#lesson{title = NewTitle, description = DescCID, lesson_number = NewLessonNumber, content_type = NewContentType,
                        duration_minutes = NewDuration, video_cid = VideoCID, audio_cid = AudioCID, slides_cid = SlidesCID,
                        text_content_cid = TextCID, transcript_cid = TransCID, code_samples = NewCodeSamples, prerequisites = NewPrerequisites,
                        quiz_id = NewQuizId, completion_criteria = NewCompletionCriteria, next_lesson_id = NewNextLessonId,
                        prev_lesson_id = NewPrevLessonId, thumbnail_cid = ThumbCID, approval_status = pending, content_status = processing,
                        date_updated = Now},
                    mnesia:write(Updated),
                    spawn(fun() ->
                        if NewDescription =/= undefined -> D = content_cache:get({les_desc_upd, LessonId}), DCID = ipfs_content:upload_text(D), update_lesson_field(LessonId, description, DCID), content_cache:delete({les_desc_upd, LessonId}); true -> ok end,
                        if NewVideoData =/= undefined -> V = content_cache:get({les_vid_upd, LessonId}), VCID = ipfs_video:upload_video(V), update_lesson_field(LessonId, video_cid, VCID), content_cache:delete({les_vid_upd, LessonId}); true -> ok end,
                        if NewAudioData =/= undefined -> A = content_cache:get({les_aud_upd, LessonId}), ACID = ipfs_media:upload_media(A), update_lesson_field(LessonId, audio_cid, ACID), content_cache:delete({les_aud_upd, LessonId}); true -> ok end,
                        if NewSlidesData =/= undefined -> S = content_cache:get({les_sld_upd, LessonId}), SCID = ipfs_media:upload_media(S), update_lesson_field(LessonId, slides_cid, SCID), content_cache:delete({les_sld_upd, LessonId}); true -> ok end,
                        if NewTextContentData =/= undefined -> T = content_cache:get({les_txt_upd, LessonId}), TCID = ipfs_content:upload_text(T), update_lesson_field(LessonId, text_content_cid, TCID), content_cache:delete({les_txt_upd, LessonId}); true -> ok end,
                        if NewTranscriptData =/= undefined -> TR = content_cache:get({les_trans_upd, LessonId}), TRCID = ipfs_content:upload_text(TR), update_lesson_field(LessonId, transcript_cid, TRCID), content_cache:delete({les_trans_upd, LessonId}); true -> ok end,
                        if NewThumbnailData =/= undefined -> TH = content_cache:get({les_thumb_upd, LessonId}), THCID = ipfs_media:upload_media(TH), update_lesson_field(LessonId, thumbnail_cid, THCID), content_cache:delete({les_thumb_upd, LessonId}); true -> ok end,
                        update_lesson_field(LessonId, content_status, ready)
                    end), ok;
                _ -> {error, unauthorized}
            end end end,
        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

    update_lesson_field(LessonId, Field, Value) ->
        F = fun() -> case mnesia:read({lesson, LessonId}) of
            [L] -> Updated = case Field of description -> L#lesson{description = Value}; video_cid -> L#lesson{video_cid = Value};
                audio_cid -> L#lesson{audio_cid = Value}; slides_cid -> L#lesson{slides_cid = Value}; text_content_cid -> L#lesson{text_content_cid = Value};
                transcript_cid -> L#lesson{transcript_cid = Value}; thumbnail_cid -> L#lesson{thumbnail_cid = Value};
                content_status -> L#lesson{content_status = Value}; ipns -> L#lesson{ipns = Value} end,
                mnesia:write(Updated); [] -> ok end end,
        mnesia:transaction(F).

    delete_lesson(LessonId, UserId) ->
        Fun = fun() -> case mnesia:read({lesson, LessonId}) of
            [] -> {error, not_found};
            [Lesson] -> case Lesson#lesson.creator_id of UserId -> mnesia:delete({lesson, LessonId}), ok; _ -> {error, unauthorized} end end end,
        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        create_quiz(ResourceId, CreatorId, CreatorName, CreatorFamily, Title, Description, Questions,
                    TimeLimit, PassingScore, AttemptsAllowed, ShuffleQuestions, ShuffleAnswers, DifficultyLevel) ->
            case can_create_content(CreatorId) of
                false -> {error, not_verified_instructor};
                true ->
                    Fun = fun() ->
                        Id = nanoid:gen(), Now = calendar:universal_time(),
                        ok = content_cache:set({quiz_questions, Id}, Questions),
                        TotalPoints = lists:foldl(fun(Q, Sum) -> Sum + maps:get(points, Q, 1.0) end, 0.0, Questions),
                        Quiz = #quiz{id = Id, resource_id = ResourceId, creator_id = CreatorId, creator_name = CreatorName, creator_family = CreatorFamily,
                            title = Title, description = Description, questions = [], questions_cid = {pending, Id}, time_limit_minutes = TimeLimit,
                            passing_score = PassingScore, attempts_allowed = AttemptsAllowed, shuffle_questions = ShuffleQuestions,
                            shuffle_answers = ShuffleAnswers, difficulty_level = DifficultyLevel, total_points = TotalPoints,
                            approval_status = approved, date_created = Now, date_updated = Now},
                        mnesia:write(Quiz), {ok, Id}
                    end,
                    case mnesia:transaction(Fun) of
                        {atomic, {ok, Id}} ->
                            spawn(fun() ->
                                Qs = content_cache:get({quiz_questions, Id}),
                                QsCID = ipfs_content:upload_text(jsx:encode(Qs)),
                                UpdateF = fun() -> case mnesia:read({quiz, Id}) of [Q] -> mnesia:write(Q#quiz{questions_cid = QsCID}); [] -> ok end end,
                                mnesia:transaction(UpdateF), content_cache:delete({quiz_questions, Id})
                            end), Id;
                        {atomic, {error, Reason}} -> {error, Reason}; {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        update_quiz(QuizId, CreatorId, NewTitle, NewDescription, NewQuestions, NewTimeLimit, NewPassingScore,
                    NewAttemptsAllowed, NewShuffleQuestions, NewShuffleAnswers, NewDifficultyLevel,
                    _NewExplanations, _NewHints, _NewFeedbackMessages) ->
            Fun = fun() -> case mnesia:read({quiz, QuizId}) of
                [] -> {error, not_found};
                [Quiz] -> case Quiz#quiz.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),
                        QuestionsCID = if NewQuestions =/= undefined ->
                            content_cache:set({quiz_qs_upd, QuizId}, NewQuestions), {pending_update, QuizId};
                            true -> Quiz#quiz.questions_cid end,
                        TotalPoints = if NewQuestions =/= undefined ->
                            lists:foldl(fun(Q, Sum) -> Sum + maps:get(points, Q, 1.0) end, 0.0, NewQuestions);
                            true -> Quiz#quiz.total_points end,
                        Updated = Quiz#quiz{title = NewTitle, description = NewDescription, questions_cid = QuestionsCID,
                            time_limit_minutes = NewTimeLimit, passing_score = NewPassingScore, attempts_allowed = NewAttemptsAllowed,
                            shuffle_questions = NewShuffleQuestions, shuffle_answers = NewShuffleAnswers, difficulty_level = NewDifficultyLevel,
                            total_points = TotalPoints, date_updated = Now},
                        mnesia:write(Updated),
                        spawn(fun() ->
                            if NewQuestions =/= undefined ->
                                Qs = content_cache:get({quiz_qs_upd, QuizId}),
                                QsCID = ipfs_content:upload_text(jsx:encode(Qs)),
                                update_quiz_field(QuizId, questions_cid, QsCID),
                                content_cache:delete({quiz_qs_upd, QuizId});
                            true -> ok end
                        end), ok;
                    _ -> {error, unauthorized}
                end end end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        update_quiz_field(QuizId, Field, Value) ->
            F = fun() -> case mnesia:read({quiz, QuizId}) of
                [Q] -> Updated = case Field of questions_cid -> Q#quiz{questions_cid = Value}; ipns -> Q#quiz{ipns = Value} end,
                    mnesia:write(Updated); [] -> ok end end,
            mnesia:transaction(F).

        delete_quiz(QuizId, UserId) ->
            Fun = fun() -> case mnesia:read({quiz, QuizId}) of
                [] -> {error, not_found};
                [Quiz] -> case Quiz#quiz.creator_id of UserId -> mnesia:delete({quiz, QuizId}), ok; _ -> {error, unauthorized} end end end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        create_exercise(ResourceId, CreatorId, CreatorName, CreatorFamily, Title, Description, Difficulty,
                        ExerciseType, ProblemStatement, StarterCode, TestCases, SolutionCode, Hints, AllowedLanguages) ->
            case can_create_content(CreatorId) of
                false -> {error, not_verified_instructor};
                true ->
                    Fun = fun() ->
                        Id = nanoid:gen(), Now = calendar:universal_time(),
                        ok = content_cache:set({ex_problem, Id}, ProblemStatement),
                        ok = content_cache:set({ex_starter, Id}, StarterCode),
                        ok = content_cache:set({ex_solution, Id}, SolutionCode),
                        Exercise = #exercise{id = Id, resource_id = ResourceId, creator_id = CreatorId, creator_name = CreatorName, creator_family = CreatorFamily,
                            title = Title, description = Description, difficulty = Difficulty, exercise_type = ExerciseType,
                            problem_statement = {pending, Id}, problem_statement_cid = {pending, Id},
                            starter_code = {pending, Id}, starter_code_cid = {pending, Id},
                            test_cases = TestCases, solution_code = {pending, Id}, solution_code_cid = {pending, Id},
                            hints = Hints, allowed_languages = AllowedLanguages, approval_status = approved,
                            date_created = Now, date_updated = Now},
                        mnesia:write(Exercise), {ok, Id}
                    end,
                    case mnesia:transaction(Fun) of
                        {atomic, {ok, Id}} ->
                            spawn(fun() ->
                                Prob = content_cache:get({ex_problem, Id}), ProbCID = ipfs_content:upload_text(Prob),
                                Start = content_cache:get({ex_starter, Id}), StartCID = ipfs_content:upload_text(Start),
                                Sol = content_cache:get({ex_solution, Id}), SolCID = ipfs_content:upload_text(Sol),
                                UpdateF = fun() -> case mnesia:read({exercise, Id}) of
                                    [E] -> mnesia:write(E#exercise{problem_statement = ProbCID, problem_statement_cid = ProbCID,
                                        starter_code = StartCID, starter_code_cid = StartCID, solution_code = SolCID, solution_code_cid = SolCID}); [] -> ok end end,
                                mnesia:transaction(UpdateF),
                                content_cache:delete({ex_problem, Id}), content_cache:delete({ex_starter, Id}), content_cache:delete({ex_solution, Id})
                            end), Id;
                        {atomic, {error, Reason}} -> {error, Reason}; {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        update_exercise(ExerciseId, CreatorId, NewTitle, NewDescription, NewDifficulty, NewExerciseType,
                        NewProblemStatement, NewStarterCode, NewTestCases, NewSolutionCode, NewHints,
                        NewAllowedLanguages, _NewTimeLimit, _NewMemoryLimit, _NewMaxSubmissions) ->
            Fun = fun() -> case mnesia:read({exercise, ExerciseId}) of
                [] -> {error, not_found};
                [Exercise] -> case Exercise#exercise.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),
                        ProbCID = if NewProblemStatement =/= undefined -> content_cache:set({ex_prob_upd, ExerciseId}, NewProblemStatement), {pending_update, ExerciseId}; true -> Exercise#exercise.problem_statement_cid end,
                        StartCID = if NewStarterCode =/= undefined -> content_cache:set({ex_start_upd, ExerciseId}, NewStarterCode), {pending_update, ExerciseId}; true -> Exercise#exercise.starter_code_cid end,
                        SolCID = if NewSolutionCode =/= undefined -> content_cache:set({ex_sol_upd, ExerciseId}, NewSolutionCode), {pending_update, ExerciseId}; true -> Exercise#exercise.solution_code_cid end,
                        Updated = Exercise#exercise{title = NewTitle, description = NewDescription, difficulty = NewDifficulty, exercise_type = NewExerciseType,
                            problem_statement = ProbCID, problem_statement_cid = ProbCID, starter_code = StartCID, starter_code_cid = StartCID,
                            test_cases = NewTestCases, solution_code = SolCID, solution_code_cid = SolCID, hints = NewHints,
                            allowed_languages = NewAllowedLanguages, date_updated = Now},
                        mnesia:write(Updated),
                        spawn(fun() ->
                            if NewProblemStatement =/= undefined -> P = content_cache:get({ex_prob_upd, ExerciseId}), PCID = ipfs_content:upload_text(P), update_exercise_field(ExerciseId, problem_statement_cid, PCID), content_cache:delete({ex_prob_upd, ExerciseId}); true -> ok end,
                            if NewStarterCode =/= undefined -> ST = content_cache:get({ex_start_upd, ExerciseId}), STCID = ipfs_content:upload_text(ST), update_exercise_field(ExerciseId, starter_code_cid, STCID), content_cache:delete({ex_start_upd, ExerciseId}); true -> ok end,
                            if NewSolutionCode =/= undefined -> SO = content_cache:get({ex_sol_upd, ExerciseId}), SOCID = ipfs_content:upload_text(SO), update_exercise_field(ExerciseId, solution_code_cid, SOCID), content_cache:delete({ex_sol_upd, ExerciseId}); true -> ok end
                        end), ok;
                    _ -> {error, unauthorized}
                end end end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        update_exercise_field(ExerciseId, Field, Value) ->
            F = fun() -> case mnesia:read({exercise, ExerciseId}) of
                [E] -> Updated = case Field of problem_statement_cid -> E#exercise{problem_statement = Value, problem_statement_cid = Value};
                    starter_code_cid -> E#exercise{starter_code = Value, starter_code_cid = Value};
                    solution_code_cid -> E#exercise{solution_code = Value, solution_code_cid = Value};
                    ipns -> E#exercise{ipns = Value} end,
                    mnesia:write(Updated); [] -> ok end end,
            mnesia:transaction(F).

        delete_exercise(ExerciseId, UserId) ->
            Fun = fun() -> case mnesia:read({exercise, ExerciseId}) of
                [] -> {error, not_found};
                [Exercise] -> case Exercise#exercise.creator_id of UserId -> mnesia:delete({exercise, ExerciseId}), ok; _ -> {error, unauthorized} end end end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        create_project(PathId, ModuleId, CreatorId, CreatorName, CreatorFamily, Title, Description, Difficulty,
                       EstimatedHours, Requirements, Deliverables, GradingRubric, DatasetsRequired, ToolsRequired, SubmissionDeadline) ->
            case can_create_content(CreatorId) of
                false -> {error, not_verified_instructor};
                true ->
                    Fun = fun() ->
                        Id = nanoid:gen(), Now = calendar:universal_time(),
                        ok = content_cache:set({proj_reqs, Id}, Requirements),
                        ok = content_cache:set({proj_rubric, Id}, GradingRubric),
                        Project = #project{id = Id, path_id = PathId, module_id = ModuleId, creator_id = CreatorId, creator_name = CreatorName, creator_family = CreatorFamily,
                            title = Title, description = Description, difficulty = Difficulty, estimated_hours = EstimatedHours,
                            requirements = [], requirements_cid = {pending, Id}, deliverables = Deliverables,
                            grading_rubric = #{}, rubric_cid = {pending, Id}, datasets_required = DatasetsRequired,
                            tools_required = ToolsRequired, submission_deadline = SubmissionDeadline,
                            approval_status = approved, date_created = Now, date_updated = Now},
                        mnesia:write(Project), {ok, Id}
                    end,
                    case mnesia:transaction(Fun) of
                        {atomic, {ok, Id}} ->
                            spawn(fun() ->
                                Reqs = content_cache:get({proj_reqs, Id}), ReqsCID = ipfs_content:upload_text(jsx:encode(Reqs)),
                                Rubric = content_cache:get({proj_rubric, Id}), RubricCID = ipfs_content:upload_text(jsx:encode(Rubric)),
                                UpdateF = fun() -> case mnesia:read({project, Id}) of
                                    [P] -> mnesia:write(P#project{requirements_cid = ReqsCID, rubric_cid = RubricCID}); [] -> ok end end,
                                mnesia:transaction(UpdateF),
                                content_cache:delete({proj_reqs, Id}), content_cache:delete({proj_rubric, Id})
                            end), Id;
                        {atomic, {error, Reason}} -> {error, Reason}; {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        update_project(ProjectId, CreatorId, NewTitle, NewDescription, NewDifficulty, NewEstimatedHours,
                       NewRequirements, NewDeliverables, NewGradingRubric, NewDatasetsRequired, NewToolsRequired,
                       NewSubmissionDeadline, _NewStarterFiles, _NewSampleSolution, _NewFAQ, _NewResourceLinks, _NewCollaborationRules) ->
            Fun = fun() -> case mnesia:read({project, ProjectId}) of
                [] -> {error, not_found};
                [Project] -> case Project#project.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),
                        ReqsCID = if NewRequirements =/= undefined -> content_cache:set({proj_reqs_upd, ProjectId}, NewRequirements), {pending_update, ProjectId}; true -> Project#project.requirements_cid end,
                        RubricCID = if NewGradingRubric =/= undefined -> content_cache:set({proj_rub_upd, ProjectId}, NewGradingRubric), {pending_update, ProjectId}; true -> Project#project.rubric_cid end,
                        Updated = Project#project{title = NewTitle, description = NewDescription, difficulty = NewDifficulty, estimated_hours = NewEstimatedHours,
                            requirements_cid = ReqsCID, deliverables = NewDeliverables, rubric_cid = RubricCID,
                            datasets_required = NewDatasetsRequired, tools_required = NewToolsRequired, submission_deadline = NewSubmissionDeadline,
                            date_updated = Now},
                        mnesia:write(Updated),
                        spawn(fun() ->
                            if NewRequirements =/= undefined -> R = content_cache:get({proj_reqs_upd, ProjectId}), RCID = ipfs_content:upload_text(jsx:encode(R)), update_project_field(ProjectId, requirements_cid, RCID), content_cache:delete({proj_reqs_upd, ProjectId}); true -> ok end,
                            if NewGradingRubric =/= undefined -> GR = content_cache:get({proj_rub_upd, ProjectId}), GRCID = ipfs_content:upload_text(jsx:encode(GR)), update_project_field(ProjectId, rubric_cid, GRCID), content_cache:delete({proj_rub_upd, ProjectId}); true -> ok end
                        end), ok;
                    _ -> {error, unauthorized}
                end end end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        update_project_field(ProjectId, Field, Value) ->
            F = fun() -> case mnesia:read({project, ProjectId}) of
                [P] -> Updated = case Field of requirements_cid -> P#project{requirements_cid = Value}; rubric_cid -> P#project{rubric_cid = Value};
                    ipns -> P#project{ipns = Value} end,
                    mnesia:write(Updated); [] -> ok end end,
            mnesia:transaction(F).

        delete_project(ProjectId, UserId) ->
            Fun = fun() -> case mnesia:read({project, ProjectId}) of
                [] -> {error, not_found};
                [Project] -> case Project#project.creator_id of UserId -> mnesia:delete({project, ProjectId}), ok; _ -> {error, unauthorized} end end end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        approve_content(AdminUsername, ContentType, ContentId, ApprovalNotes) ->
            case is_admin(AdminUsername) of
                false -> {error, unauthorized};
                true ->
                    Fun = fun() ->
                        Result = case ContentType of
                            learning_path -> case mnesia:read({learning_path, ContentId}) of
                                [P] -> mnesia:write(P#learning_path{approval_status = approved, approved_by = AdminUsername, approved_at = calendar:universal_time(), visibility = public}), ok;
                                [] -> {error, not_found} end;
                            learning_resource -> case mnesia:read({learning_resource, ContentId}) of
                                [R] -> mnesia:write(R#learning_resource{approval_status = approved, approved_by = AdminUsername, approved_at = calendar:universal_time(), visibility = public}), ok;
                                [] -> {error, not_found} end;
                            learning_module -> case mnesia:read({learning_module, ContentId}) of
                                [M] -> mnesia:write(M#learning_module{approval_status = approved, approved_by = AdminUsername, approved_at = calendar:universal_time(), visibility = public}), ok;
                                [] -> {error, not_found} end;
                            lesson -> case mnesia:read({lesson, ContentId}) of
                                [L] -> mnesia:write(L#lesson{approval_status = approved, approved_by = AdminUsername, approved_at = calendar:universal_time(), visibility = public}), ok;
                                [] -> {error, not_found} end;
                            quiz -> case mnesia:read({quiz, ContentId}) of
                                [Q] -> mnesia:write(Q#quiz{approval_status = approved, approved_by = AdminUsername, approved_at = calendar:universal_time()}), ok;
                                [] -> {error, not_found} end;
                            exercise -> case mnesia:read({exercise, ContentId}) of
                                [E] -> mnesia:write(E#exercise{approval_status = approved, approved_by = AdminUsername, approved_at = calendar:universal_time()}), ok;
                                [] -> {error, not_found} end;
                            project -> case mnesia:read({project, ContentId}) of
                                [P] -> mnesia:write(P#project{approval_status = approved, approved_by = AdminUsername, approved_at = calendar:universal_time()}), ok;
                                [] -> {error, not_found} end;
                            _ -> {error, invalid_content_type}
                        end,
                        case Result of
                            ok -> log_admin_action(AdminUsername, approve_content, ContentType, ContentId, ApprovalNotes, calendar:universal_time(), #{});
                            _ -> ok
                        end,
                        Result
                    end,
                    case mnesia:transaction(Fun) of {atomic, R} -> R; {aborted, Reason} -> {error, {transaction_failed, Reason}} end
            end.

        reject_content(AdminUsername, ContentType, ContentId, RejectionReason, RejectionNotes) ->
            case is_admin(AdminUsername) of
                false -> {error, unauthorized};
                true ->
                    Fun = fun() ->
                        Result = case ContentType of
                            learning_path -> case mnesia:read({learning_path, ContentId}) of
                                [P] -> mnesia:write(P#learning_path{approval_status = rejected, rejection_reason = RejectionReason}), ok;
                                [] -> {error, not_found} end;
                            learning_resource -> case mnesia:read({learning_resource, ContentId}) of
                                [R] -> mnesia:write(R#learning_resource{approval_status = rejected}), ok;
                                [] -> {error, not_found} end;
                            learning_module -> case mnesia:read({learning_module, ContentId}) of
                                [M] -> mnesia:write(M#learning_module{approval_status = rejected}), ok;
                                [] -> {error, not_found} end;
                            lesson -> case mnesia:read({lesson, ContentId}) of
                                [L] -> mnesia:write(L#lesson{approval_status = rejected}), ok;
                                [] -> {error, not_found} end;
                            quiz -> case mnesia:read({quiz, ContentId}) of
                                [Q] -> mnesia:write(Q#quiz{approval_status = rejected}), ok;
                                [] -> {error, not_found} end;
                            exercise -> case mnesia:read({exercise, ContentId}) of
                                [E] -> mnesia:write(E#exercise{approval_status = rejected}), ok;
                                [] -> {error, not_found} end;
                            project -> case mnesia:read({project, ContentId}) of
                                [P] -> mnesia:write(P#project{approval_status = rejected}), ok;
                                [] -> {error, not_found} end;
                            _ -> {error, invalid_content_type}
                        end,
                        case Result of
                            ok -> log_admin_action(AdminUsername, reject_content, ContentType, ContentId, RejectionNotes, calendar:universal_time(), #{reason => RejectionReason});
                            _ -> ok
                        end,
                        Result
                    end,
                    case mnesia:transaction(Fun) of {atomic, R} -> R; {aborted, Reason} -> {error, {transaction_failed, Reason}} end
            end.

        flag_content(AdminUsername, ContentType, ContentId, FlagReason, FlagNotes) ->
            case is_admin(AdminUsername) of
                false -> {error, unauthorized};
                true ->
                    Fun = fun() ->
                        Result = case ContentType of
                            learning_path -> case mnesia:read({learning_path, ContentId}) of
                                [P] -> mnesia:write(P#learning_path{flagged = true, flag_reason = FlagReason}), ok;
                                [] -> {error, not_found} end;
                            _ -> {error, invalid_content_type}
                        end,
                        case Result of
                            ok -> log_admin_action(AdminUsername, flag_content, ContentType, ContentId, FlagNotes, calendar:universal_time(), #{reason => FlagReason});
                            _ -> ok
                        end,
                        Result
                    end,
                    case mnesia:transaction(Fun) of {atomic, R} -> R; {aborted, Reason} -> {error, {transaction_failed, Reason}} end
            end.

        unflag_content(AdminUsername, ContentType, ContentId) ->
            case is_admin(AdminUsername) of
                false -> {error, unauthorized};
                true ->
                    Fun = fun() ->
                        case ContentType of
                            learning_path -> case mnesia:read({learning_path, ContentId}) of
                                [P] -> mnesia:write(P#learning_path{flagged = false, flag_reason = undefined}), ok;
                                [] -> {error, not_found} end;
                            _ -> {error, invalid_content_type}
                        end
                    end,
                    case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end
            end.

        get_pending_approvals(ContentType) ->
            Fun = fun() ->
                case ContentType of
                    learning_path -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= pending]));
                    learning_resource -> qlc:e(qlc:q([R || R <- mnesia:table(learning_resource), R#learning_resource.approval_status =:= pending]));
                    learning_module -> qlc:e(qlc:q([M || M <- mnesia:table(learning_module), M#learning_module.approval_status =:= pending]));
                    lesson -> qlc:e(qlc:q([L || L <- mnesia:table(lesson), L#lesson.approval_status =:= pending]));
                    quiz -> qlc:e(qlc:q([Q || Q <- mnesia:table(quiz), Q#quiz.approval_status =:= pending]));
                    exercise -> qlc:e(qlc:q([E || E <- mnesia:table(exercise), E#exercise.approval_status =:= pending]));
                    project -> qlc:e(qlc:q([P || P <- mnesia:table(project), P#project.approval_status =:= pending]));
                    all -> #{paths => qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= pending])),
                             resources => qlc:e(qlc:q([R || R <- mnesia:table(learning_resource), R#learning_resource.approval_status =:= pending])),
                             modules => qlc:e(qlc:q([M || M <- mnesia:table(learning_module), M#learning_module.approval_status =:= pending])),
                             lessons => qlc:e(qlc:q([L || L <- mnesia:table(lesson), L#lesson.approval_status =:= pending])),
                             quizzes => qlc:e(qlc:q([Q || Q <- mnesia:table(quiz), Q#quiz.approval_status =:= pending])),
                             exercises => qlc:e(qlc:q([E || E <- mnesia:table(exercise), E#exercise.approval_status =:= pending])),
                             projects => qlc:e(qlc:q([P || P <- mnesia:table(project), P#project.approval_status =:= pending]))};
                    _ -> {error, invalid_content_type}
                end
            end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_flagged_content(ContentType) ->
            Fun = fun() ->
                case ContentType of
                    learning_path -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.flagged =:= true]));
                    all -> #{paths => qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.flagged =:= true]))};
                    _ -> {error, invalid_content_type}
                end
            end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_creator_content(CreatorId, ContentType) ->
            Fun = fun() ->
                case ContentType of
                    learning_path -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.creator_id =:= CreatorId]));
                    learning_resource -> qlc:e(qlc:q([R || R <- mnesia:table(learning_resource), R#learning_resource.creator_id =:= CreatorId]));
                    learning_module -> qlc:e(qlc:q([M || M <- mnesia:table(learning_module), M#learning_module.creator_id =:= CreatorId]));
                    lesson -> qlc:e(qlc:q([L || L <- mnesia:table(lesson), L#lesson.creator_id =:= CreatorId]));
                    all -> #{paths => qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.creator_id =:= CreatorId])),
                             resources => qlc:e(qlc:q([R || R <- mnesia:table(learning_resource), R#learning_resource.creator_id =:= CreatorId])),
                             modules => qlc:e(qlc:q([M || M <- mnesia:table(learning_module), M#learning_module.creator_id =:= CreatorId])),
                             lessons => qlc:e(qlc:q([L || L <- mnesia:table(lesson), L#lesson.creator_id =:= CreatorId]))};
                    _ -> {error, invalid_content_type}
                end
            end,
            case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_learning_path(PathId) ->
            Fun = fun() -> mnesia:read({learning_path, PathId}) end,
            case mnesia:transaction(Fun) of
                {atomic, [Path]} -> Path;
                {atomic, []} -> {error, not_found};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_learning_resource(ResourceId) ->
            Fun = fun() -> mnesia:read({learning_resource, ResourceId}) end,
            case mnesia:transaction(Fun) of
                {atomic, [Resource]} -> Resource;
                {atomic, []} -> {error, not_found};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_module(ModuleId) ->
            Fun = fun() -> mnesia:read({learning_module, ModuleId}) end,
            case mnesia:transaction(Fun) of
                {atomic, [Module]} -> Module;
                {atomic, []} -> {error, not_found};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_lesson(LessonId) ->
            Fun = fun() -> mnesia:read({lesson, LessonId}) end,
            case mnesia:transaction(Fun) of
                {atomic, [Lesson]} -> Lesson;
                {atomic, []} -> {error, not_found};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_quiz(QuizId) ->
            Fun = fun() -> mnesia:read({quiz, QuizId}) end,
            case mnesia:transaction(Fun) of
                {atomic, [Quiz]} -> Quiz;
                {atomic, []} -> {error, not_found};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_exercise(ExerciseId) ->
            Fun = fun() -> mnesia:read({exercise, ExerciseId}) end,
            case mnesia:transaction(Fun) of
                {atomic, [Exercise]} -> Exercise;
                {atomic, []} -> {error, not_found};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_project(ProjectId) ->
            Fun = fun() -> mnesia:read({project, ProjectId}) end,
            case mnesia:transaction(Fun) of
                {atomic, [Project]} -> Project;
                {atomic, []} -> {error, not_found};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_all_learning_paths() ->
            Fun = fun() -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path)])) end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_approved_learning_paths() ->
            Fun = fun() -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= approved, P#learning_path.visibility =:= public])) end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_paths_by_level(Level) ->
            Fun = fun() -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.difficulty_level =:= Level, P#learning_path.approval_status =:= approved])) end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_paths_by_track(Track) ->
            Fun = fun() -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.track =:= Track, P#learning_path.approval_status =:= approved])) end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_paths_by_category(Category, Subcategory) ->
            Fun = fun() -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.category =:= Category,
                P#learning_path.subcategory =:= Subcategory, P#learning_path.approval_status =:= approved])) end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_featured_paths() ->
            Fun = fun() -> qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.featured =:= true, P#learning_path.approval_status =:= approved])) end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_trending_paths(Limit) ->
            Fun = fun() ->
                AllPaths = qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= approved])),
                Sorted = lists:sort(fun(A, B) -> A#learning_path.enrollment_count > B#learning_path.enrollment_count end, AllPaths),
                lists:sublist(Sorted, Limit)
            end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_bestseller_paths(Limit) ->
            Fun = fun() ->
                AllPaths = qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= approved, P#learning_path.price > 0])),
                Sorted = lists:sort(fun(A, B) -> A#learning_path.total_revenue > B#learning_path.total_revenue end, AllPaths),
                lists:sublist(Sorted, Limit)
            end,
            case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_resources_by_path(PathId) ->
            Fun = fun() -> qlc:e(qlc:q([R || R <- mnesia:table(learning_resource), R#learning_resource.learning_path_id =:= PathId])) end,
            case mnesia:transaction(Fun) of {atomic, Resources} -> Resources; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_modules_by_path(PathId) ->
            Fun = fun() -> qlc:e(qlc:q([M || M <- mnesia:table(learning_module), M#learning_module.path_id =:= PathId])) end,
            case mnesia:transaction(Fun) of {atomic, Modules} -> Modules; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

        get_lessons_by_module(ModuleId) ->
            Fun = fun() -> qlc:e(qlc:q([L || L <- mnesia:table(lesson), L#lesson.module_id =:= ModuleId])) end,
            case mnesia:transaction(Fun) of {atomic, Lessons} -> Lessons; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            enroll_user(UserId, PathId) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Enrollment = #enrollment{id = Id, user_id = UserId, path_id = PathId, enrollment_date = Now, status = active},
                    mnesia:write(Enrollment),
                    case mnesia:read({learning_path, PathId}) of
                        [Path] -> mnesia:write(Path#learning_path{enrollment_count = Path#learning_path.enrollment_count + 1});
                        [] -> ok
                    end,
                    {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {atomic, {error, Reason}} -> {error, Reason};
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            unenroll_user(UserId, PathId) ->
                Fun = fun() ->
                    Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId, E#enrollment.path_id =:= PathId])),
                    case Enrollments of
                        [E|_] -> mnesia:delete({enrollment, E#enrollment.id}),
                            case mnesia:read({learning_path, PathId}) of
                                [Path] -> mnesia:write(Path#learning_path{enrollment_count = max(0, Path#learning_path.enrollment_count - 1)});
                                [] -> ok
                            end, ok;
                        [] -> {error, not_enrolled}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_user_enrollments(UserId) ->
                Fun = fun() -> qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId])) end,
                case mnesia:transaction(Fun) of {atomic, Enrollments} -> Enrollments; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_user_progress(UserId, PathId) ->
                Fun = fun() ->
                    case qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId, E#enrollment.path_id =:= PathId])) of
                        [Enrollment] -> #{enrollment => Enrollment, progress_percentage => Enrollment#enrollment.progress_percentage,
                            last_accessed => Enrollment#enrollment.last_accessed, completed => (Enrollment#enrollment.completion_date =/= undefined)};
                        [] -> {error, not_enrolled}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            update_progress(UserId, PathId, NewProgress) ->
                Fun = fun() ->
                    case qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId, E#enrollment.path_id =:= PathId])) of
                        [Enrollment] ->
                            Updated = Enrollment#enrollment{progress_percentage = NewProgress, last_accessed = calendar:universal_time()},
                            mnesia:write(Updated), ok;
                        [] -> {error, not_enrolled}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            complete_resource(UserId, ResourceId) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Completion = #user_completion{id = Id, user_id = UserId, content_id = ResourceId, content_type = learning_resource, completed_at = Now},
                    mnesia:write(Completion), ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            complete_lesson(UserId, LessonId) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Completion = #user_completion{id = Id, user_id = UserId, content_id = LessonId, content_type = lesson, completed_at = Now},
                    mnesia:write(Completion),
                    increment_completion_count(lesson, LessonId),
                    ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            complete_module(UserId, ModuleId) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Completion = #user_completion{id = Id, user_id = UserId, content_id = ModuleId, content_type = learning_module, completed_at = Now},
                    mnesia:write(Completion),
                    increment_completion_count(learning_module, ModuleId),
                    ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            complete_path(UserId, PathId) ->
                Fun = fun() ->
                    case qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId, E#enrollment.path_id =:= PathId])) of
                        [Enrollment] ->
                            Updated = Enrollment#enrollment{completion_date = calendar:universal_time(), progress_percentage = 100.0},
                            mnesia:write(Updated),
                            case mnesia:read({learning_path, PathId}) of
                                [Path] -> mnesia:write(Path#learning_path{completion_count = Path#learning_path.completion_count + 1});
                                [] -> ok
                            end, ok;
                        [] -> {error, not_enrolled}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            submit_quiz(UserId, QuizId, Answers) ->
                Fun = fun() ->
                    case mnesia:read({quiz, QuizId}) of
                        [] -> {error, quiz_not_found};
                        [Quiz] ->
                            Id = nanoid:gen(),
                            Now = calendar:universal_time(),
                            Questions = case Quiz#quiz.questions_cid of
                                {pending, _} -> Quiz#quiz.questions;
                                _ -> []
                            end,
                            {Score, MaxScore} = calculate_quiz_score(Questions, Answers),
                            Passed = Score >= Quiz#quiz.passing_score,
                            Attempt = #quiz_attempt{id = Id, quiz_id = QuizId, user_id = UserId, answers = Answers,
                                score = Score, max_score = MaxScore, passed = Passed, start_time = Now, end_time = Now},
                            mnesia:write(Attempt),
                            {ok, #{attempt_id => Id, score => Score, max_score => MaxScore, passed => Passed}}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            calculate_quiz_score(Questions, Answers) ->
                lists:foldl(fun(Q, {Score, MaxScore}) ->
                    QuestionId = maps:get(id, Q),
                    CorrectAnswer = maps:get(correct_answer, Q),
                    Points = maps:get(points, Q, 1.0),
                    UserAnswer = maps:get(QuestionId, Answers, undefined),
                    NewScore = if UserAnswer =:= CorrectAnswer -> Score + Points; true -> Score end,
                    {NewScore, MaxScore + Points}
                end, {0.0, 0.0}, Questions).

            get_quiz_attempts(UserId, QuizId) ->
                Fun = fun() -> qlc:e(qlc:q([A || A <- mnesia:table(quiz_attempt), A#quiz_attempt.user_id =:= UserId, A#quiz_attempt.quiz_id =:= QuizId])) end,
                case mnesia:transaction(Fun) of {atomic, Attempts} -> Attempts; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_best_quiz_score(UserId, QuizId) ->
                case get_quiz_attempts(UserId, QuizId) of
                    {error, Reason} -> {error, Reason};
                    Attempts ->
                        case Attempts of
                            [] -> {error, no_attempts};
                            _ ->
                                Sorted = lists:sort(fun(A, B) -> A#quiz_attempt.score > B#quiz_attempt.score end, Attempts),
                                hd(Sorted)
                        end
                end.

            submit_exercise(UserId, ExerciseId, SubmittedCode, Language) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Submission = #exercise_submission{id = Id, exercise_id = ExerciseId, user_id = UserId,
                        code = SubmittedCode, language = Language, submitted_at = Now, status = pending},
                    mnesia:write(Submission), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            get_exercise_submissions(UserId, ExerciseId) ->
                Fun = fun() -> qlc:e(qlc:q([S || S <- mnesia:table(exercise_submission), S#exercise_submission.user_id =:= UserId, S#exercise_submission.exercise_id =:= ExerciseId])) end,
                case mnesia:transaction(Fun) of {atomic, Submissions} -> Submissions; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            submit_project(UserId, ProjectId, SubmissionFiles, SubmissionNotes) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Submission = #project_submission{id = Id, project_id = ProjectId, user_id = UserId,
                        files_cid = SubmissionFiles, description = SubmissionNotes, submission_time = Now, submitted_at = Now,
                        status = submitted},
                    mnesia:write(Submission), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            get_project_submission(UserId, ProjectId) ->
                Fun = fun() ->
                    qlc:e(qlc:q([S || S <- mnesia:table(project_submission), S#project_submission.user_id =:= UserId, S#project_submission.project_id =:= ProjectId]))
                end,
                case mnesia:transaction(Fun) of
                    {atomic, [Submission|_]} -> Submission;
                    {atomic, []} -> {error, not_found};
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            grade_project(SubmissionId, GraderId, Grade, Feedback) ->
                Fun = fun() ->
                    case mnesia:read({project_submission, SubmissionId}) of
                        [] -> {error, not_found};
                        [Submission] ->
                            Updated = Submission#project_submission{grade = Grade, grader_id = GraderId, graded_at = calendar:universal_time(),
                                feedback = Feedback, status = graded},
                            mnesia:write(Updated), ok
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            award_certificate(UserId, PathId, CertificateType) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Certificate = #certificate{id = Id, user_id = UserId, path_id = PathId, certificate_type = CertificateType,
                        issue_date = Now, verification_code = generate_verification_code()},
                    mnesia:write(Certificate), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            generate_verification_code() ->
                nanoid:gen().

            get_user_certificates(UserId) ->
                Fun = fun() -> qlc:e(qlc:q([C || C <- mnesia:table(certificate), C#certificate.user_id =:= UserId])) end,
                case mnesia:transaction(Fun) of {atomic, Certificates} -> Certificates; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            verify_certificate(VerificationCode) ->
                Fun = fun() ->
                    qlc:e(qlc:q([C || C <- mnesia:table(certificate), C#certificate.verification_code =:= VerificationCode]))
                end,
                case mnesia:transaction(Fun) of
                    {atomic, [Certificate|_]} -> {valid, Certificate};
                    {atomic, []} -> {invalid, not_found};
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            award_badge(UserId, BadgeId) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    UserBadge = #user_badge{id = Id, user_id = UserId, badge_id = BadgeId, earned_at = Now},
                    mnesia:write(UserBadge), ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_user_badges(UserId) ->
                Fun = fun() -> qlc:e(qlc:q([B || B <- mnesia:table(user_badge), B#user_badge.user_id =:= UserId])) end,
                case mnesia:transaction(Fun) of {atomic, Badges} -> Badges; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            create_discussion_post(ResourceId, UserId, Title, Content, ParentPostId, Tags) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Post = #discussion_post{id = Id, resource_id = ResourceId, user_id = UserId, title = Title, content = Content,
                        parent_id = ParentPostId, tags = Tags, created_at = Now},
                    mnesia:write(Post), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            reply_to_post(PostId, UserId, Content) ->
                create_discussion_post(undefined, UserId, undefined, Content, PostId, []).

            get_resource_discussions(ResourceId) ->
                Fun = fun() -> qlc:e(qlc:q([P || P <- mnesia:table(discussion_post), P#discussion_post.resource_id =:= ResourceId])) end,
                case mnesia:transaction(Fun) of {atomic, Posts} -> Posts; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            track_time_spent(UserId, ResourceId, TimeSeconds) ->
                Fun = fun() ->
                    Now = calendar:universal_time(),
                    Existing = qlc:e(qlc:q([T || T <- mnesia:table(time_tracking), T#time_tracking.user_id =:= UserId,
                        T#time_tracking.content_id =:= ResourceId])),
                    case Existing of
                        [Track] ->
                            Updated = Track#time_tracking{minutes = Track#time_tracking.minutes + (TimeSeconds div 60),
                                tracked_at = Now},
                            mnesia:write(Updated);
                        [] ->
                            Id = nanoid:gen(),
                            Track = #time_tracking{id = Id, user_id = UserId, content_id = ResourceId,
                                minutes = (TimeSeconds div 60), tracked_at = Now},
                            mnesia:write(Track)
                    end, ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            track_content_time(UserId, ResourceId, _ResourceType, TimeSeconds) ->
                track_time_spent(UserId, ResourceId, TimeSeconds).

            add_bookmark(UserId, ResourceId) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Bookmark = #bookmark{id = Id, user_id = UserId, resource_id = ResourceId, created_at = Now},
                    mnesia:write(Bookmark), ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            remove_bookmark(UserId, ResourceId) ->
                Fun = fun() ->
                    Bookmarks = qlc:e(qlc:q([B || B <- mnesia:table(bookmark), B#bookmark.user_id =:= UserId, B#bookmark.resource_id =:= ResourceId])),
                    case Bookmarks of
                        [B|_] -> mnesia:delete({bookmark, B#bookmark.id}), ok;
                        [] -> {error, not_found}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_user_bookmarks(UserId) ->
                Fun = fun() -> qlc:e(qlc:q([B || B <- mnesia:table(bookmark), B#bookmark.user_id =:= UserId])) end,
                case mnesia:transaction(Fun) of {atomic, Bookmarks} -> Bookmarks; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            rate_resource(UserId, ResourceId, Rating) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    RatingRecord = #resource_rating{id = Id, user_id = UserId, resource_id = ResourceId, rating = Rating, created_at = Now},
                    mnesia:write(RatingRecord), ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_resource_rating(ResourceId) ->
                Fun = fun() ->
                    Ratings = qlc:e(qlc:q([R || R <- mnesia:table(resource_rating), R#resource_rating.resource_id =:= ResourceId])),
                    case Ratings of
                        [] -> #{average_rating => 0.0, total_ratings => 0};
                        _ ->
                            Total = lists:foldl(fun(R, Sum) -> Sum + R#resource_rating.rating end, 0.0, Ratings),
                            Count = length(Ratings),
                            #{average_rating => Total / Count, total_ratings => Count}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            search_learning_content(SearchTerm) ->
                Fun = fun() ->
                    AllPaths = qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= approved])),
                    lists:filter(fun(P) ->
                        string:find(string:lowercase(P#learning_path.title), string:lowercase(SearchTerm)) =/= nomatch orelse
                        lists:any(fun(Tag) -> string:find(string:lowercase(Tag), string:lowercase(SearchTerm)) =/= nomatch end, P#learning_path.tags)
                    end, AllPaths)
                end,
                case mnesia:transaction(Fun) of {atomic, Results} -> Results; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_recommended_paths(UserId) ->
                Fun = fun() ->
                    Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId])),
                    EnrolledPathIds = [E#enrollment.path_id || E <- Enrollments],
                    AllPaths = qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= approved])),
                    lists:filter(fun(P) -> not lists:member(P#learning_path.id, EnrolledPathIds) end, AllPaths)
                end,
                case mnesia:transaction(Fun) of {atomic, Paths} -> lists:sublist(Paths, 10); {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_prerequisite_paths(PathId) ->
                Fun = fun() ->
                    case mnesia:read({learning_path, PathId}) of
                        [Path] -> Path#learning_path.prerequisite_paths;
                        [] -> []
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Prerequisites} -> Prerequisites; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_next_paths(PathId) ->
                Fun = fun() ->
                    AllPaths = qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= approved])),
                    lists:filter(fun(P) -> lists:member(PathId, P#learning_path.prerequisite_paths) end, AllPaths)
                end,
                case mnesia:transaction(Fun) of {atomic, Paths} -> Paths; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            create_study_group(PathId, CreatorId, Name, Description, MaxMembers) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Group = #study_group{id = Id, path_id = PathId, creator_id = CreatorId, name = Name, description = Description,
                        max_members = MaxMembers, members = [CreatorId], created_at = Now},
                    mnesia:write(Group), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            join_study_group(GroupId, UserId) ->
                Fun = fun() ->
                    case mnesia:read({study_group, GroupId}) of
                        [] -> {error, not_found};
                        [Group] ->
                            case length(Group#study_group.members) < Group#study_group.max_members of
                                true ->
                                    Updated = Group#study_group{members = [UserId | Group#study_group.members]},
                                    mnesia:write(Updated), ok;
                                false -> {error, group_full}
                            end
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            leave_study_group(GroupId, UserId) ->
                Fun = fun() ->
                    case mnesia:read({study_group, GroupId}) of
                        [] -> {error, not_found};
                        [Group] ->
                            Updated = Group#study_group{members = lists:delete(UserId, Group#study_group.members)},
                            mnesia:write(Updated), ok
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_user_study_groups(UserId) ->
                Fun = fun() ->
                    qlc:e(qlc:q([G || G <- mnesia:table(study_group), lists:member(UserId, G#study_group.members)]))
                end,
                case mnesia:transaction(Fun) of {atomic, Groups} -> Groups; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            schedule_mentor_session(PathId, MentorId, StudentId, ScheduledTime, Duration, Topic) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Session = #mentor_session{id = Id, path_id = PathId, mentor_id = MentorId, student_id = StudentId,
                        scheduled_time = ScheduledTime, duration_minutes = Duration, topic = Topic, status = scheduled},
                    mnesia:write(Session), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            complete_mentor_session(SessionId, Notes) ->
                Fun = fun() ->
                    case mnesia:read({mentor_session, SessionId}) of
                        [] -> {error, not_found};
                        [Session] ->
                            Updated = Session#mentor_session{status = completed, notes = Notes},
                            mnesia:write(Updated), ok
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_mentor_sessions(UserId) ->
                Fun = fun() ->
                    qlc:e(qlc:q([S || S <- mnesia:table(mentor_session), S#mentor_session.mentor_id =:= UserId orelse S#mentor_session.student_id =:= UserId]))
                end,
                case mnesia:transaction(Fun) of {atomic, Sessions} -> Sessions; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            create_live_class(PathId, InstructorId, Title, Description, ScheduledTime, Duration, MaxAttendees, MeetingLink, RecordingAllowed) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Class = #live_class{id = Id, path_id = PathId, instructor_id = InstructorId, title = Title, description = Description,
                        scheduled_time = ScheduledTime, duration_minutes = Duration, max_attendees = MaxAttendees,
                        meeting_url = MeetingLink, recording_allowed = RecordingAllowed, status = scheduled},
                    mnesia:write(Class), {ok, Id}
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} -> Id;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            register_for_live_class(ClassId, UserId) ->
                Fun = fun() ->
                    case mnesia:read({live_class, ClassId}) of
                        [] -> {error, not_found};
                        [Class] ->
                            case length(Class#live_class.attendees) < Class#live_class.max_attendees of
                                true ->
                                    Updated = Class#live_class{attendees = [UserId | Class#live_class.attendees]},
                                    mnesia:write(Updated), ok;
                                false -> {error, class_full}
                            end
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_upcoming_live_classes(PathId) ->
                Fun = fun() ->
                    Now = calendar:universal_time(),
                    qlc:e(qlc:q([C || C <- mnesia:table(live_class), C#live_class.path_id =:= PathId,
                        C#live_class.scheduled_time > Now, C#live_class.status =:= scheduled]))
                end,
                case mnesia:transaction(Fun) of {atomic, Classes} -> Classes; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_learning_analytics(UserId) ->
                Fun = fun() ->
                    Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.user_id =:= UserId])),
                    Completions = qlc:e(qlc:q([C || C <- mnesia:table(user_completion), C#user_completion.user_id =:= UserId])),
                    Certificates = qlc:e(qlc:q([C || C <- mnesia:table(certificate), C#certificate.user_id =:= UserId])),
                    Badges = qlc:e(qlc:q([B || B <- mnesia:table(user_badge), B#user_badge.user_id =:= UserId])),
                    TimeTracking = qlc:e(qlc:q([T || T <- mnesia:table(time_tracking), T#time_tracking.user_id =:= UserId])),
                    TotalTime = lists:foldl(fun(T, Sum) -> Sum + T#time_tracking.minutes end, 0, TimeTracking),
                    #{enrollments => length(Enrollments), completions => length(Completions), certificates => length(Certificates),
                      badges => length(Badges), total_time_minutes => TotalTime}
                end,
                case mnesia:transaction(Fun) of {atomic, Analytics} -> Analytics; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_path_analytics(PathId) ->
                Fun = fun() ->
                    Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.path_id =:= PathId])),
                    CompletedEnrollments = lists:filter(fun(E) -> E#enrollment.completion_date =/= undefined end, Enrollments),
                    Ratings = qlc:e(qlc:q([R || R <- mnesia:table(resource_rating), R#resource_rating.resource_id =:= PathId])),
                    AvgRating = case Ratings of
                        [] -> 0.0;
                        _ ->
                            Total = lists:foldl(fun(R, Sum) -> Sum + R#resource_rating.rating end, 0.0, Ratings),
                            Total / length(Ratings)
                    end,
                    case mnesia:read({learning_path, PathId}) of
                        [Path] ->
                            #{enrollment_count => length(Enrollments), completion_count => length(CompletedEnrollments),
                              average_rating => AvgRating, total_revenue => Path#learning_path.total_revenue};
                        [] -> {error, not_found}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Analytics} -> Analytics; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_completion_stats(PathId) ->
                Fun = fun() ->
                    Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.path_id =:= PathId])),
                    case Enrollments of
                        [] -> #{total_enrollments => 0, completed => 0, in_progress => 0, completion_rate => 0.0};
                        _ ->
                            Completed = length(lists:filter(fun(E) -> E#enrollment.completion_date =/= undefined end, Enrollments)),
                            Total = length(Enrollments),
                            #{total_enrollments => Total, completed => Completed, in_progress => Total - Completed,
                              completion_rate => (Completed / Total) * 100.0}
                    end
                end,
                case mnesia:transaction(Fun) of {atomic, Stats} -> Stats; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_leaderboard(PathId, Limit) ->
                Fun = fun() ->
                    Enrollments = qlc:e(qlc:q([E || E <- mnesia:table(enrollment), E#enrollment.path_id =:= PathId])),
                    Sorted = lists:sort(fun(A, B) -> A#enrollment.progress_percentage > B#enrollment.progress_percentage end, Enrollments),
                    lists:sublist(Sorted, Limit)
                end,
                case mnesia:transaction(Fun) of {atomic, Leaderboard} -> Leaderboard; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_user_rank(UserId, PathId) ->
                case get_leaderboard(PathId, 1000) of
                    {error, Reason} -> {error, Reason};
                    Leaderboard ->
                        case lists:search(fun(E) -> E#enrollment.user_id =:= UserId end, Leaderboard) of
                            {value, _} ->
                                Rank = length(lists:takewhile(fun(E) -> E#enrollment.user_id =/= UserId end, Leaderboard)) + 1,
                                {ok, Rank};
                            false -> {error, not_enrolled}
                        end
                end.

                create_instructor_profile(UserId, Bio, Expertise, _Experience, Education, Certifications, _Website, SocialLinks, VideoIntro, ProfileImage, _HourlyRate, _Availability) ->  %% FIXED: Added underscores
                    Fun = fun() ->
                        Id = nanoid:gen(),
                        Now = calendar:universal_time(),
                        Profile = #instructor_profile{id = Id, user_id = UserId, bio = Bio, expertise = Expertise,
                            education = Education, certifications = Certifications, social_links = SocialLinks,
                            video_intro_cid = VideoIntro, profile_image_cid = ProfileImage,
                            created_at = Now, updated_at = Now},
                        mnesia:write(Profile), {ok, Id}
                    end,
                    case mnesia:transaction(Fun) of
                        {atomic, {ok, Id}} -> Id;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                    update_instructor_profile(ProfileId, NewBio, NewExpertise, _NewExperience, NewEducation, NewCertifications,
                                              _NewWebsite, NewSocialLinks, NewVideoIntro, NewProfileImage, _NewHourlyRate, _NewAvailability, _NewLanguages) ->  %% FIXED: Added underscores
                        Fun = fun() ->
                            case mnesia:read({instructor_profile, ProfileId}) of
                                [] -> {error, not_found};
                                [Profile] ->
                                    Updated = Profile#instructor_profile{bio = NewBio, expertise = NewExpertise,
                                        education = NewEducation, certifications = NewCertifications, social_links = NewSocialLinks,
                                        video_intro_cid = NewVideoIntro, profile_image_cid = NewProfileImage,
                                        updated_at = calendar:universal_time()},
                                    mnesia:write(Updated), ok
                            end
                        end,
                        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

                        verify_instructor(AdminUsername, UserId, _VerificationNotes) ->  %% FIXED: Added underscore
                            case is_admin(AdminUsername) of
                                false -> {error, unauthorized};
                                true ->
                                    Fun = fun() ->
                                        Profiles = qlc:e(qlc:q([P || P <- mnesia:table(instructor_profile), P#instructor_profile.user_id =:= UserId])),
                                        case Profiles of
                                            [Profile] ->
                                                Updated = Profile#instructor_profile{verified = true, verified_by = AdminUsername, verified_at = calendar:universal_time()},
                                                mnesia:write(Updated), ok;
                                            [] -> {error, no_profile}
                                        end
                                    end,
                                    case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end
                            end.

            get_instructor_profile(UserId) ->
                Fun = fun() ->
                    qlc:e(qlc:q([P || P <- mnesia:table(instructor_profile), P#instructor_profile.user_id =:= UserId]))
                end,
                case mnesia:transaction(Fun) of
                    {atomic, [Profile]} -> Profile;
                    {atomic, []} -> {error, not_found};
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            log_admin_action(AdminUsername, Action, ContentType, ContentId, Notes, Timestamp, Metadata) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    AdminAction = #admin_action{id = Id, admin_username = AdminUsername, action_type = Action, content_type = ContentType,
                        content_id = ContentId, notes = Notes, timestamp = Timestamp, metadata = Metadata},
                    mnesia:write(AdminAction), ok
                end,
                case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            get_admin_actions(AdminUsername, Limit) ->
                Fun = fun() ->
                    Actions = qlc:e(qlc:q([A || A <- mnesia:table(admin_action), A#admin_action.admin_username =:= AdminUsername])),
                    Sorted = lists:sort(fun(A, B) -> A#admin_action.timestamp > B#admin_action.timestamp end, Actions),
                    lists:sublist(Sorted, Limit)
                end,
                case mnesia:transaction(Fun) of {atomic, Actions} -> Actions; {aborted, Reason} -> {error, {transaction_failed, Reason}} end.

            reverse_admin_action(AdminUsername, ActionId, ReversalNotes) ->
                case is_admin(AdminUsername) of
                    false -> {error, unauthorized};
                    true ->
                        Fun = fun() ->
                            case mnesia:read({admin_action, ActionId}) of
                                [] -> {error, not_found};
                                [Action] ->
                                    Updated = Action#admin_action{reversed = true, reversed_by = AdminUsername,
                                        reversed_at = calendar:universal_time(), reversal_reason = ReversalNotes},
                                    mnesia:write(Updated), ok
                            end
                        end,
                        case mnesia:transaction(Fun) of {atomic, Result} -> Result; {aborted, Reason} -> {error, {transaction_failed, Reason}} end
                end.

            approve_path(PathId, AdminUserId) ->
                approve_content(AdminUserId, learning_path, PathId, <<"Approved by admin">>).

            reject_path(PathId, AdminUserId, Reason) ->
                reject_content(AdminUserId, learning_path, PathId, Reason, <<"Rejected by admin">>).

            get_paths_by_status(Status) ->
                Fun = fun() ->
                    qlc:e(qlc:q([P || P <- mnesia:table(learning_path), P#learning_path.approval_status =:= Status]))
                end,
                case mnesia:transaction(Fun) of
                    {atomic, Paths} -> Paths;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.
