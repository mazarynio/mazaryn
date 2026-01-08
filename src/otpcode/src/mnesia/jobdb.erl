-module(jobdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../business.hrl").
-include("../job_records.hrl").

-export([
    create_job_posting/2,
    create_job_posting_concurrent/2,
    get_job_posting_by_id/1,
    get_job_postings_by_poster/2,
    get_all_job_postings/0,
    get_active_job_postings/0,
    update_job_posting/2,
    delete_job_posting/1,

    publish_job/1,
    unpublish_job/1,
    pause_job/1,
    resume_job/1,
    close_job/1,
    mark_job_as_filled/1,

    add_application/2,
    remove_application/2,
    get_job_applications/1,
    get_application_count/1,
    shortlist_application/2,
    reject_application/2,
    hire_candidate/2,

    increment_view_count/1,
    increment_application_count/1,
    track_application_source/2,

    search_jobs_by_title/1,
    search_jobs_by_location/2,
    search_jobs_by_industry/1,
    search_jobs_by_keywords/1,
    filter_jobs/1,

    feature_job/2,
    boost_job/2,
    is_job_featured/1,
    is_job_boosted/1,

    create_resume/2,
    get_resume_by_id/1,
    get_resume_by_user_id/1,
    get_resumes_by_user_id/1,
    update_resume/2,
    delete_resume/1,
    set_primary_resume/2,

    upload_resume_pdf/2,
    upload_cover_letter/2,
    upload_portfolio/2,

    add_work_experience/2,
    remove_work_experience/2,
    update_work_experience/3,
    add_education/2,
    remove_education/2,
    add_skill/2,
    remove_skill/2,
    add_certification_to_resume/2,
    add_project/2,
    remove_project/2,

    increment_resume_views/1,
    increment_resume_downloads/1,
    record_recruiter_contact/2,

    create_job_application/3,
    get_job_application_by_id/1,
    get_applications_by_job/1,
    get_applications_by_user/1,
    update_application_status/2,
    withdraw_application/1,

    add_interview_stage/2,
    update_interview_stage/2,
    add_screening_answer/3,
    add_skills_test_result/3,

    save_job/2,
    unsave_job/2,
    get_saved_jobs/1,
    is_job_saved/2,

    create_job_alert/2,
    update_job_alert/2,
    delete_job_alert/1,
    get_job_alerts_by_user/1,
    check_job_matches_alert/2,

    populate_resume_sample_data/1,
    get_complete_resume/1
]).

-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).
-define(LIMIT_SEARCH, 50).

create_job_posting(PosterData, JobData) ->
    Fun = fun() ->
        case validate_job_posting_data(JobData) of
            {error, Reason} -> {error, Reason};
            ok ->
                Now = calendar:universal_time(),
                JobID = nanoid:gen(),
                UniqueJobID = generate_job_id(),

                PosterType = maps:get(poster_type, PosterData),
                PosterID = maps:get(poster_id, PosterData),
                PostedByUserID = maps:get(posted_by_user_id, PosterData),

                JobPosting = #job_posting{
                    id = JobID,
                    job_id = UniqueJobID,
                    poster_type = PosterType,
                    poster_id = PosterID,
                    posted_by_user_id = PostedByUserID,
                    business_id = maps:get(business_id, PosterData, undefined),

                    title = maps:get(title, JobData),
                    description = maps:get(description, JobData),
                    short_description = maps:get(short_description, JobData, ""),

                    category = maps:get(category, JobData, ""),
                    sub_category = maps:get(sub_category, JobData, ""),
                    industry = maps:get(industry, JobData, ""),
                    job_function = maps:get(job_function, JobData, ""),
                    seniority_level = maps:get(seniority_level, JobData, "mid"),
                    employment_type = maps:get(employment_type, JobData, "full_time"),

                    location_type = maps:get(location_type, JobData, "on_site"),
                    locations = maps:get(locations, JobData, []),

                    salary_range = maps:get(salary_range, JobData, #{
                        min => 0.0,
                        max => 0.0,
                        currency => "USD",
                        period => "yearly"
                    }),
                    salary_visible = maps:get(salary_visible, JobData, false),

                    required_skills = maps:get(required_skills, JobData, []),
                    experience_required = maps:get(experience_required, JobData, #{
                        min_years => 0,
                        max_years => 0
                    }),

                    status = "draft",
                    visibility = maps:get(visibility, JobData, "public"),

                    date_created = Now,
                    date_updated = Now
                },

                mnesia:write(JobPosting),

                case PosterType of
                    business ->
                        businessdb:add_job_posting(PosterID, JobID);
                    individual ->
                        ok
                end,

                {ok, JobID}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, JobID}} -> {ok, JobID};
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    create_job_posting_concurrent(PosterData, JobData) ->
        case validate_job_posting_data(JobData) of
            {error, Reason} -> {error, Reason};
            ok ->
                JobID = nanoid:gen(),
                UniqueJobID = generate_job_id(),
                Now = calendar:universal_time(),

                PosterType = maps:get(poster_type, PosterData),
                PosterID = maps:get(poster_id, PosterData),
                PostedByUserID = maps:get(posted_by_user_id, PosterData),

                JobPosting = #job_posting{
                    id = JobID,
                    job_id = UniqueJobID,
                    poster_type = PosterType,
                    poster_id = PosterID,
                    posted_by_user_id = PostedByUserID,
                    business_id = maps:get(business_id, PosterData, undefined),

                    title = maps:get(title, JobData),
                    description = maps:get(description, JobData),
                    short_description = maps:get(short_description, JobData, ""),

                    category = maps:get(category, JobData, ""),
                    sub_category = maps:get(sub_category, JobData, ""),
                    industry = maps:get(industry, JobData, ""),
                    job_function = maps:get(job_function, JobData, ""),
                    seniority_level = maps:get(seniority_level, JobData, "mid"),
                    employment_type = maps:get(employment_type, JobData, "full_time"),

                    location_type = maps:get(location_type, JobData, "on_site"),
                    locations = maps:get(locations, JobData, []),

                    salary_range = maps:get(salary_range, JobData, #{
                        min => 0.0,
                        max => 0.0,
                        currency => "USD",
                        period => "yearly"
                    }),
                    salary_visible = maps:get(salary_visible, JobData, false),

                    required_skills = maps:get(required_skills, JobData, []),
                    experience_required = maps:get(experience_required, JobData, #{
                        min_years => 0,
                        max_years => 0
                    }),

                    status = "draft",
                    visibility = maps:get(visibility, JobData, "public"),

                    date_created = Now,
                    date_updated = Now
                },

                case write_job_posting_with_retry(JobPosting, PosterType, PosterID, ?MAX_RETRIES) of
                    ok -> {ok, JobID};
                    {error, Reason} -> {error, Reason}
                end
        end.

        write_job_posting_with_retry(JobPosting, PosterType, PosterID, RetriesLeft) when RetriesLeft > 0 ->
            try
                case mnesia:transaction(fun() ->
                    mnesia:write(JobPosting),
                    case PosterType of
                        "business" ->
                            businessdb:add_job_posting(PosterID, JobPosting#job_posting.id);
                        business ->
                            businessdb:add_job_posting(PosterID, JobPosting#job_posting.id);
                        _ ->
                            ok
                    end
                end) of
                    {atomic, ok} -> ok;
                    {aborted, Reason} ->
                        error_logger:error_msg("Job posting write transaction ABORTED - retries left: ~p~nReason: ~p~n",
                                               [RetriesLeft, Reason]),
                        timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                        write_job_posting_with_retry(JobPosting, PosterType, PosterID, RetriesLeft - 1)
                end
            catch
                error:ErrReason:Stacktrace ->
                    error_logger:error_msg("Job posting write FAILED (error) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                           [RetriesLeft, ErrReason, Stacktrace]),
                    timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                    write_job_posting_with_retry(JobPosting, PosterType, PosterID, RetriesLeft - 1);
                throw:ThrowReason:Stacktrace ->
                    error_logger:error_msg("Job posting write FAILED (throw) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                           [RetriesLeft, ThrowReason, Stacktrace]),
                    timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                    write_job_posting_with_retry(JobPosting, PosterType, PosterID, RetriesLeft - 1);
                exit:ExitReason:Stacktrace ->
                    error_logger:error_msg("Job posting write FAILED (exit) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                           [RetriesLeft, ExitReason, Stacktrace]),
                    {error, {process_died, ExitReason}}
            end;
        write_job_posting_with_retry(_JobPosting, _PosterType, _PosterID, 0) ->
            error_logger:error_msg("Job posting write FAILED - MAX RETRIES EXCEEDED~n", []),
            {error, max_retries_exceeded}.

get_job_posting_by_id(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] -> {ok, JobPosting}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, JobPosting}} -> JobPosting;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_job_postings_by_poster(PosterType, PosterID) ->
    Fun = fun() ->
        mnesia:match_object(#job_posting{poster_type = PosterType, poster_id = PosterID, _ = '_'})
    end,
    case mnesia:transaction(Fun) of
        {atomic, JobPostings} -> JobPostings;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_all_job_postings() ->
    Fun = fun() ->
        mnesia:match_object(#job_posting{_ = '_'})
    end,
    case mnesia:transaction(Fun) of
        {atomic, JobPostings} -> JobPostings;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_active_job_postings() ->
    Fun = fun() ->
        mnesia:match_object(#job_posting{status = "active", _ = '_'})
    end,
    case mnesia:transaction(Fun) of
        {atomic, JobPostings} -> JobPostings;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_job_posting(JobID, UpdateData) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                UpdatedJobPosting = apply_job_updates(JobPosting, UpdateData),
                mnesia:write(UpdatedJobPosting#job_posting{
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

apply_job_updates(JobPosting, UpdateData) ->
    maps:fold(fun(Key, Value, Acc) ->
        case Key of
            title -> Acc#job_posting{title = Value};
            description -> Acc#job_posting{description = Value};
            short_description -> Acc#job_posting{short_description = Value};
            category -> Acc#job_posting{category = Value};
            sub_category -> Acc#job_posting{sub_category = Value};
            industry -> Acc#job_posting{industry = Value};
            seniority_level -> Acc#job_posting{seniority_level = Value};
            employment_type -> Acc#job_posting{employment_type = Value};
            location_type -> Acc#job_posting{location_type = Value};
            locations -> Acc#job_posting{locations = Value};
            salary_range -> Acc#job_posting{salary_range = Value};
            salary_visible -> Acc#job_posting{salary_visible = Value};
            required_skills -> Acc#job_posting{required_skills = Value};
            preferred_skills -> Acc#job_posting{preferred_skills = Value};
            experience_required -> Acc#job_posting{experience_required = Value};
            responsibilities -> Acc#job_posting{responsibilities = Value};
            requirements -> Acc#job_posting{requirements = Value};
            benefits -> Acc#job_posting{benefits = Value};
            application_deadline -> Acc#job_posting{application_deadline = Value};
            _ -> Acc
        end
    end, JobPosting, UpdateData).

delete_job_posting(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                PosterType = JobPosting#job_posting.poster_type,
                PosterID = JobPosting#job_posting.poster_id,

                case PosterType of
                    business ->
                        businessdb:remove_job_posting(PosterID, JobID);
                    individual ->
                        ok
                end,

                mnesia:delete({job_posting, JobID}),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

publish_job(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    status = "active",
                    date_published = calendar:universal_time(),
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unpublish_job(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    status = "draft",
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

pause_job(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    status = "paused",
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

resume_job(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    status = "active",
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

close_job(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    status = "closed",
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

mark_job_as_filled(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    status = "filled",
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_application(JobID, ApplicationID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                CurrentApplications = JobPosting#job_posting.applications,
                UpdatedApplications = [ApplicationID | CurrentApplications],
                mnesia:write(JobPosting#job_posting{
                    applications = UpdatedApplications,
                    applications_count = JobPosting#job_posting.applications_count + 1,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_application(JobID, ApplicationID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                CurrentApplications = JobPosting#job_posting.applications,
                UpdatedApplications = lists:delete(ApplicationID, CurrentApplications),
                mnesia:write(JobPosting#job_posting{
                    applications = UpdatedApplications,
                    applications_count = max(0, JobPosting#job_posting.applications_count - 1),
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_job_applications(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] -> {ok, JobPosting#job_posting.applications}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Applications}} -> Applications;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_application_count(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] -> {ok, JobPosting#job_posting.applications_count}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Count}} -> Count;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

shortlist_application(JobID, ApplicationID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                CurrentShortlisted = JobPosting#job_posting.shortlisted_candidates,
                UpdatedShortlisted = [ApplicationID | CurrentShortlisted],
                mnesia:write(JobPosting#job_posting{
                    shortlisted_candidates = UpdatedShortlisted,
                    shortlisted_count = JobPosting#job_posting.shortlisted_count + 1,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

reject_application(JobID, ApplicationID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                CurrentRejected = JobPosting#job_posting.rejected_candidates,
                UpdatedRejected = [ApplicationID | CurrentRejected],
                mnesia:write(JobPosting#job_posting{
                    rejected_candidates = UpdatedRejected,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

hire_candidate(JobID, UserID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                CurrentHired = JobPosting#job_posting.hired_candidates,
                UpdatedHired = [UserID | CurrentHired],
                mnesia:write(JobPosting#job_posting{
                    hired_candidates = UpdatedHired,
                    offers_accepted = JobPosting#job_posting.offers_accepted + 1,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

increment_view_count(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    views = JobPosting#job_posting.views + 1,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

increment_application_count(JobID) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                mnesia:write(JobPosting#job_posting{
                    applications_count = JobPosting#job_posting.applications_count + 1,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

track_application_source(JobID, Source) ->
    Fun = fun() ->
        case mnesia:read(job_posting, JobID) of
            [] -> {error, job_not_found};
            [JobPosting] ->
                CurrentTracking = JobPosting#job_posting.source_tracking,
                CurrentCount = maps:get(Source, CurrentTracking, 0),
                UpdatedTracking = maps:put(Source, CurrentCount + 1, CurrentTracking),
                mnesia:write(JobPosting#job_posting{
                    source_tracking = UpdatedTracking,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

validate_job_posting_data(JobData) ->
    case maps:is_key(title, JobData) of
        false -> {error, missing_title};
        true ->
            case maps:is_key(description, JobData) of
                false -> {error, missing_description};
                true -> ok
            end
    end.

    validate_poster(business, PosterID) ->
        case mnesia:dirty_read(business, PosterID) of
            [] -> false;
            [_Business] -> true
        end;
    validate_poster(individual, _PosterID) ->
        true;
    validate_poster(_, _) ->
        false.

generate_job_id() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Random = rand:uniform(999999),
    lists:flatten(io_lib:format("JOB-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B-~6..0B",
                                [Year, Month, Day, Hour, Minute, Second, Random])).

    search_jobs_by_title(Title) ->
        Fun = fun() ->
            AllJobs = mnesia:match_object(#job_posting{_ = '_'}),
            lists:filter(fun(Job) ->
                JobTitle = string:lowercase(Job#job_posting.title),
                SearchTitle = string:lowercase(Title),
                string:find(JobTitle, SearchTitle) =/= nomatch
            end, AllJobs)
        end,
        case mnesia:transaction(Fun) of
            {atomic, Jobs} -> Jobs;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    search_jobs_by_location(City, Country) ->
        Fun = fun() ->
            AllJobs = mnesia:match_object(#job_posting{_ = '_'}),
            lists:filter(fun(Job) ->
                Locations = Job#job_posting.locations,
                lists:any(fun(Location) ->
                    LocationCity = maps:get(city, Location, ""),
                    LocationCountry = maps:get(country, Location, ""),
                    (City =:= "" orelse string:lowercase(LocationCity) =:= string:lowercase(City)) andalso
                    (Country =:= "" orelse string:lowercase(LocationCountry) =:= string:lowercase(Country))
                end, Locations)
            end, AllJobs)
        end,
        case mnesia:transaction(Fun) of
            {atomic, Jobs} -> Jobs;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    search_jobs_by_industry(Industry) ->
        Fun = fun() ->
            mnesia:match_object(#job_posting{industry = Industry, _ = '_'})
        end,
        case mnesia:transaction(Fun) of
            {atomic, Jobs} -> Jobs;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    search_jobs_by_keywords(Keywords) ->
        Fun = fun() ->
            AllJobs = mnesia:match_object(#job_posting{_ = '_'}),
            lists:filter(fun(Job) ->
                Title = string:lowercase(Job#job_posting.title),
                Description = string:lowercase(Job#job_posting.description),
                lists:any(fun(Keyword) ->
                    LowerKeyword = string:lowercase(Keyword),
                    string:find(Title, LowerKeyword) =/= nomatch orelse
                    string:find(Description, LowerKeyword) =/= nomatch
                end, Keywords)
            end, AllJobs)
        end,
        case mnesia:transaction(Fun) of
            {atomic, Jobs} -> Jobs;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    filter_jobs(FilterCriteria) ->
        Fun = fun() ->
            AllJobs = mnesia:match_object(#job_posting{_ = '_'}),
            lists:filter(fun(Job) ->
                apply_job_filters(Job, FilterCriteria)
            end, AllJobs)
        end,
        case mnesia:transaction(Fun) of
            {atomic, Jobs} -> Jobs;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    apply_job_filters(Job, FilterCriteria) ->
        lists:all(fun({Key, Value}) ->
            case Key of
                status ->
                    Job#job_posting.status =:= Value;
                employment_type ->
                    Job#job_posting.employment_type =:= Value;
                location_type ->
                    Job#job_posting.location_type =:= Value;
                seniority_level ->
                    Job#job_posting.seniority_level =:= Value;
                min_salary ->
                    SalaryRange = Job#job_posting.salary_range,
                    maps:get(min, SalaryRange, 0.0) >= Value;
                max_salary ->
                    SalaryRange = Job#job_posting.salary_range,
                    maps:get(max, SalaryRange, 999999.0) =< Value;
                remote_only ->
                    Value =:= true andalso Job#job_posting.location_type =:= "remote";
                featured_only ->
                    Value =:= true andalso Job#job_posting.featured =:= true;
                visa_sponsorship ->
                    Value =:= true andalso Job#job_posting.visa_sponsorship =:= true;
                _ ->
                    true
            end
        end, FilterCriteria).

    feature_job(JobID, UntilDate) ->
        Fun = fun() ->
            case mnesia:read(job_posting, JobID) of
                [] -> {error, job_not_found};
                [JobPosting] ->
                    mnesia:write(JobPosting#job_posting{
                        featured = true,
                        featured_until = UntilDate,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    boost_job(JobID, UntilDate) ->
        Fun = fun() ->
            case mnesia:read(job_posting, JobID) of
                [] -> {error, job_not_found};
                [JobPosting] ->
                    mnesia:write(JobPosting#job_posting{
                        boosted = true,
                        boosted_until = UntilDate,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    is_job_featured(JobID) ->
        Fun = fun() ->
            case mnesia:read(job_posting, JobID) of
                [] -> false;
                [JobPosting] ->
                    case JobPosting#job_posting.featured_until of
                        undefined -> false;
                        FeaturedUntil ->
                            JobPosting#job_posting.featured andalso
                            calendar:universal_time() < FeaturedUntil
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, _Reason} -> false
        end.

    is_job_boosted(JobID) ->
        Fun = fun() ->
            case mnesia:read(job_posting, JobID) of
                [] -> false;
                [JobPosting] ->
                    case JobPosting#job_posting.boosted_until of
                        undefined -> false;
                        BoostedUntil ->
                            JobPosting#job_posting.boosted andalso
                            calendar:universal_time() < BoostedUntil
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, _Reason} -> false
        end.

    create_resume(UserID, ResumeData) ->
        Fun = fun() ->
            case mnesia:read(user, UserID) of
                [] -> {error, user_not_found};
                [_User] ->
                    Now = calendar:universal_time(),
                    ResumeID = nanoid:gen(),

                    Resume = #resume{
                        id = ResumeID,
                        user_id = UserID,
                        resume_type = maps:get(resume_type, ResumeData, "platform_generated"),
                        is_primary = maps:get(is_primary, ResumeData, false),
                        visibility = maps:get(visibility, ResumeData, "private"),
                        status = "draft",

                        full_name = maps:get(full_name, ResumeData, ""),
                        professional_title = maps:get(professional_title, ResumeData, ""),
                        headline = maps:get(headline, ResumeData, ""),
                        summary = maps:get(summary, ResumeData, ""),

                        email = maps:get(email, ResumeData, ""),
                        phone = maps:get(phone, ResumeData, ""),
                        location = maps:get(location, ResumeData, #{
                            country => "",
                            state => "",
                            city => "",
                            postal_code => "",
                            willing_to_relocate => false
                        }),

                        work_experience = maps:get(work_experience, ResumeData, []),
                        education = maps:get(education, ResumeData, []),
                        technical_skills = maps:get(technical_skills, ResumeData, []),
                        soft_skills = maps:get(soft_skills, ResumeData, []),
                        languages = maps:get(languages, ResumeData, []),
                        certifications = maps:get(certifications, ResumeData, []),
                        projects = maps:get(projects, ResumeData, []),

                        job_preferences = maps:get(job_preferences, ResumeData, #{
                            desired_titles => [],
                            desired_industries => [],
                            desired_locations => [],
                            remote_preference => "flexible",
                            employment_types => [],
                            min_salary => 0.0,
                            salary_currency => "USD"
                        }),

                        date_created = Now,
                        date_updated = Now,
                        version = 1,
                        language = maps:get(language, ResumeData, "en")
                    },

                    mnesia:write(Resume),
                    {ok, ResumeID}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, ResumeID}} -> {ok, ResumeID};
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_resume_by_id(ResumeID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] -> {ok, Resume}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Resume}} -> Resume;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_resume_by_user_id(UserID) ->
        Fun = fun() ->
            AllResumes = mnesia:match_object(#resume{user_id = UserID, _ = '_'}),
            case lists:filter(fun(R) -> R#resume.is_primary =:= true end, AllResumes) of
                [PrimaryResume] -> {ok, PrimaryResume};
                [] ->
                    case AllResumes of
                        [] -> {error, no_resume_found};
                        [FirstResume | _] -> {ok, FirstResume}
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Resume}} -> Resume;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_resumes_by_user_id(UserID) ->
        Fun = fun() ->
            mnesia:match_object(#resume{user_id = UserID, _ = '_'})
        end,
        case mnesia:transaction(Fun) of
            {atomic, Resumes} -> Resumes;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    update_resume(ResumeID, UpdateData) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    UpdatedResume = apply_resume_updates(Resume, UpdateData),
                    mnesia:write(UpdatedResume#resume{
                        date_updated = calendar:universal_time(),
                        version = Resume#resume.version + 1
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    apply_resume_updates(Resume, UpdateData) ->
        maps:fold(fun(Key, Value, Acc) ->
            case Key of
                full_name -> Acc#resume{full_name = Value};
                professional_title -> Acc#resume{professional_title = Value};
                headline -> Acc#resume{headline = Value};
                summary -> Acc#resume{summary = Value};
                email -> Acc#resume{email = Value};
                phone -> Acc#resume{phone = Value};
                location -> Acc#resume{location = Value};
                website -> Acc#resume{website = Value};
                linkedin_url -> Acc#resume{linkedin_url = Value};
                github_url -> Acc#resume{github_url = Value};
                portfolio_url -> Acc#resume{portfolio_url = Value};
                technical_skills -> Acc#resume{technical_skills = Value};
                soft_skills -> Acc#resume{soft_skills = Value};
                languages -> Acc#resume{languages = Value};
                job_preferences -> Acc#resume{job_preferences = Value};
                visibility -> Acc#resume{visibility = Value};
                status -> Acc#resume{status = Value};
                _ -> Acc
            end
        end, Resume, UpdateData).

    delete_resume(ResumeID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [_Resume] ->
                    mnesia:delete({resume, ResumeID}),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    set_primary_resume(UserID, ResumeID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    case Resume#resume.user_id =:= UserID of
                        false -> {error, unauthorized};
                        true ->
                            AllResumes = mnesia:match_object(#resume{user_id = UserID, _ = '_'}),
                            lists:foreach(fun(R) ->
                                mnesia:write(R#resume{is_primary = false})
                            end, AllResumes),
                            mnesia:write(Resume#resume{
                                is_primary = true,
                                date_updated = calendar:universal_time()
                            }),
                            ok
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    upload_resume_pdf(ResumeID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    mnesia:write(Resume#resume{
                        resume_pdf_ipfs_hash = IPFSHash,
                        resume_type = "uploaded_pdf",
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    upload_cover_letter(ResumeID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    mnesia:write(Resume#resume{
                        cover_letter_ipfs_hash = IPFSHash,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    upload_portfolio(ResumeID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    mnesia:write(Resume#resume{
                        portfolio_ipfs_hash = IPFSHash,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_work_experience(ResumeID, Experience) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentExperience = Resume#resume.work_experience,
                    UpdatedExperience = [Experience | CurrentExperience],
                    TotalYears = calculate_total_experience(UpdatedExperience),
                    mnesia:write(Resume#resume{
                        work_experience = UpdatedExperience,
                        total_years_experience = TotalYears,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_work_experience(ResumeID, ExperienceID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentExperience = Resume#resume.work_experience,
                    UpdatedExperience = lists:filter(fun(Exp) ->
                        case Exp of
                            #work_experience{id = ID} -> ID =/= ExperienceID;
                            _ -> true
                        end
                    end, CurrentExperience),
                    TotalYears = calculate_total_experience(UpdatedExperience),
                    mnesia:write(Resume#resume{
                        work_experience = UpdatedExperience,
                        total_years_experience = TotalYears,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    update_work_experience(ResumeID, ExperienceID, UpdatedExperience) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentExperiences = Resume#resume.work_experience,
                    UpdatedExperiences = lists:map(fun(Exp) ->
                        case Exp of
                            #work_experience{id = ID} when ID =:= ExperienceID ->
                                UpdatedExperience;
                            _ ->
                                Exp
                        end
                    end, CurrentExperiences),
                    TotalYears = calculate_total_experience(UpdatedExperiences),
                    mnesia:write(Resume#resume{
                        work_experience = UpdatedExperiences,
                        total_years_experience = TotalYears,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_education(ResumeID, Education) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentEducation = Resume#resume.education,
                    UpdatedEducation = [Education | CurrentEducation],
                    mnesia:write(Resume#resume{
                        education = UpdatedEducation,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_education(ResumeID, EducationID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentEducation = Resume#resume.education,
                    UpdatedEducation = lists:filter(fun(Edu) ->
                        case Edu of
                            #education{id = ID} -> ID =/= EducationID;
                            _ -> true
                        end
                    end, CurrentEducation),
                    mnesia:write(Resume#resume{
                        education = UpdatedEducation,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_skill(ResumeID, Skill) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentSkills = Resume#resume.technical_skills,
                    case lists:member(Skill, CurrentSkills) of
                        true -> {error, skill_already_exists};
                        false ->
                            UpdatedSkills = [Skill | CurrentSkills],
                            mnesia:write(Resume#resume{
                                technical_skills = UpdatedSkills,
                                date_updated = calendar:universal_time()
                            }),
                            ok
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_skill(ResumeID, Skill) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentSkills = Resume#resume.technical_skills,
                    UpdatedSkills = lists:delete(Skill, CurrentSkills),
                    mnesia:write(Resume#resume{
                        technical_skills = UpdatedSkills,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_certification_to_resume(ResumeID, Certification) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentCertifications = Resume#resume.certifications,
                    UpdatedCertifications = [Certification | CurrentCertifications],
                    mnesia:write(Resume#resume{
                        certifications = UpdatedCertifications,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_project(ResumeID, Project) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentProjects = Resume#resume.projects,
                    UpdatedProjects = [Project | CurrentProjects],
                    mnesia:write(Resume#resume{
                        projects = UpdatedProjects,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_project(ResumeID, ProjectID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    CurrentProjects = Resume#resume.projects,
                    UpdatedProjects = lists:filter(fun(Proj) ->
                        case Proj of
                            #job_project{id = ID} -> ID =/= ProjectID;
                            _ -> true
                        end
                    end, CurrentProjects),
                    mnesia:write(Resume#resume{
                        projects = UpdatedProjects,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    increment_resume_views(ResumeID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    mnesia:write(Resume#resume{
                        views = Resume#resume.views + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    increment_resume_downloads(ResumeID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    mnesia:write(Resume#resume{
                        downloads = Resume#resume.downloads + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    record_recruiter_contact(ResumeID, RecruiterID) ->
        Fun = fun() ->
            case mnesia:read(resume, ResumeID) of
                [] -> {error, resume_not_found};
                [Resume] ->
                    mnesia:write(Resume#resume{
                        recruiter_contacts = Resume#resume.recruiter_contacts + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    calculate_total_experience(Experiences) ->
        TotalMonths = lists:foldl(fun(Exp, Acc) ->
            case Exp of
                #work_experience{duration_months = Months} ->
                    Acc + Months;
                _ ->
                    Acc
            end
        end, 0, Experiences),
        TotalMonths / 12.

        create_job_application(UserID, JobID, ApplicationData) ->
            Fun = fun() ->
                case mnesia:read(job_posting, JobID) of
                    [] -> {error, job_not_found};
                    [_JobPosting] ->
                        case mnesia:read(user, UserID) of
                            [] -> {error, user_not_found};
                            [_User] ->
                                Now = calendar:universal_time(),
                                ApplicationID = nanoid:gen(),
                                UniqueAppID = generate_application_id(),

                                Application = #job_application{
                                    id = ApplicationID,
                                    application_id = UniqueAppID,
                                    job_posting_id = JobID,
                                    applicant_user_id = UserID,
                                    resume_id = maps:get(resume_id, ApplicationData),

                                    status = "submitted",

                                    resume_ipfs_hash = maps:get(resume_ipfs_hash, ApplicationData, ""),
                                    resume_pdf_ipfs_hash = maps:get(resume_pdf_ipfs_hash, ApplicationData, ""),
                                    cover_letter = maps:get(cover_letter, ApplicationData, ""),
                                    cover_letter_ipfs_hash = maps:get(cover_letter_ipfs_hash, ApplicationData, ""),

                                    screening_question_answers = maps:get(screening_question_answers, ApplicationData, []),
                                    custom_question_answers = maps:get(custom_question_answers, ApplicationData, []),

                                    referred_by_user_id = maps:get(referred_by_user_id, ApplicationData, undefined),
                                    application_source = maps:get(application_source, ApplicationData, "direct"),

                                    consent_given = true,
                                    consent_date = Now,

                                    date_submitted = Now,
                                    date_updated = Now
                                },

                                mnesia:write(Application),
                                add_application(JobID, ApplicationID),
                                {ok, ApplicationID}
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, {ok, ApplicationID}} -> {ok, ApplicationID};
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_job_application_by_id(ApplicationID) ->
            Fun = fun() ->
                case mnesia:read(job_application, ApplicationID) of
                    [] -> {error, application_not_found};
                    [Application] -> {ok, Application}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, {ok, Application}} -> Application;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_applications_by_job(JobID) ->
            Fun = fun() ->
                mnesia:match_object(#job_application{job_posting_id = JobID, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, Applications} -> Applications;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_applications_by_user(UserID) ->
            Fun = fun() ->
                mnesia:match_object(#job_application{applicant_user_id = UserID, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, Applications} -> Applications;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        update_application_status(ApplicationID, NewStatus) ->
            ValidStatuses = ["submitted", "under_review", "shortlisted", "interviewing",
                             "offer_extended", "accepted", "rejected", "withdrawn"],
            case lists:member(NewStatus, ValidStatuses) of
                false -> {error, invalid_status};
                true ->
                    Fun = fun() ->
                        case mnesia:read(job_application, ApplicationID) of
                            [] -> {error, application_not_found};
                            [Application] ->
                                UpdatedApplication = Application#job_application{
                                    status = NewStatus,
                                    date_updated = calendar:universal_time()
                                },

                                FinalApplication = case NewStatus of
                                    "rejected" ->
                                        UpdatedApplication#job_application{
                                            rejection_date = calendar:universal_time()
                                        };
                                    _ ->
                                        UpdatedApplication
                                end,

                                mnesia:write(FinalApplication),
                                ok
                        end
                    end,
                    case mnesia:transaction(Fun) of
                        {atomic, ok} -> ok;
                        {atomic, {error, Reason}} -> {error, Reason};
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        withdraw_application(ApplicationID) ->
            Fun = fun() ->
                case mnesia:read(job_application, ApplicationID) of
                    [] -> {error, application_not_found};
                    [Application] ->
                        mnesia:write(Application#job_application{
                            status = "withdrawn",
                            withdrawn = true,
                            withdraw_date = calendar:universal_time(),
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_interview_stage(ApplicationID, InterviewStage) ->
            Fun = fun() ->
                case mnesia:read(job_application, ApplicationID) of
                    [] -> {error, application_not_found};
                    [Application] ->
                        CurrentStages = Application#job_application.interview_stages,
                        UpdatedStages = [InterviewStage | CurrentStages],
                        mnesia:write(Application#job_application{
                            interview_stages = UpdatedStages,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        update_interview_stage(ApplicationID, UpdatedStage) ->
            Fun = fun() ->
                case mnesia:read(job_application, ApplicationID) of
                    [] -> {error, application_not_found};
                    [Application] ->
                        CurrentStages = Application#job_application.interview_stages,
                        StageID = UpdatedStage#interview_stage.id,
                        UpdatedStages = lists:map(fun(Stage) ->
                            case Stage of
                                #interview_stage{id = ID} when ID =:= StageID ->
                                    UpdatedStage;
                                _ ->
                                    Stage
                            end
                        end, CurrentStages),
                        mnesia:write(Application#job_application{
                            interview_stages = UpdatedStages,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_screening_answer(ApplicationID, QuestionID, Answer) ->
            Fun = fun() ->
                case mnesia:read(job_application, ApplicationID) of
                    [] -> {error, application_not_found};
                    [Application] ->
                        CurrentAnswers = Application#job_application.screening_question_answers,
                        NewAnswer = {QuestionID, Answer},
                        UpdatedAnswers = [NewAnswer | CurrentAnswers],
                        mnesia:write(Application#job_application{
                            screening_question_answers = UpdatedAnswers,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_skills_test_result(ApplicationID, TestID, Result) ->
            Fun = fun() ->
                case mnesia:read(job_application, ApplicationID) of
                    [] -> {error, application_not_found};
                    [Application] ->
                        CurrentResults = Application#job_application.skills_test_results,
                        NewResult = {TestID, Result},
                        UpdatedResults = [NewResult | CurrentResults],
                        mnesia:write(Application#job_application{
                            skills_test_results = UpdatedResults,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        save_job(UserID, JobID) ->
            Fun = fun() ->
                case mnesia:read(job_posting, JobID) of
                    [] -> {error, job_not_found};
                    [_JobPosting] ->
                        SavedJobID = nanoid:gen(),
                        SavedJob = #saved_job{
                            id = SavedJobID,
                            user_id = UserID,
                            job_posting_id = JobID,
                            date_saved = calendar:universal_time(),
                            applied = false
                        },
                        mnesia:write(SavedJob),
                        {ok, SavedJobID}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, {ok, SavedJobID}} -> {ok, SavedJobID};
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        unsave_job(UserID, JobID) ->
            Fun = fun() ->
                SavedJobs = mnesia:match_object(#saved_job{user_id = UserID, job_posting_id = JobID, _ = '_'}),
                lists:foreach(fun(SavedJob) ->
                    mnesia:delete({saved_job, SavedJob#saved_job.id})
                end, SavedJobs),
                ok
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_saved_jobs(UserID) ->
            Fun = fun() ->
                mnesia:match_object(#saved_job{user_id = UserID, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, SavedJobs} -> SavedJobs;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        is_job_saved(UserID, JobID) ->
            Fun = fun() ->
                SavedJobs = mnesia:match_object(#saved_job{user_id = UserID, job_posting_id = JobID, _ = '_'}),
                length(SavedJobs) > 0
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, _Reason} -> false
            end.

        create_job_alert(UserID, AlertData) ->
            Fun = fun() ->
                case mnesia:read(user, UserID) of
                    [] -> {error, user_not_found};
                    [_User] ->
                        Now = calendar:universal_time(),
                        AlertID = nanoid:gen(),

                        JobAlert = #job_alert{
                            id = AlertID,
                            user_id = UserID,
                            alert_name = maps:get(alert_name, AlertData, "Job Alert"),

                            keywords = maps:get(keywords, AlertData, []),
                            job_titles = maps:get(job_titles, AlertData, []),
                            companies = maps:get(companies, AlertData, []),
                            locations = maps:get(locations, AlertData, []),
                            remote_only = maps:get(remote_only, AlertData, false),
                            employment_types = maps:get(employment_types, AlertData, []),
                            experience_levels = maps:get(experience_levels, AlertData, []),
                            salary_min = maps:get(salary_min, AlertData, 0.0),
                            salary_currency = maps:get(salary_currency, AlertData, "USD"),
                            industries = maps:get(industries, AlertData, []),

                            frequency = maps:get(frequency, AlertData, "daily"),
                            notification_methods = maps:get(notification_methods, AlertData, ["email"]),
                            max_alerts_per_period = maps:get(max_alerts_per_period, AlertData, 10),
                            active = true,

                            ai_matching_enabled = maps:get(ai_matching_enabled, AlertData, false),
                            min_match_score = maps:get(min_match_score, AlertData, 70.0),

                            date_created = Now,
                            date_updated = Now
                        },

                        mnesia:write(JobAlert),
                        {ok, AlertID}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, {ok, AlertID}} -> {ok, AlertID};
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        update_job_alert(AlertID, UpdateData) ->
            Fun = fun() ->
                case mnesia:read(job_alert, AlertID) of
                    [] -> {error, alert_not_found};
                    [Alert] ->
                        UpdatedAlert = apply_alert_updates(Alert, UpdateData),
                        mnesia:write(UpdatedAlert#job_alert{
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        apply_alert_updates(Alert, UpdateData) ->
            maps:fold(fun(Key, Value, Acc) ->
                case Key of
                    alert_name -> Acc#job_alert{alert_name = Value};
                    keywords -> Acc#job_alert{keywords = Value};
                    job_titles -> Acc#job_alert{job_titles = Value};
                    locations -> Acc#job_alert{locations = Value};
                    remote_only -> Acc#job_alert{remote_only = Value};
                    employment_types -> Acc#job_alert{employment_types = Value};
                    salary_min -> Acc#job_alert{salary_min = Value};
                    frequency -> Acc#job_alert{frequency = Value};
                    notification_methods -> Acc#job_alert{notification_methods = Value};
                    active -> Acc#job_alert{active = Value};
                    _ -> Acc
                end
            end, Alert, UpdateData).

        delete_job_alert(AlertID) ->
            Fun = fun() ->
                case mnesia:read(job_alert, AlertID) of
                    [] -> {error, alert_not_found};
                    [_Alert] ->
                        mnesia:delete({job_alert, AlertID}),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_job_alerts_by_user(UserID) ->
            Fun = fun() ->
                mnesia:match_object(#job_alert{user_id = UserID, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, Alerts} -> Alerts;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        check_job_matches_alert(JobPosting, Alert) ->
            KeywordMatch = case Alert#job_alert.keywords of
                [] -> true;
                Keywords ->
                    Title = string:lowercase(JobPosting#job_posting.title),
                    Description = string:lowercase(JobPosting#job_posting.description),
                    lists:any(fun(Keyword) ->
                        LowerKeyword = string:lowercase(Keyword),
                        string:find(Title, LowerKeyword) =/= nomatch orelse
                        string:find(Description, LowerKeyword) =/= nomatch
                    end, Keywords)
            end,

            LocationMatch = case Alert#job_alert.locations of
                [] -> true;
                AlertLocations ->
                    JobLocations = JobPosting#job_posting.locations,
                    lists:any(fun(AlertLoc) ->
                        lists:any(fun(JobLoc) ->
                            AlertCity = string:lowercase(AlertLoc),
                            JobCity = string:lowercase(maps:get(city, JobLoc, "")),
                            string:find(JobCity, AlertCity) =/= nomatch
                        end, JobLocations)
                    end, AlertLocations)
            end,

            RemoteMatch = case Alert#job_alert.remote_only of
                true -> JobPosting#job_posting.location_type =:= "remote";
                false -> true
            end,

            EmploymentTypeMatch = case Alert#job_alert.employment_types of
                [] -> true;
                Types -> lists:member(JobPosting#job_posting.employment_type, Types)
            end,

            SalaryMatch = case Alert#job_alert.salary_min of
                0.0 -> true;
                MinSalary ->
                    SalaryRange = JobPosting#job_posting.salary_range,
                    maps:get(min, SalaryRange, 0.0) >= MinSalary
            end,

            KeywordMatch andalso LocationMatch andalso RemoteMatch andalso
            EmploymentTypeMatch andalso SalaryMatch.

        generate_application_id() ->
            {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
            Random = rand:uniform(999999),
            lists:flatten(io_lib:format("APP-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B-~6..0B",
                                        [Year, Month, Day, Hour, Minute, Second, Random])).

            populate_resume_sample_data(ResumeID) ->
                Fun = fun() ->
                    case mnesia:read(resume, ResumeID) of
                        [] -> {error, resume_not_found};
                        [Resume] ->
                            WorkExp1 = #{
                                id => nanoid:gen(),
                                company_name => "Tech Corp",
                                position_title => "Senior Software Engineer",
                                employment_type => "full_time",
                                location => #{
                                    city => "San Francisco",
                                    state => "California",
                                    country => "USA",
                                    remote => false
                                },
                                start_date => {2020, 1},
                                end_date => "present",
                                current_position => true,
                                duration_months => 48,
                                description => "Lead development of cloud-native applications",
                                responsibilities => [
                                    "Architected microservices infrastructure",
                                    "Led team of 5 engineers",
                                    "Implemented CI/CD pipelines"
                                ],
                                achievements => [
                                    "Reduced deployment time by 60%",
                                    "Increased system reliability to 99.9%"
                                ],
                                technologies_used => ["Erlang", "Elixir", "Phoenix", "Docker", "Kubernetes"]
                            },

                            WorkExp2 = #{
                                id => nanoid:gen(),
                                company_name => "StartupXYZ",
                                position_title => "Software Engineer",
                                employment_type => "full_time",
                                location => #{
                                    city => "New York",
                                    state => "New York",
                                    country => "USA",
                                    remote => false
                                },
                                start_date => {2018, 6},
                                end_date => {2019, 12},
                                current_position => false,
                                duration_months => 18,
                                description => "Full-stack development of web applications",
                                responsibilities => [
                                    "Developed RESTful APIs",
                                    "Built responsive frontend interfaces",
                                    "Collaborated with product team"
                                ],
                                achievements => [
                                    "Launched 3 major features",
                                    "Improved page load time by 40%"
                                ],
                                technologies_used => ["JavaScript", "React", "Node.js", "PostgreSQL"]
                            },

                            Education1 = #{
                                id => nanoid:gen(),
                                institution_name => "Stanford University",
                                degree => "Master of Science",
                                field_of_study => "Computer Science",
                                start_date => {2016, 9},
                                end_date => {2018, 5},
                                grade => "3.8",
                                grade_type => "GPA",
                                activities => ["ACM Chapter", "Hackathon Organizer"],
                                honors => ["Dean's List", "Graduate Research Fellowship"],
                                relevant_coursework => ["Distributed Systems", "Machine Learning", "Algorithms"],
                                location => #{
                                    city => "Stanford",
                                    country => "USA"
                                }
                            },

                            Education2 = #{
                                id => nanoid:gen(),
                                institution_name => "UC Berkeley",
                                degree => "Bachelor of Science",
                                field_of_study => "Computer Engineering",
                                start_date => {2012, 9},
                                end_date => {2016, 5},
                                grade => "3.7",
                                grade_type => "GPA",
                                activities => ["IEEE Student Chapter"],
                                honors => ["Summa Cum Laude"],
                                location => #{
                                    city => "Berkeley",
                                    country => "USA"
                                }
                            },

                            TechnicalSkills = [
                                "Erlang", "Elixir", "Phoenix Framework",
                                "JavaScript", "React", "Node.js",
                                "Python", "Django", "FastAPI",
                                "Docker", "Kubernetes", "AWS",
                                "PostgreSQL", "MongoDB", "Redis",
                                "Git", "CI/CD", "Microservices"
                            ],

                            SoftSkills = [
                                "Leadership", "Team Collaboration",
                                "Problem Solving", "Communication",
                                "Project Management", "Mentoring"
                            ],

                            Languages = [
                                #{language => "English", proficiency => "Native"},
                                #{language => "Spanish", proficiency => "Fluent"},
                                #{language => "French", proficiency => "Intermediate"}
                            ],

                            Cert1 = #{
                                id => nanoid:gen(),
                                name => "AWS Certified Solutions Architect",
                                issuing_organization => "Amazon Web Services",
                                issue_date => {2022, 3, 15},
                                credential_id => "AWS-2022-123456",
                                credential_url => "https://aws.amazon.com/verification"
                            },

                            Cert2 = #{
                                id => nanoid:gen(),
                                name => "Certified Kubernetes Administrator",
                                issuing_organization => "Cloud Native Computing Foundation",
                                issue_date => {2021, 8, 10},
                                credential_id => "CKA-2021-789012",
                                credential_url => "https://cncf.io/certification/verify"
                            },

                            Project1 = #{
                                id => nanoid:gen(),
                                title => "Real-Time Chat Application",
                                description => "Built a scalable real-time chat platform using Phoenix LiveView and WebSockets supporting 10,000+ concurrent users",
                                role => "Lead Developer",
                                start_date => {2023, 1, 1},
                                end_date => {2023, 6, 30},
                                current_project => false,
                                technologies => ["Elixir", "Phoenix", "LiveView", "PostgreSQL", "Redis"],
                                url => "https://github.com/example/chat-app",
                                achievements => ["Handled 10K concurrent connections", "99.99% uptime"]
                            },

                            Project2 = #{
                                id => nanoid:gen(),
                                title => "E-Commerce Platform",
                                description => "Developed a full-featured e-commerce platform with payment integration and inventory management",
                                role => "Full-Stack Developer",
                                start_date => {2022, 3, 1},
                                end_date => {2022, 12, 31},
                                current_project => false,
                                technologies => ["React", "Node.js", "MongoDB", "Stripe API"],
                                url => "https://github.com/example/ecommerce",
                                achievements => ["Processed $1M+ in transactions", "5-star app store rating"]
                            },

                            UpdatedResume = Resume#resume{
                                work_experience = [WorkExp1, WorkExp2],
                                education = [Education1, Education2],
                                technical_skills = TechnicalSkills,
                                soft_skills = SoftSkills,
                                languages = Languages,
                                certifications = [Cert1, Cert2],
                                projects = [Project1, Project2],
                                total_years_experience = 5.5,
                                status = "active",
                                date_updated = calendar:universal_time(),
                                version = Resume#resume.version + 1
                            },

                            mnesia:write(UpdatedResume),
                            {ok, updated}
                    end
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, updated}} -> {ok, updated};
                    {atomic, {error, Reason}} -> {error, Reason};
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            get_complete_resume(ResumeID) ->
                Fun = fun() ->
                    case mnesia:read(resume, ResumeID) of
                        [] -> {error, resume_not_found};
                        [Resume] ->
                            {ok, #{
                                id => Resume#resume.id,
                                user_id => Resume#resume.user_id,
                                resume_type => Resume#resume.resume_type,
                                is_primary => Resume#resume.is_primary,
                                visibility => Resume#resume.visibility,
                                status => Resume#resume.status,

                                full_name => Resume#resume.full_name,
                                professional_title => Resume#resume.professional_title,
                                headline => Resume#resume.headline,
                                summary => Resume#resume.summary,

                                email => Resume#resume.email,
                                phone => Resume#resume.phone,
                                location => Resume#resume.location,
                                website => Resume#resume.website,
                                linkedin_url => Resume#resume.linkedin_url,
                                github_url => Resume#resume.github_url,
                                portfolio_url => Resume#resume.portfolio_url,

                                work_experience => Resume#resume.work_experience,
                                education => Resume#resume.education,
                                total_years_experience => Resume#resume.total_years_experience,

                                technical_skills => Resume#resume.technical_skills,
                                soft_skills => Resume#resume.soft_skills,
                                languages => Resume#resume.languages,

                                certifications => Resume#resume.certifications,
                                projects => Resume#resume.projects,
                                job_preferences => Resume#resume.job_preferences,

                                date_created => Resume#resume.date_created,
                                date_updated => Resume#resume.date_updated,
                                version => Resume#resume.version,
                                views => Resume#resume.views,
                                downloads => Resume#resume.downloads
                            }}
                    end
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, ResumeMap}} -> {ok, ResumeMap};
                    {atomic, {error, Reason}} -> {error, Reason};
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.
