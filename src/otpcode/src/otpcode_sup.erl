-module(otpcode_sup).

-include("records.hrl").
-include("wallet_records.hrl").
-include("business.hrl").
-include("job_records.hrl").
-include("ml_records.hrl").
-include("media_records.hrl").
-include("kademlia/kademlia.hrl").
-include("supervisor.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1, init_admin_system/0]).
-export([start_distributed/0, add_node/1, add_extra_nodes/1, create_tables_on_nodes/1]).

-define(SERVER, ?MODULE).
-define(MNESIA_DIR, "Mnesia/").
-define(CLUSTER_NODES, ['social_net@node1', 'social_net@node2', 'social_net@node3']).
-define(TABLES, [post, notif, user, blog_post, comment, blog_comment, like, reply, chat, media, report, knode, business, ads, quantum,
 ai_user, ai_post, ai_chat, ai_media, ai_business, ai_ads, p2p_node,
 pin_info, pin_params, pin_history, bulk_operation, scheduled_job, rate_limiter_usage, pin_info_lookup, pin_health, storage_quota, presence,
 dataset, competition, notebook, model, video, music, album, playlist, ai_video, ai_music, media_view, livestream,
 learning_path, learning_resource, learning_module, lesson, user_learning_progress, quiz, quiz_question, quiz_attempt, exercise,
 exercise_submission, project, project_submission, certificate, badge, user_badge, enrollment, user_completion,
 discussion_post, study_group, mentor_session, live_class, instructor_profile, admin_action, time_tracking, bookmark,
 resource_rating, learning_track, verified_instructor, instructor_request, lesson_progress, module_progress,
 video_upload_session, content_comment, content_reaction, student_question, question_answer, path_review,
 instructor_analytics, content_analytics, learning_notification, learning_schedule, group, group_member, group_message,
 group_invite, group_admin, channel, channel_post, channel_subscriber, channel_invite,
 job_posting, resume, work_experience, education, job_application, interview_stage, saved_job, job_alert,
 talent_pool, candidate_search, recruiter_contact, skill, skill_endorsement, skill_assessment, company_review,
 interview_prep, job_market_insights, user_job_analytics, job_message, background_check, employment_verification,
 job_referral, job_board_settings, leaderboard, artist, artist_request,
 solana_wallet, solana_transaction, solana_airdrop, solana_airdrop_recipient, solana_stake_account, solana_token_account, solana_nft,
 near_wallet, near_transaction, near_access_key, near_stake, near_implicit_account, near_social_post]).

start_link() ->
    case initialize() of
        ok ->
            supervisor:start_link({local, ?SERVER}, ?MODULE, []);
        {error, Reason} ->
            {error, Reason}
    end.

start_distributed() ->
    logger:info("Starting distributed Mnesia with nodes: ~p", [?CLUSTER_NODES]),
    case net_kernel:start([node_name(), shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> error({net_kernel_start_failed, Reason})
    end,

    [net_kernel:connect_node(Node) || Node <- ?CLUSTER_NODES],

    case initialize() of
        ok ->
            add_extra_nodes(?CLUSTER_NODES),
            create_tables_on_nodes(?CLUSTER_NODES),
            ok;
        Error -> Error
    end.

node_name() ->
    case node() of
        nonode@nohost ->
            list_to_atom("social_net_" ++ integer_to_list(erlang:system_time(millisecond)) ++ "@localhost");
        Name -> Name
    end.

add_node(Node) ->
    logger:info("Adding node ~p to Mnesia schema", [Node]),
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} ->
            mnesia:change_table_copy_type(schema, Node, disc_copies),
            ok;
        {ok, []} ->
            logger:warning("Node ~p could not be added to Mnesia", [Node]),
            {error, node_not_added};
        {error, Reason} ->
            logger:error("Failed to add node: ~p", [Reason]),
            {error, Reason}
    end.

add_extra_nodes(Nodes) ->
    logger:info("Adding nodes ~p to Mnesia schema", [Nodes]),
    Results = [add_node(Node) || Node <- Nodes, Node =/= node()],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false -> {error, some_nodes_not_added}
    end.

create_tables_on_nodes(Nodes) ->
    logger:info("Creating tables on nodes: ~p", [Nodes]),
    Results = [create_table_on_nodes(Table, Nodes) || Table <- ?TABLES],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false -> {error, some_tables_not_created}
    end.

create_table_on_nodes(Table, Nodes) ->
    Attributes = table_attributes(Table),
    TableType = table_type(Table),
    TableNodes = [node() | [N || N <- Nodes, N =/= node()]],

    TableInfo = mnesia:table_info(Table, all),
    case TableInfo of
        [{aborted, no_exists}] ->
            case mnesia:create_table(Table, [
                {attributes, Attributes},
                {disc_copies, TableNodes},
                {type, TableType}
            ]) of
                {atomic, ok} ->
                    logger:info("Table ~p created successfully on nodes ~p with type ~p", [Table, TableNodes, TableType]),
                    ok;
                {aborted, Reason} ->
                    logger:error("Failed to create table ~p: ~p", [Table, Reason]),
                    {error, {create_table_failed, Table, Reason}}
            end;
        _ ->
            [add_table_copy(Table, Node) || Node <- TableNodes],
            ok
    end.

add_table_copy(Table, Node) ->
    case mnesia:add_table_copy(Table, Node, disc_copies) of
        {atomic, ok} ->
            logger:info("Added table ~p to node ~p", [Table, Node]),
            ok;
        {aborted, {already_exists, Table, Node}} ->
            logger:info("Table ~p already exists on node ~p", [Table, Node]),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to add table ~p to node ~p: ~p", [Table, Node, Reason]),
            {error, {add_table_copy_failed, Table, Node, Reason}}
    end.

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 50,
        period => 1
    },
    ChildSpecs = [
        ?USER_SERVER,
        ?USER_LEVEL_SERVER,
        ?POST_SERVER,
        ?TOKEN_SERVER,
        ?CHAT_SERVER,
        ?BLOG_SERVER,
        ?MEDIA_SERVER,
        ?NOTIF_EVENT,
        ?BUSINESS_SERVER,
        ?QUANTUM_SERVER,
        ?ADS_SERVER,
        ?AI_USER_SERVER,
        ?AI_POST_SERVER,
        ?AI_CHAT_SERVER,
        ?AI_MEDIA_SERVER,
        ?AI_BUSINESS_SERVER,
        ?AI_ADS_SERVER,
        ?PIN_POST_SERVER,
        ?STORAGE_QUOTA_SERVER,
        ?RATE_LIMITER_SERVER,
        ?CONTENT_CACHE,
        ?SOLANA_WALLET_SERVER,
        ?NEAR_WALLET_SERVER
    ],
    {ok, {SupFlags, ChildSpecs}}.

initialize() ->
    try
        logger:info("Initializing distributed otpcode application..."),
        ok = set_mnesia_dir(),
        ok = create_mnesia_schema(),
        ok = start_mnesia(),
        ok = change_table_copy_type(),
        ok = start_ssl(),
        ok = start_inets(),
        ok = create_tables(),
        ok = wait_for_tables(),
        Tables = mnesia:system_info(tables),
        logger:info("Existing tables before index creation: ~p", [Tables]),
        ok = create_table_indexes(),
        ok = init_admin_system(),
        logger:info("otpcode application initialized successfully."),
        ok
    catch
        error:{Error, Reason} ->
            logger:error("Initialization failed: ~p - ~p", [Error, Reason]),
            {error, {Error, Reason}}
    end.

set_mnesia_dir() ->
    application:set_env(mnesia, dir, ?MNESIA_DIR).

create_mnesia_schema() ->
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} ->
            logger:info("Mnesia schema already exists."),
            ok;
        {error, Reason} -> error({create_schema_failed, Reason})
    end.

start_mnesia() ->
    application:start(mnesia).

change_table_copy_type() ->
    case mnesia:table_info(schema, storage_type) of
        disc_copies -> ok;
        _ ->
            case mnesia:change_table_copy_type(schema, node(), disc_copies) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> error({change_table_copy_type_failed, Reason})
            end
    end.

start_ssl() ->
    application:ensure_started(ssl).

start_inets() ->
    application:ensure_started(inets).

create_tables() ->
    Results = [create_table(Table) || Table <- ?TABLES],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false ->
            logger:info("Some tables already exist or failed to be created: ~p", [Results]),
            ok
    end.

create_table(Table) ->
    Attributes = table_attributes(Table),
    TableType = table_type(Table),
    case mnesia:create_table(Table, [
        {attributes, Attributes},
        {disc_copies, [node()]},
        {type, TableType}
    ]) of
        {atomic, ok} ->
            logger:info("Table ~p created successfully with type ~p", [Table, TableType]),
            ok;
        {aborted, {already_exists, _}} ->
            logger:info("Table ~p already exists", [Table]),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to create table ~p: ~p", [Table, Reason]),
            {error, {create_table_failed, Table, Reason}}
    end.

wait_for_tables() ->
    case mnesia:wait_for_tables(?TABLES, 5000) of
        ok ->
            logger:info("All tables are ready"),
            ok;
        {timeout, BadTables} ->
            logger:error("Timeout waiting for tables: ~p", [BadTables]),
            {error, {tables_not_ready, BadTables}};
        {error, Reason} ->
            logger:error("Error waiting for tables: ~p", [Reason]),
            {error, {wait_for_tables_failed, Reason}}
    end.

create_table_indexes() ->
    IndexesToCreate = [
        %% Core user indexes
        {user, username}, {user, email},
        %% Learning indexes
        {enrollment, user_id}, {enrollment, path_id},
        {user_completion, user_id}, {instructor_profile, user_id},
        {admin_action, admin_username}, {admin_action, content_type}, {admin_action, content_id},
        {bookmark, user_id}, {bookmark, resource_id},
        {resource_rating, resource_id}, {resource_rating, user_id},
        {lesson_progress, user_id}, {lesson_progress, lesson_id},
        {module_progress, user_id}, {module_progress, module_id},
        {content_comment, content_id}, {content_reaction, content_id},
        {student_question, lesson_id}, {question_answer, question_id},
        {path_review, path_id}, {learning_notification, user_id},
        {video_upload_session, lesson_id},
        %% Jobs indexes
        {job_posting, poster_id}, {job_posting, status},
        {resume, user_id},
        {job_application, job_posting_id}, {job_application, applicant_user_id}, {job_application, status},
        {saved_job, user_id}, {job_alert, user_id},
        {skill_endorsement, user_id}, {company_review, business_id},
        %% Solana indexes
        {solana_wallet, user_id}, {solana_wallet, public_key},
        {solana_transaction, wallet_id}, {solana_transaction, user_id}, {solana_transaction, signature},
        {solana_airdrop, wallet_id}, {solana_airdrop, user_id},
        {solana_airdrop_recipient, airdrop_id},
        {solana_stake_account, wallet_id}, {solana_stake_account, user_id}, {solana_stake_account, stake_account_address},
        {solana_token_account, wallet_id}, {solana_token_account, user_id},
        {solana_nft, wallet_id}, {solana_nft, user_id},
        %% NEAR indexes
        {near_wallet, user_id}, {near_wallet, account_id},
        {near_transaction, wallet_id}, {near_transaction, user_id},
        {near_access_key, wallet_id}, {near_access_key, user_id},
        {near_stake, wallet_id}, {near_stake, user_id},
        {near_implicit_account, wallet_id}, {near_implicit_account, user_id},
        {near_social_post, wallet_id}, {near_social_post, user_id}
    ],
    Results = [create_index(Table, Field) || {Table, Field} <- IndexesToCreate],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false ->
            logger:info("Some indexes already exist or couldn't be created: ~p", [Results]),
            ok
    end.

create_index(Table, Field) ->
    case mnesia:table_info(Table, attributes) of
        undefined ->
            logger:error("Table ~p does not exist. Cannot create index on ~p", [Table, Field]),
            {error, {table_not_found, Table}};
        Attributes ->
            case lists:member(Field, Attributes) of
                true ->
                    case mnesia:add_table_index(Table, Field) of
                        {atomic, ok} ->
                            logger:info("Index created on ~p.~p", [Table, Field]),
                            ok;
                        {aborted, {already_exists, Table, _}} ->
                            logger:info("Index on ~p.~p already exists", [Table, Field]),
                            ok;
                        {aborted, Reason} ->
                            logger:error("Failed to create index on ~p.~p: ~p", [Table, Field, Reason]),
                            {error, {create_index_failed, Table, Field, Reason}}
                    end;
                false ->
                    logger:error("Field ~p does not exist in table ~p", [Field, Table]),
                    {error, {field_not_found, Table, Field}}
            end
    end.

init_admin_system() ->
    logger:info("Initializing admin system..."),
    case admindb:init_default_admins() of
        ok ->
            Admins = admindb:list_admins(),
            logger:info("Admin system initialized. Default admins: ~p", [Admins]),
            ok;
        {error, Reason} ->
            logger:error("Failed to initialize admins: ~p", [Reason]),
            ok
    end.

table_type(post) -> ordered_set;
table_type(blog_post) -> ordered_set;
table_type(job_posting) -> ordered_set;
table_type(_) -> set.

table_attributes(post) -> record_info(fields, post);
table_attributes(notif) -> record_info(fields, notif);
table_attributes(user) -> record_info(fields, user);
table_attributes(blog_post) -> record_info(fields, blog_post);
table_attributes(comment) -> record_info(fields, comment);
table_attributes(blog_comment) -> record_info(fields, blog_comment);
table_attributes(like) -> record_info(fields, like);
table_attributes(reply) -> record_info(fields, reply);
table_attributes(chat) -> record_info(fields, chat);
table_attributes(media) -> record_info(fields, media);
table_attributes(report) -> record_info(fields, report);
table_attributes(knode) -> record_info(fields, knode);
table_attributes(business) -> record_info(fields, business);
table_attributes(ads) -> record_info(fields, ads);
table_attributes(quantum) -> record_info(fields, quantum);
table_attributes(ai_user) -> record_info(fields, ai_user);
table_attributes(ai_post) -> record_info(fields, ai_post);
table_attributes(ai_chat) -> record_info(fields, ai_chat);
table_attributes(ai_media) -> record_info(fields, ai_media);
table_attributes(ai_business) -> record_info(fields, ai_business);
table_attributes(ai_ads) -> record_info(fields, ai_ads);
table_attributes(p2p_node) -> record_info(fields, p2p_node);
table_attributes(pin_info) -> record_info(fields, pin_info);
table_attributes(pin_params) -> record_info(fields, pin_params);
table_attributes(pin_history) -> record_info(fields, pin_history);
table_attributes(bulk_operation) -> record_info(fields, bulk_operation);
table_attributes(scheduled_job) -> record_info(fields, scheduled_job);
table_attributes(rate_limiter_usage) -> record_info(fields, rate_limiter_usage);
table_attributes(pin_info_lookup) -> record_info(fields, pin_info_lookup);
table_attributes(pin_health) -> record_info(fields, pin_health);
table_attributes(storage_quota) -> record_info(fields, storage_quota);
table_attributes(presence) -> record_info(fields, presence);
table_attributes(dataset) -> record_info(fields, dataset);
table_attributes(competition) -> record_info(fields, competition);
table_attributes(notebook) -> record_info(fields, notebook);
table_attributes(model) -> record_info(fields, model);
table_attributes(video) -> record_info(fields, video);
table_attributes(music) -> record_info(fields, music);
table_attributes(album) -> record_info(fields, album);
table_attributes(playlist) -> record_info(fields, playlist);
table_attributes(ai_video) -> record_info(fields, ai_video);
table_attributes(ai_music) -> record_info(fields, ai_music);
table_attributes(media_view) -> record_info(fields, media_view);
table_attributes(livestream) -> record_info(fields, livestream);
table_attributes(learning_path) -> record_info(fields, learning_path);
table_attributes(learning_resource) -> record_info(fields, learning_resource);
table_attributes(learning_module) -> record_info(fields, learning_module);
table_attributes(lesson) -> record_info(fields, lesson);
table_attributes(user_learning_progress) -> record_info(fields, user_learning_progress);
table_attributes(quiz) -> record_info(fields, quiz);
table_attributes(quiz_question) -> record_info(fields, quiz_question);
table_attributes(quiz_attempt) -> record_info(fields, quiz_attempt);
table_attributes(exercise) -> record_info(fields, exercise);
table_attributes(exercise_submission) -> record_info(fields, exercise_submission);
table_attributes(project) -> record_info(fields, project);
table_attributes(project_submission) -> record_info(fields, project_submission);
table_attributes(certificate) -> record_info(fields, certificate);
table_attributes(badge) -> record_info(fields, badge);
table_attributes(user_badge) -> record_info(fields, user_badge);
table_attributes(enrollment) -> record_info(fields, enrollment);
table_attributes(user_completion) -> record_info(fields, user_completion);
table_attributes(discussion_post) -> record_info(fields, discussion_post);
table_attributes(study_group) -> record_info(fields, study_group);
table_attributes(mentor_session) -> record_info(fields, mentor_session);
table_attributes(live_class) -> record_info(fields, live_class);
table_attributes(instructor_profile) -> record_info(fields, instructor_profile);
table_attributes(admin_action) -> record_info(fields, admin_action);
table_attributes(time_tracking) -> record_info(fields, time_tracking);
table_attributes(bookmark) -> record_info(fields, bookmark);
table_attributes(resource_rating) -> record_info(fields, resource_rating);
table_attributes(learning_track) -> record_info(fields, learning_track);
table_attributes(verified_instructor) -> record_info(fields, verified_instructor);
table_attributes(instructor_request) -> record_info(fields, instructor_request);
table_attributes(lesson_progress) -> record_info(fields, lesson_progress);
table_attributes(module_progress) -> record_info(fields, module_progress);
table_attributes(video_upload_session) -> record_info(fields, video_upload_session);
table_attributes(content_comment) -> record_info(fields, content_comment);
table_attributes(content_reaction) -> record_info(fields, content_reaction);
table_attributes(student_question) -> record_info(fields, student_question);
table_attributes(question_answer) -> record_info(fields, question_answer);
table_attributes(path_review) -> record_info(fields, path_review);
table_attributes(instructor_analytics) -> record_info(fields, instructor_analytics);
table_attributes(content_analytics) -> record_info(fields, content_analytics);
table_attributes(learning_notification) -> record_info(fields, learning_notification);
table_attributes(learning_schedule) -> record_info(fields, learning_schedule);
table_attributes(group) -> record_info(fields, group);
table_attributes(group_member) -> record_info(fields, group_member);
table_attributes(group_message) -> record_info(fields, group_message);
table_attributes(group_invite) -> record_info(fields, group_invite);
table_attributes(group_admin) -> record_info(fields, group_admin);
table_attributes(channel) -> record_info(fields, channel);
table_attributes(channel_post) -> record_info(fields, channel_post);
table_attributes(channel_subscriber) -> record_info(fields, channel_subscriber);
table_attributes(channel_invite) -> record_info(fields, channel_invite);
table_attributes(job_posting) -> record_info(fields, job_posting);
table_attributes(resume) -> record_info(fields, resume);
table_attributes(work_experience) -> record_info(fields, work_experience);
table_attributes(education) -> record_info(fields, education);
table_attributes(job_application) -> record_info(fields, job_application);
table_attributes(interview_stage) -> record_info(fields, interview_stage);
table_attributes(saved_job) -> record_info(fields, saved_job);
table_attributes(job_alert) -> record_info(fields, job_alert);
table_attributes(talent_pool) -> record_info(fields, talent_pool);
table_attributes(candidate_search) -> record_info(fields, candidate_search);
table_attributes(recruiter_contact) -> record_info(fields, recruiter_contact);
table_attributes(skill) -> record_info(fields, skill);
table_attributes(skill_endorsement) -> record_info(fields, skill_endorsement);
table_attributes(skill_assessment) -> record_info(fields, skill_assessment);
table_attributes(company_review) -> record_info(fields, company_review);
table_attributes(interview_prep) -> record_info(fields, interview_prep);
table_attributes(job_market_insights) -> record_info(fields, job_market_insights);
table_attributes(user_job_analytics) -> record_info(fields, user_job_analytics);
table_attributes(job_message) -> record_info(fields, job_message);
table_attributes(background_check) -> record_info(fields, background_check);
table_attributes(employment_verification) -> record_info(fields, employment_verification);
table_attributes(job_referral) -> record_info(fields, job_referral);
table_attributes(job_board_settings) -> record_info(fields, job_board_settings);
table_attributes(leaderboard) -> record_info(fields, leaderboard);
table_attributes(artist) -> record_info(fields, artist);
table_attributes(artist_request) -> record_info(fields, artist_request);
%% Solana
table_attributes(solana_wallet) -> record_info(fields, solana_wallet);
table_attributes(solana_transaction) -> record_info(fields, solana_transaction);
table_attributes(solana_airdrop) -> record_info(fields, solana_airdrop);
table_attributes(solana_airdrop_recipient) -> record_info(fields, solana_airdrop_recipient);
table_attributes(solana_stake_account) -> record_info(fields, solana_stake_account);
table_attributes(solana_token_account) -> record_info(fields, solana_token_account);
table_attributes(solana_nft) -> record_info(fields, solana_nft);
%% NEAR
table_attributes(near_wallet) -> record_info(fields, near_wallet);
table_attributes(near_transaction) -> record_info(fields, near_transaction);
table_attributes(near_access_key) -> record_info(fields, near_access_key);
table_attributes(near_stake) -> record_info(fields, near_stake);
table_attributes(near_implicit_account) -> record_info(fields, near_implicit_account);
table_attributes(near_social_post) -> record_info(fields, near_social_post).
