-record(competition, {
    id,
    title,
    description,
    creator_id,
    business_id = [],
    dataset_ids = [],
    dataset_cids = [],
    dataset_ipns = [],
    start_time,
    end_time,
    reward_type,
    reward_value,
    rules = #{},
    evaluation_metric,
    submission_count_limit,
    team_size_limit,
    submission_ids = [],
    participants = [],
    status,                        % draft | active | ended | voting | archived
    visibility = public,
    tags = [],
    date_created,
    date_updated,
    report = [],
    metadata = #{},

    discussion_cids = [],          % IPFS CIDs for discussion threads
    team_ids = [],                 % List of team IDs (not full records)
    evaluation_script_cid,         % IPFS CID for automated scoring script
    prize_distribution = #{},      % Map of {rank => prize_amount/token}
    external_data_allowed = false, % Whether participants can use external datasets
    late_submission_penalty = 0.0, % Penalty percentage per day late
    compute_quota = #{},           % {cpu_hours => X, gpu_hours => Y, tpu_hours => Z}
    featured = false,              % Whether competition is featured/promoted
    difficulty_level,              % beginner | intermediate | advanced | expert
    host_evaluation_cid            % IPFS CID for host's evaluation results/analysis
}).

-record(dataset, {
    id,
    title,
    description,
    creator_id,
    content_cid,
    ipns,
    metadata_cid,
    size_bytes,
    license,
    version,
    tags = [],
    visibility = public,
    downloads = 0,
    ratings = [],
    pin_info = [],
    competition_ids = [],
    date_created,
    date_updated,
    report = [],
    metadata = #{},

    version_history = [],          % List of {version, cid, timestamp, changes}
    schema_cid,                    % IPFS CID for data schema (JSON/Avro)
    sample_cid,                    % IPFS CID for sample/preview data (first 1000 rows)
    citation_count = 0,            % Number of times cited in notebooks/papers
    doi,                           % Digital Object Identifier for academic citation
    related_dataset_ids = [],
    data_quality_score = 0.0,      % Automated quality score (0-100)
    update_frequency,              % daily | weekly | monthly | static
    access_requests = [],          % List of {user_id, reason, status, timestamp}
    collaborators = [],            % List of user_ids who can edit metadata
    used_in_notebook_ids = [],     % Track which notebooks use this dataset
    used_in_model_ids = []         % Track which models trained on this dataset
}).

-record(notebook, {
    id,
    title,
    creator_id,
    description,
    content_cid,
    dataset_cids = [],
    competition_id,
    submission_id,
    language,
    kernel_type,
    environment = #{},
    collaborators = [],
    version_cids = [],
    outputs = [],
    visibility = public,
    execution_time_limit,
    execution_count = 0,
    likes = [],
    comments = [],
    date_created,
    date_updated,
    report = [],
    data = #{},
    forked_from,                   % Parent notebook ID if this is a fork
    fork_count = 0,                % Number of times this notebook was forked
    execution_logs_cid,            % IPFS CID for execution logs/errors
    dependencies_cid,              % IPFS CID for requirements.txt or environment.yml
    compute_time_used = 0,         % Total seconds of compute time used
    gpu_time_used = 0,             % Total seconds of GPU time used
    scheduled_runs = [],           % List of {cron_expression, enabled, last_run}
    notebook_type,                 % analysis | competition_submission | tutorial | research
    citations = [],                % List of {paper_doi, dataset_id, external_url}
    interactive_widgets = []       % List of {widget_type, widget_cid, position}
}).

-record(leaderboard, {
    competition_id,
    submission_ids = [],
    evaluation_metric,
    last_updated,
    data = #{},

    public_submission_ids = [],    % Submissions visible before end
    private_submission_ids = [],   % Final evaluation submissions
    evaluation_history = [],       % History: [{timestamp, submission_id, score}]
    team_submission_ids = [],      % Team submissions
    solo_submission_ids = [],      % Individual submissions
    best_scores_per_team = #{},    % Map of team_id => {submission_id, score}
    percentile_data = #{},         % Percentile rankings {percentile => score}
    score_distribution_cid,        % IPFS CID for visualization data
    benchmark_score,               % Baseline score to beat
    prize_cutoffs = #{}            % Map of {rank_range => prize_tier}
}).

-record(model, {
    id,
    creator_id,
    title,
    description,
    framework,
    task_type,
    file_cid,
    size_bytes,
    license,
    tags = [],
    visibility = public,
    downloads = 0,
    pin_info = [],
    deployment_info = #{},
    date_created,
    date_updated,
    report = [],
    data = #{},
    training_dataset_cids = [],    % IPFS CIDs for training datasets used
    performance_metrics = #{},     % Map of metric_name => value
    inference_api_endpoint,        % URL for model inference API (if deployed)
    docker_image_cid,              % IPFS CID for containerized model
    model_card_cid,                % IPFS CID for model documentation/card
    version_history = [],          % List of {version, cid, timestamp, improvements}
    benchmark_results = [],        % List of {dataset_id, metric, score}
    dependencies_cid,              % IPFS CID for requirements/dependencies
    inference_time_ms = 0,         % Average inference time in milliseconds
    carbon_footprint = 0.0         % Estimated CO2 emissions during training (kg)
}).

-record(code_snippet, {
    id,
    creator_id,
    title,
    description,
    language,
    code_content,
    tags = [],
    visibility = public,
    likes = [],
    comments = [],
    date_created,
    date_updated,
    report = [],
    data = #{},
    execution_count = 0,           % Number of times executed
    forked_from,                   % Parent snippet ID if forked
    fork_count = 0,                % Number of forks
    input_schema = #{},            % Expected input format/types
    output_schema = #{},           % Expected output format/types
    execution_environment,         % Required runtime environment
    performance_metrics = #{},     % {execution_time, memory_usage, etc.}
    usage_examples_cid,            % IPFS CID for usage examples
    test_cases_cid,                % IPFS CID for unit tests
    dependencies = []              % List of required packages/libraries
}).

-record(learning_resource, {
    id,
    creator_id,
    title,
    description,
    type,                          % tutorial | course | exercise | workshop
    content_cid,
    level,
    tags = [],
    duration,                      % Duration in seconds
    visibility = public,
    completions = 0,
    likes = [],
    comments = [],
    date_created,
    date_updated,
    report = [],
    data = #{},
    prerequisites = [],            % List of learning_resource IDs required first
    learning_path_id,              % ID of curriculum/learning path this belongs to
    exercises_cid,                 % IPFS CID for practice exercises
    solutions_cid,                 % IPFS CID for exercise solutions
    quiz_cid,                      % IPFS CID for assessment quiz
    video_segments = [],           % List of {start_time, end_time, topic}
    interactive_demos = [],        % List of {demo_type, demo_cid, description}
    certificate_template_cid,      % IPFS CID for completion certificate template
    discussion_cid,                % IPFS CID for Q&A/discussion thread
    related_competition_ids = []
}).

-record(data_science_progress, {
    user_id,
    level,
    points = 0,
    badge_ids = [],
    completed_resource_ids = [],
    competition_wins = 0,
    dataset_contributions = 0,
    notebook_contributions = 0,
    model_contributions = 0,
    last_updated,
    data = #{},
    rank_global,                   % Global ranking position
    rank_by_country,               % Country-specific ranking
    specializations = [],          % List of areas (nlp, cv, etc.)
    contribution_streak = 0,       % Days of consecutive activity
    mentor_score = 0.0,            % Score based on helping others (0-100)
    code_review_count = 0,         % Number of code reviews performed
    discussion_contributions = 0,  % Number of helpful discussion posts
    achievements_timeline = [],    % List of {timestamp, achievement_type, details}
    follower_ids = [],             % List of user_ids following this user
    following_ids = []             % List of user_ids this user follows
}).

-record(team, {
    id,
    competition_id,
    name,
    creator_id,
    members = [],                  % List of {user_id, role, joined_at}
    invitations = [],              % List of {user_id, status, invited_at, expires_at}
    merge_requests = [],           % List of {from_team_id, status, requested_at}
    submission_ids = [],
    team_score,
    rank,
    disbanded = false,
    date_created,
    date_updated,

    discussion_cids = [],          % Team-specific discussions
    notebook_ids = [],             % Shared team notebooks
    compute_quota_used = #{},      % Track team's compute usage
    total_submissions = 0,         % Total submission count
    best_submission_id,            % ID of best scoring submission
    team_avatar_cid,               % Team avatar/logo on IPFS
    metadata = #{}                 % Additional team metadata
}).

-record(discussion_thread, {
    id,
    parent_type,                   % competition | dataset | notebook | general | team | model
    parent_id,
    creator_id,
    title,
    content_cid,                   % IPFS CID for thread content
    replies = [],                  % [{reply_id, user_id, content_cid, timestamp}]
    upvotes = [],                  % List of user_ids who upvoted
    downvotes = [],                % List of user_ids who downvoted
    pinned = false,
    solved = false,
    tags = [],
    date_created,
    date_updated,

    visibility = public,           % public | private | team_only
    reply_count = 0,               % Denormalized count for performance
    view_count = 0,                % Track engagement
    last_activity,                 % Timestamp of last reply/edit
    accepted_answer_id,            % For Q&A threads
    locked = false,                % Prevent new replies
    report = [],                   % Moderation reports
    mentioned_user_ids = [],       % Users mentioned with @
    metadata = #{}                 % Additional metadata
}).

-record(compute_session, {
    id,
    user_id,
    notebook_id,
    resource_type,                 % cpu | gpu | tpu
    start_time,
    end_time,
    duration_seconds,
    cost_tokens,
    status,                        % queued | running | completed | failed | terminated
    logs_cid,                      % IPFS CID for session logs
    team_id,                       % If part of team competition
    competition_id,                % Link to competition if applicable
    memory_used_mb,                % Track memory usage
    storage_used_mb,               % Track storage usage
    error_cid,                     % IPFS CID for error logs if failed
    checkpoint_cids = [],          % Saved checkpoints during execution
    cpu_cores_used,                % Number of CPU cores
    gpu_model,                     % GPU model (e.g., "Tesla V100")
    date_created,                  % Creation timestamp
    metadata = #{}                 % Additional session metadata
}).

-record(submission, {
    id,
    competition_id,
    team_id,                       % Team ID if team competition
    user_id,                       % User ID if solo OR the submitter from team
    submission_cid,                % IPFS CID of the submission file
    notebook_id,                   % notebook that generated this
    submission_number,             % nth submission for this team/user
    score_public,                  % Score on public leaderboard
    score_private,                 % Score on private leaderboard (if available)
    evaluation_status,             % pending | queued | evaluating | completed | failed
    evaluation_cid,                % IPFS CID for evaluation results/logs
    error_message,                 % Error message if evaluation failed
    submission_time,               % Timestamp of submission
    compute_session_id,            % Link to compute session used
    late_submission = false,       % Whether submitted after deadline
    disqualified = false,          % If violates rules
    disqualification_reason,       % Reason if disqualified
    file_size_bytes,               % Submission file size
    evaluation_time_seconds,       % Time taken to evaluate
    metadata = #{}
}).

-record(badge, {
    id,
    name,                          % e.g., "Top 10% Finisher"
    description,
    icon_cid,                      % IPFS CID for badge image/icon
    criteria = #{},                % Requirements to earn badge
    rarity,                        % common | rare | epic | legendary
    nft_contract_address,          % If badge is an NFT on blockchain
    nft_token_standard,            % ERC-721 | ERC-1155 | etc.
    total_awarded = 0,             % Number of users who have earned it
    category,                      % competition | contribution | learning | social
    points_value = 0,              % Points awarded when earned
    date_created,
    metadata = #{}
}).

-record(user_badge, {
    id,
    user_id,
    badge_id,
    earned_at,                     % Timestamp when earned
    competition_id,                % Related competition
    context = #{},                 % Additional context (rank, score, etc.)
    nft_token_id,                  % If minted as NFT
    transaction_hash,              % Blockchain transaction hash if NFT
    metadata = #{}
}).

-record(notification, {
    id,
    user_id,
    type,                          % competition_invite | submission_scored | badge_earned | etc.
    title,
    message,
    link_type,                     % competition | notebook | discussion | team | etc.
    link_id,                       % ID of the linked entity
    read = false,
    action_required = false,       % Whether user needs to take action
    priority,                      % low | normal | high | urgent
    expires_at,                    % Expiration timestamp for time-sensitive notifications
    date_created,
    metadata = #{}
}).

-record(learning_path, {
    id,
    title,
    description,
    creator_id,
    resource_ids = [],             % Ordered list of learning_resource IDs
    difficulty_level,              % beginner | intermediate | advanced
    estimated_duration,            % Total duration in seconds
    certificate_template_cid,      % IPFS CID for completion certificate
    completions = 0,               % Number of users who completed
    tags = [],
    visibility = public,
    date_created,
    date_updated,
    metadata = #{}
}).

-record(code_review, {
    id,
    notebook_id,                   % Notebook being reviewed
    reviewer_id,                   % User conducting the review
    author_id,                     % Original notebook author
    review_type,                   % informal | competition | mentor
    comments = [],                 % List of {line_number, comment_cid, timestamp}
    rating,                        % 1-5 stars
    status,                        % draft | submitted | resolved
    helpful_count = 0,             % How many found this review helpful
    date_created,
    date_updated,
    metadata = #{}
}).

-record(collaboration_request, {
    id,
    resource_type,                 % dataset | notebook | model | competition
    resource_id,
    requester_id,
    owner_id,
    request_type,                  % edit | view | download | fork
    reason,                        % Why they want access
    status,                        % pending | approved | rejected | expired
    requested_at,
    responded_at,
    expires_at,
    metadata = #{}
}).
