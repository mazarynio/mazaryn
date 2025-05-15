%% Record for managing data science competitions
-record(competition, {
    id,                        % Unique competition ID
    title,                     % Competition title (e.g., "Climate Data Quest")
    description,               % Detailed problem description
    creator_id,                % User ID of the organizer (linked to user record)
    business_id = [],          % Optional sponsor ID
    dataset_id = [],           % List of dataset IDs for the competition
    dataset_cids = [],         % List of IPFS CIDs for associated datasets
    dataset_ipns = [],
    start_time,                % Competition start timestamp
    end_time,                  % Competition end timestamp
    reward_type,               % Reward type (e.g., token, NFT, reputation)
    reward_value,              % Reward details (e.g., token amount, NFT CID)
    rules = #{},               % Rules (e.g., submission limits, team size)
    evaluation_metric,         % Metric for ranking (e.g., RMSE, F1-score)
    submission_count_limit,    % Max submissions per user/team
    team_size_limit,           % Max team size
    submission_cids = [],      % List of IPFS CIDs for user submissions
    participants = [],         % List of user IDs participating
    status,                     % active | ended | draft | voting
    visibility = public,       % public | private
    tags = [],                 % Tags for discoverability (e.g., #machinelearning)
    date_created,              % Creation timestamp
    date_updated,              % Last update timestamp
    report = [],               % Linked report records for moderation
    metadata = #{}                 % Additional metadata (e.g., smart contract address)
}).

-record(dataset, {
    id,                        % Unique dataset ID
    title,                     % Dataset title (e.g., "Urban IoT Sensor Data")
    description,               % Dataset description
    creator_id,                % User ID of the uploader
    content_cid,               % IPFS CID for dataset file(s)
    ipns,                      % IPNS of Dataset
    metadata_cid,              % IPFS CID for metadata (e.g., schema, format)
    size_bytes,                % Total size in bytes
    license,                   % License type (e.g., CC0, MIT)
    version,                   % Dataset version number
    tags = [],                 % Tags for search (e.g., #iot, #healthcare)
    visibility = public,       % public | private
    downloads = 0,             % Download count
    ratings = [],              % List of {user_id, rating} for quality
    pin_info = [],             % Linked pin_info for IPFS storage
    competitions = [],         % List of competition IDs using this dataset
    date_created,              % Creation timestamp
    date_updated,              % Last update timestamp
    report = [],               % Linked report records for moderation
    metadata = #{}             % Metadata (e.g., file format, column descriptions)
}).

%% Record for cloud-based notebooks
-record(notebook, {
    id,                        % Unique notebook ID
    title,                     % Notebook title (e.g., "Climate Model Analysis")
    creator_id,                % User ID of the creator
    description,               % Notebook description
    content_cid,               % IPFS CID for notebook content (JSON/IPYNB)
    dataset_cids = [],         % List of IPFS CIDs for associated datasets
    competition_id = [],       % Optional competition ID for submissions
    language,                  % python | r | julia
    kernel_type,               % CPU | GPU | TPU
    environment = #{},         % Custom environment settings (e.g., libraries)
    collaborators = [],        % List of user_ids for real-time editing
    version_cids = [],         % List of IPFS CIDs for version history
    outputs = [],              % List of IPFS CIDs for outputs (e.g., plots)
    visibility = public,       % public | private
    execution_time_limit,      % Max runtime in seconds
    execution_count = 0,       % Number of executions
    likes = [],                % Linked like records
    comments = [],             % Linked comment records
    date_created,              % Creation timestamp
    date_updated,              % Last update timestamp
    report = [],               % Linked report records
    data = #{}                 % Metadata (e.g., dependencies, runtime stats)
}).

%% Record for leaderboard
-record(leaderboard, {
    competition_id,            % ID of the associated competition
    entries = [],              % List of {user_id, submission_cid, score, rank}
    evaluation_metric,         % Metric used (e.g., accuracy, F1-score)
    last_updated,              % Timestamp of last update
    data = #{}                 % Additional metadata (e.g., scoring script CID)
}).

%% Record for pre-trained models
-record(model, {
    id,                        % Unique model ID
    creator_id,                % User ID of the uploader
    title,                     % Model title
    description,               % Model description
    framework,                 % e.g., TensorFlow, PyTorch
    task_type,                 % e.g., classification, regression
    file_cid,                  % IPFS CID for model file
    size_bytes,                % Model size in bytes
    license,                   % License type (e.g., CC0, MIT)
    tags = [],                 % Tags for search (e.g., #NLP, #vision)
    visibility = public,       % public | private
    downloads = 0,             % Download count
    pin_info = [],             % Linked pin_info for IPFS storage
    deployment_info = #{},     % Metadata for deployment (e.g., API endpoint)
    date_created,              % Creation timestamp
    date_updated,              % Last update timestamp
    report = [],               % Linked report records
    data = #{}                 % Metadata (e.g., accuracy, training data)
}).

%% Record for code snippets
-record(code_snippet, {
    id,                        % Unique snippet ID
    creator_id,                % User ID of the owner
    title,                     % Snippet title
    description,               % Snippet description
    language,                  % python | r | sql
    code_content,              % Code text or IPFS CID
    tags = [],                 % Tags for search (e.g., #pandas, #ML)
    visibility = public,       % public | private
    likes = [],                % Linked like records
    comments = [],             % Linked comment records
    date_created,              % Creation timestamp
    date_updated,              % Last update timestamp
    report = [],               % Linked report records
    data = #{}                 % Metadata (e.g., use case, dependencies)
}).

%% Record for learning resources (tutorials/courses)
-record(learning_resource, {
    id,                        % Unique resource ID
    creator_id,                % User ID of the creator
    title,                     % Resource title
    description,               % Resource description
    type,                      % tutorial | course | exercise
    content_cid,               % IPFS CID for content (e.g., markdown, video)
    level,                     % beginner | intermediate | advanced
    tags = [],                 % Tags for search (e.g., #python, #ML)
    duration,                  % Estimated completion time in seconds
    visibility = public,       % public | private
    completions = 0,           % Number of users who completed
    likes = [],                % Linked like records
    comments = [],             % Linked comment records
    date_created,              % Creation timestamp
    date_updated,              % Last update timestamp
    report = [],               % Linked report records
    data = #{}                 % Metadata (e.g., prerequisites, learning outcomes)
}).

%% Record for user progression in data science activities
-record(data_science_progress, {
    user_id,                   % Linked user ID
    level,                     % 1-20, based on activity
    points = 0,                % Points from competitions, notebooks, etc.
    badges = [],               % Earned badges (e.g., "Top 10% in Competition")
    completed_resources = [],  % List of completed learning_resource IDs
    competition_wins = 0,      % Number of competition wins
    dataset_contributions = 0, % Number of datasets uploaded
    notebook_contributions = 0,% Number of notebooks shared
    model_contributions = 0,   % Number of models shared
    last_updated,              % Timestamp of last update
    data = #{}                 % Additional metadata (e.g., ranking stats)
}).