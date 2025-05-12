-record(competition, {
    id,                    % Unique competition ID
    title,                 % Competition name (e.g., "Climate Data Quest")
    description,           % Detailed explanation of the challenge
    creator_id,            % User ID of the creator
    dataset_cids = [],     % List of IPFS CIDs for associated datasets
    start_time,            % Competition start timestamp
    end_time,              % Competition end timestamp
    reward_type,           % Type of reward (e.g., token, NFT, reputation)
    reward_value,          % Reward details (e.g., token amount, NFT metadata)
    submission_cids = [],  % List of IPFS CIDs for user submissions
    leaderboard = [],      % List of {user_id, score} tuples
    status,                % Status (e.g., active, closed, voting)
    tags = [],             % Tags for discoverability (e.g., #machinelearning, #climate)
    rules,                 % JSON or text describing rules
    participants = [],     % List of user IDs participating
    votes = [],            % List of {user_id, vote} for community-driven competitions
    date_created,          % Creation timestamp
    date_updated,          % Last update timestamp
    metadata = #{}         % Additional data (e.g., smart contract address)
}).

-record(dataset, {
    id,                    % Unique dataset ID
    title,                 % Dataset name (e.g., "Urban IoT Sensor Data")
    description,           % Dataset description
    creator_id,            % User ID of the uploader
    content_cid,           % IPFS CID of the dataset file(s)
    metadata_cid,          % IPFS CID of metadata (e.g., schema, format)
    size_bytes,            % Total size of the dataset
    tags = [],             % Tags for search (e.g., #iot, #urban)
    license,               % License type (e.g., CC-BY, MIT)
    competitions = [],     % List of competition IDs using this dataset
    downloads = 0,         % Number of downloads
    ratings = [],          % List of {user_id, rating} for quality
    date_created,          % Creation timestamp
    date_updated,          % Last update timestamp
    status,                % Status (e.g., public, private, pending review)
    metadata = #{}         % Additional data (e.g., file format, version)
}).

-record(notebook, {
    id,                    % Unique notebook ID
    title,                 % Notebook name (e.g., "Climate Model Analysis")
    creator_id,            % User ID of the creator
    content_cid,           % IPFS CID of the notebook file (e.g., .ipynb)
    dataset_cids = [],     % List of IPFS CIDs for associated datasets
    competition_id,        % Optional competition ID if part of a submission
    collaborators = [],    % List of user IDs for co-editing
    version_cids = [],     % List of CIDs for version history
    visibility,            % Visibility (e.g., public, private, shared)
    likes = [],            % List of user IDs who liked the notebook
    comments = [],         % List of comment IDs (linked to post record)
    date_created,          % Creation timestamp
    date_updated,          % Last update timestamp
    status,                % Status (e.g., active, archived)
    metadata = #{}         % Additional data (e.g., runtime environment)
}).

-record(leaderboard, {
    competition_id,        % ID of the associated competition
    entries = [],          % List of {user_id, submission_cid, score, rank}
    last_updated,          % Timestamp of last update
    evaluation_metric,     % Metric used (e.g., accuracy, F1 score)
    metadata = #{}         % Additional data (e.g., scoring script CID)
}).

-record(reward, {
    id,                    % Unique reward ID
    competition_id,        % ID of the associated competition
    recipient_id,          % User ID of the recipient
    type,                  % Reward type (e.g., NFT, token, reputation)
    value,                 % Reward details (e.g., NFT CID, token amount)
    status,                % Status (e.g., pending, claimed)
    date_issued,           % Issuance timestamp
    metadata = #{}         % Additional data (e.g., smart contract address)
}).