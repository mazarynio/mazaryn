-record(solana_wallet, {
    wallet_id,
    user_id,
    label,
    public_key,
    encrypted_private_key,
    encryption_iv,
    encryption_auth_tag,
    encryption_salt,
    derivation_path,
    created_at,
    last_used,
    is_primary = false,
    metadata = #{}
}).

-record(solana_transaction, {
    tx_id,
    wallet_id,
    user_id,
    signature,
    tx_type,
    from_address,
    to_address,
    amount_lamports,
    token_mint,
    nft_mint,
    status,
    fee_lamports,
    slot,
    block_time,
    memo,
    error_message,
    created_at,
    confirmed_at,
    metadata = #{}
}).

-record(solana_airdrop, {
    airdrop_id,
    wallet_id,
    user_id,
    type,
    token_mint,
    nft_collection,
    total_recipients,
    successful = 0,
    failed = 0,
    total_amount_lamports,
    status,
    created_at,
    started_at,
    completed_at,
    metadata = #{}
}).

-record(solana_airdrop_recipient, {
    id,
    airdrop_id,
    recipient_address,
    amount_lamports,
    mint_address,
    success = false,
    signature,
    error_message,
    processed_at,
    metadata = #{}
}).

-record(solana_stake_account, {
    stake_id,
    wallet_id,
    user_id,
    stake_account_address,
    validator_vote_address,
    amount_lamports,
    status,
    signature,
    created_at,
    delegated_at,
    deactivated_at,
    withdrawn_at,
    rewards_earned_lamports = 0,
    metadata = #{}
}).

-record(solana_token_account, {
    id,
    wallet_id,
    user_id,
    token_account_address,
    token_mint,
    balance,
    decimals,
    owner_address,
    created_at,
    last_synced,
    metadata = #{}
}).

-record(solana_nft, {
    id,
    wallet_id,
    user_id,
    mint_address,
    token_account_address,
    name,
    symbol,
    uri,
    collection_address,
    verified = false,
    creators = [],
    attributes = [],
    image_url,
    created_at,
    last_synced,
    metadata = #{}
}).

-record(near_wallet, {
    wallet_id,
    user_id,
    label,
    account_id,
    encrypted_private_key,
    encryption_iv,
    encryption_tag,
    network,
    created_at,
    last_used,
    is_primary = false,
    metadata = #{}
}).

-record(near_transaction, {
    tx_id,
    wallet_id,
    user_id,
    transaction_hash,
    tx_type,
    from_account_id,
    receiver_id,
    amount_near,
    contract_id,
    method_name,
    status,
    actions_count,
    error_message,
    created_at,
    metadata = #{}
}).

-record(near_access_key, {
    key_id,
    wallet_id,
    user_id,
    public_key,
    key_type,
    contract_id,
    method_names,
    allowance_near,
    label,
    created_at,
    metadata = #{}
}).

-record(near_stake, {
    stake_id,
    wallet_id,
    user_id,
    validator_account_id,
    validator_public_key,
    amount_near,
    status,
    transaction_hash,
    created_at,
    unstaked_at,
    withdrawn_at,
    metadata = #{}
}).

-record(near_implicit_account, {
    implicit_id,
    wallet_id,
    user_id,
    account_id,
    encrypted_private_key,
    encryption_iv,
    encryption_tag,
    funded = false,
    created_at,
    funded_at,
    metadata = #{}
}).

-record(near_social_post, {
    post_id,
    wallet_id,
    user_id,
    account_id,
    contract,
    text,
    media_urls = [],
    tags = [],
    transaction_hash,
    block_height,
    created_at,
    metadata = #{}
}).

-record(aptos_wallet, {
    wallet_id,
    user_id,
    label,
    account_address,
    encrypted_private_key,
    encryption_iv,
    encryption_tag,
    network,
    signing_scheme,
    is_legacy = false,
    created_at,
    last_used,
    is_primary = false,
    metadata = #{}
}).

-record(aptos_transaction, {
    tx_id,
    wallet_id,
    user_id,
    transaction_hash,
    tx_type,
    sender_address,
    receiver_address,
    amount_octas,
    gas_used,
    gas_unit_price,
    sequence_number,
    function_called,
    coin_type,
    metadata_address,
    pool_address,
    collection_name,
    token_address,
    status,
    vm_status,
    version,
    error_message,
    created_at,
    metadata = #{}
}).

-record(aptos_nft_collection, {
    collection_id,
    wallet_id,
    user_id,
    creator_address,
    collection_name,
    description,
    uri,
    max_supply,
    current_supply,
    transaction_hash,
    on_chain_id,
    created_at,
    metadata = #{}
}).

-record(aptos_nft_token, {
    token_id,
    wallet_id,
    user_id,
    collection_id,
    collection_name,
    token_name,
    description,
    uri,
    property_keys   = [],
    property_types  = [],
    property_values = [],
    transaction_hash,
    on_chain_id,
    created_at,
    metadata = #{}
}).

-record(aptos_fungible_asset, {
    fa_id,
    wallet_id,
    user_id,
    creator_address,
    asset_name,
    symbol,
    decimals,
    metadata_address,
    transaction_hash,
    created_at,
    metadata = #{}
}).

-record(aptos_stake, {
    stake_id,
    wallet_id,
    user_id,
    delegator_address,
    pool_address,
    amount_octas,
    active_stake,
    inactive_stake,
    pending_inactive_stake,
    tx_type,
    transaction_hash,
    gas_used,
    gas_unit_price,
    vm_status,
    version,
    network,
    status,
    created_at,
    metadata = #{}
}).

-record(aptos_coin_balance, {
    balance_id,
    wallet_id,
    user_id,
    account_address,
    coin_type,
    balance,
    network,
    last_synced,
    metadata = #{}
}).

-record(aptos_fa_balance, {
    fa_balance_id,
    wallet_id,
    user_id,
    account_address,
    asset_type,
    metadata_address,
    amount,
    is_primary,
    network,
    last_synced,
    metadata = #{}
}).

-record(sui_wallet, {
    wallet_id,
    user_id,
    label,
    sui_address,
    public_key,
    encrypted_private_key,
    encryption_iv,
    encryption_tag,
    scheme = ed25519,
    network = mainnet,
    is_primary = false,
    created_at,
    last_used,
    metadata = #{}
}).

-record(sui_transaction, {
    tx_id,
    wallet_id,
    user_id,
    digest,
    tx_type,
    sender_address,
    recipient_address,
    amount_mist,
    coin_type,
    object_id,
    package_id,
    module_name,
    function_name,
    validator_address,
    staked_sui_id,
    gas_computation = <<"0">>,
    gas_storage = <<"0">>,
    gas_storage_rebate = <<"0">>,
    status = pending,
    effects_status = <<"">>,
    checkpoint,
    epoch,
    error_message,
    network = mainnet,
    created_at,
    metadata = #{}
}).

-record(sui_stake, {
    stake_id,
    wallet_id,
    user_id,
    owner_address,
    validator_address,
    staked_sui_id,
    principal_mist,
    estimated_reward_mist = <<"0">>,
    stake_activation_epoch,
    status = pending,
    tx_type,
    digest,
    gas_computation = <<"0">>,
    gas_storage = <<"0">>,
    gas_storage_rebate = <<"0">>,
    network = mainnet,
    created_at,
    metadata = #{}
}).

-record(sui_object, {
    object_id,
    wallet_id,
    user_id,
    owner_address,
    object_type,
    version,
    digest,
    balance,
    previous_transaction,
    storage_rebate,
    is_nft = false,
    nft_name,
    nft_description,
    nft_url,
    network = mainnet,
    synced_at,
    metadata = #{}
}).

-record(sui_coin_balance, {
    balance_id,
    wallet_id,
    user_id,
    owner_address,
    coin_type,
    total_balance = <<"0">>,
    coin_object_count = 0,
    network = mainnet,
    synced_at,
    metadata = #{}
}).
