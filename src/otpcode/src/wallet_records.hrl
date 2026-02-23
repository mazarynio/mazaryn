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
