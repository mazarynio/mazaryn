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
