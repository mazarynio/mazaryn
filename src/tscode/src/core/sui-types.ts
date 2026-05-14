export type SuiNetwork = "mainnet" | "testnet" | "devnet" | "localnet";

export interface CreateSuiWalletRequest {
  user_id: string;
  label?: string;
  scheme?: "ed25519" | "secp256k1" | "secp256r1";
  network?: SuiNetwork;
}

export interface CreateSuiWalletResponse {
  wallet_id: string;
  sui_address: string;
  public_key: string;
  scheme: string;
  user_id: string;
  network: SuiNetwork;
  created_at: number;
}

export interface ImportSuiWalletRequest {
  user_id: string;
  private_key: string;
  label?: string;
  scheme?: "ed25519" | "secp256k1" | "secp256r1";
  network?: SuiNetwork;
}

export interface ImportSuiWalletFromMnemonicRequest {
  user_id: string;
  mnemonic: string;
  label?: string;
  scheme?: "ed25519" | "secp256k1" | "secp256r1";
  derivation_path?: string;
  network?: SuiNetwork;
}

export interface SuiWalletInfo {
  wallet_id: string;
  sui_address: string;
  public_key: string;
  scheme: string;
  user_id: string;
  label?: string;
  network: SuiNetwork;
  created_at: number;
  last_used?: number;
  is_primary: boolean;
}

export interface DeleteSuiWalletRequest {
  user_id: string;
  wallet_id: string;
}

export interface SuiBalanceResponse {
  sui_address: string;
  coin_type: string;
  total_balance: string;
  coin_object_count: number;
  locked_balance?: Record<string, string>;
}

export interface SuiAllBalancesResponse {
  sui_address: string;
  balances: Array<{
    coin_type: string;
    coin_object_count: number;
    total_balance: string;
    locked_balance?: Record<string, string>;
  }>;
}

export interface SuiCoinObject {
  coin_type: string;
  coin_object_id: string;
  version: string;
  digest: string;
  balance: string;
  previous_transaction: string;
}

export interface SuiTransferRequest {
  sender_address: string;
  recipient_address: string;
  amount_mist: bigint | string;
  coin_type?: string;
  network?: SuiNetwork;
  gas_budget?: string;
}

export interface SuiTransferResponse {
  digest: string;
  sender_address: string;
  recipient_address: string;
  amount_mist: string;
  coin_type: string;
  gas_used: SuiGasSummary;
  status: "success" | "failure";
  checkpoint?: string;
  timestamp?: number;
  effects_status: string;
}

export interface SuiBatchTransferEntry {
  to: string;
  amount: string;
}

export interface SuiBatchTransferRequest {
  sender_address: string;
  transfers: SuiBatchTransferEntry[];
  coin_type?: string;
  network?: SuiNetwork;
  gas_budget?: string;
}

export interface SuiBatchTransferResponse {
  digest: string;
  sender_address: string;
  transfers: SuiBatchTransferEntry[];
  gas_used: SuiGasSummary;
  status: "success" | "failure";
  checkpoint?: string;
}

export interface SuiGasSummary {
  computation_cost: string;
  storage_cost: string;
  storage_rebate: string;
  non_refundable_storage_fee: string;
}

export interface SuiObjectInfo {
  object_id: string;
  version: string;
  digest: string;
  type: string;
  owner: SuiObjectOwner;
  previous_transaction?: string;
  storage_rebate?: string;
  content?: Record<string, unknown>;
  balance?: string;
}

export type SuiObjectOwner =
  | { AddressOwner: string }
  | { ObjectOwner: string }
  | { Shared: { initial_shared_version: string } }
  | { Immutable: true };

export interface SuiGetObjectRequest {
  object_id: string;
  include_content?: boolean;
  include_bcs?: boolean;
  include_previous_transaction?: boolean;
  network?: SuiNetwork;
}

export interface SuiGetObjectsRequest {
  object_ids: string[];
  include_content?: boolean;
  network?: SuiNetwork;
}

export interface SuiListOwnedObjectsRequest {
  owner_address: string;
  object_type?: string;
  limit?: number;
  cursor?: string;
  network?: SuiNetwork;
}

export interface SuiListOwnedObjectsResponse {
  objects: SuiObjectInfo[];
  next_cursor?: string;
  has_next_page: boolean;
}

export interface SuiTransactionResult {
  digest: string;
  status: "success" | "failure";
  sender?: string;
  gas_used?: SuiGasSummary;
  effects?: SuiTransactionEffects;
  events?: SuiEvent[];
  timestamp?: number;
  checkpoint?: string;
  error?: string;
}

export interface SuiTransactionEffects {
  status: { success: boolean; error?: string };
  gas_used: SuiGasSummary;
  transaction_digest: string;
  created?: Array<{ reference: SuiObjectRef; owner: SuiObjectOwner }>;
  mutated?: Array<{ reference: SuiObjectRef; owner: SuiObjectOwner }>;
  deleted?: SuiObjectRef[];
  events_digest?: string;
  dependencies?: string[];
}

export interface SuiObjectRef {
  object_id: string;
  version: string;
  digest: string;
}

export interface SuiEvent {
  package_id: string;
  module: string;
  sender: string;
  event_type: string;
  parsed_json?: Record<string, unknown>;
  bcs?: Uint8Array;
  timestamp?: number;
}

export interface SuiGetTransactionRequest {
  digest: string;
  include_effects?: boolean;
  include_events?: boolean;
  include_bcs?: boolean;
  include_transaction?: boolean;
  network?: SuiNetwork;
}

export interface SuiExecuteTransactionRequest {
  transaction_bytes: Uint8Array | string;
  signatures: string[];
  include_effects?: boolean;
  include_events?: boolean;
  network?: SuiNetwork;
}

export interface SuiSimulateTransactionRequest {
  transaction_bytes: Uint8Array | string;
  include_effects?: boolean;
  include_balance_changes?: boolean;
  skip_checks?: boolean;
  network?: SuiNetwork;
}

export interface SuiSimulateTransactionResponse {
  status: "success" | "failure";
  effects?: SuiTransactionEffects;
  balance_changes?: Array<{
    address: string;
    coin_type: string;
    amount: string;
  }>;
  gas_used?: SuiGasSummary;
  error?: string;
}

export interface SuiWaitForTransactionRequest {
  digest: string;
  timeout_ms?: number;
  include_effects?: boolean;
  network?: SuiNetwork;
}

export interface SuiMoveCallRequest {
  sender_address: string;
  package_id: string;
  module_name: string;
  function_name: string;
  type_arguments?: string[];
  arguments?: unknown[];
  gas_budget?: string;
  network?: SuiNetwork;
}

export interface SuiMoveCallResponse {
  digest: string;
  status: "success" | "failure";
  gas_used: SuiGasSummary;
  effects?: SuiTransactionEffects;
  events?: SuiEvent[];
  error?: string;
}

export interface SuiPublishPackageRequest {
  sender_address: string;
  compiled_modules: string[];
  dependencies: string[];
  gas_budget?: string;
  network?: SuiNetwork;
}

export interface SuiPublishPackageResponse {
  digest: string;
  package_id: string;
  status: "success" | "failure";
  gas_used: SuiGasSummary;
  created_objects?: string[];
  error?: string;
}

export interface SuiUpgradePackageRequest {
  sender_address: string;
  package_id: string;
  compiled_modules: string[];
  dependencies: string[];
  upgrade_cap_id: string;
  gas_budget?: string;
  network?: SuiNetwork;
}

export interface SuiSplitCoinRequest {
  sender_address: string;
  coin_object_id: string;
  split_amounts: string[];
  gas_budget?: string;
  network?: SuiNetwork;
}

export interface SuiSplitCoinResponse {
  digest: string;
  new_coin_ids: string[];
  gas_used: SuiGasSummary;
  status: "success" | "failure";
}

export interface SuiMergeCoinsRequest {
  sender_address: string;
  primary_coin_id: string;
  coins_to_merge: string[];
  gas_budget?: string;
  network?: SuiNetwork;
}

export interface SuiMergeCoinsResponse {
  digest: string;
  gas_used: SuiGasSummary;
  status: "success" | "failure";
}

export interface SuiCoinMetadata {
  id: string;
  coin_type: string;
  name: string;
  symbol: string;
  description: string;
  icon_url?: string;
  decimals: number;
}

export interface SuiCoinTreasury {
  id: string;
  total_supply: string;
  supply_state: "FIXED" | "BURN_ONLY" | "SUPPLY_STATE_UNKNOWN";
}

export interface SuiGetCoinInfoRequest {
  coin_type: string;
  network?: SuiNetwork;
}

export interface SuiGetCoinInfoResponse {
  coin_type: string;
  metadata?: SuiCoinMetadata;
  treasury?: SuiCoinTreasury;
  is_regulated: boolean;
}

export interface SuiDynamicField {
  field_id: string;
  parent: string;
  name: { type: string; bcs: Uint8Array };
  value_type: string;
  value?: { type: string; bcs: Uint8Array };
  child_id?: string;
}

export interface SuiListDynamicFieldsRequest {
  parent_id: string;
  limit?: number;
  cursor?: string;
  network?: SuiNetwork;
}

export interface SuiListDynamicFieldsResponse {
  dynamic_fields: SuiDynamicField[];
  next_cursor?: string;
  has_next_page: boolean;
}

export interface SuiGetDynamicFieldRequest {
  parent_id: string;
  name_type: string;
  name_bcs: Uint8Array;
  network?: SuiNetwork;
}

export interface SuiCheckpointInfo {
  sequence_number: string;
  digest: string;
  epoch: string;
  timestamp: number;
  total_network_transactions: string;
  validator_signature?: string;
  previous_digest?: string;
  content_digest?: string;
  gas_cost_summary?: SuiGasSummary;
}

export interface SuiGetCheckpointRequest {
  sequence_number?: string;
  digest?: string;
  network?: SuiNetwork;
}

export interface SuiEpochInfo {
  epoch: string;
  reference_gas_price: string;
  start?: number;
  end?: number;
  first_checkpoint?: string;
  last_checkpoint?: string;
  protocol_version?: string;
  system_state?: SuiSystemStateSummary;
}

export interface SuiSystemStateSummary {
  epoch: string;
  protocol_version: string;
  reference_gas_price: string;
  epoch_start_timestamp_ms: string;
  safe_mode: boolean;
  total_stake: string;
  active_validators_count: number;
}

export interface SuiValidatorInfo {
  address: string;
  name: string;
  description?: string;
  image_url?: string;
  project_url?: string;
  staking_pool_id: string;
  sui_balance: string;
  pending_stake: string;
  pool_token_balance: string;
  voting_power: string;
  commission_rate: string;
  gas_price: string;
  next_epoch_stake?: string;
  next_epoch_commission_rate?: string;
  apy?: number;
}

export interface SuiGetValidatorsRequest {
  network?: SuiNetwork;
}

export interface SuiStakeRequest {
  sender_address: string;
  validator_address: string;
  amount_mist: string;
  network?: SuiNetwork;
  gas_budget?: string;
}

export interface SuiStakeResponse {
  digest: string;
  sender_address: string;
  validator_address: string;
  staked_sui_id: string;
  amount_mist: string;
  gas_used: SuiGasSummary;
  status: "success" | "failure";
  epoch: string;
  error?: string;
}

export interface SuiUnstakeRequest {
  sender_address: string;
  staked_sui_id: string;
  network?: SuiNetwork;
  gas_budget?: string;
}

export interface SuiUnstakeResponse {
  digest: string;
  sender_address: string;
  staked_sui_id: string;
  principal_amount: string;
  reward_amount?: string;
  gas_used: SuiGasSummary;
  status: "success" | "failure";
  error?: string;
}

export interface SuiGetStakedSuiRequest {
  owner_address: string;
  network?: SuiNetwork;
}

export interface SuiStakedSuiObject {
  staked_sui_id: string;
  validator_address: string;
  principal: string;
  stake_activation_epoch: string;
  estimated_reward?: string;
  status: "Active" | "Pending" | "Unstaked";
}

export interface SuiGetStakingRewardsRequest {
  owner_address: string;
  network?: SuiNetwork;
}

export interface SuiStakingRewardsSummary {
  owner_address: string;
  total_staked: string;
  total_rewards: string;
  stakes: SuiStakedSuiObject[];
}

export interface SuiNFTMintRequest {
  sender_address: string;
  name: string;
  description: string;
  url: string;
  package_id?: string;
  module_name?: string;
  function_name?: string;
  extra_args?: unknown[];
  network?: SuiNetwork;
  gas_budget?: string;
}

export interface SuiNFTMintResponse {
  digest: string;
  nft_object_id: string;
  name: string;
  description: string;
  url: string;
  owner_address: string;
  status: "success" | "failure";
  gas_used: SuiGasSummary;
  error?: string;
}

export interface SuiNFTTransferRequest {
  sender_address: string;
  nft_object_id: string;
  recipient_address: string;
  network?: SuiNetwork;
  gas_budget?: string;
}

export interface SuiGetNFTsRequest {
  owner_address: string;
  limit?: number;
  cursor?: string;
  network?: SuiNetwork;
}

export interface SuiNFTInfo {
  object_id: string;
  type: string;
  name?: string;
  description?: string;
  url?: string;
  owner_address: string;
  version: string;
  digest: string;
}

export interface SuiSignMessageRequest {
  wallet_id: string;
  message: string;
  network?: SuiNetwork;
}

export interface SuiSignMessageResponse {
  signature: string;
  public_key: string;
  wallet_address: string;
  message_hash: string;
}

export interface SuiVerifySignatureRequest {
  message: string;
  signature: string;
  address?: string;
  network?: SuiNetwork;
}

export interface SuiVerifySignatureResponse {
  is_valid: boolean;
  address?: string;
  reason?: string;
}

export interface SuiMultiSigCreateRequest {
  public_keys: Array<{
    public_key: string;
    scheme: "ed25519" | "secp256k1" | "secp256r1";
    weight: number;
  }>;
  threshold: number;
}

export interface SuiMultiSigCreateResponse {
  multisig_address: string;
  threshold: number;
  members: Array<{ public_key: string; scheme: string; weight: number }>;
}

export interface SuiMultiSigCombineRequest {
  partial_signatures: string[];
  transaction_bytes: string;
  public_keys: Array<{
    public_key: string;
    scheme: "ed25519" | "secp256k1" | "secp256r1";
    weight: number;
  }>;
  threshold: number;
  network?: SuiNetwork;
}

export interface SuiMultiSigCombineResponse {
  combined_signature: string;
  digest?: string;
  status?: "success" | "failure";
}

export interface SuiServiceInfo {
  chain: string;
  chain_id: string;
  checkpoint_height: string;
  epoch: string;
  lowest_available_checkpoint: string;
  server: string;
  timestamp: number;
}

export interface SuiGetMovePackageRequest {
  package_id: string;
  network?: SuiNetwork;
}

export interface SuiGetMoveFunctionRequest {
  package_id: string;
  module_name: string;
  function_name: string;
  network?: SuiNetwork;
}

export interface SuiMoveFunctionInfo {
  name: string;
  is_entry: boolean;
  visibility: string;
  parameters: unknown[];
  returns: unknown[];
  type_parameters: unknown[];
}

export interface SuiNameLookupRequest {
  name: string;
  network?: SuiNetwork;
}

export interface SuiNameLookupResponse {
  name: string;
  target_address?: string;
  expiration_timestamp?: number;
  id?: string;
}

export interface SuiReverseNameLookupRequest {
  address: string;
  network?: SuiNetwork;
}

export interface SuiReferenceGasPriceResponse {
  reference_gas_price: string;
  network: SuiNetwork;
}

export interface SuiBuildTransactionRequest {
  sender_address: string;
  transaction_kind:
    | "transfer"
    | "move_call"
    | "split_coin"
    | "merge_coins"
    | "publish"
    | "upgrade";
  params: Record<string, unknown>;
  gas_budget?: string;
  gas_price?: string;
  expiration_epoch?: string;
  network?: SuiNetwork;
}

export interface SuiBuildTransactionResponse {
  transaction_bytes: string;
  digest_to_sign: string;
  gas_estimate: string;
}

export interface SuiTransactionRecord {
  tx_id: string;
  wallet_id: string;
  user_id: string;
  digest: string;
  tx_type: SuiTxType;
  sender_address: string;
  recipient_address?: string;
  amount_mist?: string;
  coin_type?: string;
  object_id?: string;
  package_id?: string;
  module_name?: string;
  function_name?: string;
  validator_address?: string;
  staked_sui_id?: string;
  gas_computation: string;
  gas_storage: string;
  gas_storage_rebate: string;
  status: string;
  effects_status: string;
  checkpoint?: string;
  epoch?: string;
  error_message?: string;
  network: SuiNetwork;
  created_at: number;
}

export type SuiTxType =
  | "sui_transfer"
  | "coin_transfer"
  | "move_call"
  | "publish"
  | "upgrade"
  | "split_coin"
  | "merge_coins"
  | "stake"
  | "unstake"
  | "nft_mint"
  | "nft_transfer"
  | "batch_transfer"
  | "programmable_transaction"
  | "system";

export interface SuiLocalTransactionHistoryRequest {
  wallet_id: string;
  limit?: number;
  offset?: number;
  tx_type?: SuiTxType;
  network?: SuiNetwork;
}

export interface SuiLocalTransactionHistoryResponse {
  wallet_id: string;
  transactions: SuiTransactionRecord[];
  total: number;
  limit: number;
  offset: number;
}

export interface SuiSubscribeCheckpointsRequest {
  network?: SuiNetwork;
  include_transactions?: boolean;
  include_objects?: boolean;
}

export interface SuiEncryptedKeyData {
  encrypted_private_key: string;
  iv: string;
  auth_tag: string;
  scheme: string;
}

export interface SuiStoredWallet {
  wallet_id: string;
  user_id: string;
  label?: string;
  sui_address: string;
  public_key: string;
  scheme: string;
  network: SuiNetwork;
  is_primary: boolean;
  encrypted_data: SuiEncryptedKeyData;
  created_at: number;
  last_used?: number;
}
