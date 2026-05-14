export interface CreateAptosAccountRequest {
  user_id: string;
  label?: string;
}

export interface CreateAptosAccountResponse {
  account_address: string;
  wallet_id: string;
  user_id: string;
  public_key: string;
  signing_scheme: string;
  network: string;
  created_at: number;
}

export interface ImportAptosAccountRequest {
  user_id: string;
  private_key: string;
  label?: string;
}

export interface DeleteAptosAccountRequest {
  user_id: string;
  wallet_id: string;
}

export interface AptosAccountInfo {
  wallet_id: string;
  account_address: string;
  public_key: string;
  user_id: string;
  label?: string;
  signing_scheme: string;
  network: string;
  created_at: number;
  last_used?: number;
  is_primary: boolean;
}

export interface AptosBalanceResponse {
  account_address: string;
  balance_octas: number;
  balance_apt: string;
}

export interface AptosFABalanceResponse {
  account_address: string;
  metadata_address: string;
  balance: number;
}

export interface AptosTransferRequest {
  sender_address: string;
  recipient_address: string;
  amount_octas: number;
  network?: string;
  options?: {
    gas_unit_price?: number;
    max_gas_amount?: number;
  };
}

export interface AptosTransferResponse {
  transaction_hash: string;
  sender_address: string;
  recipient_address: string;
  amount_octas: number;
  gas_used: number;
  gas_unit_price: number;
  version: string;
  vm_status: string;
  success: boolean;
  timestamp: number;
}

export interface AptosTransferFARequest {
  sender_address: string;
  recipient_address: string;
  metadata_address: string;
  amount: number;
  network?: string;
}

export interface AptosCreateCollectionRequest {
  account_address: string;
  name: string;
  description: string;
  uri: string;
  max_supply?: number;
  mutable_description?: boolean;
  mutable_uri?: boolean;
  mutable_token_description?: boolean;
  mutable_token_name?: boolean;
  mutable_token_properties?: boolean;
  mutable_token_uri?: boolean;
  tokens_burnable_by_creator?: boolean;
  tokens_freezable_by_creator?: boolean;
  royalty_numerator?: number;
  royalty_denominator?: number;
}

export interface AptosCreateCollectionResponse {
  transaction_hash: string;
  collection_name: string;
  creator_address: string;
  version: string;
  success: boolean;
  timestamp: number;
}

export interface AptosMintNFTRequest {
  account_address: string;
  collection: string;
  name: string;
  description: string;
  uri: string;
  property_keys?: string[];
  property_types?: string[];
  property_values?: string[];
}

export interface AptosMintNFTResponse {
  transaction_hash: string;
  token_address: string;
  collection_name: string;
  token_name: string;
  creator_address: string;
  version: string;
  success: boolean;
  timestamp: number;
}

export interface AptosTransferNFTRequest {
  account_address: string;
  token_address: string;
  recipient_address: string;
  network?: string;
}

export interface AptosBurnNFTRequest {
  account_address: string;
  token_address: string;
  network?: string;
}

export interface AptosTransactionResponse {
  transaction_hash: string;
  version: string;
  sender: string;
  sequence_number: number;
  gas_used: number;
  gas_unit_price: number;
  success: boolean;
  vm_status: string;
  timestamp: number;
  function_called?: string;
}

export interface AptosLedgerInfo {
  chain_id: number;
  epoch: string;
  ledger_version: string;
  oldest_ledger_version: string;
  ledger_timestamp: string;
  node_role: string;
  oldest_block_height: string;
  block_height: string;
}

export interface AptosGasEstimateResponse {
  gas_unit_price: number;
  prioritized_gas_unit_price: number;
  deprioritized_gas_unit_price: number;
}

export interface AptosWalletRecord {
  wallet_id: string;
  user_id: string;
  account_address: string;
  public_key: string;
  encrypted_private_key: string;
  encryption_iv: string;
  encryption_tag: string;
  label?: string;
  signing_scheme: string;
  network: string;
  created_at: number;
  last_used?: number;
  is_primary: boolean;
}

export interface AptosErrorResponse {
  error: string;
  details?: unknown;
}

export interface AptosGetCoinBalanceRequest {
  account_address: string;
  coin_type: string;
  network?: string;
}

export interface AptosGetCoinBalanceResponse {
  account_address: string;
  coin_type: string;
  balance: number;
}

export interface AptosGetFAMetadataRequest {
  asset_type: string;
  network?: string;
}

export interface AptosGetFAMetadataResponse {
  asset_type: string;
  name: string;
  symbol: string;
  decimals: number;
  icon_uri?: string;
  project_uri?: string;
  creator_address: string;
}

export interface AptosGetFABalancesRequest {
  account_address: string;
  network?: string;
}

export interface AptosGetFABalancesResponse {
  account_address: string;
  balances: Array<{
    asset_type: string;
    amount: number;
    is_primary: boolean;
  }>;
}

export interface AptosTransferCoinRequest {
  sender_address: string;
  recipient_address: string;
  amount: number;
  coin_type: string;
  network?: string;
  options?: {
    gas_unit_price?: number;
    max_gas_amount?: number;
  };
}

export interface AptosTransferFungibleAssetRequest {
  sender_address: string;
  recipient_address: string;
  metadata_address: string;
  amount: number;
  network?: string;
}

export interface AptosMintCoinRequest {
  admin_address: string;
  recipient_address: string;
  amount: number;
  coin_module_address: string;
  coin_module_name: string;
  network?: string;
}

export interface AptosViewFunctionRequest {
  function: string;
  type_arguments?: string[];
  function_arguments?: unknown[];
  network?: string;
}

export interface AptosViewFunctionResponse {
  result: unknown[];
}

export interface AptosGetAccountCoinsRequest {
  account_address: string;
  network?: string;
}

export interface AptosGetAccountCoinsResponse {
  account_address: string;
  total: number;
  coins: Array<{
    asset_type: string;
    amount: number;
    name?: string;
    symbol?: string;
    decimals?: number;
  }>;
}

export interface AptosStakeRequest {
  delegator_address: string;
  pool_address: string;
  amount_octas: number;
  network?: string;
}

export interface AptosStakeResponse {
  transaction_hash: string;
  delegator_address: string;
  pool_address: string;
  amount_octas: number;
  gas_used: number;
  gas_unit_price: number;
  version: string;
  vm_status: string;
  success: boolean;
  timestamp: number;
}

export interface AptosUnstakeRequest {
  delegator_address: string;
  pool_address: string;
  amount_octas: number;
  network?: string;
}

export interface AptosUnstakeResponse {
  transaction_hash: string;
  delegator_address: string;
  pool_address: string;
  amount_octas: number;
  gas_used: number;
  gas_unit_price: number;
  version: string;
  vm_status: string;
  success: boolean;
  timestamp: number;
}

export interface AptosWithdrawStakeRequest {
  delegator_address: string;
  pool_address: string;
  network?: string;
}

export interface AptosWithdrawStakeResponse {
  transaction_hash: string;
  delegator_address: string;
  pool_address: string;
  gas_used: number;
  gas_unit_price: number;
  version: string;
  vm_status: string;
  success: boolean;
  timestamp: number;
}

export interface AptosGetStakeInfoRequest {
  delegator_address: string;
  pool_address: string;
  network?: string;
}

export interface AptosGetStakeInfoResponse {
  delegator_address: string;
  pool_address: string;
  active_stake: string;
  inactive_stake: string;
  pending_inactive_stake: string;
}

export interface AptosGetDelegationPoolsRequest {
  network?: string;
}

export interface AptosDelegationPoolInfo {
  pool_address: string;
  operator_address: string;
  voter_address: string;
  total_staked: string;
  commission_percentage: number;
}

export interface AptosGetDelegationPoolsResponse {
  pools: AptosDelegationPoolInfo[];
  total: number;
}

export interface AptosReactivateStakeRequest {
  delegator_address: string;
  pool_address: string;
  amount_octas: number;
  network?: string;
}

export interface AptosReactivateStakeResponse {
  transaction_hash: string;
  delegator_address: string;
  pool_address: string;
  amount_octas: number;
  gas_used: number;
  gas_unit_price: number;
  version: string;
  vm_status: string;
  success: boolean;
  timestamp: number;
}
