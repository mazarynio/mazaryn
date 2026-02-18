export interface CreateWalletRequest {
  user_id: string;
  wallet_name?: string;
}

export interface CreateWalletResponse {
  wallet_id: string;
  public_key: string;
  user_id: string;
  created_at: number;
}

export interface ImportWalletRequest {
  user_id: string;
  private_key: string;
  wallet_name?: string;
}

export interface GetBalanceRequest {
  public_key: string;
}

export interface GetBalanceResponse {
  public_key: string;
  balance_lamports: number;
  balance_sol: number;
}

export interface TransferRequest {
  from_wallet_id: string;
  to_public_key: string;
  amount_lamports: number;
  memo?: string;
}

export interface TransferResponse {
  signature: string;
  from_public_key: string;
  to_public_key: string;
  amount_lamports: number;
  status: string;
  timestamp: number;
}

export interface GetTransactionRequest {
  signature: string;
}

export interface GetTransactionResponse {
  signature: string;
  slot: number;
  block_time: number | null;
  status: string;
  fee: number;
  meta: unknown;
}

export interface WalletInfo {
  wallet_id: string;
  public_key: string;
  user_id: string;
  wallet_name?: string;
  created_at: number;
}

export interface ErrorResponse {
  error: string;
  details?: unknown;
}

export interface GetTokenAccountsRequest {
  public_key: string;
}

export interface TokenAccount {
  address: string;
  mint: string;
  owner: string;
  amount: string;
  decimals: number;
  ui_amount: number;
}

export interface GetTokenAccountsResponse {
  public_key: string;
  token_accounts: TokenAccount[];
}

export interface GetTokenBalanceRequest {
  public_key: string;
  token_mint: string;
}

export interface GetTokenBalanceResponse {
  public_key: string;
  token_mint: string;
  balance: string;
  decimals: number;
  ui_amount: number;
}

export interface TransferTokenRequest {
  from_wallet_id: string;
  to_public_key: string;
  token_mint: string;
  amount: number;
  memo?: string;
}

export interface TransferTokenResponse {
  signature: string;
  from_public_key: string;
  to_public_key: string;
  token_mint: string;
  amount: number;
  status: string;
  timestamp: number;
}

export interface CreateTokenAccountRequest {
  wallet_id: string;
  token_mint: string;
}

export interface CreateTokenAccountResponse {
  token_account: string;
  owner: string;
  mint: string;
  signature: string;
}

export interface NFTMetadata {
  name: string;
  symbol: string;
  uri: string;
  seller_fee_basis_points: number;
  creators: NFTCreator[];
  collection?: NFTCollection;
  uses?: NFTUses;
}

export interface NFTCreator {
  address: string;
  verified: boolean;
  share: number;
}

export interface NFTCollection {
  verified: boolean;
  key: string;
}

export interface NFTUses {
  use_method: string;
  remaining: number;
  total: number;
}

export interface NFT {
  mint: string;
  token_account: string;
  owner: string;
  name: string;
  symbol: string;
  uri: string;
  metadata?: NFTMetadata;
  image?: string;
  description?: string;
}

export interface GetNFTsRequest {
  public_key: string;
}

export interface GetNFTsResponse {
  public_key: string;
  nfts: NFT[];
}

export interface GetNFTMetadataRequest {
  mint_address: string;
}

export interface GetNFTMetadataResponse {
  mint: string;
  metadata: NFTMetadata;
  external_metadata?: {
    name?: string;
    description?: string;
    image?: string;
    attributes?: Array<{
      trait_type: string;
      value: string | number;
    }>;
  };
}

export interface TransferNFTRequest {
  from_wallet_id: string;
  to_public_key: string;
  mint_address: string;
}

export interface TransferNFTResponse {
  signature: string;
  from_public_key: string;
  to_public_key: string;
  mint_address: string;
  status: string;
  timestamp: number;
}

export interface AirdropRecipient {
  address: string;
  amount?: number;
  mint_address?: string;
}

export interface CreateAirdropRequest {
  wallet_id: string;
  type: "sol" | "token" | "nft";
  recipients: AirdropRecipient[];
  token_mint?: string;
  amount_per_recipient?: number;
  memo?: string;
}

export interface AirdropResult {
  recipient: string;
  success: boolean;
  signature?: string;
  error?: string;
}

export interface CreateAirdropResponse {
  airdrop_id: string;
  wallet_id: string;
  type: string;
  total_recipients: number;
  successful: number;
  failed: number;
  results: AirdropResult[];
  started_at: number;
  completed_at: number;
  total_amount?: number;
}

export interface AirdropRecord {
  airdrop_id: string;
  wallet_id: string;
  user_id: string;
  type: string;
  token_mint?: string;
  total_recipients: number;
  successful: number;
  failed: number;
  total_amount?: number;
  status: string;
  created_at: number;
  completed_at?: number;
}

export interface AirdropRecipientRecord {
  id: number;
  airdrop_id: string;
  recipient_address: string;
  amount?: number;
  mint_address?: string;
  success: boolean;
  signature?: string;
  error?: string;
  processed_at: number;
}

export interface TransactionHistoryRequest {
  public_key: string;
  limit?: number;
  before?: string;
  until?: string;
}

export interface ParsedTransaction {
  signature: string;
  block_time: number | null;
  slot: number;
  status: string;
  fee: number;
  type: string;
  amount?: number;
  from?: string;
  to?: string;
  token_mint?: string;
  token_amount?: number;
  token_decimals?: number;
  memo?: string;
  error?: string;
}

export interface TransactionHistoryResponse {
  public_key: string;
  transactions: ParsedTransaction[];
  total: number;
  has_more: boolean;
  oldest_signature?: string;
}

export interface TransactionStatsResponse {
  public_key: string;
  total_transactions: number;
  total_sent_sol: number;
  total_received_sol: number;
  total_fees_paid: number;
  first_transaction?: string;
  last_transaction?: string;
}

export interface CreateStakeAccountRequest {
  wallet_id: string;
  amount_lamports: number;
  validator_vote_address: string;
}

export interface CreateStakeAccountResponse {
  stake_account_address: string;
  wallet_id: string;
  validator_vote_address: string;
  amount_lamports: number;
  amount_sol: number;
  signature: string;
  created_at: number;
}

export interface DelegateStakeRequest {
  wallet_id: string;
  stake_account_address: string;
  validator_vote_address: string;
}

export interface DelegateStakeResponse {
  stake_account_address: string;
  validator_vote_address: string;
  signature: string;
  delegated_at: number;
}

export interface DeactivateStakeRequest {
  wallet_id: string;
  stake_account_address: string;
}

export interface DeactivateStakeResponse {
  stake_account_address: string;
  signature: string;
  deactivated_at: number;
}

export interface WithdrawStakeRequest {
  wallet_id: string;
  stake_account_address: string;
  recipient_address: string;
  amount_lamports: number;
}

export interface WithdrawStakeResponse {
  stake_account_address: string;
  recipient_address: string;
  amount_lamports: number;
  amount_sol: number;
  signature: string;
  withdrawn_at: number;
}

export interface StakeAccountInfo {
  stake_account_address: string;
  balance_lamports: number;
  balance_sol: number;
  status: string;
  voter: string;
  activating_epoch?: number;
  active_epoch?: number;
  deactivating_epoch?: number;
  deactivated_epoch?: number;
}

export interface ValidatorInfo {
  vote_address: string;
  node_address: string;
  commission: number;
  last_vote: number;
  root_slot: number;
  activated_stake: number;
  epoch_vote_account: boolean;
  apy?: number;
}

export interface StakingRewardsInfo {
  stake_account_address: string;
  rewards: StakingReward[];
  total_rewards_lamports: number;
  total_rewards_sol: number;
}

export interface StakingReward {
  epoch: number;
  amount_lamports: number;
  amount_sol: number;
  post_balance: number;
  commission: number;
}

export interface StakeRecord {
  stake_id: string;
  wallet_id: string;
  user_id: string;
  stake_account_address: string;
  validator_vote_address: string;
  amount_lamports: number;
  status: string;
  created_at: number;
  delegated_at?: number;
  deactivated_at?: number;
  withdrawn_at?: number;
  signature: string;
}
