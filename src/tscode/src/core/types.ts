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
