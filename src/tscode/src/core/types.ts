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
