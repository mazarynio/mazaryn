export interface CreateNamedAccountRequest {
  user_id: string;
  signer_wallet_id: string;
  account_name: string;
  initial_balance_near?: string;
  method?: "direct" | "contract";
}

export interface CreateSubAccountRequest {
  user_id: string;
  signer_wallet_id: string;
  prefix: string;
  initial_balance_near?: string;
}

export interface ImportNearAccountRequest {
  user_id: string;
  account_id: string;
  private_key: string;
  account_name?: string;
}

export interface DeleteNearAccountRequest {
  user_id: string;
  wallet_id: string;
  beneficiary_wallet_id: string;
}

export interface CreateNearAccountResponse {
  account_id: string;
  wallet_id: string;
  user_id: string;
  public_key: string;
  account_type: "named" | "subaccount" | "implicit";
  parent_account_id?: string;
  network: string;
  created_at: number;
}

export interface DeleteNearAccountResponse {
  success: boolean;
  deleted_account_id: string;
  beneficiary_account_id: string;
  timestamp: number;
}

export interface NearAccountInfo {
  wallet_id: string;
  account_id: string;
  public_key: string;
  user_id: string;
  account_name?: string;
  account_type: "named" | "subaccount" | "implicit";
  parent_account_id?: string;
  network: string;
  created_at: number;
}

export interface NearWalletRecord {
  wallet_id: string;
  user_id: string;
  account_id: string;
  public_key: string;
  encrypted_private_key: string;
  encryption_iv: string;
  encryption_tag: string;
  account_name?: string;
  account_type: "named" | "subaccount" | "implicit";
  parent_account_id?: string;
  network: string;
  created_at: number;
  last_used?: number;
  is_deleted: number;
  deleted_at?: number;
  beneficiary_account_id?: string;
}

export interface NearErrorResponse {
  error: string;
  details?: unknown;
}

export interface NearBalanceResponse {
  account_id: string;
  balance_yocto: string;
  balance_near: string;
}

export interface NearAccountStateResponse {
  account_id: string;
  amount: string;
  locked: string;
  code_hash: string;
  storage_usage: number;
  storage_paid_at: number;
}

export interface NearMultiTokenBalanceResponse {
  account_id: string;
  balances: Array<{
    token: string;
    contract: string;
    balance_raw: string;
    balance_decimal: string;
  }>;
}

export interface NearEpochPriceResponse {
  current_epoch_seat_price_near: string;
  next_epoch_seat_price_near: string;
}

export type NearTokenType = "NEAR" | "USDT" | "custom";

export interface SendNearTokensRequest {
  from_wallet_id: string;
  receiver_id: string;
  amount: string;
}

export interface SendFungibleTokenRequest {
  from_wallet_id: string;
  receiver_id: string;
  amount: string;
  token_type: "USDT" | "custom";
  contract_id?: string;
  token_decimals?: number;
  token_symbol?: string;
  token_name?: string;
}

export interface NearTransferResponse {
  transaction_hash: string;
  from_account_id: string;
  receiver_id: string;
  amount: string;
  token: string;
  token_contract?: string;
  status: string;
  timestamp: number;
}

export interface TokenRegistrationStatusResponse {
  account_id: string;
  token_contract: string;
  is_registered: boolean;
}

export interface RegisterTokenAccountRequest {
  from_wallet_id: string;
  account_id_to_register: string;
  token_type: "USDT" | "custom";
  contract_id?: string;
}

export interface RegisterTokenAccountResponse {
  success: boolean;
  registered_account_id: string;
  token_contract: string;
  funded_by: string;
  timestamp: number;
}

export interface TokenBalanceResponse {
  account_id: string;
  token_contract: string;
  token_symbol: string;
  balance_raw: string;
  balance_decimal: string;
}

export interface AccessKeyInfo {
  public_key: string;
  access_key: {
    nonce: number;
    permission:
      | "FullAccess"
      | {
          FunctionCall: {
            allowance: string | null;
            receiver_id: string;
            method_names: string[];
          };
        };
  };
}

export interface GetAccessKeysResponse {
  account_id: string;
  wallet_id?: string;
  keys: AccessKeyInfo[];
  total: number;
  block_hash: string;
  block_height: number;
}

export interface AddFullAccessKeyRequest {
  wallet_id: string;
  label?: string;
}

export interface AddFullAccessKeyResponse {
  wallet_id: string;
  account_id: string;
  new_public_key: string;
  new_private_key: string;
  label?: string;
  timestamp: number;
}

export interface AddFunctionCallKeyRequest {
  wallet_id: string;
  contract_id: string;
  method_names?: string[];
  allowance_near?: string;
  label?: string;
}

export interface AddFunctionCallKeyResponse {
  wallet_id: string;
  account_id: string;
  new_public_key: string;
  new_private_key: string;
  contract_id: string;
  method_names: string[];
  allowance_near?: string;
  label?: string;
  timestamp: number;
}

export interface DeleteKeyRequest {
  wallet_id: string;
  public_key: string;
}

export interface DeleteKeyResponse {
  wallet_id: string;
  account_id: string;
  deleted_public_key: string;
  timestamp: number;
}

export interface GenerateSeedPhraseResponse {
  seed_phrase: string;
  public_key: string;
  private_key: string;
}

export interface ImportFromSeedPhraseRequest {
  user_id: string;
  account_id: string;
  seed_phrase: string;
  account_name?: string;
}

export interface NearKeyRecord {
  key_id: string;
  wallet_id: string;
  user_id: string;
  account_id: string;
  public_key: string;
  encrypted_private_key: string;
  encryption_iv: string;
  encryption_tag: string;
  key_type: "full_access" | "function_call";
  contract_id?: string;
  method_names?: string;
  allowance_near?: string;
  label?: string;
  created_at: number;
  is_active: number;
}

export interface BatchAction {
  type:
    | "transfer"
    | "functionCall"
    | "addFullAccessKey"
    | "addFunctionCallKey"
    | "deleteKey"
    | "deployContract"
    | "stake";
  transfer?: {
    amount_near: string;
  };
  function_call?: {
    method_name: string;
    args?: Record<string, unknown>;
    gas_tera?: string;
    deposit_near?: string;
  };
  add_full_access_key?: {
    public_key: string;
  };
  add_function_call_key?: {
    public_key: string;
    contract_id: string;
    method_names?: string[];
    allowance_near?: string;
  };
  delete_key?: {
    public_key: string;
  };
  stake?: {
    amount_near: string;
    public_key: string;
  };
}

export interface SignAndSendTransactionRequest {
  from_wallet_id: string;
  receiver_id: string;
  actions: BatchAction[];
  wait_until?: "NONE" | "INCLUDED" | "EXECUTED" | "FINAL";
}

export interface SignAndSendTransactionResponse {
  transaction_hash: string;
  from_account_id: string;
  receiver_id: string;
  actions_count: number;
  status: string;
  timestamp: number;
  raw?: unknown;
}

export interface CallFunctionRequest {
  from_wallet_id: string;
  contract_id: string;
  method_name: string;
  args?: Record<string, unknown>;
  gas_tera?: string;
  deposit_near?: string;
  wait_until?: "NONE" | "INCLUDED" | "EXECUTED" | "FINAL";
}

export interface CallFunctionResponse {
  transaction_hash: string;
  from_account_id: string;
  contract_id: string;
  method_name: string;
  status: string;
  timestamp: number;
  raw?: unknown;
}

export interface ViewFunctionRequest {
  contract_id: string;
  method_name: string;
  args?: Record<string, unknown>;
}

export interface ViewFunctionResponse {
  contract_id: string;
  method_name: string;
  result: unknown;
}

export interface CreateAndSignTransactionRequest {
  from_wallet_id: string;
  receiver_id: string;
  actions: BatchAction[];
}

export interface CreateAndSignTransactionResponse {
  from_account_id: string;
  receiver_id: string;
  signed_transaction: unknown;
  timestamp: number;
}

export interface SendSignedTransactionRequest {
  signed_transaction: unknown;
}

export interface NearTransactionRecord {
  tx_id: string;
  wallet_id: string;
  user_id: string;
  from_account_id: string;
  receiver_id: string;
  transaction_hash: string;
  status: string;
  actions_count: number;
  tx_type: string;
  contract_id?: string;
  method_name?: string;
  amount_near?: string;
  gas_tera?: string;
  created_at: number;
}

export interface CreateMetaTransactionRequest {
  from_wallet_id: string;
  receiver_id: string;
  actions: BatchAction[];
  block_height_ttl?: number;
}

export interface CreateMetaTransactionResponse {
  from_account_id: string;
  receiver_id: string;
  signed_delegate: unknown;
  timestamp: number;
}

export interface RelayMetaTransactionRequest {
  relayer_wallet_id: string;
  signed_delegate: unknown;
}

export interface RelayMetaTransactionResponse {
  transaction_hash: string;
  relayer_account_id: string;
  status: string;
  timestamp: number;
  raw?: unknown;
}

export interface CreateImplicitAccountResponse {
  account_id: string;
  wallet_id: string;
  user_id: string;
  public_key: string;
  account_type: "implicit";
  network: string;
  created_at: number;
  note: string;
}

export interface FundImplicitAccountRequest {
  from_wallet_id: string;
  implicit_account_id: string;
  amount_yocto?: string;
}

export interface FundImplicitAccountResponse {
  transaction_hash: string;
  from_account_id: string;
  implicit_account_id: string;
  amount_yocto: string;
  status: string;
  timestamp: number;
}

export interface AddMultipleKeysRequest {
  wallet_id: string;
  key_count: number;
  label_prefix?: string;
}

export interface AddMultipleKeysResponse {
  wallet_id: string;
  account_id: string;
  keys_added: number;
  transaction_hash: string;
  keys: Array<{
    public_key: string;
    private_key: string;
    label: string;
  }>;
  timestamp: number;
}

export interface MultiKeyTransferRequest {
  wallet_id: string;
  transfers: Array<{
    receiver_id: string;
    amount_near: string;
  }>;
}

export interface MultiKeyTransferResponse {
  wallet_id: string;
  account_id: string;
  total_transfers: number;
  successful: number;
  failed: number;
  results: Array<{
    receiver_id: string;
    transaction_hash?: string;
    status: "confirmed" | "failed";
    error?: string;
  }>;
  timestamp: number;
}

export interface DeployContractRequest {
  wallet_id: string;
  wasm_base64: string;
}

export interface DeployContractResponse {
  wallet_id: string;
  account_id: string;
  transaction_hash: string;
  status: string;
  timestamp: number;
}

export interface DeployGlobalContractRequest {
  wallet_id: string;
  wasm_base64: string;
  deploy_mode: "accountId" | "codeHash";
}

export interface DeployGlobalContractResponse {
  wallet_id: string;
  account_id: string;
  transaction_hash: string;
  deploy_mode: "accountId" | "codeHash";
  code_hash?: string;
  status: string;
  timestamp: number;
}

export interface UseGlobalContractRequest {
  wallet_id: string;
  identifier_type: "accountId" | "codeHash";
  account_id?: string;
  code_hash?: string;
}

export interface UseGlobalContractResponse {
  wallet_id: string;
  account_id: string;
  transaction_hash: string;
  identifier_type: "accountId" | "codeHash";
  status: string;
  timestamp: number;
}

export interface VerifyNep413MessageRequest {
  signer_account_id: string;
  signer_public_key: string;
  signature_base64: string;
  message: string;
  recipient: string;
  nonce_hex: string;
}

export interface VerifyNep413MessageResponse {
  valid: boolean;
  signer_account_id: string;
  message: string;
  recipient: string;
  timestamp: number;
}

export interface StakeNearRequest {
  wallet_id: string;
  amount_near: string;
  validator_public_key: string;
}

export interface StakeNearResponse {
  wallet_id: string;
  account_id: string;
  transaction_hash: string;
  amount_near: string;
  validator_public_key: string;
  status: string;
  timestamp: number;
}

export interface UnstakeNearRequest {
  wallet_id: string;
  amount_near: string;
  validator_public_key: string;
}

export interface UnstakeNearResponse {
  wallet_id: string;
  account_id: string;
  transaction_hash: string;
  amount_near: string;
  status: string;
  timestamp: number;
}
