import { Account } from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { NEAR, FungibleToken } from "near-api-js/tokens";
import { USDT } from "near-api-js/tokens/testnet";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { KeyPair } from "near-api-js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  SendNearTokensRequest,
  SendFungibleTokenRequest,
  NearTransferResponse,
  TokenRegistrationStatusResponse,
  RegisterTokenAccountRequest,
  RegisterTokenAccountResponse,
  TokenBalanceResponse,
} from "../../core/near-types.js";

export class NearTokenManager {
  private buildSignerAccount(walletId: string): Account {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) {
      throw new Error("Wallet not found");
    }

    const privateKeyHex = encryptionService.decrypt(
      wallet.encrypted_private_key,
      wallet.encryption_iv,
      wallet.encryption_tag,
    );

    const privateKeyString = Buffer.from(
      privateKeyHex,
      "hex",
    ).toString() as KeyPairString;

    const provider = nearConnection.getProvider();
    return new Account(wallet.account_id, provider, privateKeyString);
  }

  private resolveToken(
    tokenType: "USDT" | "custom",
    contractId?: string,
    decimals?: number,
    symbol?: string,
    name?: string,
  ): FungibleToken {
    if (tokenType === "USDT") {
      if (config.nearNetwork === "mainnet") {
        return new FungibleToken("usdt.tether-token.near", {
          decimals: 6,
          symbol: "USDT",
          name: "Tether USD",
        });
      }
      return USDT;
    }

    if (!contractId) {
      throw new Error("contract_id is required for custom token");
    }

    return new FungibleToken(contractId, {
      decimals: decimals ?? 18,
      symbol: symbol ?? "TOKEN",
      name: name ?? "Custom Token",
    });
  }

  async sendNear(req: SendNearTokensRequest): Promise<NearTransferResponse> {
    try {
      logger.info(
        `Sending NEAR from wallet ${req.from_wallet_id} to ${req.receiver_id}`,
      );

      const wallet = nearDatabase.getWallet(req.from_wallet_id);
      if (!wallet) {
        throw new Error("Wallet not found");
      }

      const account = this.buildSignerAccount(req.from_wallet_id);

      const result = await account.transfer({
        token: NEAR,
        amount: NEAR.toUnits(req.amount),
        receiverId: req.receiver_id,
      });

      const txHash =
        typeof result === "object" && result !== null && "transaction" in result
          ? ((result as any).transaction?.hash ?? JSON.stringify(result))
          : String(result);

      logger.info(`NEAR transfer successful: ${txHash}`);

      nearDatabase.updateWalletLastUsed(req.from_wallet_id);

      return {
        transaction_hash: txHash,
        from_account_id: wallet.account_id,
        receiver_id: req.receiver_id,
        amount: req.amount,
        token: "NEAR",
        status: "confirmed",
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to send NEAR tokens:", error);
      throw error;
    }
  }

  async sendFungibleToken(
    req: SendFungibleTokenRequest,
  ): Promise<NearTransferResponse> {
    try {
      logger.info(
        `Sending ${req.token_type} from wallet ${req.from_wallet_id} to ${req.receiver_id}`,
      );

      const wallet = nearDatabase.getWallet(req.from_wallet_id);
      if (!wallet) {
        throw new Error("Wallet not found");
      }

      const token = this.resolveToken(
        req.token_type,
        req.contract_id,
        req.token_decimals,
        req.token_symbol,
        req.token_name,
      );

      const account = this.buildSignerAccount(req.from_wallet_id);

      const result = await account.transfer({
        token,
        amount: token.toUnits(req.amount),
        receiverId: req.receiver_id,
      });

      const txHash =
        typeof result === "object" && result !== null && "transaction" in result
          ? ((result as any).transaction?.hash ?? JSON.stringify(result))
          : String(result);

      logger.info(`Fungible token transfer successful: ${txHash}`);

      nearDatabase.updateWalletLastUsed(req.from_wallet_id);

      const contractId =
        req.token_type === "USDT"
          ? config.nearNetwork === "mainnet"
            ? "usdt.tether-token.near"
            : "usdtt.fakes.testnet"
          : req.contract_id;

      return {
        transaction_hash: txHash,
        from_account_id: wallet.account_id,
        receiver_id: req.receiver_id,
        amount: req.amount,
        token: req.token_symbol ?? req.token_type,
        token_contract: contractId,
        status: "confirmed",
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to send fungible token:", error);
      throw error;
    }
  }

  async checkTokenRegistration(
    accountId: string,
    tokenType: "USDT" | "custom",
    contractId?: string,
    decimals?: number,
    symbol?: string,
    name?: string,
  ): Promise<TokenRegistrationStatusResponse> {
    try {
      logger.info(
        `Checking ${tokenType} registration for account: ${accountId}`,
      );

      const token = this.resolveToken(
        tokenType,
        contractId,
        decimals,
        symbol,
        name,
      );

      const provider = nearConnection.getProvider();

      const isRegistered = await token.isAccountRegistered({
        accountId,
        provider,
      });

      const resolvedContractId =
        tokenType === "USDT"
          ? config.nearNetwork === "mainnet"
            ? "usdt.tether-token.near"
            : "usdtt.fakes.testnet"
          : (contractId ?? "");

      return {
        account_id: accountId,
        token_contract: resolvedContractId,
        is_registered: isRegistered,
      };
    } catch (error) {
      logger.error("Failed to check token registration:", error);
      throw error;
    }
  }

  async registerTokenAccount(
    req: RegisterTokenAccountRequest,
  ): Promise<RegisterTokenAccountResponse> {
    try {
      logger.info(
        `Registering account ${req.account_id_to_register} for ${req.token_type}`,
      );

      const wallet = nearDatabase.getWallet(req.from_wallet_id);
      if (!wallet) {
        throw new Error("Funding wallet not found");
      }

      const token = this.resolveToken(req.token_type, req.contract_id);

      const fundingAccount = this.buildSignerAccount(req.from_wallet_id);

      await token.registerAccount({
        accountIdToRegister: req.account_id_to_register,
        fundingAccount,
      });

      logger.info(
        `Account ${req.account_id_to_register} registered for ${req.token_type}`,
      );

      nearDatabase.updateWalletLastUsed(req.from_wallet_id);

      const resolvedContractId =
        req.token_type === "USDT"
          ? config.nearNetwork === "mainnet"
            ? "usdt.tether-token.near"
            : "usdtt.fakes.testnet"
          : (req.contract_id ?? "");

      return {
        success: true,
        registered_account_id: req.account_id_to_register,
        token_contract: resolvedContractId,
        funded_by: wallet.account_id,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to register token account:", error);
      throw error;
    }
  }

  async getTokenBalance(
    walletId: string,
    tokenType: "USDT" | "custom",
    contractId?: string,
    decimals?: number,
    symbol?: string,
    name?: string,
  ): Promise<TokenBalanceResponse> {
    try {
      const wallet = nearDatabase.getWallet(walletId);
      if (!wallet) {
        throw new Error("Wallet not found");
      }

      logger.info(
        `Getting ${tokenType} balance for account: ${wallet.account_id}`,
      );

      const token = this.resolveToken(
        tokenType,
        contractId,
        decimals,
        symbol,
        name,
      );

      const provider = nearConnection.getProvider();
      const account = new Account(wallet.account_id, provider);

      const balanceRaw = await token.getBalance(account);
      const balanceDecimal = token.toDecimal(balanceRaw);

      const resolvedContractId =
        tokenType === "USDT"
          ? config.nearNetwork === "mainnet"
            ? "usdt.tether-token.near"
            : "usdtt.fakes.testnet"
          : (contractId ?? "");

      const resolvedSymbol = symbol ?? tokenType;

      return {
        account_id: wallet.account_id,
        token_contract: resolvedContractId,
        token_symbol: resolvedSymbol,
        balance_raw: String(balanceRaw),
        balance_decimal: String(balanceDecimal),
      };
    } catch (error) {
      logger.error("Failed to get token balance:", error);
      throw error;
    }
  }
}

export const nearTokenManager = new NearTokenManager();
