import { Account } from "near-api-js";
import { NEAR, FungibleToken } from "near-api-js/tokens";
import { USDT } from "near-api-js/tokens/testnet";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  NearAccountStateResponse,
  NearBalanceResponse,
  NearMultiTokenBalanceResponse,
  NearEpochPriceResponse,
} from "../../core/near-types.js";

export class NearStateManager {
  async getAccountStateByAccountId(
    accountId: string,
  ): Promise<NearAccountStateResponse> {
    try {
      logger.info("Getting NEAR account state via RPC for:", accountId);
      const state = await nearConnection.viewAccount(accountId);

      return {
        account_id: accountId,
        amount: state.amount,
        locked: state.locked,
        code_hash: state.code_hash,
        storage_usage: state.storage_usage,
        storage_paid_at: state.storage_paid_at ?? 0,
      };
    } catch (error) {
      logger.error("Failed to get account state:", error);
      throw error;
    }
  }

  async getAccountStateByWalletId(
    walletId: string,
    userId: string,
  ): Promise<NearAccountStateResponse> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) {
      throw new Error("Wallet not found");
    }
    if (wallet.user_id !== userId) {
      throw new Error("Unauthorized");
    }
    return this.getAccountStateByAccountId(wallet.account_id);
  }

  async getNearBalance(accountId: string): Promise<NearBalanceResponse> {
    try {
      logger.info("Getting NEAR balance for:", accountId);

      const provider = nearConnection.getProvider();
      const account = new Account(accountId, provider);
      const balanceRaw = await account.getBalance(NEAR);
      const balanceDecimal = NEAR.toDecimal(balanceRaw, 6);

      return {
        account_id: accountId,
        balance_yocto: balanceRaw.toString(),
        balance_near: balanceDecimal,
      };
    } catch (error) {
      logger.error("Failed to get NEAR balance:", error);
      throw error;
    }
  }

  async getMultiTokenBalance(
    accountId: string,
    customTokens?: Array<{
      contract_id: string;
      decimals: number;
      symbol: string;
      name: string;
    }>,
  ): Promise<NearMultiTokenBalanceResponse> {
    try {
      logger.info("Getting multi-token balances for:", accountId);

      const provider = nearConnection.getProvider();
      const account = new Account(accountId, provider);

      const nearBalanceRaw = await account.getBalance(NEAR);
      const nearBalance = {
        token: "NEAR",
        contract: "native",
        balance_raw: nearBalanceRaw.toString(),
        balance_decimal: NEAR.toDecimal(nearBalanceRaw, 6),
      };

      const usdtBalanceRaw = await account.getBalance(USDT).catch(() => 0n);
      const usdtBalance = {
        token: "USDT",
        contract:
          config.nearNetwork === "mainnet"
            ? "usdt.tether-token.near"
            : "usdtt.fakes.testnet",
        balance_raw: usdtBalanceRaw.toString(),
        balance_decimal: USDT.toDecimal(usdtBalanceRaw, 6),
      };

      const customBalances = await Promise.all(
        (customTokens ?? []).map(async (t) => {
          try {
            const ft = new FungibleToken(t.contract_id, {
              decimals: t.decimals,
              symbol: t.symbol,
              name: t.name,
            });
            const raw = await account.getBalance(ft).catch(() => 0n);
            return {
              token: t.symbol,
              contract: t.contract_id,
              balance_raw: raw.toString(),
              balance_decimal: ft.toDecimal(raw, 6),
            };
          } catch {
            return {
              token: t.symbol,
              contract: t.contract_id,
              balance_raw: "0",
              balance_decimal: "0",
            };
          }
        }),
      );

      return {
        account_id: accountId,
        balances: [nearBalance, usdtBalance, ...customBalances],
      };
    } catch (error) {
      logger.error("Failed to get multi-token balances:", error);
      throw error;
    }
  }

  async getEpochPrices(): Promise<NearEpochPriceResponse> {
    try {
      logger.info("Getting NEAR epoch seat prices");

      const current = await nearConnection.getCurrentEpochSeatPrice();
      const next = await nearConnection.getNextEpochSeatPrice();

      return {
        current_epoch_seat_price_near: current,
        next_epoch_seat_price_near: next,
      };
    } catch (error) {
      logger.error("Failed to get epoch prices:", error);
      throw error;
    }
  }
}

export const nearStateManager = new NearStateManager();
