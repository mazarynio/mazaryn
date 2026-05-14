import { AccountAddress } from "@aptos-labs/ts-sdk";
import { aptosConnection } from "./connection.js";
import { aptosAccountManager } from "./account.js";
import { aptosDatabase } from "../database/aptos-database.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  AptosStakeRequest,
  AptosStakeResponse,
  AptosUnstakeRequest,
  AptosUnstakeResponse,
  AptosWithdrawStakeRequest,
  AptosWithdrawStakeResponse,
  AptosGetStakeInfoRequest,
  AptosGetStakeInfoResponse,
  AptosGetDelegationPoolsResponse,
  AptosReactivateStakeRequest,
  AptosReactivateStakeResponse,
} from "../../core/aptos-types.js";

export class AptosStakingManager {
  async addStake(req: AptosStakeRequest): Promise<AptosStakeResponse> {
    try {
      logger.info(
        `Adding stake of ${req.amount_octas} octas from ${req.delegator_address} to pool ${req.pool_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.delegator_address);
      if (!wallet) {
        throw new Error("Delegator wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Delegator account not loaded in cache");
      }

      const client = aptosConnection.getClient(req.network);

      const transaction = await client.transaction.build.simple({
        sender: cached.account.accountAddress,
        data: {
          function: "0x1::delegation_pool::add_stake",
          functionArguments: [
            AccountAddress.fromString(req.pool_address),
            req.amount_octas,
          ],
        },
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);
      aptosDatabase.recordTransaction({
        wallet_id: wallet.wallet_id,
        user_id: wallet.user_id,
        from_address: req.delegator_address,
        to_address: req.pool_address,
        transaction_hash: pendingTxn.hash,
        tx_type: "stake_add",
        amount_octas: req.amount_octas,
        pool_address: req.pool_address,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        network: req.network ?? config.aptosNetwork,
      });
      logger.info(`Stake added: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        delegator_address: req.delegator_address,
        pool_address: req.pool_address,
        amount_octas: req.amount_octas,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to add stake:", error);
      throw error;
    }
  }

  async unlockStake(req: AptosUnstakeRequest): Promise<AptosUnstakeResponse> {
    try {
      logger.info(
        `Unlocking ${req.amount_octas} octas from pool ${req.pool_address} for ${req.delegator_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.delegator_address);
      if (!wallet) {
        throw new Error("Delegator wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Delegator account not loaded in cache");
      }

      const client = aptosConnection.getClient(req.network);

      const transaction = await client.transaction.build.simple({
        sender: cached.account.accountAddress,
        data: {
          function: "0x1::delegation_pool::unlock",
          functionArguments: [
            AccountAddress.fromString(req.pool_address),
            req.amount_octas,
          ],
        },
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);
      aptosDatabase.recordTransaction({
        wallet_id: wallet.wallet_id,
        user_id: wallet.user_id,
        from_address: req.delegator_address,
        to_address: req.pool_address,
        transaction_hash: pendingTxn.hash,
        tx_type: "stake_unlock",
        amount_octas: req.amount_octas,
        pool_address: req.pool_address,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        network: req.network ?? config.aptosNetwork,
      });
      logger.info(`Stake unlocked: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        delegator_address: req.delegator_address,
        pool_address: req.pool_address,
        amount_octas: req.amount_octas,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to unlock stake:", error);
      throw error;
    }
  }

  async withdrawStake(
    req: AptosWithdrawStakeRequest,
  ): Promise<AptosWithdrawStakeResponse> {
    try {
      logger.info(
        `Withdrawing unlocked stake from pool ${req.pool_address} for ${req.delegator_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.delegator_address);
      if (!wallet) {
        throw new Error("Delegator wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Delegator account not loaded in cache");
      }

      const client = aptosConnection.getClient(req.network);

      const transaction = await client.transaction.build.simple({
        sender: cached.account.accountAddress,
        data: {
          function: "0x1::delegation_pool::withdraw",
          functionArguments: [
            AccountAddress.fromString(req.pool_address),
            "18446744073709551615",
          ],
        },
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);
      aptosDatabase.recordTransaction({
        wallet_id: wallet.wallet_id,
        user_id: wallet.user_id,
        from_address: req.delegator_address,
        to_address: req.pool_address,
        transaction_hash: pendingTxn.hash,
        tx_type: "stake_withdraw",
        amount_octas: 0,
        pool_address: req.pool_address,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        network: req.network ?? config.aptosNetwork,
      });
      logger.info(`Stake withdrawn: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        delegator_address: req.delegator_address,
        pool_address: req.pool_address,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to withdraw stake:", error);
      throw error;
    }
  }

  async reactivateStake(
    req: AptosReactivateStakeRequest,
  ): Promise<AptosReactivateStakeResponse> {
    try {
      logger.info(
        `Reactivating ${req.amount_octas} octas in pool ${req.pool_address} for ${req.delegator_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.delegator_address);
      if (!wallet) {
        throw new Error("Delegator wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Delegator account not loaded in cache");
      }

      const client = aptosConnection.getClient(req.network);

      const transaction = await client.transaction.build.simple({
        sender: cached.account.accountAddress,
        data: {
          function: "0x1::delegation_pool::reactivate_stake",
          functionArguments: [
            AccountAddress.fromString(req.pool_address),
            req.amount_octas,
          ],
        },
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);
      aptosDatabase.recordTransaction({
        wallet_id: wallet.wallet_id,
        user_id: wallet.user_id,
        from_address: req.delegator_address,
        to_address: req.pool_address,
        transaction_hash: pendingTxn.hash,
        tx_type: "stake_reactivate",
        amount_octas: req.amount_octas,
        pool_address: req.pool_address,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        network: req.network ?? config.aptosNetwork,
      });
      logger.info(`Stake reactivated: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        delegator_address: req.delegator_address,
        pool_address: req.pool_address,
        amount_octas: req.amount_octas,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to reactivate stake:", error);
      throw error;
    }
  }

  async getStakeInfo(
    req: AptosGetStakeInfoRequest,
  ): Promise<AptosGetStakeInfoResponse> {
    try {
      logger.info(
        `Getting stake info for delegator ${req.delegator_address} in pool ${req.pool_address}`,
      );

      const client = aptosConnection.getClient(req.network);

      const payload = {
        function:
          "0x1::delegation_pool::get_stake" as `${string}::${string}::${string}`,
        functionArguments: [req.pool_address, req.delegator_address],
      };

      const result = await client.view({ payload });

      const active = String((result as any[])[0] ?? "0");
      const inactive = String((result as any[])[1] ?? "0");
      const pendingInactive = String((result as any[])[2] ?? "0");

      return {
        delegator_address: req.delegator_address,
        pool_address: req.pool_address,
        active_stake: active,
        inactive_stake: inactive,
        pending_inactive_stake: pendingInactive,
      };
    } catch (error) {
      logger.error("Failed to get stake info:", error);
      throw error;
    }
  }

  async getDelegationPools(
    networkStr?: string,
  ): Promise<AptosGetDelegationPoolsResponse> {
    try {
      logger.info("Getting delegation pools");

      const client = aptosConnection.getClient(networkStr);

      const pools = await client.getDelegatedStakingActivities({
        options: {
          limit: 50,
        },
      });

      const poolMap = new Map<string, any>();

      for (const activity of pools) {
        const poolAddr = (activity as any).pool_address;
        if (poolAddr && !poolMap.has(poolAddr)) {
          poolMap.set(poolAddr, {
            pool_address: poolAddr,
            operator_address: (activity as any).operator_address ?? "",
            voter_address: "",
            total_staked: "0",
            commission_percentage: 0,
          });
        }
      }

      const poolList = Array.from(poolMap.values());

      return {
        pools: poolList,
        total: poolList.length,
      };
    } catch (error) {
      logger.error("Failed to get delegation pools:", error);
      throw error;
    }
  }

  async getPoolStakeInfo(
    poolAddress: string,
    networkStr?: string,
  ): Promise<any> {
    try {
      logger.info(`Getting pool stake info for pool: ${poolAddress}`);

      const client = aptosConnection.getClient(networkStr);

      const payload = {
        function:
          "0x1::delegation_pool::delegation_pool_exists" as `${string}::${string}::${string}`,
        functionArguments: [poolAddress],
      };

      const exists = await client.view({ payload });

      if (!(exists as any[])[0]) {
        throw new Error(`Delegation pool does not exist: ${poolAddress}`);
      }

      const totalStakePayload = {
        function:
          "0x1::delegation_pool::get_pool_used_voting_power" as `${string}::${string}::${string}`,
        functionArguments: [poolAddress],
      };

      let votingPower = "0";
      try {
        const vpResult = await client.view({ payload: totalStakePayload });
        votingPower = String((vpResult as any[])[0] ?? "0");
      } catch {
        logger.info("Could not fetch voting power, pool may not support it");
      }

      return {
        pool_address: poolAddress,
        exists: true,
        voting_power: votingPower,
      };
    } catch (error) {
      logger.error("Failed to get pool stake info:", error);
      throw error;
    }
  }

  async getStakingActivities(
    delegatorAddress: string,
    networkStr?: string,
  ): Promise<any[]> {
    try {
      logger.info(
        `Getting staking activities for delegator: ${delegatorAddress}`,
      );

      const client = aptosConnection.getClient(networkStr);

      const activities = await client.getDelegatedStakingActivities({
        delegatorAddress,
        options: {
          limit: 100,
        },
      });

      return activities.map((a: any) => ({
        event_index: a.event_index ?? 0,
        delegator_address: a.delegator_address ?? delegatorAddress,
        pool_address: a.pool_address ?? "",
        event_type: a.event_type ?? "",
        amount: String(a.amount ?? "0"),
        transaction_version: String(a.transaction_version ?? "0"),
      }));
    } catch (error) {
      logger.error("Failed to get staking activities:", error);
      throw error;
    }
  }
}

export const aptosStakingManager = new AptosStakingManager();
