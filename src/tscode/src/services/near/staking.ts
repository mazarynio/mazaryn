import { Account, actions, nearToYocto } from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import type {
  StakeNearRequest,
  StakeNearResponse,
  UnstakeNearRequest,
  UnstakeNearResponse,
} from "../../core/near-types.js";

export class NearStakingManager {
  private buildSignerAccount(walletId: string): {
    account: Account;
    accountId: string;
  } {
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
    const account = new Account(wallet.account_id, provider, privateKeyString);

    return { account, accountId: wallet.account_id };
  }

  private extractTxHash(result: any): string {
    if (result?.transaction?.hash) return result.transaction.hash;
    if (typeof result === "string") return result;
    return JSON.stringify(result);
  }

  async stake(
    req: StakeNearRequest,
    userId: string,
  ): Promise<StakeNearResponse> {
    const wallet = nearDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Staking ${req.amount_near} NEAR from ${wallet.account_id} with validator ${req.validator_public_key}`,
    );

    const { account, accountId } = this.buildSignerAccount(req.wallet_id);

    const result = await account.signAndSendTransaction({
      receiverId: accountId,
      actions: [
        actions.stake(nearToYocto(req.amount_near), req.validator_public_key),
      ],
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(req.wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: accountId,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "stake",
      amount_near: req.amount_near,
    });

    logger.info(`Stake transaction confirmed: ${txHash}`);

    return {
      wallet_id: req.wallet_id,
      account_id: accountId,
      transaction_hash: txHash,
      amount_near: req.amount_near,
      validator_public_key: req.validator_public_key,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async unstake(
    req: UnstakeNearRequest,
    userId: string,
  ): Promise<UnstakeNearResponse> {
    const wallet = nearDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Unstaking ${req.amount_near} NEAR from ${wallet.account_id}`);

    const { account, accountId } = this.buildSignerAccount(req.wallet_id);

    const result = await account.signAndSendTransaction({
      receiverId: accountId,
      actions: [actions.stake(0n, req.validator_public_key)],
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(req.wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: accountId,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "unstake",
      amount_near: req.amount_near,
    });

    logger.info(`Unstake transaction confirmed: ${txHash}`);

    return {
      wallet_id: req.wallet_id,
      account_id: accountId,
      transaction_hash: txHash,
      amount_near: req.amount_near,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async getStakedBalance(
    walletId: string,
    userId: string,
    validatorAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Getting staked balance for ${wallet.account_id} at validator ${validatorAccountId}`,
    );

    const provider = nearConnection.getProvider();
    const viewResult = await (provider as any).callFunction({
      contractId: validatorAccountId,
      method: "get_account_staked_balance",
      args: { account_id: wallet.account_id },
    });

    const stakedYocto = viewResult?.toString() ?? "0";
    const YOCTO_PER_NEAR = BigInt("1000000000000000000000000");
    const stakedNear = (BigInt(stakedYocto) / YOCTO_PER_NEAR).toString();

    return {
      wallet_id: walletId,
      account_id: wallet.account_id,
      validator_account_id: validatorAccountId,
      staked_yocto: stakedYocto,
      staked_near: stakedNear,
    };
  }

  async getUnstakedBalance(
    walletId: string,
    userId: string,
    validatorAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Getting unstaked balance for ${wallet.account_id} at validator ${validatorAccountId}`,
    );

    const provider = nearConnection.getProvider();
    const viewResult = await (provider as any).callFunction({
      contractId: validatorAccountId,
      method: "get_account_unstaked_balance",
      args: { account_id: wallet.account_id },
    });

    const unstakedYocto = viewResult?.toString() ?? "0";
    const YOCTO_PER_NEAR = BigInt("1000000000000000000000000");
    const unstakedNear = (BigInt(unstakedYocto) / YOCTO_PER_NEAR).toString();

    return {
      wallet_id: walletId,
      account_id: wallet.account_id,
      validator_account_id: validatorAccountId,
      unstaked_yocto: unstakedYocto,
      unstaked_near: unstakedNear,
    };
  }

  async isUnstakedBalanceAvailable(
    walletId: string,
    userId: string,
    validatorAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Checking if unstaked balance is available for ${wallet.account_id}`,
    );

    const provider = nearConnection.getProvider();
    const viewResult = await (provider as any).callFunction({
      contractId: validatorAccountId,
      method: "is_account_unstaked_balance_available",
      args: { account_id: wallet.account_id },
    });

    return {
      wallet_id: walletId,
      account_id: wallet.account_id,
      validator_account_id: validatorAccountId,
      is_available: viewResult === true,
    };
  }

  async withdrawStake(
    walletId: string,
    userId: string,
    validatorAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Withdrawing unstaked NEAR for ${wallet.account_id} from validator ${validatorAccountId}`,
    );

    const { account, accountId } = this.buildSignerAccount(walletId);

    const result = await account.callFunction({
      contractId: validatorAccountId,
      methodName: "withdraw_all",
      args: {},
      gas: BigInt("125000000000000"),
      deposit: 0n,
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: validatorAccountId,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "withdraw_stake",
    });

    logger.info(`Withdraw stake confirmed: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      validator_account_id: validatorAccountId,
      transaction_hash: txHash,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async getValidatorInfo(validatorAccountId: string): Promise<any> {
    logger.info(`Getting validator info for: ${validatorAccountId}`);

    const provider = nearConnection.getProvider();

    const [totalStaked, totalShares] = await Promise.all([
      (provider as any)
        .callFunction({
          contractId: validatorAccountId,
          method: "get_total_staked_balance",
          args: {},
        })
        .catch(() => "0"),
      (provider as any)
        .callFunction({
          contractId: validatorAccountId,
          method: "get_number_of_accounts",
          args: {},
        })
        .catch(() => 0),
    ]);

    const YOCTO_PER_NEAR = BigInt("1000000000000000000000000");
    const totalStakedYocto = totalStaked?.toString() ?? "0";
    const totalStakedNear = (
      BigInt(totalStakedYocto) / YOCTO_PER_NEAR
    ).toString();

    return {
      validator_account_id: validatorAccountId,
      total_staked_yocto: totalStakedYocto,
      total_staked_near: totalStakedNear,
      total_stakers: totalShares,
    };
  }

  async getCurrentEpochSeatPrice(): Promise<any> {
    logger.info("Getting current and next epoch seat prices");

    const current = await nearConnection.getCurrentEpochSeatPrice();
    const next = await nearConnection.getNextEpochSeatPrice();

    return {
      current_epoch_seat_price_near: current,
      next_epoch_seat_price_near: next,
    };
  }
}

export const nearStakingManager = new NearStakingManager();
