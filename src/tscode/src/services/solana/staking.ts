import {
  createTransactionMessage,
  setTransactionMessageFeePayerSigner,
  setTransactionMessageLifetimeUsingBlockhash,
  appendTransactionMessageInstructions,
  pipe,
  signTransactionMessageWithSigners,
  sendAndConfirmTransactionFactory,
  getSignatureFromTransaction,
  lamports,
  address,
  generateKeyPairSigner,
} from "@solana/kit";
import { solanaConnection } from "./connection.js";
import { walletDatabase } from "../database/database.js";
import { logger } from "../../core/logger.js";
import type {
  CreateStakeAccountRequest,
  CreateStakeAccountResponse,
  DelegateStakeRequest,
  DelegateStakeResponse,
  DeactivateStakeRequest,
  DeactivateStakeResponse,
  WithdrawStakeRequest,
  WithdrawStakeResponse,
  StakeAccountInfo,
  ValidatorInfo,
  StakingRewardsInfo,
} from "../../core/types.js";

const STAKE_PROGRAM_ID = "Stake11111111111111111111111111111111111111111";
const SYSVAR_CLOCK_PUBKEY = "SysvarC1ock11111111111111111111111111111111";
const SYSVAR_STAKE_HISTORY_PUBKEY =
  "SysvarStakeHistory1111111111111111111111111";
const STAKE_CONFIG_ID = "StakeConfig11111111111111111111111111111111";
const SYSTEM_PROGRAM_ID = "11111111111111111111111111111111";

const STAKE_ACCOUNT_SIZE = 200;
const LAMPORTS_PER_SOL = 1_000_000_000;

export class StakingManager {
  private generateStakeId(): string {
    return `stake_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }

  async createAndDelegateStake(
    signer: any,
    walletId: string,
    userId: string,
    req: CreateStakeAccountRequest,
  ): Promise<CreateStakeAccountResponse> {
    try {
      logger.info(`Creating stake account for wallet: ${walletId}`);

      const rpc = solanaConnection.getRpc();
      const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

      const rentExemptBalance = await rpc
        .getMinimumBalanceForRentExemption(BigInt(STAKE_ACCOUNT_SIZE))
        .send();

      const totalRequired = Number(rentExemptBalance) + req.amount_lamports;

      const balance = await solanaConnection.getBalance(
        req.validator_vote_address.length > 0 ? signer.address : signer.address,
      );
      const currentBalance = Number(balance.value);

      if (currentBalance < totalRequired) {
        throw new Error(
          `Insufficient balance. Required: ${totalRequired} lamports, Available: ${currentBalance} lamports`,
        );
      }

      const stakeKeypair = await generateKeyPairSigner();
      const stakeAccountAddress = stakeKeypair.address;

      const { value: latestBlockhash } =
        await solanaConnection.getLatestBlockhash();

      const createAccountInstruction = {
        programAddress: address(SYSTEM_PROGRAM_ID),
        accounts: [
          { address: signer.address, role: 3 },
          { address: stakeAccountAddress, role: 3 },
        ],
        data: this.encodeCreateAccountInstruction(
          totalRequired,
          STAKE_ACCOUNT_SIZE,
          STAKE_PROGRAM_ID,
        ),
      };

      const initializeInstruction = {
        programAddress: address(STAKE_PROGRAM_ID),
        accounts: [
          { address: stakeAccountAddress, role: 1 },
          { address: SYSVAR_CLOCK_PUBKEY, role: 0 },
          { address: SYSVAR_STAKE_HISTORY_PUBKEY, role: 0 },
        ],
        data: this.encodeInitializeStakeInstruction(
          signer.address,
          signer.address,
        ),
      };

      const delegateInstruction = {
        programAddress: address(STAKE_PROGRAM_ID),
        accounts: [
          { address: stakeAccountAddress, role: 1 },
          { address: req.validator_vote_address, role: 0 },
          { address: SYSVAR_CLOCK_PUBKEY, role: 0 },
          { address: SYSVAR_STAKE_HISTORY_PUBKEY, role: 0 },
          { address: STAKE_CONFIG_ID, role: 0 },
          { address: signer.address, role: 2 },
        ],
        data: this.encodeDelegateStakeInstruction(),
      };

      const transactionMessage = pipe(
        createTransactionMessage({ version: 0 }),
        (tx) => setTransactionMessageFeePayerSigner(signer, tx),
        (tx) =>
          setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
        (tx) =>
          appendTransactionMessageInstructions(
            [
              createAccountInstruction,
              initializeInstruction,
              delegateInstruction,
            ],
            tx,
          ),
      );

      const signedTransaction =
        await signTransactionMessageWithSigners(transactionMessage);

      await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
        signedTransaction,
        { commitment: "confirmed" },
      );

      const signature = getSignatureFromTransaction(signedTransaction);
      const stakeId = this.generateStakeId();
      const createdAt = Date.now();

      walletDatabase.createStakeAccount({
        stake_id: stakeId,
        wallet_id: walletId,
        user_id: userId,
        stake_account_address: stakeAccountAddress,
        validator_vote_address: req.validator_vote_address,
        amount_lamports: req.amount_lamports,
        status: "delegated",
        signature,
        created_at: createdAt,
      });

      walletDatabase.updateStakeAccount(stakeId, {
        delegated_at: createdAt,
      });

      logger.info(
        `Stake account created and delegated: ${stakeAccountAddress}`,
      );

      return {
        stake_account_address: stakeAccountAddress,
        wallet_id: walletId,
        validator_vote_address: req.validator_vote_address,
        amount_lamports: req.amount_lamports,
        amount_sol: req.amount_lamports / LAMPORTS_PER_SOL,
        signature,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Create stake account failed:", error);
      throw error;
    }
  }

  async deactivateStake(
    signer: any,
    walletId: string,
    userId: string,
    req: DeactivateStakeRequest,
  ): Promise<DeactivateStakeResponse> {
    try {
      logger.info(`Deactivating stake account: ${req.stake_account_address}`);

      const stakeRecord = walletDatabase.getStakeAccountByAddress(
        req.stake_account_address,
      );

      if (!stakeRecord) {
        throw new Error("Stake account not found in database");
      }

      if (stakeRecord.wallet_id !== walletId) {
        throw new Error("Stake account does not belong to this wallet");
      }

      if (stakeRecord.status === "deactivated") {
        throw new Error("Stake account is already deactivated");
      }

      if (stakeRecord.status === "withdrawn") {
        throw new Error("Stake account has already been withdrawn");
      }

      const rpc = solanaConnection.getRpc();
      const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

      const { value: latestBlockhash } =
        await solanaConnection.getLatestBlockhash();

      const deactivateInstruction = {
        programAddress: address(STAKE_PROGRAM_ID),
        accounts: [
          { address: req.stake_account_address, role: 1 },
          { address: SYSVAR_CLOCK_PUBKEY, role: 0 },
          { address: signer.address, role: 2 },
        ],
        data: this.encodeDeactivateStakeInstruction(),
      };

      const transactionMessage = pipe(
        createTransactionMessage({ version: 0 }),
        (tx) => setTransactionMessageFeePayerSigner(signer, tx),
        (tx) =>
          setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
        (tx) =>
          appendTransactionMessageInstructions([deactivateInstruction], tx),
      );

      const signedTransaction =
        await signTransactionMessageWithSigners(transactionMessage);

      await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
        signedTransaction,
        { commitment: "confirmed" },
      );

      const signature = getSignatureFromTransaction(signedTransaction);
      const deactivatedAt = Date.now();

      walletDatabase.updateStakeAccount(stakeRecord.stake_id, {
        status: "deactivated",
        deactivated_at: deactivatedAt,
      });

      logger.info(`Stake account deactivated: ${req.stake_account_address}`);

      return {
        stake_account_address: req.stake_account_address,
        signature,
        deactivated_at: deactivatedAt,
      };
    } catch (error) {
      logger.error("Deactivate stake failed:", error);
      throw error;
    }
  }

  async withdrawStake(
    signer: any,
    walletId: string,
    userId: string,
    req: WithdrawStakeRequest,
  ): Promise<WithdrawStakeResponse> {
    try {
      logger.info(`Withdrawing stake account: ${req.stake_account_address}`);

      const stakeRecord = walletDatabase.getStakeAccountByAddress(
        req.stake_account_address,
      );

      if (!stakeRecord) {
        throw new Error("Stake account not found in database");
      }

      if (stakeRecord.wallet_id !== walletId) {
        throw new Error("Stake account does not belong to this wallet");
      }

      if (stakeRecord.status === "withdrawn") {
        throw new Error("Stake account has already been withdrawn");
      }

      if (stakeRecord.status === "delegated") {
        throw new Error(
          "Stake account must be deactivated before withdrawing. Wait for deactivation to complete (1 epoch).",
        );
      }

      const rpc = solanaConnection.getRpc();
      const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

      const { value: latestBlockhash } =
        await solanaConnection.getLatestBlockhash();

      const withdrawInstruction = {
        programAddress: address(STAKE_PROGRAM_ID),
        accounts: [
          { address: req.stake_account_address, role: 1 },
          { address: req.recipient_address, role: 1 },
          { address: SYSVAR_CLOCK_PUBKEY, role: 0 },
          { address: SYSVAR_STAKE_HISTORY_PUBKEY, role: 0 },
          { address: signer.address, role: 2 },
        ],
        data: this.encodeWithdrawStakeInstruction(req.amount_lamports),
      };

      const transactionMessage = pipe(
        createTransactionMessage({ version: 0 }),
        (tx) => setTransactionMessageFeePayerSigner(signer, tx),
        (tx) =>
          setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
        (tx) => appendTransactionMessageInstructions([withdrawInstruction], tx),
      );

      const signedTransaction =
        await signTransactionMessageWithSigners(transactionMessage);

      await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
        signedTransaction,
        { commitment: "confirmed" },
      );

      const signature = getSignatureFromTransaction(signedTransaction);
      const withdrawnAt = Date.now();

      walletDatabase.updateStakeAccount(stakeRecord.stake_id, {
        status: "withdrawn",
        withdrawn_at: withdrawnAt,
      });

      logger.info(`Stake withdrawn: ${req.stake_account_address}`);

      return {
        stake_account_address: req.stake_account_address,
        recipient_address: req.recipient_address,
        amount_lamports: req.amount_lamports,
        amount_sol: req.amount_lamports / LAMPORTS_PER_SOL,
        signature,
        withdrawn_at: withdrawnAt,
      };
    } catch (error) {
      logger.error("Withdraw stake failed:", error);
      throw error;
    }
  }

  async getStakeAccountInfo(
    stakeAccountAddress: string,
  ): Promise<StakeAccountInfo> {
    try {
      const rpc = solanaConnection.getRpc();

      const accountInfo = await rpc
        .getAccountInfo(address(stakeAccountAddress), {
          encoding: "base64",
        })
        .send();

      if (!accountInfo.value) {
        throw new Error("Stake account not found on chain");
      }

      const dbRecord =
        walletDatabase.getStakeAccountByAddress(stakeAccountAddress);

      let status = "unknown";
      let voter = "";
      let activatingEpoch: number | undefined;
      let activeEpoch: number | undefined;
      let deactivatingEpoch: number | undefined;
      let deactivatedEpoch: number | undefined;

      if (dbRecord) {
        status = dbRecord.status;
        voter = dbRecord.validator_vote_address;

        if (dbRecord.delegated_at) {
          activatingEpoch = Math.floor(dbRecord.delegated_at / 1000 / 432000);
          activeEpoch = activatingEpoch + 1;
        }

        if (dbRecord.deactivated_at) {
          deactivatingEpoch = Math.floor(
            dbRecord.deactivated_at / 1000 / 432000,
          );
          deactivatedEpoch = deactivatingEpoch + 1;
        }
      }

      const balanceLamports = Number(accountInfo.value.lamports);

      return {
        stake_account_address: stakeAccountAddress,
        balance_lamports: balanceLamports,
        balance_sol: balanceLamports / LAMPORTS_PER_SOL,
        status,
        voter,
        activating_epoch: activatingEpoch,
        active_epoch: activeEpoch,
        deactivating_epoch: deactivatingEpoch,
        deactivated_epoch: deactivatedEpoch,
      };
    } catch (error) {
      logger.error("Get stake account info failed:", error);
      throw error;
    }
  }

  async getValidators(): Promise<ValidatorInfo[]> {
    try {
      const rpc = solanaConnection.getRpc();

      const voteAccounts = await rpc.getVoteAccounts().send();

      const validators: ValidatorInfo[] = [];

      for (const validator of voteAccounts.current.slice(0, 50)) {
        validators.push({
          vote_address: validator.votePubkey,
          node_address: validator.nodePubkey,
          commission: validator.commission,
          last_vote: validator.lastVote,
          root_slot: validator.rootSlot,
          activated_stake: Number(validator.activatedStake),
          epoch_vote_account: true,
        });
      }

      for (const validator of voteAccounts.delinquent.slice(0, 10)) {
        validators.push({
          vote_address: validator.votePubkey,
          node_address: validator.nodePubkey,
          commission: validator.commission,
          last_vote: validator.lastVote,
          root_slot: validator.rootSlot,
          activated_stake: Number(validator.activatedStake),
          epoch_vote_account: false,
        });
      }

      return validators;
    } catch (error) {
      logger.error("Get validators failed:", error);
      throw error;
    }
  }

  async getStakingRewards(
    stakeAccountAddress: string,
    epochs: number = 5,
  ): Promise<StakingRewardsInfo> {
    try {
      const rpc = solanaConnection.getRpc();

      const epochInfo = await rpc.getEpochInfo().send();
      const currentEpoch = epochInfo.epoch;

      const rewards: any[] = [];
      let totalRewardsLamports = 0;

      for (let i = 0; i < epochs; i++) {
        const epoch = currentEpoch - i - 1;
        if (epoch < 0) break;

        try {
          const inflationRewards = await rpc
            .getInflationReward([address(stakeAccountAddress)], { epoch })
            .send();

          if (inflationRewards[0]) {
            const rewardLamports = Number(inflationRewards[0].amount);
            totalRewardsLamports += rewardLamports;

            rewards.push({
              epoch,
              amount_lamports: rewardLamports,
              amount_sol: rewardLamports / LAMPORTS_PER_SOL,
              post_balance: Number(inflationRewards[0].postBalance),
              commission: inflationRewards[0].commission ?? 0,
            });
          }
        } catch {
          rewards.push({
            epoch,
            amount_lamports: 0,
            amount_sol: 0,
            post_balance: 0,
            commission: 0,
          });
        }
      }

      return {
        stake_account_address: stakeAccountAddress,
        rewards,
        total_rewards_lamports: totalRewardsLamports,
        total_rewards_sol: totalRewardsLamports / LAMPORTS_PER_SOL,
      };
    } catch (error) {
      logger.error("Get staking rewards failed:", error);
      throw error;
    }
  }

  async getUserStakes(userId: string): Promise<any[]> {
    const stakes = walletDatabase.getUserStakeAccounts(userId);
    return stakes.map((s) => ({
      stake_id: s.stake_id,
      stake_account_address: s.stake_account_address,
      validator_vote_address: s.validator_vote_address,
      amount_lamports: s.amount_lamports,
      amount_sol: s.amount_lamports / LAMPORTS_PER_SOL,
      status: s.status,
      created_at: s.created_at,
      delegated_at: s.delegated_at,
      deactivated_at: s.deactivated_at,
      withdrawn_at: s.withdrawn_at,
      signature: s.signature,
    }));
  }

  async getWalletStakes(walletId: string): Promise<any[]> {
    const stakes = walletDatabase.getWalletStakeAccounts(walletId);
    return stakes.map((s) => ({
      stake_id: s.stake_id,
      stake_account_address: s.stake_account_address,
      validator_vote_address: s.validator_vote_address,
      amount_lamports: s.amount_lamports,
      amount_sol: s.amount_lamports / LAMPORTS_PER_SOL,
      status: s.status,
      created_at: s.created_at,
      delegated_at: s.delegated_at,
      deactivated_at: s.deactivated_at,
      withdrawn_at: s.withdrawn_at,
      signature: s.signature,
    }));
  }

  private encodeCreateAccountInstruction(
    lamportsAmount: number,
    space: number,
    programId: string,
  ): Uint8Array {
    const buffer = new ArrayBuffer(52);
    const view = new DataView(buffer);
    view.setUint32(0, 0, true);
    view.setBigUint64(4, BigInt(lamportsAmount), true);
    view.setBigUint64(12, BigInt(space), true);
    const programIdBytes = this.decodeBase58(programId);
    new Uint8Array(buffer).set(programIdBytes, 20);
    return new Uint8Array(buffer);
  }

  private encodeInitializeStakeInstruction(
    stakerAddress: string,
    withdrawerAddress: string,
  ): Uint8Array {
    const buffer = new ArrayBuffer(100);
    const view = new DataView(buffer);
    view.setUint32(0, 0, true);
    const stakerBytes = this.decodeBase58(stakerAddress);
    const withdrawerBytes = this.decodeBase58(withdrawerAddress);
    new Uint8Array(buffer).set(stakerBytes, 4);
    new Uint8Array(buffer).set(withdrawerBytes, 36);
    return new Uint8Array(buffer);
  }

  private encodeDelegateStakeInstruction(): Uint8Array {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    view.setUint32(0, 2, true);
    return new Uint8Array(buffer);
  }

  private encodeDeactivateStakeInstruction(): Uint8Array {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    view.setUint32(0, 5, true);
    return new Uint8Array(buffer);
  }

  private encodeWithdrawStakeInstruction(lamportsAmount: number): Uint8Array {
    const buffer = new ArrayBuffer(12);
    const view = new DataView(buffer);
    view.setUint32(0, 4, true);
    view.setBigUint64(4, BigInt(lamportsAmount), true);
    return new Uint8Array(buffer);
  }

  private decodeBase58(encoded: string): Uint8Array {
    const ALPHABET =
      "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    const bytes = [0];

    for (const char of encoded) {
      let carry = ALPHABET.indexOf(char);
      if (carry < 0) throw new Error("Invalid base58 character");

      for (let i = 0; i < bytes.length; i++) {
        carry += bytes[i] * 58;
        bytes[i] = carry & 0xff;
        carry >>= 8;
      }

      while (carry > 0) {
        bytes.push(carry & 0xff);
        carry >>= 8;
      }
    }

    for (const char of encoded) {
      if (char === "1") bytes.push(0);
      else break;
    }

    return new Uint8Array(bytes.reverse());
  }
}

export const stakingManager = new StakingManager();
