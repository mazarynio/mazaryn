import { Transaction } from "@mysten/sui/transactions";
import { bcs } from "@mysten/sui/bcs";
import { logger } from "../../core/logger.js";
import { suiDatabase } from "../database/sui-database.js";
import {
  getSuiGrpcClient,
  getSuiEpoch,
  listOwnedObjects,
  normalizeSuiAddress,
  getReferenceGasPrice,
} from "./sui-client.js";
import { suiWalletManager } from "./sui-wallet.js";
import type {
  SuiNetwork,
  SuiTxType,
  SuiGasSummary,
  SuiStakeRequest,
  SuiStakeResponse,
  SuiUnstakeRequest,
  SuiUnstakeResponse,
  SuiGetStakedSuiRequest,
  SuiStakedSuiObject,
  SuiGetStakingRewardsRequest,
  SuiStakingRewardsSummary,
  SuiGetValidatorsRequest,
  SuiValidatorInfo,
  SuiEpochInfo,
  SuiSystemStateSummary,
} from "../../core/sui-types.js";

const DEFAULT_GAS_BUDGET = "50000000";

const SUI_SYSTEM_STATE_OBJECT_ID =
  "0x0000000000000000000000000000000000000000000000000000000000000005";
const SUI_SYSTEM_PACKAGE_ID =
  "0x0000000000000000000000000000000000000000000000000000000000000003";
const STAKED_SUI_TYPE = "0x3::staking_pool::StakedSui";

function generateTxId(): string {
  return `sui_tx_${Date.now()}_${Math.random().toString(36).substring(7)}`;
}

function extractGasSummary(gasUsed: unknown): SuiGasSummary {
  const g = (gasUsed ?? {}) as Record<string, unknown>;
  return {
    computation_cost: String(
      g["computationCost"] ?? g["computation_cost"] ?? 0,
    ),
    storage_cost: String(g["storageCost"] ?? g["storage_cost"] ?? 0),
    storage_rebate: String(g["storageRebate"] ?? g["storage_rebate"] ?? 0),
    non_refundable_storage_fee: String(
      g["nonRefundableStorageFee"] ?? g["non_refundable_storage_fee"] ?? 0,
    ),
  };
}

export async function getValidators(
  req: SuiGetValidatorsRequest = {},
): Promise<SuiValidatorInfo[]> {
  try {
    const network = req.network ?? "mainnet";
    const epochData = await getSuiEpoch(undefined, network);

    if (!epochData) {
      logger.warn("No epoch data returned for validators query");
      return [];
    }

    const systemState = epochData.systemState as
      | Record<string, unknown>
      | undefined;
    const validatorSet = systemState?.["validators"] as
      | Record<string, unknown>
      | undefined;
    const activeValidators =
      (validatorSet?.["activeValidators"] as unknown[]) ?? [];
    const totalStake = String(validatorSet?.["totalStake"] ?? 0);

    return activeValidators.map((v) => {
      const validator = v as Record<string, unknown>;
      const stakingPool = validator["stakingPool"] as
        | Record<string, unknown>
        | undefined;
      const suiBalance = String(stakingPool?.["suiBalance"] ?? 0);
      const pendingStake = String(stakingPool?.["pendingStake"] ?? 0);
      const poolTokenBalance = String(stakingPool?.["poolTokenBalance"] ?? 0);
      const votingPower = String(validator["votingPower"] ?? 0);
      const totalStakeBigInt = BigInt(totalStake || 0);
      const validatorStakeBigInt = BigInt(suiBalance || 0);

      let apy: number | undefined;
      if (totalStakeBigInt > 0n) {
        apy = Number((validatorStakeBigInt * 10000n) / totalStakeBigInt) / 100;
      }

      return {
        address: String(validator["address"] ?? ""),
        name: String(validator["name"] ?? ""),
        description: (validator["description"] as string) ?? undefined,
        image_url: (validator["imageUrl"] as string) ?? undefined,
        project_url: (validator["projectUrl"] as string) ?? undefined,
        staking_pool_id: String(stakingPool?.["id"] ?? ""),
        sui_balance: suiBalance,
        pending_stake: pendingStake,
        pool_token_balance: poolTokenBalance,
        voting_power: votingPower,
        commission_rate: String(validator["commissionRate"] ?? 0),
        gas_price: String(validator["gasPrice"] ?? 0),
        next_epoch_stake: String(validator["nextEpochStake"] ?? 0),
        next_epoch_commission_rate: String(
          validator["nextEpochCommissionRate"] ?? 0,
        ),
        apy,
      };
    });
  } catch (error) {
    logger.error("Failed to get Sui validators:", error);
    throw error;
  }
}

export async function getValidatorByAddress(
  validatorAddress: string,
  network: SuiNetwork = "mainnet",
): Promise<SuiValidatorInfo | null> {
  try {
    const validators = await getValidators({ network });
    return (
      validators.find(
        (v) =>
          normalizeSuiAddress(v.address) ===
          normalizeSuiAddress(validatorAddress),
      ) ?? null
    );
  } catch (error) {
    logger.error("Failed to get Sui validator by address:", error);
    throw error;
  }
}

export async function getCurrentEpoch(
  network: SuiNetwork = "mainnet",
): Promise<SuiEpochInfo> {
  try {
    const epochData = await getSuiEpoch(undefined, network);

    if (!epochData) {
      throw new Error("Failed to fetch epoch data");
    }

    const systemState = epochData.systemState as
      | Record<string, unknown>
      | undefined;
    const validatorSet = systemState?.["validators"] as
      | Record<string, unknown>
      | undefined;
    const activeValidators =
      (validatorSet?.["activeValidators"] as unknown[]) ?? [];

    const summary: SuiSystemStateSummary = {
      epoch: String(epochData.epoch ?? 0),
      protocol_version: String(systemState?.["protocolVersion"] ?? 0),
      reference_gas_price: String(epochData.referenceGasPrice ?? 0),
      epoch_start_timestamp_ms: String(
        systemState?.["epochStartTimestampMs"] ?? 0,
      ),
      safe_mode: Boolean(systemState?.["safeMode"] ?? false),
      total_stake: String(validatorSet?.["totalStake"] ?? 0),
      active_validators_count: activeValidators.length,
    };

    return {
      epoch: String(epochData.epoch ?? 0),
      reference_gas_price: String(epochData.referenceGasPrice ?? 0),
      start: epochData.start
        ? Number((epochData.start as Record<string, unknown>)["seconds"]) * 1000
        : undefined,
      end: epochData.end
        ? Number((epochData.end as Record<string, unknown>)["seconds"]) * 1000
        : undefined,
      first_checkpoint: String(epochData.firstCheckpoint ?? ""),
      last_checkpoint: String(epochData.lastCheckpoint ?? ""),
      protocol_version: String(epochData.protocolConfig?.protocolVersion ?? 0),
      system_state: summary,
    };
  } catch (error) {
    logger.error("Failed to get current Sui epoch:", error);
    throw error;
  }
}

export async function stakeWithValidator(
  req: SuiStakeRequest,
): Promise<SuiStakeResponse> {
  try {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = suiWalletManager.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const validatorAddress = normalizeSuiAddress(req.validator_address);
    const amountMist = BigInt(req.amount_mist);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const [stakeCoin] = tx.splitCoins(tx.gas, [amountMist]);

    tx.moveCall({
      target: `${SUI_SYSTEM_PACKAGE_ID}::sui_system::request_add_stake`,
      arguments: [
        tx.object(SUI_SYSTEM_STATE_OBJECT_ID),
        stakeCoin,
        tx.pure(bcs.Address.serialize(validatorAddress)),
      ],
    });

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Staking failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const createdObjects = (txData?.effects?.created ?? []) as Array<{
      reference?: { objectId?: string };
    }>;
    const stakedSuiId =
      createdObjects.find(() => true)?.reference?.objectId ?? "";

    const epochData = await getSuiEpoch(undefined, network);
    const currentEpoch = String(epochData?.epoch ?? 0);

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "stake" as SuiTxType,
      sender_address: senderAddress,
      validator_address: validatorAddress,
      staked_sui_id: stakedSuiId,
      amount_mist: amountMist.toString(),
      gas_computation: gas.computation_cost,
      gas_storage: gas.storage_cost,
      gas_storage_rebate: gas.storage_rebate,
      status: "success",
      effects_status: "success",
      epoch: currentEpoch,
      checkpoint: txData?.checkpoint ? String(txData.checkpoint) : undefined,
      network,
      created_at: Date.now(),
    };
    suiDatabase.createTransaction(record);
    suiDatabase.updateWalletLastUsed(wallet.wallet_id);

    return {
      digest: txData?.digest ?? "",
      sender_address: senderAddress,
      validator_address: validatorAddress,
      staked_sui_id: stakedSuiId,
      amount_mist: amountMist.toString(),
      gas_used: gas,
      status: "success",
      epoch: currentEpoch,
    };
  } catch (error) {
    logger.error("Failed to stake Sui:", error);
    throw error;
  }
}

export async function unstakeFromValidator(
  req: SuiUnstakeRequest,
): Promise<SuiUnstakeResponse> {
  try {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = suiWalletManager.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const stakedSuiId = normalizeSuiAddress(req.staked_sui_id);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    tx.moveCall({
      target: `${SUI_SYSTEM_PACKAGE_ID}::sui_system::request_withdraw_stake`,
      arguments: [
        tx.object(SUI_SYSTEM_STATE_OBJECT_ID),
        tx.object(stakedSuiId),
      ],
    });

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Unstaking failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const balanceChanges = (txData as Record<string, unknown>)?.[
      "balanceChanges"
    ] as Array<{ address?: string; amount?: string }> | undefined;

    let principalAmount = "0";
    let rewardAmount: string | undefined;

    if (balanceChanges) {
      const positiveChange = balanceChanges.find(
        (bc) =>
          bc.address &&
          normalizeSuiAddress(bc.address) === senderAddress &&
          BigInt(bc.amount ?? 0) > 0n,
      );
      if (positiveChange?.amount) {
        principalAmount = positiveChange.amount;
        rewardAmount = undefined;
      }
    }

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "unstake" as SuiTxType,
      sender_address: senderAddress,
      staked_sui_id: stakedSuiId,
      amount_mist: principalAmount,
      gas_computation: gas.computation_cost,
      gas_storage: gas.storage_cost,
      gas_storage_rebate: gas.storage_rebate,
      status: "success",
      effects_status: "success",
      checkpoint: txData?.checkpoint ? String(txData.checkpoint) : undefined,
      network,
      created_at: Date.now(),
    };
    suiDatabase.createTransaction(record);
    suiDatabase.updateWalletLastUsed(wallet.wallet_id);

    return {
      digest: txData?.digest ?? "",
      sender_address: senderAddress,
      staked_sui_id: stakedSuiId,
      principal_amount: principalAmount,
      reward_amount: rewardAmount,
      gas_used: gas,
      status: "success",
    };
  } catch (error) {
    logger.error("Failed to unstake Sui:", error);
    throw error;
  }
}

export async function getStakedSui(
  req: SuiGetStakedSuiRequest,
): Promise<SuiStakedSuiObject[]> {
  try {
    const ownerAddress = normalizeSuiAddress(req.owner_address);
    const network = req.network ?? "mainnet";

    const result = await listOwnedObjects(ownerAddress, network, {
      objectType: STAKED_SUI_TYPE,
      pageSize: 50,
    });

    const epochData = await getSuiEpoch(undefined, network);
    const currentEpoch = BigInt(epochData?.epoch ?? 0);

    return (result.objects ?? []).map((obj) => {
      const content = obj.json as Record<string, unknown> | undefined;
      const fields = content?.["fields"] as Record<string, unknown> | undefined;

      const principal = String(
        (fields?.["principal"] as Record<string, unknown>)?.["value"] ??
          fields?.["principal"] ??
          0,
      );
      const stakeActivationEpoch = String(
        fields?.["stakeActivationEpoch"] ?? 0,
      );
      const activationEpochBigInt = BigInt(stakeActivationEpoch);

      let status: "Active" | "Pending" | "Unstaked" = "Pending";
      if (activationEpochBigInt <= currentEpoch) {
        status = "Active";
      }

      const validatorAddress = String(
        fields?.["validatorAddress"] ?? fields?.["validator_address"] ?? "",
      );

      return {
        staked_sui_id: String(obj.objectId ?? ""),
        validator_address: validatorAddress,
        principal,
        stake_activation_epoch: stakeActivationEpoch,
        status,
      };
    });
  } catch (error) {
    logger.error("Failed to get staked Sui:", error);
    throw error;
  }
}

export async function getStakingRewards(
  req: SuiGetStakingRewardsRequest,
): Promise<SuiStakingRewardsSummary> {
  try {
    const ownerAddress = normalizeSuiAddress(req.owner_address);
    const network = req.network ?? "mainnet";

    const stakes = await getStakedSui({ owner_address: ownerAddress, network });

    const epochData = await getSuiEpoch(undefined, network);
    const referenceGasPrice = BigInt(epochData?.referenceGasPrice ?? 0);

    let totalStaked = 0n;
    let totalRewards = 0n;

    const enrichedStakes = await Promise.all(
      stakes.map(async (stake) => {
        const principal = BigInt(stake.principal);
        totalStaked += principal;

        let estimatedReward: string | undefined;
        if (stake.status === "Active") {
          const baseReward = (principal * referenceGasPrice) / 1_000_000n;
          estimatedReward = baseReward.toString();
          totalRewards += baseReward;
        }

        return { ...stake, estimated_reward: estimatedReward };
      }),
    );

    return {
      owner_address: ownerAddress,
      total_staked: totalStaked.toString(),
      total_rewards: totalRewards.toString(),
      stakes: enrichedStakes,
    };
  } catch (error) {
    logger.error("Failed to get Sui staking rewards:", error);
    throw error;
  }
}

export async function requestAddStakeWithMultipleCoins(
  senderAddress: string,
  validatorAddress: string,
  coinObjectIds: string[],
  network: SuiNetwork = "mainnet",
  gasBudget = DEFAULT_GAS_BUDGET,
): Promise<SuiStakeResponse> {
  try {
    const normalizedSender = normalizeSuiAddress(senderAddress);
    const wallet = suiDatabase.getWalletByAddress(normalizedSender);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${normalizedSender}`);

    const keypair = suiWalletManager.getKeypairByAddress(normalizedSender);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);
    const normalizedValidator = normalizeSuiAddress(validatorAddress);

    const tx = new Transaction();
    tx.setSender(normalizedSender);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(gasBudget));

    if (coinObjectIds.length > 1) {
      tx.mergeCoins(
        tx.object(coinObjectIds[0]),
        coinObjectIds.slice(1).map((id) => tx.object(id)),
      );
    }

    tx.moveCall({
      target: `${SUI_SYSTEM_PACKAGE_ID}::sui_system::request_add_stake`,
      arguments: [
        tx.object(SUI_SYSTEM_STATE_OBJECT_ID),
        tx.object(coinObjectIds[0]),
        tx.pure(bcs.Address.serialize(normalizedValidator)),
      ],
    });

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Multi-coin staking failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const createdObjects = (txData?.effects?.created ?? []) as Array<{
      reference?: { objectId?: string };
    }>;
    const stakedSuiId =
      createdObjects.find(() => true)?.reference?.objectId ?? "";

    const epochData = await getSuiEpoch(undefined, network);
    const currentEpoch = String(epochData?.epoch ?? 0);

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "stake" as SuiTxType,
      sender_address: normalizedSender,
      validator_address: normalizedValidator,
      staked_sui_id: stakedSuiId,
      gas_computation: gas.computation_cost,
      gas_storage: gas.storage_cost,
      gas_storage_rebate: gas.storage_rebate,
      status: "success",
      effects_status: "success",
      epoch: currentEpoch,
      checkpoint: txData?.checkpoint ? String(txData.checkpoint) : undefined,
      network,
      created_at: Date.now(),
    };
    suiDatabase.createTransaction(record);

    return {
      digest: txData?.digest ?? "",
      sender_address: normalizedSender,
      validator_address: normalizedValidator,
      staked_sui_id: stakedSuiId,
      amount_mist: "0",
      gas_used: gas,
      status: "success",
      epoch: currentEpoch,
    };
  } catch (error) {
    logger.error("Failed to stake Sui with multiple coins:", error);
    throw error;
  }
}

export function getLocalStakingHistory(
  walletId: string,
  limit = 50,
): import("../../core/sui-types.js").SuiTransactionRecord[] {
  return suiDatabase.getTransactionsByType(
    walletId,
    "stake",
    limit,
  ) as import("../../core/sui-types.js").SuiTransactionRecord[];
}

export function getLocalUnstakingHistory(
  walletId: string,
  limit = 50,
): import("../../core/sui-types.js").SuiTransactionRecord[] {
  return suiDatabase.getTransactionsByType(
    walletId,
    "unstake",
    limit,
  ) as import("../../core/sui-types.js").SuiTransactionRecord[];
}
