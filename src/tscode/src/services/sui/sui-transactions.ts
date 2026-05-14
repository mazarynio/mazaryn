import { Transaction } from "@mysten/sui/transactions";
import { bcs } from "@mysten/sui/bcs";
import { logger } from "../../core/logger.js";
import { suiDatabase } from "../database/sui-database.js";
import {
  getSuiGrpcClient,
  getSuiTransaction,
  batchGetSuiTransactions,
  executeTransaction,
  simulateTransaction,
  normalizeSuiAddress,
  getReferenceGasPrice,
} from "./sui-client.js";
import { suiWalletManager } from "./sui-wallet.js";
import type {
  SuiNetwork,
  SuiTxType,
  SuiTransactionResult,
  SuiTransactionEffects,
  SuiGasSummary,
  SuiEvent,
  SuiObjectRef,
  SuiGetTransactionRequest,
  SuiExecuteTransactionRequest,
  SuiSimulateTransactionRequest,
  SuiSimulateTransactionResponse,
  SuiWaitForTransactionRequest,
  SuiMoveCallRequest,
  SuiMoveCallResponse,
  SuiPublishPackageRequest,
  SuiPublishPackageResponse,
  SuiUpgradePackageRequest,
  SuiBuildTransactionRequest,
  SuiBuildTransactionResponse,
  SuiLocalTransactionHistoryRequest,
  SuiLocalTransactionHistoryResponse,
  SuiTransactionRecord,
} from "../../core/sui-types.js";

const DEFAULT_GAS_BUDGET = "50000000";
const DEFAULT_WAIT_TIMEOUT_MS = 60_000;

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

function extractEffects(effects: unknown): SuiTransactionEffects | undefined {
  if (!effects || typeof effects !== "object") return undefined;
  const e = effects as Record<string, unknown>;
  const status = e["status"] as Record<string, unknown> | undefined;
  return {
    status: {
      success: (status?.["success"] as boolean) ?? true,
      error: (status?.["error"] as string) ?? undefined,
    },
    gas_used: extractGasSummary(e["gasUsed"] ?? e["gas_used"]),
    transaction_digest: String(
      e["transactionDigest"] ?? e["transaction_digest"] ?? "",
    ),
    created: (
      (e["created"] as Array<{
        reference?: Record<string, unknown>;
        owner?: unknown;
      }>) ?? []
    ).map((c) => ({
      reference: {
        object_id: String(c.reference?.["objectId"] ?? ""),
        version: String(c.reference?.["version"] ?? 0),
        digest: String(c.reference?.["digest"] ?? ""),
      },
      owner: c.owner as SuiObjectRef,
    })),
    mutated: (
      (e["mutated"] as Array<{
        reference?: Record<string, unknown>;
        owner?: unknown;
      }>) ?? []
    ).map((m) => ({
      reference: {
        object_id: String(m.reference?.["objectId"] ?? ""),
        version: String(m.reference?.["version"] ?? 0),
        digest: String(m.reference?.["digest"] ?? ""),
      },
      owner: m.owner as SuiObjectRef,
    })),
    deleted: ((e["deleted"] as Array<Record<string, unknown>>) ?? []).map(
      (d) => ({
        object_id: String(d["objectId"] ?? ""),
        version: String(d["version"] ?? 0),
        digest: String(d["digest"] ?? ""),
      }),
    ),
    events_digest: (e["eventsDigest"] ?? e["events_digest"]) as
      | string
      | undefined,
    dependencies: (e["dependencies"] as string[]) ?? [],
  };
}

function extractEvents(events: unknown): SuiEvent[] {
  if (!Array.isArray(events)) return [];
  return events.map((ev: Record<string, unknown>) => ({
    package_id: String(ev["packageId"] ?? ev["package_id"] ?? ""),
    module: String(ev["module"] ?? ""),
    sender: String(ev["sender"] ?? ""),
    event_type: String(ev["eventType"] ?? ev["type"] ?? ""),
    parsed_json: (ev["parsedJson"] ?? ev["json"]) as
      | Record<string, unknown>
      | undefined,
    timestamp: ev["timestampMs"] ? Number(ev["timestampMs"]) : undefined,
  }));
}

function generateTxId(): string {
  return `sui_tx_${Date.now()}_${Math.random().toString(36).substring(7)}`;
}

export async function getTransaction(
  req: SuiGetTransactionRequest,
): Promise<SuiTransactionResult> {
  try {
    const network = req.network ?? "mainnet";
    const txData = await getSuiTransaction(req.digest, network, {
      effects: req.include_effects !== false,
      events: req.include_events ?? false,
      bcs: req.include_bcs ?? false,
    });

    if (!txData) {
      throw new Error(`Transaction not found: ${req.digest}`);
    }

    const effects = txData.effects;
    const gasUsed = effects?.gasUsed;
    const statusSuccess = effects?.status?.success ?? true;

    return {
      digest: txData.digest ?? req.digest,
      status: statusSuccess ? "success" : "failure",
      sender: txData.transaction?.sender ?? undefined,
      gas_used: extractGasSummary(gasUsed),
      effects: extractEffects(effects),
      events: extractEvents(txData.events?.events),
      timestamp: txData.timestamp
        ? Number(txData.timestamp.seconds) * 1000
        : undefined,
      checkpoint: txData.checkpoint ? String(txData.checkpoint) : undefined,
      error: statusSuccess
        ? undefined
        : String(effects?.status?.error ?? "Unknown error"),
    };
  } catch (error) {
    logger.error("Failed to get Sui transaction:", error);
    throw error;
  }
}

export async function batchGetTransactions(
  digests: string[],
  network: SuiNetwork = "mainnet",
): Promise<SuiTransactionResult[]> {
  try {
    const txList = await batchGetSuiTransactions(digests, network);
    return txList.map((item) => {
      const tx = (item as Record<string, unknown>)["transaction"] as
        | Record<string, unknown>
        | undefined;
      if (!tx) {
        return { digest: "", status: "failure" as const, error: "Not found" };
      }
      const effects = tx["effects"] as Record<string, unknown> | undefined;
      const gasUsed = effects?.["gasUsed"];
      const statusSuccess =
        (effects?.["status"] as Record<string, unknown>)?.["success"] ?? true;
      return {
        digest: String(tx["digest"] ?? ""),
        status: statusSuccess ? ("success" as const) : ("failure" as const),
        sender: (tx["transaction"] as Record<string, unknown>)?.["sender"] as
          | string
          | undefined,
        gas_used: extractGasSummary(gasUsed),
        effects: extractEffects(effects),
        checkpoint: tx["checkpoint"] ? String(tx["checkpoint"]) : undefined,
        timestamp: (tx["timestamp"] as { seconds?: number })?.seconds
          ? Number((tx["timestamp"] as { seconds?: number }).seconds) * 1000
          : undefined,
      };
    });
  } catch (error) {
    logger.error("Failed to batch get Sui transactions:", error);
    throw error;
  }
}

export async function executeRawTransaction(
  req: SuiExecuteTransactionRequest,
): Promise<SuiTransactionResult> {
  try {
    const network = req.network ?? "mainnet";

    const txBytes =
      typeof req.transaction_bytes === "string"
        ? Buffer.from(req.transaction_bytes, "base64")
        : req.transaction_bytes;

    const sigBytes = req.signatures.map((s) => Buffer.from(s, "base64"));

    const response = await executeTransaction(txBytes, sigBytes, network, {
      effects: req.include_effects !== false,
      events: req.include_events ?? false,
    });

    const finality = response.finality as Record<string, unknown> | undefined;
    const effects = finality?.["effects"] as
      | Record<string, unknown>
      | undefined;
    const statusSuccess =
      (effects?.["status"] as Record<string, unknown>)?.["success"] ?? true;

    return {
      digest: String(finality?.["digest"] ?? ""),
      status: statusSuccess ? "success" : "failure",
      gas_used: extractGasSummary(effects?.["gasUsed"]),
      effects: extractEffects(effects),
      error: statusSuccess
        ? undefined
        : String(
            (effects?.["status"] as Record<string, unknown>)?.["error"] ?? "",
          ),
    };
  } catch (error) {
    logger.error("Failed to execute raw Sui transaction:", error);
    throw error;
  }
}

export async function simulateSuiTransaction(
  req: SuiSimulateTransactionRequest,
): Promise<SuiSimulateTransactionResponse> {
  try {
    const network = req.network ?? "mainnet";

    const txBytes =
      typeof req.transaction_bytes === "string"
        ? Buffer.from(req.transaction_bytes, "base64")
        : req.transaction_bytes;

    const response = await simulateTransaction(txBytes, network, {
      skipChecks: req.skip_checks ?? false,
      doGasSelection: req.include_balance_changes ?? false,
    });

    const tx = response.transaction as Record<string, unknown> | undefined;
    const effects = tx?.["effects"] as Record<string, unknown> | undefined;
    const statusSuccess =
      (effects?.["status"] as Record<string, unknown>)?.["success"] ?? true;

    const balanceChanges = (response.transaction as Record<string, unknown>)?.[
      "balanceChanges"
    ] as
      | Array<{ address?: string; coinType?: string; amount?: string }>
      | undefined;

    return {
      status: statusSuccess ? "success" : "failure",
      effects: extractEffects(effects),
      balance_changes: balanceChanges?.map((bc) => ({
        address: bc.address ?? "",
        coin_type: bc.coinType ?? "",
        amount: bc.amount ?? "0",
      })),
      gas_used: extractGasSummary(effects?.["gasUsed"]),
      error: statusSuccess
        ? undefined
        : String(
            (effects?.["status"] as Record<string, unknown>)?.["error"] ?? "",
          ),
    };
  } catch (error) {
    logger.error("Failed to simulate Sui transaction:", error);
    throw error;
  }
}

export async function waitForTransaction(
  req: SuiWaitForTransactionRequest,
): Promise<SuiTransactionResult> {
  try {
    const network = req.network ?? "mainnet";
    const client = getSuiGrpcClient(network);

    const result = await client.core.waitForTransaction({
      digest: req.digest,
      timeout: req.timeout_ms ?? DEFAULT_WAIT_TIMEOUT_MS,
      include: { effects: req.include_effects !== false },
    });

    const txData = result.Transaction ?? result.FailedTransaction;
    const effects = txData?.effects;
    const gasUsed = effects?.gasUsed;
    const statusSuccess = result.FailedTransaction === undefined;

    return {
      digest: txData?.digest ?? req.digest,
      status: statusSuccess ? "success" : "failure",
      gas_used: extractGasSummary(gasUsed),
      effects: extractEffects(effects),
      checkpoint: txData?.checkpoint ? String(txData.checkpoint) : undefined,
      error: statusSuccess
        ? undefined
        : String(result.FailedTransaction?.status?.error ?? ""),
    };
  } catch (error) {
    logger.error("Failed to wait for Sui transaction:", error);
    throw error;
  }
}

export async function buildTransaction(
  req: SuiBuildTransactionRequest,
): Promise<SuiBuildTransactionResponse> {
  try {
    const network = req.network ?? "mainnet";
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(normalizeSuiAddress(req.sender_address));
    tx.setGasPrice(BigInt(req.gas_price ?? gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    if (req.expiration_epoch) {
      tx.setExpiration({ Epoch: Number(req.expiration_epoch) });
    }

    switch (req.transaction_kind) {
      case "transfer": {
        const p = req.params as {
          recipient: string;
          amount: string;
          coin_object_id?: string;
        };
        if (p.coin_object_id) {
          tx.transferObjects(
            [tx.object(p.coin_object_id)],
            normalizeSuiAddress(p.recipient),
          );
        } else {
          const [coin] = tx.splitCoins(tx.gas, [BigInt(p.amount)]);
          tx.transferObjects([coin], normalizeSuiAddress(p.recipient));
        }
        break;
      }
      case "move_call": {
        const p = req.params as {
          package_id: string;
          module_name: string;
          function_name: string;
          type_arguments?: string[];
          arguments?: unknown[];
        };
        tx.moveCall({
          target: `${normalizeSuiAddress(p.package_id)}::${p.module_name}::${p.function_name}`,
          typeArguments: p.type_arguments ?? [],
          arguments: (p.arguments ?? []).map((arg) =>
            typeof arg === "string" && arg.startsWith("0x")
              ? tx.object(arg)
              : tx.pure(bcs.U64.serialize(BigInt(String(arg)))),
          ),
        });
        break;
      }
      case "split_coin": {
        const p = req.params as { coin_object_id: string; amounts: string[] };
        const newCoins = tx.splitCoins(
          tx.object(p.coin_object_id),
          p.amounts.map((a) => BigInt(a)),
        );
        tx.transferObjects(
          p.amounts.map((_, i) => newCoins[i]),
          normalizeSuiAddress(req.sender_address),
        );
        break;
      }
      case "merge_coins": {
        const p = req.params as {
          primary_coin_id: string;
          coins_to_merge: string[];
        };
        tx.mergeCoins(
          tx.object(p.primary_coin_id),
          p.coins_to_merge.map((id) => tx.object(id)),
        );
        break;
      }
      case "publish": {
        const p = req.params as {
          compiled_modules: string[];
          dependencies: string[];
        };
        const [upgradeCap] = tx.publish({
          modules: p.compiled_modules.map((m) =>
            Array.from(Buffer.from(m, "base64")),
          ),
          dependencies: p.dependencies,
        });
        tx.transferObjects(
          [upgradeCap],
          normalizeSuiAddress(req.sender_address),
        );
        break;
      }
      case "upgrade": {
        const p = req.params as {
          package_id: string;
          compiled_modules: string[];
          dependencies: string[];
          upgrade_cap_id: string;
        };
        const ticket = tx.moveCall({
          target: "0x2::package::authorize_upgrade",
          arguments: [
            tx.object(p.upgrade_cap_id),
            tx.pure(bcs.U8.serialize(0)),
            tx.pure(bcs.vector(bcs.U8).serialize([])),
          ],
        });
        const receipt = tx.upgrade({
          modules: p.compiled_modules.map((m) =>
            Array.from(Buffer.from(m, "base64")),
          ),
          dependencies: p.dependencies,
          package: normalizeSuiAddress(p.package_id),
          ticket,
        });
        tx.moveCall({
          target: "0x2::package::commit_upgrade",
          arguments: [tx.object(p.upgrade_cap_id), receipt],
        });
        break;
      }
      default:
        throw new Error(`Unknown transaction kind: ${req.transaction_kind}`);
    }

    const builtBytes = await tx.build({
      client: client as Parameters<typeof tx.build>[0]["client"],
    });
    const digest = await tx.getDigest({
      client: client as Parameters<typeof tx.getDigest>[0]["client"],
    });

    const simulateResult = await simulateTransaction(builtBytes, network, {
      doGasSelection: true,
    });
    const simEffects = (
      simulateResult.transaction as Record<string, unknown>
    )?.["effects"] as Record<string, unknown> | undefined;
    const gasEstimate = String(
      (simEffects?.["gasUsed"] as Record<string, unknown>)?.[
        "computationCost"
      ] ?? DEFAULT_GAS_BUDGET,
    );

    return {
      transaction_bytes: Buffer.from(builtBytes).toString("base64"),
      digest_to_sign: digest,
      gas_estimate: gasEstimate,
    };
  } catch (error) {
    logger.error("Failed to build Sui transaction:", error);
    throw error;
  }
}

export async function executeMoveCall(
  req: SuiMoveCallRequest,
): Promise<SuiMoveCallResponse> {
  try {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = suiWalletManager.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const target =
      `${normalizeSuiAddress(req.package_id)}::${req.module_name}::${req.function_name}` as `${string}::${string}::${string}`;

    const resolvedArgs = (req.arguments ?? []).map((arg) => {
      if (typeof arg === "string" && arg.startsWith("0x"))
        return tx.object(arg);
      if (typeof arg === "bigint") return tx.pure(bcs.U64.serialize(arg));
      if (typeof arg === "number")
        return tx.pure(bcs.U64.serialize(BigInt(arg)));
      if (typeof arg === "string") return tx.pure(bcs.String.serialize(arg));
      if (typeof arg === "boolean") return tx.pure(bcs.Bool.serialize(arg));
      return tx.pure(bcs.U64.serialize(BigInt(String(arg))));
    });

    tx.moveCall({
      target,
      typeArguments: req.type_arguments ?? [],
      arguments: resolvedArgs,
    });

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true, events: true },
    });

    if (result.FailedTransaction) {
      const errMsg =
        result.FailedTransaction.status.error?.message ?? "Move call failed";
      const record = {
        tx_id: generateTxId(),
        wallet_id: wallet.wallet_id,
        user_id: wallet.user_id,
        digest: result.FailedTransaction.digest ?? "",
        tx_type: "move_call" as SuiTxType,
        sender_address: senderAddress,
        package_id: req.package_id,
        module_name: req.module_name,
        function_name: req.function_name,
        gas_computation: "0",
        gas_storage: "0",
        gas_storage_rebate: "0",
        status: "failure",
        effects_status: "failure",
        error_message: errMsg,
        network,
        created_at: Date.now(),
      };
      suiDatabase.createTransaction(record);
      throw new Error(errMsg);
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "move_call" as SuiTxType,
      sender_address: senderAddress,
      package_id: req.package_id,
      module_name: req.module_name,
      function_name: req.function_name,
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
      status: "success",
      gas_used: gas,
      effects: extractEffects(txData?.effects),
      events: extractEvents(txData?.events?.events),
    };
  } catch (error) {
    logger.error("Failed to execute Sui Move call:", error);
    throw error;
  }
}

export async function publishPackage(
  req: SuiPublishPackageRequest,
): Promise<SuiPublishPackageResponse> {
  try {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = suiWalletManager.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const [upgradeCap] = tx.publish({
      modules: req.compiled_modules.map((m) =>
        Array.from(Buffer.from(m, "base64")),
      ),
      dependencies: req.dependencies,
    });

    tx.transferObjects([upgradeCap], senderAddress);

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Publish failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const createdObjects = (txData?.effects?.created ?? []) as Array<{
      reference?: { objectId?: string };
    }>;
    const packageId =
      createdObjects.find(() => true)?.reference?.objectId ?? "";
    const createdIds = createdObjects
      .map((c) => c.reference?.objectId ?? "")
      .filter(Boolean);

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "publish" as SuiTxType,
      sender_address: senderAddress,
      package_id: packageId,
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
      package_id: packageId,
      status: "success",
      gas_used: gas,
      created_objects: createdIds,
    };
  } catch (error) {
    logger.error("Failed to publish Sui package:", error);
    throw error;
  }
}

export async function upgradePackage(
  req: SuiUpgradePackageRequest,
): Promise<SuiPublishPackageResponse> {
  try {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = suiWalletManager.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const ticket = tx.moveCall({
      target: "0x2::package::authorize_upgrade",
      arguments: [
        tx.object(req.upgrade_cap_id),
        tx.pure(bcs.U8.serialize(0)),
        tx.pure(bcs.vector(bcs.U8).serialize([])),
      ],
    });

    const receipt = tx.upgrade({
      modules: req.compiled_modules.map((m) =>
        Array.from(Buffer.from(m, "base64")),
      ),
      dependencies: req.dependencies,
      package: normalizeSuiAddress(req.package_id),
      ticket,
    });

    tx.moveCall({
      target: "0x2::package::commit_upgrade",
      arguments: [tx.object(req.upgrade_cap_id), receipt],
    });

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Upgrade failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const createdObjects = (txData?.effects?.created ?? []) as Array<{
      reference?: { objectId?: string };
    }>;
    const newPackageId =
      createdObjects.find(() => true)?.reference?.objectId ?? req.package_id;
    const createdIds = createdObjects
      .map((c) => c.reference?.objectId ?? "")
      .filter(Boolean);

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "upgrade" as SuiTxType,
      sender_address: senderAddress,
      package_id: req.package_id,
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
      package_id: newPackageId,
      status: "success",
      gas_used: gas,
      created_objects: createdIds,
    };
  } catch (error) {
    logger.error("Failed to upgrade Sui package:", error);
    throw error;
  }
}

export function getLocalTransactionHistory(
  req: SuiLocalTransactionHistoryRequest,
): SuiLocalTransactionHistoryResponse {
  const limit = req.limit ?? 50;
  const offset = req.offset ?? 0;

  let records: SuiTransactionRecord[];
  if (req.tx_type) {
    records = suiDatabase.getTransactionsByType(
      req.wallet_id,
      req.tx_type,
      limit,
    ) as SuiTransactionRecord[];
  } else {
    records = suiDatabase.getWalletTransactions(
      req.wallet_id,
      limit,
      offset,
    ) as SuiTransactionRecord[];
  }

  const total = suiDatabase.getWalletTransactionCount(req.wallet_id);

  return {
    wallet_id: req.wallet_id,
    transactions: records,
    total,
    limit,
    offset,
  };
}

export function getLocalTransactionByDigest(
  digest: string,
): SuiTransactionRecord | null {
  return suiDatabase.getTransactionByDigest(
    digest,
  ) as SuiTransactionRecord | null;
}

export function getLocalUserTransactionHistory(
  userId: string,
  limit = 50,
  offset = 0,
): SuiTransactionRecord[] {
  return suiDatabase.getUserTransactions(
    userId,
    limit,
    offset,
  ) as SuiTransactionRecord[];
}
