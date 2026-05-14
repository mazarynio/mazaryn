import { Transaction } from "@mysten/sui/transactions";
import { bcs } from "@mysten/sui/bcs";
import { Ed25519Keypair } from "@mysten/sui/keypairs/ed25519";
import { Secp256k1Keypair } from "@mysten/sui/keypairs/secp256k1";
import { Secp256r1Keypair } from "@mysten/sui/keypairs/secp256r1";
import { MultiSigPublicKey } from "@mysten/sui/multisig";
import {
  verifyPersonalMessageSignature,
  verifyTransactionSignature,
} from "@mysten/sui/verify";
import { fromBase64 } from "@mysten/sui/utils";
import { logger } from "../../core/logger.js";
import { suiDatabase } from "../database/sui-database.js";
import {
  getSuiGrpcClient,
  getSuiObject,
  batchGetSuiObjects,
  listOwnedObjects,
  listDynamicFields,
  getMovePackage,
  getMoveFunction,
  getMoveDatatype,
  listPackageVersions,
  lookupSuiName,
  reverseLookupSuiName,
  verifySuiSignature,
  normalizeSuiAddress,
  getReferenceGasPrice,
} from "./sui-client.js";
import { suiWalletManager } from "./sui-wallet.js";
import type {
  SuiNetwork,
  SuiTxType,
  SuiGasSummary,
  SuiObjectInfo,
  SuiObjectOwner,
  SuiGetObjectRequest,
  SuiGetObjectsRequest,
  SuiNFTMintRequest,
  SuiNFTMintResponse,
  SuiNFTTransferRequest,
  SuiNFTInfo,
  SuiGetNFTsRequest,
  SuiDynamicField,
  SuiListDynamicFieldsRequest,
  SuiListDynamicFieldsResponse,
  SuiGetDynamicFieldRequest,
  SuiGetMovePackageRequest,
  SuiGetMoveFunctionRequest,
  SuiMoveFunctionInfo,
  SuiNameLookupRequest,
  SuiNameLookupResponse,
  SuiReverseNameLookupRequest,
  SuiSignMessageRequest,
  SuiSignMessageResponse,
  SuiVerifySignatureRequest,
  SuiVerifySignatureResponse,
  SuiMultiSigCreateRequest,
  SuiMultiSigCreateResponse,
  SuiMultiSigCombineRequest,
  SuiMultiSigCombineResponse,
} from "../../core/sui-types.js";

const DEFAULT_GAS_BUDGET = "50000000";
const NFT_PACKAGE_MAINNET = "0x2";

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

function parseOwner(owner: unknown): SuiObjectOwner {
  if (!owner || typeof owner !== "object") return { Immutable: true };
  const o = owner as Record<string, unknown>;
  const kind = String(o["kind"] ?? "");
  if (kind === "SHARED") {
    return { Shared: { initial_shared_version: String(o["version"] ?? 0) } };
  }
  if (kind === "OBJECT") {
    return { ObjectOwner: String(o["address"] ?? "") };
  }
  if (kind === "IMMUTABLE") {
    return { Immutable: true };
  }
  if (o["address"]) {
    return { AddressOwner: String(o["address"]) };
  }
  return { Immutable: true };
}

function rawObjectToInfo(obj: Record<string, unknown>): SuiObjectInfo {
  return {
    object_id: String(obj["objectId"] ?? ""),
    version: String(obj["version"] ?? 0),
    digest: String(obj["digest"] ?? ""),
    type: String(obj["objectType"] ?? ""),
    owner: parseOwner(obj["owner"]),
    previous_transaction: (obj["previousTransaction"] as string) ?? undefined,
    storage_rebate: String(obj["storageRebate"] ?? 0),
    content:
      obj["json"] !== undefined
        ? (obj["json"] as Record<string, unknown>)
        : undefined,
    balance: obj["balance"] !== undefined ? String(obj["balance"]) : undefined,
  };
}

export async function getObject(
  req: SuiGetObjectRequest,
): Promise<SuiObjectInfo | null> {
  try {
    const network = req.network ?? "mainnet";
    const raw = await getSuiObject(
      req.object_id,
      network,
      req.include_content ?? false,
    );
    if (!raw) return null;
    return rawObjectToInfo(raw as Record<string, unknown>);
  } catch (error) {
    logger.error("Failed to get Sui object:", error);
    throw error;
  }
}

export async function getObjects(
  req: SuiGetObjectsRequest,
): Promise<SuiObjectInfo[]> {
  try {
    const network = req.network ?? "mainnet";
    const rawList = await batchGetSuiObjects(
      req.object_ids,
      network,
      req.include_content ?? false,
    );

    return rawList
      .map((item) => {
        const r = item as Record<string, unknown>;
        const obj = (r["object"] ?? r) as Record<string, unknown> | undefined;
        if (!obj || !obj["objectId"]) return null;
        return rawObjectToInfo(obj);
      })
      .filter((o): o is SuiObjectInfo => o !== null);
  } catch (error) {
    logger.error("Failed to batch get Sui objects:", error);
    throw error;
  }
}

export async function mintNFT(
  req: SuiNFTMintRequest,
): Promise<SuiNFTMintResponse> {
  try {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = suiWalletManager.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const packageId = req.package_id ?? NFT_PACKAGE_MAINNET;
    const moduleName = req.module_name ?? "devnet_nft";
    const functionName = req.function_name ?? "mint_to_sender";

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const baseArgs = [
      tx.pure(bcs.String.serialize(req.name)),
      tx.pure(bcs.String.serialize(req.description)),
      tx.pure(bcs.String.serialize(req.url)),
    ];

    const extraArgs = (req.extra_args ?? []).map((arg) => {
      if (typeof arg === "string" && arg.startsWith("0x"))
        return tx.object(arg);
      if (typeof arg === "string") return tx.pure(bcs.String.serialize(arg));
      if (typeof arg === "number")
        return tx.pure(bcs.U64.serialize(BigInt(arg)));
      if (typeof arg === "boolean") return tx.pure(bcs.Bool.serialize(arg));
      return tx.pure(bcs.String.serialize(String(arg)));
    });

    tx.moveCall({
      target: `${normalizeSuiAddress(packageId)}::${moduleName}::${functionName}`,
      arguments: [...baseArgs, ...extraArgs],
    });

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `NFT mint failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const createdObjects = (txData?.effects?.created ?? []) as Array<{
      reference?: { objectId?: string };
    }>;
    const nftObjectId =
      createdObjects.find(() => true)?.reference?.objectId ?? "";

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "nft_mint" as SuiTxType,
      sender_address: senderAddress,
      object_id: nftObjectId,
      package_id: packageId,
      module_name: moduleName,
      function_name: functionName,
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
      nft_object_id: nftObjectId,
      name: req.name,
      description: req.description,
      url: req.url,
      owner_address: senderAddress,
      status: "success",
      gas_used: gas,
    };
  } catch (error) {
    logger.error("Failed to mint Sui NFT:", error);
    throw error;
  }
}

export async function transferNFT(
  req: SuiNFTTransferRequest,
): Promise<{
  digest: string;
  status: "success" | "failure";
  gas_used: SuiGasSummary;
}> {
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

    tx.transferObjects(
      [tx.object(normalizeSuiAddress(req.nft_object_id))],
      normalizeSuiAddress(req.recipient_address),
    );

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `NFT transfer failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const gas = extractGasSummary(gasUsed);

    const record = {
      tx_id: generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "nft_transfer" as SuiTxType,
      sender_address: senderAddress,
      recipient_address: normalizeSuiAddress(req.recipient_address),
      object_id: req.nft_object_id,
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

    return { digest: txData?.digest ?? "", status: "success", gas_used: gas };
  } catch (error) {
    logger.error("Failed to transfer Sui NFT:", error);
    throw error;
  }
}

export async function getNFTs(req: SuiGetNFTsRequest): Promise<SuiNFTInfo[]> {
  try {
    const ownerAddress = normalizeSuiAddress(req.owner_address);
    const wallet = suiDatabase.getWalletByAddress(ownerAddress);
    const network = wallet?.network ?? req.network ?? "mainnet";

    const result = await listOwnedObjects(ownerAddress, network, {
      pageSize: req.limit ?? 50,
    });

    const SUI_COIN_TYPE = "0x2::sui::SUI";
    const STAKED_SUI_TYPE = "0x3::staking_pool::StakedSui";

    const nftObjects = (result.objects ?? []).filter((obj) => {
      const objType = String(obj.objectType ?? "");
      return (
        !objType.includes(SUI_COIN_TYPE) &&
        !objType.includes(STAKED_SUI_TYPE) &&
        objType !== "package" &&
        objType !== ""
      );
    });

    return nftObjects.map((obj) => {
      const jsonContent = obj.json as Record<string, unknown> | undefined;
      const fields = (jsonContent?.["fields"] ?? jsonContent) as
        | Record<string, unknown>
        | undefined;

      return {
        object_id: String(obj.objectId ?? ""),
        type: String(obj.objectType ?? ""),
        name: (fields?.["name"] as string) ?? undefined,
        description: (fields?.["description"] as string) ?? undefined,
        url:
          (fields?.["url"] as string) ??
          (fields?.["image_url"] as string) ??
          undefined,
        owner_address: ownerAddress,
        version: String(obj.version ?? 0),
        digest: String(obj.digest ?? ""),
      };
    });
  } catch (error) {
    logger.error("Failed to get Sui NFTs:", error);
    throw error;
  }
}

export async function listObjectDynamicFields(
  req: SuiListDynamicFieldsRequest,
): Promise<SuiListDynamicFieldsResponse> {
  try {
    const network = req.network ?? "mainnet";

    const pageToken = req.cursor
      ? Buffer.from(req.cursor, "base64")
      : undefined;

    const result = await listDynamicFields(req.parent_id, network, {
      pageSize: req.limit ?? 50,
      pageToken,
    });

    const fields: SuiDynamicField[] = (result.dynamicFields ?? []).map((f) => {
      const field = f as Record<string, unknown>;
      return {
        field_id: String(field["fieldId"] ?? ""),
        parent: String(field["parent"] ?? req.parent_id),
        name: {
          type: String(
            (field["name"] as Record<string, unknown>)?.["name"] ?? "",
          ),
          bcs:
            ((field["name"] as Record<string, unknown>)?.[
              "value"
            ] as Uint8Array) ?? new Uint8Array(),
        },
        value_type: String(field["valueType"] ?? ""),
        child_id: (field["childId"] as string) ?? undefined,
      };
    });

    return {
      dynamic_fields: fields,
      next_cursor: result.nextPageToken
        ? Buffer.from(result.nextPageToken).toString("base64")
        : undefined,
      has_next_page: result.nextPageToken !== undefined,
    };
  } catch (error) {
    logger.error("Failed to list Sui dynamic fields:", error);
    throw error;
  }
}

export async function getDynamicField(
  req: SuiGetDynamicFieldRequest,
): Promise<SuiDynamicField | null> {
  try {
    const network = req.network ?? "mainnet";
    const client = getSuiGrpcClient(network);

    const { response } = await client.stateService.listDynamicFields({
      parent: normalizeSuiAddress(req.parent_id),
      readMask: {
        paths: [
          "field_id",
          "parent",
          "name",
          "value_type",
          "child_id",
          "value",
        ],
      },
    });

    const allFields = response.dynamicFields ?? [];
    const match = allFields.find((f) => {
      const field = f as Record<string, unknown>;
      const nameData = field["name"] as Record<string, unknown> | undefined;
      return nameData?.["name"] === req.name_type;
    });

    if (!match) return null;

    const field = match as Record<string, unknown>;
    return {
      field_id: String(field["fieldId"] ?? ""),
      parent: req.parent_id,
      name: {
        type: req.name_type,
        bcs: req.name_bcs,
      },
      value_type: String(field["valueType"] ?? ""),
      value: field["value"]
        ? {
            type: String(
              (field["value"] as Record<string, unknown>)?.["name"] ?? "",
            ),
            bcs:
              ((field["value"] as Record<string, unknown>)?.[
                "value"
              ] as Uint8Array) ?? new Uint8Array(),
          }
        : undefined,
      child_id: (field["childId"] as string) ?? undefined,
    };
  } catch (error) {
    logger.error("Failed to get Sui dynamic field:", error);
    throw error;
  }
}

export async function getMovePackageInfo(
  req: SuiGetMovePackageRequest,
): Promise<Record<string, unknown> | null> {
  try {
    const network = req.network ?? "mainnet";
    const pkg = await getMovePackage(req.package_id, network);
    if (!pkg) return null;

    return {
      storage_id: String((pkg as Record<string, unknown>)["storageId"] ?? ""),
      original_id: String((pkg as Record<string, unknown>)["originalId"] ?? ""),
      version: String((pkg as Record<string, unknown>)["version"] ?? 0),
      modules: ((pkg as Record<string, unknown>)["modules"] as unknown[])?.map(
        (m) => ({
          name: String((m as Record<string, unknown>)["name"] ?? ""),
          functions: (m as Record<string, unknown>)["functions"],
          datatypes: (m as Record<string, unknown>)["datatypes"],
        }),
      ),
    };
  } catch (error) {
    logger.error("Failed to get Sui Move package:", error);
    throw error;
  }
}

export async function getMoveFunctionInfo(
  req: SuiGetMoveFunctionRequest,
): Promise<SuiMoveFunctionInfo | null> {
  try {
    const network = req.network ?? "mainnet";
    const fn = await getMoveFunction(
      req.package_id,
      req.module_name,
      req.function_name,
      network,
    );
    if (!fn) return null;

    const f = fn as Record<string, unknown>;
    return {
      name: String(f["name"] ?? ""),
      is_entry: Boolean(f["isEntry"] ?? false),
      visibility: String(f["visibility"] ?? "PRIVATE"),
      parameters: (f["parameters"] as unknown[]) ?? [],
      returns: (f["returns"] as unknown[]) ?? [],
      type_parameters: (f["typeParameters"] as unknown[]) ?? [],
    };
  } catch (error) {
    logger.error("Failed to get Sui Move function:", error);
    throw error;
  }
}

export async function getMoveTypeInfo(
  packageId: string,
  moduleName: string,
  typeName: string,
  network: SuiNetwork = "mainnet",
): Promise<Record<string, unknown> | null> {
  try {
    const dt = await getMoveDatatype(packageId, moduleName, typeName, network);
    if (!dt) return null;

    const d = dt as Record<string, unknown>;
    return {
      name: String(d["name"] ?? ""),
      module: String(d["module"] ?? ""),
      kind: String(d["kind"] ?? ""),
      abilities: (d["abilities"] as string[]) ?? [],
      fields: (d["fields"] as unknown[]) ?? [],
      variants: (d["variants"] as unknown[]) ?? [],
      type_parameters: (d["typeParameters"] as unknown[]) ?? [],
    };
  } catch (error) {
    logger.error("Failed to get Sui Move datatype:", error);
    throw error;
  }
}

export async function getPackageVersions(
  packageId: string,
  network: SuiNetwork = "mainnet",
): Promise<Array<{ package_id: string; version: string }>> {
  try {
    const result = await listPackageVersions(packageId, network);
    return (result.versions ?? []).map((v) => {
      const ver = v as Record<string, unknown>;
      return {
        package_id: String(ver["packageId"] ?? ""),
        version: String(ver["version"] ?? 0),
      };
    });
  } catch (error) {
    logger.error("Failed to list Sui package versions:", error);
    throw error;
  }
}

export async function lookupName(
  req: SuiNameLookupRequest,
): Promise<SuiNameLookupResponse> {
  try {
    const network = req.network ?? "mainnet";
    const record = await lookupSuiName(req.name, network);

    if (!record) {
      return { name: req.name };
    }

    const r = record as Record<string, unknown>;
    const expTs = r["expirationTimestamp"] as
      | Record<string, unknown>
      | undefined;

    return {
      name: String(r["name"] ?? req.name),
      target_address: (r["targetAddress"] as string) ?? undefined,
      expiration_timestamp: expTs?.["seconds"]
        ? Number(expTs["seconds"]) * 1000
        : undefined,
      id: (r["id"] as string) ?? undefined,
    };
  } catch (error) {
    logger.error("Failed to lookup Sui name:", error);
    throw error;
  }
}

export async function reverseLookupName(
  req: SuiReverseNameLookupRequest,
): Promise<SuiNameLookupResponse> {
  try {
    const network = req.network ?? "mainnet";
    const record = await reverseLookupSuiName(req.address, network);

    if (!record) {
      return { name: "" };
    }

    const r = record as Record<string, unknown>;
    const expTs = r["expirationTimestamp"] as
      | Record<string, unknown>
      | undefined;

    return {
      name: String(r["name"] ?? ""),
      target_address: req.address,
      expiration_timestamp: expTs?.["seconds"]
        ? Number(expTs["seconds"]) * 1000
        : undefined,
      id: (r["id"] as string) ?? undefined,
    };
  } catch (error) {
    logger.error("Failed to reverse lookup Sui name:", error);
    throw error;
  }
}

export async function signMessage(
  req: SuiSignMessageRequest,
): Promise<SuiSignMessageResponse> {
  try {
    const wallet = suiDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error(`Wallet not found: ${req.wallet_id}`);

    const keypair = suiWalletManager.getKeypair(req.wallet_id);
    const messageBytes = new TextEncoder().encode(req.message);
    const { signature } = await keypair.signPersonalMessage(messageBytes);
    const publicKey = keypair.getPublicKey();

    const messageHash = Buffer.from(
      await crypto.subtle.digest("SHA-256", messageBytes),
    ).toString("hex");

    return {
      signature,
      public_key: publicKey.toBase64(),
      wallet_address: wallet.sui_address,
      message_hash: messageHash,
    };
  } catch (error) {
    logger.error("Failed to sign Sui message:", error);
    throw error;
  }
}

export async function verifySignature(
  req: SuiVerifySignatureRequest,
): Promise<SuiVerifySignatureResponse> {
  try {
    const network = req.network ?? "mainnet";
    const messageBytes = new TextEncoder().encode(req.message);
    const signatureBytes = fromBase64(req.signature);

    const grpcResult = await verifySuiSignature(
      messageBytes,
      "PersonalMessage",
      signatureBytes,
      { address: req.address, network },
    );

    if (grpcResult.is_valid) {
      return {
        is_valid: true,
        address: req.address,
      };
    }

    try {
      const publicKey = await verifyPersonalMessageSignature(
        messageBytes,
        req.signature,
        {
          address: req.address,
        },
      );
      return {
        is_valid: true,
        address: publicKey.toSuiAddress(),
      };
    } catch {
      return {
        is_valid: false,
        reason: grpcResult.reason ?? "Signature verification failed",
      };
    }
  } catch (error) {
    logger.error("Failed to verify Sui signature:", error);
    return {
      is_valid: false,
      reason: String(error),
    };
  }
}

export async function verifyTransactionSig(
  transactionBytes: Uint8Array,
  signature: string,
  address?: string,
): Promise<SuiVerifySignatureResponse> {
  try {
    await verifyTransactionSignature(transactionBytes, signature, {
      address,
    });
    return { is_valid: true, address };
  } catch (error) {
    return {
      is_valid: false,
      reason: String(error),
    };
  }
}

export function createMultiSigWallet(
  req: SuiMultiSigCreateRequest,
): SuiMultiSigCreateResponse {
  try {
    const multiSigPublicKey = MultiSigPublicKey.fromPublicKeys({
      threshold: req.threshold,
      publicKeys: req.public_keys.map((pk) => {
        let publicKey: Ed25519Keypair["getPublicKey"] extends () => infer R
          ? R
          : never;

        switch (pk.scheme) {
          case "secp256k1": {
            const kp = Secp256k1Keypair.fromSecretKey(
              Buffer.from(pk.public_key, "base64"),
            );
            publicKey = kp.getPublicKey() as ReturnType<
              Secp256k1Keypair["getPublicKey"]
            >;
            break;
          }
          case "secp256r1": {
            const kp = Secp256r1Keypair.fromSecretKey(
              Buffer.from(pk.public_key, "base64"),
            );
            publicKey = kp.getPublicKey() as ReturnType<
              Secp256r1Keypair["getPublicKey"]
            >;
            break;
          }
          default: {
            const kp = Ed25519Keypair.fromSecretKey(
              Buffer.from(pk.public_key, "base64"),
            );
            publicKey = kp.getPublicKey();
            break;
          }
        }

        return {
          publicKey: publicKey as Parameters<
            typeof MultiSigPublicKey.fromPublicKeys
          >[0]["publicKeys"][0]["publicKey"],
          weight: pk.weight,
        };
      }),
    });

    const multisigAddress = multiSigPublicKey.toSuiAddress();

    return {
      multisig_address: multisigAddress,
      threshold: req.threshold,
      members: req.public_keys.map((pk) => ({
        public_key: pk.public_key,
        scheme: pk.scheme,
        weight: pk.weight,
      })),
    };
  } catch (error) {
    logger.error("Failed to create Sui multisig wallet:", error);
    throw error;
  }
}

export async function combineMultiSigAndExecute(
  req: SuiMultiSigCombineRequest,
): Promise<SuiMultiSigCombineResponse> {
  try {
    const network = req.network ?? "mainnet";

    const multiSigPublicKey = MultiSigPublicKey.fromPublicKeys({
      threshold: req.threshold,
      publicKeys: req.public_keys.map((pk) => {
        let keypair: Ed25519Keypair | Secp256k1Keypair | Secp256r1Keypair;
        switch (pk.scheme) {
          case "secp256k1":
            keypair = Secp256k1Keypair.fromSecretKey(
              Buffer.from(pk.public_key, "base64"),
            );
            break;
          case "secp256r1":
            keypair = Secp256r1Keypair.fromSecretKey(
              Buffer.from(pk.public_key, "base64"),
            );
            break;
          default:
            keypair = Ed25519Keypair.fromSecretKey(
              Buffer.from(pk.public_key, "base64"),
            );
        }
        return {
          publicKey: keypair.getPublicKey() as Parameters<
            typeof MultiSigPublicKey.fromPublicKeys
          >[0]["publicKeys"][0]["publicKey"],
          weight: pk.weight,
        };
      }),
    });

    const combinedSignature = multiSigPublicKey.combinePartialSignatures(
      req.partial_signatures,
    );
    const txBytes = Buffer.from(req.transaction_bytes, "base64");

    const client = getSuiGrpcClient(network);
    const { response } =
      await client.transactionExecutionService.executeTransaction({
        transaction: { bcs: { value: txBytes } },
        signatures: [
          {
            bcs: { value: fromBase64(combinedSignature) },
            signature: { oneofKind: undefined },
          },
        ],
        readMask: { paths: ["effects", "digest"] },
      });

    const finality = response.finality as Record<string, unknown> | undefined;
    const statusSuccess = (finality?.["effects"] as Record<string, unknown>)?.[
      "status"
    ]
      ? (
          (finality?.["effects"] as Record<string, unknown>)[
            "status"
          ] as Record<string, unknown>
        )["success"]
      : true;

    return {
      combined_signature: combinedSignature,
      digest: String(finality?.["digest"] ?? ""),
      status: statusSuccess ? "success" : "failure",
    };
  } catch (error) {
    logger.error("Failed to combine and execute Sui multisig:", error);
    throw error;
  }
}
