import { Ed25519Keypair } from "@mysten/sui/keypairs/ed25519";
import { Secp256k1Keypair } from "@mysten/sui/keypairs/secp256k1";
import { Secp256r1Keypair } from "@mysten/sui/keypairs/secp256r1";
import { decodeSuiPrivateKey } from "@mysten/sui/cryptography";
import { fromHex } from "@mysten/sui/utils";
import { Transaction } from "@mysten/sui/transactions";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import { suiDatabase } from "../database/sui-database.js";
import {
  getSuiGrpcClient,
  getSuiBalance,
  listSuiBalances,
  listOwnedObjects,
  normalizeSuiAddress,
  mistToSui,
  suiToMist,
  getReferenceGasPrice,
} from "./sui-client.js";
import type {
  SuiNetwork,
  CreateSuiWalletRequest,
  CreateSuiWalletResponse,
  ImportSuiWalletRequest,
  ImportSuiWalletFromMnemonicRequest,
  SuiWalletInfo,
  DeleteSuiWalletRequest,
  SuiBalanceResponse,
  SuiAllBalancesResponse,
  SuiCoinObject,
  SuiListOwnedObjectsRequest,
  SuiListOwnedObjectsResponse,
  SuiObjectInfo,
  SuiTransferRequest,
  SuiTransferResponse,
  SuiBatchTransferRequest,
  SuiBatchTransferResponse,
  SuiSplitCoinRequest,
  SuiSplitCoinResponse,
  SuiMergeCoinsRequest,
  SuiMergeCoinsResponse,
  SuiGetCoinInfoRequest,
  SuiGetCoinInfoResponse,
} from "../../core/sui-types.js";

type AnyKeypair = Ed25519Keypair | Secp256k1Keypair | Secp256r1Keypair;

interface CachedSuiWallet {
  keypair: AnyKeypair;
  address: string;
  scheme: string;
}

const DEFAULT_GAS_BUDGET = "50000000";
const SUI_COIN_TYPE = "0x2::sui::SUI";

export class SuiWalletManager {
  private walletCache: Map<string, CachedSuiWallet> = new Map();

  constructor() {
    this.loadWalletsFromDatabase();
  }

  private async loadWalletsFromDatabase(): Promise<void> {
    try {
      const wallets = suiDatabase.getAllWallets();
      logger.info(`Loading ${wallets.length} Sui wallets from database`);

      for (const wallet of wallets) {
        try {
          const privateKeyHex = encryptionService.decrypt(
            wallet.encrypted_private_key,
            wallet.encryption_iv,
            wallet.encryption_tag,
          );

          const keypair = this.keypairFromHexAndScheme(
            privateKeyHex,
            wallet.scheme,
          );

          this.walletCache.set(wallet.wallet_id, {
            keypair,
            address: wallet.sui_address,
            scheme: wallet.scheme,
          });
        } catch (error) {
          logger.error(`Failed to load Sui wallet ${wallet.wallet_id}:`, error);
        }
      }

      logger.info(
        `Successfully loaded ${this.walletCache.size} Sui wallets into cache`,
      );
    } catch (error) {
      logger.error("Failed to load Sui wallets from database:", error);
    }
  }

  private keypairFromHexAndScheme(
    privateKeyHex: string,
    scheme: string,
  ): AnyKeypair {
    const keyBytes = fromHex(
      privateKeyHex.startsWith("0x") ? privateKeyHex.slice(2) : privateKeyHex,
    );
    switch (scheme) {
      case "secp256k1":
        return Secp256k1Keypair.fromSecretKey(keyBytes);
      case "secp256r1":
        return Secp256r1Keypair.fromSecretKey(keyBytes);
      default:
        return Ed25519Keypair.fromSecretKey(keyBytes);
    }
  }

  private generateKeypair(
    scheme: "ed25519" | "secp256k1" | "secp256r1",
  ): AnyKeypair {
    switch (scheme) {
      case "secp256k1":
        return new Secp256k1Keypair();
      case "secp256r1":
        return new Secp256r1Keypair();
      default:
        return new Ed25519Keypair();
    }
  }

  private extractSecretKeyHex(keypair: AnyKeypair): string {
    if (keypair instanceof Ed25519Keypair) {
      return keypair.getSecretKey();
    }
    if (keypair instanceof Secp256k1Keypair) {
      return keypair.getSecretKey();
    }
    return (keypair as Secp256r1Keypair).getSecretKey();
  }

  private generateWalletId(): string {
    return `sui_wallet_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }

  private generateTxId(): string {
    return `sui_tx_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }

  getKeypair(walletId: string): AnyKeypair {
    const cached = this.walletCache.get(walletId);
    if (!cached) throw new Error(`Sui wallet not found in cache: ${walletId}`);
    return cached.keypair;
  }

  getKeypairByAddress(suiAddress: string): AnyKeypair {
    const wallet = suiDatabase.getWalletByAddress(
      normalizeSuiAddress(suiAddress),
    );
    if (!wallet)
      throw new Error(`Sui wallet not found for address: ${suiAddress}`);
    const cached = this.walletCache.get(wallet.wallet_id);
    if (!cached)
      throw new Error(`Sui wallet not in cache: ${wallet.wallet_id}`);
    return cached.keypair;
  }

  async createWallet(
    req: CreateSuiWalletRequest,
  ): Promise<CreateSuiWalletResponse> {
    try {
      logger.info("Creating new Sui wallet for user:", req.user_id);

      const scheme = req.scheme ?? "ed25519";
      const network = req.network ?? "mainnet";

      const keypair = this.generateKeypair(scheme);
      const address = keypair.toSuiAddress();
      const publicKey = keypair.getPublicKey().toBase64();
      const secretKeyHex = this.extractSecretKeyHex(keypair);

      const encrypted = encryptionService.encrypt(secretKeyHex);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      suiDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        sui_address: address,
        public_key: publicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        label: req.label,
        scheme,
        network,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, { keypair, address, scheme });

      logger.info(`Sui wallet created: ${walletId} address: ${address}`);

      return {
        wallet_id: walletId,
        sui_address: address,
        public_key: publicKey,
        scheme,
        user_id: req.user_id,
        network,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to create Sui wallet:", error);
      throw error;
    }
  }

  async importWallet(
    req: ImportSuiWalletRequest,
  ): Promise<CreateSuiWalletResponse> {
    try {
      logger.info("Importing Sui wallet for user:", req.user_id);

      const network = req.network ?? "mainnet";
      let keypair: AnyKeypair;
      let scheme = req.scheme ?? "ed25519";

      const privateKey = req.private_key.trim();

      if (privateKey.startsWith("suiprivkey")) {
        const decoded = decodeSuiPrivateKey(privateKey);
        switch (decoded.schema) {
          case "ED25519":
            keypair = Ed25519Keypair.fromSecretKey(decoded.secretKey);
            scheme = "ed25519";
            break;
          case "Secp256k1":
            keypair = Secp256k1Keypair.fromSecretKey(decoded.secretKey);
            scheme = "secp256k1";
            break;
          case "Secp256r1":
            keypair = Secp256r1Keypair.fromSecretKey(decoded.secretKey);
            scheme = "secp256r1";
            break;
          default:
            throw new Error(`Unsupported key scheme: ${decoded.schema}`);
        }
      } else {
        const keyHex = privateKey.startsWith("0x")
          ? privateKey.slice(2)
          : privateKey;
        if (keyHex.length !== 64 && keyHex.length !== 128) {
          throw new Error(
            "Invalid private key length. Expected 32 or 64 bytes hex.",
          );
        }
        keypair = this.keypairFromHexAndScheme(keyHex, scheme);
      }

      const address = keypair.toSuiAddress();

      if (suiDatabase.addressExists(address)) {
        throw new Error(`Sui address already exists: ${address}`);
      }

      const publicKey = keypair.getPublicKey().toBase64();
      const secretKeyHex = this.extractSecretKeyHex(keypair);
      const encrypted = encryptionService.encrypt(secretKeyHex);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      suiDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        sui_address: address,
        public_key: publicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        label: req.label,
        scheme,
        network,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, { keypair, address, scheme });

      logger.info(`Sui wallet imported: ${walletId} address: ${address}`);

      return {
        wallet_id: walletId,
        sui_address: address,
        public_key: publicKey,
        scheme,
        user_id: req.user_id,
        network,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to import Sui wallet:", error);
      throw error;
    }
  }

  async importWalletFromMnemonic(
    req: ImportSuiWalletFromMnemonicRequest,
  ): Promise<CreateSuiWalletResponse> {
    try {
      logger.info("Importing Sui wallet from mnemonic for user:", req.user_id);

      const network = req.network ?? "mainnet";
      const scheme = req.scheme ?? "ed25519";
      let keypair: AnyKeypair;

      switch (scheme) {
        case "secp256k1":
          keypair = Secp256k1Keypair.deriveKeypair(
            req.mnemonic,
            req.derivation_path,
          );
          break;
        case "secp256r1":
          keypair = Secp256r1Keypair.deriveKeypair(
            req.mnemonic,
            req.derivation_path,
          );
          break;
        default:
          keypair = Ed25519Keypair.deriveKeypair(
            req.mnemonic,
            req.derivation_path,
          );
      }

      const address = keypair.toSuiAddress();

      if (suiDatabase.addressExists(address)) {
        throw new Error(`Sui address already exists: ${address}`);
      }

      const publicKey = keypair.getPublicKey().toBase64();
      const secretKeyHex = this.extractSecretKeyHex(keypair);
      const encrypted = encryptionService.encrypt(secretKeyHex);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      suiDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        sui_address: address,
        public_key: publicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        label: req.label,
        scheme,
        network,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, { keypair, address, scheme });

      logger.info(
        `Sui wallet imported from mnemonic: ${walletId} address: ${address}`,
      );

      return {
        wallet_id: walletId,
        sui_address: address,
        public_key: publicKey,
        scheme,
        user_id: req.user_id,
        network,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to import Sui wallet from mnemonic:", error);
      throw error;
    }
  }

  deleteWallet(req: DeleteSuiWalletRequest): void {
    const wallet = suiDatabase.getWallet(req.wallet_id);
    if (!wallet || wallet.user_id !== req.user_id) {
      throw new Error("Wallet not found or does not belong to user");
    }
    suiDatabase.deleteWallet(req.wallet_id);
    this.walletCache.delete(req.wallet_id);
    logger.info(`Sui wallet deleted: ${req.wallet_id}`);
  }

  getWalletInfo(walletId: string): SuiWalletInfo {
    const wallet = suiDatabase.getWallet(walletId);
    if (!wallet) throw new Error(`Sui wallet not found: ${walletId}`);
    return {
      wallet_id: wallet.wallet_id,
      sui_address: wallet.sui_address,
      public_key: wallet.public_key,
      scheme: wallet.scheme,
      user_id: wallet.user_id,
      label: wallet.label,
      network: wallet.network,
      created_at: wallet.created_at,
      last_used: wallet.last_used,
      is_primary: wallet.is_primary === 1,
    };
  }

  getUserWallets(userId: string): SuiWalletInfo[] {
    return suiDatabase.getUserWallets(userId).map((w) => ({
      wallet_id: w.wallet_id,
      sui_address: w.sui_address,
      public_key: w.public_key,
      scheme: w.scheme,
      user_id: w.user_id,
      label: w.label,
      network: w.network,
      created_at: w.created_at,
      last_used: w.last_used,
      is_primary: w.is_primary === 1,
    }));
  }

  setPrimaryWallet(userId: string, walletId: string): void {
    const wallet = suiDatabase.getWallet(walletId);
    if (!wallet || wallet.user_id !== userId) {
      throw new Error("Wallet not found or does not belong to user");
    }
    suiDatabase.setPrimaryWallet(userId, walletId);
  }

  updateWalletLabel(walletId: string, label: string): void {
    suiDatabase.updateWalletLabel(walletId, label);
  }

  async getSuiBalance(walletId: string): Promise<SuiBalanceResponse> {
    const wallet = suiDatabase.getWallet(walletId);
    if (!wallet) throw new Error(`Wallet not found: ${walletId}`);

    suiDatabase.updateWalletLastUsed(walletId);
    const balance = await getSuiBalance(
      wallet.sui_address,
      SUI_COIN_TYPE,
      wallet.network,
    );

    return {
      sui_address: wallet.sui_address,
      coin_type: SUI_COIN_TYPE,
      total_balance: String(balance?.balance ?? 0n),
      coin_object_count: Number(balance?.coinObjectCount ?? 0),
    };
  }

  async getBalanceByCoinType(
    walletId: string,
    coinType: string,
  ): Promise<SuiBalanceResponse> {
    const wallet = suiDatabase.getWallet(walletId);
    if (!wallet) throw new Error(`Wallet not found: ${walletId}`);

    const balance = await getSuiBalance(
      wallet.sui_address,
      coinType,
      wallet.network,
    );

    return {
      sui_address: wallet.sui_address,
      coin_type: coinType,
      total_balance: String(balance?.balance ?? 0n),
      coin_object_count: Number(balance?.coinObjectCount ?? 0),
    };
  }

  async getAllBalances(walletId: string): Promise<SuiAllBalancesResponse> {
    const wallet = suiDatabase.getWallet(walletId);
    if (!wallet) throw new Error(`Wallet not found: ${walletId}`);

    suiDatabase.updateWalletLastUsed(walletId);
    const result = await listSuiBalances(wallet.sui_address, wallet.network);

    return {
      sui_address: wallet.sui_address,
      balances: (result.balances ?? []).map((b) => ({
        coin_type: b.coinType ?? "",
        coin_object_count: Number(b.coinObjectCount ?? 0),
        total_balance: String(b.balance ?? 0n),
      })),
    };
  }

  async getOwnedCoins(
    walletId: string,
    coinType = SUI_COIN_TYPE,
  ): Promise<SuiCoinObject[]> {
    const wallet = suiDatabase.getWallet(walletId);
    if (!wallet) throw new Error(`Wallet not found: ${walletId}`);

    const result = await listOwnedObjects(wallet.sui_address, wallet.network, {
      objectType: coinType,
      pageSize: 50,
    });

    return (result.objects ?? []).map((obj) => ({
      coin_type: obj.objectType ?? coinType,
      coin_object_id: obj.objectId ?? "",
      version: String(obj.version ?? 0),
      digest: obj.digest ?? "",
      balance: String(obj.balance ?? 0n),
      previous_transaction: obj.previousTransaction ?? "",
    }));
  }

  async getOwnedObjects(
    req: SuiListOwnedObjectsRequest,
  ): Promise<SuiListOwnedObjectsResponse> {
    const wallet = suiDatabase.getWalletByAddress(
      normalizeSuiAddress(req.owner_address),
    );
    const network = wallet?.network ?? req.network ?? "mainnet";

    const result = await listOwnedObjects(req.owner_address, network, {
      objectType: req.object_type,
      pageSize: req.limit ?? 50,
    });

    const objects: SuiObjectInfo[] = (result.objects ?? []).map((obj) => ({
      object_id: obj.objectId ?? "",
      version: String(obj.version ?? 0),
      digest: obj.digest ?? "",
      type: obj.objectType ?? "",
      owner: this.parseOwner(obj.owner),
      previous_transaction: obj.previousTransaction ?? undefined,
      storage_rebate: String(obj.storageRebate ?? 0),
      balance: obj.balance !== undefined ? String(obj.balance) : undefined,
    }));

    return {
      objects,
      next_cursor: result.nextPageToken
        ? Buffer.from(result.nextPageToken).toString("base64")
        : undefined,
      has_next_page: result.nextPageToken !== undefined,
    };
  }

  async getCoinInfo(
    req: SuiGetCoinInfoRequest,
  ): Promise<SuiGetCoinInfoResponse> {
    const network = req.network ?? "mainnet";
    const { getSuiCoinInfo } = await import("./sui-client.js");
    const result = await getSuiCoinInfo(req.coin_type, network);

    return {
      coin_type: result.coinType ?? req.coin_type,
      metadata: result.metadata
        ? {
            id: result.metadata.id ?? "",
            coin_type: req.coin_type,
            name: result.metadata.name ?? "",
            symbol: result.metadata.symbol ?? "",
            description: result.metadata.description ?? "",
            icon_url: result.metadata.iconUrl ?? undefined,
            decimals: result.metadata.decimals ?? 0,
          }
        : undefined,
      treasury: result.treasury
        ? {
            id: result.treasury.id ?? "",
            total_supply: String(result.treasury.totalSupply ?? 0),
            supply_state:
              (result.treasury.supplyState as
                | "FIXED"
                | "BURN_ONLY"
                | "SUPPLY_STATE_UNKNOWN") ?? "SUPPLY_STATE_UNKNOWN",
          }
        : undefined,
      is_regulated:
        result.regulatedMetadata?.coinRegulatedState === "REGULATED",
    };
  }

  async transferSui(req: SuiTransferRequest): Promise<SuiTransferResponse> {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = this.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const amountMist = BigInt(req.amount_mist.toString());
    const [coin] = tx.splitCoins(tx.gas, [amountMist]);
    tx.transferObjects([coin], normalizeSuiAddress(req.recipient_address));

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Transfer failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;

    const record = {
      tx_id: this.generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "sui_transfer" as const,
      sender_address: senderAddress,
      recipient_address: normalizeSuiAddress(req.recipient_address),
      amount_mist: amountMist.toString(),
      coin_type: SUI_COIN_TYPE,
      gas_computation: String(gasUsed?.computationCost ?? 0),
      gas_storage: String(gasUsed?.storageCost ?? 0),
      gas_storage_rebate: String(gasUsed?.storageRebate ?? 0),
      status: "success",
      effects_status: "success",
      checkpoint: String(txData?.checkpoint ?? ""),
      network,
      created_at: Date.now(),
    };
    suiDatabase.createTransaction(record);
    suiDatabase.updateWalletLastUsed(wallet.wallet_id);

    return {
      digest: txData?.digest ?? "",
      sender_address: senderAddress,
      recipient_address: normalizeSuiAddress(req.recipient_address),
      amount_mist: amountMist.toString(),
      coin_type: SUI_COIN_TYPE,
      gas_used: {
        computation_cost: String(gasUsed?.computationCost ?? 0),
        storage_cost: String(gasUsed?.storageCost ?? 0),
        storage_rebate: String(gasUsed?.storageRebate ?? 0),
        non_refundable_storage_fee: String(
          gasUsed?.nonRefundableStorageFee ?? 0,
        ),
      },
      status: "success",
      checkpoint: String(txData?.checkpoint ?? ""),
      timestamp: Date.now(),
      effects_status: "success",
    };
  }

  async batchTransferSui(
    req: SuiBatchTransferRequest,
  ): Promise<SuiBatchTransferResponse> {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = req.network ?? wallet.network;
    const keypair = this.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const coins = tx.splitCoins(
      tx.gas,
      req.transfers.map((t) => BigInt(t.amount)),
    );

    req.transfers.forEach((transfer, index) => {
      tx.transferObjects([coins[index]], normalizeSuiAddress(transfer.to));
    });

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Batch transfer failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;

    const record = {
      tx_id: this.generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "batch_transfer" as const,
      sender_address: senderAddress,
      gas_computation: String(gasUsed?.computationCost ?? 0),
      gas_storage: String(gasUsed?.storageCost ?? 0),
      gas_storage_rebate: String(gasUsed?.storageRebate ?? 0),
      status: "success",
      effects_status: "success",
      checkpoint: String(txData?.checkpoint ?? ""),
      network,
      created_at: Date.now(),
    };
    suiDatabase.createTransaction(record);
    suiDatabase.updateWalletLastUsed(wallet.wallet_id);

    return {
      digest: txData?.digest ?? "",
      sender_address: senderAddress,
      transfers: req.transfers,
      gas_used: {
        computation_cost: String(gasUsed?.computationCost ?? 0),
        storage_cost: String(gasUsed?.storageCost ?? 0),
        storage_rebate: String(gasUsed?.storageRebate ?? 0),
        non_refundable_storage_fee: String(
          gasUsed?.nonRefundableStorageFee ?? 0,
        ),
      },
      status: "success",
      checkpoint: String(txData?.checkpoint ?? ""),
    };
  }

  async splitCoin(req: SuiSplitCoinRequest): Promise<SuiSplitCoinResponse> {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = wallet.network;
    const keypair = this.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    const newCoins = tx.splitCoins(
      tx.object(req.coin_object_id),
      req.split_amounts.map((a) => BigInt(a)),
    );

    tx.transferObjects(
      req.split_amounts.map((_, i) => newCoins[i]),
      senderAddress,
    );

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Split coin failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;
    const createdObjects = (txData?.effects?.created ?? []).map(
      (c: { reference?: { objectId?: string } }) => c.reference?.objectId ?? "",
    );

    const record = {
      tx_id: this.generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "split_coin" as const,
      sender_address: senderAddress,
      object_id: req.coin_object_id,
      gas_computation: String(gasUsed?.computationCost ?? 0),
      gas_storage: String(gasUsed?.storageCost ?? 0),
      gas_storage_rebate: String(gasUsed?.storageRebate ?? 0),
      status: "success",
      effects_status: "success",
      network,
      created_at: Date.now(),
    };
    suiDatabase.createTransaction(record);

    return {
      digest: txData?.digest ?? "",
      new_coin_ids: createdObjects.filter(Boolean),
      gas_used: {
        computation_cost: String(gasUsed?.computationCost ?? 0),
        storage_cost: String(gasUsed?.storageCost ?? 0),
        storage_rebate: String(gasUsed?.storageRebate ?? 0),
        non_refundable_storage_fee: String(
          gasUsed?.nonRefundableStorageFee ?? 0,
        ),
      },
      status: "success",
    };
  }

  async mergeCoins(req: SuiMergeCoinsRequest): Promise<SuiMergeCoinsResponse> {
    const senderAddress = normalizeSuiAddress(req.sender_address);
    const wallet = suiDatabase.getWalletByAddress(senderAddress);
    if (!wallet)
      throw new Error(`Wallet not found for address: ${senderAddress}`);

    const network = wallet.network;
    const keypair = this.getKeypairByAddress(senderAddress);
    const client = getSuiGrpcClient(network);
    const gasPrice = await getReferenceGasPrice(network);

    const tx = new Transaction();
    tx.setSender(senderAddress);
    tx.setGasPrice(BigInt(gasPrice));
    tx.setGasBudget(BigInt(req.gas_budget ?? DEFAULT_GAS_BUDGET));

    tx.mergeCoins(
      tx.object(req.primary_coin_id),
      req.coins_to_merge.map((id) => tx.object(id)),
    );

    const result = await client.core.signAndExecuteTransaction({
      transaction: tx,
      signer: keypair,
      include: { effects: true },
    });

    if (result.FailedTransaction) {
      throw new Error(
        `Merge coins failed: ${result.FailedTransaction.status.error}`,
      );
    }

    const txData = result.Transaction;
    const gasUsed = txData?.effects?.gasUsed;

    const record = {
      tx_id: this.generateTxId(),
      wallet_id: wallet.wallet_id,
      user_id: wallet.user_id,
      digest: txData?.digest ?? "",
      tx_type: "merge_coins" as const,
      sender_address: senderAddress,
      object_id: req.primary_coin_id,
      gas_computation: String(gasUsed?.computationCost ?? 0),
      gas_storage: String(gasUsed?.storageCost ?? 0),
      gas_storage_rebate: String(gasUsed?.storageRebate ?? 0),
      status: "success",
      effects_status: "success",
      network,
      created_at: Date.now(),
    };
    suiDatabase.createTransaction(record);

    return {
      digest: txData?.digest ?? "",
      gas_used: {
        computation_cost: String(gasUsed?.computationCost ?? 0),
        storage_cost: String(gasUsed?.storageCost ?? 0),
        storage_rebate: String(gasUsed?.storageRebate ?? 0),
        non_refundable_storage_fee: String(
          gasUsed?.nonRefundableStorageFee ?? 0,
        ),
      },
      status: "success",
    };
  }

  private parseOwner(
    owner: unknown,
  ): import("../../core/sui-types.js").SuiObjectOwner {
    if (!owner || typeof owner !== "object") return { Immutable: true };
    const o = owner as Record<string, unknown>;
    if ("address" in o) {
      const kind = (o as { kind?: string }).kind;
      if (kind === "SHARED")
        return {
          Shared: { initial_shared_version: String(o["version"] ?? 0) },
        };
      if (kind === "OBJECT") return { ObjectOwner: String(o["address"]) };
      return { AddressOwner: String(o["address"]) };
    }
    return { Immutable: true };
  }
}

export const suiWalletManager = new SuiWalletManager();
export { mistToSui, suiToMist };
