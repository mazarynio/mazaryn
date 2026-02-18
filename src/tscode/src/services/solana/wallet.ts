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
  createKeyPairSignerFromBytes,
} from "@solana/kit";
import { getTransferSolInstruction } from "@solana-program/system";
import { Keypair } from "@solana/web3.js";
import bs58 from "bs58";
import { solanaConnection } from "./connection.js";
import { logger } from "../../core/logger.js";
import { walletDatabase } from "../database/database.js";
import { encryptionService } from "../security/encryption.js";
import type {
  CreateWalletRequest,
  CreateWalletResponse,
  ImportWalletRequest,
  GetBalanceResponse,
  TransferRequest,
  TransferResponse,
  GetTransactionResponse,
  WalletInfo,
} from "../../core/types.js";

interface CachedWallet {
  keypair: Keypair;
  signer: Awaited<ReturnType<typeof createKeyPairSignerFromBytes>>;
}

export class WalletManager {
  private walletCache: Map<string, CachedWallet>;

  constructor() {
    this.walletCache = new Map();
    this.loadWalletsFromDatabase();
  }

  private async loadWalletsFromDatabase(): Promise<void> {
    try {
      const wallets = walletDatabase.getAllWallets();
      logger.info(`Loading ${wallets.length} wallets from database`);

      for (const wallet of wallets) {
        try {
          const decryptedPrivateKey = encryptionService.decrypt(
            wallet.encrypted_private_key,
          );
          const privateKeyBytes = bs58.decode(decryptedPrivateKey);
          const keypair = Keypair.fromSecretKey(privateKeyBytes);
          const signer = await createKeyPairSignerFromBytes(privateKeyBytes);

          this.walletCache.set(wallet.wallet_id, {
            keypair,
            signer,
          });
        } catch (error) {
          logger.error(`Failed to load wallet ${wallet.wallet_id}:`, error);
        }
      }

      logger.info(
        `Successfully loaded ${this.walletCache.size} wallets into cache`,
      );
    } catch (error) {
      logger.error("Failed to load wallets from database:", error);
    }
  }

  async createWallet(req: CreateWalletRequest): Promise<CreateWalletResponse> {
    try {
      logger.info("Creating new wallet for user:", req.user_id);

      const keypair = Keypair.generate();
      const signer = await createKeyPairSignerFromBytes(keypair.secretKey);
      const publicKey = keypair.publicKey.toBase58();
      const privateKey = bs58.encode(keypair.secretKey);
      const encryptedPrivateKey = encryptionService.encrypt(privateKey);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      walletDatabase.saveWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        public_key: publicKey,
        encrypted_private_key: encryptedPrivateKey,
        wallet_name: req.wallet_name,
        created_at: createdAt,
        updated_at: createdAt,
      });

      this.walletCache.set(walletId, {
        keypair,
        signer,
      });

      logger.info("Wallet created and persisted:", walletId);

      return {
        wallet_id: walletId,
        public_key: publicKey,
        user_id: req.user_id,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to create wallet:", error);
      throw error;
    }
  }

  async importWallet(req: ImportWalletRequest): Promise<CreateWalletResponse> {
    try {
      logger.info("Importing wallet for user:", req.user_id);

      const privateKeyBytes = bs58.decode(req.private_key);
      const keypair = Keypair.fromSecretKey(privateKeyBytes);
      const signer = await createKeyPairSignerFromBytes(privateKeyBytes);
      const publicKey = keypair.publicKey.toBase58();
      const encryptedPrivateKey = encryptionService.encrypt(req.private_key);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      walletDatabase.saveWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        public_key: publicKey,
        encrypted_private_key: encryptedPrivateKey,
        wallet_name: req.wallet_name,
        created_at: createdAt,
        updated_at: createdAt,
      });

      this.walletCache.set(walletId, {
        keypair,
        signer,
      });

      logger.info("Wallet imported and persisted:", walletId);

      return {
        wallet_id: walletId,
        public_key: publicKey,
        user_id: req.user_id,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to import wallet:", error);
      throw error;
    }
  }

  async getBalance(publicKey: string): Promise<GetBalanceResponse> {
    try {
      logger.info("Getting balance for:", publicKey);

      const balance = await solanaConnection.getBalance(publicKey);
      const LAMPORTS_PER_SOL = 1_000_000_000n;

      return {
        public_key: publicKey,
        balance_lamports: Number(balance.value),
        balance_sol: Number(balance.value) / Number(LAMPORTS_PER_SOL),
      };
    } catch (error) {
      logger.error("Failed to get balance:", error);
      throw error;
    }
  }

  async transfer(req: TransferRequest): Promise<TransferResponse> {
    try {
      logger.info("Processing transfer from wallet:", req.from_wallet_id);

      const cachedWallet = this.walletCache.get(req.from_wallet_id);
      if (!cachedWallet) {
        throw new Error("Wallet not found in cache");
      }

      const signer = cachedWallet.signer;
      const fromAddress = cachedWallet.keypair.publicKey.toBase58();
      const toAddress = req.to_public_key as any;

      const rpc = solanaConnection.getRpc();
      const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

      const { value: latestBlockhash } =
        await solanaConnection.getLatestBlockhash();

      const transferInstruction = getTransferSolInstruction({
        source: signer,
        destination: toAddress,
        amount: lamports(BigInt(req.amount_lamports)),
      });

      const transactionMessage = pipe(
        createTransactionMessage({ version: 0 }),
        (tx) => setTransactionMessageFeePayerSigner(signer, tx),
        (tx) =>
          setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
        (tx) => appendTransactionMessageInstructions([transferInstruction], tx),
      );

      const signedTransaction =
        await signTransactionMessageWithSigners(transactionMessage);

      await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
        signedTransaction,
        { commitment: "confirmed" },
      );

      const signature = getSignatureFromTransaction(signedTransaction);

      logger.info("Transfer successful, signature:", signature);

      return {
        signature,
        from_public_key: fromAddress,
        to_public_key: req.to_public_key,
        amount_lamports: req.amount_lamports,
        status: "confirmed",
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to process transfer:", error);
      throw error;
    }
  }

  async getTransaction(signature: string): Promise<GetTransactionResponse> {
    try {
      logger.info("Getting transaction:", signature);

      const transaction = await solanaConnection.getTransaction(signature);

      if (!transaction) {
        throw new Error("Transaction not found");
      }

      return {
        signature,
        slot: Number(transaction.slot),
        block_time: transaction.blockTime
          ? Number(transaction.blockTime)
          : null,
        status: transaction.meta?.err ? "failed" : "confirmed",
        fee: Number(transaction.meta?.fee || 0),
        meta: transaction.meta,
      };
    } catch (error) {
      logger.error("Failed to get transaction:", error);
      throw error;
    }
  }

  async exportPrivateKey(walletId: string): Promise<string> {
    try {
      const wallet = walletDatabase.getWallet(walletId);
      if (!wallet) {
        throw new Error("Wallet not found");
      }

      const decryptedPrivateKey = encryptionService.decrypt(
        wallet.encrypted_private_key,
      );
      return decryptedPrivateKey;
    } catch (error) {
      logger.error("Failed to export private key:", error);
      throw error;
    }
  }

  async getWalletInfo(walletId: string): Promise<WalletInfo | null> {
    const wallet = walletDatabase.getWallet(walletId);
    if (!wallet) {
      return null;
    }

    return {
      wallet_id: wallet.wallet_id,
      public_key: wallet.public_key,
      user_id: wallet.user_id,
      wallet_name: wallet.wallet_name,
      created_at: wallet.created_at,
    };
  }

  async getUserWallets(userId: string): Promise<WalletInfo[]> {
    const wallets = walletDatabase.getUserWallets(userId);
    return wallets.map((w) => ({
      wallet_id: w.wallet_id,
      public_key: w.public_key,
      user_id: w.user_id,
      wallet_name: w.wallet_name,
      created_at: w.created_at,
    }));
  }

  deleteWallet(walletId: string, userId: string): boolean {
    const success = walletDatabase.deleteWallet(walletId, userId);
    if (success) {
      this.walletCache.delete(walletId);
    }
    return success;
  }

  getWalletSigner(walletId: string): { signer: any; publicKey: string } | null {
    const cachedWallet = this.walletCache.get(walletId);
    if (!cachedWallet) {
      return null;
    }
    return {
      signer: cachedWallet.signer,
      publicKey: cachedWallet.keypair.publicKey.toBase58(),
    };
  }

  private generateWalletId(): string {
    return `wallet_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }
}

export const walletManager = new WalletManager();
