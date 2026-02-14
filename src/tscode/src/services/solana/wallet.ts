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

interface StoredWallet {
  keypair: Keypair;
  signer: Awaited<ReturnType<typeof createKeyPairSignerFromBytes>>;
  userId: string;
  walletName?: string;
  createdAt: number;
}

export class WalletManager {
  private wallets: Map<string, StoredWallet>;
  private userWallets: Map<string, string[]>;

  constructor() {
    this.wallets = new Map();
    this.userWallets = new Map();
  }

  async createWallet(req: CreateWalletRequest): Promise<CreateWalletResponse> {
    try {
      logger.info("Creating new wallet for user:", req.user_id);

      const keypair = Keypair.generate();
      const signer = await createKeyPairSignerFromBytes(keypair.secretKey);
      const publicKey = keypair.publicKey.toBase58();
      const walletId = this.generateWalletId();

      this.wallets.set(walletId, {
        keypair,
        signer,
        userId: req.user_id,
        walletName: req.wallet_name,
        createdAt: Date.now(),
      });

      if (!this.userWallets.has(req.user_id)) {
        this.userWallets.set(req.user_id, []);
      }
      this.userWallets.get(req.user_id)!.push(walletId);

      logger.info("Wallet created successfully:", walletId);

      return {
        wallet_id: walletId,
        public_key: publicKey,
        user_id: req.user_id,
        created_at: Date.now(),
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
      const walletId = this.generateWalletId();

      this.wallets.set(walletId, {
        keypair,
        signer,
        userId: req.user_id,
        walletName: req.wallet_name,
        createdAt: Date.now(),
      });

      if (!this.userWallets.has(req.user_id)) {
        this.userWallets.set(req.user_id, []);
      }
      this.userWallets.get(req.user_id)!.push(walletId);

      logger.info("Wallet imported successfully:", walletId);

      return {
        wallet_id: walletId,
        public_key: publicKey,
        user_id: req.user_id,
        created_at: Date.now(),
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

      const wallet = this.wallets.get(req.from_wallet_id);
      if (!wallet) {
        throw new Error("Wallet not found");
      }

      const signer = wallet.signer;
      const fromAddress = wallet.keypair.publicKey.toBase58();
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
      const wallet = this.wallets.get(walletId);
      if (!wallet) {
        throw new Error("Wallet not found");
      }

      return bs58.encode(wallet.keypair.secretKey);
    } catch (error) {
      logger.error("Failed to export private key:", error);
      throw error;
    }
  }

  async getWalletInfo(walletId: string): Promise<WalletInfo | null> {
    const wallet = this.wallets.get(walletId);
    if (!wallet) {
      return null;
    }

    const publicKey = wallet.keypair.publicKey.toBase58();

    return {
      wallet_id: walletId,
      public_key: publicKey,
      user_id: wallet.userId,
      wallet_name: wallet.walletName,
      created_at: wallet.createdAt,
    };
  }

  async getUserWallets(userId: string): Promise<WalletInfo[]> {
    const walletIds = this.userWallets.get(userId) || [];
    const wallets = await Promise.all(
      walletIds.map((id) => this.getWalletInfo(id)),
    );
    return wallets.filter((info): info is WalletInfo => info !== null);
  }

  deleteWallet(walletId: string, userId: string): boolean {
    const wallet = this.wallets.get(walletId);
    if (!wallet || wallet.userId !== userId) {
      return false;
    }

    this.wallets.delete(walletId);
    const userWalletList = this.userWallets.get(userId);
    if (userWalletList) {
      const index = userWalletList.indexOf(walletId);
      if (index > -1) {
        userWalletList.splice(index, 1);
      }
    }

    return true;
  }

  private generateWalletId(): string {
    return `wallet_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }
}

export const walletManager = new WalletManager();
