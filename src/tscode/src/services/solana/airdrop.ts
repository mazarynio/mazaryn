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
} from "@solana/kit";
import { getTransferSolInstruction } from "@solana-program/system";
import {
  TOKEN_PROGRAM_ADDRESS,
  findAssociatedTokenPda,
  getTransferInstruction,
  getCreateAssociatedTokenInstruction,
} from "@solana-program/token";
import { solanaConnection } from "./connection.js";
import { walletDatabase } from "../database/database.js";
import { logger } from "../../core/logger.js";
import type {
  CreateAirdropRequest,
  CreateAirdropResponse,
  AirdropResult,
  AirdropRecipient,
} from "../../core/types.js";

const MIN_SOL_BALANCE = 1_000_000;
const AIRDROP_DELAY_MS = 200;

export class AirdropManager {
  private generateAirdropId(): string {
    return `airdrop_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }

  private sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  async createAirdrop(
    signer: any,
    fromPublicKey: string,
    userId: string,
    req: CreateAirdropRequest,
  ): Promise<CreateAirdropResponse> {
    try {
      logger.info(
        `Starting ${req.type} airdrop for ${req.recipients.length} recipients`,
      );

      const balance = await solanaConnection.getBalance(fromPublicKey);
      const lamportsBalance = Number(balance.value);

      if (lamportsBalance < MIN_SOL_BALANCE) {
        throw new Error(
          `Insufficient SOL balance. Current: ${lamportsBalance / 1_000_000_000} SOL`,
        );
      }

      const airdropId = this.generateAirdropId();
      const startedAt = Date.now();

      walletDatabase.createAirdrop({
        airdrop_id: airdropId,
        wallet_id: req.wallet_id,
        user_id: userId,
        type: req.type,
        token_mint: req.token_mint,
        total_recipients: req.recipients.length,
        successful: 0,
        failed: 0,
        total_amount: req.amount_per_recipient
          ? req.amount_per_recipient * req.recipients.length
          : undefined,
        status: "processing",
        created_at: startedAt,
      });

      const results: AirdropResult[] = [];

      for (const recipient of req.recipients) {
        try {
          let signature: string;

          if (req.type === "sol") {
            signature = await this.airdropSOL(
              signer,
              fromPublicKey,
              recipient,
              req.amount_per_recipient || 0,
            );
          } else if (req.type === "token") {
            if (!req.token_mint) {
              throw new Error("token_mint is required for token airdrops");
            }
            signature = await this.airdropToken(
              signer,
              fromPublicKey,
              recipient,
              req.token_mint,
              req.amount_per_recipient || 0,
            );
          } else if (req.type === "nft") {
            if (!recipient.mint_address) {
              throw new Error("mint_address is required for NFT airdrops");
            }
            signature = await this.airdropNFT(signer, fromPublicKey, recipient);
          } else {
            throw new Error(`Invalid airdrop type: ${req.type}`);
          }

          results.push({
            recipient: recipient.address,
            success: true,
            signature,
          });

          walletDatabase.createAirdropRecipient({
            airdrop_id: airdropId,
            recipient_address: recipient.address,
            amount: recipient.amount || req.amount_per_recipient,
            mint_address: recipient.mint_address || req.token_mint,
            success: true,
            signature,
            processed_at: Date.now(),
          });

          logger.info(
            `Airdrop to ${recipient.address} successful: ${signature}`,
          );
        } catch (error) {
          const errorMessage =
            error instanceof Error ? error.message : "Unknown error";

          results.push({
            recipient: recipient.address,
            success: false,
            error: errorMessage,
          });

          walletDatabase.createAirdropRecipient({
            airdrop_id: airdropId,
            recipient_address: recipient.address,
            amount: recipient.amount || req.amount_per_recipient,
            mint_address: recipient.mint_address || req.token_mint,
            success: false,
            error: errorMessage,
            processed_at: Date.now(),
          });

          logger.error(`Airdrop to ${recipient.address} failed:`, error);
        }

        await this.sleep(AIRDROP_DELAY_MS);
      }

      const successful = results.filter((r) => r.success).length;
      const failed = results.filter((r) => !r.success).length;
      const completedAt = Date.now();

      walletDatabase.updateAirdrop(airdropId, {
        successful,
        failed,
        status:
          failed === results.length
            ? "failed"
            : successful === results.length
              ? "completed"
              : "partial",
        completed_at: completedAt,
      });

      logger.info(
        `Airdrop ${airdropId} completed. Success: ${successful}, Failed: ${failed}`,
      );

      return {
        airdrop_id: airdropId,
        wallet_id: req.wallet_id,
        type: req.type,
        total_recipients: req.recipients.length,
        successful,
        failed,
        results,
        started_at: startedAt,
        completed_at: completedAt,
        total_amount: req.amount_per_recipient
          ? req.amount_per_recipient * successful
          : undefined,
      };
    } catch (error) {
      logger.error("Airdrop failed:", error);
      throw error;
    }
  }

  private async airdropSOL(
    signer: any,
    fromPublicKey: string,
    recipient: AirdropRecipient,
    amountLamports: number,
  ): Promise<string> {
    const rpc = solanaConnection.getRpc();
    const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

    const { value: latestBlockhash } =
      await solanaConnection.getLatestBlockhash();

    const transferInstruction = getTransferSolInstruction({
      source: signer,
      destination: recipient.address as any,
      amount: lamports(BigInt(amountLamports)),
    });

    const transactionMessage = pipe(
      createTransactionMessage({ version: 0 }),
      (tx) => setTransactionMessageFeePayerSigner(signer, tx),
      (tx) => setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
      (tx) => appendTransactionMessageInstructions([transferInstruction], tx),
    );

    const signedTransaction =
      await signTransactionMessageWithSigners(transactionMessage);

    await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
      signedTransaction,
      { commitment: "confirmed" },
    );

    return getSignatureFromTransaction(signedTransaction);
  }

  private async airdropToken(
    signer: any,
    fromPublicKey: string,
    recipient: AirdropRecipient,
    tokenMint: string,
    amount: number,
  ): Promise<string> {
    const rpc = solanaConnection.getRpc();
    const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

    const [sourceTokenAccount] = await findAssociatedTokenPda({
      owner: fromPublicKey as any,
      mint: tokenMint as any,
      tokenProgram: TOKEN_PROGRAM_ADDRESS,
    });

    const [destinationTokenAccount] = await findAssociatedTokenPda({
      owner: recipient.address as any,
      mint: tokenMint as any,
      tokenProgram: TOKEN_PROGRAM_ADDRESS,
    });

    const instructions = [];

    const destAccountInfo = await solanaConnection.getAccountInfo(
      destinationTokenAccount.toString(),
    );

    if (!destAccountInfo?.value) {
      const createAtaInstruction = getCreateAssociatedTokenInstruction({
        payer: signer,
        owner: recipient.address as any,
        mint: tokenMint as any,
      });
      instructions.push(createAtaInstruction);
    }

    const transferInstruction = getTransferInstruction({
      source: sourceTokenAccount,
      destination: destinationTokenAccount,
      authority: signer,
      amount: BigInt(amount),
    });
    instructions.push(transferInstruction);

    const { value: latestBlockhash } =
      await solanaConnection.getLatestBlockhash();

    const transactionMessage = pipe(
      createTransactionMessage({ version: 0 }),
      (tx) => setTransactionMessageFeePayerSigner(signer, tx),
      (tx) => setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
      (tx) => appendTransactionMessageInstructions(instructions, tx),
    );

    const signedTransaction =
      await signTransactionMessageWithSigners(transactionMessage);

    await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
      signedTransaction,
      { commitment: "confirmed" },
    );

    return getSignatureFromTransaction(signedTransaction);
  }

  private async airdropNFT(
    signer: any,
    fromPublicKey: string,
    recipient: AirdropRecipient,
  ): Promise<string> {
    const rpc = solanaConnection.getRpc();
    const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

    const mintAddress = recipient.mint_address!;

    const [sourceTokenAccount] = await findAssociatedTokenPda({
      owner: fromPublicKey as any,
      mint: mintAddress as any,
      tokenProgram: TOKEN_PROGRAM_ADDRESS,
    });

    const [destinationTokenAccount] = await findAssociatedTokenPda({
      owner: recipient.address as any,
      mint: mintAddress as any,
      tokenProgram: TOKEN_PROGRAM_ADDRESS,
    });

    const instructions = [];

    const destAccountInfo = await solanaConnection.getAccountInfo(
      destinationTokenAccount.toString(),
    );

    if (!destAccountInfo?.value) {
      const createAtaInstruction = getCreateAssociatedTokenInstruction({
        payer: signer,
        owner: recipient.address as any,
        mint: mintAddress as any,
      });
      instructions.push(createAtaInstruction);
    }

    const transferInstruction = getTransferInstruction({
      source: sourceTokenAccount,
      destination: destinationTokenAccount,
      authority: signer,
      amount: 1n,
    });
    instructions.push(transferInstruction);

    const { value: latestBlockhash } =
      await solanaConnection.getLatestBlockhash();

    const transactionMessage = pipe(
      createTransactionMessage({ version: 0 }),
      (tx) => setTransactionMessageFeePayerSigner(signer, tx),
      (tx) => setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
      (tx) => appendTransactionMessageInstructions(instructions, tx),
    );

    const signedTransaction =
      await signTransactionMessageWithSigners(transactionMessage);

    await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
      signedTransaction,
      { commitment: "confirmed" },
    );

    return getSignatureFromTransaction(signedTransaction);
  }

  async getAirdrop(airdropId: string): Promise<any> {
    const airdrop = walletDatabase.getAirdrop(airdropId);
    if (!airdrop) {
      throw new Error("Airdrop not found");
    }

    const recipients = walletDatabase.getAirdropRecipients(airdropId);

    return {
      ...airdrop,
      recipients: recipients.map((r) => ({
        recipient_address: r.recipient_address,
        amount: r.amount,
        mint_address: r.mint_address,
        success: r.success === true || (r.success as any) === 1,
        signature: r.signature,
        error: r.error,
        processed_at: r.processed_at,
      })),
    };
  }

  async getUserAirdrops(userId: string): Promise<any[]> {
    const airdrops = walletDatabase.getUserAirdrops(userId);
    return airdrops;
  }

  async getWalletAirdrops(walletId: string): Promise<any[]> {
    const airdrops = walletDatabase.getWalletAirdrops(walletId);
    return airdrops;
  }
}

export const airdropManager = new AirdropManager();
