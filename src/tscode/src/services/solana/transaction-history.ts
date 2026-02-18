import { solanaConnection } from "./connection.js";
import { logger } from "../../core/logger.js";
import type {
  TransactionHistoryRequest,
  TransactionHistoryResponse,
  TransactionStatsResponse,
  ParsedTransaction,
} from "../../core/types.js";

const LAMPORTS_PER_SOL = 1_000_000_000;
const BATCH_SIZE = 5;
const BATCH_DELAY_MS = 100;

export class TransactionHistoryManager {
  private sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  async getTransactionHistory(
    req: TransactionHistoryRequest,
  ): Promise<TransactionHistoryResponse> {
    try {
      logger.info("Getting transaction history for:", req.public_key);

      const limit = Math.min(req.limit || 20, 100);
      const rpc = solanaConnection.getRpc();

      const options: any = {
        limit,
        commitment: "confirmed",
      };

      if (req.before) {
        options.before = req.before;
      }

      if (req.until) {
        options.until = req.until;
      }

      const signatures = await rpc
        .getSignaturesForAddress(req.public_key as any, options)
        .send();

      if (!signatures || signatures.length === 0) {
        return {
          public_key: req.public_key,
          transactions: [],
          total: 0,
          has_more: false,
        };
      }

      const transactions: ParsedTransaction[] = [];

      for (let i = 0; i < signatures.length; i += BATCH_SIZE) {
        const batch = signatures.slice(i, i + BATCH_SIZE);

        const batchResults = await Promise.all(
          batch.map(async (sigInfo) => {
            try {
              const tx = await Promise.race([
                rpc
                  .getTransaction(sigInfo.signature as any, {
                    maxSupportedTransactionVersion: 0,
                    encoding: "jsonParsed" as any,
                  })
                  .send(),
                new Promise<null>((_, reject) =>
                  setTimeout(() => reject(new Error("timeout")), 8000),
                ),
              ]);

              if (!tx) {
                return {
                  signature: sigInfo.signature,
                  block_time: sigInfo.blockTime
                    ? Number(sigInfo.blockTime)
                    : null,
                  slot: Number(sigInfo.slot),
                  status: sigInfo.err ? "failed" : "confirmed",
                  fee: 0,
                  type: "unknown",
                  error: sigInfo.err ? JSON.stringify(sigInfo.err) : undefined,
                };
              }

              return this.parseTransaction(
                sigInfo.signature,
                tx,
                req.public_key,
              );
            } catch (error) {
              return {
                signature: sigInfo.signature,
                block_time: sigInfo.blockTime
                  ? Number(sigInfo.blockTime)
                  : null,
                slot: Number(sigInfo.slot),
                status: sigInfo.err ? "failed" : "confirmed",
                fee: 0,
                type: "unknown",
                error: sigInfo.err ? JSON.stringify(sigInfo.err) : undefined,
              };
            }
          }),
        );

        transactions.push(...batchResults);

        if (i + BATCH_SIZE < signatures.length) {
          await this.sleep(BATCH_DELAY_MS);
        }
      }

      const hasMore = signatures.length === limit;
      const oldestSignature =
        signatures.length > 0
          ? signatures[signatures.length - 1].signature
          : undefined;

      return {
        public_key: req.public_key,
        transactions,
        total: transactions.length,
        has_more: hasMore,
        oldest_signature: oldestSignature,
      };
    } catch (error) {
      logger.error("Failed to get transaction history:", error);
      throw error;
    }
  }

  async getTransactionStats(
    publicKey: string,
  ): Promise<TransactionStatsResponse> {
    try {
      logger.info("Getting transaction stats for:", publicKey);

      const rpc = solanaConnection.getRpc();

      const signatures = await Promise.race([
        rpc
          .getSignaturesForAddress(publicKey as any, {
            limit: 100,
            commitment: "confirmed",
          })
          .send(),
        new Promise<never>((_, reject) =>
          setTimeout(() => reject(new Error("timeout")), 15000),
        ),
      ]);

      if (!signatures || signatures.length === 0) {
        return {
          public_key: publicKey,
          total_transactions: 0,
          total_sent_sol: 0,
          total_received_sol: 0,
          total_fees_paid: 0,
        };
      }

      let totalSentSol = 0;
      let totalReceivedSol = 0;
      let totalFeesPaid = 0;

      for (let i = 0; i < signatures.length; i += BATCH_SIZE) {
        const batch = signatures.slice(i, i + BATCH_SIZE);

        await Promise.all(
          batch.map(async (sigInfo) => {
            try {
              const tx = await Promise.race([
                rpc
                  .getTransaction(sigInfo.signature as any, {
                    maxSupportedTransactionVersion: 0,
                  })
                  .send(),
                new Promise<null>((_, reject) =>
                  setTimeout(() => reject(new Error("timeout")), 5000),
                ),
              ]);

              if (!tx || !tx.meta) return;

              const fee = Number(tx.meta.fee || 0);
              totalFeesPaid += fee;

              const accountKeys =
                tx.transaction?.message?.accountKeys ||
                (tx.transaction?.message as any)?.staticAccountKeys ||
                [];

              let userIndex = -1;
              for (let j = 0; j < accountKeys.length; j++) {
                const key = accountKeys[j];
                const keyStr =
                  typeof key === "string"
                    ? key
                    : key?.pubkey?.toString() || key?.toString();
                if (keyStr === publicKey) {
                  userIndex = j;
                  break;
                }
              }

              if (userIndex >= 0) {
                const preBalance = Number(
                  tx.meta.preBalances?.[userIndex] || 0,
                );
                const postBalance = Number(
                  tx.meta.postBalances?.[userIndex] || 0,
                );
                const diff = postBalance - preBalance + fee;

                if (diff < 0) {
                  totalSentSol += Math.abs(diff) / LAMPORTS_PER_SOL;
                } else if (diff > 0) {
                  totalReceivedSol += diff / LAMPORTS_PER_SOL;
                }
              }
            } catch (error) {
              logger.error("Failed to process transaction for stats:", error);
            }
          }),
        );

        if (i + BATCH_SIZE < signatures.length) {
          await this.sleep(BATCH_DELAY_MS);
        }
      }

      const firstTransaction =
        signatures.length > 0
          ? signatures[signatures.length - 1].signature
          : undefined;

      const lastTransaction =
        signatures.length > 0 ? signatures[0].signature : undefined;

      return {
        public_key: publicKey,
        total_transactions: signatures.length,
        total_sent_sol: totalSentSol,
        total_received_sol: totalReceivedSol,
        total_fees_paid: totalFeesPaid / LAMPORTS_PER_SOL,
        first_transaction: firstTransaction,
        last_transaction: lastTransaction,
      };
    } catch (error) {
      logger.error("Failed to get transaction stats:", error);
      throw error;
    }
  }

  private parseTransaction(
    signature: string,
    tx: any,
    userPublicKey: string,
  ): ParsedTransaction {
    try {
      const blockTime = tx.blockTime ? Number(tx.blockTime) : null;
      const slot = Number(tx.slot || 0);
      const fee = Number(tx.meta?.fee || 0);
      const status = tx.meta?.err ? "failed" : "confirmed";
      const error = tx.meta?.err ? JSON.stringify(tx.meta.err) : undefined;

      const message = tx.transaction?.message;
      if (!message) {
        return {
          signature,
          block_time: blockTime,
          slot,
          status,
          fee,
          type: "unknown",
          error,
        };
      }

      const instructions = message.instructions || [];
      const accountKeys =
        message.accountKeys || message.staticAccountKeys || [];

      const type = this.detectTransactionType(instructions, accountKeys);

      const { amount, from, to } = this.extractTransferInfo(
        instructions,
        accountKeys,
        tx.meta,
        userPublicKey,
        type,
      );

      const { tokenMint, tokenAmount, tokenDecimals } = this.extractTokenInfo(
        instructions,
        accountKeys,
        tx.meta,
        userPublicKey,
      );

      const memo = this.extractMemo(instructions);

      return {
        signature,
        block_time: blockTime,
        slot,
        status,
        fee,
        type,
        amount,
        from,
        to,
        token_mint: tokenMint,
        token_amount: tokenAmount,
        token_decimals: tokenDecimals,
        memo,
        error,
      };
    } catch (error) {
      logger.error("Failed to parse transaction details:", error);
      return {
        signature,
        block_time: tx.blockTime ? Number(tx.blockTime) : null,
        slot: Number(tx.slot || 0),
        status: tx.meta?.err ? "failed" : "confirmed",
        fee: Number(tx.meta?.fee || 0),
        type: "unknown",
      };
    }
  }

  private detectTransactionType(
    instructions: any[],
    accountKeys: any[],
  ): string {
    if (!instructions || instructions.length === 0) {
      return "unknown";
    }

    for (const instruction of instructions) {
      const programId =
        instruction.programId?.toString() ||
        (typeof instruction.programIdIndex === "number"
          ? accountKeys[instruction.programIdIndex]?.toString() ||
            accountKeys[instruction.programIdIndex]?.pubkey?.toString()
          : null);

      if (!programId) continue;

      if (programId === "11111111111111111111111111111111") {
        return "sol_transfer";
      }

      if (
        programId === "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA" ||
        programId === "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb"
      ) {
        const parsed = instruction.parsed;
        if (parsed?.type === "transfer" || parsed?.type === "transferChecked") {
          return "token_transfer";
        }
        if (parsed?.type === "mintTo" || parsed?.type === "mintToChecked") {
          return "token_mint";
        }
        if (parsed?.type === "burn" || parsed?.type === "burnChecked") {
          return "token_burn";
        }
        if (parsed?.type === "createAccount") {
          return "token_account_create";
        }
        return "token_operation";
      }

      if (programId === "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJe8bv8") {
        return "token_account_create";
      }

      if (programId === "metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s") {
        return "nft_operation";
      }

      if (programId === "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4") {
        return "token_swap";
      }

      if (programId === "MarBmsSgKXdrN1egZf5sqe1TMai9K1rChYNDJgjq7aD") {
        return "staking";
      }

      if (programId === "Vote111111111111111111111111111111111111111") {
        return "vote";
      }
    }

    return "unknown";
  }

  private extractTransferInfo(
    instructions: any[],
    accountKeys: any[],
    meta: any,
    userPublicKey: string,
    type: string,
  ): { amount?: number; from?: string; to?: string } {
    try {
      if (type === "sol_transfer") {
        for (const instruction of instructions) {
          const parsed = instruction.parsed;
          if (
            parsed?.type === "transfer" &&
            parsed?.info?.lamports !== undefined
          ) {
            return {
              amount: Number(parsed.info.lamports),
              from: parsed.info.source,
              to: parsed.info.destination,
            };
          }
        }

        if (meta?.preBalances && meta?.postBalances) {
          let userIndex = -1;
          for (let i = 0; i < accountKeys.length; i++) {
            const key = accountKeys[i];
            const keyStr =
              typeof key === "string"
                ? key
                : key?.pubkey?.toString() || key?.toString();
            if (keyStr === userPublicKey) {
              userIndex = i;
              break;
            }
          }

          if (userIndex >= 0) {
            const diff =
              Number(meta.postBalances[userIndex]) -
              Number(meta.preBalances[userIndex]);
            return {
              amount: Math.abs(diff),
              from: diff < 0 ? userPublicKey : undefined,
              to: diff > 0 ? userPublicKey : undefined,
            };
          }
        }
      }

      if (type === "token_transfer") {
        for (const instruction of instructions) {
          const parsed = instruction.parsed;
          if (
            parsed?.type === "transfer" ||
            parsed?.type === "transferChecked"
          ) {
            return {
              from: parsed.info?.source || parsed.info?.authority,
              to: parsed.info?.destination,
            };
          }
        }
      }

      return {};
    } catch (error) {
      return {};
    }
  }

  private extractTokenInfo(
    instructions: any[],
    accountKeys: any[],
    meta: any,
    userPublicKey: string,
  ): {
    tokenMint?: string;
    tokenAmount?: number;
    tokenDecimals?: number;
  } {
    try {
      for (const instruction of instructions) {
        const parsed = instruction.parsed;
        if (!parsed) continue;

        if (parsed.type === "transfer" || parsed.type === "transferChecked") {
          if (parsed.info?.mint) {
            return {
              tokenMint: parsed.info.mint,
              tokenAmount:
                parsed.info.tokenAmount?.uiAmount ||
                Number(parsed.info.amount || 0),
              tokenDecimals: parsed.info.tokenAmount?.decimals,
            };
          }
        }

        if (parsed.type === "mintTo" || parsed.type === "mintToChecked") {
          return {
            tokenMint: parsed.info?.mint,
            tokenAmount:
              parsed.info?.tokenAmount?.uiAmount ||
              Number(parsed.info?.amount || 0),
            tokenDecimals: parsed.info?.tokenAmount?.decimals,
          };
        }
      }

      if (meta?.preTokenBalances && meta?.postTokenBalances) {
        for (const postBalance of meta.postTokenBalances) {
          const preBalance = meta.preTokenBalances.find(
            (b: any) => b.accountIndex === postBalance.accountIndex,
          );

          const preAmount = preBalance
            ? Number(preBalance.uiTokenAmount?.uiAmount || 0)
            : 0;
          const postAmount = Number(postBalance.uiTokenAmount?.uiAmount || 0);

          if (postAmount !== preAmount) {
            return {
              tokenMint: postBalance.mint,
              tokenAmount: Math.abs(postAmount - preAmount),
              tokenDecimals: postBalance.uiTokenAmount?.decimals,
            };
          }
        }
      }

      return {};
    } catch (error) {
      return {};
    }
  }

  private extractMemo(instructions: any[]): string | undefined {
    try {
      for (const instruction of instructions) {
        const programId = instruction.programId?.toString();
        if (
          programId === "MemoSq4gqABAXKb96qnH8TysNcWxMyWCqXgDLGmfcHr" ||
          programId === "Memo1UhkJRfHyvLMcVucJwxXeuD728EqVDDwQDxFMNo"
        ) {
          if (instruction.parsed) {
            return instruction.parsed;
          }
          if (instruction.data) {
            try {
              return Buffer.from(instruction.data, "base64").toString("utf8");
            } catch {
              return instruction.data;
            }
          }
        }
      }
      return undefined;
    } catch (error) {
      return undefined;
    }
  }
}

export const transactionHistoryManager = new TransactionHistoryManager();
