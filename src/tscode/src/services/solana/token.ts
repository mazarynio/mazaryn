import {
  createTransactionMessage,
  setTransactionMessageFeePayerSigner,
  setTransactionMessageLifetimeUsingBlockhash,
  appendTransactionMessageInstructions,
  pipe,
  signTransactionMessageWithSigners,
  sendAndConfirmTransactionFactory,
  getSignatureFromTransaction,
} from "@solana/kit";
import {
  TOKEN_PROGRAM_ADDRESS,
  findAssociatedTokenPda,
  getTransferInstruction,
  fetchToken,
  getCreateAssociatedTokenInstruction,
} from "@solana-program/token";
import { solanaConnection } from "./connection.js";
import { logger } from "../../core/logger.js";
import type {
  GetTokenAccountsResponse,
  TokenAccount,
  GetTokenBalanceResponse,
  TransferTokenRequest,
  TransferTokenResponse,
  CreateTokenAccountResponse,
} from "../../core/types.js";

const MIN_SOL_BALANCE = 0.001;
const MIN_LAMPORTS_BALANCE = 1_000_000;

export class TokenManager {
  async getTokenAccounts(publicKey: string): Promise<GetTokenAccountsResponse> {
    try {
      logger.info("Getting token accounts for:", publicKey);

      const response = await solanaConnection.getParsedTokenAccountsByOwner(
        publicKey,
        TOKEN_PROGRAM_ADDRESS,
      );

      const tokenAccounts: TokenAccount[] = response.value.map(
        (account: any) => {
          const parsedInfo = account.account.data.parsed.info;
          return {
            address: account.pubkey.toString(),
            mint: parsedInfo.mint,
            owner: parsedInfo.owner,
            amount: parsedInfo.tokenAmount.amount,
            decimals: parsedInfo.tokenAmount.decimals,
            ui_amount: parsedInfo.tokenAmount.uiAmount || 0,
          };
        },
      );

      return {
        public_key: publicKey,
        token_accounts: tokenAccounts,
      };
    } catch (error) {
      logger.error("Failed to get token accounts:", error);
      throw error;
    }
  }

  async getTokenBalance(
    publicKey: string,
    tokenMint: string,
  ): Promise<GetTokenBalanceResponse> {
    try {
      logger.info("Getting token balance for:", publicKey, "token:", tokenMint);

      const [tokenAccountAddress] = await findAssociatedTokenPda({
        owner: publicKey as any,
        mint: tokenMint as any,
        tokenProgram: TOKEN_PROGRAM_ADDRESS,
      });

      try {
        const accountInfo = await solanaConnection.getParsedAccountInfo(
          tokenAccountAddress.toString(),
        );

        if (!accountInfo?.value) {
          return {
            public_key: publicKey,
            token_mint: tokenMint,
            balance: "0",
            decimals: 0,
            ui_amount: 0,
          };
        }

        const parsedInfo = (accountInfo.value.data as any).parsed.info;

        return {
          public_key: publicKey,
          token_mint: tokenMint,
          balance: parsedInfo.tokenAmount.amount,
          decimals: parsedInfo.tokenAmount.decimals,
          ui_amount: parsedInfo.tokenAmount.uiAmount || 0,
        };
      } catch (error) {
        return {
          public_key: publicKey,
          token_mint: tokenMint,
          balance: "0",
          decimals: 0,
          ui_amount: 0,
        };
      }
    } catch (error) {
      logger.error("Failed to get token balance:", error);
      throw error;
    }
  }

  async transferToken(
    fromWalletSigner: any,
    fromPublicKey: string,
    req: TransferTokenRequest,
  ): Promise<TransferTokenResponse> {
    try {
      logger.info("Processing token transfer from wallet:", req.from_wallet_id);

      await this.checkBalance(fromPublicKey, "transfer tokens");

      const rpc = solanaConnection.getRpc();
      const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

      const [sourceTokenAccount] = await findAssociatedTokenPda({
        owner: fromPublicKey as any,
        mint: req.token_mint as any,
        tokenProgram: TOKEN_PROGRAM_ADDRESS,
      });

      const [destinationTokenAccount] = await findAssociatedTokenPda({
        owner: req.to_public_key as any,
        mint: req.token_mint as any,
        tokenProgram: TOKEN_PROGRAM_ADDRESS,
      });

      const mintInfo = await fetchToken(rpc, req.token_mint as any);
      const decimals = mintInfo.decimals;

      const instructions = [];

      const destAccountInfo = await solanaConnection.getAccountInfo(
        destinationTokenAccount.toString(),
      );

      if (!destAccountInfo?.value) {
        logger.info("Creating associated token account for destination");
        const createAtaInstruction = getCreateAssociatedTokenInstruction({
          payer: fromWalletSigner,
          owner: req.to_public_key as any,
          mint: req.token_mint as any,
        });
        instructions.push(createAtaInstruction);
      }

      const transferInstruction = getTransferInstruction({
        source: sourceTokenAccount,
        destination: destinationTokenAccount,
        authority: fromWalletSigner,
        amount: BigInt(req.amount),
      });
      instructions.push(transferInstruction);

      const { value: latestBlockhash } =
        await solanaConnection.getLatestBlockhash();

      const transactionMessage = pipe(
        createTransactionMessage({ version: 0 }),
        (tx) => setTransactionMessageFeePayerSigner(fromWalletSigner, tx),
        (tx) =>
          setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
        (tx) => appendTransactionMessageInstructions(instructions, tx),
      );

      const signedTransaction =
        await signTransactionMessageWithSigners(transactionMessage);

      await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
        signedTransaction,
        { commitment: "confirmed" },
      );

      const signature = getSignatureFromTransaction(signedTransaction);

      logger.info("Token transfer successful, signature:", signature);

      return {
        signature,
        from_public_key: fromPublicKey,
        to_public_key: req.to_public_key,
        token_mint: req.token_mint,
        amount: req.amount,
        status: "confirmed",
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to process token transfer:", error);
      throw error;
    }
  }

  async createTokenAccount(
    walletSigner: any,
    walletPublicKey: string,
    tokenMint: string,
  ): Promise<CreateTokenAccountResponse> {
    try {
      logger.info(
        "Creating token account for:",
        walletPublicKey,
        "token:",
        tokenMint,
      );

      await this.checkBalance(walletPublicKey, "create token account");

      const rpc = solanaConnection.getRpc();
      const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

      const [tokenAccountAddress] = await findAssociatedTokenPda({
        owner: walletPublicKey as any,
        mint: tokenMint as any,
        tokenProgram: TOKEN_PROGRAM_ADDRESS,
      });

      const accountInfo = await solanaConnection.getAccountInfo(
        tokenAccountAddress.toString(),
      );

      if (accountInfo?.value) {
        logger.info(
          "Token account already exists:",
          tokenAccountAddress.toString(),
        );
        return {
          token_account: tokenAccountAddress.toString(),
          owner: walletPublicKey,
          mint: tokenMint,
          signature: "already_exists",
        };
      }

      const createInstruction = getCreateAssociatedTokenInstruction({
        payer: walletSigner,
        owner: walletPublicKey as any,
        mint: tokenMint as any,
      });

      const { value: latestBlockhash } =
        await solanaConnection.getLatestBlockhash();

      const transactionMessage = pipe(
        createTransactionMessage({ version: 0 }),
        (tx) => setTransactionMessageFeePayerSigner(walletSigner, tx),
        (tx) =>
          setTransactionMessageLifetimeUsingBlockhash(latestBlockhash, tx),
        (tx) => appendTransactionMessageInstructions([createInstruction], tx),
      );

      const signedTransaction =
        await signTransactionMessageWithSigners(transactionMessage);

      await sendAndConfirmTransactionFactory({ rpc, rpcSubscriptions })(
        signedTransaction,
        { commitment: "confirmed" },
      );

      const signature = getSignatureFromTransaction(signedTransaction);

      logger.info("Token account created, signature:", signature);

      return {
        token_account: tokenAccountAddress.toString(),
        owner: walletPublicKey,
        mint: tokenMint,
        signature,
      };
    } catch (error) {
      logger.error("Failed to create token account:", error);
      throw error;
    }
  }

  private async checkBalance(
    publicKey: string,
    operation: string,
  ): Promise<void> {
    const balance = await solanaConnection.getBalance(publicKey);
    const lamports = Number(balance.value);

    if (lamports < MIN_LAMPORTS_BALANCE) {
      const solBalance = (lamports / 1_000_000_000).toFixed(4);
      throw new Error(
        `Insufficient SOL balance to ${operation}. ` +
          `Current balance: ${solBalance} SOL. ` +
          `Required: at least ${MIN_SOL_BALANCE} SOL. ` +
          `Please fund your wallet at https://faucet.solana.com with address: ${publicKey}`,
      );
    }
  }
}

export const tokenManager = new TokenManager();
