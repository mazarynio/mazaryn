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
  getCreateAssociatedTokenInstruction,
} from "@solana-program/token";
import { solanaConnection } from "./connection.js";
import { logger } from "../../core/logger.js";
import bs58 from "bs58";
import crypto from "crypto";
import type {
  GetNFTsResponse,
  NFT,
  GetNFTMetadataResponse,
  TransferNFTRequest,
  TransferNFTResponse,
  NFTMetadata,
} from "../../core/types.js";

const METADATA_PROGRAM_ID = "metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s";
const MIN_LAMPORTS_BALANCE = 1_000_000;

export class NFTManager {
  async getNFTs(publicKey: string): Promise<GetNFTsResponse> {
    try {
      logger.info("Getting NFTs for:", publicKey);

      const response = await solanaConnection.getParsedTokenAccountsByOwner(
        publicKey,
        TOKEN_PROGRAM_ADDRESS,
      );

      const nftAccounts = response.value.filter((account: any) => {
        const amount = account.account.data.parsed.info.tokenAmount.amount;
        const decimals = account.account.data.parsed.info.tokenAmount.decimals;
        return amount === "1" && decimals === 0;
      });

      const nfts: NFT[] = await Promise.all(
        nftAccounts.map(async (account: any) => {
          const parsedInfo = account.account.data.parsed.info;
          const mint = parsedInfo.mint;

          try {
            const metadata = await this.fetchNFTMetadata(mint);
            return {
              mint,
              token_account: account.pubkey.toString(),
              owner: parsedInfo.owner,
              name: metadata?.name || "Unknown NFT",
              symbol: metadata?.symbol || "",
              uri: metadata?.uri || "",
              metadata: metadata || undefined,
            };
          } catch (error) {
            logger.error("Failed to fetch metadata for NFT:", mint, error);
            return {
              mint,
              token_account: account.pubkey.toString(),
              owner: parsedInfo.owner,
              name: "Unknown NFT",
              symbol: "",
              uri: "",
            };
          }
        }),
      );

      return {
        public_key: publicKey,
        nfts,
      };
    } catch (error) {
      logger.error("Failed to get NFTs:", error);
      throw error;
    }
  }

  async getNFTMetadata(mintAddress: string): Promise<GetNFTMetadataResponse> {
    try {
      logger.info("Getting NFT metadata for:", mintAddress);

      const metadata = await this.fetchNFTMetadata(mintAddress);

      if (!metadata) {
        throw new Error("NFT not found");
      }

      let externalMetadata = undefined;
      if (metadata.uri) {
        try {
          const response = await fetch(metadata.uri);
          if (response.ok) {
            externalMetadata = await response.json();
          }
        } catch (error) {
          logger.error("Failed to fetch external metadata:", error);
        }
      }

      return {
        mint: mintAddress,
        metadata,
        external_metadata: externalMetadata,
      };
    } catch (error) {
      logger.error("Failed to get NFT metadata:", error);
      throw error;
    }
  }

  async transferNFT(
    fromWalletSigner: any,
    fromPublicKey: string,
    req: TransferNFTRequest,
  ): Promise<TransferNFTResponse> {
    try {
      logger.info("Processing NFT transfer from wallet:", req.from_wallet_id);

      await this.checkBalance(fromPublicKey);

      const rpc = solanaConnection.getRpc();
      const rpcSubscriptions = solanaConnection.getRpcSubscriptions();

      const [sourceTokenAccount] = await findAssociatedTokenPda({
        owner: fromPublicKey as any,
        mint: req.mint_address as any,
        tokenProgram: TOKEN_PROGRAM_ADDRESS,
      });

      const [destinationTokenAccount] = await findAssociatedTokenPda({
        owner: req.to_public_key as any,
        mint: req.mint_address as any,
        tokenProgram: TOKEN_PROGRAM_ADDRESS,
      });

      const instructions = [];

      const destAccountInfo = await solanaConnection.getAccountInfo(
        destinationTokenAccount.toString(),
      );

      if (!destAccountInfo?.value) {
        logger.info("Creating associated token account for destination");
        const createAtaInstruction = getCreateAssociatedTokenInstruction({
          payer: fromWalletSigner,
          owner: req.to_public_key as any,
          mint: req.mint_address as any,
        });
        instructions.push(createAtaInstruction);
      }

      const transferInstruction = getTransferInstruction({
        source: sourceTokenAccount,
        destination: destinationTokenAccount,
        authority: fromWalletSigner,
        amount: 1n,
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

      logger.info("NFT transfer successful, signature:", signature);

      return {
        signature,
        from_public_key: fromPublicKey,
        to_public_key: req.to_public_key,
        mint_address: req.mint_address,
        status: "confirmed",
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to process NFT transfer:", error);
      throw error;
    }
  }

  private async fetchNFTMetadata(
    mintAddress: string,
  ): Promise<NFTMetadata | null> {
    try {
      const [metadataPDA] = this.findMetadataPDA(mintAddress);

      const accountInfo = await solanaConnection.getAccountInfo(metadataPDA);

      if (!accountInfo?.value?.data) {
        return null;
      }

      const data = accountInfo.value.data as any;
      const metadata = this.deserializeMetadata(data);

      return metadata;
    } catch (error) {
      logger.error("Failed to fetch NFT metadata:", error);
      return null;
    }
  }

  private findMetadataPDA(mintAddress: string): [string, number] {
    const seeds = [
      Buffer.from("metadata"),
      bs58.decode(METADATA_PROGRAM_ID),
      bs58.decode(mintAddress),
    ];

    const [pda, bump] = this.findProgramAddress(seeds, METADATA_PROGRAM_ID);

    return [pda, bump];
  }

  private findProgramAddress(
    seeds: Buffer[],
    programId: string,
  ): [string, number] {
    const programIdBuffer = bs58.decode(programId);

    for (let bump = 255; bump >= 0; bump--) {
      try {
        const seedsWithBump = [...seeds, Buffer.from([bump])];
        const pda = this.createProgramAddress(seedsWithBump, programIdBuffer);
        return [pda, bump];
      } catch (error) {
        continue;
      }
    }

    throw new Error("Unable to find a viable program address");
  }

  private createProgramAddress(seeds: Buffer[], programId: Buffer): string {
    const buffer = Buffer.concat([
      ...seeds,
      programId,
      Buffer.from("ProgramDerivedAddress"),
    ]);
    const hash = crypto.createHash("sha256").update(buffer).digest();
    return bs58.encode(hash);
  }

  private deserializeMetadata(data: Uint8Array): NFTMetadata | null {
    try {
      let offset = 1;

      const nameLength = data[offset + 3];
      offset += 4;
      const nameBytes = data.slice(offset, offset + nameLength);
      const name = Buffer.from(nameBytes).toString("utf8").replace(/\0/g, "");
      offset += 32;

      const symbolLength = data[offset + 3];
      offset += 4;
      const symbolBytes = data.slice(offset, offset + symbolLength);
      const symbol = Buffer.from(symbolBytes)
        .toString("utf8")
        .replace(/\0/g, "");
      offset += 10;

      const uriLength = data[offset + 3];
      offset += 4;
      const uriBytes = data.slice(offset, offset + uriLength);
      const uri = Buffer.from(uriBytes).toString("utf8").replace(/\0/g, "");
      offset += 200;

      const sellerFeeBasisPoints = data[offset] | (data[offset + 1] << 8);
      offset += 2;

      const hasCreators = data[offset] === 1;
      offset += 1;

      const creators = [];
      if (hasCreators) {
        const creatorCount =
          data[offset] |
          (data[offset + 1] << 8) |
          (data[offset + 2] << 16) |
          (data[offset + 3] << 24);
        offset += 4;

        for (let i = 0; i < creatorCount; i++) {
          const addressBytes = data.slice(offset, offset + 32);
          const address = bs58.encode(Buffer.from(addressBytes));
          offset += 32;

          const verified = data[offset] === 1;
          offset += 1;

          const share = data[offset];
          offset += 1;

          creators.push({ address, verified, share });
        }
      }

      return {
        name,
        symbol,
        uri,
        seller_fee_basis_points: sellerFeeBasisPoints,
        creators,
      };
    } catch (error) {
      logger.error("Failed to deserialize metadata:", error);
      return null;
    }
  }

  private async checkBalance(publicKey: string): Promise<void> {
    const balance = await solanaConnection.getBalance(publicKey);
    const lamports = Number(balance.value);

    if (lamports < MIN_LAMPORTS_BALANCE) {
      throw new Error("Insufficient SOL balance to transfer NFT");
    }
  }
}

export const nftManager = new NFTManager();
