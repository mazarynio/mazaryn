import { aptosConnection } from "./connection.js";
import { aptosAccountManager } from "./account.js";
import { aptosDatabase } from "../database/aptos-database.js";
import { logger } from "../../core/logger.js";
import type {
  AptosCreateCollectionRequest,
  AptosCreateCollectionResponse,
  AptosMintNFTRequest,
  AptosMintNFTResponse,
  AptosTransferNFTRequest,
  AptosBurnNFTRequest,
  AptosTransferResponse,
} from "../../core/aptos-types.js";

export class AptosNFTManager {
  async createCollection(
    req: AptosCreateCollectionRequest,
    networkStr?: string,
  ): Promise<AptosCreateCollectionResponse> {
    try {
      logger.info(
        `Creating collection "${req.name}" for creator: ${req.account_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.account_address);
      if (!wallet) {
        throw new Error("Creator wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Creator account not loaded in cache");
      }

      const client = aptosConnection.getClient(networkStr);

      const transaction = await client.createCollectionTransaction({
        creator: cached.account,
        description: req.description,
        name: req.name,
        uri: req.uri,
        options: {
          maxSupply: req.max_supply,
          mutableDescription: req.mutable_description ?? true,
          mutableUri: req.mutable_uri ?? true,
          mutableTokenDescription: req.mutable_token_description ?? true,
          mutableTokenName: req.mutable_token_name ?? true,
          mutableTokenProperties: req.mutable_token_properties ?? true,
          mutableTokenUri: req.mutable_token_uri ?? true,
          tokensBurnableByCreator: req.tokens_burnable_by_creator ?? false,
          tokensFreezableByCreator: req.tokens_freezable_by_creator ?? false,
          royaltyNumerator: req.royalty_numerator ?? 0,
          royaltyDenominator: req.royalty_denominator ?? 100,
        },
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const response = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);

      logger.info(`Collection created: ${req.name}, tx: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        collection_name: req.name,
        creator_address: req.account_address,
        version: String((response as any).version ?? ""),
        success: (response as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to create collection:", error);
      throw error;
    }
  }

  async mintToken(
    req: AptosMintNFTRequest,
    networkStr?: string,
  ): Promise<AptosMintNFTResponse> {
    try {
      logger.info(
        `Minting token "${req.name}" in collection "${req.collection}"`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.account_address);
      if (!wallet) {
        throw new Error("Creator wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Creator account not loaded in cache");
      }

      const client = aptosConnection.getClient(networkStr);

      const transaction = await client.mintDigitalAssetTransaction({
        creator: cached.account,
        collection: req.collection,
        description: req.description,
        name: req.name,
        uri: req.uri,
        propertyKeys: req.property_keys ?? [],
        propertyTypes: req.property_types ?? [],
        propertyValues: req.property_values ?? [],
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const response = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);

      const tokens = await client.getAccountOwnedTokens({
        accountAddress: req.account_address,
        minimumLedgerVersion: BigInt((response as any).version ?? 0),
      });

      const mintedToken = tokens.find(
        (t: any) =>
          t.current_token_data?.collection_name === req.collection &&
          t.current_token_data?.name === req.name,
      );

      const tokenAddress =
        (mintedToken as any)?.storage_id ??
        (mintedToken as any)?.current_token_data?.token_data_id ??
        "";

      logger.info(`Token minted: ${req.name}, tx: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        token_address: tokenAddress,
        collection_name: req.collection,
        token_name: req.name,
        creator_address: req.account_address,
        version: String((response as any).version ?? ""),
        success: (response as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to mint token:", error);
      throw error;
    }
  }

  async transferToken(
    req: AptosTransferNFTRequest,
    networkStr?: string,
  ): Promise<AptosTransferResponse> {
    try {
      logger.info(
        `Transferring token ${req.token_address} from ${req.account_address} to ${req.recipient_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.account_address);
      if (!wallet) {
        throw new Error("Sender wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Sender account not loaded in cache");
      }

      const client = aptosConnection.getClient(networkStr ?? req.network);

      const transaction = await client.transferDigitalAssetTransaction({
        sender: cached.account,
        digitalAssetAddress: req.token_address,
        recipient: req.recipient_address,
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);

      logger.info(`Token transferred: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        sender_address: req.account_address,
        recipient_address: req.recipient_address,
        amount_octas: 0,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to transfer token:", error);
      throw error;
    }
  }

  async burnToken(
    req: AptosBurnNFTRequest,
    networkStr?: string,
  ): Promise<AptosTransferResponse> {
    try {
      logger.info(
        `Burning token ${req.token_address} owned by ${req.account_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.account_address);
      if (!wallet) {
        throw new Error("Owner wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Owner account not loaded in cache");
      }

      const client = aptosConnection.getClient(networkStr ?? req.network);

      const transaction = await client.burnDigitalAssetTransaction({
        creator: cached.account,
        digitalAssetAddress: req.token_address,
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);

      logger.info(`Token burned: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        sender_address: req.account_address,
        recipient_address: "",
        amount_octas: 0,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to burn token:", error);
      throw error;
    }
  }

  async getOwnedTokens(
    accountAddress: string,
    networkStr?: string,
  ): Promise<any[]> {
    try {
      logger.info("Getting owned tokens for:", accountAddress);
      return aptosConnection.getOwnedTokens(accountAddress, networkStr);
    } catch (error) {
      logger.error("Failed to get owned tokens:", error);
      throw error;
    }
  }
}

export const aptosNFTManager = new AptosNFTManager();
