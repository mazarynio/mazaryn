import { createSolanaRpc, createSolanaRpcSubscriptions } from "@solana/kit";
import { config } from "../../config/index.js";
import { logger } from "../../core/logger.js";

export class SolanaConnection {
  private rpc: ReturnType<typeof createSolanaRpc>;
  private rpcSubscriptions: ReturnType<typeof createSolanaRpcSubscriptions>;

  constructor() {
    this.rpc = createSolanaRpc(config.solanaRpcUrl);
    const wsUrl = config.solanaRpcUrl
      .replace("https://", "wss://")
      .replace("http://", "ws://");
    this.rpcSubscriptions = createSolanaRpcSubscriptions(wsUrl);
    logger.info("Solana RPC connection initialized");
  }

  getRpc() {
    return this.rpc;
  }

  getRpcSubscriptions() {
    return this.rpcSubscriptions;
  }

  async getLatestBlockhash() {
    return await this.rpc.getLatestBlockhash().send();
  }

  async getBalance(addressString: string) {
    return await this.rpc.getBalance(addressString as any).send();
  }

  async getTransaction(signature: string) {
    return await this.rpc
      .getTransaction(signature, {
        maxSupportedTransactionVersion: 0,
      })
      .send();
  }

  async getParsedTokenAccountsByOwner(owner: string, programId: string) {
    return await this.rpc
      .getTokenAccountsByOwner(
        owner as any,
        { programId: programId as any },
        { encoding: "jsonParsed" },
      )
      .send();
  }

  async getParsedAccountInfo(address: string) {
    return await this.rpc
      .getAccountInfo(address as any, {
        encoding: "jsonParsed",
      })
      .send();
  }

  async getAccountInfo(address: string) {
    return await this.rpc.getAccountInfo(address as any).send();
  }
}

export const solanaConnection = new SolanaConnection();
