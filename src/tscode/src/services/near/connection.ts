import { JsonRpcProvider, FailoverRpcProvider, yoctoToNear } from "near-api-js";
import { config } from "../../config/index.js";
import { logger } from "../../core/logger.js";

export class NearConnection {
  private provider: JsonRpcProvider | FailoverRpcProvider;

  constructor() {
    if (config.nearFallbackRpcUrls && config.nearFallbackRpcUrls.length > 0) {
      const providers = [
        new JsonRpcProvider(
          { url: config.nearRpcUrl },
          { retries: 3, backoff: 2, wait: 500 },
        ),
        ...config.nearFallbackRpcUrls.map(
          (url) =>
            new JsonRpcProvider({ url }, { retries: 3, backoff: 2, wait: 500 }),
        ),
      ];
      this.provider = new FailoverRpcProvider(providers);
      logger.info(
        `NEAR FailoverRpcProvider initialized with ${providers.length} endpoints`,
      );
    } else {
      this.provider = new JsonRpcProvider(
        { url: config.nearRpcUrl },
        { retries: 3, backoff: 2, wait: 500 },
      );
      logger.info("NEAR RPC connection initialized:", config.nearRpcUrl);
    }
  }

  getProvider(): JsonRpcProvider | FailoverRpcProvider {
    return this.provider;
  }

  async viewAccount(accountId: string): Promise<any> {
    const provider = this.provider as JsonRpcProvider;
    return await provider.viewAccount({ accountId });
  }

  async getAccountState(accountId: string): Promise<any> {
    return await this.viewAccount(accountId);
  }

  async getBalance(accountId: string): Promise<string> {
    const state = await this.viewAccount(accountId);
    return state.amount;
  }

  async viewFunction(params: {
    contractId: string;
    method: string;
    args?: Record<string, unknown>;
  }): Promise<any> {
    const provider = this.provider as JsonRpcProvider;
    return await provider.callFunction({
      contractId: params.contractId,
      method: params.method,
      args: params.args ?? {},
    });
  }

  async getTransaction(txHash: string, accountId: string): Promise<any> {
    const provider = this.provider as JsonRpcProvider;
    return await provider.viewTransactionStatus({ txHash, accountId });
  }

  async getCurrentEpochSeatPrice(): Promise<string> {
    const provider = this.provider as JsonRpcProvider;
    const price = await provider.getCurrentEpochSeatPrice();
    return yoctoToNear(price, 3);
  }

  async getNextEpochSeatPrice(): Promise<string> {
    const provider = this.provider as JsonRpcProvider;
    const price = await provider.getNextEpochSeatPrice();
    return yoctoToNear(price, 3);
  }

  async accountExists(accountId: string): Promise<boolean> {
    try {
      await this.viewAccount(accountId);
      return true;
    } catch {
      return false;
    }
  }

  async sendTransaction(signedTransaction: any): Promise<any> {
    const provider = this.provider as JsonRpcProvider;
    return await provider.sendTransaction(signedTransaction);
  }
}

export const nearConnection = new NearConnection();
