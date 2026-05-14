import {
  Aptos,
  AptosConfig,
  Network,
  NetworkToNetworkName,
} from "@aptos-labs/ts-sdk";
import { config } from "../../config/index.js";
import { logger } from "../../core/logger.js";

function resolveNetwork(networkStr: string): Network {
  const map: Record<string, Network> = {
    mainnet: Network.MAINNET,
    testnet: Network.TESTNET,
    devnet: Network.DEVNET,
    local: Network.LOCAL,
  };
  return map[networkStr] ?? Network.TESTNET;
}

export class AptosConnection {
  private clientMap: Map<string, Aptos> = new Map();

  getClient(networkStr?: string): Aptos {
    const net = networkStr ?? config.aptosNetwork;
    const existing = this.clientMap.get(net);
    if (existing) return existing;

    const network = resolveNetwork(net);
    const aptosConfig = new AptosConfig({ network });
    const client = new Aptos(aptosConfig);
    this.clientMap.set(net, client);
    logger.info(`Aptos client initialized for network: ${net}`);
    return client;
  }

  async getLedgerInfo(networkStr?: string): Promise<any> {
    return this.getClient(networkStr).getLedgerInfo();
  }

  async getAccountInfo(
    accountAddress: string,
    networkStr?: string,
  ): Promise<any> {
    return this.getClient(networkStr).getAccountInfo({ accountAddress });
  }

  async getAccountAPTBalance(
    accountAddress: string,
    networkStr?: string,
  ): Promise<number> {
    return this.getClient(networkStr).getAccountAPTAmount({ accountAddress });
  }

  async getAccountFABalance(
    accountAddress: string,
    metadataAddress: string,
    networkStr?: string,
  ): Promise<number> {
    const client = this.getClient(networkStr);
    const balances = await client.getCurrentFungibleAssetBalances({
      options: {
        where: {
          owner_address: { _eq: accountAddress },
          asset_type: { _eq: metadataAddress },
        },
      },
    });
    return balances[0]?.amount ?? 0;
  }

  async getOwnedTokens(
    accountAddress: string,
    networkStr?: string,
  ): Promise<any[]> {
    return this.getClient(networkStr).getAccountOwnedTokens({ accountAddress });
  }

  async getTransaction(
    transactionHash: string,
    networkStr?: string,
  ): Promise<any> {
    return this.getClient(networkStr).getTransactionByHash({ transactionHash });
  }

  async getAccountTransactions(
    accountAddress: string,
    networkStr?: string,
  ): Promise<any[]> {
    return this.getClient(networkStr).getAccountTransactions({
      accountAddress,
    });
  }

  async estimateGasPrice(networkStr?: string): Promise<any> {
    return this.getClient(networkStr).getGasPriceEstimation();
  }

  async getBlockByHeight(
    blockHeight: number,
    networkStr?: string,
  ): Promise<any> {
    return this.getClient(networkStr).getBlockByHeight({ blockHeight });
  }
}

export const aptosConnection = new AptosConnection();
