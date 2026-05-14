import { SuiGrpcClient } from "@mysten/sui/grpc";
import { SuiGraphQLClient } from "@mysten/sui/graphql";
import { getFaucetHost, requestSuiFromFaucetV2 } from "@mysten/sui/faucet";
import { MIST_PER_SUI } from "@mysten/sui/utils";
import type { SuiNetwork } from "./sui-types.js";

const GRPC_ENDPOINTS: Record<SuiNetwork, string> = {
  mainnet: "https://fullnode.mainnet.sui.io:443",
  testnet: "https://fullnode.testnet.sui.io:443",
  devnet: "https://fullnode.devnet.sui.io:443",
  localnet: "http://127.0.0.1:9000",
};

const GRAPHQL_ENDPOINTS: Record<SuiNetwork, string> = {
  mainnet: "https://graphql.mainnet.sui.io/graphql",
  testnet: "https://graphql.testnet.sui.io/graphql",
  devnet: "https://graphql.devnet.sui.io/graphql",
  localnet: "http://127.0.0.1:9125/graphql",
};

const grpcClientCache = new Map<SuiNetwork, SuiGrpcClient>();
const graphqlClientCache = new Map<SuiNetwork, SuiGraphQLClient>();

export function getSuiGrpcClient(
  network: SuiNetwork = "mainnet",
): SuiGrpcClient {
  const cached = grpcClientCache.get(network);
  if (cached) return cached;

  const baseUrl =
    process.env[`SUI_GRPC_URL_${network.toUpperCase()}`] ??
    GRPC_ENDPOINTS[network];

  const client = new SuiGrpcClient({
    network,
    baseUrl,
  });

  grpcClientCache.set(network, client);
  return client;
}

export function getSuiGraphQLClient(
  network: SuiNetwork = "mainnet",
): SuiGraphQLClient {
  const cached = graphqlClientCache.get(network);
  if (cached) return cached;

  const url =
    process.env[`SUI_GRAPHQL_URL_${network.toUpperCase()}`] ??
    GRAPHQL_ENDPOINTS[network];

  const client = new SuiGraphQLClient({ url });
  graphqlClientCache.set(network, client);
  return client;
}

export function mistToSui(mistBalance: string | bigint): string {
  return ((BigInt(mistBalance) * 100n) / MIST_PER_SUI)
    .toString()
    .replace(/(\d+)(\d{2})$/, "$1.$2");
}

export function suiToMist(suiAmount: string | number): bigint {
  const [whole, frac = ""] = String(suiAmount).split(".");
  const fracPadded = frac.padEnd(9, "0").slice(0, 9);
  return BigInt(whole) * MIST_PER_SUI + BigInt(fracPadded);
}

export function normalizeSuiAddress(address: string): string {
  if (!address.startsWith("0x")) return `0x${address}`;
  if (address.length < 66) return `0x${address.slice(2).padStart(64, "0")}`;
  return address;
}

export async function getSuiServiceInfo(network: SuiNetwork = "mainnet") {
  const client = getSuiGrpcClient(network);
  const { response } = await client.ledgerService.getServiceInfo({});
  return {
    chain: response.chain ?? "",
    chain_id: response.chainId ?? "",
    checkpoint_height: String(response.checkpointHeight ?? 0),
    epoch: String(response.epoch ?? 0),
    lowest_available_checkpoint: String(
      response.lowestAvailableCheckpoint ?? 0,
    ),
    server: response.server ?? "",
    timestamp: Number(response.timestamp?.seconds ?? 0) * 1000,
  };
}

export async function getSuiObject(
  objectId: string,
  network: SuiNetwork = "mainnet",
  includeContent = false,
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.ledgerService.getObject({
    objectId: normalizeSuiAddress(objectId),
    readMask: {
      paths: [
        "object_id",
        "version",
        "digest",
        "object_type",
        "owner",
        "previous_transaction",
        "storage_rebate",
        "balance",
        ...(includeContent ? ["contents", "json"] : []),
      ],
    },
  });
  return response.object ?? null;
}

export async function batchGetSuiObjects(
  objectIds: string[],
  network: SuiNetwork = "mainnet",
  includeContent = false,
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.ledgerService.batchGetObjects({
    requests: objectIds.map((id) => ({ objectId: normalizeSuiAddress(id) })),
    readMask: {
      paths: [
        "object_id",
        "version",
        "digest",
        "object_type",
        "owner",
        "previous_transaction",
        "storage_rebate",
        "balance",
        ...(includeContent ? ["contents", "json"] : []),
      ],
    },
  });
  return response.objects ?? [];
}

export async function getSuiTransaction(
  digest: string,
  network: SuiNetwork = "mainnet",
  opts: { effects?: boolean; events?: boolean; bcs?: boolean } = {},
) {
  const client = getSuiGrpcClient(network);
  const paths = ["digest", "checkpoint", "timestamp"];
  if (opts.effects !== false) paths.push("effects");
  if (opts.events) paths.push("events");
  if (opts.bcs) paths.push("transaction.bcs");
  paths.push("transaction.sender", "signatures");

  const { response } = await client.ledgerService.getTransaction({
    digest,
    readMask: { paths },
  });
  return response.transaction ?? null;
}

export async function batchGetSuiTransactions(
  digests: string[],
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.ledgerService.batchGetTransactions({
    digests,
    readMask: {
      paths: [
        "digest",
        "effects",
        "checkpoint",
        "timestamp",
        "transaction.sender",
      ],
    },
  });
  return response.transactions ?? [];
}

export async function getSuiCheckpoint(
  opts: { sequenceNumber?: string; digest?: string },
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const req: Record<string, unknown> = {
    readMask: { paths: ["sequence_number", "digest", "summary", "signature"] },
  };
  if (opts.sequenceNumber !== undefined)
    req["sequenceNumber"] = BigInt(opts.sequenceNumber);
  if (opts.digest !== undefined) req["digest"] = opts.digest;

  const { response } = await client.ledgerService.getCheckpoint(
    req as Parameters<typeof client.ledgerService.getCheckpoint>[0],
  );
  return response.checkpoint ?? null;
}

export async function getSuiEpoch(
  epoch?: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const req: Record<string, unknown> = {
    readMask: {
      paths: [
        "epoch",
        "reference_gas_price",
        "start",
        "end",
        "first_checkpoint",
        "last_checkpoint",
        "system_state",
        "committee",
      ],
    },
  };
  if (epoch !== undefined) req["epoch"] = BigInt(epoch);

  const { response } = await client.ledgerService.getEpoch(
    req as Parameters<typeof client.ledgerService.getEpoch>[0],
  );
  return response.epoch ?? null;
}

export async function listOwnedObjects(
  ownerAddress: string,
  network: SuiNetwork = "mainnet",
  opts: { objectType?: string; pageSize?: number; pageToken?: Uint8Array } = {},
) {
  const client = getSuiGrpcClient(network);
  const req: Record<string, unknown> = {
    owner: normalizeSuiAddress(ownerAddress),
    readMask: {
      paths: [
        "object_id",
        "version",
        "digest",
        "object_type",
        "owner",
        "balance",
      ],
    },
  };
  if (opts.objectType) req["objectType"] = opts.objectType;
  if (opts.pageSize) req["pageSize"] = opts.pageSize;
  if (opts.pageToken) req["pageToken"] = opts.pageToken;

  const { response } = await client.stateService.listOwnedObjects(
    req as Parameters<typeof client.stateService.listOwnedObjects>[0],
  );
  return response;
}

export async function getSuiBalance(
  ownerAddress: string,
  coinType: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.stateService.getBalance({
    owner: normalizeSuiAddress(ownerAddress),
    coinType,
  });
  return response.balance ?? null;
}

export async function listSuiBalances(
  ownerAddress: string,
  network: SuiNetwork = "mainnet",
  opts: { pageSize?: number; pageToken?: Uint8Array } = {},
) {
  const client = getSuiGrpcClient(network);
  const req: Record<string, unknown> = {
    owner: normalizeSuiAddress(ownerAddress),
  };
  if (opts.pageSize) req["pageSize"] = opts.pageSize;
  if (opts.pageToken) req["pageToken"] = opts.pageToken;

  const { response } = await client.stateService.listBalances(
    req as Parameters<typeof client.stateService.listBalances>[0],
  );
  return response;
}

export async function getSuiCoinInfo(
  coinType: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.stateService.getCoinInfo({ coinType });
  return response;
}

export async function listDynamicFields(
  parentId: string,
  network: SuiNetwork = "mainnet",
  opts: { pageSize?: number; pageToken?: Uint8Array } = {},
) {
  const client = getSuiGrpcClient(network);
  const req: Record<string, unknown> = {
    parent: normalizeSuiAddress(parentId),
    readMask: {
      paths: ["field_id", "parent", "name", "value_type", "child_id"],
    },
  };
  if (opts.pageSize) req["pageSize"] = opts.pageSize;
  if (opts.pageToken) req["pageToken"] = opts.pageToken;

  const { response } = await client.stateService.listDynamicFields(
    req as Parameters<typeof client.stateService.listDynamicFields>[0],
  );
  return response;
}

export async function executeTransaction(
  transactionBytes: Uint8Array,
  signatures: Uint8Array[],
  network: SuiNetwork = "mainnet",
  opts: { effects?: boolean; events?: boolean } = {},
) {
  const client = getSuiGrpcClient(network);
  const paths = ["digest"];
  if (opts.effects !== false) paths.push("effects");
  if (opts.events) paths.push("events");
  paths.push("checkpoint", "timestamp", "balance_changes");

  const { response } =
    await client.transactionExecutionService.executeTransaction({
      transaction: { bcs: { value: transactionBytes } },
      signatures: signatures.map((sig) => ({
        bcs: { value: sig },
        signature: { oneofKind: undefined },
      })),
      readMask: { paths },
    });

  if (!response.finality?.effects?.status?.success) {
    const error = response.finality?.effects?.status?.error;
    throw new Error(`Transaction failed: ${error ?? "Unknown error"}`);
  }

  return response;
}

export async function simulateTransaction(
  transactionBytes: Uint8Array,
  network: SuiNetwork = "mainnet",
  opts: { skipChecks?: boolean; doGasSelection?: boolean } = {},
) {
  const client = getSuiGrpcClient(network);
  const { response } =
    await client.transactionExecutionService.simulateTransaction({
      transaction: { bcs: { value: transactionBytes } },
      checks: opts.skipChecks ? 1 : 0,
      doGasSelection: opts.doGasSelection ?? false,
      readMask: { paths: ["effects", "balance_changes", "command_outputs"] },
    });
  return response;
}

export async function getMovePackage(
  packageId: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.movePackageService.getPackage({
    packageId: normalizeSuiAddress(packageId),
  });
  return response.package ?? null;
}

export async function getMoveFunction(
  packageId: string,
  moduleName: string,
  functionName: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.movePackageService.getFunction({
    packageId: normalizeSuiAddress(packageId),
    moduleName,
    name: functionName,
  });
  return response.function ?? null;
}

export async function getMoveDatatype(
  packageId: string,
  moduleName: string,
  name: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.movePackageService.getDatatype({
    packageId: normalizeSuiAddress(packageId),
    moduleName,
    name,
  });
  return response.datatype ?? null;
}

export async function listPackageVersions(
  packageId: string,
  network: SuiNetwork = "mainnet",
  opts: { pageSize?: number; pageToken?: Uint8Array } = {},
) {
  const client = getSuiGrpcClient(network);
  const req: Record<string, unknown> = {
    packageId: normalizeSuiAddress(packageId),
  };
  if (opts.pageSize) req["pageSize"] = opts.pageSize;
  if (opts.pageToken) req["pageToken"] = opts.pageToken;

  const { response } = await client.movePackageService.listPackageVersions(
    req as Parameters<typeof client.movePackageService.listPackageVersions>[0],
  );
  return response;
}

export async function lookupSuiName(
  name: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.nameService.lookupName({ name });
  return response.record ?? null;
}

export async function reverseLookupSuiName(
  address: string,
  network: SuiNetwork = "mainnet",
) {
  const client = getSuiGrpcClient(network);
  const { response } = await client.nameService.reverseLookupName({
    address: normalizeSuiAddress(address),
  });
  return response.record ?? null;
}

export async function verifySuiSignature(
  messageBytes: Uint8Array,
  messageName: string,
  signatureBytes: Uint8Array,
  opts: { address?: string; network?: SuiNetwork } = {},
) {
  const network = opts.network ?? "mainnet";
  const client = getSuiGrpcClient(network);

  const req: Record<string, unknown> = {
    message: { name: messageName, value: messageBytes },
    signature: {
      bcs: { value: signatureBytes },
      signature: { oneofKind: undefined },
    },
    jwks: [],
  };
  if (opts.address) req["address"] = normalizeSuiAddress(opts.address);

  const { response } =
    await client.signatureVerificationService.verifySignature(
      req as Parameters<
        typeof client.signatureVerificationService.verifySignature
      >[0],
    );
  return {
    is_valid: response.isValid ?? false,
    reason: response.reason ?? undefined,
  };
}

export async function requestFaucet(
  recipientAddress: string,
  network: SuiNetwork = "devnet",
) {
  if (network === "mainnet") {
    throw new Error("Faucet is not available on mainnet");
  }
  await requestSuiFromFaucetV2({
    host: getFaucetHost(network),
    recipient: normalizeSuiAddress(recipientAddress),
  });
}

export async function getReferenceGasPrice(
  network: SuiNetwork = "mainnet",
): Promise<string> {
  const epoch = await getSuiEpoch(undefined, network);
  return String(epoch?.referenceGasPrice ?? 1000);
}

export function clearClientCache(): void {
  grpcClientCache.clear();
  graphqlClientCache.clear();
}

export { MIST_PER_SUI };
