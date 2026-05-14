import { Router, type Request, type Response } from "express";
import { logger } from "../core/logger.js";
import { suiWalletManager } from "../services/sui/sui-wallet.js";
import { suiDatabase } from "../services/database/sui-database.js";
import {
  getSuiGrpcClient,
  getSuiServiceInfo,
  getSuiObject,
  getSuiCheckpoint,
  getSuiEpoch,
  getReferenceGasPrice,
  requestFaucet,
  normalizeSuiAddress,
} from "../services/sui/sui-client.js";
import {
  getTransaction,
  batchGetTransactions,
  executeRawTransaction,
  simulateSuiTransaction,
  waitForTransaction,
  buildTransaction,
  executeMoveCall,
  publishPackage,
  upgradePackage,
  getLocalTransactionHistory,
  getLocalTransactionByDigest,
  getLocalUserTransactionHistory,
} from "../services/sui/sui-transactions.js";
import {
  getValidators,
  getValidatorByAddress,
  getCurrentEpoch,
  stakeWithValidator,
  unstakeFromValidator,
  getStakedSui,
  getStakingRewards,
  requestAddStakeWithMultipleCoins,
  getLocalStakingHistory,
  getLocalUnstakingHistory,
} from "../services/sui/sui-staking.js";
import {
  getObject,
  getObjects,
  mintNFT,
  transferNFT,
  getNFTs,
  listObjectDynamicFields,
  getDynamicField,
  getMovePackageInfo,
  getMoveFunctionInfo,
  getMoveTypeInfo,
  getPackageVersions,
  lookupName,
  reverseLookupName,
  signMessage,
  verifySignature,
  verifyTransactionSig,
  createMultiSigWallet,
  combineMultiSigAndExecute,
} from "../services/sui/sui-objects.js";

const router = Router();

function handleError(res: Response, error: unknown, context: string): void {
  const message = error instanceof Error ? error.message : String(error);
  logger.error(`Sui ${context} error:`, message);

  if (
    message.includes("not found") ||
    message.includes("does not exist") ||
    message.includes("not in cache")
  ) {
    res.status(404).json({ error: message });
    return;
  }
  if (
    message.includes("already exists") ||
    message.includes("duplicate") ||
    message.includes("Invalid")
  ) {
    res.status(400).json({ error: message });
    return;
  }
  if (message.includes("does not belong")) {
    res.status(403).json({ error: message });
    return;
  }
  res.status(500).json({ error: message });
}

router.post("/wallets", async (req: Request, res: Response) => {
  try {
    const { user_id, label, scheme, network } = req.body as {
      user_id?: string;
      label?: string;
      scheme?: "ed25519" | "secp256k1" | "secp256r1";
      network?: string;
    };
    if (!user_id) {
      res.status(400).json({ error: "user_id is required" });
      return;
    }
    const result = await suiWalletManager.createWallet({
      user_id,
      label,
      scheme,
      network: network as never,
    });
    res.status(201).json(result);
  } catch (error) {
    handleError(res, error, "create wallet");
  }
});

router.post("/wallets/import", async (req: Request, res: Response) => {
  try {
    const { user_id, private_key, label, scheme, network } = req.body as {
      user_id?: string;
      private_key?: string;
      label?: string;
      scheme?: "ed25519" | "secp256k1" | "secp256r1";
      network?: string;
    };
    if (!user_id || !private_key) {
      res.status(400).json({ error: "user_id and private_key are required" });
      return;
    }
    const result = await suiWalletManager.importWallet({
      user_id,
      private_key,
      label,
      scheme,
      network: network as never,
    });
    res.status(201).json(result);
  } catch (error) {
    handleError(res, error, "import wallet");
  }
});

router.post("/wallets/import/mnemonic", async (req: Request, res: Response) => {
  try {
    const { user_id, mnemonic, label, scheme, derivation_path, network } =
      req.body as {
        user_id?: string;
        mnemonic?: string;
        label?: string;
        scheme?: "ed25519" | "secp256k1" | "secp256r1";
        derivation_path?: string;
        network?: string;
      };
    if (!user_id || !mnemonic) {
      res.status(400).json({ error: "user_id and mnemonic are required" });
      return;
    }
    const result = await suiWalletManager.importWalletFromMnemonic({
      user_id,
      mnemonic,
      label,
      scheme,
      derivation_path,
      network: network as never,
    });
    res.status(201).json(result);
  } catch (error) {
    handleError(res, error, "import wallet from mnemonic");
  }
});

router.get("/wallets/:walletId", (req: Request, res: Response) => {
  try {
    const info = suiWalletManager.getWalletInfo(req.params.walletId);
    res.json(info);
  } catch (error) {
    handleError(res, error, "get wallet");
  }
});

router.delete("/wallets/:walletId", (req: Request, res: Response) => {
  try {
    const { user_id } = req.query as { user_id?: string };
    if (!user_id) {
      res.status(400).json({ error: "user_id query parameter is required" });
      return;
    }
    suiWalletManager.deleteWallet({ user_id, wallet_id: req.params.walletId });
    res.json({ deleted: true, wallet_id: req.params.walletId });
  } catch (error) {
    handleError(res, error, "delete wallet");
  }
});

router.get("/wallets/user/:userId", (req: Request, res: Response) => {
  try {
    const wallets = suiWalletManager.getUserWallets(req.params.userId);
    res.json({ user_id: req.params.userId, wallets });
  } catch (error) {
    handleError(res, error, "get user wallets");
  }
});

router.patch("/wallets/:walletId/label", (req: Request, res: Response) => {
  try {
    const { label } = req.body as { label?: string };
    if (!label) {
      res.status(400).json({ error: "label is required" });
      return;
    }
    suiWalletManager.updateWalletLabel(req.params.walletId, label);
    res.json({ updated: true, wallet_id: req.params.walletId, label });
  } catch (error) {
    handleError(res, error, "update wallet label");
  }
});

router.post("/wallets/:walletId/primary", (req: Request, res: Response) => {
  try {
    const { user_id } = req.body as { user_id?: string };
    if (!user_id) {
      res.status(400).json({ error: "user_id is required" });
      return;
    }
    suiWalletManager.setPrimaryWallet(user_id, req.params.walletId);
    res.json({
      updated: true,
      wallet_id: req.params.walletId,
      is_primary: true,
    });
  } catch (error) {
    handleError(res, error, "set primary wallet");
  }
});

router.get(
  "/wallets/:walletId/balance",
  async (req: Request, res: Response) => {
    try {
      const result = await suiWalletManager.getSuiBalance(req.params.walletId);
      res.json(result);
    } catch (error) {
      handleError(res, error, "get SUI balance");
    }
  },
);

router.get(
  "/wallets/:walletId/balance/:coinType",
  async (req: Request, res: Response) => {
    try {
      const coinType = decodeURIComponent(req.params.coinType);
      const result = await suiWalletManager.getBalanceByCoinType(
        req.params.walletId,
        coinType,
      );
      res.json(result);
    } catch (error) {
      handleError(res, error, "get coin balance");
    }
  },
);

router.get(
  "/wallets/:walletId/balances",
  async (req: Request, res: Response) => {
    try {
      const result = await suiWalletManager.getAllBalances(req.params.walletId);
      res.json(result);
    } catch (error) {
      handleError(res, error, "get all balances");
    }
  },
);

router.get("/wallets/:walletId/coins", async (req: Request, res: Response) => {
  try {
    const { coin_type } = req.query as { coin_type?: string };
    const result = await suiWalletManager.getOwnedCoins(
      req.params.walletId,
      coin_type,
    );
    res.json({ wallet_id: req.params.walletId, coins: result });
  } catch (error) {
    handleError(res, error, "get owned coins");
  }
});

router.get(
  "/wallets/:walletId/objects",
  async (req: Request, res: Response) => {
    try {
      const { object_type, limit, cursor } = req.query as {
        object_type?: string;
        limit?: string;
        cursor?: string;
      };
      const wallet = suiDatabase.getWallet(req.params.walletId);
      if (!wallet) {
        res.status(404).json({ error: "Wallet not found" });
        return;
      }
      const result = await suiWalletManager.getOwnedObjects({
        owner_address: wallet.sui_address,
        object_type,
        limit: limit ? parseInt(limit) : undefined,
        cursor,
      });
      res.json(result);
    } catch (error) {
      handleError(res, error, "get owned objects");
    }
  },
);

router.post("/transfer", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      recipient_address,
      amount_mist,
      coin_type,
      network,
      gas_budget,
    } = req.body as {
      sender_address?: string;
      recipient_address?: string;
      amount_mist?: string;
      coin_type?: string;
      network?: string;
      gas_budget?: string;
    };
    if (!sender_address || !recipient_address || !amount_mist) {
      res.status(400).json({
        error:
          "sender_address, recipient_address, and amount_mist are required",
      });
      return;
    }
    const result = await suiWalletManager.transferSui({
      sender_address,
      recipient_address,
      amount_mist: BigInt(amount_mist),
      coin_type,
      network: network as never,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "transfer SUI");
  }
});

router.post("/transfer/batch", async (req: Request, res: Response) => {
  try {
    const { sender_address, transfers, coin_type, network, gas_budget } =
      req.body as {
        sender_address?: string;
        transfers?: Array<{ to: string; amount: string }>;
        coin_type?: string;
        network?: string;
        gas_budget?: string;
      };
    if (!sender_address || !transfers || transfers.length === 0) {
      res
        .status(400)
        .json({ error: "sender_address and transfers array are required" });
      return;
    }
    const result = await suiWalletManager.batchTransferSui({
      sender_address,
      transfers,
      coin_type,
      network: network as never,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "batch transfer SUI");
  }
});

router.post("/coins/split", async (req: Request, res: Response) => {
  try {
    const { sender_address, coin_object_id, split_amounts, gas_budget } =
      req.body as {
        sender_address?: string;
        coin_object_id?: string;
        split_amounts?: string[];
        gas_budget?: string;
      };
    if (!sender_address || !coin_object_id || !split_amounts?.length) {
      res.status(400).json({
        error: "sender_address, coin_object_id, and split_amounts are required",
      });
      return;
    }
    const result = await suiWalletManager.splitCoin({
      sender_address,
      coin_object_id,
      split_amounts,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "split coin");
  }
});

router.post("/coins/merge", async (req: Request, res: Response) => {
  try {
    const { sender_address, primary_coin_id, coins_to_merge, gas_budget } =
      req.body as {
        sender_address?: string;
        primary_coin_id?: string;
        coins_to_merge?: string[];
        gas_budget?: string;
      };
    if (!sender_address || !primary_coin_id || !coins_to_merge?.length) {
      res.status(400).json({
        error:
          "sender_address, primary_coin_id, and coins_to_merge are required",
      });
      return;
    }
    const result = await suiWalletManager.mergeCoins({
      sender_address,
      primary_coin_id,
      coins_to_merge,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "merge coins");
  }
});

router.get("/coins/info/:coinType", async (req: Request, res: Response) => {
  try {
    const { network } = req.query as { network?: string };
    const coinType = decodeURIComponent(req.params.coinType);
    const result = await suiWalletManager.getCoinInfo({
      coin_type: coinType,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "get coin info");
  }
});

router.get("/transactions/:digest", async (req: Request, res: Response) => {
  try {
    const { network, include_effects, include_events, include_bcs } =
      req.query as {
        network?: string;
        include_effects?: string;
        include_events?: string;
        include_bcs?: string;
      };
    const result = await getTransaction({
      digest: req.params.digest,
      network: network as never,
      include_effects: include_effects !== "false",
      include_events: include_events === "true",
      include_bcs: include_bcs === "true",
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "get transaction");
  }
});

router.post("/transactions/batch", async (req: Request, res: Response) => {
  try {
    const { digests, network } = req.body as {
      digests?: string[];
      network?: string;
    };
    if (!digests?.length) {
      res.status(400).json({ error: "digests array is required" });
      return;
    }
    const result = await batchGetTransactions(digests, network as never);
    res.json({ transactions: result });
  } catch (error) {
    handleError(res, error, "batch get transactions");
  }
});

router.post("/transactions/execute", async (req: Request, res: Response) => {
  try {
    const {
      transaction_bytes,
      signatures,
      network,
      include_effects,
      include_events,
    } = req.body as {
      transaction_bytes?: string;
      signatures?: string[];
      network?: string;
      include_effects?: boolean;
      include_events?: boolean;
    };
    if (!transaction_bytes || !signatures?.length) {
      res
        .status(400)
        .json({ error: "transaction_bytes and signatures are required" });
      return;
    }
    const result = await executeRawTransaction({
      transaction_bytes,
      signatures,
      network: network as never,
      include_effects,
      include_events,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "execute transaction");
  }
});

router.post("/transactions/simulate", async (req: Request, res: Response) => {
  try {
    const {
      transaction_bytes,
      network,
      include_effects,
      include_balance_changes,
      skip_checks,
    } = req.body as {
      transaction_bytes?: string;
      network?: string;
      include_effects?: boolean;
      include_balance_changes?: boolean;
      skip_checks?: boolean;
    };
    if (!transaction_bytes) {
      res.status(400).json({ error: "transaction_bytes is required" });
      return;
    }
    const result = await simulateSuiTransaction({
      transaction_bytes,
      network: network as never,
      include_effects,
      include_balance_changes,
      skip_checks,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "simulate transaction");
  }
});

router.post("/transactions/wait", async (req: Request, res: Response) => {
  try {
    const { digest, timeout_ms, network, include_effects } = req.body as {
      digest?: string;
      timeout_ms?: number;
      network?: string;
      include_effects?: boolean;
    };
    if (!digest) {
      res.status(400).json({ error: "digest is required" });
      return;
    }
    const result = await waitForTransaction({
      digest,
      timeout_ms,
      network: network as never,
      include_effects,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "wait for transaction");
  }
});

router.post("/transactions/build", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      transaction_kind,
      params,
      gas_budget,
      gas_price,
      expiration_epoch,
      network,
    } = req.body as {
      sender_address?: string;
      transaction_kind?: string;
      params?: Record<string, unknown>;
      gas_budget?: string;
      gas_price?: string;
      expiration_epoch?: string;
      network?: string;
    };
    if (!sender_address || !transaction_kind || !params) {
      res.status(400).json({
        error: "sender_address, transaction_kind, and params are required",
      });
      return;
    }
    const result = await buildTransaction({
      sender_address,
      transaction_kind: transaction_kind as never,
      params,
      gas_budget,
      gas_price,
      expiration_epoch,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "build transaction");
  }
});

router.post("/move/call", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      package_id,
      module_name,
      function_name,
      type_arguments,
      arguments: args,
      gas_budget,
      network,
    } = req.body as {
      sender_address?: string;
      package_id?: string;
      module_name?: string;
      function_name?: string;
      type_arguments?: string[];
      arguments?: unknown[];
      gas_budget?: string;
      network?: string;
    };
    if (!sender_address || !package_id || !module_name || !function_name) {
      res.status(400).json({
        error:
          "sender_address, package_id, module_name, and function_name are required",
      });
      return;
    }
    const result = await executeMoveCall({
      sender_address,
      package_id,
      module_name,
      function_name,
      type_arguments,
      arguments: args,
      gas_budget,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "execute Move call");
  }
});

router.post("/move/publish", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      compiled_modules,
      dependencies,
      gas_budget,
      network,
    } = req.body as {
      sender_address?: string;
      compiled_modules?: string[];
      dependencies?: string[];
      gas_budget?: string;
      network?: string;
    };
    if (!sender_address || !compiled_modules?.length) {
      res
        .status(400)
        .json({ error: "sender_address and compiled_modules are required" });
      return;
    }
    const result = await publishPackage({
      sender_address,
      compiled_modules,
      dependencies: dependencies ?? [],
      gas_budget,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "publish package");
  }
});

router.post("/move/upgrade", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      package_id,
      compiled_modules,
      dependencies,
      upgrade_cap_id,
      gas_budget,
      network,
    } = req.body as {
      sender_address?: string;
      package_id?: string;
      compiled_modules?: string[];
      dependencies?: string[];
      upgrade_cap_id?: string;
      gas_budget?: string;
      network?: string;
    };
    if (
      !sender_address ||
      !package_id ||
      !compiled_modules?.length ||
      !upgrade_cap_id
    ) {
      res.status(400).json({
        error:
          "sender_address, package_id, compiled_modules, and upgrade_cap_id are required",
      });
      return;
    }
    const result = await upgradePackage({
      sender_address,
      package_id,
      compiled_modules,
      dependencies: dependencies ?? [],
      upgrade_cap_id,
      gas_budget,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "upgrade package");
  }
});

router.get("/move/package/:packageId", async (req: Request, res: Response) => {
  try {
    const { network } = req.query as { network?: string };
    const result = await getMovePackageInfo({
      package_id: req.params.packageId,
      network: network as never,
    });
    if (!result) {
      res.status(404).json({ error: "Package not found" });
      return;
    }
    res.json(result);
  } catch (error) {
    handleError(res, error, "get Move package");
  }
});

router.get(
  "/move/package/:packageId/versions",
  async (req: Request, res: Response) => {
    try {
      const { network } = req.query as { network?: string };
      const result = await getPackageVersions(
        req.params.packageId,
        network as never,
      );
      res.json({ package_id: req.params.packageId, versions: result });
    } catch (error) {
      handleError(res, error, "get package versions");
    }
  },
);

router.get(
  "/move/package/:packageId/module/:moduleName/function/:functionName",
  async (req: Request, res: Response) => {
    try {
      const { network } = req.query as { network?: string };
      const result = await getMoveFunctionInfo({
        package_id: req.params.packageId,
        module_name: req.params.moduleName,
        function_name: req.params.functionName,
        network: network as never,
      });
      if (!result) {
        res.status(404).json({ error: "Function not found" });
        return;
      }
      res.json(result);
    } catch (error) {
      handleError(res, error, "get Move function");
    }
  },
);

router.get(
  "/move/package/:packageId/module/:moduleName/type/:typeName",
  async (req: Request, res: Response) => {
    try {
      const { network } = req.query as { network?: string };
      const result = await getMoveTypeInfo(
        req.params.packageId,
        req.params.moduleName,
        req.params.typeName,
        network as never,
      );
      if (!result) {
        res.status(404).json({ error: "Type not found" });
        return;
      }
      res.json(result);
    } catch (error) {
      handleError(res, error, "get Move type");
    }
  },
);

router.get("/history/:walletId", (req: Request, res: Response) => {
  try {
    const { limit, offset, tx_type } = req.query as {
      limit?: string;
      offset?: string;
      tx_type?: string;
    };
    const result = getLocalTransactionHistory({
      wallet_id: req.params.walletId,
      limit: limit ? parseInt(limit) : undefined,
      offset: offset ? parseInt(offset) : undefined,
      tx_type: tx_type as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "get local transaction history");
  }
});

router.get("/history/user/:userId", (req: Request, res: Response) => {
  try {
    const { limit, offset } = req.query as { limit?: string; offset?: string };
    const records = getLocalUserTransactionHistory(
      req.params.userId,
      limit ? parseInt(limit) : 50,
      offset ? parseInt(offset) : 0,
    );
    res.json({ user_id: req.params.userId, transactions: records });
  } catch (error) {
    handleError(res, error, "get local user transaction history");
  }
});

router.get("/history/digest/:digest", (req: Request, res: Response) => {
  try {
    const record = getLocalTransactionByDigest(req.params.digest);
    if (!record) {
      res.status(404).json({ error: "Transaction not found in local history" });
      return;
    }
    res.json(record);
  } catch (error) {
    handleError(res, error, "get local transaction by digest");
  }
});

router.get("/staking/validators", async (req: Request, res: Response) => {
  try {
    const { network } = req.query as { network?: string };
    const validators = await getValidators({ network: network as never });
    res.json({ validators, count: validators.length });
  } catch (error) {
    handleError(res, error, "get validators");
  }
});

router.get(
  "/staking/validators/:address",
  async (req: Request, res: Response) => {
    try {
      const { network } = req.query as { network?: string };
      const validator = await getValidatorByAddress(
        req.params.address,
        network as never,
      );
      if (!validator) {
        res.status(404).json({ error: "Validator not found" });
        return;
      }
      res.json(validator);
    } catch (error) {
      handleError(res, error, "get validator");
    }
  },
);

router.get("/staking/epoch", async (req: Request, res: Response) => {
  try {
    const { network } = req.query as { network?: string };
    const epoch = await getCurrentEpoch(network as never);
    res.json(epoch);
  } catch (error) {
    handleError(res, error, "get epoch");
  }
});

router.post("/staking/stake", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      validator_address,
      amount_mist,
      network,
      gas_budget,
    } = req.body as {
      sender_address?: string;
      validator_address?: string;
      amount_mist?: string;
      network?: string;
      gas_budget?: string;
    };
    if (!sender_address || !validator_address || !amount_mist) {
      res.status(400).json({
        error:
          "sender_address, validator_address, and amount_mist are required",
      });
      return;
    }
    const result = await stakeWithValidator({
      sender_address,
      validator_address,
      amount_mist,
      network: network as never,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "stake SUI");
  }
});

router.post(
  "/staking/stake/multi-coin",
  async (req: Request, res: Response) => {
    try {
      const {
        sender_address,
        validator_address,
        coin_object_ids,
        network,
        gas_budget,
      } = req.body as {
        sender_address?: string;
        validator_address?: string;
        coin_object_ids?: string[];
        network?: string;
        gas_budget?: string;
      };
      if (!sender_address || !validator_address || !coin_object_ids?.length) {
        res.status(400).json({
          error:
            "sender_address, validator_address, and coin_object_ids are required",
        });
        return;
      }
      const result = await requestAddStakeWithMultipleCoins(
        sender_address,
        validator_address,
        coin_object_ids,
        network as never,
        gas_budget,
      );
      res.json(result);
    } catch (error) {
      handleError(res, error, "stake SUI with multiple coins");
    }
  },
);

router.post("/staking/unstake", async (req: Request, res: Response) => {
  try {
    const { sender_address, staked_sui_id, network, gas_budget } = req.body as {
      sender_address?: string;
      staked_sui_id?: string;
      network?: string;
      gas_budget?: string;
    };
    if (!sender_address || !staked_sui_id) {
      res
        .status(400)
        .json({ error: "sender_address and staked_sui_id are required" });
      return;
    }
    const result = await unstakeFromValidator({
      sender_address,
      staked_sui_id,
      network: network as never,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "unstake SUI");
  }
});

router.get(
  "/staking/staked/:ownerAddress",
  async (req: Request, res: Response) => {
    try {
      const { network } = req.query as { network?: string };
      const stakes = await getStakedSui({
        owner_address: req.params.ownerAddress,
        network: network as never,
      });
      res.json({
        owner_address: req.params.ownerAddress,
        stakes,
        count: stakes.length,
      });
    } catch (error) {
      handleError(res, error, "get staked SUI");
    }
  },
);

router.get(
  "/staking/rewards/:ownerAddress",
  async (req: Request, res: Response) => {
    try {
      const { network } = req.query as { network?: string };
      const rewards = await getStakingRewards({
        owner_address: req.params.ownerAddress,
        network: network as never,
      });
      res.json(rewards);
    } catch (error) {
      handleError(res, error, "get staking rewards");
    }
  },
);

router.get("/staking/history/:walletId", (req: Request, res: Response) => {
  try {
    const { limit } = req.query as { limit?: string };
    const history = getLocalStakingHistory(
      req.params.walletId,
      limit ? parseInt(limit) : 50,
    );
    res.json({ wallet_id: req.params.walletId, history });
  } catch (error) {
    handleError(res, error, "get staking history");
  }
});

router.get(
  "/staking/unstaking-history/:walletId",
  (req: Request, res: Response) => {
    try {
      const { limit } = req.query as { limit?: string };
      const history = getLocalUnstakingHistory(
        req.params.walletId,
        limit ? parseInt(limit) : 50,
      );
      res.json({ wallet_id: req.params.walletId, history });
    } catch (error) {
      handleError(res, error, "get unstaking history");
    }
  },
);

router.get("/objects/:objectId", async (req: Request, res: Response) => {
  try {
    const { network, include_content } = req.query as {
      network?: string;
      include_content?: string;
    };
    const result = await getObject({
      object_id: req.params.objectId,
      include_content: include_content === "true",
      network: network as never,
    });
    if (!result) {
      res.status(404).json({ error: "Object not found" });
      return;
    }
    res.json(result);
  } catch (error) {
    handleError(res, error, "get object");
  }
});

router.post("/objects/batch", async (req: Request, res: Response) => {
  try {
    const { object_ids, include_content, network } = req.body as {
      object_ids?: string[];
      include_content?: boolean;
      network?: string;
    };
    if (!object_ids?.length) {
      res.status(400).json({ error: "object_ids array is required" });
      return;
    }
    const result = await getObjects({
      object_ids,
      include_content,
      network: network as never,
    });
    res.json({ objects: result });
  } catch (error) {
    handleError(res, error, "batch get objects");
  }
});

router.get(
  "/objects/:objectId/dynamic-fields",
  async (req: Request, res: Response) => {
    try {
      const { limit, cursor, network } = req.query as {
        limit?: string;
        cursor?: string;
        network?: string;
      };
      const result = await listObjectDynamicFields({
        parent_id: req.params.objectId,
        limit: limit ? parseInt(limit) : undefined,
        cursor,
        network: network as never,
      });
      res.json(result);
    } catch (error) {
      handleError(res, error, "list dynamic fields");
    }
  },
);

router.post("/nft/mint", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      name,
      description,
      url,
      package_id,
      module_name,
      function_name,
      extra_args,
      network,
      gas_budget,
    } = req.body as {
      sender_address?: string;
      name?: string;
      description?: string;
      url?: string;
      package_id?: string;
      module_name?: string;
      function_name?: string;
      extra_args?: unknown[];
      network?: string;
      gas_budget?: string;
    };
    if (!sender_address || !name || !description || !url) {
      res.status(400).json({
        error: "sender_address, name, description, and url are required",
      });
      return;
    }
    const result = await mintNFT({
      sender_address,
      name,
      description,
      url,
      package_id,
      module_name,
      function_name,
      extra_args,
      network: network as never,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "mint NFT");
  }
});

router.post("/nft/transfer", async (req: Request, res: Response) => {
  try {
    const {
      sender_address,
      nft_object_id,
      recipient_address,
      network,
      gas_budget,
    } = req.body as {
      sender_address?: string;
      nft_object_id?: string;
      recipient_address?: string;
      network?: string;
      gas_budget?: string;
    };
    if (!sender_address || !nft_object_id || !recipient_address) {
      res.status(400).json({
        error:
          "sender_address, nft_object_id, and recipient_address are required",
      });
      return;
    }
    const result = await transferNFT({
      sender_address,
      nft_object_id,
      recipient_address,
      network: network as never,
      gas_budget,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "transfer NFT");
  }
});

router.get("/nft/:ownerAddress", async (req: Request, res: Response) => {
  try {
    const { limit, cursor, network } = req.query as {
      limit?: string;
      cursor?: string;
      network?: string;
    };
    const nfts = await getNFTs({
      owner_address: req.params.ownerAddress,
      limit: limit ? parseInt(limit) : undefined,
      cursor,
      network: network as never,
    });
    res.json({
      owner_address: req.params.ownerAddress,
      nfts,
      count: nfts.length,
    });
  } catch (error) {
    handleError(res, error, "get NFTs");
  }
});

router.post("/wallets/:walletId/sign", async (req: Request, res: Response) => {
  try {
    const { message, network } = req.body as {
      message?: string;
      network?: string;
    };
    if (!message) {
      res.status(400).json({ error: "message is required" });
      return;
    }
    const result = await signMessage({
      wallet_id: req.params.walletId,
      message,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "sign message");
  }
});

router.post("/verify/signature", async (req: Request, res: Response) => {
  try {
    const { message, signature, address, network } = req.body as {
      message?: string;
      signature?: string;
      address?: string;
      network?: string;
    };
    if (!message || !signature) {
      res.status(400).json({ error: "message and signature are required" });
      return;
    }
    const result = await verifySignature({
      message,
      signature,
      address,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "verify signature");
  }
});

router.post("/verify/transaction", async (req: Request, res: Response) => {
  try {
    const { transaction_bytes, signature, address } = req.body as {
      transaction_bytes?: string;
      signature?: string;
      address?: string;
    };
    if (!transaction_bytes || !signature) {
      res
        .status(400)
        .json({ error: "transaction_bytes and signature are required" });
      return;
    }
    const txBytes = Buffer.from(transaction_bytes, "base64");
    const result = await verifyTransactionSig(txBytes, signature, address);
    res.json(result);
  } catch (error) {
    handleError(res, error, "verify transaction signature");
  }
});

router.post("/multisig/create", (req: Request, res: Response) => {
  try {
    const { public_keys, threshold } = req.body as {
      public_keys?: Array<{
        public_key: string;
        scheme: "ed25519" | "secp256k1" | "secp256r1";
        weight: number;
      }>;
      threshold?: number;
    };
    if (!public_keys?.length || threshold === undefined) {
      res.status(400).json({ error: "public_keys and threshold are required" });
      return;
    }
    const result = createMultiSigWallet({ public_keys, threshold });
    res.json(result);
  } catch (error) {
    handleError(res, error, "create multisig wallet");
  }
});

router.post("/multisig/combine", async (req: Request, res: Response) => {
  try {
    const {
      partial_signatures,
      transaction_bytes,
      public_keys,
      threshold,
      network,
    } = req.body as {
      partial_signatures?: string[];
      transaction_bytes?: string;
      public_keys?: Array<{
        public_key: string;
        scheme: "ed25519" | "secp256k1" | "secp256r1";
        weight: number;
      }>;
      threshold?: number;
      network?: string;
    };
    if (
      !partial_signatures?.length ||
      !transaction_bytes ||
      !public_keys?.length ||
      threshold === undefined
    ) {
      res.status(400).json({
        error:
          "partial_signatures, transaction_bytes, public_keys, and threshold are required",
      });
      return;
    }
    const result = await combineMultiSigAndExecute({
      partial_signatures,
      transaction_bytes,
      public_keys,
      threshold,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "combine and execute multisig");
  }
});

router.get("/names/lookup", async (req: Request, res: Response) => {
  try {
    const { name, network } = req.query as { name?: string; network?: string };
    if (!name) {
      res.status(400).json({ error: "name query parameter is required" });
      return;
    }
    const result = await lookupName({ name, network: network as never });
    res.json(result);
  } catch (error) {
    handleError(res, error, "lookup SuiNS name");
  }
});

router.get("/names/reverse/:address", async (req: Request, res: Response) => {
  try {
    const { network } = req.query as { network?: string };
    const result = await reverseLookupName({
      address: req.params.address,
      network: network as never,
    });
    res.json(result);
  } catch (error) {
    handleError(res, error, "reverse lookup SuiNS name");
  }
});

router.get("/chain/info", async (req: Request, res: Response) => {
  try {
    const { network } = req.query as { network?: string };
    const result = await getSuiServiceInfo(network as never);
    res.json(result);
  } catch (error) {
    handleError(res, error, "get chain info");
  }
});

router.get("/chain/gas-price", async (req: Request, res: Response) => {
  try {
    const { network } = req.query as { network?: string };
    const price = await getReferenceGasPrice(network as never);
    res.json({ reference_gas_price: price, network: network ?? "mainnet" });
  } catch (error) {
    handleError(res, error, "get reference gas price");
  }
});

router.get("/chain/checkpoint", async (req: Request, res: Response) => {
  try {
    const { sequence_number, digest, network } = req.query as {
      sequence_number?: string;
      digest?: string;
      network?: string;
    };
    if (!sequence_number && !digest) {
      res.status(400).json({
        error: "sequence_number or digest query parameter is required",
      });
      return;
    }
    const result = await getSuiCheckpoint(
      { sequenceNumber: sequence_number, digest },
      network as never,
    );
    if (!result) {
      res.status(404).json({ error: "Checkpoint not found" });
      return;
    }
    res.json(result);
  } catch (error) {
    handleError(res, error, "get checkpoint");
  }
});

router.get("/chain/epoch", async (req: Request, res: Response) => {
  try {
    const { epoch, network } = req.query as {
      epoch?: string;
      network?: string;
    };
    const result = await getSuiEpoch(epoch, network as never);
    if (!result) {
      res.status(404).json({ error: "Epoch not found" });
      return;
    }
    res.json(result);
  } catch (error) {
    handleError(res, error, "get epoch data");
  }
});

router.post("/faucet", async (req: Request, res: Response) => {
  try {
    const { recipient_address, network } = req.body as {
      recipient_address?: string;
      network?: string;
    };
    if (!recipient_address) {
      res.status(400).json({ error: "recipient_address is required" });
      return;
    }
    const net = (network ?? "devnet") as never;
    await requestFaucet(recipient_address, net);
    res.json({
      success: true,
      recipient_address: normalizeSuiAddress(recipient_address),
      network: net,
    });
  } catch (error) {
    handleError(res, error, "request faucet");
  }
});

export default router;
