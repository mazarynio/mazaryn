import { Account, teraToGas, nearToYocto } from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";

const SOCIAL_CONTRACT_MAINNET = "social.near";
const SOCIAL_CONTRACT_TESTNET = "v1.social08.testnet";

const MAZARYN_CONTRACT_TESTNET = "mazaryn.testnet";
const MAZARYN_CONTRACT_MAINNET = "mazaryn.near";

export class NearSocialManager {
  private getSocialContract(): string {
    return config.nearNetwork === "mainnet"
      ? SOCIAL_CONTRACT_MAINNET
      : SOCIAL_CONTRACT_TESTNET;
  }

  private getMazarynContract(): string {
    return config.nearNetwork === "mainnet"
      ? MAZARYN_CONTRACT_MAINNET
      : MAZARYN_CONTRACT_TESTNET;
  }

  private buildSignerAccount(walletId: string): {
    account: Account;
    accountId: string;
  } {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) {
      throw new Error("Wallet not found");
    }

    const privateKeyHex = encryptionService.decrypt(
      wallet.encrypted_private_key,
      wallet.encryption_iv,
      wallet.encryption_tag,
    );

    const privateKeyString = Buffer.from(
      privateKeyHex,
      "hex",
    ).toString() as KeyPairString;
    const provider = nearConnection.getProvider();
    const account = new Account(wallet.account_id, provider, privateKeyString);

    return { account, accountId: wallet.account_id };
  }

  private extractTxHash(result: any): string {
    if (result?.transaction?.hash) return result.transaction.hash;
    if (typeof result === "string") return result;
    return JSON.stringify(result);
  }

  private async viewCall(
    contractId: string,
    method: string,
    args: Record<string, unknown> = {},
  ): Promise<any> {
    const provider = nearConnection.getProvider();
    return (provider as any).callFunction({ contractId, method, args });
  }

  async setSocialProfile(
    walletId: string,
    userId: string,
    profileData: {
      name?: string;
      bio?: string;
      image?: { url?: string; ipfs_cid?: string };
      backgroundImage?: { url?: string; ipfs_cid?: string };
      linktree?: Record<string, string>;
      tags?: Record<string, string>;
    },
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Setting social profile for ${wallet.account_id}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const socialContract = this.getSocialContract();

    const data = {
      [accountId]: {
        profile: {
          ...(profileData.name !== undefined && { name: profileData.name }),
          ...(profileData.bio !== undefined && {
            description: profileData.bio,
          }),
          ...(profileData.image && { image: profileData.image }),
          ...(profileData.backgroundImage && {
            backgroundImage: profileData.backgroundImage,
          }),
          ...(profileData.linktree && { linktree: profileData.linktree }),
          ...(profileData.tags && { tags: profileData.tags }),
        },
      },
    };

    const result = await account.callFunction({
      contractId: socialContract,
      methodName: "set",
      args: { data },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.01"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: socialContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "social_set_profile",
      contract_id: socialContract,
      method_name: "set",
    });

    logger.info(`Social profile set for ${accountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      transaction_hash: txHash,
      social_contract: socialContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async getSocialProfile(accountId: string): Promise<any> {
    logger.info(`Getting social profile for ${accountId}`);

    const socialContract = this.getSocialContract();

    const result = await this.viewCall(socialContract, "get", {
      keys: [`${accountId}/profile/**`],
    });

    return {
      account_id: accountId,
      social_contract: socialContract,
      profile: result?.[accountId]?.profile ?? null,
    };
  }

  async socialFollow(
    walletId: string,
    userId: string,
    targetAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`${wallet.account_id} following ${targetAccountId}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const socialContract = this.getSocialContract();

    const data = {
      [accountId]: {
        graph: {
          follow: {
            [targetAccountId]: "",
          },
        },
        index: {
          follow: JSON.stringify({
            key: targetAccountId,
            value: { type: "follow" },
          }),
          notify: JSON.stringify({
            key: targetAccountId,
            value: { type: "follow", accountId },
          }),
        },
      },
    };

    const result = await account.callFunction({
      contractId: socialContract,
      methodName: "set",
      args: { data },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.01"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: socialContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "social_follow",
      contract_id: socialContract,
      method_name: "set",
    });

    logger.info(`${accountId} now follows ${targetAccountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      following: targetAccountId,
      transaction_hash: txHash,
      social_contract: socialContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async socialUnfollow(
    walletId: string,
    userId: string,
    targetAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`${wallet.account_id} unfollowing ${targetAccountId}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const socialContract = this.getSocialContract();

    const data = {
      [accountId]: {
        graph: {
          follow: {
            [targetAccountId]: null,
          },
        },
      },
    };

    const result = await account.callFunction({
      contractId: socialContract,
      methodName: "set",
      args: { data },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.01"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: socialContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "social_unfollow",
      contract_id: socialContract,
      method_name: "set",
    });

    logger.info(`${accountId} unfollowed ${targetAccountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      unfollowed: targetAccountId,
      transaction_hash: txHash,
      social_contract: socialContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async getFollowing(accountId: string): Promise<any> {
    logger.info(`Getting following list for ${accountId}`);

    const socialContract = this.getSocialContract();
    const result = await this.viewCall(socialContract, "get", {
      keys: [`${accountId}/graph/follow/**`],
    });

    const followMap = result?.[accountId]?.graph?.follow ?? {};
    const following = Object.keys(followMap).filter(
      (k) => followMap[k] !== null,
    );

    return {
      account_id: accountId,
      social_contract: socialContract,
      following,
      total: following.length,
    };
  }

  async getFollowers(accountId: string): Promise<any> {
    logger.info(`Getting followers for ${accountId}`);

    const socialContract = this.getSocialContract();
    const result = await this.viewCall(socialContract, "keys", {
      keys: [`*/graph/follow/${accountId}`],
      options: { return_type: "BlockHeight" },
    });

    const followers = result ? Object.keys(result) : [];

    return {
      account_id: accountId,
      social_contract: socialContract,
      followers,
      total: followers.length,
    };
  }

  async socialPost(
    walletId: string,
    userId: string,
    postData: {
      text: string;
      image?: string;
      tags?: string[];
    },
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Creating social post for ${wallet.account_id}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const socialContract = this.getSocialContract();

    const postContent: Record<string, unknown> = {
      type: "md",
      text: postData.text,
    };

    if (postData.image) {
      postContent.image = { url: postData.image };
    }

    const indexPost = JSON.stringify({
      key: "main",
      value: { type: "md" },
    });

    const data: Record<string, unknown> = {
      [accountId]: {
        post: {
          main: JSON.stringify(postContent),
        },
        index: {
          post: indexPost,
        },
      },
    };

    if (postData.tags && postData.tags.length > 0) {
      const tagNotifications = postData.tags.map((tag) =>
        JSON.stringify({ key: tag, value: { type: "tag" } }),
      );
      (data[accountId] as any).index.tag = tagNotifications;
    }

    const result = await account.callFunction({
      contractId: socialContract,
      methodName: "set",
      args: { data },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.01"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: socialContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "social_post",
      contract_id: socialContract,
      method_name: "set",
    });

    logger.info(`Social post created for ${accountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      transaction_hash: txHash,
      social_contract: socialContract,
      post_content: postContent,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async socialLike(
    walletId: string,
    userId: string,
    targetAccountId: string,
    blockHeight: number,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `${wallet.account_id} liking post by ${targetAccountId} at block ${blockHeight}`,
    );

    const { account, accountId } = this.buildSignerAccount(walletId);
    const socialContract = this.getSocialContract();

    const likeValue = JSON.stringify({
      type: "like",
      item: {
        type: "social",
        path: `${targetAccountId}/post/main`,
        blockHeight,
      },
    });

    const notifyValue = JSON.stringify({
      key: targetAccountId,
      value: {
        type: "like",
        item: {
          type: "social",
          path: `${targetAccountId}/post/main`,
          blockHeight,
        },
      },
    });

    const data = {
      [accountId]: {
        index: {
          like: likeValue,
          notify: notifyValue,
        },
      },
    };

    const result = await account.callFunction({
      contractId: socialContract,
      methodName: "set",
      args: { data },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.005"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: socialContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "social_like",
      contract_id: socialContract,
      method_name: "set",
    });

    logger.info(
      `Like recorded for ${targetAccountId}/${blockHeight}: ${txHash}`,
    );

    return {
      wallet_id: walletId,
      account_id: accountId,
      liked_post: { account_id: targetAccountId, block_height: blockHeight },
      transaction_hash: txHash,
      social_contract: socialContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async socialComment(
    walletId: string,
    userId: string,
    targetAccountId: string,
    blockHeight: number,
    commentText: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `${wallet.account_id} commenting on ${targetAccountId}/${blockHeight}`,
    );

    const { account, accountId } = this.buildSignerAccount(walletId);
    const socialContract = this.getSocialContract();

    const commentContent = JSON.stringify({ type: "md", text: commentText });

    const indexComment = JSON.stringify({
      key: {
        type: "social",
        path: `${targetAccountId}/post/main`,
        blockHeight,
      },
      value: { type: "md" },
    });

    const notifyValue = JSON.stringify({
      key: targetAccountId,
      value: {
        type: "comment",
        item: {
          type: "social",
          path: `${targetAccountId}/post/main`,
          blockHeight,
        },
      },
    });

    const data = {
      [accountId]: {
        post: {
          comment: commentContent,
        },
        index: {
          comment: indexComment,
          notify: notifyValue,
        },
      },
    };

    const result = await account.callFunction({
      contractId: socialContract,
      methodName: "set",
      args: { data },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.01"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: socialContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "social_comment",
      contract_id: socialContract,
      method_name: "set",
    });

    logger.info(`Comment posted by ${accountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      commented_on: { account_id: targetAccountId, block_height: blockHeight },
      comment_text: commentText,
      transaction_hash: txHash,
      social_contract: socialContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async getSocialFeed(accountId: string, limit: number = 10): Promise<any> {
    logger.info(`Getting social feed for ${accountId}`);

    const socialContract = this.getSocialContract();

    const following = await this.getFollowing(accountId);

    if (following.following.length === 0) {
      return {
        account_id: accountId,
        social_contract: socialContract,
        feed: [],
        total: 0,
      };
    }

    const feedAccountIds = [accountId, ...following.following].slice(0, 20);

    const keys = feedAccountIds.map((id) => `${id}/post/main`);

    const result = await this.viewCall(socialContract, "get", {
      keys,
      options: { return_type: "BlockHeight", limit, order: "desc" },
    });

    const feed: Array<{
      account_id: string;
      block_height: number;
      content: unknown;
    }> = [];

    if (result) {
      for (const [acct, data] of Object.entries(result) as any[]) {
        const posts = data?.post?.main;
        if (posts && typeof posts === "object") {
          for (const [blockHeight, content] of Object.entries(posts)) {
            feed.push({
              account_id: acct,
              block_height: parseInt(blockHeight, 10),
              content,
            });
          }
        }
      }
    }

    feed.sort((a, b) => b.block_height - a.block_height);

    return {
      account_id: accountId,
      social_contract: socialContract,
      feed: feed.slice(0, limit),
      total: feed.length,
    };
  }

  async mazarynAddPost(
    walletId: string,
    userId: string,
    postData: {
      text: string;
      media_urls?: string[];
      tags?: string[];
    },
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Creating Mazaryn post for ${wallet.account_id}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const mazarynContract = this.getMazarynContract();

    const result = await account.callFunction({
      contractId: mazarynContract,
      methodName: "add_post",
      args: {
        text: postData.text,
        media_urls: postData.media_urls ?? [],
        tags: postData.tags ?? [],
      },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.01"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: mazarynContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "mazaryn_add_post",
      contract_id: mazarynContract,
      method_name: "add_post",
    });

    logger.info(`Mazaryn post created by ${accountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      transaction_hash: txHash,
      mazaryn_contract: mazarynContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async mazarynLikePost(
    walletId: string,
    userId: string,
    postId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`${wallet.account_id} liking Mazaryn post ${postId}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const mazarynContract = this.getMazarynContract();

    const result = await account.callFunction({
      contractId: mazarynContract,
      methodName: "like_post",
      args: { post_id: postId },
      gas: teraToGas("50"),
      deposit: nearToYocto("0.001"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: mazarynContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "mazaryn_like_post",
      contract_id: mazarynContract,
      method_name: "like_post",
    });

    logger.info(
      `Mazaryn like recorded by ${accountId} on ${postId}: ${txHash}`,
    );

    return {
      wallet_id: walletId,
      account_id: accountId,
      post_id: postId,
      transaction_hash: txHash,
      mazaryn_contract: mazarynContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async mazarynFollowUser(
    walletId: string,
    userId: string,
    targetAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`${wallet.account_id} following ${targetAccountId} on Mazaryn`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const mazarynContract = this.getMazarynContract();

    const result = await account.callFunction({
      contractId: mazarynContract,
      methodName: "follow_user",
      args: { account_id: targetAccountId },
      gas: teraToGas("50"),
      deposit: nearToYocto("0.001"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: mazarynContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "mazaryn_follow",
      contract_id: mazarynContract,
      method_name: "follow_user",
    });

    logger.info(`Mazaryn follow: ${accountId} → ${targetAccountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      following: targetAccountId,
      transaction_hash: txHash,
      mazaryn_contract: mazarynContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async mazarynUnfollowUser(
    walletId: string,
    userId: string,
    targetAccountId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `${wallet.account_id} unfollowing ${targetAccountId} on Mazaryn`,
    );

    const { account, accountId } = this.buildSignerAccount(walletId);
    const mazarynContract = this.getMazarynContract();

    const result = await account.callFunction({
      contractId: mazarynContract,
      methodName: "unfollow_user",
      args: { account_id: targetAccountId },
      gas: teraToGas("50"),
      deposit: 0n,
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: mazarynContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "mazaryn_unfollow",
      contract_id: mazarynContract,
      method_name: "unfollow_user",
    });

    return {
      wallet_id: walletId,
      account_id: accountId,
      unfollowed: targetAccountId,
      transaction_hash: txHash,
      mazaryn_contract: mazarynContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async mazarynGetUserPosts(
    targetAccountId: string,
    limit: number = 20,
    offset: number = 0,
  ): Promise<any> {
    logger.info(`Getting Mazaryn posts for ${targetAccountId}`);

    const mazarynContract = this.getMazarynContract();

    const result = await this.viewCall(mazarynContract, "get_user_posts", {
      account_id: targetAccountId,
      limit,
      offset,
    });

    return {
      account_id: targetAccountId,
      mazaryn_contract: mazarynContract,
      posts: result ?? [],
      limit,
      offset,
    };
  }

  async mazarynGetPost(postId: string): Promise<any> {
    logger.info(`Getting Mazaryn post ${postId}`);

    const mazarynContract = this.getMazarynContract();

    const result = await this.viewCall(mazarynContract, "get_post", {
      post_id: postId,
    });

    return {
      post_id: postId,
      mazaryn_contract: mazarynContract,
      post: result ?? null,
    };
  }

  async mazarynGetFeed(
    accountId: string,
    limit: number = 20,
    offset: number = 0,
  ): Promise<any> {
    logger.info(`Getting Mazaryn feed for ${accountId}`);

    const mazarynContract = this.getMazarynContract();

    const result = await this.viewCall(mazarynContract, "get_feed", {
      account_id: accountId,
      limit,
      offset,
    });

    return {
      account_id: accountId,
      mazaryn_contract: mazarynContract,
      feed: result ?? [],
      limit,
      offset,
    };
  }

  async mazarynGetFollowers(accountId: string): Promise<any> {
    logger.info(`Getting Mazaryn followers for ${accountId}`);

    const mazarynContract = this.getMazarynContract();

    const result = await this.viewCall(mazarynContract, "get_followers", {
      account_id: accountId,
    });

    const followers = result ?? [];

    return {
      account_id: accountId,
      mazaryn_contract: mazarynContract,
      followers,
      total: followers.length,
    };
  }

  async mazarynGetFollowing(accountId: string): Promise<any> {
    logger.info(`Getting Mazaryn following for ${accountId}`);

    const mazarynContract = this.getMazarynContract();

    const result = await this.viewCall(mazarynContract, "get_following", {
      account_id: accountId,
    });

    const following = result ?? [];

    return {
      account_id: accountId,
      mazaryn_contract: mazarynContract,
      following,
      total: following.length,
    };
  }

  async mazarynAddComment(
    walletId: string,
    userId: string,
    postId: string,
    commentText: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`${wallet.account_id} commenting on Mazaryn post ${postId}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const mazarynContract = this.getMazarynContract();

    const result = await account.callFunction({
      contractId: mazarynContract,
      methodName: "add_comment",
      args: { post_id: postId, text: commentText },
      gas: teraToGas("100"),
      deposit: nearToYocto("0.01"),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: mazarynContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "mazaryn_add_comment",
      contract_id: mazarynContract,
      method_name: "add_comment",
    });

    logger.info(`Mazaryn comment by ${accountId} on ${postId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      post_id: postId,
      comment_text: commentText,
      transaction_hash: txHash,
      mazaryn_contract: mazarynContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async mazarynTipUser(
    walletId: string,
    userId: string,
    targetAccountId: string,
    amountNear: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `${wallet.account_id} tipping ${targetAccountId} ${amountNear} NEAR`,
    );

    const { account, accountId } = this.buildSignerAccount(walletId);
    const mazarynContract = this.getMazarynContract();

    const result = await account.callFunction({
      contractId: mazarynContract,
      methodName: "tip_user",
      args: { receiver_id: targetAccountId },
      gas: teraToGas("50"),
      deposit: nearToYocto(amountNear),
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: mazarynContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "mazaryn_tip",
      contract_id: mazarynContract,
      method_name: "tip_user",
      amount_near: amountNear,
    });

    logger.info(
      `Mazaryn tip from ${accountId} to ${targetAccountId}: ${txHash}`,
    );

    return {
      wallet_id: walletId,
      account_id: accountId,
      tipped_account: targetAccountId,
      amount_near: amountNear,
      transaction_hash: txHash,
      mazaryn_contract: mazarynContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async mazarynDeletePost(
    walletId: string,
    userId: string,
    postId: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`${wallet.account_id} deleting Mazaryn post ${postId}`);

    const { account, accountId } = this.buildSignerAccount(walletId);
    const mazarynContract = this.getMazarynContract();

    const result = await account.callFunction({
      contractId: mazarynContract,
      methodName: "delete_post",
      args: { post_id: postId },
      gas: teraToGas("50"),
      deposit: 0n,
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(walletId);
    nearDatabase.createTransactionRecord({
      wallet_id: walletId,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: mazarynContract,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "mazaryn_delete_post",
      contract_id: mazarynContract,
      method_name: "delete_post",
    });

    logger.info(`Mazaryn post ${postId} deleted by ${accountId}: ${txHash}`);

    return {
      wallet_id: walletId,
      account_id: accountId,
      deleted_post_id: postId,
      transaction_hash: txHash,
      mazaryn_contract: mazarynContract,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }
}

export const nearSocialManager = new NearSocialManager();
