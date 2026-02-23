import express from "express";
import { nearSocialManager } from "../services/near/social.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";

export const nearSocialRoutes = express.Router();

nearSocialRoutes.put(
  "/profile/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { name, bio, image, backgroundImage, linktree, tags } = req.body;

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/profile/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.setSocialProfile(
        wallet_id,
        userId,
        {
          name,
          bio,
          image,
          backgroundImage,
          linktree,
          tags,
        },
      );

      res.json(result);
    } catch (error) {
      logger.error("Set social profile error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to set social profile";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.get(
  "/profile/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const result = await nearSocialManager.getSocialProfile(account_id);
      res.json(result);
    } catch (error) {
      logger.error("Get social profile error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get social profile",
      });
    }
  },
);

nearSocialRoutes.post(
  "/follow/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { target_account_id } = req.body;

      if (!target_account_id) {
        return res.status(400).json({ error: "target_account_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/follow/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      if (wallet.account_id === target_account_id) {
        return res
          .status(400)
          .json({ error: "Cannot follow your own account" });
      }

      const result = await nearSocialManager.socialFollow(
        wallet_id,
        userId,
        target_account_id,
      );
      res.json(result);
    } catch (error) {
      logger.error("Social follow error:", error);
      const msg = error instanceof Error ? error.message : "Failed to follow";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.post(
  "/unfollow/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { target_account_id } = req.body;

      if (!target_account_id) {
        return res.status(400).json({ error: "target_account_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/unfollow/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.socialUnfollow(
        wallet_id,
        userId,
        target_account_id,
      );
      res.json(result);
    } catch (error) {
      logger.error("Social unfollow error:", error);
      const msg = error instanceof Error ? error.message : "Failed to unfollow";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.get(
  "/following/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const result = await nearSocialManager.getFollowing(account_id);
      res.json(result);
    } catch (error) {
      logger.error("Get following error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get following",
      });
    }
  },
);

nearSocialRoutes.get(
  "/followers/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const result = await nearSocialManager.getFollowers(account_id);
      res.json(result);
    } catch (error) {
      logger.error("Get followers error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get followers",
      });
    }
  },
);

nearSocialRoutes.post(
  "/post/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { text, image, tags } = req.body;

      if (!text || text.trim().length === 0) {
        return res.status(400).json({ error: "text is required" });
      }
      if (text.length > 10000) {
        return res
          .status(400)
          .json({ error: "text must not exceed 10000 characters" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/post/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.socialPost(wallet_id, userId, {
        text: text.trim(),
        image,
        tags,
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Social post error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to create post";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.post(
  "/like/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { target_account_id, block_height } = req.body;

      if (!target_account_id) {
        return res.status(400).json({ error: "target_account_id is required" });
      }
      if (block_height === undefined || block_height === null) {
        return res.status(400).json({ error: "block_height is required" });
      }
      if (!Number.isInteger(block_height) || block_height < 0) {
        return res
          .status(400)
          .json({ error: "block_height must be a non-negative integer" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/like/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.socialLike(
        wallet_id,
        userId,
        target_account_id,
        block_height,
      );
      res.json(result);
    } catch (error) {
      logger.error("Social like error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to like post";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.post(
  "/comment/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { target_account_id, block_height, text } = req.body;

      if (!target_account_id) {
        return res.status(400).json({ error: "target_account_id is required" });
      }
      if (block_height === undefined || block_height === null) {
        return res.status(400).json({ error: "block_height is required" });
      }
      if (!Number.isInteger(block_height) || block_height < 0) {
        return res
          .status(400)
          .json({ error: "block_height must be a non-negative integer" });
      }
      if (!text || text.trim().length === 0) {
        return res.status(400).json({ error: "text is required" });
      }
      if (text.length > 5000) {
        return res
          .status(400)
          .json({ error: "text must not exceed 5000 characters" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/comment/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.socialComment(
        wallet_id,
        userId,
        target_account_id,
        block_height,
        text.trim(),
      );

      res.status(201).json(result);
    } catch (error) {
      logger.error("Social comment error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to post comment";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.get(
  "/feed/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const limit = Math.min(
        parseInt((req.query.limit as string) || "10", 10),
        50,
      );
      const result = await nearSocialManager.getSocialFeed(account_id, limit);
      res.json(result);
    } catch (error) {
      logger.error("Get social feed error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get feed",
      });
    }
  },
);

nearSocialRoutes.post(
  "/mazaryn/post/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { text, media_urls, tags } = req.body;

      if (!text || text.trim().length === 0) {
        return res.status(400).json({ error: "text is required" });
      }
      if (text.length > 10000) {
        return res
          .status(400)
          .json({ error: "text must not exceed 10000 characters" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/mazaryn/post/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.mazarynAddPost(wallet_id, userId, {
        text: text.trim(),
        media_urls,
        tags,
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Mazaryn add post error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to create Mazaryn post";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.post(
  "/mazaryn/like/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { post_id } = req.body;

      if (!post_id) {
        return res.status(400).json({ error: "post_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/mazaryn/like/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.mazarynLikePost(
        wallet_id,
        userId,
        post_id,
      );
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn like post error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to like Mazaryn post";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.post(
  "/mazaryn/follow/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { target_account_id } = req.body;

      if (!target_account_id) {
        return res.status(400).json({ error: "target_account_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/mazaryn/follow/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      if (wallet.account_id === target_account_id) {
        return res
          .status(400)
          .json({ error: "Cannot follow your own account" });
      }

      const result = await nearSocialManager.mazarynFollowUser(
        wallet_id,
        userId,
        target_account_id,
      );
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn follow error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to follow on Mazaryn";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.post(
  "/mazaryn/unfollow/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { target_account_id } = req.body;

      if (!target_account_id) {
        return res.status(400).json({ error: "target_account_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/mazaryn/unfollow/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.mazarynUnfollowUser(
        wallet_id,
        userId,
        target_account_id,
      );
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn unfollow error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to unfollow on Mazaryn";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.get(
  "/mazaryn/posts/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const limit = Math.min(
        parseInt((req.query.limit as string) || "20", 10),
        100,
      );
      const offset = parseInt((req.query.offset as string) || "0", 10);
      const result = await nearSocialManager.mazarynGetUserPosts(
        account_id,
        limit,
        offset,
      );
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn get user posts error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get posts",
      });
    }
  },
);

nearSocialRoutes.get(
  "/mazaryn/post/:post_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { post_id } = req.params;
      const result = await nearSocialManager.mazarynGetPost(post_id);
      if (!result.post) {
        return res.status(404).json({ error: "Post not found" });
      }
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn get post error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get post",
      });
    }
  },
);

nearSocialRoutes.get(
  "/mazaryn/feed/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const limit = Math.min(
        parseInt((req.query.limit as string) || "20", 10),
        100,
      );
      const offset = parseInt((req.query.offset as string) || "0", 10);
      const result = await nearSocialManager.mazarynGetFeed(
        account_id,
        limit,
        offset,
      );
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn get feed error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get Mazaryn feed",
      });
    }
  },
);

nearSocialRoutes.get(
  "/mazaryn/followers/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const result = await nearSocialManager.mazarynGetFollowers(account_id);
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn get followers error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get Mazaryn followers",
      });
    }
  },
);

nearSocialRoutes.get(
  "/mazaryn/following/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const result = await nearSocialManager.mazarynGetFollowing(account_id);
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn get following error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get Mazaryn following",
      });
    }
  },
);

nearSocialRoutes.post(
  "/mazaryn/comment/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { post_id, text } = req.body;

      if (!post_id) {
        return res.status(400).json({ error: "post_id is required" });
      }
      if (!text || text.trim().length === 0) {
        return res.status(400).json({ error: "text is required" });
      }
      if (text.length > 5000) {
        return res
          .status(400)
          .json({ error: "text must not exceed 5000 characters" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/mazaryn/comment/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.mazarynAddComment(
        wallet_id,
        userId,
        post_id,
        text.trim(),
      );
      res.status(201).json(result);
    } catch (error) {
      logger.error("Mazaryn add comment error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to add comment";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.post(
  "/mazaryn/tip/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { target_account_id, amount_near } = req.body;

      if (!target_account_id) {
        return res.status(400).json({ error: "target_account_id is required" });
      }
      if (!amount_near) {
        return res.status(400).json({ error: "amount_near is required" });
      }
      if (isNaN(parseFloat(amount_near)) || parseFloat(amount_near) <= 0) {
        return res
          .status(400)
          .json({ error: "amount_near must be a positive number" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/mazaryn/tip/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      if (wallet.account_id === target_account_id) {
        return res.status(400).json({ error: "Cannot tip your own account" });
      }

      const result = await nearSocialManager.mazarynTipUser(
        wallet_id,
        userId,
        target_account_id,
        amount_near,
      );
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn tip error:", error);
      const msg = error instanceof Error ? error.message : "Failed to tip user";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearSocialRoutes.delete(
  "/mazaryn/post/:wallet_id/:post_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, post_id } = req.params;

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/social/mazaryn/post/${wallet_id}/${post_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearSocialManager.mazarynDeletePost(
        wallet_id,
        userId,
        post_id,
      );
      res.json(result);
    } catch (error) {
      logger.error("Mazaryn delete post error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to delete post";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);
