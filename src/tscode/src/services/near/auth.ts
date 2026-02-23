import { JsonRpcProvider } from "near-api-js";
import { verifyMessage } from "near-api-js/nep413";
import { nearConnection } from "./connection.js";
import { logger } from "../../core/logger.js";
import type {
  VerifyNep413MessageRequest,
  VerifyNep413MessageResponse,
} from "../../core/near-types.js";

export class NearAuthManager {
  async verifyNep413Message(
    req: VerifyNep413MessageRequest,
  ): Promise<VerifyNep413MessageResponse> {
    try {
      logger.info(
        `Verifying NEP-413 message for account: ${req.signer_account_id}`,
      );

      const provider = nearConnection.getProvider() as JsonRpcProvider;

      const signatureBytes = new Uint8Array(
        Buffer.from(req.signature_base64, "base64"),
      );
      const nonceBytes = Buffer.from(req.nonce_hex, "hex");

      await verifyMessage({
        signerAccountId: req.signer_account_id,
        signerPublicKey: req.signer_public_key,
        signature: signatureBytes,
        payload: {
          message: req.message,
          recipient: req.recipient,
          nonce: nonceBytes,
        },
        provider,
      });

      logger.info(`NEP-413 message verified for ${req.signer_account_id}`);

      return {
        valid: true,
        signer_account_id: req.signer_account_id,
        message: req.message,
        recipient: req.recipient,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.warn(
        `NEP-413 verification failed for ${req.signer_account_id}:`,
        error,
      );

      return {
        valid: false,
        signer_account_id: req.signer_account_id,
        message: req.message,
        recipient: req.recipient,
        timestamp: Date.now(),
      };
    }
  }
}

export const nearAuthManager = new NearAuthManager();
