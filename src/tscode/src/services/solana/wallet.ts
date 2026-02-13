import { generateKeyPairSigner } from "@solana/kit";

export class AccountManager {
  async createKeyPairSigner() {
    try {
      const wallet = await generateKeyPairSigner();
      console.log("Wallet created", wallet);
    } catch (error) {
      console.log("Something went wrong", error);
    }
  }
}
