import { generateKeyPair, generateKeyPairSigner } from "@solana/kit";

export class AccountManager {
  async createKeyPairSigner() {
    try {
      const wallet = await generateKeyPairSigner();
    } catch (error) {
      console.log("Something went wrong", error);
    }
  }
}

console.log("Not implemented yet...!");
