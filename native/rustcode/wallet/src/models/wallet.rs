use serde::{Deserialize, Serialize};
use std::io::BufWriter;
use chrono::NaiveDateTime;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Wallet {
    pub id: i32,
    pub wallet_address: String,
    pub amount: String,
    pub secret_key: String,
    pub public_key: String,
    pub created_at: NaiveDateTime,
    pub updated_at: NaiveDateTime
}

#[derive(Debug, Clone, Insertable, Serialize, Deserialize)]
pub struct NewWallet {
    pub wallet_address: String,
    pub public_key: String,
    pub amount: String
}
impl NewWallet {
    pub fn new(wallet_address: String, public_key: String, amount: String) -> NewWallet {
        NewWallet {
            wallet_address,
            public_key,
            amount
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WalletInfo {
    pub wallet_address: String,
    pub pblice_key: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BalanceInfo {
    pub amount: String
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Transfer {
    pub sender_wallet_address: String,
    pub receiver_wallet_address: String,
    pub sender_public_key: String,
    pub amount: String,
}
    

