use serde::{Serialize, Deserialize};
use chrono::NaiveDateTime;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transaction {
    pub id: i32,
    pub amount: String,
    pub transaction_address: String,
    pub sender_wallet: String,
    pub receiver_wallet: String,
    pub transaction_signature: String,
    pub transaction_type: String,
    pub transaction_fee: String,
    pub transaction_status: String,
    pub created_at: NaiveDateTime
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewTransaction {
    pub amount: String,
    pub sender_wallet: String,
    pub receiver_wallet: String,
    pub transaction_address: String,
    pub transaction_signature: String,
    pub transaction_type: String,
    pub transaction_fee : String,
    pub transaction_status: String,
}
impl NewTransaction {
    pub fn new(amount: String,sender_wallet: String, receiver_wallet: String, transaction_address: String, transaction_signature: String, transaction_type: String, transaction_fee : String, transaction_status: String ) -> NewTransaction {
       NewTransaction {
           amount,
           sender_wallet,
           receiver_wallet,
           transaction_address,
           transaction_signature,
           transaction_type,
           transaction_fee,
           transaction_status,
         }
     }
 }