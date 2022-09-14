use serde::{Deserialize, Serialize};
use std::io::BufWriter;

#[derive(Serialize, Deserialize, Debug)]
pub struct Wallet {
    pub secret_key: String,
    pub public_key: String,
    pub public_address: String,
}
