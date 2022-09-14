//! Key Errors.
use std::fmt;

/// Key Errors.
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Public key format error.
    InvalidPublic,
    /// Digest data format error.
    InvalidMessage,
    /// Signature data format error.
    InvalidSignature,
    /// Invalid checksum of base58check.
    InvalidChecksum,
    /// Private key format error.
    InvalidPrivate,
    /// Invalid address format.
    InvalidAddress,
    /// Unable to generate a key pair.
    FailedKeyGeneration,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match *self {
            Error::InvalidPublic => "Invalid Public",
            Error::InvalidMessage => "Invalid Message",
            Error::InvalidSignature => "Invalid Signature",
            Error::InvalidChecksum => "Invalid Checksum",
            Error::InvalidPrivate => "Invalid Private",
            Error::InvalidAddress => "Invalid Address",
            Error::FailedKeyGeneration => "Key generation failed",
        };

        msg.fmt(f)
    }
}

impl std::error::Error for Error {}