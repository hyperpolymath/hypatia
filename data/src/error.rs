// SPDX-License-Identifier: PMPL-1.0-or-later
//! Error types for data layer

use thiserror::Error;

#[derive(Error, Debug)]
pub enum DataError {
    #[error("ArangoDB error: {0}")]
    ArangoError(String),

    #[error("Dragonfly/Redis error: {0}")]
    DragonflyError(#[from] redis::RedisError),

    #[error("Connection error: {0}")]
    ConnectionError(String),

    #[error("Query error: {0}")]
    QueryError(String),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Not found: {0}")]
    NotFound(String),

    #[error("Invalid data: {0}")]
    InvalidData(String),

    #[error("Pool error: {0}")]
    PoolError(String),

    #[error("Cache miss: {0}")]
    CacheMiss(String),

    #[error("Transaction error: {0}")]
    TransactionError(String),
}

pub type Result<T> = std::result::Result<T, DataError>;
