use hiro_system_kit::slog::{self, Logger};
use reqwest::RequestBuilder;
use serde_json::{json, Value as JsonValue};

use crate::chainhook::types::{
    BitcoinBlockData, BlockHeader, BlockIdentifier, StacksBlockData, StacksMicroblockData,
    StacksTransactionData,
};

#[derive(Clone)]
pub struct Context {
    pub logger: Option<Logger>,
    pub tracer: bool,
}

impl Context {
    pub fn empty() -> Context {
        Context {
            logger: None,
            tracer: false,
        }
    }

    pub fn try_log<F>(&self, closure: F)
    where
        F: FnOnce(&Logger),
    {
        if let Some(ref logger) = self.logger {
            closure(logger)
        }
    }

    pub fn expect_logger(&self) -> &Logger {
        self.logger.as_ref().unwrap()
    }
}

pub trait AbstractStacksBlock {
    fn get_identifier(&self) -> &BlockIdentifier;
    fn get_parent_identifier(&self) -> &BlockIdentifier;
    fn get_transactions(&self) -> &Vec<StacksTransactionData>;
    fn get_timestamp(&self) -> i64;
    fn get_serialized_metadata(&self) -> JsonValue;
}

impl AbstractStacksBlock for StacksBlockData {
    fn get_identifier(&self) -> &BlockIdentifier {
        &self.block_identifier
    }

    fn get_parent_identifier(&self) -> &BlockIdentifier {
        &self.parent_block_identifier
    }

    fn get_transactions(&self) -> &Vec<StacksTransactionData> {
        &self.transactions
    }

    fn get_timestamp(&self) -> i64 {
        self.timestamp
    }

    fn get_serialized_metadata(&self) -> JsonValue {
        json!(self.metadata)
    }
}

impl AbstractStacksBlock for StacksMicroblockData {
    fn get_identifier(&self) -> &BlockIdentifier {
        &self.block_identifier
    }

    fn get_parent_identifier(&self) -> &BlockIdentifier {
        &self.parent_block_identifier
    }

    fn get_transactions(&self) -> &Vec<StacksTransactionData> {
        &self.transactions
    }

    fn get_timestamp(&self) -> i64 {
        self.timestamp
    }

    fn get_serialized_metadata(&self) -> JsonValue {
        json!(self.metadata)
    }
}

pub trait AbstractBlock {
    fn get_identifier(&self) -> &BlockIdentifier;
    fn get_parent_identifier(&self) -> &BlockIdentifier;
    fn get_header(&self) -> BlockHeader {
        BlockHeader {
            block_identifier: self.get_identifier().clone(),
            parent_block_identifier: self.get_parent_identifier().clone(),
        }
    }
}

impl AbstractBlock for BlockHeader {
    fn get_identifier(&self) -> &BlockIdentifier {
        &self.block_identifier
    }

    fn get_parent_identifier(&self) -> &BlockIdentifier {
        &self.parent_block_identifier
    }
}

impl AbstractBlock for StacksBlockData {
    fn get_identifier(&self) -> &BlockIdentifier {
        &self.block_identifier
    }

    fn get_parent_identifier(&self) -> &BlockIdentifier {
        &self.parent_block_identifier
    }
}

impl AbstractBlock for StacksMicroblockData {
    fn get_identifier(&self) -> &BlockIdentifier {
        &self.block_identifier
    }

    fn get_parent_identifier(&self) -> &BlockIdentifier {
        &self.parent_block_identifier
    }
}

impl AbstractBlock for BitcoinBlockData {
    fn get_identifier(&self) -> &BlockIdentifier {
        &self.block_identifier
    }

    fn get_parent_identifier(&self) -> &BlockIdentifier {
        &self.parent_block_identifier
    }
}

pub async fn send_request(
    request_builder: RequestBuilder,
    attempts_max: u16,
    attempts_interval_sec: u16,
    ctx: &Context,
) -> Result<(), String> {
    let mut retry = 0;
    loop {
        let request_builder = match request_builder.try_clone() {
            Some(rb) => rb,
            None => {
                ctx.try_log(|logger| slog::warn!(logger, "unable to clone request builder"));
                return Err("internal server error: unable to clone request builder".to_string());
            }
        };
        let err_msg = match request_builder.send().await {
            Ok(res) => {
                if res.status().is_success() {
                    ctx.try_log(|logger| slog::debug!(logger, "Trigger {} successful", res.url()));
                    return Ok(());
                } else {
                    retry += 1;
                    let err_msg =
                        format!("Trigger {} failed with status {}", res.url(), res.status());
                    ctx.try_log(|logger| slog::warn!(logger, "{}", err_msg));
                    err_msg
                }
            }
            Err(e) => {
                retry += 1;
                let err_msg = format!("unable to send request {}", e);
                ctx.try_log(|logger| slog::warn!(logger, "{}", err_msg));
                err_msg
            }
        };
        if retry >= attempts_max {
            let msg: String = format!(
                "unable to send request after several retries. most recent error: {}",
                err_msg
            );
            ctx.try_log(|logger| slog::warn!(logger, "{}", msg));
            return Err(msg);
        }
        std::thread::sleep(std::time::Duration::from_secs(attempts_interval_sec.into()));
    }
}

// TODO: Fold these macros into one generic macro with configurable log levels.
#[macro_export]
macro_rules! try_info {
    ($a:expr, $tag:expr, $($args:tt)*) => {
        $a.try_log(|l| slog::info!(l, $tag, $($args)*));
    };
    ($a:expr, $tag:expr) => {
        $a.try_log(|l| slog::info!(l, $tag));
    };
}

#[macro_export]
macro_rules! try_debug {
    ($a:expr, $tag:expr, $($args:tt)*) => {
        $a.try_log(|l| slog::debug!(l, $tag, $($args)*));
    };
    ($a:expr, $tag:expr) => {
        $a.try_log(|l| slog::debug!(l, $tag));
    };
}

#[macro_export]
macro_rules! try_warn {
    ($a:expr, $tag:expr, $($args:tt)*) => {
        $a.try_log(|l| slog::warn!(l, $tag, $($args)*));
    };
    ($a:expr, $tag:expr) => {
        $a.try_log(|l| slog::warn!(l, $tag));
    };
}

#[macro_export]
macro_rules! try_error {
    ($a:expr, $tag:expr, $($args:tt)*) => {
        $a.try_log(|l| slog::error!(l, $tag, $($args)*));
    };
    ($a:expr, $tag:expr) => {
        $a.try_log(|l| slog::error!(l, $tag));
    };
}
