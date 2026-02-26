use std::collections::{BTreeSet, VecDeque};
use std::fs::{self, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

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

pub fn file_append(path: String, bytes: Vec<u8>, ctx: &Context) -> Result<(), String> {
    let mut file_path = match std::env::current_dir() {
        Err(e) => {
            let msg = format!("unable to retrieve current_dir {}", e);
            ctx.try_log(|logger| slog::warn!(logger, "{}", msg));
            return Err(msg);
        }
        Ok(p) => p,
    };
    file_path.push(path);
    if !file_path.exists() {
        match std::fs::File::create(&file_path) {
            Ok(ref mut file) => {
                let _ = file.write_all(&bytes);
            }
            Err(e) => {
                let msg = format!("unable to create file {}: {}", file_path.display(), e);
                ctx.try_log(|logger| slog::warn!(logger, "{}", msg));
                return Err(msg);
            }
        }
    }

    let mut file = match OpenOptions::new()
        .create(false)
        .append(true)
        .open(file_path)
    {
        Err(e) => {
            let msg = format!("unable to open file {}", e);
            ctx.try_log(|logger| slog::warn!(logger, "{}", msg));
            return Err(msg);
        }
        Ok(p) => p,
    };

    let utf8 = match String::from_utf8(bytes) {
        Ok(string) => string,
        Err(e) => {
            let msg = format!("unable serialize bytes as utf8 string {}", e);
            ctx.try_log(|logger| slog::warn!(logger, "{}", msg));
            return Err(msg);
        }
    };

    if let Err(e) = writeln!(file, "{}", utf8) {
        let msg = format!("unable to open file {}", e);
        ctx.try_log(|logger| slog::warn!(logger, "{}", msg));
        eprintln!("Couldn't write to file: {}", e);
        return Err(msg);
    }

    Ok(())
}

#[derive(Debug)]
pub enum BlockHeightsError {
    ExceedsMaxEntries(u64, u64),
    StartLargerThanEnd,
}

pub enum BlockHeights {
    BlockRange(u64, u64),
    Blocks(Vec<u64>),
}
pub const MAX_BLOCK_HEIGHTS_ENTRIES: u64 = 1_000_000;
impl BlockHeights {
    pub fn get_sorted_entries(&self) -> Result<VecDeque<u64>, BlockHeightsError> {
        let mut entries = VecDeque::new();
        match &self {
            BlockHeights::BlockRange(start, end) => {
                if start > end {
                    return Err(BlockHeightsError::StartLargerThanEnd);
                }
                if (end - start) > MAX_BLOCK_HEIGHTS_ENTRIES {
                    return Err(BlockHeightsError::ExceedsMaxEntries(
                        MAX_BLOCK_HEIGHTS_ENTRIES,
                        end - start,
                    ));
                }
                for i in *start..=*end {
                    entries.push_back(i);
                }
            }
            BlockHeights::Blocks(heights) => {
                if heights.len() as u64 > MAX_BLOCK_HEIGHTS_ENTRIES {
                    return Err(BlockHeightsError::ExceedsMaxEntries(
                        MAX_BLOCK_HEIGHTS_ENTRIES,
                        heights.len() as u64,
                    ));
                }
                let mut sorted_entries = heights.clone();
                sorted_entries.sort();
                let mut unique_sorted_entries = BTreeSet::new();
                for entry in sorted_entries.into_iter() {
                    unique_sorted_entries.insert(entry);
                }
                for entry in unique_sorted_entries.into_iter() {
                    entries.push_back(entry)
                }
            }
        }
        Ok(entries)
    }
}

#[test]
fn test_block_heights_range_construct() {
    let range = BlockHeights::BlockRange(0, 10);
    let mut entries = range.get_sorted_entries().unwrap();

    let mut cursor = 0;
    while let Some(entry) = entries.pop_front() {
        assert_eq!(entry, cursor);
        cursor += 1;
    }
    assert_eq!(11, cursor);
}

#[test]
fn test_block_heights_range_limits_entries() {
    let range = BlockHeights::BlockRange(0, MAX_BLOCK_HEIGHTS_ENTRIES + 1);
    match range.get_sorted_entries() {
        Ok(_) => panic!("Expected block heights range to error when exceeding max entries"),
        Err(e) => match e {
            BlockHeightsError::ExceedsMaxEntries(_, _) => {}
            BlockHeightsError::StartLargerThanEnd => {
                panic!("Wrong error reported from exceeding block heights range max entries")
            }
        },
    };
}

#[test]
fn test_block_heights_range_enforces_order() {
    let range = BlockHeights::BlockRange(1, 0);
    match range.get_sorted_entries() {
        Ok(_) => panic!("Expected block heights range to error when exceeding max entries"),
        Err(e) => match e {
            BlockHeightsError::ExceedsMaxEntries(_, _) => {
                panic!("Wrong error reported from supplying start/end out of order in block heights range")
            }
            BlockHeightsError::StartLargerThanEnd => {}
        },
    };
}

#[test]
fn test_block_heights_blocks_construct() {
    let range = BlockHeights::Blocks(vec![0, 3, 5, 6, 6, 10, 9]);
    let expected = vec![0, 3, 5, 6, 9, 10];
    let entries = range.get_sorted_entries().unwrap();

    for (entry, expectation) in entries.iter().zip(expected) {
        assert_eq!(*entry, expectation);
    }
}

#[test]
fn test_block_heights_blocks_limits_entries() {
    let mut too_big = vec![];
    for i in 0..MAX_BLOCK_HEIGHTS_ENTRIES + 1 {
        too_big.push(i);
    }
    let range = BlockHeights::Blocks(too_big);
    match range.get_sorted_entries() {
        Ok(_) => panic!("Expected block heights blocks to error when exceeding max entries"),
        Err(e) => match e {
            BlockHeightsError::ExceedsMaxEntries(_, _) => {}
            BlockHeightsError::StartLargerThanEnd => {
                panic!("Wrong error reported from exceeding block heights blocks max entries")
            }
        },
    };
}

pub fn read_file_content_at_path(file_path: &Path) -> Result<Vec<u8>, String> {
    use std::fs::File;
    use std::io::BufReader;

    let file = File::open(file_path)
        .map_err(|e| format!("unable to read file {}\n{:?}", file_path.display(), e))?;
    let mut file_reader = BufReader::new(file);
    let mut file_buffer = vec![];
    file_reader
        .read_to_end(&mut file_buffer)
        .map_err(|e| format!("unable to read file {}\n{:?}", file_path.display(), e))?;
    Ok(file_buffer)
}

pub fn write_file_content_at_path(file_path: &PathBuf, content: &[u8]) -> Result<(), String> {
    use std::fs::File;
    let mut parent_directory = file_path.clone();
    parent_directory.pop();
    fs::create_dir_all(&parent_directory).map_err(|e| {
        format!(
            "unable to create parent directory {}\n{}",
            parent_directory.display(),
            e
        )
    })?;
    let mut file = File::create(file_path)
        .map_err(|e| format!("unable to open file {}\n{}", file_path.display(), e))?;
    file.write_all(content)
        .map_err(|e| format!("unable to write file {}\n{}", file_path.display(), e))?;
    Ok(())
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
