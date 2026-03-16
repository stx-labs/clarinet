use hiro_system_kit::slog;
use observer::types::{
    BitcoinChainEvent, StacksBlockData, StacksChainEvent, StacksMicroblockData,
    StacksTransactionData, StacksTransactionKind,
};
use observer::utils::Context;
use ratatui::prelude::*;

use super::util::{StatefulList, TabsState};
use crate::event::{ProtocolDeployingData, ServiceStatusData};
use crate::event_logger::DevnetEventLogger;
use crate::log::{LogData, LogLevel};
use crate::MempoolAdmissionData;

pub enum BlockData {
    Block(Box<StacksBlockData>),
    Microblock(StacksMicroblockData),
}

pub struct App<'a> {
    pub title: &'a str,
    pub devnet_path: &'a str,
    pub should_quit: bool,
    pub blocks: Vec<BlockData>,
    pub tabs: TabsState<'a>,
    pub transactions: StatefulList<StacksTransactionData>,
    pub mempool: StatefulList<MempoolAdmissionData>,
    pub logs: StatefulList<LogData>,
    pub services: StatefulList<ServiceStatusData>,
    pub ctx: Context,
}

impl<'a> App<'a> {
    pub fn new(title: &'a str, devnet_path: &'a str, ctx: Context) -> App<'a> {
        App {
            title,
            devnet_path,
            should_quit: false,
            tabs: TabsState::new(),
            blocks: vec![],
            transactions: StatefulList::with_items(vec![]),
            mempool: StatefulList::with_items(vec![]),
            logs: StatefulList::with_items(vec![]),
            services: StatefulList::with_items(vec![]),
            ctx,
        }
    }

    pub fn on_up(&mut self) {
        self.transactions.previous();
    }

    pub fn on_down(&mut self) {
        self.transactions.next();
    }

    pub fn on_right(&mut self) {
        self.tabs.next();
    }

    pub fn on_left(&mut self) {
        self.tabs.previous();
    }

    pub fn on_key(&mut self, c: char) {
        if c == 'q' {
            self.should_quit = true;
        }
    }

    pub fn on_tick(&mut self) {}

    pub fn reset(&mut self) {
        self.tabs = TabsState::new();
        self.blocks = vec![];
        self.transactions = StatefulList::with_items(vec![]);
        self.mempool = StatefulList::with_items(vec![]);
        self.logs = StatefulList::with_items(vec![]);
    }

    pub fn display_service_status_update(&mut self, service_update: ServiceStatusData) {
        let existing_index = self
            .services
            .items
            .iter()
            .position(|s| s.name == service_update.name);

        if let Some(index) = existing_index {
            self.services.items[index] = service_update;
        } else {
            let insertion_index = self
                .services
                .items
                .iter()
                .position(|s| s.order > service_update.order)
                .unwrap_or(self.services.items.len());
            self.services.items.insert(insertion_index, service_update);
        }
    }

    pub fn display_log(&mut self, log: LogData) {
        match &log.level {
            LogLevel::Error => self
                .ctx
                .try_log(|logger| slog::error!(logger, "{}", log.message)),
            LogLevel::Warning => self
                .ctx
                .try_log(|logger| slog::warn!(logger, "{}", log.message)),
            LogLevel::Debug => self
                .ctx
                .try_log(|logger| slog::debug!(logger, "{}", log.message)),
            LogLevel::Info | LogLevel::Success => self
                .ctx
                .try_log(|logger| slog::info!(logger, "{}", log.message)),
        }
        self.logs.items.push(log);
    }

    pub fn add_to_mempool(&mut self, tx: MempoolAdmissionData) {
        self.mempool.items.push(tx);
    }

    pub fn display_block(&mut self, block: StacksBlockData) {
        let has_tenure_change_tx = block
            .transactions
            .iter()
            .any(|tx| tx.metadata.kind == StacksTransactionKind::TenureChange);

        let has_coinbase_tx = block
            .transactions
            .iter()
            .any(|tx| tx.metadata.kind == StacksTransactionKind::Coinbase);

        let (start, end) = if !has_coinbase_tx {
            ("", "")
        } else if block.metadata.pox_cycle_position == (block.metadata.pox_cycle_length - 1) {
            ("", "<")
        } else if block.metadata.pox_cycle_position == 0 {
            (">", "")
        } else {
            ("", "")
        };

        let has_tx = if (block.transactions.len()
            - has_coinbase_tx as usize
            - has_tenure_change_tx as usize)
            == 0
        {
            ""
        } else {
            "␂"
        };

        self.tabs.titles.push_front(Span::styled(
            format!(
                "{}[{}{}]{}",
                end, block.block_identifier.index, has_tx, start
            ),
            if has_coinbase_tx {
                Style::default().fg(Color::Green)
            } else {
                Style::default().fg(Color::LightBlue)
            },
        ));

        self.blocks.push(BlockData::Block(Box::new(block)));

        if self.tabs.index != 0 {
            self.tabs.index += 1;
        }
    }

    pub fn display_microblock(&mut self, block: StacksMicroblockData) {
        self.tabs
            .titles
            .push_front(Span::from("[·]".to_string()).fg(Color::White));
        self.blocks.push(BlockData::Microblock(block));
        if self.tabs.index != 0 {
            self.tabs.index += 1;
        }
    }

    fn remove_mempool_txs_by_raw(&mut self, raw_txs: &[&str]) {
        let mut indices_to_remove: Vec<usize> = self
            .mempool
            .items
            .iter()
            .enumerate()
            .filter(|(_, item)| raw_txs.contains(&item.tx_data.as_str()))
            .map(|(idx, _)| idx)
            .collect();
        indices_to_remove.reverse();
        for i in indices_to_remove {
            self.mempool.items.remove(i);
        }
    }
}

impl DevnetEventLogger for App<'_> {
    fn handle_log(&mut self, log: LogData) {
        self.display_log(log);
    }

    fn handle_service_status(&mut self, status: ServiceStatusData) {
        self.display_service_status_update(status);
    }

    fn handle_stacks_chain_event(&mut self, event: &StacksChainEvent) {
        match event {
            StacksChainEvent::ChainUpdatedWithBlocks(update) => {
                let raw_txs: Vec<&str> = if self.mempool.items.is_empty() {
                    vec![]
                } else {
                    update
                        .new_blocks
                        .iter()
                        .flat_map(|b| {
                            b.block
                                .transactions
                                .iter()
                                .map(|tx| tx.metadata.raw_tx.as_str())
                        })
                        .collect()
                };
                self.remove_mempool_txs_by_raw(&raw_txs);
                for block_update in update.new_blocks.iter() {
                    self.display_block(block_update.block.clone());
                }
            }
            StacksChainEvent::ChainUpdatedWithMicroblocks(update) => {
                let raw_txs: Vec<&str> = if self.mempool.items.is_empty() {
                    vec![]
                } else {
                    update
                        .new_microblocks
                        .iter()
                        .flat_map(|b| b.transactions.iter().map(|tx| tx.metadata.raw_tx.as_str()))
                        .collect()
                };
                self.remove_mempool_txs_by_raw(&raw_txs);
                for microblock in update.new_microblocks.iter() {
                    self.display_microblock(microblock.clone());
                }
            }
            _ => {}
        }
    }

    fn handle_bitcoin_chain_event(&mut self, _event: &BitcoinChainEvent) {}

    fn handle_mempool_admission(&mut self, tx: MempoolAdmissionData) {
        self.add_to_mempool(tx);
    }

    fn handle_protocol_deploying_progress(&mut self, _data: ProtocolDeployingData) {}

    fn handle_boot_completed(&mut self) {
        self.display_log(LogData::new(
            LogLevel::Success,
            "Local Devnet network ready".into(),
        ));
    }

    fn handle_fatal_error(&mut self, message: &str) {
        self.display_log(LogData::new(LogLevel::Error, format!("Fatal: {message}")));
    }
}
