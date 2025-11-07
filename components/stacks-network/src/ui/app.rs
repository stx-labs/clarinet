use chainhook_sdk::types::{
    StacksBlockData, StacksMicroblockData, StacksTransactionData, StacksTransactionKind,
};
use chainhook_sdk::utils::Context;
use hiro_system_kit::slog;
use ratatui::prelude::*;

use super::util::{StatefulList, TabsState};
use crate::event::ServiceStatusData;
use crate::{LogData, MempoolAdmissionData};

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
}

impl<'a> App<'a> {
    pub fn new(title: &'a str, devnet_path: &'a str) -> App<'a> {
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

    pub fn display_log(&mut self, log: LogData, ctx: &Context) {
        use crate::LogLevel;
        match &log.level {
            LogLevel::Error => ctx.try_log(|logger| slog::error!(logger, "{}", log.message)),
            LogLevel::Warning => ctx.try_log(|logger| slog::warn!(logger, "{}", log.message)),
            LogLevel::Debug => ctx.try_log(|logger| slog::debug!(logger, "{}", log.message)),
            LogLevel::Info | &LogLevel::Success => {
                ctx.try_log(|logger| slog::info!(logger, "{}", log.message))
            }
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
}
