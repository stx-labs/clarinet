use hiro_system_kit::{slog, slog_term, Drain};
use observer::event_handler::MempoolAdmissionData;
use observer::types::{
    BitcoinChainEvent, StacksChainEvent, StacksTransactionData, StacksTransactionKind,
};

use crate::event::{ProtocolDeployingData, ServiceStatusData, Status};
use crate::event_logger::DevnetEventLogger;
use crate::log::{LogData, LogLevel};

pub struct LogEventLogger {
    logger: slog::Logger,
}

impl LogEventLogger {
    pub fn new() -> Self {
        let plain = slog_term::PlainSyncDecorator::new(std::io::stdout());
        let logger =
            slog::Logger::root(slog_term::FullFormat::new(plain).build().fuse(), slog::o!());
        Self { logger }
    }
}

impl Default for LogEventLogger {
    fn default() -> Self {
        Self::new()
    }
}

impl DevnetEventLogger for LogEventLogger {
    fn handle_log(&mut self, log: LogData) {
        match log.level {
            LogLevel::Debug => slog::debug!(self.logger, "{}", log.message),
            LogLevel::Info | LogLevel::Success => slog::info!(self.logger, "{}", log.message),
            LogLevel::Warning => slog::warn!(self.logger, "{}", log.message),
            LogLevel::Error => slog::error!(self.logger, "{}", log.message),
        }
    }

    fn handle_service_status(&mut self, status: ServiceStatusData) {
        let msg = format!("{} - {}", status.name, status.comment);
        match status.status {
            Status::Green => slog::info!(self.logger, "{}", msg),
            Status::Yellow => slog::warn!(self.logger, "{}", msg),
            Status::Red => slog::error!(self.logger, "{}", msg),
        }
    }

    fn handle_stacks_chain_event(&mut self, event: &StacksChainEvent) {
        let transactions: Vec<&StacksTransactionData> = match event {
            StacksChainEvent::ChainUpdatedWithBlocks(update) => update
                .new_blocks
                .iter()
                .flat_map(|b| b.block.transactions.iter())
                .collect(),
            StacksChainEvent::ChainUpdatedWithMicroblocks(update) => update
                .new_microblocks
                .iter()
                .flat_map(|b| b.transactions.iter())
                .collect(),
            _ => vec![],
        };

        for tx in transactions {
            log_transaction(&self.logger, tx);
        }
    }

    fn handle_bitcoin_chain_event(&mut self, _event: &BitcoinChainEvent) {}

    fn handle_mempool_admission(&mut self, tx: MempoolAdmissionData) {
        slog::info!(self.logger, "Mempool tx: {}", tx.tx_description);
    }

    fn handle_protocol_deploying_progress(&mut self, data: ProtocolDeployingData) {
        if data.new_contracts_deployed.is_empty() {
            return;
        }
        let contracts = data.new_contracts_deployed.join(", ");
        slog::info!(self.logger, "Contracts deployed: {}", contracts);
    }

    fn handle_boot_completed(&mut self) {
        slog::info!(self.logger, "Local Devnet network ready");
    }

    fn handle_fatal_error(&mut self, message: &str) {
        slog::error!(self.logger, "Fatal: {}", message);
    }
}

fn log_transaction(logger: &slog::Logger, tx: &StacksTransactionData) {
    let status = if tx.metadata.success { "ok" } else { "err" };

    match &tx.metadata.kind {
        StacksTransactionKind::ContractDeployment(data) => {
            slog::info!(
                logger,
                "  {} deploy {} [{}]",
                status,
                data.contract_identifier,
                tx.metadata.result
            );
        }
        StacksTransactionKind::ContractCall(data) => {
            slog::info!(
                logger,
                "  {} call {}.{} [{}]",
                status,
                data.contract_identifier,
                data.method,
                tx.metadata.result
            );
        }
        StacksTransactionKind::NativeTokenTransfer => {
            slog::info!(logger, "  {} transfer [{}]", status, tx.metadata.result);
        }
        StacksTransactionKind::Coinbase | StacksTransactionKind::TenureChange => {}
        other => {
            slog::info!(logger, "  {} {:?} [{}]", status, other, tx.metadata.result);
        }
    }
}
