use crate::chainhook::observer::MempoolAdmissionData;
use crate::chainhook::types::{BitcoinChainEvent, StacksChainEvent};
use crate::event::{ProtocolDeployingData, ServiceStatusData};
use crate::log::LogData;

/// Trait for handling devnet events in a display-agnostic way.
/// Implemented by both the TUI (`App`) and the log-based output (`LogEventLogger`).
pub trait DevnetEventLogger {
    fn handle_log(&mut self, log: LogData);
    fn handle_service_status(&mut self, status: ServiceStatusData);
    fn handle_stacks_chain_event(&mut self, event: &StacksChainEvent);
    fn handle_bitcoin_chain_event(&mut self, event: &BitcoinChainEvent);
    fn handle_mempool_admission(&mut self, tx: MempoolAdmissionData);
    fn handle_protocol_deploying_progress(&mut self, data: ProtocolDeployingData);
    fn handle_boot_completed(&mut self);
    fn handle_fatal_error(&mut self, message: &str);
}
