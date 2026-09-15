use clarinet_deployments::onchain::TransactionTracker;
use ratatui::widgets::ListState;

pub struct StatefulList<T> {
    pub state: ListState,
    pub items: Vec<T>,
}

pub struct App<'a> {
    pub node_url: &'a str,
    pub transactions: StatefulList<TransactionTracker>,
}

impl<'a> App<'a> {
    pub fn new(node_url: &'a str, transaction_trackers: Vec<TransactionTracker>) -> App<'a> {
        App {
            node_url,
            transactions: StatefulList {
                state: ListState::default(),
                items: transaction_trackers,
            },
        }
    }

    pub fn on_tick(&self) {}

    pub fn reset(&self) {}

    pub fn display_contract_status_update(&mut self, update: TransactionTracker) {
        self.transactions.items.remove(update.index);
        self.transactions.items.insert(update.index, update);
    }
}
