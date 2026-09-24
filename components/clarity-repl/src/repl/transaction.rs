//! Simnet's connection to the upstream Clarity transaction frame.

use clarity::types::StacksEpochId;
use clarity::vm::analysis::AnalysisDatabase;
use clarity::vm::clarity::{
    execute_with_abort_callback, ClarityConnection, TransactionConfig, TransactionConnection,
    TransactionOutput,
};
use clarity::vm::contexts::OwnedEnvironment;
use clarity::vm::costs::{CostTracker, LimitedCostTracker};
use clarity::vm::database::ClarityDatabase;
use clarity::vm::errors::VmExecutionError;
use clarity::vm::events::StacksTransactionEvent;
use clarity::vm::hooks::EvalHook;
use clarity_types::effects::AssetMap;
use clarity_types::types::BoundedErrorString;

use super::datastore::Datastore;

/// Keeps the outer database frame alive while the upstream helper owns the
/// nested payload frame. Hooks are reborrowed for execution and remain available
/// to Clarinet for completion notifications after settlement.
pub(crate) struct SimnetTransactionConnection<'db, 'hooks> {
    pub db: Option<ClarityDatabase<'db>>,
    pub cost_tracker: Option<LimitedCostTracker>,
    pub config: TransactionConfig,
    pub hooks: Vec<&'hooks mut dyn EvalHook>,
    pub datastore: &'db Datastore,
}

impl ClarityConnection for SimnetTransactionConnection<'_, '_> {
    fn with_clarity_db_readonly_owned<F, R>(&mut self, to_do: F) -> R
    where
        F: FnOnce(ClarityDatabase) -> (R, ClarityDatabase),
    {
        let mut db = self.db.take().expect("transaction database");
        db.begin();
        let (result, mut db) = to_do(db);
        db.roll_back().expect("rollback read-only database");
        self.db = Some(db);
        result
    }

    fn with_analysis_db_readonly<F, R>(&mut self, to_do: F) -> R
    where
        F: FnOnce(&mut AnalysisDatabase) -> R,
    {
        self.with_analysis_db(|db, tracker| {
            db.begin();
            let result = to_do(db);
            db.roll_back().expect("rollback read-only analysis");
            (tracker, result)
        })
    }

    fn get_epoch(&self) -> StacksEpochId {
        self.config.epoch
    }
}

impl TransactionConnection for SimnetTransactionConnection<'_, '_> {
    fn with_abort_callback<'hooks, F, A, R, E>(
        &'hooks mut self,
        to_do: F,
        abort_callback: A,
    ) -> Result<TransactionOutput<R>, E>
    where
        A: FnOnce(&AssetMap, &mut ClarityDatabase) -> Option<BoundedErrorString>,
        F: FnOnce(
            &mut OwnedEnvironment<'_, 'hooks>,
        ) -> Result<(R, AssetMap, Vec<StacksTransactionEvent>), E>,
        E: From<VmExecutionError>,
    {
        let db = self.db.take().expect("transaction database");
        let tracker = self.cost_tracker.take().expect("transaction cost tracker");
        let (db, tracker, result) = execute_with_abort_callback(
            db,
            tracker,
            self.config,
            |env| {
                for hook in self.hooks.iter_mut() {
                    env.add_eval_hook(&mut **hook);
                }
                to_do(env)
            },
            abort_callback,
        );
        self.db = Some(db);
        self.cost_tracker = Some(tracker);
        result
    }

    fn with_analysis_db<F, R>(&mut self, to_do: F) -> R
    where
        F: FnOnce(&mut AnalysisDatabase, LimitedCostTracker) -> (LimitedCostTracker, R),
    {
        let db = self.db.take().expect("transaction database");
        // Preserve the outer rollback log when switching database views, so
        // analysis and contract storage commit or roll back together.
        let mut analysis_db = AnalysisDatabase::new_with_rollback_wrapper(db.destroy());
        let tracker = self.cost_tracker.take().expect("transaction cost tracker");
        let (tracker, result) = to_do(&mut analysis_db, tracker);
        self.db = Some(ClarityDatabase::new_with_rollback_wrapper(
            analysis_db.destroy(),
            self.datastore,
            self.datastore,
        ));
        self.cost_tracker = Some(tracker);
        result
    }
}

impl Drop for SimnetTransactionConnection<'_, '_> {
    fn drop(&mut self) {
        if let Some(tracker) = &mut self.cost_tracker {
            tracker.reset_memory();
        }
    }
}

impl SimnetTransactionConnection<'_, '_> {
    /// Open the transaction-level frame before invoking a nested payload.
    pub fn begin(&mut self) {
        self.db.as_mut().expect("transaction database").begin();
    }

    /// An included failure keeps transaction-level writes; a rejection keeps none.
    pub fn settle(
        &mut self,
        charge: &super::interpreter::NonceCharge,
        included: bool,
    ) -> Result<(), String> {
        let db = self.db.as_mut().expect("transaction database");
        if !included {
            return db
                .roll_back()
                .map_err(|e| format!("failed to roll back transaction: {e}"));
        }
        if let Err(error) = charge.apply(db) {
            db.roll_back()
                .map_err(|e| format!("{error}; failed to roll back transaction: {e}"))?;
            return Err(error);
        }
        db.commit()
            .map_err(|e| format!("failed to commit transaction: {e}"))
    }
}
