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
use clarity::vm::ExecutionResult;
use clarity_types::effects::AssetMap;
use clarity_types::types::BoundedErrorString;

use super::datastore::Datastore;
use super::interpreter::{BlockInclusion, NonceCharge};

/// Keeps the outer database frame alive while the upstream helper owns the
/// nested payload frame. Hooks are reborrowed for execution and remain available
/// to Clarinet for completion notifications after settlement.
///
/// `db` and `cost_tracker` are only `None` while a trait method has lent them
/// out, so the accessors below never observe them missing.
pub(crate) struct SimnetTransactionConnection<'db, 'hooks> {
    db: Option<ClarityDatabase<'db>>,
    cost_tracker: Option<LimitedCostTracker>,
    config: TransactionConfig,
    hooks: Vec<&'hooks mut dyn EvalHook>,
    datastore: &'db Datastore,
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

impl<'db, 'hooks> SimnetTransactionConnection<'db, 'hooks> {
    pub fn new(
        db: ClarityDatabase<'db>,
        cost_tracker: LimitedCostTracker,
        config: TransactionConfig,
        hooks: Vec<&'hooks mut dyn EvalHook>,
        datastore: &'db Datastore,
    ) -> Self {
        Self {
            db: Some(db),
            cost_tracker: Some(cost_tracker),
            config,
            hooks,
            datastore,
        }
    }

    pub fn db(&mut self) -> &mut ClarityDatabase<'db> {
        self.db.as_mut().expect("transaction database")
    }

    pub fn cost_tracker(&self) -> &LimitedCostTracker {
        self.cost_tracker
            .as_ref()
            .expect("transaction cost tracker")
    }

    /// Tell every hook how the transaction ended, after settlement.
    pub fn did_complete(&mut self, mut result: Result<&mut ExecutionResult, String>) {
        for hook in &mut self.hooks {
            hook.did_complete(result.as_deref_mut().map_err(|error| error.clone()));
        }
    }

    /// Open the transaction-level frame before invoking a nested payload.
    pub fn begin(&mut self) {
        self.db().begin();
    }

    /// An included failure keeps transaction-level writes; a rejection keeps none.
    pub fn settle(
        &mut self,
        charge: &NonceCharge,
        inclusion: BlockInclusion,
    ) -> Result<(), String> {
        let db = self.db();
        if !inclusion.is_included() {
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
