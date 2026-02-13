use std::fmt::{Display, Formatter, Result};
use std::path::Path;

use clarinet_deployments::types::{DeploymentSpecification, TransactionSpecification};

pub struct DeploymentSynthesis {
    pub blocks_count: u64,
    pub total_cost: u64,
    pub content: String,
}

impl DeploymentSynthesis {
    pub fn from_deployment(
        deployment: &DeploymentSpecification,
        project_root: &Path,
    ) -> DeploymentSynthesis {
        let mut blocks_count = 0;
        let mut total_cost = 0;
        for batch in deployment.plan.batches.iter() {
            blocks_count += 1;
            for tx in batch.transactions.iter() {
                match tx {
                    TransactionSpecification::ContractCall(tx) => {
                        total_cost += tx.cost;
                    }
                    TransactionSpecification::ContractPublish(tx) => {
                        total_cost += tx.cost;
                    }
                    TransactionSpecification::StxTransfer(tx) => {
                        total_cost += tx.cost;
                        total_cost += tx.mstx_amount;
                    }
                    _ => {}
                }
            }
        }

        let content = deployment
            .to_file_content(project_root)
            .unwrap_or_else(|err| panic!("unable to serialize deployment {err}"));

        DeploymentSynthesis {
            total_cost,
            blocks_count,
            content: std::str::from_utf8(&content).unwrap().to_string(),
        }
    }
}

impl Display for DeploymentSynthesis {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let base: u64 = 10;
        let int_part = self.total_cost / base.pow(6);
        let frac_part = self.total_cost % base.pow(6);
        let formatted_total_cost = format!("{int_part}.{frac_part:06}");
        write!(
            f,
            "{}\n\n{}\n{}",
            green!("{}", self.content),
            blue!("Total cost:\t{formatted_total_cost} STX"),
            blue!("Duration:\t{} blocks", self.blocks_count)
        )
    }
}
