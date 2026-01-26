use std::path::PathBuf;
use std::str::FromStr;

use clarinet_files::FileLocation;
use clarity_repl::repl::{
    ClarityCodeSource, ClarityContract, ContractDeployer, DEFAULT_CLARITY_VERSION,
};
use hashbrown::HashMap;
use indoc::{formatdoc, indoc};

use super::changes::{Changes, FileCreation, FileDeletion, TOMLEdition};

pub struct GetChangesForRmContract {
    manifest_location: FileLocation,
    contract_name: String,
    changes: Vec<Changes>,
}

impl GetChangesForRmContract {
    pub fn new(manifest_location: FileLocation, contract_name: String) -> Self {
        Self {
            manifest_location,
            contract_name: contract_name.replace('.', "_"),
            changes: vec![],
        }
    }
    pub fn run(&mut self) -> Result<Vec<Changes>, String> {
        self.rm_template_contract()?;
        self.unindex_contract_in_clarinet_toml();
        self.rm_test()?;
        Ok(self.changes.clone())
    }
    fn rm_test(&mut self) -> Result<(), String> {
        let name = format!("{}.test.ts", self.contract_name);
        let mut f = self.manifest_location.get_project_root_location().unwrap();
        f.append_path("tests")?;
        f.append_path(&name)?;
        if !f.exists() {
            return Ok(());
        }
        let change = FileDeletion {
            comment: format!("{} tests/{}", red!("Deleted file"), name),
            path: f.to_string(),
        };
        self.changes.push(Changes::RemoveFile(change));
        Ok(())
    }
    fn rm_template_contract(&mut self) -> Result<(), String> {
        let name = format!("{}.clar", self.contract_name);
        let mut f = self.manifest_location.get_project_root_location().unwrap();
        f.append_path("contracts")?;
        f.append_path(&name)?;
        if !f.exists() {
            return Err(format!("{f} doesn't exist"));
        }
        let change = FileDeletion {
            comment: format!("{} contracts/{}", red!("Deleted file"), name),
            path: f.to_string(),
        };
        self.changes.push(Changes::RemoveFile(change));
        Ok(())
    }
    fn unindex_contract_in_clarinet_toml(&mut self) {
        let manifest_location = self.manifest_location.clone();
        let contracts_to_rm = vec![self.contract_name.clone()];

        let change = TOMLEdition {
            comment: format!(
                "{}, removed contract {}",
                yellow!("Updated Clarinet.toml"),
                self.contract_name
            ),
            manifest_location,
            contracts_to_rm,
            contracts_to_add: HashMap::new(),
            requirements_to_add: vec![],
        };
        self.changes.push(Changes::EditTOML(change));
    }
}

pub struct GetChangesForNewContract {
    manifest_location: FileLocation,
    contract_name: String,
    source: Option<String>,
    changes: Vec<Changes>,
}

impl GetChangesForNewContract {
    pub fn new(
        manifest_location: FileLocation,
        contract_name: String,
        source: Option<String>,
    ) -> Self {
        Self {
            manifest_location,
            contract_name: contract_name.replace('.', "_"),
            source,
            changes: vec![],
        }
    }

    pub fn run(&mut self, include_test: bool) -> Result<Vec<Changes>, String> {
        self.create_template_contract()?;
        if include_test {
            self.create_template_test()?;
        }
        self.index_contract_in_clarinet_toml();
        Ok(self.changes.clone())
    }

    fn create_template_contract(&mut self) -> Result<(), String> {
        let content = if let Some(ref source) = self.source {
            source.to_string()
        } else {
            formatdoc! {"
                ;; title: {}
                ;; version:
                ;; summary:
                ;; description:

                ;; traits
                ;;

                ;; token definitions
                ;;

                ;; constants
                ;;

                ;; data vars
                ;;

                ;; data maps
                ;;

                ;; public functions
                ;;

                ;; read only functions
                ;;

                ;; private functions
                ;;

            ", self.contract_name}
        };
        let name = format!("{}.clar", self.contract_name);
        let mut new_file = self.manifest_location.get_project_root_location().unwrap();
        new_file.append_path("contracts")?;
        new_file.append_path(&name)?;
        if new_file.exists() {
            return Err(format!("{new_file} already exists"));
        }
        let change = FileCreation {
            comment: format!("{} contracts/{}", green!("Created file"), name),
            content,
            path: new_file.to_string(),
        };
        self.changes.push(Changes::AddFile(change));
        Ok(())
    }

    fn create_template_test(&mut self) -> Result<(), String> {
        let content = indoc! {r#"
            import { describe, expect, it } from "vitest";

            const accounts = simnet.getAccounts();
            const address1 = accounts.get("wallet_1")!;

            /*
            The test below is an example. To learn more, read the testing documentation here:
            https://docs.stacks.co/clarinet/testing-with-clarinet-sdk
            */

            describe("example tests", () => {
            it("ensures simnet is well initialised", () => {
                expect(simnet.blockHeight).toBeDefined();
            });

            // it("shows an example", () => {
            //   const { result } = simnet.callReadOnlyFn("counter", "get-counter", [], address1);
            //   expect(result).toBeUint(0);
            // });
            });
        "#}
        .into();

        let name = format!("{}.test.ts", self.contract_name);
        let mut new_file = self.manifest_location.get_project_root_location().unwrap();
        new_file.append_path("tests")?;
        new_file.append_path(&name)?;
        if new_file.exists() {
            return Err(format!("{new_file} already exists"));
        }
        let change = FileCreation {
            comment: format!("{} tests/{}", green!("Created file"), name),
            content,
            path: new_file.to_string(),
        };
        self.changes.push(Changes::AddFile(change));
        Ok(())
    }

    fn index_contract_in_clarinet_toml(&mut self) {
        let contract_file_name = format!("{}.clar", self.contract_name);
        let manifest_location = self.manifest_location.clone();
        let contract_path = {
            let path = format!("contracts/{contract_file_name}");
            PathBuf::from_str(&path).unwrap()
        };
        let contract_config = ClarityContract {
            code_source: ClarityCodeSource::ContractOnDisk(contract_path),
            deployer: ContractDeployer::DefaultDeployer,
            name: self.contract_name.clone(),
            clarity_version: DEFAULT_CLARITY_VERSION,
            epoch: clarity_repl::repl::Epoch::Latest,
        };
        let mut contracts_to_add = HashMap::new();
        contracts_to_add.insert(self.contract_name.clone(), contract_config);

        let change = TOMLEdition {
            comment: format!(
                "{} with contract {}",
                yellow!("Updated Clarinet.toml"),
                self.contract_name
            ),
            manifest_location,
            contracts_to_rm: vec![],
            contracts_to_add,
            requirements_to_add: vec![],
        };
        self.changes.push(Changes::EditTOML(change));
    }
}
