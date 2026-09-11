use clarinet_files::{
    DEFAULT_BITCOIN_EXPLORER_IMAGE, DEFAULT_BITCOIN_NODE_IMAGE, DEFAULT_EPOCH_2_0,
    DEFAULT_EPOCH_2_05, DEFAULT_EPOCH_2_1, DEFAULT_EPOCH_2_2, DEFAULT_EPOCH_2_3, DEFAULT_EPOCH_2_4,
    DEFAULT_EPOCH_2_5, DEFAULT_EPOCH_3_0, DEFAULT_EPOCH_3_1, DEFAULT_EPOCH_3_2, DEFAULT_EPOCH_3_3,
    DEFAULT_EPOCH_3_4, DEFAULT_EPOCH_4_0, DEFAULT_POSTGRES_IMAGE, DEFAULT_STACKS_API_IMAGE,
    DEFAULT_STACKS_EXPLORER_IMAGE, DEFAULT_STACKS_NODE_IMAGE, DEFAULT_STACKS_SIGNER_IMAGE,
};
use clarinet_utils::{
    DEFAULT_DEPLOYER_MNEMONIC, DEFAULT_DERIVATION_PATH, DEFAULT_FAUCET_MNEMONIC,
    DEFAULT_STACKER_MNEMONIC, DEFAULT_STACKS_MINER_MNEMONIC, DEFAULT_WALLET_1_MNEMONIC,
    DEFAULT_WALLET_2_MNEMONIC, DEFAULT_WALLET_3_MNEMONIC, DEFAULT_WALLET_4_MNEMONIC,
    DEFAULT_WALLET_5_MNEMONIC, DEFAULT_WALLET_6_MNEMONIC, DEFAULT_WALLET_7_MNEMONIC,
    DEFAULT_WALLET_8_MNEMONIC,
};
use indoc::{formatdoc, indoc};

use super::changes::{Changes, DirectoryCreation, FileCreation};

pub struct GetChangesForNewProject {
    project_path: String,
    project_name: String,
    use_current_dir: bool,
    changes: Vec<Changes>,
    telemetry_enabled: bool,
}

impl GetChangesForNewProject {
    pub fn new(
        project_path: String,
        project_name: String,
        use_current_dir: bool,
        telemetry_enabled: bool,
    ) -> Self {
        let project_path = if use_current_dir {
            project_path
        } else {
            format!("{project_path}/{project_name}")
        };

        Self {
            project_path,
            project_name,
            use_current_dir,
            changes: vec![],
            telemetry_enabled,
        }
    }

    pub fn run(&mut self) -> Result<Vec<Changes>, String> {
        if !self.use_current_dir {
            self.create_root_directory();
        }
        self.create_contracts_directory();
        self.create_settings_directory();
        self.create_tests_directory();
        self.create_clarinet_toml();
        self.create_environment_mainnet_toml();
        self.create_environment_testnet_toml();
        self.create_environment_devnet_toml();
        self.create_vscode_directory();
        self.create_vscode_settings_json();
        self.create_vscode_tasks_json();
        self.create_gitignore();
        self.create_gitattributes();
        self.create_nodejs_files();
        Ok(self.changes.clone())
    }

    fn create_root_directory(&mut self) {
        let change = DirectoryCreation {
            comment: format!("{} {}", green!("Created directory"), self.project_name),
            path: self.project_path.clone(),
        };
        self.changes.push(Changes::AddDirectory(change));
    }

    fn create_contracts_directory(&mut self) {
        self.changes
            .push(self.get_changes_for_new_root_dir("contracts".into()));
    }

    fn create_settings_directory(&mut self) {
        self.changes
            .push(self.get_changes_for_new_root_dir("settings".into()));
    }

    fn create_tests_directory(&mut self) {
        self.changes
            .push(self.get_changes_for_new_root_dir("tests".into()));
    }

    fn create_vscode_directory(&mut self) {
        self.changes
            .push(self.get_changes_for_new_root_dir(".vscode".into()));
    }

    fn create_vscode_settings_json(&mut self) {
        #[rustfmt::skip]
        let content = indoc!(r#"
            {
              "files.eol": "\n"
            }
        "#).into();

        let name = ".vscode/settings.json".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_vscode_tasks_json(&mut self) {
        #[rustfmt::skip]
        let content = indoc!(r#"
            {
              "version": "2.0.0",
              "tasks": [
                {
                  "label": "check contracts",
                  "group": "test",
                  "type": "shell",
                  "command": "clarinet check"
                },
                {
                  "type": "npm",
                  "script": "test",
                  "group": "test",
                  "problemMatcher": [],
                  "label": "npm test"
                }
              ]
            }
        "#).into();
        let name = ".vscode/tasks.json".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_gitignore(&mut self) {
        #[rustfmt::skip]
        let content = indoc!(r#"
            **/settings/Mainnet.toml
            **/settings/Testnet.toml
            .cache/**
            history.txt

            logs
            *.log
            npm-debug.log*
            pnpm-debug.log*
            coverage
            *.info
            costs-reports.json
            node_modules
        "#).into();
        let name = ".gitignore".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_gitattributes(&mut self) {
        #[rustfmt::skip]
        let content = indoc!(r#"
            tests/** linguist-vendored
            vitest.config.ts linguist-vendored
            * text=lf
        "#).into();
        let name = ".gitattributes".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_clarinet_toml(&mut self) {
        #[rustfmt::skip]
        let content = formatdoc!(r#"
            [project]
            name = "{name}"
            description = ""
            authors = []
            telemetry = {tel}
            cache_dir = "./.cache"

            # [contracts.counter]
            # path = "contracts/counter.clar"
            # epoch = "latest"

            # Check-checker settings:
            # trusted_sender: if true, inputs are trusted after tx_sender has been checked.
            # trusted_caller: if true, inputs are trusted after contract-caller has been checked.
            # callee_filter: if true, untrusted data may be passed into a private function without a
            # warning, if it gets checked inside. This check will also propagate up to the
            # caller.
            # More informations: https://www.hiro.so/blog/new-safety-checks-in-clarinet

            [repl.analysis]
            passes = ["check_checker"]
            check_checker = {{ trusted_sender = false, trusted_caller = false, callee_filter = false }}
        "#,
            name = self.project_name,
            tel = self.telemetry_enabled
        );
        let name = "Clarinet.toml".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_environment_testnet_toml(&mut self) {
        #[rustfmt::skip]
        let content = indoc!(r#"
            [network]
            name = "testnet"
            stacks_node_rpc_address = "https://api.testnet.hiro.so"
            deployment_fee_rate = 10

            [accounts.deployer]
            # It is strongly suggested to use encrypted mnemonics to avoid putting
            # seed phrases directly into TOML files.  To do so, run:
            #
            #     clarinet deployments encrypt
            #
            # Enter your seed phrase and a password, then paste the encrypted mnemonic here.
            #
            # Otherwise, use the mnemonic field below:
            #
            mnemonic = "<YOUR PRIVATE TESTNET MNEMONIC HERE>"
        "#).into();
        let name = "settings/Testnet.toml".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_environment_mainnet_toml(&mut self) {
        #[rustfmt::skip]
        let content = indoc!(r#"
            [network]
            name = "mainnet"
            stacks_node_rpc_address = "https://api.hiro.so"
            deployment_fee_rate = 10

            [accounts.deployer]
            # It is strongly suggested to use encrypted mnemonics to avoid putting
            # seed phrases directly into TOML files.  To do so, run:
            #
            #     clarinet deployments encrypt
            #
            # Enter your seed phrase and a password, then paste the encrypted mnemonic here.
            #
            # Otherwise, use the mnemonic field below:
            mnemonic = "<YOUR PRIVATE MAINNET MNEMONIC HERE>"
        "#).into();
        let name = "settings/Mainnet.toml".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_environment_devnet_toml(&mut self) {
        #[rustfmt::skip]
        let content = formatdoc!(r#"
            [network]
            name = "devnet"
            deployment_fee_rate = 10

            [accounts.deployer]
            mnemonic = "{DEFAULT_DEPLOYER_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: 753b7cc01a1a2e86221266a154af739463fce51219d97e4f856cd7200c3bd2a601
            # stx_address: ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
            # btc_address: mqVnk6NPRdhntvfm4hh9vvjiRkFDUuSYsH

            [accounts.wallet_1]
            mnemonic = "{DEFAULT_WALLET_1_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: 7287ba251d44a4d3fd9276c88ce34c5c52a038955511cccaf77e61068649c17801
            # stx_address: ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5
            # btc_address: mr1iPkD9N3RJZZxXRk7xF9d36gffa6exNC

            [accounts.wallet_2]
            mnemonic = "{DEFAULT_WALLET_2_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: 530d9f61984c888536871c6573073bdfc0058896dc1adfe9a6a10dfacadc209101
            # stx_address: ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG
            # btc_address: muYdXKmX9bByAueDe6KFfHd5Ff1gdN9ErG

            [accounts.wallet_3]
            mnemonic = "{DEFAULT_WALLET_3_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: d655b2523bcd65e34889725c73064feb17ceb796831c0e111ba1a552b0f31b3901
            # stx_address: ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC
            # btc_address: mvZtbibDAAA3WLpY7zXXFqRa3T4XSknBX7

            [accounts.wallet_4]
            mnemonic = "{DEFAULT_WALLET_4_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: f9d7206a47f14d2870c163ebab4bf3e70d18f5d14ce1031f3902fbbc894fe4c701
            # stx_address: ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND
            # btc_address: mg1C76bNTutiCDV3t9nWhZs3Dc8LzUufj8

            [accounts.wallet_5]
            mnemonic = "{DEFAULT_WALLET_5_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: 3eccc5dac8056590432db6a35d52b9896876a3d5cbdea53b72400bc9c2099fe801
            # stx_address: ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB
            # btc_address: mweN5WVqadScHdA81aATSdcVr4B6dNokqx

            [accounts.wallet_6]
            mnemonic = "{DEFAULT_WALLET_6_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: 7036b29cb5e235e5fd9b09ae3e8eec4404e44906814d5d01cbca968a60ed4bfb01
            # stx_address: ST3AM1A56AK2C1XAFJ4115ZSV26EB49BVQ10MGCS0
            # btc_address: mzxXgV6e4BZSsz8zVHm3TmqbECt7mbuErt

            [accounts.wallet_7]
            mnemonic = "{DEFAULT_WALLET_7_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: b463f0df6c05d2f156393eee73f8016c5372caa0e9e29a901bb7171d90dc4f1401
            # stx_address: ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ
            # btc_address: n37mwmru2oaVosgfuvzBwgV2ysCQRrLko7

            [accounts.wallet_8]
            mnemonic = "{DEFAULT_WALLET_8_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: 6a1a754ba863d7bab14adbbc3f8ebb090af9e871ace621d3e5ab634e1422885e01
            # stx_address: ST3NBRSFKX28FQ2ZJ1MAKX58HKHSDGNV5N7R21XCP
            # btc_address: n2v875jbJ4RjBnTjgbfikDfnwsDV5iUByw

            [accounts.faucet]
            mnemonic = "{DEFAULT_FAUCET_MNEMONIC}"
            balance = 100_000_000_000_000
            sbtc_balance = 1_000_000_000
            # secret_key: de433bdfa14ec43aa1098d5be594c8ffb20a31485ff9de2923b2689471c401b801
            # stx_address: STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6
            # btc_address: mjSrB3wS4xab3kYqFktwBzfTdPg367ZJ2d

            [devnet]
            disable_stacks_explorer = false
            disable_stacks_api = false
            # disable_postgres = false
            # disable_bitcoin_explorer = true
            # working_dir = "tmp/devnet"
            # stacks_node_events_observers = ["host.docker.internal:8002"] # Defaults to events_keys = ["*"]
            # stacks_node_events_observers = [
            #   {{ endpoint = "host.docker.internal:8787", events_keys = ["burn_blocks", "memtx"] }},
            # ]
            # miner_mnemonic = "{DEFAULT_STACKS_MINER_MNEMONIC}"
            # miner_derivation_path = "{DEFAULT_DERIVATION_PATH}"
            # faucet_mnemonic = "{DEFAULT_FAUCET_MNEMONIC}"
            # faucet_derivation_path = "{DEFAULT_DERIVATION_PATH}"
            # stacker_mnemonic = "{DEFAULT_STACKER_MNEMONIC}"
            # stacker_derivation_path = "{DEFAULT_DERIVATION_PATH}"
            # orchestrator_port = 20445
            # bitcoin_node_p2p_port = 18444
            # bitcoin_node_rpc_port = 18443
            # bitcoin_node_username = "devnet"
            # bitcoin_node_password = "devnet"
            # bitcoin_controller_block_time = 30_000
            # stacks_node_rpc_port = 20443
            # stacks_node_p2p_port = 20444
            # stacks_api_port = 3999
            # stacks_api_events_port = 3700
            # bitcoin_explorer_port = 8001
            # stacks_explorer_port = 8000
            # postgres_port = 5432
            # postgres_username = "postgres"
            # postgres_password = "postgres"
            # postgres_database = "postgres"
            # bitcoin_node_image_url = "{DEFAULT_BITCOIN_NODE_IMAGE}"
            # stacks_node_image_url = "{DEFAULT_STACKS_NODE_IMAGE}"
            # stacks_signer_image_url = "{DEFAULT_STACKS_SIGNER_IMAGE}"
            # stacks_api_image_url = "{DEFAULT_STACKS_API_IMAGE}"
            # stacks_explorer_image_url = "{DEFAULT_STACKS_EXPLORER_IMAGE}"
            # bitcoin_explorer_image_url = "{DEFAULT_BITCOIN_EXPLORER_IMAGE}"
            # postgres_image_url = "{DEFAULT_POSTGRES_IMAGE}"

            # epoch_2_0 = {DEFAULT_EPOCH_2_0}
            # epoch_2_05 = {DEFAULT_EPOCH_2_05}
            # epoch_2_1 = {DEFAULT_EPOCH_2_1}
            # epoch_2_2 = {DEFAULT_EPOCH_2_2}
            # epoch_2_3 = {DEFAULT_EPOCH_2_3}
            # epoch_2_4 = {DEFAULT_EPOCH_2_4}
            # epoch_2_5 = {DEFAULT_EPOCH_2_5}
            # epoch_3_0 = {DEFAULT_EPOCH_3_0}
            # epoch_3_1 = {DEFAULT_EPOCH_3_1}
            # epoch_3_2 = {DEFAULT_EPOCH_3_2}
            # epoch_3_3 = {DEFAULT_EPOCH_3_3}
            # epoch_3_4 = {DEFAULT_EPOCH_3_4}
            # epoch_4_0 = {DEFAULT_EPOCH_4_0}

            # Send some stacking orders
            [[devnet.pox_stacking_orders]]
            start_at_cycle = 1
            duration = 10
            auto_extend = true
            wallet = "wallet_1"
            slots = 2
            btc_address = "mr1iPkD9N3RJZZxXRk7xF9d36gffa6exNC"

            [[devnet.pox_stacking_orders]]
            start_at_cycle = 1
            duration = 10
            auto_extend = true
            wallet = "wallet_2"
            slots = 2
            btc_address = "muYdXKmX9bByAueDe6KFfHd5Ff1gdN9ErG"

            [[devnet.pox_stacking_orders]]
            start_at_cycle = 1
            duration = 10
            auto_extend = true
            wallet = "wallet_3"
            slots = 2
            btc_address = "mvZtbibDAAA3WLpY7zXXFqRa3T4XSknBX7"

        "#);
        let name = "settings/Devnet.toml".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_nodejs_files(&mut self) {
        self.create_package_json();
        self.create_ts_config();
        self.create_vitest_config();
    }

    fn create_package_json(&mut self) {
        #[rustfmt::skip]
        let content = formatdoc!(r#"
            {{
              "name": "{}-tests",
              "version": "1.0.0",
              "description": "Run unit tests on this project.",
              "type": "module",
              "private": true,
              "scripts": {{
                "test": "vitest run",
                "test:report": "vitest run -- --coverage --costs",
                "test:watch": "chokidar \"tests/**/*.ts\" \"contracts/**/*.clar\" -c \"npm run test:report\""
              }},
              "author": "",
              "license": "ISC",
              "dependencies": {{
                "@stacks/clarinet-sdk": "^3.9.0",
                "@stacks/transactions": "^7.2.0",
                "@types/node": "^24.4.0",
                "chokidar-cli": "^3.0.0",
                "vitest": "^4.1.8",
                "vitest-environment-clarinet": "^3.0.0"
              }}
            }}
        "#,
            self.project_name
        );
        let name = "package.json".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_ts_config(&mut self) {
        #[rustfmt::skip]
        let content = indoc!(r#"
            {
              "compilerOptions": {
                "target": "ESNext",
                "useDefineForClassFields": true,
                "module": "ESNext",
                "lib": ["ESNext"],
                "skipLibCheck": true,

                "moduleResolution": "bundler",
                "allowImportingTsExtensions": true,
                "resolveJsonModule": true,
                "isolatedModules": true,
                "noEmit": true,

                "strict": true,
                "noImplicitAny": true,
                "noUnusedLocals": true,
                "noUnusedParameters": true,
                "noFallthroughCasesInSwitch": true
              },
              "include": [
                "node_modules/@stacks/clarinet-sdk/vitest-helpers/src",
                "tests"
              ]
            }
        "#).into();
        let name = "tsconfig.json".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content));
    }

    fn create_vitest_config(&mut self) {
        let content = indoc!(r#"
            import { defineConfig } from "vitest/config";
            import {
              vitestSetupFilePath,
              getClarinetVitestsArgv,
            } from "@stacks/clarinet-sdk/vitest";

            /*
              In this file, Vitest is configured so that it works seamlessly with Clarinet and the Simnet.

              The `vitest-environment-clarinet` will initialise the clarinet-sdk
              and make the `simnet` object available globally in the test files.

              `vitestSetupFilePath` points to a file in the `@stacks/clarinet-sdk` package that does two things:
                - run `before` hooks to initialize the simnet and `after` hooks to collect costs and coverage reports.
                - load custom vitest matchers to work with Clarity values (such as `expect(...).toBeUint()`)

              The `getClarinetVitestsArgv()` will parse options passed to the command `vitest run --`
                - vitest run -- --manifest ./Clarinet.toml  # pass a custom path
                - vitest run -- --coverage --costs          # collect coverage and cost reports
            */

            export default defineConfig({
              test: {
                // use vitest-environment-clarinet
                environment: "clarinet",
                pool: "forks",
                // clarinet handles test isolation by resetting the simnet between tests
                isolate: false,
                maxWorkers: 1,
                setupFiles: [
                  vitestSetupFilePath,
                  // custom setup files can be added here
                ],
                environmentOptions: {
                  clarinet: {
                    ...getClarinetVitestsArgv(),
                    // add or override options
                  },
                },
              },
            });

        "#).into();
        let name = "vitest.config.ts".into();
        self.changes
            .push(self.get_changes_for_new_file(name, content))
    }

    fn get_changes_for_new_root_dir(&self, name: String) -> Changes {
        let dir = format!("{}/{name}", self.project_path);
        Changes::AddDirectory(DirectoryCreation {
            comment: format!("{} {name}", green!("Created directory")),
            path: dir,
        })
    }

    fn get_changes_for_new_file(&self, name: String, content: String) -> Changes {
        let path = format!("{}/{name}", self.project_path);

        Changes::AddFile(FileCreation {
            comment: format!("{} {name}", green!("Created file")),
            content,
            path,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use clarinet_files::{compute_addresses, StacksNetwork};
    use clarinet_utils::DEFAULT_DEVNET_ACCOUNTS;

    use super::*;

    #[derive(serde::Deserialize)]
    struct GeneratedDevnet {
        accounts: BTreeMap<String, GeneratedAccount>,
    }

    #[derive(serde::Deserialize)]
    struct GeneratedAccount {
        mnemonic: String,
        derivation: Option<String>,
    }

    fn generated_devnet_toml() -> String {
        GetChangesForNewProject::new("/tmp".into(), "drift-guard".into(), false, false)
            .run()
            .expect("project generation failed")
            .into_iter()
            .find_map(|change| match change {
                Changes::AddFile(file) if file.path.ends_with("settings/Devnet.toml") => {
                    Some(file.content)
                }
                _ => None,
            })
            .expect("generated project has no settings/Devnet.toml")
    }

    /// The template interpolates `clarinet-utils` constants, so a phrase cannot
    /// drift. What this guards is the *set* of accounts: comparing against
    /// `DEFAULT_DEVNET_ACCOUNTS` rather than a local copy means an account
    /// added with an off-table phrase cannot be satisfied by editing this test
    /// — it has to join the shared list, where `clarinet-utils` asserts it has
    /// precomputed keys. Otherwise it would silently cost a PBKDF2 derivation
    /// per session, on every LSP file save.
    #[test]
    fn generated_wallets_use_the_shared_mnemonics() {
        let devnet_toml = generated_devnet_toml();
        let generated: GeneratedDevnet =
            toml::from_str(&devnet_toml).expect("generated Devnet.toml is not valid");

        let expected: BTreeMap<&str, &str> = DEFAULT_DEVNET_ACCOUNTS.into_iter().collect();
        let actual: BTreeMap<&str, &str> = generated
            .accounts
            .iter()
            .map(|(label, account)| (label.as_str(), account.mnemonic.as_str()))
            .collect();
        assert_eq!(actual, expected);

        for (label, account) in &generated.accounts {
            assert!(
                matches!(
                    account.derivation.as_deref(),
                    None | Some(DEFAULT_DERIVATION_PATH)
                ),
                "account {label} overrides the derivation path, which misses the \
                 precomputed table"
            );
        }
    }

    /// Each account block documents its derived key and addresses in comments
    /// that developers copy into tests. The mnemonics live in `clarinet-utils`
    /// now, so nothing else would notice if one were rotated and these literals
    /// left behind.
    ///
    /// The comments are stripped by TOML parsing, so this scans the raw text —
    /// but it takes the mnemonic from the parsed document rather than
    /// re-extracting it, so a template quoting change cannot silently turn this
    /// into a no-op.
    #[test]
    fn generated_devnet_documents_correct_addresses() {
        let devnet_toml = generated_devnet_toml();
        let generated: GeneratedDevnet =
            toml::from_str(&devnet_toml).expect("generated Devnet.toml is not valid");
        let networks = StacksNetwork::Devnet.get_networks();

        let mut checked = BTreeMap::new();
        for block in devnet_toml.split("[accounts.").skip(1) {
            let label = block
                .lines()
                .next()
                .and_then(|l| l.split(']').next())
                .expect("account block has no label");
            let mnemonic = &generated
                .accounts
                .get(label)
                .unwrap_or_else(|| panic!("block {label} is not in the parsed document"))
                .mnemonic;

            let documented = |prefix: &str| {
                block
                    .lines()
                    .find_map(|line| line.trim().strip_prefix(prefix))
                    .map(str::trim)
                    .unwrap_or_else(|| panic!("account {label} is missing {prefix}"))
            };
            let (stx_address, btc_address, secret_key) =
                compute_addresses(mnemonic, DEFAULT_DERIVATION_PATH, &networks);

            assert_eq!(
                documented("# secret_key: "),
                secret_key,
                "{label} secret_key"
            );
            assert_eq!(
                documented("# stx_address: "),
                stx_address,
                "{label} stx_address"
            );
            assert_eq!(
                documented("# btc_address: "),
                btc_address,
                "{label} btc_address"
            );
            checked.insert(label, ());
        }

        assert_eq!(checked.len(), generated.accounts.len());
    }
}
