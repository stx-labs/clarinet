use clarity::types::StacksEpochId;
use clarity::vm::{EvaluationResult, Value};
use clarity_repl::repl::settings::{ApiUrl, RemoteDataSettings};
use clarity_repl::repl::{Session, SessionSettings};

#[track_caller]
fn eval_snippet(session: &mut Session, snippet: &str) -> Value {
    let execution_res = session.eval(snippet.to_string(), false).unwrap();
    match execution_res.result {
        EvaluationResult::Contract(_) => unreachable!(),
        EvaluationResult::Snippet(res) => res.result,
    }
}

fn init_testnet_session(initial_heigth: u32) -> Session {
    let mut settings = SessionSettings::default();
    let temp_dir = tempfile::tempdir().unwrap();
    settings.cache_location = Some(temp_dir.path().to_path_buf());
    settings.repl_settings.remote_data = RemoteDataSettings {
        enabled: true,
        api_url: ApiUrl("https://api.testnet.stg.hiro.so".to_string()),
        initial_height: Some(initial_heigth),
        use_mainnet_wallets: false,
    };
    Session::new(settings)
}

fn init_mainnet_session(initial_heigth: u32) -> Session {
    let mut settings = SessionSettings::default();
    let temp_dir = tempfile::tempdir().unwrap();
    settings.cache_location = Some(temp_dir.path().to_path_buf());
    settings.repl_settings.remote_data = RemoteDataSettings {
        enabled: true,
        api_url: ApiUrl("https://api.stg.hiro.so".to_string()),
        initial_height: Some(initial_heigth),
        use_mainnet_wallets: true,
    };
    Session::new(settings)
}

#[test]
fn it_starts_in_the_right_epoch() {
    let session = init_testnet_session(42000);
    assert_eq!(
        session.interpreter.datastore.get_current_epoch(),
        StacksEpochId::Epoch31
    );
}

mod test_counter_contract {
    use clarity::vm::Value;

    use crate::{eval_snippet, init_testnet_session};

    // the counter contract is deployed on testnet at height #41613
    // the initial count value is 0 and is incremented:
    //   - at #56232
    //   - at #140788
    //   - at #3530272 (after COUNTER2 is deployed)
    const COUNTER_ADDR: &str = "STJCAB2T9TR2EJM7YS4DM2CGBBVTF7BV237Y8KNV.counter";
    // counter2 is deployed at #3530220
    // it calls COUNTER_ADDR to dynamically set a constant value
    const COUNTER2_ADDR: &str = "ST22JXZG7Q4AN1100RZ7MMHQP6VF1WJX41SPB94CR.counter2";

    #[test]
    fn it_can_fetch_remote() {
        // count at block 42000 is 0
        let mut session = init_testnet_session(42000);
        let snippet = format!("(contract-call? '{COUNTER_ADDR} get-count)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(0));

        // count at block 57000 is 1
        let mut session = init_testnet_session(57000);
        let snippet = format!("(contract-call? '{COUNTER_ADDR} get-count)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(1));
    }

    #[test]
    fn it_can_fork_state() {
        let mut session = init_testnet_session(57000);
        let snippet_get_count = format!("(contract-call? '{COUNTER_ADDR} get-count)");

        let result = eval_snippet(&mut session, &snippet_get_count);
        assert_eq!(result, Value::UInt(1));

        session.advance_burn_chain_tip(1);
        let snippet = format!("(contract-call? '{COUNTER_ADDR} increment)");
        let _ = eval_snippet(&mut session, &snippet);

        let result = eval_snippet(&mut session, &snippet_get_count);
        assert_eq!(result, Value::UInt(2));
    }

    #[test]
    fn it_keeps_track_of_historical_data() {
        let mut session = init_testnet_session(57000);

        let snippet = format!("(contract-call? '{COUNTER_ADDR} get-count-at-block u42000)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::okay(Value::UInt(0)).unwrap());

        let snippet = format!("(contract-call? '{COUNTER_ADDR} get-count)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(1));
    }

    #[test]
    fn it_handles_at_block() {
        let mut session = init_testnet_session(60000);

        // block 42000 hash
        let hash = "0xb4678e059aa9b82b1473597087876ef61a5c6a0c35910cd4b797201d6b423a07";

        let snippet = format!("(at-block {hash} stacks-block-height)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(42000));

        let snippet_get_count_at_10k =
            format!("(contract-call? '{COUNTER_ADDR} get-count-at-block u10000)");
        let result = eval_snippet(&mut session, &snippet_get_count_at_10k);
        assert_eq!(result, Value::okay(Value::none()).unwrap());

        let snippet_get_count_at_42k =
            format!("(contract-call? '{COUNTER_ADDR} get-count-at-block u42000)");
        let result = eval_snippet(&mut session, &snippet_get_count_at_42k);
        assert_eq!(result, Value::okay(Value::UInt(0)).unwrap());

        let snippet_get_count_at_57k =
            format!("(contract-call? '{COUNTER_ADDR} get-count-at-block u57000)");
        let result = eval_snippet(&mut session, &snippet_get_count_at_57k);
        assert_eq!(result, Value::okay(Value::UInt(1)).unwrap());
    }

    #[test]
    fn it_parses_contracts() {
        let mut session = init_testnet_session(57000);
        let snippet = format!("(contract-call? '{COUNTER_ADDR} get-count)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(1));
    }

    #[test]
    fn it_evualuates_constant_values() {
        let mut session = init_testnet_session(41614);
        let snippet = format!("(contract-call? '{COUNTER_ADDR} decrement)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::err_uint(1001));
    }

    #[test]
    fn it_properly_evaluates_constant_values() {
        let mut session = init_testnet_session(3530273);
        // we expect COUNTER2 to hold the count value from COUNTER_ADDR at deployment, which is 2
        let snippet = format!("(contract-call? '{COUNTER2_ADDR} get-count-at-deploy)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(2));
    }

    #[test]
    fn it_saves_metadata_to_cache() {
        let mut session = init_testnet_session(57000);
        let snippet = format!("(contract-call? '{COUNTER_ADDR} get-count)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(1));

        let cache_location = session.settings.cache_location.unwrap();
        let cache_file_path = cache_location
        .join(std::path::PathBuf::from(
            "datastore/STJCAB2T9TR2EJM7YS4DM2CGBBVTF7BV237Y8KNV_counter_vm-metadata__9__contract_645949d1e1701aea7bd5ca574bf26a7828a26a068ea6409134bc8a9b1329b4fd",
        ))
        .with_extension("json");
        assert!(cache_file_path.exists());
    }
}

mod test_mxs_session_test {
    use clarity::types::chainstate::{BlockHeaderHash, StacksBlockId};
    use clarity::types::StacksEpochId;
    use clarity::util::hash::hex_bytes;
    use clarity::vm::{ClarityVersion, Value};
    use clarity_repl::repl::{self, ClarityContract};

    use crate::{eval_snippet, init_mainnet_session, init_testnet_session};

    #[test]
    fn it_can_get_stacks_block_info() {
        let mut session = init_mainnet_session(3_000_000);

        let snippet = "(get-stacks-block-info? id-header-hash u2900100)";
        let result = eval_snippet(&mut session, snippet);
        let hash = StacksBlockId::from_hex(
            "f376ec69e56e8f32dc239ab86daa00b2cc54c43e30a785c61ffaa7b716403630",
        )
        .unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(hash.to_bytes().to_vec()).unwrap()).unwrap()
        );

        let snippet = "(get-stacks-block-info? header-hash u2900100)";
        let result = eval_snippet(&mut session, snippet);
        let hash = BlockHeaderHash::from_hex(
            "1045165f4f90ae690ae901d0ae78727b5a87314842d671adf49969ebe7232e83",
        )
        .unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(hash.to_bytes().to_vec()).unwrap()).unwrap()
        );

        let snippet = "(get-stacks-block-info? time u2900100)";
        let result = eval_snippet(&mut session, snippet);

        assert_eq!(result, Value::some(Value::UInt(1755670122)).unwrap());
    }

    #[test]
    fn it_can_get_heights() {
        let mut session = init_mainnet_session(3_000_000);

        let result = eval_snippet(&mut session, "stacks-block-height");
        assert_eq!(result, Value::UInt(3_000_000));
        let result = eval_snippet(&mut session, "burn-block-height");
        assert_eq!(result, Value::UInt(911_440));
        let result = eval_snippet(&mut session, "tenure-height");
        assert_eq!(result, Value::UInt(209_515));

        let hash = "0xf376ec69e56e8f32dc239ab86daa00b2cc54c43e30a785c61ffaa7b716403630";
        let snippet = format!("(at-block {hash} stacks-block-height)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(2_900_100));
        let snippet = format!("(at-block {hash} burn-block-height)");
        let result = eval_snippet(&mut session, &snippet);
        // in epochs 3.1, .2, .3, the burn-block-height is always the latest one, even in at-block
        assert_eq!(result, Value::UInt(911_440));
        let snippet = format!("(at-block {hash} tenure-height)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(208_997));
    }

    #[test]
    fn it_can_fetch_burn_chain_info() {
        let mut session = init_mainnet_session(3_100_000);

        let result = eval_snippet(&mut session, "burn-block-height");
        assert_eq!(result, Value::UInt(912035));
        let result = eval_snippet(&mut session, "(get-burn-block-info? header-hash u912035)");
        let expected_header_hash =
            hex_bytes("00000000000000000001d70a5624a4dc26eff1d4d9e54f7ac4937a68476c5c4f").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_header_hash).unwrap()).unwrap()
        );
        let result = eval_snippet(&mut session, "(get-burn-block-info? header-hash u912034)");
        let expected_header_hash =
            hex_bytes("00000000000000000000d1440042126d2f755250b351fbd11794097150ccafc8").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_header_hash).unwrap()).unwrap()
        );

        // // test for a bug where a burn block height higher than the current stacks block height would return invalid data
        let mut session = init_testnet_session(2000);
        let result = eval_snippet(&mut session, "burn-block-height");
        assert_eq!(result, Value::UInt(2836));
        let result = eval_snippet(&mut session, "(get-burn-block-info? header-hash u2832)");
        let expected_header_hash =
            hex_bytes("088722e90bf5c04639aa91cc30585b068883693a8ecc95a12aab71be2c7252ed").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_header_hash).unwrap()).unwrap()
        );

        // advance the burn chain tip will result in a fork, bitcoin data is now mocked
        session.advance_burn_chain_tip(10);
        let result = eval_snippet(&mut session, "burn-block-height");
        assert_eq!(result, Value::UInt(2846));
        let result = eval_snippet(&mut session, "(get-burn-block-info? header-hash u2840)");
        let expected_mocked_header_hash =
            hex_bytes("0224cd36a1bb63d40c62a249d8e05153ba4f6411e3024ad569ac28e0b50b41f2").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_mocked_header_hash).unwrap()).unwrap()
        );
    }

    #[test]
    fn it_can_get_tenure_info_time() {
        let mut session = init_testnet_session(57000);
        let result = eval_snippet(&mut session, "(get-tenure-info? time u56999)");
        assert_eq!(result, Value::some(Value::UInt(1737053962)).unwrap());
        let result = eval_snippet(&mut session, "(get-tenure-info? time u50999)");
        assert_eq!(result, Value::some(Value::UInt(1736980481)).unwrap());
    }

    #[test]
    fn it_can_get_tenure_info_bhh() {
        let mut session = init_testnet_session(57000);
        let result = eval_snippet(
            &mut session,
            "(get-tenure-info? burnchain-header-hash u56888)",
        );
        let expected_header_hash =
            hex_bytes("026c12afb50b4baabb5cac8b940eda8b437f979b9819eef4cdd14c9f1a78133c").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_header_hash).unwrap()).unwrap()
        );
    }

    #[test]
    fn it_can_get_tenure_info_vrf_seed() {
        let mut session = init_mainnet_session(3_000_000);
        let result = eval_snippet(&mut session, "(get-tenure-info? vrf-seed u2900100)");
        let expected_vrf_seed =
            hex_bytes("74850f3a30a46c930e5d5da07ad083a8d34fb96e14f48adf07b5e0c84fd7127a").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_vrf_seed).unwrap()).unwrap()
        );
        let result = eval_snippet(&mut session, "(get-tenure-info? vrf-seed u2900000)");
        let expected_vrf_seed =
            hex_bytes("cf1cd46c0be58694cdaec63c592c4feb1cb359a4ca2582c1dbab87b0003c4731").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_vrf_seed).unwrap()).unwrap()
        );
    }

    #[test]
    fn it_handles_chain_constants() {
        let mut session = init_testnet_session(57000);
        let result = eval_snippet(&mut session, "is-in-mainnet");
        assert_eq!(result, Value::Bool(false));
        let result = eval_snippet(&mut session, "chain-id");
        assert_eq!(result, Value::UInt(2147483648));

        let mut session = init_mainnet_session(3_000_000);
        let result = eval_snippet(&mut session, "is-in-mainnet");
        assert_eq!(result, Value::Bool(true));
        let result = eval_snippet(&mut session, "chain-id");
        assert_eq!(result, Value::UInt(1));
    }

    #[test]
    fn it_handle_tenure_height_in_epoch3() {
        let mut session = init_mainnet_session(3_000_000);
        let epoch = session.get_epoch();
        assert_eq!(epoch, "Current epoch: 3.2");

        let bbh = eval_snippet(&mut session, "burn-block-height");
        assert_eq!(bbh, Value::UInt(911440));
        let tenure_height = eval_snippet(&mut session, "tenure-height");
        assert_eq!(tenure_height, Value::UInt(209515));

        session.advance_burn_chain_tip(1);
        let bbh = eval_snippet(&mut session, "burn-block-height");
        assert_eq!(bbh, Value::UInt(911441));
        let tenure_height = eval_snippet(&mut session, "tenure-height");
        assert_eq!(tenure_height, Value::UInt(209516));
    }

    #[test]
    fn it_handles_clarity2_block_height_in_epoch3() {
        let mut session = init_testnet_session(3557367);
        let epoch = session.get_epoch();
        assert_eq!(epoch, "Current epoch: 3.2");

        let tenure_height = eval_snippet(&mut session, "tenure-height");
        assert_eq!(tenure_height, Value::UInt(88911));

        let deployer = "ST23YMXQ25679FCF71F8FRGYPQBZQJFJWA4GFT84T";

        // block-height is a Clarity 1 & 2 keyword
        // in epoch 3.2, it should return the tenure-height
        let contract = ClarityContract {
            name: "gbh".into(),
            code_source: repl::ClarityCodeSource::ContractInMemory(
                "(define-read-only (get-block-height) block-height)".into(),
            ),
            deployer: repl::ContractDeployer::Address(deployer.into()),
            clarity_version: ClarityVersion::Clarity2,
            epoch: repl::Epoch::Specific(StacksEpochId::Epoch32),
            is_requirement: false,
        };
        let result = session.deploy_contract(&contract, false, None);
        assert!(result.is_ok());

        let snippet = format!("(contract-call? '{deployer}.gbh get-block-height)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(88911));

        session.advance_burn_chain_tip(1);
        let snippet = format!("(contract-call? '{deployer}.gbh get-block-height)");
        let result = eval_snippet(&mut session, &snippet);
        assert_eq!(result, Value::UInt(88912));
    }

    #[test]
    fn it_handles_clarity2_get_block_info_in_epoch2() {
        let mut session = init_mainnet_session(107108);
        let epoch = session.get_epoch();
        assert_eq!(epoch, "Current epoch: 2.4");

        let deployer = "SPWHZ9EX7GEC7V6RG3B6EP1C0BR10B93BB53TPTN";

        let contract = indoc::indoc!(
            "(define-read-only (get-block-hash (h uint))
                (get-block-info? id-header-hash h)
            )
            (define-read-only (get-burn-block-hash (h uint))
                (get-block-info? burnchain-header-hash h)
            )"
        );
        let contract = ClarityContract {
            name: "gbh".into(),
            code_source: repl::ClarityCodeSource::ContractInMemory(contract.into()),
            deployer: repl::ContractDeployer::Address(deployer.into()),
            clarity_version: ClarityVersion::Clarity2,
            epoch: repl::Epoch::Specific(StacksEpochId::Epoch24),
            is_requirement: false,
        };
        let result = session.deploy_contract(&contract, false, None);
        assert!(result.is_ok());

        // 107107 is a block in epoch 2.4
        let snippet = format!("(contract-call? '{deployer}.gbh get-block-hash u107107)");
        let result = eval_snippet(&mut session, &snippet);
        let expected_hash = Value::buff_from(
            hex_bytes("0dd92fe70895ebcb11f94a2bf9c9bc3f24e3e9ad80b904a20c4fc9d20a5eddfc").unwrap(),
        )
        .unwrap();
        assert_eq!(result, Value::some(expected_hash).unwrap());

        let snippet = format!("(contract-call? '{deployer}.gbh get-burn-block-hash u107107)");
        let result = eval_snippet(&mut session, &snippet);
        let expected_hash = Value::buff_from(
            hex_bytes("00000000000000000001813869927e1bc1f2c2384c76dd12109875f7827f3ed0").unwrap(),
        )
        .unwrap();
        assert_eq!(result, Value::some(expected_hash).unwrap());
    }

    #[test]
    fn it_handles_clarity2_get_block_info_in_epoch3() {
        let mut session = init_mainnet_session(3586042);
        let epoch = session.get_epoch();
        assert_eq!(epoch, "Current epoch: 3.2");

        let tenure_height = eval_snippet(&mut session, "tenure-height");
        assert_eq!(tenure_height, Value::UInt(212783));

        let deployer = "SPWHZ9EX7GEC7V6RG3B6EP1C0BR10B93BB53TPTN";

        let contract = indoc::indoc!(
            "(define-read-only (get-block-height) block-height)
            (define-read-only (get-block-hash (h uint))
                (get-block-info? id-header-hash h)
            )
            (define-read-only (get-burn-block-hash (h uint))
                (get-block-info? burnchain-header-hash h)
            )"
        );
        let contract = ClarityContract {
            name: "gbh".into(),
            code_source: repl::ClarityCodeSource::ContractInMemory(contract.into()),
            deployer: repl::ContractDeployer::Address(deployer.into()),
            clarity_version: ClarityVersion::Clarity2,
            epoch: repl::Epoch::Specific(StacksEpochId::Epoch32),
            is_requirement: false,
        };
        let result = session.deploy_contract(&contract, false, None);
        assert!(result.is_ok());

        // 107_107 is a block in epoch 2.4
        let snippet = format!("(contract-call? '{deployer}.gbh get-block-hash u107107)");
        let result = eval_snippet(&mut session, &snippet);
        let expected_hash = Value::buff_from(
            hex_bytes("0dd92fe70895ebcb11f94a2bf9c9bc3f24e3e9ad80b904a20c4fc9d20a5eddfc").unwrap(),
        )
        .unwrap();
        assert_eq!(result, Value::some(expected_hash).unwrap());

        // todo: add test with a tenure height happening in epoch 3.x
    }
}
