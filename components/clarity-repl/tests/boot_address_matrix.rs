//! Which boot and sBTC addresses a simnet user can write, and what happens.
//!
//! Simnet deploys the boot contracts under *both* the mainnet and testnet
//! addresses, but only the `ST000...` copies move consensus state. The sBTC
//! contracts are deployed at their *mainnet* address only, and have no
//! testnet twin here. That asymmetry is the thing these tests pin: a mainnet
//! boot reference is redirected, a mainnet sBTC reference is left alone, and
//! both are correct.

use clarity::types::StacksEpochId;
use clarity::vm::{EvaluationResult, Value};
use clarity_repl::repl::boot::{
    BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS, SBTC_MAINNET_ADDRESS, SBTC_TESTNET_ADDRESS,
};
use clarity_repl::repl::settings::{ApiUrl, RemoteDataSettings};
use clarity_repl::repl::{Session, SessionSettings};

/// Default deployer from the generated Devnet settings.
const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

fn session(epoch: StacksEpochId) -> Session {
    let mut session = Session::new(SessionSettings::default());
    session.update_epoch(epoch);
    session.advance_burn_chain_tip(1);
    session
}

/// Evaluate the way the console and the SDK's `runSnippet` do: remap the
/// user's source, then evaluate it.
#[track_caller]
fn run_user_snippet(session: &mut Session, snippet: &str) -> Value {
    let snippet = session.remap_user_snippet(snippet.to_string());
    let result = session
        .eval(snippet, false)
        .unwrap_or_else(|d| {
            panic!(
                "snippet should evaluate: {:?}",
                d.first().map(|d| &d.message)
            )
        })
        .into_inner();

    match result.result {
        EvaluationResult::Snippet(s) => s.result,
        EvaluationResult::Contract(_) => panic!("expected a snippet result"),
    }
}

#[track_caller]
fn reward_cycle_length(session: &mut Session, deployer: &str) -> Value {
    run_user_snippet(
        session,
        &format!(
            "(get reward-cycle-length (unwrap-panic (contract-call? '{deployer}.pox-3 get-pox-info)))"
        ),
    )
}

/// The headline: both spellings of a boot contract behave identically from a
/// user snippet. `pox-3` is the sharpest case, because the two deployed
/// copies genuinely disagree — the mainnet source carries
/// `REWARD_CYCLE_LENGTH u2100` and the testnet one `u1050`.
#[test]
fn both_boot_spellings_agree_in_a_user_snippet() {
    let mut session = session(StacksEpochId::Epoch24);

    let testnet = reward_cycle_length(&mut session, BOOT_TESTNET_ADDRESS);
    let mainnet = reward_cycle_length(&mut session, BOOT_MAINNET_ADDRESS);

    assert_eq!(
        testnet,
        Value::UInt(1050),
        "the testnet pox-3 is the one simnet's chain state follows"
    );
    assert_eq!(
        mainnet, testnet,
        "a mainnet-addressed snippet must reach the same contract, not the \
         dead twin that reports u2100"
    );
}

/// The same redirect reaches a contract that only exists in later epochs.
#[test]
fn a_mainnet_boot_reference_resolves_in_epoch_40() {
    let mut session = session(StacksEpochId::Epoch40);

    let testnet = run_user_snippet(
        &mut session,
        &format!("(contract-call? '{BOOT_TESTNET_ADDRESS}.pox-5 get-rewards)"),
    );
    let mainnet = run_user_snippet(
        &mut session,
        &format!("(contract-call? '{BOOT_MAINNET_ADDRESS}.pox-5 get-rewards)"),
    );
    assert_eq!(
        mainnet, testnet,
        "pox-5 arrives in epoch 4.0 and must answer the same through either address"
    );

    // And the resolver agrees about where that went.
    assert_eq!(
        session
            .resolve_contract_id(
                BOOT_TESTNET_ADDRESS,
                &format!("{BOOT_MAINNET_ADDRESS}.pox-5")
            )
            .unwrap()
            .to_string(),
        format!("{BOOT_TESTNET_ADDRESS}.pox-5"),
    );
}

/// sBTC is deployed at its mainnet address and only there, so that address is
/// correct and must survive the remap untouched.
#[test]
fn the_mainnet_sbtc_address_works_and_is_never_remapped() {
    let mut session = session(StacksEpochId::Epoch31);

    assert_eq!(
        run_user_snippet(
            &mut session,
            &format!("(contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-name)"),
        ),
        Value::okay(Value::string_ascii_from_bytes(b"sBTC".to_vec()).unwrap()).unwrap(),
    );

    let snippet = format!("(contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-name)");
    assert_eq!(
        session.remap_user_snippet(snippet.clone()),
        snippet,
        "an sBTC principal is not a boot principal and must not be rewritten"
    );
}

/// The gap this leaves, recorded so it is a decision rather than a surprise:
/// sBTC has no testnet twin in simnet, so the testnet address does not
/// resolve. It fails loudly, unlike the boot-address bug this all exists to
/// fix. Writing the *mainnet* sBTC address is the portable spelling — it
/// works here, and devnet/testnet deployments remap it.
#[test]
fn the_testnet_sbtc_address_does_not_resolve_in_simnet() {
    let mut session = session(StacksEpochId::Epoch31);

    let snippet = format!("(contract-call? '{SBTC_TESTNET_ADDRESS}.sbtc-token get-name)");
    assert_eq!(
        session.remap_user_snippet(snippet.clone()),
        snippet,
        "nothing rewrites it"
    );

    let error = session
        .eval(snippet, false)
        .expect_err("the testnet sBTC address is not deployed in simnet");
    assert!(
        error[0].message.contains("unresolved contract"),
        "expected a loud failure, got {}",
        error[0].message
    );
}

/// The boundary that protects real deployments: `eval` itself does not remap.
///
/// `clarinet-deployments::onchain` coerces contract-call arguments for devnet,
/// testnet and mainnet deployments by evaluating plan values through `eval`.
/// A mainnet boot principal is a *value* there and has to survive verbatim,
/// so the rewrite lives in `remap_user_snippet` at the user-facing entry
/// points instead.
#[test]
fn eval_itself_leaves_a_mainnet_boot_principal_alone() {
    let mut session = session(StacksEpochId::Epoch31);

    let principal = format!("'{BOOT_MAINNET_ADDRESS}.pox-3");
    let result = session
        .eval(principal, false)
        .expect("a principal literal should evaluate")
        .into_inner();

    let value = match result.result {
        EvaluationResult::Snippet(s) => s.result,
        EvaluationResult::Contract(_) => panic!("expected a snippet result"),
    };
    assert_eq!(
        value.to_string(),
        format!("{BOOT_MAINNET_ADDRESS}.pox-3"),
        "eval must not rewrite a principal it is only being asked to parse"
    );
}

/// A node that reports `network_id`, which is what decides whether a remote
/// session is mainnet-flavored. Mocked rather than live so the assertion is
/// deterministic and adds no pressure to the shared Hiro API rate limit.
fn mock_node(network_id: u32) -> (mockito::ServerGuard, ApiUrl) {
    let mut server = mockito::Server::new();

    // Anything not mocked answers 404 rather than mockito's default 501,
    // which the client would treat as retryable and sleep over.
    server
        .mock("GET", mockito::Matcher::Any)
        .with_status(404)
        .expect_at_least(0)
        .create();

    server
        .mock("GET", "/v2/info")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(format!(
            r#"{{"network_id": {network_id}, "stacks_tip_height": 556946}}"#
        ))
        .create();

    // `Session::new` resolves the block at `initial_height`; `fetch_block`
    // unwraps, so it has to be served.
    let block = serde_json::json!({
        "height": 556946,
        "burn_block_height": 882262,
        "tenure_height": 184037,
        "block_time": 1735934294,
        "burn_block_time": 1735451504,
        "hash": "0xaff3b535a135348ed00023ec1bdc3da9005253a9ce80a4906ade03ea6685d342",
        "index_block_hash": "0x201cf66636e693d95998b40ddd0cbe038432806046eed11866052f15a9fa8fc5",
        "burn_block_hash": "0x57f3e2bd4519e4263353bf6b7614a9cee7f2d36fe61409852d42e41afe5e6cad",
    })
    .to_string();
    server
        .mock(
            "GET",
            mockito::Matcher::Regex(r"^/extended/v2/blocks/.*$".to_string()),
        )
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(block)
        .expect_at_least(0)
        .create();

    let url = ApiUrl(server.url());
    (server, url)
}

fn remote_session(api_url: ApiUrl) -> Session {
    Session::new(SessionSettings {
        repl_settings: clarity_repl::repl::Settings {
            remote_data: RemoteDataSettings {
                enabled: true,
                api_url,
                initial_height: Some(556946),
                use_mainnet_wallets: false,
            },
            ..Default::default()
        },
        ..Default::default()
    })
}

/// Against a *mainnet* node the remote chain holds the real mainnet contracts,
/// so the mainnet addresses are the correct ones and nothing is rewritten.
#[test]
fn a_mainnet_remote_session_leaves_user_snippets_alone() {
    let (_server, url) = mock_node(1);
    let session = remote_session(url);
    assert!(session.interpreter.is_mainnet(), "network_id 1 is mainnet");

    let snippet = format!("(contract-call? '{BOOT_MAINNET_ADDRESS}.pox-3 get-pox-info)");
    assert_eq!(session.remap_user_snippet(snippet.clone()), snippet);
}

/// A *testnet*-backed remote session is testnet-flavored exactly like plain
/// simnet — `GlobalContext::mainnet` is false and the chain only has the
/// `ST000...` boot contracts — so it needs the same rewrite. Gating on
/// `remote_data.enabled` instead of the resolved network got this wrong and
/// asked the testnet API for a contract that does not exist there.
#[test]
fn a_testnet_remote_session_still_remaps_user_snippets() {
    let (_server, url) = mock_node(2_147_483_648);
    let session = remote_session(url);
    assert!(
        !session.interpreter.is_mainnet(),
        "a testnet network_id is not mainnet"
    );

    assert_eq!(
        session.remap_user_snippet(format!(
            "(contract-call? '{BOOT_MAINNET_ADDRESS}.pox-3 get-pox-info)"
        )),
        format!("(contract-call? '{BOOT_TESTNET_ADDRESS}.pox-3 get-pox-info)"),
    );
}

/// Minting has to credit the same contract a call executes against, or the
/// balance lands on the dead twin where nothing can observe it. `cost-voting`
/// is the boot contract with an FT.
#[test]
fn minting_a_boot_asset_credits_the_redirected_contract() {
    let session = session(StacksEpochId::Epoch24);

    let asset = session
        .resolve_asset_identifier(
            DEPLOYER,
            &format!("{BOOT_MAINNET_ADDRESS}.cost-voting.cost-vote-token"),
        )
        .expect("a boot asset identifier should parse");

    assert_eq!(
        asset.contract_identifier.to_string(),
        format!("{BOOT_TESTNET_ADDRESS}.cost-voting"),
    );
    assert_eq!(asset.asset_name.to_string(), "cost-vote-token");
}

/// sBTC has no twin, so its assets must be credited exactly as written.
#[test]
fn minting_an_sbtc_asset_is_not_redirected() {
    let session = session(StacksEpochId::Epoch31);

    let asset = session
        .resolve_asset_identifier(
            DEPLOYER,
            &format!("{SBTC_MAINNET_ADDRESS}.sbtc-token.sbtc-token"),
        )
        .expect("an sBTC asset identifier should parse");

    assert_eq!(
        asset.contract_identifier.to_string(),
        format!("{SBTC_MAINNET_ADDRESS}.sbtc-token"),
    );
}

/// `::get_costs` evaluates user Clarity through its own branch, ahead of the
/// bare-snippet arm, so it needs the rewrite too — as do `::read`, `::debug`,
/// `::trace` and `::perf`.
#[test]
fn console_commands_apply_the_remap_too() {
    let mut session = session(StacksEpochId::Epoch24);

    let (_, output) = session.process_console_input(&format!(
        "::get_costs (get reward-cycle-length (unwrap-panic (contract-call? '{BOOT_MAINNET_ADDRESS}.pox-3 get-pox-info)))"
    ));
    let rendered = output.join("\n");

    assert!(
        rendered.contains("1050"),
        "::get_costs should reach the testnet pox-3, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("2100"),
        "::get_costs must not reach the mainnet twin, got:\n{rendered}"
    );
}

/// The console composes the same two steps, so the wiring is what is checked
/// here rather than the value.
#[test]
fn the_console_applies_the_remap() {
    let mut session = session(StacksEpochId::Epoch24);

    let (_, output) = session.process_console_input(&format!(
        "(get reward-cycle-length (unwrap-panic (contract-call? '{BOOT_MAINNET_ADDRESS}.pox-3 get-pox-info)))"
    ));
    let rendered = output.join("\n");

    assert!(
        rendered.contains("1050"),
        "the console should report the testnet value, got:\n{rendered}"
    );
    assert!(
        !rendered.contains("2100"),
        "the console must not reach the mainnet twin, got:\n{rendered}"
    );
}
