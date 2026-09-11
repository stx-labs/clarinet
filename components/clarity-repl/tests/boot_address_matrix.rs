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

// A shared cache directory, so repeated runs don't hammer the Hiro API.
// Namespaced by the nextest run id like the other remote-data tests.
fn shared_cache_dir() -> std::path::PathBuf {
    let run_id = std::env::var("NEXTEST_RUN_ID").unwrap_or_else(|_| "default".to_string());
    std::env::temp_dir().join(format!("clarinet-test-mxs-cache-{run_id}"))
}

fn remote_session(api_url: &str, initial_height: u32) -> Session {
    Session::new(SessionSettings {
        cache_location: Some(shared_cache_dir()),
        repl_settings: clarity_repl::repl::Settings {
            remote_data: RemoteDataSettings {
                enabled: true,
                api_url: ApiUrl(api_url.to_string()),
                initial_height: Some(initial_height),
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
    let session = remote_session("https://api.hiro.so", 556946);
    assert!(
        session.interpreter.is_mainnet(),
        "the fixture must really resolve to mainnet"
    );

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
    let session = remote_session("https://api.testnet.hiro.so", 80000);
    assert!(
        !session.interpreter.is_mainnet(),
        "a testnet-backed remote session is not mainnet"
    );

    assert_eq!(
        session.remap_user_snippet(format!(
            "(contract-call? '{BOOT_MAINNET_ADDRESS}.pox-3 get-pox-info)"
        )),
        format!("(contract-call? '{BOOT_TESTNET_ADDRESS}.pox-3 get-pox-info)"),
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
