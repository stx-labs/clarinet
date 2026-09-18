//! Shared fixtures for the mocked remote-data (MXS) tests.
//!
//! Included with `mod common;`, so each test binary compiles its own copy and
//! only uses part of it.
#![allow(dead_code)]

use clarity_repl::repl::settings::{ApiUrl, RemoteDataSettings};
use clarity_repl::repl::{Session, SessionSettings};
use mockito::ServerGuard;

/// The tip a mocked node reports, and the block `Session::new` then resolves.
pub const MOCK_TIP_HEIGHT: u32 = 556946;

/// A node that reports `network_id`, which is what decides whether a remote
/// session is mainnet-flavored.
///
/// Mocked rather than live so the assertions are deterministic and add no
/// pressure to the shared Hiro API rate limit. The guard is returned so a
/// caller can register further mocks on the same server.
pub fn mock_node(network_id: u32) -> (ServerGuard, ApiUrl) {
    let mut server = mockito::Server::new();

    // Anything not mocked answers 404 rather than mockito's default 501,
    // which the client would treat as retryable and sleep over.
    server
        .mock("GET", mockito::Matcher::Any)
        .with_status(404)
        .expect_at_least(0)
        .create();

    // Only the fields `Info` and `Block` actually deserialize; neither denies
    // unknown fields, so a real node response is a superset of these.
    server
        .mock("GET", "/v2/info")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(format!(
            r#"{{"network_id": {network_id}, "stacks_tip_height": {MOCK_TIP_HEIGHT}}}"#
        ))
        .create();

    // `Session::new` resolves the block at `initial_height`; `fetch_block`
    // unwraps, so it has to be served.
    let block = serde_json::json!({
        "height": MOCK_TIP_HEIGHT,
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

/// A session backed by the node at `api_url`.
pub fn remote_session(api_url: ApiUrl) -> Session {
    Session::new(SessionSettings {
        repl_settings: clarity_repl::repl::Settings {
            remote_data: RemoteDataSettings {
                enabled: true,
                api_url,
                initial_height: Some(MOCK_TIP_HEIGHT),
                use_mainnet_wallets: false,
            },
            ..Default::default()
        },
        ..Default::default()
    })
}
