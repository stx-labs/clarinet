use clarinet_files::ProjectManifest;
use clarinet_lib::frontend::cli::{load_deployment_and_artifacts_or_exit, load_manifest_or_exit};

fn load_manifest() -> ProjectManifest {
    let cwd = std::env::current_dir().unwrap();
    let manifest_path = cwd
        .join("tests/fixtures/mxs/Clarinet.toml")
        .to_string_lossy()
        .to_string();
    load_manifest_or_exit(Some(manifest_path), true)
}

#[test]
fn test_session_with_testnet_accounts() {
    let mut manifest = load_manifest();
    manifest.repl_settings.remote_data.use_mainnet_wallets = false;
    let (_deployment, _location, artifacts) =
        load_deployment_and_artifacts_or_exit(&manifest, &None, false, true, false);
    let session = artifacts.session;
    let accounts = session.interpreter.get_accounts();
    assert!(accounts.iter().all(|a| a.starts_with("ST")));
}

#[test]
fn test_session_with_mainnet_accounts() {
    let cwd = std::env::current_dir().unwrap();
    let manifest_path = cwd
        .join("tests/fixtures/mxs/Clarinet.toml")
        .to_string_lossy()
        .to_string();
    let manifest = load_manifest_or_exit(Some(manifest_path), true);
    let (_deployment, _location, artifacts) =
        load_deployment_and_artifacts_or_exit(&manifest, &None, false, true, false);
    let session = artifacts.session;
    let accounts = session.interpreter.get_accounts();
    assert!(accounts.iter().all(|a| a.starts_with("SP")));
}
