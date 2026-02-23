use clarinet_files::StacksNetwork;
use clarity_repl::clarity::util::hash::{bytes_to_hex, Hash160};
use mac_address::get_mac_address;
use segment::message::{Message, Track, User};
use segment::{Client, HttpClient};
use serde_json::json;

pub enum DeveloperUsageEvent {
    NewProject(DeveloperUsageDigest),
    PokeExecuted(DeveloperUsageDigest),
    CheckExecuted(DeveloperUsageDigest),
    DevnetExecuted(DeveloperUsageDigest),
    ProtocolPublished(DeveloperUsageDigest, StacksNetwork),
    DebugStarted(DeveloperUsageDigest, u32),
    DAPDebugStarted(DeveloperUsageDigest),
    UnknownCommand(DeveloperUsageDigest, String),
}

pub struct DeveloperUsageDigest {
    pub project_id: String,
    pub team_id: String,
}

impl DeveloperUsageDigest {
    pub fn new(project_id: &str, team_id: &[String]) -> Self {
        let hashed_project_id = Hash160::from_data(project_id.as_bytes());
        let hashed_team_id = Hash160::from_data(team_id.join(",").as_bytes());
        Self {
            project_id: format!("0x{}", bytes_to_hex(hashed_project_id.to_bytes().as_ref())),
            team_id: format!("0x{}", bytes_to_hex(hashed_team_id.to_bytes().as_ref())),
        }
    }
}

pub fn telemetry_report_event(event: DeveloperUsageEvent) {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .enable_time()
        .max_blocking_threads(32)
        .build()
        .unwrap();

    rt.block_on(send_event(event));
}

async fn send_event(event: DeveloperUsageEvent) {
    let segment_api_key = "Q3xpmFRvy0psXnwBEXErtMBIeabOVjbC";

    let clarinet_version = env!("CARGO_PKG_VERSION").to_string();
    let ci_mode = option_env!("CLARINET_MODE_CI").unwrap_or("0").to_string();
    let os = std::env::consts::OS;

    let (event_name, properties) = match event {
        DeveloperUsageEvent::NewProject(digest) => (
            "NewProject",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
            }),
        ),
        DeveloperUsageEvent::DevnetExecuted(digest) => (
            "DevnetExecuted",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
            }),
        ),
        DeveloperUsageEvent::ProtocolPublished(digest, network) => (
            "ProtocolPublished",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
                "network": format!("{:?}", network),
            }),
        ),
        DeveloperUsageEvent::CheckExecuted(digest) => (
            "CheckExecuted",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
            }),
        ),
        DeveloperUsageEvent::PokeExecuted(digest) => (
            "PokeExecuted",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
            }),
        ),
        DeveloperUsageEvent::DebugStarted(digest, num_sessions) => (
            "DebugStarted",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
                "sessions": num_sessions,
            }),
        ),
        DeveloperUsageEvent::DAPDebugStarted(digest) => (
            "DAPDebugStarted",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
            }),
        ),
        DeveloperUsageEvent::UnknownCommand(digest, command) => (
            "UnknownCommand",
            json!({
                "project_id": digest.project_id,
                "team_id": digest.team_id,
                "clarinet_version": clarinet_version,
                "ci_mode": ci_mode,
                "operating_system": os,
                "command": command,
            }),
        ),
    };

    let user_id = match get_mac_address() {
        Ok(Some(ma)) => Hash160::from_data(&ma.bytes()),
        Ok(None) => Hash160::from_data(&[0]),
        Err(_e) => return,
    };

    let client = HttpClient::default();

    let _ = client
        .send(
            segment_api_key.to_string(),
            Message::from(Track {
                user: User::UserId {
                    user_id: format!("0x{}", bytes_to_hex(user_id.to_bytes().as_ref())),
                },
                event: event_name.into(),
                properties,
                ..Default::default()
            }),
        )
        .await;
}
