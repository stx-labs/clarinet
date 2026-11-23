use std::time::Duration;

use clarinet_files::StacksNetwork;
use clarity::util::hash::{bytes_to_hex, Hash160};
use mac_address::get_mac_address;
use serde_json::{json, Value};

const SEGMENT_HOST: &str = "https://api.segment.io";
const SEGMENT_WRITE_KEY: &str = "Q3xpmFRvy0psXnwBEXErtMBIeabOVjbC";

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
    hiro_system_kit::create_basic_runtime().block_on(send_event(event));
}

async fn send_event(event: DeveloperUsageEvent) {
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

    let user_id = format!("0x{}", bytes_to_hex(user_id.to_bytes().as_ref()));

    let _ = send_track(SEGMENT_HOST, &user_id, event_name, properties).await;
}

/// Posts a `track` call to Segment's HTTP tracking API.
async fn send_track(
    host: &str,
    user_id: &str,
    event: &str,
    properties: Value,
) -> reqwest::Result<()> {
    reqwest::Client::builder()
        .connect_timeout(Duration::from_secs(10))
        .build()?
        .post(format!("{host}/v1/track"))
        .basic_auth(SEGMENT_WRITE_KEY, Some(""))
        .json(&json!({
            "userId": user_id,
            "event": event,
            "properties": properties,
        }))
        .send()
        .await?
        .error_for_status()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use mockito::Matcher;

    use super::*;

    #[tokio::test]
    async fn posts_a_segment_track_call() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("POST", "/v1/track")
            // base64 of "<write key>:"
            .match_header(
                "authorization",
                "Basic UTN4cG1GUnZ5MHBzWG53QkVYRXJ0TUJJZWFiT1ZqYkM6",
            )
            .match_header("content-type", "application/json")
            .match_body(Matcher::JsonString(
                r#"{"userId":"0xabc","event":"CheckExecuted","properties":{"project_id":"0x01","sessions":2}}"#
                    .into(),
            ))
            .create_async()
            .await;

        let properties = json!({ "project_id": "0x01", "sessions": 2 });
        send_track(&server.url(), "0xabc", "CheckExecuted", properties)
            .await
            .unwrap();

        mock.assert_async().await;
    }
}
