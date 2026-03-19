use std::process::Command;

pub fn run_docker_command(
    command: &str,
    docker_host: &str,
) -> std::io::Result<std::process::Output> {
    if cfg!(target_os = "windows") {
        Command::new("cmd")
            .args(["/C", command])
            .env("DOCKER_HOST", docker_host)
            .output()
    } else {
        Command::new("sh")
            .arg("-c")
            .arg(command)
            .env("DOCKER_HOST", docker_host)
            .output()
    }
}
