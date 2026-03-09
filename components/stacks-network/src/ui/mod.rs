#[allow(dead_code)]
mod app;
#[allow(clippy::module_inception)]
mod ui;
#[allow(dead_code)]
mod util;

use std::error::Error;
use std::io::{stdout, Stdout};
use std::sync::mpsc::{Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};

use app::App;
use chainhook_sdk::utils::Context;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use ratatui::backend::CrosstermBackend;
use ratatui::Terminal;

use super::DevnetEvent;
use crate::chains_coordinator::BitcoinMiningCommand;
use crate::event_logger::DevnetEventLogger;
use crate::ChainsCoordinatorCommand;

pub fn start_ui(
    devnet_events_tx: Sender<DevnetEvent>,
    devnet_events_rx: Receiver<DevnetEvent>,
    chains_coordinator_commands_tx: crossbeam_channel::Sender<ChainsCoordinatorCommand>,
    orchestrator_terminated_rx: Receiver<bool>,
    devnet_path: &str,
    automining_enabled: bool,
    ctx: &Context,
) -> Result<(), String> {
    let res = do_start_ui(
        devnet_events_tx,
        devnet_events_rx,
        chains_coordinator_commands_tx,
        orchestrator_terminated_rx,
        devnet_path,
        automining_enabled,
        ctx,
    );
    if let Err(ref _e) = res {
        // potential additional cleaning
    }
    res
}

pub fn do_start_ui(
    devnet_events_tx: Sender<DevnetEvent>,
    devnet_events_rx: Receiver<DevnetEvent>,
    chains_coordinator_commands_tx: crossbeam_channel::Sender<ChainsCoordinatorCommand>,
    orchestrator_terminated_rx: Receiver<bool>,
    devnet_path: &str,
    automining_enabled: bool,
    ctx: &Context,
) -> Result<(), String> {
    enable_raw_mode().map_err(|e| format!("unable to start terminal ui: {e}"))?;

    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)
        .map_err(|e| format!("unable to start terminal ui: {e}"))?;

    let backend = CrosstermBackend::new(stdout);

    let mut terminal =
        Terminal::new(backend).map_err(|e| format!("unable to start terminal ui: {e}"))?;

    // Setup input handling
    let tick_rate = Duration::from_millis(500);
    thread::spawn(move || {
        let mut last_tick = Instant::now();
        loop {
            // poll for tick rate duration, if no events, sent tick event.
            let timeout = tick_rate
                .checked_sub(last_tick.elapsed())
                .unwrap_or_else(|| Duration::from_secs(0));
            if event::poll(timeout).unwrap() {
                if let Event::Key(key) = event::read().unwrap() {
                    devnet_events_tx.send(DevnetEvent::KeyEvent(key)).unwrap();
                }
            }
            if last_tick.elapsed() >= tick_rate {
                if devnet_events_tx.send(DevnetEvent::Tick).is_err() {
                    break;
                }
                last_tick = Instant::now();
            }
        }
    });

    let mut app = App::new("Clarinet", devnet_path, ctx.clone());

    terminal
        .clear()
        .map_err(|e| format!("unable to start terminal ui: {e}"))?;

    let mut mining_command_tx: Option<Sender<BitcoinMiningCommand>> = None;

    loop {
        terminal
            .draw(|f| ui::draw(f, &mut app))
            .map_err(|e| format!("unable to update ui: {e}"))?;

        let event = match devnet_events_rx.recv() {
            Ok(event) => event,
            Err(e) => {
                app.handle_log(DevnetEvent::log_error(format!(
                    "Error receiving event: {e}"
                )));
                let _ = terminate(
                    &mut terminal,
                    chains_coordinator_commands_tx,
                    orchestrator_terminated_rx,
                );
                break;
            }
        };
        match event {
            DevnetEvent::KeyEvent(event) => match (event.modifiers, event.code) {
                (KeyModifiers::CONTROL, KeyCode::Char('c')) => {
                    app.handle_log(DevnetEvent::log_warning(
                        "Ctrl+C received, initiating termination sequence.".into(),
                    ));
                    let _ = terminate(
                        &mut terminal,
                        chains_coordinator_commands_tx,
                        orchestrator_terminated_rx,
                    );
                    break;
                }
                (KeyModifiers::NONE, KeyCode::Char('n')) => {
                    if let Some(ref tx) = mining_command_tx {
                        let _ = tx.send(BitcoinMiningCommand::Mine);
                        app.handle_log(DevnetEvent::log_success(
                            "Bitcoin block mining triggered manually".to_string(),
                        ));
                    } else {
                        app.handle_log(DevnetEvent::log_error(
                            "Manual block mining not ready".to_string(),
                        ));
                    }
                }
                (KeyModifiers::NONE, KeyCode::Left) => app.on_left(),
                (KeyModifiers::NONE, KeyCode::Up) => app.on_up(),
                (KeyModifiers::NONE, KeyCode::Right) => app.on_right(),
                (KeyModifiers::NONE, KeyCode::Down) => app.on_down(),
                _ => {}
            },
            DevnetEvent::Tick => {
                app.on_tick();
            }
            DevnetEvent::Log(log) => {
                app.handle_log(log);
            }
            DevnetEvent::ServiceStatus(status) => {
                app.handle_service_status(status);
            }
            DevnetEvent::StacksChainEvent(ref chain_event) => {
                app.handle_stacks_chain_event(chain_event);
            }
            DevnetEvent::BitcoinChainEvent(ref chain_event) => {
                app.handle_bitcoin_chain_event(chain_event);
            }
            DevnetEvent::MempoolAdmission(tx) => {
                app.handle_mempool_admission(tx);
            }
            DevnetEvent::ProtocolDeployingProgress(data) => {
                app.handle_protocol_deploying_progress(data);
            }
            DevnetEvent::FatalError(message) => {
                app.handle_fatal_error(&message);
                let _ = terminate(
                    &mut terminal,
                    chains_coordinator_commands_tx,
                    orchestrator_terminated_rx,
                );
                return Err(message);
            }
            DevnetEvent::BootCompleted(bitcoin_mining_tx) => {
                app.handle_boot_completed();
                if automining_enabled {
                    let _ = bitcoin_mining_tx.send(BitcoinMiningCommand::Start);
                }
                mining_command_tx = Some(bitcoin_mining_tx);
            }
            DevnetEvent::Terminate => {
                break;
            }
        }
        if app.should_quit {
            break;
        }
    }

    Ok(())
}

fn terminate(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    chains_coordinator_commands_tx: crossbeam_channel::Sender<ChainsCoordinatorCommand>,
    orchestrator_terminated_rx: Receiver<bool>,
) -> Result<(), Box<dyn Error>> {
    let _ = disable_raw_mode();
    let _ = execute!(terminal.backend_mut(), LeaveAlternateScreen);
    let res = chains_coordinator_commands_tx.send(ChainsCoordinatorCommand::Terminate);
    if let Err(e) = res {
        println!("Error sending terminate command: {e}");
    }
    let res = orchestrator_terminated_rx.recv();
    if let Err(e) = res {
        println!("Error sending terminate command: {e}");
    }
    let _ = terminal.show_cursor();
    Ok(())
}
