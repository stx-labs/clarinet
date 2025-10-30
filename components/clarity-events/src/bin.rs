extern crate serde;

extern crate serde_derive;

#[macro_use]
extern crate serde_json;

pub mod analysis;

use analysis::{EventCollector, Settings};
use clap::{Parser, Subcommand};
use clarinet_files::FileLocation;
use clarity_repl::clarity::analysis::type_checker::v2_05::TypeChecker;
use clarity_repl::clarity::costs::LimitedCostTracker;
use clarity_repl::clarity::EvaluationResult;
use clarity_repl::repl::{Session, SessionSettings};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Opts {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, PartialEq, Clone, Debug)]
enum Command {
    /// Format file
    #[clap(name = "scan", bin_name = "scan")]
    Scan(Scan),
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct Scan {
    /// File path
    pub file_path: String,
}

pub fn main() {
    let opts: Opts = match Opts::try_parse() {
        Ok(opts) => opts,
        Err(e) => {
            println!("{e}");
            std::process::exit(1);
        }
    };

    match opts.command {
        Command::Scan(cmd) => {
            let file = FileLocation::from_path_string(&cmd.file_path).unwrap();
            let snippet = file.read_content_as_utf8().unwrap();
            let mut session = Session::new(SessionSettings::default());
            let mut contract_analysis = match session.eval(snippet, false) {
                Ok(execution) => match execution.result {
                    EvaluationResult::Contract(evaluation) => evaluation.contract.analysis,
                    _ => {
                        println!("empty contract");
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    println!("Error path: {e:?}");
                    std::process::exit(1);
                }
            };

            {
                let mut analysis_db = session.interpreter.clarity_datastore.as_analysis_db();
                let cost_track = LimitedCostTracker::new_free();
                let type_checker = TypeChecker::new(&mut analysis_db, cost_track, true);
                let settings = Settings {
                    clarity_version: contract_analysis.clarity_version,
                };
                let mut event_collector = EventCollector::new(settings, type_checker);
                let event_map = event_collector.run(&mut contract_analysis);
                for (key, events) in event_map.iter() {
                    if events.is_empty() {
                        continue;
                    }
                    if let Some(method) = key {
                        println!("{method}");
                        for event in events.iter() {
                            println!("- {}", json!(event));
                            // println!("- {:?}", event);
                        }
                    }
                }
            }
        }
    };
}
