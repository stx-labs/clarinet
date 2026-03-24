use clarity_repl::frontend::Terminal;
use clarity_repl::repl::{Session, SessionSettings};
use pico_args::Arguments;

fn main() {
    let mut args = Arguments::from_env();
    let _subcommand = args.subcommand().unwrap().unwrap_or_default();
    let code = args.subcommand().unwrap();

    let settings = SessionSettings {
        ..Default::default()
    };

    match code {
        Some(code_str) => {
            let mut session = Session::new(settings);

            let (_, output) = session.process_console_input(&code_str);
            for line in output {
                println!("{line}");
            }
        }
        None => loop {
            let mut terminal = Terminal::new(settings.clone());
            if !terminal.start() {
                break;
            }
        },
    }
}
