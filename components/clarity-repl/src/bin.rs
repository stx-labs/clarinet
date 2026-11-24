use clarity_repl::frontend::Terminal;
use clarity_repl::repl::{Session, SessionSettings};

fn main() {
    // Positional arguments end at the first flag
    let mut args = std::env::args_os()
        .skip(1)
        .map(|arg| arg.into_string().expect("non-UTF-8 argument"))
        .take_while(|arg| !arg.starts_with('-'));
    let _subcommand = args.next();
    let code = args.next();

    let settings = SessionSettings::default();

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
