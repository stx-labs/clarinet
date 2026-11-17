use clarity::vm::EvaluationResult;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::repl::clarity_values::value_to_string;
use crate::repl::diagnostic::output_diagnostic;
use crate::repl::settings::SessionSettings;
use crate::repl::Session;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
const HISTORY_FILE: Option<&'static str> = option_env!("CLARITY_REPL_HISTORY_FILE");

enum Input {
    Incomplete(char),
    Complete(),
}

fn complete_input(str: &str) -> Result<Input, (char, char)> {
    let mut brackets = vec![];
    let mut skip_next = false;
    let mut in_string = false;

    for character in str.chars() {
        // if the previous character was a backslash, skip this character (only in strings)
        if skip_next {
            skip_next = false;
            continue;
        }

        match character {
            '\\' if in_string => {
                skip_next = true;
            }
            '"' => in_string = !in_string,
            '(' | '{' if !in_string => {
                brackets.push(character);
            }
            ')' | '}' if !in_string => {
                match (brackets.pop(), character) {
                    (Some('('), '}') => return Err((')', '}')),
                    (Some('{'), ')') => return Err(('}', ')')),
                    _ => {}
                };
            }
            _ => {}
        }
    }

    match brackets.last() {
        Some(char) => Ok(Input::Incomplete(*char)),
        _ => Ok(Input::Complete()),
    }
}

pub struct Terminal {
    pub session: Session,
    pub executed: Vec<String>,
}

impl Terminal {
    pub fn new(session_settings: SessionSettings) -> Terminal {
        Terminal {
            session: Session::new(session_settings),
            executed: vec![],
        }
    }

    pub fn load(session: Session) -> Terminal {
        Terminal {
            session,
            executed: vec![],
        }
    }

    pub fn eval(&mut self, input: &str) -> Result<(), String> {
        self.executed.push(input.to_string());
        self.session.interpreter.repl_settings.show_timings = false;
        let result = if let Some(filename) = input.strip_prefix("::read ") {
            std::fs::read_to_string(filename)
                .map_err(|error| format!("unable to read {filename}: {error}"))
                .and_then(|snippet| self.eval_snippet(&snippet))
        } else if input.starts_with("::debug") {
            Err("::debug requires an interactive console".into())
        } else if input.starts_with("::trace")
            || input.starts_with("::perf")
            || input.starts_with("::get_costs")
        {
            let mut output = vec![];
            let result = if input.starts_with("::trace") {
                self.session.trace(&mut output, input)
            } else if input.starts_with("::perf") {
                self.session.perf(&mut output, input)
            } else {
                self.session.get_costs(&mut output, input)
            };
            result
                .map(|_| output.join("\n"))
                .map_err(|errors| errors.join("\n"))
        } else if input.starts_with("::") {
            self.session.handle_command_result(input)
        } else {
            self.eval_snippet(input)
        };
        match result {
            Ok(output) => {
                println!("{output}");
                Ok(())
            }
            Err(error) => {
                eprintln!("{error}");
                Err(error)
            }
        }
    }

    fn eval_snippet(&mut self, snippet: &str) -> Result<String, String> {
        let snippet = self.session.remap_user_snippet(snippet.to_string());
        let lines = snippet.lines().map(String::from).collect::<Vec<_>>();
        let result = self.session.eval(snippet, false).map_err(|diagnostics| {
            diagnostics
                .iter()
                .flat_map(|diagnostic| output_diagnostic(diagnostic, "<stdin>", &lines))
                .collect::<Vec<_>>()
                .join("\n")
        })?;
        for diagnostic in result
            .diagnostics
            .iter()
            .cloned()
            .map(Into::into)
            .chain(result.lint_diagnostics.iter().cloned())
        {
            for line in output_diagnostic(&diagnostic, "<stdin>", &lines) {
                eprintln!("{line}");
            }
        }
        Ok(match result.into_inner().result {
            EvaluationResult::Contract(contract) => contract
                .result
                .as_ref()
                .map(value_to_string)
                .unwrap_or_default(),
            EvaluationResult::Snippet(snippet) => value_to_string(&snippet.result),
        })
    }

    pub fn start(&mut self) -> bool {
        println!("{}", green!("clarity-repl v{}", VERSION.unwrap()));
        println!("{}", black!("Enter \"::help\" for usage hints."));
        println!("{}", black!("Connected to a transient in-memory database."));

        if let Some(contracts) = self.session.get_contracts() {
            println!("{contracts}");
        }
        if let Some(accounts) = self.session.get_accounts() {
            println!("{accounts}");
        }

        let mut editor = DefaultEditor::new().expect("Failed to initialize cli");
        let mut ctrl_c_acc = 0;
        let mut input_buffer = vec![];
        let mut prompt = String::from(">> ");

        editor
            .load_history(HISTORY_FILE.unwrap_or("history.txt"))
            .ok();
        let reload = loop {
            let readline = editor.readline(prompt.as_str());
            match readline {
                Ok(command) => {
                    ctrl_c_acc = 0;
                    input_buffer.push(command);
                    let input = input_buffer.join(" ");
                    match complete_input(&input) {
                        Ok(Input::Complete()) => {
                            let (reload, output) = self.session.process_console_input(&input);

                            for line in output {
                                println!("{line}");
                            }
                            prompt = String::from(">> ");
                            self.executed.push(input.to_string());
                            let _ = editor.add_history_entry(input);
                            input_buffer.clear();
                            if reload {
                                break true;
                            }
                        }
                        Ok(Input::Incomplete(str)) => {
                            prompt = format!("{str}.. ");
                        }
                        Err((expected, got)) => {
                            println!("Error: expected closing {expected}, got {got}");
                            prompt = String::from(">> ");
                            input_buffer.pop();
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    ctrl_c_acc += 1;
                    if ctrl_c_acc == 2 {
                        break false;
                    } else {
                        println!("{}", yellow!("Hit CTRL-C a second time to quit."));
                    }
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break false;
                }
                Err(err) => {
                    println!("Error: {err:?}");
                    break false;
                }
            }
        };
        editor
            .save_history(HISTORY_FILE.unwrap_or("history.txt"))
            .unwrap();
        reload
    }
}

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::*;

    #[test_case(r#"(list (list u1)"# ; "incomplete input missing closing parenthesis")]
    #[test_case(r#"{ a: { b: 1 }"# ; "incomplete input missing closing curly brace")]
    #[test_case(r#"(concat u")""# ; "incomplete input with closing parenthesis in string")]
    #[test_case(r#"{ a: { b: "}" }"# ; "incomplete input with closing curly brace in string")]
    #[test_case(r#"{foo: "\"""# ; "incomplete input with escaped quote in string")]
    fn test_incomplete_input(input: &str) {
        let r = complete_input(input).unwrap();

        assert!(matches!(r, Input::Incomplete(_)));
    }

    #[test_case(r#"(list (list u1 u2) (list u3 u4))"# ; "complete input with parenthesis")]
    #[test_case(r#"{ a: { b: 1 } }"# ; "complete input with curly braces")]
    #[test_case(r#"(len u"And this is an UTF-8 string \u{1f601}")"# ; "complete input with escaped utf8 in tuple")]
    #[test_case(r#"(list u"\u{ff}")"# ; "complete input with escaped urf8 in parenthesis")]
    #[test_case(r#"{foo: "\\"}"# ; "complete input with escaped backslash in string")]
    #[test_case(r#"{foo: "\""}"# ; "complete input with escaped quote in string")]
    fn test_complete_input(input: &str) {
        let r = complete_input(input).unwrap();

        assert!(matches!(r, Input::Complete()));
    }
}
