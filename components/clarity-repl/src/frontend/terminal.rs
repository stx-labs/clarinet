use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::repl::settings::SessionSettings;
use crate::repl::Session;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
const HISTORY_FILE: Option<&'static str> = option_env!("CLARITY_REPL_HISTORY_FILE");

enum Input {
    Incomplete(char),
    Complete(),
}

fn complete_input(str: &str) -> Result<Input, (char, char)> {
    let mut forms: Vec<&str> = vec![];
    let mut paren_count = 0;
    let mut last_pos = 0;

    let mut brackets = vec![];
    let mut skip_next = false;
    let mut in_string = false;

    for (pos, character) in str.char_indices() {
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
                // skip whitespace between the previous form's
                // closing paren (if there is one) and the current
                // form's opening paren
                match (character, paren_count) {
                    ('(', 0) => {
                        paren_count += 1;
                        last_pos = pos
                    }
                    ('(', _) => paren_count += 1,
                    _ => {}
                }
            }
            ')' | '}' if !in_string => {
                match (brackets.pop(), character) {
                    (Some('('), '}') => return Err((')', '}')),
                    (Some('{'), ')') => return Err(('}', ')')),
                    _ => {}
                };
                if character == ')' {
                    paren_count -= 1;
                    if paren_count == 0 {
                        forms.push(&str[last_pos..pos + 1]);
                    }
                }
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
