use std::io::{self, Write};
use std::process::Child;

/// A Bash process that forwards the completion requests to the completion scripts.
pub(crate) struct BashComplete {
    process: Child,
}

impl BashComplete {
    /// Creates a new bash completion process.
    pub(crate) fn new() -> io::Result<Self> {
        let mut process = std::process::Command::new("bash")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()?;
        let stdin = process.stdin.as_mut().unwrap();
        stdin.write_all(include_bytes!("bash_completion.sh"))?;
        Ok(Self { process })
    }

    /// Completes the given line.
    pub(crate) fn complete(mut self, line: &str) -> io::Result<Vec<String>> {
        let stdin = self.process.stdin.as_mut().unwrap();
        stdin.write_all(format!("get_completions '{}'\n", line).as_bytes())?;
        let output = self.process.wait_with_output()?;
        Ok(String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter_map(|suggestion| {
                let suggestion = suggestion.trim_end();
                if suggestion.is_empty() {
                    None
                } else {
                    Some(suggestion.to_owned())
                }
            })
            .collect::<Vec<String>>())
    }
}
