use nix::{errno::Errno, sys::signal, unistd};

/// Sets the disposition of a signal handler.
///
/// For portability reasons (see `signal(2)`), signals may only be ignored or
/// reset to their default disposition.
#[cfg(unix)]
pub(crate) fn signal_hook(signal: signal::Signal, handler: signal::SigHandler) {
    unsafe {
        // SAFETY: The handler is strongly typed, and the previous handler is discarded.
        signal::signal(signal, handler).expect("signal handling");
    }
}

/// Initializes the terminal for a shell.
///
/// See [the glibc documentation](https://www.gnu.org/software/libc/manual/html_node/Initializing-the-Shell.html).
#[cfg(all(unix, not(miri)))]
pub(crate) fn acquire_terminal() -> unistd::Pid {
    // Wait for the process to be in the foreground
    let shell_pgid = unistd::getpgrp();
    let stdin = std::io::stdin();
    loop {
        match unistd::tcgetpgrp(&stdin) {
            Ok(owner_pgid) if owner_pgid == shell_pgid => break,
            Err(Errno::ENOTTY) => panic!("No TTY for interactive shell"),
            _ => {
                signal::killpg(shell_pgid, signal::Signal::SIGTTIN)
                    .expect("Failed to send SIGTTIN to shell process group");
            }
        }
    }

    // Ignore interactive and job-control signals
    // Leave SIGCHLD alone, since we use it to reap child processes
    for signal in [
        signal::SIGINT,
        signal::SIGQUIT,
        signal::SIGTSTP,
        signal::SIGTTIN,
        signal::SIGTTOU,
    ] {
        signal_hook(signal, signal::SigHandler::SigIgn);
    }

    let shell_pid = unistd::getpid();
    if shell_pid != shell_pgid {
        // Put ourselves in our own process group
        let shell_pgid = shell_pid;
        match unistd::setpgid(shell_pgid, shell_pgid) {
            Ok(()) | Err(Errno::EPERM) => {}
            Err(_) => panic!("Failed to set the process group"),
        }
        // Grab control of the terminal
        let _ = unistd::tcsetpgrp(&stdin, shell_pgid);
    }
    shell_pid
}

#[cfg(not(all(unix, not(miri))))]
pub(crate) fn acquire_terminal() -> unistd::Pid {
    unistd::getpid()
}
