macro_rules! print_flush {
    ( $($t:tt)* ) => {
        {
            let mut stdout = std::io::stdout();
            write!(stdout, $($t)* ).unwrap();
            stdout.flush().unwrap();
        }
    }
}

pub(crate) use print_flush;
