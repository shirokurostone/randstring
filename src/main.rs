use randstring::parser::generate_rand_string;
use randstring::parser::root;

fn main() {
    if std::env::args().len() != 2 {
        std::process::exit(1);
    }

    let status = match std::env::args().nth(1) {
        Some(s) => match root(&s) {
            Ok((token, _)) => match generate_rand_string(&token) {
                Ok(string) => {
                    print!("{}", string);
                    0
                }
                Err(_) => 1,
            },
            Err(_) => 1,
        },
        _ => 1,
    };
    std::process::exit(status);
}
