use randstring::parser::generate_rand_string;
use randstring::parser::root;

use clap::Parser;

#[derive(Parser)]
#[command(name = "randstring")]
#[command(author = "shirokurostone")]
#[command(version = "0.0.1")]
#[command(about = None, long_about = None)]
struct Cli {
    regexp: String,
}

fn main() {
    let cli = Cli::parse();

    let regexp = cli.regexp;

    let status = match root(&regexp){
        Ok((token, _)) => match generate_rand_string(&token) {
            Ok(string) => {
                print!("{}", string);
                0
            }
            Err(_) => 1,
        },
        Err(_) => 1,
    };

    std::process::exit(status);
}
