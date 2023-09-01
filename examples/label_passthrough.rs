// (C) Copyright 2019 Hewlett Packard Enterprise Development LP

use std::env;

use prometheus_parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    let expr = &args[1];
    let labels: Vec<&str> = args[2..].iter().map(|s| s.as_str()).collect();
    let res = prometheus_parser::parse_expr(expr);

    match res {
        Ok(r) => {
            let return_value = r.return_value();
            println!("{:#?}", return_value.passthrough(&labels));
        }
        Err(e) => {
            eprintln!("error: {}", e);
        }
    };
}
