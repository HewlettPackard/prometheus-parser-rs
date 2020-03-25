// (C) Copyright 2019 Hewlett Packard Enterprise Development LP

use std::env;

use prometheus_parser;

fn main() {
  let args: Vec<String> = env::args().collect();
  let res = prometheus_parser::parse_expr(&args[1]);

  match res {
    Ok(r) => {
      println!("{:#?}", r.return_value());
    },
    Err(e) => {
      eprintln!("error: {}", e);
    }
  };
}
