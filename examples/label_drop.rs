// (C) Copyright 2019-2020 Hewlett Packard Enterprise Development LP

use std::env;

use prometheus_parser;

use prometheus_parser::ReturnKind;

fn main() {
  let args: Vec<String> = env::args().collect();

  let expr = &args[1];
  let label = &args[2];
  let res = prometheus_parser::parse_expr(expr);

  match res {
    Ok(r) => {
      let return_value = r.return_value();

      if let ReturnKind::Unknown(cause) = &return_value.kind {
        println!("note: expression return type is unknown, result may be inaccurate");
        println!("  reason:     {}", cause.message);
        println!("  expression: {}", cause.expression);
        println!();
      }

      if let Some(drops) = return_value.drops(label) {
        println!("label '{}' is dropped:\n", label);

        if let Some(span) = drops.span {
          println!("{}", expr);
          println!(
            "{}{}",
            " ".repeat(span.start),
            "-".repeat(span.end - span.start)
          );
        }

        println!();
        println!("parent expression: {}", drops.expression);
      } else {
        println!("label '{}' is not dropped", label);
      }
    },
    Err(e) => {
      eprintln!("error: {}", e);
    }
  };
}
