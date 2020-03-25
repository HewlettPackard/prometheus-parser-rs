// (C) Copyright 2019-2020 Hewlett Packard Enterprise Development LP

extern crate prometheus_parser;

use prometheus_parser::*;

#[test]
fn parse_selector_subquery() -> Result<()> {
  // not a subquery
  assert_eq!(
    parse_expr(r#"foo[5m]"#)?,
    Selector::new()
      .metric("foo")
      .range(PromDuration::Minutes(5))
      .span((0, 7))
      .wrap()
  );

  // invalid whitespace
  assert_eq!(
    parse_expr(r#"foo[ 5m:]"#).is_err(),
    true
  );

  // valid whitespace
  assert_eq!(
    parse_expr(r#"foo[5m  :  30s   ]"#).is_ok(),
    true
  );

  assert_eq!(
    parse_expr(r#"foo[5m:]"#)?,
    Selector::new()
      .metric("foo")
      .subquery(Subquery::new(PromDuration::Minutes(5))
        .span((3, 8)))
      .span((0, 8))
      .wrap()
  );

  assert_eq!(
    parse_expr(r#"foo[5m:30s]"#)?,
    Selector::new()
      .metric("foo")
      .subquery(Subquery::new(PromDuration::Minutes(5))
        .resolution(PromDuration::Seconds(30))
        .span((3, 11)))
      .span((0, 11))
      .wrap()
  );

  Ok(())
}

#[test]
fn parse_group_subquery() -> Result<()> {
  assert_eq!(
    parse_expr(r#"(foo)[5m]"#).is_err(),
    true
  );

  assert_eq!(
    parse_expr(r#"(foo)[5m:]"#)?,
    Group::new(
      Selector::new().metric("foo").span((1, 4)).wrap()
    ).subquery(
      Subquery::new(PromDuration::Minutes(5)).span((5, 10))
    ).span((0, 10)).wrap()
  );

  Ok(())
}

#[test]
fn parse_function_subquery() -> Result<()> {
  // not a subquery
  assert_eq!(
    parse_expr(r#"sum(foo)[5m]"#).is_err(),
    true
  );

  // invalid whitespace
  assert_eq!(
    parse_expr(r#"sum(foo)[ 5m:]"#).is_err(),
    true
  );

  // valid whitespace
  assert_eq!(
    parse_expr(r#"sum(foo)[5m  :  30s   ]"#).is_ok(),
    true
  );

  assert_eq!(
    parse_expr(r#"sum(foo)[5m:]"#)?,
    Function::new("sum")
      .arg(Selector::new()
        .metric("foo")
        .span((4, 7))
        .wrap()
      )
      .subquery(Subquery::new(PromDuration::Minutes(5)).span((8, 13)))
      .span((0, 13))
      .wrap()
  );

  assert_eq!(
    parse_expr(r#"sum(foo)[5m:30s]"#)?,
    Function::new("sum")
      .arg(Selector::new()
        .metric("foo")
        .span((4, 7))
        .wrap()
      )
      .subquery(Subquery::new(PromDuration::Minutes(5))
        .resolution(PromDuration::Seconds(30))
        .span((8, 16)))
      .span((0, 16))
      .wrap()
  );

  Ok(())
}

#[test]
fn return_value_subquery() -> Result<()> {
  assert_eq!(
    parse_expr(r#"(foo)"#)?.return_value(),
    ReturnValue {
      kind: ReturnKind::InstantVector,
      label_ops: vec![]
    }
  );

  assert_eq!(
    parse_expr(r#"(foo)[5m:30s]"#)?.return_value(),
    ReturnValue {
      kind: ReturnKind::RangeVector,
      label_ops: vec![]
    }
  );

  // subquery on a range vector isn't allowed
  assert_eq!(
    parse_expr(r#"(foo[5m])[5m:30s]"#)?.return_value(),
    ReturnValue {
      kind: ReturnKind::unknown(
        "subquery on inner expression returning RangeVector is invalid",
        parse_expr(r#"(foo[5m])[5m:30s]"#)?
      ),
      label_ops: vec![]
    }
  );

  let expr = parse_expr(r#"sum(foo)"#)?;
  assert_eq!(
    expr.return_value(),
    ReturnValue {
      kind: ReturnKind::InstantVector,
      label_ops: vec![LabelSetOp::clear(expr.clone(), Some(Span {
        start: 0,
        end: 8
      }))]
    }
  );

  let expr = parse_expr(r#"sum(foo)[5m:]"#)?;
  assert_eq!(
    expr.return_value(),
    ReturnValue {
      kind: ReturnKind::RangeVector,
      label_ops: vec![LabelSetOp::clear(expr.clone(), Some(Span {
        start: 0,
        end: 13
      }))]
    }
  );

  Ok(())
}
