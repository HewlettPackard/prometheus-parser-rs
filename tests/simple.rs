// (C) Copyright 2019 Hewlett Packard Enterprise Development LP

extern crate prometheus_parser;

use prometheus_parser::*;

#[test]
fn parse_greater_than() -> Result<()> {
  assert_eq!(
    parse_expr("foo > bar")?,
    Operator::new(
      OperatorKind::GreaterThan,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((6, 9)).wrap()
    ).span((3, 6)).wrap()
  );

  assert_ne!(
    parse_expr("foo < bar")?,
    Operator::new(
      OperatorKind::GreaterThan,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((6, 9)).wrap()
    ).span((3, 6)).wrap()
  );

  Ok(())
}

#[test]
fn parse_whitespace() -> Result<()> {
  assert_eq!(
    parse_expr(r#"
      foo
      >
      bar
    "#)?,
    Operator::new(
      OperatorKind::GreaterThan,
      Selector::new().metric("foo").span((7, 10)).wrap(),
      Selector::new().metric("bar").span((25, 28)).wrap()
    ).span((10, 25)).wrap()
  );

  Ok(())
}

#[test]
fn parse_less_than() -> Result<()> {
  assert_ne!(   // set to assert not equal
    parse_expr("foo > bar")?,
    Operator::new(
      OperatorKind::LessThan,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((6, 9)).wrap()
    ).span((3, 6)).wrap()
  );

  assert_eq!(
    parse_expr("foo < bar")?,
    Operator::new(
      OperatorKind::LessThan,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((6, 9)).wrap()
    ).span((3, 6)).wrap()
  );

  Ok(())
}

#[test]
fn parse_add() -> Result<()> {
  assert_eq!(
    parse_expr("foo + bar")?,
    Operator::new(
      OperatorKind::Add,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((6, 9)).wrap()
    ).span((3, 6)).wrap()
  );

  Ok(())
}

#[test]
fn parse_equal() -> Result<()> {
  assert_eq!(
    parse_expr("foo == bar")?,
    Operator::new(
      OperatorKind::Equal,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((7, 10)).wrap()
    ).span((3, 7)).wrap()
  );

  Ok(())
}

#[test]
fn parse_not_equal() -> Result<()> {
  assert_eq!(
    parse_expr("foo != foo")?,
    Operator::new(
      OperatorKind::NotEqual,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("foo").span((7, 10)).wrap()
    ).span((3, 7)).wrap()
  );

  Ok(())
}

#[test]
fn parse_power() -> Result<()> {
  assert_eq!(
    parse_expr("check ^ taco")?,
    Operator::new(
      OperatorKind::Power,
      Selector::new().metric("check").span((0, 5)).wrap(),
      Selector::new().metric("taco").span((8, 12)).wrap()
    ).span((5, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_multiply() -> Result<()> {
  assert_eq!(
    parse_expr("check * value")?,
    Operator::new(
      OperatorKind::Multiply,
      Selector::new().metric("check").span((0, 5)).wrap(),
      Selector::new().metric("value").span((8, 13)).wrap()
    ).span((5, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_divide() -> Result<()> {
  assert_eq!(
    parse_expr("check / value")?,
    Operator::new(
      OperatorKind::Divide,
      Selector::new().metric("check").span((0, 5)).wrap(),
      Selector::new().metric("value").span((8, 13)).wrap()
    ).span((5, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_modulo() -> Result<()> {
  assert_eq!(
    parse_expr("check % value")?,
    Operator::new(
      OperatorKind::Modulo,
      Selector::new().metric("check").span((0, 5)).wrap(),
      Selector::new().metric("value").span((8, 13)).wrap()
    ).span((5, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_subtract() -> Result<()> {
  assert_eq!(
    parse_expr("check - value")?,
    Operator::new(
      OperatorKind::Subtract,
      Selector::new().metric("check").span((0, 5)).wrap(),
      Selector::new().metric("value").span((8, 13)).wrap()
    ).span((5, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_less_than_equal() -> Result<()> {
  assert_eq!(
    parse_expr("this <= that")?,
    Operator::new(
      OperatorKind::LessThanEqual,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((8, 12)).wrap()
    ).span((4, 8)).wrap()
  );

  assert_ne!(
    parse_expr("this >= that")?,
    Operator::new(
      OperatorKind::LessThanEqual,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((8, 12)).wrap()
    ).span((4, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_greater_than_equal() -> Result<()> {
  assert_eq!(
    parse_expr("this >= that")?,
    Operator::new(
      OperatorKind::GreaterThanEqual,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((8, 12)).wrap()
    ).span((4, 8)).wrap()
  );

  assert_ne!(
    parse_expr("this <= that")?,
    Operator::new(
      OperatorKind::GreaterThanEqual,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((8, 12)).wrap()
    ).span((4, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_and() -> Result<()> {
  assert_eq!(
    parse_expr("this and that")?,
    Operator::new(
      OperatorKind::And,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((9, 13)).wrap()
    ).span((4, 9)).wrap()
  );

  assert_eq!(
    parse_expr("this And that")?,
    Operator::new(
      OperatorKind::And,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((9, 13)).wrap()
    ).span((4, 9)).wrap()
  );

  assert_eq!(
    parse_expr("this AND that")?,
    Operator::new(
      OperatorKind::And,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((9, 13)).wrap()
    ).span((4, 9)).wrap()
  );

  Ok(())
}

#[test]
fn parse_or() -> Result<()> {
  assert_eq!(
    parse_expr("this or that")?,
    Operator::new(
      OperatorKind::Or,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((8, 12)).wrap()
    ).span((4, 8)).wrap()
  );

  assert_eq!(
    parse_expr("this Or that")?,
    Operator::new(
      OperatorKind::Or,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((8, 12)).wrap()
    ).span((4, 8)).wrap()
  );

  assert_eq!(
    parse_expr("this OR that")?,
    Operator::new(
      OperatorKind::Or,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((8, 12)).wrap()
    ).span((4, 8)).wrap()
  );

  Ok(())
}

#[test]
fn parse_unless() -> Result<()> {
  assert_eq!(
    parse_expr("this unless that")?,
    Operator::new(
      OperatorKind::Unless,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((12, 16)).wrap()
    ).span((4, 12)).wrap()
  );

  assert_eq!(
    parse_expr("this Unless that")?,
    Operator::new(
      OperatorKind::Unless,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((12, 16)).wrap()
    ).span((4, 12)).wrap()
  );

  assert_eq!(
    parse_expr("this UNLESS that")?,
    Operator::new(
      OperatorKind::Unless,
      Selector::new().metric("this").span((0, 4)).wrap(),
      Selector::new().metric("that").span((12, 16)).wrap()
    ).span((4, 12)).wrap()
  );

  Ok(())
}

#[test]
fn parse_parentheses() -> Result<()> {
  assert_eq!(
    parse_expr("(f) > bar")?,   // "()" need .group() and to be removed from the .span() item
    Operator::new(
      OperatorKind::GreaterThan,
      Group::new(
        Selector::new().metric("f").span((1, 2)).wrap()
      ).span((0, 3)).wrap(),
      Selector::new().metric("bar").span((6, 9)).wrap()
    ).span((3, 6)).wrap()
  );

  assert_eq!(
    parse_expr("(some)+(more)")?,
    Operator::new(
      OperatorKind::Add,
      Group::new(
        Selector::new().metric("some").span((1, 5)).wrap()
      ).span((0, 6)).wrap(),
      Group::new(
        Selector::new().metric("more").span((8, 12)).wrap()
      ).span((7, 13)).wrap(),
    ).span((6, 7)).wrap()
  );

  Ok(())
}

#[test]
fn parse_colon() -> Result<()> {
  assert_eq!(
    parse_expr("t::s and that")?,
    Operator::new(
      OperatorKind::And,
      Selector::new().metric("t::s").span((0, 4)).wrap(),
      Selector::new().metric("that").span((9, 13)).wrap()
    ).span((4, 9)).wrap()
  );

  assert_eq!(
    parse_expr("this:or:that:+:this:or:that")?,
    Operator::new(
      OperatorKind::Add,
      Selector::new().metric("this:or:that:").span((0, 13)).wrap(),
      Selector::new().metric(":this:or:that").span((14, 27)).wrap()
    ).span((13, 14)).wrap()
  );

  assert_eq!(
    parse_expr("this:+:that")?,
    Operator::new(
      OperatorKind::Add,
      Selector::new().metric("this:").span((0, 5)).wrap(),
      Selector::new().metric(":that").span((6, 11)).wrap()
    ).span((5, 6)).wrap()
  );

  Ok(())
}

#[test]
fn parse_missing_quotation() -> std::result::Result<(), ()> {
  match parse_expr(r#"hello{world="jupiter",type="gas}"#) {
    Err(_) => Ok(()),
    _ => Err(())
  }
}

#[test]
fn parse_missing_rhs() -> std::result::Result<(), ()> {
  match parse_expr(r#"hello{world="jupiter",type="gas"} !="#) {
    Err(_) => Ok(()),
    _ => Err(())
  }
}

#[test]
fn parse_trailing_comma() -> Result<()> {
  assert_eq!(
    parse_expr(r#"hello{world="jupiter",type="gas",}"#)?,
    Selector::new()
      .metric("hello")
      .label(Label::equal("world", "jupiter").span((6, 21)))
      .label(Label::equal("type", "gas").span((22, 32)))
      .span((0, 34))
      .wrap()
  );

  Ok(())
}

#[test]
fn parse_comment() -> Result<()> {
  assert_eq!(
    parse_expr(r#"
      foo # lorem
      +   # ipsum
      bar # dolor
    "#)?,
    Operator::new(
      OperatorKind::Add,
      Selector::new().metric("foo").span((7, 10)).wrap(),
      Selector::new().metric("bar").span((43, 46)).wrap()
    ).span((10, 43)).wrap()
  );

  Ok(())
}

#[test]
fn parse_weird_floats() -> Result<()> {
  // nan != nan, so gotta check in a roundabout way
  match parse_expr("NaN + 1")? {
    Expression::Operator(o) => match *o.lhs {
      Expression::Float(f) => assert!(f.is_nan(), "must be NaN"),
      _ => assert!(false, "must be float")
    },
    _ => assert!(false, "must be operator")
  }

  match parse_expr("+NaN + +1")? {
    Expression::Operator(o) => {
      match *o.lhs {
        Expression::Float(f) => assert!(f.is_nan(), "must be NaN"),
        _ => assert!(false, "must be float")
      }

      match *o.rhs {
        Expression::Float(f) => assert_eq!(f, 1.0),
        _ => assert!(false, "must be float")
      }
    },
    _ => assert!(false, "must be operator")
  }

  match parse_expr("Inf + -1")? {
    Expression::Operator(o) => {
      match *o.lhs {
        Expression::Float(f) => assert!(
          f.is_infinite() && f.is_sign_positive(),
          "must be +inf"
        ),
        _ => assert!(false, "must be float")
      }
      
      match *o.rhs {
        Expression::Float(f) => assert_eq!(f, -1.0),
        _ => assert!(false, "must be float")
      }
    },
    _ => assert!(false, "must be operator")
  }

  match parse_expr("+Inf + -.1")? {
    Expression::Operator(o) => {
      match *o.lhs {
        Expression::Float(f) => assert!(
          f.is_infinite() && f.is_sign_positive(),
          "must be +inf"
        ),
        _ => assert!(false, "must be float")
      }

      match *o.rhs {
        Expression::Float(f) => assert_eq!(f, -0.1),
        _ => assert!(false, "must be float")
      }
    },
    _ => assert!(false, "must be operator")
  }

  match parse_expr("-Inf + 1")? {
    Expression::Operator(o) => match *o.lhs {
      Expression::Float(f) => assert!(f.is_infinite() && f.is_sign_negative()),
      _ => assert!(false, "must be float")
    },
    _ => assert!(false, "must be operator")
  }

  Ok(())
}

#[test]
fn parse_bool_modifier() -> Result<()> {
  assert_eq!(
    parse_expr("foo > bool bar")?,
    BoolOperator::new(
      OperatorKind::GreaterThan,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((11, 14)).wrap()
    ).span((3, 11)).wrap()
  );

  assert_eq!(
    parse_expr("foo == bool bar")?,
    BoolOperator::new(
      OperatorKind::Equal,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((12, 15)).wrap()
    ).span((3, 12)).wrap()
  );

  assert_eq!(
    parse_expr("foo >= bool bar")?,
    BoolOperator::new(
      OperatorKind::GreaterThanEqual,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((12, 15)).wrap()
    ).span((3, 12)).wrap()
  );

  assert_eq!(
    parse_expr("foo <= bool bar")?,
    BoolOperator::new(
      OperatorKind::LessThanEqual,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((12, 15)).wrap()
    ).span((3, 12)).wrap()
  );

  assert_eq!(
    parse_expr("foo != bool bar")?,
    BoolOperator::new(
      OperatorKind::NotEqual,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((12, 15)).wrap()
    ).span((3, 12)).wrap()
  );

  assert_eq!(
    parse_expr("foo < bool bar")?,
    BoolOperator::new(
      OperatorKind::LessThan,
      Selector::new().metric("foo").span((0, 3)).wrap(),
      Selector::new().metric("bar").span((11, 14)).wrap()
    ).span((3, 11)).wrap()
  );

  Ok(())
}