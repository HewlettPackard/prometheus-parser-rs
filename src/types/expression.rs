// (C) Copyright 2019-2020 Hewlett Packard Enterprise Development LP

use std::fmt;

use super::function::Function;
use super::group::Group;
use super::operator::{Operator, BoolOperator};
use super::return_value::{LabelSetOp, ReturnKind, ReturnValue};
use super::selector::Selector;

/// A root expression node.
///
/// These are all valid root expression types.
#[derive(PartialEq, Clone)]
pub enum Expression {
  /// A single scalar float
  Float(f64),

  /// A single scalar string.
  ///
  /// Prometheus' docs claim strings aren't currently implemented, but they're
  /// valid as function arguments.
  String(String),

  /// A metric selector
  Selector(Selector),

  /// A grouped expression wrapped in parentheses
  Group(Group),

  /// A function call
  Function(Function),

  /// A binary operator expression
  Operator(Operator),

  /// A binary operator expression that returns a boolean value
  BoolOperator(BoolOperator)
}

// For handling groups when testing
impl Expression {
  /// Wraps this Expression in a Group, consuming this Expression but returning
  /// an owned Group.
  pub fn group(self) -> Expression {
    Group::new(self).wrap()
  }

  /// Determines a predicted `ReturnValue` for this Expression. A `ReturnValue`
  /// includes a predicted data type and a set of label operations that may
  /// affect which labels are returned.
  pub fn return_value(&self) -> ReturnValue {
    match self {
      Expression::Float(_) => ReturnValue {
        kind: ReturnKind::Scalar,
        label_ops: vec![LabelSetOp::clear(self.clone(), None)]
      },
      Expression::String(_) => ReturnValue {
        kind: ReturnKind::String,
        label_ops: vec![LabelSetOp::clear(self.clone(), None)]
      },
      Expression::Selector(s) => s.return_value(),
      Expression::Group(g) => g.return_value(),
      Expression::Function(f) => f.return_value(),
      Expression::Operator(o) => o.return_value(),
      Expression::BoolOperator(bo) => bo.return_value(),
    }
  }

  /// If this Expression is a Float, returns its value. Otherwise, returns None.
  pub fn as_f64(&self) -> Option<f64> {
    if let Expression::Float(f) = self {
      Some(*f)
    } else {
      None
    }
  }

  /// If this Expression is a String, returns its value. Otherwise, returns
  /// None.
  pub fn as_str(&self) -> Option<&str> {
    if let Expression::String(s) = self {
      Some(s.as_str())
    } else {
      None
    }
  }
}

impl fmt::Debug for Expression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    // don't bother formatting the Expression enum itself, this just creates
    // tons of whitespace if we pretty-print
    // this makes the output significantly more readable, if a bit misleading

    // there's gotta be a better way to pass through flags...
    if f.alternate() {
      match self {
        Expression::Float(val) => write!(f, "{:#?}", val),
        Expression::String(val) => write!(f, "{:#?}", val),
        Expression::Selector(val) => write!(f, "{:#?}", val),
        Expression::Group(val) => write!(f, "{:#?}", val),
        Expression::Function(val) => write!(f, "{:#?}", val),
        Expression::Operator(val) => write!(f, "{:#?}", val),
        Expression::BoolOperator(val) => write!(f, "{:#?}", val),
      }
    } else {
      match self {
        Expression::Float(val) => write!(f, "{:?}", val),
        Expression::String(val) => write!(f, "{:?}", val),
        Expression::Selector(val) => write!(f, "{:?}", val),
        Expression::Group(val) => write!(f, "{:?}", val),
        Expression::Function(val) => write!(f, "{:?}", val),
        Expression::Operator(val) => write!(f, "{:?}", val),
        Expression::BoolOperator(val) => write!(f, "{:#?}", val),
      }
    }
  }
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expression::Float(val) => write!(f, "{}", val),
      Expression::String(val) => write!(f, "{}", val),
      Expression::Selector(val) => write!(f, "{}", val),
      Expression::Group(val) => write!(f, "{}", val),
      Expression::Function(val) => write!(f, "{}", val),
      Expression::Operator(val) => write!(f, "{}", val),
      Expression::BoolOperator(val) => write!(f, "{}", val),
    }
  }
}

pub type BExpression = Box<Expression>;
