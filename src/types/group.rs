// (C) Copyright 2019 Hewlett Packard Enterprise Development LP

use std::fmt;

use super::expression::{BExpression, Expression};
use super::return_value::{ReturnKind, ReturnValue};
use super::misc::{Span, Subquery};

/// An expression explicitly grouped in parens
#[derive(Debug, PartialEq, Clone)]
pub struct Group {
  pub expression: BExpression,
  pub subquery: Option<Subquery>,
  pub span: Option<Span>
}

impl Group {
  pub fn new(expression: Expression) -> Self {
    Group {
      expression: Box::new(expression),
      subquery: None,
      span: None
    }
  }

  pub fn subquery(mut self, subquery: Subquery) -> Self {
    self.subquery = Some(subquery);
    self
  }

  pub fn clear_subquery(mut self) -> Self {
    self.subquery = None;
    self
  }

  pub fn span<S: Into<Span>>(mut self, span: S) -> Self {
    self.span = Some(span.into());
    self
  }

  /// Wraps this function in an Expression
  pub fn wrap(self) -> Expression {
    Expression::Group(self)
  }

  pub fn return_value(&self) -> ReturnValue {
    let mut ret = self.expression.return_value();

    if self.subquery.is_some() {
      ret.kind = match ret.kind {
        ReturnKind::InstantVector => ReturnKind::RangeVector,
        _ => ReturnKind::unknown(
          format!("subquery on inner expression returning {:?} is invalid", ret.kind),
          self.clone().wrap()
        )
      };
    }

    ret
  }
}

impl fmt::Display for Group {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "({})", self.expression)?;

    if let Some(subquery) = &self.subquery {
      write!(f, "{}", subquery)?;
    }

    Ok(())
  }
}
