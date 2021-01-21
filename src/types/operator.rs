// (C) Copyright 2019-2020 Hewlett Packard Enterprise Development LP

use std::fmt;

use super::expression::{BExpression, Expression};
use super::return_value::{LabelSetOp, ReturnKind, ReturnValue};
use super::misc::Span;

/// All legal binary operator types
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OperatorKind {
  Power,
  Multiply, Divide, Modulo,
  Add, Subtract,
  Equal, NotEqual,
  LessThan, LessThanEqual,
  GreaterThan, GreaterThanEqual,
  And, Unless, Or
}

impl OperatorKind {
  pub fn as_str(self) -> &'static str {
    use OperatorKind::*;
    match self {
      Power => "^",
      Multiply => "*",
      Divide => "/",
      Modulo => "%",
      Add => "+",
      Subtract => "-",
      Equal => "==",
      NotEqual => "!=",
      LessThan => "<",
      LessThanEqual => "<=",
      GreaterThan => ">",
      GreaterThanEqual => ">=",
      And => "and",
      Unless => "unless",
      Or => "or"
    }
  }
}

impl fmt::Display for OperatorKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

/// Matching group clause types (`group_left`, `group_right`)
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MatchingGroupOp {
  Left,
  Right
}

impl fmt::Display for MatchingGroupOp {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      MatchingGroupOp::Left => write!(f, "group_left"),
      MatchingGroupOp::Right => write!(f, "group_right")
    }
  }
}

/// A matching clause's nested grouping clause
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchingGroup {
  /// The matching group's operator type (left or right)
  pub op: MatchingGroupOp,

  /// A list of labels to copy to the opposite side of the group operator, i.e.
  /// group_left(foo) copies the label `foo` from the right hand side
  pub labels: Vec<String>,

  pub span: Option<Span>
}

impl MatchingGroup {
  pub fn new(op: MatchingGroupOp) -> Self {
    MatchingGroup {
      op,
      labels: vec![],
      span: None
    }
  }

  /// Creates a new MatchingGroup with the Left op
  pub fn left() -> Self {
    MatchingGroup::new(MatchingGroupOp::Left)
  }

  /// Creates a new MatchingGroup with the Right op
  pub fn right() -> Self {
    MatchingGroup::new(MatchingGroupOp::Right)
  }

  /// Replaces this Matching's operator
  pub fn op(mut self, op: MatchingGroupOp) -> Self {
    self.op = op;
    self
  }

  /// Adds a label key to this MatchingGroup
  pub fn label<S: Into<String>>(mut self, label: S) -> Self {
    self.labels.push(label.into());
    self
  }

  /// Replaces this MatchingGroup's labels with the given set
  pub fn labels(mut self, labels: &[&str]) -> Self {
    self.labels = labels.iter().map(|l| (*l).to_string()).collect();
    self
  }

  /// Clears this MatchingGroup's set of labels
  pub fn clear_labels(mut self) -> Self {
    self.labels.clear();
    self
  }

  pub fn span<S: Into<Span>>(mut self, span: S) -> Self {
    self.span = Some(span.into());
    self
  }
}

impl fmt::Display for MatchingGroup {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.op)?;

    if !self.labels.is_empty() {
      write!(f, "(")?;

      for (i, label) in self.labels.iter().enumerate() {
        if i > 0 {
          write!(f, ", ")?;
        }

        write!(f, "{}", label)?;
      }

      write!(f, ")")?;
    }

    Ok(())
  }
}

/// A matching clause type
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MatchingOp {
  On,
  Ignoring
}

impl fmt::Display for MatchingOp {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      MatchingOp::On => write!(f, "on"),
      MatchingOp::Ignoring => write!(f, "ignoring")
    }
  }
}

/// An operator matching clause
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Matching {
  pub op: MatchingOp,

  /// A list of labels to which the operator is applied
  pub labels: Vec<String>,

  /// An optional grouping clause for many-to-one and one-to-many vector matches
  pub group: Option<MatchingGroup>,

  pub span: Option<Span>
}

impl Matching {
  pub fn new(op: MatchingOp) -> Self {
    Matching {
      op,
      labels: vec![],
      group: None,
      span: None
    }
  }

  /// Creates a Matching cause with the On operator
  pub fn on() -> Self {
    Matching::new(MatchingOp::On)
  }

  /// Creates a Matching clause using the Ignoring operator
  pub fn ignoring() -> Self {
    Matching::new(MatchingOp::Ignoring)
  }

  /// Replaces this Matching's operator
  pub fn op(mut self, op: MatchingOp) -> Self {
    self.op = op;
    self
  }

  /// Adds a label key to this Matching
  pub fn label<S: Into<String>>(mut self, label: S) -> Self {
    self.labels.push(label.into());
    self
  }

  /// Replaces this Matching's labels with the given set
  pub fn labels(mut self, labels: &[&str]) -> Self {
    self.labels = labels.iter().map(|l| (*l).to_string()).collect();
    self
  }

  /// Clears this Matching's set of labels
  pub fn clear_labels(mut self) -> Self {
    self.labels.clear();
    self
  }

  /// Sets or replaces this Matching's group clause
  pub fn group(mut self, group: MatchingGroup) -> Self {
    self.group = Some(group);
    self
  }

  /// Clears this Matching's group clause
  pub fn clear_group(mut self) -> Self {
    self.group = None;
    self
  }

  pub fn span<S: Into<Span>>(mut self, span: S) -> Self {
    self.span = Some(span.into());
    self
  }
}

impl fmt::Display for Matching {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}(", self.op)?;

    for (i, label) in self.labels.iter().enumerate() {
      if i > 0 {
        write!(f, ", ")?;
      }

      write!(f, "{}", label)?;
    }

    write!(f, ")")?;

    if let Some(group) = &self.group {
      write!(f, " {}", group)?;
    }

    Ok(())
  }
}

/// A binary operator expression with left- and right-hand sides.
///
/// Operator expressions may optionally have a matching clause for more specific
/// vector matching behavior.
///
/// Note that operator precedence is accounted for at parse-time, so the
/// resulting tree (i.e. lhs/rhs expressions) should already account for
/// un-grouped expressions at the same syntax level.
#[derive(Debug, PartialEq, Clone)]
pub struct Operator {
  /// This Operator's function (multiply, divide, power, equals, etc)
  pub kind: OperatorKind,

  /// The left-hand-side expression
  pub lhs: BExpression,

  /// The right-hand-side expression
  pub rhs: BExpression,

  /// An optional matching clause for this operator (`on(...)`, `ignoring(...)`)
  pub matching: Option<Matching>,

  pub span: Option<Span>
}

impl Operator {
  pub fn new(kind: OperatorKind, lhs: Expression, rhs: Expression) -> Self {
    Operator {
      kind,
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
      matching: None,
      span: None
    }
  }

  /// Sets or replaces this Operator's Matching clause
  pub fn matching(mut self, matching: Matching) -> Self {
    self.matching = Some(matching);
    self
  }

  /// Clears this Operator's Matching clause, if any
  pub fn clear_matching(mut self) -> Self {
    self.matching = None;
    self
  }

  pub fn span<S: Into<Span>>(mut self, span: S) -> Self {
    self.span = Some(span.into());
    self
  }

  /// Wraps this Operator in an Expression
  pub fn wrap(self) -> Expression {
    Expression::Operator(self)
  }

  pub fn return_value(&self) -> ReturnValue {
    // note: largely based on the description from:
    // https://www.robustperception.io/using-group_left-to-calculate-label-proportions

    // binary operator exprs can only contain (and return) instant vectors
    let lhs_ret = self.lhs.return_value();
    let rhs_ret = self.rhs.return_value();

    // operators can only have instant vectors or scalars
    if !lhs_ret.kind.is_operator_valid() {
      return ReturnValue::unknown(
        format!("lhs return type ({:?}) is not valid in an operator", &lhs_ret.kind),
        self.clone().wrap()
      );
    }

    if !rhs_ret.kind.is_operator_valid() {
      return ReturnValue::unknown(
        format!("rhs return type ({:?}) is not valid in an operator", &rhs_ret.kind),
        self.clone().wrap()
      );
    }

    let kind;
    let mut label_ops;

    if lhs_ret.kind.is_scalar() && rhs_ret.kind.is_scalar() {
      // scalar / scalar = scalar, otherwise instant vector
      kind = ReturnKind::Scalar;
      label_ops = vec![LabelSetOp::clear(self.clone().wrap(), self.span)];
    } else if lhs_ret.kind.is_scalar() {
      // lhs is scalar, so pull labels from the rhs
      kind = ReturnKind::InstantVector;
      label_ops = rhs_ret.label_ops;
    } else if rhs_ret.kind.is_scalar() {
      // rhs is scalar, so pull labels from the lhs
      kind = ReturnKind::InstantVector;
      label_ops = lhs_ret.label_ops;
    } else {
      kind = ReturnKind::InstantVector;

      // neither side is scalar, so unless there's a matching clause with a
      // group_*, the choice is arbitrary
      // i.e. expressions without matching label sets will just return an empty
      // set of metrics
      
      // on/ignoring clauses don't affect labels themselves, but a group_* may
      if let Some(matching) = &self.matching {
        if let Some(group) = &matching.group {
          match &group.op {
            MatchingGroupOp::Left => label_ops = lhs_ret.label_ops,
            MatchingGroupOp::Right => label_ops = rhs_ret.label_ops
          };

          // any explicitly-specified labels are copied from the opposite side
          // we don't care about the value, but it does imply that the label
          // will exist in the output
          label_ops.push(LabelSetOp::append(
            self.clone().wrap(),
            group.span,
            group.labels.iter().cloned().collect(),
          ));
        } else {
          label_ops = lhs_ret.label_ops;
        }
      } else {
        label_ops = lhs_ret.label_ops;
      }
    };

    ReturnValue { kind, label_ops }
  }
}

impl fmt::Display for Operator {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} {}", self.lhs, self.kind)?;

    if let Some(matching) = &self.matching {
      write!(f, " {}", matching)?;
    }

    write!(f, " {}", self.rhs)?;

    Ok(())
  }
}

/// A binary operator with the "bool" modifier.
///
/// This is a subset of [Operator]s. With the "bool" modifier expressions act
/// as a boolean operator, returning a boolean value.
#[derive(Debug, PartialEq, Clone)]
pub struct BoolOperator {
    /// This Operator's function (multiply, divide, power, equals, etc)
    pub kind: OperatorKind,

    /// The left-hand-side expression
    pub lhs: BExpression,
  
    /// The right-hand-side expression
    pub rhs: BExpression,
  
    /// An optional matching clause for this operator (`on(...)`, `ignoring(...)`)
    pub matching: Option<Matching>,
  
    pub span: Option<Span>
}

impl BoolOperator {
  pub fn new(kind: OperatorKind, lhs: Expression, rhs: Expression) -> Self {
    Self {
      kind,
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
      matching: None,
      span: None
    }
  }

  /// Sets or replaces this Operator's Matching clause
  pub fn matching(mut self, matching: Matching) -> Self {
    self.matching = Some(matching);
    self
  }

  /// Clears this Operator's Matching clause, if any
  pub fn clear_matching(mut self) -> Self {
    self.matching = None;
    self
  }

  pub fn span<S: Into<Span>>(mut self, span: S) -> Self {
    self.span = Some(span.into());
    self
  }

  /// Wraps this Operator in an Expression
  pub fn wrap(self) -> Expression {
    Expression::BoolOperator(self)
  }

  pub fn return_value(&self) -> ReturnValue {
    // note: largely based on the description from:
    // https://www.robustperception.io/using-group_left-to-calculate-label-proportions

    // binary operator exprs can only contain (and return) instant vectors
    let lhs_ret = self.lhs.return_value();
    let rhs_ret = self.rhs.return_value();

    // operators can only have instant vectors or scalars
    if !lhs_ret.kind.is_operator_valid() {
      return ReturnValue::unknown(
        format!("lhs return type ({:?}) is not valid in an operator", &lhs_ret.kind),
        self.clone().wrap()
      );
    }

    if !rhs_ret.kind.is_operator_valid() {
      return ReturnValue::unknown(
        format!("rhs return type ({:?}) is not valid in an operator", &rhs_ret.kind),
        self.clone().wrap()
      );
    }

    let mut label_ops;

    if lhs_ret.kind.is_scalar() && rhs_ret.kind.is_scalar() {
      label_ops = vec![LabelSetOp::clear(self.clone().wrap(), self.span)];
    } else if lhs_ret.kind.is_scalar() {
      // lhs is scalar, so pull labels from the rhs
      label_ops = rhs_ret.label_ops;
    } else if rhs_ret.kind.is_scalar() {
      // rhs is scalar, so pull labels from the lhs
      label_ops = lhs_ret.label_ops;
    } else {
      // neither side is scalar, so unless there's a matching clause with a
      // group_*, the choice is arbitrary
      // i.e. expressions without matching label sets will just return an empty
      // set of metrics1
      
      // on/ignoring clauses don't affect labels themselves, but a group_* may
      if let Some(matching) = &self.matching {
        if let Some(group) = &matching.group {
          match &group.op {
            MatchingGroupOp::Left => label_ops = lhs_ret.label_ops,
            MatchingGroupOp::Right => label_ops = rhs_ret.label_ops
          };

          // any explicitly-specified labels are copied from the opposite side
          // we don't care about the value, but it does imply that the label
          // will exist in the output
          label_ops.push(LabelSetOp::append(
            self.clone().wrap(),
            group.span,
            group.labels.iter().cloned().collect(),
          ));
        } else {
          label_ops = lhs_ret.label_ops;
        }
      } else {
        label_ops = lhs_ret.label_ops;
      }
    };

    ReturnValue { kind: ReturnKind::Scalar, label_ops }
  }
}

impl fmt::Display for BoolOperator {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} {} bool", self.lhs, self.kind)?;

    if let Some(matching) = &self.matching {
      write!(f, " {}", matching)?;
    }

    write!(f, " {}", self.rhs)?;

    Ok(())
  }
}