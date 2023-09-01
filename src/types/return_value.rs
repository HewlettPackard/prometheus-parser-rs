// (C) Copyright 2019-2020 Hewlett Packard Enterprise Development LP

use std::collections::HashSet;

use super::expression::Expression;
use super::misc::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct UnknownCause {
    /// An explanation for why this Unknown was returned
    pub message: String,

    // would be nice to make this a ref, but we run into wrapping issues
    /// The expression that caused an Unknown to be returned
    pub expression: Expression,
}

/// A predicted return datatype
#[derive(Debug, PartialEq, Clone)]
pub enum ReturnKind {
    Unknown(Box<UnknownCause>),
    Scalar,
    String,
    InstantVector,
    RangeVector,
}

impl ReturnKind {
    pub fn unknown<S>(message: S, expression: Expression) -> Self
    where
        S: Into<String>,
    {
        ReturnKind::Unknown(Box::new(UnknownCause {
            message: message.into(),
            expression,
        }))
    }

    /// Returns true if this `ReturnKind` is a valid sub-expression of an
    /// operator, false if not.
    pub fn is_operator_valid(&self) -> bool {
        match self {
            ReturnKind::Scalar | ReturnKind::InstantVector => true,
            _ => false,
        }
    }

    pub fn is_scalar(&self) -> bool {
        match self {
            ReturnKind::Scalar => true,
            _ => false,
        }
    }
}

/// An operation on time series labels that might be performed by a particular
/// (sub-)expression.
///
/// An expression may perform more than one operation
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LabelSetOp {
    /// Convenience op that does nothing
    NoOp,

    /// Clears all labels
    Clear,

    /// Explicitly includes a set of labels in the output set
    Append(HashSet<String>),

    /// Explicitly removes a set of labels from the output set
    Remove(HashSet<String>),
}

impl LabelSetOp {
    pub fn noop(expression: Expression, span: Option<Span>) -> LabelSetOpTuple {
        LabelSetOpTuple {
            op: LabelSetOp::NoOp,
            expression,
            span,
        }
    }

    pub fn clear(expression: Expression, span: Option<Span>) -> LabelSetOpTuple {
        LabelSetOpTuple {
            op: LabelSetOp::Clear,
            expression,
            span,
        }
    }

    pub fn append(
        expression: Expression,
        span: Option<Span>,
        labels: HashSet<String>,
    ) -> LabelSetOpTuple {
        LabelSetOpTuple {
            op: LabelSetOp::Append(labels),
            expression,
            span,
        }
    }

    pub fn remove(
        expression: Expression,
        span: Option<Span>,
        labels: HashSet<String>,
    ) -> LabelSetOpTuple {
        LabelSetOpTuple {
            op: LabelSetOp::Remove(labels),
            expression,
            span,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelSetOpTuple {
    /// The label operation
    pub op: LabelSetOp,

    /// The expression that created this operation
    pub expression: Expression,

    /// A span for the particular clause that created this operation
    pub span: Option<Span>,
}

#[derive(Debug, PartialEq, Clone)]
/// A predicted return value from an expression
pub struct ReturnValue {
    pub kind: ReturnKind,

    /// A stack of operations applied to original source labels
    pub label_ops: Vec<LabelSetOpTuple>,
}

impl ReturnValue {
    pub fn unknown<S>(message: S, expression: Expression) -> Self
    where
        S: Into<String>,
    {
        ReturnValue {
            kind: ReturnKind::unknown(message, expression),
            label_ops: vec![],
        }
    }

    pub fn label_op(&mut self, op: LabelSetOpTuple) -> &mut Self {
        self.label_ops.push(op);

        self
    }

    /// Given a number of input label keys, returns a set of labels expected to be
    /// present on all returned time series
    ///
    /// Note that more labels may be returned than were provided in the input set;
    /// expressions that explicitly select for or group on labels will
    pub fn passthrough(&self, input_labels: &[&str]) -> HashSet<String> {
        let mut labels: HashSet<String> = HashSet::new();
        labels.extend(input_labels.iter().map(|l| (*l).to_string()));

        for set_op in &self.label_ops {
            match &set_op.op {
                LabelSetOp::NoOp => (),
                LabelSetOp::Clear => labels.clear(),
                LabelSetOp::Append(app) => {
                    labels = labels.union(&app).map(String::from).collect();
                }
                LabelSetOp::Remove(rem) => {
                    labels = labels.difference(&rem).map(String::from).collect();
                }
            }
        }

        labels
    }

    /// Determines the operation (plus expression and span) that caused the given
    /// label to be dropped. If the label is never dropped, returns `None`.
    ///
    /// This also accounts for expressions that drop a label and re-add it later,
    /// for instance with an aggregation clause.
    pub fn drops(&self, label: &str) -> Option<&LabelSetOpTuple> {
        let mut labels: HashSet<String> = HashSet::new();
        labels.insert(label.to_string());

        let mut ret_op = None;

        for set_op in &self.label_ops {
            match &set_op.op {
                LabelSetOp::NoOp => (),
                LabelSetOp::Clear => {
                    ret_op = Some(set_op);
                }
                LabelSetOp::Append(app) => {
                    // if this op re-adds a label, clear any previous op we thought
                    // might have removed it
                    if app.contains(label) {
                        ret_op = None;
                    }
                }
                LabelSetOp::Remove(rem) => {
                    // prefer any earlier op that already removed the label
                    if ret_op.is_none() && rem.contains(label) {
                        ret_op = Some(set_op);
                    }
                }
            }
        }

        ret_op
    }
}

pub(crate) fn strs_to_set(strs: &[&str]) -> HashSet<String> {
    let mut set: HashSet<String> = HashSet::new();
    for s in strs {
        set.insert((*s).to_string());
    }

    set
}
