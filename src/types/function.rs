// (C) Copyright 2019-2020 Hewlett Packard Enterprise Development LP

use std::collections::HashSet;
use std::fmt;
use std::iter::FromIterator;

use super::expression::{BExpression, Expression};
use super::misc::{Span, Subquery};
use super::return_value::{strs_to_set, LabelSetOp, ReturnKind, ReturnValue};

/// Aggregation operator types, namely "by" and "without"
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AggregationOp {
    By,
    Without,
}

impl fmt::Display for AggregationOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AggregationOp::By => write!(f, "by"),
            AggregationOp::Without => write!(f, "without"),
        }
    }
}

/// A function aggregation clause.
///
/// Not all functions can be aggregated, but this library currently does not
/// attempt to validate this sort of usage.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Aggregation {
    pub op: AggregationOp,
    pub labels: Vec<String>,

    pub span: Option<Span>,
}

impl Aggregation {
    pub fn new(op: AggregationOp) -> Self {
        Aggregation {
            op,
            labels: vec![],
            span: None,
        }
    }

    pub fn by() -> Self {
        Aggregation::new(AggregationOp::By)
    }

    pub fn without() -> Self {
        Aggregation::new(AggregationOp::Without)
    }

    /// Replaces this Aggregation's operator
    pub fn op(mut self, op: AggregationOp) -> Self {
        self.op = op;
        self
    }

    /// Adds a label key to this Aggregation
    pub fn label<S: Into<String>>(mut self, label: S) -> Self {
        self.labels.push(label.into());
        self
    }

    /// Replaces this Aggregation's labels with the given set
    pub fn labels(mut self, labels: &[&str]) -> Self {
        self.labels = labels.iter().map(|l| (*l).to_string()).collect();
        self
    }

    /// Clear this Matching's set of labels
    pub fn clear_labels(mut self) -> Self {
        self.labels.clear();
        self
    }

    pub fn span<S: Into<Span>>(mut self, span: S) -> Self {
        self.span = Some(span.into());
        self
    }
}

impl fmt::Display for Aggregation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.op)?;

        if !self.labels.is_empty() {
            write!(f, " (")?;

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

/// A function call.
///
/// Note that function names, arguments, argument types, and return types are
/// not validated.
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<BExpression>,

    pub aggregation: Option<Aggregation>,

    pub subquery: Option<Subquery>,

    pub span: Option<Span>,
}

/// Given a Prometheus function name, returns the argument index that determines
/// the set of resulting labels
fn arg_index_for_function(name: &str) -> Option<usize> {
    Some(match name.to_lowercase().as_str() {
        "time" => return None,

        "histogram_quantile" => 1,
        "quantile_over_time" => 1,

        // almost all functions use the first arg (often by only accepting 1 arg)
        _ => 0,
    })
}

/// Determines if a given function converts a range vector to an instance vector
///
/// Note that `_over_time` functions do not affect labels, unlike their regular
/// counterparts
fn is_aggregation_over_time(name: &str) -> bool {
    match name.to_lowercase().as_str() {
        // all the _over_time functions ...
        "avg_over_time" | "min_over_time" | "max_over_time" => true,
        "sum_over_time" | "count_over_time" | "quantile_over_time" => true,
        "stddev_over_time" | "stdvar_over_time" => true,

        // and also a few more
        "delta" | "deriv" | "idelta" | "increase" | "predict_linear" => true,
        "irate" | "rate" | "resets" => true,

        _ => false,
    }
}

/// Determines if a given function is an aggregation function
fn is_aggregation(name: &str) -> bool {
    match name.to_lowercase().as_str() {
        "sum" | "min" | "max" | "avg" | "stddev" | "stdvar" | "count" => true,
        "count_values" | "bottomk" | "topk" | "quantile" => true,
        _ => false,
    }
}

impl Function {
    pub fn new<S: Into<String>>(name: S) -> Function {
        Function {
            name: name.into(),
            args: vec![],
            aggregation: None,
            subquery: None,
            span: None,
        }
    }

    /// Replaces this Function's name with the given string
    ///
    /// Note that the new name is not validated.
    pub fn name<S: Into<String>>(mut self, name: S) -> Self {
        self.name = name.into();
        self
    }

    /// Adds the given expression as new argument to this function
    pub fn arg(mut self, arg: Expression) -> Self {
        self.args.push(Box::new(arg));
        self
    }

    /// Sets this Function's aggregation clause
    pub fn aggregation(mut self, aggregation: Aggregation) -> Self {
        self.aggregation = Some(aggregation);
        self
    }

    /// Clears this Function's aggregation clause, if any
    pub fn clear_aggregation(mut self) -> Self {
        self.aggregation = None;
        self
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
        Expression::Function(self)
    }

    pub fn return_value(&self) -> ReturnValue {
        // functions generally pass through labels of one of their arguments, with
        // some exceptions

        // determine the arg to pass through
        let labels_arg_index = match arg_index_for_function(&self.name) {
            Some(index) => index,

            // only `time(...)` accepts no arguments and always returns a scalar
            None => {
                return ReturnValue {
                    kind: ReturnKind::Scalar,
                    label_ops: vec![LabelSetOp::clear(self.clone().wrap(), self.span)],
                }
            }
        };

        // if the function call is invalid, bail with unknowns
        let labels_arg = match self.args.get(labels_arg_index) {
            Some(arg) => arg,
            None => {
                return ReturnValue {
                    kind: ReturnKind::unknown(
                        format!(
                            "function call {}(...) is missing a required argument at index {}",
                            self.name, labels_arg_index
                        ),
                        self.clone().wrap(),
                    ),

                    // we can't predict what will happen with labels, but we can at least
                    // try to output _something_
                    label_ops: vec![],
                };
            }
        };

        let arg_return = labels_arg.return_value();

        let mut kind = arg_return.kind;
        let mut label_ops = arg_return.label_ops;

        if is_aggregation_over_time(&self.name) {
            if let ReturnKind::RangeVector = kind {
                kind = ReturnKind::InstantVector;
            } else {
                // invalid arg
                kind = ReturnKind::unknown(
                    format!(
                        "aggregation over time is not valid with expression returning {:?}",
                        kind
                    ),
                    // show the label arg as the cause
                    // doesn't follow the usual pattern of showing the parent, but would
                    // otherwise be ambiguous with multiple args
                    *labels_arg.clone(),
                );
            }
        }

        // aggregation functions reset labels unless they have an aggregation clause
        // (handled below)
        let is_agg = is_aggregation(&self.name);

        // aggregations turn range vectors into instant vectors
        if let Some(agg) = &self.aggregation {
            match agg.op {
                AggregationOp::By => {
                    // by (...) resets labels to only those in the by clause
                    label_ops.push(LabelSetOp::clear(self.clone().wrap(), agg.span));
                    label_ops.push(LabelSetOp::append(
                        self.clone().wrap(),
                        agg.span,
                        HashSet::from_iter(agg.labels.iter().cloned()),
                    ));
                }
                AggregationOp::Without => label_ops.push(LabelSetOp::remove(
                    self.clone().wrap(),
                    agg.span,
                    HashSet::from_iter(agg.labels.iter().cloned()),
                )),
            }
        } else if is_agg {
            // an aggregation function with no aggregation clause clears all labels
            label_ops.push(LabelSetOp::clear(self.clone().wrap(), self.span));
        }

        // handle special cases with functions that mutate labels
        match self.name.to_lowercase().as_str() {
            // creats a new label with contents of others, leaves them intact
            "label_join" => {
                if let Some(expr) = self.args.get(1) {
                    if let Some(s) = expr.as_str() {
                        label_ops.push(LabelSetOp::append(
                            self.clone().wrap(),
                            self.span,
                            strs_to_set(&[s]),
                        ));
                    }
                }
            }

            // note: does not remove src_label
            "label_replace" => {
                if let Some(expr) = self.args.get(1) {
                    if let Some(s) = expr.as_str() {
                        label_ops.push(LabelSetOp::append(
                            self.clone().wrap(),
                            self.span,
                            strs_to_set(&[s]),
                        ));
                    }
                }
            }

            // per docs, input vector must have an `le` label
            "histogram_quantile" => label_ops.push(LabelSetOp::append(
                self.clone().wrap(),
                self.span,
                strs_to_set(&["le"]),
            )),

            // "vector" => // no-op, its input expr is a scalar anyway
            _ => (),
        }

        // subqueries turn instant vectors into ranges
        if self.subquery.is_some() {
            kind = match kind {
                ReturnKind::InstantVector => ReturnKind::RangeVector,
                _ => ReturnKind::unknown(
                    format!(
                        "subquery on inner expression returning {:?} is invalid",
                        kind
                    ),
                    self.clone().wrap(),
                ),
            };
        }

        ReturnValue { kind, label_ops }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.name)?;

        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            if let Expression::String(s) = &**arg {
                write!(f, "\"{}\"", s)?
            } else {
                write!(f, "{}", arg)?
            };
        }

        write!(f, ")")?;

        if let Some(agg) = &self.aggregation {
            write!(f, " {}", agg)?;
        }

        if let Some(subquery) = &self.subquery {
            write!(f, "{}", subquery)?;
        }

        Ok(())
    }
}
