// (C) Copyright 2019-2020 Hewlett Packard Enterprise Development LP

use std::f64;

use lazy_static::lazy_static;
use pest::prec_climber as pcl;
use pest::prec_climber::PrecClimber;
use pest_consume::{match_nodes, Error, Parser};
use enquote::unquote;

use crate::types::*;

pub type Result<T> = std::result::Result<T, Error<Rule>>;
pub(crate) type Node<'i> = pest_consume::Node<'i, Rule, ()>;

lazy_static! {
  static ref PRECCLIMBER: PrecClimber<Rule> = PrecClimber::new(
    vec![
      pcl::Operator::new(Rule::op_or, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_unless, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_and, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_greater_than, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_greater_than_equal, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_less_than, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_less_than_equal, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_not_equal, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_equal, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_subtract, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_add, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_modulo, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_divide, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_multiply, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_power, pcl::Assoc::Right),

      pcl::Operator::new(Rule::op_greater_than_bool, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_greater_than_equal_bool, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_less_than_bool, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_less_than_equal_bool, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_not_equal_bool, pcl::Assoc::Left),
      pcl::Operator::new(Rule::op_equal_bool, pcl::Assoc::Left),
    ]
  );
}

/// An intermediate struct for selectors with a named metric.
/// Label-only selectors may evaluate immediately to a Selector
struct MetricSelectorIntermediate {
  metric: String,
  labels: Vec<Label>
}

struct MatchingIntermediate {
  op: MatchingOp,
  labels: Vec<String>
}

#[derive(Parser)]
#[grammar = "prometheus.pest"]
struct PrometheusParser;

#[allow(clippy::int_plus_one)]
#[pest_consume::parser]
impl PrometheusParser {
  fn EOI(_input: Node) -> Result<()> {
    Ok(())
  }

  fn signed_float(input: Node) -> Result<Expression> {
    let val = match input.as_str().trim() {
      "Inf" | "+Inf" => f64::INFINITY,
      "-Inf" => f64::NEG_INFINITY,
      "NaN" | "+NaN" | "-NaN" => f64::NAN,
      f => f.parse().map_err(|e| input.error(e))?
    };

    Ok(Expression::Float(val))
  }

  fn string(input: Node) -> Result<Expression> {
    let s = unquote(input.as_str()).map_err(|e| input.error(e))?;

    Ok(Expression::String(s))
  }

  fn label_key(input: Node) -> Result<String> {
    Ok(input.as_str().trim().to_string())
  }

  fn label_value(input: Node) -> Result<String> {
    unquote(input.as_str()).map_err(|e| input.error(e))
  }

  fn group(input: Node) -> Result<Expression> {
    let span = Span::from_node(&input);
    Ok(match_nodes!(input.into_children();
      [expression(e)] => Group::new(e).span(span).wrap(),
      [expression(e), subquery(s)] => Group::new(e)
        .subquery(s)
        .span(span)
        .wrap()
    ))
  }

  fn selector_metric(input: Node) -> Result<String> {
    Ok(input.as_str().trim().to_string())
  }

  fn label_op_equal(_input: Node) -> Result<LabelOp> {
    Ok(LabelOp::Equal)
  }

  fn label_op_not_equal(_input: Node) -> Result<LabelOp> {
    Ok(LabelOp::NotEqual)
  }

  fn label_op_regex_equal(_input: Node) -> Result<LabelOp> {
    Ok(LabelOp::RegexEqual)
  }

  fn label_op_regex_not_equal(_input: Node) -> Result<LabelOp> {
    Ok(LabelOp::RegexNotEqual)
  }

  fn label_operator(input: Node) -> Result<LabelOp> {
    Ok(match_nodes!(input.into_children();
      [label_op_equal(op)] => op,
      [label_op_not_equal(op)] => op,
      [label_op_regex_equal(op)] => op,
      [label_op_regex_not_equal(op)] => op,
    ))
  }

  fn label(input: Node) -> Result<Label> {
    let span = Some(Span::from_node(&input));
    Ok(match_nodes!(input.into_children();
      [label_key(key), label_operator(op), label_value(value)] => Label {
        op, key, value, span
      }
    ))
  }

  fn selector_label(input: Node) -> Result<Vec<Label>> {
    Ok(match_nodes!(input.into_children();
      [label(l)..] => l.collect()
    ))
  }

  fn duration_value(input: Node) -> Result<u64> {
    Ok(input.as_str().trim().parse().map_err(|e| input.error(e))?)
  }

  fn duration_unit(input: Node) -> Result<String> {
    Ok(input.as_str().trim().to_string())
  }

  fn subquery_duration(input: Node) -> Result<PromDuration> {
    Ok(match_nodes!(input.children();
      [duration_value(val), duration_unit(unit)] => {
        PromDuration::from_pair(&unit, val).map_err(|e| input.error(e))?
      }
    ))
  }

  fn subquery(input: Node) -> Result<Subquery> {
    let span = Some(Span::from_node(&input));
    Ok(match_nodes!(input.children();
      [subquery_duration(range)] => Subquery {
        range, resolution: None, span
      },
      [subquery_duration(range), subquery_duration(resolution)] => Subquery {
        range, resolution: Some(resolution), span
      }
    ))
  }

  fn selector_range(input: Node) -> Result<PromDuration> {
    Ok(match_nodes!(input.children();
      [duration_value(val), duration_unit(unit)] => {
        PromDuration::from_pair(&unit, val).map_err(|e| input.error(e))?
      }
    ))
  }

  fn offset(input: Node) -> Result<PromDuration> {
    Ok(match_nodes!(input.children();
      [duration_value(val), duration_unit(unit)] => {
        PromDuration::from_pair(&unit, val).map_err(|e| input.error(e))?
      }
    ))
  }

  fn metric_selector(input: Node) -> Result<MetricSelectorIntermediate> {
    Ok(match_nodes!(input.into_children();
      [selector_metric(metric)] => MetricSelectorIntermediate {
        metric, labels: vec![]
      },
      [selector_metric(metric), selector_label(labels)] => {
        MetricSelectorIntermediate {
          metric, labels
        }
      }
    ))
  }

  fn selector(input: Node) -> Result<Expression> {
    let span = Some(Span::from_node(&input));

    let mut metric = None;
    let mut labels = None;
    let mut range = None;
    let mut offset = None;
    let mut subquery = None;

    for child in input.into_children() {
      match child.as_rule() {
        Rule::metric_selector => {
          let intermediate = PrometheusParser::metric_selector(child)?;
          metric = Some(intermediate.metric);
          labels = Some(intermediate.labels);
        },
        Rule::selector_label => {
          labels = Some(PrometheusParser::selector_label(child)?);
        },
        Rule::selector_range => {
          range = Some(PrometheusParser::selector_range(child)?);
        },
        Rule::offset => {
          offset = Some(PrometheusParser::offset(child)?);
        },
        Rule::subquery => {
          subquery = Some(PrometheusParser::subquery(child)?);
        },
        r => return Err(child.error(format!(
          "invalid rule {:?} in selector", r
        )))
      }
    }

    let labels = labels.unwrap_or_else(|| vec![]);

    Ok(Expression::Selector(Selector {
      metric,
      labels,
      range,
      offset,
      subquery,
      span
    }))
  }

  fn function_name(input: Node) -> Result<String> {
    Ok(input.as_str().trim().to_string())
  }

  fn function_agg_op_by(_input: Node) -> Result<AggregationOp> {
    Ok(AggregationOp::By)
  }

  fn function_agg_op_without(_input: Node) -> Result<AggregationOp> {
    Ok(AggregationOp::Without)
  }

  fn function_agg_op(input: Node) -> Result<AggregationOp> {
    Ok(match_nodes!(input.into_children();
      [function_agg_op_by(o)] => o,
      [function_agg_op_without(o)] => o
    ))
  }

  fn function_agg(input: Node) -> Result<Aggregation> {
    let span = Some(Span::from_node(&input));

    Ok(match_nodes!(input.into_children();
      [function_agg_op(op), label_key(labels)..] => Aggregation {
        op,
        labels: labels.collect(),
        span
      }
    ))
  }

  fn function(input: Node) -> Result<Expression> {
    let span = Some(Span::from_node(&input));

    let mut name = None;
    let mut args = Vec::new();
    let mut aggregation = None;
    let mut subquery = None;

    // match_nodes gets unruly with optional aggregations (2 forms) and
    // subqueries, so break out into a manual conversion
    for child in input.children() {
      match child.as_rule() {
        Rule::function_name => name = Some(PrometheusParser::function_name(child)?),
        Rule::expression => args.push(Box::new(PrometheusParser::expression(child)?)),
        Rule::function_agg => aggregation = Some(PrometheusParser::function_agg(child)?),
        Rule::subquery => subquery = Some(PrometheusParser::subquery(child)?),
        r => return Err(child.error(format!(
          "invalid rule {:?} in function", r
        )))
      }
    }

    let name = match name {
      Some(name) => name,
      None => return Err(input.error("function name is required"))
    };

    Ok(Expression::Function(Function {
      name,
      args,
      aggregation,
      subquery,
      span
    }))
  }

  fn matching_on(_input: Node) -> Result<MatchingOp> {
    Ok(MatchingOp::On)
  }

  fn matching_ignoring(_input: Node) -> Result<MatchingOp> {
    Ok(MatchingOp::Ignoring)
  }

  fn matching(input: Node) -> Result<MatchingIntermediate> {
    Ok(match_nodes!(input.into_children();
      [matching_on(op), label_key(labels)..] => MatchingIntermediate {
        op, labels: labels.collect()
      },
      [matching_ignoring(op), label_key(labels)..] => MatchingIntermediate {
        op, labels: labels.collect()
      }
    ))
  }

  fn matching_group_left(_input: Node) -> Result<MatchingGroupOp> {
    Ok(MatchingGroupOp::Left)
  }

  fn matching_group_right(_input: Node) -> Result<MatchingGroupOp> {
    Ok(MatchingGroupOp::Right)
  }

  fn matching_group(input: Node) -> Result<MatchingGroup> {
    let span = Some(Span::from_node(&input));

    Ok(match_nodes!(input.into_children();
      [matching_group_left(op), label_key(labels)..] => MatchingGroup {
        op, labels: labels.collect(), span
      },
      [matching_group_right(op), label_key(labels)..] => MatchingGroup {
        op, labels: labels.collect(), span
      }
    ))
  }

  fn matching_expression(input: Node) -> Result<Matching> {
    let span = Some(Span::from_node(&input));

    Ok(match_nodes!(input.into_children();
      [matching(m)] => Matching {
        op: m.op,
        labels: m.labels,

        group: None,

        span
      },
      [matching(m), matching_group(g)] => Matching {
        op: m.op,
        labels: m.labels,

        group: Some(g),

        span
      }
    ))
  }

  #[prec_climb(expression_term, PRECCLIMBER)]
  fn expression(lhs: Expression, op: Node, rhs: Expression) -> Result<Expression> {
    let span = Some(Span::from_node(&op));

    use Rule::*;
    let mut is_bool_op = false;
    let kind = match op.as_rule() {
      op_or => OperatorKind::Or,
      op_unless => OperatorKind::Unless,
      op_and => OperatorKind::And,
      op_greater_than => OperatorKind::GreaterThan,
      op_greater_than_equal => OperatorKind::GreaterThanEqual,
      op_less_than => OperatorKind::LessThan,
      op_less_than_equal => OperatorKind::LessThanEqual,
      op_not_equal => OperatorKind::NotEqual,
      op_equal => OperatorKind::Equal,
      op_subtract => OperatorKind::Subtract,
      op_add => OperatorKind::Add,
      op_modulo => OperatorKind::Modulo,
      op_divide => OperatorKind::Divide,
      op_multiply => OperatorKind::Multiply,
      op_power => OperatorKind::Power,
      op_greater_than_bool => {
        is_bool_op = true;
        OperatorKind::GreaterThan
      },
      op_greater_than_equal_bool => {
        is_bool_op = true;
        OperatorKind::GreaterThanEqual
      },
      op_less_than_bool => {
        is_bool_op = true;
        OperatorKind::LessThan
      },
      op_less_than_equal_bool =>
      {is_bool_op = true;
        OperatorKind::LessThanEqual
      },
      op_not_equal_bool => {
        is_bool_op = true;
        OperatorKind::NotEqual
      },
      op_equal_bool => {
        is_bool_op = true;
        OperatorKind::Equal
      },
      r => return Err(op.error(format!("rule {:?} isn't an operator", r)))
    };

    // gotta break out of pest_consume slightly for this...
    let mut matching_clause = None;
    for child in op.into_children() {
      if let Rule::matching_expression = child.as_rule() {
        matching_clause = Some(PrometheusParser::matching_expression(child)?);
        break;
      }
    }

    if is_bool_op {
      Ok(Expression::BoolOperator(BoolOperator {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),

        matching: matching_clause,

        span
      }))
    } else {
      Ok(Expression::Operator(Operator {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),

        matching: matching_clause,

        span
      }))
    }
  }

  fn expression_term(input: Node) -> Result<Expression> {
    Ok(match_nodes!(input.into_children();
      [signed_float(f)] => f,
      [string(s)] => s,
      [group(g)] => g,
      [function(f)] => f,
      [selector(s)] => s
    ))
  }

  fn prometheus(input: Node) -> Result<Expression> {
    Ok(match_nodes!(input.into_children();
      [expression(e), EOI(_)] => e,
    ))
  }

}

/// Parses a Prometheus expression into a syntax tree.
///
/// # Examples 
///
/// ```
/// use prometheus_parser::*;
///
/// let ast = parse_expr("foo > bar").unwrap();
/// 
/// assert_eq!(ast, Operator::new(
///   OperatorKind::GreaterThan,
///   Selector::new().metric("foo").span((0, 3)).wrap(),
///   Selector::new().metric("bar").span((6, 9)).wrap()
/// ).span((3, 6)).wrap());
/// ```
pub fn parse_expr(expr: &str) -> Result<Expression> {
  let inputs = PrometheusParser::parse(Rule::prometheus, expr)?;
  let input = inputs.single()?;

  let expr = PrometheusParser::prometheus(input)?;

  Ok(expr)
}
