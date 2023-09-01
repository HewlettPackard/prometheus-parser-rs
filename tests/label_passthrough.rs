// (C) Copyright 2019 Hewlett Packard Enterprise Development LP

extern crate prometheus_parser;

use maplit::hashset;
use prometheus_parser::*;

#[test]
fn passthrough_trivial_empty() -> Result<()> {
    assert_eq!(
        parse_expr("foo")?.return_value().passthrough(&[]),
        hashset! {}
    );

    Ok(())
}

#[test]
fn passthrough_trivial_label() -> Result<()> {
    assert_eq!(
        parse_expr(r#"foo{bar="baz"}"#)?
            .return_value()
            .passthrough(&[]),
        hashset! {"bar".to_string()}
    );

    Ok(())
}

#[test]
fn passthrough_trivial_user() -> Result<()> {
    assert_eq!(
        parse_expr("foo")?.return_value().passthrough(&["bar"]),
        hashset! {"bar".to_string()}
    );

    Ok(())
}

#[test]
fn passthrough_trivial_both() -> Result<()> {
    assert_eq!(
        parse_expr(r#"foo{bar="baz"}"#)?
            .return_value()
            .passthrough(&["qux"]),
        hashset! {"bar".to_string(), "qux".to_string()}
    );

    Ok(())
}

#[test]
fn passthrough_grouping() -> Result<()> {
    // non-aggregation functions don't clear labels...
    assert_eq!(
        parse_expr(r#"zxcv(foo{bar="a"})"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {"bar".to_string(), "baz".to_string()}
    );

    // ... but actual aggregations do
    assert_eq!(
        parse_expr(r#"sum(foo{bar="a"})"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {}
    );

    assert_eq!(
        parse_expr(r#"sum(foo{bar="a"}) by (baz)"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {"baz".to_string()}
    );

    assert_eq!(
        parse_expr(r#"sum(foo{bar="a"}) without (qux)"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {"bar".to_string(), "baz".to_string()}
    );

    Ok(())
}

#[test]
fn passthrough_grouping_operator() -> Result<()> {
    assert_eq!(
        parse_expr(r#"foo + bar"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {"baz".to_string()}
    );

    assert_eq!(
        parse_expr(r#"sum(foo) + bar"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {}
    );

    assert_eq!(
        parse_expr(r#"sum(foo) by (baz) + bar"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {"baz".to_string()}
    );

    Ok(())
}

#[test]
fn passthrough_matching() -> Result<()> {
    // these are likely impossible prometheus expressions (or at least, could
    // never return any results), but are concise enough tests
    assert_eq!(
        parse_expr(r#"foo{bar="a"} / on(foo) group_left bar{baz="b"}"#)?
            .return_value()
            .passthrough(&["qux"]),
        hashset! {"bar".to_string(), "qux".to_string()}
    );

    assert_eq!(
        parse_expr(r#"foo{bar="a"} / on(foo) group_right bar{baz="b"}"#)?
            .return_value()
            .passthrough(&["qux"]),
        hashset! {"baz".to_string(), "qux".to_string()}
    );

    assert_eq!(
        parse_expr(r#"foo{bar="a"} / on(foo) group_left(baz) bar{baz="b"}"#)?
            .return_value()
            .passthrough(&["qux"]),
        hashset! {"bar".to_string(), "baz".to_string(), "qux".to_string()}
    );

    Ok(())
}

#[test]
fn passthrough_special() -> Result<()> {
    // no arg scalar function
    assert_eq!(
        parse_expr(r#"time()"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {}
    );

    // weird 2nd arg functions
    // histogram_quantile also implies an `le` label
    assert_eq!(
        parse_expr(r#"histogram_quantile(0.9, rate(foo[5m]))"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {"baz".to_string(), "le".to_string()}
    );

    assert_eq!(
        parse_expr(r#"quantile_over_time(0.9, rate(foo[5m]))"#)?
            .return_value()
            .passthrough(&["baz"]),
        hashset! {"baz".to_string()}
    );

    // https://prometheus.io/docs/prometheus/latest/querying/functions/#label_join
    assert_eq!(
        parse_expr(
            r#"label_join(
      up{job="api-server",src1="a",src2="b",src3="c"},
      "foo", ",", "src1", "src2", "src3"
    )"#
        )?
        .return_value()
        .passthrough(&[]),
        hashset! {
          "job".to_string(), "src1".to_string(), "src2".to_string(),
          "src3".to_string(), "foo".to_string()
        }
    );

    // https://prometheus.io/docs/prometheus/latest/querying/functions/#label_replace
    assert_eq!(
        parse_expr(
            r#"label_replace(
      up{job="api-server",service="a:c"},
       "foo", "$1", "service", "(.*):.*"
    )"#
        )?
        .return_value()
        .passthrough(&[]),
        hashset! {
          "job".to_string(), "service".to_string(), "foo".to_string()
        }
    );

    Ok(())
}
