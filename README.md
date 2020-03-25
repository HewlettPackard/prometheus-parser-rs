# prometheus-parser-rs

prometheus-parser parses arbitrary Prometheus expressions into a syntax tree
appropriate for syntax checking, linting, and general static analysis. It does
not depend on a Prometheus server and works entirely offline.

This library is implemented in pure Rust (using [pest]) with no knowledge of
the underlying Prometheus server implementation. It's been validated against
the public Prometheus syntax documentation and various publicly available alert
collections (e.g. [kubernetes-mixin]).

Note that this crate doesn't try to _evaluate_ Prometheus expressions; consider
using [`promtool`'s unit testing][unit] for this purpose.

[pest]: https://pest.rs/
[unit]: https://prometheus.io/docs/prometheus/latest/configuration/unit_testing_rules/
[kubernetes-mixin]: https://github.com/kubernetes-monitoring/kubernetes-mixin

## Use cases

This library can be used to implement custom linting and static analysis tools,
for example:

 * Offline syntax checking with useful error messages (thanks to [pest])
 * Automatic code formatting
 * Various AST inspections:
   * Ensuring expressions don't drop required labels for e.g. alert routing
     (namespace, service, etc)
   * Ensuring expressions don't make common mistakes that can cause spurious
     alerts, e.g. division by zero with counters

## Limitations

 * It doesn't try to evaluate expressions at all
 * It doesn't validate function calls or argument types, so `foo(bar, baz, qux)`
   is considered at least _syntactically_ valid. Each function argument still
   needs to parse to a real expression, though.
   * The `return_value()` utility will raise soft errors on certain type errors
     (via `ReturnKind::Unknown`)
   * Note that Prometheus performs some extra query validation based on runtime
     data and will return errors on e.g. label mismatches that we can't catch.
 * It properly parses most obscure elements of Prometheus syntax, however it's
   not entirely lossless; equivalent expressions (e.g. `sum by (a) (foo{})` and
   `sum (foo) by (a)`) will be evaluated identically, so expect minor output
   differences if you use this to implement a formatter.
 * It probably doesn't parse entirely the same as the real Prometheus parser,
   though care was taken to match it as closely as possible. That is, this
   parser may consider expressions valid that Prometheus won't, or vice versa.
   Please consider filing bugs for any differences.

## Return value prediction

The library has a utility for predicting the return types of expressions based
only on the syntax tree. This may be useful for rough static analysis.

 * `Expression::return_value()` recursively computes an expected return type
   (scalar, instant vector, range vector, etc) along with a set of label
   operations. If for some reason this fails, `ReturnKind::Unknown` is returned
   with a message explaining why.
 * `ReturnValue::passthrough(input_labels: &[&str])` can then be used predict
   which output labels should be present for a given query. It accepts a list of
   initial label names assumed to be present on the metrics (e.g. expected
   metrics like `namespace`) and returns a set of expected output labels.

   Note that additional labels may be returned: if expressions explicitly select
   for labels, they'll be returned as well; an empty list of initial labels can be
   used to see what extra labels an expression will query for.

 * `ReturnValue::drops(label: &str)` determines which AST node, if any, dropped
   the given input label.

## Examples

### [`examples/parse.rs`](./examples/parse.rs)

This can be used to dump the (slightly reduced) syntax tree for a given
expression.

Try `cargo run --example parse 'foo'`:

```bash
$ cargo run --example parse 'count_values
  by(service) ("config_hash", alertmanager_config_hash{job="alertmanager-main",namespace="monitoring"})
  / on(service) group_left() label_replace(max by(name, job, namespace, controller)
  (prometheus_operator_spec_replicas{controller="alertmanager",job="prometheus-operator",namespace="monitoring"}),
  "service", "alertmanager-$1", "name", "(.*)") !=
  1'

Operator {
    kind: NotEqual,
    lhs: Operator {
        kind: Divide,
        lhs: Function {
            name: "count_values",
            args: [
                # ... snip ...
            ],
            aggregation: Some(
                Aggregation {
                    op: By,
                    labels: [
                        "service",
                    ],
                },
            ),
        },
        rhs: Function {
            name: "label_replace",
            args: [
                # ... snip ...
            ],
            aggregation: None,
        },
        matching: Some(
            Matching {
                op: On,
                labels: [
                    "service",
                ],
                group: Some(
                    MatchingGroup {
                        op: Left,
                        labels: [],
                    },
                ),
            },
        ),
    },
    rhs: 1.0,
    matching: None,
}
```

Note that `Expression` has a `Debug` impl that somewhat reduces boilerplate
output. It makes debug output more readable, but the displayed object structure
won't exactly match the in-memory version. Specifically, if constructing your
own AST objects, use the `.wrap()` utility functions present on most
`Expression` subtypes to convert them to an `Expression`.

### [`examples/reformat.rs`](./examples/reformat.rs)

Parses the input expression and reformats it using the default `Display` impl.

```bash
$ cargo run --example reformat 'sum by(bar)(foo)'
sum(foo) by (bar)
```

### [`examples/return_value.rs`](./examples/return_value.rs)

Displays the raw `ReturnValue` computed for a given expression.

```bash
$ cargo run --example return_value 'foo(sum(foo))'
ReturnValue {
    kind: InstantVector,
    label_ops: [
        LabelSetOpTuple {
            op: Clear,
            expression: Function {
                name: "sum",
                # ... snip ...
            },
            span: Some(
                Span {
                    start: 4,
                    end: 12,
                },
            ),
        },
    ],
}

```

### [`examples/label_drop.rs`](./examples/label_drop.rs)

Shows where a label is dropped in an expression, if at all.

```bash
$ cargo run --example label_drop 'foo(sum(foo))' namespace
label 'namespace' is dropped:

foo(sum(foo))
    --------

parent expression: sum(foo)
```

If an expression is invalid and has an unknown return type, a message will be
printed:

```bash
$ cargo run --example label_drop 'foo(sum(foo)) / bar[5m:]' namespace
note: expression return type is unknown, result may be inaccurate
  reason:     rhs return type (RangeVector) is not valid in an operator
  expression: foo(sum(foo)) / bar[5m:]

label 'namespace' is not dropped
```

### [`examples/label_passthrough.rs`](./examples/label_passthrough.rs)

Shows the expected output labels for a given expression. Arguments after the
expression are labels that exist on all input metrics.

Additional labels may be returned that were not present in the input list; these
were inferred based on labels referenced in the expression.


```bash
$ cargo run --example label_passthrough 'sum by(bar)(foo)' baz
{
    "bar",
}
```

## Contributing

Bug reports, feature requests, and pull requests are welcome! Be sure to read
though the [code of conduct] for some pointers to get started.

Note that - as mentioned in the code of conduct - code contributions must
indicate that you accept the [Developer Certificate of Origin][dco],
essentially asserting you have the necessary rights to submit the code you're 
contributing under the project's license (MIT). If you agree, simply pass `-s`
to `git commit`:

```bash
git commit -s [...]
```

... and Git will automatically append the required `Signed-off-by: ...` to the
end of your commit message.

[code of conduct]: ./CODE_OF_CONDUCT.md
[dco]: https://developercertificate.org/
