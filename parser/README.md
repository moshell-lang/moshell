Parser
======

Read a series of tokens produced by the lexer and build an AST.

Benchmark
---------

```shell
# Run Criterion on the sample valid script
cargo bench --bench sample_install

# Generate a flamegraph for the above benchmark
cargo install flamegraph # https://github.com/flamegraph-rs/flamegraph
cargo flamegraph --bench sample_install -o parse-baseline.svg -- --bench
```
