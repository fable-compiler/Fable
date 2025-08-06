# Benchmark Tests for Fable Python Library

This directory contains benchmark tests for the Fable Python Library, focusing on measuring the performance of Rust implementations compared to pure Python implementations.

## Overview

The benchmark tests are designed to:

1. Measure the performance of core operations in the Fable Python Library
2. Compare Rust implementations against pure Python implementations
3. Identify performance bottlenecks and optimization opportunities
4. Track performance changes over time

## Running Benchmarks

To run all benchmarks:

```bash
pytest tests-benchmark/ -v
```

To run a specific benchmark file:

```bash
pytest tests-benchmark/test_bench_ints.py -v
```

To run a specific benchmark function:

```bash
pytest tests-benchmark/test_bench_ints.py::test_byte_creation_benchmark -v
```

## Benchmark Options

The pytest-benchmark plugin provides several useful options:

- `--benchmark-only`: Run only the benchmarks, not the regular tests
- `--benchmark-save=NAME`: Save the benchmark results to a file
- `--benchmark-compare=NAME`: Compare the current benchmark results with a previous run
- `--benchmark-sort=COL`: Sort the results by a specific column (e.g., 'name', 'min', 'max', 'mean')
- `--benchmark-histogram=PATH`: Generate a histogram of the benchmark results

Example:

```bash
pytest tests-benchmark/ --benchmark-only --benchmark-save=baseline
```

## Interpreting Results

The benchmark results include several statistics:

- **min**: Minimum execution time
- **max**: Maximum execution time
- **mean**: Mean execution time
- **stddev**: Standard deviation of execution times
- **median**: Median execution time
- **iqr**: Interquartile range
- **ops**: Operations per second

When comparing implementations, focus on the **mean** and **median** values for the most reliable comparison.

## Adding New Benchmarks

When adding new benchmarks:

1. Create a new file named `test_bench_*.py` for each module being benchmarked
2. Follow the existing patterns for benchmark organization
3. Use fixtures from `conftest.py` for shared test data
4. Include both individual operation benchmarks and comparison benchmarks
5. Add assertions to verify correctness alongside performance measurements

## Important: Benchmark Fixture Usage

The pytest-benchmark fixture can only be used once per test function. When comparing different implementations, you must create separate test functions for each implementation:

```python
# INCORRECT - Will fail with "Fixture can only be used once" error
def test_comparison_benchmark(benchmark):
    # First implementation
    benchmark.group = "Comparison"
    benchmark.name = "Implementation A"
    result_a = benchmark(implementation_a)

    # Second implementation - This will fail
    benchmark.name = "Implementation B"
    result_b = benchmark(implementation_b)

    # Compare results
    assert result_a == result_b

# CORRECT - Split into separate test functions
def test_implementation_a_benchmark(benchmark):
    benchmark.group = "Comparison"
    benchmark.name = "Implementation A"
    result_a = benchmark(implementation_a)
    # Verify result_a

def test_implementation_b_benchmark(benchmark):
    benchmark.group = "Comparison"
    benchmark.name = "Implementation B"
    result_b = benchmark(implementation_b)
    # Verify result_b
```

By using the same `benchmark.group` and `benchmark.name` values, the results will be grouped together in the benchmark report, allowing for easy comparison.

## Best Practices

- Keep benchmarks focused on measuring one specific operation or comparison
- Use `benchmark.group` and `benchmark.name` to organize related benchmarks
- Include assertions to verify correctness alongside performance
- Use parameterization to test different input sizes and types
- Compare against Python native implementations when relevant
- Document any unexpected performance results or anomalies
- Remember to split comparison benchmarks into separate test functions

## Current Benchmark Modules

- `test_bench_ints.py`: Benchmarks for integer operations (byte, sbyte, int16, etc.)
