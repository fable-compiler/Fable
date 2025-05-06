"""
Benchmark test configuration and fixtures for Fable Python Library.
"""

import pytest


@pytest.fixture
def small_range():
    """Return a small range of integers for benchmarks."""
    return list(range(100))


@pytest.fixture
def medium_range():
    """Return a medium range of integers for benchmarks."""
    return list(range(1000))


@pytest.fixture
def large_range():
    """Return a large range of integers for benchmarks."""
    return list(range(10000))
