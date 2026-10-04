"""Python exception bases for generated .NET exception classes.

decision: retain ExceptionBase ancestry and payloads while adding one specialized Python exception base.
"""

from .types import ExceptionBase


class ValueErrorBase(ExceptionBase, ValueError):
    """A value error with .NET message and inner-exception support."""


class IndexErrorBase(ExceptionBase, IndexError):
    """An index error with .NET message and inner-exception support."""


class RuntimeErrorBase(ExceptionBase, RuntimeError):
    """A runtime error with .NET message and inner-exception support."""


class ZeroDivisionErrorBase(ExceptionBase, ZeroDivisionError):
    """A division error with .NET message and inner-exception support."""


class OverflowErrorBase(ExceptionBase, OverflowError):
    """An overflow error with .NET message and inner-exception support."""


class NotImplementedErrorBase(ExceptionBase, NotImplementedError):
    """An unimplemented operation with .NET message and inner-exception support."""


class MemoryErrorBase(ExceptionBase, MemoryError):
    """A memory error with .NET message and inner-exception support."""


class TimeoutErrorBase(ExceptionBase, TimeoutError):
    """A timeout error with .NET message and inner-exception support."""
