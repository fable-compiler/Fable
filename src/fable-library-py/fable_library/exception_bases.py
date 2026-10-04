"""Python exception bases for generated .NET exception classes.

decision: retain ExceptionBase ancestry and payloads while adding one specialized Python exception base.
"""


class ExceptionBase(Exception):
    """Base class for .NET ``System.Exception`` and its subclasses.

    Subclasses the built-in ``Exception`` so ``raise``/``except``/``isinstance``
    keep working as before. Only the message is forwarded to the built-in
    initializer, so ``str(exc)`` still returns the message even when an inner
    exception is supplied (the built-in would otherwise stringify the whole
    argument tuple). The inner exception is kept on a dedicated attribute so it
    can be read back through ``System.Exception.InnerException``.
    """

    def __init__(self, message: str | None = None, inner_exception: Exception | None = None) -> None:
        super().__init__(message if message is not None else "")
        self.inner_exception: Exception | None = inner_exception


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
