from typing import Callable, ParamSpec, TypeVar

P = ParamSpec("P")
R = TypeVar("R")


def add5(x: int) -> int:
    return x + 5


def add7(x: int) -> int:
    return x + 7


def my_decorator(path: str) -> Callable[[Callable[P, R]], Callable[P, str]]:
    """A decorator factory that simulates FastAPI's app.get/post pattern."""

    def decorator(func: Callable[P, R]) -> Callable[P, str]:
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> str:
            result = func(*args, **kwargs)
            return f"decorated: {result}"

        return wrapper

    return decorator
