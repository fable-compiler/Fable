from collections.abc import Callable
from typing import ParamSpec, TypeVar


P = ParamSpec("P")
R = TypeVar("R")


# Simulated FastAPI-like app for testing DecorateTemplate
class MockFastAPI:
    """A mock FastAPI app that simulates route decorators."""

    def get(self, path: str) -> Callable[[Callable[P, R]], Callable[P, R]]:
        """Simulate @app.get(path) decorator."""

        def decorator(func: Callable[P, R]) -> Callable[P, R]:
            return func

        return decorator

    def post(self, path: str) -> Callable[[Callable[P, R]], Callable[P, R]]:
        """Simulate @app.post(path) decorator."""

        def decorator(func: Callable[P, R]) -> Callable[P, R]:
            return func

        return decorator

    def delete(self, path: str) -> Callable[[Callable[P, R]], Callable[P, R]]:
        """Simulate @app.delete(path) decorator."""

        def decorator(func: Callable[P, R]) -> Callable[P, R]:
            return func

        return decorator

    def put(self, path: str) -> Callable[[Callable[P, R]], Callable[P, R]]:
        """Simulate @app.put(path) decorator."""

        def decorator(func: Callable[P, R]) -> Callable[P, R]:
            return func

        return decorator


# Global app instance for tests
app = MockFastAPI()


def add5(x: int) -> int:
    return x + 5


def add7(x: int) -> int:
    return x + 7


def my_decorator(path: str) -> Callable[[Callable[P, object]], Callable[P, str]]:
    """A decorator factory that simulates FastAPI's app.get/post pattern."""

    def decorator(func: Callable[P, object]) -> Callable[P, str]:
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> str:
            result = func(*args, **kwargs)
            return f"decorated: {result}"

        return wrapper

    return decorator
