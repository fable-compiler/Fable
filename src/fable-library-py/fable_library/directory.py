from __future__ import annotations

import os


def create_directory(path: str) -> None:
    os.makedirs(path, exist_ok=True)


__all__ = [
    "create_directory",
]
