from __future__ import annotations

from typing import TextIO


def write_line(writer: TextIO, text: str = "") -> None:
    writer.write(text + "\n")


__all__ = ["write_line"]
