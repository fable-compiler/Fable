import os
import tempfile
from typing import Optional


def get_temp_file_name() -> str:
    fd, name = tempfile.mkstemp()
    os.close(fd)
    return name


def get_temp_path() -> str:
    return tempfile.gettempdir()


def get_extension(path: str) -> str:
    return os.path.splitext(path)[1]


def get_random_file_name() -> str:
    name = os.urandom(4).hex()
    ext = os.urandom(2).hex()[:3]
    return f"{name}.{ext}"


def get_file_name(path: str) -> str:
    return os.path.basename(path)


def get_directory_name(path: str) -> str:
    return os.path.dirname(path)


def get_file_name_without_extension(path: str) -> str:
    return os.path.splitext(os.path.basename(path))[0]


def get_full_path(path: str, base_path: Optional[str]=None) -> str:
    return os.path.join(base_path or "", path)


def get_relative_path(relative_to: str, path: str) -> str:
    return os.path.relpath(path, relative_to)


def has_extension(path: str) -> bool:
    return os.path.splitext(path)[1] != ""


__all__ = [
    "get_temp_file_name",
    "get_temp_path",
    "get_extension",
    "get_random_file_name",
    "get_file_name",
    "get_directory_name",
    "get_file_name_without_extension",
    "get_full_path",
    "get_relative_path",
    "has_extension",
]