import os
from typing import Awaitable


def read_all_text(file_name: str) -> str:
    with open(file_name, "r") as f:
        return f.read()


def read_all_text_async(file_name: str) -> Awaitable[str]:
    async def read_all_text_async():
        with open(file_name, "r") as f:
            return f.read()

    return read_all_text_async()


def read_all_lines(file_name: str) -> list[str]:
    with open(file_name, "r") as f:
        return [line.strip("\n") for line in f.readlines()]


def read_all_lines_async(file_name: str) -> Awaitable[list[str]]:
    async def read_all_lines_async():
        with open(file_name, "r") as f:
            return [line.strip("\n") for line in f.readlines()]

    return read_all_lines_async()


def read_all_bytes(file_name: str) -> bytes:
    with open(file_name, "rb") as f:
        return f.read()


def read_all_bytes_async(file_name: str) -> Awaitable[bytes]:
    async def read_all_bytes_async():
        with open(file_name, "rb") as f:
            return f.read()

    return read_all_bytes_async()


def write_all_text(file_name: str, text: str) -> None:
    with open(file_name, "w") as f:
        f.write(text)


def write_all_text_async(file_name: str, text: str) -> Awaitable[None]:
    async def write_all_text_async():
        with open(file_name, "w") as f:
            f.write(text)

    return write_all_text_async()


def write_all_lines(file_name: str, lines: list[str]) -> None:
    with open(file_name, "w") as f:
        f.write('\n'.join(lines))


def write_all_lines_async(file_name: str, lines: list[str]) -> Awaitable[None]:
    async def write_all_lines_async():
        with open(file_name, "w") as f:
            f.write('\n'.join(lines))

    return write_all_lines_async()


def write_all_bytes(file_name: str, bytes: bytes) -> None:
    with open(file_name, "wb") as f:
        f.write(bytes)


def write_all_bytes_async(file_name: str, bytes: bytes) -> Awaitable[None]:
    async def write_all_bytes_async():
        with open(file_name, "wb") as f:
            f.write(bytes)

    return write_all_bytes_async()


def delete(path: str) -> None:
    os.remove(path)


def move(source: str, destination: str) -> None:
    os.rename(source, destination)


def copy(source: str, destination: str) -> None:
    with open(source, "rb") as f:
        data = f.read()
    with open(destination, "wb") as f:
        f.write(data)


def exists(path: str) -> bool:
    return os.path.exists(path)


def replace(
    source_file_name: str, destination_file_name: str, destination_backup_file_name: str
) -> None:
    with open(destination_file_name, "rb") as f:
        data = f.read()
    with open(destination_backup_file_name, "wb") as f:
        f.write(data)
    os.replace(source_file_name, destination_file_name)
