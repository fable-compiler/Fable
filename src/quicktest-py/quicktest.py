import sys
from fable_modules.fable_library.array import try_head
from fable_modules.fable_library.option import default_arg
from fable_modules.fable_library.string import (to_console, interpolate)
from fable_modules.fable_library.types import Array

def main(argv: Array[str]) -> int:
    name : str = default_arg(try_head(argv), "Guest")
    to_console(interpolate("Hello %P()!", [name]))
    return 0


if __name__ == "__main__":
    main(sys.argv[1:])


