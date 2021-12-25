from typing import Any

from .types import FSharpRef


def try_parse(string: str, defValue: FSharpRef[bool]) -> bool:
    string = string.strip().capitalize()
    if string == "True":
        defValue.contents = True
        return True
    elif string == "False":
        defValue.contents = False
        return True

    return False


def parse(value: Any) -> bool:
    def_value: FSharpRef[bool] = FSharpRef(False)
    if try_parse(value, def_value):
        return def_value.contents

    raise ValueError(f"String '{value}' was not recognized as a valid Boolean.")
