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


def parse(value):
    defValue = FSharpRef(False)
    if try_parse(value, defValue):
        return defValue.contents

    raise ValueError(f"String '{value}' was not recognized as a valid Boolean.")
