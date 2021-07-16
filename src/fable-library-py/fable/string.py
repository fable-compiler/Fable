import re
from abc import ABC
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Callable, Iterable, Match, NoReturn, Optional, Pattern, Union, TypeVar

# import multiply
# import Numeric
# import toExponential
# import toFixed
# import toHex
# import toPrecision
# import "./Date.js"
# import "./Numeric.js"
# import "./RegExp.js"
# import "./Types.js"
from .numeric import to_fixed, to_precision, to_exponential, to_hex, multiply
from .types import toString

# import { escape }
# import { toString as dateToString }
# import { toString }

T = TypeVar("T")


fsFormatRegExp: Pattern[str] = re.compile(r"(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w)")
interpolateRegExp: Pattern[str] = re.compile(r"(?:(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w))?%P\(\)")
formatRegExp: Pattern[str] = re.compile(r"\{(\d+)(,-?\d+)?(?:\:([a-zA-Z])(\d{0,2})|\:(.+?))?\}")


# const enum StringComparison {
#   CurrentCulture = 0,
#   CurrentCultureIgnoreCase = 1,
#   InvariantCulture = 2,
#   InvariantCultureIgnoreCase = 3,
#   Ordinal = 4,
#   OrdinalIgnoreCase = 5,
# }

# function cmp(x: string, y: string, ic: boolean | StringComparison) {
#   function isIgnoreCase(i: boolean | StringComparison) {
#     return i === true ||
#       i === StringComparison.CurrentCultureIgnoreCase ||
#       i === StringComparison.InvariantCultureIgnoreCase ||
#       i === StringComparison.OrdinalIgnoreCase;
#   }
#   function isOrdinal(i: boolean | StringComparison) {
#     return i === StringComparison.Ordinal ||
#       i === StringComparison.OrdinalIgnoreCase;
#   }
#   if (x == null) { return y == null ? 0 : -1; }
#   if (y == null) { return 1; } // everything is bigger than null

#   if (isOrdinal(ic)) {
#     if (isIgnoreCase(ic)) { x = x.toLowerCase(); y = y.toLowerCase(); }
#     return (x === y) ? 0 : (x < y ? -1 : 1);
#   } else {
#     if (isIgnoreCase(ic)) { x = x.toLocaleLowerCase(); y = y.toLocaleLowerCase(); }
#     return x.localeCompare(y);
#   }
# }

# export function compare(...args: any[]): number {
#   switch (args.length) {
#     case 2: return cmp(args[0], args[1], false);
#     case 3: return cmp(args[0], args[1], args[2]);
#     case 4: return cmp(args[0], args[1], args[2] === true);
#     case 5: return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), false);
#     case 6: return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5]);
#     case 7: return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5] === true);
#     default: throw new Error("String.compare: Unsupported number of parameters");
#   }
# }

# export function compareOrdinal(x: string, y: string) {
#   return cmp(x, y, StringComparison.Ordinal);
# }

# export function compareTo(x: string, y: string) {
#   return cmp(x, y, StringComparison.CurrentCulture);
# }

# export function startsWith(str: string, pattern: string, ic: number) {
#   if (str.length >= pattern.length) {
#     return cmp(str.substr(0, pattern.length), pattern, ic) === 0;
#   }
#   return false;
# }

# export function indexOfAny(str: string, anyOf: string[], ...args: number[]) {
#   if (str == null || str === "") {
#     return -1;
#   }
#   const startIndex = (args.length > 0) ? args[0] : 0;
#   if (startIndex < 0) {
#     throw new Error("Start index cannot be negative");
#   }
#   const length = (args.length > 1) ? args[1] : str.length - startIndex;
#   if (length < 0) {
#     throw new Error("Length cannot be negative");
#   }
#   if (length > str.length - startIndex) {
#     throw new Error("Invalid startIndex and length");
#   }
#   str = str.substr(startIndex, length);
#   for (const c of anyOf) {
#     const index = str.indexOf(c);
#     if (index > -1) {
#       return index + startIndex;
#     }
#   }
#   return -1;
# }

IPrintfFormatContinuation = Callable[[Callable[[str], Any]], Callable[[str], Any]]


@dataclass
class IPrintfFormat(ABC):
    input: str
    cont: IPrintfFormatContinuation


def printf(input: str) -> IPrintfFormat:
    # print("printf: ", input)
    format: IPrintfFormatContinuation = fsFormat(input)
    return IPrintfFormat(input=input, cont=format)


def continuePrint(cont: Callable[[str], Any], arg: Union[IPrintfFormat, str]) -> Union[Any, Callable[[str], Any]]:
    # print("continuePrint", cont)
    if isinstance(arg, str):
        return cont(arg)

    return arg.cont(cont)


def toConsole(arg: Union[IPrintfFormat, str]) -> Union[Any, Callable[[str], Any]]:
    return continuePrint(print, arg)


def toConsoleError(arg: Union[IPrintfFormat, str]):
    return continuePrint(lambda x: print(x), arg)


def toText(arg: Union[IPrintfFormat, str]) -> Union[str, Callable]:
    cont: Callable[[str], Any] = lambda x: x
    return continuePrint(cont, arg)


def toFail(arg: Union[IPrintfFormat, str]):
    def fail(msg: str):
        raise Exception(msg)

    return continuePrint(fail, arg)


def formatReplacement(rep: Any, flags: Any, padLength: Any, precision: Any, format: Any):
    # print("Got here", rep, format)
    sign = ""
    flags = flags or ""
    format = format or ""

    if isinstance(rep, (int, float)):
        if format.lower() != "x":
            if rep < 0:
                rep = rep * -1
                sign = "-"
            else:
                if flags.find(" ") >= 0:
                    sign = " "
                elif flags.find("+") >= 0:
                    sign = "+"

        elif format == "x":
            rep = to_hex(rep)
        elif format == "X":
            rep = to_hex(rep).upper()

        precision = None if precision is None else int(precision)
        if format in ("f", "F"):
            precision = precision if precision is not None else 6
            rep = to_fixed(rep, precision)
        elif format in ("g", "G"):
            rep = to_precision(rep, precision) if precision is not None else to_precision(rep)
        elif format in ("e", "E"):
            rep = to_exponential(rep, precision) if precision is not None else to_exponential(rep)
        else:  # AOid
            rep = str(rep)

    elif isinstance(rep, datetime):
        rep = dateToString(rep)
    else:
        rep = toString(rep)

    if padLength is not None:
        padLength = int(padLength)
        zeroFlag = flags.find("0") >= 0  # Use '0' for left padding
        minusFlag = flags.find("-") >= 0  # Right padding
        ch = " " if minusFlag or not zeroFlag else "0"
        if ch == "0":
            rep = padLeft(rep, padLength - len(sign), ch, minusFlag)
            rep = sign + rep
        else:
            rep = padLeft(sign + rep, padLength, ch, minusFlag)

    else:
        rep = sign + rep

    return rep


def interpolate(string: str, values: Any) -> str:
    valIdx = 0
    strIdx = 0
    result = ""
    matches = interpolateRegExp.finditer(string)
    for match in matches:
        # The first group corresponds to the no-escape char (^|[^%]), the actual pattern starts in the next char
        # Note: we don't use negative lookbehind because some browsers don't support it yet
        matchIndex = match.start() + len(match[1] or "")
        result += string[strIdx:matchIndex].replace("%%", "%")
        [_, flags, padLength, precision, format] = match.groups()
        # print(match.groups())
        result += formatReplacement(values[valIdx], flags, padLength, precision, format)
        valIdx += 1

        strIdx = match.end()

    result += string[strIdx:].replace("%%", "%")
    return result


def formatOnce(str2: str, rep: Any):
    # print("formatOnce: ", str2, rep)

    def match(m: Match[str]):
        prefix, flags, padLength, precision, format = m.groups()
        # print("prefix: ", [prefix])
        once: str = formatReplacement(rep, flags, padLength, precision, format)
        # print("once:", [once])
        return prefix + once.replace("%", "%%")

    ret = fsFormatRegExp.sub(match, str2, count=1)
    return ret


def createPrinter(string: str, cont: Callable[..., Any]):
    # print("createPrinter", string)

    def _(*args: Any):
        strCopy: str = string
        for arg in args:
            # print("Arg: ", [arg])
            strCopy = formatOnce(strCopy, arg)
            # print("strCopy", strCopy)

        # print("strCopy", strCopy)
        if fsFormatRegExp.search(strCopy):
            return createPrinter(strCopy, cont)
        return cont(strCopy.replace("%%", "%"))

    return _


def fsFormat(str: str):
    # print("fsFormat: ", [str])

    def _(cont: Callable[..., Any]):
        if fsFormatRegExp.search(str):
            return createPrinter(str, cont)
        return cont(str)

    return _


def format(string: str, *args: Any) -> str:
    print("format: ", string, args)
    # if (typeof str === "object" and args.length > 0):
    #     # Called with culture info
    #     str = args[0]
    #     args.shift()

    def match(m: Match[str]):
        print("Groups: ", m.groups())
        idx, padLength, format, precision_, pattern = list(m.groups())
        rep = args[int(idx)]
        print("rep: ", [rep])
        if isinstance(rep, (int, float)):
            precision: Optional[int] = None if precision_ is None else int(precision_)

            if format in ["f", "F"]:
                precision = precision if precision is not None else 2
                rep = to_fixed(rep, precision)

            elif format in ["g", "G"]:
                rep = to_precision(rep, precision) if precision is not None else to_precision(rep)

            elif format in ["e", "E"]:
                rep = to_exponential(rep, precision) if precision is not None else to_exponential(rep)

            elif format in ["p", "P"]:
                precision = precision if precision is not None else 2
                rep = to_fixed(multiply(rep, 100), precision) + " %"

            elif format in ["d", "D"]:
                rep = padLeft(str(rep), precision, "0") if precision is not None else str(rep)

            elif format in ["x", "X"]:
                rep = padLeft(to_hex(rep), precision, "0") if precision is not None else to_hex(rep)
                if format == "X":
                    rep = rep.upper()
            elif pattern:
                sign = ""

                def match(m: Match[str]):
                    nonlocal sign, rep

                    intPart, decimalPart = list(m.groups())
                    # print("**************************: ", rep)
                    if rep < 0:
                        rep = multiply(rep, -1)
                        sign = "-"

                    rep = to_fixed(rep, len(decimalPart) - 1 if decimalPart else 0)
                    return padLeft(rep, len(intPart or "") - len(sign) + (len(decimalPart) if decimalPart else 0), "0")

                rep = re.sub(r"(0+)(\.0+)?", match, pattern)
                rep = sign + rep

        elif isinstance(rep, datetime):
            rep = dateToString(rep, pattern or format)
        else:
            rep = toString(rep)

        try:
            padLength = int((padLength or " ")[1:])
            rep = padLeft(str(rep), abs(padLength), " ", padLength < 0)
        except ValueError:
            pass

        print("return rep: ", [rep])
        return rep

    ret = formatRegExp.sub(match, string)
    print("ret: ", ret)
    return ret


def endsWith(string: str, search: str) -> bool:
    return string.endswith(search)


def initialize(n: int, f: Callable[[int], str]) -> str:
    if n < 0:
        raise Exception("String length must be non-negative")

    xs = [""] * n
    for i in range(n):
        xs[i] = f(i)

    return "".join(xs)


# export function insert(str: string, startIndex: number, value: string) {
#   if (startIndex < 0 || startIndex > str.length) {
#     throw new Error("startIndex is negative or greater than the length of this instance.");
#   }
#   return str.substring(0, startIndex) + value + str.substring(startIndex);
# }


def isNullOrEmpty(string: Optional[str]):
    not isinstance(string, str) or not len(string)


def isNullOrWhiteSpace(string: Optional[Any]) -> bool:
    return not isinstance(string, str) or string.isspace()


def concat(*xs: Iterable[Any]) -> str:
    return "".join(map(str, xs))


def join(delimiter: str, xs: Iterable[Any]) -> str:
    return delimiter.join((str(x) for x in xs))


# export function joinWithIndices(delimiter: string, xs: string[], startIndex: number, count: number) {
#   const endIndexPlusOne = startIndex + count;
#   if (endIndexPlusOne > xs.length) {
#     throw new Error("Index and count must refer to a location within the buffer.");
#   }
#   return xs.slice(startIndex, endIndexPlusOne).join(delimiter);
# }


def notSupported(name: str) -> NoReturn:
    raise Exception("The environment doesn't support '" + name + "', please use a polyfill.")


# export function toBase64String(inArray: number[]) {
#   let str = "";
#   for (let i = 0; i < inArray.length; i++) {
#     str += String.fromCharCode(inArray[i]);
#   }
#   return typeof btoa === "function" ? btoa(str) : notSupported("btoa");
# }

# export function fromBase64String(b64Encoded: string) {
#   const binary = typeof atob === "function" ? atob(b64Encoded) : notSupported("atob");
#   const bytes = new Uint8Array(binary.length);
#   for (let i = 0; i < binary.length; i++) {
#     bytes[i] = binary.charCodeAt(i);
#   }
#   return bytes;
# }


def padLeft(string: str, length: int, ch: Optional[str] = None, isRight: Optional[bool] = False) -> str:
    ch = ch or " "
    length = length - len(string)
    for i in range(length):
        string = string + ch if isRight else ch + string

    return string


def padRight(string: str, len: int, ch: Optional[str] = None) -> str:
    return padLeft(string, len, ch, True)


# export function remove(str: string, startIndex: number, count?: number) {
#   if (startIndex >= str.length) {
#     throw new Error("startIndex must be less than length of string");
#   }
#   if (typeof count === "number" && (startIndex + count) > str.length) {
#     throw new Error("Index and count must refer to a location within the string.");
#   }
#   return str.slice(0, startIndex) + (typeof count === "number" ? str.substr(startIndex + count) : "");
# }


def replace(string: str, search: str, replace: str):
    return string.replace(search, replace)


def replicate(n: int, x: str) -> str:
    return initialize(n, lambda _=0: x)


# export function getCharAtIndex(input: string, index: number) {
#   if (index < 0 || index >= input.length) {
#     throw new Error("Index was outside the bounds of the array.");
#   }
#   return input[index];
# }

# export function split(str: string, splitters: string[], count?: number, removeEmpty?: number) {
#   count = typeof count === "number" ? count : undefined;
#   removeEmpty = typeof removeEmpty === "number" ? removeEmpty : undefined;
#   if (count && count < 0) {
#     throw new Error("Count cannot be less than zero");
#   }
#   if (count === 0) {
#     return [];
#   }
#   if (!Array.isArray(splitters)) {
#     if (removeEmpty === 0) {
#       return str.split(splitters, count);
#     }
#     const len = arguments.length;
#     splitters = Array(len - 1);
#     for (let key = 1; key < len; key++) {
#       splitters[key - 1] = arguments[key];
#     }
#   }
#   splitters = splitters.map((x) => escape(x));
#   splitters = splitters.length > 0 ? splitters : [" "];
#   let i = 0;
#   const splits: string[] = [];
#   const reg = new RegExp(splitters.join("|"), "g");
#   while (count == null || count > 1) {
#     const m = reg.exec(str);
#     if (m === null) { break; }
#     if (!removeEmpty || (m.index - i) > 0) {
#       count = count != null ? count - 1 : count;
#       splits.push(str.substring(i, m.index));
#     }
#     i = reg.lastIndex;
#   }
#   if (!removeEmpty || (str.length - i) > 0) {
#     splits.push(str.substring(i));
#   }
#   return splits;
# }

# export function trim(str: string, ...chars: string[]) {
#   if (chars.length === 0) {
#     return str.trim();
#   }
#   const pattern = "[" + escape(chars.join("")) + "]+";
#   return str.replace(new RegExp("^" + pattern), "").replace(new RegExp(pattern + "$"), "");
# }

# export function trimStart(str: string, ...chars: string[]) {
#   return chars.length === 0
#     ? (str as any).trimStart()
#     : str.replace(new RegExp("^[" + escape(chars.join("")) + "]+"), "");
# }

# export function trimEnd(str: string, ...chars: string[]) {
#   return chars.length === 0
#     ? (str as any).trimEnd()
#     : str.replace(new RegExp("[" + escape(chars.join("")) + "]+$"), "");
# }

# export function filter(pred: (char: string) => boolean, x: string) {
#   return x.split("").filter((c) => pred(c)).join("");
# }


def substring(string: str, startIndex: int, length: Optional[int] = None) -> str:
    if length is not None:
        return string[startIndex:startIndex + length]

    return string[startIndex:]
