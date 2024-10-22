import { toString as dateToString } from "./Date.js";
import { compare as numericCompare, isNumeric, multiply, Numeric, toExponential, toFixed, toHex, toPrecision } from "./Numeric.js";
import { escape } from "./RegExp.js";
import { toString } from "./Types.js";

const fsFormatRegExp = /(^|[^%])%([0+\- ]*)(\*|\d+)?(?:\.(\d+))?(\w)/g;
const interpolateRegExp = /(?:(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w))?%P\(\)/g;
const formatRegExp = /\{(\d+)(,-?\d+)?(?:\:([a-zA-Z])(\d{0,2})|\:(.+?))?\}/g;

function isLessThan(x: Numeric, y: number) {
  return numericCompare(x, y) < 0;
}

const enum StringComparison {
  CurrentCulture = 0,
  CurrentCultureIgnoreCase = 1,
  InvariantCulture = 2,
  InvariantCultureIgnoreCase = 3,
  Ordinal = 4,
  OrdinalIgnoreCase = 5,
}

function cmp(x: string, y: string, ic: boolean | StringComparison) {
  function isIgnoreCase(i: boolean | StringComparison) {
    return i === true ||
      i === StringComparison.CurrentCultureIgnoreCase ||
      i === StringComparison.InvariantCultureIgnoreCase ||
      i === StringComparison.OrdinalIgnoreCase;
  }
  function isOrdinal(i: boolean | StringComparison) {
    return i === StringComparison.Ordinal ||
      i === StringComparison.OrdinalIgnoreCase;
  }
  if (x == null) { return y == null ? 0 : -1; }
  if (y == null) { return 1; } // everything is bigger than null

  if (isOrdinal(ic)) {
    if (isIgnoreCase(ic)) { x = x.toLowerCase(); y = y.toLowerCase(); }
    return (x === y) ? 0 : (x < y ? -1 : 1);
  } else {
    if (isIgnoreCase(ic)) { x = x.toLocaleLowerCase(); y = y.toLocaleLowerCase(); }
    return x.localeCompare(y);
  }
}

export function compare(...args: any[]): number {
  switch (args.length) {
    case 2: return cmp(args[0], args[1], false);
    case 3: return cmp(args[0], args[1], args[2]);
    case 4: return cmp(args[0], args[1], args[2] === true);
    case 5: return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), false);
    case 6: return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5]);
    case 7: return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5] === true);
    default: throw new Error("String.compare: Unsupported number of parameters");
  }
}

export function compareOrdinal(x: string, y: string) {
  return cmp(x, y, StringComparison.Ordinal);
}

export function compareTo(x: string, y: string) {
  return cmp(x, y, StringComparison.CurrentCulture);
}

export function startsWith(str: string, pattern: string, ic: boolean | StringComparison) {
  if (ic === StringComparison.Ordinal) { // to avoid substring allocation
    return str.startsWith(pattern);
  }
  if (str.length >= pattern.length) {
    return cmp(str.substr(0, pattern.length), pattern, ic) === 0;
  }
  return false;
}

export function endsWith(str: string, pattern: string, ic: boolean | StringComparison) {
  if (ic === StringComparison.Ordinal) { // to avoid substring allocation
    return str.endsWith(pattern);
  }
  if (str.length >= pattern.length) {
    return cmp(str.substr(str.length - pattern.length, pattern.length), pattern, ic) === 0;
  }
  return false;
}

export function indexOfAny(str: string, anyOf: string[], ...args: number[]) {
  if (str == null || str === "") {
    return -1;
  }
  const startIndex = (args.length > 0) ? args[0] : 0;
  if (startIndex < 0) {
    throw new Error("Start index cannot be negative");
  }
  const length = (args.length > 1) ? args[1] : str.length - startIndex;
  if (length < 0) {
    throw new Error("Length cannot be negative");
  }
  if (startIndex + length > str.length) {
    throw new Error("Invalid startIndex and length");
  }
  const endIndex = startIndex + length
  const anyOfAsStr = "".concat.apply("", anyOf);
  for (let i = startIndex; i < endIndex; i++) {
    if (anyOfAsStr.indexOf(str[i]) > -1) {
      return i;
    }
  }
  return -1;
}

export type IPrintfFormatContinuation =
  (f: (x: string) => any) => any;

export interface IPrintfFormat {
  input: string;
  cont: IPrintfFormatContinuation;
}

export function printf(input: string): IPrintfFormat {
  return {
    input,
    cont: fsFormat(input) as IPrintfFormatContinuation,
  };
}

export function interpolate(str: string, values: any[]): string {
  let valIdx = 0;
  let strIdx = 0;
  let result = "";
  interpolateRegExp.lastIndex = 0;
  let match = interpolateRegExp.exec(str);
  while (match) {
    // The first group corresponds to the no-escape char (^|[^%]), the actual pattern starts in the next char
    // Note: we don't use negative lookbehind because some browsers don't support it yet
    const matchIndex = match.index + (match[1] || "").length;
    result += str.substring(strIdx, matchIndex).replace(/%%/g, "%");
    const [, , flags, padLength, precision, format] = match;
    // Save interpolateRegExp.lastIndex before running formatReplacement because the values
    // may also involve interpolation and make use of interpolateRegExp (see #3078)
    strIdx = interpolateRegExp.lastIndex;
    result += formatReplacement(values[valIdx++], flags, padLength, precision, format);
    // Move interpolateRegExp.lastIndex one char behind to make sure we match the no-escape char next time
    interpolateRegExp.lastIndex = strIdx - 1;
    match = interpolateRegExp.exec(str);
  }
  result += str.substring(strIdx).replace(/%%/g, "%");
  return result;
}

function continuePrint(cont: (x: string) => any, arg: IPrintfFormat | string) {
  return typeof arg === "string" ? cont(arg) : arg.cont(cont);
}

export function toConsole(arg: IPrintfFormat | string) {
  // Don't remove the lambda here, see #1357
  return continuePrint((x: string) => console.log(x), arg);
}

export function toConsoleError(arg: IPrintfFormat | string) {
  return continuePrint((x: string) => console.error(x), arg);
}

export function toText(arg: IPrintfFormat | string) {
  return continuePrint((x: string) => x, arg);
}

export function toFail(arg: IPrintfFormat | string) {
  return continuePrint((x: string) => {
    throw new Error(x);
  }, arg);
}

function formatReplacement(rep: any, flags: any, padLength: any, precision: any, format: any) {
  let sign = "";
  flags = flags || "";
  format = format || "";
  if (isNumeric(rep)) {
    if (format.toLowerCase() !== "x") {
      if (isLessThan(rep, 0)) {
        rep = multiply(rep, -1);
        sign = "-";
      } else {
        if (flags.indexOf(" ") >= 0) {
          sign = " ";
        } else if (flags.indexOf("+") >= 0) {
          sign = "+";
        }
      }
    }
    precision = precision == null ? null : parseInt(precision, 10);
    switch (format) {
      case "f": case "F":
        precision = precision != null ? precision : 6;
        rep = toFixed(rep, precision);
        break;
      case "g": case "G":
        rep = precision != null ? toPrecision(rep, precision) : toPrecision(rep);
        break;
      case "e": case "E":
        rep = precision != null ? toExponential(rep, precision) : toExponential(rep);
        break;
      case "x":
        rep = toHex(rep);
        break;
      case "X":
        rep = toHex(rep).toUpperCase();
        break;
      default: // AOid
        rep = String(rep);
        break;
    }
  } else if (rep instanceof Date) {
    rep = dateToString(rep);
  } else {
    rep = toString(rep);
  }
  padLength = typeof padLength === "number" ? padLength : parseInt(padLength, 10);
  if (!isNaN(padLength)) {
    const zeroFlag = flags.indexOf("0") >= 0; // Use '0' for left padding
    const minusFlag = flags.indexOf("-") >= 0; // Right padding
    const ch = minusFlag || !zeroFlag ? " " : "0";
    if (ch === "0") {
      rep = pad(rep, padLength - sign.length, ch, minusFlag);
      rep = sign + rep;
    } else {
      rep = pad(sign + rep, padLength, ch, minusFlag);
    }
  } else {
    rep = sign + rep;
  }
  return rep;
}

function createPrinter(cont: (...args: any[]) => any, _strParts: string[], _matches: RegExpExecArray[], _result = "", padArg = -1) {
  return (...args: any[]) => {
    // Make copies of the values passed by reference because the function can be used multiple times
    let result = _result;
    const strParts = _strParts.slice();
    const matches = _matches.slice();
    for (const arg of args) {
      const [, , flags, _padLength, precision, format] = matches[0];
      let padLength: any = _padLength;
      if (padArg >= 0) {
        padLength = padArg;
        padArg = -1;
      }
      else if (padLength === "*") {
        if (arg < 0) {
          throw new Error("Non-negative number required");
        }
        padArg = arg;
        continue;
      }

      result += strParts[0];
      result += formatReplacement(arg, flags, padLength, precision, format);

      strParts.splice(0, 1);
      matches.splice(0, 1);
    }

    if (matches.length === 0) {
      result += strParts[0];
      return cont(result);
    } else {
      return createPrinter(cont, strParts, matches, result, padArg);
    }
  };
}

export function fsFormat(str: string) {
  return (cont: (...args: any[]) => any) => {
    fsFormatRegExp.lastIndex = 0;
    const strParts: string[] = [];
    const matches: RegExpExecArray[] = [];
    let strIdx = 0;
    let match = fsFormatRegExp.exec(str);
    while (match) {
      // The first group corresponds to the no-escape char (^|[^%]), the actual pattern starts in the next char
      // Note: we don't use negative lookbehind because some browsers don't support it yet
      const matchIndex = match.index + (match[1] || "").length;
      strParts.push(str.substring(strIdx, matchIndex).replace(/%%/g, "%"));
      matches.push(match)
      strIdx = fsFormatRegExp.lastIndex;
      // Likewise we need to move fsFormatRegExp.lastIndex one char behind to make sure we match the no-escape char next time
      fsFormatRegExp.lastIndex -= 1;
      match = fsFormatRegExp.exec(str);
    }
    if (strParts.length === 0) {
      return cont(str.replace(/%%/g, "%"));
    } else {
      strParts.push(str.substring(strIdx).replace(/%%/g, "%"))
      return createPrinter(cont, strParts, matches);
    }
  };
}

export function format(str: string | object, ...args: any[]) {
  let str2: string;
  if (typeof str === "object") {
    // Called with culture info
    str2 = String(args[0]);
    args.shift();
  } else {
    str2 = str;
  }

  return str2.replace(formatRegExp, (_, idx: number, padLength, format, precision, pattern) => {
    if (idx < 0 || idx >= args.length) {
      throw new Error("Index must be greater or equal to zero and less than the arguments' length.")
    }
    let rep = args[idx];
    if (isNumeric(rep)) {
      precision = precision == null ? null : parseInt(precision, 10);
      switch (format) {
        case "f": case "F":
          precision = precision != null ? precision : 2;
          rep = toFixed(rep, precision);
          break;
        case "g": case "G":
          rep = precision != null ? toPrecision(rep, precision) : toPrecision(rep);
          break;
        case "e": case "E":
          rep = precision != null ? toExponential(rep, precision) : toExponential(rep);
          break;
        case "p": case "P":
          precision = precision != null ? precision : 2;
          rep = toFixed(multiply(rep, 100), precision) + " %";
          break;
        case "d": case "D":
          rep = precision != null ? padLeft(String(rep), precision, "0") : String(rep);
          break;
        case "x": case "X":
          rep = precision != null ? padLeft(toHex(rep), precision, "0") : toHex(rep);
          if (format === "X") { rep = rep.toUpperCase(); }
          break;
        default:
          if (pattern) {
            let sign = "";
            rep = (pattern as string).replace(/([0#,]+)(\.[0#]+)?/, (_, intPart: string, decimalPart: string) => {
              if (isLessThan(rep, 0)) {
                rep = multiply(rep, -1);
                sign = "-";
              }
              decimalPart = decimalPart == null ? "" : decimalPart.substring(1);
              rep = toFixed(rep, Math.max(decimalPart.length, 0));

              let [repInt, repDecimal] = (rep as string).split(".");
              repDecimal ||= "";

              const leftZeroes = intPart.replace(/,/g, "").replace(/^#+/, "").length;
              repInt = padLeft(repInt, leftZeroes, "0");

              const rightZeros = decimalPart.replace(/#+$/, "").length;
              if (rightZeros > repDecimal.length) {
                repDecimal = padRight(repDecimal, rightZeros, "0");
              } else if (rightZeros < repDecimal.length) {
                repDecimal = repDecimal.substring(0, rightZeros) + repDecimal.substring(rightZeros).replace(/0+$/, "");
              }

              // Thousands separator
              if (intPart.indexOf(",") > 0) {
                const i = repInt.length % 3;
                const thousandGroups = Math.floor(repInt.length / 3);
                let thousands = i > 0 ? repInt.substr(0, i) + (thousandGroups > 0 ? "," : "") : "";
                for (let j = 0; j < thousandGroups; j++) {
                  thousands += repInt.substr(i + j * 3, 3) + (j < thousandGroups - 1 ? "," : "");
                }
                repInt = thousands;
              }

              return repDecimal.length > 0 ? repInt + "." + repDecimal : repInt;
            });
            rep = sign + rep;
          }
      }
    } else if (rep instanceof Date) {
      rep = dateToString(rep, pattern || format);
    } else {
      rep = toString(rep)
    }
    padLength = parseInt((padLength || " ").substring(1), 10);
    if (!isNaN(padLength)) {
      rep = pad(String(rep), Math.abs(padLength), " ", padLength < 0);
    }
    return rep;
  });
}


export function initialize(n: number, f: (i: number) => string) {
  if (n < 0) {
    throw new Error("String length must be non-negative");
  }
  const xs = new Array(n);
  for (let i = 0; i < n; i++) {
    xs[i] = f(i);
  }
  return xs.join("");
}

export function insert(str: string, startIndex: number, value: string) {
  if (startIndex < 0 || startIndex > str.length) {
    throw new Error("startIndex is negative or greater than the length of this instance.");
  }
  return str.substring(0, startIndex) + value + str.substring(startIndex);
}

export function isNullOrEmpty(str: string | any) {
  return typeof str !== "string" || str.length === 0;
}

export function isNullOrWhiteSpace(str: string | any) {
  return typeof str !== "string" || /^\s*$/.test(str);
}

export function concat(...xs: any[]): string {
  return xs.map((x) => String(x)).join("");
}

export function join<T>(delimiter: string, xs: Iterable<T>): string {
  if (Array.isArray(xs)) {
    return xs.join(delimiter);
  } else {
    return Array.from(xs).join(delimiter);
  }
}

export function joinWithIndices(delimiter: string, xs: string[], startIndex: number, count: number) {
  const endIndexPlusOne = startIndex + count;
  if (endIndexPlusOne > xs.length) {
    throw new Error("Index and count must refer to a location within the buffer.");
  }
  return xs.slice(startIndex, endIndexPlusOne).join(delimiter);
}

function notSupported(name: string): never {
  throw new Error("The environment doesn't support '" + name + "', please use a polyfill.");
}

export function toBase64String(inArray: ArrayLike<number>) {
  let str = "";
  for (let i = 0; i < inArray.length; i++) {
    str += String.fromCharCode(inArray[i]);
  }
  return typeof btoa === "function" ? btoa(str) : notSupported("btoa");
}

export function fromBase64String(b64Encoded: string): number[] {
  const binary = typeof atob === "function" ? atob(b64Encoded) : notSupported("atob");
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes as any as number[];
}

function pad(str: string, len: number, ch?: string, isRight?: boolean) {
  ch = ch || " ";
  len = len - str.length;
  for (let i = 0; i < len; i++) {
    str = isRight ? str + ch : ch + str;
  }
  return str;
}

export function padLeft(str: string, len: number, ch?: string) {
  return pad(str, len, ch);
}

export function padRight(str: string, len: number, ch?: string) {
  return pad(str, len, ch, true);
}

export function remove(str: string, startIndex: number, count?: number) {
  if (startIndex >= str.length) {
    throw new Error("startIndex must be less than length of string");
  }
  if (typeof count === "number" && (startIndex + count) > str.length) {
    throw new Error("Index and count must refer to a location within the string.");
  }
  return str.slice(0, startIndex) + (typeof count === "number" ? str.substr(startIndex + count) : "");
}

export function replace(str: string, search: string, replace: string) {
  return str.replace(new RegExp(escape(search), "g"), replace);
}

export function replicate(n: number, x: string) {
  return initialize(n, () => x);
}

export function getCharAtIndex(input: string, index: number) {
  if (index < 0 || index >= input.length) {
    throw new Error("Index was outside the bounds of the array.");
  }
  return input[index];
}

export function split(str: string, splitters: string[], count?: number, options?: number) {
  count = typeof count === "number" ? count : undefined;
  options = typeof options === "number" ? options : 0;
  if (count && count < 0) {
    throw new Error("Count cannot be less than zero");
  }
  if (count === 0) {
    return [];
  }

  const removeEmpty = (options & 1) === 1;
  const trim = (options & 2) === 2;

  splitters = splitters || [];
  splitters = splitters.filter(x => x).map(escape);
  splitters = splitters.length > 0 ? splitters : ["\\s"];

  const splits: string[] = [];
  const reg = new RegExp(splitters.join("|"), "g");

  let findSplits = true;
  let i = 0;
  do {
    const match = reg.exec(str);

    if (match === null) {
      const candidate = trim ? str.substring(i).trim() : str.substring(i);
      if (!removeEmpty || candidate.length > 0) {
        splits.push(candidate);
      }
      findSplits = false;
    } else {
      const candidate = trim ? str.substring(i, match.index).trim() : str.substring(i, match.index);
      if (!removeEmpty || candidate.length > 0) {
        if (count != null && splits.length + 1 === count) {
          splits.push(trim ? str.substring(i).trim() : str.substring(i));
          findSplits = false;
        } else {
          splits.push(candidate);
        }
      }

      i = reg.lastIndex;
    }
  } while (findSplits)

  return splits;
}

export function trim(str: string, ...chars: string[]) {
  if (chars.length === 0) {
    return str.trim();
  }
  const pattern = "[" + escape(chars.join("")) + "]+";
  return str.replace(new RegExp("^" + pattern), "").replace(new RegExp(pattern + "$"), "");
}

export function trimStart(str: string, ...chars: string[]) {
  return chars.length === 0
    ? (str as any).trimStart()
    : str.replace(new RegExp("^[" + escape(chars.join("")) + "]+"), "");
}

export function trimEnd(str: string, ...chars: string[]) {
  return chars.length === 0
    ? (str as any).trimEnd()
    : str.replace(new RegExp("[" + escape(chars.join("")) + "]+$"), "");
}

export function filter(pred: (char: string) => boolean, x: string) {
  return x.split("").filter((c) => pred(c)).join("");
}

export function substring(str: string, startIndex: number, length?: number) {
  if ((startIndex + (length || 0) > str.length)) {
    throw new Error("Invalid startIndex and/or length");
  }
  return length != null ? str.substr(startIndex, length) : str.substr(startIndex);
}

export function toCharArray2(str: string, startIndex: number, length: number) {
  return substring(str, startIndex, length).split("")
}

interface FormattableString {
  strs: TemplateStringsArray,
  args: any[],
  fmts?: string[]
}

export function fmt(strs: TemplateStringsArray, ...args: any[]): FormattableString {
  return ({ strs, args });
}

export function fmtWith(fmts: string[]) {
  return (strs: TemplateStringsArray, ...args: any[]) => ({ strs, args, fmts } as FormattableString);
}

export function getFormat(s: FormattableString) {
  const strs = s.strs.map((value) => value.replace(/{/g, '{{').replace(/}/g, '}}'));

  return s.fmts
    ? strs
      .reduce((acc, newPart, index) => acc + `{${String(index - 1) + s.fmts![index - 1]}}` + newPart)
    : strs
      .reduce((acc, newPart, index) => acc + `{${index - 1}}` + newPart);
}
