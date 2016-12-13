import { toString } from "./Util"
import { getRestParams } from "./Util"
import { escape } from "./RegExp"
import { DateKind } from "./Date"
import { second } from "./Date"
import { minute } from "./Date"
import { hour } from "./Date"
import { day } from "./Date"
import { month } from "./Date"
import { year } from "./Date"

const fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;
const formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;

function toHex(value : number) {
  return value < 0
    ? "ff" + (16777215 - (Math.abs(value) - 1)).toString(16)
    : value.toString(16);
}

export function fsFormat(str: string, ...args: any[]): Function | string {
  let _cont: any;
  function isObject(x: any) {
    return x !== null && typeof x === "object" && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
  }
  function formatOnce(str: any, rep: any) {
    return str.replace(fsFormatRegExp, function (_: any, prefix: any, flags: any, pad: any, precision: any, format: any) {
      switch (format) {
        case "f": case "F":
          rep = rep.toFixed(precision || 6); break;
        case "g": case "G":
          rep = rep.toPrecision(precision); break;
        case "e": case "E":
          rep = rep.toExponential(precision); break;
        case "O":
          rep = toString(rep); break;
        case "A":
          try {
            rep = JSON.stringify(rep, function (k, v) {
              return v && v[Symbol.iterator] && !Array.isArray(v) && isObject(v) ? Array.from(v)
                : v && typeof v.ToString === "function" ? toString(v) : v;
            });
          }
          catch (err) {
            // Fallback for objects with circular references
            rep = "{" + Object.getOwnPropertyNames(rep).map(k => k + ": " + String(rep[k])).join(", ") + "}";
          }
          break;
        case "x":
          rep = toHex(Number(rep)); break;
        case "X":
          rep = toHex(Number(rep)).toUpperCase(); break;
      }
      const plusPrefix = flags.indexOf("+") >= 0 && parseInt(rep) >= 0;
      if (!isNaN(pad = parseInt(pad))) {
        const ch = pad >= 0 && flags.indexOf("0") >= 0 ? "0" : " ";
        rep = padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
      }
      let once = prefix + (plusPrefix ? "+" + rep : rep);
      return once.replace(/%/g, "%%");
    });
  }
  function makeFn(str: any) {
    return (rep: any) => {
      const str2 = formatOnce(str, rep);
      return fsFormatRegExp.test(str2)
        ? makeFn(str2) : _cont(str2.replace(/%%/g, "%"));
    };
  }
  if (args.length === 0) {
    return (cont: any) => {
      _cont = cont;
      return fsFormatRegExp.test(str) ? makeFn(str) : _cont(str);
    };
  }
  else {
    for (let i = 0; i < args.length; i++) {
      str = formatOnce(str, args[i]);
    }
    return str.replace(/%%/g, "%");
  }
}

export function format(str: string, ...args: any[]) {
  return str.replace(formatRegExp, function (match: any, idx: any, pad: any, format: any) {
    let rep = args[idx], padSymbol = " ";
    if (typeof rep === "number") {
      switch ((format || "").substring(0, 1)) {
        case "f": case "F":
          rep = format.length > 1 ? rep.toFixed(format.substring(1)) : rep.toFixed(2);
          break;
        case "g": case "G":
          rep = format.length > 1 ? rep.toPrecision(format.substring(1)) : rep.toPrecision();
          break;
        case "e": case "E":
          rep = format.length > 1 ? rep.toExponential(format.substring(1)) : rep.toExponential();
          break;
        case "p": case "P":
          rep = (format.length > 1 ? (rep * 100).toFixed(format.substring(1)) : (rep * 100).toFixed(2)) + " %";
          break;
        case "x":
         rep = toHex(Number(rep)); break;
        case "X":
          rep = toHex(Number(rep)).toUpperCase(); break;
        default:
          const m = /^(0+)(\.0+)?$/.exec(format);
          if (m != null) {
            let decs = 0;
            if (m[2] != null)
              rep = rep.toFixed(decs = m[2].length - 1);
            pad = "," + (m[1].length + (decs ? decs + 1 : 0)).toString();
            padSymbol = "0";
          } else if (format) {
            rep = format;
          }
      }
    } else if (rep instanceof Date) {
      if (format.length === 1) {
        switch (format) {
          case "D":
            rep = rep.toDateString(); break;
          case "T":
            rep = rep.toLocaleTimeString(); break;
          case "d":
            rep = rep.toLocaleDateString(); break;
          case "t":
            rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, ""); break;
          case "o": case "O":
            if ((rep as any).kind === DateKind.Local) {
              const offset = rep.getTimezoneOffset() * -1;
              rep = format("{0:yyyy-MM-dd}T{0:HH:mm}:{1:00.000}{2}{3:00}:{4:00}",
                                    rep, second(rep), offset >= 0 ? "+" : "-",
                                    ~~(offset / 60), offset % 60);
            }
            else {
              rep = rep.toISOString()
            }
        }
      } else {
        rep = format.replace(/\w+/g, function (match2: any) {
          let rep2 = match2;
          switch (match2.substring(0, 1)) {
            case "y":
              rep2 = match2.length < 4 ? year(rep) % 100 : year(rep);
              break;
            case "h":
              rep2 = rep.getHours() > 12 ? hour(rep) % 12 : hour(rep);
              break;
            case "M":
              rep2 = month(rep);
              break;
            case "d":
              rep2 = day(rep);
              break;
            case "H":
              rep2 = hour(rep);
              break;
            case "m":
              rep2 = minute(rep);
              break;
            case "s":
              rep2 = second(rep);
              break;
          }
          if (rep2 !== match2 && rep2 < 10 && match2.length > 1) {
            rep2 = "0" + rep2;
          }
          return rep2;
        });
      }
    }
    if (!isNaN(pad = parseInt((pad || "").substring(1)))) {
      rep = padLeft(rep, Math.abs(pad), padSymbol, pad < 0);
    }
    return rep;
  });
}

export function endsWith(str: string, search: string) {
  const idx = str.lastIndexOf(search);
  return idx >= 0 && idx == str.length - search.length;
}

export function initialize(n: number, f: (i: number) => string) {
  if (n < 0)
    throw new Error("String length must be non-negative");
  const xs = new Array(n);
  for (let i = 0; i < n; i++)
    xs[i] = f(i);
  return xs.join("");
}

export function insert(str: string, startIndex: number, value: string) {
  if (startIndex < 0 || startIndex > str.length) {
    throw new Error("startIndex is negative or greater than the length of this instance.");
  }
  return str.substring(0, startIndex) + value + str.substring(startIndex);
}

export function isNullOrEmpty(str: string | any) {
  return typeof str !== "string" || str.length == 0;
}

export function isNullOrWhiteSpace(str: string | any) {
  return typeof str !== "string" || /^\s*$/.test(str);
}

export function join(delimiter: string, xs: ArrayLike<string>) {
  xs = typeof xs == "string" ? getRestParams(arguments, 1) : xs;
  return (Array.isArray(xs) ? xs : Array.from(xs)).join(delimiter);
}

export function newGuid() {
  let uuid = "";
  for (let i = 0; i < 32; i++) {
    const random = Math.random() * 16 | 0;
    if (i === 8 || i === 12 || i === 16 || i === 20)
      uuid += "-";
    uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
  }
  return uuid;
}

export function padLeft(str: any, len: number, ch?: string, isRight?: boolean) {
  ch = ch || " ";
  str = String(str);
  len = len - str.length;
  for (let i = -1; ++i < len;)
    str = isRight ? str + ch : ch + str;
  return str;
}

export function padRight(str: any, len: number, ch?: string) {
  return padLeft(str, len, ch, true);
}

export function remove(str: string, startIndex: number, count?: number) {
  if (startIndex >= str.length) {
    throw new Error("startIndex must be less than length of string");
  }
  if (typeof count === "number" && (startIndex + count) > str.length) {
    throw new Error("Index and count must refer to a location within the string.")
  }
  return str.slice(0, startIndex) + (typeof count === "number" ? str.substr(startIndex + count) : "");
}


export function replace(str: string, search: string, replace: string) {
  return str.replace(new RegExp(escape(search), "g"), replace);
}

export function replicate(n: number, x: string) {
  return initialize(n, () => x);
}

export function split(str: string, splitters: string[], count?: number, removeEmpty?: number) {
  count = typeof count == "number" ? count : null;
  removeEmpty = typeof removeEmpty == "number" ? removeEmpty : null;
  if (count < 0)
    throw new Error("Count cannot be less than zero");
  if (count === 0)
    return [];
  splitters = Array.isArray(splitters) ? splitters : getRestParams(arguments, 1);
  splitters = splitters.map(x => escape(x));
  splitters = splitters.length > 0 ? splitters : [" "];
  let m: RegExpExecArray;
  let i = 0;
  const splits: string[] = [];
  const reg = new RegExp(splitters.join("|"), "g");
  while ((count == null || count > 1) && (m = reg.exec(str)) !== null) {
    if (!removeEmpty || (m.index - i) > 0) {
      count = count != null ? count - 1 : count;
      splits.push(str.substring(i, m.index));
    }
    i = reg.lastIndex;
  }
  if (!removeEmpty || (str.length - i) > 0)
    splits.push(str.substring(i));
  return splits;
}

export function trim(str: string, side: "start" | "end" | "both", ...chars: string[]) {
  if (side == "both" && chars.length == 0)
    return str.trim();
  if (side == "start" || side == "both") {
    const reg = chars.length == 0 ? /^\s+/ : new RegExp("^[" + escape(chars.join("")) + "]+");
    str = str.replace(reg, "");
  }
  if (side == "end" || side == "both") {
    const reg = chars.length == 0 ? /\s+$/ : new RegExp("[" + escape(chars.join("")) + "]+$");
    str = str.replace(reg, "");
  }
  return str;
}