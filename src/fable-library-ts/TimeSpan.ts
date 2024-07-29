import { FSharpRef } from "./Types.js";
import { comparePrimitives, padLeftAndRightWithZeros, padWithZeros } from "./Util.js";
import { toInt64, fromFloat64 } from "./BigInt.js";

// TimeSpan in runtime just becomes a number representing milliseconds
export type TimeSpan = number;

/**
 * Calls:
 * - `Math.ceil` if the `value` is **negative**
 * - `Math.floor` if the `value` is **positive**
 * @param value Value to round
 */
function signedRound(value: number) {
  return value < 0 ? Math.ceil(value) : Math.floor(value);
}

export function create(d: number = 0, h: number = 0, m: number = 0, s: number = 0, ms: number = 0) {
  switch (arguments.length) {
    case 1:
      // ticks
      return fromTicks(arguments[0]);
    case 3:
      // h,m,s
      d = 0, h = arguments[0], m = arguments[1], s = arguments[2], ms = 0;
      break;
    default:
      // d,h,m,s,ms
      break;
  }
  return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
}

export function fromTicks(ticks: number | bigint) {
  return Number(BigInt(ticks) / 10000n);
}

export function fromDays(d: number) {
  return create(d, 0, 0, 0);
}

export function fromHours(h: number) {
  return create(h, 0, 0);
}

export function fromMinutes(m: number) {
  return create(0, m, 0);
}

export function fromSeconds(s: number) {
  return create(0, 0, s);
}

export function days(ts: TimeSpan) {
  return signedRound(ts / 86400000);
}

export function hours(ts: TimeSpan) {
  return signedRound(ts % 86400000 / 3600000);
}

export function minutes(ts: TimeSpan) {
  return signedRound(ts % 3600000 / 60000);
}

export function seconds(ts: TimeSpan) {
  return signedRound(ts % 60000 / 1000);
}

export function milliseconds(ts: TimeSpan) {
  return signedRound(ts % 1000);
}

export function ticks(ts: TimeSpan) {
  return toInt64(fromFloat64(ts * 10000));
}

export function totalDays(ts: TimeSpan) {
  return ts / 86400000;
}

export function totalHours(ts: TimeSpan) {
  return ts / 3600000;
}

export function totalMinutes(ts: TimeSpan) {
  return ts / 60000;
}

export function totalSeconds(ts: TimeSpan) {
  return ts / 1000;
}

export function negate(ts: TimeSpan) {
  return ts * -1;
}

export function add(ts1: number, ts2: number) {
  return ts1 + ts2;
}

export function subtract(ts1: number, ts2: number) {
  return ts1 - ts2;
}

export function multiply(ts: TimeSpan, factor: number) {
  return ts * factor;
}

export function divide(ts: TimeSpan, b: number) {
  return ts / b;
}

export const op_Addition = add;
export const op_Subtraction = subtract;
export const op_Multiply = multiply;
export const op_Division = divide;

export const compare = comparePrimitives;
export const compareTo = comparePrimitives;

export function duration(x: number) {
  return Math.abs(x);
}

export function toString(ts: TimeSpan, format = "c", _provider?: any) {
  if (["c", "g", "G"].indexOf(format) === -1) {
    throw new Error("Custom formats are not supported");
  }
  const d = Math.abs(days(ts));
  const h = Math.abs(hours(ts));
  const m = Math.abs(minutes(ts));
  const s = Math.abs(seconds(ts));
  const ms = Math.abs(milliseconds(ts));
  const sign = ts < 0 ? "-" : "";
  return `${sign}${d === 0 && (format === "c" || format === "g") ? "" : format === "c" ? d + "." : d + ":"}${format === "g" ? h : padWithZeros(h, 2)}:${padWithZeros(m, 2)}:${padWithZeros(s, 2)}${ms === 0 && (format === "c" || format === "g") ? "" : format === "g" ? "." + padWithZeros(ms, 3) : "." + padLeftAndRightWithZeros(ms, 3, 7)}`;
}

export function parse(str: string) {
  const firstDot = str.search("\\.");
  const firstColon = str.search("\\:");
  if (firstDot === -1 && firstColon === -1) { // There is only a day ex: 4
    const d = parseInt(str, 0);
    if (isNaN(d)) {
      throw new Error(`String '${str}' was not recognized as a valid TimeSpan.`);
    } else {
      return create(d, 0, 0, 0, 0);
    }
  }
  if (firstColon > 0) { // process time part
    // WIP: (-?)(((\d+)\.)?([0-9]|0[0-9]|1[0-9]|2[0-3]):(\d+)(:\d+(\.\d{1,7})?)?|\d+(?:(?!\.)))
    const r = /^(-?)((\d+)\.)?(?:0*)([0-9]|0[0-9]|1[0-9]|2[0-3]):(?:0*)([0-5][0-9]|[0-9])(:(?:0*)([0-5][0-9]|[0-9]))?\.?(\d+)?$/.exec(str);
    if (r != null && r[4] != null && r[5] != null) {
      let d = 0;
      let ms = 0;
      let s = 0;
      const sign = r[1] != null && r[1] === "-" ? -1 : 1;
      const h = +r[4];
      const m = +r[5];
      if (r[3] != null) {
        d = +r[3];
      }
      if (r[7] != null) {
        s = +r[7];
      }
      if (r[8] != null) {
        // Depending on the number of decimals passed, we need to adapt the numbers
        switch (r[8].length) {
          case 1: ms = +r[8] * 100; break;
          case 2: ms = +r[8] * 10; break;
          case 3: ms = +r[8]; break;
          case 4: ms = +r[8] / 10; break;
          case 5: ms = +r[8] / 100; break;
          case 6: ms = +r[8] / 1000; break;
          case 7: ms = +r[8] / 10000; break;
          default:
            throw new Error(`String '${str}' was not recognized as a valid TimeSpan.`);
        }
      }
      return sign * create(d, h, m, s, ms);
    }
  }
  throw new Error(`String '${str}' was not recognized as a valid TimeSpan.`);
}

export function tryParse(v: string, defValue: FSharpRef<number>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
