import { FSharpRef } from "./Types.ts";
import { toInt64, int64 } from "./BigInt.ts";
import { Exception, padWithZeros, padLeftAndRightWithZeros } from "./Util.ts";

declare global {
  namespace Temporal {
    class Duration {
      constructor(years?: number, months?: number, weeks?: number, days?: number, hours?: number, minutes?: number,
        seconds?: number, milliseconds?: number, microseconds?: number, nanoseconds?: number);
      static compare(one: Duration, two: Duration): number;
      readonly days: number;
      readonly hours: number;
      readonly minutes: number;
      readonly seconds: number;
      readonly milliseconds: number;
      readonly microseconds: number;
      readonly nanoseconds: number;
      readonly sign: number;
      negated(): Duration;
      abs(): Duration;
      total(options: { unit: "days" | "hours" | "minutes" | "seconds" | "milliseconds" }): number;
    }
  }
}

export type TimeSpan = Temporal.Duration;
export const Duration = Temporal.Duration;
export type Duration = Temporal.Duration;

// The generic equality/comparison/hashing helpers in Util.ts dispatch at runtime
// on .NET-style Equals/CompareTo/GetHashCode methods. Attach them so TimeSpan
// also works in generic contexts: records, tuples, erased generics, etc.
const proto = Temporal.Duration.prototype as any;
proto.Equals = function (this: TimeSpan, other: TimeSpan): boolean { return Temporal.Duration.compare(this, other) === 0; };
proto.CompareTo = function (this: TimeSpan, other: TimeSpan): number { return Temporal.Duration.compare(this, other); };
proto.GetHashCode = function (this: TimeSpan): number { return hash(this); };

const nsPerMillisecond = 1_000_000n;
const nsPerSecond = 1_000_000_000n;
const nsPerMinute = 60_000_000_000n;
const nsPerHour = 3_600_000_000_000n;
const nsPerDay = 86_400_000_000_000n;

export function totalNanoseconds(ts: TimeSpan): bigint {
  return BigInt(ts.days) * nsPerDay
    + BigInt(ts.hours) * nsPerHour
    + BigInt(ts.minutes) * nsPerMinute
    + BigInt(ts.seconds) * nsPerSecond
    + BigInt(ts.milliseconds) * nsPerMillisecond
    + BigInt(ts.microseconds) * 1000n
    + BigInt(ts.nanoseconds);
}

function fromNanoseconds(totalNs: bigint): TimeSpan {
  const negative = totalNs < 0n;
  let n = negative ? -totalNs : totalNs;
  const days = Number(n / nsPerDay); n %= nsPerDay;
  const hours = Number(n / nsPerHour); n %= nsPerHour;
  const minutes = Number(n / nsPerMinute); n %= nsPerMinute;
  const seconds = Number(n / nsPerSecond); n %= nsPerSecond;
  const milliseconds = Number(n / nsPerMillisecond); n %= nsPerMillisecond;
  const microseconds = Number(n / 1000n); n %= 1000n;
  const d = new Temporal.Duration(0, 0, 0, days, hours, minutes, seconds, milliseconds, microseconds, Number(n));
  return negative ? d.negated() : d;
}

// Converts a possibly fractional unit count into an exact nanosecond amount,
// keeping the integer part exact for the full Int64 tick range.
function unitToNanoseconds(value: number, nsPerUnit: bigint): bigint {
  const whole = Math.trunc(value);
  const frac = value - whole;
  return BigInt(whole) * nsPerUnit + BigInt(Math.round(frac * Number(nsPerUnit)));
}

export function create(d: number = 0, h: number = 0, m: number = 0, s: number = 0, ms: number = 0): TimeSpan {
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
  return fromNanoseconds(
    unitToNanoseconds(d, nsPerDay) + unitToNanoseconds(h, nsPerHour) + unitToNanoseconds(m, nsPerMinute)
    + unitToNanoseconds(s, nsPerSecond) + unitToNanoseconds(ms, nsPerMillisecond));
}

export function fromTicks(ticks: number | bigint): TimeSpan {
  return fromNanoseconds(BigInt(ticks) * 100n);
}

export function zero(): TimeSpan {
  return new Temporal.Duration();
}

export function fromDays(d: number, h: number = 0, m: bigint = 0n, s: bigint = 0n, ms: bigint = 0n): TimeSpan {
  return create(d, h, Number(m), Number(s), Number(ms));
}

export function fromHours(h: number, m: bigint = 0n, s: bigint = 0n, ms: bigint = 0n): TimeSpan {
  return create(0, h, Number(m), Number(s), Number(ms));
}

export function fromMinutes(m: number | bigint, s: bigint = 0n, ms: bigint = 0n): TimeSpan {
  return create(0, 0, Number(m), Number(s), Number(ms));
}

export function fromSeconds(s: number | bigint, ms: bigint = 0n): TimeSpan {
  return create(0, 0, 0, Number(s), Number(ms));
}

export function fromMilliseconds(ms: number | bigint): TimeSpan {
  return fromNanoseconds(unitToNanoseconds(Number(ms), nsPerMillisecond));
}

export function ticks(ts: TimeSpan): int64 {
  return toInt64(totalNanoseconds(ts) / 100n);
}

// .NET computes each total as (double)Ticks / TicksPerUnit; match that exactly
// (Temporal's own .total() balances differently and can be 1 ULP off).
function totalTicks(ts: TimeSpan): number {
  return Number(totalNanoseconds(ts) / 100n);
}

export function totalDays(ts: TimeSpan): number {
  return totalTicks(ts) / 864_000_000_000;
}

export function totalHours(ts: TimeSpan): number {
  return totalTicks(ts) / 36_000_000_000;
}

export function totalMinutes(ts: TimeSpan): number {
  return totalTicks(ts) / 600_000_000;
}

export function totalSeconds(ts: TimeSpan): number {
  return totalTicks(ts) / 10_000_000;
}

export function totalMilliseconds(ts: TimeSpan): number {
  return totalTicks(ts) / 10_000;
}

export function negate(ts: TimeSpan): TimeSpan {
  return ts.negated();
}

export function add(ts1: TimeSpan, ts2: TimeSpan): TimeSpan {
  return fromNanoseconds(totalNanoseconds(ts1) + totalNanoseconds(ts2));
}

export function subtract(ts1: TimeSpan, ts2: TimeSpan): TimeSpan {
  return fromNanoseconds(totalNanoseconds(ts1) - totalNanoseconds(ts2));
}

export function multiply(ts: TimeSpan, factor: number): TimeSpan {
  return fromNanoseconds(BigInt(Math.round(Number(totalNanoseconds(ts)) * factor)));
}

export function divide(ts: TimeSpan, b: number | TimeSpan): TimeSpan | number {
  return typeof b === "number"
    ? fromNanoseconds(BigInt(Math.round(Number(totalNanoseconds(ts)) / b)))
    : Number(totalNanoseconds(ts)) / Number(totalNanoseconds(b));
}

export const op_Addition = add;
export const op_Subtraction = subtract;
export const op_Multiply = multiply;
export const op_Division = divide;
export const op_UnaryNegation = negate;

export function compare(x: TimeSpan, y: TimeSpan): number {
  return Temporal.Duration.compare(x, y);
}

export const compareTo = compare;

export function equals(x: TimeSpan, y: TimeSpan): boolean {
  return Temporal.Duration.compare(x, y) === 0;
}

export function hash(ts: TimeSpan): number {
  return Number(totalNanoseconds(ts) % 2147483647n);
}

export function duration(ts: TimeSpan): TimeSpan {
  return ts.abs();
}

export function toString(ts: TimeSpan, format = "c", _provider?: any): string {
  if (["c", "g", "G"].indexOf(format) === -1) {
    throw new Exception("Custom formats are not supported");
  }
  const d = Math.abs(ts.days);
  const h = Math.abs(ts.hours);
  const m = Math.abs(ts.minutes);
  const s = Math.abs(ts.seconds);
  const ms = Math.abs(ts.milliseconds);
  const sign = ts.sign < 0 ? "-" : "";
  return `${sign}${d === 0 && (format === "c" || format === "g") ? "" : format === "c" ? d + "." : d + ":"}${format === "g" ? h : padWithZeros(h, 2)}:${padWithZeros(m, 2)}:${padWithZeros(s, 2)}${ms === 0 && (format === "c" || format === "g") ? "" : format === "g" ? "." + padWithZeros(ms, 3) : "." + padLeftAndRightWithZeros(ms, 3, 7)}`;
}

export function parse(str: string): TimeSpan {
  const firstDot = str.search("\\.");
  const firstColon = str.search("\\:");
  if (firstDot === -1 && firstColon === -1) { // There is only a day ex: 4
    const d = parseInt(str, 0);
    if (isNaN(d)) {
      throw new Exception(`String '${str}' was not recognized as a valid TimeSpan.`);
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
            throw new Exception(`String '${str}' was not recognized as a valid TimeSpan.`);
        }
      }
      const ts = create(d, h, m, s, ms);
      return sign < 0 ? negate(ts) : ts;
    }
  }
  throw new Exception(`String '${str}' was not recognized as a valid TimeSpan.`);
}

export function tryParse(v: string, defValue: FSharpRef<TimeSpan>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
