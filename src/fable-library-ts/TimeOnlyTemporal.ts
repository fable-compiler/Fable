import { FSharpRef } from "./Types.ts";
import { toInt64, fromFloat64, int64 } from "./BigInt.ts";
import { TimeSpan, fromTicks as TimeSpan_fromTicks, totalNanoseconds as TimeSpan_totalNanoseconds } from "./TimeSpanTemporal.ts";
import { Exception } from "./Util.ts";

declare global {
  namespace Temporal {
    class PlainTime {
      constructor(hour?: number, minute?: number, second?: number, millisecond?: number, microsecond?: number, nanosecond?: number);
      static compare(one: PlainTime, two: PlainTime): number;
      readonly hour: number;
      readonly minute: number;
      readonly second: number;
      readonly millisecond: number;
      readonly microsecond: number;
      readonly nanosecond: number;
      equals(other: PlainTime): boolean;
      toString(options?: { smallestUnit?: "minute" | "second", fractionalSecondDigits?: number }): string;
    }
  }
}

export type TimeOnly = Temporal.PlainTime;
export const PlainTime = Temporal.PlainTime;
export type PlainTime = Temporal.PlainTime;

// The generic equality/comparison/hashing helpers in Util.ts dispatch at runtime
// on .NET-style Equals/CompareTo/GetHashCode methods. Attach them so TimeOnly
// also works in generic contexts: records, tuples, erased generics, etc.
const proto = Temporal.PlainTime.prototype as any;
proto.Equals = function (this: TimeOnly, other: TimeOnly): boolean { return this.equals(other); };
proto.CompareTo = function (this: TimeOnly, other: TimeOnly): number { return Temporal.PlainTime.compare(this, other); };
proto.GetHashCode = function (this: TimeOnly): number { return hash(this); };

const nanosecondsPerDay = 86_400_000_000_000;

function totalNanoseconds(t: TimeOnly): number {
  return ((t.hour * 60 + t.minute) * 60 + t.second) * 1_000_000_000
    + t.millisecond * 1_000_000 + t.microsecond * 1_000 + t.nanosecond;
}

function fromNanoseconds(n: number): TimeOnly {
  return new Temporal.PlainTime(
    Math.floor(n / 3_600_000_000_000),
    Math.floor(n / 60_000_000_000) % 60,
    Math.floor(n / 1_000_000_000) % 60,
    Math.floor(n / 1_000_000) % 1000,
    Math.floor(n / 1_000) % 1000,
    n % 1000);
}

export function create(h: number = 0, m: number = 0, s: number = 0, ms: number = 0): TimeOnly {
  return new Temporal.PlainTime(h, m, s, ms);
}

export function fromTicks(ticks: number | bigint): TimeOnly {
  return fromNanoseconds(Number(BigInt(ticks) * 100n));
}

export function fromTimeSpan(timeSpan: TimeSpan): TimeOnly {
  const ns = Number(TimeSpan_totalNanoseconds(timeSpan));
  if (ns < 0 || ns >= nanosecondsPerDay)
    throw new Exception("The TimeSpan describes an unrepresentable TimeOnly.");

  return fromNanoseconds(ns);
}

export function fromDateTime(d: Temporal.PlainDateTime): TimeOnly {
  // Under the Temporal representation a DateTime is a PlainDateTime (kind-agnostic wall-clock)
  return new Temporal.PlainTime(d.hour, d.minute, d.second, d.millisecond, d.microsecond, d.nanosecond);
}

export function minValue(): TimeOnly {
  return new Temporal.PlainTime();
}

export function maxValue(): TimeOnly {
  // This is "23:59:59.9999999" (.NET tick precision)
  return new Temporal.PlainTime(23, 59, 59, 999, 999, 900);
}

export function ticks(t: TimeOnly): int64 {
  return toInt64(fromFloat64(totalNanoseconds(t) / 100));
}

export function toTimeSpan(t: TimeOnly): TimeSpan {
  // TimeOnly is tick-precise, so the nanosecond count is always a multiple of 100.
  return TimeSpan_fromTicks(Math.round(totalNanoseconds(t) / 100));
}

function addNanoseconds(t: TimeOnly, deltaNs: number, wrappedDays?: FSharpRef<number>): TimeOnly {
  const totalNs = totalNanoseconds(t) + deltaNs;
  const days = Math.floor(totalNs / nanosecondsPerDay);

  if (wrappedDays !== undefined) {
    wrappedDays.contents = days;
  }

  return fromNanoseconds(totalNs - days * nanosecondsPerDay);
}

export function add(t: TimeOnly, ts: TimeSpan, wrappedDays?: FSharpRef<number>): TimeOnly {
  return addNanoseconds(t, Number(TimeSpan_totalNanoseconds(ts)), wrappedDays);
}

export function addHours(t: TimeOnly, h: number): TimeOnly {
  return addNanoseconds(t, Math.round(h * 3_600_000_000_000));
}

export function addMinutes(t: TimeOnly, m: number): TimeOnly {
  return addNanoseconds(t, Math.round(m * 60_000_000_000));
}

export function isBetween(t: TimeOnly, start: TimeOnly, end: TimeOnly): boolean {
  return Temporal.PlainTime.compare(start, end) <= 0
    ? (Temporal.PlainTime.compare(start, t) <= 0 && Temporal.PlainTime.compare(end, t) > 0)
    : (Temporal.PlainTime.compare(start, t) <= 0 || Temporal.PlainTime.compare(end, t) > 0);
}

export function equals(x: TimeOnly, y: TimeOnly): boolean {
  return x.equals(y);
}

export function compare(x: TimeOnly, y: TimeOnly): number {
  return Temporal.PlainTime.compare(x, y);
}

export function hash(t: TimeOnly): number {
  return totalNanoseconds(t) % 2147483647;
}

export function op_Subtraction(left: TimeOnly, right: TimeOnly): TimeSpan {
  // Returns the elapsed TimeSpan, wrapping around midnight
  const ns = (totalNanoseconds(left) - totalNanoseconds(right) + nanosecondsPerDay) % nanosecondsPerDay;
  return TimeSpan_fromTicks(Math.round(ns / 100));
}

export function toString(t: TimeOnly, format = "t", _provider?: any): string {
  switch (format) {
    case "t":
      return t.toString({ smallestUnit: "minute" });
    case "r":
    case "R":
    case "T":
      return t.toString({ smallestUnit: "second" });
    case "o":
    case "O":
      // .NET tick precision is 7 fractional digits
      return t.toString({ fractionalSecondDigits: 7 });
    default:
      throw new Exception("Custom formats are not supported");
  }
}

export function parse(str: string): TimeOnly {
  // Allowed format types:
  // hh:mm
  // hh:mm:ss
  // hh:mm:ss.fffffff
  const r = /^\s*([0-1]?\d|2[0-3])\s*:\s*([0-5]?\d)(\s*:\s*([0-5]?\d)(\.(\d+))?)?\s*$/.exec(str);
  if (r != null && r[1] != null && r[2] != null) {
    let ms = 0;
    let s = 0;
    const h = +r[1];
    const m = +r[2];
    if (r[4] != null) {
      s = +r[4];
    }
    if (r[6] != null) {
      // Depending on the number of decimals passed, we need to adapt the numbers
      switch (r[6].length) {
        case 1: ms = +r[6] * 100; break;
        case 2: ms = +r[6] * 10; break;
        case 3: ms = +r[6]; break;
        case 4: ms = +r[6] / 10; break;
        case 5: ms = +r[6] / 100; break;
        case 6: ms = +r[6] / 1000; break;
        default: ms = +r[6].substring(0, 7) / 10000; break;
      }
    }
    return create(h, m, s, Math.trunc(ms));
  }

  throw new Exception(`String '${str}' was not recognized as a valid TimeOnly.`);
}

export function tryParse(v: string, defValue: FSharpRef<TimeOnly>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
