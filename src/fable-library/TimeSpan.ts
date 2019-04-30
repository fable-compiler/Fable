import { fromNumber, op_Division, op_Multiply, toNumber } from "./Long";
import { comparePrimitives, padLeftAndRightWithZeros, padWithZeros } from "./Util";

// TimeSpan in runtime just becomes a number representing milliseconds

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

export function fromTicks(ticks: number /* Long */) {
  return toNumber(op_Division(ticks, 10000));
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

export function days(ts: number) {
  return Math.floor(ts / 86400000);
}

export function hours(ts: number) {
  return Math.floor(ts % 86400000 / 3600000);
}

export function minutes(ts: number) {
  return Math.floor(ts % 3600000 / 60000);
}

export function seconds(ts: number) {
  return Math.floor(ts % 60000 / 1000);
}

export function milliseconds(ts: number) {
  return Math.floor(ts % 1000);
}

export function ticks(ts: number /* Long */) {
  return op_Multiply(fromNumber(ts), 10000);
}

export function totalDays(ts: number) {
  return ts / 86400000;
}

export function totalHours(ts: number) {
  return ts / 3600000;
}

export function totalMinutes(ts: number) {
  return ts / 60000;
}

export function totalSeconds(ts: number) {
  return ts / 1000;
}

export function negate(ts: number) {
  return ts * -1;
}

export function add(ts1: number, ts2: number) {
  return ts1 + ts2;
}

export function subtract(ts1: number, ts2: number) {
  return ts1 - ts2;
}

export const op_Addition = add;
export const op_Subtraction = subtract;

export const compare = comparePrimitives;
export const compareTo = comparePrimitives;

export function duration(x: number) {
  return Math.abs(x);
}

export function toString(ts: number, format = "c") {
  if (["c", "g", "G"].indexOf(format) === -1) {
    throw new Error("Custom formats are not supported");
  }
  const d = days(ts);
  const h = hours(ts);
  const m = minutes(ts);
  const s = seconds(ts);
  const ms = milliseconds(ts);
  // tslint:disable-next-line:max-line-length
  return `${d === 0 && (format === "c" || format === "g") ? "" : format === "c" ? d + "." : d + ":" }${format === "g" ? h : padWithZeros(h, 2)}:${padWithZeros(m, 2)}:${padWithZeros(s, 2)}${ms === 0 && (format === "c" || format === "g") ? "" : format === "g" ? "." + padWithZeros(ms, 3) : "." + padLeftAndRightWithZeros(ms, 3, 7)}`;
}

export function parse(str: string) {
  const firstDot = str.search("\\.");
  const firstColon = str.search("\\:");
  if (firstDot === -1 && firstColon === -1 ) { // There is only a day ex: 4
    const d = parseInt(str, 0);
    if (isNaN(d)) {
      throw new Error("String was not recognized as a valid TimeSpan.");
    } else {
      return create(d, 0, 0, 0, 0);
    }
  }
  if (firstColon > 0) { // process time part
    // tslint:disable-next-line:max-line-length
    const r = /^((\d+)\.)?(?:0*)([0-9]|0[0-9]|1[0-9]|2[0-3]):(?:0*)([0-5][0-9]|[0-9])(:(?:0*)([0-5][0-9]|[0-9]))?\.?(\d+)?$/.exec(str);
    if (r != null && r[3] != null && r[4] != null) {
      let d = 0;
      let ms = 0;
      let s = 0;
      const h = +r[3];
      const m = +r[4];
      if (r[2] != null) {
        d = +r[2];
      }
      if (r[6] != null) {
        s = +r[6];
      }
      if (r[7] != null) {
        ms = +r[7];
      }
      return create(d, h, m, s, ms);
    }
  }
  throw new Error("String was not recognized as a valid TimeSpan.");
}

export function tryParse(v: any): [boolean, number] {
  try {
    return [true, parse(v)];
  } catch (_err) {
    return [false, 0];
  }
}
