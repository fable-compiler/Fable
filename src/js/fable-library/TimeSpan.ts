import { fromNumber, op_Division, op_Multiply, toNumber } from "./Long";
import { comparePrimitives } from "./Util";

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
  return Math.floor(ts as number / 86400000);
}

export function hours(ts: number) {
  return Math.floor(ts as number % 86400000 / 3600000);
}

export function minutes(ts: number) {
  return Math.floor(ts as number % 3600000 / 60000);
}

export function seconds(ts: number) {
  return Math.floor(ts as number % 60000 / 1000);
}

export function milliseconds(ts: number) {
  return Math.floor(ts as number % 1000);
}

export function ticks(ts: number /* Long */) {
  return op_Multiply(fromNumber(ts), 10000);
}

export function totalDays(ts: number) {
  return ts as number / 86400000;
}

export function totalHours(ts: number) {
  return ts as number / 3600000;
}

export function totalMinutes(ts: number) {
  return ts as number / 60000;
}

export function totalSeconds(ts: number) {
  return ts as number / 1000;
}

export function negate(ts: number) {
  return ts as number * -1;
}

export function add(ts1: number, ts2: number) {
  return ts1 as number + ts2 as number;
}

export function subtract(ts1: number, ts2: number) {
  return ts1 as number - ts2 as number;
}

export const compare = comparePrimitives;
export const compareTo = comparePrimitives;

export function duration(x: number) {
  return Math.abs(x as number);
}
