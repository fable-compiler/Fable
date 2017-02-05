import { compare as utilCompare } from "./Util"
import * as Long from "./Long"

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
      d = arguments[0], h = arguments[1], m = arguments[2], s = arguments[3], ms = arguments[4] || 0;
      break;
  }
  return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
}

export function fromTicks(ticks: Long.Long) {
  return ticks.div(10000).toNumber();
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
  return Math.floor(<number>ts / 86400000);
}

export function hours(ts: number) {
  return Math.floor(<number>ts % 86400000 / 3600000);
}

export function minutes(ts: number) {
  return Math.floor(<number>ts % 3600000 / 60000);
}

export function seconds(ts: number) {
  return Math.floor(<number>ts % 60000 / 1000);
}

export function milliseconds(ts: number) {
  return Math.floor(<number>ts % 1000);
}

export function ticks(ts: number) {
  return Long.fromNumber(ts).mul(10000);
}

export function totalDays(ts: number) {
  return <number>ts / 86400000;
}

export function totalHours(ts: number) {
  return <number>ts / 3600000;
}

export function totalMinutes(ts: number) {
  return <number>ts / 60000;
}

export function totalSeconds(ts: number) {
  return <number>ts / 1000;
}

export function negate(ts: number) {
  return <number>ts * -1;
}

export function add(ts1: number, ts2: number) {
  return <number>ts1 + <number>ts2;
}

export function subtract(ts1: number, ts2: number) {
  return <number>ts1 - <number>ts2;
}

export function compare(x: number, y: number) {
  return utilCompare(x, y)
}

export function compareTo(x: number, y: number) {
  return utilCompare(x, y)
}

export function duration(x: number) {
  return Math.abs(x as number)
}
