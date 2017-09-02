import * as Long from "./Long";
import { create as timeSpanCreate } from "./TimeSpan";
import { compare as utilCompare } from "./Util";

// Don't change, this corresponds to DateTime.Kind
// enum values in .NET
export const enum DateKind {
  Unspecified = 0,
  UTC = 1,
  Local = 2,
}

export function minValue() {
  return parse(-8640000000000000, 1);
}

export function maxValue() {
  return parse(8640000000000000, 1);
}

/* tslint:disable */
export function parse(v?: any, kind?: DateKind): any {
  if (kind == null) {
    kind = typeof v === "string" && v.slice(-1) === "Z" ? DateKind.UTC : DateKind.Local;
  }
  let date = (v == null) ? new Date() : new Date(v);
  if (isNaN(date.getTime())) {
    // Check if this is a time-only string, which JS Date parsing cannot handle (see #1045)
    if (typeof v === "string" && /^(?:[01]?\d|2[0-3]):(?:[0-5]?\d)(?::[0-5]?\d(?:\.\d+)?)?(?:\s*[AaPp][Mm])?$/.test(v)) {
      const d = new Date();
      date = new Date(d.getFullYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate() + " " + v);
    }
    else {
      throw new Error("The string is not a valid Date.");
    }
  }
  if (kind === 2 /* Local */) {
    (date as any).kind = kind;
  }
  return date;
}
/* tslint:enable */

export function tryParse(v: any): [boolean, Date] {
  try {
    return [true, parse(v)];
  } catch (_err) {
    return [false, minValue()];
  }
}

export function create(
  year: number, month: number, day: number,
  h: number = 0, m: number = 0, s: number = 0,
  ms: number = 0, kind: DateKind = DateKind.Local) {
  let date: Date;
  if (kind === DateKind.Local) {
    date = new Date(year, month - 1, day, h, m, s, ms);
    (date as any).kind = kind;
  } else {
    date = new Date(Date.UTC(year, month - 1, day, h, m, s, ms));
  }

  if (year <= 99) {
    date.setFullYear(year);
  }

  if (isNaN(date.getTime())) {
    throw new Error("The parameters describe an unrepresentable Date.");
  }
  return date;
}

export function now() {
  return parse();
}

export function utcNow() {
  return parse(null, 1);
}

export function today() {
  return date(now());
}

export function isLeapYear(year: number) {
  return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
}

export function daysInMonth(year: number, month: number) {
  return month === 2
    ? isLeapYear(year) ? 29 : 28
    : month >= 8 ? month % 2 === 0 ? 31 : 30 : month % 2 === 0 ? 30 : 31;
}

export function toUniversalTime(d: Date) {
  return (d as any).kind === DateKind.Local ? new Date(d.getTime()) : d;
}

export function toLocalTime(d: Date) {
  if ((d as any).kind === DateKind.Local) {
    return d;
  } else {
    const d2 = new Date(d.getTime());
    (d2 as any).kind = DateKind.Local;
    return d2;
  }
}

export function timeOfDay(d: Date) {
  return timeSpanCreate(0, hour(d), minute(d), second(d), millisecond(d));
}

export function date(d: Date) {
  return create(year(d), month(d), day(d), 0, 0, 0, 0, (d as any).kind || DateKind.UTC);
}

export function kind(d: Date) {
  return (d as any).kind || DateKind.UTC;
}

export function day(d: Date) {
  return (d as any).kind === DateKind.Local ? d.getDate() : d.getUTCDate();
}

export function hour(d: Date) {
  return (d as any).kind === DateKind.Local ? d.getHours() : d.getUTCHours();
}

export function millisecond(d: Date) {
  return (d as any).kind === DateKind.Local ? d.getMilliseconds() : d.getUTCMilliseconds();
}

export function minute(d: Date) {
  return (d as any).kind === DateKind.Local ? d.getMinutes() : d.getUTCMinutes();
}

export function month(d: Date) {
  return ((d as any).kind === DateKind.Local ? d.getMonth() : d.getUTCMonth()) + 1;
}

export function second(d: Date) {
  return (d as any).kind === DateKind.Local ? d.getSeconds() : d.getUTCSeconds();
}

export function year(d: Date) {
  return (d as any).kind === DateKind.Local ? d.getFullYear() : d.getUTCFullYear();
}

export function dayOfWeek(d: Date) {
  return (d as any).kind === DateKind.Local ? d.getDay() : d.getUTCDay();
}

export function ticks(d: Date) {
  return Long.fromNumber(d.getTime())
    .add(62135596800000) // UnixEpochMilliseconds
    .sub((d as any).kind === DateKind.Local ? d.getTimezoneOffset() * 60 * 1000 : 0)
    .mul(10000);
}

export function ofTicks(ticks: Long.Long) {
  return parse(ticks.div(10000).sub(62135596800000).toNumber(), DateKind.UTC);
}

export function toBinary(d: Date) {
  return ticks(d);
}

export function dayOfYear(d: Date) {
  const _year = year(d);
  const _month = month(d);
  let _day = day(d);
  for (let i = 1; i < _month; i++) {
    _day += daysInMonth(_year, i);
  }
  return _day;
}

export function add(d: Date, ts: number) {
  return parse(d.getTime() + ts as number, (d as any).kind || DateKind.UTC);
}

export function addDays(d: Date, v: number) {
  return parse(d.getTime() + v * 86400000, (d as any).kind || DateKind.UTC);
}

export function addHours(d: Date, v: number) {
  return parse(d.getTime() + v * 3600000, (d as any).kind || DateKind.UTC);
}

export function addMinutes(d: Date, v: number) {
  return parse(d.getTime() + v * 60000, (d as any).kind || DateKind.UTC);
}

export function addSeconds(d: Date, v: number) {
  return parse(d.getTime() + v * 1000, (d as any).kind || DateKind.UTC);
}

export function addMilliseconds(d: Date, v: number) {
  return parse(d.getTime() + v, (d as any).kind || DateKind.UTC);
}

export function addTicks(d: Date, t: Long.Long) {
  return parse(Long.fromNumber(d.getTime()).add(t.div(10000)).toNumber(), (d as any).kind || DateKind.UTC);
}

export function addYears(d: Date, v: number) {
  const newMonth = month(d);
  const newYear = year(d) + v;
  const _daysInMonth = daysInMonth(newYear, newMonth);
  const newDay = Math.min(_daysInMonth, day(d));
  return create(newYear, newMonth, newDay, hour(d), minute(d), second(d),
    millisecond(d), (d as any).kind || DateKind.UTC);
}

export function addMonths(d: Date, v: number) {
  let newMonth = month(d) + v;
  let newMonth_ = 0;
  let yearOffset = 0;
  if (newMonth > 12) {
    newMonth_ = newMonth % 12;
    yearOffset = Math.floor(newMonth / 12);
    newMonth = newMonth_;
  } else if (newMonth < 1) {
    newMonth_ = 12 + newMonth % 12;
    yearOffset = Math.floor(newMonth / 12) + (newMonth_ === 12 ? -1 : 0);
    newMonth = newMonth_;
  }
  const newYear = year(d) + yearOffset;
  const _daysInMonth = daysInMonth(newYear, newMonth);
  const newDay = Math.min(_daysInMonth, day(d));
  return create(newYear, newMonth, newDay, hour(d), minute(d), second(d),
    millisecond(d), (d as any).kind || DateKind.UTC);
}

export function subtract(d: Date, that: Date | number) {
  return typeof that === "number"
    ? parse(d.getTime() - that as number, (d as any).kind || DateKind.UTC)
    : d.getTime() - (that as Date).getTime();
}

export function toLongDateString(d: Date) {
  return d.toDateString();
}

export function toShortDateString(d: Date) {
  return d.toLocaleDateString();
}

export function toLongTimeString(d: Date) {
  return d.toLocaleTimeString();
}

export function toShortTimeString(d: Date) {
  return d.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
}

export function equals(d1: Date, d2: Date) {
  return d1.getTime() === d2.getTime();
}

export function compare(x: Date, y: Date) {
  return utilCompare(x, y);
}

export function compareTo(x: Date, y: Date) {
  return utilCompare(x, y);
}

export function op_Addition(x: Date, y: number) {
  return add(x, y);
}

export function op_Subtraction(x: Date, y: number | Date) {
  return subtract(x, y);
}

export function isDaylightSavingTime(x: Date) {
  const jan = new Date(x.getFullYear(), 0, 1);
  const jul = new Date(x.getFullYear(), 6, 1);
  return isDST(jan.getTimezoneOffset(), jul.getTimezoneOffset(), x.getTimezoneOffset());
}

function isDST(janOffset: number, julOffset: number, tOffset: number) {
  return Math.min(janOffset, julOffset) === tOffset;
}
