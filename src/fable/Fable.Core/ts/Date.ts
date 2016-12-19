import { create as timeSpanCreate } from "./TimeSpan"
import { compare as utilCompare } from "./Util"
import * as Long from "./Long"

export const enum DateKind {
  UTC = 1,
  Local = 2
}

function __changeKind(d: Date, kind: DateKind) {
  let d2: Date;
  return (<any>d).kind == kind ? d : (d2 = new Date(d.getTime()), (<any>d2).kind = kind, d2);
}

function __getValue(d: Date, key: string): number {
  return (<any>d)[((<any>d).kind == DateKind.UTC ? "getUTC" : "get") + key]();
}

export function minValue() {
  return parse(-8640000000000000, 1);
}

export function maxValue() {
  return parse(8640000000000000, 1);
}

export function parse(v?: any, kind?: DateKind): any {
  const date = (v == null) ? new Date() : new Date(v);
  if (isNaN(date.getTime()))
    throw new Error("The string is not a valid Date.");
  (<any>date).kind = kind ||
    (typeof v == "string" && v.slice(-1) == "Z" ? DateKind.UTC : DateKind.Local);
  return date;
}

export function tryParse(v: any): [boolean, Date] {
  try {
    return [true, parse(v)];
  }
  catch (_err) {
    return [false, minValue()];
  }
}

export function create(year: number, month: number, day: number, h: number = 0, m: number = 0, s: number = 0, ms: number = 0, kind: DateKind = DateKind.Local) {
  const date: Date = (kind === DateKind.UTC)
    ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms))
    : new Date(year, month - 1, day, h, m, s, ms);
  if (isNaN(date.getTime()))
    throw new Error("The parameters describe an unrepresentable Date.");
  (<any>date).kind = kind;
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
  return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
}

export function daysInMonth(year: number, month: number) {
  return month == 2
    ? isLeapYear(year) ? 29 : 28
    : month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
}

export function toUniversalTime(d: Date) {
  return __changeKind(d, 1);
}

export function toLocalTime(d: Date) {
  return __changeKind(d, 2);
}

export function timeOfDay(d: Date) {
  return timeSpanCreate(0, hour(d), minute(d), second(d), millisecond(d));
}

export function date(d: Date) {
  return create(year(d), month(d), day(d), 0, 0, 0, 0, (<any>d).kind);
}

export function day(d: Date) {
  return __getValue(d, "Date");
}

export function hour(d: Date) {
  return __getValue(d, "Hours");
}

export function millisecond(d: Date) {
  return __getValue(d, "Milliseconds");
}

export function minute(d: Date) {
  return __getValue(d, "Minutes");
}

export function month(d: Date) {
  return __getValue(d, "Month") + 1;
}

export function second(d: Date) {
  return __getValue(d, "Seconds");
}

export function year(d: Date) {
  return __getValue(d, "FullYear");
}

export function ticks(d: Date) {
  return Long.fromNumber(d.getTime())
             .add(62135596800000) // UnixEpochMilliseconds
             .sub((<any>d).kind == DateKind.Local ? d.getTimezoneOffset()*60*1000 : 0)
             .mul(10000);
}

export function toBinary(d: Date) {
  return ticks(d);
}

export function dayOfWeek(d: Date) {
  return __getValue(d, "Day");
}

export function dayOfYear(d: Date) {
  const _year = year(d);
  const _month = month(d);
  let _day = day(d);
  for (let i = 1; i < _month; i++)
    _day += daysInMonth(_year, i);
  return _day;
}

export function add(d: Date, ts: number) {
  return parse(d.getTime() + <number>ts, (<any>d).kind);
}

export function addDays(d: Date, v: number) {
  return parse(d.getTime() + v * 86400000, (<any>d).kind);
}

export function addHours(d: Date, v: number) {
  return parse(d.getTime() + v * 3600000, (<any>d).kind);
}

export function addMinutes(d: Date, v: number) {
  return parse(d.getTime() + v * 60000, (<any>d).kind);
}

export function addSeconds(d: Date, v: number) {
  return parse(d.getTime() + v * 1000, (<any>d).kind);
}

export function addMilliseconds(d: Date, v: number) {
  return parse(d.getTime() + v, (<any>d).kind);
}

export function addTicks(d: Date, t: Long.Long) {
  return parse(Long.fromNumber(d.getTime()).add(t.div(10000)).toNumber(), (<any>d).kind);
}

export function addYears(d: Date, v: number) {
  const newMonth = month(d);
  const newYear = year(d) + v;
  const _daysInMonth = daysInMonth(newYear, newMonth);
  const newDay = Math.min(_daysInMonth, day(d));
  return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), (<any>d).kind);
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
    yearOffset = Math.floor(newMonth / 12) + (newMonth_ == 12 ? -1 : 0);
    newMonth = newMonth_;
  }
  const newYear = year(d) + yearOffset;
  const _daysInMonth = daysInMonth(newYear, newMonth);
  const newDay = Math.min(_daysInMonth, day(d));
  return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), (<any>d).kind);
}

export function subtract(d: Date, that: Date | number) {
  return typeof that == "number"
    ? parse(d.getTime() - <number>that, (<any>d).kind)
    : d.getTime() - (<Date>that).getTime();
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
  return d1.getTime() == d2.getTime();
}

export function compare(x: Date, y: Date) {
  return utilCompare(x, y)
}

export function compareTo(x: Date, y: Date) {
  return utilCompare(x, y)
}

export function op_Addition(x: Date, y: number) {
  return add(x, y);
}

export function op_Subtraction(x: Date, y: number | Date) {
  return subtract(x, y);
}