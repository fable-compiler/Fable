import { getTicks, dayOfYear as Date_dayOfYear, year as Date_year, month as Date_month, day as Date_day } from "./Date.js";
import { IDateTime, DateKind } from "./Util.js";
import { toInt, fromNumber, op_Division as Long_op_Division, op_Multiply as Long_op_Multiply, ticksToUnixEpochMilliseconds } from "./Long.js";

export function fromUnixMilliseconds(value: number) {
  const d = new Date(value) as IDateTime;
  d.kind = DateKind.UTC;
  return d;
}

export function create(year: number, month: number, day: number) {
  const d = fromUnixMilliseconds(Date.UTC(year, month - 1, day));
  if (year <= 99) {
    d.setUTCFullYear(year);
  }
  return d;
}

export function maxValue() {
  // This is "9999-12-31T00:00:00.000Z"
  return fromUnixMilliseconds(253402214400000);
}

export function minValue() {
  // This is "0001-01-01T00:00:00.000Z"
  return fromUnixMilliseconds(-62135596800000);
}

export function dayNumber(d: IDateTime) {
   return toInt(Long_op_Division(getTicks(d), 864000000000));
}

export function fromDayNumber(dayNumber: number) {
  const ticks = Long_op_Multiply(fromNumber(864000000000), dayNumber);
  return fromUnixMilliseconds(ticksToUnixEpochMilliseconds(ticks));
}

export function fromDateTime(d: IDateTime) {
  return create(Date_year(d), Date_month(d), Date_day(d));
}

export function day(d: IDateTime) {
  return d.getUTCDate();
}

export function month(d: IDateTime) {
  return d.getUTCMonth() + 1;
}

export function year(d: IDateTime) {
  return d.getUTCFullYear();
}

export function dayOfWeek(d: IDateTime) {
  return d.getUTCDay();
}

export function dayOfYear(d: IDateTime) {
  return Date_dayOfYear(d);
}
