/**
 * DateTimeOffset functions.
 *
 * Note: DateOffset instances are always DateObjects in local
 * timezone (because JS dates are all kinds of messed up).
 * A local date returns UTC epoc when `.getTime()` is called.
 *
 * However, this means that in order to construct an UTC date
 * from a DateOffset with offset of +5 hours, you first need
 * to subtract those 5 hours, than add the "local" offset.
 * As said, all kinds of messed up.
 *
 * Basically; invariant: date.getTime() always return UTC time.
 */

import { create as createDate, dateOffsetToString, daysInMonth, parseRaw } from "./Date.js";
import { fromValue, Long, ticksToUnixEpochMilliseconds, unixEpochMillisecondsToTicks } from "./Long.js";
import { FSharpRef } from "./Types.js";
import { compareDates, DateKind, IDateTime, IDateTimeOffset, padWithZeros } from "./Util.js";

export default function DateTimeOffset(value: number, offset?: number) {
  checkOffsetInRange(offset);
  const d = new Date(value) as IDateTimeOffset;
  d.offset = offset != null ? offset : new Date().getTimezoneOffset() * -60000;
  return d;
}

function checkOffsetInRange(offset?: number) {
  if (offset != null && offset !== 0) {
    if (offset % 60000 !== 0) {
      throw new Error("Offset must be specified in whole minutes.");
    }
    if (Math.abs(offset / 3600000) > 14) {
      throw new Error("Offset must be within plus or minus 14 hours.");
    }
  }
}

export function fromDate(date: IDateTime, offset?: number) {
  let offset2: number = 0;
  switch (date.kind) {
    case DateKind.UTC:
      if(offset != null && offset !== 0) {
        throw new Error("The UTC Offset for Utc DateTime instances must be 0.");
      }
      offset2 = 0;
      break;

    case DateKind.Local:
      offset2 = date.getTimezoneOffset() * -60_000;
      if(offset != null && offset !== offset2) {
        throw new Error("The UTC Offset of the local dateTime parameter does not match the offset argument.");
      }

      break;

    case DateKind.Unspecified:
    default:
      if(offset == null) {
        offset2 = date.getTimezoneOffset() * -60_000;
      }
      else {
        offset2 = offset;
      }
      break;
  }

  return DateTimeOffset(date.getTime(), offset2);
}

export function fromTicks(ticks: number | Long, offset: number) {
  ticks = fromValue(ticks);
  const epoc = ticksToUnixEpochMilliseconds(ticks) - offset;
  return DateTimeOffset(epoc, offset);
}

export function getUtcTicks(date: IDateTimeOffset) {
  return unixEpochMillisecondsToTicks(date.getTime(), 0);
}

export function minValue() {
  // This is "0001-01-01T00:00:00.000Z", actual JS min value is -8640000000000000
  return DateTimeOffset(-62135596800000, 0);
}

export function maxValue() {
  // This is "9999-12-31T23:59:59.999Z", actual JS max value is 8640000000000000
  return DateTimeOffset(253402300799999, 0);
}

export function parse(str: string): IDateTimeOffset {
  const [date, offsetMatch] = parseRaw(str);
  const offset = offsetMatch == null
    ? date.getTimezoneOffset() * -60000
    : (offsetMatch === "Z" ? 0 : offsetMatch * 60000);
  return DateTimeOffset(date.getTime(), offset);
}

export function tryParse(v: string, defValue: FSharpRef<IDateTimeOffset>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch (_err) {
    return false;
  }
}

export function create(
  year: number, month: number, day: number,
  h: number, m: number, s: number,
  ms: number, offset?: number) {
  if (offset == null) {
    offset = ms;
    ms = 0;
  }
  checkOffsetInRange(offset);
  let date: Date;
  if (offset === 0) {
    date = new Date(Date.UTC(year, month - 1, day, h, m, s, ms));
    if (year <= 99) {
      date.setUTCFullYear(year, month - 1, day);
    }
  } else {
    const str =
      padWithZeros(year, 4) + "-" +
      padWithZeros(month, 2) + "-" +
      padWithZeros(day, 2) + "T" +
      padWithZeros(h, 2) + ":" +
      padWithZeros(m, 2) + ":" +
      padWithZeros(s, 2) + "." +
      padWithZeros(ms, 3) +
      dateOffsetToString(offset);
    date = new Date(str);
  }
  const dateValue = date.getTime();
  if (isNaN(dateValue)) {
    throw new Error("The parameters describe an unrepresentable Date");
  }
  return DateTimeOffset(dateValue, offset);
}

export function now() {
  const date = new Date();
  const offset = date.getTimezoneOffset() * -60000;
  return DateTimeOffset(date.getTime(), offset);
}

export function utcNow() {
  const date = now();
  return DateTimeOffset(date.getTime(), 0);
}

export function toUniversalTime(date: IDateTimeOffset) {
  return DateTimeOffset(date.getTime(), 0);
}

export function toLocalTime(date: Date) {
  return DateTimeOffset(date.getTime(), date.getTimezoneOffset() * -60000);
}

export function timeOfDay(d: IDateTimeOffset) {
  const d2 = new Date(d.getTime() + (d.offset ?? 0));
  return d2.getUTCHours() * 3600000
    + d2.getUTCMinutes() * 60000
    + d2.getUTCSeconds() * 1000
    + d2.getUTCMilliseconds();
}

export function date(d: IDateTimeOffset) {
  const d2 = new Date(d.getTime() + (d.offset ?? 0));
  return createDate(d2.getUTCFullYear(), d2.getUTCMonth() + 1, d2.getUTCDate(), 0, 0, 0, 0);
}

export function day(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCDate();
}

export function hour(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCHours();
}

export function millisecond(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCMilliseconds();
}

export function minute(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCMinutes();
}

export function month(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCMonth() + 1;
}

export function second(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCSeconds();
}

export function year(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCFullYear();
}

export function dayOfWeek(d: IDateTimeOffset) {
  return new Date(d.getTime() + (d.offset ?? 0)).getUTCDay();
}

export function dayOfYear(d: IDateTimeOffset) {
  const d2 = new Date(d.getTime() + (d.offset ?? 0));
  const _year = d2.getUTCFullYear();
  const _month = d2.getUTCMonth() + 1;
  let _day = d2.getUTCDate();
  for (let i = 1; i < _month; i++) {
    _day += daysInMonth(_year, i);
  }
  return _day;
}

export function add(d: IDateTimeOffset, ts: number) {
  return DateTimeOffset(d.getTime() + ts, (d.offset ?? 0));
}

export function addDays(d: IDateTimeOffset, v: number) {
  return DateTimeOffset(d.getTime() + v * 86400000, (d.offset ?? 0));
}

export function addHours(d: IDateTimeOffset, v: number) {
  return DateTimeOffset(d.getTime() + v * 3600000, (d.offset ?? 0));
}

export function addMinutes(d: IDateTimeOffset, v: number) {
  return DateTimeOffset(d.getTime() + v * 60000, (d.offset ?? 0));
}

export function addSeconds(d: IDateTimeOffset, v: number) {
  return DateTimeOffset(d.getTime() + v * 1000, (d.offset ?? 0));
}

export function addMilliseconds(d: IDateTimeOffset, v: number) {
  return DateTimeOffset(d.getTime() + v, (d.offset ?? 0));
}

export function addYears(d: IDateTimeOffset, v: number) {
  const newMonth = d.getUTCMonth() + 1;
  const newYear = d.getUTCFullYear() + v;
  const _daysInMonth = daysInMonth(newYear, newMonth);
  const newDay = Math.min(_daysInMonth, d.getUTCDate());
  return create(newYear, newMonth, newDay, d.getUTCHours(), d.getUTCMinutes(),
    d.getUTCSeconds(), d.getUTCMilliseconds(), (d.offset ?? 0));
}

export function addMonths(d: IDateTimeOffset, v: number) {
  const d2 = new Date(d.getTime() + (d.offset ?? 0));
  let newMonth = d2.getUTCMonth() + 1 + v;
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
  const newYear = d2.getUTCFullYear() + yearOffset;
  const _daysInMonth = daysInMonth(newYear, newMonth);
  const newDay = Math.min(_daysInMonth, d2.getUTCDate());
  return create(newYear, newMonth, newDay, d2.getUTCHours(), d2.getUTCMinutes(),
    d2.getUTCSeconds(), d2.getUTCMilliseconds(), (d.offset ?? 0));
}

export function subtract(d: IDateTimeOffset, that: IDateTimeOffset | number) {
  return typeof that === "number"
    ? DateTimeOffset(d.getTime() - that, (d.offset ?? 0))
    : d.getTime() - that.getTime();
}

export function equals(d1: IDateTimeOffset, d2: IDateTimeOffset) {
  return d1.getTime() === d2.getTime();
}

export function equalsExact(d1: IDateTimeOffset, d2: IDateTimeOffset) {
  return d1.getTime() === d2.getTime() && d1.offset === d2.offset;
}

export function compare(d1: IDateTimeOffset, d2: IDateTimeOffset) {
  return compareDates(d1, d2);
}

export const compareTo = compare;

export function op_Addition(x: IDateTimeOffset, y: number) {
  return add(x, y);
}

export function op_Subtraction(x: IDateTimeOffset, y: number | IDateTimeOffset) {
  return subtract(x, y);
}

export function toOffset(d: IDateTimeOffset, offset: number): IDateTimeOffset {
  return DateTimeOffset(d.getTime(), offset);
}
