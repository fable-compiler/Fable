import { FSharpRef } from "./Types.ts";
import { DateTime, year as Date_year, month as Date_month, day as Date_day } from "./Date.ts";
import DateTimeOffset from "./DateOffset.ts";
import { TimeOnly, toTimeSpan as TimeOnly_toTimeSpan } from "./TimeOnlyTemporal.ts";
import { Exception, IDateTime, IDateTimeOffset, DateTimeKind, padWithZeros } from "./Util.ts";

declare global {
  namespace Temporal {
    class PlainDate {
      constructor(isoYear: number, isoMonth: number, isoDay: number);
      static compare(one: PlainDate, two: PlainDate): number;
      readonly year: number;
      readonly month: number;
      readonly day: number;
      readonly dayOfWeek: number;
      readonly dayOfYear: number;
      add(duration: { years?: number, months?: number, days?: number }): PlainDate;
      until(other: PlainDate): Duration;
      equals(other: PlainDate): boolean;
    }
    class Duration {
      readonly days: number;
    }
    namespace Now {
      function plainDateISO(): PlainDate;
    }
  }
}

export type DateOnly = Temporal.PlainDate;

export const PlainDate = Temporal.PlainDate;
export type PlainDate = Temporal.PlainDate;

// The generic equality/comparison/hashing helpers in Util.ts dispatch at runtime
// on .NET-style Equals/CompareTo/GetHashCode methods (with a special case for JS
// Date, which Temporal.PlainDate cannot benefit from). Attach them so DateOnly
// also works in generic contexts: records, tuples, erased generics, etc.
const proto = Temporal.PlainDate.prototype as any;
proto.Equals = function (this: DateOnly, other: DateOnly): boolean { return this.equals(other); };
proto.CompareTo = function (this: DateOnly, other: DateOnly): number { return Temporal.PlainDate.compare(this, other); };
proto.GetHashCode = function (this: DateOnly): number { return dayNumber(this); };

export function create(year: number, month: number, day: number): DateOnly {
  return new Temporal.PlainDate(year, month, day);
}

export function maxValue(): DateOnly {
  return new Temporal.PlainDate(9999, 12, 31);
}

export function minValue(): DateOnly {
  return new Temporal.PlainDate(1, 1, 1);
}

export function dayNumber(d: DateOnly): number {
  return minValue().until(d).days;
}

export function fromDayNumber(dayNumber: number): DateOnly {
  return minValue().add({ days: dayNumber });
}

export function fromDateTime(d: IDateTime): DateOnly {
  return new Temporal.PlainDate(Date_year(d), Date_month(d), Date_day(d));
}

export function dayOfWeek(d: DateOnly): number {
  // Temporal: Monday = 1 ... Sunday = 7, .NET: Sunday = 0 ... Saturday = 6
  return d.dayOfWeek % 7;
}

// Unix milliseconds of midnight UTC on the given date
function toUnixMilliseconds(d: DateOnly): number {
  const ms = Date.UTC(d.year, d.month - 1, d.day);
  if (d.year <= 99) {
    const date = new Date(ms);
    date.setUTCFullYear(d.year);
    return date.getTime();
  }
  return ms;
}

export function toDateTime(d: DateOnly, time: TimeOnly, kind: DateTimeKind = DateTimeKind.Unspecified): IDateTime {
  const utcMidnight = new Date(toUnixMilliseconds(d));
  return DateTime(utcMidnight.getTime() + TimeOnly_toTimeSpan(time) + (kind !== DateTimeKind.Utc ? utcMidnight.getTimezoneOffset() : 0) * 60_000, kind);
}

export function toDateTimeOffset(d: DateOnly, time: TimeOnly, offset: number): IDateTimeOffset {
  return DateTimeOffset(toUnixMilliseconds(d) - offset + TimeOnly_toTimeSpan(time), offset);
}

export function addDays(d: DateOnly, v: number): DateOnly {
  return d.add({ days: v });
}

export function addMonths(d: DateOnly, v: number): DateOnly {
  return d.add({ months: v });
}

export function addYears(d: DateOnly, v: number): DateOnly {
  return d.add({ years: v });
}

export function equals(x: DateOnly, y: DateOnly): boolean {
  return x.equals(y);
}

export function compare(x: DateOnly, y: DateOnly): number {
  return Temporal.PlainDate.compare(x, y);
}

export function hash(d: DateOnly): number {
  return dayNumber(d);
}

export function toString(d: DateOnly, format = "d", _provider?: any): string {
  switch (format) {
    case "d":
      return `${padWithZeros(d.month, 2)}/${padWithZeros(d.day, 2)}/${padWithZeros(d.year, 4)}`;
    case "o":
    case "O":
      // PlainDate.toString() is the ISO yyyy-MM-dd round-trip format
      return d.toString();
    default:
      throw new Exception("Custom formats are not supported");
  }
}

export function parse(str: string): DateOnly {
  function fail(): DateOnly {
    throw new Exception(`String '${str}' was not recognized as a valid DateOnly.`);
  }

  // Allowed separators: . , / -
  // TODO whitespace alone as the separator
  //
  // Whitespace around separators
  //
  // Allowed format types:
  // yyyy/mm/dd
  // mm/dd/yyyy
  // mm/dd
  // mm/yyyy
  // yyyy/mm
  const r = /^\s*(\d{1,4})(?:\s*[.,-\/]\s*(\d{1,2}))?\s*[.,-\/]\s*(\d{1,4})\s*$/.exec(str);
  if (r != null) {
    let y = 0;
    let m = 0;
    let d = 1;

    if (r[2] == null) {
      if (r[1].length < 3) {
        if (r[3].length < 3) {
          // 12/30 = December 30, {CurrentYear}
          y = Temporal.Now.plainDateISO().year;
          m = +r[1];
          d = +r[3];
        } else {
          // 12/2000 = December 1, 2000
          m = +r[1];
          y = +r[3];
        }
      } else {
        if (r[3].length > 2)
          fail();

        // 2000/12 = December 1, 2000
        y = +r[1];
        m = +r[3];
      }
    } else {
      // 2000/1/30 or 1/30/2000
      const yearFirst = r[1].length > 2;
      const yTmp = r[yearFirst ? 1 : 3];
      y = +yTmp;

      // year 0-29 is 2000-2029, 30-99 is 1930-1999
      if (yTmp.length < 3)
        y += y >= 30 ? 1900 : 2000;

      m = +r[yearFirst ? 2 : 1];
      d = +r[yearFirst ? 3 : 2];
    }

    if (y > 0) {
      try {
        return new Temporal.PlainDate(y, m, d);
      } catch {
        return fail();
      }
    }
  }

  return fail();
}

export function tryParse(v: string, defValue: FSharpRef<DateOnly>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
