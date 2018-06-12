// Don't change, this corresponds to DateTime.Kind
// enum values in .NET
export const enum DateKind {
  Unspecified = 0,
  UTC = 1,
  Local = 2,
}

export interface IDateTime extends Date {
  kind?: DateKind;
}

export interface IDateTimeOffset extends Date {
  offset?: number;
}

export const offsetRegex = /(?:Z|[+-](\d+):?([0-5]?\d)?)\s*$/;

export function padWithZeros(i: number, length: number) {
  let str = i.toString(10);
  while (str.length < length) {
    str = "0" + str;
  }
  return str;
}

export function offsetToString(offset: number) {
  const isMinus = offset < 0;
  offset = Math.abs(offset);
  const hours = ~~(offset / 3600000);
  const minutes = (offset % 3600000) / 60000;
  return (isMinus ? "-" : "+") +
          padWithZeros(hours, 2) + ":" +
          padWithZeros(minutes, 2);
}

export function toHalfUTCString(date: IDateTime, half: "first" | "second") {
  const str = date.toISOString();
  return half === "first"
    ? str.substring(0, str.indexOf("T"))
    : str.substring(str.indexOf("T") + 1, str.length - 1);
}

function toISOString(d: IDateTime, utc: boolean) {
  if (utc) {
    return d.toISOString();
  } else {
    // JS Date is always local
    const printOffset = d.kind == null ? true : d.kind === DateKind.Local;
    return  padWithZeros(d.getFullYear(), 4) + "-" +
            padWithZeros(d.getMonth() + 1, 2)    + "-" +
            padWithZeros(d.getDate(), 2)     + "T" +
            padWithZeros(d.getHours(), 2)    + ":" +
            padWithZeros(d.getMinutes(), 2)  + ":" +
            padWithZeros(d.getSeconds(), 2)  + "." +
            padWithZeros(d.getMilliseconds(), 3) +
            (printOffset ? offsetToString(d.getTimezoneOffset() * -60000) : "");
  }
}

function toISOStringWithOffset(dateWithOffset: Date, offset: number) {
  const str = dateWithOffset.toISOString();
  return str.substring(0, str.length - 1) + offsetToString(offset);
}

function toStringWithCustomFormat(date: Date, format: string, utc: boolean) {
  return format.replace(/(\w)\1*/g, (match: any) => {
    let rep = match;
    switch (match.substring(0, 1)) {
      case "y":
        const y = utc ? date.getUTCFullYear() : date.getFullYear();
        rep = match.length < 4 ? y % 100 : y; break;
      case "M": rep = (utc ? date.getUTCMonth() : date.getMonth()) + 1; break;
      case "d": rep = utc ? date.getUTCDate() : date.getDate(); break;
      case "H": rep = utc ? date.getUTCHours() : date.getHours(); break;
      case "h":
        const h = utc ? date.getUTCHours() : date.getHours();
        rep = h > 12 ? h % 12 : h; break;
      case "m": rep = utc ? date.getUTCMinutes() : date.getMinutes(); break;
      case "s": rep = utc ? date.getUTCSeconds() : date.getSeconds(); break;
    }
    if (rep !== match && rep < 10 && match.length > 1) {
      rep = "0" + rep;
    }
    return rep;
  });
}

export function toStringWithOffset(date: IDateTimeOffset, format?: string) {
  const d = new Date(date.getTime() + date.offset);
  if (!format) {
    return d.toISOString().replace(/\.\d+/, "").replace(/[A-Z]|\.\d+/g, " ") + offsetToString(date.offset);
  } else if (format.length === 1) {
    switch (format) {
      case "D": case "d": return toHalfUTCString(d, "first");
      case "T": case "t": return toHalfUTCString(d, "second");
      case "O": case "o": return toISOStringWithOffset(d, date.offset);
      default: throw new Error("Unrecognized Date print format");
    }
  } else {
    return toStringWithCustomFormat(d, format, true);
  }
}

export function toStringWithKind(date: IDateTime, format?: string) {
  const utc = date.kind === DateKind.UTC;
  if (!format) {
    return utc ? date.toUTCString() : date.toLocaleString();
  } else if (format.length === 1) {
    switch (format) {
      case "D": case "d":
        return utc ? toHalfUTCString(date, "first") : date.toLocaleDateString();
      case "T": case "t":
        return utc ? toHalfUTCString(date, "second") : date.toLocaleTimeString();
      case "O": case "o":
        return toISOString(date, utc);
      default:
        throw new Error("Unrecognized Date print format");
    }
  } else {
    return toStringWithCustomFormat(date, format, utc);
  }
}

export function toString(date: IDateTime | IDateTimeOffset, format?: string) {
  return (date as IDateTimeOffset).offset != null
    ? toStringWithOffset(date, format)
    : toStringWithKind(date, format);
}

export default function DateTime(value: number, kind?: DateKind) {
  kind = kind == null ? DateKind.Unspecified : kind;
  const d = new Date(value) as IDateTime;
  d.kind = kind | 0;
  return d;
}

export function minValue() {
  // This is "0001-01-01T00:00:00.000Z", actual JS min value is -8640000000000000
  return DateTime(-62135596800000, DateKind.Unspecified);
}

export function maxValue() {
  // This is "9999-12-31T23:59:59.999Z", actual JS max value is 8640000000000000
  return DateTime(253402300799999, DateKind.Unspecified);
}

export function parseRaw(str: string) {
  let date = new Date(str);
  if (isNaN(date.getTime())) {
    // Try to check strings JS Date cannot parse (see #1045, #1422)
    /* tslint:disable */
    const m = /^\s*(\d+[^\w\s:]\d+[^\w\s:]\d+)?\s*(\d+:\d+(?::\d+(?:\.\d+)?)?)?\s*([AaPp][Mm])?\s*([+-]\d+(?::\d+)?)?\s*$/.exec(str);
    /* tslint:enable */
    if (m != null) {
      let baseDate: Date = null;
      let timeInSeconds = 0;
      if (m[2] != null) {
        const timeParts = m[2].split(":");
        timeInSeconds = parseInt(timeParts[0], 10) * 3600 +
                        parseInt(timeParts[1] || "0", 10) * 60 +
                        parseFloat(timeParts[2] || "0");
        if (m[3] != null && m[3].toUpperCase() === "PM") {
          timeInSeconds += 720;
        }
      }
      if (m[4] != null) { // There's an offset, parse as UTC
        if (m[1] != null) {
          baseDate = new Date(m[1] + " UTC");
        } else {
          const d = new Date();
          baseDate = new Date(d.getUTCFullYear() + "/" + (d.getUTCMonth() + 1) + "/" + d.getUTCDate());
        }
        const offsetParts = m[4].substr(1).split(":");
        let offsetInMinutes = parseInt(offsetParts[0], 10) * 60 + parseInt(offsetParts[1] || "0", 10);
        if (m[4][0] === "+") {
          offsetInMinutes *= -1;
        }
        timeInSeconds += offsetInMinutes * 60;
      } else {
        if (m[1] != null) {
          baseDate = new Date(m[1]);
        } else {
          const d = new Date();
          baseDate = new Date(d.getFullYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate());
        }
      }
      date = new Date(baseDate.getTime() + timeInSeconds * 1000);
    } else {
      throw new Error("The string is not a valid Date.");
    }
  }
  return date;
}

export function parse(str: string, detectUTC = false): IDateTime {
  const date = parseRaw(str);
  const offset = offsetRegex.exec(str);
  // .NET always parses DateTime as Local if there's offset info (even "Z")
  // Newtonsoft.Json uses UTC if the offset is "Z"
  const kind = offset != null
    ? (detectUTC && offset[0] === "Z" ? DateKind.UTC : DateKind.Local)
    : DateKind.Unspecified;
  return DateTime(date.getTime(), kind);
}

export function tryParse(v: any): [boolean, IDateTime] {
  try {
    return [true, parse(v)];
  } catch (_err) {
    return [false, minValue()];
  }
}

export function offset(date: IDateTime | IDateTimeOffset): number {
  const date1 = date as IDateTimeOffset;
  return typeof date1.offset === "number"
    ? date1.offset
    : ((date as IDateTime).kind === DateKind.UTC
        ? 0 : date.getTimezoneOffset() * -60000);
}

export function create(
    year: number, month: number, day: number,
    h: number = 0, m: number = 0, s: number = 0,
    ms: number = 0, kind?: DateKind) {
  const dateValue = kind === DateKind.UTC
    ? Date.UTC(year, month - 1, day, h, m, s, ms)
    : new Date(year, month - 1, day, h, m, s, ms).getTime();
  if (isNaN(dateValue)) {
    throw new Error("The parameters describe an unrepresentable Date.");
  }
  const date = DateTime(dateValue, kind);
  if (year <= 99) {
    date.setFullYear(year, month - 1, day);
  }
  return date;
}

export function now() {
  return DateTime(Date.now(), DateKind.Local);
}

export function utcNow() {
  return DateTime(Date.now(), DateKind.UTC);
}

export function today() {
  return date(now());
}

export function isLeapYear(year: number) {
  return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
}

export function daysInMonth(year: number, month: number) {
  return month === 2
    ? (isLeapYear(year) ? 29 : 28)
    : (month >= 8 ? (month % 2 === 0 ? 31 : 30) : (month % 2 === 0 ? 30 : 31));
}

export function toUniversalTime(date: IDateTime) {
  return date.kind === DateKind.UTC ? date : DateTime(date.getTime(), DateKind.UTC);
}

export function toLocalTime(date: IDateTime) {
  return date.kind === DateKind.Local ? date : DateTime(date.getTime(), DateKind.Local);
}

export function timeOfDay(d: IDateTime) {
  return hour(d) * 3600000
    + minute(d) * 60000
    + second(d) * 1000
    + millisecond(d);
}

export function date(d: IDateTime) {
  return create(year(d), month(d), day(d), 0, 0, 0, 0, d.kind);
}

export function day(d: IDateTime) {
  return d.kind === DateKind.UTC ? d.getUTCDate() : d.getDate();
}

export function hour(d: IDateTime) {
  return d.kind === DateKind.UTC ? d.getUTCHours() : d.getHours();
}

export function millisecond(d: IDateTime) {
  return d.kind === DateKind.UTC ? d.getUTCMilliseconds() : d.getMilliseconds();
}

export function minute(d: IDateTime) {
  return d.kind === DateKind.UTC ? d.getUTCMinutes() : d.getMinutes();
}

export function month(d: IDateTime) {
  return (d.kind === DateKind.UTC ? d.getUTCMonth() : d.getMonth()) + 1;
}

export function second(d: IDateTime) {
  return d.kind === DateKind.UTC ? d.getUTCSeconds() : d.getSeconds();
}

export function year(d: IDateTime) {
  return d.kind === DateKind.UTC ? d.getUTCFullYear() : d.getFullYear();
}

export function dayOfWeek(d: IDateTime) {
  return d.kind === DateKind.UTC ? d.getUTCDay() : d.getDay();
}

export function dayOfYear(d: IDateTime) {
  const _year = year(d);
  const _month = month(d);
  let _day = day(d);
  for (let i = 1; i < _month; i++) {
    _day += daysInMonth(_year, i);
  }
  return _day;
}

export function add(d: IDateTime, ts: number) {
  return DateTime(d.getTime() + ts, d.kind);
}

export function addDays(d: IDateTime, v: number) {
  return DateTime(d.getTime() + v * 86400000, d.kind);
}

export function addHours(d: IDateTime, v: number) {
  return DateTime(d.getTime() + v * 3600000, d.kind);
}

export function addMinutes(d: IDateTime, v: number) {
  return DateTime(d.getTime() + v * 60000, d.kind);
}

export function addSeconds(d: IDateTime, v: number) {
  return DateTime(d.getTime() + v * 1000, d.kind);
}

export function addMilliseconds(d: IDateTime, v: number) {
  return DateTime(d.getTime() + v, d.kind);
}

export function addYears(d: IDateTime, v: number) {
  const newMonth = month(d);
  const newYear = year(d) + v;
  const _daysInMonth = daysInMonth(newYear, newMonth);
  const newDay = Math.min(_daysInMonth, day(d));
  return create(newYear, newMonth, newDay, hour(d), minute(d), second(d),
    millisecond(d), d.kind);
}

export function addMonths(d: IDateTime, v: number) {
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
    millisecond(d), d.kind);
}

export function subtract(d: IDateTime, that: IDateTime | number) {
  return typeof that === "number"
    ? DateTime(d.getTime() - that, d.kind)
    : d.getTime() - that.getTime();
}

export function toLongDateString(d: IDateTime) {
  return d.toDateString();
}

export function toShortDateString(d: IDateTime) {
  return d.toLocaleDateString();
}

export function toLongTimeString(d: IDateTime) {
  return d.toLocaleTimeString();
}

export function toShortTimeString(d: IDateTime) {
  return d.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
}

export function equals(d1: IDateTime, d2: IDateTime) {
  return d1.getTime() === d2.getTime();
}

export function compare(x: Date, y: Date) {
  const xtime = x.getTime();
  const ytime = y.getTime();
  return xtime === ytime ? 0 : (xtime < ytime ? -1 : 1);
}

export const compareTo = compare;

export function op_Addition(x: IDateTime, y: number) {
  return add(x, y);
}

export function op_Subtraction(x: IDateTime, y: number | Date) {
  return subtract(x, y);
}

export function isDaylightSavingTime(x: IDateTime) {
  const jan = new Date(x.getFullYear(), 0, 1);
  const jul = new Date(x.getFullYear(), 6, 1);
  return isDST(jan.getTimezoneOffset(), jul.getTimezoneOffset(), x.getTimezoneOffset());
}

function isDST(janOffset: number, julOffset: number, tOffset: number) {
  return Math.min(janOffset, julOffset) === tOffset;
}
