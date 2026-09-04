/**
 * Field-based .NET date/time formatting and lenient parsing, shared by the
 * Temporal DateTime / DateTimeOffset representations.
 *
 * The formatters operate on a plain `DateInfo` (wall-clock fields), which the
 * Temporal types expose directly (`d.year`, `d.hour`, `d.dayOfWeek % 7`, ...),
 * so no JS-Date intermediate is needed there.
 *
 * Parsing is the exception. .NET accepts far more than ISO 8601 ("12/30/2009",
 * "June 15, 2009 1:45 PM"), and the JS `Date` built-in is the only lenient date
 * parser the platform offers — Temporal's `from` is strict ISO and rejects all of
 * it. So `parseRaw` uses `Date` as a parsing engine and returns the instant it
 * resolved, which callers convert into the Temporal value they need. That instant
 * is only millisecond-precise, hence `subMillisecondTicks`: the finer digits are
 * read straight off the input, because `Date` has already discarded them.
 *
 * This is a port of the equivalent logic in Date.ts / DateOffset.ts, kept
 * separate so the Temporal modules do not depend on the JS-Date representation.
 */

import { Exception, DateTimeKind, padWithZeros } from "./Util.ts";

// Wall-clock fields. dayOfWeek is .NET-style: 0 = Sunday ... 6 = Saturday.
//
// `tick` is the sub-second component in .NET ticks (100ns units), 0..9_999_999 —
// not milliseconds. .NET's fractional-second format specifiers ("f"/"F", and the
// round-trip "O") go all the way down to a single tick, so carrying only
// milliseconds here would cap the output at 3 of the 7 digits .NET prints.
export interface DateInfo {
  year: number;
  month: number;
  day: number;
  hour: number;
  minute: number;
  second: number;
  tick: number;
  dayOfWeek: number;
}

// The 7 fractional-second digits .NET works with, most significant first.
function fractionDigits(info: DateInfo): string {
  return padWithZeros(info.tick, 7);
}

type OffsetInMinutes = number;
export type Offset = "Z" | OffsetInMinutes | null;

const shortDays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
const longDays = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
const shortMonths = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
const longMonths =
  ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

function parseRepeatToken(format: string, pos: number, patternChar: string) {
  let tokenLength = 0;
  let internalPos = pos;
  while (internalPos < format.length && format[internalPos] === patternChar) {
    internalPos++;
    tokenLength++;
  }

  return tokenLength;
}

function parseNextChar(format: string, pos: number) {
  if (pos >= format.length - 1) {
    return -1;
  }

  return format.charCodeAt(pos + 1);
}

function parseQuotedString(format: string, pos: number): [string, number] {
  let beginPos = pos;
  // Get the character used to quote the string
  const quoteChar = format[pos];

  let result = "";
  let foundQuote = false;

  while (pos < format.length) {
    pos++;
    const currentChar = format[pos];
    if (currentChar === quoteChar) {
      foundQuote = true;
      break;
    } else if (currentChar === "\\") {
      if (pos < format.length) {
        pos++;
        result += format[pos];
      } else {
        // This means that '\' is the last character in the string.
        throw new Exception("Invalid string format");
      }
    } else {
      result += currentChar;
    }
  }

  if (!foundQuote) {
    // We could not find the matching quote
    throw new Exception(`Invalid string format could not find matching quote for ${quoteChar}`);
  }

  return [result, pos - beginPos + 1];
}

// Custom format string. `kind` decides the K/z tokens; `hostOffsetString`
// (e.g. "+01:00") is the host-zone offset at this wall-clock, used for the
// Local / Unspecified cases.
export function dateToStringWithCustomFormat(
  info: DateInfo, format: string, kind: DateTimeKind, hostOffsetString: string): string {
  let cursorPos = 0;
  let tokenLength = 0;
  let result = "";

  while (cursorPos < format.length) {
    const token = format[cursorPos];

    switch (token) {
      case "d":
        tokenLength = parseRepeatToken(format, cursorPos, "d");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            result += info.day;
            break;
          case 2:
            result += padWithZeros(info.day, 2);
            break;
          case 3:
            result += shortDays[info.dayOfWeek];
            break;
          case 4:
          default:
            result += longDays[info.dayOfWeek];
            break;
        }
        break;
      case "f":
        // The first `tokenLength` fractional-second digits, always padded.
        tokenLength = parseRepeatToken(format, cursorPos, "f");
        cursorPos += tokenLength;
        if (tokenLength <= 7) {
          result += fractionDigits(info).substring(0, tokenLength);
        } else {
          throw "Input string was not in a correct format.";
        }
        break;
      case "F":
        // As "f", but with trailing zeros dropped — and nothing at all when the
        // requested digits are all zero.
        tokenLength = parseRepeatToken(format, cursorPos, "F");
        cursorPos += tokenLength;
        if (tokenLength <= 7) {
          result += fractionDigits(info).substring(0, tokenLength).replace(/0+$/, "");
        } else {
          throw "Input string was not in a correct format.";
        }
        break;
      case "g":
        tokenLength = parseRepeatToken(format, cursorPos, "g");
        cursorPos += tokenLength;
        result += "A.D.";
        break;
      case "h":
        tokenLength = parseRepeatToken(format, cursorPos, "h");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            const h1Value = info.hour % 12;
            result += h1Value ? h1Value : 12;
            break;
          case 2:
          default:
            const h2Value = info.hour % 12;
            result += padWithZeros(h2Value ? h2Value : 12, 2);
            break;
        }
        break;
      case "H":
        tokenLength = parseRepeatToken(format, cursorPos, "H");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            result += info.hour;
            break;
          case 2:
          default:
            result += padWithZeros(info.hour, 2);
            break;
        }
        break;
      case "K":
        tokenLength = parseRepeatToken(format, cursorPos, "K");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            switch (kind) {
              case DateTimeKind.Utc:
                result += "Z";
                break;
              case DateTimeKind.Local:
                result += hostOffsetString;
                break;
              case DateTimeKind.Unspecified:
                break;
            }
            break;
          default:
            break;
        }
        break;
      case "m":
        tokenLength = parseRepeatToken(format, cursorPos, "m");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            result += info.minute;
            break;
          case 2:
          default:
            result += padWithZeros(info.minute, 2);
            break;
        }
        break;
      case "M":
        tokenLength = parseRepeatToken(format, cursorPos, "M");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            result += info.month;
            break;
          case 2:
            result += padWithZeros(info.month, 2);
            break;
          case 3:
            result += shortMonths[info.month - 1];
            break;
          case 4:
          default:
            result += longMonths[info.month - 1];
            break;
        }
        break;
      case "s":
        tokenLength = parseRepeatToken(format, cursorPos, "s");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            result += info.second;
            break;
          case 2:
          default:
            result += padWithZeros(info.second, 2);
            break;
        }
        break;
      case "t":
        tokenLength = parseRepeatToken(format, cursorPos, "t");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            result += info.hour < 12 ? "A" : "P";
            break;
          case 2:
          default:
            result += info.hour < 12 ? "AM" : "PM";
            break;
        }
        break;
      case "y":
        tokenLength = parseRepeatToken(format, cursorPos, "y");
        cursorPos += tokenLength;
        switch (tokenLength) {
          case 1:
            result += info.year % 100;
            break;
          case 2:
            result += padWithZeros(info.year % 100, 2);
            break;
          default:
            result += padWithZeros(info.year, tokenLength);
            break;
        }
        break;
      case "z":
        tokenLength = parseRepeatToken(format, cursorPos, "z");
        cursorPos += tokenLength;
        const utcOffsetText = kind === DateTimeKind.Utc ? "+00:00" : hostOffsetString;

        const sign = utcOffsetText[0] === "-" ? "-" : "+";
        const hours = parseInt(utcOffsetText.substring(1, 3), 10);
        const minutes = parseInt(utcOffsetText.substring(4, 6), 10);

        switch (tokenLength) {
          case 1:
            result += `${sign}${hours}`;
            break;
          case 2:
            result += `${sign}${padWithZeros(hours, 2)}`;
            break;
          default:
            result += `${sign}${padWithZeros(hours, 2)}:${padWithZeros(minutes, 2)}`;
            break;
        }
        break;
      case ":":
        result += ":";
        cursorPos++;
        break;
      case "/":
        result += "/";
        cursorPos++;
        break;
      case "'":
      case '"':
        const [quotedString, quotedStringLenght] = parseQuotedString(format, cursorPos);
        result += quotedString;
        cursorPos += quotedStringLenght;
        break;
      case "%":
        const nextChar = parseNextChar(format, cursorPos);
        if (nextChar >= 0 && nextChar !== "%".charCodeAt(0)) {
          cursorPos += 2;
          result += dateToStringWithCustomFormat(info, String.fromCharCode(nextChar), kind, hostOffsetString);
        } else {
          throw new Exception("Invalid format string");
        }
        break;
      case "\\":
        const nextChar2 = parseNextChar(format, cursorPos);
        if (nextChar2 >= 0) {
          cursorPos += 2;
          result += String.fromCharCode(nextChar2);
        } else {
          throw new Exception("Invalid format string");
        }
        break;
      default:
        cursorPos++;
        result += token;
        break;
    }
  }

  return result;
}

// --- Standard single-letter format building blocks (InvariantCulture) ---

export function dateToString_D(info: DateInfo): string {
  return longDays[info.dayOfWeek]
    + ", " + padWithZeros(info.day, 2)
    + " " + longMonths[info.month - 1]
    + " " + info.year;
}

export function dateToString_d(info: DateInfo): string {
  return padWithZeros(info.month, 2)
    + "/" + padWithZeros(info.day, 2)
    + "/" + info.year;
}

export function dateToString_T(info: DateInfo): string {
  return padWithZeros(info.hour, 2)
    + ":" + padWithZeros(info.minute, 2)
    + ":" + padWithZeros(info.second, 2);
}

export function dateToString_t(info: DateInfo): string {
  return padWithZeros(info.hour, 2)
    + ":" + padWithZeros(info.minute, 2);
}

// RFC 1123: "Thu, 01 Jan 2009 00:00:00 GMT". `info` must already be UTC.
export function dateToString_R(info: DateInfo): string {
  return shortDays[info.dayOfWeek] + ", "
    + padWithZeros(info.day, 2) + " "
    + shortMonths[info.month - 1] + " "
    + info.year + " "
    + padWithZeros(info.hour, 2) + ":"
    + padWithZeros(info.minute, 2) + ":"
    + padWithZeros(info.second, 2) + " GMT";
}

// Universal sortable: "2009-06-15 13:45:30Z". `info` must already be UTC.
export function dateToString_u(info: DateInfo): string {
  return padWithZeros(info.year, 4) + "-"
    + padWithZeros(info.month, 2) + "-"
    + padWithZeros(info.day, 2) + " "
    + padWithZeros(info.hour, 2) + ":"
    + padWithZeros(info.minute, 2) + ":"
    + padWithZeros(info.second, 2) + "Z";
}

// Month/day (InvariantCulture "MMMM dd"): "June 15"
export function dateToString_M(info: DateInfo): string {
  return longMonths[info.month - 1] + " " + padWithZeros(info.day, 2);
}

// Year/month (InvariantCulture "yyyy MMMM"): "2009 June"
export function dateToString_Y(info: DateInfo): string {
  return info.year + " " + longMonths[info.month - 1];
}

// Everything a standard format specifier needs beyond the wall-clock fields.
// DateTime and DateTimeOffset differ only in these, so they share the table below.
// Everything past `info` is a thunk: most specifiers need none of them, and each
// costs a time-zone conversion.
export interface FormatContext {
  info: DateInfo;
  utcInfo: () => DateInfo;
  // "O"/"o" and "s" are ISO 8601, which Temporal renders itself — and renders
  // tick-exactly — so the owning module supplies them rather than this file
  // reassembling the fields by hand.
  roundTrip: () => string;
  sortable: () => string;
  // Trails the no-format rendering. A DateTimeOffset shows its offset there; a
  // DateTime has none to show.
  defaultSuffix: string;
  // Drive the K and z specifiers of a custom format.
  kind: DateTimeKind;
  hostOffsetString: () => string;
}

// .NET's standard date/time format specifiers, InvariantCulture. Anything that is
// not a single recognised letter is treated as a custom format string.
export function dateToString(ctx: FormatContext, format?: string): string {
  const info = ctx.info;

  if (typeof format !== "string") {
    return dateToString_d(info) + " " + dateToString_T(info) + ctx.defaultSuffix;
  }

  if (format.length === 1) {
    switch (format) {
      case "D": return dateToString_D(info);
      case "d": return dateToString_d(info);
      case "F": return dateToString_D(info) + " " + dateToString_T(info);
      case "f": return dateToString_D(info) + " " + dateToString_t(info);
      case "G": return dateToString_d(info) + " " + dateToString_T(info);
      case "g": return dateToString_d(info) + " " + dateToString_t(info);
      case "M": case "m": return dateToString_M(info);
      case "O": case "o": return ctx.roundTrip();
      case "R": case "r": return dateToString_R(ctx.utcInfo());
      case "s": return ctx.sortable();
      case "T": return dateToString_T(info);
      case "t": return dateToString_t(info);
      case "u": return dateToString_u(ctx.utcInfo());
      case "U": {
        const utc = ctx.utcInfo();
        return dateToString_D(utc) + " " + dateToString_T(utc);
      }
      case "Y": case "y": return dateToString_Y(info);
      default: throw new Exception("Unrecognized Date print format");
    }
  }

  return dateToStringWithCustomFormat(info, format, ctx.kind, ctx.hostOffsetString());
}

// The only date words .NET's invariant parser recognises: month names, weekday names,
// meridiem designators and zone markers (plus the ISO "T" separator). Anything else is
// rejected. Used to reject JS-permissive inputs (see `parseRaw`).
const recognizedDateWords = new Set([
  "january", "february", "march", "april", "may", "june",
  "july", "august", "september", "october", "november", "december",
  "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "sept", "oct", "nov", "dec",
  "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
  "mon", "tue", "wed", "thu", "fri", "sat", "sun",
  "am", "pm", "gmt", "utc", "ut", "t", "z",
]);

// The ticks below the millisecond (0..9999) written in the seconds fraction of
// `input`, e.g. "…:30.6175425" -> 5425.
//
// `parseRaw` resolves the instant through the JS `Date` built-in, which is
// millisecond-precise, so those digits would otherwise be lost. Anchoring on
// ":ss." keeps date separators (as in "15.06.2009") from matching. Digits past
// the 7th are truncated, matching how TimeOnly/TimeSpan parsing treats them.
export function subMillisecondTicks(input: string): number {
  const m = /:\d{1,2}\.(\d+)/.exec(input);
  return m == null ? 0 : +m[1].padEnd(7, "0").substring(3, 7);
}

// A trailing UTC designator, if the input carries one. The JS `Date` built-in
// applies the offset but does not report whether there was one, and callers need
// that: .NET gives a parsed value Kind=Local when an offset is present and
// Kind=Unspecified when it is not.
//
// The offset is only recognised after a time, so that the trailing "-11" of a
// date like "2014-09-11" is not mistaken for one.
function detectOffset(input: string): Offset {
  const m = /\d\d:\d\d(?::\d\d)?(?:\.\d+)?\s*(Z|[+-]\d\d:?(?:\d\d)?)\s*$/i.exec(input);
  if (m == null) {
    return null;
  }

  const designator = m[1];
  if (designator.toUpperCase() === "Z") {
    return "Z";
  }

  const digits = designator.substring(1).replace(":", "");
  const minutes = +digits.substring(0, 2) * 60 + (digits.length > 2 ? +digits.substring(2) : 0);
  return designator[0] === "-" ? -minutes : minutes;
}

// Lenient .NET-compatible parse. Returns a JS Date (the parsed instant) and the
// detected offset, if any. Callers read the wall-clock fields and build the
// appropriate Temporal value.
export function parseRaw(input: string): [Date, Offset] {
  function fail() {
    throw new Exception(`The string is not a valid Date: ${input}`);
  }

  if (input == null || input.trim() === "") {
    fail();
  }

  if ((input.match(/[a-z]+/gi) ?? []).some(word => !recognizedDateWords.has(word.toLowerCase()))) {
    fail();
  }

  // ISO dates without TZ are parsed as UTC. Adding time without TZ keeps them local.
  if (input.length === 10 && input[4] === "-" && input[7] === "-") {
    input += "T00:00:00";
  }
  let date = new Date(input);
  let offset: Offset = detectOffset(input);

  if (isNaN(date.getTime())) {
    // Try to check strings JS Date cannot parse (see #1045, #1422)
    const m = /^\s*(\d+[^\w\s:]\d+[^\w\s:]\d+)?\s*(\d+:\d+(?::\d+(?:\.\d+)?)?)?\s*([AaPp][Mm])?\s*(Z|[+-]([01]?\d):?([0-5]?\d)?)?\s*$/.exec(input);
    if (m != null) {
      let baseDate: Date;
      let timeInSeconds = 0;
      if (m[2] != null) {
        const timeParts = m[2].split(":");
        const hourPart = parseInt(timeParts[0], 10);
        timeInSeconds =
          hourPart * 3600 +
          parseInt(timeParts[1] || "0", 10) * 60 +
          parseFloat(timeParts[2] || "0");
        if (m[3] != null && m[3].toUpperCase() === "PM" && hourPart < 12) {
          timeInSeconds += 12 * 3600;
        } else if (m[3] != null && m[3].toUpperCase() === "AM" && hourPart === 12) {
          timeInSeconds -= 12 * 3600;
        }
      }
      if (m[4] != null) { // There's an offset, parse as UTC
        if (m[1] != null) {
          baseDate = new Date(m[1] + " UTC");
        } else {
          const d = new Date();
          baseDate = new Date(d.getUTCFullYear() + "/" + (d.getUTCMonth() + 1) + "/" + d.getUTCDate());
        }
        if (m[4] === "Z") {
          offset = "Z";
        } else {
          let offsetInMinutes = parseInt(m[5], 10) * 60 + parseInt(m[6] || "0", 10);
          if (m[4][0] === "-") {
            offsetInMinutes *= -1;
          }
          offset = offsetInMinutes;
          timeInSeconds -= offsetInMinutes * 60;
        }
      } else {
        if (m[1] != null) {
          baseDate = new Date(m[1]);
        } else {
          const d = new Date();
          baseDate = new Date(d.getFullYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate());
        }
      }
      date = new Date(baseDate.getTime() + timeInSeconds * 1000);
      // correct for daylight savings time
      date = new Date(date.getTime() + (date.getTimezoneOffset() - baseDate.getTimezoneOffset()) * 60_000);
    } else {
      fail();
    }

    // Check again the date is valid after transformations, see #2229
    if (isNaN(date.getTime())) {
      fail();
    }
  }

  return [date, offset];
}
