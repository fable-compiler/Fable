/**
 * DateTimeOffset functions.
 *
 * Note: Date instances are always DateObjects in local
 * timezone (because JS dates are all kinds of messed up).
 * A local date returns UTC epoch when `.getTime()` is called.
 *
 * Basically; invariant: date.getTime() always return UTC time.
 */
import { toInt64, toFloat64 } from "./BigInt.js";
import { compareDates, dateOffset, padWithZeros } from "./Util.js";
export function kind(value) {
    return value.kind || 0;
}
export function unixEpochMillisecondsToTicks(ms, offset) {
    return toInt64(((BigInt(ms) + 62135596800000n) + BigInt(offset)) * 10000n);
}
export function ticksToUnixEpochMilliseconds(ticks) {
    return Number(((BigInt(ticks) / 10000n) - 62135596800000n));
}
export function dateOffsetToString(offset) {
    const isMinus = offset < 0;
    offset = Math.abs(offset);
    const hours = ~~(offset / 3600000);
    const minutes = (offset % 3600000) / 60000;
    return (isMinus ? "-" : "+") +
        padWithZeros(hours, 2) + ":" +
        padWithZeros(minutes, 2);
}
export function dateToHalfUTCString(date, half) {
    const str = date.toISOString();
    return half === "first"
        ? str.substring(0, str.indexOf("T"))
        : str.substring(str.indexOf("T") + 1, str.length - 1);
}
function dateToISOString(d, utc) {
    if (utc) {
        return d.toISOString();
    }
    else {
        // JS Date is always local
        const printOffset = d.kind == null ? true : d.kind === 2 /* DateKind.Local */;
        return padWithZeros(d.getFullYear(), 4) + "-" +
            padWithZeros(d.getMonth() + 1, 2) + "-" +
            padWithZeros(d.getDate(), 2) + "T" +
            padWithZeros(d.getHours(), 2) + ":" +
            padWithZeros(d.getMinutes(), 2) + ":" +
            padWithZeros(d.getSeconds(), 2) + "." +
            padWithZeros(d.getMilliseconds(), 3) +
            (printOffset ? dateOffsetToString(d.getTimezoneOffset() * -60000) : "");
    }
}
function dateToISOStringWithOffset(dateWithOffset, offset) {
    const str = dateWithOffset.toISOString();
    return str.substring(0, str.length - 1) + dateOffsetToString(offset);
}
function dateToStringWithCustomFormat(date, format, utc) {
    return format.replace(/(\w)\1*/g, (match) => {
        let rep = Number.NaN;
        switch (match.substring(0, 1)) {
            case "y":
                const y = utc ? date.getUTCFullYear() : date.getFullYear();
                rep = match.length < 4 ? y % 100 : y;
                break;
            case "M":
                rep = (utc ? date.getUTCMonth() : date.getMonth()) + 1;
                break;
            case "d":
                rep = utc ? date.getUTCDate() : date.getDate();
                break;
            case "H":
                rep = utc ? date.getUTCHours() : date.getHours();
                break;
            case "h":
                const h = utc ? date.getUTCHours() : date.getHours();
                rep = h > 12 ? h % 12 : h;
                break;
            case "m":
                rep = utc ? date.getUTCMinutes() : date.getMinutes();
                break;
            case "s":
                rep = utc ? date.getUTCSeconds() : date.getSeconds();
                break;
            case "f":
                rep = utc ? date.getUTCMilliseconds() : date.getMilliseconds();
                break;
        }
        if (Number.isNaN(rep)) {
            return match;
        }
        else {
            return padWithZeros(rep, match.length);
        }
    });
}
function dateToStringWithOffset(date, format) {
    const d = new Date(date.getTime() + (date.offset ?? 0));
    if (typeof format !== "string") {
        return d.toISOString().replace(/\.\d+/, "").replace(/[A-Z]|\.\d+/g, " ") + dateOffsetToString((date.offset ?? 0));
    }
    else if (format.length === 1) {
        switch (format) {
            case "D":
            case "d": return dateToHalfUTCString(d, "first");
            case "T":
            case "t": return dateToHalfUTCString(d, "second");
            case "O":
            case "o": return dateToISOStringWithOffset(d, (date.offset ?? 0));
            default: throw new Error("Unrecognized Date print format");
        }
    }
    else {
        return dateToStringWithCustomFormat(d, format, true);
    }
}
function dateToStringWithKind(date, format) {
    const utc = date.kind === 1 /* DateKind.UTC */;
    if (typeof format !== "string") {
        return utc ? date.toUTCString() : date.toLocaleString();
    }
    else if (format.length === 1) {
        switch (format) {
            case "D":
            case "d":
                return utc ? dateToHalfUTCString(date, "first") : date.toLocaleDateString();
            case "T":
            case "t":
                return utc ? dateToHalfUTCString(date, "second") : date.toLocaleTimeString();
            case "O":
            case "o":
                return dateToISOString(date, utc);
            default:
                throw new Error("Unrecognized Date print format");
        }
    }
    else {
        return dateToStringWithCustomFormat(date, format, utc);
    }
}
export function toString(date, format, _provider) {
    return date.offset != null
        ? dateToStringWithOffset(date, format)
        : dateToStringWithKind(date, format);
}
export function DateTime(value, kind) {
    const d = new Date(value);
    d.kind = (kind == null ? 0 /* DateKind.Unspecified */ : kind) | 0;
    return d;
}
export function fromTicks(ticks, kind) {
    kind = kind != null ? kind : 2 /* DateKind.Local */; // better default than Unspecified
    let date = DateTime(ticksToUnixEpochMilliseconds(ticks), kind);
    // Ticks are local to offset (in this case, either UTC or Local/Unknown).
    // If kind is anything but UTC, that means that the tick number was not
    // in utc, thus getTime() cannot return UTC, and needs to be shifted.
    if (kind !== 1 /* DateKind.UTC */) {
        date = DateTime(date.getTime() - dateOffset(date), kind);
    }
    return date;
}
export function fromDateTimeOffset(date, kind) {
    switch (kind) {
        case 1 /* DateKind.UTC */: return DateTime(date.getTime(), 1 /* DateKind.UTC */);
        case 2 /* DateKind.Local */: return DateTime(date.getTime(), 2 /* DateKind.Local */);
        default:
            const d = DateTime(date.getTime() + (date.offset ?? 0), kind);
            return DateTime(d.getTime() - dateOffset(d), kind);
    }
}
export function getTicks(date) {
    return unixEpochMillisecondsToTicks(date.getTime(), dateOffset(date));
}
export function minValue() {
    // This is "0001-01-01T00:00:00.000Z", actual JS min value is -8640000000000000
    return DateTime(-62135596800000, 0 /* DateKind.Unspecified */);
}
export function maxValue() {
    // This is "9999-12-31T23:59:59.999Z", actual JS max value is 8640000000000000
    return DateTime(253402300799999, 0 /* DateKind.Unspecified */);
}
export function parseRaw(input) {
    function fail() {
        throw new Error(`The string is not a valid Date: ${input}`);
    }
    if (input == null || input.trim() === "") {
        fail();
    }
    // ISO dates without TZ are parsed as UTC. Adding time without TZ keeps them local.
    if (input.length === 10 && input[4] === "-" && input[7] === "-") {
        input += "T00:00:00";
    }
    let date = new Date(input);
    let offset = null;
    if (isNaN(date.getTime())) {
        // Try to check strings JS Date cannot parse (see #1045, #1422)
        // tslint:disable-next-line:max-line-length
        const m = /^\s*(\d+[^\w\s:]\d+[^\w\s:]\d+)?\s*(\d+:\d+(?::\d+(?:\.\d+)?)?)?\s*([AaPp][Mm])?\s*(Z|[+-]([01]?\d):?([0-5]?\d)?)?\s*$/.exec(input);
        if (m != null) {
            let baseDate;
            let timeInSeconds = 0;
            if (m[2] != null) {
                const timeParts = m[2].split(":");
                timeInSeconds =
                    parseInt(timeParts[0], 10) * 3600 +
                        parseInt(timeParts[1] || "0", 10) * 60 +
                        parseFloat(timeParts[2] || "0");
                if (m[3] != null && m[3].toUpperCase() === "PM") {
                    timeInSeconds += 720;
                }
            }
            if (m[4] != null) { // There's an offset, parse as UTC
                if (m[1] != null) {
                    baseDate = new Date(m[1] + " UTC");
                }
                else {
                    const d = new Date();
                    baseDate = new Date(d.getUTCFullYear() + "/" + (d.getUTCMonth() + 1) + "/" + d.getUTCDate());
                }
                if (m[4] === "Z") {
                    offset = "Z";
                }
                else {
                    let offsetInMinutes = parseInt(m[5], 10) * 60 + parseInt(m[6] || "0", 10);
                    if (m[4][0] === "-") {
                        offsetInMinutes *= -1;
                    }
                    offset = offsetInMinutes;
                    timeInSeconds -= offsetInMinutes * 60;
                }
            }
            else {
                if (m[1] != null) {
                    baseDate = new Date(m[1]);
                }
                else {
                    const d = new Date();
                    baseDate = new Date(d.getFullYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate());
                }
            }
            date = new Date(baseDate.getTime() + timeInSeconds * 1000);
            // correct for daylight savings time
            date = new Date(date.getTime() + (date.getTimezoneOffset() - baseDate.getTimezoneOffset()) * 60000);
        }
        else {
            fail();
        }
        // Check again the date is valid after transformations, see #2229
        if (isNaN(date.getTime())) {
            fail();
        }
    }
    return [date, offset];
}
export function parse(str, detectUTC = false) {
    const [date, offset] = parseRaw(str);
    // .NET always parses DateTime as Local if there's offset info (even "Z")
    // Newtonsoft.Json uses UTC if the offset is "Z"
    const kind = offset != null
        ? (detectUTC && offset === "Z" ? 1 /* DateKind.UTC */ : 2 /* DateKind.Local */)
        : 0 /* DateKind.Unspecified */;
    return DateTime(date.getTime(), kind);
}
export function tryParse(v, defValue) {
    try {
        defValue.contents = parse(v);
        return true;
    }
    catch (_err) {
        return false;
    }
}
export function create(year, month, day, h = 0, m = 0, s = 0, ms = 0, kind) {
    const date = kind === 1 /* DateKind.UTC */
        ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms))
        : new Date(year, month - 1, day, h, m, s, ms);
    if (year <= 99) {
        if (kind === 1 /* DateKind.UTC */) {
            date.setUTCFullYear(year, month - 1, day);
        }
        else {
            date.setFullYear(year, month - 1, day);
        }
    }
    const dateValue = date.getTime();
    if (isNaN(dateValue)) {
        throw new Error("The parameters describe an unrepresentable Date.");
    }
    return DateTime(dateValue, kind);
}
export function now() {
    return DateTime(Date.now(), 2 /* DateKind.Local */);
}
export function utcNow() {
    return DateTime(Date.now(), 1 /* DateKind.UTC */);
}
export function today() {
    return date(now());
}
export function isLeapYear(year) {
    return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
}
export function daysInMonth(year, month) {
    return month === 2
        ? (isLeapYear(year) ? 29 : 28)
        : (month >= 8 ? (month % 2 === 0 ? 31 : 30) : (month % 2 === 0 ? 30 : 31));
}
export function toUniversalTime(date) {
    return date.kind === 1 /* DateKind.UTC */ ? date : DateTime(date.getTime(), 1 /* DateKind.UTC */);
}
export function toLocalTime(date) {
    return date.kind === 2 /* DateKind.Local */ ? date : DateTime(date.getTime(), 2 /* DateKind.Local */);
}
export function specifyKind(d, kind) {
    return create(year(d), month(d), day(d), hour(d), minute(d), second(d), millisecond(d), kind);
}
export function timeOfDay(d) {
    return hour(d) * 3600000
        + minute(d) * 60000
        + second(d) * 1000
        + millisecond(d);
}
export function date(d) {
    return create(year(d), month(d), day(d), 0, 0, 0, 0, d.kind);
}
export function day(d) {
    return d.kind === 1 /* DateKind.UTC */ ? d.getUTCDate() : d.getDate();
}
export function hour(d) {
    return d.kind === 1 /* DateKind.UTC */ ? d.getUTCHours() : d.getHours();
}
export function millisecond(d) {
    return d.kind === 1 /* DateKind.UTC */ ? d.getUTCMilliseconds() : d.getMilliseconds();
}
export function minute(d) {
    return d.kind === 1 /* DateKind.UTC */ ? d.getUTCMinutes() : d.getMinutes();
}
export function month(d) {
    return (d.kind === 1 /* DateKind.UTC */ ? d.getUTCMonth() : d.getMonth()) + 1;
}
export function second(d) {
    return d.kind === 1 /* DateKind.UTC */ ? d.getUTCSeconds() : d.getSeconds();
}
export function year(d) {
    return d.kind === 1 /* DateKind.UTC */ ? d.getUTCFullYear() : d.getFullYear();
}
export function dayOfWeek(d) {
    return d.kind === 1 /* DateKind.UTC */ ? d.getUTCDay() : d.getDay();
}
export function dayOfYear(d) {
    const _year = year(d);
    const _month = month(d);
    let _day = day(d);
    for (let i = 1; i < _month; i++) {
        _day += daysInMonth(_year, i);
    }
    return _day;
}
export function add(d, ts) {
    const newDate = DateTime(d.getTime() + ts, d.kind);
    if (d.kind === 2 /* DateKind.Local */) {
        const oldTzOffset = d.getTimezoneOffset();
        const newTzOffset = newDate.getTimezoneOffset();
        return oldTzOffset !== newTzOffset
            ? DateTime(newDate.getTime() + (newTzOffset - oldTzOffset) * 60000, d.kind)
            : newDate;
    }
    else {
        return newDate;
    }
}
export function addDays(d, v) {
    return add(d, v * 86400000);
}
export function addHours(d, v) {
    return add(d, v * 3600000);
}
export function addMinutes(d, v) {
    return add(d, v * 60000);
}
export function addSeconds(d, v) {
    return add(d, v * 1000);
}
export function addMilliseconds(d, v) {
    return add(d, v);
}
export function addTicks(d, v) {
    return add(d, toFloat64(v / 10000n));
}
export function addYears(d, v) {
    const newMonth = month(d);
    const newYear = year(d) + v;
    const _daysInMonth = daysInMonth(newYear, newMonth);
    const newDay = Math.min(_daysInMonth, day(d));
    return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind);
}
export function addMonths(d, v) {
    let newMonth = month(d) + v;
    let newMonth_ = 0;
    let yearOffset = 0;
    if (newMonth > 12) {
        newMonth_ = newMonth % 12;
        yearOffset = Math.floor(newMonth / 12);
        newMonth = newMonth_;
    }
    else if (newMonth < 1) {
        newMonth_ = 12 + newMonth % 12;
        yearOffset = Math.floor(newMonth / 12) + (newMonth_ === 12 ? -1 : 0);
        newMonth = newMonth_;
    }
    const newYear = year(d) + yearOffset;
    const _daysInMonth = daysInMonth(newYear, newMonth);
    const newDay = Math.min(_daysInMonth, day(d));
    return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind);
}
export function subtract(d, that) {
    return typeof that === "number"
        ? add(d, -that)
        : d.getTime() - that.getTime();
}
export function toLongDateString(d) {
    return d.toDateString();
}
export function toShortDateString(d) {
    return d.toLocaleDateString();
}
export function toLongTimeString(d) {
    return d.toLocaleTimeString();
}
export function toShortTimeString(d) {
    return d.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
}
export function equals(d1, d2) {
    return d1.getTime() === d2.getTime();
}
export const compare = compareDates;
export const compareTo = compareDates;
export function op_Addition(x, y) {
    return add(x, y);
}
export function op_Subtraction(x, y) {
    return subtract(x, y);
}
export function isDaylightSavingTime(x) {
    const jan = new Date(x.getFullYear(), 0, 1);
    const jul = new Date(x.getFullYear(), 6, 1);
    return isDST(jan.getTimezoneOffset(), jul.getTimezoneOffset(), x.getTimezoneOffset());
}
function isDST(janOffset, julOffset, tOffset) {
    return Math.min(janOffset, julOffset) === tOffset;
}
export default DateTime;
