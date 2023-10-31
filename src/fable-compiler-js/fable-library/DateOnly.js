import { DateTime, getTicks, dayOfYear as Date_dayOfYear, year as Date_year, month as Date_month, day as Date_day, daysInMonth as Date_daysInMonth, ticksToUnixEpochMilliseconds } from "./Date.js";
import { padWithZeros } from "./Util.js";
export function fromUnixMilliseconds(value) {
    return DateTime(value, 1 /* DateKind.UTC */);
}
export function create(year, month, day) {
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
export function dayNumber(d) {
    return Number((getTicks(d) / 864000000000n));
}
export function fromDayNumber(dayNumber) {
    const ticks = 864000000000n * BigInt(dayNumber);
    return fromUnixMilliseconds(ticksToUnixEpochMilliseconds(ticks));
}
export function fromDateTime(d) {
    return create(Date_year(d), Date_month(d), Date_day(d));
}
export function day(d) {
    return d.getUTCDate();
}
export function month(d) {
    return d.getUTCMonth() + 1;
}
export function year(d) {
    return d.getUTCFullYear();
}
export function dayOfWeek(d) {
    return d.getUTCDay();
}
export function dayOfYear(d) {
    return Date_dayOfYear(d);
}
export function toDateTime(d, time, kind = 0 /* DateKind.Unspecified */) {
    return DateTime(d.getTime() + time + (kind !== 1 /* DateKind.UTC */ ? d.getTimezoneOffset() : 0) * 60000, kind);
}
export function toString(d, format = "d", _provider) {
    if (["d", "o", "O"].indexOf(format) === -1) {
        throw new Error("Custom formats are not supported");
    }
    const y = padWithZeros(year(d), 4);
    const m = padWithZeros(month(d), 2);
    const dd = padWithZeros(day(d), 2);
    return format === "d" ? `${m}/${dd}/${y}` : `${y}-${m}-${dd}`;
}
export function parse(str) {
    function fail() {
        throw new Error(`String '${str}' was not recognized as a valid DateOnly.`);
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
                    y = new Date().getFullYear();
                    m = +r[1];
                    d = +r[3];
                }
                else {
                    // 12/2000 = December 1, 2000
                    m = +r[1];
                    y = +r[3];
                }
            }
            else {
                if (r[3].length > 2)
                    fail();
                // 2000/12 = December 1, 2000
                y = +r[1];
                m = +r[3];
            }
        }
        else {
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
        if (y > 0 && m > 0 && m < 13 && d > 0 && d <= Date_daysInMonth(y, m))
            return create(y, m, d);
    }
    return fail();
}
export function tryParse(v, defValue) {
    try {
        defValue.contents = parse(v);
        return true;
    }
    catch {
        return false;
    }
}
