import { hours, minutes, seconds, milliseconds } from "./TimeSpan.js";
import { padWithZeros } from "./Util.js";
const millisecondsPerDay = 86400000;
export function create(h = 0, m = 0, s = 0, ms = 0) {
    if (h < 0 || m < 0 || s < 0 || ms < 0)
        throw new Error("The parameters describe an unrepresentable TimeOnly.");
    return h * 3600000 + m * 60000 + s * 1000 + ms;
}
export function fromTicks(ticks) {
    return Number(BigInt(ticks) / 10000n);
}
export function fromTimeSpan(timeSpan) {
    if (timeSpan < 0 || timeSpan >= millisecondsPerDay)
        throw new Error("The TimeSpan describes an unrepresentable TimeOnly.");
    return timeSpan;
}
export function fromDateTime(d) {
    return d.kind === 1 /* DateKind.UTC */
        ? create(d.getUTCHours(), d.getUTCMinutes(), d.getUTCSeconds(), d.getUTCMilliseconds())
        : create(d.getHours(), d.getMinutes(), d.getSeconds(), d.getMilliseconds());
}
export function maxValue() {
    // This is "23:59:59.999"
    return millisecondsPerDay - 1;
}
export function add(t, ts, wrappedDays) {
    if (wrappedDays === undefined) {
        const t2 = (t + ts) % millisecondsPerDay;
        return t2 < 0 ? millisecondsPerDay + t2 : t2;
    }
    wrappedDays.contents = ts / millisecondsPerDay;
    let newMs = t + ts % millisecondsPerDay;
    if (newMs < 0) {
        wrappedDays.contents--;
        newMs += millisecondsPerDay;
    }
    else {
        if (newMs >= millisecondsPerDay) {
            wrappedDays.contents++;
            newMs -= millisecondsPerDay;
        }
    }
    return newMs;
}
export function addHours(t, h) {
    return add(t, h * 3600000);
}
export function addMinutes(t, m) {
    return add(t, m * 60000);
}
export function isBetween(t, start, end) {
    return start <= end
        ? (start <= t && end > t)
        : (start <= t || end > t);
}
export function toString(t, format = "t", _provider) {
    if (["r", "R", "o", "O", "t", "T"].indexOf(format) === -1) {
        throw new Error("Custom formats are not supported");
    }
    const base = `${padWithZeros(hours(t), 2)}:${padWithZeros(minutes(t), 2)}`;
    if (format === "t")
        return base;
    const s = padWithZeros(seconds(t), 2);
    // We're limited to millisecond precision, so the last 4 digits will always be 0
    return `${base}${format === "o" || format === "O" ? `:${s}.${padWithZeros(milliseconds(t), 3)}0000` : `:${s}`}`;
}
export function parse(str) {
    // Allowed format types:
    // hh:mm
    // hh:mm:ss
    // hh:mm:ss.fffffff
    const r = /^\s*([0-1]?\d|2[0-3])\s*:\s*([0-5]?\d)(\s*:\s*([0-5]?\d)(\.(\d+))?)?\s*$/.exec(str);
    if (r != null && r[1] != null && r[2] != null) {
        let ms = 0;
        let s = 0;
        const h = +r[1];
        const m = +r[2];
        if (r[4] != null) {
            s = +r[4];
        }
        if (r[6] != null) {
            // Depending on the number of decimals passed, we need to adapt the numbers
            switch (r[6].length) {
                case 1:
                    ms = +r[6] * 100;
                    break;
                case 2:
                    ms = +r[6] * 10;
                    break;
                case 3:
                    ms = +r[6];
                    break;
                case 4:
                    ms = +r[6] / 10;
                    break;
                case 5:
                    ms = +r[6] / 100;
                    break;
                case 6:
                    ms = +r[6] / 1000;
                    break;
                default:
                    ms = +r[6].substring(0, 7) / 10000;
                    break;
            }
        }
        return create(h, m, s, Math.trunc(ms));
    }
    throw new Error(`String '${str}' was not recognized as a valid TimeOnly.`);
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
export function op_Subtraction(left, right) {
    return add(left, -right);
}
