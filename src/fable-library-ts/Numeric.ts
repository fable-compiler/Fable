import { IComparable } from "./Util.ts";

export const symbol = Symbol("numeric");

export interface CustomNumeric extends IComparable<Numeric> {
    [symbol](): {
        multiply(y: Numeric): Numeric,
        toPrecision(sd?: number): string,
        toExponential(dp?: number): string,
        toFixed(dp?: number): string,
        toHex(): string,
    }
}

export type Numeric = number | bigint | CustomNumeric;

export function isNumeric(x: any) {
    return typeof x === "number" || typeof x === "bigint" || x?.[symbol];
}

export function isIntegral(x: Numeric) {
    // Not perfect, because in JS we can't distinguish between 1.0 and 1
    return typeof x === "number" && Number.isInteger(x) || typeof x === "bigint";
}

export function compare(x: Numeric, y: number) {
    if (typeof x === "number") {
        return x < y ? -1 : (x > y ? 1 : 0);
    } else if (typeof x === "bigint") {
        return x < y ? -1 : (x > y ? 1 : 0);
    } else {
        return x.CompareTo(y);
    }
}

export function multiply(x: Numeric, y: number) {
    if (typeof x === "number") {
        return x * y;
    } else if (typeof x === "bigint") {
        return x * BigInt(y);
    } else {
        return x[symbol]().multiply(y);
    }
}

export function divide(x: Numeric, y: number) {
    if (typeof x === "number") {
        return x / y;
    } else if (typeof x === "bigint") {
        // Keep the fractional part so the caller can round it like .NET does
        return Number(x) / y;
    } else {
        return x[symbol]().multiply(1 / y);
    }
}

export function toFixed(x: Numeric, dp?: number) {
    if (typeof x === "number") {
        return x.toFixed(dp);
    } else if (typeof x === "bigint") {
        return x.toString();
    } else {
        return x[symbol]().toFixed(dp);
    }
}

export function toPrecision(x: Numeric, sd?: number) {
    if (typeof x === "number") {
        return x.toPrecision(sd);
    } else if (typeof x === "bigint") {
        return x.toString();
    } else {
        return x[symbol]().toPrecision(sd);
    }
}

// A bigint can hold more significant digits than a double, so the mantissa is rounded on the
// decimal digits themselves instead of going through Number
function bigintToExponential(x: bigint, dp?: number) {
    const sign = x < 0n ? "-" : "";
    let digits = (x < 0n ? -x : x).toString();
    if (x === 0n) {
        return sign + (dp ? "0." + "0".repeat(dp) : "0") + "e+0";
    }
    dp = dp ?? digits.length - 1;
    let exponent = digits.length - 1;
    if (digits.length > dp + 1) {
        // .NET rounds the discarded digits away from zero
        const roundUp = digits.charCodeAt(dp + 1) >= 53;
        digits = (BigInt(digits.slice(0, dp + 1)) + (roundUp ? 1n : 0n)).toString();
        if (digits.length > dp + 1) {
            digits = digits.slice(0, dp + 1);
            exponent += 1;
        }
    } else {
        digits = digits.padEnd(dp + 1, "0");
    }
    const mantissa = dp > 0 ? digits[0] + "." + digits.slice(1) : digits[0];
    return sign + mantissa + "e+" + exponent;
}

export function toExponential(x: Numeric, dp?: number) {
    if (typeof x === "number") {
        return x.toExponential(dp);
    } else if (typeof x === "bigint") {
        return bigintToExponential(x, dp);
    } else {
        return x[symbol]().toExponential(dp);
    }
}

function isExactTie(x: number, k: number) {
    if (k >= 0) {
        const t = x * 2 ** (k + 1);
        return Number.isInteger(t) && t % 2 !== 0;
    } else if (Number.isInteger(x)) {
        const p = 10n ** BigInt(-k);
        return BigInt(Math.abs(x)) % p === p / 2n;
    } else {
        return false;
    }
}

function truncateLastDigit(digits: string) {
    const s = digits.slice(0, -1);
    return s.endsWith(".") ? s.slice(0, -1) : s;
}

function isLastDigitEven(digits: string) {
    return digits.charCodeAt(digits.length - 1) % 2 === 0;
}

// .NET standard numeric formats give an exact midpoint of a double the even digit
export function toFixedHalfEven(x: Numeric, dp: number) {
    if (typeof x === "number" && dp < 100 && isExactTie(x, dp)) {
        const truncated = truncateLastDigit(x.toFixed(dp + 1));
        return isLastDigitEven(truncated) ? truncated : x.toFixed(dp);
    } else {
        return toFixed(x, dp);
    }
}

export function toExponentialHalfEven(x: Numeric, dp: number) {
    if (typeof x === "number" && Number.isFinite(x) && dp < 100) {
        const exact = x.toExponential(dp + 1);
        const eIdx = exact.indexOf("e");
        const mantissa = exact.slice(0, eIdx);
        if (mantissa.endsWith("5") && isExactTie(x, dp - Number(exact.slice(eIdx + 1)))) {
            const truncated = truncateLastDigit(mantissa);
            return isLastDigitEven(truncated) ? truncated + exact.slice(eIdx) : x.toExponential(dp);
        }
    }
    return toExponential(x, dp);
}

export function toPrecisionHalfEven(x: Numeric, sd: number) {
    if (typeof x !== "number" || !(sd > 0)) {
        return toPrecision(x, sd);
    }
    const s = toExponentialHalfEven(x, sd - 1);
    if (s === x.toExponential(sd - 1)) {
        return x.toPrecision(sd);
    }
    // Same notation as Number.prototype.toPrecision
    const eIdx = s.indexOf("e");
    const e = Number(s.slice(eIdx + 1));
    if (e < -6 || e >= sd) {
        return s;
    }
    const sign = s.startsWith("-") ? "-" : "";
    const digits = s.slice(sign.length, eIdx).replace(".", "");
    return e < 0
        ? sign + "0." + "0".repeat(-e - 1) + digits
        : sign + digits.slice(0, e + 1) + (sd > e + 1 ? "." + digits.slice(e + 1) : "");
}

export function toRadix(x: Numeric, radix: number) {
    if (typeof x === "number") {
        return (Number(x) >>> 0).toString(radix);
    } else if (typeof x === "bigint") {
        // TODO: properly handle other bit sizes
        return BigInt.asUintN(64, x).toString(radix);
    } else if (radix === 16) {
        return x[symbol]().toHex();
    } else {
        return String(x);
    }
}

export function toHex(x: Numeric) {
    return toRadix(x, 16);
}
