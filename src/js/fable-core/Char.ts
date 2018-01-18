const digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

export function isLetter(input: string) {
    return input.toLowerCase() !== input.toUpperCase();
}

export function isUpper(input: string) {
    const upper = input.toUpperCase();
    return upper === input && upper !== input.toLowerCase();
}

export function isLower(input: string) {
    const lower = input.toLowerCase();
    return lower === input && lower !== input.toUpperCase();
}

export function isDigit(input: string) {
    return digits.indexOf(input) >= 0;
}

export function isLetterOrDigit(input: string) {
    return isLetter(input) || isDigit(input);
}

export function isWhiteSpace(input: string) {
    return /\s/.test(input);
}

export function parse(input: string) {
    if (input.length === 1) {
        return input[0];
    } else {
        throw Error ("String must be exactly one character long.");
    }
}
