function isChar(input: string) {
    return typeof input === "string" && input.length === 1;
}

export function isLetter(input: string) {
    return isChar(input) && input.toLowerCase() !== input.toUpperCase();
}

export function isUpper(input: string) {
    return isLetter(input) && input.toUpperCase() === input;
}

export function isLower(input: string) {
    return isLetter(input) && input.toLowerCase() === input;
}

export function isDigit(input: string) {
    return isChar(input) && /\d/.test(input);
}

export function isLetterOrDigit(input: string) {
    return isChar(input) &&
        (input.toLowerCase() !== input.toUpperCase() || /\d/.test(input));
}

export function isWhiteSpace(input: string) {
    return isChar(input) && /\s/.test(input);
}
