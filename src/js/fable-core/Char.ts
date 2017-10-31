export function isLetter(input: string) {
    if (typeof input !== "string") {
        return false;
    }
    if (input.length !== 1) {
        return false;
    }
    if (input.toLowerCase() !== input.toUpperCase()) {
        return true;
    }
    return false;
}
