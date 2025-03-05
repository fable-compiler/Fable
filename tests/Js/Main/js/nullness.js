export function maybeUndefined(value) {
    if (value === "ok") {
        return value;
    }
    else {
        return undefined;
    }
}

export function maybeNull(value) {
    if (value === "ok") {
        return value;
    }
    else {
        return null;
    }
}
