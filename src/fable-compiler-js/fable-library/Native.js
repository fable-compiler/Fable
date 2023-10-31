export function Helpers_allocateArrayFromCons(cons, len) {
    if ((typeof cons) === "function") {
        return new cons(len);
    }
    else {
        return new Array(len);
    }
}
