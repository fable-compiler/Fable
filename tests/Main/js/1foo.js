"use strict";

export const foo = "foo";

export default {
    myKey: "a secret"
}


export function apply(f, x, y) {
    return f(x, y);
}

export function square(x) {
    return x * x;
}

export function add(x, y) {
    return x + y;
}

export function add4(x) {
    return x + 4;
}

export function addWithSurprise(x, y) {
    return x + y + 3;
}

export function add2Arguments(x, y) {
    return x + y;
}

export function add10Arguments(a, b, c, d, e, f, g, h, i, j) {
    return a + b + c + d + e + f + g + h + i + j;
}

export class MyClass {
    constructor(v) {
        this.__value = typeof v === "string" ? v : "haha";
    }

    get value() {
        return this.__value;
    }

    static foo(i) {
        return typeof i === "number" ? i * i : "foo";
    }

    bar(s) {
        return typeof s === "string" ? s.toUpperCase() : "bar";
    }
}

export const fooOptional = {
    Foo1() { return arguments.length },
    Foo2() { return arguments.length },
    Foo3() { return arguments.length },
    toString() {
        return "much foo, much awesome"
    },
}

export function addFooString(x) {
    return x + " foo"
}

export class MyJsClass {
    constructor(x) {
        this.value = x;
    }
    foo() {
        return this.bar() + this.value;
    }
    static fuzzyMultiply(x, i) {
        return x * (i - 1);
    }
}

export function handleClass(constructor) {
    const x = new constructor(4);
    x.Times = 2;
    return x.SaySomethingTo("Narumi");
}
