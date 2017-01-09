"use strict";

exports.foo = "foo"

exports.MyClass = class {
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