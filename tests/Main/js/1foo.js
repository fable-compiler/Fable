"use strict";

const _exports = {

    foo: "foo",

    apply: function(f, x, y) {
        return f(x, y);
    },

    square: function(x) {
        return x * x;
    },

    add: function(x, y) {
        return x + y;
    },

    MyClass: class {
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
    },

    fooOptional: {
        Foo1() { return arguments.length },
        Foo2() { return arguments.length },
        Foo3() { return arguments.length }
    },

    MyJsClass: class {
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
};

export const { foo, apply, square, add, MyClass, MyJsClass, fooOptional } = _exports;
// module.exports = _exports;
