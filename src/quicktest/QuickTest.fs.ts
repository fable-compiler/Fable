import { isNullOrEmpty, toFail, printf, toConsole } from "./.fable/fable-library.3.1.1/String.js";
import { equals } from "./.fable/fable-library.3.1.1/Util.js";
import { startImmediate } from "./.fable/fable-library.3.1.1/Async.js";
import { singleton } from "./.fable/fable-library.3.1.1/AsyncBuilder.js";

export function log(o: any): void {
    toConsole(printf("%O"))(o);
}

export function equal<a$>(expected: a$, actual: a$): void {
    const areEqual = equals(expected, actual);
    toConsole(printf("%A = %A \u003e %b"))(expected)(actual)(areEqual);
    if (!areEqual) {
        toFail(printf("[ASSERT ERROR] Expected %A but got %A"))(expected)(actual);
    }
}

export function throwsError<a>(expected: string, f: (arg0: void) => a): void {
    let success;
    try {
        const value = f();
        void value;
        success = true;
    }
    catch (e) {
        if (!isNullOrEmpty(expected)) {
            equal(e.message, expected);
        }
        success = false;
    }
    equal(false, success);
}

export function testCase(msg: string, f: (arg0: void) => void): void {
    try {
        toConsole(printf("%s"))(msg);
        f();
    }
    catch (ex) {
        const arg10_1 = ex.message;
        toConsole(printf("%s"))(arg10_1);
        if ((ex.message !== null) ? (!(ex.message.indexOf("[ASSERT ERROR]") === 0)) : false) {
            const arg10_2 = ex.stack;
            toConsole(printf("%s"))(arg10_2);
        }
    }
    toConsole(printf(""));
}

export function testCaseAsync(msg: string, f: (arg0: void) => any): void {
    testCase(msg, (): void => {
        startImmediate(singleton.Delay((): any => singleton.TryWith(singleton.Delay((): any => singleton.Bind(f(), (): any => singleton.Return())), (_arg2: Error): any => {
            const ex = _arg2;
            const arg10 = ex.message;
            toConsole(printf("%s"))(arg10);
            if ((ex.message !== null) ? (!(ex.message.indexOf("[ASSERT ERROR]") === 0)) : false) {
                const arg10_1 = ex.stack;
                toConsole(printf("%s"))(arg10_1);
                return singleton.Zero();
            }
            else {
                return singleton.Zero();
            }
        })));
    });
}

export function measureTime<a$>(f: (arg0: void) => void): a$ {
    //js
    const startTime = process.hrtime();
    f();
    const elapsed = process.hrtime(startTime);
    console.log("Ms:", elapsed[0] * 1e3 + elapsed[1] / 1e6);
    //!js
}

