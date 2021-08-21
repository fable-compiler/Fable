local mod = {}
printf, toConsole = require("./.fable/fable-library.3.0.0-local-build-20210819-1229/String.js");
int32ToString = require("./.fable/fable-library.3.0.0-local-build-20210819-1229/Util.js");

mod.const hello = toConsole(printf("hello world"));

mod.const a = 2 + 2;

mod.const b = 3 - 1;

mod.const c = a + b;

mod.function fn(a_1, b_1, c_1) 
    const d = ((a_1 + b_1) + c_1) | 0;
    toConsole(printf("%A %A"))(b_1)(d);
end

mod.function execute() 
    let arg10;
    let copyOfStruct = a;
    arg10 = int32ToString(copyOfStruct);
    toConsole(printf("%s"))(arg10);
    toConsole(printf("c"));
end

return mod
