// Use ES6 export syntax to check transpilation
export function fooGenerator(i) {
    let s = "";
    for (var j=0; j<i; j++)
        s += "foo";
    return s;
}