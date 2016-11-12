// Use ES6 export syntax to check transpilation
export function fooGenerator(i) {
    let s = "";
    for (var j=0; j<i; j++)
        s += "foo";
    return s;
}

export class Bar {
    constructor(i) {
        this.i = i;
    }
    generator() {
        let s = "";
        for (var j=0; j<this.i; j++)
            s += "bar";
        return s;
    }
}