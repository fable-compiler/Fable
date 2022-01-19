// Use ES6 export syntax to check transpilation
export function fooGenerator(i) {
    let s = "";
    for (var j=0; j<i; j++)
        s += "foo";
    return s;
}

export class Bar {
    constructor(count, text) {
        this.count = count;
        this.text = text;
    }
    generator() {
        let s = "";
        for (var j=0; j<this.count; j++)
            s += this.text;
        return s;
    }
}

export const getArgCount = {
    foo() {
        return arguments.length;
    }
};

export default "bar";