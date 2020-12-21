global.MyHTMLElement = class {
    constructor(value) {
        this.value = value;
    }
    getMotivated() {
        let xs = [];
        for (let i = 0; i < this.value; i++) {
            xs.push("I can do it!")
        }
        return xs.join(" ");
    }
    static get lunchTime() {
        return "13:00";
    }
}

global.atob = function(b64Encoded) {
    return Buffer.from(b64Encoded, "base64").toString();
};

global.btoa = function(str) {
    return Buffer.from(str).toString("base64")
};

if (!String.prototype.trimStart) {
    String.prototype.trimStart = String.prototype.trimLeft;
}

if (!String.prototype.trimEnd) {
    String.prototype.trimEnd = String.prototype.trimRight;
}