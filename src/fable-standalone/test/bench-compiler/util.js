const fs = require("fs");
const Path = require("path");

export function ensureDirExists(dir, cont) {
    if (fs.existsSync(dir)) {
        if (typeof cont === "function") { cont(); }
    } else {
        ensureDirExists(Path.dirname(dir), () => {
            if (!fs.existsSync(dir)) { fs.mkdirSync(dir); }
            if (typeof cont === "function") { cont(); }
        });
    }
}

export function serializeToJson(data) {
    return JSON.stringify(data, (key, value) => {
        if (value === Infinity) {
           return "Infinity";
        } else if (value === -Infinity) {
           return "-Infinity";
        } else if (value !== value) {
           return "NaN";
        }
        return value;
     });
}
