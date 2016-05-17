var fableCore = require("../../import/core/fable-core.js");

var skipped = ["__esModule","Symbol","Util"];
var modTemplate = '        "[MODULE]", set [ [METHODS] ]\n';
var output =
`// AUTOMATICALLY GENERATED FILE - DO NOT EDIT
module Fable.CoreLibMethods

let staticMethods =
    [
`;

Object.getOwnPropertyNames(fableCore)
    .filter(mod => skipped.indexOf(mod) == -1)
    .forEach(mod => {
        output += modTemplate
            .replace("[MODULE]", mod)
            .replace("[METHODS]", Object.getOwnPropertyNames(fableCore[mod])
                .filter(x => x.indexOf("__") !== 0)
                .map(x => `"${x}"`).join("; "));
    })

var fs = require("fs");
fs.writeFileSync(
    __dirname + "/../fable-fsharp/Replacements/CoreLibMethods.fs",
    output + `    ] |> Map`);
