// This file is just a reference to locate the `lib` directory

// path.join(path.dirname(require.resolve("fable-metadata")), "lib")

exports.getAssembliesDir = function () {
    const path = require("path");
    return path.join(__dirname, "lib");
}