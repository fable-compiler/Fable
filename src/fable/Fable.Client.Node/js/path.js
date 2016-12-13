// This module is intended as a replacement for node's path
// to prevent crashes if Fable is run in a browser environment

/** Returns arguments joined by '/' */
exports.join = function() {
    var ar = []
    for (var i = 0; i < arguments.length; i++) {
        if (typeof arguments[i] === "string")
            ar.push(arguments[i]);
    }
    return ar.join("/");
}

/** TODO: Does nothing */
exports.resolve = function(path) {
    return path;
}

/** TODO: Does nothing */
exports.relative = function(path1, path2) {
    return path2;
}

/** Returns the substring until last index of '/' or '' */
exports.dirname = function(path) {
    if (typeof path === "string") {
        var i = path.lastIndexOf("/");
        return i > 0 ? path.substr(0, i - 1) : "";
    }
    return "";
}

/** Returns the substring from the last index of '/' + 1 or '' */
exports.basename = function(path) {
    if (typeof path === "string") {
        return path.substr(path.lastIndexOf("/") + 1);
    }
    return "";
}

/** Returns the substring from the last index of '.' + 1 or '' */
exports.extname = function(path) {
    if (typeof path === "string") {
        return path.substr(path.lastIndexOf(".") + 1);
    }
    return "";
}
