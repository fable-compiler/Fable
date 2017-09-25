global.atob = function(b64Encoded) {
    return new Buffer(b64Encoded, "base64").toString();
};

global.btoa = function(str) {
    return new Buffer(str).toString("base64")
};