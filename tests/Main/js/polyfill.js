global.atob = function(b64Encoded) {
    return Buffer.from(b64Encoded, "base64").toString();
};

global.btoa = function(str) {
    return Buffer.from(str).toString("base64")
};