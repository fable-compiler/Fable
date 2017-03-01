var child_process  = require('child_process');
var client = require("./client.js");

var port = 61225;

module.exports = function(buffer) {
    this.cacheable();
    var callback = this.async();
    var msg = {
        path: this.resourcePath,
        options: {
            symbols: [],
            plugins: [],
            clamp: false,
            declaration: false,
            typedArrays: true
        }
    };
    client.send(port, JSON.stringify(msg))
        .then(data => {
            this.emitWarning(data);
            console.log("JASJA", data)
            // TODO: Mark dependencies
            callback(null, "const foo = " + JSON.stringify(data))
        })
        .catch(err => {
            // this.emitError(err.message)
            callback(err)
        })
};
module.exports.raw = true;
