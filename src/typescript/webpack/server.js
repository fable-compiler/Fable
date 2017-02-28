var net = require("net");
var HOST = '127.0.0.1';

var server = null;

exports.startServer = function (port) {
    server = net.createServer(function(sock) {
        console.log('Socket connected: ' + sock.remoteAddress +':'+ sock.remotePort);

        sock.on('data', function(data) {
            console.log('Socket received ' + sock.remoteAddress + ': ' + data);
            sock.write('You said "' + data + '"');
        });

        sock.on('close', function(data) {
            console.log('Socket closed: ' + sock.remoteAddress +' '+ sock.remotePort);
        });

    }).listen(port, HOST);

    server.on("close", function () {
        console.log("Server closed")
    })

    console.log('Server listening on ' + HOST +':'+ port);
}

exports.endServer = function () {
    if (server != null) {
        server.close();
    }
}
