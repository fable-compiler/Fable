var net = require('net');

exports.send = function(host, port, msg) {
  return new Promise((resolve, reject) => {
    var buffer = "";
    var client = new net.Socket(), resolved = false;

    client.connect(port, host, function() {
      client.write(msg);
    });

    client.on('error', function(err) {
      if (!resolved) {
        resolved = true;
        reject(err);
      }
    });

    client.on('data', function(data) {
      buffer += data.toString();
    });

    client.on('close', function() {
      if (!resolved) {
        resolved = true;
        resolve(buffer);
      }
    });
  });
}