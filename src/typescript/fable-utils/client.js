var net = require('net');

var HOST = '127.0.0.1';

exports.send = function(port, msg) {
  return new Promise((resolve, reject) => {
    var buffer = '';
    var client = new net.Socket(), resolved = false;

    client.connect(port, HOST, function() {
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
};
