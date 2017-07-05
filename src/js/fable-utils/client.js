var net = require('net');

var HOST = '127.0.0.1';

function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min)) + min;
}

// From http://stackoverflow.com/a/29872303
var isPortFree = function(port) {
  return new Promise(function (resolve) {
    var server = net.createServer(function(socket) {
      socket.write('Echo server\r\n');
      socket.pipe(socket);
    });

    server.listen(port, HOST);
    server.on('error', function (e) {
      resolve(false);
    });

    server.on('listening', function (e) {
      server.close();
      resolve(true);
    });
  })
};

exports.getFreePort = function(port) {
  var port = port || getRandomInt(1000, 10000);
  return isPortFree(port).then(res =>
    res ? port : getFreePort()
  );
}

exports.send = function(port, msg) {
  return new Promise((resolve, reject) => {
    var buffer = "";
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
}
