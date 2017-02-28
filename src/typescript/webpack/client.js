var net = require('net');
var server = require('./server');

var HOST = '127.0.0.1';

function sleep(ms) {
  return new Promise(function (resolve) {
    setTimeout(() => resolve(), ms);
  })
}

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

function getFreePort(port) {
  var port = port || getRandomInt(1000, 10000);
  console.log("Checking port " + port + "...");
  return isPortFree(port).then(res =>
    res ? port : getFreePort()
  );
}

function send(port, msg) {
  return new Promise(resolve => {
    var client = new net.Socket();
    client.connect(port, HOST, function() {
      console.log('Client connected');
      client.write(msg);
    });

    client.on('data', function(data) {
      console.log('Client Received: ' + data);
      client.destroy();
    });

    client.on('close', function() {
      console.log('Client connection closed');
      resolve();
    });
  });
}

function init() {
  var port = 1300;
  Promise.resolve()
  // getFreePort()
    // .then(p => {
    //   port = p;
    //   server.startServer(port);
    //   return sleep(1000);
    // })
    .then(() => send(port, 'Hello, server! Love, Client.'))
    .then(() => sleep(500))
    .then(() => send(port, 'Are you there?'))
    .then(() => sleep(500))
    .then(() => send(port, 'Can you hear me?'))
    .then(() => sleep(500))
    .then(() => send(port, 'Finito'))
    .then(() => sleep(500))
    .then(() => server.endServer())
}

init();
