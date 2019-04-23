import * as net from 'net';

export function send(host: string, port: number, msg: any) {
  return new Promise<any>((resolve, reject) => {
    const client = new net.Socket();
    let resolved = false;
    let buffer = "";

    client.connect(port, host, function() {
      client.write(JSON.stringify(msg));
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
        resolve(JSON.parse(buffer));
      }
    });
  });
}