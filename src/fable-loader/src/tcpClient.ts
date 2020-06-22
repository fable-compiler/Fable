import * as net from 'net';

export default function send(host: string, port: number, msg: any): Promise<string> {
  return new Promise((resolve, reject) => {
    let buffer = "";
    let resolved = false;
    const client = new net.Socket();

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