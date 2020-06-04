import * as net from 'net';

export function send(host: string, port: number, msg: any) {
  return new Promise<any>((resolve, reject) => {
    const client = new net.Socket();
    let resolved = false;
    let buffer = "";

    client.connect(port, host, () => {
      client.write(JSON.stringify(msg));
    });

    client.on('error', (err) => {
      if (!resolved) {
        resolved = true;
        reject(err);
      }
    });

    client.on('data', (data) => {
      buffer += data.toString();
    });

    client.on('close', () => {
      if (!resolved) {
        resolved = true;
        resolve(JSON.parse(buffer));
      }
    });
  });
}