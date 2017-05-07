#!/usr/bin/env node

const net = require('net');
const childProcess = require('child_process');

if (require.main === module) {
  const [, , port, msg] = process.argv;

  let buff = '';

  const c = new net.Socket();
  c.connect({ port }, () => c.write(msg));

  c.on('data', x => (buff += x));
  // eslint-disable-next-line no-console
  c.on('close', () => console.log(buff));
}

module.exports = args => {
  const resp = childProcess.spawnSync('node', [
    `${__dirname}/client.js`,
    args.port,
    JSON.stringify(args)
  ]);

  resp.stderr = resp.stderr.toString();
  resp.stdout = resp.stdout.toString();

  if (resp.status !== 0) throw new Error(resp.stderr);

  return resp;
};
