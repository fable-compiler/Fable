const http = require('http')
const url = require('url')
const fs = require('fs')
const path = require('path')
const zlib = require('zlib');

const baseDirectory = __dirname
const port = 8080

http.createServer(function (request, response) {
  try {
    console.log(`${request.method} ${request.url}`);
    let requestUrl = url.parse(request.url);
    let fsPath = baseDirectory + path.normalize(requestUrl.pathname);
    var fileStream = fs.createReadStream(fsPath)
    var acceptEncoding = request.headers['accept-encoding'] || '';
    if (acceptEncoding.match(/\bdeflate\b/)) {
      response.writeHead(200, { 'Content-Encoding': 'deflate' });
      fileStream.pipe(zlib.createDeflate()).pipe(response);
    } else if (acceptEncoding.match(/\bgzip\b/)) {
      response.writeHead(200, { 'Content-Encoding': 'gzip' });
      fileStream.pipe(zlib.createGzip()).pipe(response);
    } else {
      response.writeHead(200, {});
      fileStream.pipe(response);
    }
    fileStream.on('error',function(e) {
        response.writeHead(404)     // assume the file doesn't exist
        response.end()
    })
  } catch(e) {
    response.writeHead(500)
    response.end()     // end the response so browsers don't hang
    console.log(e.stack)
  }
}).listen(port)

console.log("listening on port "+port)