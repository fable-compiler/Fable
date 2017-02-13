importScripts('repl/bundle.min.js');

var checker = null;
var metadata = {}

var references = [
    "FSharp.Core.dll",
    "FSharp.Core.sigdata",
    "mscorlib.dll",
    "System.dll",
    "System.Core.dll",
    "System.Data.dll",
    "System.IO.dll",
    "System.Xml.dll",
    "System.Numerics.dll"
];

var getFileBlob = function (key, url) {
    var xhr = new XMLHttpRequest();
    xhr.open("GET", url);
    xhr.responseType = "arraybuffer";
    xhr.onload = function (oEvent) {
      var arrayBuffer = xhr.response;
      if (arrayBuffer) {
        metadata[key] = new Uint8Array(arrayBuffer);
      }
    };
    xhr.onerror = function (oEvent) {
      console.log('Error loading ' + url);
    };
    xhr.send();
};

references.map(function(fileName){
    getFileBlob(fileName, '/repl/metadata/' + fileName);
});

function compile(source) {
    try {
        if (checker === null) {
            if (Object.getOwnPropertyNames(metadata).length < references.length) {
                setTimeout(() => compile(source), 200);
                return;
            }
            var readAllBytes = function (fileName) { return metadata[fileName]; }
            var references2 = references.filter(x => x.endsWith(".dll")).map(x => x.replace(".dll", ""));
            checker = project.createChecker(readAllBytes, references2);
        }
        var json = project.compileSource(checker, source);
        postMessage(json);
    }
    catch (err) {
        postMessage(JSON.stringify({ error: { message: err.message, stack: err.stack }}));
    }
}

onmessage = function (e) {
    compile(e.data);
}