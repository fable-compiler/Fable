importScripts('repl/bundle.min.js');

var checker = null;
var metadata = {}

// Files have .txt extension to allow gzipping in Github Pages
var references = [
    "mscorlib.txt",
    "System.txt",
    "System.Core.txt",
    "System.Data.txt",
    "System.IO.txt",
    "System.Xml.txt",
    "System.Numerics.txt",
    "FSharp.Core.sigdata.txt",
    "FSharp.Core.txt",
    "Fable.Core.txt"
];

function isSigdata(ref) {
    return ref.indexOf(".sigdata") >= 0;
}

function getFileBlob(key, url) {
    key = key.replace(".txt", isSigdata(key) ? "" : ".dll")
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
            var references2 = references.filter(x => !isSigdata(x)).map(x => x.replace(".txt", ""));
            checker = Fable.createChecker(readAllBytes, references2);
        }
        var json = Fable.compileSource(checker, source);
        postMessage(json);
    }
    catch (err) {
        postMessage(JSON.stringify({ error: { message: err.message, stack: err.stack }}));
    }
}

onmessage = function (e) {
    compile(e.data);
}