importScripts('repl/bundle.min.js');

var checker = null;
var metadata = {}

// Files have .txt extension to allow gzipping in Github Pages
var references = [
    "mscorlib.dll",
    "System.dll",
    "System.Core.dll",
    "System.Data.dll",
    "System.IO.dll",
    "System.Xml.dll",
    "System.Numerics.dll",
    "FSharp.Core.sigdata",
    "FSharp.Core.dll",
    "Fable.Core.dll"
];

function isSigdata(ref) {
    return ref.indexOf(".sigdata") >= 0;
}

function getFileBlob(key, url) {
    var xhr = new XMLHttpRequest();
    xhr.open("GET", url + ".txt");
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
            var references2 = references.filter(x => !isSigdata(x)).map(x => x.replace(".dll", ""));
            checker = Fable.createChecker(readAllBytes, references2);
        }
        var startTime = performance.now();
        var json = Fable.compileToJson(checker, source);
        var elapsed = performance.now() - startTime;
        postMessage({ json: json, elapsed: elapsed });
    }
    catch (err) {
        postMessage({ error: { message: err.message, stack: err.stack }});
    }
}

onmessage = function (e) {
    compile(e.data);
}