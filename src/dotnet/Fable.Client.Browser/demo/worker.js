importScripts('out/bundle.js');

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
    getFileBlob(fileName, '/out/metadata/' + fileName);
});

onmessage = function (e) {
    if (checker === null) {
        var readAllBytes = function (fileName) { return metadata[fileName]; }
        var references2 = references.filter(x => x.endsWith(".dll")).map(x => x.replace(".dll", ""));
        checker = project.createChecker(readAllBytes, references2);
    }
    var json = project.compileSource(checker, e.data);
    postMessage(json);
}