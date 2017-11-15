importScripts('build/bundle.min.js');

// performance polyfill for Safari (see #902) from
// https://gist.github.com/paulirish/5438650
(function(){
  if ("performance" in self == false) {
      self.performance = {};
  }
  Date.now = (Date.now || function () {  // thanks IE8
	  return new Date().getTime();
  });
  if ("now" in self.performance == false){
    var nowOffset = Date.now();
    if (performance.timing && performance.timing.navigationStart){
      nowOffset = performance.timing.navigationStart
    }
    self.performance.now = function now(){
      return Date.now() - nowOffset;
    }
  }
})();

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
    "FSharp.Core.dll",
    "Fable.Core.dll",
    "Fable.Import.Browser.dll"
    // When loading the REPL the browser console always shows: "Cannot find type System.ValueTuple`1"
    // However, adding the following reference prevents opening System namespace
    // See https://github.com/fable-compiler/Fable/issues/1152#issuecomment-330315250
    // "System.ValueTuple.dll",
];

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
    getFileBlob(fileName, 'metadata/' + fileName);
});

function compile(source, replacements) {
    try {
        if (checker === null) {
            if (Object.getOwnPropertyNames(metadata).length < references.length) {
                setTimeout(() => compile(source, replacements), 200);
                return;
            }
            var readAllBytes = function (fileName) { return metadata[fileName]; }
            var references2 = references.map(x => x.replace(".dll", ""));
            checker = Fable.createChecker(readAllBytes, references2);
        }
        var com = Fable.makeCompiler(replacements);

        // FSharp AST
        var startTime1 = performance.now();
        var fileName = "stdin.fsx"
        let fsharpAst = Fable.parseFSharpProject(checker, com, fileName, source);
        var elapsed1 = performance.now() - startTime1;

        // Fable AST
        var startTime2 = performance.now();
        var babelAst = Fable.compileAst(com, fsharpAst, fileName);
        var jsonAst = Fable.convertToJson(babelAst);
        var elapsed2 = performance.now() - startTime2;

        postMessage({ jsonAst: jsonAst, fsharpTime: elapsed1, fableTime: elapsed2 });
    }
    catch (err) {
        postMessage({ error: { message: err.message, stack: err.stack }});
    }
}

onmessage = function (e) {
    compile(e.data.source, e.data.replacements);
}