/// @ts-check

var os = require("os")
var fs = require("fs");
var path = require("path");
var crypto = require('crypto');

// In theory, msgpack should be more performant than JSON serialization,
// but I haven't noted that in my tests and it also caused some problems.
// For now, msgpack code is kept in tryLoadCache and trySaveCache
// var msgpack = require("msgpack-lite");

// From https://stackoverflow.com/a/32197381/3922220
var deleteFolderRecursive = function(path) {
  if( fs.existsSync(path) ) {
    fs.readdirSync(path).forEach(function(file,index){
      var curPath = path + "/" + file;
      if(fs.lstatSync(curPath).isDirectory()) { // recurse
        deleteFolderRecursive(curPath);
      } else { // delete file
        fs.unlinkSync(curPath);
      }
    });
    fs.rmdirSync(path);
  }
};

function getCachePath(fileName) {
    var fableTempDir = path.join(os.tmpdir(), "fable");
    if (!fs.existsSync(fableTempDir)){
        fs.mkdirSync(fableTempDir);
    }
    var hash = crypto.createHash('md5').update(fileName).digest('hex');
    return path.join(fableTempDir, hash);
}

exports.clearCache = function () {
    var fableTempDir = path.join(os.tmpdir(), "fable");
    deleteFolderRecursive(fableTempDir);
}

function CachedFile(fileContents) {
    var json = JSON.parse(fileContents);
    this.code = json.code;
    this.map = json.map;
}
exports.CachedFile = CachedFile;

exports.tryLoadCache = function (extraOpts, fileName) {
    if (!extraOpts || !extraOpts.useCache || path.extname(fileName) !== ".fs") {
        return Promise.resolve(null);
    }

    return new Promise(resolve => {
        try {
            var cachePath = getCachePath(fileName);
            if (fs.existsSync(cachePath)) {
                var sourcemtime = fs.statSync(fileName).mtime;
                var cachemtime = fs.statSync(cachePath).mtime;
                if (sourcemtime < cachemtime) {
                    var fileContents = fs.readFileSync(cachePath, "utf8").toString();
                    resolve(new CachedFile(fileContents));
                    // var readStream = fs.createReadStream(cachePath);
                    // var decodeStream = msgpack.createDecodeStream();
                    // readStream.pipe(decodeStream).on("data", data => { resolve(data) });
                    return;
                }
            }
        }
        catch (err) {
            var relPath = path.relative(process.cwd(), fileName);
            console.log("fable: Couldn't load " + relPath + " from cache. " + String(err));
            // Do nothing, just fall through
        }
        resolve(null);
    });
}

// This may need some kind of lock when building projects in parallel which share files
// and both try to cache the same file at the same time
exports.trySaveCache = function (extraOpts, fileName, data) {
    try {
        if (extraOpts && extraOpts.useCache && extraOpts.useCache !== "readonly") {
            fs.writeFileSync(getCachePath(fileName), JSON.stringify(data), {encoding: "utf8"});
            // var writeStream = fs.createWriteStream(getCachePath(fileName));
            // var encodeStream = msgpack.createEncodeStream();
            // encodeStream.pipe(data);
            // encodeStream.write(babelParsed);
            // encodeStream.end();
        }
    }
    catch (err) {
        var relPath = path.relative(process.cwd(), fileName);
        console.log("fable: Couldn't save " + relPath + " to cache. " + String(err));
    }
}
