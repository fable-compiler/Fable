const babel = require("@babel/core");
const fs = require("fs");
const path = require("path");

function getFiles(dir) {
    const subdirs = fs.readdirSync(dir);
    const files = subdirs.map((subdir) => {
        const res = path.resolve(dir, subdir);
        return fs.statSync(res).isDirectory() ? getFiles(res) : res;
    });
    return files.reduce((acc, file) => acc.concat(file), []);
}

const files = getFiles("./test_out");

for (const fileIn of files) {
    if (fileIn.endsWith(".json")) {
        const babelJson = fs.readFileSync(fileIn, "utf8");
        const babelAst = JSON.parse(babelJson);
        const res = babel.transformFromAst(babelAst[0]);
        const fileOut = fileIn.replace(".json", ".js")
        fs.writeFileSync(fileOut, res.code);
    }
}