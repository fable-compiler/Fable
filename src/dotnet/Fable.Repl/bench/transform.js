const babel = require("@babel/core");
const fs = require("fs");

const fileIn = "test_script.babel.ast.json";
const fileOut = "test_script.compiled.js";
const babelJson = fs.readFileSync(fileIn, "utf8");
const babelAst = JSON.parse(babelJson);
const res = babel.transformFromAst(babelAst[0]);
fs.writeFileSync(fileOut, res.code);
