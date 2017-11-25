var fable = require('rollup-plugin-fable')
var fableUtils = require("fable-utils")
var path = require('path')

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
    presets: [["env", { modules: "false" }]],
    sourceMaps: true,
})

var fableOptions = {
  //babel: babelOptions,
  fableCore: resolve("../../build/fable-core"),
  //plugins: [],
  define: ["DEBUG"],
  extra: { saveAst: resolve("temp") }
};

export default {
  input: resolve('./QuickTest.fsproj'),
  output: {
    file: resolve('./temp/QuickTest.js'),
    format: 'cjs', // 'amd', 'cjs', 'es', 'iife', 'umd'
  },
  //sourceMap: 'inline',
  plugins: [
    fable(fableOptions),
  ],
};