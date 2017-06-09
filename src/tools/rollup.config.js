var fable = require('rollup-plugin-fable')
var path = require('path')

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

// var babelOptions = {
//   "presets": [
//     [resolve("../../../../node_modules/babel-preset-es2015"), {"modules": false}],
//     //[resolve("../../../../node_modules/babel-preset-babili"), {}]
//   ]
// };

var fableOptions = {
  //babel: babelOptions,
  fableCore: resolve("../../build/fable-core"),
  //plugins: [],
  define: ["DEBUG"],
  extra: { saveAst: resolve("temp") }
};

export default {
  entry: resolve('./QuickTest.fsx'),
  dest: resolve('./temp/QuickTest.js'),
  format: 'cjs', // 'amd', 'cjs', 'es', 'iife', 'umd'
  //sourceMap: 'inline',
  plugins: [
    fable(fableOptions),
  ],
};