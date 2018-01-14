import commonjs from 'rollup-plugin-commonjs';
import fable from 'rollup-plugin-fable';
var path = require('path');

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

// var babelOptions = {
//   "presets": [
//     ["env", {"modules": false}]
//   ]
// }

var fableOptions = {
  //babel: babelOptions,
  fableCore: resolve("../../build/fable-core"),
  define: "DOTNETCORE",
  plugins: resolve("../../build/nunit/Fable.Plugins.NUnit.dll")
};

export default {
  input: resolve('./Main/Fable.Tests.fsproj'),
  output: {
    file: resolve('../../build/tests/bundle.js'),
    format: 'cjs', // 'amd', 'cjs', 'es', 'iife', 'umd'
  },
  //sourceMap: 'inline',
  plugins: [
    commonjs({
      namedExports: { './Main/js/foo.js': ['foo','foo2','apply', 'fooOptional', 'MyClass', 'foo_js'] }
    }),
    fable(fableOptions)
  ]
};
