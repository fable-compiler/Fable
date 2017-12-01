const path = require('path');

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

export default {
  input: resolve('./out/Main.js'),
  output: {
    file: resolve('./repl/build/bundle.js'),
    format: 'iife', // 'amd', 'cjs', 'es', 'iife', 'umd'
  },
  name: 'Fable',
  //sourceMap: 'inline',
};
