var fable = require('rollup-plugin-fable')

export default {
  input: 'QuickTest.fsproj',
  output: {
    file: 'temp/QuickTest.js',
    format: 'cjs',
  },
  //sourceMap: 'inline',
  plugins: [
    fable({
      define: ["DEBUG"],
      // babel: { presets: [["env", { modules: "false" }]], },
      // extra: { saveAst: resolve("temp") }
    }),
  ],
};