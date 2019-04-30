const path = require("path");

const testLocal = process.argv.find(v => v === "--test-local");
if (testLocal) {
  console.log("Using local fable-standalone and fable-metadata...")
}

module.exports = {
  entry: path.join(__dirname, "./src/fable-compiler-js.fsproj"),
  outDir: path.join(__dirname, "./dist"),
  babel: {
    plugins: ["@babel/plugin-transform-modules-commonjs"],
  },
  fable: {
    define: testLocal ? ["TEST_LOCAL"] : []
  }
};
