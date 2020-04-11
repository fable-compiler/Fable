const resolve = (path) => require("path").join(__dirname, path);

const testLocal = process.argv.find(v => v === "--test-local");
if (testLocal) {
  console.log("Using local fable-standalone and fable-metadata...")
}

module.exports = {
  cli: { path: resolve("../Fable.Cli") },
  entry: resolve("./src/fable-compiler-js.fsproj"),
  outDir: resolve("./dist"),
  babel: {
    plugins: ["@babel/plugin-transform-modules-commonjs"],
  },
  fable: {
    define: testLocal ? ["TEST_LOCAL"] : []
  }
};
