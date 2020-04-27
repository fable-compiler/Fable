const resolve = (path) => require("path").join(__dirname, path);

module.exports = {
  cli: { path: resolve("../Fable.Cli") },
  entry: resolve("./src/fable-compiler-js.fsproj"),
  outDir: resolve("../../build/fable-compiler-js/out"),
};
