const path = require("path");

module.exports = {
  cli: { path: resolve("../src/Fable.Cli") },
  entry: resolve("Main/Fable.Tests.fsproj"),
  outDir: resolve("../build/tests"),
  fable: { define: defineConstants() },
  babel: {
    plugins: ["@babel/plugin-transform-modules-commonjs"],
    // presets: [ ["@babel/preset-env", {"modules": false}] ]
  },
  // allFiles: true
};

function defineConstants() {
  var ar = ["DEBUG"];
  if (process.env.APPVEYOR) {
    console.log("Running on APPVEYOR...");
    ar.push("APPVEYOR");
  } else if (process.env.TRAVIS) {
    console.log("Running on TRAVIS...");
    ar.push("TRAVIS");
  }
  if (process.argv.find(v => v === "-d:OPTIMIZE_FCS")) {
    ar.push("OPTIMIZE_FCS");
  }
  ar.push("FABLE_QUOTATIONS");
  
  return ar;
}

function resolve(p) {
  return path.join(__dirname, p);
}