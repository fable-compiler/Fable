const path = require("path");

module.exports = {
  entry: path.join(__dirname, "Main/Fable.Tests.fsproj"),
  outDir: path.join(__dirname, "../build/tests"),
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
  return ar;
}
