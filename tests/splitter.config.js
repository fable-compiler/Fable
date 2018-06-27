module.exports = {
  entry: "Main/Fable.Tests.fsproj",
  outDir: "../build/tests",
  fable: { define: defineConstants() },
  babel: {
    plugins: ["transform-es2015-modules-commonjs"],
    // presets: [ ["env", {"modules": false}] ]
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
