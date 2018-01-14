const fableSplitter = require("fable-splitter").default;
const fableUtils = require("fable-utils");

const babelOptions = fableUtils.resolveBabelOptions({
  plugins: [
    ["transform-es2015-modules-commonjs"],
  ],
});

const fableOptions = {
  fableCore: "../../../../build/fable-core",
};

const options = {
  entry: "./testapp.fsproj",
  outDir: "./out",
  babel: babelOptions,
  fable: fableOptions,
};

fableSplitter(options);
