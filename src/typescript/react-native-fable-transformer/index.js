// react-native start --transformer ./smTransform.js

const transformer = require('react-native/packager/transformer');
const client = require('fable-utils/client-sync');
const babelPlugins = require('fable-utils/babel-plugins');
const babel = require('babel-core');

const isFableFile = /\.(?:fs|fsx|fsproj)$/;
const fableCoreVersion = null;
const DEFAULT_PORT = process.env.FABLE_SERVER_PORT != null
  ? parseInt(process.env.FABLE_SERVER_PORT, 10)
  : 61225;

const customPlugins = [
  babelPlugins.getRemoveUnneededNulls(),
  babelPlugins.getTransformMacroExpressions(babel.template)
];

module.exports = (src, filename, options) => {
  options = options || {};
  if (isFableFile.test(filename)) {
    if (babelOpts.sourceMaps) {
      fsCode = src;
      babelOpts.sourceMaps = true;
      babelOpts.sourceFileName = relative(
        process.cwd(),
        fileName.replace(/\\/g, '/')
      );
    }

    return babel.transformFromAst(data, fsCode, babelOpts).code;
  } else {return transformer.transform(src, filename, options);}
};
