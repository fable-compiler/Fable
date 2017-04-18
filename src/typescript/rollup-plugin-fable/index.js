const path = require('path');
const babel = require('babel-core');
const client = require('fable-utils/client');
const babelPlugins = require('fable-utils/babel-plugins');
const { createFilter } = require('rollup-pluginutils');

let fableCoreVersion = null;
const customPlugins = [
  babelPlugins.getRemoveUnneededNulls(),
  babelPlugins.getTransformMacroExpressions(babel.template)
];

module.exports = (
  {
    include,
    exclude,
    babel: babelOpts = {
      plugins: []
    },
    define = [],
    plugins = [],
    fableCore = null,
    declaration = false,
    typedArrays = true,
    clampByteArrays = false,
    port = 61225,
    extra
  } = {}
) => {
  const filter = createFilter(include, exclude);

  return {
    name: 'fable',
    transform(code, id) {
      if (!filter(id)) return;

      if (!/\.fs$/.test(id) && !/.fsx$/.test(id) && !/.fsproj$/.test(id))
        return;

      babelOpts.plugins = customPlugins.concat(babelOpts.plugins || []);

      const msg = {
        path: id,
        define,
        plugins,
        fableCore,
        declaration,
        typedArrays,
        clampByteArrays,
        extra
      };

      if (fableCore == null) {
        if (fableCoreVersion == null)
          fableCoreVersion = require('fable-core/package.json').version;

        msg.fableCore = path.join(__dirname, '../fable-core');
        msg.fableCoreVersion = fableCoreVersion;
      } else {
        msg.fableCoreVersion = '*';
      }

      console.log(`Fable Plugin sent: ${msg.path}`);

      return client
        .send(port, JSON.stringify(msg))
        .then(r => {
          console.log(`Fable Plugin received: ${msg.path}`);

          const data = JSON.parse(r);

          const {
            error = null,
            logs = {},
          } = data;

          if (error) throw new Error(error);

          Object.keys(logs).forEach(key => {
            // TODO: Fail if there's one or more error logs?
            // That would prevent compilation of other files
            if (key === "warning" || key === "error") {
              ensureArray(logs[key]).forEach(x => this.warn(x));
            }
            else {
              ensureArray(logs[key]).forEach(x => console.log(x));
            }
          })

          let fsCode = null;
          if (this.sourceMap) {
            fsCode = code;
            babelOpts.sourceMaps = true;
            babelOpts.sourceFileName = path.relative(
              process.cwd(),
              data.fileName.replace(/\\/g, '/')
            );
          }
          const transformed = babel.transformFromAst(data, fsCode, babelOpts);

          return { code: transformed.code, map: transformed.map };
        })
        .catch(err => {
          const msg = err.message +
            '\nMake sure Fable server is running on port ' +
            port;
          throw new Error(msg);
        });
    }
  };
};
