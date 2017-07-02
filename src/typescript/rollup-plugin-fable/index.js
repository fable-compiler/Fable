const path = require('path');
const babel = require('babel-core');
const client = require('fable-utils/client');
const babelPlugins = require('fable-utils/babel-plugins');
const { createFilter } = require('rollup-pluginutils');

const DEFAULT_PORT =
    process.env.FABLE_SERVER_PORT != null
    ? parseInt(process.env.FABLE_SERVER_PORT, 10)
    : 61225;

const customPlugins = [
  babelPlugins.getRemoveUnneededNulls(),
  babelPlugins.getTransformMacroExpressions(babel.template)
];

const ensureArray = obj =>
  (Array.isArray(obj) ? obj : obj != null ? [obj] : []);

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
    port = DEFAULT_PORT,
    extra
  } = {}
) => {
  const filter = createFilter(include, exclude);

  return {
    name: 'fable',
    transform(code, id) {
      if (!filter(id)) return;

      if (!/\.(?:fs|fsx|fsproj)$/.test(id)) return;

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

      // console.log(`Fable Plugin sent: ${msg.path}`);

      return client
        .send(port, JSON.stringify(msg))
        .then(r => {
          console.log(`fable: Compiled ${path.relative(process.cwd(), msg.path)}`);

          const data = JSON.parse(r);

          const { error = null, logs = {} } = data;

          if (error) throw new Error(error);

          Object.keys(logs).forEach(key => {
            let print = x => console.log(x);
            if (key === "warning") {
              print = x => this.warn(x);
            }
            else if (key === "error") {
              // `this.error` stops compilation (also watching), so we force
              // the user explictly pass the `extra.failOnError option
              print = extra && extra.failOnError ? x => this.error(x) : x => this.warn(x);
            }
            ensureArray(logs[key]).forEach(print);
          });

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
          const msg = `${err.message} \n Make sure Fable server is running on port ${port}`;
          throw new Error(msg);
        });
    }
  };
};
