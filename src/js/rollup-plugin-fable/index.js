/// @ts-check

const path = require('path');
const babel = require('babel-core');
const fableUtils = require ("fable-utils");
const { createFilter } = require('rollup-pluginutils');

const DEFAULT_PORT =
    process.env.FABLE_SERVER_PORT != null
    ? parseInt(process.env.FABLE_SERVER_PORT, 10)
    : 61225;

const customPlugins = [
  fableUtils.babelPlugins.getRemoveUnneededNulls(),
  fableUtils.babelPlugins.getTransformMacroExpressions(babel.template)
];

const ensureArray = obj =>
  (Array.isArray(obj) ? obj : obj != null ? [obj] : []);

module.exports = (opts) => {
  fableUtils.validateFableOptions(opts);
  let {
    include = [],
    exclude = [],
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
    extra = {}
  } = opts;

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

      return fableUtils.client.send(port, JSON.stringify(msg))
        .then(r => {
          const data = JSON.parse(r);

          const { error = null, logs = {} } = data;

          if (error) throw new Error(error);

          Object.keys(logs).forEach(key => {
            ensureArray(logs[key]).forEach(x => {
              if (key === "warning") {
                this.warn(x);
              }
              else if (key === "error") {
                this.error(x);
              }
              else {
                console.log(x);
              }
            });
          });

          const babelOptsLocal = Object.assign({}, babelOpts, {
            sourceMaps: true,
            sourceFileName: path.relative(
              process.cwd(),
              data.fileName.replace(/\\/g, '/')
            )
          });
          const transformed = babel.transformFromAst(data, code, babelOptsLocal);

          console.log("fable: Compiled " + path.relative(process.cwd(), msg.path));
          return { code: transformed.code, map: transformed.map };
        })
        .catch(err => {
          const msg = `${err.message} \n Make sure Fable server is running on port ${port}`;
          throw new Error(msg);
        });
    }
  };
};
