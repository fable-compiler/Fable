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

      console.log(`Fable Plugin sent: ${msg.path}`);

      return client
        .send(port, JSON.stringify(msg))
        .then(r => {
          console.log(`Fable Plugin received: ${msg.path}`);

          const data = JSON.parse(r);

          const { error = null, logs = {} } = data;

          if (error) throw new Error(error);

          Object.keys(logs).forEach(key => {
            // TODO: Fail if there's one or more error logs?
            // That would prevent compilation of other files
            if (key === 'warning' || key === 'error')
              // Don't use `this.error` as it will stop the bundle
              ensureArray(logs[key]).forEach(x => this.warn(x));
            else ensureArray(logs[key]).forEach(x => console.log(x));
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
