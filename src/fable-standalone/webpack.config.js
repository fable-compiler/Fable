const resolve = (path) => require("path").join(__dirname, path);

module.exports = {
    entry: resolve('src/Fable.Standalone.fsproj'),
    output: {
        filename: 'bundle.min.js',
        path: resolve('dist'),
        library: '__FABLE_STANDALONE__',
        libraryTarget: 'umd',
        // https://github.com/webpack/webpack/issues/6525
        globalObject: `typeof self !== 'undefined' ? self : this`,
    },
    mode: "production",
    module: {
        rules: [
          {
            test: /\.fs(x|proj)?$/,
            use: {
                loader: "fable-loader",
                options: {
                    cli: { path: resolve("../Fable.Cli") },
                    define: [
                        "FX_NO_CORHOST_SIGNER",
                        "FX_NO_LINKEDRESOURCES",
                        "FX_NO_PDB_READER",
                        "FX_NO_PDB_WRITER",
                        "FX_NO_WEAKTABLE",
                        "FX_REDUCED_EXCEPTIONS",
                        "NO_COMPILER_BACKEND",
                        "NO_EXTENSIONTYPING",
                        "NO_INLINE_IL_PARSER"
                    ],
                }
            }
          }
        ]
    }
};
