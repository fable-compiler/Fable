// ATTENTION: PKG_VERSION and PKG_NAME are modified
// during the build process. DON'T CHANGE!

/** Fable compiler version */
exports.PKG_VERSION = "0.0.1";

/** "fable-compiler" | "fable-compiler-netcore" */
exports.PKG_NAME = "fable-compiler";

/** "fableconfig.json" */
exports.FABLE_CONFIG_FILE = "fableconfig.json";

/** [".fsproj", ".fsx"] */
exports.FSHARP_PROJECT_EXTENSIONS = [".fsproj", ".fsx"];

/** [".fsproj", ".fsx", ".fs"] */
exports.FSHARP_FILE_EXTENSIONS = [".fsproj", ".fsx", ".fs"];

/** Set of options compatible with Fable's .NET process */
exports.FABLE_BIN_OPTIONS = new Set([
    "projFile", "outDir", "refs", "symbols", "plugins", "module",
    "watch", "dll", "clamp", "extra", "declaration", "noTypedArrays"
]);

/** Accepted JS modules an the alias for Rollup */
exports.JS_MODULES = {
    "amd": "amd",
    "commonjs": "cjs",
    "systemjs": null,
    "umd": "umd",
    "es2105": "es",
    "es6": "es",
    "iife": "iife" // Accepted by Rollup
};
