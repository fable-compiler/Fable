// ATTENTION: PKG_VERSION and PKG_NAME are modified
// during the build process. DON'T CHANGE!

/** Fable compiler version */
exports.PKG_VERSION = "0.0.1";

/** "fable-compiler" | "fable-compiler-netcore" */
exports.PKG_NAME = "fable-compiler";

/** "fableconfig.json" */
exports.FABLE_CONFIG_FILE = "fableconfig.json";

/** Set of options compatible with Fable's .NET process */
exports.FABLE_BIN_OPTIONS = new Set([
    "projFile", "coreLib", "symbols", "plugins", "msbuild",
    "refs", "watch", "clamp", "copyExt", "extra", "declaration", "noTypedArrays"
]);

/**
 * Accepted JS modules an the alias for Rollup
 * (Rollup also accepts 'iife')
*/
exports.JS_MODULES = {
    "amd": "amd",
    "commonjs": "cjs",
    "systemjs": null,
    "umd": "umd"
};
