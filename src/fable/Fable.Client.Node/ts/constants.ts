// ATTENTION: PKG_VERSION and PKG_NAME are modified
// during the build process. DON'T CHANGE!

/** Fable compiler version */
export const PKG_VERSION = "0.0.1";

/** "fable-compiler" | "fable-compiler-netcore" */
export const PKG_NAME = "fable-compiler";

/** "fableconfig.json" */
export const FABLE_CONFIG_FILE = "fableconfig.json";

/** [".fsproj", ".fsx"] */
export const FSHARP_PROJECT_EXTENSIONS = [".fsproj", ".fsx"];

/** [".fsproj", ".fsx", ".fs"] */
export const FSHARP_FILE_EXTENSIONS = [".fsproj", ".fsx", ".fs"];

/** Set of options compatible with Fable's .NET process */
export const FABLE_BIN_OPTIONS = new Set([
    "projFile", "outDir", "refs", "symbols", "plugins", "module", "rollup",
    "watch", "dll", "clamp", "extra", "declaration", "noTypedArrays"
]);

/** Accepted JS modules an the alias for Rollup */
export const JS_MODULES = {
    amd: "amd",
    commonjs: "cjs",
    systemjs: null as string,
    umd: "umd",
    es2105: "es",
    es6: "es",
    iife: "iife" // Accepted by Rollup
};
