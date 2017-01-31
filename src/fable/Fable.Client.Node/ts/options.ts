import * as fs from "fs";
import * as path from "path";
import * as commandLineArgs from "command-line-args";
import * as commandLineUsage from "command-line-usage/lib/command-line-usage";
import * as semver from "semver";
import * as json5 from "json5";

import * as fableLib from "./lib";
import * as constants from "./constants";
import * as customPlugins from "./babelPlugins";
import { FableOptions, BabelAst, Continuation, BuildResult } from "./types";
import { readRollupOptions } from "./bundle";

// Don't use default values as they would block options from fableconfig.json
const optionDefinitions = [
  { name: 'projFile', defaultOption: true, multiple: true, description: "The F# project (.fsproj) or script (.fsx) to compile." },
  { name: 'outDir', alias: 'o', description: "Where to put compiled JS files. Defaults to project directory." },
  { name: 'module', alias: 'm', description: "Specify module code generation: `commonjs`, `umd`, `amd` or `es2015` (default)." },
  { name: 'sourceMaps', alias: 's', description: "Generate source maps: `false` (default), `true` or `inline`." },
  { name: 'watch', alias: 'w', multiple: 'true', description: "Recompile project much faster on file modifications." },
  { name: 'ecma', description: "Specify ECMAScript target version: `es5` (default) or `es2015`." },
  { name: 'verbose', type: Boolean, description: "Print more information about the compilation process." },
  { name: 'symbols', multiple: true, description: "F# symbols for conditional compilation, like `DEBUG`." },
  { name: 'dll', type: Boolean, description: "Generate a `.dll` assembly when creating libraries." },
  { name: 'rollup', description: "Bundle files and dependencies with Rollup." },
  { name: 'includeJs', type: Boolean, description: "Compile with Babel and copy to `outDir` relative imports (starting with '.')." },
  { name: 'plugins', multiple: true, description: "Paths to Fable plugins." },
  { name: 'babelPlugins', multiple: true, description: "Additional Babel plugins (without `babel-plugin-` prefix). Must be installed in the project directory." },
  { name: 'refs', multiple: true, description: "Alternative location for compiled JS files of referenced libraries." },
  { name: 'coreLib', description: "Shortcut for `--refs Fable.Core={VALUE}`." },
  { name: 'loose', type: Boolean, description: "Enable loose transformations for babel-preset-es2015 plugins (true by default)." },
  { name: 'babelrc', type: Boolean, description: "Use a `.babelrc` file for Babel configuration (invalidates other Babel related options)." },
  { name: 'clamp', type: Boolean, description: "Compile unsigned byte arrays as Uint8ClampedArray." },
  { name: 'noTypedArrays', type: Boolean, description: "Don't compile numeric arrays as JS typed arrays." },
  { name: 'target', alias: 't', description: "Use options from a specific target in `fableconfig.json`." },
  { name: 'debug', alias: 'd', description: "Shortcut for `--target debug`." },
  { name: 'production', alias: 'p', description: "Shortcut for `--target production`." },
  { name: 'declaration', type: Boolean, description: "[EXPERIMENTAL] Generates corresponding ‘.d.ts’ file." },
  { name: 'extra', multiple: true, description: "Custom options for plugins in `{KEY}={VALUE}` format." },
  { name: 'help', alias: 'h', description: "Display usage guide." }
];

function getAppDescription() {
    return [{ header: 'Fable ' + constants.PKG_VERSION, content: 'F# to JavaScript compiler' },
            { header: 'Options', optionList: optionDefinitions },
            { content: 'All arguments can be defined in a fableconfig.json file' }];
}

function resolvePath(optName: string, value: any, workingDir: string) {
    function resolve(x: string) {
        return fableLib.pathJoin(workingDir, x)
    }
    // Discard null values or empty strings
    if (value) {
        switch (optName) {
            case "outDir":
                return resolve(value);
            // Multiple values
            case "projFile":
            case "plugins":
            case "babelPlugins":
                return value.map(resolve);
            // Only resolve refs if they starts with '.'
            case "refs":
                var o: any = {};
                for (var k in value) {
                    o[k] = value[k].startsWith('.') ? resolve(value[k]) : value[k];
                }
                return o;
        }
    }
    return value;
}

/** Reads options from command line, requires command-line-args */
function readCommandLineOptions() {
    function resolveKeyValuePairs(kvs: string[]) {
        var o: any = {};
        for (var i=0; i<kvs.length; i++) {
            var kv = kvs[i].split("=");
            o[kv[0]] = kv[1] || true;
        }
        return o;
    }
    var opts = commandLineArgs(optionDefinitions);
    if (opts.help) {
        fableLib.stdoutLog(commandLineUsage(getAppDescription()));
        fableLib.finish(0);
    }
    if (opts.refs) {
        opts.refs = resolveKeyValuePairs(opts.refs);
    }
    if (opts.coreLib) {
        opts.refs = Object.assign(opts.refs || {}, { "Fable.Core": opts.coreLib })
        delete opts.coreLib;
    }
    if (opts.extra) {
        opts.extra = resolveKeyValuePairs(opts.extra);
    }
    return opts;
}

/** Reads options from fableconfig.json, requires json5 */
function readFableConfigOptions(opts: any) {
    opts.workingDir = path.resolve(opts.workingDir || process.cwd());
    if (typeof opts.projFile === "string") {
        opts.projFile = [opts.projFile];
    }
    var cfgFile = fableLib.pathJoin(opts.workingDir, constants.FABLE_CONFIG_FILE);

    if (Array.isArray(opts.projFile) && opts.projFile.length === 1) {
        var fullProjFile = fableLib.pathJoin(opts.workingDir, opts.projFile[0]);
        var projDir = fs.statSync(fullProjFile).isDirectory()
                        ? fullProjFile
                        : path.dirname(fullProjFile);
        cfgFile = fableLib.pathJoin(projDir, constants.FABLE_CONFIG_FILE);

        // Delete projFile from opts if it isn't a true F# project
        if (!fableLib.isFSharpProject(fullProjFile)) {
            delete opts.projFile;
        }
    }

    if (fs.existsSync(cfgFile)) {
        // Change workingDir to where fableconfig.json is if necessary
        if (opts.workingDir !== path.dirname(cfgFile)) {
            for (var key in opts) {
                opts[key] = resolvePath(key, opts[key], opts.workingDir);
            }
            opts.workingDir = path.dirname(cfgFile);
        }

        var cfg = json5.parse(fs.readFileSync(cfgFile).toString());
        for (var key in cfg) {
            if (key in opts === false)
                opts[key] = cfg[key];
        }
        // Check if a target is requested
        if (opts.debug) { opts.target = "debug" }
        if (opts.production) { opts.target = "production" }
        if (opts.target) {
            if (!opts.targets || !opts.targets[opts.target]) {
                throw "Target " + opts.target + " is missing";
            }
            cfg = opts.targets[opts.target];
            for (key in cfg) {
                if ((typeof cfg[key] === "object") && !Array.isArray(cfg[key]) &&
                    (typeof opts[key] === "object") && !Array.isArray(opts[key])) {
                    for (var key2 in cfg[key])
                        opts[key][key2] = cfg[key][key2];
                }
                else {
                    opts[key] = cfg[key];
                }
            }
        }
    }
    return opts;
}

/** Reads Babel options: plugins and presets */
function readBabelOptions(opts: FableOptions) {
    var babelPresets: any[] = [],
        // Add plugins to emit .d.ts files if necessary
        babelPlugins: any[] = opts.declaration
        ? [[require.resolve("babel-dts-generator"),
            {
                "packageName": "",
                "typings": fableLib.pathJoin(opts.workingDir, opts.outDir),
                "suppressAmbientDeclaration": true,
                "ignoreEmptyInterfaces": false
            }],
            require.resolve("babel-plugin-transform-flow-strip-types"),
            require.resolve("babel-plugin-transform-class-properties")]
        : [];

    // Add custom plugins
    babelPlugins = babelPlugins.concat(
        customPlugins.transformMacroExpressions,
        // removeUnneededNulls must come after transformMacroExpressions (see #377)
        customPlugins.removeUnneededNulls,
        customPlugins.removeFunctionExpressionNames
    );

    // If opts.babelrc is true, let Babel read plugins and presets from .babelrc
    // Babel will automatically read configuration from .babelrc if we don't pass `babelrc: false`
    if (opts.babelrc) {
        opts.babel = { presets: babelPresets, plugins: babelPlugins };
        return opts;
    }

    // ECMAScript target
    if (opts.ecma != "es2015" && opts.ecma != "es6") {
        let module: boolean | string = opts.module;
        if (opts.module === "es2015" || opts.module === "es6") {
            module = false;
        }
        else if (opts.module in constants.JS_MODULES === false) {
            throw "Unknown module target: " + opts.module;
        }
        babelPresets.push([require.resolve("babel-preset-es2015"), {
            "loose": "loose" in opts ? opts.loose : true,
            "modules": opts.rollup ? false : module
        }]);
    }
    else if (!opts.rollup && opts.module in constants.JS_MODULES) {
        babelPlugins.push(require.resolve("babel-plugin-transform-es2015-modules-" + opts.module));
    }

    // Extra Babel plugins
    if (opts.babelPlugins) {
        babelPlugins = babelPlugins.concat(
            fableLib.resolvePlugins(opts.babelPlugins, opts.workingDir, "babel-plugin-"));
    }

    opts.babel = { presets: babelPresets, plugins: babelPlugins };
    return opts;
}

/** Prepares options: read from command line, fableconfig.json, etc */
export function readOptions(opts?: FableOptions) {
    opts = opts || readCommandLineOptions();
    opts = readFableConfigOptions(opts);

    opts.projFile = Array.isArray(opts.projFile) ? opts.projFile : [opts.projFile];
    if (!opts.projFile[0]) {
        throw "--projFile is empty";
    }
    for (var i = 0; i < opts.projFile.length; i++) {
        var fullProjFile = fableLib.pathJoin(opts.workingDir, opts.projFile[i] || '');
        if (!fableLib.isFSharpProject(fullProjFile)) {
            throw "Not an F# project (.fsproj) or script (.fsx): " + fullProjFile;
        }
        if (!fs.existsSync(fullProjFile)) {
            throw "Cannot find file: " + fullProjFile;
        }
    }

    // Default values & option processing
    opts.ecma = opts.ecma || "es5";
    opts.outDir = opts.outDir ? opts.outDir : (opts.projFile.length === 1 ? path.dirname(opts.projFile[0]) : ".");
    if (opts.module == null) {
        opts.module = opts.rollup ? "iife" : "es2015";
    }
    if (opts.coreLib) {
        opts.refs = Object.assign(opts.refs || {}, { "Fable.Core": opts.coreLib })
        delete opts.coreLib;
    }
    if (opts.refs) {
        for (var k in opts.refs) {
            var k2 = k.replace(/\.dll$/, "");
            if (k !== k2) {
                opts.refs[k2] = opts.refs[k];
                delete opts.refs[k];
            }
        }
    }

    // Check version
    const curNpmCfgPath = fableLib.pathJoin(opts.workingDir, "package.json");
    if (!(opts.extra && opts.extra.noVersionCheck) && fs.existsSync(curNpmCfgPath)) {
        const curNpmCfg: any = JSON.parse(fs.readFileSync(curNpmCfgPath).toString());
        if (curNpmCfg.engines && (curNpmCfg.engines.fable || curNpmCfg.engines["fable-compiler"])) {
            var fableRequiredVersion = curNpmCfg.engines.fable || curNpmCfg.engines["fable-compiler"];
            if (!semver.satisfies(constants.PKG_VERSION, fableRequiredVersion)) {
                throw "Fable version: " + constants.PKG_VERSION + "\n" +
                    "Required: " + fableRequiredVersion + "\n" +
                    "Please upgrade fable-compiler package";
            }
        }
    }

    opts = readBabelOptions(opts);
    opts = readRollupOptions(opts);

    return opts;
}