import * as path from "path";
import * as child_process from "child_process";
import * as rollup from 'rollup';
import * as hypothetical from 'rollup-plugin-hypothetical';

import * as fableLib from "./lib";
import * as constants from "./constants";
import { postbuild } from "./build";
import { Dic, FableOptions, BabelFile, Continuation, BuildResult } from "./types";

/** Bundles generated JS files and dependencies, requires rollup and plugins */
let bundleCache: any = null;
let fullRebuildTriggered = false;

function normalizeProjectName(opts: FableOptions) {
    var projName = path.basename(opts.projFile[opts.projFile.length - 1]);
    return projName.substr(0, projName.indexOf(".")).replace(/[^A-Z_]/ig, "_");
}

export function readRollupOptions(opts: FableOptions) {
    if (!opts.rollup) {
        return opts;
    }
    var rollupOpts = opts.rollup;
    var outDir = fableLib.pathJoin(opts.workingDir, opts.outDir);

    rollupOpts = typeof rollupOpts === "boolean" ? {} : rollupOpts;
    rollupOpts = typeof rollupOpts === "string" ? { dest: fableLib.pathJoin(outDir, rollupOpts) } : rollupOpts;
    rollupOpts.plugins = fableLib.resolvePlugins(rollupOpts.plugins, opts.workingDir, "rollup-plugin-");

    var plugins = [],
        nodeResolve = 'rollup-plugin-node-resolve',
        commonjs = 'rollup-plugin-commonjs';

    // Attention: Plugin order is important
    if (!rollupOpts.plugins.some(function(kv: [string, any]) { return kv[0].indexOf(nodeResolve) >= 0 })) {
        plugins.push(
            require(nodeResolve)({ ignoreGlobal: true })
        );
    }
    if (!rollupOpts.plugins.some(function(kv: [string, any]) { return kv[0].indexOf(commonjs) >= 0 })) {
        plugins.push(
            require(commonjs)({ jsnext: true, main: true, browser: true })
        );
    }

    // Custom plugins
    for (var i = 0; i < rollupOpts.plugins.length; i++) {
        var kv = rollupOpts.plugins[i];
        plugins.push(require(kv[0])(kv[1]));
    }
    rollupOpts.plugins = plugins;

    // Other options
    rollupOpts.format = rollupOpts.format == null ? (constants.JS_MODULES as any)[opts.module] : rollupOpts.format;
    rollupOpts.sourceMap = rollupOpts.sourceMap == null ? opts.sourceMaps : rollupOpts.sourceMap
    rollupOpts.moduleName = rollupOpts.moduleName || normalizeProjectName(opts);
    rollupOpts.dest = rollupOpts.dest == null
        ? fableLib.pathJoin(outDir, "bundle.js")
        : fableLib.pathJoin(opts.workingDir, rollupOpts.dest);

    if (!opts.verbose) { // Swallow Rollup warnings
        rollupOpts.onwarn = warning => {};
    }

    opts.rollup = rollupOpts;
    return opts;
}

export function clearBundleCache() {
    bundleCache = null;
}

export function bundle(jsFiles: Dic<BabelFile>, opts: FableOptions, fableProc: child_process.ChildProcess, continuation: Continuation) {
    var rollupOpts = Object.assign({}, opts.rollup);
    rollupOpts.cache = bundleCache;
    rollupOpts.plugins.splice(0, 0, hypothetical({
        files: jsFiles, allowRealFiles: true, allowExternalModules: true
    }));

    if (rollupOpts.entry == null) {
        rollupOpts.entry = Object.getOwnPropertyNames(jsFiles)
                        .find(function(f) { return jsFiles[f].isEntry });
    }

    fableLib.stdoutLog("Bundling...");
    rollup.rollup(rollupOpts)
        .then((bundle: any) => {
            var parsed = bundle.generate(rollupOpts);
            if (opts.watch) {
                bundleCache = bundle;
            }
            // Write to disk, bundle.write doesn't seem to work
            // bundle.write({ dest: rollupOpts.dest, format: rollupOpts.format, sourceMap: rollupOpts.sourceMap });
            fableLib.writeFile(rollupOpts.dest, parsed.code,
                rollupOpts.sourceMap === true ? parsed.map : null);
            fableLib.stdoutLog("Bundled " + path.relative(opts.workingDir, rollupOpts.dest) + " at " + (new Date()).toLocaleTimeString());
            postbuild(opts, "SUCCESS", fableProc, continuation);
        })
        .catch((err: Error) => {
            // The stack here is a bit noisy just report the message
            fableLib.stderrLog(err.message);
            bundleCache = null;
            let buildResult: BuildResult = null;
            if (!fullRebuildTriggered) {
                fullRebuildTriggered = true;
                buildResult = "NEEDS_FULL_REBUILD";
            }
            else {
                fullRebuildTriggered = false;   // Prevent infinite loop
                buildResult = "FAIL";
            }
            postbuild(opts, buildResult, fableProc, continuation);
        });
}
