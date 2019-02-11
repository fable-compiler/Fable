/// @ts-check

// @ts-ignore
import * as Babel from "@babel/standalone";
import BabelTemplate from "@babel/template";
import * as BabelPlugins from "fable-babel-plugins";

export function resolveLibCall(libMap, entityName) {
    if (libMap != null) {
        var k = Object.keys(libMap).find((k) => entityName.indexOf(k) === 0);
        if (k != null) {
            var result = libMap[k];
            // Remove the root module
            var entityNameTrimmed = entityName.substr(result[0].length).replace(/^\.+/, "");
            return [entityNameTrimmed, "fable-repl-lib/" + result[1]];
        }
    }
    return null;
}

function fetchBlob(url) {
    return fetch(url)
        .then(function (res) {
            if (res.ok) {
                return res.arrayBuffer().then(b => [name, new Uint8Array(b)]);
            } else {
                throw new Error("[ASSEMBLY LOAD] " + res.status + ": " + res.statusText);
            }
        });
}

export function getAssemblyReader(getBlobUrl, assemblies) {
    return Promise.all(assemblies.map(name => fetchBlob(getBlobUrl(name))))
        .then(function (kvs) {
            var metadata = new Map();
            for (var kv of kvs) {
                metadata.set(kv[0] + ".dll", kv[1]);
            }
            return (name) => metadata.get(name);
        });
}

function babelOptions(extraPlugin) {
    var commonPlugins = [
        BabelPlugins.getTransformMacroExpressions(BabelTemplate),
        BabelPlugins.getRemoveUnneededNulls(),
    ];
    return {
        plugins:
            extraPlugin != null
                ? commonPlugins.concat(extraPlugin)
                : commonPlugins,
        filename: 'repl',
        babelrc: false,
    };
}

export function compileBabelAst(ast) {
    try {
        var optionsES2015 = babelOptions();
        var codeES2015 = Babel.transformFromAst(ast, null, optionsES2015).code;
        return codeES2015;
    } catch (err) {
        console.error(err.message + "\n" + err.stack);
    }
}
