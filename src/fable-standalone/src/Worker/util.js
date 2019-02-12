/// @ts-check

// @ts-ignore
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

function fetchBlob(getUrl, name) {
    return fetch(getUrl(name))
        .then(function (res) {
            if (res.ok) {
                return res.arrayBuffer().then(b => {
                    return [name, new Uint8Array(b)]
                });
            } else {
                throw new Error("[ASSEMBLY LOAD] " + res.status + ": " + res.statusText);
            }
        });
}

export function getAssemblyReader(getUrl, assemblies) {
    return Promise.all(assemblies.map(name => fetchBlob(getUrl, name)))
        .then(function (kvs) {
            var metadata = new Map();
            for (var kv of kvs) {
                metadata.set(kv[0] + ".dll", kv[1]);
            }
            return (name) => metadata.get(name);
        });
}

function babelOptions(BabelTemplate, extraPlugin) {
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

export function getBabelAstCompiler() {
    // @ts-ignore
    return import("@babel/standalone").then(Babel => {
        return (ast) => {
            try {
                var optionsES2015 = babelOptions(Babel.template);
                var codeES2015 = Babel.transformFromAst(ast, null, optionsES2015).code;
                return codeES2015;
            } catch (err) {
                console.error(err.message + "\n" + err.stack);
            }
        }
    })
}