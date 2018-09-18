var Babel = require("babel-standalone");
var BabelTemplate = require("babel-template");
var BabelPlugins = require("${entryDir}/../../../js/fable-utils/babel-plugins");

export function babelAstToJs(ast) {
    var options = {
        plugins: [
            BabelPlugins.getTransformMacroExpressions(BabelTemplate),
            BabelPlugins.getRemoveUnneededNulls(),
            "transform-es2015-modules-commonjs"
        ],
        babelrc: false,
    };

    return Babel.transformFromAst(ast, null, options).code;
}