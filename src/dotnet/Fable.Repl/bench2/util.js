var Babel = require("@babel/standalone");
var BabelTemplate = require("@babel/template");
var BabelPluginCommonJs = require("@babel/plugin-transform-modules-commonjs").default;
var BabelPlugins = require("${entryDir}/../../../js/fable-utils/babel-plugins");

export function babelAstToJs(ast) {
    var options = {
        plugins: [
            BabelPlugins.getTransformMacroExpressions(BabelTemplate),
            BabelPlugins.getRemoveUnneededNulls(),
            BabelPluginCommonJs
        ],
        babelrc: false,
    };

    return Babel.transformFromAst(ast, null, options).code;
}