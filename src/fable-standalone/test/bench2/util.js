var Babel = require("@babel/core");
var BabelPluginCommonJs = require("@babel/plugin-transform-modules-commonjs").default;
var BabelPlugins = require("${entryDir}/../../../fable-babel-plugins")

export function babelAstToJs(ast) {
    var options = {
        plugins: [
            BabelPlugins.getTransformMacroExpressions(Babel.template),
            BabelPlugins.getRemoveUnneededNulls(),
            BabelPluginCommonJs
        ],
        babelrc: false,
    };

    return Babel.transformFromAstSync(ast, null, options).code;
}