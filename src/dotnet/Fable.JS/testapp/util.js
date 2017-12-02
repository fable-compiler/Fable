import * as Babel from "${outDir}/babel-standalone";
import * as BabelPlugins from "../../../js/fable-utils/babel-plugins";

export function babelAstToJs(jsonAst) {
    var options = {
        plugins: [
            BabelPlugins.getTransformMacroExpressions(Babel.template),
            BabelPlugins.getRemoveUnneededNulls(),
            "transform-es2015-modules-commonjs"
        ],
        filename: 'repl',
        babelrc: false,
    };

    var ast = JSON.parse(jsonAst);
    return Babel.transformFromAst(ast, null, options).code;
}