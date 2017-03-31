import * as traverse from "babel-traverse";
import * as types from "babel-types";

type UseTemplate = (nodes?: {[placeholder: string]: types.Node}) => types.Node[];
const template: (code: string)=>UseTemplate = require("babel-template");

/**
 * Removes unnecessary null statements (e.g. at the end of constructors)
 */
export const removeUnneededNulls = {
  visitor: {
    // Remove `null;` statements (e.g. at the end of constructors)
    ExpressionStatement: function(path: traverse.NodePath<types.ExpressionStatement>) {
      if (types.isNullLiteral(path.node.expression))
        path.remove();
    }
  }
};

/**
 * When Babel compiles class methods to ES5 it keeps the function named
 * even if it's a function expression, this is causing problems with Rollup.
 */
export const removeFunctionExpressionNames = {
  visitor: {
    FunctionExpression: function(path: traverse.NodePath<types.FunctionExpression>) {
      path.node.id = null;
    }
  }
};

/**
 * Custom plugin to simulate macro expressions.
 */
export const transformMacroExpressions = {
  visitor: {
    StringLiteral: function(path: traverse.NodePath<types.StringLiteral>) {
      var node = path.node as any;
      if (!node.macro || !node.value) {
          return;
      }
      var buildArgs: any = {}, macro: string = node.value;
      try {
        var args = node.args;
        for (var i = 0; i < args.length; i++) {
            buildArgs["$" + i] = args[i];
        }
        macro = macro
            // Replace spread aguments like in `$0($1...)`
            .replace(/\$(\d+)\.\.\./, function (m, i) {
                var rep = [], j = parseInt(i);
                for (; j < args.length; j++) {
                    rep.push("$" + j);
                }
                return rep.join(",");
            })
            // Replace conditional arguments like in `/$0/g{{$1?i:}}{{$2?m:}}`
            .replace(/\{\{\$(\d+)\?(.*?)\:(.*?)\}\}/g, function (_, g1, g2, g3) {
                var i = parseInt(g1);
                return i < args.length && args[i].value ? g2 : g3;
            })
            // Replace optional arguments like in `$0[$1]{{=$2}}`
            .replace(/\{\{([^\}]*\$(\d+).*?)\}\}/g, function (_, g1, g2) {
                var i = parseInt(g2);
                return i < args.length ? g1 : "";
            });
        var buildMacro = template(macro);
        path.replaceWithMultiple(buildMacro(buildArgs));
      }
      catch (err) {
        err.message =
          "BABEL ERROR: Failed to parse macro: " + macro + "\n" +
          "MACRO ARGUMENTS: " + Object.getOwnPropertyNames(buildArgs).join() + "\n" +
          err.message;
        throw err;
      }
    }
  }
};
