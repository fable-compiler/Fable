import * as traverse from "babel-traverse";
import * as types from "babel-types";

type UseTemplate = (nodes?: {[placeholder: string]: types.Node}) => types.Node[];
const template: (code: string)=>UseTemplate = require("babel-template");

// Remove null args at the end of method or constructor calls.
// This may conflict in some situations when comparing null to undefined, see #231,
// but if disabled can cause problems with some APIs that use options to represent optional
// arguments like CanvasRenderingContext2D.fill: ?fillRule: string -> unit (fails if passed null).
function removeNullTailArgs<T>(path: traverse.NodePath<types.NewExpression | types.CallExpression>) {
  if (Array.isArray(path.node.arguments)) {
    for (var i = path.node.arguments.length - 1; i >= 0; i--) {
      if (types.isNullLiteral(path.node.arguments[i]))
        path.node.arguments.splice(i, 1);
      else
        break;
    }
  }
}

/**
 * Removes unnecessary null statements and null arguments at the end
 * of method/constructor calls, as these usually represent optional
 * arguments set to None by F# compiler and may conflict with some JS APIs.
 * This plugin must come after transformMacroExpressions (see #377).
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

export const removeRedundantBlocks = {
  visitor: {
    BlockStatement: function(path: traverse.NodePath<types.BlockStatement>) {
      var node = path.node as any;
      if (node.redundant) {
        path.replaceWithMultiple(path.node.body);
      }
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
          console.log("BABEL ERROR: Failed to parse macro: " + macro);
          console.log("MACRO ARGUMENTS: " + Object.getOwnPropertyNames(buildArgs).join());
          console.log(err.message);
          process.exit(1);
      }
    }
  }
};
