var template = require("babel-template");

// Remove null args at the end of method or constructor calls.
// This may conflict in some situations when comparing null to undefined, see #231,
// but if disabled can cause problems with some APIs that use options to represent optional
// arguments like CanvasRenderingContext2D.fill: ?fillRule: string -> unit (fails if passed null).
function removeNullTailArgs(path) {
  if (Array.isArray(path.node.arguments)) {
    for (var i = path.node.arguments.length - 1; i >= 0; i--) {
      if (path.node.arguments[i].type === "NullLiteral")
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
exports.removeUnneededNulls = {
  visitor: {
    // Remove `null;` statements (e.g. at the end of constructors)
    ExpressionStatement: function(path) {
      if (path.node.expression.type == "NullLiteral")
        path.remove();
    },
    CallExpression: removeNullTailArgs,
    NewExpression: removeNullTailArgs
  }
};

/**
 * When Babel compiles class methods to ES5 it keeps the function named
 * even if it's a function expression, this is causing problems with Rollup.
 */
exports.removeFunctionExpressionNames = {
  visitor: {
    FunctionExpression: function(path) {
      path.node.id = null;
    }
  }
};

/**
 * Custom plugin to simulate macro expressions.
 */
exports.transformMacroExpressions = {
  visitor: {
    StringLiteral: function(path) {
      if (!path.node.macro || !path.node.value) {
          return;
      }
      var buildArgs = {}, macro = path.node.value;
      try {
        var args = path.node.args;
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
          if (opts.verbose && err.stack) {
            console.log(err.stack);
          }
          process.exit(1);
      }
    }
  }
};
