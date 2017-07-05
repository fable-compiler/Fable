/**
 * Removes unnecessary null statements (e.g. at the end of constructors)
 */
exports.getRemoveUnneededNulls = function() {
  return {
    visitor: {
        // Remove `null;` statements (e.g. at the end of constructors)
        ExpressionStatement: function (path) {
            if (path.node.expression.type === "NullLiteral") {
                path.remove();
            }
        }
    }
  }
};

/**
 * Custom plugin to simulate macro expressions.
 */
exports.getTransformMacroExpressions = function(babelTemplate) {
  return {
    visitor: {
        StringLiteral: function (path) {
            var node = path.node;
            if (!node.macro) {
                // This has nothing to do with macros but it's necessary to prevent
                // Babel from escaping Unicode strings. See #784
                if (typeof node.value === "string") {
                    node.extra = {
                        raw: JSON.stringify(node.value),
                        rawValue: node.value
                    }
                }
                return;
            }
            var buildArgs = {}, macro = node.value;
            // console.log("MACRO 1: " + macro);
            try {
                // Check if there are more placeholders than args, this may happen
                // if null optional arguments have been removed.
                var placeholders = [], m = null;
                var reg = /\$\d+/g, args = node.args;
                while (m = reg.exec(macro)) {
                    placeholders.push(parseInt(m[0].substr(1), 10))
                }
                if (placeholders.length > 0) {
                    var max = placeholders.reduce((x, y) => Math.max(x, y))
                    // If there're actually more args than placeholders (e.g `$0($1...)`), use the args length
                    var argsLength = Math.max(max + 1, args.length);
                    for (var i = 0; i < argsLength; i++) {
                        buildArgs["$" + i] = args[i] == null
                            ? { type: "NullLiteral" }
                            : args[i];
                    }
                }
                macro = macro
                    .replace(/\$(\d+)\.\.\./, function (m, i) {
                        var rep = [], j = parseInt(i, 10);
                        for (; j < args.length; j++) {
                            rep.push("$" + j);
                        }
                        return rep.join(",");
                    })
                    .replace(/\{\{\$(\d+)\?(.*?)\:(.*?)\}\}/g, function (_, g1, g2, g3) {
                        var i = parseInt(g1, 10);
                        return typeof args[i] === "object" && args[i].value ? g2 : g3;
                    })
                    .replace(/\{\{([^\}]*\$(\d+).*?)\}\}/g, function (_, g1, g2) {
                        var i = parseInt(g2, 10);
                        return typeof args[i] === "object" && args[i].type !== "NullLiteral" ? g1 : "";
                    });
                // console.log("MACRO 2: " + macro);
                // console.log("MACRO ARGS: " + JSON.stringify(buildArgs));
                // console.log("BUILT MACRO: " + JSON.stringify(babelTemplate(macro)(buildArgs)));
                var builtMacro = babelTemplate(macro)(buildArgs);
                if (builtMacro != null) {
                    path.replaceWithMultiple(builtMacro);
                }
                else {
                    // Apparently if the macro is just a string, babel-template will fail
                    path.replaceWith({
                        type: "StringLiteral",
                        value: JSON.parse(macro)
                    });
                }
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
  }
};
