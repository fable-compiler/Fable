/**
 * Removes unnecessary null statements (e.g. at the end of constructors)
 */
exports.getRemoveUnneededNulls = function() {
  return {
    visitor: {
        // Remove `null;` statements (e.g. at the end of constructors)
        // and orphan empty object literals (on top of constructors with no base)
        ExpressionStatement: function (path) {
            var expr = path.node.expression;
            if (expr.type === "NullLiteral"
                || (expr.type === "ObjectExpression" && expr.properties.length === 0)) {
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

            var DOLLAR_REGEX = /\$\d+/g;
            var buildArgs = {};
            var originalMacro = node.value;
            var macro = originalMacro;

            try {
                // Check if there are more placeholders than args, this may happen
                // if null optional arguments have been removed.
                var m = null;
                var placeholders = [];
                var args = node.args || [];

                while (m = DOLLAR_REGEX.exec(macro)) {
                    placeholders.push(parseInt(m[0].substr(1), 10))
                }

                if (placeholders.length > 0) {
                    var max = placeholders.reduce((x, y) => Math.max(x, y))
                    // If there're actually more args than placeholders (e.g `$0($1...)`), use the args length
                    var argsLength = Math.max(max + 1, args.length);
                    for (var i = 0; i < argsLength; i++) {
                        buildArgs["$" + String(i)] =
                            args[i] == null
                                ? { type: "NullLiteral" }
                                : args[i];
                    }
                }

                macro = macro
                    // Macro transformations, see http://fable.io/docs/interacting.html#emit-attribute
                    .replace(/\$(\d+)\.\.\./, function (_, i) {
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
                    })

                // Babel 7 throws error if there're unused arguments, remove them
                var actualArgs = [];
                var buildArgKeys = Object.keys(buildArgs);
                while (m = DOLLAR_REGEX.exec(macro)) {
                    actualArgs.push(m[0]);
                }
                for (var j = 0; j < buildArgKeys.length; j++) {
                    if (actualArgs.indexOf(buildArgKeys[j]) < 0) {
                        delete buildArgs[buildArgKeys[j]];
                    }
                }

                // console.log("MACRO 1", originalMacro);
                // console.log("MACRO 2", macro);
                // console.log("MACRO ARGS", buildArgs);

                var builtMacro = babelTemplate(macro, {
                    placeholderPattern: /^\$\d+$/
                })(buildArgs);

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
                throw new Error(
                    "BABEL ERROR: Failed to parse macro: " + originalMacro + " -> " + macro + "\n" +
                    "MACRO ARGUMENTS: " + Object.keys(buildArgs).join() + "\n" +
                    err.message);
            }
        }
    }
  }
};
