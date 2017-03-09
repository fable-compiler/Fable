var babel = require("babel-core");

var template = babel.template;
/**
 * Removes unnecessary null statements (e.g. at the end of constructors)
 */
exports.removeUnneededNulls = {
    visitor: {
        // Remove `null;` statements (e.g. at the end of constructors)
        ExpressionStatement: function (path) {
            if (path.node.expression.type === "NullLiteral") {
                path.remove();
            }
        }
    }
};
/**
 * Custom plugin to simulate macro expressions.
 */
exports.transformMacroExpressions = {
    visitor: {
        StringLiteral: function (path) {
            var node = path.node;
            if (!node.macro || !node.value) {
                return;
            }
            var buildArgs = {}, macro = node.value;
            try {
                var args = node.args;
                for (var i = 0; i < args.length; i++) {
                    buildArgs["$" + i] = args[i];
                }
                macro = macro
                    .replace(/\$(\d+)\.\.\./, function (m, i) {
                    var rep = [], j = parseInt(i);
                    for (; j < args.length; j++) {
                        rep.push("$" + j);
                    }
                    return rep.join(",");
                })
                    .replace(/\{\{\$(\d+)\?(.*?)\:(.*?)\}\}/g, function (_, g1, g2, g3) {
                    var i = parseInt(g1);
                    return i < args.length && args[i].value ? g2 : g3;
                })
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
