module.exports = {
    getRemoveUnneededNulls,
    getTransformMacroExpressions
}

/**
 * Removes unnecessary null statements (e.g. at the end of constructors)
 */
function getRemoveUnneededNulls() {
  return {
    visitor: {
        // Remove `null;` statements (e.g. at the end of constructors)
        // and orphan empty object literals (on top of constructors with no base)
        ExpressionStatement(path) {
            const expr = path.node.expression;
            if (expr.type === "NullLiteral"
                || (expr.type === "ObjectExpression" && expr.properties.length === 0)) {
                path.remove();
            }
        },
    },
  };
}

/**
 * Custom plugin to simulate macro expressions.
 */
function getTransformMacroExpressions(babelTemplate) {
  return {
    visitor: {
        StringLiteral(path) {
            const node = path.node;
            if (!node.macro) {
                // This has nothing to do with macros but it's necessary to prevent
                // Babel from escaping Unicode strings. See #784
                if (typeof node.value === "string") {
                    node.extra = {
                        raw: JSON.stringify(node.value),
                        rawValue: node.value,
                    };
                }
                return;
            }

            const DOLLAR_REGEX = /\$\d+/g;
            const buildArgs = {};
            const originalMacro = node.value;
            let macro = originalMacro;

            try {
                // Check if there are more placeholders than args, this may happen
                // if null optional arguments have been removed.
                let m = null;
                const placeholders = [];
                const args = node.args || [];

                // tslint:disable-next-line:no-conditional-assignment
                while (m = DOLLAR_REGEX.exec(macro)) {
                    placeholders.push(parseInt(m[0].substr(1), 10));
                }

                if (placeholders.length > 0) {
                    const max = placeholders.reduce((x, y) => Math.max(x, y));
                    // If there're actually more args than placeholders (e.g `$0($1...)`), use the args length
                    const argsLength = Math.max(max + 1, args.length);
                    for (let i = 0; i < argsLength; i++) {
                        buildArgs["$" + String(i)] =
                            args[i] == null
                                ? { type: "NullLiteral" }
                                : args[i];
                    }
                }

                macro = macro
                    // Macro transformations, see http://fable.io/docs/interacting.html#emit-attribute
                    .replace(/\$(\d+)\.\.\./, (_, i) => {
                        const rep = [];
                        for (let j = parseInt(i, 10); j < args.length; j++) {
                            rep.push("$" + j);
                        }
                        return rep.join(",");
                    })
                    .replace(/\{\{\s*\$(\d+)\s*\?(.*?)\:(.*?)\}\}/g, (_, g1, g2, g3) => {
                        const i = parseInt(g1, 10);
                        return typeof args[i] === "object" && args[i].value ? g2 : g3;
                    })
                    .replace(/\{\{([^\}]*\$(\d+).*?)\}\}/g, (_, g1, g2) => {
                        const i = parseInt(g2, 10);
                        return typeof args[i] === "object" && args[i].type !== "NullLiteral" ? g1 : "";
                    })
                    .replace(/\$(\d+)!/, (_, i) => {
                        const arg = args[parseInt(i, 10)]
                        return typeof arg === "object" && arg.type === "StringLiteral" ? arg.value : "";
                    });

                // Babel 7 throws error if there're unused arguments, remove them
                const actualArgs = [];
                const buildArgKeys = Object.keys(buildArgs);

                // tslint:disable-next-line:no-conditional-assignment
                while (m = DOLLAR_REGEX.exec(macro)) {
                    actualArgs.push(m[0]);
                }
                for (const key of buildArgKeys) {
                    if (actualArgs.indexOf(key) < 0) {
                        delete buildArgs[key];
                    }
                }

                // console.log("MACRO 1", originalMacro);
                // console.log("MACRO 2", macro);
                // console.log("MACRO ARGS", buildArgs);

                const builtMacro = babelTemplate(macro, {
                    placeholderPattern: /^\$\d+$/,
                })(buildArgs);

                if (builtMacro != null) {
                    path.replaceWithMultiple(builtMacro);
                } else {
                    // Apparently if the macro is just a string, babel-template will fail
                    path.replaceWith({
                        type: "StringLiteral",
                        value: JSON.parse(macro),
                    });
                }
            } catch (err) {
                throw new Error(
                    "BABEL ERROR: Failed to parse macro: " + originalMacro + " -> " + macro + "\n" +
                    "MACRO ARGUMENTS: " + Object.keys(buildArgs).join() + "\n" +
                    err.message);
            }
        },
    },
  };
}
