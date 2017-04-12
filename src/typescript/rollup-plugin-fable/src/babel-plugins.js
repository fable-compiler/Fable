const babel = require('babel-core');
const template = babel.template;

/**
 * Removes unnecessary null statements (e.g. at the end of constructors)
 */
exports.removeUnneededNulls = {
  visitor: {
    // Remove `null;` statements (e.g. at the end of constructors)
    ExpressionStatement: function(path) {
      if (path.node.expression.type === 'NullLiteral') path.remove();
    }
  }
};
/**
 * Custom plugin to simulate macro expressions.
 */
exports.transformMacroExpressions = {
  visitor: {
    StringLiteral: function(path) {
      const node = path.node;
      if (!node.macro || !node.value) return;

      const buildArgs = {};
      let macro = node.value;
      try {
        // Check if there are more placeholders than args, this may happen
        // if null optional arguments have been removed.
        const placeholders = [];
        let m = null;
        const reg = /\$\d+/g, args = node.args;
        while ((m = reg.exec(macro)))
          placeholders.push(parseInt(m[0].substr(1)));

        if (placeholders.length > 0) {
          const max = placeholders.reduce((x, y) => Math.max(x, y));
          // If there're actually more args than placeholders (e.g `$0($1...)`), use the args length
          const argsLength = Math.max(max + 1, args.length);
          for (let i = 0; i < argsLength; i++)
            buildArgs['$' + i] = args[i] == null
              ? { type: 'NullLiteral' }
              : args[i];
        }
        macro = macro
          .replace(/\$(\d+)\.\.\./, function(m, i) {
            const rep = [];
            let j = parseInt(i);
            for (; j < args.length; j++) rep.push('$' + j);

            return rep.join(',');
          })
          .replace(/\{\{\$(\d+)\?(.*?)\:(.*?)\}\}/g, function(_, g1, g2, g3) {
            const i = parseInt(g1);
            return i < args.length && args[i].value ? g2 : g3;
          })
          .replace(/\{\{([^\}]*\$(\d+).*?)\}\}/g, function(_, g1, g2) {
            const i = parseInt(g2);
            return i < args.length ? g1 : '';
          });
        const buildMacro = template(macro);
        path.replaceWithMultiple(buildMacro(buildArgs));
      } catch (err) {
        err.message = 'BABEL ERROR: Failed to parse macro: ' +
          macro +
          '\n' +
          'MACRO ARGUMENTS: ' +
          Object.getOwnPropertyNames(buildArgs).join() +
          '\n' +
          err.message;
        throw err;
      }
    }
  }
};
