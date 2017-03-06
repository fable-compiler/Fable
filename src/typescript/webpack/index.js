var child_process  = require('child_process');
var client = require("./client.js");
var babel = require("babel-core");

var port = 61225;

var babelPlugins = (function () {
  var template = babel.template;
  /**
   * Removes unnecessary null statements (e.g. at the end of constructors)
   */
  var removeUnneededNulls = {
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
  var transformMacroExpressions = {
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
  return {
    removeUnneededNulls: removeUnneededNulls,
    transformMacroExpressions: transformMacroExpressions
  }
})();


module.exports = function(buffer) {
    this.cacheable();
    var callback = this.async();
    var msg = {
        path: this.resourcePath,
        options: {
            symbols: [],
            plugins: [],
            clamp: false,
            declaration: false,
            typedArrays: true
        }
    };
    client.send(port, JSON.stringify(msg))
        .then(data => {
            var data = JSON.parse(data);
            if (data.error) {
                callback(data.error);
            }
            else {
                data.infos.forEach(x => console.log(x))
                data.warnings.forEach(x => this.emitWarning(x))
                var options = {
                    plugins: [
                        babelPlugins.transformMacroExpressions,
                        babelPlugins.removeUnneededNulls,
                    ],
                };
                var transformed = babel.transformFromAst(data, null, options);
                // TODO: Mark dependencies
                callback(null, transformed.code);
            }
        })
        .catch(err => {
            var msg = err.message + " Make sure Fable server is running on port " + port;
            callback(new Error(msg))
        })
};
module.exports.raw = true;
