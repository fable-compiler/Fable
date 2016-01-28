var template = require("babel-template");

module.exports = function(babel) {
  return {
    visitor: {
      TemplateLiteral(path) {
        if (!path.node.macro)
            return;

        var macro = "";
        var quasisLength = path.node.quasis.length;
        for (var i = 0; i < quasisLength; i++) {
            macro += path.node.quasis[i].value.raw;
            if (i < quasisLength - 1) {
                macro += "$" + i;
            }
        }
        
        var buildArgs = {};
        var buildMacro = template(macro);

        for (var i = 0; i < path.node.expressions.length; i++) {
            buildArgs["$" + i] = path.node.expressions[i];
        }
        
        path.replaceWith(buildMacro(buildArgs));
      }
    }
  };
};