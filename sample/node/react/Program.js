"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _fableCore = require("../../../lib\\fable-core.js");

var $M3 = _interopRequireWildcard(_fableCore);

var _server = require("react-dom/server");

var $M2 = _interopRequireWildcard(_server);

var _react = require("react");

var $M1 = _interopRequireWildcard(_react);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

exports.default = function ($M0) {
  process.exit(function (argv) {
    var a, b;
    return a = $M1.createElement("div", {}), b = $M2.renderToString(a), console.log($M3.String.format(b)), 0;
  }(process.argv.slice(2)));
  return $M0;
}({});
//# sourceMappingURL=Program.js.map