"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _server = require("react-dom/server");

var $M3 = _interopRequireWildcard(_server);

var _fableCore = require("../../../lib\\fable-core.js");

var $M2 = _interopRequireWildcard(_fableCore);

var _react = require("react");

var $M1 = _interopRequireWildcard(_react);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) arr2[i] = arr[i]; return arr2; } else { return Array.from(arr); } }

exports.default = function ($M0) {
  var createElement = $M0.createElement = function (a, b, c) {
    return $M1.createElement.apply($M1, [a, b].concat(_toConsumableArray(c)));
  };

  process.exit(function (argv) {
    var root;
    return root = createElement("div", {
      className: "root"
    }, []), console.log($M2.String.format(function (arg00) {
      return $M3.renderToString(arg00);
    }(root))), 0;
  }(process.argv.slice(2)));
  return $M0;
}({});
//# sourceMappingURL=Program.js.map