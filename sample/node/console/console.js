(function (global, factory) {
  if (typeof define === "function" && define.amd) {
    define(["exports", "../../../lib\\fabel-core.js"], factory);
  } else if (typeof exports !== "undefined") {
    factory(exports, require("../../../lib\\fabel-core.js"));
  } else {
    var mod = {
      exports: {}
    };
    factory(mod.exports, global.fabelCore);
    global.unknown = mod.exports;
  }
})(this, function (exports, _fabelCore) {
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });

  var $M1 = _interopRequireWildcard(_fabelCore);

  function _interopRequireWildcard(obj) {
    if (obj && obj.__esModule) {
      return obj;
    } else {
      var newObj = {};

      if (obj != null) {
        for (var key in obj) {
          if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key];
        }
      }

      newObj.default = obj;
      return newObj;
    }
  }

  exports.default = function ($M0) {
    process.exit(function (argv) {
      var res;
      return res = $M1.String.concat("", $M1.Seq.map(function (value) {
        return value.toString();
      }, function () {
        throw 1;
      }()(argv[0]))), function () {
        var clo1;
        return clo1 = $M1.String.fsFormat("Hello %s!")(function (x) {
          console.log(x);
        }), function (arg10) {
          clo1(arg10);
        };
      }()(res), 0;
    }(process.argv.slice(2)));
    return $M0;
  }({});
});
//# sourceMappingURL=console.js.map