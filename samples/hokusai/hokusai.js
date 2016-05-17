define(["exports", "fable-core"], function (exports, _fableCore) {
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });
  exports.go = exports.render = exports.op_Dynamic = exports.setPixel = exports.height = exports.width = exports.h = exports.w = exports.palette = exports.op_MinusMinusGreater = exports.op_MinusMinus = exports.countIterations = exports.iterate = exports.c = exports.ComplexModule = exports.Complex = undefined;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  var _createClass = function () {
    function defineProperties(target, props) {
      for (var i = 0; i < props.length; i++) {
        var descriptor = props[i];
        descriptor.enumerable = descriptor.enumerable || false;
        descriptor.configurable = true;
        if ("value" in descriptor) descriptor.writable = true;
        Object.defineProperty(target, descriptor.key, descriptor);
      }
    }

    return function (Constructor, protoProps, staticProps) {
      if (protoProps) defineProperties(Constructor.prototype, protoProps);
      if (staticProps) defineProperties(Constructor, staticProps);
      return Constructor;
    };
  }();

  var Complex = exports.Complex = function () {
    function Complex() {
      _classCallCheck(this, Complex);

      this.Case = arguments[0];
      this.Fields = [];

      for (var i = 1; i < arguments.length; i++) {
        this.Fields[i - 1] = arguments[i];
      }
    }

    _createClass(Complex, null, [{
      key: "Abs",
      value: function Abs(_arg1) {
        var r, i, patternInput, num2, num1, num3, num4;
        return r = _arg1.Fields[0], i = _arg1.Fields[1], patternInput = [Math.abs(r), Math.abs(i)], num2 = patternInput[1], num1 = patternInput[0], num1 > num2 ? (num3 = num2 / num1, num1 * Math.sqrt(1 + num3 * num3)) : num2 === 0 ? num1 : (num4 = num1 / num2, num2 * Math.sqrt(1 + num4 * num4));
      }
    }, {
      key: "op_Addition",
      value: function op_Addition(_arg2, _arg3) {
        var r1, i1, r2, i2;
        return r1 = _arg2.Fields[0], i1 = _arg2.Fields[1], r2 = _arg3.Fields[0], i2 = _arg3.Fields[1], new Complex("Complex", r1 + r2, i1 + i2);
      }
    }]);

    return Complex;
  }();

  var ComplexModule = exports.ComplexModule = function ($exports) {
    var Pow = $exports.Pow = function (_arg1, power) {
      var r, i, num, num2, num3, num4;
      return r = _arg1.Fields[0], i = _arg1.Fields[1], num = Complex.Abs(new Complex("Complex", r, i)), num2 = Math.atan2(i, r), num3 = power * num2, num4 = Math.pow(num, power), new Complex("Complex", num4 * Math.cos(num3), num4 * Math.sin(num3));
    };

    return $exports;
  }({});

  var c = exports.c = new Complex("Complex", -0.70176, -0.3842);

  var iterate = exports.iterate = function (x, y) {
    var loop;
    return loop = function (current) {
      return _fableCore.Seq.delay(function (unitVar) {
        return _fableCore.Seq.append(_fableCore.Seq.singleton(current), _fableCore.Seq.delay(function (unitVar_1) {
          return loop(Complex.op_Addition(ComplexModule.Pow(current, 2), c));
        }));
      });
    }, loop(new Complex("Complex", x, y));
  };

  var countIterations = exports.countIterations = function (max, x, y) {
    return _fableCore.Seq.length(_fableCore.Seq.takeWhile(function (v) {
      return Complex.Abs(v) < 2;
    }, _fableCore.Seq.take(max - 1, iterate(x, y))));
  };

  var op_MinusMinus = exports.op_MinusMinus = function (clr, count) {
    return [clr, count];
  };

  var op_MinusMinusGreater = exports.op_MinusMinusGreater = function (_arg1, count, r2, g2, b2) {
    var r1, g1, b1;
    return r1 = _arg1[0], g1 = _arg1[1], b1 = _arg1[2], _fableCore.Seq.toList(_fableCore.Seq.delay(function (unitVar) {
      return _fableCore.Seq.map(function (c_1) {
        var k, mid;
        return k = c_1 / count, mid = function (v1) {
          return function (v2) {
            return v1 + (v2 - v1) * k;
          };
        }, [mid(r1)(r2), mid(g1)(g2), mid(b1)(b2)];
      }, _fableCore.Seq.range(0, count - 1));
    }));
  };

  var palette = exports.palette = Array.from(_fableCore.Seq.delay(function (unitVar) {
    var tupledArg, tupledArg_1, arg00_, count, r2, g2, b2;
    return _fableCore.Seq.append((tupledArg = op_MinusMinus([245, 219, 184], 3), tupledArg_1 = [245, 219, 184], arg00_ = tupledArg[0], count = tupledArg[1], r2 = tupledArg_1[0], g2 = tupledArg_1[1], b2 = tupledArg_1[2], op_MinusMinusGreater(arg00_, count, r2, g2, b2)), _fableCore.Seq.delay(function (unitVar_1) {
      var tupledArg, tupledArg_1, arg00_, count, r2, g2, b2;
      return _fableCore.Seq.append((tupledArg = op_MinusMinus([245, 219, 184], 4), tupledArg_1 = [138, 173, 179], arg00_ = tupledArg[0], count = tupledArg[1], r2 = tupledArg_1[0], g2 = tupledArg_1[1], b2 = tupledArg_1[2], op_MinusMinusGreater(arg00_, count, r2, g2, b2)), _fableCore.Seq.delay(function (unitVar_2) {
        var tupledArg, tupledArg_1, arg00_, count, r2, g2, b2;
        return _fableCore.Seq.append((tupledArg = op_MinusMinus([138, 173, 179], 4), tupledArg_1 = [2, 12, 74], arg00_ = tupledArg[0], count = tupledArg[1], r2 = tupledArg_1[0], g2 = tupledArg_1[1], b2 = tupledArg_1[2], op_MinusMinusGreater(arg00_, count, r2, g2, b2)), _fableCore.Seq.delay(function (unitVar_3) {
          var tupledArg, tupledArg_1, arg00_, count, r2, g2, b2;
          return _fableCore.Seq.append((tupledArg = op_MinusMinus([2, 12, 74], 4), tupledArg_1 = [61, 102, 130], arg00_ = tupledArg[0], count = tupledArg[1], r2 = tupledArg_1[0], g2 = tupledArg_1[1], b2 = tupledArg_1[2], op_MinusMinusGreater(arg00_, count, r2, g2, b2)), _fableCore.Seq.delay(function (unitVar_4) {
            var tupledArg, tupledArg_1, arg00_, count, r2, g2, b2;
            return _fableCore.Seq.append((tupledArg = op_MinusMinus([61, 102, 130], 8), tupledArg_1 = [249, 243, 221], arg00_ = tupledArg[0], count = tupledArg[1], r2 = tupledArg_1[0], g2 = tupledArg_1[1], b2 = tupledArg_1[2], op_MinusMinusGreater(arg00_, count, r2, g2, b2)), _fableCore.Seq.delay(function (unitVar_5) {
              var tupledArg, tupledArg_1, arg00_, count, r2, g2, b2;
              return _fableCore.Seq.append((tupledArg = op_MinusMinus([249, 243, 221], 32), tupledArg_1 = [138, 173, 179], arg00_ = tupledArg[0], count = tupledArg[1], r2 = tupledArg_1[0], g2 = tupledArg_1[1], b2 = tupledArg_1[2], op_MinusMinusGreater(arg00_, count, r2, g2, b2)), _fableCore.Seq.delay(function (unitVar_6) {
                var tupledArg, tupledArg_1, arg00_, count, r2, g2, b2;
                return tupledArg = op_MinusMinus([138, 173, 179], 32), tupledArg_1 = [61, 102, 130], arg00_ = tupledArg[0], count = tupledArg[1], r2 = tupledArg_1[0], g2 = tupledArg_1[1], b2 = tupledArg_1[2], op_MinusMinusGreater(arg00_, count, r2, g2, b2);
              }));
            }));
          }));
        }));
      }));
    }));
  }));
  var w = exports.w = [-0.4, 0.4];
  var h = exports.h = [-0.95, -0.35];
  var width = exports.width = 400;
  var height = exports.height = 300;

  var setPixel = exports.setPixel = function (img, x, y, width_1, r, g, b) {
    var index = (x + y * Math.floor(width_1)) * 4;
    img.data[index + 0] = r;
    img.data[index + 1] = g;
    img.data[index + 2] = b;
    img.data[index + 3] = 255;
  };

  var op_Dynamic = exports.op_Dynamic = function (doc, name) {
    return doc.getElementById(name);
  };

  var render = exports.render = function () {
    return function (builder_) {
      return builder_.delay(function (unitVar) {
        var canv, ctx, img;
        return canv = op_Dynamic(document, "canvas"), ctx = canv.getContext('2d'), img = ctx.createImageData(width, height), builder_.for(_fableCore.Seq.range(0, Math.floor(width) - 1), function (_arg1) {
          var x;
          return x = _arg1, builder_.combine(builder_.for(_fableCore.Seq.range(0, Math.floor(height) - 1), function (_arg2) {
            var y, x_, y_, it, tupledArg, r, g, b;
            return y = _arg2, x_ = x / width * (w[1] - w[0]) + w[0], y_ = y / height * (h[1] - h[0]) + h[0], it = countIterations(palette.length, x_, y_), (tupledArg = palette[it], r = tupledArg[0], g = tupledArg[1], b = tupledArg[2], setPixel(img, x, y, width, r, g, b)), builder_.zero();
          }), builder_.delay(function (unitVar_1) {
            return builder_.bind(_fableCore.Async.sleep(1), function (_arg3) {
              return ctx.putImageData(img, 0, 0), builder_.zero();
            });
          }));
        });
      });
    }(_fableCore.Async);
  };

  var go = exports.go = op_Dynamic(document, "go");
  go.addEventListener('click', function (_arg1) {
    return function (arg00) {
      _fableCore.Async.startImmediate(arg00);
    }(render()), null;
  });
});
//# sourceMappingURL=hokusai.js.map