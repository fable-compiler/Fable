define(["exports", "fable-core"], function (exports, _fableCore) {
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });
  exports.mario = exports.update = exports.h = exports.w = exports.patternInput = exports.render = exports.step = exports.walk = exports.physics = exports.gravity = exports.jump = exports.Mario = exports.Win = exports.Keyboard = exports.max = undefined;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  var max = exports.max = function (a, b) {
    return a > b ? a : b;
  };

  var Keyboard = exports.Keyboard = function ($exports) {
    var keysPressed = $exports.keysPressed = new Set();

    var code = $exports.code = function (x) {
      return keysPressed.has(x) ? 1 : 0;
    };

    var update = $exports.update = function (e, pressed) {
      var keyCode, op;
      return keyCode = Math.floor(e.keyCode), op = pressed ? function (value) {
        return function (set) {
          return new Set(set).add(value);
        };
      } : function (value) {
        return function (set) {
          return _fableCore.Set.remove(value, set);
        };
      }, keysPressed = op(keyCode)(keysPressed), null;
    };

    var arrows = $exports.arrows = function () {
      return [code(39) - code(37), code(38) - code(40)];
    };

    var init = $exports.init = function () {
      document.addEventListener('keydown', function (e) {
        return update(e, true);
      });
      document.addEventListener('keyup', function (e) {
        return update(e, false);
      });
    };

    return $exports;
  }({});

  var Win = exports.Win = function ($exports) {
    var canvas = $exports.canvas = document.getElementsByTagName('canvas')[0];
    var context = $exports.context = canvas.getContext('2d');

    var op_Dollar = $exports.op_Dollar = function (s, n) {
      return s + n.toString();
    };

    var rgb = $exports.rgb = function (r, g, b) {
      return op_Dollar(op_Dollar(op_Dollar(op_Dollar(op_Dollar(op_Dollar("rgb(", r), ","), g), ","), b), ")");
    };

    var filled = $exports.filled = function (color, rect_0, rect_1, rect_2, rect_3) {
      var rect = [rect_0, rect_1, rect_2, rect_3];
      var ctx = context;
      ctx.fillStyle = color;

      (function (tupledArg) {
        var arg00 = tupledArg[0];
        var arg01 = tupledArg[1];
        var arg02 = tupledArg[2];
        var arg03 = tupledArg[3];
        ctx.fillRect(arg00, arg01, arg02, arg03);
      })(rect);
    };

    var position = $exports.position = function (x, y, img) {
      var copyOfStruct;
      img.style.left = x.toString() + "px";
      img.style.top = (copyOfStruct = canvas.offsetTop + y, copyOfStruct.toString()) + "px";
    };

    var dimensions = $exports.dimensions = function () {
      return [canvas.width, canvas.height];
    };

    var image = $exports.image = function (src) {
      var image;
      return image = document.getElementsByTagName('img')[0], image.src.indexOf(src) === -1 ? image.src = src : null, image;
    };

    return $exports;
  }({});

  var Mario = exports.Mario = function Mario($arg0, $arg1, $arg2, $arg3, $arg4) {
    _classCallCheck(this, Mario);

    this.x = $arg0;
    this.y = $arg1;
    this.vx = $arg2;
    this.vy = $arg3;
    this.dir = $arg4;
  };

  var jump = exports.jump = function (_arg1, y, m) {
    var vy;
    return (y > 0 ? m.y === 0 : false) ? (vy = 5, new Mario(m.x, m.y, m.vx, vy, m.dir)) : m;
  };

  var gravity = exports.gravity = function (m) {
    var vy;
    return m.y > 0 ? (vy = m.vy - 0.1, new Mario(m.x, m.y, m.vx, vy, m.dir)) : m;
  };

  var physics = exports.physics = function (m) {
    return new Mario(m.x + m.vx, max(0, m.y + m.vy), m.vx, m.vy, m.dir);
  };

  var walk = exports.walk = function (x, _arg1, m) {
    var dir, vx;
    return dir = x < 0 ? "left" : x > 0 ? "right" : m.dir, vx = x, new Mario(m.x, m.y, vx, m.vy, dir);
  };

  var step = exports.step = function (dir_0, dir_1, mario) {
    var dir;
    return dir = [dir_0, dir_1], jump(dir[0], dir[1], gravity(walk(dir[0], dir[1], physics(mario))));
  };

  var render = exports.render = function (w, h, mario) {
    (function () {
      return function () {
        var color;
        return color = Win.rgb(174, 238, 238), function (tupledArg) {
          var arg10_ = tupledArg[0];
          var arg11_ = tupledArg[1];
          var arg12_ = tupledArg[2];
          var arg13_ = tupledArg[3];
          Win.filled(color, arg10_, arg11_, arg12_, arg13_);
        };
      }();
    })()([0, 0, w, h]);
    (function () {
      return function () {
        var color;
        return color = Win.rgb(74, 163, 41), function (tupledArg) {
          var arg10_ = tupledArg[0];
          var arg11_ = tupledArg[1];
          var arg12_ = tupledArg[2];
          var arg13_ = tupledArg[3];
          Win.filled(color, arg10_, arg11_, arg12_, arg13_);
        };
      }();
    })()([0, h - 50, w, 50]);
    var verb = mario.y > 0 ? "jump" : mario.vx !== 0 ? "walk" : "stand";
    (function () {
      return function () {
        var tupledArg, x, y;
        return tupledArg = [w / 2 - 16 + mario.x, h - 50 - 31 - mario.y], x = tupledArg[0], y = tupledArg[1], function (img) {
          Win.position(x, y, img);
        };
      }();
    })()(Win.image("images/mario" + verb + mario.dir + ".gif"));
  };

  Keyboard.init();
  var patternInput = exports.patternInput = Win.dimensions();
  var w = exports.w = patternInput[0];
  var h = exports.h = patternInput[1];

  var update = exports.update = function (mario, unitVar1) {
    var mario_1 = function () {
      return function () {
        var tupledArg, arg00_, arg01_;
        return tupledArg = Keyboard.arrows(), arg00_ = tupledArg[0], arg01_ = tupledArg[1], function (mario_1) {
          return step(arg00_, arg01_, mario_1);
        };
      }();
    }()(mario);

    render(w, h, mario_1);
    window.setTimeout(function (arg10_) {
      update(mario_1, arg10_);
    }, 1000 / 60);
  };

  var mario = exports.mario = new Mario(0, 0, 0, 0, "right");
  update(mario);
});
//# sourceMappingURL=mario.js.map