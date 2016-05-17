define(["exports", "fable-core"], function (exports, _fableCore) {
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });
  exports.update = exports.completed = exports.game = exports.countDrops = exports.updateDrops = exports.newShrink = exports.newGrow = exports.newDrop = exports.shrink = exports.grow = exports.absorb = exports.collide = exports.step = exports.move = exports.bounce = exports.gravity = exports.direct = exports.drawBlob = exports.Blob = exports.drawText = exports.drawBg = exports.drawGrd = exports.ctx = exports.canvas = exports.atmosHeight = exports.floorHeight = exports.height = exports.width = exports.Keyboard = undefined;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  var Keyboard = exports.Keyboard = function ($exports) {
    var keysPressed = $exports.keysPressed = new Set();

    var code = $exports.code = function (x) {
      return keysPressed.has(x) ? 1 : 0;
    };

    var arrows = $exports.arrows = function () {
      return [code(39) - code(37), code(38) - code(40)];
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

    var init = $exports.init = function () {
      window.addEventListener('keydown', function (e) {
        return update(e, true);
      });
      window.addEventListener('keyup', function (e) {
        return update(e, false);
      });
    };

    return $exports;
  }({});

  var width = exports.width = 900;
  var height = exports.height = 668;
  var floorHeight = exports.floorHeight = 100;
  var atmosHeight = exports.atmosHeight = 300;
  Keyboard.init();
  var canvas = exports.canvas = document.getElementsByTagName('canvas')[0];
  var ctx = exports.ctx = canvas.getContext('2d');
  canvas.width = width;
  canvas.height = height;

  var drawGrd = exports.drawGrd = function (ctx_1, canvas_1, y0, y1, c0, c1) {
    var grd = ctx_1.createLinearGradient(0, y0, 0, y1);
    grd.addColorStop(0, c0);
    grd.addColorStop(1, c1);
    ctx_1.fillStyle = grd;
    ctx_1.fillRect(0, y0, canvas_1.width, y1 - y0);
  };

  var drawBg = exports.drawBg = function (ctx_1, canvas_1) {
    drawGrd(ctx_1, canvas_1, 0, atmosHeight, "yellow", "orange");
    drawGrd(ctx_1, canvas_1, atmosHeight, canvas_1.height - floorHeight, "grey", "white");
    ctx_1.fillStyle = "black";
    ctx_1.fillRect(0, canvas_1.height - floorHeight, canvas_1.width, floorHeight);
  };

  var drawText = exports.drawText = function (text, x, y) {
    ctx.fillStyle = "white";
    ctx.font = "bold 40pt";
    ctx.fillText(text, x, y);
  };

  var Blob = exports.Blob = function Blob($arg0, $arg1, $arg2, $arg3, $arg4, $arg5) {
    _classCallCheck(this, Blob);

    this.X = $arg0;
    this.Y = $arg1;
    this.vx = $arg2;
    this.vy = $arg3;
    this.Radius = $arg4;
    this.color = $arg5;
  };

  var drawBlob = exports.drawBlob = function (ctx_1, canvas_1, blob) {
    ctx_1.beginPath();
    ctx_1.arc(blob.X, canvas_1.height - (blob.Y + floorHeight + blob.Radius), blob.Radius, 0, 2 * 3.141592653589793, false);
    ctx_1.fillStyle = blob.color;
    ctx_1.fill();
    ctx_1.lineWidth = 3;
    ctx_1.strokeStyle = blob.color;
    ctx_1.stroke();
  };

  var direct = exports.direct = function (dx, dy, blob) {
    var vx;
    return vx = blob.vx + dx / 4, new Blob(blob.X, blob.Y, vx, blob.vy, blob.Radius, blob.color);
  };

  var gravity = exports.gravity = function (blob) {
    var vy;
    return blob.Y > 0 ? (vy = blob.vy - 0.1, new Blob(blob.X, blob.Y, blob.vx, vy, blob.Radius, blob.color)) : blob;
  };

  var bounce = exports.bounce = function (blob) {
    var n, X, vx;
    return n = width, blob.X < 0 ? (X = -blob.X, vx = -blob.vx, new Blob(X, blob.Y, vx, blob.vy, blob.Radius, blob.color)) : blob.X > n ? (X = n - (blob.X - n), vx = -blob.vx, new Blob(X, blob.Y, vx, blob.vy, blob.Radius, blob.color)) : blob;
  };

  var move = exports.move = function (blob) {
    return new Blob(blob.X + blob.vx, 0 > blob.Y + blob.vy ? 0 : blob.Y + blob.vy, blob.vx, blob.vy, blob.Radius, blob.color);
  };

  var step = exports.step = function (dir_0, dir_1, blob) {
    var dir;
    return dir = [dir_0, dir_1], bounce(move(direct(dir[0], dir[1], blob)));
  };

  var collide = exports.collide = function (a, b) {
    var dx, dy, dist;
    return dx = (a.X - b.X) * (a.X - b.X), dy = (a.Y - b.Y) * (a.Y - b.Y), dist = Math.sqrt(dx + dy), dist < Math.abs(a.Radius - b.Radius);
  };

  var absorb = exports.absorb = function (blob, drops) {
    return _fableCore.List.filter(function (drop) {
      return !collide(blob, drop);
    }, drops);
  };

  var grow = exports.grow = "black";
  var shrink = exports.shrink = "white";

  var newDrop = exports.newDrop = function (color) {
    var X, Y, Radius;
    return X = Math.random() * width * 0.8 + width * 0.1, Y = 600, Radius = 10, new Blob(X, Y, 0, 0, Radius, color);
  };

  var newGrow = exports.newGrow = function () {
    return newDrop(grow);
  };

  var newShrink = exports.newShrink = function () {
    return newDrop(shrink);
  };

  var updateDrops = exports.updateDrops = function (drops, countdown) {
    var drop;
    return countdown > 0 ? [drops, countdown - 1] : Math.floor(Math.random() * 8) === 0 ? (drop = Math.floor(Math.random() * 3) === 0 ? newGrow() : newShrink(), [_fableCore.List.ofArray([drop], drops), 8]) : [drops, countdown];
  };

  var countDrops = exports.countDrops = function (drops) {
    var count;
    return count = function (color) {
      return _fableCore.List.filter(function (drop) {
        return drop.color === color;
      }, drops).length;
    }, [count(grow), count(shrink)];
  };

  var game = exports.game = function () {
    return function (builder_) {
      return builder_.delay(function (unitVar) {
        var blob, X, Y, Radius;
        return blob = (X = 300, Y = 0, Radius = 50, new Blob(X, Y, 0, 0, Radius, "black")), builder_.returnFrom(update(blob, _fableCore.List.ofArray([newGrow()]), 0));
      });
    }(_fableCore.Async);
  };

  var completed = exports.completed = function () {
    return function (builder_) {
      return builder_.delay(function (unitVar) {
        return drawText("COMPLETED", 320, 300), builder_.bind(_fableCore.Async.sleep(10000), function (_arg1) {
          return builder_.returnFrom(game());
        });
      });
    }(_fableCore.Async);
  };

  var update = exports.update = function (blob, drops, countdown) {
    return function (builder_) {
      return builder_.delay(function (unitVar) {
        var patternInput, drops_1, countdown_1, patternInput_1, beforeShrink, beforeGrow, drops_2, patternInput_2, afterShrink, afterGrow, drops_3, radius, radius_1, radius_2, blob_1, blob_2;
        return patternInput = updateDrops(drops, countdown), drops_1 = patternInput[0], countdown_1 = patternInput[1], patternInput_1 = countDrops(drops_1), beforeShrink = patternInput_1[1], beforeGrow = patternInput_1[0], drops_2 = function (drops_2) {
          return absorb(blob, drops_2);
        }(_fableCore.List.map(function ($var1) {
          return move(gravity($var1));
        }, drops_1)), patternInput_2 = countDrops(drops_2), afterShrink = patternInput_2[1], afterGrow = patternInput_2[0], drops_3 = _fableCore.List.filter(function (blob_1) {
          return blob_1.Y > 0;
        }, drops_2), radius = blob.Radius + (beforeGrow - afterGrow) * 4, radius_1 = radius - (beforeShrink - afterShrink) * 4, radius_2 = 5 > radius_1 ? 5 : radius_1, blob_1 = new Blob(blob.X, blob.Y, blob.vx, blob.vy, radius_2, blob.color), blob_2 = function () {
          return function () {
            var tupledArg, arg00_, arg01_;
            return tupledArg = Keyboard.arrows(), arg00_ = tupledArg[0], arg01_ = tupledArg[1], function (blob_2) {
              return step(arg00_, arg01_, blob_2);
            };
          }();
        }()(blob_1), drawBg(ctx, canvas), builder_.combine(builder_.for(drops_3, function (_arg2) {
          var drop;
          return drop = _arg2, drawBlob(ctx, canvas, drop), builder_.zero();
        }), builder_.delay(function (unitVar_1) {
          return drawBlob(ctx, canvas, blob_2), blob_2.Radius > 150 ? builder_.returnFrom(completed()) : builder_.bind(_fableCore.Async.sleep(Math.floor(1000 / 60)), function (_arg3) {
            return builder_.returnFrom(update(blob_2, drops_3, countdown_1));
          });
        }));
      });
    }(_fableCore.Async);
  };

  (function (arg00) {
    _fableCore.Async.startImmediate(arg00);
  })(game());
});
//# sourceMappingURL=ozmo.js.map