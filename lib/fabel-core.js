/* global define */

(function (global, factory) {
  if (typeof define === "function" && define.amd) {
    define(["exports"], factory);
  } else if (typeof exports !== "undefined") {
    factory(exports);
  } else {
    var mod = {
      exports: {}
    };
    factory(mod.exports);
    global.unknown = mod.exports;
  }
})(this, function (exports) {
  "use strict";

  Object.defineProperty(exports, "__esModule", {
    value: true
  });

  var F = {};
  exports.default = F;

  F.Symbols = {
    interfaces: Symbol("interfaces")
  };

  F.Time = {
    "+": function(left, right) {
      return left + right;
    }
  }

  F.Util = {
    // To set an interface on a class Foo, after declaration use
    // Util.setInterfaces(Foo.prototype, ["IFoo", "IBar"]);
    setInterfaces: function(obj, infcs) {
      var parent = obj[F.Symbols.interfaces];
      obj[F.Symbols.interfaces] = infcs;
      if (parent) {
        obj[F.Symbols.interfaces].push(parent);
      }
    },
    hasInterface: function(obj, infc) {
      return (F.Symbols.interfaces in obj) &&
        obj[F.Symbols.interfaces].indexOf(infc) >= 0;
    },
  };

  F.String = {
    printFormatToString: function(str, args) {
      return str.replace(/%[+\-* ]?\d*(?:\.(\d+))?(\w)/g, function(match, number, precision, format) {
        switch (format) {
          case "f": case "F": return precision ? match.toFixed(precision) : match.toFixed(6);
          case "g": case "G": return match.toPrecision(precision);
          case "e": case "E": return match.toExponential(precision);
          case "A": return JSON.stringify(match);
          default:  return match;
        }
      });
    },
    format: function(str, args) {
      return str.replace(/\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g, function(match, number, alignment, format) {
        var rep = match;
        if (args[number] !== undefined) {
          rep = args[number];
          if (format !== undefined) {
            if (typeof rep === 'number') {
              switch (format.substring(0,1)) {
              case "f": case "F": return format.length > 1 ? rep.toFixed(format.substring(1)) : rep.toFixed(2);
              case "g": case "G": return format.length > 1 ? rep.toPrecision(format.substring(1)) : rep.toPrecision();
              case "e": case "E": return format.length > 1 ? rep.toExponential(format.substring(1)) : rep.toExponential();
              case "p": case "P": return (format.length > 1 ? (rep * 100).toFixed(format.substring(1)) : (rep * 100).toFixed(2)) + " %";
              }
            }
            else if (rep instanceof Date) {
              if (format.length === 1) {
              switch (format) {
                case "D": return rep.toDateString();
                case "T": return rep.toLocaleTimeString();
                case "d": return rep.toLocaleDateString();
                case "t": return rep.toLocaleTimeString().replace(/:\d\d(?!:)/, '');
              }
            }
            return format.replace(/(\w)\1*/g, function(match2) {
              var rep2 = match2;
              switch (match2.substring(0,1)) {
                case "y": rep2 = match2.length < 4 ? rep.getFullYear() % 100 : rep.getFullYear(); break;
                case "h": rep2 = rep.getHours() > 12 ? rep.getHours() % 12 : rep.getHours(); break;
                case "M": rep2 = rep.getMonth() + 1; break;
                case "d": rep2 = rep.getDate(); break;
                case "H": rep2 = rep.getHours(); break;
                case "m": rep2 = rep.getMinutes(); break;
                case "s": rep2 = rep.getSeconds(); break;
              }
              if (rep2 !== match2 && rep2 < 10 && match2.length > 1) { rep2 = "0" + rep2; }
              return rep2;
            })
          }
        }
      }
      return rep;
    });
    }
  };

  F.List = (function(){
    var List = function List(head, tail) {
      this.head = head;
      this.tail = tail;
    }
    Object.defineProperty(List.prototype, 'length', {
      get: function() {
        return F.Seq.fold(function (acc, x) {
            return acc + 1;
          }, 0, this);
      }
    });
    List.prototype[Symbol.iterator] = function() {
      var cur = this;
      return {
        next: function() {
          var tmp = cur;
          cur = cur.tail;
          return { done: tmp.tail == null, value: tmp.head }
        }
      }
    }
    List.prototype.rev = function() {
      return F.Seq.fold(function(acc, x) {
        return new List(x, acc);
      }, new List(), this);
    };
    List.prototype.append = function(ys) {
      return F.Seq.fold(function(acc, x) {
        return new List(x, acc);
      }, ys, this.rev());
    };
    List.prototype.collect = function(f) {
      return F.Seq.fold(function(acc, x) {
        return f(x).append(acc);
      }, new List(), this.rev());
    };
    List.prototype.map = function(f) {
      return F.Seq.fold(function(acc, x) {
        return new List(f(x), acc);
      }, new List(), this).rev();
    };
    List.prototype.mapi = function(f) {
      return F.Seq.foldi(function(i, acc, x) {
        return new List(f(i, x), acc);
      }, new List(), this).rev();
    };
    List.prototype.map2 = function(f) {
      throw "TODO"
    };
    List.prototype.mapi2 = function(f) {
      throw "TODO"
    };
    List.prototype.choose = function(f) {
      return F.Seq.fold(function (acc, x) {
        var y = f(x);
        return y != null ? new List(y, acc) : acc;
      }, new List(), this).rev();
    };
    // Static methods
    List.init = function(n,f) {
      var xs = new List();
      for (var i = 1; i <= n; i++) {
        xs = new List(f(n-i), xs);
      }
      return xs;
    };
    List.replicate = function(n,x) {
      return List.init(n,function(){return x});
    };
    return List;
  }());

  F.Seq = (function(){
    var Seq = {};
    Seq.tryFind = function(f, xs) {
        for (var i = 0, iter = xs[Symbol.iterator]();;i++) {
          var cur = iter.next();
          if (cur.done) { break; }
          if (f(cur.value, i)) {
            return cur.value;
          }
        }
    };
    Seq.find = function (f, xs) {
      var res = Seq.tryFind(f, xs);
      if (typeof res === "undefined") {
        throw "List did not contain any matching elements";
      }
      return res;
    };
    Seq.item = function(i) {
      return Seq.find(function(x,j) {
        return i === j;
      }, this);
    };
    Seq.foldi = function(f, acc, xs) {
      for (var i=0, iter=xs[Symbol.iterator]();;i++) {
        var cur = iter.next();
        if (cur.done) { break; }
        acc = f(i, acc, cur.value);
      }
      return acc;
    };    
    Seq.fold = function(f, acc, xs) {
      if (Array.isArray(xs)) {
        xs.reduce(f, acc);
      }
      else {
        for (var iter = xs[Symbol.iterator]();;) {
          var cur = iter.next();
          if (cur.done) { break; }
          acc = f(acc, cur.value);
        }
        return acc;
      }
    };
    Seq.foldBack = function(f, xs, acc) {
      var ar = Array.isArray(xs) ? xs : Seq.toArray(xs);
      for (var i = ar.length - 1; i >= 0; i--) {
        acc = f(ar[i], acc);
      }
      return acc;
    };
    Seq.fold2 = function(f, acc, xs) {
      // TODO
    };
    Seq.foldBack2 = function(f, acc, xs) {
      // TODO
    };    
    Seq.reduce = function(f, xs) {
      if (Array.isArray(xs)) {
        xs.reduce(f);
      }
      else {
        var iter = xs[Symbol.iterator]();
        var cur = iter.next();
        if (cur.done) { throw "Seq was empty"; }
        var acc = cur.value;
        for (;;) {
          cur = iter.next();
          if (cur.done) { break; }
          acc = f(acc, cur.value);
        }
        return acc;
      }
    };
    Seq.reduceBack = function(f, xs, acc) {
      // TODO
    };     
    Seq.delay = function(f) {
      var e = {};
      e[Symbol.iterator] = function () {
        return f()[Symbol.iterator]();
      };
      return e;
    };
    Seq.unfold = function(f, acc) {
      var e = {};
      e[Symbol.iterator] = function () {
        return {
          next: function() {
            var res = f(acc);
            if (res != null) {
              acc = res[1];
              return { done: false, value: res[0] };
            }
            else {
              return { done: true };
            }
          }
        }
      };
      return e;
    };
    Seq.append = function(xs, ys) {
      return Seq.delay(function () {
        var firstDone = false;
        var iters = [xs[Symbol.iterator](), ys[Symbol.iterator]];
        return Seq.unfold(function(x) {
          var cur;
          if (!firstDone) {
            cur = iters[0].next();
            if (!cur.done) {
              return [cur.value, iters];  
            }
            else {
              firstDone = true;
              iters = [null,iters[1]()];
            }
          }
          cur = iters[1].next();
          return !cur.done ? [cur.value, iters] : null;
        }, iters);
      });
    };
    Seq.singleton = function(x) {
      return Seq.unfold(function(x) {
        return x != null ? [x,null] : null;
      }, x);
    };
    Seq.skip = function(n, xs) {
      // TODO
    };
    Seq.skipWhile = function(f, xs) {
      // TODO
    };
    Seq.take = function(n, xs) {
      // TODO
    };
    Seq.takeWhile = function(f, xs) {
      // TODO
    };
    Seq.head = function (xs) {
        var iter = xs[Symbol.iterator]();
        var cur = iter.next();
        if (cur.done) { throw "Seq was empty"; }
        return cur.value;
    };
    Seq.empty = function() {
      return Seq.unfold(function(){});
    };
    Seq.length = function(xs) {
      return Array.isArray(xs)
        ? xs.length : Seq.fold(function (acc, x) {
          return acc + 1;
        }, 0, xs);
    };
    Seq.item = function(i,xs) {
      if (Array.isArray(xs)) {
        return xs[i];
      }
      else {
        for (var j=0, iter=xs[Symbol.iterator]();;j++) {
          var cur = iter.next();
          if (cur.done) { break; }
          if (j === i) { return cur.value; }
        }
        throw "Seq has an insufficient number of elements";
      }
    };    
    Seq.isEmpty = function(xs) {
      var i = xs[Symbol.iterator]();
      return i.next().done;
    };
    Seq.map = function(f, xs) {
      return Seq.delay(function() {
        return Seq.unfold(function(iter) {
          var cur = iter.next();
          return !cur.done ? [f(cur.value), iter] : null;
        }, xs[Symbol.iterator]())
      });
    };
    Seq.mapi = function(f, xs) {
      return Seq.delay(function() {
        var i = 0;
        return Seq.unfold(function(iter) {
          var cur = iter.next();
          return !cur.done ? [f(i++, cur.value), iter] : null;
        }, xs[Symbol.iterator]())
      });
    };
    Seq.map2 = function(f, xs, ys) {
      throw "TODO";
    };
    Seq.map3 = function(f, xs, ys, zs) {
      throw "TODO";
    };
    Seq.filter = function(f, xs) {
      // TODO
    };
    Seq.choose = function(f, xs) {
      // TODO
    };
    // TODO: Check custom operator?
    Seq.sum = function(xs) {
      return Seq.reduce(function(acc, x) {
        return acc + x;
      }, xs);
    };
    Seq.sumBy = function(f, xs) {
      var fst = true;
      return Seq.reduce(function(acc, x) {
        acc = fst ? f(acc) : acc, fst = false;
        return acc + f(x);
      }, xs);
    };
    Seq.max = function(xs) {
      return Seq.reduce(function(acc, x) {
        return Math.max(acc, x);
      }, xs);
    };
    Seq.maxBy = function(f, xs) {
      var fst = true;
      return Seq.reduce(function(acc, x) {
        acc = fst ? f(acc) : acc, fst = false;
        return Math.max(acc, f(x));
      }, xs);
    };
    Seq.min = function(xs) {
      return Seq.reduce(function(acc, x) {
        return Math.min(acc, x);
      }, xs);
    };
    Seq.minBy = function(f, xs) {
      var fst = true;
      return Seq.reduce(function(acc, x) {
        acc = fst ? f(acc) : acc, fst = false;
        return Math.min(acc, f(x));
      }, xs);
    };
    Seq.average = function(xs) {
      var count = 1;
      var sum = Seq.reduce(function(acc, x) {
        count++;
        return acc + x;
      }, xs);
      return sum / count;
    };
    Seq.averageBy = function(f, xs) {
      var count = 1;
      var sum = Seq.reduce(function(acc, x) {
        count++;
        return (count === 2 ? f(acc) : acc) + f(x);
      }, xs);
      return sum / count;
    };
    Seq.rangeStep = function(first, step, last) {
      if (step === 0) {
        throw "Step cannot be 0";
      }
      return Seq.unfold(function(x) {
        return (step > 0 && x <= last) || (step < 0 && x >= last)
          ? [x, x + step] : null;
      }, first);
    };
    Seq.iter = function (f, xs) {
      Seq.fold(function(_,x) {
        f(x);
      }, null, xs);
    };
    Seq.iteri = function (f, xs) {
      throw "TODO"
    };
    Seq.concat = function(xs) {
      return Seq.delay(function() {
        var iter = xs[Symbol.iterator]();
        return Seq.unfold(function(innerIter) {
          var cur, output = null, hasFinished = false;
          while (!hasFinished) {
            if (innerIter == null) {
              cur = iter.next();
              if (!cur.done) {
                innerIter = cur.value[Symbol.iterator]();
              }
              else {
                hasFinished = true;
              }
            }
            else {
              cur = innerIter.next();
              if (!cur.done) {
                output = cur.value;
                hasFinished = true;
              }
              else {
                innerIter = null;
              }              
            }
          }
          return (innerIter != null && output != null) ? [output, innerIter] : null;
        }, null);
      });
    };
    Seq.enumerateWhile = function(cond, xs) {
      return Seq.concat(Seq.unfold(function() {
        return (cond()) ? [xs, true] : null; 
      }), true);
    };
    Seq.enumerateThenFinally = function(xs, finalFn) {
      return Seq.delay(function() {
        var iter;
        try {
          iter = xs[Symbol.iterator]();
        }
        finally {
          finalFn();
        }
        return Seq.unfold(function(iter) {
          try {
            var cur = iter.next();
            return !cur.done ? [cur.value, iter] : null;
          }
          finally {
            finalFn();
          }
        }, iter);
      });     
    };
    Seq.enumerateUsing = function(disp, work) {
      var isDisposed = false;
      var disposeOnce = function() {
        if (!isDisposed) {
          isDisposed = true;
          disp.dispose();
        }
      };
      try {
        return Seq.enumerateThenFinally(work(disp), disposeOnce);
      }
      finally {
        disposeOnce();
      }
    };
    Seq.toArray = function(xs) {
      return Seq.fold(function(acc, x) {
        acc.push(x);
        return acc;
      },[],xs);
    };
    Seq.toList = function(xs) {
      return Seq.foldBack(function(x, acc) {
        return new F.List(x, acc);
      }, xs, new F.List());
    };
    return Seq;
  }());

  F.Async = (function(){
    var Async = {};
    function protectedCont(f) {
      return function(ctx) {
        if (ctx.cancelToken != null && ctx.cancelToken.isCancelled) {
          throw "cancelled";
        }
        try {
          f(ctx);
        }
        catch (err) {
          ctx.onError(err);
        }
      };
    }
    Async.bind = function(work, cont) {
      return protectedCont(function(ctx) {
        work ({
          onSuccess: function(x) {
            return cont(x)(ctx);
          },
          onError: ctx.onError,
          cancelToken: ctx.cancelToken
        })
      });
    };
    Async.combine = function(work1, work2) {
      return Async.bind(work1, function() {
        return work2;
      });
    };
    Async.delay = function(cont) {
      return protectedCont(function(ctx) {
        cont()(ctx);
      });
    };
    Async.for = function(seq, body) {
      var iter = seq[Symbol.iterator](),
          cur = iter.next();
      return Async.while(function() {
          return !cur.done;
        },
        Async.delay(function() {
          var res = body(cur.value);
          cur = iter.next();
          return res;
        })
      );
    };
    Async.return = function(x) {
      return protectedCont(function(ctx) {
        ctx.onSuccess(x);
      });
    };
    Async.returnFrom = function(work) {
      return work;
    };
    Async.tryFinally = function(work, finalFn) {
      return protectedCont(function(ctx) {
        work ({
          onSuccess: function(x) {
            finalFn();
            ctx.onSuccess(x);
          },
          onError: function(x) {
            finalFn();
            ctx.onError(x);
          },
          cancelToken: ctx.cancelToken
        });
      });
    };
    Async.tryWith = function(work, catchFn) {
      return protectedCont(function(ctx) {
        work ({
          onSuccess: ctx.onSuccess,
          cancelToken: ctx.cancelToken,
          onError: function(ex) {
            ctx.onSuccess(catchFn(ex));
          }
        })
      });
    };
    Async.using = function(disp, cont) {
      return Async.tryFinally(cont(disp), function() {
        disp.dispose();
      });
    };
    Async.while = function(cond, body) {
      if (cond()) {
        return Async.bind(body, function() {
          return Async.while(cond, body);
        });
      }
      else {
        return Async.return();
      }
    };
    Async.zero = function() {
      return protectedCont(function(ctx) {
        ctx.onSuccess();
      });
    };
    Async.startImmediate = function(work, cancelToken) {
      work ({
        onSuccess: function(){},
        onError: function(){},
        cancelToken: cancelToken
      });
    };
    Async.start = Async.startImmediate;
    return Async;
  }());
});