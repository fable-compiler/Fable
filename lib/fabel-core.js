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

  var Symbols = exports.Symbols = {
    interfaces: Symbol("interfaces")
  };

  var Time = exports.Time = {
    "+": function (left, right) {
      return left + right;
    }
  }

  var Util = exports.Util = {};
  // To set an interface on a class Foo, after declaration use
  // Util.setInterfaces(Foo.prototype, ["IFoo", "IBar"]);
  Util.setInterfaces = function (obj, infcs) {
    var parent = obj[Symbols.interfaces];
    obj[Symbols.interfaces] = infcs;
    if (parent) {
      obj[Symbols.interfaces].push(parent);
    }
  };
  Util.hasInterface = function (obj, infc) {
    return (Symbols.interfaces in obj) &&
      obj[Symbols.interfaces].indexOf(infc) >= 0;
  };
  Util.getRestParams = function (args, idx) {
    for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++) {
      restArgs[_key - idx] = args[_key];
    }    
    return restArgs;
  };

  var FString = exports.String = {};
  FString.fsFormatRegExp = /%[+\-* ]?\d*(?:\.(\d+))?(\w)/;
  FString.fsFormat = function (str) {
    var _cont;
    function formatOnce(str, rep) {
      return str.replace(FString.fsFormatRegExp, function (_, precision, format) {
        switch (format) {
          case "f": case "F": return precision ? rep.toFixed(precision) : rep.toFixed(6);
          case "g": case "G": return rep.toPrecision(precision);
          case "e": case "E": return rep.toExponential(precision);
          case "A": return JSON.stringify(rep);
          default: return rep;
        }
      });
    }
    function makeFn(str) {
      return function(rep) {
        var str2 = formatOnce(str, rep);
        return FString.fsFormatRegExp.test(str2)
                ? makeFn(str2) : _cont(str2);
      }
    }
    return function (cont) {
      _cont = cont;
      return makeFn(str);
    }
  };
  FString.formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;
  FString.format = function (str, args) {
    args = Util.getRestParams(arguments, 1);
    return str.replace(FString.formatRegExp, function (match, idx, alignment, format) {
      var rep = args[idx];
      if (rep != null && format != null) {
        if (typeof rep === 'number') {
          switch (format.substring(0, 1)) {
            case "f":
            case "F":
              return format.length > 1
                      ? rep.toFixed(format.substring(1))
                      : rep.toFixed(2);
            case "g":
            case "G":
              return format.length > 1
                      ? rep.toPrecision(format.substring(1))
                      : rep.toPrecision();
            case "e":
            case "E":
              return format.length > 1
                      ? rep.toExponential(format.substring(1))
                      : rep.toExponential();
            case "p":
            case "P":
              return (format.length > 1
                      ? (rep * 100).toFixed(format.substring(1))
                      : (rep * 100).toFixed(2)) + " %";
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
          return format.replace(/\w+/g, function (match2) {
            var rep2 = match2;
            switch (match2.substring(0, 1)) {
              case "y":
                rep2 = match2.length < 4
                        ? rep.getFullYear() % 100
                        : rep.getFullYear();
                break;
              case "h":
                rep2 = rep.getHours() > 12
                        ? rep.getHours() % 12
                        : rep.getHours();
                break;
              case "M":
                rep2 = rep.getMonth() + 1;
                break;
              case "d":
                rep2 = rep.getDate();
                break;
              case "H":
                rep2 = rep.getHours();
                break;
              case "m":
                rep2 = rep.getMinutes();
                break;
              case "s":
                rep2 = rep.getSeconds();
                break;
            }
            if (rep2 !== match2 && rep2 < 10 && match2.length > 1) {
              rep2 = "0" + rep2;
            }
            return rep2;
          });
        }
      }
      return rep;
    });
  };
  FString.isNullOrEmpty = function (str) {
    return typeof str !== "string" || str.length == 0;
  };
  FString.isNullOrWhiteSpace = function (str) {
    return typeof str !== "string" || /^\s*$/.test(str);
  };
  FString.replace = function (str, search, replace) {
    return str.replace(new RegExp(Regex.escape(search), "g"), replace);
  };
  FString.split = function (str, splitters, removeEmpty) {
    splitters = Array.isArray(splitters) ? splitters : Util.getRestParams(arguments, 1);
    var reg = new RegExp("[" + Regex.escape(splitters.join("")) + "]", "g");
    var splits = str.split(reg);
    return !removeEmpty ? splits : splits.filter(function (x) {
      return x.length > 0;
    });
  };
  FString.join = FString.concat = function (delimiter, xs) {
    xs = typeof xs == "string" 
          ? Util.getRestParams(arguments, 1) : xs;
    return Array.isArray(xs)
      ? xs.join(delimiter)
      : Seq.reduce(function (acc, x) {
        return acc + delimiter + x;
      }, xs);
  };
  FString.endsWith = function (str, search) {
    var idx = str.lastIndexOf(search);
    return idx >= 0 && idx == (str.length - search.length);
  };

  var Regex = exports.Regex = {};
  // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
  Regex.escape = function (str) {
    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&');
  }
  Regex.unescape = function (str) {
    return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, '$1');
  };

  var FArray = exports.Array = {};
  FArray.permute = function (f, xs) {
    // Keep the type of the array
    var ys = xs.map(function () { return 0 });
    var checkFlags = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
      var j = f(i);
      if (j < 0 || j >= xs.length) {
        throw "Not a valid permutation";
      }
      ys[j] = xs[i];
      checkFlags[j] = 1;
    }
    for (i = 0; i < xs.length; i++) {
      if (checkFlags[i] != 1) {
        throw "Not a valid permutation";
      }
    }
    return ys;
  };
  FArray.sortInPlaceBy = function (f, xs, dir) {
    dir = dir || 1;
    return xs.sort(function (x, y) {
      x = f(x);
      y = f(y);
      return (x < y ? -1 : (x == y ? 0 : 1)) * dir;
    });
  };

  var List = exports.List = function List(head, tail) {
    this.head = head;
    this.tail = tail;
  }
  List.ofArray = function (args, base) {
    var i = args.length - 1, acc = base || new List();
    for (; i >= 0; i--) {
      acc = new List(args[i], acc);
    }
    return acc;
  };
  Object.defineProperty(List.prototype, 'length', {
    get: function () {
      return Seq.fold(function (acc, x) {
        return acc + 1;
      }, 0, this);
    }
  });
  List.prototype[Symbol.iterator] = function () {
    var cur = this;
    return {
      next: function () {
        var tmp = cur;
        cur = cur.tail;
        return {
          done: tmp.tail == null,
          value: tmp.head
        }
      }
    }
  };
  List.append = List.prototype.append = function (xs, ys) {
    if (ys == null) {
      ys = xs, xs = this;
    }
    return Seq.fold(function (acc, x) {
      return new List(x, acc);
    }, ys, List.rev(xs));
  };
  List.choose = List.prototype.choose = function (f, xs) {
    return List.rev(Seq.fold(function (acc, x) {
      var y = f(x);
      return y != null ? new List(y, acc) : acc;
    }, new List(), xs || this));
  };
  List.collect = List.prototype.collect = function (f, xs) {
    return Seq.fold(function (acc, x) {
      return f(x).append(acc);
    }, new List(), List.rev(xs || this));
  };
  List.concat = List.prototype.concat = function (xs) {
    return List.collect(function (x) {
      return x;
    }, xs || this);
  };
  List.filter = List.prototype.filter =
  List.where = List.prototype.where = function (f, xs) {
    return List.rev(Seq.fold(function (acc, x) {
      return f(x) ? new List(x, acc) : acc;
    }, new List(), xs || this));
  };
  List.init = function (n, f) {
    var xs = new List();
    for (var i = 1; i <= n; i++) {
      xs = new List(f(n - i), xs);
    }
    return xs;
  };
  List.map = List.prototype.map = function (f, xs) {
    return List.rev(Seq.fold(function (acc, x) {
      return new List(f(x), acc);
    }, new List(), xs || this));
  };
  List.mapi = List.prototype.mapi = function (f, xs) {
    return List.rev(Seq.fold(function (acc, x, i) {
      return new List(f(i, x), acc);
    }, new List(), xs || this));
  };
  List.partition = List.prototype.partition = function (f, xs) {
    return Seq.fold(function (acc, x) {
      var lacc = acc[0], racc = acc[1];
      return f(x) ? [new List(x, lacc), racc] : [lacc, new List(x, racc)];
    }, [new List(), new List()], xs || this);
  };
  List.replicate = function (n, x) {
    return List.init(n, function () { return x });
  };
  List.rev = List.prototype.rev = function (xs) {
    return Seq.fold(function (acc, x) {
      return new List(x, acc);
    }, new List(), xs || this);
  };
  List.singleton = function (x) {
    return new List(x, new List());
  };
  List.slice = List.prototype.slice = function (lower, upper, xs) {
    var noLower = lower == null, noUpper = upper == null;
    return List.rev(Seq.fold(function (acc, x, i) {
      var within = (noLower || lower <= i) && (noUpper || i <= upper);
      return within ? new List(x, acc) : acc;
    }, new List(), xs || this));
  };
  List.unzip = List.prototype.unzip = function (xs) {
    return Seq.foldBack(function (xy, acc) {
      return [new List(xy[0], acc[0]), new List(xy[1], acc[1])];
    }, xs || this, [new List(), new List()]);
  };
  List.unzip3 = List.prototype.unzip3 = function (xs) {
    return Seq.foldBack(function (xyz, acc) {
      return [new List(xyz[0], acc[0]), new List(xyz[1], acc[1]), new List(xyz[2], acc[2])];
    }, xs || this, [new List(), new List(), new List()]);
  };

  var Seq = exports.Seq = {};
  Seq.toList = function (xs) {
    return Seq.foldBack(function (x, acc) {
      return new List(x, acc);
    }, xs, new List());
  };
  Seq.ofList = function (xs) {
    return Seq.unfold(function (x) {
      return x.tail != null ? [x.head, x.tail] : null;
    }, xs);
  };
  Seq.toArray = function (xs) {
    return Array.from(xs);
    // return Seq.fold(function (acc, x) {
    //   acc.push(x);
    //   return acc;
    // }, [], xs);
  };
  Seq.ofArray = function (xs) {
    return Seq.unfold(function (i) {
      return i < xs.length ? [xs[i], i + 1] : null;
    }, 0);
  };
  Seq.append = function (xs, ys) {
    return Seq.delay(function () {
      var firstDone = false;
      var iters = [xs[Symbol.iterator](), ys];
      return Seq.unfold(function () {
        var cur;
        if (!firstDone) {
          cur = iters[0].next();
          if (!cur.done) {
            return [cur.value, iters];
          }
          else {
            firstDone = true;
            iters = [null, iters[1][Symbol.iterator]()];
          }
        }
        cur = iters[1].next();
        return !cur.done ? [cur.value, iters] : null;
      }, iters);
    });
  };
  Seq.average = function (xs) {
    var count = 1;
    var sum = Seq.reduce(function (acc, x) {
      count++;
      return acc + x;
    }, xs);
    return sum / count;
  };
  Seq.averageBy = function (f, xs) {
    var count = 1;
    var sum = Seq.reduce(function (acc, x) {
      count++;
      return (count === 2 ? f(acc) : acc) + f(x);
    }, xs);
    return sum / count;
  };
  Seq.countBy = function (f, xs) {
    return Seq.map(function (k, vs) {
      return [k, Seq.length(vs)];
    }, Seq.groupBy(f, xs));
  };
  Seq.concat = function (xs) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (innerIter) {
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
  Seq.collect = function (f, xs) {
    return Seq.concat(Seq.map(f, xs));
  };
  Seq.choose = function (f, xs) {
    var trySkipToNext = function (iter) {
      var cur = iter.next();
      if (!cur.done) {
        var y = f(cur.value)
        return y != null ? [y, iter] : trySkipToNext(iter);
      }
    };
    return Seq.delay(function () {
      return Seq.unfold(function (iter) {
        return trySkipToNext(iter);
      }, xs[Symbol.iterator]());
    });
  };
  Seq.compareWith = function (f, xs, ys) {
    var nonZero = Seq.tryFind(function (i) {
      return i != 0;
    }, Seq.map2(function (x, y) {
      return f(x, y);
    }, xs, ys));
    return nonZero != null
      ? nonZero : Seq.length(xs) - Seq.length(ys);
  };
  Seq.delay = function (f) {
    return {
      [Symbol.iterator]: function () {
        return f()[Symbol.iterator]();
      }
    };
  };
  Seq.distinctBy = function (f, xs) {
    return Seq.choose(function (tup) {
      return tup[0];
    }, Seq.scan(function (tup, x) {
      var acc = tup[1];
      var y = f(x);
      return acc.has(y)
        ? [null, acc] : [x, acc.add(y)];
    }, [null, new Set()], xs));
  };
  Seq.distinct = function (xs) {
    return Seq.distinctBy(function (x) { return x }, xs);
  };
  Seq.empty = function () {
    return Seq.unfold(function () { });
  };
  Seq.enumerateWhile = function (cond, xs) {
    return Seq.concat(Seq.unfold(function () {
      return (cond()) ? [xs, true] : null;
    }), true);
  };
  Seq.enumerateThenFinally = function (xs, finalFn) {
    return Seq.delay(function () {
      var iter;
      try {
        iter = xs[Symbol.iterator]();
      }
      finally {
        finalFn();
      }
      return Seq.unfold(function (iter) {
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
  Seq.enumerateUsing = function (disp, work) {
    var isDisposed = false;
    var disposeOnce = function () {
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
  Seq.exactlyOne = function (xs) {
    var iter = xs[Symbol.iterator]();
    var fst = iter.next();
    if (fst.done) {
      throw "Seq was empty";
    }
    var snd = iter.next();
    if (!snd.done) {
      throw "Seq had multiple items";
    }
    return fst.value;
  };
  Seq.exists = function (f, xs) {
    var aux = function (iter) {
      var cur = iter.next();
      return !cur.done && (f(cur.value) || aux(iter));
    };
    return aux(xs[Symbol.iterator]());
  };
  Seq.exists2 = function (f, xs, ys) {
    var aux = function (iter1, iter2) {
      var cur1 = iter1.next(), cur2 = iter2.next();
      return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
    };
    return aux(xs[Symbol.iterator](), ys[Symbol.iterator]());
  };
  Seq.filter = Seq.where = function (f, xs) {
    var trySkipToNext = function (iter) {
      var cur = iter.next();
      if (!cur.done) {
        return f(cur.value)
          ? [cur.value, iter]
          : trySkipToNext(iter);
      }
    };
    return Seq.delay(function () {
      return Seq.unfold(trySkipToNext, xs[Symbol.iterator]());
    });
  };
  Seq.fold = function (f, acc, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return xs.reduce(f, acc);
    }
    else {
      for (var i = 0, cur = null, iter = xs[Symbol.iterator](); ; i++) {
        cur = iter.next();
        if (cur.done) { break; }
        acc = f(acc, cur.value, i);
      }
      return acc;
    }
  };
  Seq.foldBack = function (f, xs, acc) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs)
      ? xs : Seq.toArray(xs);
    for (var i = ar.length - 1; i >= 0; i--) {
      acc = f(ar[i], acc, i);
    }
    return acc;
  };
  Seq.fold2 = function (f, acc, xs, ys) {
    var iter1 = xs[Symbol.iterator](),
      iter2 = ys[Symbol.iterator]();
    for (var i = 0, cur1, cur2; ; i++) {
      cur1 = iter1.next();
      cur2 = iter2.next();
      if (cur1.done || cur2.done) { break; }
      acc = f(acc, cur1.value, cur2.value, i);
    }
    return acc;
  };
  Seq.foldBack2 = function (f, xs, ys, acc) {
    var ar1 = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Seq.toArray(xs);
    var ar2 = Array.isArray(ys) || ArrayBuffer.isView(ys) ? ys : Seq.toArray(ys);
    for (var i = ar1.length - 1; i >= 0; i--) {
      acc = f(ar1[i], ar2[i], acc, i);
    }
    return acc;
  };
  Seq.forall = function (f, xs) {
    return Seq.fold(function (acc, x) {
      return acc && f(x);
    }, true, xs);
  };
  Seq.forall2 = function (f, xs, ys) {
    return Seq.fold2(function (acc, x, y) {
      return acc && f(x, y);
    }, true, xs, ys);
  };
  Seq.groupBy = function (f, xs) {
    return Seq.fold(function (acc, x) {
      var k = f(x), vs = acc.get(k);
      return vs != null
        ? Map.set(k, new List(x, vs))
        : acc.set(k, new List(x, new List()));
    }, new Map(), xs);
  };
  Seq.head = function (xs) {
    var iter = xs[Symbol.iterator]();
    var cur = iter.next();
    if (cur.done) { throw "Seq was empty"; }
    return cur.value;
  };
  Seq.init = function (n, f) {
    return Seq.unfold(function (i) {
      return i < n ? [f(i), i + 1] : null;
    }, 0);
  };
  Seq.initInfinite = function (f) {
    return Seq.delay(function () {
      return Seq.unfold(function (i) {
        return [f(i), i + 1];
      }, 0);
    });
  };
  Seq.item = function (i, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return xs[i];
    }
    else {
      for (var j = 0, iter = xs[Symbol.iterator](); ; j++) {
        var cur = iter.next();
        if (cur.done) { break; }
        if (j === i) { return cur.value; }
      }
      throw "Seq has an insufficient number of elements";
    }
  };
  Seq.iter = function (f, xs) {
    Seq.fold(function (_, x) {
      f(x);
    }, null, xs);
  };
  Seq.iter2 = function (f, xs, ys) {
    Seq.fold2(function (_, x, y) {
      f(x, y);
    }, null, xs, ys);
  };
  Seq.iteri = function (f, xs) {
    Seq.fold(function (_, x, i) {
      f(i, x);
    }, null, xs);
  };
  Seq.iteri2 = function (f, xs, ys) {
    Seq.fold2(function (_, x, y, i) {
      f(i, x, y);
    }, null, xs, ys);
  };
  Seq.isEmpty = function (xs) {
    var i = xs[Symbol.iterator]();
    return i.next().done;
  };
  Seq.last = function (xs) {
    return Seq.reduce(function (_, x) {
      return x;
    }, xs);
  };
  Seq.length = function (xs) {
    return Array.isArray(xs) || ArrayBuffer.isView(xs)
      ? xs.length : Seq.fold(function (acc, x) {
        return acc + 1;
      }, 0, xs);
  };
  Seq.map = function (f, xs) {
    return Seq.delay(function () {
      return Seq.unfold(function (iter) {
        var cur = iter.next();
        return !cur.done ? [f(cur.value), iter] : null;
      }, xs[Symbol.iterator]())
    });
  };
  Seq.mapi = function (f, xs) {
    return Seq.delay(function () {
      var i = 0;
      return Seq.unfold(function (iter) {
        var cur = iter.next();
        return !cur.done ? [f(i++, cur.value), iter] : null;
      }, xs[Symbol.iterator]())
    });
  };
  Seq.map2 = function (f, xs, ys) {
    return Seq.delay(function () {
      var iter1 = xs[Symbol.iterator]();
      var iter2 = ys[Symbol.iterator]();
      return Seq.unfold(function () {
        var cur1 = iter1.next(), cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
      })
    });
  };
  Seq.mapi2 = function (f, xs, ys) {
    return Seq.delay(function () {
      var i = 0;
      var iter1 = xs[Symbol.iterator]();
      var iter2 = ys[Symbol.iterator]();
      return Seq.unfold(function () {
        var cur1 = iter1.next(), cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
      })
    });
  };
  Seq.map3 = function (f, xs, ys, zs) {
    return Seq.delay(function () {
      var iter1 = xs[Symbol.iterator]();
      var iter2 = ys[Symbol.iterator]();
      var iter3 = zs[Symbol.iterator]();
      return Seq.unfold(function () {
        var cur1 = iter1.next(), cur2 = iter2.next(), cur3 = iter3.next();
        return !cur1.done && !cur2.done && !cur3.done
          ? [f(cur1.value, cur2.value, cur3.value), null] : null;
      })
    });
  };
  Seq.max = function (xs) {
    return Seq.reduce(function (acc, x) {
      return Math.max(acc, x);
    }, xs);
  };
  Seq.maxBy = function (f, xs) {
    return Seq.reduce(function (x, y) {
      return f(y) > f(x) ? y : x;
    }, xs);
  };
  Seq.min = function (xs) {
    return Seq.reduce(function (acc, x) {
      return Math.min(acc, x);
    }, xs);
  };
  Seq.minBy = function (f, xs) {
    return Seq.reduce(function (x, y) {
      return f(y) > f(x) ? x : y;
    }, xs);
  };
  Seq.pairwise = function (xs) {
    return Seq.skip(1, Seq.Scan(function (last, next) {
      return [last[1], next];
    }, [0, 0], xs));
  };
  Seq.permute = function (f, xs) {
    var ar = Seq.toArray(xs);
    return Seq.ofArray(FArray.permute(f, ar));
  };
  Seq.rangeStep = function (first, step, last) {
    if (step === 0) {
      throw "Step cannot be 0";
    }
    return Seq.unfold(function (x) {
      return (step > 0 && x <= last) || (step < 0 && x >= last)
        ? [x, x + step] : null;
    }, first);
  };
  Seq.range = function (first, last) {
    return Seq.rangeStep(first, 1, last);
  };
  Seq.readonly = function (xs) {
    return Seq.map(function (x) { return x }, xs);
  };
  Seq.reduce = function (f, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return xs.reduce(f);
    }
    else {
      var iter = xs[Symbol.iterator]();
      var cur = iter.next();
      if (cur.done) { throw "Seq was empty"; }
      var acc = cur.value;
      for (; ;) {
        cur = iter.next();
        if (cur.done) { break; }
        acc = f(acc, cur.value);
      }
      return acc;
    }
  };
  Seq.reduceBack = function (f, xs) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs)
      ? xs : Seq.toArray(xs);
    if (ar.length === 0) { throw "Seq was empty"; }
    var acc = ar[ar.length - 1];
    for (var i = ar.length - 2; i >= 0; i--) {
      acc = f(ar[i], acc, i);
    }
    return acc;
  };
  Seq.replicate = function (n, x) {
    return Seq.init(n, function () { return x });
  };
  Seq.rev = function (xs) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs)
      ? xs.slice(0) : Seq.toArray(xs);
    return ar.reverse();
  };
  Seq.scan = function (f, seed, xs) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (acc) {
        if (acc == null) {
          return [seed, seed];
        }
        else {
          var cur = iter.next();
          if (!cur.done) {
            acc = f(acc, cur.value);
            return [acc, acc];
          }
        }
      }, null);
    });
  };
  Seq.scanBack = function (f, xs, seed) {
    return Seq.rev(Seq.scan(function (acc, x) {
      return f(x, acc);
    }, seed, Seq.rev(xs)));
  };
  Seq.singleton = function (x) {
    return Seq.unfold(function (x) {
      return x != null ? [x, null] : null;
    }, x);
  };
  Seq.skip = function (n, xs) {
    return {
      [Symbol.iterator]: function () {
        var iter = xs[Symbol.iterator]();
        for (var i = 1; i <= n; i++) {
          iter.next();
        }
        return iter;
      }
    };
  };
  Seq.skipWhile = function (f, xs) {
    return Seq.delay(function () {
      var hasPassed = false;
      return Seq.filter(function (x) {
        return hasPassed || (hasPassed = !f(x));
      }, xs);
    });
  };
  Seq.sort = function (xs) {
    var ys = Seq.toArray(xs);
    return Seq.ofArray(ys.sort());
  };
  Seq.sortWith = function (f, xs) {
    var ys = Seq.toArray(xs);
    return Seq.ofArray(ys.sort(f));
  };
  Seq.sortBy = function (f, xs) {
    var ys = Seq.toArray(xs);
    return Seq.ofArray(FArray.sortInPlaceBy(f, ys));
  };
  Seq.sortDescending = function (f, xs) {
    var ys = Seq.toArray(xs);
    return Seq.ofArray(ys.sort(function (x, y) {
      return y < x ? -1 : (x == y ? 0 : 1);
    }));
  };
  Seq.sortByDescending = function (f, xs) {
    var ys = Seq.toArray(xs);
    return Seq.ofArray(FArray.sortInPlaceBy(f, ys, -1));
  };
  Seq.sum = function (xs, add) {
    add = add || function (x, y) { return x + y };
    return Seq.reduce(function (acc, x) {
      return add(acc, x);
    }, xs);
  };
  Seq.sumBy = function (f, xs, add) {
    var fst = true;
    add = add || function (x, y) { return x + y };
    return Seq.reduce(function (acc, x) {
      acc = fst ? f(acc) : acc, fst = false;
      return acc + f(x);
    }, xs);
  };
  Seq.tail = function (xs) {
    var iter = xs[Symbol.iterator]();
    var cur = iter.next();
    if (cur.done) { throw "Seq was empty"; }
    return {
      [Symbol.iterator]: function () {
        return iter;
      }
    };
  };
  Seq.take = Seq.truncate = function (n, xs) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (i) {
        if (i < n) {
          var cur = iter.next();
          return [cur.value, i + 1];
        }
      }, 0);
    });
  };
  Seq.takeWhile = function (f, xs) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (i) {
        var cur = iter.next();
        if (!cur.done && f(cur.value)) {
          return [cur.value, null];
        }
      }, 0);
    });
  };
  Seq.tryFind = function (f, xs) {
    for (var i = 0, iter = xs[Symbol.iterator](), cur; ; i++) {
      cur = iter.next();
      if (cur.done) { break; }
      if (f(cur.value, i)) {
        return cur.value;
      }
    }
  };
  Seq.find = function (f, xs) {
    var res = Seq.tryFind(f, xs);
    if (res == null) {
      throw "Seq did not contain any matching elements";
    }
    return res;
  };
  Seq.tryFindIndex = function (f, xs) {
    for (var i = 0, iter = xs[Symbol.iterator](), cur; ; i++) {
      cur = iter.next();
      if (cur.done) { break; }
      if (f(cur.value, i)) {
        return i;
      }
    }
  };
  Seq.findIndex = function (f, xs) {
    var res = Seq.tryFindIndex(f, xs);
    if (res == null) {
      throw "Seq did not contain any matching elements";
    }
    return res;
  };
  Seq.tryPick = function (f, xs) {
    for (var i = 0, iter = xs[Symbol.iterator](), cur; ; i++) {
      cur = iter.next();
      if (cur.done) { break; }
      var y = f(cur.value, i);
      if (y != null) {
        return y;
      }
    }
  };
  Seq.pick = function (f, xs) {
    var res = Seq.tryPick(f, xs);
    if (res == null) {
      throw "Seq did not contain any matching elements";
    }
    return res;
  };
  Seq.unfold = function (f, acc) {
    return {
      [Symbol.iterator]: function () {
        return {
          next: function () {
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
      }
    };
  };
  Seq.zip = function (xs, ys) {
    return Seq.map2(function (x, y) {
      return [x, y];
    }, xs, ys);
  };
  Seq.zip3 = function (xs, ys, zs) {
    return Seq.map3(function (x, y, z) {
      return [x, y, z];
    }, xs, ys, zs);
  };

  var FSet = exports.Set = {};
  FSet.ofArray = function (xs) {
    var set = new Set();
    for (var i = 0; i < xs.length; i++) {
      set.set(xs[i]);
    }
    return set;
  };
  FSet.ofSeq = function (xs) {
    return Seq.fold(function (acc, x) {
      acc.set(x);
    }, new Set(), xs);
  };
  FSet.partition = function (f, xs) {
    return Seq.fold(function (acc, x) {
      var lacc = acc[0], racc = acc[1];
      return f(x) ? [lacc.set(x), racc] : [lacc, racc.set(x)];
    }, [new Set(), new Set()], xs);
  };

  var FMap = exports.Map = {};
  FMap.ofArray = function (xs) {
    var map = new Map();
    for (var i = 0; i < xs.length; i++) {
      map.set(xs[i][0], xs[i][1]);
    }
    return map;
  };
  FSet.ofSeq = function (xs) {
    return Seq.fold(function (acc, kv) {
      acc.set(kv[0], kv[1]);
    }, new Map(), xs);
  };
  FMap.partition = function (f, map) {
    return Seq.fold(function (acc, kv) {
      var lacc = acc[0], racc = acc[1], k = kv[0], v = kv[1];
      return f(k, v) ? [lacc.set(k, v), racc] : [lacc, racc.set(k, v)];
    }, [new Map(), new Map()], map);
  };
  FMap.findKey = function (f, map) {
    return Seq.pick(function (kv) {
      return f(kv[0], kv[1]) ? kv[0] : null;
    }, map);
  };
  FMap.tryFindKey = function (f, map) {
    return Seq.tryPick(function (kv) {
      return f(kv[0], kv[1]) ? kv[0] : null;
    }, map);
  };
  FMap.pick = function (f, map) {
    return Seq.pick(function (kv) {
      var res = f(kv[0], kv[1]);
      return res != null ? res : null;
    }, map);
  };
  FMap.tryPick = function (f, map) {
    return Seq.tryPick(function (kv) {
      var res = f(kv[0], kv[1]);
      return res != null ? res : null;
    }, map);
  };

  var Async = exports.Async = {};
  function protectedCont(f) {
    return function (ctx) {
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
  Async.bind = function (work, cont) {
    return protectedCont(function (ctx) {
      work({
        onSuccess: function (x) {
          return cont(x)(ctx);
        },
        onError: ctx.onError,
        cancelToken: ctx.cancelToken
      })
    });
  };
  Async.combine = function (work1, work2) {
    return Async.bind(work1, function () {
      return work2;
    });
  };
  Async.delay = function (cont) {
    return protectedCont(function (ctx) {
      cont()(ctx);
    });
  };
  Async.for = function (seq, body) {
    var iter = seq[Symbol.iterator](),
      cur = iter.next();
    return Async.while(function () {
      return !cur.done;
    },
      Async.delay(function () {
        var res = body(cur.value);
        cur = iter.next();
        return res;
      })
      );
  };
  Async.return = function (x) {
    return protectedCont(function (ctx) {
      ctx.onSuccess(x);
    });
  };
  Async.returnFrom = function (work) {
    return work;
  };
  Async.tryFinally = function (work, finalFn) {
    return protectedCont(function (ctx) {
      work({
        onSuccess: function (x) {
          finalFn();
          ctx.onSuccess(x);
        },
        onError: function (x) {
          finalFn();
          ctx.onError(x);
        },
        cancelToken: ctx.cancelToken
      });
    });
  };
  Async.tryWith = function (work, catchFn) {
    return protectedCont(function (ctx) {
      work({
        onSuccess: ctx.onSuccess,
        cancelToken: ctx.cancelToken,
        onError: function (ex) {
          ctx.onSuccess(catchFn(ex));
        }
      })
    });
  };
  Async.using = function (disp, cont) {
    return Async.tryFinally(cont(disp), function () {
      disp.dispose();
    });
  };
  Async.while = function (cond, body) {
    if (cond()) {
      return Async.bind(body, function () {
        return Async.while(cond, body);
      });
    }
    else {
      return Async.return();
    }
  };
  Async.zero = function () {
    return protectedCont(function (ctx) {
      ctx.onSuccess();
    });
  };
  Async.startImmediate = function (work, cancelToken) {
    work({
      onSuccess: function () { },
      onError: function () { },
      cancelToken: cancelToken
    });
  };
  Async.start = Async.startImmediate;
});
