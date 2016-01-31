// Union constructor template
// class MyUnion {
//   constructor(tag, ...items) {
//   this.Tag = tag;
//   this.Item = items.length > 1 ? items : items[0];
//   }
// }
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

  F.List = function List(head, tail) {
    this.head = head;
    this.tail = tail;
  }
  F.List.prototype[Symbol.iterator] = function() {
    var head = this.head;
    return {
      next: function() {
        var cur = head;
        head = cur.tail;
        return { done: cur == null, value: cur }
      }
    }
  }
  F.Async = (function(){
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

    var Async = {
      bind: function(work, cont) {
        return protectedCont(function(ctx) {
          work ({
            onSuccess: function(x) {
              return cont(x)(ctx);
            },
            onError: ctx.onError,
            cancelToken: ctx.cancelToken  
          })
        });
      },
      combine: function(work1, work2) {
        return F.Async.bind(work1, function() {
          return work2;
        });
      },
      delay: function(cont) {
        return protectedCont(function(ctx) {
          cont()(ctx);
        });
      },
      for: function(seq, body) {
        var iter = seq[Symbol.iterator](),
            cur = iter.next();
        return F.Async.while(function() {
            return !cur.done;
          },
          F.Async.delay(function() {
            var res = body(cur.value);
            cur = iter.next();
            return res;
          })
        );
      },
      return: function(x) {
        return protectedCont(function(ctx) {
          ctx.onSuccess(x);
        });
      },
      returnFrom: function(work) {
        return work;
      },
      tryFinally: function(work, finalFn) {
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
      },
      tryWith: function(work, catchFn) {
        return protectedCont(function(ctx) {
          work ({
            onSuccess: ctx.onSuccess,
            cancelToken: ctx.cancelToken,  
            onError: function(ex) {
              ctx.onSuccess(catchFn(ex));
            }
          })
        });
      },
      using: function(disp, cont) {
        return F.Async.tryFinally(cont(disp), function() {
          disp.dispose();
        });
      },
      while: function(cond, body) {
        if (cond()) {
          return F.Async.bind(body, function() {
            return F.Async.while(cond, body);
          });
        }
        else {
          return F.Async.return();
        }
      },
      zero: function() {
        return protectedCont(function(ctx) {
          ctx.onSuccess();
        });
      }
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