//
// TODO: enable "noImplicitReturns"
// TODO: Improve types / remove "any"s
// TODO: Replace "= function ()" with member functions. -- Keeping them for now to minimize differences when comparing to fable-core.js
// TODO: One class per file.
//

const FSymbol = {
  interfaces: Symbol("interfaces"),
  typeName: Symbol("typeName")
};
export { FSymbol as Symbol };

export class Choice {
  public Case: any;
  public Fields: any;

  constructor(t: any, d: any) {
    this.Case = t;
    this.Fields = [d];
  }
}

export class Util {
  private static __types = new Map()

  // For legacy reasons the name is kept, but this method also adds
  // the type name to a cache. Use it after declaration:
  // Util.setInterfaces(Foo.prototype, ["IFoo", "IBar"], "MyModule.Foo");
  public static setInterfaces = function (proto: any, interfaces: any, typeName?: any) {
    var curInfcs = proto[FSymbol.interfaces];
    if (Array.isArray(interfaces) && interfaces.length > 0) {
      if (Array.isArray(curInfcs)) {
        for (var i = 0; i < interfaces.length; i++) {
          curInfcs.push(interfaces[i]);
        }
      } else {
        proto[FSymbol.interfaces] = interfaces;
      }
    }
    if (typeName) {
      proto[FSymbol.typeName] = typeName;
      Util.__types.set(typeName, proto.constructor);
    }
  };
  static hasInterface = function (obj: any, infc: any) {
    return Array.isArray(obj[FSymbol.interfaces]) && obj[FSymbol.interfaces].indexOf(infc) >= 0;
  };
  static getRestParams = function (args: any, idx: any) {
    for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++) {
      restArgs[_key - idx] = args[_key];
    }
    return restArgs;
  };
  static compareTo = function (x: any, y: any) {
    function isCollectionComparable(o: any) {
      return Array.isArray(o) || ArrayBuffer.isView(o) || o instanceof List || o instanceof Map || o instanceof Set;
    }
    function sortIfMapOrSet(o: any) {
      return o instanceof Map || o instanceof Set ? Array.from(o).sort() : o;
    }
    if (typeof x != typeof y) {
      return -1;
    }
    if (x != null && y != null && typeof x == "object" && typeof y == "object") {
      var lengthComp: any;
      if (Object.getPrototypeOf(x) != Object.getPrototypeOf(y)) {
        return -1;
      }
      if (Util.hasInterface(x, "System.IComparable")) {
        return x.compareTo(y);
      }
      if (isCollectionComparable(x)) {
        lengthComp = Util.compareTo(Seq.count(x), Seq.count(y));
        return lengthComp != 0 ? lengthComp : Seq.fold2(function (prev: any, v1: any, v2: any) {
          return prev != 0 ? prev : Util.compareTo(v1, v2);
        }, 0, sortIfMapOrSet(x), sortIfMapOrSet(y));
      }
      if (x instanceof Date) {
        return x < y ? -1 : x > y ? 1 : 0;
      }
      var keys1 = Object.getOwnPropertyNames(x),
        keys2 = Object.getOwnPropertyNames(y);
      lengthComp = Util.compareTo(keys1.length, keys2.length);
      return lengthComp != 0 ? lengthComp : Seq.fold2(function (prev: any, k1: any, k2: any) {
        return prev != 0 ? prev : Util.compareTo(x[k1], y[k2]);
      }, 0, keys1.sort(), keys2.sort());
    }
    return x < y ? -1 : x > y ? 1 : 0;
  };
  static createObj = function (fields: any) {
    return Seq.fold(function (acc: any, kv: any) {
      acc[kv[0]] = kv[1];
      return acc;
    }, {}, fields);
  };
  static toJson = function (o: any) {
    function replacer(k: any, v: any) {
      if (ArrayBuffer.isView(v)) {
        return Array.from(v);
      }
      if (typeof v == "object") {
        if (v instanceof List || v instanceof Map || v instanceof Set) {
          throw "JSON serialization of List, Map or Set is not supported";
        }
        if (v[FSymbol.typeName]) {
          var o2 = { __type: v[FSymbol.typeName] };
          return Object.assign(o2, v);
        }
      }
      return v;
    }
    return JSON.stringify(o, replacer);
  };
  static ofJson = function (json: any) {
    function reviver(k: any, v: any) {
      if (typeof v == "object" && v.__type) {
        var T = Util.__types.get(v.__type);
        if (T) {
          delete v.__type;
          return Object.assign(new T(), v);
        }
      }
      return v;
    }
    return JSON.parse(json, reviver);
  };
}

export class TimeSpan extends Number {
  static create(d: number = 0, h: number = 0, m: number = 0, s: number = 0, ms: number = 0): number {
    switch (arguments.length) {
      case 1:
        // ticks
        return this.fromTicks(arguments[0]);
      case 3:
        // h,m,s
        d = 0, h = arguments[0], m = arguments[1], s = arguments[2], ms = 0;
        break;
      default:
        // d,h,m,s,ms
        d = arguments[0], h = arguments[1], m = arguments[2], s = arguments[3], ms = arguments[4] || 0;
        break;
    }
    return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
  };

  static fromTicks(ticks: number) {
    return ticks / 10000;
  };

  static fromDays(d: number) {
    return TimeSpan.create(d, 0, 0, 0);
  };

  static fromHours(h: number) {
    return TimeSpan.create(h, 0, 0);
  };

  static fromMinutes(m: number) {
    return TimeSpan.create(0, m, 0);
  };

  static fromSeconds(s: number) {
    return TimeSpan.create(0, 0, s);
  };

  static days(ts: TimeSpan) {
    return Math.floor(<number>ts / 86400000);
  };

  static hours(ts: TimeSpan) {
    return Math.floor(<number>ts % 86400000 / 3600000);
  };

  static minutes(ts: TimeSpan) {
    return Math.floor(<number>ts % 3600000 / 60000);
  };

  static seconds(ts: TimeSpan) {
    return Math.floor(<number>ts % 60000 / 1000);
  };

  static milliseconds(ts: TimeSpan) {
    return Math.floor(<number>ts % 1000);
  };

  static ticks(ts: TimeSpan) {
    return <number>ts * 10000;
  };

  static totalDays(ts: TimeSpan) {
    return <number>ts / 86400000;
  };

  static totalHours(ts: TimeSpan) {
    return <number>ts / 3600000;
  };

  static totalMinutes(ts: TimeSpan) {
    return <number>ts / 60000;
  };

  static totalSeconds(ts: TimeSpan) {
    return <number>ts / 1000;
  };

  static negate(ts: TimeSpan) {
    return <number>ts * -1;
  };

  static add(ts1: TimeSpan, ts2: TimeSpan) {
    return <number>ts1 + <number>ts2;
  };

  static subtract(ts1: TimeSpan, ts2: TimeSpan) {
    return <number>ts1 - <number>ts2;
  };

  static compare = Util.compareTo;
  static compareTo = Util.compareTo;
  static duration = Math.abs;
}

export enum DateKind {
  UTC = 1,
  Local
}
class FDate extends Date {
  public kind: DateKind;

  private static __changeKind(d: Date, kind: DateKind) {
    var d2: Date;
    return (<FDate>d).kind == kind ? d : (d2 = new Date(d.getTime()), (<FDate>d2).kind = kind, d2);
  };

  private static __getValue(d: Date, key: string): number {
    return (<any>d)[((<FDate>d).kind == DateKind.UTC ? 'getUTC' : 'get') + key]();
  };

  static minValue() {
    return FDate.parse(-8640000000000000, 1);
  };

  static maxValue() {
    return FDate.parse(8640000000000000, 1);
  };

  static parse(v?: any, kind?: DateKind): any {
    var date = (v == null) ? new Date() : new Date(v);
    if (isNaN(date.getTime())) {
      throw "The string is not a valid Date.";
    }
    (<FDate>date).kind = kind || DateKind.Local;
    return date;
  };

  static create(year: number, month: number, day: number, h: number = 0, m: number = 0, s: number = 0, ms: number = 0, kind: DateKind = DateKind.Local): Date {
    var date: Date = (kind === DateKind.UTC)
      ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms))
      : new Date(year, month - 1, day, h, m, s, ms);
    if (isNaN(date.getTime())) {
      throw "The parameters describe an unrepresentable Date.";
    }
    (<FDate>date).kind = kind;
    return date;
  };

  static now = FDate.parse;

  static utcNow() {
    return FDate.parse(null, 1);
  };

  static today() {
    return FDate.date(FDate.now());
  };

  static isLeapYear(year: number) {
    return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
  };

  static daysInMonth(year: number, month: number) {
    if (month == 2) {
      return FDate.isLeapYear(year) ? 29 : 28;
    } else {
      return month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
    }
  };

  static toUniversalTime(d: Date) {
    return FDate.__changeKind(d, 1);
  };

  static toLocalTime(d: Date) {
    return FDate.__changeKind(d, 2);
  };

  static timeOfDay(d: Date) {
    return TimeSpan.create(FDate.hour(d), FDate.minute(d), FDate.second(d));
  };

  static date(d: Date) {
    return FDate.create(FDate.year(d), FDate.month(d), FDate.day(d), 0, 0, 0, 0, (<FDate>d).kind);
  };

  static day(d: Date) {
    return FDate.__getValue(d, "Date");
  };

  static hour(d: Date) {
    return FDate.__getValue(d, "Hours");
  };

  static millisecond(d: Date) {
    return FDate.__getValue(d, "Milliseconds");
  };

  static minute(d: Date) {
    return FDate.__getValue(d, "Minutes");
  };

  static month(d: Date) {
    return FDate.__getValue(d, "Month") + 1;
  };

  static second(d: Date) {
    return FDate.__getValue(d, "Seconds");
  };

  static year(d: Date) {
    return FDate.__getValue(d, "FullYear");
  };

  static ticks(d: Date) {
    return (d.getTime() + 6.2135604e+13 /* millisecondsJSOffset */) * 10000;
  };

  static toBinary = FDate.ticks;

  static dayOfWeek(d: Date) {
    return FDate.__getValue(d, "Day");
  };

  static dayOfYear(d: Date) {
    var year = FDate.year(d),
      month = FDate.month(d),
      day = FDate.day(d);
    for (var i = 1; i < month; i++) {
      day += FDate.daysInMonth(year, i);
    }
    return day;
  };

  static add(d: Date, ts: TimeSpan) {
    return FDate.parse(d.getTime() + <number>ts, (<FDate>d).kind);
  };

  static addDays(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 86400000, (<FDate>d).kind);
  };

  static addHours(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 3600000, (<FDate>d).kind);
  };

  static addMinutes(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 60000, (<FDate>d).kind);
  };

  static addSeconds(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 1000, (<FDate>d).kind);
  };

  static addMilliseconds(d: Date, v: number) {
    return FDate.parse(d.getTime() + v, (<FDate>d).kind);
  };

  static addTicks(d: Date, v: number) {
    return FDate.parse(d.getTime() + v / 10000, (<FDate>d).kind);
  };

  static addYears(d: Date, v: number) {
    var newMonth = FDate.month(d),
      newYear = FDate.year(d) + v,
      daysInMonth = FDate.daysInMonth(newYear, newMonth),
      newDay = Math.min(daysInMonth, FDate.day(d));
    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), (<FDate>d).kind);
  };

  static addMonths(d: Date, v: number) {
    var newMonth = FDate.month(d) + v,
      newMonth_ = 0,
      yearOffset = 0;
    if (newMonth > 12) {
      newMonth_ = newMonth % 12;
      yearOffset = Math.floor(newMonth / 12);
      newMonth = newMonth_;
    } else if (newMonth < 1) {
      newMonth_ = 12 + newMonth % 12;
      yearOffset = Math.floor(newMonth / 12) + (newMonth_ == 12 ? -1 : 0);
      newMonth = newMonth_;
    }
    var newYear = FDate.year(d) + yearOffset;
    var daysInMonth = FDate.daysInMonth(newYear, newMonth);
    var newDay = Math.min(daysInMonth, FDate.day(d));
    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), (<FDate>d).kind);
  };

  static subtract(d: Date, that: Date | number) {
    return typeof that == "number"
      ? FDate.parse(d.getTime() - <number>that, (<FDate>d).kind)
      : d.getTime() - (<Date>that).getTime();
  };

  static toLongDateString(d: Date) {
    return d.toDateString();
  };

  static toShortDateString(d: Date) {
    return d.toLocaleDateString();
  };

  static toLongTimeString(d: Date) {
    return d.toLocaleTimeString();
  };

  static toShortTimeString(d: Date) {
    return d.toLocaleTimeString().replace(/:\d\d(?!:)/, '');
  };

  static equals(d1: Date, d2: Date) {
    return d1.getTime() == d2.getTime();
  };

  static compareTo = Util.compareTo;
  static compare = Util.compareTo;

  static op_Addition = FDate.add;
  static op_Subtraction = FDate.subtract
}
export { FDate as Date };

export class Timer {
  public interval: any;
  public autoReset: boolean;
  public _elapsed: any;

  private _enabled: boolean;
  private _isDisposed: boolean;
  private _intervalId: any;
  private _timeoutId: any;

  constructor(interval: any) {
    this.interval = interval > 0 ? interval : 100;
    this.autoReset = true;
    this._elapsed = new Event();
  }

  get elapsed(): number {
    return this._elapsed;
  }

  get enabled(): boolean {
    return this._enabled;
  }

  set enabled(x: boolean) {
    if (!this._isDisposed && this._enabled != x) {
      if (this._enabled = x) {
        if (this.autoReset) {
          var _this = this;
          this._intervalId = setInterval(function () {
            if (!_this.autoReset) {
              _this.enabled = false;
            }
            _this._elapsed.trigger(new Date());
          }, this.interval);
        } else {
          var _this = this;
          this._timeoutId = setTimeout(function () {
            _this.enabled = false;
            _this._timeoutId = 0;
            if (_this.autoReset) {
              _this.enabled = true;
            }
            _this._elapsed.trigger(new Date());
          }, this.interval);
        }
      } else {
        if (this._timeoutId) {
          clearTimeout(this._timeoutId);
          this._timeoutId = 0;
        }
        if (this._intervalId) {
          clearInterval(this._intervalId);
          this._intervalId = 0;
        }
      }
    }
  }
  dispose: any;  // ToDo: Improve this
  close: any;
  start: any;
  stop: any;
}
Timer.prototype.dispose = Timer.prototype.close = function () {
  this.enabled = false;
  this._isDisposed = true;
};
Timer.prototype.start = function () {
  this.enabled = true;
};
Timer.prototype.stop = function () {
  this.enabled = false;
};
Util.setInterfaces(Timer.prototype, ["System.IDisposable"]);

class FString {
  static fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;
  static fsFormat = function (str: any) {
    function isObject(x: any) {
      return x !== null && typeof x === 'object' && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
    };
    function formatOnce(str: any, rep: any) {
      return str.replace(FString.fsFormatRegExp, function (_: any, prefix: any, flags: any, pad: any, precision: any, format: any) {
        switch (format) {
          case "f": case "F":
            rep = rep.toFixed(precision || 6); break;
          case "g": case "G":
            rep = rep.toPrecision(precision); break;
          case "e": case "E":
            rep = rep.toExponential(precision); break;
          case "A":
            rep = (rep instanceof Map ? "map " : rep instanceof Set ? "set " : "") + JSON.stringify(rep, function (k, v) {
              return v && v[Symbol.iterator] && !Array.isArray(v) && isObject(v) ? Array.from(v) : v;
            });
            break;
        }
        var plusPrefix = flags.indexOf('+') >= 0 && parseInt(rep) >= 0;
        if (!isNaN(pad = parseInt(pad))) {
          var ch = pad >= 0 && flags.indexOf('0') >= 0 ? '0' : ' ';
          rep = FString.padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
        }
        return prefix + (plusPrefix ? "+" + rep : rep);
      });
    }
    function makeFn(str: any) {
      return function (rep: any) {
        var str2 = formatOnce(str, rep);
        return FString.fsFormatRegExp.test(str2)
          ? makeFn(str2) : _cont(str2.replace(/%%/g, '%'));
      };
    }
    var _cont: any;
    return function (cont: any) {
      _cont = cont;
      return FString.fsFormatRegExp.test(str) ? makeFn(str) : _cont(str);
    };
  };
  static formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;
  static format = function (str: any, args: any) {
    args = Util.getRestParams(arguments, 1);
    return str.replace(FString.formatRegExp, function (match: any, idx: any, pad: any, format: any) {
      var rep = args[idx];
      if (typeof rep === 'number') {
        switch ((format || '').substring(0, 1)) {
          case "f": case "F":
            rep = format.length > 1 ? rep.toFixed(format.substring(1)) : rep.toFixed(2);
            break;
          case "g": case "G":
            rep = format.length > 1 ? rep.toPrecision(format.substring(1)) : rep.toPrecision();
            break;
          case "e": case "E":
            rep = format.length > 1 ? rep.toExponential(format.substring(1)) : rep.toExponential();
            break;
          case "p": case "P":
            rep = (format.length > 1 ? (rep * 100).toFixed(format.substring(1)) : (rep * 100).toFixed(2)) + " %";
            break;
        }
      } else if (rep instanceof Date) {
        if (format.length === 1) {
          switch (format) {
            case "D":
              rep = rep.toDateString(); break;
            case "T":
              rep = rep.toLocaleTimeString(); break;
            case "d":
              rep = rep.toLocaleDateString(); break;
            case "t":
              rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, ''); break;
          }
        }
        rep = format.replace(/\w+/g, function (match2: any) {
          var rep2 = match2;
          switch (match2.substring(0, 1)) {
            case "y":
              rep2 = match2.length < 4 ? FDate.year(rep) % 100 : FDate.year(rep);
              break;
            case "h":
              rep2 = rep.getHours() > 12 ? FDate.hour(rep) % 12 : FDate.hour(rep);
              break;
            case "M":
              rep2 = FDate.month(rep);
              break;
            case "d":
              rep2 = FDate.day(rep);
              break;
            case "H":
              rep2 = FDate.hour(rep);
              break;
            case "m":
              rep2 = FDate.minute(rep);
              break;
            case "s":
              rep2 = FDate.second(rep);
              break;
          }
          if (rep2 !== match2 && rep2 < 10 && match2.length > 1) {
            rep2 = "0" + rep2;
          }
          return rep2;
        });
      }
      if (!isNaN(pad = parseInt((pad || '').substring(1)))) {
        rep = FString.padLeft(rep, Math.abs(pad), ' ', pad < 0);
      }
      return rep;
    });
  };
  static init = function (n: any, f: any) {
    if (n < 0) {
      throw "String length must be non-negative";
    }
    var xs = new Array(n);
    for (var i = 0; i < n; i++) {
      xs[i] = f(i);
    }
    return xs.join("");
  };
  static isNullOrEmpty = function (str: any) {
    return typeof str !== "string" || str.length == 0;
  };
  static isNullOrWhiteSpace = function (str: any) {
    return typeof str !== "string" || /^\s*$/.test(str);
  };
  static padLeft = function (str: any, len: any, ch: any, isRight: any) {
    var i = -1;
    ch = ch || ' ';
    str = String(str);
    len = len - str.length;
    while (++i < len) {
      str = isRight ? str + ch : ch + str;
    }
    return str;
  };
  static padRight = function (str: any, len: any, ch: any) {
    return FString.padLeft(str, len, ch, true);
  };
  static replace = function (str: any, search: any, replace: any) {
    return str.replace(new RegExp(FRegExp.escape(search), "g"), replace);
  };
  static replicate = function (n: any, x: any) {
    return FString.init(n, function () {
      return x;
    });
  };
  static split = function (str: any, splitters: any, count: any, removeEmpty: any) {
    count = typeof count == "number" ? count : null;
    removeEmpty = typeof removeEmpty == "number" ? removeEmpty : null;
    if (count < 0) { throw "Count cannot be less than zero"; }
    if (count === 0) { return []; }
    splitters = Array.isArray(splitters) ? splitters : Util.getRestParams(arguments, 1);
    splitters = splitters.filter(function (x: any) { return x }).map(function (x: any) { return FRegExp.escape(x) });
    splitters = splitters.length > 0 ? splitters : [" "];
    var m: any, i = 0, splits: any = [], reg = new RegExp(splitters.join("|"), "g");
    while ((count == null || count > 1) && (m = reg.exec(str)) !== null) {
      if (!removeEmpty || (m.index - i) > 0) {
        count = count != null ? count - 1 : count;
        splits.push(str.substring(i, m.index));
      }
      i = reg.lastIndex;
    }
    if (!removeEmpty || (str.length - i) > 0)
      splits.push(str.substring(i));
    return splits;
  };
  static join = function (delimiter: any, xs: any) {
    xs = typeof xs == "string" ? Util.getRestParams(arguments, 1) : xs;
    return (Array.isArray(xs) ? xs : Array.from(xs)).join(delimiter);
  };
  static concat = FString.join;
  static endsWith = function (str: any, search: any) {
    var idx = str.lastIndexOf(search);
    return idx >= 0 && idx == str.length - search.length;
  };
  static newGuid = function newGuid() {
    var i: any, random: any, uuid = '';
    for (i = 0; i < 32; i++) {
      random = Math.random() * 16 | 0;
      if (i === 8 || i === 12 || i === 16 || i === 20) {
        uuid += '-';
      }
      uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
    }
    return uuid;
  };
}
export { FString as String };

class FRegExp {
  static create = function (pattern: any, options: any) {
    var flags = "g";
    flags += options & 1 ? "i" : "";
    flags += options & 2 ? "m" : "";
    return new RegExp(pattern, flags);
  };
  // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
  static escape = function (str: any) {
    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&');
  };
  static unescape = function (str: any) {
    return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, '$1');
  };
  static isMatch = function (str: any, pattern: any, options: any) {
    var reg: any = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
    return reg.test(str);
  };
  static match = function (str: any, pattern: any, options: any) {
    var reg: any = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
    return reg.exec(str);
  };
  static matches = function (str: any, pattern: any, options?: any) {
    var reg: any = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
    if (!reg.global) {
      throw "Non-global RegExp"; // Prevent infinite loop
    }
    var m: any,
      matches: any = [];
    while ((m = reg.exec(str)) !== null) {
      matches.push(m);
    }
    return matches;
  };
  static options = function (reg: any) {
    var options = 256; // ECMAScript
    options |= reg.ignoreCase ? 1 : 0;
    options |= reg.multiline ? 2 : 0;
    return options;
  };
  static replace = function (reg: any, input: any, replacement: any, limit: any, offset: any) {
    if (typeof reg == "string") {
      var tmp = reg;
      reg = FRegExp.create(input, limit);
      input = tmp, limit = undefined;
    }
    if (typeof replacement == "function") {
      limit = limit == null ? -1 : limit;
      offset = offset == null ? 0 : offset;
      var replacer = function () {
        var res = arguments[0];
        if (limit !== 0) {
          limit--;
          var match: any = [];
          var len = arguments.length;
          for (var i = 0; i < len - 2; i++) {
            match.push(arguments[i]);
          }
          match.index = arguments[len - 2];
          match.input = arguments[len - 1];
          res = replacement(match);
        }
        return res;
      };
      return input.substring(0, offset) + input.substring(offset).replace(reg, replacer);
    } else {
      if (limit != null) {
        var m: any;
        offset = offset == null ? 0 : offset;
        var sub1 = input.substring(offset);
        var matches = FRegExp.matches(reg, sub1);
        var sub2 = matches.length > limit ? (m = matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
        return input.substring(0, offset) + sub2.replace(reg, replacement) + input.substring(offset + sub2.length);
      } else {
        return input.replace(reg, replacement);
      }
    }
  };
  static split = function (reg: any, input: any, limit: any, offset: any) {
    if (typeof reg == "string") {
      var tmp = reg;
      reg = FRegExp.create(input, limit);
      input = tmp, limit = undefined;
    }
    input = offset != null ? input.substring(offset) : input;
    return input.split(reg, limit);
  };
}
export { FRegExp as RegExp };

class FArray {
  static addRangeInPlace = function (range: any, xs: any) {
    Seq.iter(function (x: any) {
      xs.push(x);
    }, range);
  };
  static blit = function (source: any, sourceIndex: any, target: any, targetIndex: any, count: any) {
    while (count--) {
      target[targetIndex++] = source[sourceIndex++];
    }
  };
  static partition = function (f: any, xs: any) {
    var ys: any = [],
      zs: any = [],
      j = 0,
      k = 0;
    for (var i = 0; i < xs.length; i++) {
      if (f(xs[i])) {
        ys[j++] = xs[i];
      } else {
        zs[k++] = xs[i];
      }
    }
    return [ys, zs];
  };
  static permute = function (f: any, xs: any) {
    // Keep the type of the array
    var ys = xs.map(function () {
      return 0;
    });
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
  static removeInPlace = function (item: any, xs: any) {
    var i = xs.indexOf(item);
    if (i > -1) {
      xs.splice(i, 1);
      return true;
    }
    return false;
  };
  static setSlice = function (target: any, lower: any, upper: any, source: any) {
    var length = (upper || target.length - 1) - lower;
    if (ArrayBuffer.isView(target) && source.length <= length) {
      target.set(source, lower);
    } else {
      for (var i = lower | 0, j = 0; j <= length; i++ , j++) {
        target[i] = source[j];
      }
    }
  };
  static sortInPlaceBy = function (f: any, xs: any, dir: any) {
    dir = dir || 1;
    return xs.sort(function (x: any, y: any) {
      x = f(x);
      y = f(y);
      return (x < y ? -1 : x == y ? 0 : 1) * dir;
    });
  };
  static unzip = function (xs: any) {
    var bs = new Array(xs.length),
      cs = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
      bs[i] = xs[i][0];
      cs[i] = xs[i][1];
    }
    return [bs, cs];
  };
  static unzip3 = function (xs: any) {
    var bs = new Array(xs.length),
      cs = new Array(xs.length),
      ds = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
      bs[i] = xs[i][0];
      cs[i] = xs[i][1];
      ds[i] = xs[i][2];
    }
    return [bs, cs, ds];
  };
}
export { FArray as Array };

export class List<T> {
  public head: T;
  public tail: List<T>;

  constructor(head?: T, tail?: List<T>) {
    this.head = head;
    this.tail = tail;
  }

  static ofArray<T>(args: Array<T>, base?: List<T>): List<T> {
    let acc = base || new List<T>();
    for (let i = args.length - 1; i >= 0; i--) {
      acc = new List<T>(args[i], acc);
    }
    return acc;
  }

  get length(): number {
    return Seq.fold((acc, x) => acc + 1, 0, this);
  }

  public [Symbol.iterator](): Iterator<T> {
    let cur: List<T> = this;
    return {
      next: () => {
        const tmp = cur;
        cur = cur.tail;
        return { done: tmp.tail == null, value: tmp.head }
      }
    }
  }

  append(ys: List<T>): List<T> {
    return List.append(this, ys);
  }

  static append<T>(xs: List<T>, ys: List<T>): List<T> {
    return Seq.fold((acc, x) => new List<T>(x, acc), ys, List.rev(xs));
  }

  choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
    return List.choose(f, this);
  }

  static choose<T, U>(f: (x: T) => U, xs: List<T>): List<U> {
    const r = Seq.fold((acc, x) => {
      const y = f(x);
      return y != null ? new List<U>(y, acc) : acc;
    }, new List<U>(), xs);

    return List.rev(r);
  }

  collect<U>(f: (x: T) => List<U>): List<U> {
    return List.collect(f, this);
  }

  static collect<T, U>(f: (x: T) => List<U>, xs: List<T>): List<U> {
    return Seq.fold((acc, x) => acc.append(f(x)), new List<U>(), xs);
  }

  // ToDo: 'xs' should be Seq<List<T>> 
  static concat<T>(xs: List<List<T>>): List<T> {
    return List.collect((x) => x, xs);
  }

  filter(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static filter<T>(f: (x: T) => boolean, xs: List<T>): List<T> {
    return List.rev(Seq.fold((acc, x) => f(x) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  where(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static where<T>(f: (x: T) => boolean, xs: List<T>): List<T> {
    return List.filter(f, xs);
  }

  static init<T>(n: number, f: (i: number) => T): List<T> {
    if (n < 0) {
      throw "List length must be non-negative";
    }
    let xs = new List<T>();
    for (let i = 1; i <= n; i++) {
      xs = new List<T>(f(n - i), xs);
    }
    return xs;
  }

  map<U>(f: (x: T) => U): List<U> {
    return List.map(f, this);
  }

  static map<T, U>(f: (x: T) => U, xs: List<T>): List<U> {
    return List.rev(Seq.fold((acc: List<U>, x: T) => new List<U>(f(x), acc), new List<U>(), xs));
  }

  mapi<U>(f: (i: number, x: T) => U): List<U> {
    return List.mapi(f, this);
  }

  static mapi<T, U>(f: (i: number, x: T) => U, xs: List<T>): List<U> {
    return List.rev(Seq.fold((acc, x, i) => new List<U>(f(i, x), acc), new List<U>(), xs));
  }

  partition(f: (x: T) => boolean): [List<T>, List<T>] {
    return List.partition(f, this);
  }

  static partition<T>(f: (x: T) => boolean, xs: List<T>): [List<T>, List<T>] {
    const ini: TTuple<List<T>, List<T>> = [new List<T>(), new List<T>()];
    return Seq.fold((acc: TTuple<List<T>, List<T>>, x: T) => {
      const lacc = acc[0], racc = acc[1];
      const l: TTuple<List<T>, List<T>> = [new List<T>(x, lacc), racc];
      const r: TTuple<List<T>, List<T>> = [lacc, new List<T>(x, racc)];
      return f(x) ? l : r;
    }, ini, List.rev(xs));
  }

  static replicate<T>(n: number, x: T): List<T> {
    return List.init(n, () => x);
  }

  rev(): List<T> {
    return List.rev(this);
  }

  static rev<T>(xs: List<T>): List<T> {
    return Seq.fold((acc, x) => new List<T>(x, acc), new List<T>(), xs);
  }

  static singleton<T>(x: T): List<T> {
    return new List<T>(x, new List<T>());
  }

  slice(lower: number, upper: number): List<T> {
    return List.slice(lower, upper, this);
  }

  static slice<T>(lower: number, upper: number, xs: List<T>): List<T> {
    const noLower = (lower == null);
    const noUpper = (upper == null);
    return List.rev(Seq.fold((acc, x, i) => (noLower || lower <= i) && (noUpper || i <= upper) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  /* ToDo: ?
  unzip<T1, T2, T extends TTuple<T1, T2>>(): [List<T1>, List<T2>] {
    return List.unzip(this);
  } */

  static unzip<T1, T2>(xs: List<TTuple<T1, T2>>): [List<T1>, List<T2>] {
    const ini: TTuple<List<T1>, List<T2>> = [new List<T1>(), new List<T2>()];
    return Seq.foldBack((xy, acc): TTuple<List<T1>, List<T2>> => [new List<T1>(xy[0], acc[0]), new List<T2>(xy[1], acc[1])], xs, ini);
  }

  /* ToDo: unzip3() */

  static unzip3<T1, T2, T3>(xs: List<TTuple3<T1, T2, T3>>): [List<T1>, List<T2>, List<T3>] {
    const ini: TTuple3<List<T1>, List<T2>, List<T3>> = [new List<T1>(), new List<T2>(), new List<T3>()];
    return Seq.foldBack((xyz, acc): TTuple3<List<T1>, List<T2>, List<T3>> => [new List<T1>(xyz[0], acc[0]), new List<T2>(xyz[1], acc[1]), new List<T3>(xyz[2], acc[2])], xs, ini);
  }
}

export class Seq {
  static __failIfNone = function (res: any) {
    if (res == null) {
      throw "Seq did not contain any matching element";
    }
    return res;
  };
  static toList = function (xs: any) {
    return Seq.foldBack(function (x, acc) {
      return new List(x, acc);
    }, xs, new List());
  };
  static ofList = function (xs: any) {
    return Seq.delay(function () {
      return Seq.unfold(function (x: any) {
        return x.tail != null ? [x.head, x.tail] : null;
      }, xs);
    });
  };
  static ofArray = function (xs: any) {
    return Seq.delay(function () {
      return Seq.unfold(function (i: any) {
        return i < xs.length ? [xs[i], i + 1] : null;
      }, 0);
    });
  };
  static append = function (xs: any, ys: any) {
    return Seq.delay(function () {
      var firstDone = false;
      var iters = [xs[Symbol.iterator](), ys];
      return Seq.unfold(function () {
        var cur: any;
        if (!firstDone) {
          cur = iters[0].next();
          if (!cur.done) {
            return [cur.value, iters];
          } else {
            firstDone = true;
            iters = [null, iters[1][Symbol.iterator]()];
          }
        }
        cur = iters[1].next();
        return !cur.done ? [cur.value, iters] : null;
      }, iters);
    });
  };
  static average = function (xs: any) {
    var count = 1;
    var sum = Seq.reduce(function (acc: any, x: any) {
      count++;
      return acc + x;
    }, xs);
    return sum / count;
  };
  static averageBy = function (f: any, xs: any) {
    var count = 1;
    var sum = Seq.reduce(function (acc: any, x: any) {
      count++;
      return (count === 2 ? f(acc) : acc) + f(x);
    }, xs);
    return sum / count;
  };
  static countBy = function (f: any, xs: any) {
    return Seq.map(function (kv: any) {
      return [kv[0], Seq.count(kv[1])];
    }, Seq.groupBy(f, xs));
  };
  static concat = function (xs: any) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (innerIter: any) {
        var cur: any,
          output: any = null,
          hasFinished = false;
        while (!hasFinished) {
          if (innerIter == null) {
            cur = iter.next();
            if (!cur.done) {
              innerIter = cur.value[Symbol.iterator]();
            } else {
              hasFinished = true;
            }
          } else {
            cur = innerIter.next();
            if (!cur.done) {
              output = cur.value;
              hasFinished = true;
            } else {
              innerIter = null;
            }
          }
        }
        return innerIter != null && output != null ? [output, innerIter] : null;
      }, null);
    });
  };
  static collect = function (f: any, xs: any) {
    return Seq.concat(Seq.map(f, xs));
  };
  static choose = function (f: any, xs: any) {
    var trySkipToNext = function (iter: any): any {
      var cur = iter.next();
      if (!cur.done) {
        var y = f(cur.value);
        return y != null ? [y, iter] : trySkipToNext(iter);
      }
    };
    return Seq.delay(function () {
      return Seq.unfold(function (iter: any) {
        return trySkipToNext(iter);
      }, xs[Symbol.iterator]());
    });
  };
  static compareWith = function (f: any, xs: any, ys: any) {
    var nonZero = Seq.tryFind(function (i: any) {
      return i != 0;
    }, Seq.map2(function (x: any, y: any) {
      return f(x, y);
    }, xs, ys));
    return nonZero != null ? nonZero : Seq.count(xs) - Seq.count(ys);
  };
  static delay = function (f: any) {
    var e: any = {};
    e[Symbol.iterator] = function () {
      return f()[Symbol.iterator]();
    };
    return e;
  };
  static distinctBy = function (f: any, xs: any) {
    return Seq.choose(function (tup: any) {
      return tup[0];
    }, Seq.scan(function (tup: any, x: any) {
      var acc = tup[1];
      var y = f(x);
      return acc.has(y) ? [null, acc] : [x, acc.add(y)];
    }, [null, new Set()], xs));
  };
  static distinct = function (xs: any) {
    return Seq.distinctBy(function (x: any) {
      return x;
    }, xs);
  };
  static empty = function () {
    return Seq.unfold(function () { });
  };
  static enumerateWhile = function (cond: any, xs: any) {
    return Seq.concat(Seq.unfold(function () {
      return cond() ? [xs, true] : null;
    }));
  };
  static enumerateThenFinally = function (xs: any, finalFn: any) {
    return Seq.delay(function () {
      var iter: any;
      try {
        iter = xs[Symbol.iterator]();
      } finally {
        finalFn();
      }
      return Seq.unfold(function (iter: any) {
        try {
          var cur = iter.next();
          return !cur.done ? [cur.value, iter] : null;
        } finally {
          finalFn();
        }
      }, iter);
    });
  };
  static enumerateUsing = function (disp: any, work: any) {
    var isDisposed = false;
    var disposeOnce = function () {
      if (!isDisposed) {
        isDisposed = true;
        disp.dispose();
      }
    };
    try {
      return Seq.enumerateThenFinally(work(disp), disposeOnce);
    } finally {
      disposeOnce();
    }
  };
  static exactlyOne = function (xs: any) {
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
  static exists = function (f: any, xs: any) {
    var aux = function (iter: any): any {
      var cur = iter.next();
      return !cur.done && (f(cur.value) || aux(iter));
    };
    return aux(xs[Symbol.iterator]());
  };
  static exists2 = function (f: any, xs: any, ys: any) {
    var aux = function (iter1: any, iter2: any): any {
      var cur1 = iter1.next(),
        cur2 = iter2.next();
      return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
    };
    return aux(xs[Symbol.iterator](), ys[Symbol.iterator]());
  };
  static filter = function (f: any, xs: any) {
    var trySkipToNext = function (iter: any): any {
      var cur = iter.next();
      if (!cur.done) {
        return f(cur.value) ? [cur.value, iter] : trySkipToNext(iter);
      }
    };
    return Seq.delay(function () {
      return Seq.unfold(trySkipToNext, xs[Symbol.iterator]());
    });
  };
  static where = Seq.filter;

  static fold<T, ST>(f: (previousValue: ST, currentValue: T, currentIndex?: number) => ST, acc: ST, xs: Iterable<T>): ST {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return (xs as Array<T>).reduce(f, acc);
    } else {
      let cur: IteratorResult<T> = null;
      for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
        cur = iter.next();
        if (cur.done) {
          break;
        }
        acc = f(acc, cur.value, i);
      }
      return acc;
    }
  }

  static foldBack<T, ST>(f: (currentValue: T, previousValue: ST, currentIndex?: number) => ST, xs: Iterable<T>, acc: ST): ST {
    const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as Array<T> : Array.from(xs);
    for (let i = arr.length - 1; i >= 0; i--) {
      acc = f(arr[i], acc, i);
    }
    return acc;
  }

  static fold2(f: any, acc: any, xs: any, ys: any) {
    var iter1 = xs[Symbol.iterator](),
      iter2 = ys[Symbol.iterator]();
    let cur1: any, cur2: any;
    for (var i = 0; ; i++) {
      cur1 = iter1.next();
      cur2 = iter2.next();
      if (cur1.done || cur2.done) {
        break;
      }
      acc = f(acc, cur1.value, cur2.value, i);
    }
    return acc;
  }

  static foldBack2 = function (f: any, xs: any, ys: any, acc: any) {
    var ar1 = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
    var ar2 = Array.isArray(ys) || ArrayBuffer.isView(ys) ? ys : Array.from(ys);
    for (var i = ar1.length - 1; i >= 0; i--) {
      acc = f(ar1[i], ar2[i], acc, i);
    }
    return acc;
  };
  static forall = function (f: any, xs: any) {
    return Seq.fold(function (acc, x) {
      return acc && f(x);
    }, true, xs);
  };
  static forall2 = function (f: any, xs: any, ys: any) {
    return Seq.fold2(function (acc: any, x: any, y: any) {
      return acc && f(x, y);
    }, true, xs, ys);
  };
  static groupBy = function (f: any, xs: any) {
    return Seq.fold(function (acc, x) {
      var k = f(x),
        vs = acc.get(k);
      return vs != null ? acc.set(k, new List(x, vs)) : acc.set(k, new List(x, new List()));
    }, new Map(), xs);
  };
  static tryHead = function (xs: any) {
    var iter = xs[Symbol.iterator]();
    var cur = iter.next();
    return cur.done ? null : cur.value;
  };
  static head = function (xs: any) {
    return Seq.__failIfNone(Seq.tryHead(xs));
  };
  static init = function (n: any, f: any) {
    return Seq.delay(function () {
      return Seq.unfold(function (i: any) {
        return i < n ? [f(i), i + 1] : null;
      }, 0);
    });
  };
  static initInfinite = function (f: any) {
    return Seq.delay(function () {
      return Seq.unfold(function (i: any) {
        return [f(i), i + 1];
      }, 0);
    });
  };
  static tryItem = function (i: any, xs: any) {
    if (i < 0) {
      return null;
    } else if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return i < xs.length ? xs[i] : null;
    } else {
      for (var j = 0, iter = xs[Symbol.iterator](); ; j++) {
        var cur = iter.next();
        if (cur.done) {
          return null;
        }
        if (j === i) {
          return cur.value;
        }
      }
    }
  };
  static item = function (i: any, xs: any) {
    return Seq.__failIfNone(Seq.tryItem(i, xs));
  };
  static iter = function (f: any, xs: any) {
    Seq.fold(function (_, x) {
      f(x);
    }, null, xs);
  };
  static iter2 = function (f: any, xs: any, ys: any) {
    Seq.fold2(function (_: any, x: any, y: any) {
      f(x, y);
    }, null, xs, ys);
  };
  static iteri = function (f: any, xs: any) {
    Seq.fold(function (_, x, i) {
      f(i, x);
    }, null, xs);
  };
  static iteri2 = function (f: any, xs: any, ys: any) {
    Seq.fold2(function (_: any, x: any, y: any, i: any) {
      f(i, x, y);
    }, null, xs, ys);
  };
  static isEmpty = function (xs: any) {
    var i = xs[Symbol.iterator]();
    return i.next().done;
  };
  static tryLast = function (xs: any) {
    try {
      return Seq.reduce(function (_: any, x: any) {
        return x;
      }, xs);
    }
    catch (err) {
      return null;
    }
  };
  static last = function (xs: any) {
    return Seq.__failIfNone(Seq.tryLast(xs));
  };

  // A static 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
  static count<T>(xs: Iterable<T>): number {
    return Array.isArray(xs) || ArrayBuffer.isView(xs) ? (xs as Array<T>).length : Seq.fold(function (acc, x) {
      return acc + 1;
    }, 0, xs);
  };

  static map = function (f: any, xs: any) {
    return Seq.delay(function () {
      return Seq.unfold(function (iter: any) {
        var cur = iter.next();
        return !cur.done ? [f(cur.value), iter] : null;
      }, xs[Symbol.iterator]());
    });
  };
  static mapi = function (f: any, xs: any) {
    return Seq.delay(function () {
      var i = 0;
      return Seq.unfold(function (iter: any) {
        var cur = iter.next();
        return !cur.done ? [f(i++, cur.value), iter] : null;
      }, xs[Symbol.iterator]());
    });
  };
  static map2 = function (f: any, xs: any, ys: any) {
    return Seq.delay(function () {
      var iter1 = xs[Symbol.iterator]();
      var iter2 = ys[Symbol.iterator]();
      return Seq.unfold(function () {
        var cur1 = iter1.next(),
          cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
      });
    });
  };
  static mapi2 = function (f: any, xs: any, ys: any) {
    return Seq.delay(function () {
      var i = 0;
      var iter1 = xs[Symbol.iterator]();
      var iter2 = ys[Symbol.iterator]();
      return Seq.unfold(function () {
        var cur1 = iter1.next(),
          cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
      });
    });
  };
  static map3 = function (f: any, xs: any, ys: any, zs: any) {
    return Seq.delay(function () {
      var iter1 = xs[Symbol.iterator]();
      var iter2 = ys[Symbol.iterator]();
      var iter3 = zs[Symbol.iterator]();
      return Seq.unfold(function () {
        var cur1 = iter1.next(),
          cur2 = iter2.next(),
          cur3 = iter3.next();
        return !cur1.done && !cur2.done && !cur3.done ? [f(cur1.value, cur2.value, cur3.value), null] : null;
      });
    });
  };
  static max = function (xs: any) {
    return Seq.reduce(function (acc: any, x: any) {
      return Math.max(acc, x);
    }, xs);
  };
  static maxBy = function (f: any, xs: any) {
    return Seq.reduce(function (x: any, y: any) {
      return f(y) > f(x) ? y : x;
    }, xs);
  };
  static min = function (xs: any) {
    return Seq.reduce(function (acc: any, x: any) {
      return Math.min(acc, x);
    }, xs);
  };
  static minBy = function (f: any, xs: any) {
    return Seq.reduce(function (x: any, y: any) {
      return f(y) > f(x) ? x : y;
    }, xs);
  };
  static pairwise = function (xs: any) {
    return Seq.skip(1, Seq.scan(function (last: any, next: any) {
      return [last[1], next];
    }, [0, 0], xs));
  };
  static permute = function (f: any, xs: any) {
    var ar = Array.from(xs);
    return Seq.ofArray(FArray.permute(f, ar));
  };
  static rangeStep = function (first: any, step: any, last: any) {
    if (step === 0) {
      throw "Step cannot be 0";
    }
    return Seq.unfold(function (x: any) {
      return step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null;
    }, first);
  };
  static rangeChar = function (first: any, last: any) {
    return Seq.unfold(function (x: any) {
      return x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null;
    }, first);
  };
  static range = function (first: any, last: any) {
    return Seq.rangeStep(first, 1, last);
  };
  static readonly = function (xs: any) {
    return Seq.map(function (x: any) {
      return x;
    }, xs);
  };
  static reduce = function (f: any, xs: any) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return xs.reduce(f);
    } else {
      var iter = xs[Symbol.iterator]();
      var cur = iter.next();
      if (cur.done) {
        throw "Seq was empty";
      }
      var acc = cur.value;
      for (; ;) {
        cur = iter.next();
        if (cur.done) {
          break;
        }
        acc = f(acc, cur.value);
      }
      return acc;
    }
  };
  static reduceBack = function (f: any, xs: any) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
    if (ar.length === 0) {
      throw "Seq was empty";
    }
    var acc = ar[ar.length - 1];
    for (var i = ar.length - 2; i >= 0; i--) {
      acc = f(ar[i], acc, i);
    }
    return acc;
  };
  static replicate = function (n: any, x: any) {
    return Seq.init(n, function () {
      return x;
    });
  };
  static rev = function (xs: any) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.slice(0) : Array.from(xs);
    return ar.reverse();
  };
  static scan = function (f: any, seed: any, xs: any) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (acc: any) {
        if (acc == null) {
          return [seed, seed];
        } else {
          var cur = iter.next();
          if (!cur.done) {
            acc = f(acc, cur.value);
            return [acc, acc];
          }
        }
      }, null);
    });
  };
  static scanBack = function (f: any, xs: any, seed: any) {
    return Seq.rev(Seq.scan(function (acc: any, x: any) {
      return f(x, acc);
    }, seed, Seq.rev(xs)));
  };
  static singleton = function (x: any) {
    return Seq.unfold(function (x: any) {
      return x != null ? [x, null] : null;
    }, x);
  };
  static skip = function (n: any, xs: any) {
    var e: any = {};
    e[Symbol.iterator] = function () {
      var iter = xs[Symbol.iterator]();
      for (var i = 1; i <= n; i++) {
        if (iter.next().done) throw "Seq has not enough elements";
      }
      return iter;
    };
    return e;
  };
  static skipWhile = function (f: any, xs: any) {
    return Seq.delay(function () {
      var hasPassed = false;
      return Seq.filter(function (x: any) {
        return hasPassed || (hasPassed = !f(x));
      }, xs);
    });
  };
  static sortWith = function (f: any, xs: any) {
    var ys = Array.from(xs);
    return Seq.ofArray(ys.sort(f));
  };
  static sum = function (xs: any, add: any) {
    add = add || function (x: any, y: any) {
      return x + y;
    };
    return Seq.reduce(function (acc: any, x: any) {
      return add(acc, x);
    }, xs);
  };
  static sumBy = function (f: any, xs: any, add: any) {
    var fst = true;
    add = add || function (x: any, y: any) {
      return x + y;
    };
    return Seq.reduce(function (acc: any, x: any) {
      acc = fst ? f(acc) : acc, fst = false;
      return acc + f(x);
    }, xs);
  };
  static tail = function (xs: any) {
    var iter = xs[Symbol.iterator]();
    var cur = iter.next();
    if (cur.done) {
      throw "Seq was empty";
    }
    var e: any = {};
    e[Symbol.iterator] = function () {
      return iter;
    };
    return e;
  };
  static take = function (n: any, xs: any, truncate: any) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (i: any) {
        if (i < n) {
          var cur = iter.next();
          if (!cur.done) {
            return [cur.value, i + 1];
          } else if (!truncate) {
            throw "Seq has not enough elements";
          }
        }
      }, 0);
    });
  };
  static truncate = function (n: any, xs: any) {
    return Seq.take(n, xs, true);
  };
  static takeWhile = function (f: any, xs: any) {
    return Seq.delay(function () {
      var iter = xs[Symbol.iterator]();
      return Seq.unfold(function (i: any) {
        var cur = iter.next();
        if (!cur.done && f(cur.value)) {
          return [cur.value, null];
        }
      }, 0);
    });
  };
  static tryFind = function (f: any, xs: any) {
    let cur: any;
    for (var i = 0, iter = xs[Symbol.iterator](); ; i++) {
      cur = iter.next();
      if (cur.done) {
        return null;
      }
      if (f(cur.value, i)) {
        return cur.value;
      }
    }
  };
  static find = function (f: any, xs: any) {
    return Seq.__failIfNone(Seq.tryFind(f, xs));
  };
  static tryFindBack = function (f: any, xs: any) {
    var match: any = null;
    let cur: any;
    for (var i = 0, iter = xs[Symbol.iterator](); ; i++) {
      cur = iter.next();
      if (cur.done) {
        return match;
      }
      if (f(cur.value, i)) {
        match = cur.value;
      }
    }
  };
  static findBack = function (f: any, xs: any) {
    return Seq.__failIfNone(Seq.tryFindBack(f, xs));
  };
  static tryFindIndex = function (f: any, xs: any) {
    let cur: any;
    for (var i = 0, iter = xs[Symbol.iterator](); ; i++) {
      cur = iter.next();
      if (cur.done) {
        return null;
      }
      if (f(cur.value, i)) {
        return i;
      }
    }
  };
  static findIndex = function (f: any, xs: any) {
    return Seq.__failIfNone(Seq.tryFindIndex(f, xs));
  };
  static tryFindIndexBack = function (f: any, xs: any) {
    var match: any = null;
    let cur: any;
    for (var i = 0, iter = xs[Symbol.iterator](); ; i++) {
      cur = iter.next();
      if (cur.done) {
        return match;
      }
      if (f(cur.value, i)) {
        match = i;
      }
    }
  };
  static findIndexBack = function (f: any, xs: any) {
    return Seq.__failIfNone(Seq.tryFindIndexBack(f, xs));
  };
  static tryPick = function (f: any, xs: any) {
    let cur: any;
    for (var i = 0, iter = xs[Symbol.iterator](); ; i++) {
      cur = iter.next();
      if (cur.done) {
        break;
      }
      var y = f(cur.value, i);
      if (y != null) {
        return y;
      }
    }
  };
  static pick = function (f: any, xs: any) {
    return Seq.__failIfNone(Seq.tryPick(f, xs));
  };
  static unfold = function (f: any, acc?: any) {
    var e: any = {};
    e[Symbol.iterator] = function () {
      return {
        next: function (): any {
          var res = f(acc);
          if (res != null) {
            acc = res[1];
            return { done: false, value: res[0] };
          } else {
            return { done: true };
          }
        }
      };
    };
    return e;
  };
  static zip = function (xs: any, ys: any) {
    return Seq.map2(function (x: any, y: any) {
      return [x, y];
    }, xs, ys);
  };
  static zip3 = function (xs: any, ys: any, zs: any) {
    return Seq.map3(function (x: any, y: any, z: any) {
      return [x, y, z];
    }, xs, ys, zs);
  };
}

class FSet {
  static op_Addition = function (set1: any, set2: any) {
    var set = new Set(set1);
    set2.forEach(function (x: any) {
      set.add(x);
    });
    return set;
  };
  static union = FSet.op_Addition;
  static unionMany = function (sets: any) {
    return Seq.fold(function (acc: any, s: any) {
      s.forEach(function (x: any) {
        acc.add(x);
      });
      return acc;
    }, new Set(), sets);
  };
  static op_Subtraction = function (set1: any, set2: any) {
    var set = new Set(set1);
    set2.forEach(function (x: any) {
      set.delete(x);
    });
    return set;
  };
  static difference = FSet.op_Subtraction;
  static intersect = function (set1: any, set2: any) {
    var set = new Set(set1);
    set1.forEach(function (x: any) {
      if (!set2.has(x)) set.delete(x);
    });
    return set;
  };
  static intersectMany = function (sets: any) {
    var ar = Array.isArray(sets) ? sets : Array.from(sets);
    if (ar.length == 0) {
      throw "Seq was empty";
    }
    var set = new Set(ar[0]);
    Seq.iter(function (x: any) {
      for (var i = 1; i < ar.length; i++) {
        if (!ar[i].has(x)) {
          set.delete(x);
          break;
        }
      }
    }, ar[0]);
    return set;
  };
  static isProperSubsetOf = function (set1: any, set2: any) {
    return Seq.forall(function (x: any) {
      return set2.has(x);
    }, set1) && Seq.exists(function (x: any) {
      return !set1.has(x);
    }, set2);
  };
  static isProperSubset = FSet.isProperSubsetOf;
  static isSubsetOf = function (set1: any, set2: any) {
    return Seq.forall(function (x: any) {
      return set2.has(x);
    }, set1);
  };
  static isSubset = FSet.isSubsetOf;
  static isProperSupersetOf = function (set1: any, set2: any) {
    return FSet.isProperSubset(set2, set1);
  };
  static isProperSuperset = FSet.isProperSupersetOf;
  static isSupersetOf = function (set1: any, set2: any) {
    return FSet.isSubset(set2, set1);
  };
  static isSuperset = FSet.isSupersetOf;
  static copyTo = function (xs: any, arr: any, arrayIndex: any, count: any) {
    if (!Array.isArray(arr) && !ArrayBuffer.isView(arr))
      throw "Array is invalid";

    count = count || arr.length;
    var i = arrayIndex || 0;
    var iter = xs[Symbol.iterator]();
    while (count--) {
      var el = iter.next();
      if (el.done) break;
      arr[i++] = el.value;
    };
  };
  static partition = function (f: any, xs: any) {
    return Seq.fold(function (acc, x) {
      var lacc = acc[0],
        racc = acc[1];
      return f(x) ? [lacc.add(x), racc] : [lacc, racc.add(x)];
    }, [new Set(), new Set()], xs);
  };
  static removeInPlace = function (item: any, xs: any) {
    xs.delete(item);
    return xs;
  };
  static remove = function (item: any, xs: any) {
    return FSet.removeInPlace(item, new Set(xs));
  };
}
export { FSet as Set };

class FMap {
  static containsValue = function (v: any, map: any) {
    return Seq.fold(function (acc, k) {
      return acc || map.get(k) === v;
    }, false, map.keys());
  };
  static exists = function (f: any, map: any) {
    return Seq.exists(function (kv: any) {
      return f(kv[0], kv[1]);
    }, map);
  };
  static filter = function (f: any, map: any) {
    return Seq.fold(function (acc: any, kv: any) {
      return f(kv[0], kv[1]) ? acc.set(kv[0], kv[1]) : acc;
    }, new Map(), map);
  };
  static fold = function (f: any, seed: any, map: any) {
    return Seq.fold(function (acc: any, kv: any) {
      return f(acc, kv[0], kv[1]);
    }, seed, map);
  };
  static foldBack = function (f: any, map: any, seed: any) {
    return Seq.foldBack(function (kv: any, acc: any) {
      return f(kv[0], kv[1], acc);
    }, map, seed);
  };
  static forall = function (f: any, map: any) {
    return Seq.forall(function (kv: any) {
      return f(kv[0], kv[1]);
    }, map);
  };
  static iter = function (f: any, map: any) {
    return Seq.iter(function (kv: any) {
      f(kv[0], kv[1]);
    }, map);
  };
  static map = function (f: any, map: any) {
    return Seq.fold(function (acc: any, kv: any) {
      return acc.set(kv[0], f(kv[0], kv[1]));
    }, new Map(), map);
  };
  static partition = function (f: any, map: any) {
    return Seq.fold(function (acc: any, kv: any) {
      var lacc = acc[0],
        racc = acc[1],
        k = kv[0],
        v = kv[1];
      return f(k, v) ? [lacc.set(k, v), racc] : [lacc, racc.set(k, v)];
    }, [new Map(), new Map()], map);
  };
  static findKey = function (f: any, map: any) {
    return Seq.pick(function (kv: any) {
      return f(kv[0], kv[1]) ? kv[0] : null;
    }, map);
  };
  static tryFindKey = function (f: any, map: any) {
    return Seq.tryPick(function (kv: any) {
      return f(kv[0], kv[1]) ? kv[0] : null;
    }, map);
  };
  static pick = function (f: any, map: any) {
    return Seq.pick(function (kv: any) {
      var res = f(kv[0], kv[1]);
      return res != null ? res : null;
    }, map);
  };
  static removeInPlace = FSet.removeInPlace;
  static remove = function (item: any, map: any) {
    return FMap.removeInPlace(item, new Map(map));
  };
  static tryPick = function (f: any, map: any) {
    return Seq.tryPick(function (kv: any) {
      var res = f(kv[0], kv[1]);
      return res != null ? res : null;
    }, map);
  };
}
export { FMap as Map };

export class Async {
  static __protectedCont = function (f: any) {
    return function (ctx: any) {
      if (ctx.cancelToken.isCancelled) {
        ctx.onCancel("cancelled");
      } else {
        try {
          f(ctx);
        } catch (err) {
          ctx.onError(err);
        }
      }
    };
  };
  static bind = function (work: any, cont: any) {
    return Async.__protectedCont(function (ctx: any) {
      work({
        onSuccess: function (x: any) {
          return cont(x)(ctx);
        },
        onError: ctx.onError,
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken
      });
    });
  };
  static combine = function (work1: any, work2: any) {
    return Async.bind(work1, function () {
      return work2;
    });
  };
  static delay = function (cont: any) {
    return Async.__protectedCont(function (ctx: any) {
      cont()(ctx);
    });
  };
  static for = function (seq: any, body: any) {
    var iter = seq[Symbol.iterator](),
      cur = iter.next();
    return Async.while(function () {
      return !cur.done;
    }, Async.delay(function () {
      var res = body(cur.value);
      cur = iter.next();
      return res;
    }));
  };
  static return = function (x?: any) {
    return Async.__protectedCont(function (ctx: any) {
      ctx.onSuccess(x);
    });
  };
  static returnFrom = function (work: any) {
    return work;
  };
  static tryFinally = function (work: any, finalFn: any) {
    return Async.__protectedCont(function (ctx: any) {
      work({
        onSuccess: function (x: any) {
          finalFn();
          ctx.onSuccess(x);
        },
        onError: function (x: any) {
          finalFn();
          ctx.onError(x);
        },
        onCancel: function (x: any) {
          finalFn();
          ctx.onCancel(x);
        },
        cancelToken: ctx.cancelToken
      });
    });
  };
  static tryWith = function (work: any, catchFn: any) {
    return Async.__protectedCont(function (ctx: any) {
      work({
        onSuccess: ctx.onSuccess,
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken,
        onError: function (ex: any) {
          ctx.onSuccess(catchFn(ex));
        }
      });
    });
  };
  static using = function (disp: any, cont: any) {
    return Async.tryFinally(cont(disp), function () {
      disp.dispose();
    });
  };
  static while = function (cond: any, body: any) {
    if (cond()) {
      return Async.bind(body, function () {
        return Async.while(cond, body);
      });
    } else {
      return Async.return();
    }
  };
  static zero = function () {
    return Async.__protectedCont(function (ctx: any) {
      ctx.onSuccess();
    });
  };
  static start = function (work: any, onSuccess?: any, onError?: any, onCancel?: any, cancelToken?: any) {
    if (typeof onSuccess !== "function") {
      cancelToken = onSuccess;
      onSuccess = null;
    }
    work({
      onSuccess: onSuccess ? onSuccess : function () { },
      onError: onError ? onError : function () { },
      onCancel: onCancel ? onCancel : function () { },
      cancelToken: cancelToken ? cancelToken : {}
    });
  };
  static startImmediate = Async.start;
  static startWithContinuations = Async.start;
  static ignore = function (work: any) {
    return Async.bind(work, function () {
      return Async.return();
    });
  };

  get cancellationToken() {
    return Async.__protectedCont(function (ctx: any) {
      return ctx.onSuccess(ctx.cancelToken);
    });
  }

  static fromContinuations = function (f: any) {
    return Async.__protectedCont(function (ctx: any) {
      return f([ctx.onSuccess, ctx.onError, ctx.onCancel]);
    });
  };
  static startAsPromise = function (work: any, cancelToken?: any) {
    return new Promise(function (resolve, reject) {
      Async.startWithContinuations(work, resolve, reject, reject, cancelToken ? cancelToken : {});
    });
  };
  static awaitPromise = function (p: any) {
    return Async.fromContinuations(function (conts: any) {
      p.then(conts[0]).catch(function (err: any) {
        (err == "cancelled" ? conts[2] : conts[1])(err);
      });
    });
  };
  static parallel = function (works: any) {
    return Async.awaitPromise(Promise.all(Seq.map(function (w: any) {
      return Async.startAsPromise(w);
    }, works)));
  };
  static catch = function (work: any) {
    return Async.__protectedCont(function (ctx: any) {
      work({
        onSuccess: function (x: any) {
          ctx.onSuccess(new Choice("Choice1Of2", x));
        },
        onError: function (ex: any) {
          ctx.onSuccess(new Choice("Choice2Of2", ex));
        },
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken
      });
    });
  };
  static sleep = function (ms: any) {
    return Async.__protectedCont(function (ctx: any) {
      setTimeout(function () {
        ctx.cancelToken.isCancelled ? ctx.onCancel("cancelled") : ctx.onSuccess();
      }, ms);
    });
  };
}

class Queue {
  public firstAndLast: any;
  add(it: any) {
    var itCell = { value: it };
    if (this.firstAndLast) {
      this.firstAndLast[1].next = itCell;
      this.firstAndLast = [this.firstAndLast[0], itCell];
    }
    else {
      this.firstAndLast = [itCell, itCell];
    }
  };
  tryGet(it?: any) {
    if (this.firstAndLast) {
      var value = this.firstAndLast[0].value;
      if (this.firstAndLast[0].next) {
        this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
      }
      else {
        delete this.firstAndLast;
      }
      return value;
    }
  };
}

export class MailboxProcessor {
  public body: any;
  public messages: any;

  private continuation: any;

  constructor(body: any) {
    this.body = body;
    this.messages = new Queue();
  };

  __processEvents() {
    if (this.continuation) {
      var value = this.messages.tryGet();
      if (value) {
        var cont = this.continuation;
        delete this.continuation;
        cont(value);
      }
    }
  };
  start() {
    Async.startImmediate(this.body(this));
  };
  static start = function (body: any) {
    var mbox = new MailboxProcessor(body);
    mbox.start();
    return mbox;
  };
  receive() {
    var _this = this;
    return Async.fromContinuations(function (conts: any) {
      if (_this.continuation) {
        throw "Receive can only be called once!";
      }
      _this.continuation = conts[0];
      _this.__processEvents();
    });
  };
  postAndAsyncReply(f: any) {
    var result: any, continuation: any;
    function checkCompletion() {
      if (result && continuation) {
        continuation(result);
      }
    };
    var reply = {
      reply: function (res: any) {
        result = res;
        checkCompletion();
      }
    };
    this.messages.add(f(reply));
    this.__processEvents();
    return Async.fromContinuations(function (conts: any) {
      continuation = conts[0];
      checkCompletion();
    });
  };
  post(msg: any) {
    this.messages.add(msg);
    this.__processEvents();
  };
}

class Observer {
  public onNext: any;
  public onError: any;
  public onCompleted: any;
  constructor(onNext: any, onError?: any, onCompleted?: any) {
    this.onNext = onNext;
    this.onError = onError || function (e: any) { };
    this.onCompleted = onCompleted || function () { };
  }
};
Util.setInterfaces(Observer.prototype, ["System.IObserver"]);

class Observable {
  public subscribe: any;
  constructor(subscribe: any) {
    this.subscribe = subscribe;
  };
}
Util.setInterfaces(Observable.prototype, ["System.IObservable"]);

class Obs {
  static __protect = function (f: any, succeed: any, fail: any) {
    try {
      succeed(f());
    } catch (e) {
      fail(e);
    }
  };
  static map = function (f: any, w: any): any {
    return new Observable(function (observer: any): any {
      return w.subscribe(new Observer(function (v: any): any {
        Obs.__protect(function (): any {
          f(v);
        }, observer.onNext, observer.onError);
      }, observer.onError, observer.onCompleted));
    });
  };
  static choose = function (f: any, w: any): any {
    return new Observable(function (observer: any) {
      return w.subscribe(new Observer(function (v: any) {
        Obs.__protect(function () {
          f(v);
        }, function (v: any) {
          if (v != null) {
            observer.onNext(v);
          }
        }, observer.onError);
      }, observer.onError, observer.onCompleted));
    });
  };
  static filter = function (f: any, w: any) {
    return Obs.choose(function (x: any) {
      return f(x) ? x : null;
    }, w);
  };
  static partition = function (f: any, w: any) {
    return [Obs.filter(f, w), Obs.filter(function (x: any) {
      return !f(x);
    }, w)];
  };
  static scan = function (f: any, state: any, w: any): any {
    return new Observable(function (observer: any) {
      return w.subscribe(new Observer(function (v: any) {
        Obs.__protect(function () {
          f(state, v);
        }, function (z: any) {
          state = z;
          observer.onNext(z);
        }, observer.onError);
      }, observer.onError, observer.onCompleted));
    });
  };
  static add = function (f: any, w: any) {
    w.subscribe(new Observer(f));
  };
  static subscribe = function (f: any, w: any) {
    return w.subscribe(new Observer(f));
  };
  static pairwise = function (w: any): any {
    return new Observable(function (observer: any) {
      var lastArgs: any = null;
      return w.subscribe(new Observer(function (args2: any) {
        if (lastArgs != null) {
          observer.onNext([lastArgs, args2]);
        }
        lastArgs = args2;
      }, observer.onError, observer.onCompleted));
    });
  };
  static merge = function (w1: any, w2: any): any {
    return new Observable(function (observer: any) {
      var stopped = false,
        completed1 = false,
        completed2 = false;
      var h1 = w1.subscribe(new Observer(function (v: any) {
        if (!stopped) {
          observer.onNext(v);
        }
      }, function (e: any) {
        if (!stopped) {
          stopped = true;
          observer.onError(e);
        }
      }, function () {
        if (!stopped) {
          completed1 = true;
          if (completed2) {
            stopped = true;
            observer.onCompleted();
          }
        }
      }));
      var h2 = w2.subscribe(new Observer(function (v: any) {
        if (!stopped) {
          observer.onNext(v);
        }
      }, function (e: any) {
        if (!stopped) {
          stopped = true;
          observer.onError(e);
        }
      }, function () {
        if (!stopped) {
          completed2 = true;
          if (completed1) {
            stopped = true;
            observer.onCompleted();
          }
        }
      }));
      var disp: any = {
        dispose: function () {
          h1.dispose();
          h2.dispose();
        }
      };
      disp[FSymbol.interfaces] = ["System.IDisposable"];
      return disp;
    });
  };
  static split = function (f: any, w: any) {
    return [Obs.choose(function (v: any) {
      var res = f(v);
      return res.Case == "Choice1Of2" ? res.Fields[0] : null;
    }, w), Obs.choose(function (v: any) {
      var res = f(v);
      return res.Case == "Choice2Of2" ? res.Fields[0] : null;
    }, w)];
  };
}
export { Obs as Observable };

export class Event {
  public sbscrb: any;
  public delegates: any;

  constructor(sbscrb?: any, delegates?: any) {
    this.sbscrb = sbscrb;
    this.delegates = delegates || new Array();
  }

  trigger(value: any) {
    Seq.iter(function (f: any) {
      f(value);
    }, this.delegates);
  };

  private _addHandler(f: any) {
    this.delegates.push(f);
  };

  private _removeHandler(f: any) {
    var fnd = function (el: any, i: any, arr: any) {
      return '' + el == '' + f; //Special dedication to Chet Husk.
    };

    var index = this.delegates.findIndex(fnd);
    if (index > -1) {
      this.delegates.splice(index, 1);
    }
  };

  subscribe(f: any) {
    var disp: any;
    return this._addHandler(f), disp = {
      dispose: () => this._removeHandler(f)
    }, disp[FSymbol.interfaces] = ["System.IDisposable"], disp;
  };

  add(f: any) {
    this._addHandler(f);
  };

  addHandler(f: any) {
    var h = function (x: any) {
      return f(undefined, x);
    };
    this._addHandler(h);
  };

  removeHandler(f: any) {
    var h = function (x: any) {
      return f(undefined, x);
    };
    this._removeHandler(h);
  };

  _subscribe(observer: any) {
    if (this.sbscrb) return this.sbscrb(observer);
    var disp: any,
      f = observer.onNext;
    return this._addHandler(f), disp = {
      dispose: function () {
        this._removeHandler(f);
      }
    }, disp[FSymbol.interfaces] = ["System.IDisposable"], disp;
  };

  get publish() {
    return this;
  }

  static add = function (f: any, w: any) {
    w._subscribe(new Observer(f));
  };

  static map = function (f: any, w: any) {
    var s = function (observer: any) {
      w._subscribe(new Observer(function (v: any) {
        Obs.__protect(function () {
          return f(v);
        }, observer.onNext, observer.onError);
      }, observer.onError, observer.onCompleted));
    };
    return new Event(s, w.delegates);
  };

  static choose = function (f: any, w: any) {
    var s = function (observer: any) {
      return w._subscribe(new Observer(function (v: any) {
        Obs.__protect(function () {
          return f(v);
        }, function (v: any) {
          if (v != null) {
            observer.onNext(v);
          }
        }, observer.onError);
      }, observer.onError, observer.onCompleted));
    };
    return new Event(s, w.delegates);
  };

  static filter = function (f: any, w: any) {
    return Event.choose(function (x: any) {
      return f(x) ? x : null;
    }, w);
  };

  static partition = function (f: any, w: any) {
    return [Event.filter(f, w), Event.filter(function (x: any) {
      return !f(x);
    }, w)];
  };

  static scan = function (f: any, state: any, w: any) {
    var s = function (observer: any) {
      return w._subscribe(new Observer(function (v: any) {
        Obs.__protect(function () {
          return f(state, v);
        }, function (z: any) {
          state = z;
          observer.onNext(z);
        }, observer.onError);
      }, observer.onError, observer.onCompleted));
    };
    return new Event(s, w.delegates);
  };

  static pairwise = function (w: any) {
    var s = function (observer: any) {
      var lastArgs: any = null;
      return w._subscribe(new Observer(function (args2: any) {
        if (lastArgs != null) {
          observer.onNext([lastArgs, args2]);
        }
        lastArgs = args2;
      }, observer.onError, observer.onCompleted));
    };
    return new Event(s, w.delegates);
  };

  static merge = function (w1: any, w2: any) {
    var s = function (observer: any) {
      var stopped = false,
        completed1 = false,
        completed2 = false;
      var h1 = w1._subscribe(new Observer(function (v: any) {
        if (!stopped) {
          observer.onNext(v);
        }
      }, function (e: any) {
        if (!stopped) {
          stopped = true;
          observer.onError(e);
        }
      }, function () {
        if (!stopped) {
          completed1 = true;
          if (completed2) {
            stopped = true;
            observer.onCompleted();
          }
        }
      }));
      var h2 = w2._subscribe(new Observer(function (v: any) {
        if (!stopped) {
          observer.onNext(v);
        }
      }, function (e: any) {
        if (!stopped) {
          stopped = true;
          observer.onError(e);
        }
      }, function () {
        if (!stopped) {
          completed2 = true;
          if (completed1) {
            stopped = true;
            observer.onCompleted();
          }
        }
      }));
      var disp: any = {
        dispose: function () {
          h1.dispose();
          h2.dispose();
        }
      };
      disp[FSymbol.interfaces] = ["System.IDisposable"];
      return disp;
    };

    return new Event(s, w1.delegates.concat(w2.delegates));
  };

  static split = function (f: any, w: any) {
    return [Event.choose(function (v: any) {
      var res = f(v);
      return res.Case == "Choice1Of2" ? res.Fields[0] : null;
    }, w), Event.choose(function (v: any) {
      var res = f(v);
      return res.Case == "Choice2Of2" ? res.Fields[0] : null;
    }, w)];
  };
}

export class Lazy {
  public factory: any;
  public isValueCreated: boolean;

  private createdValue: any;

  constructor(factory: any) {
    this.factory = factory;
    this.isValueCreated = false;
  };

  static createFromValue = function (v: any) {
    return new Lazy(function () { return v; });
  };

  get value() {
    if (!this.isValueCreated) {
      this.createdValue = this.factory();
      this.isValueCreated = true;
    }
    return this.createdValue;
  }
}

// Types needed for unzip/unzip3 -- http://stackoverflow.com/a/32191614
export type TTuple<T1, T2> = [T1, T2];
export type TTuple3<T1, T2, T3> = [T1, T2, T3];
