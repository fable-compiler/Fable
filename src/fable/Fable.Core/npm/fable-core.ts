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

export type TTuple<T1, T2> = [T1, T2];
export type TTuple3<T1, T2, T3> = [T1, T2, T3];

export function Tuple<T1, T2>(x: T1, y: T2) {
  return <TTuple<T1, T2>>[x, y];
}

export function Tuple3<T1, T2, T3>(x: T1, y: T2, z: T3) {
  return <TTuple3<T1, T2, T3>>[x, y, z];
}

export class Util {
  private static __types = new Map<string, any>()

  // For legacy reasons the name is kept, but this method also adds
  // the type name to a cache. Use it after declaration:
  // Util.setInterfaces(Foo.prototype, ["IFoo", "IBar"], "MyModule.Foo");
  public static setInterfaces(proto: any, interfaces: string[], typeName?: string) {
    if (Array.isArray(interfaces) && interfaces.length > 0) {
      const currentInterfaces = proto[FSymbol.interfaces];
      if (Array.isArray(currentInterfaces)) {
        for (let i = 0; i < interfaces.length; i++)
          currentInterfaces.push(interfaces[i]);
      } else
        proto[FSymbol.interfaces] = interfaces;
    }

    if (typeName) {
      proto[FSymbol.typeName] = typeName;
      Util.__types.set(typeName, proto.constructor);
    }
  };

  static hasInterface(obj: any, interfaceName: string) {
    return Array.isArray(obj[FSymbol.interfaces]) && obj[FSymbol.interfaces].indexOf(interfaceName) >= 0;
  };

  static getRestParams(args: ArrayLike<any>, idx: number) {
    for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++)
      restArgs[_key - idx] = args[_key];
    return restArgs;
  };

  static compareTo(x: any, y: any): number {
    function isCollectionComparable(o: any) {
      return Array.isArray(o) || ArrayBuffer.isView(o) || o instanceof List || o instanceof Map || o instanceof Set;
    }

    function sortIfMapOrSet(o: any) {
      return o instanceof Map || o instanceof Set ? Array.from(o).sort() : o;
    }

    if (typeof x != typeof y)
      return -1;

    if (x != null && y != null && typeof x == "object" && typeof y == "object") {
      if (Object.getPrototypeOf(x) != Object.getPrototypeOf(y))
        return -1;

      if (Util.hasInterface(x, "System.IComparable"))
        return x.compareTo(y);

      if (isCollectionComparable(x)) {
        const lengthComp = Util.compareTo(Seq.count(x), Seq.count(y));
        return lengthComp != 0
          ? lengthComp
          : Seq.fold2((prev, v1, v2) => prev != 0 ? prev : Util.compareTo(v1, v2), 0, sortIfMapOrSet(x), sortIfMapOrSet(y));
      }

      if (x instanceof Date)
        return x < y ? -1 : x > y ? 1 : 0;

      const keys1 = Object.getOwnPropertyNames(x), keys2 = Object.getOwnPropertyNames(y);
      const lengthComp = Util.compareTo(keys1.length, keys2.length);
      return lengthComp != 0
        ? lengthComp
        : Seq.fold2((prev, k1, k2) => prev != 0 ? prev : Util.compareTo(x[k1], y[k2]), 0, keys1.sort(), keys2.sort());
    }
    return x < y ? -1 : x > y ? 1 : 0;
  };

  static createObj(fields: Iterable<TTuple<string, any>>) {
    return Seq.fold((acc, kv) => { acc[kv[0]] = kv[1]; return acc; }, <any>{}, fields);
  };

  static toJson(o: any) {
    return JSON.stringify(o, (k, v) => {
      if (ArrayBuffer.isView(v))
        return Array.from(v);

      if (typeof v == "object") {
        if (v instanceof List || v instanceof Map || v instanceof Set)
          throw "JSON serialization of List, Map or Set is not supported";

        if (v[FSymbol.typeName])
          return Object.assign({ __type: v[FSymbol.typeName] }, v);
      }
      return v;
    });
  };

  static ofJson(json: any) {
    return JSON.parse(json, (k, v) => {
      if (typeof v == "object" && v.__type) {
        const T = Util.__types.get(v.__type);
        if (T) {
          delete v.__type;
          return Object.assign(new T(), v);
        }
      }
      return v;
    });
  };
}

export class TimeSpan extends Number {
  static create(d: number = 0, h: number = 0, m: number = 0, s: number = 0, ms: number = 0) {
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
    let d2: Date;
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
    const date = (v == null) ? new Date() : new Date(v);
    if (isNaN(date.getTime()))
      throw "The string is not a valid Date.";
    (<FDate>date).kind = kind || DateKind.Local;
    return date;
  };

  static create(year: number, month: number, day: number, h: number = 0, m: number = 0, s: number = 0, ms: number = 0, kind: DateKind = DateKind.Local) {
    const date: Date = (kind === DateKind.UTC)
      ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms))
      : new Date(year, month - 1, day, h, m, s, ms);
    if (isNaN(date.getTime()))
      throw "The parameters describe an unrepresentable Date.";
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
    return month == 2
      ? FDate.isLeapYear(year) ? 29 : 28
      : month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
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
    const year = FDate.year(d);
    const month = FDate.month(d);
    let day = FDate.day(d);
    for (let i = 1; i < month; i++)
      day += FDate.daysInMonth(year, i);
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
    const newMonth = FDate.month(d);
    const newYear = FDate.year(d) + v;
    const daysInMonth = FDate.daysInMonth(newYear, newMonth);
    const newDay = Math.min(daysInMonth, FDate.day(d));
    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), (<FDate>d).kind);
  };

  static addMonths(d: Date, v: number) {
    let newMonth = FDate.month(d) + v;
    let newMonth_ = 0;
    let yearOffset = 0;
    if (newMonth > 12) {
      newMonth_ = newMonth % 12;
      yearOffset = Math.floor(newMonth / 12);
      newMonth = newMonth_;
    } else if (newMonth < 1) {
      newMonth_ = 12 + newMonth % 12;
      yearOffset = Math.floor(newMonth / 12) + (newMonth_ == 12 ? -1 : 0);
      newMonth = newMonth_;
    }
    const newYear = FDate.year(d) + yearOffset;
    const daysInMonth = FDate.daysInMonth(newYear, newMonth);
    const newDay = Math.min(daysInMonth, FDate.day(d));
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

export interface Disposable {
  dispose(): void;
}

export class Timer implements Disposable {
  public interval: number;
  public autoReset: boolean;
  public _elapsed: Event;

  private _enabled: boolean;
  private _isDisposed: boolean;
  private _intervalId: number;
  private _timeoutId: number;

  constructor(interval?: number) {
    this.interval = interval > 0 ? interval : 100;
    this.autoReset = true;
    this._elapsed = new Event();
  }

  get elapsed() {
    return this._elapsed;
  }

  get enabled() {
    return this._enabled;
  }

  set enabled(x: boolean) {
    if (!this._isDisposed && this._enabled != x) {
      if (this._enabled = x) {
        if (this.autoReset) {
          var _this = this;
          this._intervalId = setInterval(() => {
            if (!_this.autoReset)
              _this.enabled = false;
            _this._elapsed.trigger(new Date());
          }, this.interval);
        } else {
          var _this = this;
          this._timeoutId = setTimeout(() => {
            _this.enabled = false;
            _this._timeoutId = 0;
            if (_this.autoReset)
              _this.enabled = true;
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

  dispose() {
    this.enabled = false;
    this._isDisposed = true;
  };

  close() {
    this.dispose();
  }

  start() {
    this.enabled = true;
  }
  stop() {
    this.enabled = false;
  }
}
Util.setInterfaces(Timer.prototype, ["System.IDisposable"]);

class FString {
  private static fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;

  private static fsFormat(str: any) {
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

  static format(str: string, ...args: any[]) {
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

  static endsWith(str: string, search: string) {
    const idx = str.lastIndexOf(search);
    return idx >= 0 && idx == str.length - search.length;
  };

  static init(n: number, f: (i: number) => string) {
    if (n < 0)
      throw "String length must be non-negative";

    const xs = new Array(n);
    for (let i = 0; i < n; i++)
      xs[i] = f(i);
    return xs.join("");
  };

  static isNullOrEmpty(str: string | any) {
    return typeof str !== "string" || str.length == 0;
  };

  static isNullOrWhiteSpace(str: string | any) {
    return typeof str !== "string" || /^\s*$/.test(str);
  };

  static join(delimiter: string, xs: ArrayLike<string>) {
    xs = typeof xs == "string" ? Util.getRestParams(arguments, 1) : xs;
    return (Array.isArray(xs) ? xs : Array.from(xs)).join(delimiter);
  };
  static concat = FString.join;

  static newGuid() {
    let uuid = '';
    for (let i = 0; i < 32; i++) {
      const random = Math.random() * 16 | 0;
      if (i === 8 || i === 12 || i === 16 || i === 20)
        uuid += '-';
      uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
    }
    return uuid;
  };

  static padLeft(str: any, len: number, ch?: string, isRight?: boolean) {
    ch = ch || ' ';
    str = String(str);
    len = len - str.length;
    for (let i = -1; ++i < len;)
      str = isRight ? str + ch : ch + str;
    return str;
  };

  static padRight(str: any, len: number, ch?: string) {
    return FString.padLeft(str, len, ch, true);
  };

  static replace(str: string, search: string, replace: string) {
    return str.replace(new RegExp(FRegExp.escape(search), "g"), replace);
  };

  static replicate(n: number, x: string) {
    return FString.init(n, () => x);
  };

  static split(str: string, splitters: string[], count?: number, removeEmpty?: number) {
    count = typeof count == "number" ? count : null;
    removeEmpty = typeof removeEmpty == "number" ? removeEmpty : null;
    if (count < 0)
      throw "Count cannot be less than zero";
    if (count === 0)
      return [];
    splitters = Array.isArray(splitters) ? splitters : Util.getRestParams(arguments, 1);
    splitters = splitters.map((x) => FRegExp.escape(x));
    splitters = splitters.length > 0 ? splitters : [" "];
    let m: RegExpExecArray;
    let i = 0;
    const splits: string[] = [];
    const reg = new RegExp(splitters.join("|"), "g");
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
}
export { FString as String };

export type MatchEvaluator = (match: any) => string;

class FRegExp {
  static create(pattern: string, options: number) {
    let flags = "g";
    flags += options & 1 ? "i" : "";
    flags += options & 2 ? "m" : "";
    return new RegExp(pattern, flags);
  };

  // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
  static escape(str: string) {
    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&');
  };

  static unescape(str: string) {
    return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, '$1');
  };

  static isMatch(str: string | RegExp, pattern: string, options: number = 0) {
    var reg: RegExp = str instanceof RegExp
      ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
      : reg = FRegExp.create(pattern, options)
    return reg.test(<string>str);
  };

  static match(str: string | RegExp, pattern: string, options: number = 0) {
    var reg: RegExp = str instanceof RegExp
      ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
      : reg = FRegExp.create(pattern, options);
    return reg.exec(<string>str);
  };

  static matches(str: string | RegExp, pattern: string, options: number = 0) {
    var reg: RegExp = str instanceof RegExp
      ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
      : reg = FRegExp.create(pattern, options);
    if (!reg.global)
      throw "Non-global RegExp"; // Prevent infinite loop

    let m: RegExpExecArray;
    const matches: RegExpExecArray[] = [];
    while ((m = reg.exec(<string>str)) !== null)
      matches.push(m);
    return matches;
  };

  static options(reg: RegExp) {
    let options = 256; // ECMAScript
    options |= reg.ignoreCase ? 1 : 0;
    options |= reg.multiline ? 2 : 0;
    return options;
  };

  static replace(reg: string | RegExp, input: string, replacement: string | MatchEvaluator, limit?: number, offset: number = 0) {
    if (typeof reg == "string") {
      const tmp = <string>reg;
      reg = FRegExp.create(input, limit);
      input = tmp;
      limit = undefined;
    }
    if (typeof replacement == "function") {
      limit = limit == null ? -1 : limit;
      const replacer = () => {
        let res = arguments[0];
        if (limit !== 0) {
          limit--;
          const match: any = [];
          const len = arguments.length;
          for (let i = 0; i < len - 2; i++)
            match.push(arguments[i]);
          match.index = arguments[len - 2];
          match.input = arguments[len - 1];
          res = (<MatchEvaluator>replacement)(match);
        }
        return res;
      };
      return input.substring(0, offset) + input.substring(offset).replace(<RegExp>reg, replacer);
    } else {
      if (limit != null) {
        let m: RegExpExecArray;
        const sub1 = input.substring(offset);
        const matches = FRegExp.matches(reg, sub1);
        const sub2 = matches.length > limit ? (m = matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
        return input.substring(0, offset) + sub2.replace(<RegExp>reg, <string>replacement) + input.substring(offset + sub2.length);
      } else {
        return input.replace(<RegExp>reg, <string>replacement);
      }
    }
  };

  static split(reg: string | RegExp, input: string, limit?: number, offset: number = 0) {
    if (typeof reg == "string") {
      const tmp = <string>reg;
      reg = FRegExp.create(input, limit);
      input = tmp
      limit = undefined;
    }
    input = input.substring(offset);
    return input.split(<RegExp>reg, limit);
  };
}
export { FRegExp as RegExp };

class FArray {
  static addRangeInPlace<T>(range: Iterable<T>, xs: Array<T>) {
    Seq.iter((x) => xs.push(x), range);
  };

  static blit<T>(source: ArrayLike<T>, sourceIndex: number, target: ArrayLike<T>, targetIndex: number, count: number) {
    while (count--)
      target[targetIndex++] = source[sourceIndex++];
  };

  static partition<T>(f: (x: T) => boolean, xs: ArrayLike<T>) {
    let ys = <T[]>[], zs = <T[]>[], j = 0, k = 0;
    for (let i = 0; i < xs.length; i++)
      if (f(xs[i]))
        ys[j++] = xs[i];
      else
        zs[k++] = xs[i];
    return Tuple(ys, zs);
  };

  static permute<T>(f: (i: number) => number, xs: Array<T>) {
    // Keep the type of the array
    let ys = xs.map(() => <T>null);
    let checkFlags = new Array(xs.length);
    for (let i = 0; i < xs.length; i++) {
      const j = f(i);
      if (j < 0 || j >= xs.length)
        throw "Not a valid permutation";

      ys[j] = xs[i];
      checkFlags[j] = 1;
    }
    for (let i = 0; i < xs.length; i++)
      if (checkFlags[i] != 1)
        throw "Not a valid permutation";

    return ys;
  };

  static removeInPlace<T>(item: T, xs: Array<T>) {
    const i = xs.indexOf(item);
    if (i > -1) {
      xs.splice(i, 1);
      return true;
    }
    return false;
  };

  static setSlice<T>(target: any, lower: number, upper: number, source: ArrayLike<T>) {
    const length = (upper || target.length - 1) - lower;
    if (ArrayBuffer.isView(target) && source.length <= length)
      target.set(source, lower);
    else
      for (let i = lower | 0, j = 0; j <= length; i++ , j++)
        target[i] = source[j];
  };

  static sortInPlaceBy<T>(f: (x: T) => T, xs: Array<T>, dir: number = 1) {
    return xs.sort((x, y) => {
      x = f(x);
      y = f(y);
      return (x < y ? -1 : x == y ? 0 : 1) * dir;
    });
  };

  static unzip<T1, T2>(xs: ArrayLike<TTuple<T1, T2>>) {
    const bs = new Array<T1>(xs.length), cs = new Array<T2>(xs.length);
    for (let i = 0; i < xs.length; i++) {
      bs[i] = xs[i][0];
      cs[i] = xs[i][1];
    }
    return Tuple(bs, cs);
  };

  static unzip3<T1, T2, T3>(xs: ArrayLike<TTuple3<T1, T2, T3>>) {
    const bs = new Array<T1>(xs.length), cs = new Array<T2>(xs.length), ds = new Array<T3>(xs.length);
    for (let i = 0; i < xs.length; i++) {
      bs[i] = xs[i][0];
      cs[i] = xs[i][1];
      ds[i] = xs[i][2];
    }
    return Tuple3(bs, cs, ds);
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

  static ofArray<T>(args: Array<T>, base?: List<T>) {
    let acc = base || new List<T>();
    for (let i = args.length - 1; i >= 0; i--) {
      acc = new List<T>(args[i], acc);
    }
    return acc;
  }

  get length() {
    return Seq.fold((acc, x) => acc + 1, 0, this);
  }

  public [Symbol.iterator]() {
    let cur: List<T> = this;
    return <Iterator<T>>{
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

  static append<T>(xs: List<T>, ys: List<T>) {
    return Seq.fold((acc, x) => new List<T>(x, acc), ys, List.rev(xs));
  }

  choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
    return List.choose(f, this);
  }

  static choose<T, U>(f: (x: T) => U, xs: List<T>) {
    const r = Seq.fold((acc, x) => {
      const y = f(x);
      return y != null ? new List<U>(y, acc) : acc;
    }, new List<U>(), xs);

    return List.rev(r);
  }

  collect<U>(f: (x: T) => List<U>): List<U> {
    return List.collect(f, this);
  }

  static collect<T, U>(f: (x: T) => List<U>, xs: List<T>) {
    return Seq.fold((acc, x) => acc.append(f(x)), new List<U>(), xs);
  }

  // TODO: should be xs: Iterable<List<T>>
  static concat<T>(xs: List<List<T>>) {
    return List.collect((x) => x, xs);
  }

  filter(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static filter<T>(f: (x: T) => boolean, xs: List<T>) {
    return List.rev(Seq.fold((acc, x) => f(x) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  where(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static where<T>(f: (x: T) => boolean, xs: List<T>) {
    return List.filter(f, xs);
  }

  static init<T>(n: number, f: (i: number) => T) {
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

  static map<T, U>(f: (x: T) => U, xs: List<T>) {
    return List.rev(Seq.fold((acc: List<U>, x: T) => new List<U>(f(x), acc), new List<U>(), xs));
  }

  mapi<U>(f: (i: number, x: T) => U): List<U> {
    return List.mapi(f, this);
  }

  static mapi<T, U>(f: (i: number, x: T) => U, xs: List<T>) {
    return List.rev(Seq.fold((acc, x, i) => new List<U>(f(i, x), acc), new List<U>(), xs));
  }

  partition(f: (x: T) => boolean): [List<T>, List<T>] {
    return List.partition(f, this);
  }

  static partition<T>(f: (x: T) => boolean, xs: List<T>) {
    return Seq.fold((acc, x) => {
      const lacc = acc[0], racc = acc[1];
      return f(x) ? Tuple(new List<T>(x, lacc), racc) : Tuple(lacc, new List<T>(x, racc));
    }, Tuple(new List<T>(), new List<T>()), List.rev(xs));
  }

  static replicate<T>(n: number, x: T) {
    return List.init(n, () => x);
  }

  rev(): List<T> {
    return List.rev(this);
  }

  static rev<T>(xs: List<T>) {
    return Seq.fold((acc, x) => new List<T>(x, acc), new List<T>(), xs);
  }

  static singleton<T>(x: T) {
    return new List<T>(x, new List<T>());
  }

  slice(lower: number, upper: number): List<T> {
    return List.slice(lower, upper, this);
  }

  static slice<T>(lower: number, upper: number, xs: List<T>) {
    const noLower = (lower == null);
    const noUpper = (upper == null);
    return List.rev(Seq.fold((acc, x, i) => (noLower || lower <= i) && (noUpper || i <= upper) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  /* ToDo: instance unzip() */

  static unzip<T1, T2>(xs: List<TTuple<T1, T2>>) {
    return Seq.foldBack((xy, acc) =>
      Tuple(new List<T1>(xy[0], acc[0]), new List<T2>(xy[1], acc[1])), xs, Tuple(new List<T1>(), new List<T2>()));
  }

  /* ToDo: instance unzip3() */

  static unzip3<T1, T2, T3>(xs: List<TTuple3<T1, T2, T3>>) {
    return Seq.foldBack((xyz, acc) =>
      Tuple3(new List<T1>(xyz[0], acc[0]), new List<T2>(xyz[1], acc[1]), new List<T3>(xyz[2], acc[2])), xs, Tuple3(new List<T1>(), new List<T2>(), new List<T3>()));
  }
}

export class Seq {
  private static __failIfNone<T>(res: T) {
    if (res == null)
      throw "Seq did not contain any matching element";
    return res;
  };

  static toList<T>(xs: Iterable<T>) {
    return Seq.foldBack((x, acc) =>
      new List(x, acc), xs, new List<T>());
  };

  static ofList<T>(xs: List<T>) {
    return Seq.delay(() =>
      Seq.unfold((x) =>
        x.tail != null ? [x.head, x.tail] : null, xs));
  };

  static ofArray<T>(xs: ArrayLike<T>) {
    return Seq.delay(() =>
      Seq.unfold((i) =>
        i < xs.length ? [xs[i], i + 1] : null, 0));
  };

  static append<T>(xs: Iterable<T>, ys: Iterable<T>) {
    return Seq.delay(() => {
      let firstDone = false;
      let i = xs[Symbol.iterator]();
      let iters = Tuple(i, <Iterator<T>>null);
      return Seq.unfold(() => {
        let cur: IteratorResult<T>;
        if (!firstDone) {
          cur = iters[0].next();
          if (!cur.done) {
            return [cur.value, iters];
          } else {
            firstDone = true;
            iters = [<Iterator<T>>null, ys[Symbol.iterator]()];
          }
        }
        cur = iters[1].next();
        return !cur.done ? [cur.value, iters] : null;
      }, iters);
    });
  };

  static average(xs: Iterable<number>) {
    let count = 1;
    const sum = Seq.reduce((acc: number, x: number) => {
      count++;
      return acc + x;
    }, xs);
    return sum / count;
  };

  static averageBy(f: (a: number) => number, xs: Iterable<number>) {
    let count = 1;
    const sum = Seq.reduce((acc: number, x: number) => {
      count++;
      return (count === 2 ? f(acc) : acc) + f(x);
    }, xs);
    return sum / count;
  };

  static countBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
    return Seq.map((kv) => Tuple(kv[0], Seq.count(kv[1])), Seq.groupBy(f, xs));
  };

  static concat<T>(xs: Iterable<Iterable<T>>) {
    return Seq.delay(() => {
      let iter = xs[Symbol.iterator]();
      let output: T = null;
      return Seq.unfold((innerIter) => {
        let hasFinished = false;
        while (!hasFinished) {
          if (innerIter == null) {
            let cur = iter.next();
            if (!cur.done) {
              innerIter = cur.value[Symbol.iterator]();
            } else {
              hasFinished = true;
            }
          } else {
            let cur = innerIter.next();
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

  static collect<T, U>(f: (x: T) => Iterable<U>, xs: Iterable<T>) {
    return Seq.concat(Seq.map(f, xs));
  };

  static choose<T, U>(f: (x: T) => U, xs: Iterable<T>) {
    const trySkipToNext = (iter: Iterator<T>): TTuple<U, Iterator<T>> => {
      const cur = iter.next();
      if (!cur.done) {
        const y = f(cur.value);
        return y != null ? Tuple(y, iter) : trySkipToNext(iter);
      }
    };
    return Seq.delay(() =>
      Seq.unfold((iter) =>
        trySkipToNext(iter), xs[Symbol.iterator]()));
  };

  static compareWith<T>(f: (x: T, y: T) => number, xs: Iterable<T>, ys: Iterable<T>) {
    let nonZero = Seq.tryFind((i: number) => i != 0, Seq.map2((x: T, y: T) => f(x, y), xs, ys));
    return nonZero != null ? nonZero : Seq.count(xs) - Seq.count(ys);
  };

  static delay<T>(f: () => Iterable<T>) {
    return <Iterable<T>>{
      [Symbol.iterator]: () => f()[Symbol.iterator]()
    };
  };

  static distinctBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
    return Seq.choose(
      (tup) => tup[0],
      Seq.scan((tup, x) => {
        const acc = tup[1];
        const k = f(x);
        return acc.has(k) ? Tuple(<T>null, acc) : Tuple(x, acc.add(k));
      }, Tuple(<T>null, new Set<K>()), xs));
  };

  static distinct<T>(xs: Iterable<T>) {
    return Seq.distinctBy((x) => x, xs);
  };

  static empty<T>() {
    return Seq.unfold((): TTuple<T, T> => { return void 0 });
  };

  static enumerateWhile<T>(cond: () => boolean, xs: Iterable<T>) {
    return Seq.concat(Seq.unfold(() => cond() ? [xs, true] : null));
  };

  static enumerateThenFinally<T>(xs: Iterable<T>, finalFn: () => void) {
    return Seq.delay(() => {
      let iter: Iterator<T>;
      try {
        iter = xs[Symbol.iterator]();
      } finally {
        finalFn();
      }
      return Seq.unfold((iter) => {
        try {
          const cur = iter.next();
          return !cur.done ? [cur.value, iter] : null;
        } finally {
          finalFn();
        }
      }, iter);
    });
  };

  static enumerateUsing<T extends Disposable, U>(disp: T, work: (x: T) => Iterable<U>) {
    let isDisposed = false;
    const disposeOnce = () => {
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

  static exactlyOne<T>(xs: Iterable<T>) {
    const iter = xs[Symbol.iterator]();
    const fst = iter.next();
    if (fst.done)
      throw "Seq was empty";

    const snd = iter.next();
    if (!snd.done)
      throw "Seq had multiple items";

    return fst.value;
  };

  static exists<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    function aux(iter: Iterator<T>): boolean {
      const cur = iter.next();
      return !cur.done && (f(cur.value) || aux(iter));
    };
    return aux(xs[Symbol.iterator]());
  };

  static exists2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
    function aux(iter1: Iterator<T1>, iter2: Iterator<T2>): boolean {
      const cur1 = iter1.next(), cur2 = iter2.next();
      return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
    };
    return aux(xs[Symbol.iterator](), ys[Symbol.iterator]());
  };

  static filter<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    function trySkipToNext(iter: Iterator<T>): TTuple<T, Iterator<T>> {
      const cur = iter.next();
      if (!cur.done) {
        return f(cur.value) ? [cur.value, iter] : trySkipToNext(iter);
      }
    };
    return Seq.delay(() => Seq.unfold(trySkipToNext, xs[Symbol.iterator]()));
  };

  static where<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.filter(f, xs);
  };

  static fold<T, ST>(f: (acc: ST, x: T, i?: number) => ST, acc: ST, xs: Iterable<T>) {
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

  static foldBack<T, ST>(f: (x: T, acc: ST, i?: number) => ST, xs: Iterable<T>, acc: ST) {
    const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as Array<T> : Array.from(xs);
    for (let i = arr.length - 1; i >= 0; i--) {
      acc = f(arr[i], acc, i);
    }
    return acc;
  }

  static fold2<T1, T2, ST>(f: (acc: ST, x: T1, y: T2, i?: number) => ST, acc: ST, xs: Iterable<T1>, ys: Iterable<T2>) {
    const iter1 = xs[Symbol.iterator](), iter2 = ys[Symbol.iterator]();
    let cur1: IteratorResult<T1>, cur2: IteratorResult<T2>;
    for (let i = 0; ; i++) {
      cur1 = iter1.next();
      cur2 = iter2.next();
      if (cur1.done || cur2.done) {
        break;
      }
      acc = f(acc, cur1.value, cur2.value, i);
    }
    return acc;
  }

  static foldBack2<T1, T2, ST>(f: (x: T1, y: T2, acc: ST, i?: number) => ST, xs: Iterable<T1>, ys: Iterable<T2>, acc: ST) {
    const ar1: Array<T1> = Array.isArray(xs) || ArrayBuffer.isView(xs) ? <Array<T1>>xs : Array.from(xs);
    const ar2: Array<T2> = Array.isArray(ys) || ArrayBuffer.isView(ys) ? <Array<T2>>ys : Array.from(ys);
    for (let i = ar1.length - 1; i >= 0; i--) {
      acc = f(ar1[i], ar2[i], acc, i);
    }
    return acc;
  };

  static forall<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.fold((acc, x) => acc && f(x), true, xs);
  };

  static forall2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.fold2((acc, x, y) => acc && f(x, y), true, xs, ys);
  };

  // TODO: Should return a Iterable<Tuple<K, Iterable<T>>> instead of a Map<K, Iterable<T>>
  // Seq.groupBy : ('T -> 'Key) -> seq<'T> -> seq<'Key * seq<'T>>
  static groupBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
    return Seq.fold((acc, x) => {
      const k = f(x), vs = acc.get(k);
      return vs != null ? acc.set(k, new List(x, <List<T>>vs)) : acc.set(k, List.singleton(x));
    }, new Map<K, Iterable<T>>(), xs);
  };

  static tryHead<T>(xs: Iterable<T>) {
    const iter = xs[Symbol.iterator]();
    const cur = iter.next();
    return cur.done ? null : cur.value;
  };

  static head<T>(xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryHead(xs));
  };

  static init<T>(n: number, f: (i: number) => T) {
    return Seq.delay(() =>
      Seq.unfold((i) => i < n ? [f(i), i + 1] : null, 0));
  };

  static initInfinite<T>(f: (i: number) => T) {
    return Seq.delay(() =>
      Seq.unfold((i) => [f(i), i + 1], 0));
  };

  static tryItem<T>(i: number, xs: Iterable<T>) {
    if (i < 0)
      return null;

    if (Array.isArray(xs) || ArrayBuffer.isView(xs))
      return i < (<Array<T>>xs).length ? (<Array<T>>xs)[i] : null;

    for (let j = 0, iter = xs[Symbol.iterator](); ; j++) {
      const cur = iter.next();
      if (cur.done)
        return null;

      if (j === i)
        return cur.value;
    }
  };

  static item<T>(i: number, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryItem(i, xs));
  };

  static iter<T>(f: (x: T) => void, xs: Iterable<T>) {
    Seq.fold((_, x) => f(x), null, xs);
  };

  static iter2<T1, T2>(f: (x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
    Seq.fold2((_, x, y) => f(x, y), null, xs, ys);
  };

  static iteri<T>(f: (i: number, x: T) => void, xs: Iterable<T>) {
    Seq.fold((_, x, i) => f(i, x), null, xs);
  };

  static iteri2<T1, T2>(f: (i: number, x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
    Seq.fold2((_, x, y, i) => f(i, x, y), null, xs, ys);
  };

  static isEmpty<T>(xs: Iterable<T>) {
    const i = xs[Symbol.iterator]();
    return i.next().done;
  };

  static tryLast<T>(xs: Iterable<T>) {
    try {
      return Seq.reduce((_, x) => x, xs);
    }
    catch (err) {
      return null;
    }
  };

  static last<T>(xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryLast(xs));
  };

  // A static 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
  static count<T>(xs: Iterable<T>) {
    return Array.isArray(xs) || ArrayBuffer.isView(xs)
      ? (xs as Array<T>).length
      : Seq.fold((acc, x) => acc + 1, 0, xs);
  };

  static map<T, U>(f: (x: T) => U, xs: Iterable<T>) {
    return Seq.delay(() => Seq.unfold((iter) => {
      const cur = iter.next();
      return !cur.done ? [f(cur.value), iter] : null;
    }, xs[Symbol.iterator]()));
  };

  static mapi<T, U>(f: (i: number, x: T) => U, xs: Iterable<T>) {
    return Seq.delay(() => {
      let i = 0;
      return Seq.unfold((iter) => {
        const cur = iter.next();
        return !cur.done ? [f(i++, cur.value), iter] : null;
      }, xs[Symbol.iterator]());
    });
  };

  static map2<T1, T2, U>(f: (x: T1, y: T2) => U, xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.delay(() => {
      const iter1 = xs[Symbol.iterator]();
      const iter2 = ys[Symbol.iterator]();
      return Seq.unfold(() => {
        const cur1 = iter1.next(), cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
      });
    });
  };

  static mapi2<T1, T2, U>(f: (i: number, x: T1, y: T2) => U, xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.delay(() => {
      let i = 0;
      const iter1 = xs[Symbol.iterator]();
      const iter2 = ys[Symbol.iterator]();
      return Seq.unfold(() => {
        const cur1 = iter1.next(), cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
      });
    });
  };

  static map3<T1, T2, T3, U>(f: (x: T1, y: T2, z: T3) => U, xs: Iterable<T1>, ys: Iterable<T2>, zs: Iterable<T3>) {
    return Seq.delay(() => {
      const iter1 = xs[Symbol.iterator]();
      const iter2 = ys[Symbol.iterator]();
      const iter3 = zs[Symbol.iterator]();
      return Seq.unfold(() => {
        const cur1 = iter1.next(), cur2 = iter2.next(), cur3 = iter3.next();
        return !cur1.done && !cur2.done && !cur3.done ? [f(cur1.value, cur2.value, cur3.value), null] : null;
      });
    });
  };

  static max<T extends number>(xs: Iterable<T>) {
    return Seq.reduce((acc: T, x: T) => Math.max(acc, x), xs);
  };

  static maxBy<T, U extends number>(f: (x: T) => U, xs: Iterable<T>) {
    return Seq.reduce((x, y) => f(x) > f(y) ? x : y, xs);
  };

  static min<T extends number>(xs: Iterable<T>) {
    return Seq.reduce((acc: T, x: T) => Math.min(acc, x), xs);
  };

  static minBy<T, U extends number>(f: (x: T) => U, xs: Iterable<T>) {
    return Seq.reduce((x, y) => f(x) < f(y) ? x : y, xs);
  };

  static pairwise<T extends number>(xs: Iterable<T>) {
    return Seq.skip(1, Seq.scan((last, next) => Tuple(last[1], next), Tuple(0, 0), xs));
  };

  static permute<T>(f: (i: number) => number, xs: Iterable<T>) {
    return Seq.ofArray(FArray.permute(f, Array.from(xs)));
  };

  static rangeStep(first: number, step: number, last: number) {
    if (step === 0)
      throw "Step cannot be 0";
    return Seq.unfold((x) => step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null, first);
  };

  static rangeChar(first: string, last: string) {
    return Seq.unfold((x) => x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null, first);
  };

  static range(first: number, last: number) {
    return Seq.rangeStep(first, 1, last);
  };

  static readonly<T>(xs: Iterable<T>) {
    return Seq.map((x) => x, xs);
  };

  static reduce<T>(f: (acc: T, x: T) => T, xs: Iterable<T>) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs))
      return (<Array<T>>xs).reduce(f);

    const iter = xs[Symbol.iterator]();
    let cur = iter.next();
    if (cur.done)
      throw "Seq was empty";

    let acc = cur.value;
    for (; ;) {
      cur = iter.next();
      if (cur.done) {
        break;
      }
      acc = f(acc, cur.value);
    }
    return acc;
  };

  static reduceBack<T>(f: (acc: T, x: T, i?: number) => T, xs: Iterable<T>) {
    const ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? <Array<T>>xs : Array.from(xs);
    if (ar.length === 0)
      throw "Seq was empty";

    let acc = ar[ar.length - 1];
    for (let i = ar.length - 2; i >= 0; i--)
      acc = f(ar[i], acc, i);

    return acc;
  };

  static replicate<T>(n: number, x: T) {
    return Seq.init(n, () => x);
  };

  static rev<T>(xs: Iterable<T>) {
    const ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? (<Array<T>>xs).slice(0) : Array.from(xs);
    return Seq.ofArray(ar.reverse());
  };

  static scan<T, ST>(f: (st: ST, x: T) => ST, seed: ST, xs: Iterable<T>) {
    return Seq.delay(() => {
      const iter = xs[Symbol.iterator]();
      return Seq.unfold((acc) => {
        if (acc == null)
          return [seed, seed];

        const cur = iter.next();
        if (!cur.done) {
          acc = f(acc, cur.value);
          return [acc, acc];
        }
      }, <ST>null);
    });
  };

  static scanBack<T, ST>(f: (x: T, st: ST) => ST, xs: Iterable<T>, seed: ST) {
    return Seq.rev(Seq.scan((acc, x) => f(x, acc), seed, Seq.rev(xs)));
  };

  static singleton<T>(x: T) {
    return Seq.unfold((x) => x != null ? [x, null] : null, x);
  };

  static skip<T>(n: number, xs: Iterable<T>) {
    return <Iterable<T>>{
      [Symbol.iterator]: () => {
        const iter = xs[Symbol.iterator]();
        for (let i = 1; i <= n; i++)
          if (iter.next().done)
            throw "Seq has not enough elements";
        return iter;
      }
    };
  };

  static skipWhile<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.delay(() => {
      let hasPassed = false;
      return Seq.filter((x) => hasPassed || (hasPassed = !f(x)), xs);
    });
  };

  static sortWith<T>(f: (x: T, y: T) => number, xs: Iterable<T>) {
    const ys = Array.from(xs);
    return Seq.ofArray(ys.sort(f));
  };

  private static defaultAdder(x: number, y: number) {
    return x + y;
  }

  static sum(xs: Iterable<number>, adder?: (x: number, y: number) => number) {
    adder = adder || Seq.defaultAdder;
    return Seq.reduce((acc, x) => adder(acc, x), xs);
  };

  static sumBy(f: (x: number) => number, xs: Iterable<number>, adder?: (x: number, y: number) => number) {
    let fst = true;
    adder = adder || Seq.defaultAdder;
    return Seq.reduce((acc, x) => {
      acc = fst ? f(acc) : acc, fst = false;
      return adder(acc, f(x));
    }, xs);
  };

  static tail<T>(xs: Iterable<T>) {
    const iter = xs[Symbol.iterator]();
    const cur = iter.next();
    if (cur.done)
      throw "Seq was empty";

    return <Iterable<T>>{
      [Symbol.iterator]: () => iter
    };
  };

  static take<T>(n: number, xs: Iterable<T>, truncate: boolean = false) {
    return Seq.delay(() => {
      const iter = xs[Symbol.iterator]();
      return Seq.unfold((i) => {
        if (i < n) {
          const cur = iter.next();
          if (!cur.done)
            return [cur.value, i + 1];
          if (!truncate)
            throw "Seq has not enough elements";
        }
      }, 0);
    });
  };

  static truncate<T>(n: number, xs: Iterable<T>) {
    return Seq.take(n, xs, true);
  };

  static takeWhile<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.delay(() => {
      const iter = xs[Symbol.iterator]();
      return Seq.unfold((i) => {
        const cur = iter.next();
        if (!cur.done && f(cur.value))
          return [cur.value, null];
      }, 0);
    });
  };

  static tryFind<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return null;
      if (f(cur.value, i))
        return cur.value;
    }
  };

  static find<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFind(f, xs));
  };

  static tryFindBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    let match = <T>null;
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return match;
      if (f(cur.value, i))
        match = cur.value;
    }
  };

  static findBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFindBack(f, xs));
  };

  static tryFindIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return null;
      if (f(cur.value, i))
        return i;
    }
  };

  static findIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFindIndex(f, xs));
  };

  static tryFindIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    let match = 0;
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return match;
      if (f(cur.value, i))
        match = i;
    }
  };

  static findIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFindIndexBack(f, xs));
  };

  static tryPick<T, U>(f: (x: T, i?: number) => U, xs: Iterable<T>) {
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        break;
      const y = f(cur.value, i);
      if (y != null) {
        return y;
      }
    }
  };

  static pick<T, U>(f: (x: T, i?: number) => U, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryPick(f, xs));
  };

  static unfold<T, ST>(f: (st: ST) => TTuple<T, ST>, acc?: ST) {
    return <Iterable<T>>{
      [Symbol.iterator]: () => {
        return {
          next: () => {
            const res = f(acc);
            if (res != null) {
              acc = res[1];
              return { done: false, value: res[0] };
            }
            return { done: true };
          }
        };
      }
    };
  };

  static zip<T1, T2>(xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.map2((x, y) => [x, y], xs, ys);
  };

  static zip3<T1, T2, T3>(xs: Iterable<T1>, ys: Iterable<T2>, zs: Iterable<T3>) {
    return Seq.map3((x, y, z) => [x, y, z], xs, ys, zs);
  };
}

class FSet {
  static union<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.fold((acc, x) => { acc.add(x); return acc; }, new Set(set1), set2);
  };
  static op_Addition = FSet.union;

  static unionMany<T>(sets: Iterable<Set<T>>) {
    return Seq.fold((acc, s) => FSet.union(acc, s), new Set<T>(), sets);
  };

  static difference<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.fold((acc, x) => { acc.delete(x); return acc; }, new Set(set1), set2);
  };
  static op_Subtraction = FSet.difference;

  static intersect<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.fold((acc, x) => {
      if (!set2.has(x))
        acc.delete(x);
      return acc;
    }, new Set(set1), set1);
  };

  static intersectMany<T>(sets: Iterable<Set<T>>) {
    const ar = Array.isArray(sets) ? <Array<Set<T>>>sets : Array.from(sets);
    if (ar.length == 0)
      throw "Seq was empty";

    const set = new Set<T>(ar[0]);
    Seq.iter((x: T) => {
      for (let i = 1; i < ar.length; i++) {
        if (!ar[i].has(x)) {
          set.delete(x);
          break;
        }
      }
    }, ar[0]);
    return set;
  };

  static isProperSubsetOf<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.forall((x) => set2.has(x), set1) && Seq.exists((x) => !set1.has(x), set2);
  };
  static isProperSubset = FSet.isProperSubsetOf;

  static isSubsetOf<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.forall((x) => set2.has(x), set1);
  };
  static isSubset = FSet.isSubsetOf;

  static isProperSupersetOf<T>(set1: Set<T>, set2: Set<T>) {
    return FSet.isProperSubset(set2, set1);
  };
  static isProperSuperset = FSet.isProperSupersetOf;

  static isSupersetOf<T>(set1: Set<T>, set2: Set<T>) {
    return FSet.isSubset(set2, set1);
  };
  static isSuperset = FSet.isSupersetOf;

  static copyTo<T>(xs: Set<T>, arr: ArrayLike<T>, arrayIndex?: number, count?: number) {
    if (!Array.isArray(arr) && !ArrayBuffer.isView(arr))
      throw "Array is invalid";

    count = count || arr.length;
    let i = arrayIndex || 0;
    const iter = xs[Symbol.iterator]();
    while (count--) {
      const el = iter.next();
      if (el.done) break;
      arr[i++] = el.value;
    };
  };

  static partition<T>(f: (x: T) => boolean, xs: Set<T>) {
    return Seq.fold((acc, x) => {
      const lacc = acc[0], racc = acc[1];
      return f(x) ? Tuple(lacc.add(x), racc) : Tuple(lacc, racc.add(x));
    }, Tuple(new Set<T>(), new Set<T>()), xs);
  };

  static remove<T>(item: T, xs: Set<T>) {
    return FSet.removeInPlace(item, new Set(xs));
  };

  static removeInPlace<T>(item: T, xs: Set<T>) {
    xs.delete(item);
    return xs;
  };
}
export { FSet as Set };

class FMap {
  static containsValue<K, V>(v: V, map: Map<K, V>) {
    return Seq.fold((acc, k) => acc || map.get(k) === v, false, map.keys());
  };

  static exists<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V>) {
    return Seq.exists((kv) => f(kv[0], kv[1]), map);
  };

  static filter<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V>) {
    return Seq.fold((acc, kv) => f(kv[0], kv[1]) ? acc.set(kv[0], kv[1]) : acc, new Map<K, V>(), map);
  };

  static fold<K, V, ST>(f: (acc: ST, k: K, v: V) => ST, seed: ST, map: Map<K, V>) {
    return Seq.fold((acc, kv) => f(acc, kv[0], kv[1]), seed, map);
  };

  static foldBack<K, V, ST>(f: (k: K, v: V, acc: ST) => ST, map: Map<K, V>, seed: ST) {
    return Seq.foldBack((kv, acc) => f(kv[0], kv[1], acc), map, seed);
  };

  static forall<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V>) {
    return Seq.forall((kv) => f(kv[0], kv[1]), map);
  };

  static iter<K, V>(f: (k: K, v: V) => void, map: Map<K, V>) {
    return Seq.iter((kv) => f(kv[0], kv[1]), map);
  };

  static map<K, T, U>(f: (k: K, v: T) => U, map: Map<K, T>) {
    return Seq.fold((acc, kv) => acc.set(kv[0], f(kv[0], kv[1])), new Map<K, U>(), map);
  };

  static partition<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V>) {
    return Seq.fold((acc, kv) => {
      const lacc = acc[0], racc = acc[1];
      const k = kv[0], v = kv[1];
      return f(k, v) ? Tuple(lacc.set(k, v), racc) : Tuple(lacc, racc.set(k, v));
    }, Tuple(new Map<K, V>(), new Map<K, V>()), map);
  };

  static findKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V>) {
    return Seq.pick((kv) => f(kv[0], kv[1]) ? kv[0] : null, map);
  };

  static tryFindKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V>) {
    return Seq.tryPick((kv) => f(kv[0], kv[1]) ? kv[0] : null, map);
  };

  static pick<K, T, U>(f: (k: K, v: T) => U, map: Map<K, T>) {
    return Seq.pick((kv) => {
      const res = f(kv[0], kv[1]);
      return res != null ? res : null;
    }, map);
  };

  static remove<K, V>(item: K, map: Map<K, V>) {
    return FMap.removeInPlace(item, new Map<K, V>(map));
  };

  static removeInPlace<K, V>(item: K, map: Map<K, V>) {
    map.delete(item);
    return map;
  };

  static tryPick<K, T, U>(f: (k: K, v: T) => U, map: Map<K, T>) {
    return Seq.tryPick((kv) => {
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

export class Lazy<T> {
  public factory: () => T;
  public isValueCreated: boolean;

  private createdValue: T;

  constructor(factory: () => T) {
    this.factory = factory;
    this.isValueCreated = false;
  };

  static createFromValue<T>(v: T) {
    return new Lazy(() => v);
  };

  get value() {
    if (!this.isValueCreated) {
      this.createdValue = this.factory();
      this.isValueCreated = true;
    }
    return this.createdValue;
  }
}
