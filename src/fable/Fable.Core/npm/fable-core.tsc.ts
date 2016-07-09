//
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
        lengthComp = Util.compareTo(Seq.length(x), Seq.length(y));
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

export class TimeSpan {
  static create = function (d: number = 0, h: number = 0, m: number = 0, s: number = 0, ms: number = 0): number {
    switch (arguments.length) {
      case 1:
        // ticks
        return arguments[0] / 10000;
      case 3:
        // h,m,s
        h = arguments[0], m = arguments[1], s = arguments[2];
        break;
      default:
        // d,h,m,s,ms
        d = arguments[0], h = arguments[1], m = arguments[2], s = arguments[3], ms = arguments[4] || 0;
        break;
    }
    return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
  };
  static fromTicks = TimeSpan.create;

  static fromDays = function (d: number) {
    return TimeSpan.create(d, 0, 0, 0);
  };
  static fromHours = function (h: number) {
    return TimeSpan.create(h, 0, 0);
  };
  static fromMinutes = function (m: number) {
    return TimeSpan.create(0, m, 0);
  };
  static fromSeconds = function (s: number) {
    return TimeSpan.create(0, 0, s);
  };
  static days = function (ts: number) {
    return Math.floor(ts / 86400000);
  };
  static hours = function (ts: number) {
    return Math.floor(ts % 86400000 / 3600000);
  };
  static minutes = function (ts: number) {
    return Math.floor(ts % 3600000 / 60000);
  };
  static seconds = function (ts: number) {
    return Math.floor(ts % 60000 / 1000);
  };
  static milliseconds = function (ts: number) {
    return Math.floor(ts % 1000);
  };
  static ticks = function (ts: number) {
    return ts * 10000;
  };
  static totalDays = function (ts: number) {
    return ts / 86400000;
  };
  static totalHours = function (ts: number) {
    return ts / 3600000;
  };
  static totalMinutes = function (ts: number) {
    return ts / 60000;
  };
  static totalSeconds = function (ts: number) {
    return ts / 1000;
  };
  static duration = Math.abs;
  static negate = function (ts: number) {
    return -ts;
  };
  static add = function (ts1: number, ts2: number) {
    return ts1 + ts2;
  };
  static subtract = function (ts1: number, ts2: number) {
    return ts1 - ts2;
  };
  static compareTo = Util.compareTo;
  static compare = Util.compareTo;
}

export { FDate as Date };
class FDate {
  static __changeKind = function (d: any, kind: any) {
    var d2: any;
    return d.kind == kind ? d : (d2 = new Date(d.getTime()), d2.kind = kind, d2);
  };
  static __getValue = function (d: any, key: any) {
    return d.kind == 1 ? d['getUTC' + key]() : d['get' + key]();
  };
  static minValue = function () {
    return FDate.parse(-8640000000000000, 1);
  };
  static maxValue = function () {
    return FDate.parse(8640000000000000, 1);
  };
  static parse = function (v?: any, kind?: any): any {
    var date: any = v == null ? new Date() : new Date(v);
    if (isNaN(date.getTime())) {
      throw "The string is not a valid Date.";
    }
    date.kind = kind || 2; // Local
    return date;
  };
  static create = function (year: any, month: any, day: any, h: any, m: any, s: any, ms: any, kind: any) {
    h = h || 0, m = m || 0, s = s || 0, ms = ms || 0, kind = kind || 2;
    var date: any = kind === 1 // UTC
      ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms)) : new Date(year, month - 1, day, h, m, s, ms);
    if (isNaN(date.getTime())) {
      throw "The parameters describe an unrepresentable Date.";
    }
    date.kind = kind;
    return date;
  };
  static now = FDate.parse;
  static utcNow = function () {
    return FDate.parse(null, 1);
  };
  static today = function () {
    return FDate.date(FDate.now());
  };
  static isLeapYear = function (year: any) {
    return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
  };
  static daysInMonth = function (year: any, month: any) {
    if (month == 2) {
      return FDate.isLeapYear(year) ? 29 : 28;
    } else {
      return month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
    }
  };
  static toUniversalTime = function (d: any) {
    return FDate.__changeKind(d, 1);
  };
  static toLocalTime = function (d: any) {
    return FDate.__changeKind(d, 2);
  };
  static timeOfDay = function (d: any) {
    return TimeSpan.create(FDate.hour(d), FDate.minute(d), FDate.second(d));
  };
  static date = function (d: any) {
    return FDate.create(FDate.year(d), FDate.month(d), FDate.day(d), 0, 0, 0, 0, d.kind);
  };
  static day = function (d: any) {
    return FDate.__getValue(d, "Date");
  };
  static hour = function (d: any) {
    return FDate.__getValue(d, "Hours");
  };
  static millisecond = function (d: any) {
    return FDate.__getValue(d, "Milliseconds");
  };
  static minute = function (d: any) {
    return FDate.__getValue(d, "Minutes");
  };
  static month = function (d: any) {
    return FDate.__getValue(d, "Month") + 1;
  };
  static second = function (d: any) {
    return FDate.__getValue(d, "Seconds");
  };
  static year = function (d: any) {
    return FDate.__getValue(d, "FullYear");
  };
  static ticks = function (d: any) {
    return (d.getTime() + 6.2135604e+13 /* millisecondsJSOffset */) * 10000;
  };
  static toBinary = FDate.ticks;
  static dayOfWeek = function (d: any) {
    return FDate.__getValue(d, "Day");
  };
  static dayOfYear = function (d: any) {
    var year = FDate.year(d),
      month = FDate.month(d),
      day = FDate.day(d);
    for (var i = 1; i < month; i++) {
      day += FDate.daysInMonth(year, i);
    }
    return day;
  };
  static add = function (d: any, ts: any) {
    return FDate.parse(d.getTime() + ts, d.kind);
  };
  static op_Addition = FDate.add;
  static addDays = function (d: any, v: any) {
    return FDate.parse(d.getTime() + v * 86400000, d.kind);
  };
  static addHours = function (d: any, v: any) {
    return FDate.parse(d.getTime() + v * 3600000, d.kind);
  };
  static addMinutes = function (d: any, v: any) {
    return FDate.parse(d.getTime() + v * 60000, d.kind);
  };
  static addSeconds = function (d: any, v: any) {
    return FDate.parse(d.getTime() + v * 1000, d.kind);
  };
  static addMilliseconds = function (d: any, v: any) {
    return FDate.parse(d.getTime() + v, d.kind);
  };
  static addTicks = function (d: any, v: any) {
    return FDate.parse(d.getTime() + v / 10000, d.kind);
  };
  static addYears = function (d: any, v: any) {
    var newMonth = FDate.month(d),
      newYear = FDate.year(d) + v,
      daysInMonth = FDate.daysInMonth(newYear, newMonth),
      newDay = Math.min(daysInMonth, FDate.day(d));
    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), d.kind);
  };
  static addMonths = function (d: any, v: any) {
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
    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), d.kind);
  };
  static subtract = function (d: any, that: any) {
    return typeof that == "number" ? FDate.parse(d.getTime() - that, d.kind) : d.getTime() - that.getTime();
  };
  static op_Subtraction = FDate.subtract
  static toLongDateString = function (d: any) {
    return d.toDateString();
  };
  static toShortDateString = function (d: any) {
    return d.toLocaleDateString();
  };
  static toLongTimeString = function (d: any) {
    return d.toLocaleTimeString();
  };
  static toShortTimeString = function (d: any) {
    return d.toLocaleTimeString().replace(/:\d\d(?!:)/, '');
  };
  static equals = function (d1: any, d2: any) {
    return d1.getTime() == d2.getTime();
  };
  static compareTo = Util.compareTo;
  static compare = Util.compareTo;
}

class Timer {
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

class Event {
  // ToDo
}



// Types needed for unzip/unzip3 -- http://stackoverflow.com/a/32191614
export type TTuple<T1, T2> = [T1, T2];
export type TTuple3<T1, T2, T3> = [T1, T2, T3];

export class Seq {
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

  static length<T>(xs: Iterable<T>): number {
    return Array.isArray(xs) || ArrayBuffer.isView(xs) ? (xs as Array<T>).length : Seq.fold(function (acc, x) {
      return acc + 1;
    }, 0, xs);
  };

}

export class List<T> {
  public head: T;
  public tail: List<T>;

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

  get length(): number {
    return Seq.fold((acc, x) => acc + 1, 0, this);
  }

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
    return Seq.fold((acc, x) => f(x).append(acc), new List<U>(), List.rev(xs));
  }

  concat(xs: List<T>): List<T> {
    return List.concat(this);
  }

  static concat<T>(xs: List<T>): List<T> {
    return List.collect((x) => List.singleton(x), xs);
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
