const FSymbol = {
  interfaces: Symbol("interfaces"),
  typeName: Symbol("typeName")
};
export { FSymbol as Symbol }

export class Choice<T1, T2> {
  public Case: "Choice1Of2" | "Choice2Of2";
  public Fields: Array<T1 | T2>;

  constructor(t: "Choice1Of2" | "Choice2Of2", d: T1[] | T2[]) {
    this.Case = t;
    this.Fields = d;
  }

  static Choice1Of2<T1, T2>(v: T1) {
    return new Choice<T1, T2>("Choice1Of2", [v]);
  }

  static Choice2Of2<T1, T2>(v: T2) {
    return new Choice<T1, T2>("Choice2Of2", [v]);
  }

  get valueIfChoice1() {
    return this.Case === "Choice1Of2" ? <T1>this.Fields[0] : null;
  }

  get valueIfChoice2() {
    return this.Case === "Choice2Of2" ? <T2>this.Fields[0] : null;
  }
}

export type Tuple<T1, T2> = [T1, T2];
export type Tuple3<T1, T2, T3> = [T1, T2, T3];

export function Tuple<T1, T2>(x: T1, y: T2) {
  return <Tuple<T1, T2>>[x, y];
}

export function Tuple3<T1, T2, T3>(x: T1, y: T2, z: T3) {
  return <Tuple3<T1, T2, T3>>[x, y, z];
}

export class Util {
  private static __types = new Map<string, any>();

  // For legacy reasons the name is kept, but this method also adds
  // the type name to a cache. Use it after declaration:
  // Util.setInterfaces(Foo.prototype, ["IFoo", "IBar"], "MyModule.Foo");
  public static setInterfaces(proto: any, interfaces: string[], typeName?: string) {
    if (Array.isArray(interfaces) && interfaces.length > 0) {
      const currentInterfaces = proto[FSymbol.interfaces];
      if (Array.isArray(currentInterfaces)) {
        for (let i = 0; i < interfaces.length; i++)
          if (currentInterfaces.indexOf(interfaces[i]) == -1)
            currentInterfaces.push(interfaces[i]);
      } else
        proto[FSymbol.interfaces] = interfaces;
    }

    if (typeName) {
      proto[FSymbol.typeName] = typeName;
      Util.__types.set(typeName, proto.constructor);
    }
  }

  static hasInterface(obj: any, interfaceName: string) {
    return Array.isArray(obj[FSymbol.interfaces]) && obj[FSymbol.interfaces].indexOf(interfaceName) >= 0;
  }

  static getTypeFullName(cons: any): string {
    if (cons.prototype && cons.prototype[FSymbol.typeName]) {
      return cons.prototype[FSymbol.typeName];
    }
    else {
      return cons.name || "unknown";
    }
  }

  static getTypeNamespace(cons: any): string {
    const fullName = Util.getTypeFullName(cons);
    const i = fullName.lastIndexOf('.');
    return i > -1 ? fullName.substr(0, i) : "";
  } 

  static getTypeName(cons: any): string {
    const fullName = Util.getTypeFullName(cons);
    const i = fullName.lastIndexOf('.');
    return fullName.substr(i + 1);
  }

  static getRestParams(args: ArrayLike<any>, idx: number) {
    for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++)
      restArgs[_key - idx] = args[_key];
    return restArgs;
  }

  static equals(x: any, y: any): boolean {
    if (x == null) // Return true if both are null or undefined
      return y == null;
    else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y))
      return false;
    else if (Array.isArray(x) || ArrayBuffer.isView(x))
      return x.length != y.length
        ? false
        : Seq.fold2((prev, v1, v2) => !prev ? prev : Util.equals(v1, v2), true, x, y);
    else if (x instanceof Date)
      return FDate.equals(x, y);
    else if (Util.hasInterface(x, "System.IEquatable"))
      return x.Equals(y);
    else
      return x === y;
  }

  static compare(x: any, y: any): number {
    if (x == null) // Return 0 if both are null or undefined
      return y == null ? 0 : -1;
    else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y))
      return -1;
    else if (Array.isArray(x) || ArrayBuffer.isView(x))
      return x.length != y.length
        ? (x.length < y.length ? -1 : 1)
        : Seq.fold2((prev, v1, v2) => prev !== 0 ? prev : Util.compare(v1, v2), 0, x, y);
    else if (Util.hasInterface(x, "System.IComparable"))
      return x.CompareTo(y);
    else
      return x < y ? -1 : x > y ? 1 : 0;
  }

  static equalsRecords(x: any, y: any): boolean {
    const keys = Object.getOwnPropertyNames(x), keys2 = Object.getOwnPropertyNames(y);
    for (let i=0; i<keys.length; i++) {
      if (!Util.equals(x[keys[i]], y[keys[i]]))
        return false;
    }
    return true;
  }

  static compareRecords(x: any, y: any): number {
    const keys = Object.getOwnPropertyNames(x), keys2 = Object.getOwnPropertyNames(y);
    for (let i=0; i<keys.length; i++) {
      let res = Util.compare(x[keys[i]], y[keys[i]]);
      if (res !== 0)
        return res;
    }
    return 0;
  }

  static equalsUnions(x: any, y: any): boolean {
    if (x.Case !== y.Case)
      return false;
    for (let i=0; i<x.Fields.length; i++) {
      if (!Util.equals(x.Fields[i], y.Fields[i]))
        return false;
    }
    return true;
  }

  static compareUnions(x: any, y: any): number {
    if (x.Case !== y.Case)
      return -1;
    for (let i=0; i<x.Fields.length; i++) {
      let res = Util.compare(x.Fields[i], y.Fields[i]);
      if (res !== 0)
        return res;
    }
    return 0;
  }

  static createDisposable(f: () => void) {
    const disp: IDisposable = { Dispose: f };
    (<any>disp)[FSymbol.interfaces] = ["System.IDisposable"];
    return disp;
  }

  static createObj(fields: Iterable<Tuple<string, any>>) {
    return Seq.fold((acc, kv) => { acc[kv[0]] = kv[1]; return acc; }, <any>{}, fields);
  }

  static toPlainJsObj = function (source: any) {
    if (source != null && source.constructor != Object) {
      let target: { [index: string]: string } = {};
      let props = Object.getOwnPropertyNames(source);
      for (let i = 0; i < props.length; i++) {
        target[props[i]] = source[props[i]];
      }
      // Copy also properties from prototype, see #192
      const proto = Object.getPrototypeOf(source);
      if (proto != null) {
        props = Object.getOwnPropertyNames(proto);
        for (let i = 0; i < props.length; i++) {
          const prop = Object.getOwnPropertyDescriptor(proto, props[i]);
          if (prop.value) {
            target[props[i]] = prop.value;
          }
          else if (prop.get) {
            target[props[i]] = prop.get.apply(source);
          }
        }
      }
      return target;
    }
    else {
      return source;
    }
  }

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
  }

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
  }
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
  }

  static fromTicks(ticks: number) {
    return ticks / 10000;
  }

  static fromDays(d: number) {
    return TimeSpan.create(d, 0, 0, 0);
  }

  static fromHours(h: number) {
    return TimeSpan.create(h, 0, 0);
  }

  static fromMinutes(m: number) {
    return TimeSpan.create(0, m, 0);
  }

  static fromSeconds(s: number) {
    return TimeSpan.create(0, 0, s);
  }

  static days(ts: TimeSpan) {
    return Math.floor(<number>ts / 86400000);
  }

  static hours(ts: TimeSpan) {
    return Math.floor(<number>ts % 86400000 / 3600000);
  }

  static minutes(ts: TimeSpan) {
    return Math.floor(<number>ts % 3600000 / 60000);
  }

  static seconds(ts: TimeSpan) {
    return Math.floor(<number>ts % 60000 / 1000);
  }

  static milliseconds(ts: TimeSpan) {
    return Math.floor(<number>ts % 1000);
  }

  static ticks(ts: TimeSpan) {
    return <number>ts * 10000;
  }

  static totalDays(ts: TimeSpan) {
    return <number>ts / 86400000;
  }

  static totalHours(ts: TimeSpan) {
    return <number>ts / 3600000;
  }

  static totalMinutes(ts: TimeSpan) {
    return <number>ts / 60000;
  }

  static totalSeconds(ts: TimeSpan) {
    return <number>ts / 1000;
  }

  static negate(ts: TimeSpan) {
    return <number>ts * -1;
  }

  static add(ts1: TimeSpan, ts2: TimeSpan) {
    return <number>ts1 + <number>ts2;
  }

  static subtract(ts1: TimeSpan, ts2: TimeSpan) {
    return <number>ts1 - <number>ts2;
  }

  static compare = Util.compare;
  static compareTo = Util.compare;
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
  }

  private static __getValue(d: Date, key: string): number {
    return (<any>d)[((<FDate>d).kind == DateKind.UTC ? "getUTC" : "get") + key]();
  }

  static minValue() {
    return FDate.parse(-8640000000000000, 1);
  }

  static maxValue() {
    return FDate.parse(8640000000000000, 1);
  }

  static parse(v?: any, kind?: DateKind): any {
    const date = (v == null) ? new Date() : new Date(v);
    if (isNaN(date.getTime()))
      throw "The string is not a valid Date.";
    (<FDate>date).kind = kind || DateKind.Local;
    return date;
  }

  static create(year: number, month: number, day: number, h: number = 0, m: number = 0, s: number = 0, ms: number = 0, kind: DateKind = DateKind.Local) {
    const date: Date = (kind === DateKind.UTC)
      ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms))
      : new Date(year, month - 1, day, h, m, s, ms);
    if (isNaN(date.getTime()))
      throw "The parameters describe an unrepresentable Date.";
    (<FDate>date).kind = kind;
    return date;
  }

  static now = FDate.parse;

  static utcNow() {
    return FDate.parse(null, 1);
  }

  static today() {
    return FDate.date(FDate.now());
  }

  static isLeapYear(year: number) {
    return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
  }

  static daysInMonth(year: number, month: number) {
    return month == 2
      ? FDate.isLeapYear(year) ? 29 : 28
      : month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
  }

  static toUniversalTime(d: Date) {
    return FDate.__changeKind(d, 1);
  }

  static toLocalTime(d: Date) {
    return FDate.__changeKind(d, 2);
  }

  static timeOfDay(d: Date) {
    return TimeSpan.create(FDate.hour(d), FDate.minute(d), FDate.second(d));
  }

  static date(d: Date) {
    return FDate.create(FDate.year(d), FDate.month(d), FDate.day(d), 0, 0, 0, 0, (<FDate>d).kind);
  }

  static day(d: Date) {
    return FDate.__getValue(d, "Date");
  }

  static hour(d: Date) {
    return FDate.__getValue(d, "Hours");
  }

  static millisecond(d: Date) {
    return FDate.__getValue(d, "Milliseconds");
  }

  static minute(d: Date) {
    return FDate.__getValue(d, "Minutes");
  }

  static month(d: Date) {
    return FDate.__getValue(d, "Month") + 1;
  }

  static second(d: Date) {
    return FDate.__getValue(d, "Seconds");
  }

  static year(d: Date) {
    return FDate.__getValue(d, "FullYear");
  }

  static ticks(d: Date) {
    return (d.getTime() + 6.2135604e+13 /* millisecondsJSOffset */) * 10000;
  }

  static toBinary = FDate.ticks;

  static dayOfWeek(d: Date) {
    return FDate.__getValue(d, "Day");
  }

  static dayOfYear(d: Date) {
    const year = FDate.year(d);
    const month = FDate.month(d);
    let day = FDate.day(d);
    for (let i = 1; i < month; i++)
      day += FDate.daysInMonth(year, i);
    return day;
  }

  static add(d: Date, ts: TimeSpan) {
    return FDate.parse(d.getTime() + <number>ts, (<FDate>d).kind);
  }

  static addDays(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 86400000, (<FDate>d).kind);
  }

  static addHours(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 3600000, (<FDate>d).kind);
  }

  static addMinutes(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 60000, (<FDate>d).kind);
  }

  static addSeconds(d: Date, v: number) {
    return FDate.parse(d.getTime() + v * 1000, (<FDate>d).kind);
  }

  static addMilliseconds(d: Date, v: number) {
    return FDate.parse(d.getTime() + v, (<FDate>d).kind);
  }

  static addTicks(d: Date, v: number) {
    return FDate.parse(d.getTime() + v / 10000, (<FDate>d).kind);
  }

  static addYears(d: Date, v: number) {
    const newMonth = FDate.month(d);
    const newYear = FDate.year(d) + v;
    const daysInMonth = FDate.daysInMonth(newYear, newMonth);
    const newDay = Math.min(daysInMonth, FDate.day(d));
    return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), (<FDate>d).kind);
  }

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
  }

  static subtract(d: Date, that: Date | number) {
    return typeof that == "number"
      ? FDate.parse(d.getTime() - <number>that, (<FDate>d).kind)
      : d.getTime() - (<Date>that).getTime();
  }

  static toLongDateString(d: Date) {
    return d.toDateString();
  }

  static toShortDateString(d: Date) {
    return d.toLocaleDateString();
  }

  static toLongTimeString(d: Date) {
    return d.toLocaleTimeString();
  }

  static toShortTimeString(d: Date) {
    return d.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
  }

  static equals(d1: Date, d2: Date) {
    return d1.getTime() == d2.getTime();
  }

  static compareTo = Util.compare;
  static compare = Util.compare;
  static op_Addition = FDate.add;
  static op_Subtraction = FDate.subtract;
}
export { FDate as Date }

export interface IDisposable {
  Dispose(): void;
}

export class Timer implements IDisposable {
  public Interval: number;
  public AutoReset: boolean;
  
  private _elapsed: Event<Date>;
  private _enabled: boolean;
  private _isDisposed: boolean;
  private _intervalId: number;
  private _timeoutId: number;

  constructor(interval?: number) {
    this.Interval = interval > 0 ? interval : 100;
    this.AutoReset = true;
    this._elapsed = new Event<Date>();
  }

  get Elapsed() {
    return this._elapsed;
  }

  get Enabled() {
    return this._enabled;
  }

  set Enabled(x: boolean) {
    if (!this._isDisposed && this._enabled != x) {
      if (this._enabled = x) {
        if (this.AutoReset) {
          this._intervalId = setInterval(() => {
            if (!this.AutoReset)
              this.Enabled = false;
            this._elapsed.Trigger(new Date());
          }, this.Interval);
        } else {
          this._timeoutId = setTimeout(() => {
            this.Enabled = false;
            this._timeoutId = 0;
            if (this.AutoReset)
              this.Enabled = true;
            this._elapsed.Trigger(new Date());
          }, this.Interval);
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

  Dispose() {
    this.Enabled = false;
    this._isDisposed = true;
  }

  Close() {
    this.Dispose();
  }

  Start() {
    this.Enabled = true;
  }

  Stop() {
    this.Enabled = false;
  }
}
Util.setInterfaces(Timer.prototype, ["System.IDisposable"]);

class FString {
  private static fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;

  private static fsFormat(str: any) {
    function isObject(x: any) {
      return x !== null && typeof x === "object" && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
    }

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
        const plusPrefix = flags.indexOf("+") >= 0 && parseInt(rep) >= 0;
        if (!isNaN(pad = parseInt(pad))) {
          const ch = pad >= 0 && flags.indexOf("0") >= 0 ? "0" : " ";
          rep = FString.padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
        }
        return prefix + (plusPrefix ? "+" + rep : rep);
      });
    }

    function makeFn(str: any) {
      return (rep: any) => {
        const str2 = formatOnce(str, rep);
        return FString.fsFormatRegExp.test(str2)
          ? makeFn(str2) : _cont(str2.replace(/%%/g, "%"));
      };
    }

    let _cont: any;
    return (cont: any) => {
      _cont = cont;
      return FString.fsFormatRegExp.test(str) ? makeFn(str) : _cont(str);
    };
  }

  static formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;

  static format(str: string, ...args: any[]) {
    return str.replace(FString.formatRegExp, function (match: any, idx: any, pad: any, format: any) {
      let rep = args[idx];
      if (typeof rep === "number") {
        switch ((format || "").substring(0, 1)) {
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
              rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, ""); break;
          }
        }
        rep = format.replace(/\w+/g, function (match2: any) {
          let rep2 = match2;
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
      if (!isNaN(pad = parseInt((pad || "").substring(1)))) {
        rep = FString.padLeft(rep, Math.abs(pad), " ", pad < 0);
      }
      return rep;
    });
  }

  static endsWith(str: string, search: string) {
    const idx = str.lastIndexOf(search);
    return idx >= 0 && idx == str.length - search.length;
  }

  static initialize(n: number, f: (i: number) => string) {
    if (n < 0)
      throw "String length must be non-negative";

    const xs = new Array(n);
    for (let i = 0; i < n; i++)
      xs[i] = f(i);
    return xs.join("");
  }

  static isNullOrEmpty(str: string | any) {
    return typeof str !== "string" || str.length == 0;
  }

  static isNullOrWhiteSpace(str: string | any) {
    return typeof str !== "string" || /^\s*$/.test(str);
  }

  static join(delimiter: string, xs: ArrayLike<string>) {
    xs = typeof xs == "string" ? Util.getRestParams(arguments, 1) : xs;
    return (Array.isArray(xs) ? xs : Array.from(xs)).join(delimiter);
  }
  static concat = FString.join;

  static newGuid() {
    let uuid = "";
    for (let i = 0; i < 32; i++) {
      const random = Math.random() * 16 | 0;
      if (i === 8 || i === 12 || i === 16 || i === 20)
        uuid += "-";
      uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
    }
    return uuid;
  }

  static padLeft(str: any, len: number, ch?: string, isRight?: boolean) {
    ch = ch || " ";
    str = String(str);
    len = len - str.length;
    for (let i = -1; ++i < len;)
      str = isRight ? str + ch : ch + str;
    return str;
  }

  static padRight(str: any, len: number, ch?: string) {
    return FString.padLeft(str, len, ch, true);
  }

  static replace(str: string, search: string, replace: string) {
    return str.replace(new RegExp(FRegExp.escape(search), "g"), replace);
  }

  static replicate(n: number, x: string) {
    return FString.initialize(n, () => x);
  }

  static split(str: string, splitters: string[], count?: number, removeEmpty?: number) {
    count = typeof count == "number" ? count : null;
    removeEmpty = typeof removeEmpty == "number" ? removeEmpty : null;
    if (count < 0)
      throw "Count cannot be less than zero";
    if (count === 0)
      return [];
    splitters = Array.isArray(splitters) ? splitters : Util.getRestParams(arguments, 1);
    splitters = splitters.map(x => FRegExp.escape(x));
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
  }

  static trim(str: string, side: "start" | "end" | "both", ...chars: string[]) {
    if (side == "both" && chars.length == 0)
      return str.trim();

    if (side == "start" || side == "both") {
      const reg = chars.length == 0 ? /^\s+/ : new RegExp("^[" + FRegExp.escape(chars.join("")) + "]+");
      str = str.replace(reg, "");
    }
    if (side == "end" || side == "both") {
      const reg = chars.length == 0 ? /\s+$/ : new RegExp("[" + FRegExp.escape(chars.join("")) + "]+$");
      str = str.replace(reg, "");
    }
    return str;
  }
}
export { FString as String }

export type MatchEvaluator = (match: any) => string;

class FRegExp {
  static create(pattern: string, options: number) {
    let flags = "g";
    flags += options & 1 ? "i" : "";
    flags += options & 2 ? "m" : "";
    return new RegExp(pattern, flags);
  }

  // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
  static escape(str: string) {
    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
  }

  static unescape(str: string) {
    return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, "$1");
  }

  static isMatch(str: string | RegExp, pattern: string, options: number = 0) {
    var reg: RegExp = str instanceof RegExp
      ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
      : reg = FRegExp.create(pattern, options);
    return reg.test(<string>str);
  }

  static match(str: string | RegExp, pattern: string, options: number = 0) {
    var reg: RegExp = str instanceof RegExp
      ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
      : reg = FRegExp.create(pattern, options);
    return reg.exec(<string>str);
  }

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
  }

  static options(reg: RegExp) {
    let options = 256; // ECMAScript
    options |= reg.ignoreCase ? 1 : 0;
    options |= reg.multiline ? 2 : 0;
    return options;
  }

  static replace(reg: string | RegExp, input: string, replacement: string | MatchEvaluator, limit?: number, offset: number = 0) {
    function replacer() {
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
    }

    if (typeof reg == "string") {
      const tmp = <string>reg;
      reg = FRegExp.create(input, limit);
      input = tmp;
      limit = undefined;
    }
    if (typeof replacement == "function") {
      limit = limit == null ? -1 : limit;
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
  }

  static split(reg: string | RegExp, input: string, limit?: number, offset: number = 0) {
    if (typeof reg == "string") {
      const tmp = <string>reg;
      reg = FRegExp.create(input, limit);
      input = tmp;
      limit = undefined;
    }
    input = input.substring(offset);
    return input.split(<RegExp>reg, limit);
  }
}
export { FRegExp as RegExp }

class FArray {
  static addRangeInPlace<T>(range: Iterable<T>, xs: Array<T>) {
    Seq.iterate(x => xs.push(x), range);
  }

  static copyTo<T>(source: ArrayLike<T>, sourceIndex: number, target: ArrayLike<T>, targetIndex: number, count: number) {
    while (count--)
      target[targetIndex++] = source[sourceIndex++];
  }

  static partition<T>(f: (x: T) => boolean, xs: ArrayLike<T>) {
    let ys = <T[]>[], zs = <T[]>[], j = 0, k = 0;
    for (let i = 0; i < xs.length; i++)
      if (f(xs[i]))
        ys[j++] = xs[i];
      else
        zs[k++] = xs[i];
    return Tuple(ys, zs);
  }

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
  }

  static removeInPlace<T>(item: T, xs: Array<T>) {
    const i = xs.indexOf(item);
    if (i > -1) {
      xs.splice(i, 1);
      return true;
    }
    return false;
  }

  static setSlice<T>(target: any, lower: number, upper: number, source: ArrayLike<T>) {
    const length = (upper || target.length - 1) - lower;
    if (ArrayBuffer.isView(target) && source.length <= length)
      target.set(source, lower);
    else
      for (let i = lower | 0, j = 0; j <= length; i++ , j++)
        target[i] = source[j];
  }

  static sortInPlaceBy<T>(f: (x: T) => T, xs: Array<T>, dir: number = 1) {
    return xs.sort((x, y) => {
      x = f(x);
      y = f(y);
      return (x < y ? -1 : x == y ? 0 : 1) * dir;
    });
  }

  static unzip<T1, T2>(xs: ArrayLike<Tuple<T1, T2>>) {
    const bs = new Array<T1>(xs.length), cs = new Array<T2>(xs.length);
    for (let i = 0; i < xs.length; i++) {
      bs[i] = xs[i][0];
      cs[i] = xs[i][1];
    }
    return Tuple(bs, cs);
  }

  static unzip3<T1, T2, T3>(xs: ArrayLike<Tuple3<T1, T2, T3>>) {
    const bs = new Array<T1>(xs.length), cs = new Array<T2>(xs.length), ds = new Array<T3>(xs.length);
    for (let i = 0; i < xs.length; i++) {
      bs[i] = xs[i][0];
      cs[i] = xs[i][1];
      ds[i] = xs[i][2];
    }
    return Tuple3(bs, cs, ds);
  }
}
export { FArray as Array }

export class List<T> implements IEquatable<List<T>>, IComparable<List<T>>, Iterable<T> {
  public head: T;
  public tail: List<T>;

  constructor(head?: T, tail?: List<T>) {
    this.head = head;
    this.tail = tail;
  }

  Equals(x: List<T>) {
    const iter1 = this[Symbol.iterator](), iter2 = x[Symbol.iterator]();
    for (let i = 0; ; i++) {
      let cur1 = iter1.next(), cur2 = iter2.next();
      if (cur1.done)
        return cur2.done ? true : false;
      else if (cur2.done)
        return false;
      else if (!Util.equals(cur1.value, cur2.value))
        return false
    }
  }

  CompareTo(x: List<T>) {
    let acc = 0;
    const iter1 = this[Symbol.iterator](), iter2 = x[Symbol.iterator]();
    for (let i = 0; ; i++) {
      let cur1 = iter1.next(), cur2 = iter2.next();
      if (cur1.done)
        return cur2.done ? acc : -1;
      else if (cur2.done)
        return 1;
      else {
        acc = Util.compare(cur1.value, cur2.value);
        if (acc != 0) return acc;
      }
    }
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
        return { done: tmp.tail == null, value: tmp.head };
      }
    };
  }

  append(ys: List<T>): List<T> {
    return List.append(this, ys);
  }

  static append<T>(xs: List<T>, ys: List<T>) {
    return Seq.fold((acc, x) => new List<T>(x, acc), ys, List.reverse(xs));
  }

  choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
    return List.choose(f, this);
  }

  static choose<T, U>(f: (x: T) => U, xs: List<T>) {
    const r = Seq.fold((acc, x) => {
      const y = f(x);
      return y != null ? new List<U>(y, acc) : acc;
    }, new List<U>(), xs);

    return List.reverse(r);
  }

  collect<U>(f: (x: T) => List<U>): List<U> {
    return List.collect(f, this);
  }

  static collect<T, U>(f: (x: T) => List<U>, xs: List<T>) {
    return Seq.fold((acc, x) => acc.append(f(x)), new List<U>(), xs);
  }

  // TODO: should be xs: Iterable<List<T>>
  static concat<T>(xs: List<List<T>>) {
    return List.collect(x => x, xs);
  }

  filter(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static filter<T>(f: (x: T) => boolean, xs: List<T>) {
    return List.reverse(Seq.fold((acc, x) => f(x) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  where(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static where<T>(f: (x: T) => boolean, xs: List<T>) {
    return List.filter(f, xs);
  }

  static initialize<T>(n: number, f: (i: number) => T) {
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
    return List.reverse(Seq.fold((acc: List<U>, x: T) => new List<U>(f(x), acc), new List<U>(), xs));
  }

  mapIndexed<U>(f: (i: number, x: T) => U): List<U> {
    return List.mapIndexed(f, this);
  }

  static mapIndexed<T, U>(f: (i: number, x: T) => U, xs: List<T>) {
    return List.reverse(Seq.fold((acc, x, i) => new List<U>(f(i, x), acc), new List<U>(), xs));
  }

  partition(f: (x: T) => boolean): [List<T>, List<T>] {
    return List.partition(f, this);
  }

  static partition<T>(f: (x: T) => boolean, xs: List<T>) {
    return Seq.fold((acc, x) => {
      const lacc = acc[0], racc = acc[1];
      return f(x) ? Tuple(new List<T>(x, lacc), racc) : Tuple(lacc, new List<T>(x, racc));
    }, Tuple(new List<T>(), new List<T>()), List.reverse(xs));
  }

  static replicate<T>(n: number, x: T) {
    return List.initialize(n, () => x);
  }

  reverse(): List<T> {
    return List.reverse(this);
  }

  static reverse<T>(xs: List<T>) {
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
    return List.reverse(Seq.fold((acc, x, i) => (noLower || lower <= i) && (noUpper || i <= upper) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  /* ToDo: instance unzip() */

  static unzip<T1, T2>(xs: List<Tuple<T1, T2>>) {
    return Seq.foldBack((xy, acc) =>
      Tuple(new List<T1>(xy[0], acc[0]), new List<T2>(xy[1], acc[1])), xs, Tuple(new List<T1>(), new List<T2>()));
  }

  /* ToDo: instance unzip3() */

  static unzip3<T1, T2, T3>(xs: List<Tuple3<T1, T2, T3>>) {
    return Seq.foldBack((xyz, acc) =>
      Tuple3(new List<T1>(xyz[0], acc[0]), new List<T2>(xyz[1], acc[1]), new List<T3>(xyz[2], acc[2])), xs, Tuple3(new List<T1>(), new List<T2>(), new List<T3>()));
  }
}
Util.setInterfaces(List.prototype, ["System.IEquatable", "System.IComparable"], "Microsoft.FSharp.Collections.FSharpList"); 

export class Seq {
  private static __failIfNone<T>(res: T) {
    if (res == null)
      throw "Seq did not contain any matching element";
    return res;
  }

  static toList<T>(xs: Iterable<T>) {
    return Seq.foldBack((x, acc) =>
      new List(x, acc), xs, new List<T>());
  }

  static ofList<T>(xs: List<T>) {
    return Seq.delay(() =>
      Seq.unfold(x =>
        x.tail != null ? [x.head, x.tail] : null, xs));
  }

  static ofArray<T>(xs: ArrayLike<T>) {
    return Seq.delay(() =>
      Seq.unfold(i =>
        i < xs.length ? [xs[i], i + 1] : null, 0));
  }

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
  }

  static average(xs: Iterable<number>) {
    let count = 1;
    const sum = Seq.reduce((acc: number, x: number) => {
      count++;
      return acc + x;
    }, xs);
    return sum / count;
  }

  static averageBy(f: (a: number) => number, xs: Iterable<number>) {
    let count = 1;
    const sum = Seq.reduce((acc: number, x: number) => {
      count++;
      return (count === 2 ? f(acc) : acc) + f(x);
    }, xs);
    return sum / count;
  }

  static countBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
    return Seq.map(kv => Tuple(kv[0], Seq.count(kv[1])), Seq.groupBy(f, xs));
  }

  static concat<T>(xs: Iterable<Iterable<T>>) {
    return Seq.delay(() => {
      let iter = xs[Symbol.iterator]();
      let output: T = null;
      return Seq.unfold(innerIter => {
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
  }

  static collect<T, U>(f: (x: T) => Iterable<U>, xs: Iterable<T>) {
    return Seq.concat(Seq.map(f, xs));
  }

  static choose<T, U>(f: (x: T) => U, xs: Iterable<T>) {
    const trySkipToNext = (iter: Iterator<T>): Tuple<U, Iterator<T>> => {
      const cur = iter.next();
      if (!cur.done) {
        const y = f(cur.value);
        return y != null ? Tuple(y, iter) : trySkipToNext(iter);
      }
      return void 0;
    };
    return Seq.delay(() =>
      Seq.unfold(iter =>
        trySkipToNext(iter), xs[Symbol.iterator]()));
  }

  static compareWith<T>(f: (x: T, y: T) => number, xs: Iterable<T>, ys: Iterable<T>) {
    let nonZero = Seq.tryFind((i: number) => i != 0, Seq.map2((x: T, y: T) => f(x, y), xs, ys));
    return nonZero != null ? nonZero : Seq.count(xs) - Seq.count(ys);
  }

  static delay<T>(f: () => Iterable<T>) {
    return <Iterable<T>>{
      [Symbol.iterator]: () => f()[Symbol.iterator]()
    };
  }

  static distinctBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
    return Seq.choose(
      tup => tup[0],
      Seq.scan((tup, x) => {
        const acc = tup[1];
        const k = f(x);
        return acc.has(k) ? Tuple(<T>null, acc) : Tuple(x, acc.add(k));
      }, Tuple(<T>null, new Set<K>()), xs));
  }

  static distinct<T>(xs: Iterable<T>) {
    return Seq.distinctBy(x => x, xs);
  }

  static empty<T>() {
    return Seq.unfold((): Tuple<T, T> => { return void 0; });
  }

  static enumerateWhile<T>(cond: () => boolean, xs: Iterable<T>) {
    return Seq.concat(Seq.unfold(() => cond() ? [xs, true] : null));
  }

  static enumerateThenFinally<T>(xs: Iterable<T>, finalFn: () => void) {
    return Seq.delay(() => {
      let iter: Iterator<T>;
      try {
        iter = xs[Symbol.iterator]();
      } finally {
        finalFn();
      }
      return Seq.unfold(iter => {
        try {
          const cur = iter.next();
          return !cur.done ? [cur.value, iter] : null;
        } finally {
          finalFn();
        }
        return void 0;
      }, iter);
    });
  }

  static enumerateUsing<T extends IDisposable, U>(disp: T, work: (x: T) => Iterable<U>) {
    let isDisposed = false;
    const disposeOnce = () => {
      if (!isDisposed) {
        isDisposed = true;
        disp.Dispose();
      }
    };
    try {
      return Seq.enumerateThenFinally(work(disp), disposeOnce);
    } finally {
      disposeOnce();
    }
    return void 0;
  }

  static exactlyOne<T>(xs: Iterable<T>) {
    const iter = xs[Symbol.iterator]();
    const fst = iter.next();
    if (fst.done)
      throw "Seq was empty";

    const snd = iter.next();
    if (!snd.done)
      throw "Seq had multiple items";

    return fst.value;
  }

  static exists<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    function aux(iter: Iterator<T>): boolean {
      const cur = iter.next();
      return !cur.done && (f(cur.value) || aux(iter));
    }
    return aux(xs[Symbol.iterator]());
  }

  static exists2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
    function aux(iter1: Iterator<T1>, iter2: Iterator<T2>): boolean {
      const cur1 = iter1.next(), cur2 = iter2.next();
      return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
    }
    return aux(xs[Symbol.iterator](), ys[Symbol.iterator]());
  }

  static filter<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    function trySkipToNext(iter: Iterator<T>): Tuple<T, Iterator<T>> {
      const cur = iter.next();
      if (!cur.done)
        return f(cur.value) ? [cur.value, iter] : trySkipToNext(iter);
      return void 0;
    }
    return Seq.delay(() => Seq.unfold(trySkipToNext, xs[Symbol.iterator]()));
  }

  static where<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.filter(f, xs);
  }

  static fold<T, ST>(f: (acc: ST, x: T, i?: number) => ST, acc: ST, xs: Iterable<T>) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return (xs as Array<T>).reduce(f, acc);
    } else {
      let cur: IteratorResult<T>;
      for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
        cur = iter.next();
        if (cur.done)
          break;

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
  }

  static forAll<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.fold((acc, x) => acc && f(x), true, xs);
  }

  static forAll2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.fold2((acc, x, y) => acc && f(x, y), true, xs, ys);
  }

  // TODO: Should return a Iterable<Tuple<K, Iterable<T>>> instead of a Map<K, Iterable<T>>
  // Seq.groupBy : ('T -> 'Key) -> seq<'T> -> seq<'Key * seq<'T>>
  static groupBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
    return Seq.fold((acc, x) => {
      const k = f(x), vs = acc.get(k);
      return vs != null ? acc.set(k, new List(x, <List<T>>vs)) : acc.set(k, List.singleton(x));
    }, new Map<K, Iterable<T>>(), xs);
  }

  static tryHead<T>(xs: Iterable<T>) {
    const iter = xs[Symbol.iterator]();
    const cur = iter.next();
    return cur.done ? null : cur.value;
  }

  static head<T>(xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryHead(xs));
  }

  static initialize<T>(n: number, f: (i: number) => T) {
    return Seq.delay(() =>
      Seq.unfold(i => i < n ? [f(i), i + 1] : null, 0));
  }

  static initializeInfinite<T>(f: (i: number) => T) {
    return Seq.delay(() =>
      Seq.unfold(i => [f(i), i + 1], 0));
  }

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
  }

  static item<T>(i: number, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryItem(i, xs));
  }

  static iterate<T>(f: (x: T) => void, xs: Iterable<T>) {
    Seq.fold((_, x) => f(x), null, xs);
  }

  static iterate2<T1, T2>(f: (x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
    Seq.fold2((_, x, y) => f(x, y), null, xs, ys);
  }

  static iterateIndexed<T>(f: (i: number, x: T) => void, xs: Iterable<T>) {
    Seq.fold((_, x, i) => f(i, x), null, xs);
  }

  static iterateIndexed2<T1, T2>(f: (i: number, x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
    Seq.fold2((_, x, y, i) => f(i, x, y), null, xs, ys);
  }

  static isEmpty<T>(xs: Iterable<T>) {
    const i = xs[Symbol.iterator]();
    return i.next().done;
  }

  static tryLast<T>(xs: Iterable<T>) {
    try {
      return Seq.reduce((_, x) => x, xs);
    }
    catch (err) {
      return null;
    }
  }

  static last<T>(xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryLast(xs));
  }

  // A static 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
  static count<T>(xs: Iterable<T>) {
    return Array.isArray(xs) || ArrayBuffer.isView(xs)
      ? (xs as Array<T>).length
      : Seq.fold((acc, x) => acc + 1, 0, xs);
  }

  static map<T, U>(f: (x: T) => U, xs: Iterable<T>) {
    return Seq.delay(() => Seq.unfold(iter => {
      const cur = iter.next();
      return !cur.done ? [f(cur.value), iter] : null;
    }, xs[Symbol.iterator]()));
  }

  static mapIndexed<T, U>(f: (i: number, x: T) => U, xs: Iterable<T>) {
    return Seq.delay(() => {
      let i = 0;
      return Seq.unfold(iter => {
        const cur = iter.next();
        return !cur.done ? [f(i++, cur.value), iter] : null;
      }, xs[Symbol.iterator]());
    });
  }

  static map2<T1, T2, U>(f: (x: T1, y: T2) => U, xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.delay(() => {
      const iter1 = xs[Symbol.iterator]();
      const iter2 = ys[Symbol.iterator]();
      return Seq.unfold(() => {
        const cur1 = iter1.next(), cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
      });
    });
  }

  static mapIndexed2<T1, T2, U>(f: (i: number, x: T1, y: T2) => U, xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.delay(() => {
      let i = 0;
      const iter1 = xs[Symbol.iterator]();
      const iter2 = ys[Symbol.iterator]();
      return Seq.unfold(() => {
        const cur1 = iter1.next(), cur2 = iter2.next();
        return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
      });
    });
  }

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
  }

  static mapFold<T, ST, R>(f: (acc: ST, x: T) => Tuple<R, ST>, acc: ST, xs: Iterable<T>) {
    let result: Array<R> = [];
    let r: R;
    let cur: IteratorResult<T>;
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      cur = iter.next();
      if (cur.done)
        break;

      [r, acc] = f(acc, cur.value);
      result.push(r);
    }
    return Tuple(<Iterable<R>>result, acc);
  }

  static mapFoldBack<T, ST, R>(f: (x: T, acc: ST) => Tuple<R, ST>, xs: Iterable<T>, acc: ST) {
    const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as Array<T> : Array.from(xs);
    let result: Array<R> = [];
    let r: R;
    for (let i = arr.length - 1; i >= 0; i--) {
      [r, acc] = f(arr[i], acc);
      result.push(r);
    }
    return Tuple(<Iterable<R>>result, acc);
  }

  static max<T extends number>(xs: Iterable<T>) {
    return Seq.reduce((acc: T, x: T) => Math.max(acc, x), xs);
  }

  static maxBy<T, U extends number>(f: (x: T) => U, xs: Iterable<T>) {
    return Seq.reduce((x, y) => f(x) > f(y) ? x : y, xs);
  }

  static min<T extends number>(xs: Iterable<T>) {
    return Seq.reduce((acc: T, x: T) => Math.min(acc, x), xs);
  }

  static minBy<T, U extends number>(f: (x: T) => U, xs: Iterable<T>) {
    return Seq.reduce((x, y) => f(x) < f(y) ? x : y, xs);
  }

  static pairwise<T extends number>(xs: Iterable<T>) {
    return Seq.skip(1, Seq.scan((last, next) => Tuple(last[1], next), Tuple(0, 0), xs));
  }

  static permute<T>(f: (i: number) => number, xs: Iterable<T>) {
    return Seq.ofArray(FArray.permute(f, Array.from(xs)));
  }

  static rangeStep(first: number, step: number, last: number) {
    if (step === 0)
      throw "Step cannot be 0";
    return Seq.unfold(x => step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null, first);
  }

  static rangeChar(first: string, last: string) {
    return Seq.unfold(x => x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null, first);
  }

  static range(first: number, last: number) {
    return Seq.rangeStep(first, 1, last);
  }

  static readOnly<T>(xs: Iterable<T>) {
    return Seq.map(x => x, xs);
  }

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
      if (cur.done)
        break;

      acc = f(acc, cur.value);
    }
    return acc;
  }

  static reduceBack<T>(f: (acc: T, x: T, i?: number) => T, xs: Iterable<T>) {
    const ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? <Array<T>>xs : Array.from(xs);
    if (ar.length === 0)
      throw "Seq was empty";

    let acc = ar[ar.length - 1];
    for (let i = ar.length - 2; i >= 0; i--)
      acc = f(ar[i], acc, i);

    return acc;
  }

  static replicate<T>(n: number, x: T) {
    return Seq.initialize(n, () => x);
  }

  static reverse<T>(xs: Iterable<T>) {
    const ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? (<Array<T>>xs).slice(0) : Array.from(xs);
    return Seq.ofArray(ar.reverse());
  }

  static scan<T, ST>(f: (st: ST, x: T) => ST, seed: ST, xs: Iterable<T>) {
    return Seq.delay(() => {
      const iter = xs[Symbol.iterator]();
      return Seq.unfold(acc => {
        if (acc == null)
          return [seed, seed];

        const cur = iter.next();
        if (!cur.done) {
          acc = f(acc, cur.value);
          return [acc, acc];
        }
        return void 0;
      }, <ST>null);
    });
  }

  static scanBack<T, ST>(f: (x: T, st: ST) => ST, xs: Iterable<T>, seed: ST) {
    return Seq.reverse(Seq.scan((acc, x) => f(x, acc), seed, Seq.reverse(xs)));
  }

  static singleton<T>(x: T) {
    return Seq.unfold(x => x != null ? [x, null] : null, x);
  }

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
  }

  static skipWhile<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.delay(() => {
      let hasPassed = false;
      return Seq.filter((x) => hasPassed || (hasPassed = !f(x)), xs);
    });
  }

  static sortWith<T>(f: (x: T, y: T) => number, xs: Iterable<T>) {
    const ys = Array.from(xs);
    return Seq.ofArray(ys.sort(f));
  }

  private static defaultAdder(x: number, y: number) {
    return x + y;
  }

  static sum(xs: Iterable<number>, adder?: (x: number, y: number) => number) {
    adder = adder || Seq.defaultAdder;
    return Seq.reduce((acc, x) => adder(acc, x), xs);
  }

  static sumBy(f: (x: number) => number, xs: Iterable<number>, adder?: (x: number, y: number) => number) {
    let fst = true;
    adder = adder || Seq.defaultAdder;
    return Seq.reduce((acc, x) => {
      acc = fst ? f(acc) : acc, fst = false;
      return adder(acc, f(x));
    }, xs);
  }

  static tail<T>(xs: Iterable<T>) {
    const iter = xs[Symbol.iterator]();
    const cur = iter.next();
    if (cur.done)
      throw "Seq was empty";

    return <Iterable<T>>{
      [Symbol.iterator]: () => iter
    };
  }

  static take<T>(n: number, xs: Iterable<T>, truncate: boolean = false) {
    return Seq.delay(() => {
      const iter = xs[Symbol.iterator]();
      return Seq.unfold(i => {
        if (i < n) {
          const cur = iter.next();
          if (!cur.done)
            return [cur.value, i + 1];
          if (!truncate)
            throw "Seq has not enough elements";
        }
        return void 0;
      }, 0);
    });
  }

  static truncate<T>(n: number, xs: Iterable<T>) {
    return Seq.take(n, xs, true);
  }

  static takeWhile<T>(f: (x: T) => boolean, xs: Iterable<T>) {
    return Seq.delay(() => {
      const iter = xs[Symbol.iterator]();
      return Seq.unfold(i => {
        const cur = iter.next();
        if (!cur.done && f(cur.value))
          return [cur.value, null];
        return void 0;
      }, 0);
    });
  }

  static tryFind<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return null;
      if (f(cur.value, i))
        return cur.value;
    }
  }

  static find<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFind(f, xs));
  }

  static tryFindBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    let match = <T>null;
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return match;
      if (f(cur.value, i))
        match = cur.value;
    }
  }

  static findBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFindBack(f, xs));
  }

  static tryFindIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return null;
      if (f(cur.value, i))
        return i;
    }
  }

  static findIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFindIndex(f, xs));
  }

  static tryFindIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    let match = 0;
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        return match;
      if (f(cur.value, i))
        match = i;
    }
  }

  static findIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryFindIndexBack(f, xs));
  }

  static tryPick<T, U>(f: (x: T, i?: number) => U, xs: Iterable<T>) {
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      const cur = iter.next();
      if (cur.done)
        break;
      const y = f(cur.value, i);
      if (y != null)
        return y;
    }
    return void 0;
  }

  static pick<T, U>(f: (x: T, i?: number) => U, xs: Iterable<T>) {
    return Seq.__failIfNone(Seq.tryPick(f, xs));
  }

  static unfold<T, ST>(f: (st: ST) => Tuple<T, ST>, acc?: ST) {
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
  }

  static zip<T1, T2>(xs: Iterable<T1>, ys: Iterable<T2>) {
    return Seq.map2((x, y) => [x, y], xs, ys);
  }

  static zip3<T1, T2, T3>(xs: Iterable<T1>, ys: Iterable<T2>, zs: Iterable<T3>) {
    return Seq.map3((x, y, z) => [x, y, z], xs, ys, zs);
  }
}

class FSet {
  static union<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.fold((acc, x) => { acc.add(x); return acc; }, new Set(set1), set2);
  }
  static op_Addition = FSet.union;

  static unionMany<T>(sets: Iterable<Set<T>>) {
    return Seq.fold((acc, s) => FSet.union(acc, s), new Set<T>(), sets);
  }

  static difference<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.fold((acc, x) => { acc.delete(x); return acc; }, new Set(set1), set2);
  }
  static op_Subtraction = FSet.difference;

  static intersect<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.fold((acc, x) => {
      if (!set2.has(x))
        acc.delete(x);
      return acc;
    }, new Set(set1), set1);
  }

  static intersectMany<T>(sets: Iterable<Set<T>>) {
    const ar = Array.isArray(sets) ? <Array<Set<T>>>sets : Array.from(sets);
    if (ar.length == 0)
      throw "Seq was empty";

    const set = new Set<T>(ar[0]);
    Seq.iterate((x: T) => {
      for (let i = 1; i < ar.length; i++) {
        if (!ar[i].has(x)) {
          set.delete(x);
          break;
        }
      }
    }, ar[0]);
    return set;
  }

  static isProperSubsetOf<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.forAll(x => set2.has(x), set1) && Seq.exists(x => !set1.has(x), set2);
  }
  static isProperSubset = FSet.isProperSubsetOf;

  static isSubsetOf<T>(set1: Set<T>, set2: Set<T>) {
    return Seq.forAll(x => set2.has(x), set1);
  }
  static isSubset = FSet.isSubsetOf;

  static isProperSupersetOf<T>(set1: Set<T>, set2: Set<T>) {
    return FSet.isProperSubset(set2, set1);
  }
  static isProperSuperset = FSet.isProperSupersetOf;

  static isSupersetOf<T>(set1: Set<T>, set2: Set<T>) {
    return FSet.isSubset(set2, set1);
  }
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
    }
  }

  static partition<T>(f: (x: T) => boolean, xs: Set<T>) {
    return Seq.fold((acc, x) => {
      const lacc = acc[0], racc = acc[1];
      return f(x) ? Tuple(lacc.add(x), racc) : Tuple(lacc, racc.add(x));
    }, Tuple(new Set<T>(), new Set<T>()), xs);
  }

  static remove<T>(item: T, xs: Set<T>) {
    return FSet.removeInPlace(item, new Set(xs));
  }

  static removeInPlace<T>(item: T, xs: Set<T>) {
    xs.delete(item);
    return xs;
  }
}
export { FSet as Set }

export interface IComparer<T> {
  Compare(x: T, y: T): number;
}

export interface IComparable<T> {
  CompareTo(x: T): number;
}

export interface IEquatable<T> {
  Equals(x: T): boolean;
}

export class GenericComparer<T> implements IComparer<T> {
  Compare: (x:T, y:T) => number;
  
  constructor(f?: (x:T, y:T) => number) {
    this.Compare = f || Util.compare;
  }
} 

interface MapIterator {
  stack: List<MapTree>;
  started: boolean;
}

class MapTree {
  public Case: string;
  public Fields: any[];

  constructor(caseName: "MapEmpty" | "MapOne" | "MapNode", fields: any[]) {
    this.Case = caseName;
    this.Fields = fields;
  }

  static sizeAux(acc: number, m: MapTree): number {
    return m.Case === "MapOne"
      ? acc + 1
      : m.Case === "MapNode"
        ? MapTree.sizeAux(MapTree.sizeAux(acc + 1, m.Fields[2]), m.Fields[3])
        : acc;
  }

  static size(x: MapTree) {
    return MapTree.sizeAux(0, x);
  }

  static empty() {
    return new MapTree("MapEmpty", []);
  }

  static height(_arg1: MapTree) {
    return _arg1.Case === "MapOne" ? 1 : _arg1.Case === "MapNode" ? _arg1.Fields[4] : 0;
  }

  static isEmpty(m: MapTree) {
    return m.Case === "MapEmpty" ? true : false;
  }

  static mk(l: MapTree, k: any, v: any, r: MapTree) {
    var matchValue = [l, r];
    var $target1 = () => {
      var hl = MapTree.height(l);
      var hr = MapTree.height(r);
      var m = hl < hr ? hr : hl;
      return new MapTree("MapNode", [k, v, l, r, m + 1]);
    };
    if (matchValue[0].Case === "MapEmpty") {
      if (matchValue[1].Case === "MapEmpty") {
        return new MapTree("MapOne", [k, v]);
      } else {
        return $target1();
      }
    } else {
      return $target1();
    }
  };

  static rebalance(t1: MapTree, k: any, v: any, t2: MapTree) {
    var t1h = MapTree.height(t1);
    var t2h = MapTree.height(t2);
    if (t2h > t1h + 2) {
      if (t2.Case === "MapNode") {
        if (MapTree.height(t2.Fields[2]) > t1h + 1) {
          if (t2.Fields[2].Case === "MapNode") {
            return MapTree.mk(MapTree.mk(t1, k, v, t2.Fields[2].Fields[2]), t2.Fields[2].Fields[0], t2.Fields[2].Fields[1], MapTree.mk(t2.Fields[2].Fields[3], t2.Fields[0], t2.Fields[1], t2.Fields[3]));
          } else {
            throw "rebalance";
          }
        } else {
          return MapTree.mk(MapTree.mk(t1, k, v, t2.Fields[2]), t2.Fields[0], t2.Fields[1], t2.Fields[3]);
        }
      } else {
        throw "rebalance";
      }
    } else {
      if (t1h > t2h + 2) {
        if (t1.Case === "MapNode") {
          if (MapTree.height(t1.Fields[3]) > t2h + 1) {
            if (t1.Fields[3].Case === "MapNode") {
              return MapTree.mk(MapTree.mk(t1.Fields[2], t1.Fields[0], t1.Fields[1], t1.Fields[3].Fields[2]), t1.Fields[3].Fields[0], t1.Fields[3].Fields[1], MapTree.mk(t1.Fields[3].Fields[3], k, v, t2));
            } else {
              throw "rebalance";
            }
          } else {
            return MapTree.mk(t1.Fields[2], t1.Fields[0], t1.Fields[1], MapTree.mk(t1.Fields[3], k, v, t2));
          }
        } else {
          throw "rebalance";
        }
      } else {
        return MapTree.mk(t1, k, v, t2);
      }
    }
  }

  static add(comparer: IComparer<any>, k: any, v: any, m: MapTree): MapTree {
    if (m.Case === "MapOne") {
      var c = comparer.Compare(k, m.Fields[0]);
      if (c < 0) {
        return new MapTree("MapNode", [k, v, new MapTree("MapEmpty", []), m, 2]);
      }
      else if (c === 0) {
        return new MapTree("MapOne", [k, v]);
      }
      return new MapTree("MapNode", [k, v, m, new MapTree("MapEmpty", []), 2]);
    }
    else if (m.Case === "MapNode") {
      var c = comparer.Compare(k, m.Fields[0]);
      if (c < 0) {
        return MapTree.rebalance(MapTree.add(comparer, k, v, m.Fields[2]), m.Fields[0], m.Fields[1], m.Fields[3]);
      }
      else if (c === 0) {
        return new MapTree("MapNode", [k, v, m.Fields[2], m.Fields[3], m.Fields[4]]);
      }
      return MapTree.rebalance(m.Fields[2], m.Fields[0], m.Fields[1], MapTree.add(comparer, k, v, m.Fields[3]));
    }
    return new MapTree("MapOne", [k, v]);
  }

  static find(comparer: IComparer<any>, k: any, m: MapTree): any {
    const res = MapTree.tryFind(comparer, k, m);
    if (res != null)
      return res;
    throw "key not found";
  }

  static tryFind(comparer: IComparer<any>, k: any, m: MapTree): any {
    if (m.Case === "MapOne") {
      var c = comparer.Compare(k, m.Fields[0]);
      return c === 0 ? m.Fields[1] : null;
    }
    else if (m.Case === "MapNode") {
      var c = comparer.Compare(k, m.Fields[0]);
      if (c < 0) {
        return MapTree.tryFind(comparer, k, m.Fields[2]);
      } else {
        if (c === 0) {
          return m.Fields[1];
        } else {
          return MapTree.tryFind(comparer, k, m.Fields[3]);
        }
      }
    }
    return null;
  }

  static partition1(comparer: IComparer<any>, f: (k:any, v:any) => boolean, k: any, v: any, acc1: MapTree, acc2: MapTree): [MapTree, MapTree] {
    return f(k,v) ? [MapTree.add(comparer, k, v, acc1), acc2] : [acc1, MapTree.add(comparer, k, v, acc2)];
  }

  static partitionAux(comparer: IComparer<any>, f: (k:any, v:any) => boolean, s: MapTree, acc_0: MapTree, acc_1: MapTree): [MapTree, MapTree] {
    const acc: [MapTree, MapTree] = [acc_0, acc_1];
    if (s.Case === "MapOne") {
      return MapTree.partition1(comparer, f, s.Fields[0], s.Fields[1], acc[0], acc[1]);
    }
    else if (s.Case === "MapNode") {
      const acc_2 = MapTree.partitionAux(comparer, f, s.Fields[3], acc[0], acc[1]);
      const acc_3 = MapTree.partition1(comparer, f, s.Fields[0], s.Fields[1], acc_2[0], acc_2[1]);
      return MapTree.partitionAux(comparer, f, s.Fields[2], acc_3[0], acc_3[1]);
    }
    return acc;
  }

  static partition(comparer: IComparer<any>, f: (k:any, v:any) => boolean, s: MapTree) {
    return MapTree.partitionAux(comparer, f, s, MapTree.empty(), MapTree.empty());
  }

  static filter1(comparer: IComparer<any>, f: (k:any, v:any) => boolean, k: any, v: any, acc: MapTree) {
    return f(k,v) ? MapTree.add(comparer, k, v, acc) : acc;
  }

  static filterAux(comparer: IComparer<any>, f: (k:any, v:any) => boolean, s: MapTree, acc: MapTree): MapTree {
    return s.Case === "MapOne" ? MapTree.filter1(comparer, f, s.Fields[0], s.Fields[1], acc) : s.Case === "MapNode" ? (() => {
      var acc_1 = MapTree.filterAux(comparer, f, s.Fields[2], acc);
      var acc_2 = MapTree.filter1(comparer, f, s.Fields[0], s.Fields[1], acc_1);
      return MapTree.filterAux(comparer, f, s.Fields[3], acc_2);
    })() : acc;
  }

  static filter(comparer: IComparer<any>, f: (k:any, v:any) => boolean, s: MapTree) {
    return MapTree.filterAux(comparer, f, s, MapTree.empty());
  }

  static spliceOutSuccessor(m: MapTree): [any,any,MapTree] {
    if (m.Case === "MapOne") {
      return [m.Fields[0], m.Fields[1], new MapTree("MapEmpty", [])];
    }
    else if (m.Case === "MapNode") {
      if (m.Fields[2].Case === "MapEmpty") {
        return [m.Fields[0], m.Fields[1], m.Fields[3]];
      }
      else {
        const kvl = MapTree.spliceOutSuccessor(m.Fields[2]);
        return [kvl[0], kvl[1], MapTree.mk(kvl[2], m.Fields[0], m.Fields[1], m.Fields[3])];
      }
    }
    throw "internal error: Map.spliceOutSuccessor";
  }

  static remove(comparer: IComparer<any>, k: any, m: MapTree): MapTree {
    if (m.Case === "MapOne") {
      var c = comparer.Compare(k, m.Fields[0]);
      if (c === 0) {
        return new MapTree("MapEmpty", []);
      } else {
        return m;
      }
    }
    else if (m.Case === "MapNode") {
      var c = comparer.Compare(k, m.Fields[0]);
      if (c < 0) {
        return MapTree.rebalance(MapTree.remove(comparer, k, m.Fields[2]), m.Fields[0], m.Fields[1], m.Fields[3]);
      } else {
        if (c === 0) {
          var matchValue = [m.Fields[2], m.Fields[3]];
          if (matchValue[0].Case === "MapEmpty") {
            return m.Fields[3];
          } else {
            if (matchValue[1].Case === "MapEmpty") {
              return m.Fields[2];
            } else {
              var patternInput = MapTree.spliceOutSuccessor(m.Fields[3]);
              var sv = patternInput[1];
              var sk = patternInput[0];
              var r_ = patternInput[2];
              return MapTree.mk(m.Fields[2], sk, sv, r_);
            }
          }
        } else {
          return MapTree.rebalance(m.Fields[2], m.Fields[0], m.Fields[1], MapTree.remove(comparer, k, m.Fields[3]));
        }
      }
    }
    else {
      return MapTree.empty();
    }
  }

  static mem(comparer: IComparer<any>, k: any, m: MapTree): boolean {
    return m.Case === "MapOne" ? comparer.Compare(k, m.Fields[0]) === 0 : m.Case === "MapNode" ? (() => {
      var c = comparer.Compare(k, m.Fields[0]);
      if (c < 0) {
        return MapTree.mem(comparer, k, m.Fields[2]);
      } else {
        if (c === 0) {
          return true;
        } else {
          return MapTree.mem(comparer, k, m.Fields[3]);
        }
      }
    })() : false;
  }

  static iter(f: (k:any, v:any) => void, m: MapTree): void {
    if (m.Case === "MapOne") {
      f(m.Fields[0], m.Fields[1]);
    }
    else if(m.Case === "MapNode") {
      MapTree.iter(f, m.Fields[2]);
      f(m.Fields[0], m.Fields[1]);
      MapTree.iter(f, m.Fields[3]);
    }
  }

  static tryPick(f: (k:any, v:any) => any, m: MapTree): any {
    return m.Case === "MapOne" ? f(m.Fields[0], m.Fields[1]) : m.Case === "MapNode" ? (() => {
      var matchValue = MapTree.tryPick(f, m.Fields[2]);
      if (matchValue == null) {
        var matchValue_1 = f(m.Fields[0], m.Fields[1]);
        if (matchValue_1 == null) {
          return MapTree.tryPick(f, m.Fields[3]);
        } else {
          var res = matchValue_1;
          return res;
        }
      } else {
        var res = matchValue;
        return res;
      }
    })() : null;
  }

  static exists(f: (k:any, v:any) => boolean, m: MapTree): boolean {
    return m.Case === "MapOne" ? f(m.Fields[0], m.Fields[1]) : m.Case === "MapNode" ? (MapTree.exists(f, m.Fields[2]) ? true : f(m.Fields[0], m.Fields[1])) ? true : MapTree.exists(f, m.Fields[3]) : false;
  }

  static forall(f: (k:any, v:any) => boolean, m: MapTree): boolean {
    return m.Case === "MapOne" ? f(m.Fields[0], m.Fields[1]) : m.Case === "MapNode" ? (MapTree.forall(f, m.Fields[2]) ? f(m.Fields[0], m.Fields[1]) : false) ? MapTree.forall(f, m.Fields[3]) : false : true;
  }

  // static map(f: (v:any) => any, m: MapTree): MapTree {
  //   return m.Case === "MapOne" ? new MapTree("MapOne", [m.Fields[0], f(m.Fields[1])]) : m.Case === "MapNode" ? (() => {
  //     var l2 = MapTree.map(f, m.Fields[2]);
  //     var v2 = f(m.Fields[1]);
  //     var r2 = MapTree.map(f, m.Fields[3]);
  //     return new MapTree("MapNode", [m.Fields[0], v2, l2, r2, m.Fields[4]]);
  //   })() : MapTree.empty();
  // }

  static mapi(f: (k:any, v:any) => any, m: MapTree): MapTree {
    return m.Case === "MapOne" ? new MapTree("MapOne", [m.Fields[0], f(m.Fields[0], m.Fields[1])]) : m.Case === "MapNode" ? (() => {
      var l2 = MapTree.mapi(f, m.Fields[2]);
      var v2 = f(m.Fields[0], m.Fields[1]);
      var r2 = MapTree.mapi(f, m.Fields[3]);
      return new MapTree("MapNode", [m.Fields[0], v2, l2, r2, m.Fields[4]]);
    })() : MapTree.empty();
  }

  static foldBack(f: (k:any, v:any, acc: any) => any, m: MapTree, x: any): any {
    return m.Case === "MapOne" ? f(m.Fields[0], m.Fields[1], x) : m.Case === "MapNode" ? (() => {
      var x_1 = MapTree.foldBack(f, m.Fields[3], x);
      var x_2 = f(m.Fields[0], m.Fields[1], x_1);
      return MapTree.foldBack(f, m.Fields[2], x_2);
    })() : x;
  }

  static fold(f: (acc: any, k:any, v:any) => any, x: any, m: MapTree): any {
    return m.Case === "MapOne" ? f(x, m.Fields[0], m.Fields[1]) : m.Case === "MapNode" ? (() => {
      var x_1 = MapTree.fold(f, x, m.Fields[2]);
      var x_2 = f(x_1, m.Fields[0], m.Fields[1]);
      return MapTree.fold(f, x_2, m.Fields[3]);
    })() : x;
  }

  // static foldFromTo(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any): any {
  //   if (m.Case === "MapOne") {
  //     var cLoKey = comparer.Compare(lo, m.Fields[0]);
  //     var cKeyHi = comparer.Compare(m.Fields[0], hi);
  //     var x_1 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.Fields[0], m.Fields[1], x) : x;
  //     return x_1;
  //   }
  //   else if (m.Case === "MapNode") {
  //     var cLoKey = comparer.Compare(lo, m.Fields[0]);
  //     var cKeyHi = comparer.Compare(m.Fields[0], hi);
  //     var x_1 = cLoKey < 0 ? MapTree.foldFromTo(comparer, lo, hi, f, m.Fields[2], x) : x;
  //     var x_2 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.Fields[0], m.Fields[1], x_1) : x_1;
  //     var x_3 = cKeyHi < 0 ? MapTree.foldFromTo(comparer, lo, hi, f, m.Fields[3], x_2) : x_2;
  //     return x_3;
  //   }
  //   return x;
  // }

  // static foldSection(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any) {
  //   return comparer.Compare(lo, hi) === 1 ? x : MapTree.foldFromTo(comparer, lo, hi, f, m, x);
  // }

  // static loop(m: MapTree, acc: any): List<[any,any]> {
  //   return m.Case === "MapOne"
  //     ? new List([m.Fields[0], m.Fields[1]], acc)
  //     : m.Case === "MapNode"
  //       ? MapTree.loop(m.Fields[2], new List([m.Fields[0], m.Fields[1]], MapTree.loop(m.Fields[3], acc)))
  //       : acc;
  // }

  // static toList(m: MapTree) {
  //   return MapTree.loop(m, new List());
  // }

  // static toArray(m: MapTree) {
  //   return Array.from(MapTree.toList(m));
  // }

  // static ofList(comparer: IComparer<any>, l: List<[any,any]>) {
  //   return Seq.fold((acc: MapTree, tupledArg: [any, any]) => {
  //     return MapTree.add(comparer, tupledArg[0], tupledArg[1], acc);
  //   }, MapTree.empty(), l);
  // }

  static mkFromEnumerator(comparer: IComparer<any>, acc: MapTree, e: Iterator<any>): MapTree {
    const cur = e.next();
    return !cur.done
      ? MapTree.mkFromEnumerator(comparer, MapTree.add(comparer, cur.value[0], cur.value[1], acc), e)
      : acc;
  }

  // static ofArray(comparer: IComparer<any>, arr: ArrayLike<[any,any]>) {
  //   var res = MapTree.empty();
  //   for (var i = 0; i <= arr.length - 1; i++) {
  //     res = MapTree.add(comparer, arr[i][0], arr[i][1], res);
  //   }
  //   return res;
  // }

  static ofSeq(comparer: IComparer<any>, c: Iterable<any>): MapTree {
    var ie = c[Symbol.iterator]();
    return MapTree.mkFromEnumerator(comparer, MapTree.empty(), ie);
  }

  // static copyToArray(s: MapTree, arr: ArrayLike<any>, i: number) {
  //   MapTree.iter((x, y) => { arr[i++] = [x, y]; }, s);
  // }

  static collapseLHS(stack: List<any>): List<any> {
    if (stack.tail != null) {
      if (stack.head.Case === "MapOne") {
        return stack;
      }
      else if (stack.head.Case === "MapNode") {
        var k = stack.head.Fields[0];
        var l = stack.head.Fields[2];
        var r = stack.head.Fields[3];
        var rest = stack.tail;
        var v = stack.head.Fields[1];
        return MapTree.collapseLHS(List.ofArray([l, new MapTree("MapOne", [k, v]), r], rest));
      }
      else {
        return MapTree.collapseLHS(stack.tail);
      }
    }
    else {
      return new List();
    }
  }

  static mkIterator(s: MapTree): MapIterator {
    return { stack: MapTree.collapseLHS(new List(s, new List())), started: false };
  }

  static moveNext(i: MapIterator): IteratorResult<[any,any]> {
    function current(i: MapIterator): [any,any] {
      if (i.stack.tail == null) {
        return null;
      } else {
        if (i.stack.head.Case === "MapOne") {
          return [i.stack.head.Fields[0], i.stack.head.Fields[1]];
        } else {
          throw "Please report error: Map iterator, unexpected stack for current";
        }
      }
    }
    if (i.started) {
      if (i.stack.tail == null) {
        return { done: true };
      } else {
        if (i.stack.head.Case === "MapOne") {
          i.stack = MapTree.collapseLHS(i.stack.tail);
          return {
            done: i.stack.tail == null,
            value: current(i)
          };
        } else {
          throw "Please report error: Map iterator, unexpected stack for moveNext";
        }
      }
    }
    else {
      i.started = true;
      return {
        done: i.stack.tail == null,
        value: current(i)
      };
    };
  }
}

class FMap<K,V> implements IEquatable<FMap<K,V>>, IComparable<FMap<K,V>>, Iterable<[K,V]> {
  private tree: MapTree;
  private comparer: IComparer<K>;

  /** Do not call, use Map.create instead. */
  constructor () {}

  private static from<K,V>(comparer: IComparer<K>, tree: MapTree) {
    let map = new FMap<K,V>();
    map.tree = tree
    map.comparer = comparer || new GenericComparer<K>();
    return map;
  }

  private static create<K,V>(ie?: Iterable<[K,V]>, comparer?: IComparer<K>) {
    comparer = comparer || new GenericComparer<K>();
    return FMap.from(comparer, ie ? MapTree.ofSeq(comparer, ie) : MapTree.empty());
  }

  Equals(m2: FMap<K,V>) {
    return this.CompareTo(m2) == 0;
  }

  CompareTo(m2: FMap<K,V>) {
    return Seq.compareWith((kvp1, kvp2) => {
      var c = this.comparer.Compare(kvp1[0], kvp2[0]);
      return c !== 0 ? c : Util.compare(kvp1[1], kvp2[1]);
    }, this, m2);
  }

  [Symbol.iterator](): Iterator<[K,V]> {
    let i = MapTree.mkIterator(this.tree);
    return <Iterator<[K,V]>>{
      next: () => MapTree.moveNext(i)
    };
  }

  entries() {
    return this[Symbol.iterator]();
  }

  keys() {
    return Seq.map(kv => kv[0], this);
  }

  values() {
    return Seq.map(kv => kv[1], this);
  }

  get(k: K) {
    return MapTree.find(this.comparer, k, this.tree);
  }

  has(k: K) {
    return MapTree.mem(this.comparer, k, this.tree);
  }

  set(k: K, v: V): FMap<K,V> {
    throw "not supported";
  }

  delete(k: K): boolean {
    throw "not supported";
  }

  get size() {
    return MapTree.size(this.tree);
  }

  static add<K,V>(k: K, v: V, map: FMap<K,V>) {
    return FMap.from(map.comparer, MapTree.add(map.comparer, k, v, map.tree));
  }

  static remove<K, V>(item: K, map: FMap<K, V>) {
    return FMap.from(map.comparer, MapTree.remove(map.comparer, item, map.tree));
  }

  static containsValue<K, V>(v: V, map: Map<K, V> | FMap<K,V>) {
    return Seq.fold((acc, k) => acc || Util.equals(map.get(k), v), false, map.keys());
  }

  static exists<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    return MapTree.exists(f, map.tree);
  }

  static find<K, V>(k: K, map: FMap<K, V>) {
    return MapTree.find(map.comparer, k, map.tree);
  }

  static tryFind<K, V>(k: K, map: FMap<K, V>) {
    return MapTree.tryFind(map.comparer, k, map.tree);
  }

  static filter<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    return FMap.from(map.comparer, MapTree.filter(map.comparer, f, map.tree));
  }

  static fold<K, V, ST>(f: (acc: ST, k: K, v: V) => ST, seed: ST, map: FMap<K, V>) {
    return MapTree.fold(f, seed, map.tree);
  }

  static foldBack<K, V, ST>(f: (k: K, v: V, acc: ST) => ST, map: FMap<K, V>, seed: ST) {
    return MapTree.foldBack(f, map.tree, seed);
  }

  static forAll<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    return MapTree.forall(f, map.tree);
  }

  static isEmpty<K, V>(map: FMap<K, V>) {
    return MapTree.isEmpty(map.tree);
  }

  static iterate<K, V>(f: (k: K, v: V) => void, map: FMap<K, V>) {
    return MapTree.iter(f, map.tree);
  }

  static map<K, T, U>(f: (k: K, v: T) => U, map: FMap<K, T>) {
    return FMap.from(map.comparer, MapTree.mapi(f, map.tree));
  }

  static partition<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    const rs = MapTree.partition(map.comparer, f, map.tree);
    return [FMap.from(map.comparer, rs[0]), FMap.from(map.comparer, rs[1])];
  }

  static findKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V> | FMap<K, V>) {
    return Seq.pick(kv => f(kv[0], kv[1]) ? kv[0] : null, map);
  }

  static tryFindKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V> | FMap<K, V>) {
    return Seq.tryPick(kv => f(kv[0], kv[1]) ? kv[0] : null, map);
  }

  static pick<K, T, U>(f: (k: K, v: T) => U, map: FMap<K, T>) {
    const res = FMap.tryPick(f, map);
    if (res != null)
      return res;
    throw "key not found";
  }

  static tryPick<K, T, U>(f: (k: K, v: T) => U, map: FMap<K, T>) {
    return MapTree.tryPick(f, map.tree);
  }
}
Util.setInterfaces(FMap.prototype, ["System.IEquatable", "System.IComparable"], "Microsoft.FSharp.Collections.FSharpMap"); 

export { FMap as Map }

export type Unit = void;

export const Nothing: Unit = void 0;

export type Continuation<T> = (x: T) => Unit;

export interface CancellationToken {
  isCancelled: boolean;
}

export interface IAsyncContext<T> {
  onSuccess: Continuation<T>;
  onError: Continuation<any>;
  onCancel: Continuation<any>;

  cancelToken: CancellationToken;
}

export type IAsync<T> = (x: IAsyncContext<T>) => void;

const AsyncImpl = {
  protectedCont<T>(f: IAsync<T>) {
    return (ctx: IAsyncContext<T>) => {
      if (ctx.cancelToken.isCancelled)
        ctx.onCancel("cancelled");
      else
        try {
          return f(ctx);
        } catch (err) {
          ctx.onError(err);
        }
    };
  },
  bind<T, U>(computation: IAsync<T>, binder: (x: T) => IAsync<U>) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<U>) => {
      computation({
        onSuccess: (x: T) => binder(x)(ctx),
        onError: ctx.onError,
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken
      });
    });
  },
  return<T>(value?: T) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<T>) => ctx.onSuccess(value));
  }
}

export class AsyncBuilder {
  Bind<T, U>(computation: IAsync<T>, binder: (x: T) => IAsync<U>) {
    return AsyncImpl.bind(computation, binder);
  }

  Combine<T>(computation1: IAsync<Unit>, computation2: IAsync<T>) {
    return this.Bind(computation1, () => computation2);
  }

  Delay<T>(generator: () => IAsync<T>) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<T>) => generator()(ctx));
  }

  For<T>(sequence: Iterable<T>, body: (x: T) => IAsync<Unit>) {
    const iter = sequence[Symbol.iterator]();
    let cur = iter.next();
    return this.While(() => !cur.done, this.Delay(() => {
      const res = body(cur.value);
      cur = iter.next();
      return res;
    }));
  }

  Return<T>(value?: T) {
    return AsyncImpl.return(value);
  }

  ReturnFrom<T>(computation: IAsync<T>) {
    return computation;
  }

  TryFinally<T>(computation: IAsync<T>, compensation: () => void) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<T>) => {
      computation({
        onSuccess: (x: T) => {
          compensation();
          ctx.onSuccess(x);
        },
        onError: (x: any) => {
          compensation();
          ctx.onError(x);
        },
        onCancel: (x: any) => {
          compensation();
          ctx.onCancel(x);
        },
        cancelToken: ctx.cancelToken
      });
    });
  }

  TryWith<T>(computation: IAsync<T>, catchHandler: (e: any) => IAsync<T>) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<T>) => {
      computation({
        onSuccess: ctx.onSuccess,
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken,
        onError: (ex: any) => catchHandler(ex)(ctx)
      });
    });
  }

  Using<T extends IDisposable, U>(resource: T, binder: (x: T) => IAsync<U>) {
    return this.TryFinally(binder(resource), () => resource.Dispose());
  }

  While(guard: () => boolean, computation: IAsync<Unit>): IAsync<Unit> {
    if (guard())
      return this.Bind(computation, () => this.While(guard, computation));
    else
      return this.Return(Nothing);
  }

  Zero() {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<Unit>) => ctx.onSuccess(Nothing));
  }
}

export const defaultAsyncBuilder = new AsyncBuilder();

export class Async {
  static awaitPromise<T>(p: Promise<T>) {
    return Async.fromContinuations((conts: Array<Continuation<T>>) =>
      p.then(conts[0]).catch(err =>
        (err == "cancelled" ? conts[2] : conts[1])(err)));
  }

  static get cancellationToken() {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<CancellationToken>) => ctx.onSuccess(ctx.cancelToken));
  }

  static catch<T>(work: IAsync<T>) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<Choice<T, any>>) => {
      work({
        onSuccess: x => ctx.onSuccess(Choice.Choice1Of2<T, any>(x)),
        onError: ex => ctx.onSuccess(Choice.Choice2Of2<T, any>(ex)),
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken
      });
    });
  }

  static defaultCancellationToken = {
    isCancelled: false
  };

  static fromContinuations<T>(f: (conts: Array<Continuation<T>>) => void) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<T>) => f([ctx.onSuccess, ctx.onError, ctx.onCancel]));
  }

  static ignore<T>(computation: IAsync<T>) {
    return AsyncImpl.bind(computation, x => AsyncImpl.return(Nothing));
  }

  static parallel<T>(computations: Iterable<IAsync<T>>) {
    return Async.awaitPromise(Promise.all(Seq.map(w => Async.startAsPromise(w), computations)));
  }

  static sleep(millisecondsDueTime: number) {
    return AsyncImpl.protectedCont((ctx: IAsyncContext<Unit>) => {
      setTimeout(() => ctx.cancelToken.isCancelled ? ctx.onCancel("cancelled") : ctx.onSuccess(Nothing), millisecondsDueTime);
    });
  }

  private static start<T>(computation: IAsync<Unit>, cancellationToken?: CancellationToken) {
    return Async.startWithContinuations(computation, cancellationToken);
  }
  static startImmediate = Async.start;

  private static emptyContinuation<T>(x: T) {
    // NOP
  }

  private static startWithContinuations<T>(computation: IAsync<T>,
    continuation?: Continuation<T> | CancellationToken,
    exceptionContinuation?: Continuation<any>,
    cancellationContinuation?: Continuation<any>,
    cancelToken?: CancellationToken) {
    if (typeof continuation !== "function") {
      cancelToken = <CancellationToken>continuation;
      continuation = null;
    }
    computation({
      onSuccess: continuation ? <Continuation<T>>continuation : Async.emptyContinuation,
      onError: exceptionContinuation ? exceptionContinuation : Async.emptyContinuation,
      onCancel: cancellationContinuation ? cancellationContinuation : Async.emptyContinuation,
      cancelToken: cancelToken ? cancelToken : Async.defaultCancellationToken
    });
  }

  static startAsPromise<T>(computation: IAsync<T>, cancellationToken?: CancellationToken) {
    return new Promise((resolve: Continuation<T>, reject: Continuation<any>) =>
      Async.startWithContinuations(computation, resolve, reject, reject, cancellationToken ? cancellationToken : Async.defaultCancellationToken));
  }
}

class QueueCell<Msg> {
  value: Msg;
  next: QueueCell<Msg>;

  constructor(message: Msg) {
    this.value = message;
  }
}

class MailboxQueue<Msg> {
  private firstAndLast: Tuple<QueueCell<Msg>, QueueCell<Msg>>;

  add(message: Msg) {
    const itCell = new QueueCell(message);
    if (this.firstAndLast) {
      this.firstAndLast[1].next = itCell;
      this.firstAndLast = [this.firstAndLast[0], itCell];
    }
    else
      this.firstAndLast = [itCell, itCell];
  }

  tryGet() {
    if (this.firstAndLast) {
      const value = this.firstAndLast[0].value;
      if (this.firstAndLast[0].next)
        this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
      else
        delete this.firstAndLast;
      return value;
    }
    return void 0;
  }
}

export type MailboxBody<Msg> = (m: MailboxProcessor<Msg>) => IAsync<Unit>;

export interface AsyncReplyChannel<Reply> {
  reply: (r: Reply) => Unit;
}

export class MailboxProcessor<Msg> {
  private body: MailboxBody<Msg>;
  private cancellationToken: CancellationToken;
  private messages: MailboxQueue<Msg>;

  private continuation: Continuation<Msg>;

  constructor(body: MailboxBody<Msg>, cancellationToken?: CancellationToken) {
    this.body = body;
    this.cancellationToken = cancellationToken || Async.defaultCancellationToken;
    this.messages = new MailboxQueue<Msg>();
  }

  static start<Msg>(body: MailboxBody<Msg>, cancellationToken?: CancellationToken) {
    const mbox = new MailboxProcessor(body, cancellationToken);
    mbox.start();
    return mbox;
  }

  __processEvents() {
    if (this.continuation) {
      const value = this.messages.tryGet();
      if (value) {
        const cont = this.continuation;
        delete this.continuation;
        cont(value);
      }
    }
  }

  start() {
    Async.startImmediate(this.body(this), this.cancellationToken);
  }

  receive() {
    return Async.fromContinuations((conts: Array<Continuation<Msg>>) => {
      if (this.continuation)
        throw "Receive can only be called once!";

      this.continuation = conts[0];
      this.__processEvents();
    });
  }

  post(message: Msg) {
    this.messages.add(message);
    this.__processEvents();
  }

  postAndAsyncReply<Reply>(buildMessage: (c: AsyncReplyChannel<Reply>) => Msg) {
    let result: Reply;
    let continuation: Continuation<Reply>;
    function checkCompletion() {
      if (result && continuation)
        continuation(result);
    }
    const reply = {
      reply: (res: Reply) => {
        result = res;
        checkCompletion();
      }
    };
    this.messages.add(buildMessage(reply));
    this.__processEvents();
    return Async.fromContinuations((conts: Array<Continuation<Reply>>) => {
      continuation = conts[0];
      checkCompletion();
    });
  }
}

export interface IObserver<T> {
  OnNext: (x: T) => void;
  OnError: (e: any) => void;
  OnCompleted: () => void;
}

class Observer<T> implements IObserver<T> {
  public OnNext: (x: T) => void;
  public OnError: (e: any) => void;
  public OnCompleted: () => void;

  constructor(onNext: (x: T) => void, onError?: (e: any) => void, onCompleted?: () => void) {
    this.OnNext = onNext;
    this.OnError = onError || ((e: any) => { });
    this.OnCompleted = onCompleted || function () { };
  }
}
Util.setInterfaces(Observer.prototype, ["System.IObserver"]);

export interface IObservable<T> {
  Subscribe: (o: IObserver<T>) => IDisposable;
}

class Observable<T> implements IObservable<T> {
  public Subscribe: (o: IObserver<T>) => IDisposable;

  constructor(subscribe: (o: IObserver<T>) => IDisposable) {
    this.Subscribe = subscribe;
  }
}
Util.setInterfaces(Observable.prototype, ["System.IObservable"]);

class FObservable {
  static __protect<T>(f: () => T, succeed: (x: T) => void, fail: (e: any) => void) {
    try {
      return succeed(f());
    } catch (e) {
      fail(e);
    }
  }

  static add<T>(callback: (x: T) => Unit, source: IObservable<T>) {
    source.Subscribe(new Observer(callback));
  }

  static choose<T, U>(chooser: (x: T) => U, source: IObservable<T>) {
    return <IObservable<U>>new Observable<U>(observer =>
      source.Subscribe(new Observer<T>(t =>
        FObservable.__protect(
          () => chooser(t),
          u => { if (u != null) observer.OnNext(u); },
          observer.OnError),
        observer.OnError, observer.OnCompleted)));
  }

  static filter<T>(predicate: (x: T) => boolean, source: IObservable<T>) {
    return FObservable.choose(x => predicate(x) ? x : null, source);
  }

  static map<T, U>(mapping: (x: T) => U, source: IObservable<T>) {
    return <IObservable<U>>new Observable<U>(observer =>
      source.Subscribe(new Observer<T>(t => {
        FObservable.__protect(
          () => mapping(t),
          observer.OnNext,
          observer.OnError);
      }, observer.OnError, observer.OnCompleted)));
  }

  static merge<T>(source1: IObservable<T>, source2: IObservable<T>) {
    return <IObservable<T>>new Observable(observer => {
      let stopped = false, completed1 = false, completed2 = false;

      const h1 = source1.Subscribe(new Observer<T>(
        v => { if (!stopped) observer.OnNext(v); },
        e => {
          if (!stopped) {
            stopped = true;
            observer.OnError(e);
          }
        },
        () => {
          if (!stopped) {
            completed1 = true;
            if (completed2) {
              stopped = true;
              observer.OnCompleted();
            }
          }
        }));

      const h2 = source2.Subscribe(new Observer<T>(
        v => { if (!stopped) { observer.OnNext(v); } },
        e => {
          if (!stopped) {
            stopped = true;
            observer.OnError(e);
          }
        },
        () => {
          if (!stopped) {
            completed2 = true;
            if (completed1) {
              stopped = true;
              observer.OnCompleted();
            }
          }
        }));

      return Util.createDisposable(() => {
        h1.Dispose();
        h2.Dispose();
      });
    });
  }

  static pairwise<T>(source: IObservable<T>) {
    return <IObservable<Tuple<T, T>>>new Observable<Tuple<T, T>>(observer => {
      let last: T = null;
      return source.Subscribe(new Observer<T>(next => {
        if (last != null)
          observer.OnNext([last, next]);
        last = next;
      }, observer.OnError, observer.OnCompleted));
    });
  }

  static partition<T>(predicate: (x: T) => boolean, source: IObservable<T>) {
    return Tuple(FObservable.filter(predicate, source), FObservable.filter(x => !predicate(x), source));
  }

  static scan<U, T>(collector: (u: U, t: T) => U, state: U, source: IObservable<T>) {
    return <IObservable<U>>new Observable<U>(observer => {
      return source.Subscribe(new Observer<T>(t => {
        FObservable.__protect(
          () => collector(state, t),
          u => { state = u; observer.OnNext(u); },
          observer.OnError);
      }, observer.OnError, observer.OnCompleted));
    });
  }

  static split<T, U1, U2>(splitter: (x: T) => Choice<U1, U2>, source: IObservable<T>) {
    return Tuple(FObservable.choose(v => splitter(v).valueIfChoice1, source), FObservable.choose(v => splitter(v).valueIfChoice2, source));
  }

  static subscribe<T>(callback: (x: T) => Unit, source: IObservable<T>) {
    return source.Subscribe(new Observer(callback));
  }
}
export { FObservable as Observable }

export type Delegate<T> = (x: T) => void;
export type DotNetDelegate<T> = (sender: any, x: T) => void;

export interface IDelegateEvent<T> {
  AddHandler(d: DotNetDelegate<T>): void;
  RemoveHandler(d: DotNetDelegate<T>): void;
}

export interface IEvent<T> extends IObservable<T>, IDelegateEvent<T> {
  Publish: IEvent<T>;
  Trigger(x: T): void;
}

export class Event<T> implements IEvent<T> {
  private _subscriber: (o: IObserver<T>) => IDisposable;
  private delegates: Array<Delegate<T>>;

  constructor(_subscriber?: (o: IObserver<T>) => IDisposable, delegates?: any[]) {
    this._subscriber = _subscriber;
    this.delegates = delegates || new Array<Delegate<T>>();
  }

  public Add(f: Delegate<T>) {
    this._addHandler(f);
  }

  // IEvent<T> methods

  public get Publish() {
    return this;
  }

  public Trigger(value: T) {
    Seq.iterate(f => f(value), this.delegates);
  }

  // IDelegateEvent<T> methods

  private _addHandler(f: Delegate<T>) {
    this.delegates.push(f);
  }

  private _removeHandler(f: Delegate<T>) {
    const index = this.delegates.findIndex(el => "" + el == "" + f);  // Special dedication to Chet Husk.
    if (index > -1)
      this.delegates.splice(index, 1);
  }

  public AddHandler(handler: DotNetDelegate<T>) {
    this._addHandler(x => handler(undefined, x));
  }

  public RemoveHandler(handler: DotNetDelegate<T>) {
    this._removeHandler(x => handler(undefined, x));
  }

  // IObservable<T> methods

  private _subscribeFromObserver(observer: IObserver<T>) {
    if (this._subscriber)
      return this._subscriber(observer);

    const callback = observer.OnNext;
    this._addHandler(callback);
    return Util.createDisposable(() => this._removeHandler(callback));
  }

  private _subscribeFromCallback(callback: Delegate<T>) {
    this._addHandler(callback);
    return Util.createDisposable(() => this._removeHandler(callback));
  }

  public Subscribe(arg: IObserver<T> | Delegate<T>) {
    return typeof arg == "function"
      ? this._subscribeFromCallback(<Delegate<T>>arg)
      : this._subscribeFromObserver(<IObserver<T>>arg);
  }

  static add<T>(callback: (x: T) => Unit, sourceEvent: IEvent<T>) {
    (<Event<T>>sourceEvent).Subscribe(new Observer(callback));
  }

  static choose<T, U>(chooser: (x: T) => U, sourceEvent: IEvent<T>) {
    const source = <Event<T>>sourceEvent;
    return <IEvent<U>>new Event<U>(observer =>
      source.Subscribe(new Observer<T>(t =>
        FObservable.__protect(
          () => chooser(t),
          u => { if (u != null) observer.OnNext(u); },
          observer.OnError),
        observer.OnError, observer.OnCompleted)),
      source.delegates);
  }

  static filter<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>) {
    return Event.choose(x => predicate(x) ? x : null, sourceEvent);
  }

  static map<T, U>(mapping: (x: T) => U, sourceEvent: IEvent<T>) {
    const source = <Event<T>>sourceEvent;
    return <IEvent<U>>new Event<U>(observer =>
      source.Subscribe(new Observer<T>(t =>
        FObservable.__protect(
          () => mapping(t),
          observer.OnNext,
          observer.OnError),
        observer.OnError, observer.OnCompleted)),
      source.delegates);
  }

  static merge<T>(event1: IEvent<T>, event2: IEvent<T>) {
    const source1 = <Event<T>>event1;
    const source2 = <Event<T>>event2;
    return <IEvent<T>>new Event<T>(observer => {
      let stopped = false, completed1 = false, completed2 = false;

      const h1 = source1.Subscribe(new Observer<T>(
        v => { if (!stopped) observer.OnNext(v); },
        e => {
          if (!stopped) {
            stopped = true;
            observer.OnError(e);
          }
        },
        () => {
          if (!stopped) {
            completed1 = true;
            if (completed2) {
              stopped = true;
              observer.OnCompleted();
            }
          }
        }));

      const h2 = source2.Subscribe(new Observer<T>(
        v => { if (!stopped) observer.OnNext(v); },
        e => {
          if (!stopped) {
            stopped = true;
            observer.OnError(e);
          }
        },
        () => {
          if (!stopped) {
            completed2 = true;
            if (completed1) {
              stopped = true;
              observer.OnCompleted();
            }
          }
        }));

      return Util.createDisposable(() => {
        h1.Dispose();
        h2.Dispose();
      });
    }, source1.delegates.concat(source2.delegates));
  }

  static pairwise<T>(sourceEvent: IEvent<T>) {
    const source = <Event<T>>sourceEvent;
    return <IEvent<Tuple<T, T>>>new Event<Tuple<T, T>>(observer => {
      let last: T = null;
      return source.Subscribe(new Observer<T>(next => {
        if (last != null)
          observer.OnNext([last, next]);
        last = next;
      }, observer.OnError, observer.OnCompleted));
    }, source.delegates);
  }

  static partition<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>) {
    return Tuple(Event.filter(predicate, sourceEvent), Event.filter(x => !predicate(x), sourceEvent));
  }

  static scan<U, T>(collector: (u: U, t: T) => U, state: U, sourceEvent: IEvent<T>) {
    const source = <Event<T>>sourceEvent;
    return <IEvent<U>>new Event<U>(observer => {
      return source.Subscribe(new Observer<T>(t => {
        FObservable.__protect(
          () => collector(state, t),
          u => { state = u; observer.OnNext(u); },
          observer.OnError);
      }, observer.OnError, observer.OnCompleted));
    }, source.delegates);
  }

  static split<T, U1, U2>(splitter: (x: T) => Choice<U1, U2>, sourceEvent: IEvent<T>) {
    return Tuple(Event.choose(v => splitter(v).valueIfChoice1, sourceEvent), Event.choose(v => splitter(v).valueIfChoice2, sourceEvent));
  }
}

export class Lazy<T> {
  public factory: () => T;
  public isValueCreated: boolean;

  private createdValue: T;

  constructor(factory: () => T) {
    this.factory = factory;
    this.isValueCreated = false;
  }

  static createFromValue<T>(v: T) {
    return new Lazy(() => v);
  }

  get value() {
    if (!this.isValueCreated) {
      this.createdValue = this.factory();
      this.isValueCreated = true;
    }
    return this.createdValue;
  }
}
