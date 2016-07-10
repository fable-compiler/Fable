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
        global.fableCoreTsc = mod.exports;
    }
})(this, function (exports) {
    "use strict";

    Object.defineProperty(exports, "__esModule", {
        value: true
    });

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

    var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) {
        return typeof obj;
    } : function (obj) {
        return obj && typeof Symbol === "function" && obj.constructor === Symbol ? "symbol" : typeof obj;
    };

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    //
    // TODO: enable "noImplicitReturns"
    // TODO: Improve types / remove "any"s
    // TODO: Replace "= function ()" with member functions. -- Keeping them for now to minimize differences when comparing to fable-core.js
    // TODO: One class per file.
    //
    var FSymbol = {
        interfaces: Symbol("interfaces"),
        typeName: Symbol("typeName")
    };
    exports.Symbol = FSymbol;

    var Choice = exports.Choice = function Choice(t, d) {
        _classCallCheck(this, Choice);

        this.Case = t;
        this.Fields = [d];
    };

    var Util = exports.Util = function Util() {
        _classCallCheck(this, Util);
    };

    Util.__types = new Map();
    // For legacy reasons the name is kept, but this method also adds
    // the type name to a cache. Use it after declaration:
    // Util.setInterfaces(Foo.prototype, ["IFoo", "IBar"], "MyModule.Foo");
    Util.setInterfaces = function (proto, interfaces, typeName) {
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
    Util.hasInterface = function (obj, infc) {
        return Array.isArray(obj[FSymbol.interfaces]) && obj[FSymbol.interfaces].indexOf(infc) >= 0;
    };
    Util.getRestParams = function (args, idx) {
        for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++) {
            restArgs[_key - idx] = args[_key];
        }
        return restArgs;
    };
    Util.compareTo = function (x, y) {
        function isCollectionComparable(o) {
            return Array.isArray(o) || ArrayBuffer.isView(o) || o instanceof List || o instanceof Map || o instanceof Set;
        }
        function sortIfMapOrSet(o) {
            return o instanceof Map || o instanceof Set ? Array.from(o).sort() : o;
        }
        if ((typeof x === "undefined" ? "undefined" : _typeof(x)) != (typeof y === "undefined" ? "undefined" : _typeof(y))) {
            return -1;
        }
        if (x != null && y != null && (typeof x === "undefined" ? "undefined" : _typeof(x)) == "object" && (typeof y === "undefined" ? "undefined" : _typeof(y)) == "object") {
            var lengthComp;
            if (Object.getPrototypeOf(x) != Object.getPrototypeOf(y)) {
                return -1;
            }
            if (Util.hasInterface(x, "System.IComparable")) {
                return x.compareTo(y);
            }
            if (isCollectionComparable(x)) {
                lengthComp = Util.compareTo(Seq._length(x), Seq._length(y));
                return lengthComp != 0 ? lengthComp : Seq.fold2(function (prev, v1, v2) {
                    return prev != 0 ? prev : Util.compareTo(v1, v2);
                }, 0, sortIfMapOrSet(x), sortIfMapOrSet(y));
            }
            if (x instanceof Date) {
                return x < y ? -1 : x > y ? 1 : 0;
            }
            var keys1 = Object.getOwnPropertyNames(x),
                keys2 = Object.getOwnPropertyNames(y);
            lengthComp = Util.compareTo(keys1.length, keys2.length);
            return lengthComp != 0 ? lengthComp : Seq.fold2(function (prev, k1, k2) {
                return prev != 0 ? prev : Util.compareTo(x[k1], y[k2]);
            }, 0, keys1.sort(), keys2.sort());
        }
        return x < y ? -1 : x > y ? 1 : 0;
    };
    Util.createObj = function (fields) {
        return Seq.fold(function (acc, kv) {
            acc[kv[0]] = kv[1];
            return acc;
        }, {}, fields);
    };
    Util.toJson = function (o) {
        function replacer(k, v) {
            if (ArrayBuffer.isView(v)) {
                return Array.from(v);
            }
            if ((typeof v === "undefined" ? "undefined" : _typeof(v)) == "object") {
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
    Util.ofJson = function (json) {
        function reviver(k, v) {
            if ((typeof v === "undefined" ? "undefined" : _typeof(v)) == "object" && v.__type) {
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

    var TimeSpan = exports.TimeSpan = function TimeSpan() {
        _classCallCheck(this, TimeSpan);
    };

    TimeSpan.create = function () {
        var d = arguments.length <= 0 || arguments[0] === undefined ? 0 : arguments[0];
        var h = arguments.length <= 1 || arguments[1] === undefined ? 0 : arguments[1];
        var m = arguments.length <= 2 || arguments[2] === undefined ? 0 : arguments[2];
        var s = arguments.length <= 3 || arguments[3] === undefined ? 0 : arguments[3];
        var ms = arguments.length <= 4 || arguments[4] === undefined ? 0 : arguments[4];

        switch (arguments.length) {
            case 1:
                // ticks
                return arguments[0] / 10000;
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
    TimeSpan.fromTicks = TimeSpan.create;
    TimeSpan.fromDays = function (d) {
        return TimeSpan.create(d, 0, 0, 0);
    };
    TimeSpan.fromHours = function (h) {
        return TimeSpan.create(h, 0, 0);
    };
    TimeSpan.fromMinutes = function (m) {
        return TimeSpan.create(0, m, 0);
    };
    TimeSpan.fromSeconds = function (s) {
        return TimeSpan.create(0, 0, s);
    };
    TimeSpan.days = function (ts) {
        return Math.floor(ts / 86400000);
    };
    TimeSpan.hours = function (ts) {
        return Math.floor(ts % 86400000 / 3600000);
    };
    TimeSpan.minutes = function (ts) {
        return Math.floor(ts % 3600000 / 60000);
    };
    TimeSpan.seconds = function (ts) {
        return Math.floor(ts % 60000 / 1000);
    };
    TimeSpan.milliseconds = function (ts) {
        return Math.floor(ts % 1000);
    };
    TimeSpan.ticks = function (ts) {
        return ts * 10000;
    };
    TimeSpan.totalDays = function (ts) {
        return ts / 86400000;
    };
    TimeSpan.totalHours = function (ts) {
        return ts / 3600000;
    };
    TimeSpan.totalMinutes = function (ts) {
        return ts / 60000;
    };
    TimeSpan.totalSeconds = function (ts) {
        return ts / 1000;
    };
    TimeSpan.duration = Math.abs;
    TimeSpan.negate = function (ts) {
        return -ts;
    };
    TimeSpan.add = function (ts1, ts2) {
        return ts1 + ts2;
    };
    TimeSpan.subtract = function (ts1, ts2) {
        return ts1 - ts2;
    };
    TimeSpan.compareTo = Util.compareTo;
    TimeSpan.compare = Util.compareTo;

    var FDate = function FDate() {
        _classCallCheck(this, FDate);
    };

    FDate.__changeKind = function (d, kind) {
        var d2;
        return d.kind == kind ? d : (d2 = new Date(d.getTime()), d2.kind = kind, d2);
    };
    FDate.__getValue = function (d, key) {
        return d.kind == 1 ? d['getUTC' + key]() : d['get' + key]();
    };
    FDate.minValue = function () {
        return FDate.parse(-8640000000000000, 1);
    };
    FDate.maxValue = function () {
        return FDate.parse(8640000000000000, 1);
    };
    FDate.parse = function (v, kind) {
        var date = v == null ? new Date() : new Date(v);
        if (isNaN(date.getTime())) {
            throw "The string is not a valid Date.";
        }
        date.kind = kind || 2; // Local
        return date;
    };
    FDate.create = function (year, month, day, h, m, s, ms, kind) {
        h = h || 0, m = m || 0, s = s || 0, ms = ms || 0, kind = kind || 2;
        var date = kind === 1 // UTC
        ? new Date(Date.UTC(year, month - 1, day, h, m, s, ms)) : new Date(year, month - 1, day, h, m, s, ms);
        if (isNaN(date.getTime())) {
            throw "The parameters describe an unrepresentable Date.";
        }
        date.kind = kind;
        return date;
    };
    FDate.now = FDate.parse;
    FDate.utcNow = function () {
        return FDate.parse(null, 1);
    };
    FDate.today = function () {
        return FDate.date(FDate.now());
    };
    FDate.isLeapYear = function (year) {
        return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
    };
    FDate.daysInMonth = function (year, month) {
        if (month == 2) {
            return FDate.isLeapYear(year) ? 29 : 28;
        } else {
            return month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
        }
    };
    FDate.toUniversalTime = function (d) {
        return FDate.__changeKind(d, 1);
    };
    FDate.toLocalTime = function (d) {
        return FDate.__changeKind(d, 2);
    };
    FDate.timeOfDay = function (d) {
        return TimeSpan.create(FDate.hour(d), FDate.minute(d), FDate.second(d));
    };
    FDate.date = function (d) {
        return FDate.create(FDate.year(d), FDate.month(d), FDate.day(d), 0, 0, 0, 0, d.kind);
    };
    FDate.day = function (d) {
        return FDate.__getValue(d, "Date");
    };
    FDate.hour = function (d) {
        return FDate.__getValue(d, "Hours");
    };
    FDate.millisecond = function (d) {
        return FDate.__getValue(d, "Milliseconds");
    };
    FDate.minute = function (d) {
        return FDate.__getValue(d, "Minutes");
    };
    FDate.month = function (d) {
        return FDate.__getValue(d, "Month") + 1;
    };
    FDate.second = function (d) {
        return FDate.__getValue(d, "Seconds");
    };
    FDate.year = function (d) {
        return FDate.__getValue(d, "FullYear");
    };
    FDate.ticks = function (d) {
        return (d.getTime() + 6.2135604e+13 /* millisecondsJSOffset */) * 10000;
    };
    FDate.toBinary = FDate.ticks;
    FDate.dayOfWeek = function (d) {
        return FDate.__getValue(d, "Day");
    };
    FDate.dayOfYear = function (d) {
        var year = FDate.year(d),
            month = FDate.month(d),
            day = FDate.day(d);
        for (var i = 1; i < month; i++) {
            day += FDate.daysInMonth(year, i);
        }
        return day;
    };
    FDate.add = function (d, ts) {
        return FDate.parse(d.getTime() + ts, d.kind);
    };
    FDate.op_Addition = FDate.add;
    FDate.addDays = function (d, v) {
        return FDate.parse(d.getTime() + v * 86400000, d.kind);
    };
    FDate.addHours = function (d, v) {
        return FDate.parse(d.getTime() + v * 3600000, d.kind);
    };
    FDate.addMinutes = function (d, v) {
        return FDate.parse(d.getTime() + v * 60000, d.kind);
    };
    FDate.addSeconds = function (d, v) {
        return FDate.parse(d.getTime() + v * 1000, d.kind);
    };
    FDate.addMilliseconds = function (d, v) {
        return FDate.parse(d.getTime() + v, d.kind);
    };
    FDate.addTicks = function (d, v) {
        return FDate.parse(d.getTime() + v / 10000, d.kind);
    };
    FDate.addYears = function (d, v) {
        var newMonth = FDate.month(d),
            newYear = FDate.year(d) + v,
            daysInMonth = FDate.daysInMonth(newYear, newMonth),
            newDay = Math.min(daysInMonth, FDate.day(d));
        return FDate.create(newYear, newMonth, newDay, FDate.hour(d), FDate.minute(d), FDate.second(d), FDate.millisecond(d), d.kind);
    };
    FDate.addMonths = function (d, v) {
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
    FDate.subtract = function (d, that) {
        return typeof that == "number" ? FDate.parse(d.getTime() - that, d.kind) : d.getTime() - that.getTime();
    };
    FDate.op_Subtraction = FDate.subtract;
    FDate.toLongDateString = function (d) {
        return d.toDateString();
    };
    FDate.toShortDateString = function (d) {
        return d.toLocaleDateString();
    };
    FDate.toLongTimeString = function (d) {
        return d.toLocaleTimeString();
    };
    FDate.toShortTimeString = function (d) {
        return d.toLocaleTimeString().replace(/:\d\d(?!:)/, '');
    };
    FDate.equals = function (d1, d2) {
        return d1.getTime() == d2.getTime();
    };
    FDate.compareTo = Util.compareTo;
    FDate.compare = Util.compareTo;
    exports.Date = FDate;

    var Timer = exports.Timer = function () {
        function Timer(interval) {
            _classCallCheck(this, Timer);

            this.interval = interval > 0 ? interval : 100;
            this.autoReset = true;
            this._elapsed = new Event();
        }

        _createClass(Timer, [{
            key: "elapsed",
            get: function get() {
                return this._elapsed;
            }
        }, {
            key: "enabled",
            get: function get() {
                return this._enabled;
            },
            set: function set(x) {
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
        }]);

        return Timer;
    }();

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

    var FString = function FString() {
        _classCallCheck(this, FString);
    };

    FString.fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;
    FString.fsFormat = function (str) {
        function isObject(x) {
            return x !== null && (typeof x === "undefined" ? "undefined" : _typeof(x)) === 'object' && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
        }
        ;
        function formatOnce(str, rep) {
            return str.replace(FString.fsFormatRegExp, function (_, prefix, flags, pad, precision, format) {
                switch (format) {
                    case "f":
                    case "F":
                        rep = rep.toFixed(precision || 6);
                        break;
                    case "g":
                    case "G":
                        rep = rep.toPrecision(precision);
                        break;
                    case "e":
                    case "E":
                        rep = rep.toExponential(precision);
                        break;
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
        function makeFn(str) {
            return function (rep) {
                var str2 = formatOnce(str, rep);
                return FString.fsFormatRegExp.test(str2) ? makeFn(str2) : _cont(str2.replace(/%%/g, '%'));
            };
        }
        var _cont;
        return function (cont) {
            _cont = cont;
            return FString.fsFormatRegExp.test(str) ? makeFn(str) : _cont(str);
        };
    };
    FString.formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;
    FString.format = function (str, args) {
        args = Util.getRestParams(arguments, 1);
        return str.replace(FString.formatRegExp, function (match, idx, pad, format) {
            var rep = args[idx];
            if (typeof rep === 'number') {
                switch ((format || '').substring(0, 1)) {
                    case "f":
                    case "F":
                        rep = format.length > 1 ? rep.toFixed(format.substring(1)) : rep.toFixed(2);
                        break;
                    case "g":
                    case "G":
                        rep = format.length > 1 ? rep.toPrecision(format.substring(1)) : rep.toPrecision();
                        break;
                    case "e":
                    case "E":
                        rep = format.length > 1 ? rep.toExponential(format.substring(1)) : rep.toExponential();
                        break;
                    case "p":
                    case "P":
                        rep = (format.length > 1 ? (rep * 100).toFixed(format.substring(1)) : (rep * 100).toFixed(2)) + " %";
                        break;
                }
            } else if (rep instanceof Date) {
                if (format.length === 1) {
                    switch (format) {
                        case "D":
                            rep = rep.toDateString();
                            break;
                        case "T":
                            rep = rep.toLocaleTimeString();
                            break;
                        case "d":
                            rep = rep.toLocaleDateString();
                            break;
                        case "t":
                            rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, '');
                            break;
                    }
                }
                rep = format.replace(/\w+/g, function (match2) {
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
    FString.init = function (n, f) {
        if (n < 0) {
            throw "String length must be non-negative";
        }
        var xs = new Array(n);
        for (var i = 0; i < n; i++) {
            xs[i] = f(i);
        }
        return xs.join("");
    };
    FString.isNullOrEmpty = function (str) {
        return typeof str !== "string" || str.length == 0;
    };
    FString.isNullOrWhiteSpace = function (str) {
        return typeof str !== "string" || /^\s*$/.test(str);
    };
    FString.padLeft = function (str, len, ch, isRight) {
        var i = -1;
        ch = ch || ' ';
        str = String(str);
        len = len - str.length;
        while (++i < len) {
            str = isRight ? str + ch : ch + str;
        }
        return str;
    };
    FString.padRight = function (str, len, ch) {
        return FString.padLeft(str, len, ch, true);
    };
    FString.replace = function (str, search, replace) {
        return str.replace(new RegExp(FRegExp.escape(search), "g"), replace);
    };
    FString.replicate = function (n, x) {
        return FString.init(n, function () {
            return x;
        });
    };
    FString.split = function (str, splitters, count, removeEmpty) {
        count = typeof count == "number" ? count : null;
        removeEmpty = typeof removeEmpty == "number" ? removeEmpty : null;
        if (count < 0) {
            throw "Count cannot be less than zero";
        }
        if (count === 0) {
            return [];
        }
        splitters = Array.isArray(splitters) ? splitters : Util.getRestParams(arguments, 1);
        splitters = splitters.filter(function (x) {
            return x;
        }).map(function (x) {
            return FRegExp.escape(x);
        });
        splitters = splitters.length > 0 ? splitters : [" "];
        var m,
            i = 0,
            splits = [],
            reg = new RegExp(splitters.join("|"), "g");
        while ((count == null || count > 1) && (m = reg.exec(str)) !== null) {
            if (!removeEmpty || m.index - i > 0) {
                count = count != null ? count - 1 : count;
                splits.push(str.substring(i, m.index));
            }
            i = reg.lastIndex;
        }
        if (!removeEmpty || str.length - i > 0) splits.push(str.substring(i));
        return splits;
    };
    FString.join = function (delimiter, xs) {
        xs = typeof xs == "string" ? Util.getRestParams(arguments, 1) : xs;
        return (Array.isArray(xs) ? xs : Array.from(xs)).join(delimiter);
    };
    FString.concat = FString.join;
    FString.endsWith = function (str, search) {
        var idx = str.lastIndexOf(search);
        return idx >= 0 && idx == str.length - search.length;
    };
    FString.newGuid = function newGuid() {
        var i,
            random,
            uuid = '';
        for (i = 0; i < 32; i++) {
            random = Math.random() * 16 | 0;
            if (i === 8 || i === 12 || i === 16 || i === 20) {
                uuid += '-';
            }
            uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
        }
        return uuid;
    };
    exports.String = FString;

    var FRegExp = function FRegExp() {
        _classCallCheck(this, FRegExp);
    };

    FRegExp.create = function (pattern, options) {
        var flags = "g";
        flags += options & 1 ? "i" : "";
        flags += options & 2 ? "m" : "";
        return new RegExp(pattern, flags);
    };
    // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
    FRegExp.escape = function (str) {
        return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&');
    };
    FRegExp.unescape = function (str) {
        return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, '$1');
    };
    FRegExp.isMatch = function (str, pattern, options) {
        var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
        return reg.test(str);
    };
    FRegExp.match = function (str, pattern, options) {
        var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
        return reg.exec(str);
    };
    FRegExp.matches = function (str, pattern, options) {
        var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options != null ? options : 0, reg) : reg = FRegExp.create(pattern, options);
        if (!reg.global) {
            throw "Non-global RegExp"; // Prevent infinite loop
        }
        var m,
            matches = [];
        while ((m = reg.exec(str)) !== null) {
            matches.push(m);
        }
        return matches;
    };
    FRegExp.options = function (reg) {
        var options = 256; // ECMAScript
        options |= reg.ignoreCase ? 1 : 0;
        options |= reg.multiline ? 2 : 0;
        return options;
    };
    FRegExp.replace = function (reg, input, replacement, limit, offset) {
        if (typeof reg == "string") {
            var tmp = reg;
            reg = FRegExp.create(input, limit);
            input = tmp, limit = undefined;
        }
        if (typeof replacement == "function") {
            limit = limit == null ? -1 : limit;
            offset = offset == null ? 0 : offset;
            var replacer = function replacer() {
                var res = arguments[0];
                if (limit !== 0) {
                    limit--;
                    var match = [];
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
                var m;
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
    FRegExp.split = function (reg, input, limit, offset) {
        if (typeof reg == "string") {
            var tmp = reg;
            reg = FRegExp.create(input, limit);
            input = tmp, limit = undefined;
        }
        input = offset != null ? input.substring(offset) : input;
        return input.split(reg, limit);
    };
    exports.RegExp = FRegExp;

    var FArray = function FArray() {
        _classCallCheck(this, FArray);
    };

    FArray.addRangeInPlace = function (range, xs) {
        Seq.iter(function (x) {
            xs.push(x);
        }, range);
    };
    FArray.blit = function (source, sourceIndex, target, targetIndex, count) {
        while (count--) {
            target[targetIndex++] = source[sourceIndex++];
        }
    };
    FArray.partition = function (f, xs) {
        var ys = [],
            zs = [],
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
    FArray.permute = function (f, xs) {
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
    FArray.removeInPlace = function (item, xs) {
        var i = xs.indexOf(item);
        if (i > -1) {
            xs.splice(i, 1);
            return true;
        }
        return false;
    };
    FArray.setSlice = function (target, lower, upper, source) {
        var length = (upper || target.length - 1) - lower;
        if (ArrayBuffer.isView(target) && source.length <= length) {
            target.set(source, lower);
        } else {
            for (var i = lower | 0, j = 0; j <= length; i++, j++) {
                target[i] = source[j];
            }
        }
    };
    FArray.sortInPlaceBy = function (f, xs, dir) {
        dir = dir || 1;
        return xs.sort(function (x, y) {
            x = f(x);
            y = f(y);
            return (x < y ? -1 : x == y ? 0 : 1) * dir;
        });
    };
    FArray.unzip = function (xs) {
        var bs = new Array(xs.length),
            cs = new Array(xs.length);
        for (var i = 0; i < xs.length; i++) {
            bs[i] = xs[i][0];
            cs[i] = xs[i][1];
        }
        return [bs, cs];
    };
    FArray.unzip3 = function (xs) {
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
    exports.Array = FArray;

    var List = exports.List = function () {
        function List(head, tail) {
            _classCallCheck(this, List);

            this.head = head;
            this.tail = tail;
        }

        List.ofArray = function ofArray(args, base) {
            var acc = base || new List();
            for (var i = args.length - 1; i >= 0; i--) {
                acc = new List(args[i], acc);
            }
            return acc;
        };

        List.prototype[Symbol.iterator] = function () {
            var cur = this;
            return {
                next: function next() {
                    var tmp = cur;
                    cur = cur.tail;
                    return { done: tmp.tail == null, value: tmp.head };
                }
            };
        };

        List.prototype.append = function append(ys) {
            return List.append(this, ys);
        };

        List.append = function append(xs, ys) {
            return Seq.fold(function (acc, x) {
                return new List(x, acc);
            }, ys, List.rev(xs));
        };

        List.prototype.choose = function choose(f, xs) {
            return List.choose(f, this);
        };

        List.choose = function choose(f, xs) {
            var r = Seq.fold(function (acc, x) {
                var y = f(x);
                return y != null ? new List(y, acc) : acc;
            }, new List(), xs);
            return List.rev(r);
        };

        List.prototype.collect = function collect(f) {
            return List.collect(f, this);
        };

        List.collect = function collect(f, xs) {
            return Seq.fold(function (acc, x) {
                return acc.append(f(x));
            }, new List(), xs);
        };
        // ToDo: 'xs' should be Seq<List<T>>


        List.concat = function concat(xs) {
            return List.collect(function (x) {
                return x;
            }, xs);
        };

        List.prototype.filter = function filter(f) {
            return List.filter(f, this);
        };

        List.filter = function filter(f, xs) {
            return List.rev(Seq.fold(function (acc, x) {
                return f(x) ? new List(x, acc) : acc;
            }, new List(), xs));
        };

        List.prototype.where = function where(f) {
            return List.filter(f, this);
        };

        List.where = function where(f, xs) {
            return List.filter(f, xs);
        };

        List.init = function init(n, f) {
            if (n < 0) {
                throw "List length must be non-negative";
            }
            var xs = new List();
            for (var i = 1; i <= n; i++) {
                xs = new List(f(n - i), xs);
            }
            return xs;
        };

        List.prototype.map = function map(f) {
            return List.map(f, this);
        };

        List.map = function map(f, xs) {
            return List.rev(Seq.fold(function (acc, x) {
                return new List(f(x), acc);
            }, new List(), xs));
        };

        List.prototype.mapi = function mapi(f) {
            return List.mapi(f, this);
        };

        List.mapi = function mapi(f, xs) {
            return List.rev(Seq.fold(function (acc, x, i) {
                return new List(f(i, x), acc);
            }, new List(), xs));
        };

        List.prototype.partition = function partition(f) {
            return List.partition(f, this);
        };

        List.partition = function partition(f, xs) {
            var ini = [new List(), new List()];
            return Seq.fold(function (acc, x) {
                var lacc = acc[0],
                    racc = acc[1];
                var l = [new List(x, lacc), racc];
                var r = [lacc, new List(x, racc)];
                return f(x) ? l : r;
            }, ini, List.rev(xs));
        };

        List.replicate = function replicate(n, x) {
            return List.init(n, function () {
                return x;
            });
        };

        List.prototype.rev = function rev() {
            return List.rev(this);
        };

        List.rev = function rev(xs) {
            return Seq.fold(function (acc, x) {
                return new List(x, acc);
            }, new List(), xs);
        };

        List.singleton = function singleton(x) {
            return new List(x, new List());
        };

        List.prototype.slice = function slice(lower, upper) {
            return List.slice(lower, upper, this);
        };

        List.slice = function slice(lower, upper, xs) {
            var noLower = lower == null;
            var noUpper = upper == null;
            return List.rev(Seq.fold(function (acc, x, i) {
                return (noLower || lower <= i) && (noUpper || i <= upper) ? new List(x, acc) : acc;
            }, new List(), xs));
        };
        /* ToDo: ?
        unzip<T1, T2, T extends TTuple<T1, T2>>(): [List<T1>, List<T2>] {
          return List.unzip(this);
        } */


        List.unzip = function unzip(xs) {
            var ini = [new List(), new List()];
            return Seq.foldBack(function (xy, acc) {
                return [new List(xy[0], acc[0]), new List(xy[1], acc[1])];
            }, xs, ini);
        };
        /* ToDo: unzip3() */


        List.unzip3 = function unzip3(xs) {
            var ini = [new List(), new List(), new List()];
            return Seq.foldBack(function (xyz, acc) {
                return [new List(xyz[0], acc[0]), new List(xyz[1], acc[1]), new List(xyz[2], acc[2])];
            }, xs, ini);
        };

        _createClass(List, [{
            key: "length",
            get: function get() {
                return Seq.fold(function (acc, x) {
                    return acc + 1;
                }, 0, this);
            }
        }]);

        return List;
    }();

    var Seq = exports.Seq = function () {
        function Seq() {
            _classCallCheck(this, Seq);
        }

        Seq.fold = function fold(f, acc, xs) {
            if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
                return xs.reduce(f, acc);
            } else {
                var cur = null;
                for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
                    cur = iter.next();
                    if (cur.done) {
                        break;
                    }
                    acc = f(acc, cur.value, i);
                }
                return acc;
            }
        };

        Seq.foldBack = function foldBack(f, xs, acc) {
            var arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
            for (var i = arr.length - 1; i >= 0; i--) {
                acc = f(arr[i], acc, i);
            }
            return acc;
        };

        Seq.fold2 = function fold2(f, acc, xs, ys) {
            var iter1 = xs[Symbol.iterator](),
                iter2 = ys[Symbol.iterator]();
            var cur1 = void 0,
                cur2 = void 0;
            for (var i = 0;; i++) {
                cur1 = iter1.next();
                cur2 = iter2.next();
                if (cur1.done || cur2.done) {
                    break;
                }
                acc = f(acc, cur1.value, cur2.value, i);
            }
            return acc;
        };
        // A static 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442


        Seq._length = function _length(xs) {
            return Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.length : Seq.fold(function (acc, x) {
                return acc + 1;
            }, 0, xs);
        };

        return Seq;
    }();

    Seq.__failIfNone = function (res) {
        if (res == null) {
            throw "Seq did not contain any matching element";
        }
        return res;
    };
    Seq.toList = function (xs) {
        return Seq.foldBack(function (x, acc) {
            return new List(x, acc);
        }, xs, new List());
    };
    Seq.ofList = function (xs) {
        return Seq.delay(function () {
            return Seq.unfold(function (x) {
                return x.tail != null ? [x.head, x.tail] : null;
            }, xs);
        });
    };
    Seq.ofArray = function (xs) {
        return Seq.delay(function () {
            return Seq.unfold(function (i) {
                return i < xs.length ? [xs[i], i + 1] : null;
            }, 0);
        });
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
        return Seq.map(function (kv) {
            return [kv[0], Seq._length(kv[1])];
        }, Seq.groupBy(f, xs));
    };
    Seq.concat = function (xs) {
        return Seq.delay(function () {
            var iter = xs[Symbol.iterator]();
            return Seq.unfold(function (innerIter) {
                var cur,
                    output = null,
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
    Seq.collect = function (f, xs) {
        return Seq.concat(Seq.map(f, xs));
    };
    Seq.choose = function (f, xs) {
        var trySkipToNext = function trySkipToNext(iter) {
            var cur = iter.next();
            if (!cur.done) {
                var y = f(cur.value);
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
        return nonZero != null ? nonZero : Seq._length(xs) - Seq._length(ys);
    };
    Seq.delay = function (f) {
        var e = {};
        e[Symbol.iterator] = function () {
            return f()[Symbol.iterator]();
        };
        return e;
    };
    Seq.distinctBy = function (f, xs) {
        return Seq.choose(function (tup) {
            return tup[0];
        }, Seq.scan(function (tup, x) {
            var acc = tup[1];
            var y = f(x);
            return acc.has(y) ? [null, acc] : [x, acc.add(y)];
        }, [null, new Set()], xs));
    };
    Seq.distinct = function (xs) {
        return Seq.distinctBy(function (x) {
            return x;
        }, xs);
    };
    Seq.empty = function () {
        return Seq.unfold(function () {});
    };
    Seq.enumerateWhile = function (cond, xs) {
        return Seq.concat(Seq.unfold(function () {
            return cond() ? [xs, true] : null;
        }));
    };
    Seq.enumerateThenFinally = function (xs, finalFn) {
        return Seq.delay(function () {
            var iter;
            try {
                iter = xs[Symbol.iterator]();
            } finally {
                finalFn();
            }
            return Seq.unfold(function (iter) {
                try {
                    var cur = iter.next();
                    return !cur.done ? [cur.value, iter] : null;
                } finally {
                    finalFn();
                }
            }, iter);
        });
    };
    Seq.enumerateUsing = function (disp, work) {
        var isDisposed = false;
        var disposeOnce = function disposeOnce() {
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
        var aux = function aux(iter) {
            var cur = iter.next();
            return !cur.done && (f(cur.value) || aux(iter));
        };
        return aux(xs[Symbol.iterator]());
    };
    Seq.exists2 = function (f, xs, ys) {
        var aux = function aux(iter1, iter2) {
            var cur1 = iter1.next(),
                cur2 = iter2.next();
            return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
        };
        return aux(xs[Symbol.iterator](), ys[Symbol.iterator]());
    };
    Seq.filter = function (f, xs) {
        var trySkipToNext = function trySkipToNext(iter) {
            var cur = iter.next();
            if (!cur.done) {
                return f(cur.value) ? [cur.value, iter] : trySkipToNext(iter);
            }
        };
        return Seq.delay(function () {
            return Seq.unfold(trySkipToNext, xs[Symbol.iterator]());
        });
    };
    Seq.where = Seq.filter;
    Seq.foldBack2 = function (f, xs, ys, acc) {
        var ar1 = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : Array.from(xs);
        var ar2 = Array.isArray(ys) || ArrayBuffer.isView(ys) ? ys : Array.from(ys);
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
            var k = f(x),
                vs = acc.get(k);
            return vs != null ? acc.set(k, new List(x, vs)) : acc.set(k, new List(x, new List()));
        }, new Map(), xs);
    };
    Seq.tryHead = function (xs) {
        var iter = xs[Symbol.iterator]();
        var cur = iter.next();
        return cur.done ? null : cur.value;
    };
    Seq.head = function (xs) {
        return Seq.__failIfNone(Seq.tryHead(xs));
    };
    Seq.init = function (n, f) {
        return Seq.delay(function () {
            return Seq.unfold(function (i) {
                return i < n ? [f(i), i + 1] : null;
            }, 0);
        });
    };
    Seq.initInfinite = function (f) {
        return Seq.delay(function () {
            return Seq.unfold(function (i) {
                return [f(i), i + 1];
            }, 0);
        });
    };
    Seq.tryItem = function (i, xs) {
        if (i < 0) {
            return null;
        } else if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
            return i < xs.length ? xs[i] : null;
        } else {
            for (var j = 0, iter = xs[Symbol.iterator]();; j++) {
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
    Seq.item = function (i, xs) {
        return Seq.__failIfNone(Seq.tryItem(i, xs));
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
    Seq.tryLast = function (xs) {
        try {
            return Seq.reduce(function (_, x) {
                return x;
            }, xs);
        } catch (err) {
            return null;
        }
    };
    Seq.last = function (xs) {
        return Seq.__failIfNone(Seq.tryLast(xs));
    };
    Seq.map = function (f, xs) {
        return Seq.delay(function () {
            return Seq.unfold(function (iter) {
                var cur = iter.next();
                return !cur.done ? [f(cur.value), iter] : null;
            }, xs[Symbol.iterator]());
        });
    };
    Seq.mapi = function (f, xs) {
        return Seq.delay(function () {
            var i = 0;
            return Seq.unfold(function (iter) {
                var cur = iter.next();
                return !cur.done ? [f(i++, cur.value), iter] : null;
            }, xs[Symbol.iterator]());
        });
    };
    Seq.map2 = function (f, xs, ys) {
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
    Seq.mapi2 = function (f, xs, ys) {
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
    Seq.map3 = function (f, xs, ys, zs) {
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
        return Seq.skip(1, Seq.scan(function (last, next) {
            return [last[1], next];
        }, [0, 0], xs));
    };
    Seq.permute = function (f, xs) {
        var ar = Array.from(xs);
        return Seq.ofArray(FArray.permute(f, ar));
    };
    Seq.rangeStep = function (first, step, last) {
        if (step === 0) {
            throw "Step cannot be 0";
        }
        return Seq.unfold(function (x) {
            return step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null;
        }, first);
    };
    Seq.rangeChar = function (first, last) {
        return Seq.unfold(function (x) {
            return x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null;
        }, first);
    };
    Seq.range = function (first, last) {
        return Seq.rangeStep(first, 1, last);
    };
    Seq.readonly = function (xs) {
        return Seq.map(function (x) {
            return x;
        }, xs);
    };
    Seq.reduce = function (f, xs) {
        if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
            return xs.reduce(f);
        } else {
            var iter = xs[Symbol.iterator]();
            var cur = iter.next();
            if (cur.done) {
                throw "Seq was empty";
            }
            var acc = cur.value;
            for (;;) {
                cur = iter.next();
                if (cur.done) {
                    break;
                }
                acc = f(acc, cur.value);
            }
            return acc;
        }
    };
    Seq.reduceBack = function (f, xs) {
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
    Seq.replicate = function (n, x) {
        return Seq.init(n, function () {
            return x;
        });
    };
    Seq.rev = function (xs) {
        var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.slice(0) : Array.from(xs);
        return ar.reverse();
    };
    Seq.scan = function (f, seed, xs) {
        return Seq.delay(function () {
            var iter = xs[Symbol.iterator]();
            return Seq.unfold(function (acc) {
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
        var e = {};
        e[Symbol.iterator] = function () {
            var iter = xs[Symbol.iterator]();
            for (var i = 1; i <= n; i++) {
                if (iter.next().done) throw "Seq has not enough elements";
            }
            return iter;
        };
        return e;
    };
    Seq.skipWhile = function (f, xs) {
        return Seq.delay(function () {
            var hasPassed = false;
            return Seq.filter(function (x) {
                return hasPassed || (hasPassed = !f(x));
            }, xs);
        });
    };
    Seq.sortWith = function (f, xs) {
        var ys = Array.from(xs);
        return Seq.ofArray(ys.sort(f));
    };
    Seq.sum = function (xs, add) {
        add = add || function (x, y) {
            return x + y;
        };
        return Seq.reduce(function (acc, x) {
            return add(acc, x);
        }, xs);
    };
    Seq.sumBy = function (f, xs, add) {
        var fst = true;
        add = add || function (x, y) {
            return x + y;
        };
        return Seq.reduce(function (acc, x) {
            acc = fst ? f(acc) : acc, fst = false;
            return acc + f(x);
        }, xs);
    };
    Seq.tail = function (xs) {
        var iter = xs[Symbol.iterator]();
        var cur = iter.next();
        if (cur.done) {
            throw "Seq was empty";
        }
        var e = {};
        e[Symbol.iterator] = function () {
            return iter;
        };
        return e;
    };
    Seq.take = function (n, xs, truncate) {
        return Seq.delay(function () {
            var iter = xs[Symbol.iterator]();
            return Seq.unfold(function (i) {
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
    Seq.truncate = function (n, xs) {
        return Seq.take(n, xs, true);
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
        var cur = void 0;
        for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
            cur = iter.next();
            if (cur.done) {
                return null;
            }
            if (f(cur.value, i)) {
                return cur.value;
            }
        }
    };
    Seq.find = function (f, xs) {
        return Seq.__failIfNone(Seq.tryFind(f, xs));
    };
    Seq.tryFindBack = function (f, xs) {
        var match = null;
        var cur = void 0;
        for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
            cur = iter.next();
            if (cur.done) {
                return match;
            }
            if (f(cur.value, i)) {
                match = cur.value;
            }
        }
    };
    Seq.findBack = function (f, xs) {
        return Seq.__failIfNone(Seq.tryFindBack(f, xs));
    };
    Seq.tryFindIndex = function (f, xs) {
        var cur = void 0;
        for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
            cur = iter.next();
            if (cur.done) {
                return null;
            }
            if (f(cur.value, i)) {
                return i;
            }
        }
    };
    Seq.findIndex = function (f, xs) {
        return Seq.__failIfNone(Seq.tryFindIndex(f, xs));
    };
    Seq.tryFindIndexBack = function (f, xs) {
        var match = null;
        var cur = void 0;
        for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
            cur = iter.next();
            if (cur.done) {
                return match;
            }
            if (f(cur.value, i)) {
                match = i;
            }
        }
    };
    Seq.findIndexBack = function (f, xs) {
        return Seq.__failIfNone(Seq.tryFindIndexBack(f, xs));
    };
    Seq.tryPick = function (f, xs) {
        var cur = void 0;
        for (var i = 0, iter = xs[Symbol.iterator]();; i++) {
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
    Seq.pick = function (f, xs) {
        return Seq.__failIfNone(Seq.tryPick(f, xs));
    };
    Seq.unfold = function (f, acc) {
        var e = {};
        e[Symbol.iterator] = function () {
            return {
                next: function next() {
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

    var FSet = function FSet() {
        _classCallCheck(this, FSet);
    };

    FSet.op_Addition = function (set1, set2) {
        var set = new Set(set1);
        set2.forEach(function (x) {
            set.add(x);
        });
        return set;
    };
    FSet.union = FSet.op_Addition;
    FSet.unionMany = function (sets) {
        return Seq.fold(function (acc, s) {
            s.forEach(function (x) {
                acc.add(x);
            });
            return acc;
        }, new Set(), sets);
    };
    FSet.op_Subtraction = function (set1, set2) {
        var set = new Set(set1);
        set2.forEach(function (x) {
            set.delete(x);
        });
        return set;
    };
    FSet.difference = FSet.op_Subtraction;
    FSet.intersect = function (set1, set2) {
        var set = new Set(set1);
        set1.forEach(function (x) {
            if (!set2.has(x)) set.delete(x);
        });
        return set;
    };
    FSet.intersectMany = function (sets) {
        var ar = Array.isArray(sets) ? sets : Array.from(sets);
        if (ar.length == 0) {
            throw "Seq was empty";
        }
        var set = new Set(ar[0]);
        Seq.iter(function (x) {
            for (var i = 1; i < ar.length; i++) {
                if (!ar[i].has(x)) {
                    set.delete(x);
                    break;
                }
            }
        }, ar[0]);
        return set;
    };
    FSet.isProperSubsetOf = function (set1, set2) {
        return Seq.forall(function (x) {
            return set2.has(x);
        }, set1) && Seq.exists(function (x) {
            return !set1.has(x);
        }, set2);
    };
    FSet.isProperSubset = FSet.isProperSubsetOf;
    FSet.isSubsetOf = function (set1, set2) {
        return Seq.forall(function (x) {
            return set2.has(x);
        }, set1);
    };
    FSet.isSubset = FSet.isSubsetOf;
    FSet.isProperSupersetOf = function (set1, set2) {
        return FSet.isProperSubset(set2, set1);
    };
    FSet.isProperSuperset = FSet.isProperSupersetOf;
    FSet.isSupersetOf = function (set1, set2) {
        return FSet.isSubset(set2, set1);
    };
    FSet.isSuperset = FSet.isSupersetOf;
    FSet.copyTo = function (xs, arr, arrayIndex, count) {
        if (!Array.isArray(arr) && !ArrayBuffer.isView(arr)) throw "Array is invalid";
        count = count || arr.length;
        var i = arrayIndex || 0;
        var iter = xs[Symbol.iterator]();
        while (count--) {
            var el = iter.next();
            if (el.done) break;
            arr[i++] = el.value;
        }
        ;
    };
    FSet.partition = function (f, xs) {
        return Seq.fold(function (acc, x) {
            var lacc = acc[0],
                racc = acc[1];
            return f(x) ? [lacc.add(x), racc] : [lacc, racc.add(x)];
        }, [new Set(), new Set()], xs);
    };
    FSet.removeInPlace = function (item, xs) {
        xs.delete(item);
        return xs;
    };
    FSet.remove = function (item, xs) {
        return FSet.removeInPlace(item, new Set(xs));
    };
    exports.Set = FSet;

    var FMap = function FMap() {
        _classCallCheck(this, FMap);
    };

    FMap.containsValue = function (v, map) {
        return Seq.fold(function (acc, k) {
            return acc || map.get(k) === v;
        }, false, map.keys());
    };
    FMap.exists = function (f, map) {
        return Seq.exists(function (kv) {
            return f(kv[0], kv[1]);
        }, map);
    };
    FMap.filter = function (f, map) {
        return Seq.fold(function (acc, kv) {
            return f(kv[0], kv[1]) ? acc.set(kv[0], kv[1]) : acc;
        }, new Map(), map);
    };
    FMap.fold = function (f, seed, map) {
        return Seq.fold(function (acc, kv) {
            return f(acc, kv[0], kv[1]);
        }, seed, map);
    };
    FMap.foldBack = function (f, map, seed) {
        return Seq.foldBack(function (kv, acc) {
            return f(kv[0], kv[1], acc);
        }, map, seed);
    };
    FMap.forall = function (f, map) {
        return Seq.forall(function (kv) {
            return f(kv[0], kv[1]);
        }, map);
    };
    FMap.iter = function (f, map) {
        return Seq.iter(function (kv) {
            f(kv[0], kv[1]);
        }, map);
    };
    FMap.map = function (f, map) {
        return Seq.fold(function (acc, kv) {
            return acc.set(kv[0], f(kv[0], kv[1]));
        }, new Map(), map);
    };
    FMap.partition = function (f, map) {
        return Seq.fold(function (acc, kv) {
            var lacc = acc[0],
                racc = acc[1],
                k = kv[0],
                v = kv[1];
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
    FMap.removeInPlace = FSet.removeInPlace;
    FMap.remove = function (item, map) {
        return FMap.removeInPlace(item, new Map(map));
    };
    FMap.tryPick = function (f, map) {
        return Seq.tryPick(function (kv) {
            var res = f(kv[0], kv[1]);
            return res != null ? res : null;
        }, map);
    };
    exports.Map = FMap;

    var Async = exports.Async = function () {
        function Async() {
            _classCallCheck(this, Async);
        }

        _createClass(Async, [{
            key: "cancellationToken",
            get: function get() {
                return Async.__protectedCont(function (ctx) {
                    return ctx.onSuccess(ctx.cancelToken);
                });
            }
        }]);

        return Async;
    }();

    Async.__protectedCont = function (f) {
        return function (ctx) {
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
    Async.bind = function (work, cont) {
        return Async.__protectedCont(function (ctx) {
            work({
                onSuccess: function onSuccess(x) {
                    return cont(x)(ctx);
                },
                onError: ctx.onError,
                onCancel: ctx.onCancel,
                cancelToken: ctx.cancelToken
            });
        });
    };
    Async.combine = function (work1, work2) {
        return Async.bind(work1, function () {
            return work2;
        });
    };
    Async.delay = function (cont) {
        return Async.__protectedCont(function (ctx) {
            cont()(ctx);
        });
    };
    Async.for = function (seq, body) {
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
    Async.return = function (x) {
        return Async.__protectedCont(function (ctx) {
            ctx.onSuccess(x);
        });
    };
    Async.returnFrom = function (work) {
        return work;
    };
    Async.tryFinally = function (work, finalFn) {
        return Async.__protectedCont(function (ctx) {
            work({
                onSuccess: function onSuccess(x) {
                    finalFn();
                    ctx.onSuccess(x);
                },
                onError: function onError(x) {
                    finalFn();
                    ctx.onError(x);
                },
                onCancel: function onCancel(x) {
                    finalFn();
                    ctx.onCancel(x);
                },
                cancelToken: ctx.cancelToken
            });
        });
    };
    Async.tryWith = function (work, catchFn) {
        return Async.__protectedCont(function (ctx) {
            work({
                onSuccess: ctx.onSuccess,
                onCancel: ctx.onCancel,
                cancelToken: ctx.cancelToken,
                onError: function onError(ex) {
                    ctx.onSuccess(catchFn(ex));
                }
            });
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
        } else {
            return Async.return();
        }
    };
    Async.zero = function () {
        return Async.__protectedCont(function (ctx) {
            ctx.onSuccess();
        });
    };
    Async.start = function (work, onSuccess, onError, onCancel, cancelToken) {
        if (typeof onSuccess !== "function") {
            cancelToken = onSuccess;
            onSuccess = null;
        }
        work({
            onSuccess: onSuccess ? onSuccess : function () {},
            onError: onError ? onError : function () {},
            onCancel: onCancel ? onCancel : function () {},
            cancelToken: cancelToken ? cancelToken : {}
        });
    };
    Async.startImmediate = Async.start;
    Async.startWithContinuations = Async.start;
    Async.ignore = function (work) {
        return Async.bind(work, function () {
            return Async.return();
        });
    };
    Async.fromContinuations = function (f) {
        return Async.__protectedCont(function (ctx) {
            return f([ctx.onSuccess, ctx.onError, ctx.onCancel]);
        });
    };
    Async.startAsPromise = function (work, cancelToken) {
        return new Promise(function (resolve, reject) {
            Async.startWithContinuations(work, resolve, reject, reject, cancelToken ? cancelToken : {});
        });
    };
    Async.awaitPromise = function (p) {
        return Async.fromContinuations(function (conts) {
            p.then(conts[0]).catch(function (err) {
                (err == "cancelled" ? conts[2] : conts[1])(err);
            });
        });
    };
    Async.parallel = function (works) {
        return Async.awaitPromise(Promise.all(Seq.map(function (w) {
            return Async.startAsPromise(w);
        }, works)));
    };
    Async.catch = function (work) {
        return Async.__protectedCont(function (ctx) {
            work({
                onSuccess: function onSuccess(x) {
                    ctx.onSuccess(new Choice("Choice1Of2", x));
                },
                onError: function onError(ex) {
                    ctx.onSuccess(new Choice("Choice2Of2", ex));
                },
                onCancel: ctx.onCancel,
                cancelToken: ctx.cancelToken
            });
        });
    };
    Async.sleep = function (ms) {
        return Async.__protectedCont(function (ctx) {
            setTimeout(function () {
                ctx.cancelToken.isCancelled ? ctx.onCancel("cancelled") : ctx.onSuccess();
            }, ms);
        });
    };

    var Queue = function () {
        function Queue() {
            _classCallCheck(this, Queue);
        }

        Queue.prototype.add = function add(it) {
            var itCell = { value: it };
            if (this.firstAndLast) {
                this.firstAndLast[1].next = itCell;
                this.firstAndLast = [this.firstAndLast[0], itCell];
            } else {
                this.firstAndLast = [itCell, itCell];
            }
        };

        Queue.prototype.tryGet = function tryGet(it) {
            if (this.firstAndLast) {
                var value = this.firstAndLast[0].value;
                if (this.firstAndLast[0].next) {
                    this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
                } else {
                    delete this.firstAndLast;
                }
                return value;
            }
        };

        return Queue;
    }();

    var MailboxProcessor = exports.MailboxProcessor = function () {
        function MailboxProcessor(body) {
            _classCallCheck(this, MailboxProcessor);

            this.body = body;
            this.messages = new Queue();
        }

        MailboxProcessor.prototype.__processEvents = function __processEvents() {
            if (this.continuation) {
                var value = this.messages.tryGet();
                if (value) {
                    var cont = this.continuation;
                    delete this.continuation;
                    cont(value);
                }
            }
        };

        MailboxProcessor.prototype.start = function start() {
            Async.startImmediate(this.body(this));
        };

        MailboxProcessor.prototype.receive = function receive() {
            var _this = this;
            return Async.fromContinuations(function (conts) {
                if (_this.continuation) {
                    throw "Receive can only be called once!";
                }
                _this.continuation = conts[0];
                _this.__processEvents();
            });
        };

        MailboxProcessor.prototype.postAndAsyncReply = function postAndAsyncReply(f) {
            var result, continuation;
            function checkCompletion() {
                if (result && continuation) {
                    continuation(result);
                }
            }
            ;
            var reply = {
                reply: function reply(res) {
                    result = res;
                    checkCompletion();
                }
            };
            this.messages.add(f(reply));
            this.__processEvents();
            return Async.fromContinuations(function (conts) {
                continuation = conts[0];
                checkCompletion();
            });
        };

        MailboxProcessor.prototype.post = function post(msg) {
            this.messages.add(msg);
            this.__processEvents();
        };

        return MailboxProcessor;
    }();

    MailboxProcessor.start = function (body) {
        var mbox = new MailboxProcessor(body);
        mbox.start();
        return mbox;
    };

    var Observer = function Observer(onNext, onError, onCompleted) {
        _classCallCheck(this, Observer);

        this.onNext = onNext;
        this.onError = onError || function (e) {};
        this.onCompleted = onCompleted || function () {};
    };

    ;
    Util.setInterfaces(Observer.prototype, ["System.IObserver"]);

    var Observable = function Observable(subscribe) {
        _classCallCheck(this, Observable);

        this.subscribe = subscribe;
    };

    Util.setInterfaces(Observable.prototype, ["System.IObservable"]);

    var Obs = function Obs() {
        _classCallCheck(this, Obs);
    };

    Obs.__protect = function (f, succeed, fail) {
        try {
            succeed(f());
        } catch (e) {
            fail(e);
        }
    };
    Obs.map = function (f, w) {
        return new Observable(function (observer) {
            return w.subscribe(new Observer(function (v) {
                Obs.__protect(function () {
                    f(v);
                }, observer.onNext, observer.onError);
            }, observer.onError, observer.onCompleted));
        });
    };
    Obs.choose = function (f, w) {
        return new Observable(function (observer) {
            return w.subscribe(new Observer(function (v) {
                Obs.__protect(function () {
                    f(v);
                }, function (v) {
                    if (v != null) {
                        observer.onNext(v);
                    }
                }, observer.onError);
            }, observer.onError, observer.onCompleted));
        });
    };
    Obs.filter = function (f, w) {
        return Obs.choose(function (x) {
            return f(x) ? x : null;
        }, w);
    };
    Obs.partition = function (f, w) {
        return [Obs.filter(f, w), Obs.filter(function (x) {
            return !f(x);
        }, w)];
    };
    Obs.scan = function (f, state, w) {
        return new Observable(function (observer) {
            return w.subscribe(new Observer(function (v) {
                Obs.__protect(function () {
                    f(state, v);
                }, function (z) {
                    state = z;
                    observer.onNext(z);
                }, observer.onError);
            }, observer.onError, observer.onCompleted));
        });
    };
    Obs.add = function (f, w) {
        w.subscribe(new Observer(f));
    };
    Obs.subscribe = function (f, w) {
        return w.subscribe(new Observer(f));
    };
    Obs.pairwise = function (w) {
        return new Observable(function (observer) {
            var lastArgs = null;
            return w.subscribe(new Observer(function (args2) {
                if (lastArgs != null) {
                    observer.onNext([lastArgs, args2]);
                }
                lastArgs = args2;
            }, observer.onError, observer.onCompleted));
        });
    };
    Obs.merge = function (w1, w2) {
        return new Observable(function (observer) {
            var stopped = false,
                completed1 = false,
                completed2 = false;
            var h1 = w1.subscribe(new Observer(function (v) {
                if (!stopped) {
                    observer.onNext(v);
                }
            }, function (e) {
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
            var h2 = w2.subscribe(new Observer(function (v) {
                if (!stopped) {
                    observer.onNext(v);
                }
            }, function (e) {
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
            var disp = {
                dispose: function dispose() {
                    h1.dispose();
                    h2.dispose();
                }
            };
            disp[FSymbol.interfaces] = ["System.IDisposable"];
            return disp;
        });
    };
    Obs.split = function (f, w) {
        return [Obs.choose(function (v) {
            var res = f(v);
            return res.Case == "Choice1Of2" ? res.Fields[0] : null;
        }, w), Obs.choose(function (v) {
            var res = f(v);
            return res.Case == "Choice2Of2" ? res.Fields[0] : null;
        }, w)];
    };
    exports.Observable = Obs;

    var Event = exports.Event = function () {
        function Event(sbscrb, delegates) {
            _classCallCheck(this, Event);

            this.sbscrb = sbscrb;
            this.delegates = delegates || new Array();
        }

        Event.prototype.trigger = function trigger(value) {
            Seq.iter(function (f) {
                f(value);
            }, this.delegates);
        };

        Event.prototype._addHandler = function _addHandler(f) {
            this.delegates.push(f);
        };

        Event.prototype._removeHandler = function _removeHandler(f) {
            var fnd = function fnd(el, i, arr) {
                return '' + el == '' + f; //Special dedication to Chet Husk.
            };
            var index = this.delegates.findIndex(fnd);
            if (index > -1) {
                this.delegates.splice(index, 1);
            }
        };

        Event.prototype.subscribe = function subscribe(f) {
            var _this2 = this;

            var disp;
            return this._addHandler(f), disp = {
                dispose: function dispose() {
                    return _this2._removeHandler(f);
                }
            }, disp[FSymbol.interfaces] = ["System.IDisposable"], disp;
        };

        Event.prototype.add = function add(f) {
            this._addHandler(f);
        };

        Event.prototype.addHandler = function addHandler(f) {
            var h = function h(x) {
                return f(undefined, x);
            };
            this._addHandler(h);
        };

        Event.prototype.removeHandler = function removeHandler(f) {
            var h = function h(x) {
                return f(undefined, x);
            };
            this._removeHandler(h);
        };

        Event.prototype._subscribe = function _subscribe(observer) {
            if (this.sbscrb) return this.sbscrb(observer);
            var disp,
                f = observer.onNext;
            return this._addHandler(f), disp = {
                dispose: function dispose() {
                    this._removeHandler(f);
                }
            }, disp[FSymbol.interfaces] = ["System.IDisposable"], disp;
        };

        _createClass(Event, [{
            key: "publish",
            get: function get() {
                return this;
            }
        }]);

        return Event;
    }();

    Event.add = function (f, w) {
        w._subscribe(new Observer(f));
    };
    Event.map = function (f, w) {
        var s = function s(observer) {
            w._subscribe(new Observer(function (v) {
                Obs.__protect(function () {
                    return f(v);
                }, observer.onNext, observer.onError);
            }, observer.onError, observer.onCompleted));
        };
        return new Event(s, w.delegates);
    };
    Event.choose = function (f, w) {
        var s = function s(observer) {
            return w._subscribe(new Observer(function (v) {
                Obs.__protect(function () {
                    return f(v);
                }, function (v) {
                    if (v != null) {
                        observer.onNext(v);
                    }
                }, observer.onError);
            }, observer.onError, observer.onCompleted));
        };
        return new Event(s, w.delegates);
    };
    Event.filter = function (f, w) {
        return Event.choose(function (x) {
            return f(x) ? x : null;
        }, w);
    };
    Event.partition = function (f, w) {
        return [Event.filter(f, w), Event.filter(function (x) {
            return !f(x);
        }, w)];
    };
    Event.scan = function (f, state, w) {
        var s = function s(observer) {
            return w._subscribe(new Observer(function (v) {
                Obs.__protect(function () {
                    return f(state, v);
                }, function (z) {
                    state = z;
                    observer.onNext(z);
                }, observer.onError);
            }, observer.onError, observer.onCompleted));
        };
        return new Event(s, w.delegates);
    };
    Event.pairwise = function (w) {
        var s = function s(observer) {
            var lastArgs = null;
            return w._subscribe(new Observer(function (args2) {
                if (lastArgs != null) {
                    observer.onNext([lastArgs, args2]);
                }
                lastArgs = args2;
            }, observer.onError, observer.onCompleted));
        };
        return new Event(s, w.delegates);
    };
    Event.merge = function (w1, w2) {
        var s = function s(observer) {
            var stopped = false,
                completed1 = false,
                completed2 = false;
            var h1 = w1._subscribe(new Observer(function (v) {
                if (!stopped) {
                    observer.onNext(v);
                }
            }, function (e) {
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
            var h2 = w2._subscribe(new Observer(function (v) {
                if (!stopped) {
                    observer.onNext(v);
                }
            }, function (e) {
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
            var disp = {
                dispose: function dispose() {
                    h1.dispose();
                    h2.dispose();
                }
            };
            disp[FSymbol.interfaces] = ["System.IDisposable"];
            return disp;
        };
        return new Event(s, w1.delegates.concat(w2.delegates));
    };
    Event.split = function (f, w) {
        return [Event.choose(function (v) {
            var res = f(v);
            return res.Case == "Choice1Of2" ? res.Fields[0] : null;
        }, w), Event.choose(function (v) {
            var res = f(v);
            return res.Case == "Choice2Of2" ? res.Fields[0] : null;
        }, w)];
    };

    var Lazy = exports.Lazy = function () {
        function Lazy(factory) {
            _classCallCheck(this, Lazy);

            this.factory = factory;
            this.isValueCreated = false;
        }

        _createClass(Lazy, [{
            key: "value",
            get: function get() {
                if (!this.isValueCreated) {
                    this.createdValue = this.factory();
                    this.isValueCreated = true;
                }
                return this.createdValue;
            }
        }]);

        return Lazy;
    }();

    Lazy.createFromValue = function (v) {
        return new Lazy(function () {
            return v;
        });
    };
});
