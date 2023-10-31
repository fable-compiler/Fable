(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('fable-metadata'), require('fs'), require('path'), require('fable-standalone'), require('os'), require('process')) :
  typeof define === 'function' && define.amd ? define(['exports', 'fable-metadata', 'fs', 'path', 'fable-standalone', 'os', 'process'], factory) :
  (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.Fable = {}, global.fableMetadata, global.fs, global.Path, global.fableStandalone, global.os, global.process$1));
})(this, (function (exports, fableMetadata, fs, Path, fableStandalone, os, process$1) { 'use strict';

  function _interopNamespaceDefault(e) {
    var n = Object.create(null);
    if (e) {
      Object.keys(e).forEach(function (k) {
        if (k !== 'default') {
          var d = Object.getOwnPropertyDescriptor(e, k);
          Object.defineProperty(n, k, d.get ? d : {
            enumerable: true,
            get: function () { return e[k]; }
          });
        }
      });
    }
    n.default = e;
    return Object.freeze(n);
  }

  var fs__namespace = /*#__PURE__*/_interopNamespaceDefault(fs);
  var Path__namespace = /*#__PURE__*/_interopNamespaceDefault(Path);
  var os__namespace = /*#__PURE__*/_interopNamespaceDefault(os);
  var process__namespace = /*#__PURE__*/_interopNamespaceDefault(process$1);

  function getVersion$1() {
    return require("../package.json").version;
  }

  function getFableLibDir$1() {
    return Path__namespace.join(Path__namespace.dirname(require.resolve("fable-standalone")), "fable-library");
  }

  function getDirFiles$1(dir) {
    if (!fs__namespace.existsSync(dir)) return [];
    const files = fs__namespace.readdirSync(dir).map((subdir) => {
      const res = Path__namespace.resolve(dir, subdir);
      return fs__namespace.statSync(res).isDirectory() ? getDirFiles$1(res) : res;
    });
    return files.reduce((a, f) => a.concat(f), []);
  }

  function ensureDirExists$1(dir, cont) {
    if (fs__namespace.existsSync(dir)) {
      if (typeof cont === "function") { cont(); }
    } else {
      ensureDirExists$1(Path__namespace.dirname(dir), () => {
        if (!fs__namespace.existsSync(dir)) { fs__namespace.mkdirSync(dir); }
        if (typeof cont === "function") { cont(); }
      });
    }
  }

  function serializeToJson$1(data) {
    return JSON.stringify(data, (key, value) => {
      if (value === Infinity) {
        return "Infinity";
      } else if (value === -Infinity) {
        return "-Infinity";
      } else if (value !== value) {
        return "NaN";
      }
      return value;
    });
  }

  function copyFolder(from, dest) {
    if (!fs__namespace.existsSync(dest)) {
      ensureDirExists$1(dest);
    }
    fs__namespace.readdirSync(from).forEach(element => {
      if (fs__namespace.lstatSync(Path__namespace.join(from, element)).isDirectory()) {
        copyFolder(Path__namespace.join(from, element), Path__namespace.join(dest, element));
      } else {
        fs__namespace.copyFileSync(Path__namespace.join(from, element), Path__namespace.join(dest, element));
      }
    });
  }

  function runCmdAndExitIfFails$1(cmd) {
    var child_process = require("child_process");
    console.log(">", cmd);
    try {
      child_process.execSync(cmd, {
        stdio: "inherit"
      });
    } catch (error) {
      process.exit(-1);
    }
  }

  var util = /*#__PURE__*/Object.freeze({
    __proto__: null,
    copyFolder: copyFolder,
    ensureDirExists: ensureDirExists$1,
    getDirFiles: getDirFiles$1,
    getFableLibDir: getFableLibDir$1,
    getVersion: getVersion$1,
    runCmdAndExitIfFails: runCmdAndExitIfFails$1,
    serializeToJson: serializeToJson$1
  });

  const coreAssemblies = ["Fable.Core", "FSharp.Core", "mscorlib", "netstandard", "System.Collections", "System.Collections.Concurrent", "System.ComponentModel", "System.ComponentModel.Primitives", "System.ComponentModel.TypeConverter", "System.Console", "System.Core", "System.Diagnostics.Debug", "System.Diagnostics.Tools", "System.Diagnostics.Tracing", "System.Globalization", "System", "System.IO", "System.Net.Requests", "System.Net.WebClient", "System.Numerics", "System.Reflection", "System.Reflection.Extensions", "System.Reflection.Metadata", "System.Reflection.Primitives", "System.Reflection.TypeExtensions", "System.Runtime", "System.Runtime.Extensions", "System.Runtime.Numerics", "System.Runtime.InteropServices", "System.Text.Encoding", "System.Text.Encoding.Extensions", "System.Text.RegularExpressions", "System.Threading", "System.Threading.Tasks", "System.Threading.Thread", "System.ValueTuple"];

  // Adapted from https://github.com/MikeMcl/big.js/blob/0f94dc9110d55c4f324a47ba6a2e832ce23ac589/big.mjs
  /* tslint:disable */
  var P = {};
  /*
   *  big.js v6.0.3
   *  A small, fast, easy-to-use library for arbitrary-precision decimal arithmetic.
   *  Copyright (c) 2020 Michael Mclaughlin
   *  https://github.com/MikeMcl/big.js/LICENCE.md
   */
  /************************************** EDITABLE DEFAULTS *****************************************/
  // The default values below must be integers within the stated ranges.
  /*
   * The maximum number of decimal places (DP) of the results of operations involving division:
   * div and sqrt, and pow with negative exponents.
   */
  var DP = 28, // 0 to MAX_DP
  /*
   * The rounding mode (RM) used when rounding to the above decimal places.
   *
   *  0  Towards zero (i.e. truncate, no rounding).       (ROUND_DOWN)
   *  1  To nearest neighbour. If equidistant, round up.  (ROUND_HALF_UP)
   *  2  To nearest neighbour. If equidistant, to even.   (ROUND_HALF_EVEN)
   *  3  Away from zero.                                  (ROUND_UP)
   */
  RM = 1, // 0, 1, 2 or 3
  // The maximum value of DP and Big.DP.
  MAX_DP = 1E6, // 0 to 1000000
  // The maximum magnitude of the exponent argument to the pow method.
  MAX_POWER = 1E6, // 1 to 1000000
  /*
   * The negative exponent (NE) at and beneath which toString returns exponential notation.
   * (JavaScript numbers: -7)
   * -1000000 is the minimum recommended exponent value of a Big.
   */
  NE = -29, // 0 to -1000000
  /*
   * The positive exponent (PE) at and above which toString returns exponential notation.
   * (JavaScript numbers: 21)
   * 1000000 is the maximum recommended exponent value of a Big, but this limit is not enforced.
   */
  PE = 29, // 0 to 1000000
  /*
   * When true, an error will be thrown if a primitive number is passed to the Big constructor,
   * or if valueOf is called, or if toNumber is called on a Big which cannot be converted to a
   * primitive number without a loss of precision.
   */
  STRICT = false, // true or false
  /**************************************************************************************************/
  // Error messages.
  NAME = '[big.js] ', INVALID = NAME + 'Invalid ', INVALID_DP = INVALID + 'decimal places', INVALID_RM = INVALID + 'rounding mode', DIV_BY_ZERO = NAME + 'Division by zero', UNDEFINED = void 0, NUMERIC = /^-?(\d+(\.\d*)?|\.\d+)(e[+-]?\d+)?$/i;
  /*
   * Create and return a Big constructor.
   */
  function _Big_() {
      /*
       * The Big constructor and exported function.
       * Create and return a new instance of a Big number object.
       *
       * n {number|string|Big} A numeric value.
       */
      function Big(n) {
          var x = this;
          // Enable constructor usage without new.
          if (!(x instanceof Big))
              return n === UNDEFINED ? _Big_() : new Big(n);
          // Duplicate.
          if (n instanceof Big) {
              x.s = n.s;
              x.e = n.e;
              x.c = n.c.slice();
              normalize(x);
          }
          else {
              if (typeof n !== 'string') {
                  if (Big.strict === true) {
                      throw TypeError(INVALID + 'number');
                  }
                  // Minus zero?
                  n = n === 0 && 1 / n < 0 ? '-0' : String(n);
              }
              parse(x, n);
          }
          // Retain a reference to this Big constructor.
          // Shadow Big.prototype.constructor which points to Object.
          x.constructor = Big;
      }
      Big.prototype = P;
      Big.DP = DP;
      Big.RM = RM;
      Big.NE = NE;
      Big.PE = PE;
      Big.strict = STRICT;
      return Big;
  }
  function normalize(x) {
      // x = round(x, DP, 0);
      if (x.c.length > 1 && !x.c[0]) {
          let i = x.c.findIndex(x => x);
          x.c = x.c.slice(i);
          x.e = x.e - i;
      }
  }
  /*
   * Parse the number or string value passed to a Big constructor.
   *
   * x {Big} A Big number instance.
   * n {number|string} A numeric value.
   */
  function parse(x, n) {
      var e, i, nl;
      if (!NUMERIC.test(n)) {
          throw Error(INVALID + 'number');
      }
      // Determine sign.
      x.s = n.charAt(0) == '-' ? (n = n.slice(1), -1) : 1;
      // Decimal point?
      if ((e = n.indexOf('.')) > -1)
          n = n.replace('.', '');
      // Exponential form?
      if ((i = n.search(/e/i)) > 0) {
          // Determine exponent.
          if (e < 0)
              e = i;
          e += +n.slice(i + 1);
          n = n.substring(0, i);
      }
      else if (e < 0) {
          // Integer.
          e = n.length;
      }
      nl = n.length;
      // Determine leading zeros before decimal point.
      for (i = 0; i < e && i < nl && n.charAt(i) == '0';)
          ++i;
      // original version (ignores decimal point).
      // // Determine leading zeros.
      // for (i = 0; i < nl && n.charAt(i) == '0';) ++i;
      if (i == nl) {
          // Zero.
          x.c = [x.e = 0];
      }
      else {
          x.e = e - i - 1;
          x.c = [];
          // Convert string to array of digits without leading zeros
          for (e = 0; i < nl;)
              x.c[e++] = +n.charAt(i++);
          // older version (doesn't keep trailing zeroes).
          // // Determine trailing zeros.
          // for (; nl > 0 && n.charAt(--nl) == '0';);
          // // Convert string to array of digits without leading/trailing zeros.
          // for (e = 0; i <= nl;) x.c[e++] = +n.charAt(i++);
      }
      x = round(x, Big.DP + 1, Big.RM);
      return x;
  }
  /*
   * Round Big x to a maximum of sd significant digits using rounding mode rm.
   *
   * x {Big} The Big to round.
   * sd {number} Significant digits: integer, 0 to MAX_DP inclusive.
   * rm {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   * [more] {boolean} Whether the result of division was truncated.
   */
  function round(x, sd, rm, more) {
      var xc = x.c;
      if (rm === UNDEFINED)
          rm = Big.RM;
      if (rm !== 0 && rm !== 1 && rm !== 2 && rm !== 3) {
          throw Error(INVALID_RM);
      }
      if (sd < 1) {
          more =
              rm === 3 && (more || !!xc[0]) || sd === 0 && (rm === 1 && xc[0] >= 5 ||
                  rm === 2 && (xc[0] > 5 || xc[0] === 5 && (more || xc[1] !== UNDEFINED)));
          xc.length = 1;
          if (more) {
              // 1, 0.1, 0.01, 0.001, 0.0001 etc.
              x.e = x.e - sd + 1;
              xc[0] = 1;
          }
          else {
              // Zero.
              xc[0] = x.e = 0;
          }
      }
      else if (sd < xc.length) {
          // xc[sd] is the digit after the digit that may be rounded up.
          const isZero = xc.findIndex((xci, idx) => idx >= sd && xci > 0) < 0;
          more =
              rm === 1 && xc[sd] >= 5 ||
                  rm === 2 && (xc[sd] > 5 || xc[sd] === 5 &&
                      (more || xc[sd + 1] !== UNDEFINED || xc[sd - 1] & 1)) ||
                  rm === 3 && (more || !isZero);
          // Remove any digits after the required precision.
          xc.length = sd--;
          // Round up?
          if (more) {
              // Rounding up may mean the previous digit has to be rounded up.
              for (; ++xc[sd] > 9;) {
                  xc[sd] = 0;
                  if (!sd--) {
                      ++x.e;
                      xc.unshift(1);
                  }
              }
          }
          // Remove trailing zeros.
          for (sd = xc.length; !xc[--sd];)
              xc.pop();
      }
      return x;
  }
  /*
   * Return a string representing the value of Big x in normal or exponential notation.
   * Handles P.toExponential, P.toFixed, P.toJSON, P.toPrecision, P.toString and P.valueOf.
   */
  function stringify(x, doExponential, isNonzero) {
      var e = x.e, s = x.c.join(''), n = s.length;
      // Exponential notation?
      if (doExponential) {
          s = s.charAt(0) + (n > 1 ? '.' + s.slice(1) : '') + (e < 0 ? 'e' : 'e+') + e;
          // Normal notation.
      }
      else if (e < 0) {
          for (; ++e;)
              s = '0' + s;
          s = '0.' + s;
      }
      else if (e > 0) {
          if (++e > n) {
              for (e -= n; e--;)
                  s += '0';
          }
          else if (e < n) {
              s = s.slice(0, e) + '.' + s.slice(e);
          }
      }
      else if (n > 1) {
          s = s.charAt(0) + '.' + s.slice(1);
      }
      return x.s < 0 && isNonzero ? '-' + s : s;
  }
  // Prototype/instance methods
  /*
   * Return a new Big whose value is the absolute value of this Big.
   */
  P.abs = function () {
      var x = new this.constructor(this);
      x.s = 1;
      return x;
  };
  /*
   * Return 1 if the value of this Big is greater than the value of Big y,
   *       -1 if the value of this Big is less than the value of Big y, or
   *        0 if they have the same value.
   */
  P.cmp = function (y) {
      var isneg, Big = this.constructor, x = new Big(this), y = new Big(y), xc = x.c, yc = y.c, i = x.s, j = y.s, k = x.e, l = y.e;
      // Either zero?
      if (!xc[0] || !yc[0])
          return !xc[0] ? !yc[0] ? 0 : -j : i;
      // Signs differ?
      if (i != j)
          return i;
      isneg = i < 0;
      // Compare exponents.
      if (k != l)
          return k > l ^ isneg ? 1 : -1;
      // Compare digit by digit.
      j = Math.max(xc.length, yc.length);
      for (i = 0; i < j; i++) {
          k = i < xc.length ? xc[i] : 0;
          l = i < yc.length ? yc[i] : 0;
          if (k != l)
              return k > l ^ isneg ? 1 : -1;
      }
      return 0;
      // original version (doesn't compare well trailing zeroes, e.g. 1.0 with 1.00)
      // j = (k = xc.length) < (l = yc.length) ? k : l;
      // // Compare digit by digit.
      // for (i = -1; ++i < j;) {
      //   if (xc[i] != yc[i]) return xc[i] > yc[i] ^ isneg ? 1 : -1;
      // }
      // // Compare lengths.
      // return k == l ? 0 : k > l ^ isneg ? 1 : -1;
  };
  /*
   * Return a new Big whose value is the value of this Big divided by the value of Big y, rounded,
   * if necessary, to a maximum of Big.DP decimal places using rounding mode Big.RM.
   */
  P.div = function (y) {
      var Big = this.constructor, x = new Big(this), y = new Big(y), a = x.c, // dividend
      b = y.c, // divisor
      k = x.s == y.s ? 1 : -1, dp = Big.DP;
      if (dp !== ~~dp || dp < 0 || dp > MAX_DP) {
          throw Error(INVALID_DP);
      }
      // Divisor is zero?
      if (!b[0]) {
          throw Error(DIV_BY_ZERO);
      }
      // Dividend is 0? Return +-0.
      if (!a[0]) {
          y.s = k;
          y.c = [y.e = 0];
          return y;
      }
      var bl, bt, n, cmp, ri, bz = b.slice(), ai = bl = b.length, al = a.length, r = a.slice(0, bl), // remainder
      rl = r.length, q = y, // quotient
      qc = q.c = [], qi = 0, p = dp + (q.e = x.e - y.e) + 1; // precision of the result
      q.s = k;
      k = p < 0 ? 0 : p;
      // Create version of divisor with leading zero.
      bz.unshift(0);
      // Add zeros to make remainder as long as divisor.
      for (; rl++ < bl;)
          r.push(0);
      do {
          // n is how many times the divisor goes into current remainder.
          for (n = 0; n < 10; n++) {
              // Compare divisor and remainder.
              if (bl != (rl = r.length)) {
                  cmp = bl > rl ? 1 : -1;
              }
              else {
                  for (ri = -1, cmp = 0; ++ri < bl;) {
                      if (b[ri] != r[ri]) {
                          cmp = b[ri] > r[ri] ? 1 : -1;
                          break;
                      }
                  }
              }
              // If divisor < remainder, subtract divisor from remainder.
              if (cmp < 0) {
                  // Remainder can't be more than 1 digit longer than divisor.
                  // Equalise lengths using divisor with extra leading zero?
                  for (bt = rl == bl ? b : bz; rl;) {
                      if (r[--rl] < bt[rl]) {
                          ri = rl;
                          for (; ri && !r[--ri];)
                              r[ri] = 9;
                          --r[ri];
                          r[rl] += 10;
                      }
                      r[rl] -= bt[rl];
                  }
                  for (; !r[0];)
                      r.shift();
              }
              else {
                  break;
              }
          }
          // Add the digit n to the result array.
          qc[qi++] = cmp ? n : ++n;
          // Update the remainder.
          if (r[0] && cmp)
              r[rl] = a[ai] || 0;
          else
              r = [a[ai]];
      } while ((ai++ < al || r[0] !== UNDEFINED) && k--);
      // Leading zero? Do not remove if result is simply zero (qi == 1).
      if (!qc[0] && qi != 1) {
          // There can't be more than one zero.
          qc.shift();
          q.e--;
          p--;
      }
      // Round?
      if (qi > p)
          round(q, p, Big.RM, r[0] !== UNDEFINED);
      return q;
  };
  /*
   * Return true if the value of this Big is equal to the value of Big y, otherwise return false.
   */
  P.eq = function (y) {
      return this.cmp(y) === 0;
  };
  /*
   * Return true if the value of this Big is greater than the value of Big y, otherwise return
   * false.
   */
  P.gt = function (y) {
      return this.cmp(y) > 0;
  };
  /*
   * Return true if the value of this Big is greater than or equal to the value of Big y, otherwise
   * return false.
   */
  P.gte = function (y) {
      return this.cmp(y) > -1;
  };
  /*
   * Return true if the value of this Big is less than the value of Big y, otherwise return false.
   */
  P.lt = function (y) {
      return this.cmp(y) < 0;
  };
  /*
   * Return true if the value of this Big is less than or equal to the value of Big y, otherwise
   * return false.
   */
  P.lte = function (y) {
      return this.cmp(y) < 1;
  };
  /*
   * Return a new Big whose value is the value of this Big minus the value of Big y.
   */
  P.minus = P.sub = function (y) {
      var i, j, t, xlty, Big = this.constructor, x = new Big(this), y = new Big(y), a = x.s, b = y.s;
      // Signs differ?
      if (a != b) {
          y.s = -b;
          return x.plus(y);
      }
      var xc = x.c.slice(), xe = x.e, yc = y.c, ye = y.e;
      // Either zero?
      if (!xc[0] || !yc[0]) {
          if (yc[0]) {
              y.s = -b;
          }
          else if (xc[0]) {
              y = new Big(x);
          }
          else {
              y.s = 1;
          }
          return y;
      }
      // Determine which is the bigger number. Prepend zeros to equalise exponents.
      if (a = xe - ye) {
          if (xlty = a < 0) {
              a = -a;
              t = xc;
          }
          else {
              ye = xe;
              t = yc;
          }
          t.reverse();
          for (b = a; b--;)
              t.push(0);
          t.reverse();
      }
      else {
          // Exponents equal. Check digit by digit.
          j = ((xlty = xc.length < yc.length) ? xc : yc).length;
          for (a = b = 0; b < j; b++) {
              if (xc[b] != yc[b]) {
                  xlty = xc[b] < yc[b];
                  break;
              }
          }
      }
      // x < y? Point xc to the array of the bigger number.
      if (xlty) {
          t = xc;
          xc = yc;
          yc = t;
          y.s = -y.s;
      }
      /*
       * Append zeros to xc if shorter. No need to add zeros to yc if shorter as subtraction only
       * needs to start at yc.length.
       */
      if ((b = (j = yc.length) - (i = xc.length)) > 0)
          for (; b--;)
              xc[i++] = 0;
      // Subtract yc from xc.
      for (b = i; j > a;) {
          if (xc[--j] < yc[j]) {
              for (i = j; i && !xc[--i];)
                  xc[i] = 9;
              --xc[i];
              xc[j] += 10;
          }
          xc[j] -= yc[j];
      }
      // Remove trailing zeros.
      for (; xc[--b] === 0;)
          xc.pop();
      // Remove leading zeros and adjust exponent accordingly.
      for (; xc[0] === 0;) {
          xc.shift();
          --ye;
      }
      if (!xc[0]) {
          // n - n = +0
          y.s = 1;
          // Result must be zero.
          xc = [ye = 0];
      }
      y.c = xc;
      y.e = ye;
      return y;
  };
  /*
   * Return a new Big whose value is the value of this Big modulo the value of Big y.
   */
  P.mod = function (y) {
      var ygtx, Big = this.constructor, x = new Big(this), y = new Big(y), a = x.s, b = y.s;
      if (!y.c[0]) {
          throw Error(DIV_BY_ZERO);
      }
      x.s = y.s = 1;
      ygtx = y.cmp(x) == 1;
      x.s = a;
      y.s = b;
      if (ygtx)
          return new Big(x);
      a = Big.DP;
      b = Big.RM;
      Big.DP = Big.RM = 0;
      x = x.div(y);
      Big.DP = a;
      Big.RM = b;
      return this.minus(x.times(y));
  };
  /*
   * Return a new Big whose value is the value of this Big plus the value of Big y.
   */
  P.plus = P.add = function (y) {
      var e, k, t, Big = this.constructor, x = new Big(this), y = new Big(y);
      // Signs differ?
      if (x.s != y.s) {
          y.s = -y.s;
          return x.minus(y);
      }
      var xe = x.e, xc = x.c, ye = y.e, yc = y.c;
      // Either zero?
      if (!xc[0] || !yc[0]) {
          if (!yc[0]) {
              if (xc[0]) {
                  y = new Big(x);
              }
              else {
                  y.s = x.s;
              }
          }
          return y;
      }
      xc = xc.slice();
      // Prepend zeros to equalise exponents.
      // Note: reverse faster than unshifts.
      if (e = xe - ye) {
          if (e > 0) {
              ye = xe;
              t = yc;
          }
          else {
              e = -e;
              t = xc;
          }
          t.reverse();
          for (; e--;)
              t.push(0);
          t.reverse();
      }
      // Point xc to the longer array.
      if (xc.length - yc.length < 0) {
          t = yc;
          yc = xc;
          xc = t;
      }
      e = yc.length;
      // Only start adding at yc.length - 1 as the further digits of xc can be left as they are.
      for (k = 0; e; xc[e] %= 10)
          k = (xc[--e] = xc[e] + yc[e] + k) / 10 | 0;
      // No need to check for zero, as +x + +y != 0 && -x + -y != 0
      if (k) {
          xc.unshift(k);
          ++ye;
      }
      // Remove trailing zeros.
      for (e = xc.length; xc[--e] === 0;)
          xc.pop();
      y.c = xc;
      y.e = ye;
      return y;
  };
  /*
   * Return a Big whose value is the value of this Big raised to the power n.
   * If n is negative, round to a maximum of Big.DP decimal places using rounding
   * mode Big.RM.
   *
   * n {number} Integer, -MAX_POWER to MAX_POWER inclusive.
   */
  P.pow = function (n) {
      var Big = this.constructor, x = new Big(this), y = new Big('1'), one = new Big('1'), isneg = n < 0;
      if (n !== ~~n || n < -MAX_POWER || n > MAX_POWER) {
          throw Error(INVALID + 'exponent');
      }
      if (isneg)
          n = -n;
      for (;;) {
          if (n & 1)
              y = y.times(x);
          n >>= 1;
          if (!n)
              break;
          x = x.times(x);
      }
      return isneg ? one.div(y) : y;
  };
  /*
   * Return a new Big whose value is the value of this Big rounded to a maximum precision of sd
   * significant digits using rounding mode rm, or Big.RM if rm is not specified.
   *
   * sd {number} Significant digits: integer, 1 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.prec = function (sd, rm) {
      if (sd !== ~~sd || sd < 1 || sd > MAX_DP) {
          throw Error(INVALID + 'precision');
      }
      return round(new this.constructor(this), sd, rm);
  };
  /*
   * Return a new Big whose value is the value of this Big rounded to a maximum of dp decimal places
   * using rounding mode rm, or Big.RM if rm is not specified.
   * If dp is negative, round to an integer which is a multiple of 10**-dp.
   * If dp is not specified, round to 0 decimal places.
   *
   * dp? {number} Integer, -MAX_DP to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.round = function (dp, rm) {
      if (dp === UNDEFINED)
          dp = 0;
      else if (dp !== ~~dp || dp < -MAX_DP || dp > MAX_DP) {
          throw Error(INVALID_DP);
      }
      return round(new this.constructor(this), dp + this.e + 1, rm);
  };
  /*
   * Return a new Big whose value is the square root of the value of this Big, rounded, if
   * necessary, to a maximum of Big.DP decimal places using rounding mode Big.RM.
   */
  P.sqrt = function () {
      var r, c, t, Big = this.constructor, x = new Big(this), s = x.s, e = x.e, half = new Big('0.5');
      // Zero?
      if (!x.c[0])
          return new Big(x);
      // Negative?
      if (s < 0) {
          throw Error(NAME + 'No square root');
      }
      // Estimate.
      s = Math.sqrt(x + '');
      // Math.sqrt underflow/overflow?
      // Re-estimate: pass x coefficient to Math.sqrt as integer, then adjust the result exponent.
      if (s === 0 || s === 1 / 0) {
          c = x.c.join('');
          if (!(c.length + e & 1))
              c += '0';
          s = Math.sqrt(c);
          e = ((e + 1) / 2 | 0) - (e < 0 || e & 1);
          r = new Big((s == 1 / 0 ? '5e' : (s = s.toExponential()).slice(0, s.indexOf('e') + 1)) + e);
      }
      else {
          r = new Big(s + '');
      }
      e = r.e + (Big.DP += 4);
      // Newton-Raphson iteration.
      do {
          t = r;
          r = half.times(t.plus(x.div(t)));
      } while (t.c.slice(0, e).join('') !== r.c.slice(0, e).join(''));
      return round(r, (Big.DP -= 4) + r.e + 1, Big.RM);
  };
  /*
   * Return a new Big whose value is the value of this Big times the value of Big y.
   */
  P.times = P.mul = function (y) {
      var c, Big = this.constructor, x = new Big(this), y = new Big(y), xc = x.c, yc = y.c, a = xc.length, b = yc.length, i = x.e, j = y.e;
      // Determine sign of result.
      y.s = x.s == y.s ? 1 : -1;
      // Return signed 0 if either 0.
      if (!xc[0] || !yc[0]) {
          y.c = [y.e = 0];
          return y;
      }
      // Initialise exponent of result as x.e + y.e.
      y.e = i + j;
      // If array xc has fewer digits than yc, swap xc and yc, and lengths.
      if (a < b) {
          c = xc;
          xc = yc;
          yc = c;
          j = a;
          a = b;
          b = j;
      }
      // Initialise coefficient array of result with zeros.
      for (c = new Array(j = a + b); j--;)
          c[j] = 0;
      // Multiply.
      // i is initially xc.length.
      for (i = b; i--;) {
          b = 0;
          // a is yc.length.
          for (j = a + i; j > i;) {
              // Current sum of products at this digit position, plus carry.
              b = c[j] + yc[i] * xc[j - i - 1] + b;
              c[j--] = b % 10;
              // carry
              b = b / 10 | 0;
          }
          c[j] = b;
      }
      // Increment result exponent if there is a final carry, otherwise remove leading zero.
      if (b)
          ++y.e;
      else
          c.shift();
      // Remove trailing zeros.
      for (i = c.length; !c[--i];)
          c.pop();
      y.c = c;
      return y;
  };
  /*
   * Return a string representing the value of this Big in exponential notation rounded to dp fixed
   * decimal places using rounding mode rm, or Big.RM if rm is not specified.
   *
   * dp? {number} Decimal places: integer, 0 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.toExponential = function (dp, rm) {
      var x = this, n = x.c[0];
      if (dp !== UNDEFINED) {
          if (dp !== ~~dp || dp < 0 || dp > MAX_DP) {
              throw Error(INVALID_DP);
          }
          x = round(new x.constructor(x), ++dp, rm);
          for (; x.c.length < dp;)
              x.c.push(0);
      }
      return stringify(x, true, !!n);
  };
  /*
   * Return a string representing the value of this Big in normal notation rounded to dp fixed
   * decimal places using rounding mode rm, or Big.RM if rm is not specified.
   *
   * dp? {number} Decimal places: integer, 0 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   *
   * (-0).toFixed(0) is '0', but (-0.1).toFixed(0) is '-0'.
   * (-0).toFixed(1) is '0.0', but (-0.01).toFixed(1) is '-0.0'.
   */
  P.toFixed = function (dp, rm) {
      var x = this, n = x.c[0];
      if (dp !== UNDEFINED) {
          if (dp !== ~~dp || dp < 0 || dp > MAX_DP) {
              throw Error(INVALID_DP);
          }
          x = round(new x.constructor(x), dp + x.e + 1, rm);
          // x.e may have changed if the value is rounded up.
          for (dp = dp + x.e + 1; x.c.length < dp;)
              x.c.push(0);
      }
      return stringify(x, false, !!n);
  };
  /*
   * Return a string representing the value of this Big.
   * Return exponential notation if this Big has a positive exponent equal to or greater than
   * Big.PE, or a negative exponent equal to or less than Big.NE.
   * Omit the sign for negative zero.
   */
  P.toJSON = P.toString = function () {
      var x = this, Big = x.constructor;
      return stringify(x, x.e <= Big.NE || x.e >= Big.PE, !!x.c[0]);
  };
  /*
   * Return the value of this Big as a primitve number.
   */
  P.toNumber = function () {
      var n = Number(stringify(this, true, true));
      if (this.constructor.strict === true && !this.eq(n.toString())) {
          throw Error(NAME + 'Imprecise conversion');
      }
      return n;
  };
  /*
   * Return a string representing the value of this Big rounded to sd significant digits using
   * rounding mode rm, or Big.RM if rm is not specified.
   * Use exponential notation if sd is less than the number of digits necessary to represent
   * the integer part of the value in normal notation.
   *
   * sd {number} Significant digits: integer, 1 to MAX_DP inclusive.
   * rm? {number} Rounding mode: 0 (down), 1 (half-up), 2 (half-even) or 3 (up).
   */
  P.toPrecision = function (sd, rm) {
      var x = this, Big = x.constructor, n = x.c[0];
      if (sd !== UNDEFINED) {
          if (sd !== ~~sd || sd < 1 || sd > MAX_DP) {
              throw Error(INVALID + 'precision');
          }
          x = round(new Big(x), sd, rm);
          for (; x.c.length < sd;)
              x.c.push(0);
      }
      return stringify(x, sd <= x.e || x.e <= Big.NE || x.e >= Big.PE, !!n);
  };
  /*
   * Return a string representing the value of this Big.
   * Return exponential notation if this Big has a positive exponent equal to or greater than
   * Big.PE, or a negative exponent equal to or less than Big.NE.
   * Include the sign for negative zero.
   */
  P.valueOf = function () {
      var x = this, Big = x.constructor;
      if (Big.strict === true) {
          throw Error(NAME + 'valueOf disallowed');
      }
      return stringify(x, x.e <= Big.NE || x.e >= Big.PE, true);
  };
  // Export
  /**
   * @type object
   */
  var Big = _Big_();

  const symbol = Symbol("numeric");
  function isNumeric(x) {
      return typeof x === "number" || typeof x === "bigint" || x?.[symbol];
  }
  function compare$1(x, y) {
      if (typeof x === "number") {
          return x < y ? -1 : (x > y ? 1 : 0);
      }
      else if (typeof x === "bigint") {
          return x < y ? -1 : (x > y ? 1 : 0);
      }
      else {
          return x.CompareTo(y);
      }
  }
  function multiply(x, y) {
      if (typeof x === "number") {
          return x * y;
      }
      else if (typeof x === "bigint") {
          return x * BigInt(y);
      }
      else {
          return x[symbol]().multiply(y);
      }
  }
  function toFixed(x, dp) {
      if (typeof x === "number") {
          return x.toFixed(dp);
      }
      else if (typeof x === "bigint") {
          return x;
      }
      else {
          return x[symbol]().toFixed(dp);
      }
  }
  function toPrecision(x, sd) {
      if (typeof x === "number") {
          return x.toPrecision(sd);
      }
      else if (typeof x === "bigint") {
          return x;
      }
      else {
          return x[symbol]().toPrecision(sd);
      }
  }
  function toExponential(x, dp) {
      if (typeof x === "number") {
          return x.toExponential(dp);
      }
      else if (typeof x === "bigint") {
          return x;
      }
      else {
          return x[symbol]().toExponential(dp);
      }
  }
  function toHex(x) {
      if (typeof x === "number") {
          return (Number(x) >>> 0).toString(16);
      }
      else if (typeof x === "bigint") {
          // TODO: properly handle other bit sizes
          return BigInt.asUintN(64, x).toString(16);
      }
      else {
          return x[symbol]().toHex();
      }
  }

  // tslint:disable:ban-types
  function isArrayLike(x) {
      return Array.isArray(x) || ArrayBuffer.isView(x);
  }
  function isEnumerable(x) {
      return x != null && typeof x.GetEnumerator === "function";
  }
  function isComparable(x) {
      return x != null && typeof x.CompareTo === "function";
  }
  function isEquatable(x) {
      return x != null && typeof x.Equals === "function";
  }
  function isHashable(x) {
      return x != null && typeof x.GetHashCode === "function";
  }
  function isDisposable(x) {
      return x != null && typeof x.Dispose === "function";
  }
  function disposeSafe(x) {
      if (isDisposable(x)) {
          x.Dispose();
      }
  }
  function defaultOf() {
      return null;
  }
  function sameConstructor(x, y) {
      return Object.getPrototypeOf(x)?.constructor === Object.getPrototypeOf(y)?.constructor;
  }
  class Enumerator {
      constructor(iter) {
          this.iter = iter;
          this.current = defaultOf();
      }
      ["System.Collections.Generic.IEnumerator`1.get_Current"]() {
          return this.current;
      }
      ["System.Collections.IEnumerator.get_Current"]() {
          return this.current;
      }
      ["System.Collections.IEnumerator.MoveNext"]() {
          const cur = this.iter.next();
          this.current = cur.value;
          return !cur.done;
      }
      ["System.Collections.IEnumerator.Reset"]() {
          throw new Error("JS iterators cannot be reset");
      }
      Dispose() {
          return;
      }
  }
  function getEnumerator(e) {
      if (isEnumerable(e)) {
          return e.GetEnumerator();
      }
      else {
          return new Enumerator(e[Symbol.iterator]());
      }
  }
  function toIterator(en) {
      return {
          next() {
              const hasNext = en["System.Collections.IEnumerator.MoveNext"]();
              const current = hasNext ? en["System.Collections.Generic.IEnumerator`1.get_Current"]() : undefined;
              return { done: !hasNext, value: current };
          },
      };
  }
  class Lazy {
      constructor(factory) {
          this.factory = factory;
          this.isValueCreated = false;
      }
      get Value() {
          if (!this.isValueCreated) {
              this.createdValue = this.factory();
              this.isValueCreated = true;
          }
          return this.createdValue;
      }
      get IsValueCreated() {
          return this.isValueCreated;
      }
  }
  function padWithZeros(i, length) {
      let str = i.toString(10);
      while (str.length < length) {
          str = "0" + str;
      }
      return str;
  }
  function dateOffset(date) {
      const date1 = date;
      return typeof date1.offset === "number"
          ? date1.offset
          : (date.kind === 1 /* DateKind.UTC */
              ? 0 : date.getTimezoneOffset() * -60000);
  }
  function int32ToString(i, radix) {
      i = i < 0 && radix != null && radix !== 10 ? 0xFFFFFFFF + i + 1 : i;
      return i.toString(radix);
  }
  class ObjectRef {
      static id(o) {
          if (!ObjectRef.idMap.has(o)) {
              ObjectRef.idMap.set(o, ++ObjectRef.count);
          }
          return ObjectRef.idMap.get(o);
      }
  }
  ObjectRef.idMap = new WeakMap();
  ObjectRef.count = 0;
  function stringHash(s) {
      let i = 0;
      let h = 5381;
      const len = s.length;
      while (i < len) {
          h = (h * 33) ^ s.charCodeAt(i++);
      }
      return h;
  }
  function numberHash(x) {
      return x * 2654435761 | 0;
  }
  function bigintHash(x) {
      return stringHash(x.toString(32));
  }
  // From https://stackoverflow.com/a/37449594
  function combineHashCodes(hashes) {
      let h1 = 0;
      const len = hashes.length;
      for (let i = 0; i < len; i++) {
          const h2 = hashes[i];
          h1 = ((h1 << 5) + h1) ^ h2;
      }
      return h1;
  }
  function dateHash(x) {
      return x.getTime();
  }
  function arrayHash(x) {
      const len = x.length;
      const hashes = new Array(len);
      for (let i = 0; i < len; i++) {
          hashes[i] = structuralHash(x[i]);
      }
      return combineHashCodes(hashes);
  }
  function structuralHash(x) {
      if (x == null) {
          return 0;
      }
      switch (typeof x) {
          case "boolean":
              return x ? 1 : 0;
          case "number":
              return numberHash(x);
          case "bigint":
              return bigintHash(x);
          case "string":
              return stringHash(x);
          default: {
              if (isHashable(x)) {
                  return x.GetHashCode();
              }
              else if (isArrayLike(x)) {
                  return arrayHash(x);
              }
              else if (x instanceof Date) {
                  return dateHash(x);
              }
              else if (Object.getPrototypeOf(x)?.constructor === Object) {
                  // TODO: check call-stack to prevent cyclic objects?
                  const hashes = Object.values(x).map((v) => structuralHash(v));
                  return combineHashCodes(hashes);
              }
              else {
                  // Classes don't implement GetHashCode by default, but must use identity hashing
                  return numberHash(ObjectRef.id(x));
                  // return stringHash(String(x));
              }
          }
      }
  }
  function equalArraysWith(x, y, eq) {
      if (x == null) {
          return y == null;
      }
      if (y == null) {
          return false;
      }
      if (x.length !== y.length) {
          return false;
      }
      for (let i = 0; i < x.length; i++) {
          if (!eq(x[i], y[i])) {
              return false;
          }
      }
      return true;
  }
  function equalArrays(x, y) {
      return equalArraysWith(x, y, equals$1);
  }
  function equalObjects(x, y) {
      const xKeys = Object.keys(x);
      const yKeys = Object.keys(y);
      if (xKeys.length !== yKeys.length) {
          return false;
      }
      xKeys.sort();
      yKeys.sort();
      for (let i = 0; i < xKeys.length; i++) {
          if (xKeys[i] !== yKeys[i] || !equals$1(x[xKeys[i]], y[yKeys[i]])) {
              return false;
          }
      }
      return true;
  }
  function equals$1(x, y) {
      if (x === y) {
          return true;
      }
      else if (x == null) {
          return y == null;
      }
      else if (y == null) {
          return false;
      }
      else if (isEquatable(x)) {
          return x.Equals(y);
      }
      else if (isArrayLike(x)) {
          return isArrayLike(y) && equalArrays(x, y);
      }
      else if (typeof x !== "object") {
          return false;
      }
      else if (x instanceof Date) {
          return (y instanceof Date) && compareDates(x, y) === 0;
      }
      else {
          return Object.getPrototypeOf(x)?.constructor === Object && equalObjects(x, y);
      }
  }
  function compareDates(x, y) {
      let xtime;
      let ytime;
      // DateTimeOffset and DateTime deals with equality differently.
      if ("offset" in x && "offset" in y) {
          xtime = x.getTime();
          ytime = y.getTime();
      }
      else {
          xtime = x.getTime() + dateOffset(x);
          ytime = y.getTime() + dateOffset(y);
      }
      return xtime === ytime ? 0 : (xtime < ytime ? -1 : 1);
  }
  function comparePrimitives(x, y) {
      return x === y ? 0 : (x < y ? -1 : 1);
  }
  function compareArraysWith(x, y, comp) {
      if (x == null) {
          return y == null ? 0 : 1;
      }
      if (y == null) {
          return -1;
      }
      if (x.length !== y.length) {
          return x.length < y.length ? -1 : 1;
      }
      for (let i = 0, j = 0; i < x.length; i++) {
          j = comp(x[i], y[i]);
          if (j !== 0) {
              return j;
          }
      }
      return 0;
  }
  function compareArrays(x, y) {
      return compareArraysWith(x, y, compare);
  }
  function compareObjects(x, y) {
      const xKeys = Object.keys(x);
      const yKeys = Object.keys(y);
      if (xKeys.length !== yKeys.length) {
          return xKeys.length < yKeys.length ? -1 : 1;
      }
      xKeys.sort();
      yKeys.sort();
      for (let i = 0, j = 0; i < xKeys.length; i++) {
          const key = xKeys[i];
          if (key !== yKeys[i]) {
              return key < yKeys[i] ? -1 : 1;
          }
          else {
              j = compare(x[key], y[key]);
              if (j !== 0) {
                  return j;
              }
          }
      }
      return 0;
  }
  function compare(x, y) {
      if (x === y) {
          return 0;
      }
      else if (x == null) {
          return y == null ? 0 : -1;
      }
      else if (y == null) {
          return 1;
      }
      else if (isComparable(x)) {
          return x.CompareTo(y);
      }
      else if (isArrayLike(x)) {
          return isArrayLike(y) ? compareArrays(x, y) : -1;
      }
      else if (typeof x !== "object") {
          return x < y ? -1 : 1;
      }
      else if (x instanceof Date) {
          return y instanceof Date ? compareDates(x, y) : -1;
      }
      else {
          return Object.getPrototypeOf(x)?.constructor === Object ? compareObjects(x, y) : -1;
      }
  }
  const curried = new WeakMap();
  function uncurry2(f) {
      if (f == null) {
          return null;
      }
      const f2 = (a1, a2) => f(a1)(a2);
      curried.set(f2, f);
      return f2;
  }
  function curry2(f) {
      return curried.get(f) ?? ((a1) => (a2) => f(a1, a2));
  }

  function seqToString(self) {
      let count = 0;
      let str = "[";
      for (const x of self) {
          if (count === 0) {
              str += toString$1(x);
          }
          else if (count === 100) {
              str += "; ...";
              break;
          }
          else {
              str += "; " + toString$1(x);
          }
          count++;
      }
      return str + "]";
  }
  function toString$1(x, callStack = 0) {
      if (x != null && typeof x === "object") {
          if (typeof x.toString === "function") {
              return x.toString();
          }
          else if (Symbol.iterator in x) {
              return seqToString(x);
          }
          else { // TODO: Date?
              const cons = Object.getPrototypeOf(x)?.constructor;
              return cons === Object && callStack < 10
                  // Same format as recordToString
                  ? "{ " + Object.entries(x).map(([k, v]) => k + " = " + toString$1(v, callStack + 1)).join("\n  ") + " }"
                  : cons?.name ?? "";
          }
      }
      return String(x);
  }
  function unionToString(name, fields) {
      if (fields.length === 0) {
          return name;
      }
      else {
          let fieldStr;
          let withParens = true;
          if (fields.length === 1) {
              fieldStr = toString$1(fields[0]);
              withParens = fieldStr.indexOf(" ") >= 0;
          }
          else {
              fieldStr = fields.map((x) => toString$1(x)).join(", ");
          }
          return name + (withParens ? " (" : " ") + fieldStr + (withParens ? ")" : "");
      }
  }
  class Union {
      get name() {
          return this.cases()[this.tag];
      }
      toJSON() {
          return this.fields.length === 0 ? this.name : [this.name].concat(this.fields);
      }
      toString() {
          return unionToString(this.name, this.fields);
      }
      GetHashCode() {
          const hashes = this.fields.map((x) => structuralHash(x));
          hashes.splice(0, 0, numberHash(this.tag));
          return combineHashCodes(hashes);
      }
      Equals(other) {
          if (this === other) {
              return true;
          }
          else if (!sameConstructor(this, other)) {
              return false;
          }
          else if (this.tag === other.tag) {
              return equalArrays(this.fields, other.fields);
          }
          else {
              return false;
          }
      }
      CompareTo(other) {
          if (this === other) {
              return 0;
          }
          else if (!sameConstructor(this, other)) {
              return -1;
          }
          else if (this.tag === other.tag) {
              return compareArrays(this.fields, other.fields);
          }
          else {
              return this.tag < other.tag ? -1 : 1;
          }
      }
  }
  function recordToJSON(self) {
      const o = {};
      const keys = Object.keys(self);
      for (let i = 0; i < keys.length; i++) {
          o[keys[i]] = self[keys[i]];
      }
      return o;
  }
  function recordToString(self) {
      return "{ " + Object.entries(self).map(([k, v]) => k + " = " + toString$1(v)).join("\n  ") + " }";
  }
  function recordGetHashCode(self) {
      const hashes = Object.values(self).map((v) => structuralHash(v));
      return combineHashCodes(hashes);
  }
  function recordEquals(self, other) {
      if (self === other) {
          return true;
      }
      else if (!sameConstructor(self, other)) {
          return false;
      }
      else {
          const thisNames = Object.keys(self);
          for (let i = 0; i < thisNames.length; i++) {
              if (!equals$1(self[thisNames[i]], other[thisNames[i]])) {
                  return false;
              }
          }
          return true;
      }
  }
  function recordCompareTo(self, other) {
      if (self === other) {
          return 0;
      }
      else if (!sameConstructor(self, other)) {
          return -1;
      }
      else {
          const thisNames = Object.keys(self);
          for (let i = 0; i < thisNames.length; i++) {
              const result = compare(self[thisNames[i]], other[thisNames[i]]);
              if (result !== 0) {
                  return result;
              }
          }
          return 0;
      }
  }
  class Record {
      toJSON() { return recordToJSON(this); }
      toString() { return recordToString(this); }
      GetHashCode() { return recordGetHashCode(this); }
      Equals(other) { return recordEquals(this, other); }
      CompareTo(other) { return recordCompareTo(this, other); }
  }
  class FSharpRef {
      get contents() {
          return this.getter();
      }
      set contents(v) {
          this.setter(v);
      }
      constructor(contentsOrGetter, setter) {
          if (typeof setter === "function") {
              this.getter = contentsOrGetter;
              this.setter = setter;
          }
          else {
              this.getter = () => contentsOrGetter;
              this.setter = (v) => { contentsOrGetter = v; };
          }
      }
  }
  // EXCEPTIONS
  // Exception is intentionally not derived from Error, for performance reasons (see #2160)
  class Exception {
      constructor(message) {
          this.message = message;
      }
  }
  function isException(x) {
      return x instanceof Exception || x instanceof Error;
  }
  function isPromise(x) {
      return x instanceof Promise;
  }
  function ensureErrorOrException(e) {
      // Exceptionally admitting promises as errors for compatibility with React.suspense (see #3298)
      return (isException(e) || isPromise(e)) ? e : new Error(String(e));
  }

  Big.prototype.GetHashCode = function () {
      return combineHashCodes([this.s, this.e].concat(this.c));
  };
  Big.prototype.Equals = function (x) {
      return !this.cmp(x);
  };
  Big.prototype.CompareTo = function (x) {
      return this.cmp(x);
  };
  Big.prototype[symbol] = function () {
      const _this = this;
      return {
          multiply: (y) => _this.mul(y),
          toPrecision: (sd) => _this.toPrecision(sd),
          toExponential: (dp) => _this.toExponential(dp),
          toFixed: (dp) => _this.toFixed(dp),
          toHex: () => (Number(_this) >>> 0).toString(16),
      };
  };
  new Big(0);
  new Big(1);
  new Big(-1);
  new Big("79228162514264337593543950335");
  new Big("-79228162514264337593543950335");
  // export function makeRangeStepFunction(step: Decimal, last: Decimal) {
  //   const stepComparedWithZero = step.cmp(get_Zero);
  //   if (stepComparedWithZero === 0) {
  //     throw new Error("The step of a range cannot be zero");
  //   }
  //   const stepGreaterThanZero = stepComparedWithZero > 0;
  //   return (x: Decimal) => {
  //     const comparedWithLast = x.cmp(last);
  //     if ((stepGreaterThanZero && comparedWithLast <= 0)
  //       || (!stepGreaterThanZero && comparedWithLast >= 0)) {
  //       return [x, op_Addition(x, step)];
  //     } else {
  //       return undefined;
  //     }
  //   };
  // }

  BigInt.prototype.toJSON = function () {
      return `${this.toString()}`;
  };
  function fromFloat64(n) { return BigInt(Math.trunc(n)); }
  function toInt64(x) { return BigInt.asIntN(64, x); }

  /**
   * DateTimeOffset functions.
   *
   * Note: Date instances are always DateObjects in local
   * timezone (because JS dates are all kinds of messed up).
   * A local date returns UTC epoch when `.getTime()` is called.
   *
   * Basically; invariant: date.getTime() always return UTC time.
   */
  function dateOffsetToString(offset) {
      const isMinus = offset < 0;
      offset = Math.abs(offset);
      const hours = ~~(offset / 3600000);
      const minutes = (offset % 3600000) / 60000;
      return (isMinus ? "-" : "+") +
          padWithZeros(hours, 2) + ":" +
          padWithZeros(minutes, 2);
  }
  function dateToHalfUTCString(date, half) {
      const str = date.toISOString();
      return half === "first"
          ? str.substring(0, str.indexOf("T"))
          : str.substring(str.indexOf("T") + 1, str.length - 1);
  }
  function dateToISOString(d, utc) {
      if (utc) {
          return d.toISOString();
      }
      else {
          // JS Date is always local
          const printOffset = d.kind == null ? true : d.kind === 2 /* DateKind.Local */;
          return padWithZeros(d.getFullYear(), 4) + "-" +
              padWithZeros(d.getMonth() + 1, 2) + "-" +
              padWithZeros(d.getDate(), 2) + "T" +
              padWithZeros(d.getHours(), 2) + ":" +
              padWithZeros(d.getMinutes(), 2) + ":" +
              padWithZeros(d.getSeconds(), 2) + "." +
              padWithZeros(d.getMilliseconds(), 3) +
              (printOffset ? dateOffsetToString(d.getTimezoneOffset() * -60000) : "");
      }
  }
  function dateToISOStringWithOffset(dateWithOffset, offset) {
      const str = dateWithOffset.toISOString();
      return str.substring(0, str.length - 1) + dateOffsetToString(offset);
  }
  function dateToStringWithCustomFormat(date, format, utc) {
      return format.replace(/(\w)\1*/g, (match) => {
          let rep = Number.NaN;
          switch (match.substring(0, 1)) {
              case "y":
                  const y = utc ? date.getUTCFullYear() : date.getFullYear();
                  rep = match.length < 4 ? y % 100 : y;
                  break;
              case "M":
                  rep = (utc ? date.getUTCMonth() : date.getMonth()) + 1;
                  break;
              case "d":
                  rep = utc ? date.getUTCDate() : date.getDate();
                  break;
              case "H":
                  rep = utc ? date.getUTCHours() : date.getHours();
                  break;
              case "h":
                  const h = utc ? date.getUTCHours() : date.getHours();
                  rep = h > 12 ? h % 12 : h;
                  break;
              case "m":
                  rep = utc ? date.getUTCMinutes() : date.getMinutes();
                  break;
              case "s":
                  rep = utc ? date.getUTCSeconds() : date.getSeconds();
                  break;
              case "f":
                  rep = utc ? date.getUTCMilliseconds() : date.getMilliseconds();
                  break;
          }
          if (Number.isNaN(rep)) {
              return match;
          }
          else {
              return padWithZeros(rep, match.length);
          }
      });
  }
  function dateToStringWithOffset(date, format) {
      const d = new Date(date.getTime() + (date.offset ?? 0));
      if (typeof format !== "string") {
          return d.toISOString().replace(/\.\d+/, "").replace(/[A-Z]|\.\d+/g, " ") + dateOffsetToString((date.offset ?? 0));
      }
      else if (format.length === 1) {
          switch (format) {
              case "D":
              case "d": return dateToHalfUTCString(d, "first");
              case "T":
              case "t": return dateToHalfUTCString(d, "second");
              case "O":
              case "o": return dateToISOStringWithOffset(d, (date.offset ?? 0));
              default: throw new Error("Unrecognized Date print format");
          }
      }
      else {
          return dateToStringWithCustomFormat(d, format, true);
      }
  }
  function dateToStringWithKind(date, format) {
      const utc = date.kind === 1 /* DateKind.UTC */;
      if (typeof format !== "string") {
          return utc ? date.toUTCString() : date.toLocaleString();
      }
      else if (format.length === 1) {
          switch (format) {
              case "D":
              case "d":
                  return utc ? dateToHalfUTCString(date, "first") : date.toLocaleDateString();
              case "T":
              case "t":
                  return utc ? dateToHalfUTCString(date, "second") : date.toLocaleTimeString();
              case "O":
              case "o":
                  return dateToISOString(date, utc);
              default:
                  throw new Error("Unrecognized Date print format");
          }
      }
      else {
          return dateToStringWithCustomFormat(date, format, utc);
      }
  }
  function toString(date, format, _provider) {
      return date.offset != null
          ? dateToStringWithOffset(date, format)
          : dateToStringWithKind(date, format);
  }

  function create(pattern, options = 0) {
      // Supported RegexOptions
      // * IgnoreCase:  0x0001
      // * Multiline:   0x0002
      // * Compiled:    0x0008 (ignored)
      // * Singleline:  0x0010
      // * ECMAScript:  0x0100 (ignored)
      if ((options & ~(1 ^ 2 ^ 8 ^ 16 ^ 256)) !== 0) {
          throw new Error("RegexOptions only supports: IgnoreCase, Multiline, Compiled, Singleline and ECMAScript");
      }
      // Set always global and unicode flags for compatibility with dotnet, see #2925
      let flags = "gu";
      flags += options & 1 ? "i" : ""; // 0x0001 RegexOptions.IgnoreCase
      flags += options & 2 ? "m" : "";
      flags += options & 16 ? "s" : "";
      return new RegExp(pattern, flags);
  }
  // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
  function escape(str) {
      return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
  }
  function match(reg, input, startAt = 0) {
      reg.lastIndex = startAt;
      return reg.exec(input);
  }
  function matches(reg, input, startAt = 0) {
      if (input == null) {
          throw new Error("Input cannot ve null");
      }
      if (!reg.global) {
          throw new Error("Non-global RegExp"); // Prevent infinite loop
      }
      reg.lastIndex = startAt;
      const matches = [];
      let m;
      let lastMatchIndex = -1;
      // tslint:disable-next-line:no-conditional-assignment
      while ((m = reg.exec(input)) != null) {
          // It can happen even global regex get stuck, see #2845
          if (m.index === lastMatchIndex) {
              reg.lastIndex++;
          }
          else {
              lastMatchIndex = m.index;
              matches.push(m);
          }
      }
      return matches;
  }
  function replace$1(reg, input, replacement, limit, offset = 0) {
      function replacer() {
          let res = arguments[0];
          if (limit) {
              limit--;
              const match = [];
              const len = arguments.length;
              // arguments: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#specifying_a_function_as_a_parameter
              // * match: matched substring
              // * p1, p2, ...: nth capture group string
              // * offset: offset of matched substring
              // * string: whole string examined
              // * groups: named capturing groups
              //           ONLY if regex contains a named capture group AND browser supports named groups
              // -> last element can be groups OR input string
              // -> check if last element is string
              const withGroups = typeof arguments[len - 1] !== "string";
              let pLast = withGroups ? len - 3 : len - 2;
              for (let i = 0; i < pLast; i++) {
                  match.push(arguments[i]);
              }
              match.index = arguments[pLast++];
              match.input = arguments[pLast++];
              if (withGroups) {
                  match.groups = arguments[pLast];
              }
              res = replacement(match);
          }
          return res;
      }
      if (typeof reg === "string") {
          const tmp = reg;
          reg = create(input, limit ?? 0);
          input = tmp;
          limit = undefined;
      }
      if (typeof replacement === "function") {
          limit = limit == null ? -1 : limit;
          return input.substring(0, offset) + input.substring(offset).replace(reg, replacer);
      }
      else {
          replacement =
              replacement
                  // $0 doesn't work with JS regex, see #1155
                  .replace(/\$0/g, (_s) => "$&")
                  // named groups in replacement are `${name}` in .Net, but `$<name>` in JS (in regex: groups are `(?<name>...)` in both)
                  .replace(/\${([^}]+)}/g, "\$<$1>");
          if (limit != null) {
              let m;
              const sub1 = input.substring(offset);
              const _matches = matches(reg, sub1);
              const sub2 = matches.length > limit ? (m = _matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
              return input.substring(0, offset) + sub2.replace(reg, replacement)
                  + input.substring(offset + sub2.length);
          }
          else {
              return input.replace(reg, replacement);
          }
      }
  }

  const fsFormatRegExp = /(^|[^%])%([0+\- ]*)(\*|\d+)?(?:\.(\d+))?(\w)/g;
  const formatRegExp = /\{(\d+)(,-?\d+)?(?:\:([a-zA-Z])(\d{0,2})|\:(.+?))?\}/g;
  function isLessThan(x, y) {
      return compare$1(x, y) < 0;
  }
  function printf(input) {
      return {
          input,
          cont: fsFormat(input),
      };
  }
  function continuePrint(cont, arg) {
      return typeof arg === "string" ? cont(arg) : arg.cont(cont);
  }
  function toConsole(arg) {
      // Don't remove the lambda here, see #1357
      return continuePrint((x) => console.log(x), arg);
  }
  function toText(arg) {
      return continuePrint((x) => x, arg);
  }
  function toFail(arg) {
      return continuePrint((x) => {
          throw new Error(x);
      }, arg);
  }
  function formatReplacement(rep, flags, padLength, precision, format) {
      let sign = "";
      flags = flags || "";
      format = format || "";
      if (isNumeric(rep)) {
          if (format.toLowerCase() !== "x") {
              if (isLessThan(rep, 0)) {
                  rep = multiply(rep, -1);
                  sign = "-";
              }
              else {
                  if (flags.indexOf(" ") >= 0) {
                      sign = " ";
                  }
                  else if (flags.indexOf("+") >= 0) {
                      sign = "+";
                  }
              }
          }
          precision = precision == null ? null : parseInt(precision, 10);
          switch (format) {
              case "f":
              case "F":
                  precision = precision != null ? precision : 6;
                  rep = toFixed(rep, precision);
                  break;
              case "g":
              case "G":
                  rep = precision != null ? toPrecision(rep, precision) : toPrecision(rep);
                  break;
              case "e":
              case "E":
                  rep = precision != null ? toExponential(rep, precision) : toExponential(rep);
                  break;
              case "x":
                  rep = toHex(rep);
                  break;
              case "X":
                  rep = toHex(rep).toUpperCase();
                  break;
              default: // AOid
                  rep = String(rep);
                  break;
          }
      }
      else if (rep instanceof Date) {
          rep = toString(rep);
      }
      else {
          rep = toString$1(rep);
      }
      padLength = typeof padLength === "number" ? padLength : parseInt(padLength, 10);
      if (!isNaN(padLength)) {
          const zeroFlag = flags.indexOf("0") >= 0; // Use '0' for left padding
          const minusFlag = flags.indexOf("-") >= 0; // Right padding
          const ch = minusFlag || !zeroFlag ? " " : "0";
          if (ch === "0") {
              rep = pad(rep, padLength - sign.length, ch, minusFlag);
              rep = sign + rep;
          }
          else {
              rep = pad(sign + rep, padLength, ch, minusFlag);
          }
      }
      else {
          rep = sign + rep;
      }
      return rep;
  }
  function createPrinter(cont, _strParts, _matches, _result = "", padArg = -1) {
      return (...args) => {
          // Make copies of the values passed by reference because the function can be used multiple times
          let result = _result;
          const strParts = _strParts.slice();
          const matches = _matches.slice();
          for (const arg of args) {
              const [, , flags, _padLength, precision, format] = matches[0];
              let padLength = _padLength;
              if (padArg >= 0) {
                  padLength = padArg;
                  padArg = -1;
              }
              else if (padLength === "*") {
                  if (arg < 0) {
                      throw new Error("Non-negative number required");
                  }
                  padArg = arg;
                  continue;
              }
              result += strParts[0];
              result += formatReplacement(arg, flags, padLength, precision, format);
              strParts.splice(0, 1);
              matches.splice(0, 1);
          }
          if (matches.length === 0) {
              result += strParts[0];
              return cont(result);
          }
          else {
              return createPrinter(cont, strParts, matches, result, padArg);
          }
      };
  }
  function fsFormat(str) {
      return (cont) => {
          fsFormatRegExp.lastIndex = 0;
          const strParts = [];
          const matches = [];
          let strIdx = 0;
          let match = fsFormatRegExp.exec(str);
          while (match) {
              // The first group corresponds to the no-escape char (^|[^%]), the actual pattern starts in the next char
              // Note: we don't use negative lookbehind because some browsers don't support it yet
              const matchIndex = match.index + (match[1] || "").length;
              strParts.push(str.substring(strIdx, matchIndex).replace(/%%/g, "%"));
              matches.push(match);
              strIdx = fsFormatRegExp.lastIndex;
              // Likewise we need to move fsFormatRegExp.lastIndex one char behind to make sure we match the no-escape char next time
              fsFormatRegExp.lastIndex -= 1;
              match = fsFormatRegExp.exec(str);
          }
          if (strParts.length === 0) {
              return cont(str.replace(/%%/g, "%"));
          }
          else {
              strParts.push(str.substring(strIdx).replace(/%%/g, "%"));
              return createPrinter(cont, strParts, matches);
          }
      };
  }
  function format(str, ...args) {
      let str2;
      if (typeof str === "object") {
          // Called with culture info
          str2 = String(args[0]);
          args.shift();
      }
      else {
          str2 = str;
      }
      return str2.replace(formatRegExp, (_, idx, padLength, format, precision, pattern) => {
          if (idx < 0 || idx >= args.length) {
              throw new Error("Index must be greater or equal to zero and less than the arguments' length.");
          }
          let rep = args[idx];
          if (isNumeric(rep)) {
              precision = precision == null ? null : parseInt(precision, 10);
              switch (format) {
                  case "f":
                  case "F":
                      precision = precision != null ? precision : 2;
                      rep = toFixed(rep, precision);
                      break;
                  case "g":
                  case "G":
                      rep = precision != null ? toPrecision(rep, precision) : toPrecision(rep);
                      break;
                  case "e":
                  case "E":
                      rep = precision != null ? toExponential(rep, precision) : toExponential(rep);
                      break;
                  case "p":
                  case "P":
                      precision = precision != null ? precision : 2;
                      rep = toFixed(multiply(rep, 100), precision) + " %";
                      break;
                  case "d":
                  case "D":
                      rep = precision != null ? padLeft(String(rep), precision, "0") : String(rep);
                      break;
                  case "x":
                  case "X":
                      rep = precision != null ? padLeft(toHex(rep), precision, "0") : toHex(rep);
                      if (format === "X") {
                          rep = rep.toUpperCase();
                      }
                      break;
                  default:
                      if (pattern) {
                          let sign = "";
                          rep = pattern.replace(/([0#,]+)(\.[0#]+)?/, (_, intPart, decimalPart) => {
                              if (isLessThan(rep, 0)) {
                                  rep = multiply(rep, -1);
                                  sign = "-";
                              }
                              decimalPart = decimalPart == null ? "" : decimalPart.substring(1);
                              rep = toFixed(rep, Math.max(decimalPart.length, 0));
                              let [repInt, repDecimal] = rep.split(".");
                              repDecimal || (repDecimal = "");
                              const leftZeroes = intPart.replace(/,/g, "").replace(/^#+/, "").length;
                              repInt = padLeft(repInt, leftZeroes, "0");
                              const rightZeros = decimalPart.replace(/#+$/, "").length;
                              if (rightZeros > repDecimal.length) {
                                  repDecimal = padRight(repDecimal, rightZeros, "0");
                              }
                              else if (rightZeros < repDecimal.length) {
                                  repDecimal = repDecimal.substring(0, rightZeros) + repDecimal.substring(rightZeros).replace(/0+$/, "");
                              }
                              // Thousands separator
                              if (intPart.indexOf(",") > 0) {
                                  const i = repInt.length % 3;
                                  const thousandGroups = Math.floor(repInt.length / 3);
                                  let thousands = i > 0 ? repInt.substr(0, i) + (thousandGroups > 0 ? "," : "") : "";
                                  for (let j = 0; j < thousandGroups; j++) {
                                      thousands += repInt.substr(i + j * 3, 3) + (j < thousandGroups - 1 ? "," : "");
                                  }
                                  repInt = thousands;
                              }
                              return repDecimal.length > 0 ? repInt + "." + repDecimal : repInt;
                          });
                          rep = sign + rep;
                      }
              }
          }
          else if (rep instanceof Date) {
              rep = toString(rep, pattern || format);
          }
          else {
              rep = toString$1(rep);
          }
          padLength = parseInt((padLength || " ").substring(1), 10);
          if (!isNaN(padLength)) {
              rep = pad(String(rep), Math.abs(padLength), " ", padLength < 0);
          }
          return rep;
      });
  }
  function endsWith(str, search) {
      const idx = str.lastIndexOf(search);
      return idx >= 0 && idx === str.length - search.length;
  }
  function isNullOrEmpty(str) {
      return typeof str !== "string" || str.length === 0;
  }
  function isNullOrWhiteSpace(str) {
      return typeof str !== "string" || /^\s*$/.test(str);
  }
  function join(delimiter, xs) {
      if (Array.isArray(xs)) {
          return xs.join(delimiter);
      }
      else {
          return Array.from(xs).join(delimiter);
      }
  }
  function pad(str, len, ch, isRight) {
      ch = ch || " ";
      len = len - str.length;
      for (let i = 0; i < len; i++) {
          str = isRight ? str + ch : ch + str;
      }
      return str;
  }
  function padLeft(str, len, ch) {
      return pad(str, len, ch);
  }
  function padRight(str, len, ch) {
      return pad(str, len, ch, true);
  }
  function replace(str, search, replace) {
      return str.replace(new RegExp(escape(search), "g"), replace);
  }
  function split(str, splitters, count, options) {
      count = typeof count === "number" ? count : undefined;
      options = typeof options === "number" ? options : 0;
      if (count && count < 0) {
          throw new Error("Count cannot be less than zero");
      }
      if (count === 0) {
          return [];
      }
      const removeEmpty = (options & 1) === 1;
      const trim = (options & 2) === 2;
      splitters = splitters || [];
      splitters = splitters.filter(x => x).map(escape);
      splitters = splitters.length > 0 ? splitters : ["\\s"];
      const splits = [];
      const reg = new RegExp(splitters.join("|"), "g");
      let findSplits = true;
      let i = 0;
      do {
          const match = reg.exec(str);
          if (match === null) {
              const candidate = trim ? str.substring(i).trim() : str.substring(i);
              if (!removeEmpty || candidate.length > 0) {
                  splits.push(candidate);
              }
              findSplits = false;
          }
          else {
              const candidate = trim ? str.substring(i, match.index).trim() : str.substring(i, match.index);
              if (!removeEmpty || candidate.length > 0) {
                  if (count != null && splits.length + 1 === count) {
                      splits.push(trim ? str.substring(i).trim() : str.substring(i));
                      findSplits = false;
                  }
                  else {
                      splits.push(candidate);
                  }
              }
              i = reg.lastIndex;
          }
      } while (findSplits);
      return splits;
  }
  function trimStart(str, ...chars) {
      return chars.length === 0
          ? str.trimStart()
          : str.replace(new RegExp("^[" + escape(chars.join("")) + "]+"), "");
  }
  function trimEnd(str, ...chars) {
      return chars.length === 0
          ? str.trimEnd()
          : str.replace(new RegExp("[" + escape(chars.join("")) + "]+$"), "");
  }
  function substring(str, startIndex, length) {
      if ((startIndex + (length || 0) > str.length)) {
          throw new Error("Invalid startIndex and/or length");
      }
      return length != null ? str.substr(startIndex, length) : str.substr(startIndex);
  }

  class TypeInfo {
      constructor(fullname, generics, construct, parent, fields, cases, enumCases) {
          this.fullname = fullname;
          this.generics = generics;
          this.construct = construct;
          this.parent = parent;
          this.fields = fields;
          this.cases = cases;
          this.enumCases = enumCases;
      }
      toString() {
          return fullName(this);
      }
      GetHashCode() {
          return getHashCode(this);
      }
      Equals(other) {
          return equals(this, other);
      }
  }
  function getGenerics(t) {
      return t.generics != null ? t.generics : [];
  }
  function getHashCode(t) {
      const fullnameHash = stringHash(t.fullname);
      const genHashes = getGenerics(t).map(getHashCode);
      return combineHashCodes([fullnameHash, ...genHashes]);
  }
  function equals(t1, t2) {
      if (t1.fullname === "") { // Anonymous records
          return t2.fullname === ""
              && equalArraysWith(getRecordElements(t1), getRecordElements(t2), ([k1, v1], [k2, v2]) => k1 === k2 && equals(v1, v2));
      }
      else {
          return t1.fullname === t2.fullname
              && equalArraysWith(getGenerics(t1), getGenerics(t2), equals);
      }
  }
  function class_type(fullname, generics, construct, parent) {
      return new TypeInfo(fullname, generics, construct, parent);
  }
  function fullName(t) {
      const elemType = getElementType(t);
      if (elemType != null) {
          return fullName(elemType) + "[]";
      }
      else if (t.generics == null || t.generics.length === 0) {
          return t.fullname;
      }
      else {
          return t.fullname + "[" + t.generics.map((x) => fullName(x)).join(",") + "]";
      }
  }
  function getElementType(t) {
      return t.fullname === "[]" && t.generics?.length === 1 ? t.generics[0] : undefined;
  }
  function getRecordElements(t) {
      if (t.fields != null) {
          return t.fields();
      }
      else {
          throw new Error(`${t.fullname} is not an F# record type`);
      }
  }

  function Helpers_allocateArrayFromCons(cons, len) {
      if ((typeof cons) === "function") {
          return new cons(len);
      }
      else {
          return new Array(len);
      }
  }

  // Using a class here for better compatibility with TS files importing Some
  class Some {
      constructor(value) {
          this.value = value;
      }
      toJSON() {
          return this.value;
      }
      // Don't add "Some" for consistency with erased options
      toString() {
          return String(this.value);
      }
      GetHashCode() {
          return structuralHash(this.value);
      }
      Equals(other) {
          if (other == null) {
              return false;
          }
          else {
              return equals$1(this.value, other instanceof Some ? other.value : other);
          }
      }
      CompareTo(other) {
          if (other == null) {
              return 1;
          }
          else {
              return compare(this.value, other instanceof Some ? other.value : other);
          }
      }
  }
  function value(x) {
      if (x == null) {
          throw new Error("Option has no value");
      }
      else {
          return x instanceof Some ? x.value : x;
      }
  }
  function unwrap(opt) {
      return opt instanceof Some ? opt.value : opt;
  }
  function some(x) {
      return x == null || x instanceof Some ? new Some(x) : x;
  }
  function toArray$3(opt) {
      return (opt == null) ? [] : [value(opt)];
  }
  function defaultArg(opt, defaultValue) {
      return (opt != null) ? value(opt) : defaultValue;
  }
  function orElse(opt, ifNone) {
      return opt == null ? ifNone : opt;
  }
  function map$2(mapping, opt) {
      return (opt != null) ? some(mapping(value(opt))) : undefined;
  }
  function bind(binder, opt) {
      return opt != null ? binder(value(opt)) : undefined;
  }

  function max(x, y) {
      return x > y ? x : y;
  }

  const SR_indexOutOfBounds = "The index was outside the range of elements in the collection.";
  const SR_inputWasEmpty = "Collection was empty.";

  function append$1(array1, array2, cons) {
      const len1 = array1.length | 0;
      const len2 = array2.length | 0;
      const newArray = Helpers_allocateArrayFromCons(cons, len1 + len2);
      for (let i = 0; i <= (len1 - 1); i++) {
          newArray[i] = array1[i];
      }
      for (let i_1 = 0; i_1 <= (len2 - 1); i_1++) {
          newArray[i_1 + len1] = array2[i_1];
      }
      return newArray;
  }
  function fill(target, targetIndex, count, value) {
      const start = targetIndex | 0;
      return target.fill(value, start, (start + count));
  }
  function tryLast(array) {
      if (array.length === 0) {
          return void 0;
      }
      else {
          return some(array[array.length - 1]);
      }
  }
  function map$1(f, source, cons) {
      const len = source.length | 0;
      const target = Helpers_allocateArrayFromCons(cons, len);
      for (let i = 0; i <= (len - 1); i++) {
          target[i] = f(source[i]);
      }
      return target;
  }
  function truncate(count, array) {
      const count_1 = max(0, count) | 0;
      return array.slice(0, (0 + count_1));
  }
  function concat$1(arrays, cons) {
      const arrays_1 = Array.isArray(arrays) ? arrays : (Array.from(arrays));
      const matchValue = arrays_1.length | 0;
      switch (matchValue) {
          case 0:
              return Helpers_allocateArrayFromCons(cons, 0);
          case 1:
              return arrays_1[0];
          default: {
              let totalIdx = 0;
              let totalLength = 0;
              for (let idx = 0; idx <= (arrays_1.length - 1); idx++) {
                  const arr_1 = arrays_1[idx];
                  totalLength = ((totalLength + arr_1.length) | 0);
              }
              const result = Helpers_allocateArrayFromCons(cons, totalLength);
              for (let idx_1 = 0; idx_1 <= (arrays_1.length - 1); idx_1++) {
                  const arr_2 = arrays_1[idx_1];
                  for (let j = 0; j <= (arr_2.length - 1); j++) {
                      result[totalIdx] = arr_2[j];
                      totalIdx = ((totalIdx + 1) | 0);
                  }
              }
              return result;
          }
      }
  }
  function collect$1(mapping, array, cons) {
      return concat$1(map$1(mapping, array, defaultOf()), cons);
  }
  function indexOf(array, item_1, start, count, eq) {
      const start_1 = defaultArg(start, 0) | 0;
      const end$0027 = defaultArg(map$2((c) => (start_1 + c), count), array.length) | 0;
      const loop = (i_mut) => {
          loop: while (true) {
              const i = i_mut;
              if (i >= end$0027) {
                  return -1;
              }
              else if (eq.Equals(item_1, array[i])) {
                  return i | 0;
              }
              else {
                  i_mut = (i + 1);
                  continue loop;
              }
          }
      };
      return loop(start_1) | 0;
  }
  function contains$1(value, array, eq) {
      return indexOf(array, value, void 0, void 0, eq) >= 0;
  }
  function singleton$3(value, cons) {
      const ar = Helpers_allocateArrayFromCons(cons, 1);
      ar[0] = value;
      return ar;
  }
  function pairwise(array) {
      if (array.length < 2) {
          return [];
      }
      else {
          const count = (array.length - 1) | 0;
          const result = new Array(count);
          for (let i = 0; i <= (count - 1); i++) {
              result[i] = [array[i], array[i + 1]];
          }
          return result;
      }
  }
  function reverse(array) {
      const array_2 = array.slice();
      return array_2.reverse();
  }
  function partition(f, source, cons) {
      const len = source.length | 0;
      const res1 = Helpers_allocateArrayFromCons(cons, len);
      const res2 = Helpers_allocateArrayFromCons(cons, len);
      let iTrue = 0;
      let iFalse = 0;
      for (let i = 0; i <= (len - 1); i++) {
          if (f(source[i])) {
              res1[iTrue] = source[i];
              iTrue = ((iTrue + 1) | 0);
          }
          else {
              res2[iFalse] = source[i];
              iFalse = ((iFalse + 1) | 0);
          }
      }
      return [truncate(iTrue, res1), truncate(iFalse, res2)];
  }
  function tryFindIndex$1(predicate, array) {
      const matchValue = (array.findIndex(predicate)) | 0;
      if (matchValue > -1) {
          return matchValue;
      }
      else {
          return void 0;
      }
  }
  function tryFindBack(predicate, array) {
      const loop = (i_mut) => {
          loop: while (true) {
              const i = i_mut;
              if (i < 0) {
                  return void 0;
              }
              else if (predicate(array[i])) {
                  return some(array[i]);
              }
              else {
                  i_mut = (i - 1);
                  continue loop;
              }
          }
      };
      return loop(array.length - 1);
  }
  function fold$2(folder, state, array) {
      return array.reduce((folder), state);
  }
  function sortInPlace(xs, comparer) {
      xs.sort((x, y) => comparer.Compare(x, y));
  }
  function sort(xs, comparer) {
      const xs_1 = xs.slice();
      xs_1.sort((x, y) => comparer.Compare(x, y));
      return xs_1;
  }
  function splitAt(index, array) {
      if ((index < 0) ? true : (index > array.length)) {
          throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
      }
      return [array.slice(0, (0 + index)), array.slice(index)];
  }
  function equalsWith(equals, array1, array2) {
      if (array1 == null) {
          if (array2 == null) {
              return true;
          }
          else {
              return false;
          }
      }
      else if (array2 == null) {
          return false;
      }
      else {
          let i = 0;
          let result = true;
          const length1 = array1.length | 0;
          const length2 = array2.length | 0;
          if (length1 > length2) {
              return false;
          }
          else if (length1 < length2) {
              return false;
          }
          else {
              while ((i < length1) && result) {
                  result = equals(array1[i], array2[i]);
                  i = ((i + 1) | 0);
              }
              return result;
          }
      }
  }
  function tryHead$2(array) {
      if (array.length === 0) {
          return void 0;
      }
      else {
          return some(array[0]);
      }
  }

  class CmdLineOptions extends Record {
      constructor(outDir, libDir, benchmark, optimize, sourceMaps, typedArrays, language, printAst) {
          super();
          this.outDir = outDir;
          this.libDir = libDir;
          this.benchmark = benchmark;
          this.optimize = optimize;
          this.sourceMaps = sourceMaps;
          this.typedArrays = typedArrays;
          this.language = language;
          this.printAst = printAst;
      }
  }

  const JS_fs = fs__namespace;

  const JS_os = os__namespace;

  const JS_proc = process__namespace;

  const JS_path = Path__namespace;

  const JS_util = util;

  function readAllBytes(filePath) {
      return JS_fs.readFileSync(filePath);
  }

  function readAllText(filePath) {
      return trimStart(JS_fs.readFileSync(filePath, "utf8"), "﻿");
  }

  function writeAllText(filePath, text) {
      JS_fs.writeFileSync(filePath, text);
  }

  function measureTime(f, x) {
      const startTime = JS_proc.hrtime();
      const res = f(x);
      const elapsed = JS_proc.hrtime(startTime);
      return [res, toInt64(fromFloat64((elapsed[0] * 1000) + (elapsed[1] / 1000000)))];
  }

  function ensureDirExists(dir) {
      JS_util.ensureDirExists(dir);
  }

  function serializeToJson(data) {
      return JS_util.serializeToJson(data);
  }

  function runCmdAndExitIfFails(cmd) {
      JS_util.runCmdAndExitIfFails(cmd);
  }

  function normalizePath$1(path) {
      return replace(path, "\\", "/");
  }

  function normalizeFullPath(path) {
      return replace(JS_path.resolve(path), "\\", "/");
  }

  function getRelativePath$1(path, pathTo) {
      const relPath = replace(JS_path.relative(path, pathTo), "\\", "/");
      if ((relPath.indexOf("./") === 0) ? true : (relPath.indexOf("../") === 0)) {
          return relPath;
      }
      else {
          return "./" + relPath;
      }
  }

  function getHomePath() {
      return JS_os.homedir();
  }

  function getDirFiles(path, extension) {
      let array;
      return sort(map$1((x_1) => replace(x_1, "\\", "/"), (array = JS_util.getDirFiles(path), array.filter((x) => endsWith(x, extension)))), {
          Compare: comparePrimitives,
      });
  }

  function getGlobFiles(path) {
      let normPath, i;
      if ((path.indexOf("*") >= 0) ? true : (path.indexOf("?") >= 0)) {
          return getDirFiles((normPath = replace(path, "\\", "/"), (i = (normPath.lastIndexOf("/") | 0), (i < 0) ? "" : substring(normPath, 0, i))), ".fs");
      }
      else {
          return [path];
      }
  }

  function Path_Combine(path1, path2) {
      return ((path1.length === 0) ? path1 : (trimEnd(path1, "\\", "/") + "/")) + trimStart(path2, "\\", "/");
  }

  function Path_ChangeExtension(path, ext) {
      const i = path.lastIndexOf(".") | 0;
      if (i < 0) {
          return path;
      }
      else {
          return substring(path, 0, i) + ext;
      }
  }

  function Path_GetFileName(path) {
      const normPath = trimEnd(replace(path, "\\", "/"), "/");
      return substring(normPath, normPath.lastIndexOf("/") + 1);
  }

  function Path_GetFileNameWithoutExtension(path) {
      const path_1 = Path_GetFileName(path);
      return substring(path_1, 0, path_1.lastIndexOf("."));
  }

  function Path_GetDirectoryName(path) {
      const normPath = replace(path, "\\", "/");
      const i = normPath.lastIndexOf("/") | 0;
      if (i < 0) {
          return "";
      }
      else {
          return substring(normPath, 0, i);
      }
  }

  class FSharpList extends Record {
      constructor(head, tail) {
          super();
          this.head = head;
          this.tail = tail;
      }
      toString() {
          const xs = this;
          return ("[" + join("; ", xs)) + "]";
      }
      Equals(other) {
          const xs = this;
          if (xs === other) {
              return true;
          }
          else {
              const loop = (xs_1_mut, ys_1_mut) => {
                  loop: while (true) {
                      const xs_1 = xs_1_mut, ys_1 = ys_1_mut;
                      const matchValue = xs_1.tail;
                      const matchValue_1 = ys_1.tail;
                      if (matchValue != null) {
                          if (matchValue_1 != null) {
                              const xt = value(matchValue);
                              const yt = value(matchValue_1);
                              if (equals$1(xs_1.head, ys_1.head)) {
                                  xs_1_mut = xt;
                                  ys_1_mut = yt;
                                  continue loop;
                              }
                              else {
                                  return false;
                              }
                          }
                          else {
                              return false;
                          }
                      }
                      else if (matchValue_1 != null) {
                          return false;
                      }
                      else {
                          return true;
                      }
                  }
              };
              return loop(xs, other);
          }
      }
      GetHashCode() {
          const xs = this;
          const loop = (i_mut, h_mut, xs_1_mut) => {
              loop: while (true) {
                  const i = i_mut, h = h_mut, xs_1 = xs_1_mut;
                  const matchValue = xs_1.tail;
                  if (matchValue != null) {
                      const t = value(matchValue);
                      if (i > 18) {
                          return h | 0;
                      }
                      else {
                          i_mut = (i + 1);
                          h_mut = (((h << 1) + structuralHash(xs_1.head)) + (631 * i));
                          xs_1_mut = t;
                          continue loop;
                      }
                  }
                  else {
                      return h | 0;
                  }
              }
          };
          return loop(0, 0, xs) | 0;
      }
      toJSON() {
          const this$ = this;
          return Array.from(this$);
      }
      CompareTo(other) {
          const xs = this;
          const loop = (xs_1_mut, ys_1_mut) => {
              loop: while (true) {
                  const xs_1 = xs_1_mut, ys_1 = ys_1_mut;
                  const matchValue = xs_1.tail;
                  const matchValue_1 = ys_1.tail;
                  if (matchValue != null) {
                      if (matchValue_1 != null) {
                          const xt = value(matchValue);
                          const yt = value(matchValue_1);
                          const c = compare(xs_1.head, ys_1.head) | 0;
                          if (c === 0) {
                              xs_1_mut = xt;
                              ys_1_mut = yt;
                              continue loop;
                          }
                          else {
                              return c | 0;
                          }
                      }
                      else {
                          return 1;
                      }
                  }
                  else if (matchValue_1 != null) {
                      return -1;
                  }
                  else {
                      return 0;
                  }
              }
          };
          return loop(xs, other) | 0;
      }
      GetEnumerator() {
          const xs = this;
          return ListEnumerator$1_$ctor_3002E699(xs);
      }
      [Symbol.iterator]() {
          return toIterator(getEnumerator(this));
      }
      "System.Collections.IEnumerable.GetEnumerator"() {
          const xs = this;
          return getEnumerator(xs);
      }
  }
  class ListEnumerator$1 {
      constructor(xs) {
          this.xs = xs;
          this.it = this.xs;
          this.current = defaultOf();
      }
      "System.Collections.Generic.IEnumerator`1.get_Current"() {
          const _ = this;
          return _.current;
      }
      "System.Collections.IEnumerator.get_Current"() {
          const _ = this;
          return _.current;
      }
      "System.Collections.IEnumerator.MoveNext"() {
          const _ = this;
          const matchValue = _.it.tail;
          if (matchValue != null) {
              const t = value(matchValue);
              _.current = _.it.head;
              _.it = t;
              return true;
          }
          else {
              return false;
          }
      }
      "System.Collections.IEnumerator.Reset"() {
          const _ = this;
          _.it = _.xs;
          _.current = defaultOf();
      }
      Dispose() {
      }
  }
  function ListEnumerator$1_$ctor_3002E699(xs) {
      return new ListEnumerator$1(xs);
  }
  function FSharpList_get_Empty() {
      return new FSharpList(defaultOf(), void 0);
  }
  function FSharpList_Cons_305B8EAC(x, xs) {
      return new FSharpList(x, xs);
  }
  function FSharpList__get_IsEmpty(xs) {
      return xs.tail == null;
  }
  function FSharpList__get_Length(xs) {
      const loop = (i_mut, xs_1_mut) => {
          loop: while (true) {
              const i = i_mut, xs_1 = xs_1_mut;
              const matchValue = xs_1.tail;
              if (matchValue != null) {
                  i_mut = (i + 1);
                  xs_1_mut = value(matchValue);
                  continue loop;
              }
              else {
                  return i | 0;
              }
          }
      };
      return loop(0, xs) | 0;
  }
  function FSharpList__get_Head(xs) {
      const matchValue = xs.tail;
      if (matchValue != null) {
          return xs.head;
      }
      else {
          throw new Error((SR_inputWasEmpty + "\\nParameter name: ") + "list");
      }
  }
  function FSharpList__get_Tail(xs) {
      const matchValue = xs.tail;
      if (matchValue != null) {
          return value(matchValue);
      }
      else {
          throw new Error((SR_inputWasEmpty + "\\nParameter name: ") + "list");
      }
  }
  function empty$1() {
      return FSharpList_get_Empty();
  }
  function cons(x, xs) {
      return FSharpList_Cons_305B8EAC(x, xs);
  }
  function singleton$2(x) {
      return FSharpList_Cons_305B8EAC(x, FSharpList_get_Empty());
  }
  function isEmpty(xs) {
      return FSharpList__get_IsEmpty(xs);
  }
  function length$1(xs) {
      return FSharpList__get_Length(xs);
  }
  function head(xs) {
      return FSharpList__get_Head(xs);
  }
  function tryHead$1(xs) {
      if (FSharpList__get_IsEmpty(xs)) {
          return void 0;
      }
      else {
          return some(FSharpList__get_Head(xs));
      }
  }
  function tail(xs) {
      return FSharpList__get_Tail(xs);
  }
  function toArray$2(xs) {
      const len = FSharpList__get_Length(xs) | 0;
      const res = fill(new Array(len), 0, len, null);
      const loop = (i_mut, xs_1_mut) => {
          loop: while (true) {
              const i = i_mut, xs_1 = xs_1_mut;
              if (!FSharpList__get_IsEmpty(xs_1)) {
                  res[i] = FSharpList__get_Head(xs_1);
                  i_mut = (i + 1);
                  xs_1_mut = FSharpList__get_Tail(xs_1);
                  continue loop;
              }
              break;
          }
      };
      loop(0, xs);
      return res;
  }
  function fold$1(folder, state, xs) {
      let acc = state;
      let xs_1 = xs;
      while (!FSharpList__get_IsEmpty(xs_1)) {
          acc = folder(acc, head(xs_1));
          xs_1 = FSharpList__get_Tail(xs_1);
      }
      return acc;
  }
  function ofArrayWithTail(xs, tail_1) {
      let res = tail_1;
      for (let i = xs.length - 1; i >= 0; i--) {
          res = FSharpList_Cons_305B8EAC(xs[i], res);
      }
      return res;
  }
  function ofArray(xs) {
      return ofArrayWithTail(xs, FSharpList_get_Empty());
  }
  function ofSeq$3(xs) {
      let xs_3, t;
      if (isArrayLike(xs)) {
          return ofArray(xs);
      }
      else if (xs instanceof FSharpList) {
          return xs;
      }
      else {
          const root = FSharpList_get_Empty();
          let node = root;
          const enumerator = getEnumerator(xs);
          try {
              while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                  const x = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                  node = ((xs_3 = node, (t = (new FSharpList(x, void 0)), (xs_3.tail = t, t))));
              }
          }
          finally {
              disposeSafe(enumerator);
          }
          const xs_5 = node;
          const t_2 = FSharpList_get_Empty();
          xs_5.tail = t_2;
          return FSharpList__get_Tail(root);
      }
  }
  function tryFindIndex(f, xs) {
      const loop = (i_mut, xs_1_mut) => {
          loop: while (true) {
              const i = i_mut, xs_1 = xs_1_mut;
              if (FSharpList__get_IsEmpty(xs_1)) {
                  return void 0;
              }
              else if (f(FSharpList__get_Head(xs_1))) {
                  return i;
              }
              else {
                  i_mut = (i + 1);
                  xs_1_mut = FSharpList__get_Tail(xs_1);
                  continue loop;
              }
          }
      };
      return loop(0, xs);
  }
  function contains(value, xs, eq) {
      return tryFindIndex((v) => eq.Equals(value, v), xs) != null;
  }

  class StringBuilder {
      constructor(value, capacity) {
          this.buf = [];
          if (!isNullOrEmpty(value)) {
              void (this.buf.push(value));
          }
      }
      toString() {
          const _ = this;
          return join("", _.buf);
      }
  }
  function StringBuilder_$ctor_Z18115A39(value, capacity) {
      return new StringBuilder(value, capacity);
  }
  function StringBuilder_$ctor() {
      return StringBuilder_$ctor_Z18115A39("", 16);
  }
  function StringBuilder__Append_Z721C83C5(x, s) {
      void (x.buf.push(s));
      return x;
  }
  function StringBuilder__Append_244C7CD6(x, c) {
      void (x.buf.push(c));
      return x;
  }

  function Operators_NullArg(x) {
      throw new Error(x);
  }

  const SR_enumerationAlreadyFinished = "Enumeration already finished.";
  const SR_enumerationNotStarted = "Enumeration has not started. Call MoveNext.";
  const SR_resetNotSupported = "Reset is not supported on this enumerator.";
  function Enumerator_noReset() {
      throw new Error(SR_resetNotSupported);
  }
  function Enumerator_notStarted() {
      throw new Error(SR_enumerationNotStarted);
  }
  function Enumerator_alreadyFinished() {
      throw new Error(SR_enumerationAlreadyFinished);
  }
  class Enumerator_Seq {
      constructor(f) {
          this.f = f;
      }
      toString() {
          const xs = this;
          let i = 0;
          let str = "seq [";
          const e = getEnumerator(xs);
          try {
              while ((i < 4) && e["System.Collections.IEnumerator.MoveNext"]()) {
                  if (i > 0) {
                      str = (str + "; ");
                  }
                  str = (str + toString$1(e["System.Collections.Generic.IEnumerator`1.get_Current"]()));
                  i = ((i + 1) | 0);
              }
              if (i === 4) {
                  str = (str + "; ...");
              }
              return str + "]";
          }
          finally {
              disposeSafe(e);
          }
      }
      GetEnumerator() {
          const x = this;
          return x.f();
      }
      [Symbol.iterator]() {
          return toIterator(getEnumerator(this));
      }
      "System.Collections.IEnumerable.GetEnumerator"() {
          const x = this;
          return x.f();
      }
  }
  function Enumerator_Seq_$ctor_673A07F2(f) {
      return new Enumerator_Seq(f);
  }
  class Enumerator_FromFunctions$1 {
      constructor(current, next, dispose) {
          this.current = current;
          this.next = next;
          this.dispose = dispose;
      }
      "System.Collections.Generic.IEnumerator`1.get_Current"() {
          const _ = this;
          return _.current();
      }
      "System.Collections.IEnumerator.get_Current"() {
          const _ = this;
          return _.current();
      }
      "System.Collections.IEnumerator.MoveNext"() {
          const _ = this;
          return _.next();
      }
      "System.Collections.IEnumerator.Reset"() {
          Enumerator_noReset();
      }
      Dispose() {
          const _ = this;
          _.dispose();
      }
  }
  function Enumerator_FromFunctions$1_$ctor_58C54629(current, next, dispose) {
      return new Enumerator_FromFunctions$1(current, next, dispose);
  }
  function Enumerator_concat(sources) {
      let outerOpt = void 0;
      let innerOpt = void 0;
      let started = false;
      let finished = false;
      let curr = void 0;
      const finish = () => {
          finished = true;
          if (innerOpt != null) {
              const inner = value(innerOpt);
              try {
                  disposeSafe(inner);
              }
              finally {
                  innerOpt = void 0;
              }
          }
          if (outerOpt != null) {
              const outer = value(outerOpt);
              try {
                  disposeSafe(outer);
              }
              finally {
                  outerOpt = void 0;
              }
          }
      };
      return Enumerator_FromFunctions$1_$ctor_58C54629(() => {
          if (!started) {
              Enumerator_notStarted();
          }
          else if (finished) {
              Enumerator_alreadyFinished();
          }
          if (curr != null) {
              return value(curr);
          }
          else {
              return Enumerator_alreadyFinished();
          }
      }, () => {
          let copyOfStruct;
          if (!started) {
              started = true;
          }
          if (finished) {
              return false;
          }
          else {
              let res = void 0;
              while (res == null) {
                  const outerOpt_1 = outerOpt;
                  const innerOpt_1 = innerOpt;
                  if (outerOpt_1 != null) {
                      if (innerOpt_1 != null) {
                          const inner_1 = value(innerOpt_1);
                          if (inner_1["System.Collections.IEnumerator.MoveNext"]()) {
                              curr = some(inner_1["System.Collections.Generic.IEnumerator`1.get_Current"]());
                              res = true;
                          }
                          else {
                              try {
                                  disposeSafe(inner_1);
                              }
                              finally {
                                  innerOpt = void 0;
                              }
                          }
                      }
                      else {
                          const outer_1 = value(outerOpt_1);
                          if (outer_1["System.Collections.IEnumerator.MoveNext"]()) {
                              const ie = outer_1["System.Collections.Generic.IEnumerator`1.get_Current"]();
                              innerOpt = ((copyOfStruct = ie, getEnumerator(copyOfStruct)));
                          }
                          else {
                              finish();
                              res = false;
                          }
                      }
                  }
                  else {
                      outerOpt = getEnumerator(sources);
                  }
              }
              return value(res);
          }
      }, () => {
          if (!finished) {
              finish();
          }
      });
  }
  function Enumerator_generateWhileSome(openf, compute, closef) {
      let started = false;
      let curr = void 0;
      let state = some(openf());
      const dispose = () => {
          if (state != null) {
              const x_1 = value(state);
              try {
                  closef(x_1);
              }
              finally {
                  state = void 0;
              }
          }
      };
      const finish = () => {
          try {
              dispose();
          }
          finally {
              curr = void 0;
          }
      };
      return Enumerator_FromFunctions$1_$ctor_58C54629(() => {
          if (!started) {
              Enumerator_notStarted();
          }
          if (curr != null) {
              return value(curr);
          }
          else {
              return Enumerator_alreadyFinished();
          }
      }, () => {
          if (!started) {
              started = true;
          }
          if (state != null) {
              const s = value(state);
              let matchValue_1;
              try {
                  matchValue_1 = compute(s);
              }
              catch (matchValue) {
                  finish();
                  throw matchValue;
              }
              if (matchValue_1 != null) {
                  curr = matchValue_1;
                  return true;
              }
              else {
                  finish();
                  return false;
              }
          }
          else {
              return false;
          }
      }, dispose);
  }
  function Enumerator_unfold(f, state) {
      let curr = void 0;
      let acc = state;
      return Enumerator_FromFunctions$1_$ctor_58C54629(() => {
          if (curr != null) {
              const x = value(curr)[0];
              value(curr)[1];
              return x;
          }
          else {
              return Enumerator_notStarted();
          }
      }, () => {
          curr = f(acc);
          if (curr != null) {
              value(curr)[0];
              const st_1 = value(curr)[1];
              acc = st_1;
              return true;
          }
          else {
              return false;
          }
      }, () => {
      });
  }
  function checkNonNull(argName, arg) {
      if (arg == null) {
          Operators_NullArg(argName);
      }
  }
  function mkSeq(f) {
      return Enumerator_Seq_$ctor_673A07F2(f);
  }
  function ofSeq$2(xs) {
      checkNonNull("source", xs);
      return getEnumerator(xs);
  }
  function delay(generator) {
      return mkSeq(() => getEnumerator(generator()));
  }
  function concat(sources) {
      return mkSeq(() => Enumerator_concat(sources));
  }
  function unfold(generator, state) {
      return mkSeq(() => Enumerator_unfold(generator, state));
  }
  function empty() {
      return delay(() => (new Array(0)));
  }
  function singleton$1(x) {
      return delay(() => singleton$3(x));
  }
  function toArray$1(xs) {
      if (xs instanceof FSharpList) {
          const a = xs;
          return toArray$2(a);
      }
      else {
          return Array.from(xs);
      }
  }
  function toList(xs) {
      if (isArrayLike(xs)) {
          return ofArray(xs);
      }
      else if (xs instanceof FSharpList) {
          return xs;
      }
      else {
          return ofSeq$3(xs);
      }
  }
  function generate(create, compute, dispose) {
      return mkSeq(() => Enumerator_generateWhileSome(create, compute, dispose));
  }
  function append(xs, ys) {
      return concat([xs, ys]);
  }
  function choose(chooser, xs) {
      return generate(() => ofSeq$2(xs), (e) => {
          let curr = void 0;
          while ((curr == null) && e["System.Collections.IEnumerator.MoveNext"]()) {
              curr = chooser(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
          }
          return curr;
      }, (e_1) => {
          disposeSafe(e_1);
      });
  }
  function compareWith(comparer, xs, ys) {
      const e1 = ofSeq$2(xs);
      try {
          const e2 = ofSeq$2(ys);
          try {
              let c = 0;
              let b1 = e1["System.Collections.IEnumerator.MoveNext"]();
              let b2 = e2["System.Collections.IEnumerator.MoveNext"]();
              while (((c === 0) && b1) && b2) {
                  c = (comparer(e1["System.Collections.Generic.IEnumerator`1.get_Current"](), e2["System.Collections.Generic.IEnumerator`1.get_Current"]()) | 0);
                  if (c === 0) {
                      b1 = e1["System.Collections.IEnumerator.MoveNext"]();
                      b2 = e2["System.Collections.IEnumerator.MoveNext"]();
                  }
              }
              return ((c !== 0) ? c : (b1 ? 1 : (b2 ? -1 : 0))) | 0;
          }
          finally {
              disposeSafe(e2);
          }
      }
      finally {
          disposeSafe(e1);
      }
  }
  function filter(f, xs) {
      return choose((x) => {
          if (f(x)) {
              return some(x);
          }
          else {
              return void 0;
          }
      }, xs);
  }
  function fold(folder, state, xs) {
      const e = ofSeq$2(xs);
      try {
          let acc = state;
          while (e["System.Collections.IEnumerator.MoveNext"]()) {
              acc = folder(acc, e["System.Collections.Generic.IEnumerator`1.get_Current"]());
          }
          return acc;
      }
      finally {
          disposeSafe(e);
      }
  }
  function tryHead(xs) {
      if (isArrayLike(xs)) {
          return tryHead$2(xs);
      }
      else if (xs instanceof FSharpList) {
          return tryHead$1(xs);
      }
      else {
          const e = ofSeq$2(xs);
          try {
              return e["System.Collections.IEnumerator.MoveNext"]() ? some(e["System.Collections.Generic.IEnumerator`1.get_Current"]()) : void 0;
          }
          finally {
              disposeSafe(e);
          }
      }
  }
  function iterate(action, xs) {
      fold((unitVar, x) => {
          action(x);
      }, void 0, xs);
  }
  function iterateIndexed(action, xs) {
      fold((i, x) => {
          action(i, x);
          return (i + 1) | 0;
      }, 0, xs);
  }
  function length(xs) {
      if (isArrayLike(xs)) {
          const a = xs;
          return a.length | 0;
      }
      else if (xs instanceof FSharpList) {
          return length$1(xs) | 0;
      }
      else {
          const e = ofSeq$2(xs);
          try {
              let count = 0;
              while (e["System.Collections.IEnumerator.MoveNext"]()) {
                  count = ((count + 1) | 0);
              }
              return count | 0;
          }
          finally {
              disposeSafe(e);
          }
      }
  }
  function map(mapping, xs) {
      return generate(() => ofSeq$2(xs), (e) => (e["System.Collections.IEnumerator.MoveNext"]() ? some(mapping(e["System.Collections.Generic.IEnumerator`1.get_Current"]())) : void 0), (e_1) => {
          disposeSafe(e_1);
      });
  }
  function collect(mapping, xs) {
      return delay(() => concat(map(mapping, xs)));
  }

  function tryGetValue(map, key, defaultValue) {
      if (map.has(key)) {
          defaultValue.contents = map.get(key);
          return true;
      }
      return false;
  }
  function addToSet(v, set) {
      if (set.has(v)) {
          return false;
      }
      set.add(v);
      return true;
  }
  function addToDict(dict, k, v) {
      if (dict.has(k)) {
          throw new Error("An item with the same key has already been added. Key: " + k);
      }
      dict.set(k, v);
  }
  function getItemFromDict(map, key) {
      if (map.has(key)) {
          return map.get(key);
      }
      else {
          throw new Error(`The given key '${key}' was not present in the dictionary.`);
      }
  }

  class HashSet {
      constructor(items, comparer) {
          const this$ = new FSharpRef(defaultOf());
          this.comparer = comparer;
          this$.contents = this;
          this.hashMap = (new Map([]));
          this["init@9"] = 1;
          const enumerator = getEnumerator(items);
          try {
              while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                  const item = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                  HashSet__Add_2B595(this$.contents, item);
              }
          }
          finally {
              disposeSafe(enumerator);
          }
      }
      get [Symbol.toStringTag]() {
          return "HashSet";
      }
      toJSON() {
          const this$ = this;
          return Array.from(this$);
      }
      "System.Collections.IEnumerable.GetEnumerator"() {
          const this$ = this;
          return getEnumerator(this$);
      }
      GetEnumerator() {
          const this$ = this;
          return getEnumerator(concat(this$.hashMap.values()));
      }
      [Symbol.iterator]() {
          return toIterator(getEnumerator(this));
      }
      "System.Collections.Generic.ICollection`1.Add2B595"(item) {
          const this$ = this;
          HashSet__Add_2B595(this$, item);
      }
      "System.Collections.Generic.ICollection`1.Clear"() {
          const this$ = this;
          HashSet__Clear(this$);
      }
      "System.Collections.Generic.ICollection`1.Contains2B595"(item) {
          const this$ = this;
          return HashSet__Contains_2B595(this$, item);
      }
      "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(array, arrayIndex) {
          const this$ = this;
          iterateIndexed((i, e) => {
              array[arrayIndex + i] = e;
          }, this$);
      }
      "System.Collections.Generic.ICollection`1.get_Count"() {
          const this$ = this;
          return HashSet__get_Count(this$) | 0;
      }
      "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
          return false;
      }
      "System.Collections.Generic.ICollection`1.Remove2B595"(item) {
          const this$ = this;
          return HashSet__Remove_2B595(this$, item);
      }
      get size() {
          const this$ = this;
          return HashSet__get_Count(this$) | 0;
      }
      add(k) {
          const this$ = this;
          HashSet__Add_2B595(this$, k);
          return this$;
      }
      clear() {
          const this$ = this;
          HashSet__Clear(this$);
      }
      delete(k) {
          const this$ = this;
          return HashSet__Remove_2B595(this$, k);
      }
      has(k) {
          const this$ = this;
          return HashSet__Contains_2B595(this$, k);
      }
      keys() {
          const this$ = this;
          return map((x) => x, this$);
      }
      values() {
          const this$ = this;
          return map((x) => x, this$);
      }
      entries() {
          const this$ = this;
          return map((v) => [v, v], this$);
      }
      forEach(f, thisArg) {
          const this$ = this;
          iterate((x) => {
              f(x, x, this$);
          }, this$);
      }
  }
  function HashSet__TryFindIndex_2B595(this$, k) {
      const h = this$.comparer.GetHashCode(k) | 0;
      let matchValue;
      let outArg = defaultOf();
      matchValue = [tryGetValue(this$.hashMap, h, new FSharpRef(() => outArg, (v) => {
              outArg = v;
          })), outArg];
      if (matchValue[0]) {
          return [true, h, matchValue[1].findIndex((v_1) => this$.comparer.Equals(k, v_1))];
      }
      else {
          return [false, h, -1];
      }
  }
  function HashSet__Clear(this$) {
      this$.hashMap.clear();
  }
  function HashSet__get_Count(this$) {
      let count = 0;
      let enumerator = getEnumerator(this$.hashMap.values());
      try {
          while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
              const items = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
              count = ((count + items.length) | 0);
          }
      }
      finally {
          disposeSafe(enumerator);
      }
      return count | 0;
  }
  function HashSet__Add_2B595(this$, k) {
      const matchValue = HashSet__TryFindIndex_2B595(this$, k);
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              return false;
          }
          else {
              void (getItemFromDict(this$.hashMap, matchValue[1]).push(k));
              return true;
          }
      }
      else {
          this$.hashMap.set(matchValue[1], [k]);
          return true;
      }
  }
  function HashSet__Contains_2B595(this$, k) {
      const matchValue = HashSet__TryFindIndex_2B595(this$, k);
      let matchResult;
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              matchResult = 0;
          }
          else {
              matchResult = 1;
          }
      }
      else {
          matchResult = 1;
      }
      switch (matchResult) {
          case 0:
              return true;
          default:
              return false;
      }
  }
  function HashSet__Remove_2B595(this$, k) {
      const matchValue = HashSet__TryFindIndex_2B595(this$, k);
      let matchResult;
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              matchResult = 0;
          }
          else {
              matchResult = 1;
          }
      }
      else {
          matchResult = 1;
      }
      switch (matchResult) {
          case 0: {
              getItemFromDict(this$.hashMap, matchValue[1]).splice(matchValue[2], 1);
              return true;
          }
          default:
              return false;
      }
  }

  class SetTreeLeaf$1 {
      constructor(k) {
          this.k = k;
      }
  }
  function SetTreeLeaf$1_$ctor_2B595(k) {
      return new SetTreeLeaf$1(k);
  }
  function SetTreeLeaf$1__get_Key(_) {
      return _.k;
  }
  class SetTreeNode$1 extends SetTreeLeaf$1 {
      constructor(v, left, right, h) {
          super(v);
          this.left = left;
          this.right = right;
          this.h = (h | 0);
      }
  }
  function SetTreeNode$1_$ctor_5F465FC9(v, left, right, h) {
      return new SetTreeNode$1(v, left, right, h);
  }
  function SetTreeNode$1__get_Left(_) {
      return _.left;
  }
  function SetTreeNode$1__get_Right(_) {
      return _.right;
  }
  function SetTreeNode$1__get_Height(_) {
      return _.h;
  }
  function SetTreeModule_empty() {
      return void 0;
  }
  function SetTreeModule_countAux(t_mut, acc_mut) {
      SetTreeModule_countAux: while (true) {
          const t = t_mut, acc = acc_mut;
          if (t != null) {
              const t2 = value(t);
              if (t2 instanceof SetTreeNode$1) {
                  const tn = t2;
                  t_mut = SetTreeNode$1__get_Left(tn);
                  acc_mut = SetTreeModule_countAux(SetTreeNode$1__get_Right(tn), acc + 1);
                  continue SetTreeModule_countAux;
              }
              else {
                  return (acc + 1) | 0;
              }
          }
          else {
              return acc | 0;
          }
      }
  }
  function SetTreeModule_count(s) {
      return SetTreeModule_countAux(s, 0);
  }
  function SetTreeModule_mk(l, k, r) {
      let tn, tn_1;
      let hl;
      const t = l;
      if (t != null) {
          const t2 = value(t);
          hl = ((t2 instanceof SetTreeNode$1) ? ((tn = t2, SetTreeNode$1__get_Height(tn))) : 1);
      }
      else {
          hl = 0;
      }
      let hr;
      const t_1 = r;
      if (t_1 != null) {
          const t2_1 = value(t_1);
          hr = ((t2_1 instanceof SetTreeNode$1) ? ((tn_1 = t2_1, SetTreeNode$1__get_Height(tn_1))) : 1);
      }
      else {
          hr = 0;
      }
      const m = ((hl < hr) ? hr : hl) | 0;
      if (m === 0) {
          return SetTreeLeaf$1_$ctor_2B595(k);
      }
      else {
          return SetTreeNode$1_$ctor_5F465FC9(k, l, r, m + 1);
      }
  }
  function SetTreeModule_rebalance(t1, v, t2) {
      let tn, tn_1, t_2, t2_3, tn_2, t_3, t2_4, tn_3;
      let t1h;
      const t = t1;
      if (t != null) {
          const t2_1 = value(t);
          t1h = ((t2_1 instanceof SetTreeNode$1) ? ((tn = t2_1, SetTreeNode$1__get_Height(tn))) : 1);
      }
      else {
          t1h = 0;
      }
      let t2h;
      const t_1 = t2;
      if (t_1 != null) {
          const t2_2 = value(t_1);
          t2h = ((t2_2 instanceof SetTreeNode$1) ? ((tn_1 = t2_2, SetTreeNode$1__get_Height(tn_1))) : 1);
      }
      else {
          t2h = 0;
      }
      if (t2h > (t1h + 2)) {
          const matchValue = value(t2);
          if (matchValue instanceof SetTreeNode$1) {
              const t2$0027 = matchValue;
              if (((t_2 = SetTreeNode$1__get_Left(t2$0027), (t_2 != null) ? ((t2_3 = value(t_2), (t2_3 instanceof SetTreeNode$1) ? ((tn_2 = t2_3, SetTreeNode$1__get_Height(tn_2))) : 1)) : 0)) > (t1h + 1)) {
                  const matchValue_1 = value(SetTreeNode$1__get_Left(t2$0027));
                  if (matchValue_1 instanceof SetTreeNode$1) {
                      const t2l = matchValue_1;
                      return SetTreeModule_mk(SetTreeModule_mk(t1, v, SetTreeNode$1__get_Left(t2l)), SetTreeLeaf$1__get_Key(t2l), SetTreeModule_mk(SetTreeNode$1__get_Right(t2l), SetTreeLeaf$1__get_Key(t2$0027), SetTreeNode$1__get_Right(t2$0027)));
                  }
                  else {
                      throw new Error("internal error: Set.rebalance");
                  }
              }
              else {
                  return SetTreeModule_mk(SetTreeModule_mk(t1, v, SetTreeNode$1__get_Left(t2$0027)), SetTreeLeaf$1__get_Key(t2$0027), SetTreeNode$1__get_Right(t2$0027));
              }
          }
          else {
              throw new Error("internal error: Set.rebalance");
          }
      }
      else if (t1h > (t2h + 2)) {
          const matchValue_2 = value(t1);
          if (matchValue_2 instanceof SetTreeNode$1) {
              const t1$0027 = matchValue_2;
              if (((t_3 = SetTreeNode$1__get_Right(t1$0027), (t_3 != null) ? ((t2_4 = value(t_3), (t2_4 instanceof SetTreeNode$1) ? ((tn_3 = t2_4, SetTreeNode$1__get_Height(tn_3))) : 1)) : 0)) > (t2h + 1)) {
                  const matchValue_3 = value(SetTreeNode$1__get_Right(t1$0027));
                  if (matchValue_3 instanceof SetTreeNode$1) {
                      const t1r = matchValue_3;
                      return SetTreeModule_mk(SetTreeModule_mk(SetTreeNode$1__get_Left(t1$0027), SetTreeLeaf$1__get_Key(t1$0027), SetTreeNode$1__get_Left(t1r)), SetTreeLeaf$1__get_Key(t1r), SetTreeModule_mk(SetTreeNode$1__get_Right(t1r), v, t2));
                  }
                  else {
                      throw new Error("internal error: Set.rebalance");
                  }
              }
              else {
                  return SetTreeModule_mk(SetTreeNode$1__get_Left(t1$0027), SetTreeLeaf$1__get_Key(t1$0027), SetTreeModule_mk(SetTreeNode$1__get_Right(t1$0027), v, t2));
              }
          }
          else {
              throw new Error("internal error: Set.rebalance");
          }
      }
      else {
          return SetTreeModule_mk(t1, v, t2);
      }
  }
  function SetTreeModule_add(comparer, k, t) {
      if (t != null) {
          const t2 = value(t);
          const c = comparer.Compare(k, SetTreeLeaf$1__get_Key(t2)) | 0;
          if (t2 instanceof SetTreeNode$1) {
              const tn = t2;
              if (c < 0) {
                  return SetTreeModule_rebalance(SetTreeModule_add(comparer, k, SetTreeNode$1__get_Left(tn)), SetTreeLeaf$1__get_Key(tn), SetTreeNode$1__get_Right(tn));
              }
              else if (c === 0) {
                  return t;
              }
              else {
                  return SetTreeModule_rebalance(SetTreeNode$1__get_Left(tn), SetTreeLeaf$1__get_Key(tn), SetTreeModule_add(comparer, k, SetTreeNode$1__get_Right(tn)));
              }
          }
          else {
              const c_1 = comparer.Compare(k, SetTreeLeaf$1__get_Key(t2)) | 0;
              if (c_1 < 0) {
                  return SetTreeNode$1_$ctor_5F465FC9(k, SetTreeModule_empty(), t, 2);
              }
              else if (c_1 === 0) {
                  return t;
              }
              else {
                  return SetTreeNode$1_$ctor_5F465FC9(k, t, SetTreeModule_empty(), 2);
              }
          }
      }
      else {
          return SetTreeLeaf$1_$ctor_2B595(k);
      }
  }
  function SetTreeModule_mem(comparer_mut, k_mut, t_mut) {
      SetTreeModule_mem: while (true) {
          const comparer = comparer_mut, k = k_mut, t = t_mut;
          if (t != null) {
              const t2 = value(t);
              const c = comparer.Compare(k, SetTreeLeaf$1__get_Key(t2)) | 0;
              if (t2 instanceof SetTreeNode$1) {
                  const tn = t2;
                  if (c < 0) {
                      comparer_mut = comparer;
                      k_mut = k;
                      t_mut = SetTreeNode$1__get_Left(tn);
                      continue SetTreeModule_mem;
                  }
                  else if (c === 0) {
                      return true;
                  }
                  else {
                      comparer_mut = comparer;
                      k_mut = k;
                      t_mut = SetTreeNode$1__get_Right(tn);
                      continue SetTreeModule_mem;
                  }
              }
              else {
                  return c === 0;
              }
          }
          else {
              return false;
          }
      }
  }
  function SetTreeModule_iter(f_mut, t_mut) {
      SetTreeModule_iter: while (true) {
          const f = f_mut, t = t_mut;
          if (t != null) {
              const t2 = value(t);
              if (t2 instanceof SetTreeNode$1) {
                  const tn = t2;
                  SetTreeModule_iter(f, SetTreeNode$1__get_Left(tn));
                  f(SetTreeLeaf$1__get_Key(tn));
                  f_mut = f;
                  t_mut = SetTreeNode$1__get_Right(tn);
                  continue SetTreeModule_iter;
              }
              else {
                  f(SetTreeLeaf$1__get_Key(t2));
              }
          }
          break;
      }
  }
  class SetTreeModule_SetIterator$1 extends Record {
      constructor(stack, started) {
          super();
          this.stack = stack;
          this.started = started;
      }
  }
  function SetTreeModule_collapseLHS(stack_mut) {
      SetTreeModule_collapseLHS: while (true) {
          const stack = stack_mut;
          if (!isEmpty(stack)) {
              const x = head(stack);
              const rest = tail(stack);
              if (x != null) {
                  const x2 = value(x);
                  if (x2 instanceof SetTreeNode$1) {
                      const xn = x2;
                      stack_mut = ofArrayWithTail([SetTreeNode$1__get_Left(xn), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(xn)), SetTreeNode$1__get_Right(xn)], rest);
                      continue SetTreeModule_collapseLHS;
                  }
                  else {
                      return stack;
                  }
              }
              else {
                  stack_mut = rest;
                  continue SetTreeModule_collapseLHS;
              }
          }
          else {
              return empty$1();
          }
      }
  }
  function SetTreeModule_mkIterator(s) {
      return new SetTreeModule_SetIterator$1(SetTreeModule_collapseLHS(singleton$2(s)), false);
  }
  function SetTreeModule_notStarted() {
      throw new Error("Enumeration not started");
  }
  function SetTreeModule_alreadyFinished() {
      throw new Error("Enumeration already started");
  }
  function SetTreeModule_current(i) {
      if (i.started) {
          const matchValue = i.stack;
          if (isEmpty(matchValue)) {
              return SetTreeModule_alreadyFinished();
          }
          else if (head(matchValue) != null) {
              const t = value(head(matchValue));
              return SetTreeLeaf$1__get_Key(t);
          }
          else {
              throw new Error("Please report error: Set iterator, unexpected stack for current");
          }
      }
      else {
          return SetTreeModule_notStarted();
      }
  }
  function SetTreeModule_moveNext(i) {
      if (i.started) {
          const matchValue = i.stack;
          if (!isEmpty(matchValue)) {
              if (head(matchValue) != null) {
                  const t = value(head(matchValue));
                  if (t instanceof SetTreeNode$1) {
                      throw new Error("Please report error: Set iterator, unexpected stack for moveNext");
                  }
                  else {
                      i.stack = SetTreeModule_collapseLHS(tail(matchValue));
                      return !isEmpty(i.stack);
                  }
              }
              else {
                  throw new Error("Please report error: Set iterator, unexpected stack for moveNext");
              }
          }
          else {
              return false;
          }
      }
      else {
          i.started = true;
          return !isEmpty(i.stack);
      }
  }
  function SetTreeModule_mkIEnumerator(s) {
      let i = SetTreeModule_mkIterator(s);
      return {
          "System.Collections.Generic.IEnumerator`1.get_Current"() {
              return SetTreeModule_current(i);
          },
          "System.Collections.IEnumerator.get_Current"() {
              return SetTreeModule_current(i);
          },
          "System.Collections.IEnumerator.MoveNext"() {
              return SetTreeModule_moveNext(i);
          },
          "System.Collections.IEnumerator.Reset"() {
              i = SetTreeModule_mkIterator(s);
          },
          Dispose() {
          },
      };
  }
  /**
   * Set comparison.  Note this can be expensive.
   */
  function SetTreeModule_compareStacks(comparer_mut, l1_mut, l2_mut) {
      SetTreeModule_compareStacks: while (true) {
          const comparer = comparer_mut, l1 = l1_mut, l2 = l2_mut;
          if (!isEmpty(l1)) {
              if (!isEmpty(l2)) {
                  if (head(l2) != null) {
                      if (head(l1) != null) {
                          const x1_3 = value(head(l1));
                          const x2_3 = value(head(l2));
                          if (x1_3 instanceof SetTreeNode$1) {
                              const x1n_2 = x1_3;
                              if (SetTreeNode$1__get_Left(x1n_2) == null) {
                                  if (x2_3 instanceof SetTreeNode$1) {
                                      const x2n_2 = x2_3;
                                      if (SetTreeNode$1__get_Left(x2n_2) == null) {
                                          const c = comparer.Compare(SetTreeLeaf$1__get_Key(x1n_2), SetTreeLeaf$1__get_Key(x2n_2)) | 0;
                                          if (c !== 0) {
                                              return c | 0;
                                          }
                                          else {
                                              comparer_mut = comparer;
                                              l1_mut = cons(SetTreeNode$1__get_Right(x1n_2), tail(l1));
                                              l2_mut = cons(SetTreeNode$1__get_Right(x2n_2), tail(l2));
                                              continue SetTreeModule_compareStacks;
                                          }
                                      }
                                      else {
                                          let matchResult, t1_6, x1_4, t2_6, x2_4;
                                          if (!isEmpty(l1)) {
                                              if (head(l1) != null) {
                                                  matchResult = 0;
                                                  t1_6 = tail(l1);
                                                  x1_4 = value(head(l1));
                                              }
                                              else if (!isEmpty(l2)) {
                                                  if (head(l2) != null) {
                                                      matchResult = 1;
                                                      t2_6 = tail(l2);
                                                      x2_4 = value(head(l2));
                                                  }
                                                  else {
                                                      matchResult = 2;
                                                  }
                                              }
                                              else {
                                                  matchResult = 2;
                                              }
                                          }
                                          else if (!isEmpty(l2)) {
                                              if (head(l2) != null) {
                                                  matchResult = 1;
                                                  t2_6 = tail(l2);
                                                  x2_4 = value(head(l2));
                                              }
                                              else {
                                                  matchResult = 2;
                                              }
                                          }
                                          else {
                                              matchResult = 2;
                                          }
                                          switch (matchResult) {
                                              case 0:
                                                  if (x1_4 instanceof SetTreeNode$1) {
                                                      const x1n_3 = x1_4;
                                                      comparer_mut = comparer;
                                                      l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_3), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_3), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_3), 0)], t1_6);
                                                      l2_mut = l2;
                                                      continue SetTreeModule_compareStacks;
                                                  }
                                                  else {
                                                      comparer_mut = comparer;
                                                      l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_4))], t1_6);
                                                      l2_mut = l2;
                                                      continue SetTreeModule_compareStacks;
                                                  }
                                              case 1:
                                                  if (x2_4 instanceof SetTreeNode$1) {
                                                      const x2n_3 = x2_4;
                                                      comparer_mut = comparer;
                                                      l1_mut = l1;
                                                      l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_3), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_3), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_3), 0)], t2_6);
                                                      continue SetTreeModule_compareStacks;
                                                  }
                                                  else {
                                                      comparer_mut = comparer;
                                                      l1_mut = l1;
                                                      l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_4))], t2_6);
                                                      continue SetTreeModule_compareStacks;
                                                  }
                                              default:
                                                  throw new Error("unexpected state in SetTree.compareStacks");
                                          }
                                      }
                                  }
                                  else {
                                      const c_1 = comparer.Compare(SetTreeLeaf$1__get_Key(x1n_2), SetTreeLeaf$1__get_Key(x2_3)) | 0;
                                      if (c_1 !== 0) {
                                          return c_1 | 0;
                                      }
                                      else {
                                          comparer_mut = comparer;
                                          l1_mut = cons(SetTreeNode$1__get_Right(x1n_2), tail(l1));
                                          l2_mut = cons(SetTreeModule_empty(), tail(l2));
                                          continue SetTreeModule_compareStacks;
                                      }
                                  }
                              }
                              else {
                                  let matchResult_1, t1_7, x1_5, t2_7, x2_5;
                                  if (!isEmpty(l1)) {
                                      if (head(l1) != null) {
                                          matchResult_1 = 0;
                                          t1_7 = tail(l1);
                                          x1_5 = value(head(l1));
                                      }
                                      else if (!isEmpty(l2)) {
                                          if (head(l2) != null) {
                                              matchResult_1 = 1;
                                              t2_7 = tail(l2);
                                              x2_5 = value(head(l2));
                                          }
                                          else {
                                              matchResult_1 = 2;
                                          }
                                      }
                                      else {
                                          matchResult_1 = 2;
                                      }
                                  }
                                  else if (!isEmpty(l2)) {
                                      if (head(l2) != null) {
                                          matchResult_1 = 1;
                                          t2_7 = tail(l2);
                                          x2_5 = value(head(l2));
                                      }
                                      else {
                                          matchResult_1 = 2;
                                      }
                                  }
                                  else {
                                      matchResult_1 = 2;
                                  }
                                  switch (matchResult_1) {
                                      case 0:
                                          if (x1_5 instanceof SetTreeNode$1) {
                                              const x1n_4 = x1_5;
                                              comparer_mut = comparer;
                                              l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_4), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_4), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_4), 0)], t1_7);
                                              l2_mut = l2;
                                              continue SetTreeModule_compareStacks;
                                          }
                                          else {
                                              comparer_mut = comparer;
                                              l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_5))], t1_7);
                                              l2_mut = l2;
                                              continue SetTreeModule_compareStacks;
                                          }
                                      case 1:
                                          if (x2_5 instanceof SetTreeNode$1) {
                                              const x2n_4 = x2_5;
                                              comparer_mut = comparer;
                                              l1_mut = l1;
                                              l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_4), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_4), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_4), 0)], t2_7);
                                              continue SetTreeModule_compareStacks;
                                          }
                                          else {
                                              comparer_mut = comparer;
                                              l1_mut = l1;
                                              l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_5))], t2_7);
                                              continue SetTreeModule_compareStacks;
                                          }
                                      default:
                                          throw new Error("unexpected state in SetTree.compareStacks");
                                  }
                              }
                          }
                          else if (x2_3 instanceof SetTreeNode$1) {
                              const x2n_5 = x2_3;
                              if (SetTreeNode$1__get_Left(x2n_5) == null) {
                                  const c_2 = comparer.Compare(SetTreeLeaf$1__get_Key(x1_3), SetTreeLeaf$1__get_Key(x2n_5)) | 0;
                                  if (c_2 !== 0) {
                                      return c_2 | 0;
                                  }
                                  else {
                                      comparer_mut = comparer;
                                      l1_mut = cons(SetTreeModule_empty(), tail(l1));
                                      l2_mut = cons(SetTreeNode$1__get_Right(x2n_5), tail(l2));
                                      continue SetTreeModule_compareStacks;
                                  }
                              }
                              else {
                                  let matchResult_2, t1_8, x1_6, t2_8, x2_6;
                                  if (!isEmpty(l1)) {
                                      if (head(l1) != null) {
                                          matchResult_2 = 0;
                                          t1_8 = tail(l1);
                                          x1_6 = value(head(l1));
                                      }
                                      else if (!isEmpty(l2)) {
                                          if (head(l2) != null) {
                                              matchResult_2 = 1;
                                              t2_8 = tail(l2);
                                              x2_6 = value(head(l2));
                                          }
                                          else {
                                              matchResult_2 = 2;
                                          }
                                      }
                                      else {
                                          matchResult_2 = 2;
                                      }
                                  }
                                  else if (!isEmpty(l2)) {
                                      if (head(l2) != null) {
                                          matchResult_2 = 1;
                                          t2_8 = tail(l2);
                                          x2_6 = value(head(l2));
                                      }
                                      else {
                                          matchResult_2 = 2;
                                      }
                                  }
                                  else {
                                      matchResult_2 = 2;
                                  }
                                  switch (matchResult_2) {
                                      case 0:
                                          if (x1_6 instanceof SetTreeNode$1) {
                                              const x1n_5 = x1_6;
                                              comparer_mut = comparer;
                                              l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_5), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_5), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_5), 0)], t1_8);
                                              l2_mut = l2;
                                              continue SetTreeModule_compareStacks;
                                          }
                                          else {
                                              comparer_mut = comparer;
                                              l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_6))], t1_8);
                                              l2_mut = l2;
                                              continue SetTreeModule_compareStacks;
                                          }
                                      case 1:
                                          if (x2_6 instanceof SetTreeNode$1) {
                                              const x2n_6 = x2_6;
                                              comparer_mut = comparer;
                                              l1_mut = l1;
                                              l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_6), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_6), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_6), 0)], t2_8);
                                              continue SetTreeModule_compareStacks;
                                          }
                                          else {
                                              comparer_mut = comparer;
                                              l1_mut = l1;
                                              l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_6))], t2_8);
                                              continue SetTreeModule_compareStacks;
                                          }
                                      default:
                                          throw new Error("unexpected state in SetTree.compareStacks");
                                  }
                              }
                          }
                          else {
                              const c_3 = comparer.Compare(SetTreeLeaf$1__get_Key(x1_3), SetTreeLeaf$1__get_Key(x2_3)) | 0;
                              if (c_3 !== 0) {
                                  return c_3 | 0;
                              }
                              else {
                                  comparer_mut = comparer;
                                  l1_mut = tail(l1);
                                  l2_mut = tail(l2);
                                  continue SetTreeModule_compareStacks;
                              }
                          }
                      }
                      else {
                          value(head(l2));
                          let matchResult_3, t1_2, x1, t2_2, x2_1;
                          if (!isEmpty(l1)) {
                              if (head(l1) != null) {
                                  matchResult_3 = 0;
                                  t1_2 = tail(l1);
                                  x1 = value(head(l1));
                              }
                              else if (!isEmpty(l2)) {
                                  if (head(l2) != null) {
                                      matchResult_3 = 1;
                                      t2_2 = tail(l2);
                                      x2_1 = value(head(l2));
                                  }
                                  else {
                                      matchResult_3 = 2;
                                  }
                              }
                              else {
                                  matchResult_3 = 2;
                              }
                          }
                          else if (!isEmpty(l2)) {
                              if (head(l2) != null) {
                                  matchResult_3 = 1;
                                  t2_2 = tail(l2);
                                  x2_1 = value(head(l2));
                              }
                              else {
                                  matchResult_3 = 2;
                              }
                          }
                          else {
                              matchResult_3 = 2;
                          }
                          switch (matchResult_3) {
                              case 0:
                                  if (x1 instanceof SetTreeNode$1) {
                                      const x1n = x1;
                                      comparer_mut = comparer;
                                      l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n), 0)], t1_2);
                                      l2_mut = l2;
                                      continue SetTreeModule_compareStacks;
                                  }
                                  else {
                                      comparer_mut = comparer;
                                      l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1))], t1_2);
                                      l2_mut = l2;
                                      continue SetTreeModule_compareStacks;
                                  }
                              case 1:
                                  if (x2_1 instanceof SetTreeNode$1) {
                                      const x2n = x2_1;
                                      comparer_mut = comparer;
                                      l1_mut = l1;
                                      l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n), 0)], t2_2);
                                      continue SetTreeModule_compareStacks;
                                  }
                                  else {
                                      comparer_mut = comparer;
                                      l1_mut = l1;
                                      l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_1))], t2_2);
                                      continue SetTreeModule_compareStacks;
                                  }
                              default:
                                  throw new Error("unexpected state in SetTree.compareStacks");
                          }
                      }
                  }
                  else if (head(l1) != null) {
                      value(head(l1));
                      let matchResult_4, t1_4, x1_2, t2_4, x2_2;
                      if (!isEmpty(l1)) {
                          if (head(l1) != null) {
                              matchResult_4 = 0;
                              t1_4 = tail(l1);
                              x1_2 = value(head(l1));
                          }
                          else if (!isEmpty(l2)) {
                              if (head(l2) != null) {
                                  matchResult_4 = 1;
                                  t2_4 = tail(l2);
                                  x2_2 = value(head(l2));
                              }
                              else {
                                  matchResult_4 = 2;
                              }
                          }
                          else {
                              matchResult_4 = 2;
                          }
                      }
                      else if (!isEmpty(l2)) {
                          if (head(l2) != null) {
                              matchResult_4 = 1;
                              t2_4 = tail(l2);
                              x2_2 = value(head(l2));
                          }
                          else {
                              matchResult_4 = 2;
                          }
                      }
                      else {
                          matchResult_4 = 2;
                      }
                      switch (matchResult_4) {
                          case 0:
                              if (x1_2 instanceof SetTreeNode$1) {
                                  const x1n_1 = x1_2;
                                  comparer_mut = comparer;
                                  l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_1), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_1), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_1), 0)], t1_4);
                                  l2_mut = l2;
                                  continue SetTreeModule_compareStacks;
                              }
                              else {
                                  comparer_mut = comparer;
                                  l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_2))], t1_4);
                                  l2_mut = l2;
                                  continue SetTreeModule_compareStacks;
                              }
                          case 1:
                              if (x2_2 instanceof SetTreeNode$1) {
                                  const x2n_1 = x2_2;
                                  comparer_mut = comparer;
                                  l1_mut = l1;
                                  l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_1), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_1), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_1), 0)], t2_4);
                                  continue SetTreeModule_compareStacks;
                              }
                              else {
                                  comparer_mut = comparer;
                                  l1_mut = l1;
                                  l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_2))], t2_4);
                                  continue SetTreeModule_compareStacks;
                              }
                          default:
                              throw new Error("unexpected state in SetTree.compareStacks");
                      }
                  }
                  else {
                      comparer_mut = comparer;
                      l1_mut = tail(l1);
                      l2_mut = tail(l2);
                      continue SetTreeModule_compareStacks;
                  }
              }
              else {
                  return 1;
              }
          }
          else if (isEmpty(l2)) {
              return 0;
          }
          else {
              return -1;
          }
      }
  }
  function SetTreeModule_compare(comparer, t1, t2) {
      if (t1 == null) {
          if (t2 == null) {
              return 0;
          }
          else {
              return -1;
          }
      }
      else if (t2 == null) {
          return 1;
      }
      else {
          return SetTreeModule_compareStacks(comparer, singleton$2(t1), singleton$2(t2)) | 0;
      }
  }
  function SetTreeModule_copyToArray(s, arr, i) {
      let j = i;
      SetTreeModule_iter((x) => {
          arr[j] = x;
          j = ((j + 1) | 0);
      }, s);
  }
  function SetTreeModule_mkFromEnumerator(comparer_mut, acc_mut, e_mut) {
      SetTreeModule_mkFromEnumerator: while (true) {
          const comparer = comparer_mut, acc = acc_mut, e = e_mut;
          if (e["System.Collections.IEnumerator.MoveNext"]()) {
              comparer_mut = comparer;
              acc_mut = SetTreeModule_add(comparer, e["System.Collections.Generic.IEnumerator`1.get_Current"](), acc);
              e_mut = e;
              continue SetTreeModule_mkFromEnumerator;
          }
          else {
              return acc;
          }
      }
  }
  function SetTreeModule_ofArray(comparer, l) {
      return fold$2((acc, k) => SetTreeModule_add(comparer, k, acc), SetTreeModule_empty(), l);
  }
  function SetTreeModule_ofList(comparer, l) {
      return fold$1((acc, k) => SetTreeModule_add(comparer, k, acc), SetTreeModule_empty(), l);
  }
  function SetTreeModule_ofSeq(comparer, c) {
      if (isArrayLike(c)) {
          return SetTreeModule_ofArray(comparer, c);
      }
      else if (c instanceof FSharpList) {
          return SetTreeModule_ofList(comparer, c);
      }
      else {
          const ie = getEnumerator(c);
          try {
              return SetTreeModule_mkFromEnumerator(comparer, SetTreeModule_empty(), ie);
          }
          finally {
              disposeSafe(ie);
          }
      }
  }
  class FSharpSet {
      constructor(comparer, tree) {
          this.comparer = comparer;
          this.tree = tree;
      }
      GetHashCode() {
          const this$ = this;
          return FSharpSet__ComputeHashCode(this$) | 0;
      }
      Equals(that) {
          let that_1;
          const this$ = this;
          return (that instanceof FSharpSet) && ((that_1 = that, SetTreeModule_compare(FSharpSet__get_Comparer(this$), FSharpSet__get_Tree(this$), FSharpSet__get_Tree(that_1)) === 0));
      }
      toString() {
          const this$ = this;
          return ("set [" + join("; ", map((x) => {
              let copyOfStruct = x;
              return toString$1(copyOfStruct);
          }, this$))) + "]";
      }
      get [Symbol.toStringTag]() {
          return "FSharpSet";
      }
      toJSON() {
          const this$ = this;
          return Array.from(this$);
      }
      CompareTo(that) {
          const s = this;
          return SetTreeModule_compare(FSharpSet__get_Comparer(s), FSharpSet__get_Tree(s), FSharpSet__get_Tree(that)) | 0;
      }
      "System.Collections.Generic.ICollection`1.Add2B595"(x) {
          throw new Error("ReadOnlyCollection");
      }
      "System.Collections.Generic.ICollection`1.Clear"() {
          throw new Error("ReadOnlyCollection");
      }
      "System.Collections.Generic.ICollection`1.Remove2B595"(x) {
          throw new Error("ReadOnlyCollection");
      }
      "System.Collections.Generic.ICollection`1.Contains2B595"(x) {
          const s = this;
          return SetTreeModule_mem(FSharpSet__get_Comparer(s), x, FSharpSet__get_Tree(s));
      }
      "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(arr, i) {
          const s = this;
          SetTreeModule_copyToArray(FSharpSet__get_Tree(s), arr, i);
      }
      "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
          return true;
      }
      "System.Collections.Generic.ICollection`1.get_Count"() {
          const s = this;
          return FSharpSet__get_Count(s) | 0;
      }
      "System.Collections.Generic.IReadOnlyCollection`1.get_Count"() {
          const s = this;
          return FSharpSet__get_Count(s) | 0;
      }
      GetEnumerator() {
          const s = this;
          return SetTreeModule_mkIEnumerator(FSharpSet__get_Tree(s));
      }
      [Symbol.iterator]() {
          return toIterator(getEnumerator(this));
      }
      "System.Collections.IEnumerable.GetEnumerator"() {
          const s = this;
          return SetTreeModule_mkIEnumerator(FSharpSet__get_Tree(s));
      }
      get size() {
          const s = this;
          return FSharpSet__get_Count(s) | 0;
      }
      add(k) {
          throw new Error("Set cannot be mutated");
      }
      clear() {
          throw new Error("Set cannot be mutated");
      }
      delete(k) {
          throw new Error("Set cannot be mutated");
      }
      has(k) {
          const s = this;
          return FSharpSet__Contains(s, k);
      }
      keys() {
          const s = this;
          return map((x) => x, s);
      }
      values() {
          const s = this;
          return map((x) => x, s);
      }
      entries() {
          const s = this;
          return map((v) => [v, v], s);
      }
      forEach(f, thisArg) {
          const s = this;
          iterate((x) => {
              f(x, x, s);
          }, s);
      }
  }
  function FSharpSet_$ctor(comparer, tree) {
      return new FSharpSet(comparer, tree);
  }
  function FSharpSet__get_Comparer(set$) {
      return set$.comparer;
  }
  function FSharpSet__get_Tree(set$) {
      return set$.tree;
  }
  function FSharpSet__get_Count(s) {
      return SetTreeModule_count(FSharpSet__get_Tree(s));
  }
  function FSharpSet__Contains(s, value) {
      return SetTreeModule_mem(FSharpSet__get_Comparer(s), value, FSharpSet__get_Tree(s));
  }
  function FSharpSet__ComputeHashCode(this$) {
      let res = 0;
      const enumerator = getEnumerator(this$);
      try {
          while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
              const x_1 = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
              res = ((((res << 1) + structuralHash(x_1)) + 631) | 0);
          }
      }
      finally {
          disposeSafe(enumerator);
      }
      return Math.abs(res) | 0;
  }
  function ofSeq$1(elements, comparer) {
      return FSharpSet_$ctor(comparer, SetTreeModule_ofSeq(comparer, elements));
  }
  function unionWith(s1, s2) {
      return fold((acc, x) => acc.add(x), s1, s2);
  }

  class CancellationToken {
      constructor(cancelled = false) {
          this._id = 0;
          this._cancelled = cancelled;
          this._listeners = new Map();
      }
      get isCancelled() {
          return this._cancelled;
      }
      cancel() {
          if (!this._cancelled) {
              this._cancelled = true;
              for (const [, listener] of this._listeners) {
                  listener();
              }
          }
      }
      addListener(f) {
          const id = this._id;
          this._listeners.set(this._id++, f);
          return id;
      }
      removeListener(id) {
          return this._listeners.delete(id);
      }
      register(f, state) {
          const $ = this;
          const id = this.addListener(state == null ? f : () => f(state));
          return { Dispose() { $.removeListener(id); } };
      }
      Dispose() {
          // Implement IDisposable for compatibility but do nothing
          // According to docs, calling Dispose does not trigger cancellation
          // https://docs.microsoft.com/en-us/dotnet/api/system.threading.cancellationtokensource.dispose?view=net-6.0
      }
  }
  class OperationCanceledError extends Error {
      constructor() {
          super("The operation was canceled");
          Object.setPrototypeOf(this, OperationCanceledError.prototype);
      }
  }
  class Trampoline {
      static get maxTrampolineCallCount() {
          return 2000;
      }
      constructor() {
          this.callCount = 0;
      }
      incrementAndCheck() {
          return this.callCount++ > Trampoline.maxTrampolineCallCount;
      }
      hijack(f) {
          this.callCount = 0;
          setTimeout(f, 0);
      }
  }
  function protectedCont(f) {
      return (ctx) => {
          if (ctx.cancelToken.isCancelled) {
              ctx.onCancel(new OperationCanceledError());
          }
          else if (ctx.trampoline.incrementAndCheck()) {
              ctx.trampoline.hijack(() => {
                  try {
                      f(ctx);
                  }
                  catch (err) {
                      ctx.onError(ensureErrorOrException(err));
                  }
              });
          }
          else {
              try {
                  f(ctx);
              }
              catch (err) {
                  ctx.onError(ensureErrorOrException(err));
              }
          }
      };
  }
  function protectedBind(computation, binder) {
      return protectedCont((ctx) => {
          computation({
              onSuccess: (x) => {
                  try {
                      binder(x)(ctx);
                  }
                  catch (err) {
                      ctx.onError(ensureErrorOrException(err));
                  }
              },
              onError: ctx.onError,
              onCancel: ctx.onCancel,
              cancelToken: ctx.cancelToken,
              trampoline: ctx.trampoline,
          });
      });
  }
  function protectedReturn(value) {
      return protectedCont((ctx) => ctx.onSuccess(value));
  }
  class AsyncBuilder {
      Bind(computation, binder) {
          return protectedBind(computation, binder);
      }
      Combine(computation1, computation2) {
          return this.Bind(computation1, () => computation2);
      }
      Delay(generator) {
          return protectedCont((ctx) => generator()(ctx));
      }
      For(sequence, body) {
          const iter = sequence[Symbol.iterator]();
          let cur = iter.next();
          return this.While(() => !cur.done, this.Delay(() => {
              const res = body(cur.value);
              cur = iter.next();
              return res;
          }));
      }
      Return(value) {
          return protectedReturn(value);
      }
      ReturnFrom(computation) {
          return computation;
      }
      TryFinally(computation, compensation) {
          return protectedCont((ctx) => {
              computation({
                  onSuccess: (x) => {
                      compensation();
                      ctx.onSuccess(x);
                  },
                  onError: (x) => {
                      compensation();
                      ctx.onError(x);
                  },
                  onCancel: (x) => {
                      compensation();
                      ctx.onCancel(x);
                  },
                  cancelToken: ctx.cancelToken,
                  trampoline: ctx.trampoline,
              });
          });
      }
      TryWith(computation, catchHandler) {
          return protectedCont((ctx) => {
              computation({
                  onSuccess: ctx.onSuccess,
                  onCancel: ctx.onCancel,
                  cancelToken: ctx.cancelToken,
                  trampoline: ctx.trampoline,
                  onError: (ex) => {
                      try {
                          catchHandler(ex)(ctx);
                      }
                      catch (err) {
                          ctx.onError(ensureErrorOrException(err));
                      }
                  },
              });
          });
      }
      Using(resource, binder) {
          return this.TryFinally(binder(resource), () => resource.Dispose());
      }
      While(guard, computation) {
          if (guard()) {
              return this.Bind(computation, () => this.While(guard, computation));
          }
          else {
              return this.Return(void 0);
          }
      }
      Zero() {
          return protectedCont((ctx) => ctx.onSuccess(void 0));
      }
  }
  const singleton = new AsyncBuilder();

  class MappingIndex extends Record {
      constructor(line, column) {
          super();
          this.line = (line | 0);
          this.column = (column | 0);
      }
  }

  class Mapping extends Record {
      constructor(Generated, Original, Source, Name) {
          super();
          this.Generated = Generated;
          this.Original = Original;
          this.Source = Source;
          this.Name = Name;
      }
  }

  class SourceGeneratorJSON extends Record {
      constructor(version, sources, names, mappings, file, sourcesContent, sourceRoot) {
          super();
          this.version = (version | 0);
          this.sources = sources;
          this.names = names;
          this.mappings = mappings;
          this.file = file;
          this.sourcesContent = sourcesContent;
          this.sourceRoot = sourceRoot;
      }
  }

  function strcmp(s1, s2) {
      if ((s1 != null) && (s2 != null)) {
          if (equals$1(s1, s2)) {
              return 0;
          }
          else if (compare(s1, s2) > 0) {
              return 1;
          }
          else {
              return -1;
          }
      }
      else if ((s1 == null) && (s2 == null)) {
          return 0;
      }
      else if (s1 == null) {
          return 1;
      }
      else if (s2 == null) {
          return -1;
      }
      else {
          return -1;
      }
  }

  function compareByGeneratedPositionsInflated(mappingA, mappingB) {
      let cmp = mappingA.Generated.line - mappingB.Generated.line;
      if (cmp !== 0) {
          return cmp | 0;
      }
      else {
          cmp = ((mappingA.Generated.column - mappingB.Generated.column) | 0);
          if (cmp !== 0) {
              return cmp | 0;
          }
          else {
              cmp = (strcmp(mappingA.Source, mappingB.Source) | 0);
              if (cmp !== 0) {
                  return cmp | 0;
              }
              else {
                  const matchValue = mappingA.Original;
                  const matchValue_1 = mappingB.Original;
                  let matchResult, mapAOriginal, mapBOriginal;
                  if (matchValue != null) {
                      if (matchValue_1 != null) {
                          matchResult = 0;
                          mapAOriginal = matchValue;
                          mapBOriginal = matchValue_1;
                      }
                      else {
                          matchResult = 1;
                      }
                  }
                  else {
                      matchResult = 1;
                  }
                  switch (matchResult) {
                      case 0: {
                          cmp = ((mapAOriginal.line - mapBOriginal.line) | 0);
                          if (cmp !== 0) {
                              return cmp | 0;
                          }
                          else {
                              cmp = ((mapAOriginal.column - mapBOriginal.column) | 0);
                              if (cmp !== 0) {
                                  return cmp | 0;
                              }
                              else {
                                  return strcmp(mappingA.Name, mappingB.Name) | 0;
                              }
                          }
                      }
                      default:
                          return strcmp(mappingA.Name, mappingB.Name) | 0;
                  }
              }
          }
      }
  }

  function Combine(path1, path2) {
      return ((path1.length === 0) ? path1 : (trimEnd(path1, "\\", "/") + "/")) + trimStart(path2, "\\", "/");
  }

  function GetExtension(path) {
      const i = path.lastIndexOf(".") | 0;
      if (i < 0) {
          return "";
      }
      else {
          return substring(path, i);
      }
  }

  function normalizePath(path) {
      return replace(path, "\\", "/");
  }

  function getRelativeFileOrDirPath(fromIsDir, fromFullPath, toIsDir, toFullPath) {
      let path1, path2, c, d, builder;
      const addDummyFile = (isDir, path) => {
          if (isDir) {
              return Combine(path, "__DUMMY-FILE__.txt");
          }
          else {
              return path;
          }
      };
      const fromFullPath_1 = normalizePath(fromFullPath);
      const toFullPath_1 = normalizePath(toFullPath);
      const matchValue = replace((path1 = addDummyFile(fromIsDir, fromFullPath_1), (path2 = addDummyFile(toIsDir, toFullPath_1), (c = 0, (d = -1, ((() => {
          while (((c < path1.length) && (c < path2.length)) && (path1[c] === path2[c])) {
              if (path1[c] === "/") {
                  d = (c | 0);
              }
              c = ((c + 1) | 0);
          }
      })(), (c === 0) ? path2 : (((c === path1.length) && (c === path2.length)) ? "" : ((builder = "", ((() => {
          while (c < path1.length) {
              if (path1[c] === "/") {
                  builder = (builder + "../");
              }
              c = ((c + 1) | 0);
          }
      })(), ((builder.length === 0) && ((path2.length - 1) === d)) ? "./" : (builder + substring(path2, d + 1))))))))))), "__DUMMY-FILE__.txt", "");
      if (matchValue === "") {
          return ".";
      }
      else {
          return matchValue;
      }
  }

  function getRelativePath(fromFullPath, toFullPath) {
      const isDir = (arg) => isNullOrWhiteSpace(GetExtension(arg));
      return getRelativeFileOrDirPath(isDir(fromFullPath), fromFullPath, isDir(toFullPath), toFullPath);
  }

  class MappingComparer {
      constructor() {
      }
      Compare(a, b) {
          return compareByGeneratedPositionsInflated(a, b);
      }
  }

  function MappingComparer_$ctor() {
      return new MappingComparer();
  }

  class Dictionary {
      constructor(pairs, comparer) {
          const this$ = new FSharpRef(defaultOf());
          this.comparer = comparer;
          this$.contents = this;
          this.hashMap = (new Map([]));
          this["init@9"] = 1;
          const enumerator = getEnumerator(pairs);
          try {
              while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                  const pair = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                  Dictionary__Add_5BDDA1(this$.contents, pair[0], pair[1]);
              }
          }
          finally {
              disposeSafe(enumerator);
          }
      }
      get [Symbol.toStringTag]() {
          return "Dictionary";
      }
      toJSON() {
          const this$ = this;
          return Array.from(this$);
      }
      "System.Collections.IEnumerable.GetEnumerator"() {
          const this$ = this;
          return getEnumerator(this$);
      }
      GetEnumerator() {
          const this$ = this;
          return getEnumerator(concat(this$.hashMap.values()));
      }
      [Symbol.iterator]() {
          return toIterator(getEnumerator(this));
      }
      "System.Collections.Generic.ICollection`1.Add2B595"(item) {
          const this$ = this;
          Dictionary__Add_5BDDA1(this$, item[0], item[1]);
      }
      "System.Collections.Generic.ICollection`1.Clear"() {
          const this$ = this;
          Dictionary__Clear(this$);
      }
      "System.Collections.Generic.ICollection`1.Contains2B595"(item) {
          const this$ = this;
          const matchValue = Dictionary__TryFind_2B595(this$, item[0]);
          let matchResult;
          if (matchValue != null) {
              if (equals$1(value(matchValue)[1], item[1])) {
                  matchResult = 0;
                  value(matchValue);
              }
              else {
                  matchResult = 1;
              }
          }
          else {
              matchResult = 1;
          }
          switch (matchResult) {
              case 0:
                  return true;
              default:
                  return false;
          }
      }
      "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(array, arrayIndex) {
          const this$ = this;
          iterateIndexed((i, e) => {
              array[arrayIndex + i] = e;
          }, this$);
      }
      "System.Collections.Generic.ICollection`1.get_Count"() {
          const this$ = this;
          return Dictionary__get_Count(this$) | 0;
      }
      "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
          return false;
      }
      "System.Collections.Generic.ICollection`1.Remove2B595"(item) {
          const this$ = this;
          const matchValue = Dictionary__TryFind_2B595(this$, item[0]);
          if (matchValue != null) {
              if (equals$1(value(matchValue)[1], item[1])) {
                  Dictionary__Remove_2B595(this$, item[0]);
              }
              return true;
          }
          else {
              return false;
          }
      }
      "System.Collections.Generic.IDictionary`2.Add5BDDA1"(key, value) {
          const this$ = this;
          Dictionary__Add_5BDDA1(this$, key, value);
      }
      "System.Collections.Generic.IDictionary`2.ContainsKey2B595"(key) {
          const this$ = this;
          return Dictionary__ContainsKey_2B595(this$, key);
      }
      "System.Collections.Generic.IDictionary`2.get_Item2B595"(key) {
          const this$ = this;
          return Dictionary__get_Item_2B595(this$, key);
      }
      "System.Collections.Generic.IDictionary`2.set_Item5BDDA1"(key, v) {
          const this$ = this;
          Dictionary__set_Item_5BDDA1(this$, key, v);
      }
      "System.Collections.Generic.IDictionary`2.get_Keys"() {
          const this$ = this;
          return toArray$1(delay(() => map((pair) => pair[0], this$)));
      }
      "System.Collections.Generic.IDictionary`2.Remove2B595"(key) {
          const this$ = this;
          return Dictionary__Remove_2B595(this$, key);
      }
      "System.Collections.Generic.IDictionary`2.TryGetValue6DC89625"(key, value$1) {
          const this$ = this;
          const matchValue = Dictionary__TryFind_2B595(this$, key);
          if (matchValue != null) {
              const pair = value(matchValue);
              value$1.contents = pair[1];
              return true;
          }
          else {
              return false;
          }
      }
      "System.Collections.Generic.IDictionary`2.get_Values"() {
          const this$ = this;
          return toArray$1(delay(() => map((pair) => pair[1], this$)));
      }
      get size() {
          const this$ = this;
          return Dictionary__get_Count(this$) | 0;
      }
      clear() {
          const this$ = this;
          Dictionary__Clear(this$);
      }
      delete(k) {
          const this$ = this;
          return Dictionary__Remove_2B595(this$, k);
      }
      entries() {
          const this$ = this;
          return map((p) => [p[0], p[1]], this$);
      }
      get(k) {
          const this$ = this;
          return Dictionary__get_Item_2B595(this$, k);
      }
      has(k) {
          const this$ = this;
          return Dictionary__ContainsKey_2B595(this$, k);
      }
      keys() {
          const this$ = this;
          return map((p) => p[0], this$);
      }
      set(k, v) {
          const this$ = this;
          Dictionary__set_Item_5BDDA1(this$, k, v);
          return this$;
      }
      values() {
          const this$ = this;
          return map((p) => p[1], this$);
      }
      forEach(f, thisArg) {
          const this$ = this;
          iterate((p) => {
              f(p[1], p[0], this$);
          }, this$);
      }
  }
  function Dictionary__TryFindIndex_2B595(this$, k) {
      const h = this$.comparer.GetHashCode(k) | 0;
      let matchValue;
      let outArg = defaultOf();
      matchValue = [tryGetValue(this$.hashMap, h, new FSharpRef(() => outArg, (v) => {
              outArg = v;
          })), outArg];
      if (matchValue[0]) {
          return [true, h, matchValue[1].findIndex((pair) => this$.comparer.Equals(k, pair[0]))];
      }
      else {
          return [false, h, -1];
      }
  }
  function Dictionary__TryFind_2B595(this$, k) {
      const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
      let matchResult;
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              matchResult = 0;
          }
          else {
              matchResult = 1;
          }
      }
      else {
          matchResult = 1;
      }
      switch (matchResult) {
          case 0:
              return getItemFromDict(this$.hashMap, matchValue[1])[matchValue[2]];
          default:
              return void 0;
      }
  }
  function Dictionary__Clear(this$) {
      this$.hashMap.clear();
  }
  function Dictionary__get_Count(this$) {
      let count = 0;
      let enumerator = getEnumerator(this$.hashMap.values());
      try {
          while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
              const pairs = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
              count = ((count + pairs.length) | 0);
          }
      }
      finally {
          disposeSafe(enumerator);
      }
      return count | 0;
  }
  function Dictionary__get_Item_2B595(this$, k) {
      const matchValue = Dictionary__TryFind_2B595(this$, k);
      if (matchValue != null) {
          return value(matchValue)[1];
      }
      else {
          throw new Error("The item was not found in collection");
      }
  }
  function Dictionary__set_Item_5BDDA1(this$, k, v) {
      const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              getItemFromDict(this$.hashMap, matchValue[1])[matchValue[2]] = [k, v];
          }
          else {
              void (getItemFromDict(this$.hashMap, matchValue[1]).push([k, v]));
          }
      }
      else {
          this$.hashMap.set(matchValue[1], [[k, v]]);
      }
  }
  function Dictionary__Add_5BDDA1(this$, k, v) {
      const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              throw new Error(format("An item with the same key has already been added. Key: {0}", k));
          }
          else {
              void (getItemFromDict(this$.hashMap, matchValue[1]).push([k, v]));
          }
      }
      else {
          this$.hashMap.set(matchValue[1], [[k, v]]);
      }
  }
  function Dictionary__ContainsKey_2B595(this$, k) {
      const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
      let matchResult;
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              matchResult = 0;
          }
          else {
              matchResult = 1;
          }
      }
      else {
          matchResult = 1;
      }
      switch (matchResult) {
          case 0:
              return true;
          default:
              return false;
      }
  }
  function Dictionary__Remove_2B595(this$, k) {
      const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
      let matchResult;
      if (matchValue[0]) {
          if (matchValue[2] > -1) {
              matchResult = 0;
          }
          else {
              matchResult = 1;
          }
      }
      else {
          matchResult = 1;
      }
      switch (matchResult) {
          case 0: {
              getItemFromDict(this$.hashMap, matchValue[1]).splice(matchValue[2], 1);
              return true;
          }
          default:
              return false;
      }
  }

  class ArraySet$1 {
      constructor() {
          this._array = [];
          this._set = (new Dictionary([], {
              Equals: equals$1,
              GetHashCode: structuralHash,
          }));
      }
  }

  function ArraySet$1_$ctor() {
      return new ArraySet$1();
  }

  function ArraySet$1__Has_2B595(_, aStr) {
      return _._set.has(aStr);
  }

  function ArraySet$1__indexOf_2B595(_, aStr) {
      if (_._set.has(aStr)) {
          return getItemFromDict(_._set, aStr);
      }
      else {
          return void 0;
      }
  }

  function ArraySet$1__Add_Z1FE5A521(_, aStr, aAllowDuplicates) {
      const isDuplicate = contains$1(aStr, _._array, {
          Equals: equals$1,
          GetHashCode: structuralHash,
      });
      const idx = _._set.size | 0;
      if (!isDuplicate ? true : aAllowDuplicates) {
          void (_._array.push(aStr));
      }
      if (!isDuplicate) {
          _._set.set(aStr, idx);
      }
  }

  function ArraySet$1__ToArray(_) {
      return _._array;
  }

  function MappingListModule_generatedPositionAfter(mappingA, mappingB) {
      const lineA = mappingA.Generated.line | 0;
      const lineB = mappingB.Generated.line | 0;
      if ((lineB > lineA) ? true : ((lineB === lineA) && (mappingB.Generated.column >= mappingA.Generated.column))) {
          return true;
      }
      else {
          return compareByGeneratedPositionsInflated(mappingA, mappingB) <= 0;
      }
  }

  class MappingList {
      constructor() {
          this._array = [];
          this._sorted = true;
          this._last = (new Mapping(new MappingIndex(-1, 0), void 0, void 0, void 0));
      }
  }

  function MappingList_$ctor() {
      return new MappingList();
  }

  function MappingList__Add_Z3411214C(_, aMapping) {
      if (MappingListModule_generatedPositionAfter(_._last, aMapping)) {
          _._last = aMapping;
          void (_._array.push(aMapping));
      }
      else {
          _._sorted = false;
          void (_._array.push(aMapping));
      }
  }

  function MappingList__ToArray(_) {
      if (!_._sorted) {
          sortInPlace(_._array, MappingComparer_$ctor());
          _._sorted = true;
      }
      return _._array;
  }

  function makeRangeStepFunction(step, stop, zero, add) {
      const stepComparedWithZero = compare(step, zero) | 0;
      if (stepComparedWithZero === 0) {
          throw new Error("The step of a range cannot be zero");
      }
      const stepGreaterThanZero = stepComparedWithZero > 0;
      return (x) => {
          const comparedWithLast = compare(x, stop) | 0;
          return ((stepGreaterThanZero && (comparedWithLast <= 0)) ? true : (!stepGreaterThanZero && (comparedWithLast >= 0))) ? [x, add(x, step)] : void 0;
      };
  }
  function integralRangeStep(start, step, stop, zero, add) {
      const stepFn = makeRangeStepFunction(step, stop, zero, add);
      return delay(() => unfold(stepFn, start));
  }
  function rangeDouble(start, step, stop) {
      return integralRangeStep(start, step, stop, 0, (x, y) => (x + y));
  }

  var NumberStyles;
  (function (NumberStyles) {
      // None = 0x00000000,
      // AllowLeadingWhite = 0x00000001,
      // AllowTrailingWhite = 0x00000002,
      // AllowLeadingSign = 0x00000004,
      // AllowTrailingSign = 0x00000008,
      // AllowParentheses = 0x00000010,
      // AllowDecimalPoint = 0x00000020,
      // AllowThousands = 0x00000040,
      // AllowExponent = 0x00000080,
      // AllowCurrencySymbol = 0x00000100,
      NumberStyles[NumberStyles["AllowHexSpecifier"] = 512] = "AllowHexSpecifier";
      // Integer = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign,
      // HexNumber = AllowLeadingWhite | AllowTrailingWhite | AllowHexSpecifier,
      // Number = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign |
      //          AllowTrailingSign | AllowDecimalPoint | AllowThousands,
      // Float = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign |
      //         AllowDecimalPoint | AllowExponent,
      // Currency = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign | AllowTrailingSign |
      //            AllowParentheses | AllowDecimalPoint | AllowThousands | AllowCurrencySymbol,
      // Any = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign | AllowTrailingSign |
      //       AllowParentheses | AllowDecimalPoint | AllowThousands | AllowCurrencySymbol | AllowExponent,
  })(NumberStyles || (NumberStyles = {}));
  function op_UnaryNegation_Int32(x) {
      return x === -2147483648 ? x : -x;
  }

  class VlqException extends Exception {
      constructor() {
          super();
      }
  }

  function VlqException_$ctor() {
      return new VlqException();
  }

  function VlqException_$ctor_Z721C83C5(message) {
      VlqException_$ctor();
  }

  const Base64_intToCharMap = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".split("");

  function Base64_base64Encode(c) {
      if ((0 <= c) && (c < Base64_intToCharMap.length)) {
          return Base64_intToCharMap[c];
      }
      else {
          throw VlqException_$ctor_Z721C83C5("Must be between 0 and 63: " + int32ToString(c));
      }
  }

  function Base64Vlq_get_VLQ_BASE_SHIFT() {
      return 5;
  }

  function Base64Vlq_get_VLQ_BASE() {
      return 1 << Base64Vlq_get_VLQ_BASE_SHIFT();
  }

  function Base64Vlq_get_VLQ_BASE_MASK() {
      return Base64Vlq_get_VLQ_BASE() - 1;
  }

  function Base64Vlq_get_VLQ_CONTINUATION_BIT() {
      return Base64Vlq_get_VLQ_BASE();
  }

  function Base64Vlq_ToVLQSigned_Z524259A4(value) {
      if (value < 0) {
          return ((op_UnaryNegation_Int32(value) << 1) + 1) | 0;
      }
      else {
          return ((value << 1) + 0) | 0;
      }
  }

  function Base64Vlq_Encode_Z524259A4(number) {
      let encoded = StringBuilder_$ctor();
      let digit = 0;
      let vlq = Base64Vlq_ToVLQSigned_Z524259A4(number);
      let isZeroCase = number === 0;
      while ((vlq > 0) ? true : isZeroCase) {
          isZeroCase = false;
          digit = ((vlq & Base64Vlq_get_VLQ_BASE_MASK()) | 0);
          vlq = ((vlq >> Base64Vlq_get_VLQ_BASE_SHIFT()) | 0);
          if (vlq > 0) {
              digit = ((digit | Base64Vlq_get_VLQ_CONTINUATION_BIT()) | 0);
          }
          StringBuilder__Append_244C7CD6(encoded, Base64_base64Encode(digit));
      }
      return toString$1(encoded);
  }

  class SourceMapGenerator {
      constructor(skipValidation, file, sourceRoot) {
          this._file = file;
          this._sourceRoot = sourceRoot;
          this._skipValidation = defaultArg(skipValidation, false);
          this._names = ArraySet$1_$ctor();
          this._mappings = MappingList_$ctor();
          this["_sourcesContents@"] = (new Map([]));
          this["_sources@"] = ArraySet$1_$ctor();
      }
  }

  function SourceMapGenerator_$ctor_Z257FAB7(skipValidation, file, sourceRoot) {
      return new SourceMapGenerator(skipValidation, file, sourceRoot);
  }

  function SourceMapGenerator__get__sourcesContents(__) {
      return __["_sourcesContents@"];
  }

  function SourceMapGenerator__get__sources(__) {
      return __["_sources@"];
  }

  function SourceMapGenerator_get_version() {
      return 3;
  }

  function SourceMapGenerator_ValidateMapping_Z1908099D(generated, original, source, name) {
      let arg;
      let isInvalid = true;
      if (((((generated.line > 0) && (generated.column >= 0)) && (original == null)) && (source == null)) && (name == null)) {
          isInvalid = false;
      }
      else if ((((((generated.line > 0) && (generated.column >= 0)) && (original != null)) && (value(original).line > 0)) && (value(original).column >= 0)) && (source != null)) {
          isInvalid = false;
      }
      if (isInvalid) {
          throw new Error((arg = {
              generated: generated,
              name: unwrap(name),
              original: unwrap(original),
              source: unwrap(source),
          }, toText(printf("Invalid mapping: %A"))(arg)));
      }
  }

  function SourceMapGenerator__AddMapping_Z1908099D(this$, generated, original, source, name) {
      if (!this$._skipValidation) {
          SourceMapGenerator_ValidateMapping_Z1908099D(generated, original, source, name);
      }
      if (source != null) {
          if (!ArraySet$1__Has_2B595(SourceMapGenerator__get__sources(this$), value(source))) {
              ArraySet$1__Add_Z1FE5A521(SourceMapGenerator__get__sources(this$), value(source), false);
          }
      }
      if (name != null) {
          if (!ArraySet$1__Has_2B595(this$._names, value(name))) {
              ArraySet$1__Add_Z1FE5A521(this$._names, value(name), false);
          }
      }
      MappingList__Add_Z3411214C(this$._mappings, new Mapping(generated, original, source, name));
  }

  function SourceMapGenerator__GenerateSourcesContent_67C6C06C(this$, aSources, aSourceRoot) {
      return map((source_1) => {
          if (length(SourceMapGenerator__get__sourcesContents(this$).keys()) === 0) {
              return void 0;
          }
          else {
              let s = source_1;
              if (aSourceRoot != null) {
                  s = getRelativePath(value(this$._sourceRoot), source_1);
              }
              if (SourceMapGenerator__get__sourcesContents(this$).has(s)) {
                  return getItemFromDict(SourceMapGenerator__get__sourcesContents(this$), s);
              }
              else {
                  return void 0;
              }
          }
      }, Array.from(aSources));
  }

  function SourceMapGenerator__toJSON(this$) {
      const matchValue = SourceMapGenerator_get_version() | 0;
      const sources = ArraySet$1__ToArray(SourceMapGenerator__get__sources(this$));
      return new SourceGeneratorJSON(matchValue, sources, ArraySet$1__ToArray(this$._names), SourceMapGenerator__SerializeMappings(this$), this$._file, (length(SourceMapGenerator__get__sourcesContents(this$).keys()) > 0) ? SourceMapGenerator__GenerateSourcesContent_67C6C06C(this$, sources, this$._sourceRoot) : void 0, this$._sourceRoot);
  }

  function SourceMapGenerator__SerializeMappings(this$) {
      let objectArg;
      let previousGeneratedColumn = 0;
      let previousGeneratedLine = 1;
      let previousOriginalColumn = 0;
      let previousOriginalLine = 0;
      let previousName = 0;
      let previousSource = 0;
      const result = StringBuilder_$ctor();
      let next = StringBuilder_$ctor();
      let nameIdx = 0;
      let sourceIdx = 0;
      const mappings = MappingList__ToArray(this$._mappings);
      const enumerator = getEnumerator(toList(rangeDouble(0, 1, mappings.length - 1)));
      try {
          while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
              const i = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]() | 0;
              let shouldContinue = false;
              const mapping = mappings[i];
              next = StringBuilder_$ctor();
              if (mapping.Generated.line !== previousGeneratedLine) {
                  previousGeneratedColumn = 0;
                  while (mapping.Generated.line !== previousGeneratedLine) {
                      StringBuilder__Append_Z721C83C5(next, ";");
                      previousGeneratedLine = ((previousGeneratedLine + 1) | 0);
                  }
              }
              else if (i > 0) {
                  if (compareByGeneratedPositionsInflated(mapping, mappings[i - 1]) === 0) {
                      shouldContinue = true;
                  }
                  else {
                      StringBuilder__Append_Z721C83C5(next, ",");
                  }
              }
              if (!shouldContinue) {
                  StringBuilder__Append_Z721C83C5(next, Base64Vlq_Encode_Z524259A4(mapping.Generated.column - previousGeneratedColumn));
                  previousGeneratedColumn = (mapping.Generated.column | 0);
                  if (mapping.Source != null) {
                      iterate((indexOfMappingSource) => {
                          sourceIdx = (indexOfMappingSource | 0);
                          StringBuilder__Append_Z721C83C5(next, Base64Vlq_Encode_Z524259A4(sourceIdx - previousSource));
                          previousSource = (sourceIdx | 0);
                      }, toArray$3(bind((objectArg = SourceMapGenerator__get__sources(this$), (aStr) => ArraySet$1__indexOf_2B595(objectArg, aStr)), mapping.Source)));
                      iterate((original) => {
                          StringBuilder__Append_Z721C83C5(next, Base64Vlq_Encode_Z524259A4((original.line - 1) - previousOriginalLine));
                          previousOriginalLine = ((original.line - 1) | 0);
                          StringBuilder__Append_Z721C83C5(next, Base64Vlq_Encode_Z524259A4(original.column - previousOriginalColumn));
                          previousOriginalColumn = (original.column | 0);
                      }, toArray$3(mapping.Original));
                      if (mapping.Name != null) {
                          iterate((indexOfMappingName) => {
                              nameIdx = (indexOfMappingName | 0);
                              StringBuilder__Append_Z721C83C5(next, Base64Vlq_Encode_Z524259A4(nameIdx - previousName));
                              previousName = (nameIdx | 0);
                          }, toArray$3(bind((aStr_1) => ArraySet$1__indexOf_2B595(this$._names, aStr_1), mapping.Name)));
                      }
                  }
                  StringBuilder__Append_Z721C83C5(result, toString$1(next));
              }
          }
      }
      finally {
          disposeSafe(enumerator);
      }
      return toString$1(result);
  }

  function emptyContinuation(_x) {
      // NOP
  }
  const defaultCancellationToken = new CancellationToken();
  function start(computation, cancellationToken) {
      return startWithContinuations(computation, cancellationToken);
  }
  function startImmediate(computation, cancellationToken) {
      return start(computation, cancellationToken);
  }
  function startWithContinuations(computation, continuation, exceptionContinuation, cancellationContinuation, cancelToken) {
      if (typeof continuation !== "function") {
          cancelToken = continuation;
          continuation = undefined;
      }
      const trampoline = new Trampoline();
      computation({
          onSuccess: continuation ? continuation : emptyContinuation,
          onError: exceptionContinuation ? exceptionContinuation : emptyContinuation,
          onCancel: cancellationContinuation ? cancellationContinuation : emptyContinuation,
          cancelToken: cancelToken ? cancelToken : defaultCancellationToken,
          trampoline,
      });
  }

  function distinct(xs, comparer) {
      return delay(() => {
          const hashSet = new HashSet([], comparer);
          return filter((x) => addToSet(x, hashSet), xs);
      });
  }
  function except(itemsToExclude, xs, comparer) {
      return delay(() => {
          const hashSet = new HashSet(itemsToExclude, comparer);
          return filter((x) => addToSet(x, hashSet), xs);
      });
  }
  function Array_distinct(xs, comparer) {
      return toArray$1(distinct(xs, comparer));
  }

  class ReferenceType extends Union {
      constructor(tag, fields) {
          super();
          this.tag = tag;
          this.fields = fields;
      }
      cases() {
          return ["ProjectReference", "PackageReference"];
      }
  }

  function $007CRegex$007C_$007C(pattern, input) {
      const m = match(create(pattern), input);
      if (m != null) {
          return toList(delay(() => map((x) => (x || ""), m)));
      }
      else {
          return void 0;
      }
  }

  function getXmlWithoutComments(xml) {
      return replace$1(xml, "<!--[\\s\\S]*?-->", "");
  }

  function getXmlTagContents(tag, xml) {
      return map((m) => (m[1] || "").trim(), matches(create(toText(printf("<%s[^>]*>([^<]*)<\\/%s[^>]*>"))(tag)(tag)), xml));
  }

  function getXmlTagContentsFirstOrDefault(tag, defaultValue, xml) {
      return defaultArg(tryHead(getXmlTagContents(tag, xml)), defaultValue);
  }

  function getXmlTagAttributes1(tag, attr1, xml) {
      return map((m) => trimStart(trimStart(m[1] || "", "\""), "\'").trim(), matches(create(toText(printf("<%s\\s+[^>]*%s\\s*=\\s*(\"[^\"]*|\'[^\']*)"))(tag)(attr1)), xml));
  }

  function getXmlTagAttributes2(tag, attr1, attr2, xml) {
      return map((m) => [trimStart(trimStart(m[1] || "", "\""), "\'").trim(), trimStart(trimStart(m[2] || "", "\""), "\'").trim()], matches(create(toText(printf("<%s\\s+[^>]*%s\\s*=\\s*(\"[^\"]*|\'[^\']*)[^>]*%s\\s*=\\s*(\"[^\"]*|\'[^\']*)"))(tag)(attr1)(attr2)), xml));
  }

  function isSystemPackage(pkgName) {
      if (((((pkgName.indexOf("System.") === 0) ? true : (pkgName.indexOf("Microsoft.") === 0)) ? true : (pkgName.indexOf("runtime.") === 0)) ? true : (pkgName === "NETStandard.Library")) ? true : (pkgName === "FSharp.Core")) {
          return true;
      }
      else {
          return pkgName === "Fable.Core";
      }
  }

  function parsePackageSpec(nuspecPath) {
      return toArray$1(map((tupledArg) => (new ReferenceType(1, [tupledArg[0], tupledArg[1]])), getXmlTagAttributes2("dependency", "id", "version", getXmlWithoutComments(readAllText(nuspecPath)))));
  }

  function resolvePackage(pkgName, pkgVersion) {
      let projRef, dependencies, dllRef;
      if (!isSystemPackage(pkgName)) {
          const homePath = replace(getHomePath(), "\\", "/");
          const nugetPath = toText(printf(".nuget/packages/%s/%s"))(pkgName)(pkgVersion);
          const pkgPath = Path_Combine(homePath, nugetPath.toLowerCase());
          const libPath = Path_Combine(pkgPath, "lib");
          const fablePath = Path_Combine(pkgPath, "fable");
          const binaryPaths = getDirFiles(libPath, ".dll");
          const nuspecPaths = getDirFiles(pkgPath, ".nuspec");
          const fsprojPaths = getDirFiles(fablePath, ".fsproj");
          if (nuspecPaths.length === 0) {
              toConsole(printf("ERROR: Cannot find package %s"))(pkgPath);
          }
          const binaryOpt = tryLast(binaryPaths);
          const dependOpt = map$2(parsePackageSpec, tryLast(nuspecPaths));
          const fsprojOpt = map$2((Item) => (new ReferenceType(0, [Item])), tryLast(fsprojPaths));
          const patternInput = (fsprojOpt != null) ? ((projRef = fsprojOpt, [[projRef], []])) : ((binaryOpt != null) ? ((dependOpt != null) ? ((dependencies = dependOpt, (dllRef = binaryOpt, [dependencies, [dllRef]]))) : [[], []]) : [[], []]);
          return [patternInput[0], patternInput[1]];
      }
      else {
          return [[], []];
      }
  }

  function parseCompilerOptions(projectXml) {
      const target = getXmlTagContentsFirstOrDefault("OutputType", "", projectXml);
      const langVersion = getXmlTagContentsFirstOrDefault("LangVersion", "", projectXml);
      const warnLevel = getXmlTagContentsFirstOrDefault("WarningLevel", "", projectXml);
      const treatWarningsAsErrors = getXmlTagContentsFirstOrDefault("TreatWarningsAsErrors", "", projectXml);
      const defines = toArray$1(except(["$(DefineConstants)", ""], distinct(map((s_1) => s_1.trim(), append(["FABLE_COMPILER", "FABLE_COMPILER_4", "FABLE_COMPILER_JAVASCRIPT"], collect((s) => split(s, [";"], void 0, 0), getXmlTagContents("DefineConstants", projectXml)))), {
          Equals: (x, y) => (x === y),
          GetHashCode: stringHash,
      }), {
          Equals: (x_1, y_1) => (x_1 === y_1),
          GetHashCode: stringHash,
      }));
      const nowarns = toArray$1(except(["$(NoWarn)", ""], distinct(map((s_3) => s_3.trim(), collect((s_2) => split(s_2, [";"], void 0, 0), getXmlTagContents("NoWarn", projectXml))), {
          Equals: (x_2, y_2) => (x_2 === y_2),
          GetHashCode: stringHash,
      }), {
          Equals: (x_3, y_3) => (x_3 === y_3),
          GetHashCode: stringHash,
      }));
      const warnAsErrors = toArray$1(except(["$(WarningsAsErrors)", ""], distinct(map((s_5) => s_5.trim(), collect((s_4) => split(s_4, [";"], void 0, 0), getXmlTagContents("WarningsAsErrors", projectXml))), {
          Equals: (x_4, y_4) => (x_4 === y_4),
          GetHashCode: stringHash,
      }), {
          Equals: (x_5, y_5) => (x_5 === y_5),
          GetHashCode: stringHash,
      }));
      const otherFlags = toArray$1(except(["$(OtherFlags)", ""], distinct(map((s_7) => s_7.trim(), collect((s_6) => split(s_6, [" "], void 0, 0), getXmlTagContents("OtherFlags", projectXml))), {
          Equals: (x_6, y_6) => (x_6 === y_6),
          GetHashCode: stringHash,
      }), {
          Equals: (x_7, y_7) => (x_7 === y_7),
          GetHashCode: stringHash,
      }));
      return toArray$1(delay(() => append((target.length > 0) ? singleton$1("--target:" + target) : empty(), delay(() => append((langVersion.length > 0) ? singleton$1("--langversion:" + langVersion) : empty(), delay(() => append((warnLevel.length > 0) ? singleton$1("--warn:" + warnLevel) : empty(), delay(() => append((treatWarningsAsErrors === "true") ? singleton$1("--warnaserror+") : empty(), delay(() => append(map((d) => ("-d:" + d), defines), delay(() => append(map((n) => ("--nowarn:" + n), nowarns), delay(() => append(map((e) => ("--warnaserror:" + e), warnAsErrors), delay(() => map((o) => o, otherFlags)))))))))))))))));
  }

  function makeFullPath(projectFileDir, path) {
      let path_2;
      const path_1 = replace(path, "\\", "/");
      return normalizeFullPath(((path_2 = path_1, (path_2.indexOf("/") === 0) ? true : (path_2.indexOf(":") === 1))) ? path_1 : Path_Combine(projectFileDir, path_1));
  }

  function parseProjectScript(projectFilePath) {
      const projectXml = readAllText(projectFilePath);
      const projectDir = Path_GetDirectoryName(projectFilePath);
      const patternInput = fold$2((tupledArg, line) => {
          const dllRefs = tupledArg[0];
          const srcFiles = tupledArg[1];
          const matchValue = line.trim();
          let matchResult, path_1;
          const activePatternResult = $007CRegex$007C_$007C("^#r\\s+\"(.*?)\"$", matchValue);
          if (activePatternResult != null) {
              if (!isEmpty(activePatternResult)) {
                  if (!isEmpty(tail(activePatternResult))) {
                      if (isEmpty(tail(tail(activePatternResult)))) {
                          if (!endsWith(head(tail(activePatternResult)), "Fable.Core.dll")) {
                              matchResult = 0;
                              path_1 = head(tail(activePatternResult));
                          }
                          else {
                              matchResult = 1;
                          }
                      }
                      else {
                          matchResult = 1;
                      }
                  }
                  else {
                      matchResult = 1;
                  }
              }
              else {
                  matchResult = 1;
              }
          }
          else {
              matchResult = 1;
          }
          switch (matchResult) {
              case 0:
                  return [append$1([Path_Combine(projectDir, path_1)], dllRefs), srcFiles];
              default: {
                  let matchResult_1, path_2;
                  const activePatternResult_1 = $007CRegex$007C_$007C("^#load\\s+\"(.*?)\"$", matchValue);
                  if (activePatternResult_1 != null) {
                      if (!isEmpty(activePatternResult_1)) {
                          if (!isEmpty(tail(activePatternResult_1))) {
                              if (isEmpty(tail(tail(activePatternResult_1)))) {
                                  matchResult_1 = 0;
                                  path_2 = head(tail(activePatternResult_1));
                              }
                              else {
                                  matchResult_1 = 1;
                              }
                          }
                          else {
                              matchResult_1 = 1;
                          }
                      }
                      else {
                          matchResult_1 = 1;
                      }
                  }
                  else {
                      matchResult_1 = 1;
                  }
                  switch (matchResult_1) {
                      case 0:
                          return [dllRefs, append$1([Path_Combine(projectDir, path_2)], srcFiles)];
                      default:
                          return [dllRefs, srcFiles];
                  }
              }
          }
      }, [[], []], split(projectXml, ["\n"], void 0, 0));
      return [[], patternInput[0], append$1(patternInput[1], [Path_GetFileName(projectFilePath)]), ["--define:FABLE_COMPILER", "--define:FABLE_COMPILER_4", "--define:FABLE_COMPILER_JAVASCRIPT"]];
  }

  function parseProjectFile(projectFilePath) {
      const projectXml = getXmlWithoutComments(readAllText(projectFilePath));
      const projectDir = Path_GetDirectoryName(projectFilePath);
      const packageRefs = toArray$1(map((tupledArg) => (new ReferenceType(1, [tupledArg[0], tupledArg[1]])), getXmlTagAttributes2("PackageReference", "Include", "Version", projectXml)));
      const projectRefs = toArray$1(map((arg) => (new ReferenceType(0, [makeFullPath(projectDir, arg)])), getXmlTagAttributes1("ProjectReference", "Include", projectXml)));
      const projectXml_1 = replace(projectXml, "$(MSBuildProjectDirectory)", ".");
      const projectXml_2 = replace(projectXml_1, "$(FSharpSourcesRoot)", replace(getXmlTagContentsFirstOrDefault("FSharpSourcesRoot", "", projectXml_1), "\\", "/"));
      const projectXml_3 = replace(projectXml_2, "$(FsYaccOutputFolder)", replace(getXmlTagContentsFirstOrDefault("FsYaccOutputFolder", "", projectXml_2), "\\", "/"));
      const sourceFiles = toArray$1(collect(getGlobFiles, map((path_1) => makeFullPath(projectDir, path_1), getXmlTagAttributes1("Compile", "Include", projectXml_3))));
      return [append$1(projectRefs, packageRefs), [], sourceFiles, parseCompilerOptions(projectXml_3)];
  }

  function makeHashSetIgnoreCase() {
      return new HashSet([], {
          Equals(x, y) {
              return x.toLowerCase() === y.toLowerCase();
          },
          GetHashCode(x_1) {
              return stringHash(x_1.toLowerCase());
          },
      });
  }

  function dedupReferences(refSet, references) {
      const refName = (_arg) => {
          if (_arg.tag === 1) {
              return (_arg.fields[0] + ",") + _arg.fields[1];
          }
          else {
              return _arg.fields[0];
          }
      };
      const newRefs = references.filter((arg_1) => {
          let item;
          return !((item = refName(arg_1), refSet.has(item)));
      });
      unionWith(refSet, map$1(refName, newRefs));
      return newRefs;
  }

  function parseProject(projectFilePath) {
      const parseProject_1 = (refSet, projectRef) => {
          let patternInput_1;
          if (projectRef.tag === 1) {
              const patternInput = resolvePackage(projectRef.fields[0], projectRef.fields[1]);
              patternInput_1 = [patternInput[0], patternInput[1], [], []];
          }
          else {
              const path = projectRef.fields[0];
              patternInput_1 = (endsWith(path, ".fsx") ? parseProjectScript(path) : parseProjectFile(path));
          }
          let parseResult;
          const array = dedupReferences(refSet, patternInput_1[0]);
          parseResult = map$1(curry2(parseProject_1)(refSet), array);
          return [append$1(collect$1((tupledArg) => tupledArg[0], parseResult), patternInput_1[1]), append$1(collect$1((tupledArg_1) => tupledArg_1[1], parseResult), patternInput_1[2]), append$1(collect$1((tupledArg_2) => tupledArg_2[2], parseResult), patternInput_1[3])];
      };
      const patternInput_2 = parseProject_1(makeHashSetIgnoreCase(), new ReferenceType(0, [projectFilePath]));
      return [Array_distinct(patternInput_2[0], {
          Equals: (x_3, y) => (x_3 === y),
          GetHashCode: stringHash,
      }), Array_distinct(patternInput_2[1], {
          Equals: (x_4, y_1) => (x_4 === y_1),
          GetHashCode: stringHash,
      }), Array_distinct(patternInput_2[2], {
          Equals: (x_5, y_2) => (x_5 === y_2),
          GetHashCode: stringHash,
      })];
  }

  class MapTreeLeaf$2 {
      constructor(k, v) {
          this.k = k;
          this.v = v;
      }
  }
  function MapTreeLeaf$2_$ctor_5BDDA1(k, v) {
      return new MapTreeLeaf$2(k, v);
  }
  function MapTreeLeaf$2__get_Key(_) {
      return _.k;
  }
  function MapTreeLeaf$2__get_Value(_) {
      return _.v;
  }
  class MapTreeNode$2 extends MapTreeLeaf$2 {
      constructor(k, v, left, right, h) {
          super(k, v);
          this.left = left;
          this.right = right;
          this.h = (h | 0);
      }
  }
  function MapTreeNode$2_$ctor_Z39DE9543(k, v, left, right, h) {
      return new MapTreeNode$2(k, v, left, right, h);
  }
  function MapTreeNode$2__get_Left(_) {
      return _.left;
  }
  function MapTreeNode$2__get_Right(_) {
      return _.right;
  }
  function MapTreeNode$2__get_Height(_) {
      return _.h;
  }
  function MapTreeModule_empty() {
      return void 0;
  }
  function MapTreeModule_sizeAux(acc_mut, m_mut) {
      MapTreeModule_sizeAux: while (true) {
          const acc = acc_mut, m = m_mut;
          if (m != null) {
              const m2 = value(m);
              if (m2 instanceof MapTreeNode$2) {
                  const mn = m2;
                  acc_mut = MapTreeModule_sizeAux(acc + 1, MapTreeNode$2__get_Left(mn));
                  m_mut = MapTreeNode$2__get_Right(mn);
                  continue MapTreeModule_sizeAux;
              }
              else {
                  return (acc + 1) | 0;
              }
          }
          else {
              return acc | 0;
          }
      }
  }
  function MapTreeModule_size(x) {
      return MapTreeModule_sizeAux(0, x);
  }
  function MapTreeModule_mk(l, k, v, r) {
      let mn, mn_1;
      let hl;
      const m = l;
      if (m != null) {
          const m2 = value(m);
          hl = ((m2 instanceof MapTreeNode$2) ? ((mn = m2, MapTreeNode$2__get_Height(mn))) : 1);
      }
      else {
          hl = 0;
      }
      let hr;
      const m_1 = r;
      if (m_1 != null) {
          const m2_1 = value(m_1);
          hr = ((m2_1 instanceof MapTreeNode$2) ? ((mn_1 = m2_1, MapTreeNode$2__get_Height(mn_1))) : 1);
      }
      else {
          hr = 0;
      }
      const m_2 = ((hl < hr) ? hr : hl) | 0;
      if (m_2 === 0) {
          return MapTreeLeaf$2_$ctor_5BDDA1(k, v);
      }
      else {
          return MapTreeNode$2_$ctor_Z39DE9543(k, v, l, r, m_2 + 1);
      }
  }
  function MapTreeModule_rebalance(t1, k, v, t2) {
      let mn, mn_1, m_2, m2_2, mn_2, m_3, m2_3, mn_3;
      let t1h;
      const m = t1;
      if (m != null) {
          const m2 = value(m);
          t1h = ((m2 instanceof MapTreeNode$2) ? ((mn = m2, MapTreeNode$2__get_Height(mn))) : 1);
      }
      else {
          t1h = 0;
      }
      let t2h;
      const m_1 = t2;
      if (m_1 != null) {
          const m2_1 = value(m_1);
          t2h = ((m2_1 instanceof MapTreeNode$2) ? ((mn_1 = m2_1, MapTreeNode$2__get_Height(mn_1))) : 1);
      }
      else {
          t2h = 0;
      }
      if (t2h > (t1h + 2)) {
          const matchValue = value(t2);
          if (matchValue instanceof MapTreeNode$2) {
              const t2$0027 = matchValue;
              if (((m_2 = MapTreeNode$2__get_Left(t2$0027), (m_2 != null) ? ((m2_2 = value(m_2), (m2_2 instanceof MapTreeNode$2) ? ((mn_2 = m2_2, MapTreeNode$2__get_Height(mn_2))) : 1)) : 0)) > (t1h + 1)) {
                  const matchValue_1 = value(MapTreeNode$2__get_Left(t2$0027));
                  if (matchValue_1 instanceof MapTreeNode$2) {
                      const t2l = matchValue_1;
                      return MapTreeModule_mk(MapTreeModule_mk(t1, k, v, MapTreeNode$2__get_Left(t2l)), MapTreeLeaf$2__get_Key(t2l), MapTreeLeaf$2__get_Value(t2l), MapTreeModule_mk(MapTreeNode$2__get_Right(t2l), MapTreeLeaf$2__get_Key(t2$0027), MapTreeLeaf$2__get_Value(t2$0027), MapTreeNode$2__get_Right(t2$0027)));
                  }
                  else {
                      throw new Error("internal error: Map.rebalance");
                  }
              }
              else {
                  return MapTreeModule_mk(MapTreeModule_mk(t1, k, v, MapTreeNode$2__get_Left(t2$0027)), MapTreeLeaf$2__get_Key(t2$0027), MapTreeLeaf$2__get_Value(t2$0027), MapTreeNode$2__get_Right(t2$0027));
              }
          }
          else {
              throw new Error("internal error: Map.rebalance");
          }
      }
      else if (t1h > (t2h + 2)) {
          const matchValue_2 = value(t1);
          if (matchValue_2 instanceof MapTreeNode$2) {
              const t1$0027 = matchValue_2;
              if (((m_3 = MapTreeNode$2__get_Right(t1$0027), (m_3 != null) ? ((m2_3 = value(m_3), (m2_3 instanceof MapTreeNode$2) ? ((mn_3 = m2_3, MapTreeNode$2__get_Height(mn_3))) : 1)) : 0)) > (t2h + 1)) {
                  const matchValue_3 = value(MapTreeNode$2__get_Right(t1$0027));
                  if (matchValue_3 instanceof MapTreeNode$2) {
                      const t1r = matchValue_3;
                      return MapTreeModule_mk(MapTreeModule_mk(MapTreeNode$2__get_Left(t1$0027), MapTreeLeaf$2__get_Key(t1$0027), MapTreeLeaf$2__get_Value(t1$0027), MapTreeNode$2__get_Left(t1r)), MapTreeLeaf$2__get_Key(t1r), MapTreeLeaf$2__get_Value(t1r), MapTreeModule_mk(MapTreeNode$2__get_Right(t1r), k, v, t2));
                  }
                  else {
                      throw new Error("internal error: Map.rebalance");
                  }
              }
              else {
                  return MapTreeModule_mk(MapTreeNode$2__get_Left(t1$0027), MapTreeLeaf$2__get_Key(t1$0027), MapTreeLeaf$2__get_Value(t1$0027), MapTreeModule_mk(MapTreeNode$2__get_Right(t1$0027), k, v, t2));
              }
          }
          else {
              throw new Error("internal error: Map.rebalance");
          }
      }
      else {
          return MapTreeModule_mk(t1, k, v, t2);
      }
  }
  function MapTreeModule_add(comparer, k, v, m) {
      if (m != null) {
          const m2 = value(m);
          const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
          if (m2 instanceof MapTreeNode$2) {
              const mn = m2;
              if (c < 0) {
                  return MapTreeModule_rebalance(MapTreeModule_add(comparer, k, v, MapTreeNode$2__get_Left(mn)), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeNode$2__get_Right(mn));
              }
              else if (c === 0) {
                  return MapTreeNode$2_$ctor_Z39DE9543(k, v, MapTreeNode$2__get_Left(mn), MapTreeNode$2__get_Right(mn), MapTreeNode$2__get_Height(mn));
              }
              else {
                  return MapTreeModule_rebalance(MapTreeNode$2__get_Left(mn), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeModule_add(comparer, k, v, MapTreeNode$2__get_Right(mn)));
              }
          }
          else if (c < 0) {
              return MapTreeNode$2_$ctor_Z39DE9543(k, v, MapTreeModule_empty(), m, 2);
          }
          else if (c === 0) {
              return MapTreeLeaf$2_$ctor_5BDDA1(k, v);
          }
          else {
              return MapTreeNode$2_$ctor_Z39DE9543(k, v, m, MapTreeModule_empty(), 2);
          }
      }
      else {
          return MapTreeLeaf$2_$ctor_5BDDA1(k, v);
      }
  }
  function MapTreeModule_tryFind(comparer_mut, k_mut, m_mut) {
      MapTreeModule_tryFind: while (true) {
          const comparer = comparer_mut, k = k_mut, m = m_mut;
          if (m != null) {
              const m2 = value(m);
              const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
              if (c === 0) {
                  return some(MapTreeLeaf$2__get_Value(m2));
              }
              else if (m2 instanceof MapTreeNode$2) {
                  const mn = m2;
                  comparer_mut = comparer;
                  k_mut = k;
                  m_mut = ((c < 0) ? MapTreeNode$2__get_Left(mn) : MapTreeNode$2__get_Right(mn));
                  continue MapTreeModule_tryFind;
              }
              else {
                  return void 0;
              }
          }
          else {
              return void 0;
          }
      }
  }
  function MapTreeModule_find(comparer, k, m) {
      const matchValue = MapTreeModule_tryFind(comparer, k, m);
      if (matchValue == null) {
          throw new Error();
      }
      else {
          return value(matchValue);
      }
  }
  function MapTreeModule_mem(comparer_mut, k_mut, m_mut) {
      MapTreeModule_mem: while (true) {
          const comparer = comparer_mut, k = k_mut, m = m_mut;
          if (m != null) {
              const m2 = value(m);
              const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
              if (m2 instanceof MapTreeNode$2) {
                  const mn = m2;
                  if (c < 0) {
                      comparer_mut = comparer;
                      k_mut = k;
                      m_mut = MapTreeNode$2__get_Left(mn);
                      continue MapTreeModule_mem;
                  }
                  else if (c === 0) {
                      return true;
                  }
                  else {
                      comparer_mut = comparer;
                      k_mut = k;
                      m_mut = MapTreeNode$2__get_Right(mn);
                      continue MapTreeModule_mem;
                  }
              }
              else {
                  return c === 0;
              }
          }
          else {
              return false;
          }
      }
  }
  function MapTreeModule_iterOpt(f_mut, m_mut) {
      MapTreeModule_iterOpt: while (true) {
          const f = f_mut, m = m_mut;
          if (m != null) {
              const m2 = value(m);
              if (m2 instanceof MapTreeNode$2) {
                  const mn = m2;
                  MapTreeModule_iterOpt(f, MapTreeNode$2__get_Left(mn));
                  f(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn));
                  f_mut = f;
                  m_mut = MapTreeNode$2__get_Right(mn);
                  continue MapTreeModule_iterOpt;
              }
              else {
                  f(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2));
              }
          }
          break;
      }
  }
  function MapTreeModule_iter(f, m) {
      MapTreeModule_iterOpt(f, m);
  }
  function MapTreeModule_copyToArray(m, arr, i) {
      let j = i;
      MapTreeModule_iter((x, y) => {
          arr[j] = [x, y];
          j = ((j + 1) | 0);
      }, m);
  }
  function MapTreeModule_toArray(m) {
      const n = MapTreeModule_size(m) | 0;
      const res = fill(new Array(n), 0, n, [null, null]);
      MapTreeModule_copyToArray(m, res, 0);
      return res;
  }
  function MapTreeModule_ofList(comparer, l) {
      return fold$1((acc, tupledArg) => MapTreeModule_add(comparer, tupledArg[0], tupledArg[1], acc), MapTreeModule_empty(), l);
  }
  function MapTreeModule_mkFromEnumerator(comparer_mut, acc_mut, e_mut) {
      MapTreeModule_mkFromEnumerator: while (true) {
          const comparer = comparer_mut, acc = acc_mut, e = e_mut;
          if (e["System.Collections.IEnumerator.MoveNext"]()) {
              const patternInput = e["System.Collections.Generic.IEnumerator`1.get_Current"]();
              comparer_mut = comparer;
              acc_mut = MapTreeModule_add(comparer, patternInput[0], patternInput[1], acc);
              e_mut = e;
              continue MapTreeModule_mkFromEnumerator;
          }
          else {
              return acc;
          }
      }
  }
  function MapTreeModule_ofArray(comparer, arr) {
      let res = MapTreeModule_empty();
      for (let idx = 0; idx <= (arr.length - 1); idx++) {
          const forLoopVar = arr[idx];
          res = MapTreeModule_add(comparer, forLoopVar[0], forLoopVar[1], res);
      }
      return res;
  }
  function MapTreeModule_ofSeq(comparer, c) {
      if (isArrayLike(c)) {
          return MapTreeModule_ofArray(comparer, c);
      }
      else if (c instanceof FSharpList) {
          return MapTreeModule_ofList(comparer, c);
      }
      else {
          const ie = getEnumerator(c);
          try {
              return MapTreeModule_mkFromEnumerator(comparer, MapTreeModule_empty(), ie);
          }
          finally {
              disposeSafe(ie);
          }
      }
  }
  class MapTreeModule_MapIterator$2 extends Record {
      constructor(stack, started) {
          super();
          this.stack = stack;
          this.started = started;
      }
  }
  function MapTreeModule_collapseLHS(stack_mut) {
      MapTreeModule_collapseLHS: while (true) {
          const stack = stack_mut;
          if (!isEmpty(stack)) {
              const rest = tail(stack);
              const m = head(stack);
              if (m != null) {
                  const m2 = value(m);
                  if (m2 instanceof MapTreeNode$2) {
                      const mn = m2;
                      stack_mut = ofArrayWithTail([MapTreeNode$2__get_Left(mn), MapTreeLeaf$2_$ctor_5BDDA1(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn)), MapTreeNode$2__get_Right(mn)], rest);
                      continue MapTreeModule_collapseLHS;
                  }
                  else {
                      return stack;
                  }
              }
              else {
                  stack_mut = rest;
                  continue MapTreeModule_collapseLHS;
              }
          }
          else {
              return empty$1();
          }
      }
  }
  function MapTreeModule_mkIterator(m) {
      return new MapTreeModule_MapIterator$2(MapTreeModule_collapseLHS(singleton$2(m)), false);
  }
  function MapTreeModule_notStarted() {
      throw new Error("enumeration not started");
  }
  function MapTreeModule_alreadyFinished() {
      throw new Error("enumeration already finished");
  }
  function MapTreeModule_current(i) {
      if (i.started) {
          const matchValue = i.stack;
          if (!isEmpty(matchValue)) {
              if (head(matchValue) != null) {
                  const m = value(head(matchValue));
                  if (m instanceof MapTreeNode$2) {
                      throw new Error("Please report error: Map iterator, unexpected stack for current");
                  }
                  else {
                      return [MapTreeLeaf$2__get_Key(m), MapTreeLeaf$2__get_Value(m)];
                  }
              }
              else {
                  throw new Error("Please report error: Map iterator, unexpected stack for current");
              }
          }
          else {
              return MapTreeModule_alreadyFinished();
          }
      }
      else {
          return MapTreeModule_notStarted();
      }
  }
  function MapTreeModule_moveNext(i) {
      if (i.started) {
          const matchValue = i.stack;
          if (!isEmpty(matchValue)) {
              if (head(matchValue) != null) {
                  const m = value(head(matchValue));
                  if (m instanceof MapTreeNode$2) {
                      throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
                  }
                  else {
                      i.stack = MapTreeModule_collapseLHS(tail(matchValue));
                      return !isEmpty(i.stack);
                  }
              }
              else {
                  throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
              }
          }
          else {
              return false;
          }
      }
      else {
          i.started = true;
          return !isEmpty(i.stack);
      }
  }
  function MapTreeModule_mkIEnumerator(m) {
      let i = MapTreeModule_mkIterator(m);
      return {
          "System.Collections.Generic.IEnumerator`1.get_Current"() {
              return MapTreeModule_current(i);
          },
          "System.Collections.IEnumerator.get_Current"() {
              return MapTreeModule_current(i);
          },
          "System.Collections.IEnumerator.MoveNext"() {
              return MapTreeModule_moveNext(i);
          },
          "System.Collections.IEnumerator.Reset"() {
              i = MapTreeModule_mkIterator(m);
          },
          Dispose() {
          },
      };
  }
  class FSharpMap {
      constructor(comparer, tree) {
          this.comparer = comparer;
          this.tree = tree;
      }
      GetHashCode() {
          const this$ = this;
          return FSharpMap__ComputeHashCode(this$) | 0;
      }
      Equals(that) {
          const this$ = this;
          if (that instanceof FSharpMap) {
              const that_1 = that;
              const e1 = getEnumerator(this$);
              try {
                  const e2 = getEnumerator(that_1);
                  try {
                      const loop = () => {
                          const m1 = e1["System.Collections.IEnumerator.MoveNext"]();
                          if (m1 === e2["System.Collections.IEnumerator.MoveNext"]()) {
                              if (!m1) {
                                  return true;
                              }
                              else {
                                  const e1c = e1["System.Collections.Generic.IEnumerator`1.get_Current"]();
                                  const e2c = e2["System.Collections.Generic.IEnumerator`1.get_Current"]();
                                  if (equals$1(e1c[0], e2c[0]) && equals$1(e1c[1], e2c[1])) {
                                      return loop();
                                  }
                                  else {
                                      return false;
                                  }
                              }
                          }
                          else {
                              return false;
                          }
                      };
                      return loop();
                  }
                  finally {
                      disposeSafe(e2);
                  }
              }
              finally {
                  disposeSafe(e1);
              }
          }
          else {
              return false;
          }
      }
      toString() {
          const this$ = this;
          return ("map [" + join("; ", map((kv) => format("({0}, {1})", kv[0], kv[1]), this$))) + "]";
      }
      get [Symbol.toStringTag]() {
          return "FSharpMap";
      }
      toJSON() {
          const this$ = this;
          return Array.from(this$);
      }
      GetEnumerator() {
          const _ = this;
          return MapTreeModule_mkIEnumerator(_.tree);
      }
      [Symbol.iterator]() {
          return toIterator(getEnumerator(this));
      }
      "System.Collections.IEnumerable.GetEnumerator"() {
          const _ = this;
          return MapTreeModule_mkIEnumerator(_.tree);
      }
      CompareTo(obj) {
          const m = this;
          if (obj instanceof FSharpMap) {
              const m2 = obj;
              return compareWith((kvp1, kvp2) => {
                  const c = m.comparer.Compare(kvp1[0], kvp2[0]) | 0;
                  return ((c !== 0) ? c : compare(kvp1[1], kvp2[1])) | 0;
              }, m, m2) | 0;
          }
          else {
              throw new Error("not comparable\\nParameter name: obj");
          }
      }
      "System.Collections.Generic.ICollection`1.Add2B595"(x) {
          throw new Error("Map cannot be mutated");
      }
      "System.Collections.Generic.ICollection`1.Clear"() {
          throw new Error("Map cannot be mutated");
      }
      "System.Collections.Generic.ICollection`1.Remove2B595"(x) {
          throw new Error("Map cannot be mutated");
      }
      "System.Collections.Generic.ICollection`1.Contains2B595"(x) {
          const m = this;
          return FSharpMap__ContainsKey(m, x[0]) && equals$1(FSharpMap__get_Item(m, x[0]), x[1]);
      }
      "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(arr, i) {
          const m = this;
          MapTreeModule_copyToArray(m.tree, arr, i);
      }
      "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
          return true;
      }
      "System.Collections.Generic.ICollection`1.get_Count"() {
          const m = this;
          return FSharpMap__get_Count(m) | 0;
      }
      "System.Collections.Generic.IReadOnlyCollection`1.get_Count"() {
          const m = this;
          return FSharpMap__get_Count(m) | 0;
      }
      get size() {
          const m = this;
          return FSharpMap__get_Count(m) | 0;
      }
      clear() {
          throw new Error("Map cannot be mutated");
      }
      delete(_arg) {
          throw new Error("Map cannot be mutated");
      }
      entries() {
          const m = this;
          return map((p) => [p[0], p[1]], m);
      }
      get(k) {
          const m = this;
          return FSharpMap__get_Item(m, k);
      }
      has(k) {
          const m = this;
          return FSharpMap__ContainsKey(m, k);
      }
      keys() {
          const m = this;
          return map((p) => p[0], m);
      }
      set(k, v) {
          throw new Error("Map cannot be mutated");
      }
      values() {
          const m = this;
          return map((p) => p[1], m);
      }
      forEach(f, thisArg) {
          const m = this;
          iterate((p) => {
              f(p[1], p[0], m);
          }, m);
      }
  }
  function FSharpMap_$ctor(comparer, tree) {
      return new FSharpMap(comparer, tree);
  }
  function FSharpMap__get_Item(m, key) {
      return MapTreeModule_find(m.comparer, key, m.tree);
  }
  function FSharpMap__get_Count(m) {
      return MapTreeModule_size(m.tree);
  }
  function FSharpMap__ContainsKey(m, key) {
      return MapTreeModule_mem(m.comparer, key, m.tree);
  }
  function FSharpMap__TryFind(m, key) {
      return MapTreeModule_tryFind(m.comparer, key, m.tree);
  }
  function FSharpMap__ToArray(m) {
      return MapTreeModule_toArray(m.tree);
  }
  function FSharpMap__ComputeHashCode(this$) {
      const combineHash = (x, y) => (((x << 1) + y) + 631);
      let res = 0;
      const enumerator = getEnumerator(this$);
      try {
          while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
              const activePatternResult = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
              res = (combineHash(res, structuralHash(activePatternResult[0])) | 0);
              res = (combineHash(res, structuralHash(activePatternResult[1])) | 0);
          }
      }
      finally {
          disposeSafe(enumerator);
      }
      return res | 0;
  }
  function tryFind(key, table) {
      return FSharpMap__TryFind(table, key);
  }
  function ofSeq(elements, comparer) {
      return FSharpMap_$ctor(comparer, MapTreeModule_ofSeq(comparer, elements));
  }
  function toArray(table) {
      return FSharpMap__ToArray(table);
  }

  function tryParse(str, defValue) {
      if (str.match(/^\s*true\s*$/i)) {
          defValue.contents = true;
          return true;
      }
      else if (str.match(/^\s*false\s*$/i)) {
          defValue.contents = false;
          return true;
      }
      return false;
  }

  const getMetadataDir = fableMetadata.getAssembliesDir;

  const getFableLibDir = getFableLibDir$1;

  const getVersion = getVersion$1;

  const initFable = fableStandalone.init;

  const references = coreAssemblies;

  const metadataPath = trimEnd(getMetadataDir(), "\\", "/") + "/";

  function Imports_trimPath(path) {
      return replace(replace(replace(path, "../", ""), "./", ""), ":", "");
  }

  function Imports_isRelativePath(path) {
      if (path.indexOf("./") === 0) {
          return true;
      }
      else {
          return path.indexOf("../") === 0;
      }
  }

  function Imports_isAbsolutePath(path) {
      if (path.indexOf("/") === 0) {
          return true;
      }
      else {
          return path.indexOf(":") === 1;
      }
  }

  function Imports_preventConflicts(conflicts, originalName) {
      const check = (originalName_1_mut, n_mut) => {
          check:
          while (true) {
              const originalName_1 = originalName_1_mut, n = n_mut;
              const name = (n > 0) ? ((originalName_1 + "_") + int32ToString(n)) : originalName_1;
              if (!conflicts(name)) {
                  return name;
              }
              else {
                  originalName_1_mut = originalName_1;
                  n_mut = (n + 1);
                  continue check;
              }
          }
      };
      return check(originalName, 0);
  }

  function Imports_getTargetAbsolutePath(getOrAddDeduplicateTargetDir, importPath, projDir, outDir) {
      const importPath_1 = normalizePath$1(importPath);
      const outDir_1 = normalizePath$1(outDir);
      if (importPath_1.indexOf(outDir_1 + "/") === 0) {
          return importPath_1;
      }
      else {
          const importDir = Path_GetDirectoryName(importPath_1);
          return Path_Combine(getOrAddDeduplicateTargetDir(importDir, (currentTargetDirs) => Imports_preventConflicts((value) => FSharpSet__Contains(currentTargetDirs, value), Path_Combine(outDir_1, Imports_trimPath(getRelativePath$1(projDir, importDir))))), Path_GetFileName(importPath_1));
      }
  }

  function Imports_getTargetRelativePath(getOrAddDeduplicateTargetDir, importPath, targetDir, projDir, outDir) {
      const relPath = getRelativePath$1(targetDir, Imports_getTargetAbsolutePath(getOrAddDeduplicateTargetDir, importPath, projDir, outDir));
      if (Imports_isRelativePath(relPath)) {
          return relPath;
      }
      else {
          return "./" + relPath;
      }
  }

  function Imports_getImportPath(getOrAddDeduplicateTargetDir, sourcePath, targetPath, projDir, outDir, importPath) {
      if (outDir != null) {
          const outDir_1 = outDir;
          const importPath_1 = (importPath.indexOf("${outDir}") === 0) ? normalizeFullPath(Path_Combine(outDir_1, replace(importPath, "${outDir}", ""))) : importPath;
          const sourceDir = Path_GetDirectoryName(sourcePath);
          const targetDir = Path_GetDirectoryName(targetPath);
          const importPath_2 = Imports_isRelativePath(importPath_1) ? normalizeFullPath(Path_Combine(sourceDir, importPath_1)) : importPath_1;
          if (Imports_isAbsolutePath(importPath_2)) {
              if (endsWith(importPath_2, ".fs")) {
                  return Imports_getTargetRelativePath(getOrAddDeduplicateTargetDir, importPath_2, targetDir, projDir, outDir_1);
              }
              else {
                  return getRelativePath$1(targetDir, importPath_2);
              }
          }
          else {
              return importPath_2;
          }
      }
      else {
          return replace(importPath, "${outDir}", ".");
      }
  }

  class SourceWriter {
      constructor(sourcePath, targetPath, projDir, options, fileExt, dedupTargetDir) {
          this.sourcePath = sourcePath;
          this.targetPath = targetPath;
          this.projDir = projDir;
          this.options = options;
          this.dedupTargetDir = dedupTargetDir;
          this["fileExt@86"] = (endsWith(fileExt, ".ts") ? Path_ChangeExtension(fileExt, ".js") : fileExt);
          this.sb = StringBuilder_$ctor();
          this.mapGenerator = (new Lazy(SourceMapGenerator_$ctor_Z257FAB7));
      }
      Write(str) {
          const _ = this;
          return singleton.Delay(() => singleton.Return(void StringBuilder__Append_Z721C83C5(_.sb, str)));
      }
      MakeImportPath(path) {
          const _ = this;
          const path_1 = Imports_getImportPath(_.dedupTargetDir, _.sourcePath, _.targetPath, _.projDir, _.options.outDir, path);
          return endsWith(path_1, ".fs") ? Path_ChangeExtension(path_1, _["fileExt@86"]) : path_1;
      }
      AddSourceMapping(_arg) {
          const _ = this;
          if (_.options.sourceMaps) {
              const generated = new MappingIndex(_arg[2], _arg[3]);
              const original = new MappingIndex(_arg[0], _arg[1]);
              SourceMapGenerator__AddMapping_Z1908099D(_.mapGenerator.Value, generated, original, _.sourcePath, unwrap(_arg[4]));
          }
      }
      Dispose() {
      }
  }

  function SourceWriter_$reflection() {
      return class_type("Fable.Compiler.App.SourceWriter", void 0, SourceWriter);
  }

  function SourceWriter_$ctor_71349DF3(sourcePath, targetPath, projDir, options, fileExt, dedupTargetDir) {
      return new SourceWriter(sourcePath, targetPath, projDir, options, fileExt, dedupTargetDir);
  }

  function SourceWriter__get_SourceMap(_) {
      return SourceMapGenerator__toJSON(_.mapGenerator.Value);
  }

  function SourceWriter__get_Result(_) {
      return toString$1(_.sb);
  }

  function printErrors(showWarnings, errors) {
      const printError = (e) => {
          const errorType = e.IsWarning ? "Warning" : "Error";
          toConsole(printf("%s"))(`${e.FileName} (${e.StartLine},${e.StartColumn}): ${errorType}: ${e.Message}`);
      };
      const patternInput = partition((e_1) => e_1.IsWarning, errors);
      const errors_1 = patternInput[1];
      const hasErrors = !(errors_1.length === 0);
      if (showWarnings) {
          patternInput[0].forEach(printError);
      }
      if (hasErrors) {
          errors_1.forEach(printError);
          throw new Error("Too many errors.");
      }
  }

  function runAsync(computation) {
      startImmediate(singleton.Delay(() => singleton.TryWith(singleton.Delay(() => singleton.Bind(computation, () => singleton.Return(void 0))), (_arg_1) => {
          const e = _arg_1;
          const arg = e.message;
          toConsole(printf("[ERROR] %s"))(arg);
          const arg_1 = e.stack;
          toConsole(printf("%s"))(arg_1);
          return singleton.Zero();
      })));
  }

  function parseFiles(projectFileName, options) {
      const patternInput = parseProject(projectFileName);
      const fileNames = patternInput[1];
      const sources = map$1(readAllText, fileNames);
      const nugetPath = Path_Combine(replace(getHomePath(), "\\", "/"), ".nuget");
      const fileNames_1 = map$1((x) => replace(x, nugetPath, ""), fileNames);
      const dllRefMap = ofSeq(map$1((x_1) => [Path_GetFileName(x_1), x_1], reverse(patternInput[0])), {
          Compare: comparePrimitives,
      });
      const references_1 = append$1(references, map$1((tuple) => tuple[0], toArray(dllRefMap)));
      const fable = initFable();
      const otherOptions_1 = append$1(["--optimize" + (options.optimize ? "+" : "-")], patternInput[2]);
      const patternInput_1 = measureTime(() => fable.CreateChecker(references_1, (dllName_1) => {
          let dllName, value;
          return readAllBytes((dllName = dllName_1, (value = (metadataPath + dllName), defaultArg(tryFind(dllName, dllRefMap), value))));
      }, otherOptions_1), void 0);
      const checker = patternInput_1[0];
      const arg = getVersion();
      toConsole(printf("fable-compiler-js v%s"))(arg);
      toConsole(printf("--------------------------------------------"));
      toConsole(printf("InteractiveChecker created in %d ms"))(patternInput_1[1]);
      const patternInput_2 = measureTime(() => fable.ParseAndCheckProject(checker, projectFileName, fileNames_1, sources), void 0);
      const parseRes = patternInput_2[0];
      toConsole(printf("Project: %s, FCS time: %d ms"))(projectFileName)(patternInput_2[1]);
      toConsole(printf("--------------------------------------------"));
      const showWarnings = !options.benchmark;
      printErrors(showWarnings, parseRes.Errors);
      if (options.benchmark) ;
      else {
          fable.ClearCache(checker);
          const fileNames_2 = fileNames_1.filter((x_3) => !endsWith(x_3, ".fsi"));
          const projDir = Path_GetDirectoryName(normalizeFullPath(projectFileName));
          const libDir = normalizeFullPath(defaultArg(options.libDir, getFableLibDir()));
          let fileExt;
          const matchValue = options.language.toLowerCase();
          switch (matchValue) {
              case "js":
              case "javascript": {
                  fileExt = ".js";
                  break;
              }
              case "ts":
              case "typescript": {
                  fileExt = ".ts";
                  break;
              }
              case "py":
              case "python": {
                  fileExt = ".py";
                  break;
              }
              case "php": {
                  fileExt = ".php";
                  break;
              }
              case "dart": {
                  fileExt = ".dart";
                  break;
              }
              case "rust": {
                  fileExt = ".rs";
                  break;
              }
              default:
                  fileExt = toFail(printf("Unsupported language: %s"))(options.language);
          }
          const fileExt_1 = (options.outDir == null) ? (".fs" + fileExt) : fileExt;
          let getOrAddDeduplicateTargetDir;
          const dedupDic = new Map([]);
          getOrAddDeduplicateTargetDir = ((importDir) => ((addTargetDir) => {
              const importDir_1 = normalizeFullPath(importDir).toLocaleLowerCase();
              let matchValue_1;
              let outArg = defaultOf();
              matchValue_1 = [tryGetValue(dedupDic, importDir_1, new FSharpRef(() => outArg, (v) => {
                  outArg = v;
              })), outArg];
              if (matchValue_1[0]) {
                  return matchValue_1[1];
              }
              else {
                  const v_2 = addTargetDir(ofSeq$1(dedupDic.values(), {
                      Compare: comparePrimitives,
                  }));
                  addToDict(dedupDic, importDir_1, v_2);
                  return v_2;
              }
          }));
          runAsync(singleton.Delay(() => singleton.For(fileNames_2, (_arg) => {
              let fsAstStr;
              const fileName_1 = _arg;
              const patternInput_3 = measureTime((tupledArg) => fable.CompileToTargetAst(libDir, tupledArg[0], tupledArg[1], options.typedArrays, options.language), [parseRes, fileName_1]);
              const res_1 = patternInput_3[0];
              toConsole(printf("File: %s, Fable time: %d ms"))(fileName_1)(patternInput_3[1]);
              printErrors(showWarnings, res_1.FableErrors);
              let outPath;
              const matchValue_2 = options.outDir;
              outPath = ((matchValue_2 != null) ? Path_ChangeExtension(Imports_getTargetAbsolutePath(uncurry2(getOrAddDeduplicateTargetDir), fileName_1, projDir, matchValue_2), fileExt_1) : Path_ChangeExtension(fileName_1, fileExt_1));
              return singleton.Combine(options.printAst ? ((fsAstStr = fable.FSharpAstToString(parseRes, fileName_1), (writeAllText(substring(outPath, 0, outPath.lastIndexOf(fileExt_1)) + ".fs.ast", fsAstStr), singleton.Zero()))) : singleton.Zero(), singleton.Delay(() => {
                  const writer = SourceWriter_$ctor_71349DF3(fileName_1, outPath, projDir, options, fileExt_1, uncurry2(getOrAddDeduplicateTargetDir));
                  return singleton.Bind(fable.PrintTargetAst(res_1, writer), () => {
                      let mapPath, sourceMapUrl;
                      ensureDirExists(Path_GetDirectoryName(outPath));
                      return singleton.Combine(options.sourceMaps ? ((mapPath = (outPath + ".map"), (sourceMapUrl = ("//# sourceMappingURL=" + Path_GetFileName(mapPath)), singleton.Bind(writer.Write(sourceMapUrl), () => {
                          writeAllText(mapPath, serializeToJson(SourceWriter__get_SourceMap(writer)));
                          return singleton.Zero();
                      })))) : singleton.Zero(), singleton.Delay(() => {
                          writeAllText(outPath, SourceWriter__get_Result(writer));
                          return singleton.Zero();
                      }));
                  });
              }));
          })));
      }
  }

  function argValue(keys, args) {
      return map$2((tuple) => tuple[1], tryFindBack((tupledArg) => {
          if (!(tupledArg[1].indexOf("-") === 0)) {
              return contains(tupledArg[0], keys, {
                  Equals: (x, y) => (x === y),
                  GetHashCode: stringHash,
              });
          }
          else {
              return false;
          }
      }, pairwise(args)));
  }

  function tryFlag(flag, args) {
      const matchValue = argValue(singleton$2(flag), args);
      if (matchValue == null) {
          if (contains$1(flag, args, {
              Equals: (x, y) => (x === y),
              GetHashCode: stringHash,
          })) {
              return true;
          }
          else {
              return void 0;
          }
      }
      else {
          let matchValue_1;
          let outArg = false;
          matchValue_1 = [tryParse(matchValue, new FSharpRef(() => outArg, (v) => {
              outArg = v;
          })), outArg];
          if (matchValue_1[0]) {
              return matchValue_1[1];
          }
          else {
              return void 0;
          }
      }
  }

  function hasFlag(flag, args) {
      return defaultArg(tryFlag(flag, args), false);
  }

  function run(opts, projectFileName, outDir) {
      const commandToRun = map$2((i) => {
          const scriptFile = Path_Combine(defaultArg(outDir, "."), Path_GetFileNameWithoutExtension(projectFileName) + ".js");
          const runArgs = join(" ", opts.slice(i + 1, opts.length));
          return toText(printf("node %s %s"))(scriptFile)(runArgs);
      }, tryFindIndex$1((y) => ("--run" === y), opts));
      parseFiles(projectFileName, new CmdLineOptions(orElse(argValue(ofArray(["--outDir", "-o"]), opts), outDir), argValue(singleton$2("--fableLib"), opts), hasFlag("--benchmark", opts), hasFlag("--optimize", opts), hasFlag("--sourceMaps", opts) ? true : hasFlag("-s", opts), tryFlag("--typedArrays", opts), defaultArg(map$2((_arg) => "TypeScript", argValue(ofArray(["--language", "--lang"]), opts)), "JavaScript"), hasFlag("--printAst", opts)));
      iterate((cmd) => {
          runCmdAndExitIfFails(cmd);
      }, toArray$3(commandToRun));
  }

  function parseArguments(argv) {
      let testExpr, testExpr_1;
      const usage = "Usage: fable <F#_PROJECT_PATH> [OUT_DIR] [--options]\n\nOptions:\n  --help            Show help\n  --version         Print version\n  -o|--outDir       Redirect compilation output to a directory\n  -s|--sourceMaps   Enable source maps\n  --fableLib        Specify Fable library path\n  --typedArrays     Compile numeric arrays as JS typed arrays (default is true for JS, false for TS)\n  --run             Execute the script after compilation\n\n  --optimize        Compile with optimized F# AST (experimental)\n";
      let patternInput;
      const matchValue = tryFindIndex$1((s) => (s.indexOf("-") === 0), argv);
      patternInput = ((matchValue != null) ? splitAt(matchValue, argv) : [argv, []]);
      const opts = patternInput[1];
      const args = patternInput[0];
      if (hasFlag("--help", argv)) {
          toConsole(printf("%s"))(usage);
      }
      else if (hasFlag("--version", argv)) {
          const arg_1 = getVersion();
          toConsole(printf("v%s"))(arg_1);
      }
      else if ((testExpr = args, !equalsWith((x, y) => (x === y), testExpr, defaultOf()) && (testExpr.length === 1))) {
          run(opts, args[0], void 0);
      }
      else if ((testExpr_1 = args, !equalsWith((x_1, y_1) => (x_1 === y_1), testExpr_1, defaultOf()) && (testExpr_1.length === 2))) {
          run(opts, args[0], args[1]);
      }
      else {
          toConsole(printf("%s"))(usage);
      }
  }

  (function (argv) {
      try {
          parseArguments(argv);
      }
      catch (ex) {
          const arg = ex.message;
          const arg_1 = ex.stack;
          toConsole(printf("Error: %s\n%s"))(arg)(arg_1);
      }
      return 0;
  })(typeof process === 'object' ? process.argv.slice(2) : []);

  exports.Imports_getImportPath = Imports_getImportPath;
  exports.Imports_getTargetAbsolutePath = Imports_getTargetAbsolutePath;
  exports.Imports_getTargetRelativePath = Imports_getTargetRelativePath;
  exports.Imports_isAbsolutePath = Imports_isAbsolutePath;
  exports.Imports_isRelativePath = Imports_isRelativePath;
  exports.Imports_preventConflicts = Imports_preventConflicts;
  exports.Imports_trimPath = Imports_trimPath;
  exports.SourceWriter = SourceWriter;
  exports.SourceWriter_$ctor_71349DF3 = SourceWriter_$ctor_71349DF3;
  exports.SourceWriter_$reflection = SourceWriter_$reflection;
  exports.SourceWriter__get_Result = SourceWriter__get_Result;
  exports.SourceWriter__get_SourceMap = SourceWriter__get_SourceMap;
  exports.argValue = argValue;
  exports.getFableLibDir = getFableLibDir;
  exports.getMetadataDir = getMetadataDir;
  exports.getVersion = getVersion;
  exports.hasFlag = hasFlag;
  exports.initFable = initFable;
  exports.metadataPath = metadataPath;
  exports.parseArguments = parseArguments;
  exports.parseFiles = parseFiles;
  exports.printErrors = printErrors;
  exports.references = references;
  exports.run = run;
  exports.runAsync = runAsync;
  exports.tryFlag = tryFlag;

}));
