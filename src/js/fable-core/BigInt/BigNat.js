// import { setType } from "../Symbol";
// import _Symbol from "../Symbol";
// import { Array as FArray } from "../Util";
import { fromBits, fromNumber, toNumber, add as add_1, mul as mul_1, div as div_1, mod as mod_1, shl as shl_1, and as and_1 } from "../Long";
import { replicate, initialize } from "../Seq";
import { computeFftPaddedPolynomialProduct, toInt, mzero, ofInt32 as ofInt32_1, maxBitsInsideFp } from "./FFT";
import List from "../ListClass";
import { isNullOrEmpty, join } from "../String";

export default class BigNat {
  constructor(bound, digits) {
    this.bound = bound;
    this.digits = digits;
  }

  // [_Symbol.reflection]() {
  //   return {
  //     type: "Microsoft.FSharp.Math.BigNat",
  //     interfaces: ["FSharpRecord"],
  //     properties: {
  //       bound: "number",
  //       digits: FArray(Int32Array, true),
  //     },
  //   };
  // }

}
// setType("Microsoft.FSharp.Math.BigNat", BigNat);
export function bound(n) {
  return n.bound;
}
export function setBound(n, v) {
  n.bound = v;
}
export function coeff(n, i) {
  return n.digits[i];
}
export function coeff64(n, i) {
  return fromNumber(coeff(n, i), false);
}
export function setCoeff(n, i, v) {
  n.digits[i] = v;
}
export function pow64(x, n) {
  if (n === 0) {
    return fromBits(1, 0, false);
  } else if (n % 2 === 0) {
    return pow64(mul_1(x, x), ~~(n / 2));
  } else {
    return mul_1(x, pow64(mul_1(x, x), ~~(n / 2)));
  }
}
export function pow32(x, n) {
  if (n === 0) {
    return 1;
  } else if (n % 2 === 0) {
    return pow32(x * x, ~~(n / 2));
  } else {
    return x * pow32(x * x, ~~(n / 2));
  }
}
export function hash(n) {
  let res = 0;

  for (let i = 0; i <= n.bound - 1; i++) {
    res = n.digits[i] + (res << 3);
  }

  return res;
}
export function maxInt(a, b) {
  if (a < b) {
    return b;
  } else {
    return a;
  }
}
export function minInt(a, b) {
  if (a < b) {
    return a;
  } else {
    return b;
  }
}
export const baseBits = 24;
export const baseN = 16777216;
export const baseMask = 16777215;
export const baseNi64 = fromBits(16777216, 0, false);
export const baseMaski64 = fromBits(16777215, 0, false);
export const baseMaskU = fromBits(16777215, 0, true);
export const baseMask32A = 16777215;
export const baseMask32B = 255;
export const baseShift32B = 24;
export const baseMask64A = 16777215;
export const baseMask64B = 16777215;
export const baseMask64C = 65535;
export const baseShift64B = 24;
export const baseShift64C = 48;
export function divbase(x) {
  return ~~(x >>> 0 >> baseBits);
}
export function modbase(x) {
  return x & baseMask;
}
export function createN(b) {
  return new BigNat(b, new Int32Array(b));
}
export function copyN(x) {
  return new BigNat(x.bound, x.digits.slice());
}
export function normN(n) {
  const findLeastBound = na => i => {
    if (i === -1 ? true : na[i] !== 0) {
      return i + 1;
    } else {
      return findLeastBound(na)(i - 1);
    }
  };

  const bound_1 = findLeastBound(n.digits)(n.bound - 1);
  n.bound = bound_1;
  return n;
}
export const boundInt = 2;
export const boundInt64 = 3;
export const boundBase = 1;
export function embed(x) {
  const x_1 = x < 0 ? 0 : x;

  if (x_1 < baseN) {
    const r = createN(1);
    r.digits[0] = x_1;
    return normN(r);
  } else {
    const r = createN(boundInt);

    for (let i = 0; i <= boundInt - 1; i++) {
      r.digits[i] = ~~(x_1 / pow32(baseN, i)) % baseN;
    }

    return normN(r);
  }
}
export function embed64(x) {
  const x_1 = x.CompareTo(fromBits(0, 0, false)) < 0 ? fromBits(0, 0, false) : x;
  const r = createN(boundInt64);

  for (let i = 0; i <= boundInt64 - 1; i++) {
    r.digits[i] = ~~toNumber(mod_1(div_1(x_1, pow64(baseNi64, i)), baseNi64));
  }

  return normN(r);
}
export function eval32(n) {
  if (n.bound === 1) {
    return n.digits[0];
  } else {
    let acc = 0;

    for (let i = n.bound - 1; i >= 0; i--) {
      acc = n.digits[i] + baseN * acc;
    }

    return acc;
  }
}
export function eval64(n) {
  if (n.bound === 1) {
    return fromNumber(n.digits[0], false);
  } else {
    let acc = fromBits(0, 0, false);

    for (let i = n.bound - 1; i >= 0; i--) {
      acc = add_1(fromNumber(n.digits[i], false), mul_1(baseNi64, acc));
    }

    return acc;
  }
}
export const one = embed(1);
export const zero = embed(0);
export function restrictTo(d, n) {
  return new BigNat(minInt(d, n.bound), n.digits);
}
export function shiftUp(d, n) {
  const m = createN(n.bound + d);

  for (let i = 0; i <= n.bound - 1; i++) {
    m.digits[i + d] = n.digits[i];
  }

  return m;
}
export function shiftDown(d, n) {
  if (n.bound - d <= 0) {
    return zero;
  } else {
    const m = createN(n.bound - d);

    for (let i = 0; i <= m.bound - 1; i++) {
      m.digits[i] = n.digits[i + d];
    }

    return m;
  }
}
export function degree(n) {
  return n.bound - 1;
}
export function addP(i, n, c, p, q, r) {
  if (i < n) {
    const x = (i < p.bound ? p.digits[i] : 0) + (i < q.bound ? q.digits[i] : 0) + c;

    r.digits[i] = modbase(x);
    const c_1 = divbase(x);
    addP(i + 1, n, c_1, p, q, r);
  }
}
export function add(p, q) {
  const rbound = 1 + maxInt(p.bound, q.bound);
  const r = createN(rbound);
  const carry = 0;
  addP(0, rbound, carry, p, q, r);
  return normN(r);
}
export function subP(i, n, c, p, q, r) {
  if (i < n) {
    const x = (i < p.bound ? p.digits[i] : 0) - (i < q.bound ? q.digits[i] : 0) + c;

    if (x > 0) {
      r.digits[i] = modbase(x);
      const c_1 = divbase(x);
      return subP(i + 1, n, c_1, p, q, r);
    } else {
      const x_1 = x + baseN;
      r.digits[i] = modbase(x_1);
      const c_1 = divbase(x_1) - 1;
      return subP(i + 1, n, c_1, p, q, r);
    }
  } else {
    const underflow = c !== 0;
    return underflow;
  }
}
export function sub(p, q) {
  const rbound = maxInt(p.bound, q.bound);
  const r = createN(rbound);
  const carry = 0;
  const underflow = subP(0, rbound, carry, p, q, r);

  if (underflow) {
    return embed(0);
  } else {
    return normN(r);
  }
}
export function isZero(p) {
  return p.bound === 0;
}
export function IsZero(p) {
  return isZero(p);
}
export function isOne(p) {
  if (p.bound === 1) {
    return p.digits[0] === 1;
  } else {
    return false;
  }
}
export function equal(p, q) {
  if (p.bound === q.bound) {
    const check = pa => qa => i => {
      if (i === -1) {
        return true;
      } else if (pa[i] === qa[i]) {
        return check(pa)(qa)(i - 1);
      } else {
        return false;
      }
    };

    return check(p.digits)(q.digits)(p.bound - 1);
  } else {
    return false;
  }
}
export function shiftCompare(p, pn, q, qn) {
  if (p.bound + pn < q.bound + qn) {
    return -1;
  } else if (p.bound + pn > q.bound + pn) {
    return 1;
  } else {
    const check = pa => qa => i => {
      if (i === -1) {
        return 0;
      } else {
        const pai = i < pn ? 0 : pa[i - pn];
        const qai = i < qn ? 0 : qa[i - qn];

        if (pai === qai) {
          return check(pa)(qa)(i - 1);
        } else if (pai < qai) {
          return -1;
        } else {
          return 1;
        }
      }
    };

    return check(p.digits)(q.digits)(p.bound + pn - 1);
  }
}
export function compare(p, q) {
  if (p.bound < q.bound) {
    return -1;
  } else if (p.bound > q.bound) {
    return 1;
  } else {
    const check = pa => qa => i => {
      if (i === -1) {
        return 0;
      } else if (pa[i] === qa[i]) {
        return check(pa)(qa)(i - 1);
      } else if (pa[i] < qa[i]) {
        return -1;
      } else {
        return 1;
      }
    };

    return check(p.digits)(q.digits)(p.bound - 1);
  }
}
export function lt(p, q) {
  return compare(p, q) === -1;
}
export function gt(p, q) {
  return compare(p, q) === 1;
}
export function lte(p, q) {
  return compare(p, q) !== 1;
}
export function gte(p, q) {
  return compare(p, q) !== -1;
}
export function min(a, b) {
  if (lt(a, b)) {
    return a;
  } else {
    return b;
  }
}
export function max(a, b) {
  if (lt(a, b)) {
    return b;
  } else {
    return a;
  }
}
export function contributeArr(a, i, c) {
  const x = add_1(fromNumber(a[i], false), c);
  const c_1 = div_1(x, baseNi64);
  const x_1 = ~~toNumber(and_1(x, baseMaski64));
  a[i] = x_1;

  if (c_1.CompareTo(fromBits(0, 0, false)) > 0) {
    contributeArr(a, i + 1, c_1);
  }
}
export function scale(k, p) {
  const rbound = p.bound + boundInt;
  const r = createN(rbound);
  const k_1 = fromNumber(k, false);

  for (let i = 0; i <= p.bound - 1; i++) {
    const kpi = mul_1(k_1, fromNumber(p.digits[i], false));
    contributeArr(r.digits, i, kpi);
  }

  return normN(r);
}
export function mulSchoolBookBothSmall(p, q) {
  const r = createN(2);
  const rak = mul_1(fromNumber(p, false), fromNumber(q, false));
  setCoeff(r, 0, ~~toNumber(and_1(rak, baseMaski64)));
  setCoeff(r, 1, ~~toNumber(div_1(rak, baseNi64)));
  return normN(r);
}
export function mulSchoolBookCarry(r, c, k) {
  if (c.CompareTo(fromBits(0, 0, false)) > 0) {
    const rak = add_1(coeff64(r, k), c);
    setCoeff(r, k, ~~toNumber(and_1(rak, baseMaski64)));
    mulSchoolBookCarry(r, div_1(rak, baseNi64), k + 1);
  }
}
export function mulSchoolBookOneSmall(p, q) {
  const bp = bound(p);
  const rbound = bp + 1;
  const r = createN(rbound);
  const q_1 = fromNumber(q, false);
  let c = fromBits(0, 0, false);

  for (let i = 0; i <= bp - 1; i++) {
    const rak = add_1(add_1(c, coeff64(r, i)), mul_1(coeff64(p, i), q_1));
    setCoeff(r, i, ~~toNumber(and_1(rak, baseMaski64)));
    c = div_1(rak, baseNi64);
  }

  mulSchoolBookCarry(r, c, bp);
  return normN(r);
}
export function mulSchoolBookNeitherSmall(p, q) {
  const rbound = p.bound + q.bound;
  const r = createN(rbound);

  for (let i = 0; i <= p.bound - 1; i++) {
    const pai = fromNumber(p.digits[i], false);
    let c = fromBits(0, 0, false);
    let k = i;

    for (let j = 0; j <= q.bound - 1; j++) {
      const qaj = fromNumber(q.digits[j], false);
      const rak = add_1(add_1(fromNumber(r.digits[k], false), c), mul_1(pai, qaj));
      r.digits[k] = ~~toNumber(and_1(rak, baseMaski64));
      c = div_1(rak, baseNi64);
      k = k + 1;
    }

    mulSchoolBookCarry(r, c, k);
  }

  return normN(r);
}
export function mulSchoolBook(p, q) {
  const pSmall = bound(p) === 1;
  const qSmall = bound(q) === 1;

  if (pSmall ? qSmall : false) {
    return mulSchoolBookBothSmall(coeff(p, 0), coeff(q, 0));
  } else if (pSmall) {
    return mulSchoolBookOneSmall(q, coeff(p, 0));
  } else if (qSmall) {
    return mulSchoolBookOneSmall(p, coeff(q, 0));
  } else {
    return mulSchoolBookNeitherSmall(p, q);
  }
}
export class Encoding {
  constructor(bigL, twoToBigL, k, bigK, bigN, split, splits) {
    this.bigL = bigL;
    this.twoToBigL = twoToBigL;
    this.k = k;
    this.bigK = bigK;
    this.bigN = bigN;
    this.split = split;
    this.splits = splits;
  }

  // [_Symbol.reflection]() {
  //   return {
  //     type: "Microsoft.FSharp.Math.BigNatModule.Encoding",
  //     interfaces: ["FSharpRecord"],
  //     properties: {
  //       bigL: "number",
  //       twoToBigL: "number",
  //       k: "number",
  //       bigK: "number",
  //       bigN: "number",
  //       split: "number",
  //       splits: Int32Array,
  //     },
  //   };
  // }

}
// setType("Microsoft.FSharp.Math.BigNatModule.Encoding", Encoding);
export function mkEncoding(bigL, k, bigK, bigN) {
  return new Encoding(bigL, pow32(2, bigL), k, bigK, bigN, ~~(baseBits / bigL), Int32Array.from(initialize(~~(baseBits / bigL), i => pow32(2, bigL * i))));
}
export const table = [mkEncoding(1, 28, 268435456, 268435456), mkEncoding(2, 26, 67108864, 134217728), mkEncoding(3, 24, 16777216, 50331648), mkEncoding(4, 22, 4194304, 16777216), mkEncoding(5, 20, 1048576, 5242880), mkEncoding(6, 18, 262144, 1572864), mkEncoding(7, 16, 65536, 458752), mkEncoding(8, 14, 16384, 131072), mkEncoding(9, 12, 4096, 36864), mkEncoding(10, 10, 1024, 10240), mkEncoding(11, 8, 256, 2816), mkEncoding(12, 6, 64, 768), mkEncoding(13, 4, 16, 208)];
export function calculateTableTow(bigL) {
  const k = maxBitsInsideFp - 2 * bigL;
  const bigK = pow64(fromBits(2, 0, false), k);
  const N = mul_1(bigK, fromNumber(bigL, false));
  return [bigL, k, bigK, N];
}
export function encodingGivenResultBits(bitsRes) {
  const selectFrom = i => {
    if (i + 1 < table.length ? bitsRes < table[i + 1].bigN : false) {
      return selectFrom(i + 1);
    } else {
      return table[i];
    }
  };

  if (bitsRes >= table[0].bigN) {
    throw new Error("Product is huge, around 268435456 bits, beyond quickmul");
  } else {
    return selectFrom(0);
  }
}
export const bitmask = Int32Array.from(initialize(baseBits, i => pow32(2, i) - 1));
export const twopowers = Int32Array.from(initialize(baseBits, i => pow32(2, i)));
export const twopowersI64 = Array.from(initialize(baseBits, i => pow64(fromBits(2, 0, false), i)));
export function wordBits(word) {
  const hi = k => {
    if (k === 0) {
      return 0;
    } else if ((word & twopowers[k - 1]) !== 0) {
      return k;
    } else {
      return hi(k - 1);
    }
  };

  return hi(baseBits);
}
export function bits(u) {
  if (u.bound === 0) {
    return 0;
  } else {
    return degree(u) * baseBits + wordBits(u.digits[degree(u)]);
  }
}
export function extractBits(n, enc, bi) {
  const bj = bi + enc.bigL - 1;
  const biw = ~~(bi / baseBits);
  const bjw = ~~(bj / baseBits);

  if (biw !== bjw) {
    const x = biw < n.bound ? n.digits[biw] : 0;
    const y = bjw < n.bound ? n.digits[bjw] : 0;

    const xbit = bi % baseBits;
    const nxbits = baseBits - xbit;
    const x_1 = x >> xbit;
    const y_1 = y << nxbits;
    const x_2 = x_1 | y_1;
    const x_3 = x_2 & bitmask[enc.bigL];
    return x_3;
  } else {
    const x = biw < n.bound ? n.digits[biw] : 0;

    const xbit = bi % baseBits;
    const x_1 = x >> xbit;
    const x_2 = x_1 & bitmask[enc.bigL];
    return x_2;
  }
}
export function encodePoly(enc, n) {
  const poly = Uint32Array.from(replicate(enc.bigK, ofInt32_1(0)));
  const biMax = n.bound * baseBits;

  const encoder = i => bi => {
    if (i === enc.bigK ? true : bi > biMax) {} else {
      const pi = extractBits(n, enc, bi);
      poly[i] = ofInt32_1(pi);
      const i_1 = i + 1;
      const bi_1 = bi + enc.bigL;
      encoder(i_1)(bi_1);
    }
  };

  encoder(0)(0);
  return poly;
}
export function decodeResultBits(enc, poly) {
  let n = 0;

  for (let i = 0; i <= poly.length - 1; i++) {
    if (poly[i] !== mzero) {
      n = i;
    }
  }

  const rbits = maxBitsInsideFp + enc.bigL * n + 1;
  return rbits + 1;
}
export function decodePoly(enc, poly) {
  const rbound = ~~(decodeResultBits(enc, poly) / baseBits) + 1;
  const r = createN(rbound);

  const evaluate = i => j => d => {
    if (i === enc.bigK) {} else {
      if (j >= rbound) {} else {
        const x = mul_1(fromNumber(toInt(poly[i]), false), twopowersI64[d]);
        contributeArr(r.digits, j, x);
      }

      const i_1 = i + 1;
      const d_1 = d + enc.bigL;
      const patternInput = d_1 >= baseBits ? [j + 1, d_1 - baseBits] : [j, d_1];
      evaluate(i_1)(patternInput[0])(patternInput[1]);
    }
  };

  evaluate(0)(0)(0);
  return normN(r);
}
export function quickMulUsingFft(u, v) {
  const bitsRes = bits(u) + bits(v);
  const enc = encodingGivenResultBits(bitsRes);
  const upoly = encodePoly(enc, u);
  const vpoly = encodePoly(enc, v);
  const rpoly = computeFftPaddedPolynomialProduct(enc.bigK, enc.k, upoly, vpoly);
  const r = decodePoly(enc, rpoly);
  return normN(r);
}
export const minDigitsKaratsuba = 16;
export function recMulKaratsuba(mul, p, q) {
  const bp = p.bound;
  const bq = q.bound;
  const bmax = maxInt(bp, bq);

  if (bmax > minDigitsKaratsuba) {
    const k = ~~(bmax / 2);
    const a0 = restrictTo(k, p);
    const a1 = shiftDown(k, p);
    const b0 = restrictTo(k, q);
    const b1 = shiftDown(k, q);
    const q0 = mul(a0)(b0);
    const q1 = mul(add(a0, a1))(add(b0, b1));
    const q2 = mul(a1)(b1);
    const p1 = sub(q1, add(q0, q2));
    const r = add(q0, shiftUp(k, add(p1, shiftUp(k, q2))));
    return r;
  } else {
    return mulSchoolBook(p, q);
  }
}
export function mulKaratsuba(x, y) {
  return recMulKaratsuba(x_1 => y_1 => mulKaratsuba(x_1, y_1), x, y);
}
export const productDigitsUpperSchoolBook = ~~(64000 / baseBits);
export const singleDigitForceSchoolBook = ~~(32000 / baseBits);
export const productDigitsUpperFft = ~~(table[0].bigN / baseBits);
export function mul(p, q) {
  return mulSchoolBook(p, q);
}
export function scaleSubInPlace(x, f, a, n) {
  const invariant = tupledArg => {};

  const patternInput = [x.digits, degree(x)];
  const patternInput_1 = [a.digits, degree(a)];
  const f_1 = fromNumber(f, false);
  let j = 0;
  let z = mul_1(f_1, fromNumber(patternInput_1[0][0], false));

  while (z.CompareTo(fromBits(0, 0, false)) > 0 ? true : j < patternInput_1[1]) {
    if (j > patternInput[1]) {
      throw new Error("scaleSubInPlace: pre-condition did not apply, result would be -ve");
    }

    invariant([z, j, n]);
    let zLo = ~~toNumber(and_1(z, baseMaski64));
    let zHi = div_1(z, baseNi64);

    if (zLo <= patternInput[0][j + n]) {
      patternInput[0][j + n] = patternInput[0][j + n] - zLo;
    } else {
      patternInput[0][j + n] = patternInput[0][j + n] + (baseN - zLo);
      zHi = add_1(zHi, fromBits(1, 0, false));
    }

    if (j < patternInput_1[1]) {
      z = add_1(zHi, mul_1(f_1, fromNumber(patternInput_1[0][j + 1], false)));
    } else {
      z = zHi;
    }

    j = j + 1;
  }

  normN(x);
}
export function scaleSub(x, f, a, n) {
  const freshx = add(x, zero);
  scaleSubInPlace(freshx, f, a, n);
  return normN(freshx);
}
export function scaleAddInPlace(x, f, a, n) {
  const invariant = tupledArg => {};

  const patternInput = [x.digits, degree(x)];
  const patternInput_1 = [a.digits, degree(a)];
  const f_1 = fromNumber(f, false);
  let j = 0;
  let z = mul_1(f_1, fromNumber(patternInput_1[0][0], false));

  while (z.CompareTo(fromBits(0, 0, false)) > 0 ? true : j < patternInput_1[1]) {
    if (j > patternInput[1]) {
      throw new Error("scaleSubInPlace: pre-condition did not apply, result would be -ve");
    }

    invariant([z, j, n]);
    let zLo = ~~toNumber(and_1(z, baseMaski64));
    let zHi = div_1(z, baseNi64);

    if (zLo < baseN - patternInput[0][j + n]) {
      patternInput[0][j + n] = patternInput[0][j + n] + zLo;
    } else {
      patternInput[0][j + n] = zLo - (baseN - patternInput[0][j + n]);
      zHi = add_1(zHi, fromBits(1, 0, false));
    }

    if (j < patternInput_1[1]) {
      z = add_1(zHi, mul_1(f_1, fromNumber(patternInput_1[0][j + 1], false)));
    } else {
      z = zHi;
    }

    j = j + 1;
  }

  normN(x);
}
export function scaleAdd(x, f, a, n) {
  const freshx = add(x, zero);
  scaleAddInPlace(freshx, f, a, n);
  return normN(freshx);
}
export function removeFactor(x, a, n) {
  const patternInput = [degree(a), degree(x)];

  if (patternInput[1] < patternInput[0] + n) {
    return 0;
  } else {
    const patternInput_1 = [a.digits, x.digits];
    const f = patternInput[0] === 0
      ? patternInput[1] === n
        ? ~~(patternInput_1[1][n] / patternInput_1[0][0])
        : toNumber(div_1(add_1(mul_1(fromNumber(patternInput_1[1][patternInput[1]], false), baseNi64), fromNumber(patternInput_1[1][patternInput[1] - 1], false)), fromNumber(patternInput_1[0][0], false)))
      : patternInput[1] === patternInput[0] + n
        ? ~~(patternInput_1[1][patternInput[1]] / (patternInput_1[0][patternInput[0]] + 1))
        : toNumber(div_1(add_1(mul_1(fromNumber(patternInput_1[1][patternInput[1]], false), baseNi64), fromNumber(patternInput_1[1][patternInput[1] - 1], false)), add_1(fromNumber(patternInput_1[0][patternInput[0]], false), fromBits(1, 0, false))));

    if (f === 0) {
      const lte_1 = shiftCompare(a, n, x, 0) !== 1;

      if (lte_1) {
        return 1;
      } else {
        return 0;
      }
    } else {
      return f;
    }
  }
}
export function divmod(b, a) {
  if (isZero(a)) {
    throw new Error();
  } else if (degree(b) < degree(a)) {
    return [zero, b];
  } else {
    const x = copyN(b);
    const d = createN(degree(b) - degree(a) + 1 + 1);
    let p = degree(b);
    const m = degree(a);
    let n = p - m;

    const Invariant = tupledArg => {};

    let finished = false;

    while (!finished) {
      Invariant([d, x, n, p]);
      const f = removeFactor(x, a, n);

      if (f > 0) {
        scaleSubInPlace(x, f, a, n);
        scaleAddInPlace(d, f, one, n);
        Invariant([d, x, n, p]);
      } else {
        if (f === 0) {
          finished = n === 0;
        } else {
          finished = false;
        }

        if (!finished) {
          if (p === m + n) {
            Invariant([d, x, n - 1, p]);
            n = n - 1;
          } else {
            Invariant([d, x, n - 1, p - 1]);
            n = n - 1;
            p = p - 1;
          }
        }
      }
    }

    return [normN(d), normN(x)];
  }
}
export function div(b, a) {
  return divmod(b, a)[0];
}
export function rem(b, a) {
  return divmod(b, a)[1];
}
export function bitAnd(a, b) {
  const rbound = minInt(a.bound, b.bound);
  const r = createN(rbound);

  for (let i = 0; i <= r.bound - 1; i++) {
    r.digits[i] = a.digits[i] & b.digits[i];
  }

  return normN(r);
}
export function bitOr(a, b) {
  const rbound = maxInt(a.bound, b.bound);
  const r = createN(rbound);

  for (let i = 0; i <= a.bound - 1; i++) {
    r.digits[i] = r.digits[i] | a.digits[i];
  }

  for (let i = 0; i <= b.bound - 1; i++) {
    r.digits[i] = r.digits[i] | b.digits[i];
  }

  return normN(r);
}
export function hcf(a, b) {
  const hcfloop = a_1 => b_1 => {
    if (equal(zero, a_1)) {
      return b_1;
    } else {
      const patternInput = divmod(b_1, a_1);
      return hcfloop(patternInput[1])(a_1);
    }
  };

  if (lt(a, b)) {
    return hcfloop(a)(b);
  } else {
    return hcfloop(b)(a);
  }
}
export const two = embed(2);
export function powi(x, n) {
  const power = acc => x_1 => n_1 => {
    if (n_1 === 0) {
      return acc;
    } else if (n_1 % 2 === 0) {
      return power(acc)(mul(x_1, x_1))(~~(n_1 / 2));
    } else {
      return power(mul(x_1, acc))(mul(x_1, x_1))(~~(n_1 / 2));
    }
  };

  return power(one)(x)(n);
}
export function pow(x, n) {
  const power = acc => x_1 => n_1 => {
    if (isZero(n_1)) {
      return acc;
    } else {
      const patternInput = divmod(n_1, two);

      if (isZero(patternInput[1])) {
        return power(acc)(mul(x_1, x_1))(patternInput[0]);
      } else {
        return power(mul(x_1, acc))(mul(x_1, x_1))(patternInput[0]);
      }
    }
  };

  return power(one)(x)(n);
}
export function toFloat(n) {
  const basef = baseN;

  const evalFloat = acc => k => i => {
    if (i === n.bound) {
      return acc;
    } else {
      return evalFloat(acc + k * n.digits[i])(k * basef)(i + 1);
    }
  };

  return evalFloat(0)(1)(0);
}
export function ofInt32(n) {
  return embed(n);
}
export function ofInt64(n) {
  return embed64(n);
}
export function toUInt32(n) {
  let $var15 = null;

  switch (n.bound) {
    case 0:
      $var15 = 0;
      break;

    case 1:
      $var15 = n.digits[0] >>> 0;
      break;

    case 2:
      if (n.digits[1] > baseMask32B) {
        throw new Error();
      }

      $var15 = ((n.digits[0] & baseMask32A) >>> 0) + ((n.digits[1] & baseMask32B) >>> 0 << baseShift32B);
      break;

    default:
      throw new Error();
  }

  return $var15;
}
export function toUInt64(n) {
  let $var16 = null;

  switch (n.bound) {
    case 0:
        $var16 = fromBits(0, 0, true);
        break;

    case 1:
        $var16 = fromNumber(n.digits[0], true);
        break;

    case 2:
      $var16 = add_1(fromNumber(n.digits[0] & baseMask64A, true), shl_1(fromNumber(n.digits[1] & baseMask64B, true), baseShift64B));
      break;

    case 3:
      if (n.digits[2] > baseMask64C) {
        throw new Error();
      }
      $var16 = add_1(add_1(fromNumber(n.digits[0] & baseMask64A, true), shl_1(fromNumber(n.digits[1] & baseMask64B, true), baseShift64B)), shl_1(fromNumber(n.digits[2] & baseMask64C, true), baseShift64C));
      break;

    default:
        throw new Error();
  }

  return $var16;
}
export function toString(n) {
  const degn = degree(n);

  const route = prior => k => ten2k => {
    if (degree(ten2k) > degn) {
      return new List([k, ten2k], prior);
    } else {
      return route(new List([k, ten2k], prior))(k + 1)(mul(ten2k, ten2k));
    }
  };

  const kten2ks = route(new List())(0)(embed(10));

  const collect = isLeading => digits => n_1 => _arg1 => {
    if (_arg1.tail != null) {
      const ten2k = _arg1.head[1];
      const patternInput = divmod(n_1, ten2k);

      if (isLeading ? isZero(patternInput[0]) : false) {
        const digits_1 = collect(isLeading)(digits)(patternInput[1])(_arg1.tail);
        return digits_1;
      } else {
        const digits_1 = collect(false)(digits)(patternInput[1])(_arg1.tail);
        const digits_2 = collect(isLeading)(digits_1)(patternInput[0])(_arg1.tail);
        return digits_2;
      }
    } else {
      const n_2 = eval32(n_1);

      if (isLeading ? n_2 === 0 : false) {
        return digits;
      } else {
        return new List(String(n_2), digits);
      }
    }
  };

  const digits = collect(true)(new List())(n)(kten2ks);

  if (digits.tail == null) {
    return "0";
  } else {
    return join("", ...Array.from(digits));
  }
}
export function ofString(str) {
  const len = str.length;

  if (isNullOrEmpty(str)) {
    throw new Error("empty string" + "\nParameter name: " + "str");
  }

  const ten = embed(10);

  const build = acc => i => {
    if (i === len) {
      return acc;
    } else {
      const c = str[i];
      const d = c.charCodeAt(0) - "0".charCodeAt(0);

      if (0 <= d ? d <= 9 : false) {
        return build(add(mul(ten, acc), embed(d)))(i + 1);
      } else {
        throw new Error();
      }
    }
  };

  return build(embed(0))(0);
}
export function isSmall(n) {
  return n.bound <= 1;
}
export function getSmall(n) {
  if (0 < n.bound) {
    return n.digits[0];
  } else {
    return 0;
  }
}
export function factorial(n) {
  const productR = a => b => {
    if (equal(a, b)) {
      return a;
    } else {
      const m = div(add(a, b), ofInt32(2));
      return mul(productR(a)(m), productR(add(m, one))(b));
    }
  };

  return productR(one)(n);
}
