import { fromNumber, fromBits } from "../Long";
import { map, initialize, replicate } from "../Seq";
export function pow32(x, n) {
  if (n === 0) {
    return 1;
  } else if (n % 2 === 0) {
    return pow32(x * x, ~~(n / 2));
  } else {
    return x * pow32(x * x, ~~(n / 2));
  }
}
export function leastBounding2Power(b) {
  const findBounding2Power = b_1 => tp => i => {
    if (b_1 <= tp) {
      return [tp, i];
    } else {
      return findBounding2Power(b_1)(tp * 2)(i + 1);
    }
  };

  return findBounding2Power(b)(1)(0);
}
export const k = 27;
export const m = 15;
export const g = 31;
export const w = 440564289;
export const primeP = fromBits(2013265921, 0, false);
export const maxBitsInsideFp = 30;
export const p = 2013265921;
export const p64 = fromBits(2013265921, 0, true);
export function toInt(x) {
  return ~~x;
}
export function ofInt32(x) {
  return x >>> 0;
}
export const mzero = 0;
export const mone = 1;
export const mtwo = 2;
export function mpow(x, n) {
  if (n === 0) {
    return mone;
  } else if (n % 2 === 0) {
    return mpow(fromNumber(x, true).mul(fromNumber(x, true)).mod(p64).toNumber() >>> 0, ~~(n / 2));
  } else {
    return fromNumber(x, true).mul(fromNumber(mpow(fromNumber(x, true).mul(fromNumber(x, true)).mod(p64).toNumber() >>> 0, ~~(n / 2)), true)).mod(p64).toNumber() >>> 0;
  }
}
export function mpowL(x, n) {
  if (n.Equals(fromBits(0, 0, false))) {
    return mone;
  } else if (n.mod(fromBits(2, 0, false)).Equals(fromBits(0, 0, false))) {
    return mpowL(fromNumber(x, true).mul(fromNumber(x, true)).mod(p64).toNumber() >>> 0, n.div(fromBits(2, 0, false)));
  } else {
    return fromNumber(x, true).mul(fromNumber(mpowL(fromNumber(x, true).mul(fromNumber(x, true)).mod(p64).toNumber() >>> 0, n.div(fromBits(2, 0, false))), true)).mod(p64).toNumber() >>> 0;
  }
}
export function m2PowNthRoot(n) {
  return mpow(w >>> 0, pow32(2, k - n));
}
export function minv(x) {
  return mpowL(x, primeP.sub(fromBits(2, 0, false)));
}
export function computeFFT(lambda, mu, n, w_1, u, res, offset) {
  if (n === 1) {
    res[offset] = u[mu];
  } else {
    const halfN = ~~(n / 2);
    const ww = fromNumber(w_1, true).mul(fromNumber(w_1, true)).mod(p64).toNumber() >>> 0;
    const offsetHalfN = offset + halfN;
    computeFFT(lambda * 2, mu, halfN, ww, u, res, offset);
    computeFFT(lambda * 2, lambda + mu, halfN, ww, u, res, offsetHalfN);
    let wj = mone;

    for (let j = 0; j <= halfN - 1; j++) {
      const even = res[offset + j];
      const odd = res[offsetHalfN + j];
      res[offset + j] = (even + (fromNumber(wj, true).mul(fromNumber(odd, true)).mod(p64).toNumber() >>> 0)) % p;
      res[offsetHalfN + j] = (even + p - (fromNumber(wj, true).mul(fromNumber(odd, true)).mod(p64).toNumber() >>> 0)) % p;
      wj = fromNumber(w_1, true).mul(fromNumber(wj, true)).mod(p64).toNumber() >>> 0;
    }
  }
}
export function computFftInPlace(n, w_1, u) {
  const lambda = 1;
  const mu = 0;
  const res = Uint32Array.from(replicate(n, mzero));
  const offset = 0;
  computeFFT(lambda, mu, n, w_1, u, res, offset);
  return res;
}
export function computeInverseFftInPlace(n, w_1, uT) {
  const bigKInv = minv(n >>> 0);
  return computFftInPlace(n, minv(w_1), uT).map(y => fromNumber(bigKInv, true).mul(fromNumber(y, true)).mod(p64).toNumber() >>> 0);
}
export const maxTwoPower = 29;
export const twoPowerTable = Int32Array.from(initialize(maxTwoPower - 1, i => pow32(2, i)));
export function computeFftPaddedPolynomialProduct(bigK, k_1, u, v) {
  const w_1 = m2PowNthRoot(k_1);
  const uT = computFftInPlace(bigK, w_1, u);
  const vT = computFftInPlace(bigK, w_1, v);
  const rT = Uint32Array.from(initialize(bigK, i => fromNumber(uT[i], true).mul(fromNumber(vT[i], true)).mod(p64).toNumber() >>> 0));
  const r = computeInverseFftInPlace(bigK, w_1, rT);
  return r;
}
export function padTo(n, u) {
  const uBound = u.length;
  return Uint32Array.from(initialize(n, i => {
    if (i < uBound) {
      return ofInt32(u[i]);
    } else {
      return mzero;
    }
  }));
}
export function computeFftPolynomialProduct(degu, u, degv, v) {
  const deguv = degu + degv;
  const bound = deguv + 1;
  const patternInput = leastBounding2Power(bound);
  const w_1 = m2PowNthRoot(patternInput[1]);
  const u_1 = padTo(patternInput[0], u);
  const v_1 = padTo(patternInput[0], v);
  const uT = computFftInPlace(patternInput[0], w_1, u_1);
  const vT = computFftInPlace(patternInput[0], w_1, v_1);
  const rT = Uint32Array.from(initialize(patternInput[0], i => fromNumber(uT[i], true).mul(fromNumber(vT[i], true)).mod(p64).toNumber() >>> 0));
  const r = computeInverseFftInPlace(patternInput[0], w_1, rT);
  return Int32Array.from(map(x => toInt(x), r));
}
export const maxFp = (p + p - mone) % p;