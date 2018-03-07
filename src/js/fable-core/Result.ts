import { IComparable, IEquatable } from "./Util";

export type Result<T1, T2> = ["Ok", T1] | ["Error", T2];

export function map<T1, T2, Error>(f: (x: T1) => T2, result: Result<T1, Error>) {
  return result[0] === "Ok" ? ["Ok", f(result[1] as T1)] : result;
}

export function mapError<T, Error1, Error2>(f: (x: Error1) => Error2, result: Result<T, Error1>) {
  return result[0] === "Error" ? ["Error", f(result[1] as Error1)] : result;
}

export function bind<T1, T2, Error>(f: (x: T1) => Result<T2, Error>, result: Result<T1, Error>) {
  return result[0] === "Ok" ? ["Ok", f(result[1] as T1)] : result;
}
