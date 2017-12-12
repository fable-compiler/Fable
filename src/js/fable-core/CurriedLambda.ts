export interface ICurriedLambda {
  curried?: boolean;
  (...args: any[]): any;
}

export default function CurriedLambda(f: ICurriedLambda, expectedArgsLength?: number): any {
  if (f.curried === true) {
    return f;
  }
  const curriedFn: ICurriedLambda = (...args: any[]) => {
    const args2 = args.map((x) => typeof x === "function" ? CurriedLambda(x) : x);
    const actualArgsLength = Math.max(args2.length, 1);
    expectedArgsLength = Math.max(expectedArgsLength || f.length, 1);
    if (actualArgsLength >= expectedArgsLength) {
      const restArgs = args2.splice(expectedArgsLength);
      const res = f(...args2);
      if (typeof res === "function") {
        const newLambda = CurriedLambda(res);
        return restArgs.length === 0 ? newLambda : newLambda(...restArgs);
      } else {
        return res;
      }
    } else {
      return CurriedLambda((...args3: any[]) => {
        return f(...args2.concat(args3));
      }, expectedArgsLength - actualArgsLength);
    }
  };
  curriedFn.curried = true;
  return curriedFn;
}
