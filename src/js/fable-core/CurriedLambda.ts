export interface ICurriedLambda {
  curried?: boolean;
  (...args: any[]): any;
}

export default function CurriedLambda(f: ICurriedLambda, _this?: any, expectedArgsLength?: number): any {
  if (f.curried === true) {
    return f;
  }
  const curriedFn: ICurriedLambda = (...args: any[]) => {
    // _this = _this || this;
    const actualArgsLength = Math.max(args.length, 1);
    expectedArgsLength = Math.max(expectedArgsLength || f.length, 1);
    if (actualArgsLength >= expectedArgsLength) {
      const restArgs = args.splice(expectedArgsLength);
      const res = f.apply(_this, args);
      if (typeof res === "function") {
        const newLambda = CurriedLambda(res, _this);
        return restArgs.length === 0 ? newLambda : newLambda.apply(_this, restArgs);
      } else {
        return res;
      }
    } else {
      return CurriedLambda((...args2: any[]) => {
        return f.apply(_this, args.concat(args2));
      }, _this, expectedArgsLength - actualArgsLength);
    }
  };
  curriedFn.curried = true;
  return curriedFn;
}
