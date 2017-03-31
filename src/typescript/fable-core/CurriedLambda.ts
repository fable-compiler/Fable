export default function CurriedLambda(f: Function, _this?: any, expectedArgsLength?: number): any {
  return function(): any {
    _this = _this || this;
    let args: any[] = [];
    expectedArgsLength = expectedArgsLength || f.length;
    for (let i = 0; i < arguments.length; i++) {
        args.push(arguments[i])
    }
    if (args.length >= expectedArgsLength) {
      let restArgs = args.splice(expectedArgsLength);
      let res = f.apply(_this, args);
      if (typeof res === "function") {
        let newLambda = CurriedLambda(res, _this);
        return restArgs.length === 0 ? newLambda : newLambda.apply(_this, restArgs);
      }
      else {
        return res;
      }
    }
    else {
      return CurriedLambda(function () {
        for (let i = 0; i < arguments.length; i++) {
          args.push(arguments[i]);
        }
        return f.apply(_this, args);
      }, _this, expectedArgsLength - args.length);
    }
  }
}