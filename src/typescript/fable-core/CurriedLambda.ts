export default function CurriedLambda(arities: number[], f: Function, _this?: any): any {
  return function(): any {
    _this = _this || this;
    let args: any[] = [];
    for (let i = 0; i < arguments.length; i++) {
        args.push(arguments[i])
    }
    if (args.length >= arities[0]) {
      let restArgs = args.splice(arities[0]);
      let res = f.apply(_this, args);
      if (arities.length > 1) {
        let newLambda = CurriedLambda(arities.slice(1), res, _this);
        return restArgs.length === 0 ? newLambda : newLambda.apply(_this, restArgs);
      }
      else {
        return res;
      }
    }
    else {
      arities[0] -= args.length;
      return CurriedLambda(arities, function () {
        for (let i = 0; i < arguments.length; i++) {
          args.push(arguments[i]);
        }
        return f.apply(_this, args);
      }, _this);
    }
  }
}