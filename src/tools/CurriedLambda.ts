// This code is not being used yet but we may need it in the future
// to create curried lambdas dynamically

function getPartialArgs(argsObj: IArguments, partialArgCount: number, totalArgCount: number) {
  let ar = Array(totalArgCount);
  for (let i=0; i < partialArgCount; i++) {
    ar[0] = argsObj[i];
  }
  return ar;
}

function getRestArgs(argsObj: IArguments, totalArgCount: number) {
  let ar = Array(argsObj.length - totalArgCount);
  for (let i = totalArgCount; i < argsObj.length; i++) {
    ar[0] = argsObj[i];
  }
  return ar;
}

export function CurriedLambda(totalArgCount: number, f: Function, _this?: any) {
  return function(): any {
    _this = _this === void 0 ? this : _this;
    const actualArgCount = Math.max(arguments.length, 1);
    if (actualArgCount === totalArgCount) {
      return f.apply(_this, arguments);
    }
    else if (actualArgCount < totalArgCount) {
      const argDiff = totalArgCount - actualArgCount;
      let args = getPartialArgs(arguments, actualArgCount, totalArgCount);
      return argDiff === 1
        ? function (x: any) {
          args[actualArgCount] = x;
          return f.apply(_this, args);
        }
        : CurriedLambda(argDiff, function () {
          for (let i = 0; i < arguments.length; i++)
            args[actualArgCount + i] = arguments[i];
          return f.apply(_this, args);
        })
    }
    else {
      const res: Function = f.apply(_this, getPartialArgs(arguments, totalArgCount, totalArgCount));
      return res.apply(_this, getRestArgs(arguments, totalArgCount))
    }
  }
}
