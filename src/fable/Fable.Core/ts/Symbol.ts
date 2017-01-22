declare var global: any;

const fableGlobal: {
  types: Map<string, FunctionConstructor>,
  symbols: {
    reflection: symbol,
    generics: symbol,
  }
} = function () {
  const globalObj =
    typeof window !== "undefined" ? window
    : (typeof global !== "undefined" ? global
    : (typeof self !== "undefined" ? self : {}));
  if (typeof globalObj.__FABLE_CORE__ === "undefined") {
    globalObj.__FABLE_CORE__ = {
      types: new Map<string, FunctionConstructor>(),
      symbols: {
        reflection: Symbol("reflection"),
      }
    };
  }
  return globalObj.__FABLE_CORE__;
}();

export function setType(fullName: string, cons: FunctionConstructor) {
  fableGlobal.types.set(fullName, cons);
}

export function getType(fullName: string) {
  return fableGlobal.types.get(fullName);
}

export default (fableGlobal.symbols);
