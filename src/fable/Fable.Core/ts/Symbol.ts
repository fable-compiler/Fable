declare var global: any;

export const fableGlobal: {
  types: Map<string, FunctionConstructor>,
  symbols: {
    reflection: symbol,
    generics: symbol,
  }
} = function () {
  const globalObj =
    typeof window !== "undefined" ? window
    : (typeof global !== "undefined" ? global
    : (typeof self !== "undefined" ? self : null));
  if (typeof globalObj.__FABLE_CORE__ === "undefined") {
    globalObj.__FABLE_CORE__ = {
      types: new Map<string, FunctionConstructor>(),
      symbols: {
        reflection: Symbol("reflection"),
        generics: Symbol("generics")
      }
    };
  }
  return globalObj.__FABLE_CORE__;
}();

export default (fableGlobal.symbols);
