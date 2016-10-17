declare var global: any;

export const fableGlobal: {
  types: Map<string, FunctionConstructor>,
  symbols: {
    interfaces: symbol,
    typeName: symbol,
    properties: symbol,
    generics: symbol,
    cases: symbol
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
        interfaces: Symbol("interfaces"),
        typeName: Symbol("typeName"),
        properties: Symbol("properties"),
        generics: Symbol("generics"),
        cases: Symbol("cases")
      }
    };
  }
  return globalObj.__FABLE_CORE__;
}();

export default fableGlobal.symbols;
