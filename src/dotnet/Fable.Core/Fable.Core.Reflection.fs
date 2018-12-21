module Fable.Core.Reflection

let isUnion (x: obj): bool = jsNative
let isRecord (x: obj): bool = jsNative

let getCaseTag (x: obj): int = jsNative
let getCaseName (x: obj): string = jsNative
let getCaseFields (x: obj): obj[] = jsNative
