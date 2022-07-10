module Fable.Core.Rust

open System

// Force pass by reference
type ByRefAttribute() =
    inherit Attribute()

// Outer attributes
type AttrAttribute private (name: string, value: string option, items: string[]) =
    inherit Attribute()
    new (name: string) = AttrAttribute(name, None, [||])
    new (name: string, value: string) = AttrAttribute(name, Some value, [||])
    new (name: string, items: string[]) = AttrAttribute(name, None, items)

// Inner attributes
type InnerAttrAttribute private (name: string, value: string option, items: string[]) =
    inherit Attribute()
    new (name: string) = InnerAttrAttribute(name, None, [||])
    new (name: string, value: string) = InnerAttrAttribute(name, Some value, [||])
    new (name: string, items: string[]) = InnerAttrAttribute(name, None, items)

//Rc/Arc control
type PointerType =
    | Rc = 0
    | Arc = 1

// Rust - Defines the pointer type that is to be used to wrap the object (Rc/Arc)
type ReferenceTypeAttribute(pointerType: PointerType) =
    inherit Attribute()

/// Destructure a tuple of arguments and apply them to literal code as with EmitAttribute.
/// E.g. `emitExpr (arg1, arg2) "$0 + $1"` becomes `arg1 + arg2`
let emitExpr<'T> (args: obj) (code: string): 'T = nativeOnly

/// Works like `ImportAttribute` (same semantics as Dart imports).
/// You can use "*" selector.
let import<'T> (selector: string) (path: string): 'T = nativeOnly

/// Imports a whole external module.
let importAll<'T> (path: string): 'T = nativeOnly
