module Fable.Core.Rust

open System

// Async attribute
type AsyncAttribute() =
    inherit Attribute()

// Force pass by reference
type ByRefAttribute() =
    inherit Attribute()

// Const attribute
type ConstAttribute() =
    inherit Attribute()

// Extern attribute
type ExternAttribute(abi: string) =
    inherit Attribute()
    new() = ExternAttribute("")

// Inner attributes
type InnerAttrAttribute
    private (name: string, value: string option, items: string[])
    =
    inherit Attribute()
    new(name: string) = InnerAttrAttribute(name, None, [||])

    new(name: string, value: string) =
        InnerAttrAttribute(name, Some value, [||])

    new(name: string, items: string[]) = InnerAttrAttribute(name, None, items)

// Outer attributes
type OuterAttrAttribute
    private (name: string, value: string option, items: string[])
    =
    inherit Attribute()
    new(name: string) = OuterAttrAttribute(name, None, [||])

    new(name: string, value: string) =
        OuterAttrAttribute(name, Some value, [||])

    new(name: string, items: string[]) = OuterAttrAttribute(name, None, items)

//Rc/Arc control
type PointerType =
    | Lrc = 0
    | Rc = 1
    | Arc = 2
    | Box = 3

// Rust - Defines the pointer type that is to be used to wrap the object (Rc/Arc)
type ReferenceTypeAttribute(pointerType: PointerType) =
    inherit Attribute()

// Unsafe attribute
type UnsafeAttribute() =
    inherit Attribute()

/// Works like `ImportAttribute` (same semantics as Dart imports).
/// You can use "*" selector.
let import<'T> (selector: string) (path: string) : 'T = nativeOnly

/// Imports a whole external module.
let importAll<'T> (path: string) : 'T = nativeOnly
