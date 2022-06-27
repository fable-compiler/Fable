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
