module ClassMemberClassMemberWithAttachMembers

open Fable.Core

/// My superb class with attachments and doc comments
[<Fable.Core.AttachMembers>]
type ClassWithAttachmentsAndDoc(v: int) =
    /// Gets the value
    member _.Value = v
    /// Returns a greeting
    member _.Greet(name: string) = $"Hello, {name}! Value is {v}."
