module Fable.Core.Beam

open System

/// Pins the name of the Erlang module generated for this file.
///
/// Erlang's module namespace is flat and global, so Fable qualifies generated module names with
/// the application they belong to (`MyApp/Server.fs` -> `my_app_server`). Use this attribute when
/// the module name is part of a contract and must be an exact, known atom — a module implementing
/// an OTP behaviour, or one called from hand-written Erlang.
///
/// The name must be a plain Erlang atom: lowercase first letter, then letters, digits or
/// underscores. It must not collide with any other module in the compilation.
///
///     [&lt;Fable.Core.Beam.ModuleName("my_server")&gt;]
///     module MyApp.Server
[<AttributeUsage(AttributeTargets.Class)>]
type ModuleNameAttribute(name: string) =
    inherit Attribute()
    member _.Name = name
