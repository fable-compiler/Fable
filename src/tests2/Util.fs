namespace Fable.Tests

type SingleTypeInNamespace() =
    // Check self references for external files (see #114)
    static let greeting = "Hello"
    static member Hello = Util2.Helper.Format greeting