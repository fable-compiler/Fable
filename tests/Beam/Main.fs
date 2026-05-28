#if FABLE_COMPILER
module Program
()
#else
module Program
// On .NET, xunit.v3 generates the test runner entry point automatically.
()
#endif
