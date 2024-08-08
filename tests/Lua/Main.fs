#if FABLE_COMPILER
module Program
()
#else
module Program = let [<EntryPoint>] main _ = 0
#endif