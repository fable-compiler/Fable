// HACK: fixes value initialization
// https://github.com/dotnet/netcorecli-fsc/issues/79
module Program = let [<EntryPoint>] main _ = 0
