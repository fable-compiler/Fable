module Build.FcsRepo

open SimpleExec
open Build.Utils

[<Literal>]
let FCS_REPO = "https://github.com/ncave/fsharp"

let FCS_REPO_LOCAL = Path.Resolve("../../../fsharp_fable")

[<Literal>]
let FCS_REPO_FABLE_BRANCH = "fable"

[<Literal>]
let FCS_REPO_SERVICE_SLIM_BRANCH = "service_slim"

let sync () =
    printfn "Waiting for ncave support on how to use the FCS repo in Fable"
    printfn "See: https://github.com/fable-compiler/Fable/issues/3531"
//     printfn
//         $"""Expecting {FCS_REPO} repo to be cloned at {FCS_REPO_LOCAL}

// Run the following command to clone the repo:
//     git clone {FCS_REPO} {FCS_REPO_LOCAL}
//     """

//     Command.Run(
//         "git",
//         $"checkout {FCS_REPO_SERVICE_SLIM_BRANCH}",
//         workingDirectory = FCS_REPO_LOCAL
//     )

//     Command.Run(
//         "git",
//         "pull",
//         workingDirectory = FCS_REPO_LOCAL
//     )

//     let program, args =
//         if Environment.isWindows () then
//             "cmd.exe", "/c build.cmd"
//         else
//             "sh", "build.sh"

//     Command.Run(
//         program,
//         args,
//         workingDirectory = (FCS_REPO_LOCAL </> "fcs")
//     )

//     Command.Run(
//         "git",
//         $"checkout {FCS_REPO_FABLE_BRANCH}",
//         workingDirectory = FCS_REPO_LOCAL
//     )

//     Command.Run(
//         "git",
//         "pull",
//         workingDirectory = FCS_REPO_LOCAL
//     )

//     Command.Run(
//         program,
//         $"{args} CodeGen.Fable",
//         workingDirectory = (FCS_REPO_LOCAL </> "fcs")
//     )

let copy () =
    printfn "Waiting for ncave support on how to use the FCS repo in Fable"
    printfn "See: https://github.com/fable-compiler/Fable/issues/3531"
