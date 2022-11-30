// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.Diagnostics

open System
open System.Diagnostics

[<RequireQualifiedAccess>]
module Activity =

    let private activitySourceName = "fsc"
    let private activitySource = new ActivitySource(activitySourceName)

    let start (name: string) (tags: (string * string) seq) : IDisposable =
        let activity = activitySource.StartActivity(name)

#if FABLE_COMPILER
        ignore tags
#else
        match activity with
        | null -> ()
        | activity ->
            for key, value in tags do
                activity.AddTag(key, value) |> ignore
#endif

        activity

    let startNoTags (name: string) : IDisposable = activitySource.StartActivity(name)
