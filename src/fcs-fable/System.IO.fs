//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.IO

module Path =
    let Combine (path1: string, path2: string) = //TODO: proper xplat implementation
        let path1 =
            if (String.length path1) = 0 then path1
            else (path1.TrimEnd [|'\\';'/'|]) + "/"
        path1 + (path2.TrimStart [|'\\';'/'|])

    let ChangeExtension (path: string, ext: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then path
        else path.Substring(0, i) + ext

    let HasExtension (path: string) =
        let i = path.LastIndexOf(".")
        i >= 0

    let GetExtension (path: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then ""
        else path.Substring(i)

    let GetInvalidPathChars () = //TODO: proper xplat implementation
        Seq.toArray "<>\"|?*\b\t"
    
    let GetInvalidFileNameChars () = //TODO: proper xplat implementation
        Seq.toArray "<>:\"|\\/?*\b\t"

    let GetFileName (path: string) =
        let normPath = path.Replace("\\", "/").TrimEnd('/')
        let i = normPath.LastIndexOf("/")
        normPath.Substring(i + 1)

    let GetFileNameWithoutExtension (path: string) =
        let filename = GetFileName path
        let i = filename.LastIndexOf(".")
        if i < 0 then filename
        else filename.Substring(0, i)

    let GetDirectoryName (path: string) = //TODO: proper xplat implementation
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i <= 0 then ""
        else normPath.Substring(0, i)

    let DirectorySeparatorChar = '/'
    let AltDirectorySeparatorChar = '/'

module Directory =
    let GetCurrentDirectory() = //TODO: proper xplat implementation
        "."
