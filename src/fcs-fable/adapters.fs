namespace Internal.Utilities

#nowarn "1182"

//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

module System =

    module Decimal =
        let GetBits(d: decimal): int[] = [| 0; 0; 0; 0 |] //TODO: proper implementation

    module Diagnostics =
        type Trace() =
            static member TraceInformation(s) = () //TODO: proper implementation

    module Reflection =
        type AssemblyName(assemblyName: string) =
            member x.Name = assemblyName //TODO: proper implementation

    type WeakReference<'T>(v: 'T) =
        member x.TryGetTarget () = (true, v)

    type StringComparer(comp: System.StringComparison) =
        static member Ordinal = StringComparer(System.StringComparison.Ordinal)
        static member OrdinalIgnoreCase = StringComparer(System.StringComparison.OrdinalIgnoreCase)
        interface System.Collections.Generic.IEqualityComparer<string> with
            member x.Equals(a,b) = System.String.Compare(a, b, comp) = 0
            member x.GetHashCode(a) =
                match comp with
                | System.StringComparison.Ordinal -> hash a
                | System.StringComparison.OrdinalIgnoreCase -> hash (a.ToLowerInvariant())
                | _ -> failwithf "Unsupported StringComparison: %A" comp
        interface System.Collections.Generic.IComparer<string> with
            member x.Compare(a,b) = System.String.Compare(a, b, comp)

    module Collections =
        module Concurrent =
            open System.Collections.Generic

            type ConcurrentDictionary<'TKey, 'TValue when 'TKey: equality>(comparer: IEqualityComparer<'TKey>) =
                inherit Dictionary<'TKey, 'TValue>(comparer)
                new () = ConcurrentDictionary {
                        new IEqualityComparer<'TKey> with
                            member __.GetHashCode(x) = x.GetHashCode()
                            member __.Equals(x, y) = x.Equals(y) }
                member x.TryAdd (key:'TKey, value:'TValue) = x.[key] <- value; true
                member x.GetOrAdd (key:'TKey, valueFactory: 'TKey -> 'TValue): 'TValue =
                    match x.TryGetValue key with
                    | true, v -> v
                    | false, _ -> let v = valueFactory(key) in x.[key] <- v; v

    module IO =
        module Directory =
            let GetCurrentDirectory () = "." //TODO: proper xplat implementation

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
                Seq.toArray "<>:\"|?*\b\t"
            
            let GetInvalidFileNameChars () = //TODO: proper xplat implementation
                Seq.toArray "<>:\"|\\/?*\b\t"

            let GetFullPath (path: string) = //TODO: proper xplat implementation
                path

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

            let IsPathRooted (path: string) = //TODO: proper xplat implementation
                let normPath = path.Replace("\\", "/").TrimEnd('/')
                normPath.StartsWith("/")


module Microsoft =
    module FSharp =

        //------------------------------------------------------------------------
        // From reshapedreflection.fs
        //------------------------------------------------------------------------
        module Core =
            module XmlAdapters =
                let s_escapeChars = [| '<'; '>'; '\"'; '\''; '&' |]
                let getEscapeSequence c =
                    match c with
                    | '<'  -> "&lt;"
                    | '>'  -> "&gt;"
                    | '\"' -> "&quot;"
                    | '\'' -> "&apos;"
                    | '&'  -> "&amp;"
                    | _ as ch -> ch.ToString()
                let escape str = String.collect getEscapeSequence str

        //------------------------------------------------------------------------
        // From sr.fs
        //------------------------------------------------------------------------
        module Compiler =
            module SR =
                let GetString(name: string) =
                    match SR.Resources.resources.TryGetValue(name) with
                    | true, value -> value
                    | _ -> "Missing FSStrings error message for: " + name

            module DiagnosticMessage =
                type ResourceString<'T>(sfmt: string, fmt: string) =
                    member x.Format =
                        let a = fmt.Split('%')
                                |> Array.filter (fun s -> String.length s > 0)
                                |> Array.map (fun s -> box("%" + s))
                        let tmp = System.String.Format(sfmt, a)
                        let fmt = Printf.StringFormat<'T>(tmp)
                        sprintf fmt

                let postProcessString (s: string) =
                    s.Replace("\\n","\n").Replace("\\t","\t")

                let DeclareResourceString (messageID: string, fmt: string) =
                    let messageString = SR.GetString(messageID) |> postProcessString
                    ResourceString<'T>(messageString, fmt)
