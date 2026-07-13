namespace Fable.Beam

module Naming =
    open System.Text.RegularExpressions

    // https://www.erlang.org/doc/system/expressions#reserved-words
    let erlKeywords =
        System.Collections.Generic.HashSet
            [
                "after"
                "and"
                "andalso"
                "band"
                "begin"
                "bnot"
                "bor"
                "bsl"
                "bsr"
                "bxor"
                "case"
                "catch"
                "cond"
                "div"
                "end"
                "fun"
                "if"
                "let"
                "not"
                "of"
                "or"
                "orelse"
                "receive"
                "rem"
                "try"
                "when"
                "xor"
                "maybe"
                "else"
            ]

    /// Auto-imported BIFs that cause "ambiguous call" errors when used as
    /// module-level function names. These need no_auto_import in generated modules.
    let erlAutoImportedBifs =
        System.Collections.Generic.HashSet
            [ "apply"; "now"; "self"; "throw"; "exit"; "error"; "spawn"; "link"; "monitor" ]

    let checkErlKeywords name =
        if erlKeywords.Contains name then
            name + "_"
        else
            name

    let toSnakeCase (name: string) =
        let sb = System.Text.StringBuilder()

        for i = 0 to name.Length - 1 do
            let c = name.[i]

            if System.Char.IsUpper(c) then
                if i > 0 then
                    sb.Append('_') |> ignore

                sb.Append(System.Char.ToLowerInvariant(c)) |> ignore
            else
                sb.Append(c) |> ignore

        sb.ToString()

    let sanitizeErlangName (name: string) =
        // Decode $XXXX hex sequences from F# compiled names (e.g. $0020 -> space -> _)
        Regex.Replace(
            name,
            @"\$([0-9A-Fa-f]{4})",
            fun m ->
                let c = char (System.Convert.ToInt32(m.Groups.[1].Value, 16))

                if System.Char.IsLetterOrDigit(c) then
                    c.ToString()
                else
                    "_"
        )
        |> fun s ->
            s.Replace("'", "").Replace("$", "_").Replace("@", "").Replace(".", "_").Replace("`", "_").Replace("-", "_")
        |> toSnakeCase
        |> fun s -> Regex.Replace(s, "_+", "_")
        |> fun s -> s.Trim('_')
        |> checkErlKeywords

    let moduleNameFromFile (filePath: string) =
        Fable.Path.GetFileNameWithoutExtension(filePath)
        |> fun s -> s.Replace(".", "_").Replace("-", "_")
        |> Fable.Naming.applyCaseRule Fable.Core.CaseRules.SnakeCase

    // ----------------------------------------------------------------------------------
    // Qualified module names
    //
    // Erlang's module namespace is flat and global: the atom in `-module(...)` is a module's
    // only identity, and neither the directory nor the OTP application an .erl file lives in
    // scopes it. Naming a module after the bare basename of its F# file therefore collides
    // with OTP's own modules (`gen`, `random`, `string`, ...) and with same-named files from
    // other assemblies — silently, and fatally at runtime.
    //
    // So qualify the name with the application it belongs to, which is what OTP itself does
    // (`cowboy_req`, `rebar_app_info`) and what Fable's own runtime already does (`fable_list`).
    // ----------------------------------------------------------------------------------

    let private splitPath (path: string) =
        path.Replace('\\', '/').Split('/')
        |> Array.filter (fun s -> s <> "" && s <> ".")

    /// True when a dot-segment looks like a version number (starts with a digit).
    let private isVersionSegment (s: string) =
        s.Length > 0 && System.Char.IsDigit(s.[0])

    /// Normalize a name to a valid OTP application name (lowercase snake_case, no leading/trailing underscores).
    ///   "Fable.Tests.Beam" → "fable_tests_beam"
    ///   "fable-library-beam" → "fable_library_beam"
    let normalizeAppName (name: string) =
        name.Replace('.', '_').Replace('-', '_').ToLowerInvariant().Trim('_')

    /// Derive an OTP application name from a fable_modules directory name.
    ///   "Fable.Logging.0.10.0"         → "fable_logging"
    ///   "fable-library-beam"           → "fable_library_beam"
    ///   "Fable.Python.4.0.0-theta-003" → "fable_python"
    let deriveDepAppName (dirName: string) =
        let dotParts = dirName.Split('.')

        let namePart =
            match dotParts |> Array.tryFindIndex isVersionSegment with
            | Some idx when idx > 0 -> dotParts.[.. idx - 1] |> String.concat "."
            | _ -> dirName

        normalizeAppName namePart

    /// Extract the version string from a fable_modules directory name.
    ///   "Fable.Logging.0.10.0"         → "0.10.0"
    ///   "Fable.Python.4.0.0-theta-003" → "4.0.0-theta-003"
    ///   "fable-library-beam"           → "0.1.0"
    let extractDepVersion (dirName: string) =
        let dotParts = dirName.Split('.')

        match dotParts |> Array.tryFindIndex isVersionSegment with
        | Some idx -> dotParts.[idx..] |> String.concat "."
        | None -> "0.1.0"

    /// True for a source file Fable compiles into a generated Erlang module. A signature file
    /// declares no module of its own, so it is not one.
    let isGeneratedModuleSource (path: string) =
        let ext = Fable.Path.GetExtension(path)
        ext = ".fs" || ext = ".fsx"

    /// True for a path naming an F# source file. Anything else (a native Erlang module named in
    /// a `BeamInterop` import, one of fable-library's hand-written `.erl` files) names a module
    /// Fable does not generate, and keeps its own name.
    let isFSharpSource (path: string) =
        isGeneratedModuleSource path || Fable.Path.GetExtension(path) = ".fsi"

    /// Modules of OTP's own `erts`, `kernel` and `stdlib` applications. The module namespace is
    /// flat and global, so a generated module named after one of these shadows it (or is shadowed
    /// by it) everywhere in the release, and calls to either raise `undef` at runtime.
    ///
    /// Qualifying generated names by their app rules out the bare ones (`gen`, `string`, ...),
    /// but a two-segment name can still land on `gen_server` or `erl_eval` — an app named `Gen`
    /// with a `Server.fs` does exactly that — and fable-library's exempt modules are not
    /// qualified at all. `checkBeamModuleNames` fails the build on any name in this set.
    let otpModules =
        System.Collections.Generic.HashSet
            [
                // erts (preloaded)
                "atomics"
                "counters"
                "erl_init"
                "erl_prim_loader"
                "erl_tracer"
                "erlang"
                "erts_code_purger"
                "erts_dirty_process_signal_handler"
                "erts_internal"
                "erts_literal_area_collector"
                "erts_trace_cleaner"
                "init"
                "persistent_term"
                "prim_buffer"
                "prim_eval"
                "prim_file"
                "prim_inet"
                "prim_net"
                "prim_socket"
                "prim_zip"
                "socket_registry"
                "zlib"
                // kernel + stdlib
                "application"
                "application_controller"
                "application_master"
                "application_starter"
                "array"
                "auth"
                "base64"
                "beam_lib"
                "binary"
                "c"
                "calendar"
                "code"
                "code_server"
                "dets"
                "dets_server"
                "dets_sup"
                "dets_utils"
                "dets_v9"
                "dict"
                "digraph"
                "digraph_utils"
                "disk_log"
                "disk_log_1"
                "disk_log_server"
                "disk_log_sup"
                "dist_ac"
                "dist_util"
                "edlin"
                "edlin_expand"
                "epp"
                "erl_abstract_code"
                "erl_anno"
                "erl_bits"
                "erl_boot_server"
                "erl_compile"
                "erl_compile_server"
                "erl_ddll"
                "erl_distribution"
                "erl_epmd"
                "erl_error"
                "erl_erts_errors"
                "erl_eval"
                "erl_expand_records"
                "erl_features"
                "erl_internal"
                "erl_kernel_errors"
                "erl_lint"
                "erl_parse"
                "erl_posix_msg"
                "erl_pp"
                "erl_reply"
                "erl_scan"
                "erl_signal_handler"
                "erl_stdlib_errors"
                "erl_tar"
                "erpc"
                "error_handler"
                "error_logger"
                "error_logger_file_h"
                "error_logger_tty_h"
                "erts_debug"
                "escript"
                "ets"
                "eval_bits"
                "file"
                "file_io_server"
                "file_server"
                "file_sorter"
                "filelib"
                "filename"
                "gb_sets"
                "gb_trees"
                "gen"
                "gen_event"
                "gen_fsm"
                "gen_sctp"
                "gen_server"
                "gen_statem"
                "gen_tcp"
                "gen_tcp_socket"
                "gen_udp"
                "gen_udp_socket"
                "global"
                "global_group"
                "global_search"
                "group"
                "group_history"
                "heart"
                "inet"
                "inet6_sctp"
                "inet6_tcp"
                "inet6_tcp_dist"
                "inet6_udp"
                "inet_config"
                "inet_db"
                "inet_dns"
                "inet_gethost_native"
                "inet_hosts"
                "inet_parse"
                "inet_res"
                "inet_sctp"
                "inet_tcp"
                "inet_tcp_dist"
                "inet_udp"
                "io"
                "io_lib"
                "io_lib_format"
                "io_lib_fread"
                "io_lib_pretty"
                "kernel"
                "kernel_config"
                "kernel_refc"
                "lists"
                "local_tcp"
                "local_udp"
                "log_mf_h"
                "logger"
                "logger_backend"
                "logger_config"
                "logger_disk_log_h"
                "logger_filters"
                "logger_formatter"
                "logger_h_common"
                "logger_handler_watcher"
                "logger_olp"
                "logger_proxy"
                "logger_server"
                "logger_simple_h"
                "logger_std_h"
                "logger_sup"
                "maps"
                "math"
                "ms_transform"
                "net"
                "net_adm"
                "net_kernel"
                "orddict"
                "ordsets"
                "os"
                "otp_internal"
                "peer"
                "pg"
                "pg2"
                "pool"
                "proc_lib"
                "proplists"
                "qlc"
                "qlc_pt"
                "queue"
                "ram_file"
                "rand"
                "random"
                "raw_file_io"
                "raw_file_io_compressed"
                "raw_file_io_deflate"
                "raw_file_io_delayed"
                "raw_file_io_inflate"
                "raw_file_io_list"
                "re"
                "rpc"
                "seq_trace"
                "sets"
                "shell"
                "shell_default"
                "shell_docs"
                "slave"
                "socket"
                "sofs"
                "standard_error"
                "string"
                "supervisor"
                "supervisor_bridge"
                "sys"
                "timer"
                "unicode"
                "unicode_util"
                "uri_string"
                "user"
                "user_drv"
                "user_sup"
                "win32reg"
                "wrap_log_reader"
                "zip"
            ]

    /// True for a path inside fable-library — its own sources, or its sources copied into a
    /// consumer's fable_modules.
    ///
    /// fable-library is the one Fable project whose *compiled* output ships as a dependency of
    /// other projects, so it gets two exemptions: its modules keep their bare, hand-maintained
    /// names (`fable_list`, `seq`, ...) — already namespaced by the `fable_` convention, and
    /// `Transforms.Util.getLibPath` refers to them by exactly those names — and it is never
    /// given an entry-point shim, which would collide with the consuming app's own.
    let isFableLibraryPath (path: string) =
        splitPath path
        |> Array.exists (fun seg -> seg.StartsWith("fable-library", System.StringComparison.Ordinal))

    /// Join path segments into a single snake_case Erlang atom.
    /// `[ "Scriptorium.Quill"; "DSL" ]` → `scriptorium_quill_dsl`
    let private joinModuleName (segments: string seq) =
        segments
        |> Seq.map (fun s -> s.Replace('.', '_').Replace('-', '_'))
        |> String.concat "_"
        |> Fable.Naming.applyCaseRule Fable.Core.CaseRules.SnakeCase
        |> fun s -> Regex.Replace(s, "_+", "_")
        |> fun s -> s.Trim('_')
        |> checkErlKeywords

    /// Number of leading segments `a` and `b` have in common.
    let private commonPrefixLength (a: string[]) (b: string[]) =
        let mutable i = 0

        while i < a.Length
              && i < b.Length
              && System.String.Equals(a.[i], b.[i], System.StringComparison.OrdinalIgnoreCase) do
            i <- i + 1

        i

    /// The Erlang module name for an F# source file, qualified by the assembly it belongs to
    /// so that it is unique across the whole compilation and cannot shadow an OTP module:
    ///
    ///   fable_modules/Hedgehog.0.11.0/Gen.fs  → hedgehog_gen        (package source)
    ///   <projDir>/Misc/Util2.fs               → my_app_misc_util2   (the project's own source)
    ///   <projDir>/../Quill/DSL.fs             → quill_dsl           (referenced project)
    ///
    /// Non-source paths (native Erlang modules such as `string`, and fable-library's `.erl`
    /// files) are passed through untouched — they name modules Fable does not generate.
    let erlangModuleName (projectFile: string) (filePath: string) =
        if not (isFSharpSource filePath) || isFableLibraryPath filePath then
            moduleNameFromFile filePath
        else
            let fileSegs = splitPath filePath

            // Drop the extension from the file name segment
            let fileSegs =
                Array.append
                    fileSegs.[.. fileSegs.Length - 2]
                    [| Fable.Path.GetFileNameWithoutExtension(fileSegs.[fileSegs.Length - 1]) |]

            match fileSegs |> Array.tryFindIndex (fun s -> s = Fable.Naming.fableModules) with
            | Some i when i + 2 <= fileSegs.Length - 1 ->
                // A package's sources, copied into fable_modules: the containing directory names
                // the app (`Hedgehog.0.11.0` → `hedgehog`), the rest is its path within the app.
                Array.append [| deriveDepAppName fileSegs.[i + 1] |] fileSegs.[i + 2 ..]
                |> joinModuleName
            | _ ->
                let projSegs = splitPath (Fable.Path.GetDirectoryName(projectFile))
                let shared = commonPrefixLength projSegs fileSegs
                let belowProjDir = fileSegs.[shared..]
                let appName = Fable.Path.GetFileNameWithoutExtension(projectFile)

                // A file outside the project directory belongs to a referenced project, and its
                // path below the shared ancestor starts with that project's own directory —
                // conventionally its name — which is the qualifier we want. Unless there is no
                // such directory (the file sits directly in the shared ancestor, as a linked
                // `../Gen.fs` does), in which case the only assembly it can belong to is this one.
                if projSegs.Length > 0 && shared = 0 then
                    // The two paths have no ancestor in common at all (different Windows drives,
                    // say). "Below the shared ancestor" is then the whole absolute path, which
                    // would bake the machine's directory layout into the module name — qualify by
                    // the project and keep only the file name.
                    joinModuleName [| appName; Array.last fileSegs |]
                elif shared = projSegs.Length || belowProjDir.Length < 2 then
                    Array.append [| appName |] belowProjDir |> joinModuleName
                else
                    belowProjDir |> joinModuleName

    let capitalizeFirst (s: string) =
        if s.Length = 0 then
            s
        else
            s.[0..0].ToUpperInvariant() + s.[1..]

    /// Convert a field name to a safe Erlang atom (snake_case + keyword escaping).
    /// camelCase names get a trailing '_' to avoid collision with PascalCase names
    /// (e.g., firstName -> first_name_, FirstName -> first_name).
    let sanitizeFieldName (name: string) =
        let snakeName = toSnakeCase name

        let disambiguated =
            if name.Length > 0 && System.Char.IsLower(name.[0]) then
                snakeName + "_"
            else
                snakeName

        checkErlKeywords disambiguated

    /// Quote an Erlang atom if it needs quoting (starts with uppercase, contains special chars, etc.)
    let quoteErlangAtom (name: string) =
        if name.Length = 0 then
            "''"
        elif
            System.Char.IsLower(name.[0])
            && name
               |> Seq.forall (fun c -> System.Char.IsLetterOrDigit(c) || c = '_' || c = '@')
        then
            name
        else
            $"'%s{name}'"

    let sanitizeErlangVar (name: string) =
        // Remove/replace characters invalid in Erlang variable names
        name.Replace("$", "_").Replace("@", "_")
        |> fun s -> Regex.Replace(s, "[^a-zA-Z0-9_]", "_")
