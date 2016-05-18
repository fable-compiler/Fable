module Suave.DotLiquid

open System
open System.IO
open DotLiquid
open Microsoft.FSharp.Reflection

// -------------------------------------------------------------------------------------------------
// Registering things with DotLiquid
// -------------------------------------------------------------------------------------------------

/// Represents a local file system relative to the specified 'root'
let private localFileSystem roots =
  { new DotLiquid.FileSystems.IFileSystem with
      member this.ReadTemplateFile(context, templateName) =
        let templatePath = context.[templateName] :?> string
        let fullPath = 
          roots |> Seq.pick (fun root ->
            let fn = Path.Combine(root, templatePath)
            if File.Exists fn then Some fn else None)
        if not (File.Exists(fullPath)) then failwithf "File not found: %s" fullPath
        File.ReadAllText(fullPath) }

/// Protects accesses to various DotLiquid internal things
let private safe =
  let o = obj()
  fun f -> lock o f

/// Given a type which is an F# record containing seq<_>, list<_> and other
/// records, register the type with DotLiquid so that its fields are accessible
let private registerTypeTree =
  let registered = System.Collections.Generic.Dictionary<_, _>()
  let rec loop ty =
    if not (registered.ContainsKey ty) then
      if FSharpType.IsRecord ty then
        let fields = FSharpType.GetRecordFields(ty)
        Template.RegisterSafeType(ty, [| for f in fields -> f.Name |])
        for f in fields do loop f.PropertyType
      elif ty.IsGenericType &&
          ( let t = ty.GetGenericTypeDefinition()
            in t = typedefof<seq<_>> || t = typedefof<list<_>> ) then
        loop (ty.GetGenericArguments().[0])
      registered.[ty] <- true
  fun ty -> safe (fun () -> loop ty)

/// Use the ruby naming convention by default
do Template.NamingConvention <- DotLiquid.NamingConventions.RubyNamingConvention()

// -------------------------------------------------------------------------------------------------
// Parsing and loading DotLiquid templates and caching the results
// -------------------------------------------------------------------------------------------------

/// Memoize asynchronous function. An item is recomputed when `isValid` returns `false`
let private asyncMemoize isValid f =
  let cache = Collections.Concurrent.ConcurrentDictionary<_ , _>()
  fun x -> async {
      match cache.TryGetValue(x) with
      | true, res when isValid x res -> return res
      | _ ->
          let! res = f x
          cache.[x] <- res
          return res }

/// Parse the specified template & register the type that we want to use as "model"
let private parseTemplate template ty =
  registerTypeTree ty
  let t = Template.Parse(template)
  fun k v -> t.Render(Hash.FromDictionary(dict [k, v]))

/// Asynchronously loads a template & remembers the last write time
/// (so that we can automatically reload the template when file changes)
let private loadTemplate (ty, fileName) = async {
  let writeTime = File.GetLastWriteTime(fileName)
  use file = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
  use reader = new StreamReader(file)
  let! razorTemplate = reader.ReadToEndAsync() |> Async.AwaitTask
  return writeTime, parseTemplate razorTemplate ty }

/// Load template & memoize & automatically reload when the file changes
let private loadTemplateCached =
  loadTemplate |> asyncMemoize (fun (_, templatePath) (lastWrite, _) ->
    File.GetLastWriteTime(templatePath) <= lastWrite ) 

// -------------------------------------------------------------------------------------------------
// Public API
// -------------------------------------------------------------------------------------------------

let mutable private templatesDirs : string list option = None

/// Set the root directory where DotLiquid is looking for templates. For example, you can 
/// write something like this:
///
///     DotLiquid.setTemplatesDirs (__SOURCE_DIRECTORY__ + "/templates")
///
/// The current directory is a global variable and so it should not change between 
/// multiple HTTP requests. This is a DotLiquid limitation.
let setTemplatesDirs dirs =
  if templatesDirs <> Some dirs then
    templatesDirs <- Some dirs
    safe (fun () -> Template.FileSystem <- localFileSystem dirs)

/// Render a page using DotLiquid template. Takes a path (relative to the directory specified
/// using `setTemplatesDirs` and a value that is exposed as the "model" variable. You can use
/// any F# record type, seq<_> and list<_> without having to explicitly register the fields.
///
///     type Page = { Total : int }
///     let app = page "index.html" { Total = 42 } 
///
let page<'T> path (model : 'T) = 
  let path = 
    if Path.IsPathRooted path then path else
      match templatesDirs with
      | None -> failwith "Templates directory not set!"
      | Some roots -> 
          roots |> Seq.pick (fun root ->
            let fn = Path.Combine(root, path)
            if File.Exists fn then Some fn else None)
  let writeTime, renderer = 
    loadTemplateCached (typeof<'T>, path) 
    |> Async.RunSynchronously
  renderer "model" (box model) 

/// Register functions from a module as filters available in DotLiquid templates.
/// For example, the following snippet lets you write `{{ model.Total | nuce_num }}`:
///
///     module MyFilters = 
///       let niceNum i = if i > 10 then "lot" else "not much"
///
///     do registerFiltersByName "MyFilters"
///
let registerFiltersByName name =
  let asm = System.Reflection.Assembly.GetExecutingAssembly()
  let typ =
    [ for t in asm.GetTypes() do
        if t.FullName.EndsWith(name) && not(t.FullName.Contains("<StartupCode")) then yield t ]
    |> Seq.last
  Template.RegisterFilter(typ)

/// Similar to `registerFiltersByName`, but the module is speicfied by its `System.Type`
/// (This is more cumbersome, but safer alternative.)
let registerFiltersByType typ =
  Template.RegisterFilter(typ)
