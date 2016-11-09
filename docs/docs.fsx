// --------------------------------------------------------------------------------------
// Fable documentation build script
// --------------------------------------------------------------------------------------

#load "../packages/docs/FSharp.Formatting/FSharp.Formatting.fsx"
#I "../packages/docs/FAKE/tools/"
#I "../packages/docs/Suave/lib/net40"
#I "../packages/docs/DotLiquid/lib/NET451"
#r "FakeLib.dll"
#r "Suave.dll"
#r "DotLiquid.dll"
#load "liquid.fs"
#load "helpers.fs"
open Fake
open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Literate
open FSharp.Markdown
open Suave
open Suave.Web
open Suave.Http
open System.IO
open Helpers
open Fake.Git

// --------------------------------------------------------------------------------------
// Global definitions and folders
// --------------------------------------------------------------------------------------

// Where to push generated documentation
let publishSite = "//fable.io"
let githubLink = "http://github.com/fable-compiler/fable-compiler.github.io"
let publishBranch = "master"

// Paths with template/source/output locations
let source      = __SOURCE_DIRECTORY__ </> "source"
let templates   = __SOURCE_DIRECTORY__ </> "templates"
let output      = __SOURCE_DIRECTORY__ </> "output"
let contentPage = "content.html"
let samplePage  = "sample.html"
let fableRoot   = __SOURCE_DIRECTORY__ </> ".." |> Path.GetFullPath
let temp        = fableRoot </> "temp"
let samplesRoot = fableRoot </> "samples" 

let samples =
  [ // Browser samples with HTML content
    "samples/browser/mario", true
    "samples/browser/hokusai", true
    "samples/browser/ozmo", true
    "samples/browser/pacman", true
    "samples/browser/d3map", true
    "samples/browser/webGLTerrain", true
    "samples/browser/samegame", true
    "samples/browser/virtualdom", true
    "samples/browser/react-todomvc", true
    "samples/browser/redux-todomvc", true
    "samples/browser/lsystem", true

    // Non-browser samples without embedded HTML
    "samples/node/server", false
    "samples/node/express", false
    "samples/node/nunit", false ]


// Set templates directory for DotLiquid
DotLiquid.setTemplatesDirs [templates]
DotLiquid.Template.NamingConvention <- DotLiquid.NamingConventions.CSharpNamingConvention()

// --------------------------------------------------------------------------------------
// Markdown pre-processing
// --------------------------------------------------------------------------------------

/// Extract heading from the document
let extractHeading name paragraphs =
    let heading, other =
      paragraphs
      |> List.map (function
        | Heading(1, [Literal text]) -> Choice1Of2 text
        | p -> Choice2Of2 p)
      |> List.partition (function Choice1Of2 _ -> true | _ -> false)
    match heading, other with
    | [Choice1Of2 text], pars -> text, List.map (function Choice2Of2 p -> p | _ -> failwith "unexpected") pars
    | _ -> failwithf "Document '%s' does not contain parseable top-level heading." name

/// Extracts list with metadata from a document
let extractAttributes name (doc:MarkdownDocument) paragraphs =
    match paragraphs with
    | ListBlock(_, lis)::pars ->
        [ for li in lis do
            match li with
            // The <li> is either Span (for short items) or Paragraph (if there
            // are any long items). We extract key from "key: value" in the first
            // paragraph. Then we format short things as Span and long as Paragraph
            // (black magic to make sure we emit nice HTML....)
            | Paragraph (Literal s::spans)::pars
            | Span (Literal s::spans)::pars when s.Contains(":") ->
                let col = s.IndexOf(":")
                let wrap = match li with (Span _)::_ | [Paragraph _] -> Span | _ -> Paragraph
                let rest = wrap(Literal (s.Substring(col+1))::spans)::pars
                yield s.Substring(0, col).Trim(), Markdown.WriteHtml(MarkdownDocument(rest, doc.DefinedLinks)).Trim()
            | _ -> failwithf "Document '%s' has unsupported formatting in header block: %A" name li ],
        pars
    | _ -> failwithf "Document '%s' is missing header block." name


// --------------------------------------------------------------------------------------
// Generating static parts of the web site
// --------------------------------------------------------------------------------------

/// Simple model for processed Markdown documents
type Page =
  { Root : string
    Active : string
    Heading : string
    Tagline : string
    Content : string }

// Copy static files from the 'source' folder to 'output' folder (add more extensions!)
let copyFiles force =
    Helpers.processDirectory force source output [".css"; ".js"; ".png"; ".gif"; ".jpg"; ""]
      (fun source outdir ->
          let name = Path.GetFileName(source)
          File.Copy(source, outdir </> name, true) )

// Build documentation from Markdown files in `content`
let processMarkdown siteRoot force =
    Helpers.processDirectory force source output [".md"]
      (fun source outdir ->
          let name = Path.GetFileNameWithoutExtension(source)
          printfn "Processing markdown file: %s.md" name
          use tmp = TempFile.New()
          let attrs = System.Collections.Generic.Dictionary<_, _>()
          let heading = ref ""
          Literate.ProcessMarkdown(source, output=tmp.Name, generateAnchors=true, customizeDocument = fun _ doc ->
              let kvps, pars = extractAttributes name doc.MarkdownDocument doc.Paragraphs
              for k, v in kvps do attrs.Add(k, v)
              let htext, pars = extractHeading name pars
              heading.Value <- htext
              doc.With(pars) )
          let html =
              { Root = siteRoot
                Active = if attrs.ContainsKey("active") then attrs.["active"] else name
                Tagline = attrs.["tagline"]
                Heading = heading.Value
                Content = File.ReadAllText(tmp.Name) }
              |> DotLiquid.page contentPage
          File.WriteAllText(outdir </> name + ".html", html))

// Build documentation from HTML files in `content` (just apply templating)
let processHtml siteRoot force =
    Helpers.processDirectory force source output [".html"]
      (fun source outdir ->
          let name = Path.GetFileNameWithoutExtension(source)
          printfn "Processing html file: %s.html" name
          let html =
            { Active = name; Content = ""; Tagline = ""; Heading = ""; Root = siteRoot  }
            |> DotLiquid.page source
          File.WriteAllText(outdir </> name + ".html", html))

// Generate all static parts of the web site (this is pretty fast)
let generateStaticPages siteRoot force () =
    traceImportant "Updating static pages"
    copyFiles force
    processMarkdown siteRoot force
    processHtml siteRoot force
    traceImportant "Updating static pages completed"


// --------------------------------------------------------------------------------------
// Generating pages for the Fable demos
// --------------------------------------------------------------------------------------


/// Install 'core-js' and 'requirejs' into temp & copy the JS files
/// into the output folder (we may need to add more dependencies here)
let copySharedScripts () =
    CleanDir temp
    Npm.run temp "init" ["--yes"]
    Npm.install temp ["core-js"; "requirejs"]

    ensureDirectory (output </> "samples" </> "scripts")
    temp </> "node_modules/core-js/client/core.min.js"
    |> CopyFile (output </> "samples" </> "scripts" </> "core.min.js")
    temp </> "node_modules/requirejs/require.js"
    |> CopyFile (output </> "samples" </> "scripts" </> "require.js")
    fableRoot </> "src/fable/Fable.Core/npm/fable-core.min.js"
    |> CopyFile (output </> "samples" </> "scripts" </> "fable-core.min.js")


/// Extract string (from HTML page) enclosed between
/// <!-- [tag] --> and <!-- [/tag] -->
let extractMarkedPagePart tag (page:string) =
    let pattern =  """\<\!--\s*\[""" + tag + """\]\s*--\>(.*)\<\!--\s*\[/""" + tag + """\]\s*--\>"""
    let mtch = Regex.Match(page, pattern, RegexOptions.Singleline)
    if mtch.Success then mtch.Groups.[1].Value else ""


/// Compile sample using Fable & copy static and JS files to `samples/<name>`
let compileSample copyOutput name path =
    // Compile and copy JS files
    CleanDir temp
    System.Environment.CurrentDirectory <- fableRoot
    Node.run fableRoot "build/fable" [path; "-o"; "../../../temp"; "--symbols"; "TUTORIAL"]
    ensureDirectory (output </> "samples" </> name)

    if copyOutput then
        // Copy compiled JavaScript files
        // Attention, for some reason it seems !!(temp </> "*.*") doesn't work properly in OSX
        // !!(temp </> "*.*") |> CopyFiles (output </> "samples" </> name)
        directoryCopy(temp, output </> "samples" </> name, true)

        // Copy subdirectories and static files (except for special ones)
        Directory.GetDirectories(path)
        |> Seq.filter (fun d -> not (d.EndsWith("out")) && not (d.EndsWith("node_modules")))
        |> Seq.iter (fun d -> CopyDir (output </> "samples" </> name </> (Path.GetFileName d)) d (fun _ -> true))

        Directory.GetFiles(path)
        |> Seq.filter (fun d ->
              ["index.html"; ".json"; ".fsx"; ".fsproj"; ".fs"]
              |> Seq.forall (fun s -> not (d.EndsWith(s))) )
        |> Seq.iter (fun d -> CopyFile (output </> "samples" </> name </> (Path.GetFileName d)) d)


/// Model passed to DotLiquid sample page
type Sample =
  { Root : string
    Active : string
    Name : string
    Title : string
    Tagline : string
    Introduction : string
    Document : string
    RequirePaths : string
    Application : string
    Head : string
    AppStyle : string }


/// Generate page from `.fsx` and (optionally) `index.html` files in the sample path
let generateSamplePage siteRoot name path =

    /// Find first `.fsx` file and format it using F# Formatting
    /// (and also extract meta-data from the first paragraph)
    let script = Directory.GetFiles(fableRoot </> path, "*.fsx") |> Seq.head
    use f = TempFile.New()
    let attrs = System.Collections.Generic.Dictionary<_, _>()
    Literate.ProcessScriptFile(script, output=f.Name, generateAnchors=true, customizeDocument = fun _ doc ->
        let kvps, pars = extractAttributes name doc.MarkdownDocument doc.Paragraphs
        for k, v in kvps do attrs.Add(k, v)
        doc.With(pars) )
    let document = File.ReadAllText(f.Name)

    // If there is `index.html` page, read `<!-- [body] -->` bit from it
    let (app, appHead), appStyle =
      let index = fableRoot </> path </> "index.html"
      ( if File.Exists index then 
           let indexHtml = File.ReadAllText(index)
           extractMarkedPagePart "body" indexHtml,
           extractMarkedPagePart "head" indexHtml
        else "", "" ),
      ( if attrs.ContainsKey("app-style") then attrs.["app-style"] else "" )

    // Require paths are specified using ` .. ` - drop <code>
    let requirePaths = 
      if not (attrs.ContainsKey("require-paths")) then "" else
      Regex.Match(attrs.["require-paths"], "<code>(.*)</code>").Groups.[1].Value

    // Get `title` and `tagline` from the attrs and generate sample page
    let html =
        { Root = siteRoot;
          Active = "samples"; Name = name; Document = document
          Application = app; Head = appHead; RequirePaths = requirePaths
          Title = attrs.["title"]
          Introduction = if attrs.ContainsKey("intro") then attrs.["intro"] else ""
          Tagline = attrs.["tagline"]
          AppStyle = appStyle }
        |> DotLiquid.page samplePage
    ensureDirectory(output </> "samples" </> name)
    File.WriteAllText(output </> "samples" </> name </> "index.html", html)


/// Generates pages from all samples & optionally recompiles the JS too
let generateSamplePages siteRoot recompile () =
  traceImportant "Updating sample pages"
  let lastEdit path = Directory.GetFiles(path) |> Seq.map File.GetLastWriteTime |> Seq.fold max DateTime.MinValue
  for sample, embed in samples do
    let outOfDate =
        if Directory.Exists(output </> "samples" </> Path.GetFileName(sample)) |> not
        then true
        else
            let sourceModified = lastEdit (fableRoot </> sample)
            let outputModified = lastEdit (output </> "samples" </> Path.GetFileName(sample))
            sourceModified > outputModified
    if outOfDate then
      let name = Path.GetFileName(sample)
      traceImportant (sprintf "Generating sample page: %s" name)
      generateSamplePage siteRoot name sample
      if recompile then
        traceImportant (sprintf "Compiling sample: %s" name)
        compileSample embed name sample


// --------------------------------------------------------------------------------------
// Local Suave server for hosting page during development (with WebSockets refresh!)
// --------------------------------------------------------------------------------------

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Suave.Operators

let refreshEvent = new Event<unit>()

let socketHandler (webSocket : WebSocket) cx = socket {
    while true do
      let! refreshed =
        Control.Async.AwaitEvent(refreshEvent.Publish)
        |> Suave.Sockets.SocketOp.ofAsync
      do! webSocket.send Text (Suave.Utils.ASCII.bytes "refreshed") true }

let startWebServer () =
    let port = 8911
    let serverConfig =
        { defaultConfig with
           homeFolder = Some (FullName output)
           bindings = [ HttpBinding.mkSimple HTTP "127.0.0.1" port ] }
    let app =
      choose [
        Filters.path "/websocket" >=> handShake socketHandler
        Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
        >=> Writers.setHeader "Pragma" "no-cache"
        >=> Writers.setHeader "Expires" "0"
        >=> choose [ Files.browseHome; Filters.path "/" >=> Files.browseFileHome "index.html" ] ]

    let addMime f = function 
      | ".wav" -> Writers.mkMimeType "audio/wav" false 
      | ".tsv" -> Writers.mkMimeType "text/tsv" false | ext -> f ext
    let app ctx = app { ctx with runtime = { ctx.runtime with mimeTypesMap = addMime ctx.runtime.mimeTypesMap } }

    startWebServerAsync serverConfig app |> snd |> Async.Start
    System.Diagnostics.Process.Start (sprintf "http://localhost:%d/index.html" port) |> ignore


// --------------------------------------------------------------------------------------
// FAKE targets for generating and releasing documentation
// --------------------------------------------------------------------------------------

Target "CleanDocs" (fun _ ->
    CleanDirs [output]
)

Target "GenerateDocs" (fun _ ->
    // copySharedScripts ()
    generateStaticPages publishSite true ()
    // generateSamplePages publishSite true ()
)

Target "BrowseDocs" (fun _ ->
    // Update static pages & sample pages (but don't recompile JS)
    let root = "http://localhost:8911"
    copySharedScripts ()
    generateStaticPages root true ()
    generateSamplePages root false ()

    // Setup watchers to regenerate things as needed
    let watchAndRefresh f = WatchChanges (fun _ ->
      try f(); refreshEvent.Trigger() with e -> traceException e)
    use w1 = !! (source + "/**/*.*") |> watchAndRefresh (generateStaticPages root false)
    use w2 = !! (templates + "/*.*") |> watchAndRefresh (generateSamplePages root false >> generateStaticPages root true)
    use w3 = !! (samplesRoot + "/**/*.*") |> watchAndRefresh (generateSamplePages root true)

    // Start local server
    startWebServer ()
    traceImportant "Waiting for page edits. Press ^C to kill the process."
    System.Threading.Thread.Sleep(-1)
)

let publishDocs() =
  CleanDir temp
  Repository.cloneSingleBranch "" (githubLink + ".git") publishBranch temp

  CopyRecursive output temp true |> tracefn "%A"
  StageAll temp
  Git.Commit.Commit temp (sprintf "Update site (%s)" (DateTime.Now.ToShortDateString()))
  Branches.push temp

Target "PublishDocs" (fun _ ->
    publishDocs()
)

Target "PublishStaticPages" (fun _ ->
    generateStaticPages publishSite true ()
    publishDocs()
)

// --------------------------------------------------------------------------------------
// Regenerate all docs when publishing, by default just generate & browse
// --------------------------------------------------------------------------------------

"CleanDocs"
  ==> "GenerateDocs"
  ==> "PublishDocs"

RunTargetOrDefault "BrowseDocs"
