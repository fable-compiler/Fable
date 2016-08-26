[<RequireQualifiedAccess>]
module Forge.Constants

// Common Constants
let [<Literal>] Name        = "Name"
let [<Literal>] None        = "None"
let [<Literal>] Reference   = "Reference"

// Platform Constants

let [<Literal>] X86         = "x86"
let [<Literal>] X64         = "x64"
let [<Literal>] AnyCPU      = "AnyCPU"
let [<Literal>] PlatformVar = "$(Platform)"

// BuildAction Constants

let [<Literal>] Compile          = "Compile"
let [<Literal>] Content          = "Content"
let [<Literal>] Resource         = "Resource"
let [<Literal>] EmbeddedResource = "EmbeddedResource"

// CopyToOutputDirectory Constants
let [<Literal>] Never             = "Never"
let [<Literal>] Always            = "Always"
let [<Literal>] PreserveNewest    = "PreserveNewest"

// DebugType Constants

let [<Literal>] PdbOnly   = "PdbOnly"
let [<Literal>] Full      = "Full"


// OutputType Constants
let [<Literal>] Exe       = "Exe"
let [<Literal>] Winexe    = "Winexe"
let [<Literal>] Library   = "Library"
let [<Literal>] Module    = "Module"


// XML Attribute Name Constants

let [<Literal>] DefaultTargets  = "DefaultTargets"
let [<Literal>] ToolsVersion    = "ToolsVersion"
let [<Literal>] Include         = "Include"
let [<Literal>] Condition       = "Condition"

// MSBuild XML Element Constants

let [<Literal>] Project          = "Project"
let [<Literal>] ItemGroup        = "ItemGroup"
let [<Literal>] PropertyGroup    = "PropertyGroup"
let [<Literal>] ProjectReference = "ProjectReference"


// XML Property Constants (found in PropertyGroups)

let [<Literal>] AssemblyName                 = "AssemblyName"
let [<Literal>] RootNamespace                = "RootNamespace"
let [<Literal>] Configuration                = "Configuration"
let [<Literal>] Platform                     = "Platform"
let [<Literal>] SchemaVersion                = "SchemaVersion"
let [<Literal>] ProjectGuid                  = "ProjectGuid"
let [<Literal>] ProjectType                  = "ProjectType"
let [<Literal>] OutputType                   = "OutputType"
let [<Literal>] TargetFrameworkVersion       = "TargetFrameworkVersion"
let [<Literal>] TargetFrameworkProfile       = "TargetFrameworkProfile"
let [<Literal>] AutoGenerateBindingRedirects = "AutoGenerateBindingRedirects"
let [<Literal>] TargetFSharpCoreVersion      = "TargetFSharpCoreVersion"
let [<Literal>] DebugSymbols                 = "DebugSymbols"
let [<Literal>] DebugType                    = "DebugType"
let [<Literal>] Optimize                     = "Optimize"
let [<Literal>] Tailcalls                    = "Tailcalls"
let [<Literal>] OutputPath                   = "OutputPath"
let [<Literal>] CompilationConstants         = "DefineConstants"
let [<Literal>] WarningLevel                 = "WarningLevel"
let [<Literal>] PlatformTarget               = "PlatformTarget"
let [<Literal>] DocumentationFile            = "DocumentationFile"
let [<Literal>] Prefer32Bit                  = "Prefer32Bit"
let [<Literal>] OtherFlags                   = "OtherFlags"


// XML Elements

let [<Literal>] CopyToOutputDirectory   = "CopyToOutputDirectory"
let [<Literal>] HintPath                = "HintPath"
let [<Literal>] Private                 = "Private"
let [<Literal>] SpecificVersion         = "SpecificVersion"
let [<Literal>] Link                    = "Link"
let [<Literal>] Paket                   = "Paket"

let [<Literal>] XmlDecl                 = @"<?xml version='1.0' encoding='utf-8'?>"
let [<Literal>] Xmlns                   = "http://schemas.microsoft.com/developer/msbuild/2003"



