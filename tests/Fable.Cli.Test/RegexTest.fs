module RegexTest

open Expecto
open Fable.Cli

let linuxDotnet3Info = """.NET Core SDK (reflecting any global.json):
Version:   3.0.100-preview8-013656
Commit:    8bf06ffc8d

Runtime Environment:
OS Name:     alpine
OS Version:  3.9
OS Platform: Linux
RID:         alpine.3.9-x64
Base Path:   /usr/share/dotnet/sdk/3.0.100-preview8-013656/

Host (useful for support):
 Version: 3.0.0-preview8-28405-07
 Commit:  d01b2fb7bc

.NET Core SDKs installed:
 3.0.100-preview8-013656 [/usr/share/dotnet/sdk]

.NET Core runtimes installed:
 Microsoft.AspNetCore.App 3.0.0-preview8.19405.7 [/usr/share/dotnet/shared/Microsoft.AspNetCore.App]
 Microsoft.NETCore.App 3.0.0-preview8-28405-07 [/usr/share/dotnet/shared/Microsoft.NETCore.App]

To install additional .NET Core runtimes or SDKs:
 https://aka.ms/dotnet-download
"""

let linuxDotnet2Info = """.NET Core SDK (reflecting any global.json):
Version:   2.2.301
Commit:    70d6be0814

Runtime Environment:
OS Name:     alpine
OS Version:  3.8
OS Platform: Linux
RID:         alpine.3.8-x64
Base Path:   /usr/share/dotnet/sdk/2.2.301/

Host (useful for support):
 Version: 2.2.6
 Commit:  7dac9b1b51

.NET Core SDKs installed:
 2.2.301 [/usr/share/dotnet/sdk]

.NET Core runtimes installed:
 Microsoft.AspNetCore.All 2.2.6 [/usr/share/dotnet/shared/Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.App 2.2.6 [/usr/share/dotnet/shared/Microsoft.AspNetCore.App]
 Microsoft.NETCore.App 2.2.6 [/usr/share/dotnet/shared/Microsoft.NETCore.App]

To install additional .NET Core runtimes or SDKs:
 https://aka.ms/dotnet-download"""

let windowsDotnet3Info = """.NET Core SDK (reflecting any global.json):
Version:   3.0.100-preview8-013656
Commit:    8bf06ffc8d

Runtime Environment:
OS Name:     Windows
OS Version:  10.0.18362
OS Platform: Windows
RID:         win10-x64
Base Path:   C:\Program Files\dotnet\sdk\3.0.100-preview8-013656\

Host (useful for support):
 Version: 3.0.0-preview8-28405-07
 Commit:  d01b2fb7bc

.NET Core SDKs installed:
 2.1.302 [C:\Program Files\dotnet\sdk]
 2.1.402 [C:\Program Files\dotnet\sdk]
 2.1.403 [C:\Program Files\dotnet\sdk]
 2.1.500 [C:\Program Files\dotnet\sdk]
 2.1.503 [C:\Program Files\dotnet\sdk]
 2.1.504 [C:\Program Files\dotnet\sdk]
 2.1.505 [C:\Program Files\dotnet\sdk]
 2.1.507 [C:\Program Files\dotnet\sdk]
 2.1.602 [C:\Program Files\dotnet\sdk]
 2.1.603 [C:\Program Files\dotnet\sdk]
 2.1.801 [C:\Program Files\dotnet\sdk]
 2.2.100 [C:\Program Files\dotnet\sdk]
 2.2.102 [C:\Program Files\dotnet\sdk]
 2.2.202 [C:\Program Files\dotnet\sdk]
 2.2.204 [C:\Program Files\dotnet\sdk]
 2.2.300 [C:\Program Files\dotnet\sdk]
 2.2.401 [C:\Program Files\dotnet\sdk]
 3.0.100-preview8-013656 [C:\Program Files\dotnet\sdk]

.NET Core runtimes installed:
 Microsoft.AspNetCore.All 2.1.2 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.4 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.7 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.8 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.9 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.10 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.11 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.12 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.0 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.1 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.3 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.App 2.1.2 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.4 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.7 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.8 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.9 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.10 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.11 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.12 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.0 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.1 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.3 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 3.0.0-preview8.19405.7 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.NETCore.App 2.1.2 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.4 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.5 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.6 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.7 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.8 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.9 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.10 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.11 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.12 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.0 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.1 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.3 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.5 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.6 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 3.0.0-preview8-28405-07 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.WindowsDesktop.App 3.0.0-preview8-28405-07 [C:\Program Files\dotnet\shared\Microsoft.WindowsDesktop.App]

To install additional .NET Core runtimes or SDKs:
 https://aka.ms/dotnet-download"""

let windowsDotnet2Info = """.NET Core SDK (reflecting any global.json):
Version:   2.2.401
Commit:    729b316c13

Runtime Environment:
OS Name:     Windows
OS Version:  10.0.18362
OS Platform: Windows
RID:         win10-x64
Base Path:   C:\Program Files\dotnet\sdk\2.2.401\

Host (useful for support):
 Version: 2.2.6
 Commit:  7dac9b1b51

.NET Core SDKs installed:
 2.1.302 [C:\Program Files\dotnet\sdk]
 2.1.402 [C:\Program Files\dotnet\sdk]
 2.1.403 [C:\Program Files\dotnet\sdk]
 2.1.500 [C:\Program Files\dotnet\sdk]
 2.1.503 [C:\Program Files\dotnet\sdk]
 2.1.504 [C:\Program Files\dotnet\sdk]
 2.1.505 [C:\Program Files\dotnet\sdk]
 2.1.507 [C:\Program Files\dotnet\sdk]
 2.1.602 [C:\Program Files\dotnet\sdk]
 2.1.603 [C:\Program Files\dotnet\sdk]
 2.1.801 [C:\Program Files\dotnet\sdk]
 2.2.100 [C:\Program Files\dotnet\sdk]
 2.2.102 [C:\Program Files\dotnet\sdk]
 2.2.202 [C:\Program Files\dotnet\sdk]
 2.2.204 [C:\Program Files\dotnet\sdk]
 2.2.300 [C:\Program Files\dotnet\sdk]
 2.2.401 [C:\Program Files\dotnet\sdk]

.NET Core runtimes installed:
 Microsoft.AspNetCore.All 2.1.2 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.4 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.7 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.8 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.9 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.10 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.11 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.1.12 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.0 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.1 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.3 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.All 2.2.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.All]
 Microsoft.AspNetCore.App 2.1.2 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.4 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.7 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.8 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.9 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.10 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.11 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.1.12 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.0 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.1 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.3 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.5 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.AspNetCore.App 2.2.6 [C:\Program Files\dotnet\shared\Microsoft.AspNetCore.App]
 Microsoft.NETCore.App 2.1.2 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.4 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.5 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.6 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.7 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.8 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.9 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.10 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.11 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.1.12 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.0 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.1 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.3 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.5 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]
 Microsoft.NETCore.App 2.2.6 [C:\Program Files\dotnet\shared\Microsoft.NETCore.App]

To install additional .NET Core runtimes or SDKs:
 https://aka.ms/dotnet-download"""

let linuxTests =
     testList "RegexTest with linux host" [
         testCase "Get Host Version with dotnet Core 2" <| fun () ->
             let actual = 
                Process.getHostVersionRegex linuxDotnet2Info
                |> fun m -> m.Groups.[1].Value
             Expect.equal actual "2.2.6" "Host Version of linuxDotnet2Info should be 2.2.6" 
         
         testCase "Get Host Version with dotnet Core 3" <| fun () ->
            let actual = 
               Process.getHostVersionRegex linuxDotnet3Info
               |> fun m -> m.Groups.[1].Value
            Expect.equal actual "3.0.0-preview8-28405-07" "Host Version of linuxDotnet3Info should be 3.0.0-preview8-28405-07"

         testCase "Get Base Path for dotnet Core 2" <| fun () ->
            let actual = 
                Process.getBasePathRegex linuxDotnet2Info
                |> fun m -> m.Groups.[1].Value
            Expect.equal actual "/usr/share/dotnet" "Base Path for linuxdDotnet2Info should be /usr/share/dotnet"
 
         testCase "Get Base Path for dotnet Core 3" <| fun () ->
           let actual = 
               Process.getBasePathRegex linuxDotnet3Info
               |> fun m -> m.Groups.[1].Value
           Expect.equal actual "/usr/share/dotnet" "Base Path for linuxdDotnet3Info should be /usr/share/dotnet"
     ]

let windowsTests = 
    testList "RegexTest with windows host" [
        testCase "Get Host Version with dotnet Core 2" <| fun () ->
            let actual = 
               Process.getHostVersionRegex windowsDotnet2Info
               |> fun m -> m.Groups.[1].Value
            Expect.equal actual "2.2.6" "Host Version of windowsDotnet2Info should be 2.2.6" 
    
        testCase "Get Host Version with dotnet Core 3" <| fun () ->
           let actual = 
              Process.getHostVersionRegex windowsDotnet3Info
              |> fun m -> m.Groups.[1].Value
           Expect.equal actual "3.0.0-preview8-28405-07" "Host Version of windowsDotnet3Info should be 3.0.0-preview8-28405-07"

        testCase "Get Base Path for dotnet Core 2" <| fun () ->
           let actual = 
               Process.getBasePathRegex windowsDotnet2Info
               |> fun m -> m.Groups.[1].Value
           Expect.equal actual @"C:\Program Files\dotnet" @"Base Path for windowsDotnet2Info should be C:\Program Files\dotnet"
 
        testCase "Get Base Path for dotnet Core 3" <| fun () ->
          let actual = 
              Process.getBasePathRegex windowsDotnet3Info
              |> fun m -> m.Groups.[1].Value
          Expect.equal actual @"C:\Program Files\dotnet" @"Base Path for windowsDotnet3Info should be C:\Program Files\dotnet"
    ]