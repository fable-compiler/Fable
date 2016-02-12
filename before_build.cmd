call setup.cmd
utils\fnr\fnr.exe --cl --find "FSharp.Compiler.Service.ProjectCrackerToolCopy" --replace "FSharpCompilerServiceProjectCrackerToolCopy" --dir "%cd%" --fileMask "*.targets" --includeSubDirectories
set PATH=C:\Program Files (x86)\MSBuild\14.0\Bin;%PATH%
