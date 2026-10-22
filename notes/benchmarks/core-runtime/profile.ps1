param([ValidateSet('binding', 'js')][string]$Project = 'binding', [int]$Round = 1)
$ErrorActionPreference = 'Stop'
$root = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
Set-Location $root
$dest = Join-Path $root 'temp/core-profile'
New-Item -ItemType Directory -Force $dest | Out-Null
$fixture = if ($Project -eq 'binding') { 'temp/binding-probe/Binding.fsproj' } else { 'tests/Js/Main/Fable.Tests.fsproj' }
$name = "$Project-$Round"
$previousActivities = $env:FICKL_FCS_ACTIVITIES
$env:FICKL_FCS_ACTIVITIES = "$dest/$name-activities.csv"
try {
    $info = New-Object Diagnostics.ProcessStartInfo
    $info.FileName = 'dotnet'
    $info.Arguments = "`"$root/temp/core-aligned/baseline/fable.dll`" $fixture --outDir `"$dest/output/$Project`" --noCache --noRestore --noPackageCache --timings --exclude Fable.Core"
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.RedirectStandardOutput = $true
    $info.RedirectStandardError = $true
    $proc = [Diagnostics.Process]::Start($info)
    $stdout = $proc.StandardOutput.ReadToEndAsync()
    $stderr = $proc.StandardError.ReadToEndAsync()
    & rtk proxy "$root/temp/binding-probe/tools/dotnet-trace.exe" collect --process-id $proc.Id --profile dotnet-sampled-thread-time,gc-verbose --format Speedscope --output "$dest/$name.nettrace"
    $traceCode = $LASTEXITCODE
    $proc.WaitForExit()
    ($stdout.Result + $stderr.Result) | Set-Content "$dest/$name.log"
    if ($traceCode -ne 0 -or $proc.ExitCode -ne 0) { throw "Profile failed: $name" }
    $sha = [Security.Cryptography.SHA256]::Create()
    try {
        Get-ChildItem "$dest/output/$Project" -Recurse -Filter '*.js' | Sort-Object FullName | ForEach-Object {
            [pscustomobject]@{ Path = $_.FullName.Substring(("$dest/output/$Project").Length); SHA256 = [BitConverter]::ToString($sha.ComputeHash([IO.File]::ReadAllBytes($_.FullName))) }
        } | Export-Csv "$dest/$name-output.csv" -NoTypeInformation
    } finally { $sha.Dispose() }
    $stdout.Result
} finally {
    $env:FICKL_FCS_ACTIVITIES = $previousActivities
}
