$ErrorActionPreference = 'Stop'
function Get-LeftoverMSBuild {
    $processes = @(Get-CimInstance Win32_Process)
    $rider = @($processes | Where-Object { $_.Name -ieq 'RiderStub.exe' } | Select-Object -ExpandProperty ProcessId)
    $processes | Where-Object {
        # Rider launches active project evaluations repeatedly; these are not leftover benchmark workers.
        $_.ParentProcessId -notin $rider -and (
    $_.Name -ieq 'MSBuild.exe' -or
    ($_.Name -ieq 'dotnet.exe' -and $_.CommandLine -match '(?i)(MSBuild\.dll|\s+msbuild\s)')
        )
    }
}
$leftovers = @(Get-LeftoverMSBuild)
foreach ($worker in $leftovers) {
    Write-Output "Stopping leftover MSBuild process $($worker.ProcessId)"
    Stop-Process -Id $worker.ProcessId -Force -ErrorAction SilentlyContinue
}
if ($leftovers.Count -gt 0) { Wait-Process -Id $leftovers.ProcessId -Timeout 5 -ErrorAction SilentlyContinue }
Write-Output "MSBuild cleanup complete: $($leftovers.Count) processes stopped."
$remaining = @(Get-LeftoverMSBuild)
if ($remaining.Count -gt 0) { throw "$($remaining.Count) MSBuild processes remain after cleanup" }
Write-Output 'Verified: no leftover MSBuild workers remain (active Rider evaluations excluded).'
