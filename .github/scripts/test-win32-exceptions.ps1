# Run after the normal CI build has finalized Win32Exceptions.Test.exe.
# The control is the same executable with ONLY its load-config directory
# reference cleared. Keep it in RUNNER_TEMP and always delete it.
param(
    [string]$Executable = 'build/Win32Exceptions.Test.exe',
    [ValidateRange(1, 60)][int]$TimeoutSeconds = 15
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. "$PSScriptRoot/safeseh-control.ps1"

Invoke-SafeSEHControl $Executable {
    param($control)
    $fixed = $control.Executable
    $baseline = $control.Baseline
    $bytes = $control.Bytes
    $pe = $control.PeOffset
    $optional = $control.OptionalOffset
    $directory = $control.DirectoryOffset

    # Resolve the table through section headers; do not assume a fixed image base
    # or file layout. The build's safeseh --check supplies full manifest validation.
    $sectionCount = [BitConverter]::ToUInt16($bytes, $pe + 6)
    $sections = $optional + [BitConverter]::ToUInt16($bytes, $pe + 20)
    function Resolve-Rva([uint32]$Rva, [int]$Size) {
        for ($index = 0; $index -lt $sectionCount; $index++) {
            $section = $sections + $index * 40
            $start = [BitConverter]::ToUInt32($bytes, $section + 12)
            $length = [BitConverter]::ToUInt32($bytes, $section + 16)
            $raw = [BitConverter]::ToUInt32($bytes, $section + 20)
            $delta = [long]$Rva - $start
            if ($delta -ge 0 -and $delta + $Size -le $length -and
                [long]$raw + $delta + $Size -le $bytes.Length) {
                return [int]($raw + $delta)
            }
        }
        throw 'SafeSEH metadata is not backed by file bytes'
    }
    $config = Resolve-Rva ([BitConverter]::ToUInt32($bytes, $directory)) 72
    $count = [BitConverter]::ToUInt32($bytes, $config + 68)
    if ([BitConverter]::ToUInt32($bytes, $config) -ne 72 -or $count -ne 8) {
        throw 'Expected all seven system handlers and the signals handler'
    }
    $imageBase = [BitConverter]::ToUInt32($bytes, $optional + 28)
    $tableVa = [BitConverter]::ToUInt32($bytes, $config + 64)
    if ($tableVa -lt $imageBase) { throw 'Invalid SafeSEH table VA' }
    $table = Resolve-Rva ($tableVa - $imageBase) ([int]$count * 4)
    $previous = 0
    for ($index = 0; $index -lt $count; $index++) {
        $handler = [BitConverter]::ToUInt32($bytes, $table + $index * 4)
        if ($handler -le $previous) { throw 'Handler RVAs must be nonzero, unique and sorted' }
        $null = Resolve-Rva $handler 1
        $previous = $handler
    }

    function Invoke-Probe([string]$Path, [string]$Mode) {
        $info = [Diagnostics.ProcessStartInfo]::new()
        $info.FileName = $Path
        $info.Arguments = $Mode
        $info.UseShellExecute = $false
        $info.CreateNoWindow = $true
        $info.RedirectStandardOutput = $true
        $info.RedirectStandardError = $true
        $process = [Diagnostics.Process]::new()
        $process.StartInfo = $info
        try {
            $null = $process.Start()
            $stdout = $process.StandardOutput.ReadToEndAsync()
            $stderr = $process.StandardError.ReadToEndAsync()
            if (-not $process.WaitForExit($TimeoutSeconds * 1000)) {
                $process.Kill()
                $null = $process.WaitForExit(5000)
                throw "Exception probe timed out: $Mode"
            }
            [pscustomobject]@{
                ExitCode = $process.ExitCode
                Stdout = $stdout.GetAwaiter().GetResult().Replace("`r`n", "`n")
                Stderr = $stderr.GetAwaiter().GetResult().Replace("`r`n", "`n")
            }
        } finally {
            $process.Dispose()
        }
    }

    $modes = @('finally', 'except', 'typed-on', 'reraise', 'cleanup-raises',
        'except-raises', 'safecall', 'access-violation', 'integer-divide',
        'floating-point', 'default-pascal', 'default-hardware',
        'signals-access', 'signals-divide', 'thread-caught', 'thread-default',
        'signals-restore', 'signals-api-restore')
    foreach ($mode in $modes) {
        $expectedExit = if ($mode.StartsWith('default-') -or $mode -eq 'thread-default') { 217 } else { 0 }
        $before = Invoke-Probe $baseline $mode
        $after = Invoke-Probe $fixed $mode
        foreach ($result in @($before, $after)) {
            if ($result.ExitCode -ne $expectedExit -or
                $result.Stdout -cne "PASS $mode`n" -or $result.Stderr -cne '') {
                throw "Unexpected result for ${mode}: exit=$($result.ExitCode), stdout=$($result.Stdout), stderr=$($result.Stderr)"
            }
        }
        foreach ($field in @('ExitCode', 'Stdout', 'Stderr')) {
            if ($before.$field -cne $after.$field) { throw "$mode changed $field" }
        }
        Write-Host "PASS $mode before/after (exit $expectedExit)"
    }
}
