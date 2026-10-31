# Run an assertion against the finalized executable and a disposable copy whose
# only changed bytes are the load-configuration directory reference.
function Invoke-SafeSEHControl([string]$Executable, [scriptblock]$Action) {
    if (-not $env:RUNNER_TEMP) { throw 'RUNNER_TEMP is required for the temporary control' }
    $fixed = (Resolve-Path $Executable).Path
    $originalHash = (Get-FileHash $fixed -Algorithm SHA256).Hash
    $bytes = [IO.File]::ReadAllBytes($fixed)
    if ($bytes.Length -lt 64 -or [BitConverter]::ToUInt16($bytes, 0) -ne 0x5a4d) {
        throw 'Expected a PE executable'
    }
    $pe = [BitConverter]::ToInt32($bytes, 60)
    $optional = $pe + 24
    $directory = $optional + 96 + 10 * 8
    if ($pe -lt 64 -or $directory + 8 -gt $bytes.Length -or
        [BitConverter]::ToUInt32($bytes, $pe) -ne 0x4550 -or
        [BitConverter]::ToUInt16($bytes, $pe + 4) -ne 0x14c -or
        [BitConverter]::ToUInt16($bytes, $pe + 20) -lt 184 -or
        [long]$optional + [BitConverter]::ToUInt16($bytes, $pe + 20) -gt $bytes.Length -or
        [BitConverter]::ToUInt16($bytes, $optional) -ne 0x10b -or
        [BitConverter]::ToUInt32($bytes, $optional + 92) -lt 11 -or
        [BitConverter]::ToUInt32($bytes, $directory) -eq 0 -or
        [BitConverter]::ToUInt32($bytes, $directory + 4) -ne 72) {
        throw 'Expected an i386 executable with a 72-byte SafeSEH load configuration'
    }
    $baseline = Join-Path $env:RUNNER_TEMP ('safeseh-control-' + [Guid]::NewGuid().ToString('N') + '.exe')
    $created = $false
    try {
        $copy = $bytes.Clone()
        [Array]::Clear($copy, $directory, 8)
        $stream = [IO.File]::Open($baseline, [IO.FileMode]::CreateNew, [IO.FileAccess]::Write, [IO.FileShare]::None)
        $created = $true
        try { $stream.Write($copy, 0, $copy.Length) } finally { $stream.Dispose() }
        & $Action ([pscustomobject]@{
            Executable = $fixed
            Baseline = $baseline
            Bytes = $bytes
            PeOffset = $pe
            OptionalOffset = $optional
            DirectoryOffset = $directory
        })
    } finally {
        try {
            if ($created) { Remove-Item $baseline -ErrorAction Stop }
        } finally {
            if ((Get-FileHash $fixed -Algorithm SHA256).Hash -ne $originalHash) {
                throw 'The finalized executable changed during testing'
            }
        }
    }
}
