# Same-code comparison; the temporary control is never staged for shipping.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
. "$PSScriptRoot/safeseh-control.ps1"

Invoke-SafeSEHControl 'build/GocciaTestRunner.exe' {
    param($control)
    $runner = $control.Executable
    $baseline = $control.Baseline
    foreach ($variant in @('before', 'after')) {
        $executable = if ($variant -eq 'before') { $baseline } else { $runner }
        & $executable -P tests/built-ins/Proxy/prototype-cycle.js --silent --no-progress --timeout=120s "--output=safeseh-$variant.json"
        if ($LASTEXITCODE -ne 0) { throw "$variant measurement failed: $LASTEXITCODE" }
        $result = Get-Content "safeseh-$variant.json" -Raw | ConvertFrom-Json
        if (-not $result.ok -or $result.totalFiles -ne 1 -or $result.passed -le 0 -or
            $result.failed -ne 0 -or $result.suiteErrors -ne 0) {
            throw "$variant measurement did not pass"
        }
        Write-Host "$variant prototype-cycle.js execute: $($result.executeTimeNanoseconds / 1000000) ms"
    }
    $before = Get-Content 'safeseh-before.json' -Raw | ConvertFrom-Json
    $after = Get-Content 'safeseh-after.json' -Raw | ConvertFrom-Json
    foreach ($field in @('totalTests', 'passed', 'failed', 'skipped', 'assertions')) {
        if ($before.$field -ne $after.$field) { throw "Outcome changed: $field" }
    }
}
