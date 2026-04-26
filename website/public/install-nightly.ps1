# GocciaScript nightly installer — Windows.
#
# Usage:
#   irm https://gocciascript.dev/install-nightly.ps1 | iex
#
# Pulls the rolling `nightly` GitHub release instead of the latest
# tagged stable. Same env-var overrides as install.ps1
# ($env:GOCCIA_INSTALL_DIR, $env:GOCCIA_REPO).

$ErrorActionPreference = "Stop"

$Repo = if ($env:GOCCIA_REPO) { $env:GOCCIA_REPO } else { "frostney/GocciaScript" }
$InstallDir = if ($env:GOCCIA_INSTALL_DIR) { $env:GOCCIA_INSTALL_DIR } else { "$env:USERPROFILE\bin" }
$Arch = if ([Environment]::Is64BitOperatingSystem) { "x64" } else { "x86" }

$Tag = "nightly"
$Version = "nightly"
$Asset = "gocciascript-$Version-windows-$Arch.zip"
$Url = "https://github.com/$Repo/releases/download/$Tag/$Asset"

$TempDir = Join-Path $env:TEMP "goccia-install-$([guid]::NewGuid())"
New-Item -ItemType Directory -Force -Path $TempDir | Out-Null

try {
  Write-Host "Downloading $Asset (rolling nightly)"
  $ZipPath = Join-Path $TempDir $Asset
  Invoke-WebRequest -Uri $Url -OutFile $ZipPath -UseBasicParsing
  Expand-Archive -Path $ZipPath -DestinationPath $TempDir -Force

  New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null
  foreach ($exe in @("GocciaScriptLoader", "GocciaTestRunner", "GocciaREPL")) {
    $src = Join-Path $TempDir "build\$exe.exe"
    if (Test-Path $src) {
      Move-Item -Force $src (Join-Path $InstallDir "$exe.exe")
    } else {
      Write-Warning "install-nightly.ps1: $src not found in archive — skipping"
    }
  }

  $UserPath = [Environment]::GetEnvironmentVariable("Path", "User")
  $alreadyOnPath = ($UserPath -split ';') -contains $InstallDir
  if (-not $alreadyOnPath) {
    $newPath = if ($UserPath) { "$UserPath;$InstallDir" } else { $InstallDir }
    [Environment]::SetEnvironmentVariable("Path", $newPath, "User")
    Write-Host "Added $InstallDir to user PATH (open a new shell to pick it up)."
  }

  Write-Host ""
  Write-Host "GocciaScript nightly installed to $InstallDir"
} finally {
  Remove-Item -Recurse -Force $TempDir -ErrorAction SilentlyContinue
}
