# GocciaScript installer — Windows.
#
# Usage:
#   irm https://gocciascript.dev/install.ps1 | iex
#
# Honors the following environment variables:
#   $env:GOCCIA_INSTALL_DIR  — where to drop the binaries
#                              (default: $env:USERPROFILE\bin)
#   $env:GOCCIA_VERSION      — tag to install (default: latest release)
#   $env:GOCCIA_REPO         — GitHub owner/repo
#                              (default: frostney/GocciaScript)
#
# The release ships a zip per arch; we expand it under a temp dir and
# move GocciaScriptLoader.exe, GocciaTestRunner.exe, GocciaREPL.exe
# into the install dir, then add the dir to the user's PATH.

$ErrorActionPreference = "Stop"

$Repo = if ($env:GOCCIA_REPO) { $env:GOCCIA_REPO } else { "frostney/GocciaScript" }
$InstallDir = if ($env:GOCCIA_INSTALL_DIR) { $env:GOCCIA_INSTALL_DIR } else { "$env:USERPROFILE\bin" }

# --- detect arch -----------------------------------------------------
$Arch = if ([Environment]::Is64BitOperatingSystem) { "x64" } else { "x86" }

# --- resolve version -------------------------------------------------
if ($env:GOCCIA_VERSION) {
  $Tag = $env:GOCCIA_VERSION
} else {
  $Latest = Invoke-RestMethod "https://api.github.com/repos/$Repo/releases/latest" -UseBasicParsing
  $Tag = $Latest.tag_name
  if (-not $Tag) {
    throw "install.ps1: could not resolve latest release"
  }
}
$Version = $Tag -replace '^v', ''

$Asset = "gocciascript-$Version-windows-$Arch.zip"
$Url = "https://github.com/$Repo/releases/download/$Tag/$Asset"

# --- download + extract ---------------------------------------------
$TempDir = Join-Path $env:TEMP "goccia-install-$([guid]::NewGuid())"
New-Item -ItemType Directory -Force -Path $TempDir | Out-Null

try {
  Write-Host "Downloading $Asset"
  $ZipPath = Join-Path $TempDir $Asset
  Invoke-WebRequest -Uri $Url -OutFile $ZipPath -UseBasicParsing

  Expand-Archive -Path $ZipPath -DestinationPath $TempDir -Force

  # --- install -------------------------------------------------------
  New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null

  foreach ($exe in @("GocciaScriptLoader", "GocciaTestRunner", "GocciaREPL")) {
    $src = Join-Path $TempDir "build\$exe.exe"
    if (Test-Path $src) {
      Move-Item -Force $src (Join-Path $InstallDir "$exe.exe")
    } else {
      Write-Warning "install.ps1: $src not found in archive — skipping"
    }
  }

  # --- ensure InstallDir is on user PATH -----------------------------
  $UserPath = [Environment]::GetEnvironmentVariable("Path", "User")
  $alreadyOnPath = ($UserPath -split ';') -contains $InstallDir
  if (-not $alreadyOnPath) {
    $newPath = if ($UserPath) { "$UserPath;$InstallDir" } else { $InstallDir }
    [Environment]::SetEnvironmentVariable("Path", $newPath, "User")
    Write-Host "Added $InstallDir to user PATH (open a new shell to pick it up)."
  }

  Write-Host ""
  Write-Host "GocciaScript $Version installed to $InstallDir"
} finally {
  Remove-Item -Recurse -Force $TempDir -ErrorAction SilentlyContinue
}
