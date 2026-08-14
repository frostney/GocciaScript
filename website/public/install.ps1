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

  # The archive expands into a single top-level directory named after the
  # release (gocciascript-<version>-windows-<arch>) with the executables
  # sitting at its root. Resolve it by glob rather than by exact name, and
  # keep the legacy build\ and flat layouts as fallbacks.
  $Candidates = @(Join-Path $TempDir "gocciascript-$Version-windows-$Arch")
  $Candidates += @(
    Get-ChildItem -Path $TempDir -Directory -Filter "gocciascript-*" |
      ForEach-Object { $_.FullName }
  )
  $Candidates += (Join-Path $TempDir "build"), $TempDir

  $SrcDir = $Candidates |
    Where-Object { Test-Path -LiteralPath (Join-Path $_ "GocciaScriptLoader.exe") } |
    Select-Object -First 1
  if (-not $SrcDir) {
    throw "install.ps1: could not find GocciaScriptLoader.exe in $Asset"
  }

  # Every release archive carries all three; a missing one means a broken
  # download or a layout change, so fail rather than report a partial
  # install as success.
  foreach ($exe in @("GocciaScriptLoader", "GocciaTestRunner", "GocciaREPL")) {
    $src = Join-Path $SrcDir "$exe.exe"
    if (-not (Test-Path -LiteralPath $src)) {
      throw "install.ps1: $src not found in archive"
    }
  }
  foreach ($exe in @("GocciaScriptLoader", "GocciaTestRunner", "GocciaREPL")) {
    Move-Item -Force (Join-Path $SrcDir "$exe.exe") (Join-Path $InstallDir "$exe.exe")
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
