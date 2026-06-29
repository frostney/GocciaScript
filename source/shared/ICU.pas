unit ICU;

{$I Shared.inc}

interface

uses
  DynLibs;

function TryGetICULibraryHandle(out AHandle: TLibHandle): Boolean;
function ICULibraryAvailable: Boolean;
function ICUGetProcAddress(const AName: string): Pointer;

{$IFDEF LINUX}
{ Exposed for unit tests of the runtime ICU version discovery.
  ParseICUSoMajorVersion extracts the major from a versioned SONAME
  ('libicui18n.so.77' -> 77; 'libicui18n.so.76.1' -> 76; unversioned/garbage -> 0).
  HighestICUMajorVersionInDir returns the newest ICU major whose i18n library is
  present in ADir, or 0 when none is found. }
function ParseICUSoMajorVersion(const AFileName, ABase: string): Integer;
function HighestICUMajorVersionInDir(const ADir: string): Integer;
{$ENDIF}

implementation

uses
  SysUtils;

const
  {$IFDEF DARWIN}
  ICU_LIBRARY_NAME = 'libicucore.dylib';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ICU_LIBRARY_PRIMARY = 'icu.dll';
  ICU_LIBRARY_FALLBACK = 'icuin.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  ICU_I18N_BASE = 'libicui18n.so';
  ICU_UC_BASE = 'libicuuc.so';
  // Oldest ICU major that still exports the symbols the engine resolves. There is
  // deliberately NO maximum: the newest installed major is discovered at runtime
  // (DiscoverHighestICUMajorVersion), so a newer ICU — 77 and beyond — is picked
  // up with no code change.
  ICU_VERSION_MIN = 70;
  {$ENDIF}

var
  LibraryHandle: TLibHandle;
  LoadAttempted: Boolean;
  LoadSucceeded: Boolean;
  InitLock: TRTLCriticalSection;
  {$IFDEF LINUX}
  UCHandle: TLibHandle;
  ICUVersionSuffix: string;
  {$ENDIF}

{$IFDEF LINUX}
const
  // Standard locations distributions install the versioned ICU runtime into,
  // across the Linux targets the project builds (Debian/Ubuntu multiarch for
  // x86_64 and aarch64, plus the generic lib dirs other distros use). These are
  // only scanned to learn which ICU majors are present; LoadLibrary still
  // resolves the final path through the dynamic linker, and a non-existent dir
  // is simply skipped, so listing both arch triplets is harmless.
  ICU_SCAN_DIRS: array[0..6] of string = (
    '/usr/lib/x86_64-linux-gnu', '/usr/lib/aarch64-linux-gnu',
    '/lib/x86_64-linux-gnu', '/lib/aarch64-linux-gnu',
    '/usr/lib64', '/usr/lib', '/usr/local/lib');

function ParseICUSoMajorVersion(const AFileName, ABase: string): Integer;
var
  Prefix, Digits: string;
  Index: Integer;
begin
  Result := 0;
  Prefix := ABase + '.';
  if Copy(AFileName, 1, Length(Prefix)) <> Prefix then
    Exit;
  Digits := '';
  Index := Length(Prefix) + 1;
  while (Index <= Length(AFileName)) and
        (AFileName[Index] >= '0') and (AFileName[Index] <= '9') do
  begin
    Digits := Digits + AFileName[Index];
    Inc(Index);
  end;
  if Digits <> '' then
    Result := StrToIntDef(Digits, 0);
end;

function HighestICUMajorVersionInDir(const ADir: string): Integer;
var
  SearchRec: TSearchRec;
  Major: Integer;
begin
  Result := 0;
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + ICU_I18N_BASE + '.*',
      faAnyFile, SearchRec) = 0 then
    try
      repeat
        Major := ParseICUSoMajorVersion(SearchRec.Name, ICU_I18N_BASE);
        if Major > Result then
          Result := Major;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
end;

function DiscoverHighestICUMajorVersion: Integer;
var
  DirIndex, Major: Integer;
begin
  Result := 0;
  for DirIndex := Low(ICU_SCAN_DIRS) to High(ICU_SCAN_DIRS) do
  begin
    Major := HighestICUMajorVersionInDir(ICU_SCAN_DIRS[DirIndex]);
    if Major > Result then
      Result := Major;
  end;
end;

function TryLoadVersionedICU(AVersion: Integer; out AHandle: TLibHandle): Boolean;
var
  UC: TLibHandle;
begin
  Result := False;
  AHandle := LoadLibrary(ICU_I18N_BASE + '.' + IntToStr(AVersion));
  if AHandle = NilHandle then
    Exit;
  UC := LoadLibrary(ICU_UC_BASE + '.' + IntToStr(AVersion));
  if UC = NilHandle then
  begin
    UnloadLibrary(AHandle);
    AHandle := NilHandle;
    Exit;
  end;
  UCHandle := UC;
  ICUVersionSuffix := '_' + IntToStr(AVersion);
  Result := True;
end;

function TryLoadLinuxICU(out AHandle: TLibHandle): Boolean;
var
  Version, Highest: Integer;
begin
  AHandle := NilHandle;
  UCHandle := NilHandle;

  // Try the newest installed ICU first, then any older co-installed majors down
  // to the compatibility floor. The ceiling is whatever is installed, so newer
  // releases (77+) need no code change.
  Highest := DiscoverHighestICUMajorVersion;
  for Version := Highest downto ICU_VERSION_MIN do
    if TryLoadVersionedICU(Version, AHandle) then
    begin
      Result := True;
      Exit;
    end;

  // Fallback: an unversioned SONAME (a -dev symlink, or a toolchain that keeps
  // unversioned exported symbols).
  AHandle := LoadLibrary(ICU_I18N_BASE);
  if AHandle <> NilHandle then
  begin
    UCHandle := LoadLibrary(ICU_UC_BASE);
    if UCHandle = NilHandle then
    begin
      UnloadLibrary(AHandle);
      AHandle := NilHandle;
    end
    else
    begin
      ICUVersionSuffix := '';
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;
{$ENDIF}

function TryGetICULibraryHandle(out AHandle: TLibHandle): Boolean;
var
  LoadedHandle: TLibHandle;
begin
  EnterCriticalSection(InitLock);
  try
    if not LoadAttempted then
    begin
      LoadAttempted := True;
      LoadedHandle := NilHandle;

      {$IFDEF DARWIN}
      LoadedHandle := LoadLibrary(ICU_LIBRARY_NAME);
      LoadSucceeded := LoadedHandle <> NilHandle;
      {$ENDIF}

      {$IFDEF MSWINDOWS}
      LoadedHandle := LoadLibrary(ICU_LIBRARY_PRIMARY);
      if LoadedHandle = NilHandle then
        LoadedHandle := LoadLibrary(ICU_LIBRARY_FALLBACK);
      LoadSucceeded := LoadedHandle <> NilHandle;
      {$ENDIF}

      {$IFDEF LINUX}
      LoadSucceeded := TryLoadLinuxICU(LoadedHandle);
      {$ENDIF}

      if LoadSucceeded then
        LibraryHandle := LoadedHandle;
    end;

    AHandle := LibraryHandle;
    Result := LoadSucceeded;
  finally
    LeaveCriticalSection(InitLock);
  end;
end;

function ICULibraryAvailable: Boolean;
var
  Dummy: TLibHandle;
begin
  Result := TryGetICULibraryHandle(Dummy);
end;

function ICUGetProcAddress(const AName: string): Pointer;
var
  Handle: TLibHandle;
  {$IFDEF LINUX}
  VersionedName: string;
  {$ENDIF}
begin
  Result := nil;
  if not TryGetICULibraryHandle(Handle) then
    Exit;
  Result := GetProcAddress(Handle, AName);
  {$IFDEF LINUX}
  if (Result = nil) and (ICUVersionSuffix <> '') then
  begin
    VersionedName := AName + ICUVersionSuffix;
    Result := GetProcAddress(Handle, VersionedName);
  end;
  if (Result = nil) and (UCHandle <> NilHandle) then
  begin
    Result := GetProcAddress(UCHandle, AName);
    if (Result = nil) and (ICUVersionSuffix <> '') then
    begin
      VersionedName := AName + ICUVersionSuffix;
      Result := GetProcAddress(UCHandle, VersionedName);
    end;
  end;
  {$ENDIF}
end;

initialization
  InitCriticalSection(InitLock);
  LibraryHandle := NilHandle;
  LoadAttempted := False;
  LoadSucceeded := False;
  {$IFDEF LINUX}
  UCHandle := NilHandle;
  ICUVersionSuffix := '';
  {$ENDIF}

finalization
  DoneCriticalSection(InitLock);
  if LibraryHandle <> NilHandle then
    UnloadLibrary(LibraryHandle);
  {$IFDEF LINUX}
  if UCHandle <> NilHandle then
    UnloadLibrary(UCHandle);
  {$ENDIF}

end.
