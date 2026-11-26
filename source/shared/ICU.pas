unit ICU;

{$I Shared.inc}

interface

uses
  CriticalSections,
  DynamicLibraries;

function TryGetICULibraryHandle(out AHandle: TLibHandle): Boolean;
function ICULibraryAvailable: Boolean;
function ICUGetProcAddress(const AName: string): Pointer;

{ Platform-independent helpers behind the runtime ICU version discovery (the
  library loading that uses them is Linux-only). They are pure string/directory
  logic, so they are available — and unit-tested — on every platform.
  ParseICUSoMajorVersion extracts the major from a versioned SONAME
  ('libicui18n.so.77' -> 77; '...so.76.1' -> 76; unversioned/garbage -> 0).
  HighestICUMajorVersionInDir returns the newest major among '<ABase>.<major>'
  files in ADir, or 0 when none is found. HighestICUMajorVersionInDirList does the
  same across a separator-delimited directory list (e.g. the entries of
  LD_LIBRARY_PATH), skipping empty segments. }
function ParseICUSoMajorVersion(const AFileName, ABase: string): Integer;
function HighestICUMajorVersionInDir(const ADir, ABase: string): Integer;
function HighestICUMajorVersionInDirList(const ADirList: string; ASeparator: Char;
  const ABase: string): Integer;
{ The same for ICU's own Windows build, whose libraries carry the major in their
  names: ParseICUDllMajorVersion('icuin79.dll', 'icuin') -> 79, case-insensitive,
  0 for any other name; the scans find the newest '<APrefix><major>.dll'. }
function ParseICUDllMajorVersion(const AFileName, APrefix: string): Integer;
function HighestICUDllMajorVersionInDir(const ADir, APrefix: string): Integer;
function HighestICUDllMajorVersionInDirList(const ADirList: string;
  ASeparator: Char; const APrefix: string): Integer;

implementation

uses
  SysUtils;

const
  {$IFDEF DARWIN}
  ICU_LIBRARY_NAME = 'libicucore.dylib';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  { ICU's own Windows build (icuin<major>.dll and icuuc<major>.dll, their
    symbols suffixed _<major>) is preferred: Windows' icu.dll exports only part
    of ICU's C API. The system library is the fallback. }
  ICU_WIN_I18N_PREFIX = 'icuin';
  ICU_WIN_UC_PREFIX = 'icuuc';
  ICU_LIBRARY_PRIMARY = 'icu.dll';
  ICU_LIBRARY_FALLBACK = 'icuin.dll';
  ICU_VERSION_MIN = 70;
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
  InitLock: TGocciaCriticalSection;
  {$IF DEFINED(LINUX) OR DEFINED(MSWINDOWS)}
  UCHandle: TLibHandle;
  ICUVersionSuffix: string;
  {$IFEND}

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

function HighestICUMajorVersionInDir(const ADir, ABase: string): Integer;
var
  SearchRec: TSearchRec;
  Major: Integer;
begin
  // Scan every entry and let ParseICUSoMajorVersion decide what matches, rather
  // than rely on FindFirst wildcard semantics (which differ across platforms), so
  // the helper behaves identically everywhere it is tested.
  Result := 0;
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*',
      faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          Major := ParseICUSoMajorVersion(SearchRec.Name, ABase);
          if Major > Result then
            Result := Major;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
end;

function HighestICUMajorVersionInDirList(const ADirList: string; ASeparator: Char;
  const ABase: string): Integer;
var
  Rest, Dir: string;
  SepPos, Major: Integer;
begin
  Result := 0;
  Rest := ADirList;
  while Rest <> '' do
  begin
    SepPos := Pos(ASeparator, Rest);
    if SepPos > 0 then
    begin
      Dir := Copy(Rest, 1, SepPos - 1);
      Delete(Rest, 1, SepPos);
    end
    else
    begin
      Dir := Rest;
      Rest := '';
    end;
    if Dir <> '' then
    begin
      Major := HighestICUMajorVersionInDir(Dir, ABase);
      if Major > Result then
        Result := Major;
    end;
  end;
end;

function ParseICUDllMajorVersion(const AFileName, APrefix: string): Integer;
var
  Name, Digits: string;
  Index: Integer;
begin
  Result := 0;
  Name := LowerCase(AFileName);
  if (Copy(Name, 1, Length(APrefix)) <> LowerCase(APrefix)) or
     (Length(Name) < Length(APrefix) + 5) or
     (Copy(Name, Length(Name) - 3, 4) <> '.dll') then
    Exit;
  Digits := Copy(Name, Length(APrefix) + 1, Length(Name) - Length(APrefix) - 4);
  for Index := 1 to Length(Digits) do
    if (Digits[Index] < '0') or (Digits[Index] > '9') then
      Exit;
  if Length(Digits) <= 4 then
    Result := StrToIntDef(Digits, 0);
end;

function HighestICUDllMajorVersionInDir(const ADir, APrefix: string): Integer;
var
  SearchRec: TSearchRec;
  Major: Integer;
begin
  Result := 0;
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*',
      faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          Major := ParseICUDllMajorVersion(SearchRec.Name, APrefix);
          if Major > Result then
            Result := Major;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
end;

function HighestICUDllMajorVersionInDirList(const ADirList: string;
  ASeparator: Char; const APrefix: string): Integer;
var
  Rest, Dir: string;
  SepPos, Major: Integer;
begin
  Result := 0;
  Rest := ADirList;
  while Rest <> '' do
  begin
    SepPos := Pos(ASeparator, Rest);
    if SepPos > 0 then
    begin
      Dir := Copy(Rest, 1, SepPos - 1);
      Delete(Rest, 1, SepPos);
    end
    else
    begin
      Dir := Rest;
      Rest := '';
    end;
    if Dir <> '' then
    begin
      Major := HighestICUDllMajorVersionInDir(Dir, APrefix);
      if Major > Result then
        Result := Major;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
function TryLoadVersionedWindowsICU(AVersion: Integer;
  out AHandle: TLibHandle): Boolean;
var
  UC: TLibHandle;
begin
  Result := False;
  AHandle := LoadLibrary(ICU_WIN_I18N_PREFIX + IntToStr(AVersion) + '.dll');
  if AHandle = NilHandle then
    Exit;
  UC := LoadLibrary(ICU_WIN_UC_PREFIX + IntToStr(AVersion) + '.dll');
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

{ ICU's own build beside the executable or on PATH, the newest major first (the
  loader's search order then finds it by name), else the system's icu.dll }
function TryLoadWindowsICU(out AHandle: TLibHandle): Boolean;
var
  Version, Highest, Major: Integer;
begin
  AHandle := NilHandle;
  UCHandle := NilHandle;
  ICUVersionSuffix := '';
  Highest := HighestICUDllMajorVersionInDir(ExtractFileDir(ParamStr(0)),
    ICU_WIN_I18N_PREFIX);
  Major := HighestICUDllMajorVersionInDirList(GetEnvironmentVariable('PATH'),
    ';', ICU_WIN_I18N_PREFIX);
  if Major > Highest then
    Highest := Major;
  for Version := Highest downto ICU_VERSION_MIN do
    if TryLoadVersionedWindowsICU(Version, AHandle) then
      Exit(True);
  AHandle := LoadLibrary(ICU_LIBRARY_PRIMARY);
  if AHandle = NilHandle then
    AHandle := LoadLibrary(ICU_LIBRARY_FALLBACK);
  Result := AHandle <> NilHandle;
end;
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

function DiscoverHighestICUMajorVersion: Integer;
var
  DirIndex, Major: Integer;
begin
  Result := 0;
  // Standard system locations.
  for DirIndex := Low(ICU_SCAN_DIRS) to High(ICU_SCAN_DIRS) do
  begin
    Major := HighestICUMajorVersionInDir(ICU_SCAN_DIRS[DirIndex], ICU_I18N_BASE);
    if Major > Result then
      Result := Major;
  end;
  // Directories the dynamic linker also searches via LD_LIBRARY_PATH, so an ICU
  // reachable only through an env override is still discovered — LoadLibrary then
  // resolves the chosen major by SONAME through the linker.
  Major := HighestICUMajorVersionInDirList(
    GetEnvironmentVariable('LD_LIBRARY_PATH'), ':', ICU_I18N_BASE);
  if Major > Result then
    Result := Major;
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
  CriticalSectionEnter(InitLock);
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
      LoadSucceeded := TryLoadWindowsICU(LoadedHandle);
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
    CriticalSectionLeave(InitLock);
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
  {$IF DEFINED(LINUX) OR DEFINED(MSWINDOWS)}
  VersionedName: string;
  {$IFEND}
begin
  Result := nil;
  if not TryGetICULibraryHandle(Handle) then
    Exit;
  {$IFNDEF LAKON}
  Result := GetProcedureAddress(Handle, AName);
  {$ENDIF}
  {$IF DEFINED(LINUX) OR DEFINED(MSWINDOWS)}
  if (Result = nil) and (ICUVersionSuffix <> '') then
  begin
    VersionedName := AName + ICUVersionSuffix;
    Result := GetProcedureAddress(Handle, VersionedName);
  end;
  if (Result = nil) and (UCHandle <> NilHandle) then
  begin
    Result := GetProcedureAddress(UCHandle, AName);
    if (Result = nil) and (ICUVersionSuffix <> '') then
    begin
      VersionedName := AName + ICUVersionSuffix;
      Result := GetProcedureAddress(UCHandle, VersionedName);
    end;
  end;
  {$IFEND}
end;

initialization
  CriticalSectionInit(InitLock);
  LibraryHandle := NilHandle;
  LoadAttempted := False;
  LoadSucceeded := False;
  {$IF DEFINED(LINUX) OR DEFINED(MSWINDOWS)}
  UCHandle := NilHandle;
  ICUVersionSuffix := '';
  {$IFEND}

finalization
  CriticalSectionDone(InitLock);
  {$IFNDEF LAKON}
  if LibraryHandle <> NilHandle then
    UnloadLibrary(LibraryHandle);
  {$ENDIF}
  {$IF DEFINED(LINUX) OR DEFINED(MSWINDOWS)}
  if UCHandle <> NilHandle then
    UnloadLibrary(UCHandle);
  {$IFEND}

end.
