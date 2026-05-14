unit ICU;

{$I Shared.inc}

interface

uses
  DynLibs;

function TryGetICULibraryHandle(out AHandle: TLibHandle): Boolean;
function ICULibraryAvailable: Boolean;
function ICUGetProcAddress(const AName: string): Pointer;

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
  ICU_VERSION_MAX = 76;
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
function TryLoadLinuxICU(out AHandle: TLibHandle): Boolean;
var
  Version: Integer;
  LibName, UCLibName: string;
begin
  Result := False;
  AHandle := NilHandle;
  UCHandle := NilHandle;

  for Version := ICU_VERSION_MAX downto ICU_VERSION_MIN do
  begin
    AHandle := NilHandle;
    UCHandle := NilHandle;
    LibName := ICU_I18N_BASE + '.' + IntToStr(Version);
    UCLibName := ICU_UC_BASE + '.' + IntToStr(Version);
    AHandle := LoadLibrary(LibName);
    if AHandle <> NilHandle then
    begin
      UCHandle := LoadLibrary(UCLibName);
      if UCHandle = NilHandle then
      begin
        UnloadLibrary(AHandle);
        AHandle := NilHandle;
        Continue;
      end;
      ICUVersionSuffix := '_' + IntToStr(Version);
      Result := True;
      Exit;
    end;
  end;

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
    end;
  end;
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
