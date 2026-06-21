unit Goccia.Temporal.TimeZone;

{$I Goccia.inc}

interface

type
  TTemporalTimeZoneDisambiguation = (ttzdCompatible, ttzdEarlier, ttzdLater, ttzdReject);

function GetSystemTimeZoneId: string;
function IsValidTimeZone(const ATimeZone: string): Boolean;
function IsSupportedCanonicalTimeZoneIdentifier(const ATimeZone: string): Boolean;
function TryCanonicalizeTimeZoneIdentifierCase(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;
function TimeZoneIdentifiersEqual(const ALeft, ARight: string): Boolean;
function GetUtcOffsetSeconds(const ATimeZone: string; const AEpochSeconds: Int64): Integer;
function TryGetTimeZoneTransitionMilliseconds(const ATimeZone: string;
  const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer;
  const ANext: Boolean; out ATransitionEpochMilliseconds: Int64): Boolean;
function LocalDateTimeToEpochMilliseconds(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string): Int64;
function LocalDateTimeToEpochMillisecondsWithDisambiguation(const AYear, AMonth,
  ADay, AHour, AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string;
  const ADisambiguation: TTemporalTimeZoneDisambiguation): Int64;
function StartOfDayToEpochMilliseconds(const AYear, AMonth, ADay: Integer;
  const ATimeZone: string): Int64;
function FormatOffsetString(const AOffsetSeconds: Integer): string;
function ParseOffsetString(const AStr: string; out AOffsetSeconds: Integer): Boolean;
procedure ClearTimeZoneCache;

implementation

uses
  SysUtils,
  Goccia.Temporal.Utils,
  Goccia.Temporal.TimeZoneData,
  Goccia.Values.ErrorHelper,
  TimeZoneInformationFile,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  DynLibs,
  ICU,
  {$ENDIF}
  Classes;

const
  UTC_TIMEZONE_ID = 'UTC';
  GOCCIA_TZDIR_ENV = 'GOCCIA_TZDIR';
  UNIX_ZONEINFO_PATH = '/usr/share/zoneinfo';
  MACOS_DEFAULT_ZONEINFO_PATH = '/usr/share/zoneinfo.default';
  {$IFDEF UNIX}
  LOCALTIME_PATH = '/etc/localtime';
  {$ENDIF}
  MINUTES_PER_HOUR = 60;
  SECONDS_PER_MINUTE = 60;
  MILLISECONDS_PER_SECOND = 1000;
  MILLISECONDS_PER_MINUTE = SECONDS_PER_MINUTE * MILLISECONDS_PER_SECOND;
  MILLISECONDS_PER_HOUR = MINUTES_PER_HOUR * MILLISECONDS_PER_MINUTE;
  MILLISECONDS_PER_DAY = 24 * MILLISECONDS_PER_HOUR;
  MAX_SYMLINK_LENGTH = 1024;
  {$IFDEF MSWINDOWS}
  ICU_LIBRARY = 'icu.dll';
  ICU_I18N_LIBRARY = 'icuin.dll';
  ICU_SUCCESS = 0;
  ICU_TIMEZONE_ID_CAPACITY = 128;
  ICU_CAL_GREGORIAN = 1;
  ICU_CAL_ZONE_OFFSET = 15;
  ICU_CAL_DST_OFFSET = 16;
  ICU_TZ_TRANSITION_NEXT = 0;
  ICU_TRANSITION_WINDOW_START_SECONDS = -5364662400; // 1800-01-01T00:00:00Z
  ICU_TRANSITION_WINDOW_END_SECONDS = 7258118400; // 2200-01-01T00:00:00Z
  ICU_MAX_TRANSITION_COUNT = 4096;
  {$ENDIF}

{$IFDEF MSWINDOWS}
type
  TICUCalendar = Pointer;
  TICUDate = Double;
  TICUErrorCode = LongInt;
  TICUCalendarOpen = function(const AZoneId: PWideChar; const AZoneIdLength: LongInt;
    const ALocale: PAnsiChar; const ACalendarType: LongInt;
    var AStatus: TICUErrorCode): TICUCalendar; cdecl;
  TICUCalendarClose = procedure(const ACalendar: TICUCalendar); cdecl;
  TICUCalendarSetMillis = procedure(const ACalendar: TICUCalendar;
    const AMilliseconds: TICUDate; var AStatus: TICUErrorCode); cdecl;
  TICUCalendarGet = function(const ACalendar: TICUCalendar; const AField: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TICUCalendarGetTimeZoneTransitionDate = function(const ACalendar: TICUCalendar;
    const ATransitionType: LongInt; var ATransition: TICUDate;
    var AStatus: TICUErrorCode): ByteBool; cdecl;
  TICUCalendarGetCanonicalTimeZoneID = function(const AId: PWideChar;
    const AIdLength: LongInt; AResult: PWideChar; const AResultCapacity: LongInt;
    var AIsSystemId: ByteBool; var AStatus: TICUErrorCode): LongInt; cdecl;
  TICUCalendarGetDefaultTimeZone = function(AResult: PWideChar;
    const AResultCapacity: LongInt; var AStatus: TICUErrorCode): LongInt; cdecl;

  TWindowsICU = record
    Handle: TLibHandle;
    OpenCalendar: TICUCalendarOpen;
    CloseCalendar: TICUCalendarClose;
    SetMillis: TICUCalendarSetMillis;
    GetField: TICUCalendarGet;
    GetTimeZoneTransitionDate: TICUCalendarGetTimeZoneTransitionDate;
    GetCanonicalTimeZoneID: TICUCalendarGetCanonicalTimeZoneID;
    GetDefaultTimeZone: TICUCalendarGetDefaultTimeZone;
  end;
{$ENDIF}

type
  TTimeZonePathCacheEntry = record
    Identifier: string;
    Path: string;
    Found: Boolean;
  end;

  TTimeZoneCaseCacheEntry = record
    Identifier: string;
    CanonicalIdentifier: string;
    Found: Boolean;
  end;

  TTimeZoneEpochMillisecondsArray = array of Int64;

threadvar
  CachedTimeZones: array of TTimeZoneInformationData;
  CachedTimeZoneCount: Integer;
  CachedTimeZonePaths: array of TTimeZonePathCacheEntry;
  CachedTimeZonePathCount: Integer;
  CachedTimeZoneCases: array of TTimeZoneCaseCacheEntry;
  CachedTimeZoneCaseCount: Integer;

function ASCIIEqualsIgnoreCase(const ALeft, ARight: string): Boolean;
begin
  Result := SameText(ALeft, ARight);
end;

{$IFDEF MSWINDOWS}
var
  WindowsICU: TWindowsICU;
  WindowsICULoadAttempted: Boolean;
  WindowsICUAvailable: Boolean;
  WindowsICUInitLock: TRTLCriticalSection;

function ICUSucceeded(const AStatus: TICUErrorCode): Boolean;
begin
  Result := AStatus <= ICU_SUCCESS;
end;

function TryLoadCalendarFunctions(const AHandle: TLibHandle;
  out AICU: TWindowsICU): Boolean;
var
  Symbol: Pointer;
begin
  Result := False;
  FillChar(AICU, SizeOf(AICU), 0);
  AICU.Handle := AHandle;

  Symbol := ICUGetProcAddress('ucal_open');
  if not Assigned(Symbol) then Exit;
  AICU.OpenCalendar := TICUCalendarOpen(Symbol);

  Symbol := ICUGetProcAddress('ucal_close');
  if not Assigned(Symbol) then Exit;
  AICU.CloseCalendar := TICUCalendarClose(Symbol);

  Symbol := ICUGetProcAddress('ucal_setMillis');
  if not Assigned(Symbol) then Exit;
  AICU.SetMillis := TICUCalendarSetMillis(Symbol);

  Symbol := ICUGetProcAddress('ucal_get');
  if not Assigned(Symbol) then Exit;
  AICU.GetField := TICUCalendarGet(Symbol);

  Symbol := ICUGetProcAddress('ucal_getTimeZoneTransitionDate');
  if not Assigned(Symbol) then Exit;
  AICU.GetTimeZoneTransitionDate := TICUCalendarGetTimeZoneTransitionDate(Symbol);

  Symbol := ICUGetProcAddress('ucal_getCanonicalTimeZoneID');
  if not Assigned(Symbol) then Exit;
  AICU.GetCanonicalTimeZoneID := TICUCalendarGetCanonicalTimeZoneID(Symbol);

  Symbol := ICUGetProcAddress('ucal_getDefaultTimeZone');
  if not Assigned(Symbol) then Exit;
  AICU.GetDefaultTimeZone := TICUCalendarGetDefaultTimeZone(Symbol);

  Result := True;
end;

function TryGetWindowsICU(out AICU: TWindowsICU): Boolean;
var
  Handle: TLibHandle;
  LoadedICU: TWindowsICU;
begin
  EnterCriticalSection(WindowsICUInitLock);
  try
    if not WindowsICULoadAttempted then
    begin
      WindowsICULoadAttempted := True;
      if TryGetICULibraryHandle(Handle) then
        WindowsICUAvailable := TryLoadCalendarFunctions(Handle, LoadedICU);
      if WindowsICUAvailable then
        WindowsICU := LoadedICU;
    end;

    AICU := WindowsICU;
    Result := WindowsICUAvailable;
  finally
    LeaveCriticalSection(WindowsICUInitLock);
  end;
end;

function UnicodeBufferToString(const ABuffer: array of WideChar;
  const ALength: Integer): string;
var
  Value: UnicodeString;
begin
  SetLength(Value, ALength);
  if ALength > 0 then
    Move(ABuffer[0], Value[1], ALength * SizeOf(WideChar));
  Result := string(Value);
end;

function TryCanonicalizeWindowsICUTimeZone(const AICU: TWindowsICU;
  const ATimeZone: string; out ACanonicalTimeZone: string): Boolean;
var
  InputId: UnicodeString;
  Buffer: array[0..ICU_TIMEZONE_ID_CAPACITY - 1] of WideChar;
  Status: TICUErrorCode;
  IsSystemId: ByteBool;
  ResultLength: LongInt;
begin
  Result := False;
  ACanonicalTimeZone := '';
  InputId := UnicodeString(ATimeZone);
  FillChar(Buffer, SizeOf(Buffer), 0);
  Status := ICU_SUCCESS;
  IsSystemId := False;

  ResultLength := AICU.GetCanonicalTimeZoneID(PWideChar(InputId), Length(InputId),
    @Buffer[0], ICU_TIMEZONE_ID_CAPACITY, IsSystemId, Status);
  if (not ICUSucceeded(Status)) or (not IsSystemId) or
     (ResultLength <= 0) or (ResultLength >= ICU_TIMEZONE_ID_CAPACITY) then
    Exit;

  ACanonicalTimeZone := UnicodeBufferToString(Buffer, ResultLength);
  Result := ACanonicalTimeZone <> 'Etc/Unknown';
end;

function TryOpenWindowsICUCalendar(const AICU: TWindowsICU;
  const ATimeZone: string; out ACalendar: TICUCalendar): Boolean;
var
  ZoneId: UnicodeString;
  Status: TICUErrorCode;
begin
  Result := False;
  ACalendar := nil;
  ZoneId := UnicodeString(ATimeZone);
  Status := ICU_SUCCESS;
  ACalendar := AICU.OpenCalendar(PWideChar(ZoneId), Length(ZoneId), nil,
    ICU_CAL_GREGORIAN, Status);
  Result := ICUSucceeded(Status) and Assigned(ACalendar);
end;

function EpochSecondsToICUMilliseconds(const AEpochSeconds: Int64): TICUDate;
var
  Seconds: TICUDate;
begin
  Seconds := AEpochSeconds;
  Result := Seconds * MILLISECONDS_PER_SECOND;
end;

function ICUMillisecondsToEpochSeconds(const AMilliseconds: TICUDate): Int64;
var
  Milliseconds: Int64;
begin
  Milliseconds := Round(AMilliseconds);
  Result := Milliseconds div MILLISECONDS_PER_SECOND;
  if (Milliseconds < 0) and (Milliseconds mod MILLISECONDS_PER_SECOND <> 0) then
    Dec(Result);
end;

function TryGetWindowsICUOffsetSeconds(const AICU: TWindowsICU;
  const ACalendar: TICUCalendar; const AEpochSeconds: Int64;
  out AOffsetSeconds: Integer): Boolean;
var
  Status: TICUErrorCode;
  ZoneOffset, DstOffset: LongInt;
begin
  Result := False;
  AOffsetSeconds := 0;

  Status := ICU_SUCCESS;
  AICU.SetMillis(ACalendar, EpochSecondsToICUMilliseconds(AEpochSeconds), Status);
  if not ICUSucceeded(Status) then
    Exit;

  Status := ICU_SUCCESS;
  ZoneOffset := AICU.GetField(ACalendar, ICU_CAL_ZONE_OFFSET, Status);
  if not ICUSucceeded(Status) then
    Exit;

  Status := ICU_SUCCESS;
  DstOffset := AICU.GetField(ACalendar, ICU_CAL_DST_OFFSET, Status);
  if not ICUSucceeded(Status) then
    Exit;

  AOffsetSeconds := (ZoneOffset + DstOffset) div MILLISECONDS_PER_SECOND;
  Result := True;
end;

procedure AppendWindowsICUTransition(var AData: TTimeZoneInformationData;
  const ATransitionEpochSeconds: Int64; const AOffsetSeconds: Integer);
var
  TransitionIndex: Integer;
begin
  TransitionIndex := Length(AData.Transitions);
  SetLength(AData.Transitions, TransitionIndex + 1);
  AData.Transitions[TransitionIndex].TransitionEpochSeconds := ATransitionEpochSeconds;
  AData.Transitions[TransitionIndex].UtcOffsetSeconds := AOffsetSeconds;
end;

function TryLoadTimeZoneDataFromWindowsICU(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  ICU: TWindowsICU;
  Calendar: TICUCalendar;
  CanonicalTimeZone: string;
  CurrentEpochSeconds, TransitionEpochSeconds: Int64;
  TransitionMilliseconds: TICUDate;
  OffsetSeconds, TransitionCount: Integer;
  Status: TICUErrorCode;
begin
  Result := False;

  if not TryGetWindowsICU(ICU) then
    Exit;

  if not TryCanonicalizeWindowsICUTimeZone(ICU, ATimeZone, CanonicalTimeZone) then
    Exit;

  if not TryOpenWindowsICUCalendar(ICU, CanonicalTimeZone, Calendar) then
    Exit;

  try
    AData.Identifier := ATimeZone;
    SetLength(AData.Transitions, 0);

    if not TryGetWindowsICUOffsetSeconds(ICU, Calendar,
      ICU_TRANSITION_WINDOW_START_SECONDS, AData.DefaultOffsetSeconds) then
      Exit;

    CurrentEpochSeconds := ICU_TRANSITION_WINDOW_START_SECONDS;
    TransitionCount := 0;
    while (CurrentEpochSeconds < ICU_TRANSITION_WINDOW_END_SECONDS) and
          (TransitionCount < ICU_MAX_TRANSITION_COUNT) do
    begin
      Status := ICU_SUCCESS;
      ICU.SetMillis(Calendar, EpochSecondsToICUMilliseconds(CurrentEpochSeconds), Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      TransitionMilliseconds := 0;
      if not ICU.GetTimeZoneTransitionDate(Calendar, ICU_TZ_TRANSITION_NEXT,
        TransitionMilliseconds, Status) then
        Break;

      if not ICUSucceeded(Status) then
        Exit;

      TransitionEpochSeconds := ICUMillisecondsToEpochSeconds(TransitionMilliseconds);
      if (TransitionEpochSeconds <= CurrentEpochSeconds) or
         (TransitionEpochSeconds > ICU_TRANSITION_WINDOW_END_SECONDS) then
        Break;

      if not TryGetWindowsICUOffsetSeconds(ICU, Calendar,
        TransitionEpochSeconds + 1, OffsetSeconds) then
        Exit;

      AppendWindowsICUTransition(AData, TransitionEpochSeconds, OffsetSeconds);
      CurrentEpochSeconds := TransitionEpochSeconds + 1;
      Inc(TransitionCount);
    end;

    Result := True;
  finally
    ICU.CloseCalendar(Calendar);
  end;
end;

function TryGetWindowsICUDefaultTimeZoneId(out ATimeZone: string): Boolean;
var
  ICU: TWindowsICU;
  Buffer: array[0..ICU_TIMEZONE_ID_CAPACITY - 1] of WideChar;
  Status: TICUErrorCode;
  ResultLength: LongInt;
begin
  Result := False;
  ATimeZone := '';

  if not TryGetWindowsICU(ICU) then
    Exit;

  FillChar(Buffer, SizeOf(Buffer), 0);
  Status := ICU_SUCCESS;
  ResultLength := ICU.GetDefaultTimeZone(@Buffer[0], ICU_TIMEZONE_ID_CAPACITY, Status);
  if (not ICUSucceeded(Status)) or (ResultLength <= 0) or
     (ResultLength >= ICU_TIMEZONE_ID_CAPACITY) then
    Exit;

  ATimeZone := UnicodeBufferToString(Buffer, ResultLength);
  Result := (ATimeZone <> '') and (ATimeZone <> 'Etc/Unknown');
end;
{$ENDIF}

function FindCachedTimeZone(const ATimeZone: string): Integer;
var
  I: Integer;
begin
  for I := 0 to CachedTimeZoneCount - 1 do
  begin
    if CachedTimeZones[I].Identifier = ATimeZone then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function FindCachedTimeZonePath(const ATimeZone: string): Integer;
var
  I: Integer;
begin
  for I := 0 to CachedTimeZonePathCount - 1 do
  begin
    if CachedTimeZonePaths[I].Identifier = ATimeZone then
      Exit(I);
  end;
  Result := -1;
end;

procedure CacheTimeZonePath(const ATimeZone, APath: string; const AFound: Boolean);
begin
  if CachedTimeZonePathCount >= Length(CachedTimeZonePaths) then
    SetLength(CachedTimeZonePaths, CachedTimeZonePathCount + 16);
  CachedTimeZonePaths[CachedTimeZonePathCount].Identifier := ATimeZone;
  CachedTimeZonePaths[CachedTimeZonePathCount].Path := APath;
  CachedTimeZonePaths[CachedTimeZonePathCount].Found := AFound;
  Inc(CachedTimeZonePathCount);
end;

function FindCachedTimeZoneCase(const ATimeZone: string): Integer;
var
  I: Integer;
begin
  for I := 0 to CachedTimeZoneCaseCount - 1 do
  begin
    if CachedTimeZoneCases[I].Identifier = ATimeZone then
      Exit(I);
  end;
  Result := -1;
end;

procedure CacheTimeZoneCase(const ATimeZone, ACanonicalTimeZone: string;
  const AFound: Boolean);
begin
  if CachedTimeZoneCaseCount >= Length(CachedTimeZoneCases) then
    SetLength(CachedTimeZoneCases, CachedTimeZoneCaseCount + 16);
  CachedTimeZoneCases[CachedTimeZoneCaseCount].Identifier := ATimeZone;
  CachedTimeZoneCases[CachedTimeZoneCaseCount].CanonicalIdentifier :=
    ACanonicalTimeZone;
  CachedTimeZoneCases[CachedTimeZoneCaseCount].Found := AFound;
  Inc(CachedTimeZoneCaseCount);
end;

function IsValidTimeZoneName(const AName: string): Boolean;
var
  I, SegStart: Integer;
begin
  Result := False;
  if Length(AName) = 0 then Exit;
  // Reject absolute paths
  if AName[1] = '/' then Exit;
  // Reject Windows path separators, drive paths, and alternate data streams.
  if Pos('\', AName) > 0 then Exit;
  if Pos(':', AName) > 0 then Exit;
  // Check each path segment for . and ..
  SegStart := 1;
  for I := 1 to Length(AName) + 1 do
  begin
    if (I > Length(AName)) or (AName[I] = '/') then
    begin
      // Check segment
      if I = SegStart then Exit; // empty segment (double slash)
      if (I - SegStart = 1) and (AName[SegStart] = '.') then Exit; // "."
      if (I - SegStart = 2) and (AName[SegStart] = '.') and (AName[SegStart + 1] = '.') then Exit; // ".."
      SegStart := I + 1;
    end;
  end;
  Result := True;
end;

function TimeZoneNameToFilePath(const ATimeZone: string): string;
begin
  Result := StringReplace(ATimeZone, '/', PathDelim, [rfReplaceAll]);
end;

function TimeZonePathInDirectory(const ADirectory, ATimeZone: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory)) +
    TimeZoneNameToFilePath(ATimeZone);
end;

function TryFindCaseInsensitiveDirectoryEntry(const ADirectory,
  AName: string; out AActualName: string): Boolean;
var
  Search: TSearchRec;
  Status: Integer;
begin
  Result := False;
  AActualName := '';
  if (ADirectory = '') or not DirectoryExists(ADirectory) then
    Exit;

  Status := FindFirst(IncludeTrailingPathDelimiter(ADirectory) + '*',
    faAnyFile, Search);
  try
    while Status = 0 do
    begin
      if (Search.Name <> '.') and (Search.Name <> '..') and
         ASCIIEqualsIgnoreCase(Search.Name, AName) then
      begin
        AActualName := Search.Name;
        Exit(True);
      end;
      Status := FindNext(Search);
    end;
  finally
    FindClose(Search);
  end;
end;

function TryCanonicalizeTimeZoneInDirectory(const ADirectory,
  ATimeZone: string; out ACanonicalTimeZone: string): Boolean;
var
  CurrentDirectory, Segment, ActualName: string;
  StartIndex, I: Integer;
begin
  Result := False;
  ACanonicalTimeZone := '';
  if (ADirectory = '') or (ATimeZone = '') then
    Exit;

  CurrentDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));
  StartIndex := 1;
  for I := 1 to Length(ATimeZone) + 1 do
  begin
    if (I > Length(ATimeZone)) or (ATimeZone[I] = '/') then
    begin
      Segment := Copy(ATimeZone, StartIndex, I - StartIndex);
      if (Segment = '') or
         not TryFindCaseInsensitiveDirectoryEntry(CurrentDirectory, Segment,
           ActualName) then
        Exit;

      if ACanonicalTimeZone <> '' then
        ACanonicalTimeZone := ACanonicalTimeZone + '/';
      ACanonicalTimeZone := ACanonicalTimeZone + ActualName;
      if I <= Length(ATimeZone) then
        CurrentDirectory := IncludeTrailingPathDelimiter(CurrentDirectory +
          ActualName);
      StartIndex := I + 1;
    end;
  end;

  Result := ACanonicalTimeZone <> '';
end;

function TryCanonicalizeTimeZoneIdentifierCase(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;
var
  EnvDirectory: string;
  CachedIndex: Integer;
begin
  CachedIndex := FindCachedTimeZoneCase(ATimeZone);
  if CachedIndex >= 0 then
  begin
    ACanonicalTimeZone := CachedTimeZoneCases[CachedIndex].CanonicalIdentifier;
    Exit(CachedTimeZoneCases[CachedIndex].Found);
  end;

  Result := False;
  ACanonicalTimeZone := '';
  if not IsValidTimeZoneName(ATimeZone) then
  begin
    CacheTimeZoneCase(ATimeZone, '', False);
    Exit;
  end;

  EnvDirectory := GetEnvironmentVariable(GOCCIA_TZDIR_ENV);
  if TryCanonicalizeTimeZoneInDirectory(EnvDirectory, ATimeZone,
    ACanonicalTimeZone) then
    Result := True;

  {$IFDEF UNIX}
  if (not Result) and TryCanonicalizeTimeZoneInDirectory(UNIX_ZONEINFO_PATH,
    ATimeZone, ACanonicalTimeZone) then
    Result := True;

  if (not Result) and TryCanonicalizeTimeZoneInDirectory(MACOS_DEFAULT_ZONEINFO_PATH,
    ATimeZone, ACanonicalTimeZone) then
    Result := True;
  {$ENDIF}

  CacheTimeZoneCase(ATimeZone, ACanonicalTimeZone, Result);
end;

function TryGetTimeZonePathInDirectory(const ADirectory, ATimeZone: string;
  out APath: string): Boolean;
var
  CanonicalTimeZone: string;
begin
  Result := False;
  APath := '';
  if TryCanonicalizeTimeZoneInDirectory(ADirectory, ATimeZone,
    CanonicalTimeZone) then
  begin
    APath := TimeZonePathInDirectory(ADirectory, CanonicalTimeZone);
    Result := FileExists(APath);
  end;
end;

function TryGetKnownTimeZonePath(const ATimeZone: string; out APath: string): Boolean;
var
  EnvDirectory: string;
  CachedIndex: Integer;
begin
  CachedIndex := FindCachedTimeZonePath(ATimeZone);
  if CachedIndex >= 0 then
  begin
    APath := CachedTimeZonePaths[CachedIndex].Path;
    Exit(CachedTimeZonePaths[CachedIndex].Found);
  end;

  Result := False;
  APath := '';
  EnvDirectory := GetEnvironmentVariable(GOCCIA_TZDIR_ENV);
  if TryGetTimeZonePathInDirectory(EnvDirectory, ATimeZone, APath) then
    Result := True;

  {$IFDEF UNIX}
  if (not Result) and TryGetTimeZonePathInDirectory(MACOS_DEFAULT_ZONEINFO_PATH,
    ATimeZone, APath) then
    Result := True;

  if (not Result) and TryGetTimeZonePathInDirectory(UNIX_ZONEINFO_PATH,
    ATimeZone, APath) then
    Result := True;
  {$ENDIF}

  CacheTimeZonePath(ATimeZone, APath, Result);
end;

function IsUTCPrimaryEquivalent(const ATimeZone: string): Boolean;
begin
  Result := ASCIIEqualsIgnoreCase(ATimeZone, 'UTC') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/UTC') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT+0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT-0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT+0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT-0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/Greenwich') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Greenwich') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/UCT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'UCT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/Universal') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Universal') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/Zulu') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Zulu');
end;

function IsSupportedCanonicalTimeZoneIdentifier(const ATimeZone: string): Boolean;
begin
  Result := (ATimeZone = 'Africa/Abidjan') or
    (ATimeZone = 'Africa/Cairo') or
    (ATimeZone = 'Africa/Casablanca') or
    (ATimeZone = 'Africa/Johannesburg') or
    (ATimeZone = 'Africa/Lagos') or
    (ATimeZone = 'Africa/Nairobi') or
    (ATimeZone = 'America/Anchorage') or
    (ATimeZone = 'America/Argentina/Buenos_Aires') or
    (ATimeZone = 'America/Bogota') or
    (ATimeZone = 'America/Chicago') or
    (ATimeZone = 'America/Denver') or
    (ATimeZone = 'America/Halifax') or
    (ATimeZone = 'America/Los_Angeles') or
    (ATimeZone = 'America/Mexico_City') or
    (ATimeZone = 'America/New_York') or
    (ATimeZone = 'America/Phoenix') or
    (ATimeZone = 'America/Santiago') or
    (ATimeZone = 'America/Sao_Paulo') or
    (ATimeZone = 'America/St_Johns') or
    (ATimeZone = 'America/Toronto') or
    (ATimeZone = 'America/Vancouver') or
    (ATimeZone = 'Asia/Baghdad') or
    (ATimeZone = 'Asia/Bangkok') or
    (ATimeZone = 'Asia/Calcutta') or
    (ATimeZone = 'Asia/Dhaka') or
    (ATimeZone = 'Asia/Dubai') or
    (ATimeZone = 'Asia/Hong_Kong') or
    (ATimeZone = 'Asia/Istanbul') or
    (ATimeZone = 'Asia/Jakarta') or
    (ATimeZone = 'Asia/Jerusalem') or
    (ATimeZone = 'Asia/Karachi') or
    (ATimeZone = 'Asia/Kathmandu') or
    (ATimeZone = 'Asia/Riyadh') or
    (ATimeZone = 'Asia/Seoul') or
    (ATimeZone = 'Asia/Shanghai') or
    (ATimeZone = 'Asia/Singapore') or
    (ATimeZone = 'Asia/Taipei') or
    (ATimeZone = 'Asia/Tehran') or
    (ATimeZone = 'Asia/Tokyo') or
    (ATimeZone = 'Atlantic/Reykjavik') or
    (ATimeZone = 'Australia/Melbourne') or
    (ATimeZone = 'Australia/Perth') or
    (ATimeZone = 'Australia/Sydney') or
    (ATimeZone = 'Europe/Amsterdam') or
    (ATimeZone = 'Europe/Athens') or
    (ATimeZone = 'Europe/Belgrade') or
    (ATimeZone = 'Europe/Berlin') or
    (ATimeZone = 'Europe/Brussels') or
    (ATimeZone = 'Europe/Bucharest') or
    (ATimeZone = 'Europe/Budapest') or
    (ATimeZone = 'Europe/Copenhagen') or
    (ATimeZone = 'Europe/Dublin') or
    (ATimeZone = 'Europe/Helsinki') or
    (ATimeZone = 'Europe/Kyiv') or
    (ATimeZone = 'Europe/Lisbon') or
    (ATimeZone = 'Europe/London') or
    (ATimeZone = 'Europe/Madrid') or
    (ATimeZone = 'Europe/Moscow') or
    (ATimeZone = 'Europe/Oslo') or
    (ATimeZone = 'Europe/Paris') or
    (ATimeZone = 'Europe/Prague') or
    (ATimeZone = 'Europe/Rome') or
    (ATimeZone = 'Europe/Stockholm') or
    (ATimeZone = 'Europe/Vienna') or
    (ATimeZone = 'Europe/Warsaw') or
    (ATimeZone = 'Europe/Zurich') or
    (ATimeZone = 'Pacific/Auckland') or
    (ATimeZone = 'Pacific/Fiji') or
    (ATimeZone = 'Pacific/Honolulu') or
    (ATimeZone = 'UTC');
end;

function TimeZoneFilesEqual(const ALeft, ARight: string): Boolean;
{$IFDEF UNIX}
var
  LeftPath, RightPath: string;
  LeftStat, RightStat: TStat;
{$ENDIF}
begin
  Result := False;
  {$IFDEF UNIX}
  if not TryGetKnownTimeZonePath(ALeft, LeftPath) or
     not TryGetKnownTimeZonePath(ARight, RightPath) then
    Exit;
  if (fpStat(PChar(LeftPath), LeftStat) <> 0) or
     (fpStat(PChar(RightPath), RightStat) <> 0) then
    Exit;
  Result := (LeftStat.st_dev = RightStat.st_dev) and
    (LeftStat.st_ino = RightStat.st_ino);
  {$ENDIF}
end;

function TimeZoneIdentifiersEqual(const ALeft, ARight: string): Boolean;
var
  LeftOffset, RightOffset: Integer;
  LeftIsOffset, RightIsOffset: Boolean;
begin
  if ALeft = ARight then
    Exit(True);

  LeftIsOffset := ParseOffsetString(ALeft, LeftOffset);
  RightIsOffset := ParseOffsetString(ARight, RightOffset);
  if LeftIsOffset or RightIsOffset then
    Exit(LeftIsOffset and RightIsOffset and (LeftOffset = RightOffset));

  if IsUTCPrimaryEquivalent(ALeft) and IsUTCPrimaryEquivalent(ARight) then
    Exit(True);

  if IsSupportedCanonicalTimeZoneIdentifier(ALeft) and
     IsSupportedCanonicalTimeZoneIdentifier(ARight) then
    Exit(False);

  Result := TimeZoneFilesEqual(ALeft, ARight);
end;

function TryLoadTimeZoneDataFromFile(const ATimeZone, AFilePath: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  Stream: TFileStream;
  Buffer: TBytes;
  FileSize: Int64;
  BufferSize: Integer;
begin
  Result := False;

  if not FileExists(AFilePath) then
    Exit;

  Stream := nil;
  try
    Stream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
    FileSize := Stream.Size;
    if FileSize > High(Integer) then
    begin
      Stream.Free;
      Exit;
    end;

    BufferSize := Integer(FileSize);
    SetLength(Buffer, BufferSize);
    if BufferSize > 0 then
      Stream.ReadBuffer(Buffer[0], BufferSize);
    Stream.Free;
    Stream := nil;

    Result := TryParseTimeZoneInformationFile(ATimeZone, Buffer, AData);
  except
    Stream.Free;
  end;
end;

function TryLoadTimeZoneDataFromDirectory(const ADirectory, ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
begin
  Result := False;
  if ADirectory = '' then
    Exit;

  Result := TryLoadTimeZoneDataFromFile(ATimeZone,
    TimeZonePathInDirectory(ADirectory, ATimeZone), AData);
end;

function TryLoadTimeZoneDataFromEmbeddedData(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  Buffer: TBytes;
begin
  Result := False;

  if not TryGetEmbeddedTimeZoneFile(ATimeZone, Buffer) then
    Exit;

  Result := TryParseTimeZoneInformationFile(ATimeZone, Buffer, AData);
end;

function TryLoadTimeZoneDataFromKnownLocations(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  EnvDirectory: string;
begin
  Result := False;

  EnvDirectory := GetEnvironmentVariable(GOCCIA_TZDIR_ENV);
  if TryLoadTimeZoneDataFromDirectory(EnvDirectory, ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;

  {$IFDEF UNIX}
  if TryLoadTimeZoneDataFromDirectory(UNIX_ZONEINFO_PATH, ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;

  if TryLoadTimeZoneDataFromDirectory(MACOS_DEFAULT_ZONEINFO_PATH, ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  if TryLoadTimeZoneDataFromWindowsICU(ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;
  {$ENDIF}

  if TryLoadTimeZoneDataFromEmbeddedData(ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;
end;

function LoadTimeZoneData(const ATimeZone: string; out AData: TTimeZoneInformationData): Boolean;
begin
  Result := False;

  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    AData.Identifier := UTC_TIMEZONE_ID;
    SetLength(AData.Transitions, 0);
    AData.DefaultOffsetSeconds := 0;
    Result := True;
    Exit;
  end;

  if not IsValidTimeZoneName(ATimeZone) then
    Exit;

  Result := TryLoadTimeZoneDataFromKnownLocations(ATimeZone, AData);
end;

function GetOrLoadTimeZoneData(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  Index: Integer;
begin
  Index := FindCachedTimeZone(ATimeZone);
  if Index >= 0 then
  begin
    AData := CachedTimeZones[Index];
    Result := True;
    Exit;
  end;

  Result := LoadTimeZoneData(ATimeZone, AData);
  if not Result then
    Exit;

  // Add to cache
  if CachedTimeZoneCount >= Length(CachedTimeZones) then
    SetLength(CachedTimeZones, CachedTimeZoneCount + 4);
  CachedTimeZones[CachedTimeZoneCount] := AData;
  Inc(CachedTimeZoneCount);
end;

function BinarySearchTransitions(const ATransitions: TTimeZoneInformationTransitionArray;
  const ACount: Integer; const AEpochSeconds: Int64): Integer;
var
  Low, High, Mid: Integer;
begin
  // Find the last transition where TransitionEpochSeconds <= AEpochSeconds
  Result := -1;
  Low := 0;
  High := ACount - 1;
  while Low <= High do
  begin
    Mid := Low + (High - Low) div 2;
    if ATransitions[Mid].TransitionEpochSeconds <= AEpochSeconds then
    begin
      Result := Mid;
      Low := Mid + 1;
    end
    else
      High := Mid - 1;
  end;
end;

function GetSystemTimeZoneId: string;
{$IFDEF UNIX}
var
  LinkTarget: string;
  PrefixPos: Integer;
  Prefix: string;
  LocalTimeDirectory: string;
begin
  // Read the symlink target of /etc/localtime
  SetLength(LinkTarget, MAX_SYMLINK_LENGTH);
  PrefixPos := fpReadLink(LOCALTIME_PATH, @LinkTarget[1], MAX_SYMLINK_LENGTH);
  if PrefixPos > 0 then
  begin
    SetLength(LinkTarget, PrefixPos);
    if (Length(LinkTarget) > 0) and (LinkTarget[1] <> PathDelim) then
    begin
      LocalTimeDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(LOCALTIME_PATH));
      LinkTarget := ExpandFileName(LocalTimeDirectory + LinkTarget);
    end
    else
      LinkTarget := ExpandFileName(LinkTarget);

    Prefix := IncludeTrailingPathDelimiter(UNIX_ZONEINFO_PATH);
    PrefixPos := Pos(Prefix, LinkTarget);
    if PrefixPos = 1 then
    begin
      Result := Copy(LinkTarget, Length(Prefix) + 1, Length(LinkTarget) -
        Length(Prefix));
      Exit;
    end;

    Prefix := IncludeTrailingPathDelimiter(MACOS_DEFAULT_ZONEINFO_PATH);
    PrefixPos := Pos(Prefix, LinkTarget);
    if PrefixPos = 1 then
    begin
      Result := Copy(LinkTarget, Length(Prefix) + 1, Length(LinkTarget) -
        Length(Prefix));
      Exit;
    end;
  end;
  Result := UTC_TIMEZONE_ID;
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  TimeZone: string;
begin
  if TryGetWindowsICUDefaultTimeZoneId(TimeZone) then
  begin
    Result := TimeZone;
    Exit;
  end;
  Result := UTC_TIMEZONE_ID;
end;
{$ELSE}
begin
  Result := UTC_TIMEZONE_ID;
end;
{$ENDIF}
{$ENDIF}

function IsValidTimeZone(const ATimeZone: string): Boolean;
var
  Data: TTimeZoneInformationData;
  Dummy: Integer;
begin
  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    Result := True;
    Exit;
  end;
  // Accept offset strings like +05:30 as valid timezone IDs
  if ParseOffsetString(ATimeZone, Dummy) then
  begin
    Result := True;
    Exit;
  end;
  if not IsValidTimeZoneName(ATimeZone) then
  begin
    Result := False;
    Exit;
  end;

  Result := GetOrLoadTimeZoneData(ATimeZone, Data);
end;

function GetUtcOffsetSeconds(const ATimeZone: string; const AEpochSeconds: Int64): Integer;
var
  Data: TTimeZoneInformationData;
  Index: Integer;
  OffsetSecs: Integer;
begin
  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    Result := 0;
    Exit;
  end;

  // Handle offset-based timezone IDs (e.g. +05:30, -08:00)
  if ParseOffsetString(ATimeZone, OffsetSecs) then
  begin
    Result := OffsetSecs;
    Exit;
  end;

  if not GetOrLoadTimeZoneData(ATimeZone, Data) then
  begin
    Result := 0;
    Exit;
  end;

  if Length(Data.Transitions) = 0 then
  begin
    Result := Data.DefaultOffsetSeconds;
    Exit;
  end;

  Index := BinarySearchTransitions(Data.Transitions, Length(Data.Transitions), AEpochSeconds);
  if Index < 0 then
    Result := Data.DefaultOffsetSeconds
  else
    Result := Data.Transitions[Index].UtcOffsetSeconds;
end;

function FloorEpochSecond(const AEpochMilliseconds: Int64): Int64;
begin
  Result := AEpochMilliseconds div MILLISECONDS_PER_SECOND;
  if (AEpochMilliseconds < 0) and
     (AEpochMilliseconds mod MILLISECONDS_PER_SECOND <> 0) then
    Dec(Result);
end;

function IsTransitionAfterInstant(const ATransitionEpochSeconds,
  AEpochMilliseconds: Int64): Boolean;
begin
  Result := ATransitionEpochSeconds > FloorEpochSecond(AEpochMilliseconds);
end;

function IsTransitionBeforeInstant(const ATransitionEpochSeconds,
  AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer): Boolean;
var
  EpochSeconds: Int64;
begin
  EpochSeconds := FloorEpochSecond(AEpochMilliseconds);
  if ATransitionEpochSeconds < EpochSeconds then
    Exit(True);
  if ATransitionEpochSeconds > EpochSeconds then
    Exit(False);

  Result := (AEpochMilliseconds <> ATransitionEpochSeconds * MILLISECONDS_PER_SECOND) or
    (ASubMillisecondNanoseconds <> 0);
end;

function IsOffsetTransition(const AData: TTimeZoneInformationData;
  const AIndex: Integer): Boolean;
var
  PreviousOffsetSeconds: Integer;
begin
  if AIndex <= 0 then
    PreviousOffsetSeconds := AData.DefaultOffsetSeconds
  else
    PreviousOffsetSeconds := AData.Transitions[AIndex - 1].UtcOffsetSeconds;

  Result := AData.Transitions[AIndex].UtcOffsetSeconds <> PreviousOffsetSeconds;
end;

function TryGetTimeZoneTransitionMilliseconds(const ATimeZone: string;
  const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer;
  const ANext: Boolean; out ATransitionEpochMilliseconds: Int64): Boolean;
var
  Data: TTimeZoneInformationData;
  OffsetSecs: Integer;
  I: Integer;
begin
  Result := False;
  ATransitionEpochMilliseconds := 0;

  if (ATimeZone = UTC_TIMEZONE_ID) or ParseOffsetString(ATimeZone, OffsetSecs) then
    Exit;
  if not GetOrLoadTimeZoneData(ATimeZone, Data) then
    Exit;

  if ANext then
  begin
    for I := 0 to Length(Data.Transitions) - 1 do
    begin
      if IsTransitionAfterInstant(Data.Transitions[I].TransitionEpochSeconds,
        AEpochMilliseconds) and IsOffsetTransition(Data, I) then
      begin
        ATransitionEpochMilliseconds :=
          Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
        Exit(True);
      end;
    end;
  end
  else
  begin
    for I := Length(Data.Transitions) - 1 downto 0 do
    begin
      if IsTransitionBeforeInstant(Data.Transitions[I].TransitionEpochSeconds,
        AEpochMilliseconds, ASubMillisecondNanoseconds) and
        IsOffsetTransition(Data, I) then
      begin
        ATransitionEpochMilliseconds :=
          Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
        Exit(True);
      end;
    end;
  end;
end;

function LocalDateTimeToLocalEpochMilliseconds(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond: Integer): Int64;
begin
  Result := DateToEpochDays(AYear, AMonth, ADay) * MILLISECONDS_PER_DAY +
            Int64(AHour) * MILLISECONDS_PER_HOUR +
            Int64(AMinute) * MILLISECONDS_PER_MINUTE +
            Int64(ASecond) * MILLISECONDS_PER_SECOND +
            AMillisecond;
end;

function EpochMillisecondsMatchesLocalDateTime(const AEpochMilliseconds: Int64;
  const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Integer;
  const ATimeZone: string): Boolean;
var
  OffsetSeconds: Integer;
  LocalEpochMilliseconds, EpochDays, RemainingMilliseconds: Int64;
  DateRec: TTemporalDateRecord;
  LocalHour, LocalMinute, LocalSecond, LocalMillisecond: Integer;
begin
  OffsetSeconds := GetUtcOffsetSeconds(ATimeZone,
    FloorEpochSecond(AEpochMilliseconds));
  LocalEpochMilliseconds := AEpochMilliseconds +
    Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND;

  EpochDays := LocalEpochMilliseconds div MILLISECONDS_PER_DAY;
  RemainingMilliseconds := LocalEpochMilliseconds mod MILLISECONDS_PER_DAY;
  if RemainingMilliseconds < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMilliseconds, MILLISECONDS_PER_DAY);
  end;

  DateRec := EpochDaysToDate(EpochDays);
  LocalHour := Integer(RemainingMilliseconds div MILLISECONDS_PER_HOUR);
  RemainingMilliseconds := RemainingMilliseconds mod MILLISECONDS_PER_HOUR;
  LocalMinute := Integer(RemainingMilliseconds div MILLISECONDS_PER_MINUTE);
  RemainingMilliseconds := RemainingMilliseconds mod MILLISECONDS_PER_MINUTE;
  LocalSecond := Integer(RemainingMilliseconds div MILLISECONDS_PER_SECOND);
  LocalMillisecond := Integer(RemainingMilliseconds mod MILLISECONDS_PER_SECOND);

  Result := (DateRec.Year = AYear) and (DateRec.Month = AMonth) and
    (DateRec.Day = ADay) and (LocalHour = AHour) and
    (LocalMinute = AMinute) and (LocalSecond = ASecond) and
    (LocalMillisecond = AMillisecond);
end;

procedure AddPossibleEpochMilliseconds(var AInstants: TTimeZoneEpochMillisecondsArray;
  var ACount: Integer; const AEpochMilliseconds: Int64);
var
  I, InsertIndex: Integer;
begin
  InsertIndex := ACount;
  for I := 0 to ACount - 1 do
  begin
    if AInstants[I] = AEpochMilliseconds then
      Exit;
    if AInstants[I] > AEpochMilliseconds then
    begin
      InsertIndex := I;
      Break;
    end;
  end;

  if ACount >= Length(AInstants) then
    SetLength(AInstants, ACount + 4);
  for I := ACount downto InsertIndex + 1 do
    AInstants[I] := AInstants[I - 1];
  AInstants[InsertIndex] := AEpochMilliseconds;
  Inc(ACount);
end;

procedure ConsiderOffsetForLocalDateTime(var AInstants: TTimeZoneEpochMillisecondsArray;
  var ACount: Integer; const ALocalEpochMilliseconds: Int64;
  const AOffsetSeconds: Integer; const AYear, AMonth, ADay, AHour, AMinute,
  ASecond, AMillisecond: Integer; const ATimeZone: string);
var
  CandidateEpochMilliseconds: Int64;
begin
  CandidateEpochMilliseconds := ALocalEpochMilliseconds -
    Int64(AOffsetSeconds) * MILLISECONDS_PER_SECOND;
  if EpochMillisecondsMatchesLocalDateTime(CandidateEpochMilliseconds,
    AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond,
    ATimeZone) then
    AddPossibleEpochMilliseconds(AInstants, ACount, CandidateEpochMilliseconds);
end;

procedure GetPossibleEpochMillisecondsForLocalDateTime(const AYear, AMonth,
  ADay, AHour, AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string;
  out AInstants: TTimeZoneEpochMillisecondsArray; out ACount: Integer);
var
  Data: TTimeZoneInformationData;
  LocalEpochMilliseconds: Int64;
  OffsetSeconds, I: Integer;
begin
  SetLength(AInstants, 0);
  ACount := 0;
  LocalEpochMilliseconds := LocalDateTimeToLocalEpochMilliseconds(AYear,
    AMonth, ADay, AHour, AMinute, ASecond, AMillisecond);

  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    AddPossibleEpochMilliseconds(AInstants, ACount, LocalEpochMilliseconds);
    Exit;
  end;

  if ParseOffsetString(ATimeZone, OffsetSeconds) then
  begin
    AddPossibleEpochMilliseconds(AInstants, ACount,
      LocalEpochMilliseconds - Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND);
    Exit;
  end;

  if not GetOrLoadTimeZoneData(ATimeZone, Data) then
  begin
    ConsiderOffsetForLocalDateTime(AInstants, ACount, LocalEpochMilliseconds,
      0, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond,
      ATimeZone);
    Exit;
  end;

  ConsiderOffsetForLocalDateTime(AInstants, ACount, LocalEpochMilliseconds,
    Data.DefaultOffsetSeconds, AYear, AMonth, ADay, AHour, AMinute,
    ASecond, AMillisecond, ATimeZone);
  for I := 0 to Length(Data.Transitions) - 1 do
    ConsiderOffsetForLocalDateTime(AInstants, ACount, LocalEpochMilliseconds,
      Data.Transitions[I].UtcOffsetSeconds, AYear, AMonth, ADay, AHour,
      AMinute, ASecond, AMillisecond, ATimeZone);
end;

function TryFindGapForLocalEpochMilliseconds(const ATimeZone: string;
  const ALocalEpochMilliseconds: Int64; out APreviousOffsetSeconds,
  ANextOffsetSeconds: Integer; out ATransitionEpochMilliseconds: Int64): Boolean;
var
  Data: TTimeZoneInformationData;
  PreviousOffsetSeconds, NextOffsetSeconds, I: Integer;
  TransitionEpochMilliseconds, LocalBeforeTransition, LocalAfterTransition: Int64;
begin
  Result := False;
  APreviousOffsetSeconds := 0;
  ANextOffsetSeconds := 0;
  ATransitionEpochMilliseconds := 0;

  if (ATimeZone = UTC_TIMEZONE_ID) or
     ParseOffsetString(ATimeZone, PreviousOffsetSeconds) or
     (not GetOrLoadTimeZoneData(ATimeZone, Data)) then
    Exit;

  PreviousOffsetSeconds := Data.DefaultOffsetSeconds;
  for I := 0 to Length(Data.Transitions) - 1 do
  begin
    NextOffsetSeconds := Data.Transitions[I].UtcOffsetSeconds;
    if NextOffsetSeconds > PreviousOffsetSeconds then
    begin
      TransitionEpochMilliseconds :=
        Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
      LocalBeforeTransition := TransitionEpochMilliseconds +
        Int64(PreviousOffsetSeconds) * MILLISECONDS_PER_SECOND;
      LocalAfterTransition := TransitionEpochMilliseconds +
        Int64(NextOffsetSeconds) * MILLISECONDS_PER_SECOND;
      if (ALocalEpochMilliseconds >= LocalBeforeTransition) and
         (ALocalEpochMilliseconds < LocalAfterTransition) then
      begin
        APreviousOffsetSeconds := PreviousOffsetSeconds;
        ANextOffsetSeconds := NextOffsetSeconds;
        ATransitionEpochMilliseconds := TransitionEpochMilliseconds;
        Exit(True);
      end;
    end;
    PreviousOffsetSeconds := NextOffsetSeconds;
  end;
end;

function TryFindStartOfDayGap(const AYear, AMonth, ADay: Integer;
  const ATimeZone: string; out ATransitionEpochMilliseconds: Int64): Boolean;
var
  Data: TTimeZoneInformationData;
  DateRec: TTemporalDateRecord;
  DayStartMilliseconds, DayEndMilliseconds: Int64;
  TransitionEpochMilliseconds, LocalBeforeTransition, LocalAfterTransition: Int64;
  PreviousOffsetSeconds, NextOffsetSeconds, I: Integer;
begin
  Result := False;
  ATransitionEpochMilliseconds := 0;

  if (ATimeZone = UTC_TIMEZONE_ID) or
     ParseOffsetString(ATimeZone, PreviousOffsetSeconds) or
     (not GetOrLoadTimeZoneData(ATimeZone, Data)) then
    Exit;

  DayStartMilliseconds := LocalDateTimeToLocalEpochMilliseconds(AYear,
    AMonth, ADay, 0, 0, 0, 0);
  DateRec := AddDaysToDate(AYear, AMonth, ADay, 1);
  DayEndMilliseconds := LocalDateTimeToLocalEpochMilliseconds(DateRec.Year,
    DateRec.Month, DateRec.Day, 0, 0, 0, 0);

  PreviousOffsetSeconds := Data.DefaultOffsetSeconds;
  for I := 0 to Length(Data.Transitions) - 1 do
  begin
    NextOffsetSeconds := Data.Transitions[I].UtcOffsetSeconds;
    if NextOffsetSeconds > PreviousOffsetSeconds then
    begin
      TransitionEpochMilliseconds :=
        Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
      LocalBeforeTransition := TransitionEpochMilliseconds +
        Int64(PreviousOffsetSeconds) * MILLISECONDS_PER_SECOND;
      LocalAfterTransition := TransitionEpochMilliseconds +
        Int64(NextOffsetSeconds) * MILLISECONDS_PER_SECOND;
      if (DayStartMilliseconds >= LocalBeforeTransition) and
         (DayStartMilliseconds < LocalAfterTransition) and
         (LocalAfterTransition < DayEndMilliseconds) then
      begin
        ATransitionEpochMilliseconds := TransitionEpochMilliseconds;
        Exit(True);
      end;
    end;
    PreviousOffsetSeconds := NextOffsetSeconds;
  end;
end;

procedure ThrowDisambiguationRangeError;
begin
  ThrowRangeError('Temporal local date-time is ambiguous or does not exist in the time zone');
end;

function LocalDateTimeToEpochMilliseconds(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string): Int64;
begin
  Result := LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear, AMonth,
    ADay, AHour, AMinute, ASecond, AMillisecond, ATimeZone, ttzdCompatible);
end;

function LocalDateTimeToEpochMillisecondsWithDisambiguation(const AYear, AMonth,
  ADay, AHour, AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string;
  const ADisambiguation: TTemporalTimeZoneDisambiguation): Int64;
var
  Instants: TTimeZoneEpochMillisecondsArray;
  InstantCount: Integer;
  LocalEpochMilliseconds: Int64;
  PreviousOffsetSeconds, NextOffsetSeconds: Integer;
  TransitionEpochMilliseconds: Int64;
begin
  GetPossibleEpochMillisecondsForLocalDateTime(AYear, AMonth, ADay, AHour,
    AMinute, ASecond, AMillisecond, ATimeZone, Instants, InstantCount);
  if InstantCount > 0 then
  begin
    if (ADisambiguation = ttzdReject) and (InstantCount <> 1) then
      ThrowDisambiguationRangeError;
    if ADisambiguation = ttzdLater then
      Exit(Instants[InstantCount - 1]);
    Exit(Instants[0]);
  end;

  if ADisambiguation = ttzdReject then
    ThrowDisambiguationRangeError;

  LocalEpochMilliseconds := LocalDateTimeToLocalEpochMilliseconds(AYear,
    AMonth, ADay, AHour, AMinute, ASecond, AMillisecond);
  if TryFindGapForLocalEpochMilliseconds(ATimeZone, LocalEpochMilliseconds,
    PreviousOffsetSeconds, NextOffsetSeconds, TransitionEpochMilliseconds) then
  begin
    if ADisambiguation = ttzdEarlier then
      Exit(LocalEpochMilliseconds - Int64(NextOffsetSeconds) *
        MILLISECONDS_PER_SECOND);
    Exit(LocalEpochMilliseconds - Int64(PreviousOffsetSeconds) *
      MILLISECONDS_PER_SECOND);
  end;

  Result := LocalEpochMilliseconds -
    Int64(GetUtcOffsetSeconds(ATimeZone, FloorEpochSecond(LocalEpochMilliseconds))) *
    MILLISECONDS_PER_SECOND;
end;

function StartOfDayToEpochMilliseconds(const AYear, AMonth, ADay: Integer;
  const ATimeZone: string): Int64;
var
  Instants: TTimeZoneEpochMillisecondsArray;
  InstantCount: Integer;
begin
  GetPossibleEpochMillisecondsForLocalDateTime(AYear, AMonth, ADay, 0, 0, 0,
    0, ATimeZone, Instants, InstantCount);
  if InstantCount > 0 then
    Exit(Instants[0]);

  if TryFindStartOfDayGap(AYear, AMonth, ADay, ATimeZone, Result) then
    Exit;

  Result := LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear, AMonth,
    ADay, 0, 0, 0, 0, ATimeZone, ttzdCompatible);
end;

function FormatOffsetString(const AOffsetSeconds: Integer): string;
var
  AbsOffset, Hours, Minutes, Seconds: Integer;
  Sign: Char;
begin
  if AOffsetSeconds >= 0 then
    Sign := '+'
  else
    Sign := '-';

  AbsOffset := Abs(AOffsetSeconds);
  Hours := AbsOffset div (MINUTES_PER_HOUR * SECONDS_PER_MINUTE);
  Minutes := (AbsOffset mod (MINUTES_PER_HOUR * SECONDS_PER_MINUTE)) div SECONDS_PER_MINUTE;
  Seconds := AbsOffset mod SECONDS_PER_MINUTE;

  if Seconds = 0 then
    Result := Sign + Format('%.2d:%.2d', [Hours, Minutes])
  else
    Result := Sign + Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);
end;

function ParseOffsetString(const AStr: string; out AOffsetSeconds: Integer): Boolean;
var
  Sign: Integer;
  Hours, Minutes: Integer;
  Pos: Integer;
begin
  Result := False;
  AOffsetSeconds := 0;

  if Length(AStr) = 0 then
    Exit;

  // Handle 'Z' for UTC
  if (Length(AStr) = 1) and ((AStr[1] = 'Z') or (AStr[1] = 'z')) then
  begin
    AOffsetSeconds := 0;
    Result := True;
    Exit;
  end;

  // Must start with + or -
  case AStr[1] of
    '+': Sign := 1;
    '-': Sign := -1;
  else
    Exit;
  end;

  Pos := 2;

  // Parse hours (2 digits)
  if Pos + 1 > Length(AStr) then Exit;
  if (AStr[Pos] < '0') or (AStr[Pos] > '9') then Exit;
  if (AStr[Pos + 1] < '0') or (AStr[Pos + 1] > '9') then Exit;
  Hours := (Ord(AStr[Pos]) - Ord('0')) * 10 + (Ord(AStr[Pos + 1]) - Ord('0'));
  Inc(Pos, 2);

  // Check for optional colon separator
  if Pos > Length(AStr) then
  begin
    // +HH format (hours only)
    AOffsetSeconds := Sign * Hours * MINUTES_PER_HOUR * SECONDS_PER_MINUTE;
    Result := True;
    Exit;
  end;

  if AStr[Pos] = ':' then
    Inc(Pos);

  // Parse minutes (2 digits)
  if Pos + 1 > Length(AStr) then Exit;
  if (AStr[Pos] < '0') or (AStr[Pos] > '9') then Exit;
  if (AStr[Pos + 1] < '0') or (AStr[Pos + 1] > '9') then Exit;
  Minutes := (Ord(AStr[Pos]) - Ord('0')) * 10 + (Ord(AStr[Pos + 1]) - Ord('0'));
  Inc(Pos, 2);

  // Must be at end of string
  if Pos <= Length(AStr) then Exit;

  // Validate ranges
  if (Hours > 23) or (Minutes > 59) then Exit;

  AOffsetSeconds := Sign * (Hours * MINUTES_PER_HOUR * SECONDS_PER_MINUTE + Minutes * SECONDS_PER_MINUTE);
  Result := True;
end;

procedure ClearTimeZoneCache;
begin
  SetLength(CachedTimeZones, 0);
  CachedTimeZoneCount := 0;
  SetLength(CachedTimeZonePaths, 0);
  CachedTimeZonePathCount := 0;
  SetLength(CachedTimeZoneCases, 0);
  CachedTimeZoneCaseCount := 0;
end;

initialization
  CachedTimeZoneCount := 0;
  CachedTimeZonePathCount := 0;
  CachedTimeZoneCaseCount := 0;
  {$IFDEF MSWINDOWS}
  InitCriticalSection(WindowsICUInitLock);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  DoneCriticalSection(WindowsICUInitLock);
  {$ENDIF}

end.
