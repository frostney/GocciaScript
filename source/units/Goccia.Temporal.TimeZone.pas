unit Goccia.Temporal.TimeZone;

{$I Goccia.inc}

interface

function GetSystemTimeZoneId: string;
function IsValidTimeZone(const ATimeZone: string): Boolean;
function GetUtcOffsetSeconds(const ATimeZone: string; const AEpochSeconds: Int64): Integer;
function FormatOffsetString(const AOffsetSeconds: Integer): string;
function ParseOffsetString(const AStr: string; out AOffsetSeconds: Integer): Boolean;
procedure ClearTimeZoneCache;

implementation

uses
  SysUtils,
  Goccia.Temporal.TimeZoneData,
  TimeZoneInformationFile,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  DynLibs,
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

threadvar
  CachedTimeZones: array of TTimeZoneInformationData;
  CachedTimeZoneCount: Integer;

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

function TryLoadWindowsICUFromLibrary(const ALibraryName: string;
  out AICU: TWindowsICU): Boolean;
var
  Symbol: Pointer;

  function FailLoad: Boolean;
  begin
    if AICU.Handle <> NilHandle then
      UnloadLibrary(AICU.Handle);
    FillChar(AICU, SizeOf(AICU), 0);
    Result := False;
  end;

begin
  Result := False;
  FillChar(AICU, SizeOf(AICU), 0);

  AICU.Handle := LoadLibrary(ALibraryName);
  if AICU.Handle = NilHandle then
    Exit;

  Symbol := GetProcAddress(AICU.Handle, 'ucal_open');
  if not Assigned(Symbol) then
  begin
    Result := FailLoad;
    Exit;
  end;
  AICU.OpenCalendar := TICUCalendarOpen(Symbol);

  Symbol := GetProcAddress(AICU.Handle, 'ucal_close');
  if not Assigned(Symbol) then
  begin
    Result := FailLoad;
    Exit;
  end;
  AICU.CloseCalendar := TICUCalendarClose(Symbol);

  Symbol := GetProcAddress(AICU.Handle, 'ucal_setMillis');
  if not Assigned(Symbol) then
  begin
    Result := FailLoad;
    Exit;
  end;
  AICU.SetMillis := TICUCalendarSetMillis(Symbol);

  Symbol := GetProcAddress(AICU.Handle, 'ucal_get');
  if not Assigned(Symbol) then
  begin
    Result := FailLoad;
    Exit;
  end;
  AICU.GetField := TICUCalendarGet(Symbol);

  Symbol := GetProcAddress(AICU.Handle, 'ucal_getTimeZoneTransitionDate');
  if not Assigned(Symbol) then
  begin
    Result := FailLoad;
    Exit;
  end;
  AICU.GetTimeZoneTransitionDate := TICUCalendarGetTimeZoneTransitionDate(Symbol);

  Symbol := GetProcAddress(AICU.Handle, 'ucal_getCanonicalTimeZoneID');
  if not Assigned(Symbol) then
  begin
    Result := FailLoad;
    Exit;
  end;
  AICU.GetCanonicalTimeZoneID := TICUCalendarGetCanonicalTimeZoneID(Symbol);

  Symbol := GetProcAddress(AICU.Handle, 'ucal_getDefaultTimeZone');
  if not Assigned(Symbol) then
  begin
    Result := FailLoad;
    Exit;
  end;
  AICU.GetDefaultTimeZone := TICUCalendarGetDefaultTimeZone(Symbol);

  Result := True;
end;

function TryGetWindowsICU(out AICU: TWindowsICU): Boolean;
var
  LoadedICU: TWindowsICU;
begin
  EnterCriticalSection(WindowsICUInitLock);
  try
    if not WindowsICULoadAttempted then
    begin
      WindowsICULoadAttempted := True;
      WindowsICUAvailable := TryLoadWindowsICUFromLibrary(ICU_LIBRARY, LoadedICU);
      if not WindowsICUAvailable then
        WindowsICUAvailable := TryLoadWindowsICUFromLibrary(ICU_I18N_LIBRARY, LoadedICU);
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

function FormatOffsetString(const AOffsetSeconds: Integer): string;
var
  AbsOffset, Hours, Minutes: Integer;
  Sign: Char;
begin
  if AOffsetSeconds >= 0 then
    Sign := '+'
  else
    Sign := '-';

  AbsOffset := Abs(AOffsetSeconds);
  Hours := AbsOffset div (MINUTES_PER_HOUR * SECONDS_PER_MINUTE);
  Minutes := (AbsOffset mod (MINUTES_PER_HOUR * SECONDS_PER_MINUTE)) div SECONDS_PER_MINUTE;

  Result := Sign + Format('%.2d:%.2d', [Hours, Minutes]);
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
end;

initialization
  CachedTimeZoneCount := 0;
  {$IFDEF MSWINDOWS}
  InitCriticalSection(WindowsICUInitLock);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  if WindowsICU.Handle <> NilHandle then
    UnloadLibrary(WindowsICU.Handle);
  DoneCriticalSection(WindowsICUInitLock);
  {$ENDIF}

end.
