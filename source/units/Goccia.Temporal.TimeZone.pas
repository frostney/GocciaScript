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
  MAX_SYMLINK_LENGTH = 1024;

threadvar
  CachedTimeZones: array of TTimeZoneInformationData;
  CachedTimeZoneCount: Integer;

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

function TryLoadTimeZoneDataFromGeneratedData(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  Buffer: TBytes;
begin
  Result := False;

  if not TryGetGeneratedTimeZoneFile(ATimeZone, Buffer) then
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

  if TryLoadTimeZoneDataFromGeneratedData(ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;

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
begin
  // Read the symlink target of /etc/localtime
  SetLength(LinkTarget, MAX_SYMLINK_LENGTH);
  PrefixPos := fpReadLink(LOCALTIME_PATH, @LinkTarget[1], MAX_SYMLINK_LENGTH);
  if PrefixPos > 0 then
  begin
    SetLength(LinkTarget, PrefixPos);

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
begin
  Result := UTC_TIMEZONE_ID;
end;
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

end.
