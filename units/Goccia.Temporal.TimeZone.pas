unit Goccia.Temporal.TimeZone;

{$I Goccia.inc}

interface

type
  TTimeZoneTransition = record
    TransitionEpochSeconds: Int64;
    UtcOffsetSeconds: Integer;
  end;

  TTimeZoneTransitionArray = array of TTimeZoneTransition;

  TTimeZoneData = record
    TimeZoneId: string;
    Transitions: TTimeZoneTransitionArray;
    DefaultOffsetSeconds: Integer;
  end;

function GetSystemTimeZoneId: string;
function IsValidTimeZone(const ATimeZone: string): Boolean;
function GetUtcOffsetSeconds(const ATimeZone: string; const AEpochSeconds: Int64): Integer;
function FormatOffsetString(const AOffsetSeconds: Integer): string;
function ParseOffsetString(const AStr: string; out AOffsetSeconds: Integer): Boolean;

implementation

uses
  SysUtils,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  Classes;

const
  TZIF_MAGIC = 'TZif';
  TZIF_MAGIC_LENGTH = 4;
  TZIF_HEADER_SIZE = 44;
  TZIF_V1_TRANSITION_SIZE = 4;
  TZIF_V2_TRANSITION_SIZE = 8;
  TZIF_TYPE_INDEX_SIZE = 1;
  TZIF_TTINFO_SIZE = 6;
  TZIF_TTINFO_UTOFF_SIZE = 4;
  UTC_TIMEZONE_ID = 'UTC';
  {$IFDEF UNIX}
  ZONEINFO_PATH = '/usr/share/zoneinfo/';
  LOCALTIME_PATH = '/etc/localtime';
  {$ENDIF}
  MINUTES_PER_HOUR = 60;
  SECONDS_PER_MINUTE = 60;
  MAX_SYMLINK_LENGTH = 1024;

var
  CachedTimeZones: array of TTimeZoneData;
  CachedTimeZoneCount: Integer;

function ReadBigEndianInt32(const ABuffer: array of Byte; const AOffset: Integer): Int32;
begin
  Result := Int32((Int32(ABuffer[AOffset]) shl 24) or
                  (Int32(ABuffer[AOffset + 1]) shl 16) or
                  (Int32(ABuffer[AOffset + 2]) shl 8) or
                  Int32(ABuffer[AOffset + 3]));
end;

function ReadBigEndianUInt32(const ABuffer: array of Byte; const AOffset: Integer): UInt32;
begin
  Result := (UInt32(ABuffer[AOffset]) shl 24) or
            (UInt32(ABuffer[AOffset + 1]) shl 16) or
            (UInt32(ABuffer[AOffset + 2]) shl 8) or
            UInt32(ABuffer[AOffset + 3]);
end;

function ReadBigEndianInt64(const ABuffer: array of Byte; const AOffset: Integer): Int64;
var
  High32, Low32: UInt32;
begin
  High32 := ReadBigEndianUInt32(ABuffer, AOffset);
  Low32 := ReadBigEndianUInt32(ABuffer, AOffset + 4);
  Result := (Int64(High32) shl 32) or Int64(Low32);
end;

function FindCachedTimeZone(const ATimeZone: string): Integer;
var
  I: Integer;
begin
  for I := 0 to CachedTimeZoneCount - 1 do
  begin
    if CachedTimeZones[I].TimeZoneId = ATimeZone then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function ParseTZifData(const ATimeZone: string; const ABuffer: array of Byte;
  const ASize: Integer; out AData: TTimeZoneData): Boolean;
var
  Offset: Integer;
  Version: Char;
  TimeCnt, TypeCnt, CharCnt, LeapCnt, IsStdCnt, IsUtCnt: Int32;
  V1DataSize: Integer;
  TransitionTimes: array of Int64;
  TypeIndices: array of Byte;
  UtOffsets: array of Int32;
  I: Integer;
  UseV2: Boolean;
begin
  Result := False;

  // Validate minimum size for magic + header
  if ASize < TZIF_MAGIC_LENGTH + TZIF_HEADER_SIZE then
    Exit;

  // Check magic bytes
  if (ABuffer[0] <> Ord('T')) or (ABuffer[1] <> Ord('Z')) or
     (ABuffer[2] <> Ord('i')) or (ABuffer[3] <> Ord('f')) then
    Exit;

  // Read version byte
  Version := Char(ABuffer[4]);
  UseV2 := (Version = '2') or (Version = '3');

  // Read v1 header (starts at offset 20)
  Offset := 20;
  IsUtCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
  IsStdCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
  LeapCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
  TimeCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
  TypeCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
  CharCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);

  // Calculate v1 data section size
  V1DataSize := TimeCnt * TZIF_V1_TRANSITION_SIZE +   // transition times
                TimeCnt * TZIF_TYPE_INDEX_SIZE +       // type indices
                TypeCnt * TZIF_TTINFO_SIZE +           // ttinfo structs
                CharCnt +                              // time zone abbreviations
                LeapCnt * 8 +                          // leap second records
                IsStdCnt +                             // standard/wall indicators
                IsUtCnt;                               // UT/local indicators

  if UseV2 then
  begin
    // Skip v1 data to reach v2 header
    Offset := TZIF_HEADER_SIZE + V1DataSize;

    // Validate we have enough data for v2 header
    if Offset + TZIF_HEADER_SIZE > ASize then
      Exit;

    // Check v2 magic bytes
    if (ABuffer[Offset] <> Ord('T')) or (ABuffer[Offset + 1] <> Ord('Z')) or
       (ABuffer[Offset + 2] <> Ord('i')) or (ABuffer[Offset + 3] <> Ord('f')) then
      Exit;

    // Read v2 header
    Offset := Offset + 20;
    IsUtCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
    IsStdCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
    LeapCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
    TimeCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
    TypeCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);
    CharCnt := ReadBigEndianInt32(ABuffer, Offset); Inc(Offset, 4);

    // Validate we have enough data for v2 transitions
    if Offset + TimeCnt * TZIF_V2_TRANSITION_SIZE + TimeCnt * TZIF_TYPE_INDEX_SIZE +
       TypeCnt * TZIF_TTINFO_SIZE > ASize then
      Exit;

    // Read v2 transition times (8 bytes each)
    SetLength(TransitionTimes, TimeCnt);
    for I := 0 to TimeCnt - 1 do
    begin
      TransitionTimes[I] := ReadBigEndianInt64(ABuffer, Offset);
      Inc(Offset, TZIF_V2_TRANSITION_SIZE);
    end;
  end
  else
  begin
    // Use v1 data
    Offset := TZIF_HEADER_SIZE;

    // Validate we have enough data
    if Offset + TimeCnt * TZIF_V1_TRANSITION_SIZE + TimeCnt * TZIF_TYPE_INDEX_SIZE +
       TypeCnt * TZIF_TTINFO_SIZE > ASize then
      Exit;

    // Read v1 transition times (4 bytes each)
    SetLength(TransitionTimes, TimeCnt);
    for I := 0 to TimeCnt - 1 do
    begin
      TransitionTimes[I] := Int64(ReadBigEndianInt32(ABuffer, Offset));
      Inc(Offset, TZIF_V1_TRANSITION_SIZE);
    end;
  end;

  // Read type indices (1 byte each)
  SetLength(TypeIndices, TimeCnt);
  for I := 0 to TimeCnt - 1 do
  begin
    TypeIndices[I] := ABuffer[Offset];
    Inc(Offset, TZIF_TYPE_INDEX_SIZE);
  end;

  // Read ttinfo records (6 bytes each: 4-byte utoff, 1-byte is_dst, 1-byte idx)
  SetLength(UtOffsets, TypeCnt);
  for I := 0 to TypeCnt - 1 do
  begin
    UtOffsets[I] := ReadBigEndianInt32(ABuffer, Offset);
    Inc(Offset, TZIF_TTINFO_SIZE);
  end;

  // Build the result
  AData.TimeZoneId := ATimeZone;

  // Set default offset from the first non-DST ttinfo, or the first ttinfo
  if TypeCnt > 0 then
    AData.DefaultOffsetSeconds := UtOffsets[0]
  else
    AData.DefaultOffsetSeconds := 0;

  // Build transitions array
  SetLength(AData.Transitions, TimeCnt);
  for I := 0 to TimeCnt - 1 do
  begin
    AData.Transitions[I].TransitionEpochSeconds := TransitionTimes[I];
    if TypeIndices[I] < TypeCnt then
      AData.Transitions[I].UtcOffsetSeconds := UtOffsets[TypeIndices[I]]
    else
      AData.Transitions[I].UtcOffsetSeconds := 0;
  end;

  Result := True;
end;

function IsValidTimeZoneName(const AName: string): Boolean;
var
  I, SegStart: Integer;
begin
  Result := False;
  if Length(AName) = 0 then Exit;
  // Reject absolute paths
  if AName[1] = '/' then Exit;
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

{$IFDEF UNIX}
function LoadTimeZoneData(const ATimeZone: string; out AData: TTimeZoneData): Boolean;
var
  FilePath: string;
  Stream: TFileStream;
  Buffer: array of Byte;
  FileSize: Int64;
begin
  Result := False;

  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    AData.TimeZoneId := UTC_TIMEZONE_ID;
    SetLength(AData.Transitions, 0);
    AData.DefaultOffsetSeconds := 0;
    Result := True;
    Exit;
  end;

  if not IsValidTimeZoneName(ATimeZone) then
    Exit;

  FilePath := ZONEINFO_PATH + ATimeZone;
  if not FileExists(FilePath) then
    Exit;

  Stream := nil;
  try
    Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    FileSize := Stream.Size;
    if FileSize < TZIF_MAGIC_LENGTH + TZIF_HEADER_SIZE then
    begin
      Stream.Free;
      Exit;
    end;

    SetLength(Buffer, FileSize);
    Stream.ReadBuffer(Buffer[0], FileSize);
    Stream.Free;
    Stream := nil;

    Result := ParseTZifData(ATimeZone, Buffer, FileSize, AData);
  except
    Stream.Free;
  end;
end;
{$ELSE}
function LoadTimeZoneData(const ATimeZone: string; out AData: TTimeZoneData): Boolean;
begin
  // On Windows, only UTC is supported for now
  Result := False;
  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    AData.TimeZoneId := UTC_TIMEZONE_ID;
    SetLength(AData.Transitions, 0);
    AData.DefaultOffsetSeconds := 0;
    Result := True;
  end;
end;
{$ENDIF}

function GetOrLoadTimeZoneData(const ATimeZone: string; out AData: TTimeZoneData): Boolean;
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

function BinarySearchTransitions(const ATransitions: TTimeZoneTransitionArray;
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
begin
  // Read the symlink target of /etc/localtime
  SetLength(LinkTarget, MAX_SYMLINK_LENGTH);
  PrefixPos := fpReadLink(LOCALTIME_PATH, @LinkTarget[1], MAX_SYMLINK_LENGTH);
  if PrefixPos > 0 then
  begin
    SetLength(LinkTarget, PrefixPos);
    // Strip the /usr/share/zoneinfo/ prefix
    PrefixPos := Pos(ZONEINFO_PATH, LinkTarget);
    if PrefixPos > 0 then
    begin
      Result := Copy(LinkTarget, PrefixPos + Length(ZONEINFO_PATH),
        Length(LinkTarget) - PrefixPos - Length(ZONEINFO_PATH) + 1);
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
{$IFDEF UNIX}
begin
  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    Result := True;
    Exit;
  end;
  if not IsValidTimeZoneName(ATimeZone) then
  begin
    Result := False;
    Exit;
  end;
  Result := FileExists(ZONEINFO_PATH + ATimeZone);
end;
{$ELSE}
begin
  Result := ATimeZone = UTC_TIMEZONE_ID;
end;
{$ENDIF}

function GetUtcOffsetSeconds(const ATimeZone: string; const AEpochSeconds: Int64): Integer;
var
  Data: TTimeZoneData;
  Index: Integer;
begin
  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    Result := 0;
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

initialization
  CachedTimeZoneCount := 0;

end.
