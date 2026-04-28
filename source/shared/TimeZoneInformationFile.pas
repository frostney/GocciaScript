unit TimeZoneInformationFile;

{$I Shared.inc}

interface

uses
  SysUtils;

type
  TTimeZoneInformationTransition = record
    TransitionEpochSeconds: Int64;
    UtcOffsetSeconds: Integer;
  end;

  TTimeZoneInformationTransitionArray = array of TTimeZoneInformationTransition;

  TTimeZoneInformationData = record
    Identifier: string;
    Transitions: TTimeZoneInformationTransitionArray;
    DefaultOffsetSeconds: Integer;
  end;

function TryParseTimeZoneInformationFile(const AIdentifier: string;
  const ABytes: TBytes; out AData: TTimeZoneInformationData): Boolean;

implementation

const
  TIMEZONE_INFORMATION_MAGIC_LENGTH = 4;
  TIMEZONE_INFORMATION_VERSION_OFFSET = 4;
  TIMEZONE_INFORMATION_COUNT_SIZE = 4;
  TIMEZONE_INFORMATION_HEADER_SIZE = 44;
  TIMEZONE_INFORMATION_V1_TRANSITION_SIZE = 4;
  TIMEZONE_INFORMATION_V2_TRANSITION_SIZE = 8;
  TIMEZONE_INFORMATION_TYPE_INDEX_SIZE = 1;
  TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE = 6;
  TIMEZONE_INFORMATION_TYPE_INFORMATION_OFFSET_SIZE = 4;
  TIMEZONE_INFORMATION_HEADER_COUNT_OFFSET = 20;
  TIMEZONE_INFORMATION_LEAP_V1_RECORD_SIZE = 8;
  TIMEZONE_INFORMATION_MAGIC_T = Ord('T');
  TIMEZONE_INFORMATION_MAGIC_Z = Ord('Z');
  TIMEZONE_INFORMATION_MAGIC_I = Ord('i');
  TIMEZONE_INFORMATION_MAGIC_F = Ord('f');

function ReadBigEndianInt32(const ABuffer: TBytes; const AOffset: Integer): Int32;
begin
  Result := Int32((Int32(ABuffer[AOffset]) shl 24) or
                  (Int32(ABuffer[AOffset + 1]) shl 16) or
                  (Int32(ABuffer[AOffset + 2]) shl 8) or
                  Int32(ABuffer[AOffset + 3]));
end;

function ReadBigEndianUInt32(const ABuffer: TBytes; const AOffset: Integer): UInt32;
begin
  Result := (UInt32(ABuffer[AOffset]) shl 24) or
            (UInt32(ABuffer[AOffset + 1]) shl 16) or
            (UInt32(ABuffer[AOffset + 2]) shl 8) or
            UInt32(ABuffer[AOffset + 3]);
end;

function ReadBigEndianInt64(const ABuffer: TBytes; const AOffset: Integer): Int64;
var
  High32, Low32: UInt32;
begin
  High32 := ReadBigEndianUInt32(ABuffer, AOffset);
  Low32 := ReadBigEndianUInt32(ABuffer, AOffset + TIMEZONE_INFORMATION_COUNT_SIZE);
  Result := (Int64(High32) shl 32) or Int64(Low32);
end;

function HasTimeZoneInformationMagic(const ABuffer: TBytes; const AOffset: Integer): Boolean;
begin
  Result := (ABuffer[AOffset] = TIMEZONE_INFORMATION_MAGIC_T) and
            (ABuffer[AOffset + 1] = TIMEZONE_INFORMATION_MAGIC_Z) and
            (ABuffer[AOffset + 2] = TIMEZONE_INFORMATION_MAGIC_I) and
            (ABuffer[AOffset + 3] = TIMEZONE_INFORMATION_MAGIC_F);
end;

function TryParseTimeZoneInformationFile(const AIdentifier: string;
  const ABytes: TBytes; out AData: TTimeZoneInformationData): Boolean;
var
  Offset: Integer;
  Version: Char;
  TimeCount, TypeCount, CharacterCount, LeapCount, IsStandardCount, IsUtCount: Int32;
  V1DataSize: Integer;
  TransitionTimes: array of Int64;
  TypeIndices: array of Byte;
  UtcOffsets: array of Int32;
  I: Integer;
  UseV2: Boolean;
begin
  Result := False;

  if Length(ABytes) < TIMEZONE_INFORMATION_MAGIC_LENGTH + TIMEZONE_INFORMATION_HEADER_SIZE then
    Exit;

  if not HasTimeZoneInformationMagic(ABytes, 0) then
    Exit;

  Version := Char(ABytes[TIMEZONE_INFORMATION_VERSION_OFFSET]);
  UseV2 := (Version = '2') or (Version = '3');

  Offset := TIMEZONE_INFORMATION_HEADER_COUNT_OFFSET;
  IsUtCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  IsStandardCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  LeapCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  TimeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  TypeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
  CharacterCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);

  V1DataSize := TimeCount * TIMEZONE_INFORMATION_V1_TRANSITION_SIZE +
                TimeCount * TIMEZONE_INFORMATION_TYPE_INDEX_SIZE +
                TypeCount * TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE +
                CharacterCount +
                LeapCount * TIMEZONE_INFORMATION_LEAP_V1_RECORD_SIZE +
                IsStandardCount +
                IsUtCount;

  if UseV2 then
  begin
    Offset := TIMEZONE_INFORMATION_HEADER_SIZE + V1DataSize;

    if Offset + TIMEZONE_INFORMATION_HEADER_SIZE > Length(ABytes) then
      Exit;

    if not HasTimeZoneInformationMagic(ABytes, Offset) then
      Exit;

    Offset := Offset + TIMEZONE_INFORMATION_HEADER_COUNT_OFFSET;
    IsUtCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    IsStandardCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    LeapCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    TimeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    TypeCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);
    CharacterCount := ReadBigEndianInt32(ABytes, Offset); Inc(Offset, TIMEZONE_INFORMATION_COUNT_SIZE);

    if Offset + TimeCount * TIMEZONE_INFORMATION_V2_TRANSITION_SIZE +
       TimeCount * TIMEZONE_INFORMATION_TYPE_INDEX_SIZE +
       TypeCount * TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE > Length(ABytes) then
      Exit;

    SetLength(TransitionTimes, TimeCount);
    for I := 0 to TimeCount - 1 do
    begin
      TransitionTimes[I] := ReadBigEndianInt64(ABytes, Offset);
      Inc(Offset, TIMEZONE_INFORMATION_V2_TRANSITION_SIZE);
    end;
  end
  else
  begin
    Offset := TIMEZONE_INFORMATION_HEADER_SIZE;

    if Offset + TimeCount * TIMEZONE_INFORMATION_V1_TRANSITION_SIZE +
       TimeCount * TIMEZONE_INFORMATION_TYPE_INDEX_SIZE +
       TypeCount * TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE > Length(ABytes) then
      Exit;

    SetLength(TransitionTimes, TimeCount);
    for I := 0 to TimeCount - 1 do
    begin
      TransitionTimes[I] := Int64(ReadBigEndianInt32(ABytes, Offset));
      Inc(Offset, TIMEZONE_INFORMATION_V1_TRANSITION_SIZE);
    end;
  end;

  SetLength(TypeIndices, TimeCount);
  for I := 0 to TimeCount - 1 do
  begin
    TypeIndices[I] := ABytes[Offset];
    Inc(Offset, TIMEZONE_INFORMATION_TYPE_INDEX_SIZE);
  end;

  SetLength(UtcOffsets, TypeCount);
  for I := 0 to TypeCount - 1 do
  begin
    UtcOffsets[I] := ReadBigEndianInt32(ABytes, Offset);
    Inc(Offset, TIMEZONE_INFORMATION_TYPE_INFORMATION_OFFSET_SIZE);
    Inc(Offset, TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE -
      TIMEZONE_INFORMATION_TYPE_INFORMATION_OFFSET_SIZE);
  end;

  AData.Identifier := AIdentifier;

  if TypeCount > 0 then
    AData.DefaultOffsetSeconds := UtcOffsets[0]
  else
    AData.DefaultOffsetSeconds := 0;

  SetLength(AData.Transitions, TimeCount);
  for I := 0 to TimeCount - 1 do
  begin
    AData.Transitions[I].TransitionEpochSeconds := TransitionTimes[I];
    if TypeIndices[I] < TypeCount then
      AData.Transitions[I].UtcOffsetSeconds := UtcOffsets[TypeIndices[I]]
    else
      AData.Transitions[I].UtcOffsetSeconds := 0;
  end;

  Result := True;
end;

end.
