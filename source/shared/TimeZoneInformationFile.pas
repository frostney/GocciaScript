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
  TIMEZONE_INFORMATION_LEAP_V2_RECORD_SIZE = 12;
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

function HasBytesAvailable(const ABuffer: TBytes; const AOffset, ALength: Integer): Boolean;
begin
  Result := False;

  if (AOffset < 0) or (ALength < 0) then
    Exit;

  if AOffset > Length(ABuffer) then
    Exit;

  Result := ALength <= Length(ABuffer) - AOffset;
end;

function HasTimeZoneInformationMagic(const ABuffer: TBytes; const AOffset: Integer): Boolean;
begin
  Result := HasBytesAvailable(ABuffer, AOffset, TIMEZONE_INFORMATION_MAGIC_LENGTH) and
            (ABuffer[AOffset] = TIMEZONE_INFORMATION_MAGIC_T) and
            (ABuffer[AOffset + 1] = TIMEZONE_INFORMATION_MAGIC_Z) and
            (ABuffer[AOffset + 2] = TIMEZONE_INFORMATION_MAGIC_I) and
            (ABuffer[AOffset + 3] = TIMEZONE_INFORMATION_MAGIC_F);
end;

function AreTimeZoneInformationCountsValid(const ATimeCount, ATypeCount, ACharacterCount,
  ALeapCount, AIsStandardCount, AIsUtCount: Int32): Boolean;
begin
  Result := (ATimeCount >= 0) and
            (ATypeCount >= 0) and
            (ACharacterCount >= 0) and
            (ALeapCount >= 0) and
            (AIsStandardCount >= 0) and
            (AIsUtCount >= 0);
end;

function TryComputeTimeZoneInformationDataSize(const ATimeCount, ATypeCount,
  ACharacterCount, ALeapCount, AIsStandardCount, AIsUtCount: Int32;
  const ATransitionSize, ALeapRecordSize: Integer; out ADataSize: Integer): Boolean;
var
  DataSize: Int64;
begin
  Result := False;
  ADataSize := 0;

  if not AreTimeZoneInformationCountsValid(ATimeCount, ATypeCount, ACharacterCount,
    ALeapCount, AIsStandardCount, AIsUtCount) then
    Exit;

  DataSize := Int64(ATimeCount) * ATransitionSize +
              Int64(ATimeCount) * TIMEZONE_INFORMATION_TYPE_INDEX_SIZE +
              Int64(ATypeCount) * TIMEZONE_INFORMATION_TYPE_INFORMATION_SIZE +
              Int64(ACharacterCount) +
              Int64(ALeapCount) * ALeapRecordSize +
              Int64(AIsStandardCount) +
              Int64(AIsUtCount);

  if (DataSize < 0) or (DataSize > High(Integer)) then
    Exit;

  ADataSize := Integer(DataSize);
  Result := True;
end;

function TryParseTimeZoneInformationFile(const AIdentifier: string;
  const ABytes: TBytes; out AData: TTimeZoneInformationData): Boolean;
var
  Offset: Integer;
  Version: Char;
  TimeCount, TypeCount, CharacterCount, LeapCount, IsStandardCount, IsUtCount: Int32;
  V1DataSize, DataBlockSize: Integer;
  TransitionTimes: array of Int64;
  TypeIndices: array of Byte;
  UtcOffsets: array of Int32;
  I: Integer;
  UseV2: Boolean;
begin
  Result := False;

  if not HasBytesAvailable(ABytes, 0, TIMEZONE_INFORMATION_HEADER_SIZE) then
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

  if not TryComputeTimeZoneInformationDataSize(TimeCount, TypeCount, CharacterCount,
    LeapCount, IsStandardCount, IsUtCount, TIMEZONE_INFORMATION_V1_TRANSITION_SIZE,
    TIMEZONE_INFORMATION_LEAP_V1_RECORD_SIZE, V1DataSize) then
    Exit;

  if not HasBytesAvailable(ABytes, TIMEZONE_INFORMATION_HEADER_SIZE, V1DataSize) then
    Exit;

  if UseV2 then
  begin
    Offset := TIMEZONE_INFORMATION_HEADER_SIZE + V1DataSize;

    if not HasBytesAvailable(ABytes, Offset, TIMEZONE_INFORMATION_HEADER_SIZE) then
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

    if not TryComputeTimeZoneInformationDataSize(TimeCount, TypeCount, CharacterCount,
      LeapCount, IsStandardCount, IsUtCount, TIMEZONE_INFORMATION_V2_TRANSITION_SIZE,
      TIMEZONE_INFORMATION_LEAP_V2_RECORD_SIZE, DataBlockSize) then
      Exit;

    if not HasBytesAvailable(ABytes, Offset, DataBlockSize) then
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

    if not HasBytesAvailable(ABytes, Offset, V1DataSize) then
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
