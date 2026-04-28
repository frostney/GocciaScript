unit Goccia.Temporal.TimeZoneData;

{$I Goccia.inc}

interface

uses
  SysUtils;

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;

implementation

{$IFDEF GOCCIA_TEMPORAL_EMBEDDED_TZDATA}
uses
  Classes,

  Generated.TimeZoneData;

type
  TEmbeddedTimeZoneDataEntry = record
    NameOffset: Integer;
    NameLength: Integer;
    DataOffset: Integer;
    DataLength: Integer;
  end;

const
  TIME_ZONE_DATA_MAGIC_LENGTH = 8;
  TIME_ZONE_DATA_HEADER_FIELD_SIZE = 4;
  TIME_ZONE_DATA_HEADER_FIELD_COUNT = 6;
  TIME_ZONE_DATA_HEADER_SIZE = TIME_ZONE_DATA_MAGIC_LENGTH +
    TIME_ZONE_DATA_HEADER_FIELD_COUNT * TIME_ZONE_DATA_HEADER_FIELD_SIZE;
  TIME_ZONE_DATA_ENTRY_SIZE = 16;
  TIME_ZONE_DATA_FORMAT_VERSION = 1;
  TIME_ZONE_RCDATA_RESOURCE_TYPE = MAKEINTRESOURCE(10);
  TIME_ZONE_DATA_MAGIC: array[0..TIME_ZONE_DATA_MAGIC_LENGTH - 1] of Byte =
    (Ord('G'), Ord('O'), Ord('C'), Ord('C'), Ord('I'), Ord('A'), Ord('T'), Ord('Z'));

function HasBytesAvailable(const ABuffer: TBytes; const AOffset, ALength: Integer): Boolean;
begin
  Result := False;

  if (AOffset < 0) or (ALength < 0) then
    Exit;

  if AOffset > Length(ABuffer) then
    Exit;

  Result := ALength <= Length(ABuffer) - AOffset;
end;

function TryUInt32ToInteger(const AValue: UInt32; out AInteger: Integer): Boolean;
begin
  Result := AValue <= UInt32(High(Integer));
  if Result then
    AInteger := Integer(AValue)
  else
    AInteger := 0;
end;

function ReadUInt32LE(const ABuffer: TBytes; const AOffset: Integer): UInt32;
begin
  Result := UInt32(ABuffer[AOffset]) or
            (UInt32(ABuffer[AOffset + 1]) shl 8) or
            (UInt32(ABuffer[AOffset + 2]) shl 16) or
            (UInt32(ABuffer[AOffset + 3]) shl 24);
end;

function HasExpectedMagic(const ABuffer: TBytes): Boolean;
var
  Index: Integer;
begin
  Result := HasBytesAvailable(ABuffer, 0, TIME_ZONE_DATA_MAGIC_LENGTH);
  if not Result then
    Exit;

  for Index := 0 to TIME_ZONE_DATA_MAGIC_LENGTH - 1 do
  begin
    if ABuffer[Index] <> TIME_ZONE_DATA_MAGIC[Index] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TryReadEmbeddedHeader(const ABuffer: TBytes; out AEntryCount,
  AEntryTableOffset, ANamesOffset, ADataOffset, ANamesByteCount,
  ADataByteCount: Integer): Boolean;
var
  Offset: Integer;
  FormatVersion, Reserved: UInt32;
  VersionLength, EntryCount, NamesByteCount, DataByteCount: Integer;
  EntryTableOffset, EntryTableByteCount, NamesOffset, DataOffset, TotalSize: Int64;
begin
  Result := False;
  AEntryCount := 0;
  AEntryTableOffset := 0;
  ANamesOffset := 0;
  ADataOffset := 0;
  ANamesByteCount := 0;
  ADataByteCount := 0;

  if not HasBytesAvailable(ABuffer, 0, TIME_ZONE_DATA_HEADER_SIZE) then
    Exit;

  if not HasExpectedMagic(ABuffer) then
    Exit;

  Offset := TIME_ZONE_DATA_MAGIC_LENGTH;
  FormatVersion := ReadUInt32LE(ABuffer, Offset); Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);
  if FormatVersion <> TIME_ZONE_DATA_FORMAT_VERSION then
    Exit;

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), VersionLength) then
    Exit;
  Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), EntryCount) then
    Exit;
  Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), NamesByteCount) then
    Exit;
  Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), DataByteCount) then
    Exit;
  Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);

  Reserved := ReadUInt32LE(ABuffer, Offset);
  if Reserved <> 0 then
    Exit;

  EntryTableByteCount := Int64(EntryCount) * TIME_ZONE_DATA_ENTRY_SIZE;
  if (EntryTableByteCount < 0) or (EntryTableByteCount > High(Integer)) then
    Exit;

  EntryTableOffset := Int64(TIME_ZONE_DATA_HEADER_SIZE) + VersionLength;
  NamesOffset := EntryTableOffset + EntryTableByteCount;
  DataOffset := NamesOffset + NamesByteCount;
  TotalSize := DataOffset + DataByteCount;
  if (EntryTableOffset < 0) or (EntryTableOffset > High(Integer)) or
     (TotalSize < 0) or (TotalSize > High(Integer)) then
    Exit;

  if not HasBytesAvailable(ABuffer, 0, Integer(TotalSize)) then
    Exit;

  AEntryCount := EntryCount;
  AEntryTableOffset := Integer(EntryTableOffset);
  ANamesOffset := Integer(NamesOffset);
  ADataOffset := Integer(DataOffset);
  ANamesByteCount := NamesByteCount;
  ADataByteCount := DataByteCount;
  Result := True;
end;

function TryReadEmbeddedEntry(const ABuffer: TBytes; const AEntryTableOffset,
  AEntryIndex: Integer; out AEntry: TEmbeddedTimeZoneDataEntry): Boolean;
var
  Offset: Integer;
begin
  Result := False;
  AEntry.NameOffset := 0;
  AEntry.NameLength := 0;
  AEntry.DataOffset := 0;
  AEntry.DataLength := 0;

  Offset := AEntryTableOffset + AEntryIndex * TIME_ZONE_DATA_ENTRY_SIZE;
  if not HasBytesAvailable(ABuffer, Offset, TIME_ZONE_DATA_ENTRY_SIZE) then
    Exit;

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.NameOffset) then
    Exit;
  Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.NameLength) then
    Exit;
  Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.DataOffset) then
    Exit;
  Inc(Offset, TIME_ZONE_DATA_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.DataLength) then
    Exit;

  Result := True;
end;

function CopyStringFromBytes(const ABuffer: TBytes; const AOffset,
  ALength: Integer): string;
begin
  SetLength(Result, ALength);
  if ALength > 0 then
    Move(ABuffer[AOffset], Result[1], ALength);
end;

function TryGetEntryName(const ABuffer: TBytes; const AEntry: TEmbeddedTimeZoneDataEntry;
  const ANamesOffset, ANamesByteCount: Integer; out AName: string): Boolean;
var
  AbsoluteOffset, EntryEnd: Int64;
begin
  Result := False;
  AName := '';

  if not HasBytesAvailable(ABuffer, ANamesOffset, ANamesByteCount) then
    Exit;

  AbsoluteOffset := Int64(ANamesOffset) + AEntry.NameOffset;
  EntryEnd := Int64(AEntry.NameOffset) + AEntry.NameLength;
  if (AbsoluteOffset < 0) or (AbsoluteOffset > High(Integer)) or
     (EntryEnd < 0) or (EntryEnd > ANamesByteCount) then
    Exit;

  if not HasBytesAvailable(ABuffer, Integer(AbsoluteOffset), AEntry.NameLength) then
    Exit;

  AName := CopyStringFromBytes(ABuffer, Integer(AbsoluteOffset), AEntry.NameLength);
  Result := True;
end;

function TryFindEmbeddedEntry(const ABuffer: TBytes; const ATimeZone: string;
  const AEntryCount, AEntryTableOffset, ANamesOffset, ANamesByteCount: Integer;
  out AEntry: TEmbeddedTimeZoneDataEntry): Boolean;
var
  LowIndex, HighIndex, MiddleIndex, CompareResult: Integer;
  EntryName: string;
begin
  Result := False;
  LowIndex := 0;
  HighIndex := AEntryCount - 1;

  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if not TryReadEmbeddedEntry(ABuffer, AEntryTableOffset, MiddleIndex, AEntry) then
      Exit;

    if not TryGetEntryName(ABuffer, AEntry, ANamesOffset, ANamesByteCount, EntryName) then
      Exit;

    CompareResult := CompareStr(ATimeZone, EntryName);
    if CompareResult = 0 then
    begin
      Result := True;
      Exit;
    end;

    if CompareResult < 0 then
      HighIndex := MiddleIndex - 1
    else
      LowIndex := MiddleIndex + 1;
  end;
end;

function TryReadEmbeddedResource(out ABuffer: TBytes): Boolean;
var
  Stream: TResourceStream;
  BufferSize: Integer;
begin
  Result := False;
  SetLength(ABuffer, 0);
  Stream := nil;
  try
    Stream := TResourceStream.Create(HInstance, GeneratedTimeZoneDataResourceName,
      TIME_ZONE_RCDATA_RESOURCE_TYPE);
    if Stream.Size > High(Integer) then
    begin
      Stream.Free;
      Exit;
    end;

    BufferSize := Integer(Stream.Size);
    SetLength(ABuffer, BufferSize);
    if BufferSize > 0 then
      Stream.ReadBuffer(ABuffer[0], BufferSize);
    Stream.Free;
    Stream := nil;
    Result := True;
  except
    Stream.Free;
  end;
end;

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;
var
  Resource: TBytes;
  Entry: TEmbeddedTimeZoneDataEntry;
  EntryCount, EntryTableOffset, NamesOffset, DataOffset: Integer;
  NamesByteCount, DataByteCount: Integer;
  AbsoluteDataOffset, EntryDataEnd: Int64;
begin
  Result := False;
  SetLength(ABytes, 0);

  if not TryReadEmbeddedResource(Resource) then
    Exit;

  if not TryReadEmbeddedHeader(Resource, EntryCount, EntryTableOffset, NamesOffset,
    DataOffset, NamesByteCount, DataByteCount) then
    Exit;

  if not TryFindEmbeddedEntry(Resource, ATimeZone, EntryCount, EntryTableOffset,
    NamesOffset, NamesByteCount, Entry) then
    Exit;

  AbsoluteDataOffset := Int64(DataOffset) + Entry.DataOffset;
  EntryDataEnd := Int64(Entry.DataOffset) + Entry.DataLength;
  if (AbsoluteDataOffset < 0) or (AbsoluteDataOffset > High(Integer)) or
     (EntryDataEnd < 0) or (EntryDataEnd > DataByteCount) then
    Exit;

  if not HasBytesAvailable(Resource, Integer(AbsoluteDataOffset), Entry.DataLength) then
    Exit;

  SetLength(ABytes, Entry.DataLength);
  if Entry.DataLength > 0 then
    Move(Resource[Integer(AbsoluteDataOffset)], ABytes[0], Entry.DataLength);
  Result := True;
end;

{$ELSE}

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;
begin
  Result := False;
  SetLength(ABytes, 0);
end;

{$ENDIF}

end.
