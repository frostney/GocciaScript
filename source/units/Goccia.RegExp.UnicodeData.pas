unit Goccia.RegExp.UnicodeData;

{$I Goccia.inc}

interface

uses
  UnicodeICU;

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;

implementation

{$IFDEF GOCCIA_REGEXP_EMBEDDED_UCD}
uses
  Classes,
  SysUtils,

  EmbeddedResourceReader,
  Generated.UnicodeData;

type
  TEmbeddedUCDEntry = record
    NameOffset: Integer;
    NameLength: Integer;
    DataOffset: Integer;
    DataLength: Integer;
  end;

const
  UCD_MAGIC_LENGTH = 8;
  UCD_HEADER_FIELD_SIZE = 4;
  UCD_HEADER_FIELD_COUNT = 6;
  UCD_HEADER_SIZE = UCD_MAGIC_LENGTH +
    UCD_HEADER_FIELD_COUNT * UCD_HEADER_FIELD_SIZE;
  UCD_ENTRY_SIZE = 16;
  UCD_FORMAT_VERSION = 1;
  UCD_RANGE_SIZE = 8;
  UCD_RCDATA_RESOURCE_TYPE = MAKEINTRESOURCE(10);
  UCD_MAGIC: array[0..UCD_MAGIC_LENGTH - 1] of Byte =
    (Ord('G'), Ord('O'), Ord('C'), Ord('C'), Ord('I'), Ord('A'), Ord('U'), Ord('C'));

function HasExpectedMagic(const ABuffer: TBytes): Boolean;
var
  Index: Integer;
begin
  Result := HasBytesAvailable(ABuffer, 0, UCD_MAGIC_LENGTH);
  if not Result then
    Exit;

  for Index := 0 to UCD_MAGIC_LENGTH - 1 do
  begin
    if ABuffer[Index] <> UCD_MAGIC[Index] then
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

  if not HasBytesAvailable(ABuffer, 0, UCD_HEADER_SIZE) then
    Exit;

  if not HasExpectedMagic(ABuffer) then
    Exit;

  Offset := UCD_MAGIC_LENGTH;
  FormatVersion := ReadUInt32LE(ABuffer, Offset); Inc(Offset, UCD_HEADER_FIELD_SIZE);
  if FormatVersion <> UCD_FORMAT_VERSION then
    Exit;

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), VersionLength) then
    Exit;
  Inc(Offset, UCD_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), EntryCount) then
    Exit;
  Inc(Offset, UCD_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), NamesByteCount) then
    Exit;
  Inc(Offset, UCD_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), DataByteCount) then
    Exit;
  Inc(Offset, UCD_HEADER_FIELD_SIZE);

  Reserved := ReadUInt32LE(ABuffer, Offset);
  if Reserved <> 0 then
    Exit;

  EntryTableByteCount := Int64(EntryCount) * UCD_ENTRY_SIZE;
  if (EntryTableByteCount < 0) or (EntryTableByteCount > High(Integer)) then
    Exit;

  EntryTableOffset := Int64(UCD_HEADER_SIZE) + VersionLength;
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
  AEntryIndex: Integer; out AEntry: TEmbeddedUCDEntry): Boolean;
var
  Offset: Integer;
begin
  Result := False;
  AEntry.NameOffset := 0;
  AEntry.NameLength := 0;
  AEntry.DataOffset := 0;
  AEntry.DataLength := 0;

  Offset := AEntryTableOffset + AEntryIndex * UCD_ENTRY_SIZE;
  if not HasBytesAvailable(ABuffer, Offset, UCD_ENTRY_SIZE) then
    Exit;

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.NameOffset) then
    Exit;
  Inc(Offset, UCD_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.NameLength) then
    Exit;
  Inc(Offset, UCD_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.DataOffset) then
    Exit;
  Inc(Offset, UCD_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.DataLength) then
    Exit;

  Result := True;
end;

function TryGetEntryName(const ABuffer: TBytes; const AEntry: TEmbeddedUCDEntry;
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

function TryFindEmbeddedEntry(const ABuffer: TBytes; const AKey: string;
  const AEntryCount, AEntryTableOffset, ANamesOffset, ANamesByteCount: Integer;
  out AEntry: TEmbeddedUCDEntry): Boolean;
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

    CompareResult := CompareStr(AKey, EntryName);
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

function TryExtractRanges(const ABuffer: TBytes; const AEntry: TEmbeddedUCDEntry;
  const ADataOffset, ADataByteCount: Integer;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
var
  AbsoluteDataOffset, EntryDataEnd: Int64;
  RangeCount, I, Offset: Integer;
begin
  Result := False;
  SetLength(ARanges, 0);

  AbsoluteDataOffset := Int64(ADataOffset) + AEntry.DataOffset;
  EntryDataEnd := Int64(AEntry.DataOffset) + AEntry.DataLength;
  if (AbsoluteDataOffset < 0) or (AbsoluteDataOffset > High(Integer)) or
     (EntryDataEnd < 0) or (EntryDataEnd > ADataByteCount) then
    Exit;

  if not HasBytesAvailable(ABuffer, Integer(AbsoluteDataOffset), AEntry.DataLength) then
    Exit;

  if (AEntry.DataLength mod UCD_RANGE_SIZE) <> 0 then
    Exit;

  RangeCount := AEntry.DataLength div UCD_RANGE_SIZE;
  SetLength(ARanges, RangeCount);
  Offset := Integer(AbsoluteDataOffset);

  for I := 0 to RangeCount - 1 do
  begin
    ARanges[I].Lo := ReadUInt32LE(ABuffer, Offset);
    ARanges[I].Hi := ReadUInt32LE(ABuffer, Offset + 4);
    Inc(Offset, UCD_RANGE_SIZE);
  end;

  Result := True;
end;

var
  CachedUCDResource: TBytes;
  CachedUCDResourceLoaded: Boolean;
  CachedUCDResourceLock: TRTLCriticalSection;

function TryReadEmbeddedResource(out ABuffer: TBytes): Boolean;
var
  Stream: TResourceStream;
  BufferSize: Integer;
begin
  EnterCriticalSection(CachedUCDResourceLock);
  try
    if CachedUCDResourceLoaded then
    begin
      ABuffer := CachedUCDResource;
      Result := Length(ABuffer) > 0;
      Exit;
    end;

    Result := False;
    SetLength(ABuffer, 0);
    Stream := nil;
    try
      Stream := TResourceStream.Create(HInstance, GeneratedUnicodeDataResourceName,
        UCD_RCDATA_RESOURCE_TYPE);
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
      CachedUCDResource := ABuffer;
      CachedUCDResourceLoaded := True;
      Result := True;
    except
      Stream.Free;
    end;
  finally
    LeaveCriticalSection(CachedUCDResourceLock);
  end;
end;

function TryGetEmbeddedPropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
var
  Resource: TBytes;
  Entry: TEmbeddedUCDEntry;
  EntryCount, EntryTableOffset, NamesOffset, DataOffset: Integer;
  NamesByteCount, DataByteCount: Integer;
begin
  Result := False;
  SetLength(ARanges, 0);

  if not TryReadEmbeddedResource(Resource) then
    Exit;

  if not TryReadEmbeddedHeader(Resource, EntryCount, EntryTableOffset, NamesOffset,
    DataOffset, NamesByteCount, DataByteCount) then
    Exit;

  if not TryFindEmbeddedEntry(Resource, AKey, EntryCount, EntryTableOffset,
    NamesOffset, NamesByteCount, Entry) then
    Exit;

  Result := TryExtractRanges(Resource, Entry, DataOffset, DataByteCount, ARanges);
end;

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
begin
  Result := TryGetEmbeddedPropertyRanges(AKey, ARanges);
end;

initialization
  InitCriticalSection(CachedUCDResourceLock);

finalization
  DoneCriticalSection(CachedUCDResourceLock);

{$ELSE}

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
begin
  Result := False;
  SetLength(ARanges, 0);
end;

{$ENDIF}

end.
