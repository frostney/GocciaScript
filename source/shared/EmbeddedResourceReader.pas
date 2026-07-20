unit EmbeddedResourceReader;

{$I Shared.inc}

interface

uses
  SysUtils;

type
  {$IFDEF FPC}
  TEmbeddedResourceType = PAnsiChar;
  {$ELSE}
  TEmbeddedResourceType = PChar;
  {$ENDIF}

  TEmbeddedResourceMagic = array[0..7] of Byte;

  TEmbeddedResourceContainer = record
    EntryCount: Integer;
    EntryTableOffset: Integer;
    NamesOffset: Integer;
    DataOffset: Integer;
    NamesByteCount: Integer;
    DataByteCount: Integer;
  end;

  TEmbeddedResourceEntry = record
    NameOffset: Integer;
    NameLength: Integer;
    DataOffset: Integer;
    DataLength: Integer;
  end;

function RCDATAResourceType: TEmbeddedResourceType; {$IFDEF FPC}inline;{$ENDIF}
function HasBytesAvailable(const ABuffer: TBytes; const AOffset, ALength: Integer): Boolean;
function TryUInt32ToInteger(const AValue: UInt32; out AInteger: Integer): Boolean;
function ReadUInt32LE(const ABuffer: TBytes; const AOffset: Integer): UInt32;
function CopyStringFromBytes(const ABuffer: TBytes; const AOffset,
  ALength: Integer): string;
function HasEmbeddedResourceMagic(const ABuffer: TBytes;
  const AMagic: TEmbeddedResourceMagic): Boolean;
function TryReadEmbeddedResourceContainer(const ABuffer: TBytes;
  const AMagic: TEmbeddedResourceMagic;
  out AContainer: TEmbeddedResourceContainer): Boolean;
function TryReadEmbeddedResourceEntry(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntryIndex: Integer;
  out AEntry: TEmbeddedResourceEntry): Boolean;
function TryCompareEmbeddedResourceEntryName(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntry: TEmbeddedResourceEntry;
  const AKey: string; out ACompareResult: Integer): Boolean;
function TryFindEmbeddedResourceEntry(const ABuffer: TBytes;
  const AKey: string; const AContainer: TEmbeddedResourceContainer;
  out AEntry: TEmbeddedResourceEntry): Boolean;
function TryGetEmbeddedResourceEntryDataBounds(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntry: TEmbeddedResourceEntry;
  out ADataOffset, ADataLength: Integer): Boolean;
function TryCopyEmbeddedResourceEntryData(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntry: TEmbeddedResourceEntry;
  out ABytes: TBytes): Boolean;

implementation

uses
  TextEncoding;

const
  EMBEDDED_RESOURCE_MAGIC_LENGTH = 8;
  EMBEDDED_RESOURCE_HEADER_FIELD_SIZE = 4;
  EMBEDDED_RESOURCE_HEADER_FIELD_COUNT = 6;
  EMBEDDED_RESOURCE_HEADER_SIZE = EMBEDDED_RESOURCE_MAGIC_LENGTH +
    EMBEDDED_RESOURCE_HEADER_FIELD_COUNT * EMBEDDED_RESOURCE_HEADER_FIELD_SIZE;
  EMBEDDED_RESOURCE_ENTRY_SIZE = 16;
  EMBEDDED_RESOURCE_FORMAT_VERSION = 1;

function RCDATAResourceType: TEmbeddedResourceType;
begin
  Result := TEmbeddedResourceType(NativeUInt(10));
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

function CopyStringFromBytes(const ABuffer: TBytes; const AOffset,
  ALength: Integer): string;
var
  Bytes: TBytes;
begin
  SetLength(Bytes, ALength);
  if ALength > 0 then
    Move(ABuffer[AOffset], Bytes[0], ALength);
  Result := DecodeUTF8WithReplacement(Bytes);
end;

function HasEmbeddedResourceMagic(const ABuffer: TBytes;
  const AMagic: TEmbeddedResourceMagic): Boolean;
var
  Index: Integer;
begin
  Result := HasBytesAvailable(ABuffer, 0, EMBEDDED_RESOURCE_MAGIC_LENGTH);
  if not Result then
    Exit;

  for Index := 0 to EMBEDDED_RESOURCE_MAGIC_LENGTH - 1 do
  begin
    if ABuffer[Index] <> AMagic[Index] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TryReadEmbeddedResourceContainer(const ABuffer: TBytes;
  const AMagic: TEmbeddedResourceMagic;
  out AContainer: TEmbeddedResourceContainer): Boolean;
var
  Offset: Integer;
  FormatVersion, Reserved: UInt32;
  VersionLength, EntryCount, NamesByteCount, DataByteCount: Integer;
  EntryTableOffset, EntryTableByteCount, NamesOffset, DataOffset, TotalSize: Int64;
begin
  Result := False;
  AContainer.EntryCount := 0;
  AContainer.EntryTableOffset := 0;
  AContainer.NamesOffset := 0;
  AContainer.DataOffset := 0;
  AContainer.NamesByteCount := 0;
  AContainer.DataByteCount := 0;

  if not HasBytesAvailable(ABuffer, 0, EMBEDDED_RESOURCE_HEADER_SIZE) then
    Exit;

  if not HasEmbeddedResourceMagic(ABuffer, AMagic) then
    Exit;

  Offset := EMBEDDED_RESOURCE_MAGIC_LENGTH;
  FormatVersion := ReadUInt32LE(ABuffer, Offset);
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);
  if FormatVersion <> EMBEDDED_RESOURCE_FORMAT_VERSION then
    Exit;

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), VersionLength) then
    Exit;
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), EntryCount) then
    Exit;
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), NamesByteCount) then
    Exit;
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), DataByteCount) then
    Exit;
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);

  Reserved := ReadUInt32LE(ABuffer, Offset);
  if Reserved <> 0 then
    Exit;

  EntryTableByteCount := Int64(EntryCount) * EMBEDDED_RESOURCE_ENTRY_SIZE;
  if (EntryTableByteCount < 0) or (EntryTableByteCount > High(Integer)) then
    Exit;

  EntryTableOffset := Int64(EMBEDDED_RESOURCE_HEADER_SIZE) + VersionLength;
  NamesOffset := EntryTableOffset + EntryTableByteCount;
  DataOffset := NamesOffset + NamesByteCount;
  TotalSize := DataOffset + DataByteCount;
  if (EntryTableOffset < 0) or (EntryTableOffset > High(Integer)) or
     (TotalSize < 0) or (TotalSize > High(Integer)) then
    Exit;

  if not HasBytesAvailable(ABuffer, 0, Integer(TotalSize)) then
    Exit;

  AContainer.EntryCount := EntryCount;
  AContainer.EntryTableOffset := Integer(EntryTableOffset);
  AContainer.NamesOffset := Integer(NamesOffset);
  AContainer.DataOffset := Integer(DataOffset);
  AContainer.NamesByteCount := NamesByteCount;
  AContainer.DataByteCount := DataByteCount;
  Result := True;
end;

function TryReadEmbeddedResourceEntry(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntryIndex: Integer;
  out AEntry: TEmbeddedResourceEntry): Boolean;
var
  EntryOffset: Int64;
  Offset: Integer;
begin
  Result := False;
  AEntry.NameOffset := 0;
  AEntry.NameLength := 0;
  AEntry.DataOffset := 0;
  AEntry.DataLength := 0;

  if (AEntryIndex < 0) or (AEntryIndex >= AContainer.EntryCount) then
    Exit;

  EntryOffset := Int64(AContainer.EntryTableOffset) +
    Int64(AEntryIndex) * EMBEDDED_RESOURCE_ENTRY_SIZE;
  if (EntryOffset < 0) or (EntryOffset > High(Integer)) then
    Exit;

  Offset := Integer(EntryOffset);
  if not HasBytesAvailable(ABuffer, Offset, EMBEDDED_RESOURCE_ENTRY_SIZE) then
    Exit;

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.NameOffset) then
    Exit;
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.NameLength) then
    Exit;
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.DataOffset) then
    Exit;
  Inc(Offset, EMBEDDED_RESOURCE_HEADER_FIELD_SIZE);

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, Offset), AEntry.DataLength) then
    Exit;

  Result := True;
end;

function TryCompareEmbeddedResourceEntryName(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntry: TEmbeddedResourceEntry;
  const AKey: string; out ACompareResult: Integer): Boolean;
var
  AbsoluteOffset, EntryEnd: Int64;
  KeyLength, EntryLength, MinimumLength, Index: Integer;
  KeyByte, EntryByte: Byte;
begin
  Result := False;
  ACompareResult := 0;

  if not HasBytesAvailable(ABuffer, AContainer.NamesOffset,
     AContainer.NamesByteCount) then
    Exit;

  AbsoluteOffset := Int64(AContainer.NamesOffset) + AEntry.NameOffset;
  EntryEnd := Int64(AEntry.NameOffset) + AEntry.NameLength;
  if (AbsoluteOffset < 0) or (AbsoluteOffset > High(Integer)) or
     (EntryEnd < 0) or (EntryEnd > AContainer.NamesByteCount) then
    Exit;

  if not HasBytesAvailable(ABuffer, Integer(AbsoluteOffset), AEntry.NameLength) then
    Exit;

  KeyLength := Length(AKey);
  EntryLength := AEntry.NameLength;
  if KeyLength < EntryLength then
    MinimumLength := KeyLength
  else
    MinimumLength := EntryLength;

  for Index := 0 to MinimumLength - 1 do
  begin
    KeyByte := Byte(AKey[Index + 1]);
    EntryByte := ABuffer[Integer(AbsoluteOffset) + Index];
    if KeyByte <> EntryByte then
    begin
      if KeyByte < EntryByte then
        ACompareResult := -1
      else
        ACompareResult := 1;
      Result := True;
      Exit;
    end;
  end;

  if KeyLength < EntryLength then
    ACompareResult := -1
  else if KeyLength > EntryLength then
    ACompareResult := 1
  else
    ACompareResult := 0;
  Result := True;
end;

function TryFindEmbeddedResourceEntry(const ABuffer: TBytes;
  const AKey: string; const AContainer: TEmbeddedResourceContainer;
  out AEntry: TEmbeddedResourceEntry): Boolean;
var
  LowIndex, HighIndex, MiddleIndex, CompareResult: Integer;
begin
  Result := False;
  LowIndex := 0;
  HighIndex := AContainer.EntryCount - 1;

  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if not TryReadEmbeddedResourceEntry(ABuffer, AContainer, MiddleIndex, AEntry) then
      Exit;

    if not TryCompareEmbeddedResourceEntryName(ABuffer, AContainer, AEntry,
       AKey, CompareResult) then
      Exit;

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

function TryGetEmbeddedResourceEntryDataBounds(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntry: TEmbeddedResourceEntry;
  out ADataOffset, ADataLength: Integer): Boolean;
var
  AbsoluteDataOffset, EntryDataEnd: Int64;
begin
  Result := False;
  ADataOffset := 0;
  ADataLength := 0;

  AbsoluteDataOffset := Int64(AContainer.DataOffset) + AEntry.DataOffset;
  EntryDataEnd := Int64(AEntry.DataOffset) + AEntry.DataLength;
  if (AbsoluteDataOffset < 0) or (AbsoluteDataOffset > High(Integer)) or
     (EntryDataEnd < 0) or (EntryDataEnd > AContainer.DataByteCount) then
    Exit;

  if not HasBytesAvailable(ABuffer, Integer(AbsoluteDataOffset),
     AEntry.DataLength) then
    Exit;

  ADataOffset := Integer(AbsoluteDataOffset);
  ADataLength := AEntry.DataLength;
  Result := True;
end;

function TryCopyEmbeddedResourceEntryData(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer; const AEntry: TEmbeddedResourceEntry;
  out ABytes: TBytes): Boolean;
var
  DataOffset, DataLength: Integer;
begin
  Result := False;
  SetLength(ABytes, 0);

  if not TryGetEmbeddedResourceEntryDataBounds(ABuffer, AContainer, AEntry,
     DataOffset, DataLength) then
    Exit;

  SetLength(ABytes, DataLength);
  if DataLength > 0 then
    Move(ABuffer[DataOffset], ABytes[0], DataLength);
  Result := True;
end;

end.
