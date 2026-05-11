unit Goccia.RegExp.UnicodeData;

{$I Goccia.inc}

interface

uses
  UnicodeICU;

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
function TryGetUnicodeSimpleCaseFold(ACodePoint: Cardinal;
  out AFoldedCodePoint: Cardinal): Boolean;
function RegExpCanonicalizeCodePoint(ACodePoint: Cardinal;
  AUnicodeAware, AIgnoreCase: Boolean): Cardinal;
procedure ExpandUnicodeSimpleCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);

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

  TSimpleCaseFoldPair = record
    Source: Cardinal;
    Target: Cardinal;
  end;

  TSimpleCaseFoldPairArray = array of TSimpleCaseFoldPair;

const
  UCD_MAGIC_LENGTH = 8;
  UCD_HEADER_FIELD_SIZE = 4;
  UCD_HEADER_FIELD_COUNT = 6;
  UCD_HEADER_SIZE = UCD_MAGIC_LENGTH +
    UCD_HEADER_FIELD_COUNT * UCD_HEADER_FIELD_SIZE;
  UCD_ENTRY_SIZE = 16;
  UCD_FORMAT_VERSION = 1;
  UCD_RANGE_SIZE = 8;
  CASE_FOLDING_ENTRY_KEY = 'CaseFolding/Simple';
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

function TryCompareEntryName(const ABuffer: TBytes; const AEntry: TEmbeddedUCDEntry;
  const ANamesOffset, ANamesByteCount: Integer; const AKey: string;
  out ACompareResult: Integer): Boolean;
var
  AbsoluteOffset, EntryEnd: Int64;
  KeyLen, EntryLen, MinLen, I: Integer;
  KeyByte, EntryByte: Byte;
begin
  Result := False;
  ACompareResult := 0;

  AbsoluteOffset := Int64(ANamesOffset) + AEntry.NameOffset;
  EntryEnd := Int64(AEntry.NameOffset) + AEntry.NameLength;
  if (AbsoluteOffset < 0) or (AbsoluteOffset > High(Integer)) or
     (EntryEnd < 0) or (EntryEnd > ANamesByteCount) then
    Exit;

  if not HasBytesAvailable(ABuffer, Integer(AbsoluteOffset), AEntry.NameLength) then
    Exit;

  KeyLen := Length(AKey);
  EntryLen := AEntry.NameLength;
  if KeyLen < EntryLen then
    MinLen := KeyLen
  else
    MinLen := EntryLen;

  for I := 0 to MinLen - 1 do
  begin
    KeyByte := Byte(AKey[I + 1]);
    EntryByte := ABuffer[Integer(AbsoluteOffset) + I];
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

  if KeyLen < EntryLen then
    ACompareResult := -1
  else if KeyLen > EntryLen then
    ACompareResult := 1
  else
    ACompareResult := 0;
  Result := True;
end;

function TryFindEmbeddedEntry(const ABuffer: TBytes; const AKey: string;
  const AEntryCount, AEntryTableOffset, ANamesOffset, ANamesByteCount: Integer;
  out AEntry: TEmbeddedUCDEntry): Boolean;
var
  LowIndex, HighIndex, MiddleIndex, CompareResult: Integer;
begin
  Result := False;
  LowIndex := 0;
  HighIndex := AEntryCount - 1;

  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if not TryReadEmbeddedEntry(ABuffer, AEntryTableOffset, MiddleIndex, AEntry) then
      Exit;

    if not TryCompareEntryName(ABuffer, AEntry, ANamesOffset, ANamesByteCount,
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

function TryExtractCaseFoldPairs(const ABuffer: TBytes;
  const AEntry: TEmbeddedUCDEntry; const ADataOffset, ADataByteCount: Integer;
  out APairs: TSimpleCaseFoldPairArray): Boolean;
var
  AbsoluteDataOffset, EntryDataEnd: Int64;
  PairCount, I, Offset: Integer;
begin
  Result := False;
  SetLength(APairs, 0);

  AbsoluteDataOffset := Int64(ADataOffset) + AEntry.DataOffset;
  EntryDataEnd := Int64(AEntry.DataOffset) + AEntry.DataLength;
  if (AbsoluteDataOffset < 0) or (AbsoluteDataOffset > High(Integer)) or
     (EntryDataEnd < 0) or (EntryDataEnd > ADataByteCount) then
    Exit;

  if not HasBytesAvailable(ABuffer, Integer(AbsoluteDataOffset),
     AEntry.DataLength) then
    Exit;

  if (AEntry.DataLength mod UCD_RANGE_SIZE) <> 0 then
    Exit;

  PairCount := AEntry.DataLength div UCD_RANGE_SIZE;
  SetLength(APairs, PairCount);
  Offset := Integer(AbsoluteDataOffset);

  for I := 0 to PairCount - 1 do
  begin
    APairs[I].Source := ReadUInt32LE(ABuffer, Offset);
    APairs[I].Target := ReadUInt32LE(ABuffer, Offset + 4);
    Inc(Offset, UCD_RANGE_SIZE);
  end;

  Result := True;
end;

var
  CachedUCDResource: TBytes;
  CachedUCDResourceLoaded: Boolean;
  CachedUCDResourceLock: TRTLCriticalSection;
  CachedCaseFoldPairs: TSimpleCaseFoldPairArray;
  CachedCaseFoldPairsLoaded: Boolean;
  CachedCaseFoldPairsAvailable: Boolean;
  CachedCaseFoldPairsLock: TRTLCriticalSection;

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

function TryGetEmbeddedCaseFoldPairs(
  out APairs: TSimpleCaseFoldPairArray): Boolean;
var
  Resource: TBytes;
  Entry: TEmbeddedUCDEntry;
  EntryCount, EntryTableOffset, NamesOffset, DataOffset: Integer;
  NamesByteCount, DataByteCount: Integer;
begin
  EnterCriticalSection(CachedCaseFoldPairsLock);
  try
    if CachedCaseFoldPairsLoaded then
    begin
      APairs := CachedCaseFoldPairs;
      Result := CachedCaseFoldPairsAvailable;
      Exit;
    end;

    Result := False;
    SetLength(APairs, 0);
    CachedCaseFoldPairsLoaded := True;
    CachedCaseFoldPairsAvailable := False;

    if not TryReadEmbeddedResource(Resource) then
      Exit;

    if not TryReadEmbeddedHeader(Resource, EntryCount, EntryTableOffset,
       NamesOffset, DataOffset, NamesByteCount, DataByteCount) then
      Exit;

    if not TryFindEmbeddedEntry(Resource, CASE_FOLDING_ENTRY_KEY, EntryCount,
       EntryTableOffset, NamesOffset, NamesByteCount, Entry) then
      Exit;

    if not TryExtractCaseFoldPairs(Resource, Entry, DataOffset, DataByteCount,
       CachedCaseFoldPairs) then
      Exit;

    APairs := CachedCaseFoldPairs;
    CachedCaseFoldPairsAvailable := True;
    Result := True;
  finally
    LeaveCriticalSection(CachedCaseFoldPairsLock);
  end;
end;

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
begin
  Result := TryGetEmbeddedPropertyRanges(AKey, ARanges);
end;

function TryGetUnicodeSimpleCaseFold(ACodePoint: Cardinal;
  out AFoldedCodePoint: Cardinal): Boolean;
var
  Pairs: TSimpleCaseFoldPairArray;
  LowIndex, HighIndex, MiddleIndex: Integer;
begin
  Result := False;
  AFoldedCodePoint := ACodePoint;

  if not TryGetEmbeddedCaseFoldPairs(Pairs) then
    Exit;

  LowIndex := 0;
  HighIndex := High(Pairs);
  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if Pairs[MiddleIndex].Source = ACodePoint then
    begin
      AFoldedCodePoint := Pairs[MiddleIndex].Target;
      Result := True;
      Exit;
    end;
    if Pairs[MiddleIndex].Source < ACodePoint then
      LowIndex := MiddleIndex + 1
    else
      HighIndex := MiddleIndex - 1;
  end;
end;

function RegExpCanonicalizeCodePoint(ACodePoint: Cardinal;
  AUnicodeAware, AIgnoreCase: Boolean): Cardinal;
begin
  Result := ACodePoint;
  if not AIgnoreCase then
    Exit;

  if AUnicodeAware then
  begin
    TryGetUnicodeSimpleCaseFold(ACodePoint, Result);
    Exit;
  end;

  if (Result >= Ord('A')) and (Result <= Ord('Z')) then
    Inc(Result, Ord('a') - Ord('A'));
end;

function RangeContainsCodePoint(const ARanges: TUnicodePropertyRangeArray;
  ACodePoint: Cardinal): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(ARanges) do
    if (ACodePoint >= ARanges[I].Lo) and (ACodePoint <= ARanges[I].Hi) then
      Exit(True);
  Result := False;
end;

procedure AddFoldRange(var ARanges: TUnicodePropertyRangeArray;
  ACodePoint: Cardinal);
var
  NewIndex: Integer;
begin
  if RangeContainsCodePoint(ARanges, ACodePoint) then
    Exit;
  NewIndex := Length(ARanges);
  SetLength(ARanges, NewIndex + 1);
  ARanges[NewIndex].Lo := ACodePoint;
  ARanges[NewIndex].Hi := ACodePoint;
end;

procedure ExpandUnicodeSimpleCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);
var
  OriginalRanges: TUnicodePropertyRangeArray;
  Pairs: TSimpleCaseFoldPairArray;
  I, J: Integer;
  Target: Cardinal;
begin
  if not TryGetEmbeddedCaseFoldPairs(Pairs) then
    Exit;

  SetLength(OriginalRanges, Length(ARanges));
  for I := 0 to High(ARanges) do
    OriginalRanges[I] := ARanges[I];

  for I := 0 to High(Pairs) do
  begin
    if not RangeContainsCodePoint(OriginalRanges, Pairs[I].Source) and
       not RangeContainsCodePoint(OriginalRanges, Pairs[I].Target) then
      Continue;

    Target := Pairs[I].Target;
    AddFoldRange(ARanges, Target);
    for J := 0 to High(Pairs) do
      if Pairs[J].Target = Target then
        AddFoldRange(ARanges, Pairs[J].Source);
  end;
end;

initialization
  InitCriticalSection(CachedUCDResourceLock);
  InitCriticalSection(CachedCaseFoldPairsLock);

finalization
  DoneCriticalSection(CachedCaseFoldPairsLock);
  DoneCriticalSection(CachedUCDResourceLock);

{$ELSE}

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
begin
  Result := False;
  SetLength(ARanges, 0);
end;

function TryGetUnicodeSimpleCaseFold(ACodePoint: Cardinal;
  out AFoldedCodePoint: Cardinal): Boolean;
begin
  AFoldedCodePoint := ACodePoint;
  Result := False;
end;

function RegExpCanonicalizeCodePoint(ACodePoint: Cardinal;
  AUnicodeAware, AIgnoreCase: Boolean): Cardinal;
begin
  Result := ACodePoint;
  if AUnicodeAware or not AIgnoreCase then
    Exit;
  if (Result >= Ord('A')) and (Result <= Ord('Z')) then
    Inc(Result, Ord('a') - Ord('A'));
end;

procedure ExpandUnicodeSimpleCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);
begin
end;

{$ENDIF}

end.
