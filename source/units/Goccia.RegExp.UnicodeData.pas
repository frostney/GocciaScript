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
procedure ReduceUnicodeSimpleCaseFoldClosed(
  var ARanges: TUnicodePropertyRangeArray);
procedure ExpandRegExpNonUnicodeCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);

implementation

{$IFDEF GOCCIA_REGEXP_EMBEDDED_UCD}
uses
  Classes,
  SysUtils,

  EmbeddedResourceReader,
  Generated.UnicodeData,
  LazyPublishedCache;

type
  TCodePointPair = record
    Source: Cardinal;
    Target: Cardinal;
  end;

  TCodePointPairArray = array of TCodePointPair;

const
  UCD_RANGE_SIZE = 8;
  CASE_FOLDING_ENTRY_KEY = 'CaseFolding/Simple';
  NON_UNICODE_UPPERCASE_ENTRY_KEY = 'CaseMapping/RegExpNonUnicodeUppercase';
  UCD_RCDATA_RESOURCE_TYPE = MAKEINTRESOURCE(10);
  UCD_MAGIC: TEmbeddedResourceMagic =
    (Ord('G'), Ord('O'), Ord('C'), Ord('C'), Ord('I'), Ord('A'), Ord('U'), Ord('C'));

function TryExtractRanges(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer;
  const AEntry: TEmbeddedResourceEntry;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
var
  DataOffset, DataLength: Integer;
  RangeCount, I, Offset: Integer;
begin
  Result := False;
  SetLength(ARanges, 0);

  if not TryGetEmbeddedResourceEntryDataBounds(ABuffer, AContainer, AEntry,
     DataOffset, DataLength) then
    Exit;

  if (DataLength mod UCD_RANGE_SIZE) <> 0 then
    Exit;

  RangeCount := DataLength div UCD_RANGE_SIZE;
  SetLength(ARanges, RangeCount);
  Offset := DataOffset;

  for I := 0 to RangeCount - 1 do
  begin
    ARanges[I].Lo := ReadUInt32LE(ABuffer, Offset);
    ARanges[I].Hi := ReadUInt32LE(ABuffer, Offset + 4);
    Inc(Offset, UCD_RANGE_SIZE);
  end;

  Result := True;
end;

function TryExtractCaseFoldPairs(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer;
  const AEntry: TEmbeddedResourceEntry;
  out APairs: TCodePointPairArray): Boolean;
var
  DataOffset, DataLength: Integer;
  PairCount, I, Offset: Integer;
begin
  Result := False;
  SetLength(APairs, 0);

  if not TryGetEmbeddedResourceEntryDataBounds(ABuffer, AContainer, AEntry,
     DataOffset, DataLength) then
    Exit;

  if (DataLength mod UCD_RANGE_SIZE) <> 0 then
    Exit;

  PairCount := DataLength div UCD_RANGE_SIZE;
  SetLength(APairs, PairCount);
  Offset := DataOffset;

  for I := 0 to PairCount - 1 do
  begin
    APairs[I].Source := ReadUInt32LE(ABuffer, Offset);
    APairs[I].Target := ReadUInt32LE(ABuffer, Offset + 4);
    Inc(Offset, UCD_RANGE_SIZE);
  end;

  Result := True;
end;

var
  UCDResourceCache: TLazyPublishedCache<TBytes>;
  CaseFoldPairsCache: TLazyPublishedCache<TCodePointPairArray>;
  NonUnicodeUppercasePairsCache: TLazyPublishedCache<TCodePointPairArray>;

function LoadUCDResource(const AKey: string; out ABuffer: TBytes): Boolean;
var
  Stream: TResourceStream;
  BufferSize: Integer;
begin
  Result := False;
  SetLength(ABuffer, 0);
  Stream := nil;
  try
    Stream := TResourceStream.Create(HInstance, AKey, UCD_RCDATA_RESOURCE_TYPE);
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

function TryReadEmbeddedResource(out ABuffer: TBytes): Boolean;
begin
  if UCDResourceCache.Ensure(GeneratedUnicodeDataResourceName, @LoadUCDResource) then
    ABuffer := UCDResourceCache.Data
  else
    SetLength(ABuffer, 0);
  Result := Length(ABuffer) > 0;
end;

function TryGetEmbeddedPropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
var
  Resource: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
begin
  Result := False;
  SetLength(ARanges, 0);

  if not TryReadEmbeddedResource(Resource) then
    Exit;

  if not TryReadEmbeddedResourceContainer(Resource, UCD_MAGIC, Container) then
    Exit;

  if not TryFindEmbeddedResourceEntry(Resource, AKey, Container, Entry) then
    Exit;

  Result := TryExtractRanges(Resource, Container, Entry, ARanges);
end;

{ Lazily load the immutable case-fold/uppercase pair table named AKey. The cold
  load (resource read, container/entry lookup, pair extraction) runs once; the
  table is then consulted on every code-point comparison of an icase
  backreference and of a unicode+icase \b assertion, so the warm path must not
  take a lock. The barrier-correct lazy-publication discipline lives in
  TLazyPublishedCache (introduced for these tables in #813, unified across the
  engine's embedded-data caches in #894): the loaded flag is published last
  behind a write barrier and read behind a matching read barrier, so a
  concurrent regex worker that observes the table as loaded also observes its
  fully-written contents on weakly-ordered targets. Load failure is memoized so
  an absent or corrupt resource is not re-attempted. Callers binary-search the
  cache's Data array in place (passed as a const argument, which makes no
  managed copy), avoiding per-comparison dynamic-array refcount churn. }
function LoadEmbeddedPairs(const AKey: string;
  out APairs: TCodePointPairArray): Boolean;
var
  Resource: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
begin
  SetLength(APairs, 0);
  Result :=
    TryReadEmbeddedResource(Resource) and
    TryReadEmbeddedResourceContainer(Resource, UCD_MAGIC, Container) and
    TryFindEmbeddedResourceEntry(Resource, AKey, Container, Entry) and
    TryExtractCaseFoldPairs(Resource, Container, Entry, APairs);
end;

function EnsureCaseFoldPairsReady: Boolean;
begin
  Result := CaseFoldPairsCache.Ensure(CASE_FOLDING_ENTRY_KEY, @LoadEmbeddedPairs);
end;

function EnsureNonUnicodeUppercasePairsReady: Boolean;
begin
  Result := NonUnicodeUppercasePairsCache.Ensure(NON_UNICODE_UPPERCASE_ENTRY_KEY,
    @LoadEmbeddedPairs);
end;

function TryFindPairTarget(const APairs: TCodePointPairArray;
  ASource: Cardinal; out ATarget: Cardinal): Boolean;
var
  LowIndex, HighIndex, MiddleIndex: Integer;
begin
  Result := False;
  ATarget := ASource;

  LowIndex := 0;
  HighIndex := High(APairs);
  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if APairs[MiddleIndex].Source = ASource then
    begin
      ATarget := APairs[MiddleIndex].Target;
      Result := True;
      Exit;
    end;
    if APairs[MiddleIndex].Source < ASource then
      LowIndex := MiddleIndex + 1
    else
      HighIndex := MiddleIndex - 1;
  end;
end;

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
begin
  Result := TryGetEmbeddedPropertyRanges(AKey, ARanges);
end;

function TryGetUnicodeSimpleCaseFold(ACodePoint: Cardinal;
  out AFoldedCodePoint: Cardinal): Boolean;
begin
  if EnsureCaseFoldPairsReady then
    Result := TryFindPairTarget(CaseFoldPairsCache.Data, ACodePoint,
      AFoldedCodePoint)
  else
  begin
    AFoldedCodePoint := ACodePoint;
    Result := False;
  end;
end;

function TryGetRegExpNonUnicodeUppercase(ACodePoint: Cardinal;
  out AUpperCodePoint: Cardinal): Boolean; forward;

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

  TryGetRegExpNonUnicodeUppercase(ACodePoint, Result);
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

procedure RemoveFoldRange(var ARanges: TUnicodePropertyRangeArray;
  ACodePoint: Cardinal);
var
  I, LastIndex: Integer;
begin
  I := 0;
  while I <= High(ARanges) do
  begin
    if (ACodePoint < ARanges[I].Lo) or (ACodePoint > ARanges[I].Hi) then
    begin
      Inc(I);
      Continue;
    end;

    if (ARanges[I].Lo = ACodePoint) and (ARanges[I].Hi = ACodePoint) then
    begin
      LastIndex := High(ARanges);
      while I < LastIndex do
      begin
        ARanges[I] := ARanges[I + 1];
        Inc(I);
      end;
      SetLength(ARanges, LastIndex);
      Exit;
    end;

    if ARanges[I].Lo = ACodePoint then
    begin
      Inc(ARanges[I].Lo);
      Exit;
    end;

    if ARanges[I].Hi = ACodePoint then
    begin
      Dec(ARanges[I].Hi);
      Exit;
    end;

    LastIndex := Length(ARanges);
    SetLength(ARanges, LastIndex + 1);
    ARanges[LastIndex].Lo := ACodePoint + 1;
    ARanges[LastIndex].Hi := ARanges[I].Hi;
    ARanges[I].Hi := ACodePoint - 1;
    Exit;
  end;
end;

function TryGetRegExpNonUnicodeUppercase(ACodePoint: Cardinal;
  out AUpperCodePoint: Cardinal): Boolean;
begin
  if EnsureNonUnicodeUppercasePairsReady then
    Result := TryFindPairTarget(NonUnicodeUppercasePairsCache.Data, ACodePoint,
      AUpperCodePoint)
  else
  begin
    AUpperCodePoint := ACodePoint;
    Result := False;
  end;
end;

procedure ExpandCaseEquivalence(var ARanges: TUnicodePropertyRangeArray;
  const APairs: TCodePointPairArray);
var
  OriginalRanges: TUnicodePropertyRangeArray;
  I, J: Integer;
  Target: Cardinal;
begin
  SetLength(OriginalRanges, Length(ARanges));
  for I := 0 to High(ARanges) do
    OriginalRanges[I] := ARanges[I];
  for I := 0 to High(APairs) do
  begin
    if not RangeContainsCodePoint(OriginalRanges, APairs[I].Source) and
       not RangeContainsCodePoint(OriginalRanges, APairs[I].Target) then
      Continue;

    Target := APairs[I].Target;
    AddFoldRange(ARanges, Target);
    for J := 0 to High(APairs) do
      if APairs[J].Target = Target then
        AddFoldRange(ARanges, APairs[J].Source);
  end;
end;

procedure ExpandUnicodeSimpleCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);
begin
  if EnsureCaseFoldPairsReady then
    ExpandCaseEquivalence(ARanges, CaseFoldPairsCache.Data);
end;

procedure ReduceUnicodeSimpleCaseFoldClosed(
  var ARanges: TUnicodePropertyRangeArray);
var
  OriginalRanges: TUnicodePropertyRangeArray;
  I, J: Integer;
  Target: Cardinal;
  HasInside, HasOutside: Boolean;
begin
  if not EnsureCaseFoldPairsReady then
    Exit;

  SetLength(OriginalRanges, Length(ARanges));
  for I := 0 to High(ARanges) do
    OriginalRanges[I] := ARanges[I];

  for I := 0 to High(CaseFoldPairsCache.Data) do
  begin
    Target := CaseFoldPairsCache.Data[I].Target;
    HasInside := RangeContainsCodePoint(OriginalRanges, Target);
    HasOutside := not HasInside;

    for J := 0 to High(CaseFoldPairsCache.Data) do
      if CaseFoldPairsCache.Data[J].Target = Target then
      begin
        if RangeContainsCodePoint(OriginalRanges,
           CaseFoldPairsCache.Data[J].Source) then
          HasInside := True
        else
          HasOutside := True;
      end;

    if not (HasInside and HasOutside) then
      Continue;

    RemoveFoldRange(ARanges, Target);
    for J := 0 to High(CaseFoldPairsCache.Data) do
      if CaseFoldPairsCache.Data[J].Target = Target then
        RemoveFoldRange(ARanges, CaseFoldPairsCache.Data[J].Source);
  end;
end;

procedure ExpandRegExpNonUnicodeCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);
begin
  if EnsureNonUnicodeUppercasePairsReady then
    ExpandCaseEquivalence(ARanges, NonUnicodeUppercasePairsCache.Data);
end;

initialization
  UCDResourceCache.Init;
  CaseFoldPairsCache.Init;
  NonUnicodeUppercasePairsCache.Init;

finalization
  NonUnicodeUppercasePairsCache.Done;
  CaseFoldPairsCache.Done;
  UCDResourceCache.Done;

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

procedure ReduceUnicodeSimpleCaseFoldClosed(
  var ARanges: TUnicodePropertyRangeArray);
begin
end;

procedure ExpandRegExpNonUnicodeCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);
var
  OriginalLength, I, NewIndex: Integer;

  procedure AddRange(ALo, AHi: Cardinal);
  begin
    NewIndex := Length(ARanges);
    SetLength(ARanges, NewIndex + 1);
    ARanges[NewIndex].Lo := ALo;
    ARanges[NewIndex].Hi := AHi;
  end;

  procedure AddShiftedOverlap(const ARange: TUnicodePropertyRange;
    ALo, AHi: Cardinal; AShift: Integer);
  var
    Lo, Hi: Cardinal;
  begin
    Lo := ARange.Lo;
    if Lo < ALo then
      Lo := ALo;
    Hi := ARange.Hi;
    if Hi > AHi then
      Hi := AHi;
    if Lo <= Hi then
      AddRange(Cardinal(Integer(Lo) + AShift), Cardinal(Integer(Hi) + AShift));
  end;

begin
  OriginalLength := Length(ARanges);
  for I := 0 to OriginalLength - 1 do
  begin
    AddShiftedOverlap(ARanges[I], Ord('A'), Ord('Z'), Ord('a') - Ord('A'));
    AddShiftedOverlap(ARanges[I], Ord('a'), Ord('z'), Ord('A') - Ord('a'));
  end;
end;

{$ENDIF}

end.
