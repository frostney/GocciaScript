unit Goccia.RegExp.UnicodeData;

{$I Goccia.inc}

interface

uses
  UnicodeICU;

type
  TUnicodeStringSequence = record
    CodePoints: array of Cardinal;
  end;

  TUnicodeStringSequenceArray = array of TUnicodeStringSequence;

function TryGetUnicodePropertyRanges(const AKey: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
function TryGetUnicodeStringPropertySequences(const AKey: string;
  out ASequences: TUnicodeStringSequenceArray): Boolean;
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

  // Indexes into a TCodePointPairArray of N pairs, two per pair: [2 * I] is
  // the first pair with pair I's target, [2 * I + 1] the next pair after I
  // with that target, or -1. Following them visits the pairs with one target
  // in table order, as a scan of the whole table would. Then [2 * N + K] is
  // the K-th pair in (target, index) order.
  TPairGroupArray = array of Integer;

const
  UCD_RANGE_SIZE = 8;
  CASE_FOLDING_ENTRY_KEY = 'CaseFolding/Simple';
  NON_UNICODE_UPPERCASE_ENTRY_KEY = 'CaseMapping/RegExpNonUnicodeUppercase';
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

function TryExtractStringSequences(const ABuffer: TBytes;
  const AContainer: TEmbeddedResourceContainer;
  const AEntry: TEmbeddedResourceEntry;
  out ASequences: TUnicodeStringSequenceArray): Boolean;
var
  DataOffset, DataLength: Integer;
  I, J, MaxSequences, Offset, SequenceCount, SequenceLength: Integer;
  SequenceCountValue, SequenceLengthValue: UInt32;
begin
  Result := False;
  SetLength(ASequences, 0);

  if not TryGetEmbeddedResourceEntryDataBounds(ABuffer, AContainer, AEntry,
     DataOffset, DataLength) then
    Exit;

  if DataLength < SizeOf(UInt32) then
    Exit;

  Offset := DataOffset;
  SequenceCountValue := ReadUInt32LE(ABuffer, Offset);
  Inc(Offset, SizeOf(UInt32));
  MaxSequences := (DataOffset + DataLength - Offset) div SizeOf(UInt32);
  if SequenceCountValue > UInt32(MaxSequences) then
    Exit;
  SequenceCount := Integer(SequenceCountValue);
  SetLength(ASequences, SequenceCount);

  for I := 0 to SequenceCount - 1 do
  begin
    if Offset + SizeOf(UInt32) > DataOffset + DataLength then
      Exit(False);
    SequenceLengthValue := ReadUInt32LE(ABuffer, Offset);
    Inc(Offset, SizeOf(UInt32));
    if SequenceLengthValue > UInt32((DataOffset + DataLength - Offset) div
       SizeOf(UInt32)) then
      Exit(False);
    SequenceLength := Integer(SequenceLengthValue);
    SetLength(ASequences[I].CodePoints, SequenceLength);
    for J := 0 to SequenceLength - 1 do
    begin
      ASequences[I].CodePoints[J] := ReadUInt32LE(ABuffer, Offset);
      Inc(Offset, SizeOf(UInt32));
    end;
  end;

  Result := Offset = DataOffset + DataLength;
end;

var
  UCDResourceCache: TLazyPublishedCache<TBytes>;
  CaseFoldPairsCache: TLazyPublishedCache<TCodePointPairArray>;
  NonUnicodeUppercasePairsCache: TLazyPublishedCache<TCodePointPairArray>;
  // Per pair table, the pairs that share a target (see BuildPairGroups).
  CaseFoldGroupsCache: TLazyPublishedCache<TPairGroupArray>;
  NonUnicodeUppercaseGroupsCache: TLazyPublishedCache<TPairGroupArray>;

{$IFDEF LAKON}
function LoadUCDResource(const AKey: string; out ABuffer: TBytes): Boolean;
var
  I: Integer;
begin
  { Lakon has no Windows-resource loader; Generated.UnicodeData ships
    the identical GOCCIA_UCD container as a Pascal typed constant
    (see Generated.UnicodeData.Blob.inc). }
  SetLength(ABuffer, Length(GeneratedUnicodeDataResourceBlob));
  for I := 0 to High(GeneratedUnicodeDataResourceBlob) do
    ABuffer[I] := GeneratedUnicodeDataResourceBlob[I];
  Result := True;
end;
{$ELSE}
function LoadUCDResource(const AKey: string; out ABuffer: TBytes): Boolean;
var
  Stream: TResourceStream;
  BufferSize: Integer;
begin
  Result := False;
  SetLength(ABuffer, 0);
  Stream := nil;
  try
    Stream := TResourceStream.Create(HInstance, AKey, RCDATAResourceType);
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
{$ENDIF}

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

function BuildPairGroups(const APairs: TCodePointPairArray): TPairGroupArray;
var
  Order: array of Integer;
  I, J, K, Gap, Tmp, First: Integer;

  function Less(const A, B: Integer): Boolean;
  begin
    Result := (APairs[A].Target < APairs[B].Target) or
      ((APairs[A].Target = APairs[B].Target) and (A < B));
  end;

begin
  SetLength(Result, 3 * Length(APairs));
  SetLength(Order, Length(APairs));
  for I := 0 to High(Order) do
    Order[I] := I;
  // Shell sort by (target, index): once per table.
  Gap := Length(Order) div 2;
  while Gap > 0 do
  begin
    for I := Gap to High(Order) do
    begin
      Tmp := Order[I];
      J := I;
      while (J >= Gap) and Less(Tmp, Order[J - Gap]) do
      begin
        Order[J] := Order[J - Gap];
        Dec(J, Gap);
      end;
      Order[J] := Tmp;
    end;
    Gap := Gap div 2;
  end;
  I := 0;
  while I <= High(Order) do
  begin
    First := Order[I];
    J := I;
    while (J + 1 <= High(Order)) and
          (APairs[Order[J + 1]].Target = APairs[First].Target) do
      Inc(J);
    for K := I to J do
    begin
      Result[2 * Order[K]] := First;
      if K < J then
        Result[2 * Order[K] + 1] := Order[K + 1]
      else
        Result[2 * Order[K] + 1] := -1;
    end;
    I := J + 1;
  end;
  for I := 0 to High(Order) do
    Result[2 * Length(APairs) + I] := Order[I];
end;

function LoadPairGroups(const AKey: string;
  out AGroups: TPairGroupArray): Boolean;
begin
  SetLength(AGroups, 0);
  if AKey = CASE_FOLDING_ENTRY_KEY then
  begin
    Result := EnsureCaseFoldPairsReady;
    if Result then
      AGroups := BuildPairGroups(CaseFoldPairsCache.Data);
  end
  else
  begin
    Result := EnsureNonUnicodeUppercasePairsReady;
    if Result then
      AGroups := BuildPairGroups(NonUnicodeUppercasePairsCache.Data);
  end;
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

function TryGetUnicodeStringPropertySequences(const AKey: string;
  out ASequences: TUnicodeStringSequenceArray): Boolean;
var
  Resource: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
begin
  SetLength(ASequences, 0);
  Result :=
    TryReadEmbeddedResource(Resource) and
    TryReadEmbeddedResourceContainer(Resource, UCD_MAGIC, Container) and
    TryFindEmbeddedResourceEntry(Resource, AKey, Container, Entry) and
    TryExtractStringSequences(Resource, Container, Entry, ASequences);
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

type
  TPairIndexArray = array of Integer;

const
  // Classes of at most this many code points find their pairs by lookup
  // instead of testing every pair (a case-insensitive literal character is
  // a class of one).
  SMALL_CLASS_CODE_POINTS = 256;

// The pairs whose source or target is in ARanges (at most
// SMALL_CLASS_CODE_POINTS code points), in table order.
function SmallClassPairs(const ARanges: TUnicodePropertyRangeArray;
  const APairs: TCodePointPairArray; const AGroups: TPairGroupArray): TPairIndexArray;
var
  Count, I, Lo, Hi, Mid, K, N, Tmp, J: Integer;
  C: Cardinal;

  procedure Add(const AIndex: Integer);
  begin
    if Count = Length(Result) then
      SetLength(Result, Count * 2 + 8);
    Result[Count] := AIndex;
    Inc(Count);
  end;

begin
  Count := 0;
  N := Length(APairs);
  Result := nil;
  for I := 0 to High(ARanges) do
  begin
    C := ARanges[I].Lo;
    while C <= ARanges[I].Hi do
    begin
      // by source (the table's order)
      Lo := 0;
      Hi := N - 1;
      while Lo < Hi do
      begin
        Mid := (Lo + Hi) div 2;
        if APairs[Mid].Source < C then Lo := Mid + 1 else Hi := Mid;
      end;
      K := Lo;
      while (K < N) and (APairs[K].Source = C) do
      begin
        Add(K);
        Inc(K);
      end;
      // by target ([2 * N + K]: the pairs in target order)
      Lo := 0;
      Hi := N - 1;
      while Lo < Hi do
      begin
        Mid := (Lo + Hi) div 2;
        if APairs[AGroups[2 * N + Mid]].Target < C then Lo := Mid + 1 else Hi := Mid;
      end;
      K := Lo;
      while (K < N) and (APairs[AGroups[2 * N + K]].Target = C) do
      begin
        Add(AGroups[2 * N + K]);
        Inc(K);
      end;
      if C = High(Cardinal) then Break;
      Inc(C);
    end;
  end;
  // table order, once each
  for I := 1 to Count - 1 do
  begin
    Tmp := Result[I];
    J := I - 1;
    while (J >= 0) and (Result[J] > Tmp) do
    begin
      Result[J + 1] := Result[J];
      Dec(J);
    end;
    Result[J + 1] := Tmp;
  end;
  K := 0;
  for I := 0 to Count - 1 do
    if (K = 0) or (Result[K - 1] <> Result[I]) then
    begin
      Result[K] := Result[I];
      Inc(K);
    end;
  SetLength(Result, K);
end;

function ClassCodePointCount(const ARanges: TUnicodePropertyRangeArray): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(ARanges) do
    if ARanges[I].Hi >= ARanges[I].Lo then
      Inc(Result, Int64(ARanges[I].Hi) - ARanges[I].Lo + 1);
end;

procedure ExpandCaseEquivalence(var ARanges: TUnicodePropertyRangeArray;
  const APairs: TCodePointPairArray; const AGroups: TPairGroupArray);
var
  OriginalRanges: TUnicodePropertyRangeArray;
  Hits: TPairIndexArray;
  I, J, H: Integer;
  Small: Boolean;
begin
  SetLength(OriginalRanges, Length(ARanges));
  for I := 0 to High(ARanges) do
    OriginalRanges[I] := ARanges[I];
  Small := (Length(APairs) > 0) and
    (ClassCodePointCount(OriginalRanges) <= SMALL_CLASS_CODE_POINTS);
  if Small then
    Hits := SmallClassPairs(OriginalRanges, APairs, AGroups);
  if Small then H := Length(Hits) else H := Length(APairs);
  for J := 0 to H - 1 do
  begin
    if Small then
      I := Hits[J]
    else
    begin
      I := J;
      if not RangeContainsCodePoint(OriginalRanges, APairs[I].Source) and
         not RangeContainsCodePoint(OriginalRanges, APairs[I].Target) then
        Continue;
    end;

    AddFoldRange(ARanges, APairs[I].Target);
    // every pair with this target, in table order
    I := AGroups[2 * I];
    while I >= 0 do
    begin
      AddFoldRange(ARanges, APairs[I].Source);
      I := AGroups[2 * I + 1];
    end;
  end;
end;

procedure ExpandUnicodeSimpleCaseFolding(
  var ARanges: TUnicodePropertyRangeArray);
begin
  if EnsureCaseFoldPairsReady and
     CaseFoldGroupsCache.Ensure(CASE_FOLDING_ENTRY_KEY, @LoadPairGroups) then
    ExpandCaseEquivalence(ARanges, CaseFoldPairsCache.Data,
      CaseFoldGroupsCache.Data);
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
  if EnsureNonUnicodeUppercasePairsReady and
     NonUnicodeUppercaseGroupsCache.Ensure(NON_UNICODE_UPPERCASE_ENTRY_KEY,
       @LoadPairGroups) then
    ExpandCaseEquivalence(ARanges, NonUnicodeUppercasePairsCache.Data,
      NonUnicodeUppercaseGroupsCache.Data);
end;

initialization
  UCDResourceCache.Init;
  CaseFoldPairsCache.Init;
  NonUnicodeUppercasePairsCache.Init;
  CaseFoldGroupsCache.Init;
  NonUnicodeUppercaseGroupsCache.Init;

finalization
  NonUnicodeUppercaseGroupsCache.Done;
  CaseFoldGroupsCache.Done;
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

function TryGetUnicodeStringPropertySequences(const AKey: string;
  out ASequences: TUnicodeStringSequenceArray): Boolean;
begin
  Result := False;
  SetLength(ASequences, 0);
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
