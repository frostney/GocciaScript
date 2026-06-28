unit Goccia.Identifier;

{$I Goccia.inc}

interface

function IsIdentifierStartCodePoint(ACodePoint: Cardinal): Boolean;
function IsIdentifierPartCodePoint(ACodePoint: Cardinal): Boolean;

implementation

uses
  SysUtils,

  LazyPublishedCache,
  UnicodeICU,

  Goccia.RegExp.UnicodeData;

const
  IDENTIFIER_START_PROPERTY = 'ID_Start';
  IDENTIFIER_PART_PROPERTY = 'ID_Continue';
  ZERO_WIDTH_NON_JOINER_CODE_POINT = $200C;
  ZERO_WIDTH_JOINER_CODE_POINT = $200D;

var
  IdentifierStartCache: TLazyPublishedCache<TUnicodePropertyRangeArray>;
  IdentifierPartCache: TLazyPublishedCache<TUnicodePropertyRangeArray>;

function IsASCIIIdentifierStartCodePoint(ACodePoint: Cardinal): Boolean;
begin
  Result := ((ACodePoint >= Ord('a')) and (ACodePoint <= Ord('z'))) or
            ((ACodePoint >= Ord('A')) and (ACodePoint <= Ord('Z'))) or
            (ACodePoint = Ord('_')) or (ACodePoint = Ord('$'));
end;

function IsASCIIIdentifierPartCodePoint(ACodePoint: Cardinal): Boolean;
begin
  Result := IsASCIIIdentifierStartCodePoint(ACodePoint) or
            ((ACodePoint >= Ord('0')) and (ACodePoint <= Ord('9')));
end;

function TryLoadIdentifierRanges(const APropertyName: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
begin
  Result := TryGetUnicodePropertyRanges(APropertyName, ARanges);
  if not Result then
    Result := TryICUGetUnicodePropertyRanges(APropertyName, '', ARanges);
end;

function RangeContainsCodePoint(const ARanges: TUnicodePropertyRangeArray;
  ACodePoint: Cardinal): Boolean;
var
  LowIndex, HighIndex, MiddleIndex: Integer;
begin
  Result := False;
  LowIndex := 0;
  HighIndex := High(ARanges);
  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if ACodePoint < ARanges[MiddleIndex].Lo then
      HighIndex := MiddleIndex - 1
    else if ACodePoint > ARanges[MiddleIndex].Hi then
      LowIndex := MiddleIndex + 1
    else
      Exit(True);
  end;
end;

// ES2026 §12.7 IdentifierStartChar
function IsIdentifierStartCodePoint(ACodePoint: Cardinal): Boolean;
begin
  if ACodePoint > $10FFFF then
    Exit(False);
  if IsASCIIIdentifierStartCodePoint(ACodePoint) then
    Exit(True);
  if ACodePoint <= $7F then
    Exit(False);
  if not IdentifierStartCache.Ensure(IDENTIFIER_START_PROPERTY,
     @TryLoadIdentifierRanges) then
    Exit(False);
  Result := RangeContainsCodePoint(IdentifierStartCache.Data, ACodePoint);
end;

// ES2026 §12.7 IdentifierPartChar
function IsIdentifierPartCodePoint(ACodePoint: Cardinal): Boolean;
begin
  if ACodePoint > $10FFFF then
    Exit(False);
  if IsASCIIIdentifierPartCodePoint(ACodePoint) then
    Exit(True);
  if (ACodePoint = ZERO_WIDTH_NON_JOINER_CODE_POINT) or
     (ACodePoint = ZERO_WIDTH_JOINER_CODE_POINT) then
    Exit(True);
  if ACodePoint <= $7F then
    Exit(False);
  if not IdentifierPartCache.Ensure(IDENTIFIER_PART_PROPERTY,
     @TryLoadIdentifierRanges) then
    Exit(False);
  Result := RangeContainsCodePoint(IdentifierPartCache.Data, ACodePoint);
end;

initialization
  IdentifierStartCache.Init;
  IdentifierPartCache.Init;

finalization
  IdentifierPartCache.Done;
  IdentifierStartCache.Done;

end.
