{
  TGocciaOrderedValueMap — the SameValueZero-keyed, insertion-ordered store
  that backs strong `Map` and `Set`.

  It is the single specialization of the generic insertion-ordered hash map
  (TOrderedMap, see ADR 0019) for ECMAScript keyed collections: it overrides
  HashKey/KeysEqual with a hash that is exactly consistent with
  SameValueZero (ES2026 §7.2.11 / Goccia.Arithmetic.IsSameValueZero), and
  overrides CanCompact so live, index-based Map/Set iterators never observe a
  renumbering compaction.

  Both Map (key -> value) and Set (key -> key) use this one type; there is no
  per-collection subclass. Identity-keyed weak collections keep using THashMap
  (WeakMap/WeakSet) — the content-vs-identity distinction is recorded in
  docs/adr/0077.

  Hashing must stay consistent with IsSameValueZero. The invariant
  "IsSameValueZero(a, b) implies HashValue(a) = HashValue(b)" is enforced by
  Goccia.Values.OrderedValueMap.Test.pas; an inconsistent hash silently loses
  entries, so change the two in lockstep.
}

unit Goccia.Values.OrderedValueMap;

{$I Goccia.inc}

interface

uses
  OrderedMap,

  Goccia.Values.Primitives;

type
  TGocciaOrderedValueMap = class(TOrderedMap<TGocciaValue, TGocciaValue>)
  private
    // While > 0 a live Map/Set iterator (or forEach) holds index-based
    // cursors into the entry array; compaction (which renumbers entries) is
    // suppressed until every cursor is released so the cursors stay valid.
    FActiveIterators: Integer;
  protected
    function HashKey(const AKey: TGocciaValue): Cardinal; override;
    function KeysEqual(const A, B: TGocciaValue): Boolean; override;
    function CanCompact: Boolean; override;
  public
    // ES2026 §24.5.1 CanonicalizeKeyedCollectionKey: -0 is stored as +0 so
    // keys()/values()/entries()/forEach observe +0. SameValueZero already
    // treats -0 and +0 as equal for lookup, so callers only need this for the
    // value that gets stored.
    class function CanonicalizeKey(const AKey: TGocciaValue): TGocciaValue; static;
    {$IFDEF FPC}inline;{$ENDIF}

    // Insert or in-place update (keeping the original insertion position),
    // canonicalizing the key first.
    procedure SetEntry(const AKey, AValue: TGocciaValue);

    // Insert a Set member: the canonical element is stored as both key and
    // value, so iteration (values/entries/forEach) observes the canonical form
    // (-0 as +0). Keeping this in the store means Set callers cannot forget to
    // canonicalize the value slot.
    procedure AddSetMember(const AValue: TGocciaValue);

    // Live, insertion-ordered cursor over active entries. ACursor is a
    // physical index; seed it with 0 and keep calling until False. New
    // entries appended during iteration are visited and deleted (tombstoned)
    // entries are skipped, matching the spec [[MapData]] iteration model.
    function NextEntry(var ACursor: Integer;
      out AKey, AValue: TGocciaValue): Boolean;
      {$IFDEF FPC}inline;{$ENDIF}

    // Bracket a live iteration so compaction cannot renumber entries mid-walk.
    procedure RetainIterator;
    {$IFDEF FPC}inline;{$ENDIF}
    procedure ReleaseIterator;
    {$IFDEF FPC}inline;{$ENDIF}

    property ActiveIterators: Integer read FActiveIterators;
  end;

implementation

uses
  BigInteger,
  NumberBits,
  TextSemantics,

  Goccia.Arithmetic,
  Goccia.Values.BigIntValue;

const
  // Distinct seeds per primitive category. Cross-type values are never
  // SameValueZero-equal, so these only need to be deterministic; the seeds
  // just keep categories from colliding for better probe distribution.
  HASH_UNDEFINED = Cardinal($9E3779B1);
  HASH_NULL      = Cardinal($85EBCA77);
  HASH_TRUE      = Cardinal($C2B2AE3D);
  HASH_FALSE     = Cardinal($27D4EB2F);
  HASH_ZERO      = Cardinal($165667B1);
  HASH_NAN       = Cardinal($D6E8FEB8);

{$IFDEF FPC}
  {$PUSH}
{$ENDIF}
{$R-}{$Q-}
function TGocciaOrderedValueMap.HashKey(const AKey: TGocciaValue): Cardinal;
var
  Num: TGocciaNumberLiteralValue;
  D: Double;
  Bits: UInt64;
  S: string;
  I: Integer;
begin
  // Numbers: canonicalize NaN (all NaNs are SameValueZero-equal) and signed
  // zero (-0 and +0 are equal); otherwise mix the raw IEEE-754 bits, which
  // are identical for any two doubles that compare equal (except +0/-0, just
  // handled).
  if AKey is TGocciaNumberLiteralValue then
  begin
    Num := TGocciaNumberLiteralValue(AKey);
    if Num.IsNaN then
      Exit(HASH_NAN);
    D := Num.Value;
    if D = 0 then
      Exit(HASH_ZERO);
    Bits := DoubleToBits(D);
    Bits := (Bits xor (Bits shr 4)) * UInt64(11400714819323198485);
    Exit(Cardinal(Bits xor (Bits shr 32)));
  end;

  // Strings: hash the ECMAScript UTF-16 value, not its internal UTF-8/WTF-8
  // byte representation. This mirrors OrderedStringMap and SameValueZero.
  if AKey is TGocciaStringLiteralValue then
    Exit(UTF16StringHash(TGocciaStringLiteralValue(AKey).Value));

  if AKey is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(AKey).Value then
      Exit(HASH_TRUE)
    else
      Exit(HASH_FALSE);
  end;

  // BigInt: hash the canonical decimal string. TBigInteger is normalized, so
  // Value.Equal(other) holds iff the decimal strings match.
  if AKey is TGocciaBigIntValue then
  begin
    S := TGocciaBigIntValue(AKey).Value.ToString;
    Result := 5381;
    for I := 1 to Length(S) do
      Result := Result * 33 + Ord(S[I]);
    Exit(Result xor Cardinal($9747B28C));
  end;

  if AKey is TGocciaUndefinedLiteralValue then
    Exit(HASH_UNDEFINED);
  if AKey is TGocciaNullLiteralValue then
    Exit(HASH_NULL);

  // Objects, functions, symbols: SameValueZero is reference identity, so hash
  // the pointer. The GC never relocates objects, so the pointer is stable for
  // the entry's lifetime.
  Bits := UInt64(NativeUInt(AKey));
  Bits := (Bits xor (Bits shr 4)) * UInt64(11400714819323198485);
  Result := Cardinal(Bits xor (Bits shr 32));
end;
{$IFDEF FPC}
  {$POP}
{$ELSE}
  {$IFNDEF PRODUCTION}{$R+}{$Q+}{$ENDIF}
{$ENDIF}

function TGocciaOrderedValueMap.KeysEqual(const A, B: TGocciaValue): Boolean;
begin
  // The equality half of the hash/equality pair must be the exact function
  // SameValueZero hashing above is consistent with.
  Result := IsSameValueZero(A, B);
end;

function TGocciaOrderedValueMap.CanCompact: Boolean;
begin
  Result := FActiveIterators = 0;
end;

class function TGocciaOrderedValueMap.CanonicalizeKey(
  const AKey: TGocciaValue): TGocciaValue;
begin
  if (AKey is TGocciaNumberLiteralValue) and
     TGocciaNumberLiteralValue(AKey).IsNegativeZero then
    Result := TGocciaNumberLiteralValue.ZeroValue
  else
    Result := AKey;
end;

procedure TGocciaOrderedValueMap.SetEntry(const AKey, AValue: TGocciaValue);
begin
  // inherited Add updates the value in place when the key already exists,
  // preserving the entry's original insertion position (Map.set/Set.add
  // semantics).
  Add(CanonicalizeKey(AKey), AValue);
end;

procedure TGocciaOrderedValueMap.AddSetMember(const AValue: TGocciaValue);
var
  Canonical: TGocciaValue;
begin
  Canonical := CanonicalizeKey(AValue);
  Add(Canonical, Canonical);
end;

function TGocciaOrderedValueMap.NextEntry(var ACursor: Integer;
  out AKey, AValue: TGocciaValue): Boolean;
begin
  Result := GetNextEntry(ACursor, AKey, AValue);
end;

procedure TGocciaOrderedValueMap.RetainIterator;
begin
  Inc(FActiveIterators);
end;

procedure TGocciaOrderedValueMap.ReleaseIterator;
begin
  if FActiveIterators > 0 then
    Dec(FActiveIterators);
end;

end.
