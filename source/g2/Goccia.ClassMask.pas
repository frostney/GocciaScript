unit Goccia.ClassMask;

{$I Goccia.inc}

// Dispatch on an AST node's class without a chain of `is` tests. Each `is`
// walks the class's parents, and a node that matches late in a chain, or not
// at all, fails every test before it: the binding collectors and the
// compiler's dispatch ran twenty such tests per node.
//
// ClassMaskOf returns one bit per listed class the object inherits from,
// cached per exact class. Testing the bits in the chain's order gives the
// chain's result, including for a subclass and for a branch whose extra
// condition fails and falls through to later tests.

interface

type
  TClassMaskCache = record
    Keys: array[0..511] of TClass;
    Masks: array[0..511] of Cardinal;
    Count: Integer;
  end;

// AClasses holds at most 32 classes. A nil object has an empty mask, as `is`
// is False for nil.
function ClassMaskOf(var ACache: TClassMaskCache;
  const AClasses: array of TClass; const AObject: TObject): Cardinal;

implementation

function ComputeMask(const AClasses: array of TClass;
  const AClass: TClass): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AClasses) do
    if AClass.InheritsFrom(AClasses[I]) then
      Result := Result or (Cardinal(1) shl I);
end;

function ClassMaskOf(var ACache: TClassMaskCache;
  const AClasses: array of TClass; const AObject: TObject): Cardinal;
var
  K: TClass;
  H: Integer;
begin
  if AObject = nil then
    Exit(0);
  K := AObject.ClassType;
  H := Integer((PtrUInt(K) shr 4) and 511);
  while ACache.Keys[H] <> nil do
  begin
    if ACache.Keys[H] = K then
      Exit(ACache.Masks[H]);
    H := (H + 1) and 511;
  end;
  Result := ComputeMask(AClasses, K);
  // Half full at most, so a probe always ends at an empty slot.
  if ACache.Count < 256 then
  begin
    ACache.Keys[H] := K;
    ACache.Masks[H] := Result;
    Inc(ACache.Count);
  end;
end;

end.
