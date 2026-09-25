unit Goccia.Arguments.ArrayLike;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ArrayValue,
  Goccia.Values.Primitives;

// ES2026 §7.3.19 CreateListFromArrayLike(obj [, elementTypes])
// Converts an array or array-like object to a TGocciaArgumentsCollection.
// Throws TypeError if the value is not an object.
function CreateListFromArrayLike(const AValue: TGocciaValue; const AMethodName: string): TGocciaArgumentsCollection;

// True when every index below the array's length is a plain dense data element.
// Callers use this to decide whether an argument list can be read straight out
// of the element storage: on a dense hole-free array such a read observes no
// accessor and therefore runs no guest code, so no intermediate needs a GC root
// and no read order is observable. Both conditions matter:
//   - a hole is not an own property, so resolving it per ES2026 §7.3.19 step 6b
//     continues up the prototype chain and can invoke an inherited accessor;
//   - a length grown past the dense element count (`a.length = 5`) means the
//     element count is not LengthOfArrayLike, so the generic path has to run to
//     produce the spec argument count and the RangeError ceiling.
// An own accessor on an index is covered too: defining one punches a hole in the
// dense storage (Goccia.Values.ArrayValue DefineProperty).
function IsDenseHoleFreeArgumentArray(const AArray: TGocciaArrayValue): Boolean;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.ObjectValue;

const
  // Ceiling for array-like argument lists. The ES spec caps ToLength at 2^53−1,
  // but allocating that many elements is infeasible. This limit prevents
  // pathological OOM from script-controlled length values like { length: 2e9 }.
  MAX_ARGUMENTS_LIST_LENGTH = 1048576; // 2^20

function IsDenseHoleFreeArgumentArray(const AArray: TGocciaArrayValue): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not AArray.HasDenseElementLength then
    Exit;

  for I := 0 to AArray.Elements.Count - 1 do
    if AArray.Elements[I] = TGocciaHoleValue.HoleValue then
      Exit;

  Result := True;
end;

// ES2026 §7.3.19 CreateListFromArrayLike(obj [, elementTypes])
function CreateListFromArrayLike(const AValue: TGocciaValue; const AMethodName: string): TGocciaArgumentsCollection;
var
  ArrVal: TGocciaArrayValue;
  ArrayObj: TGocciaObjectValue;
  LengthProp: TGocciaValue;
  Len, I: Integer;
  Element: TGocciaValue;
begin
  // ES2026 §7.3.19 step 2: If obj is not an Object, throw a TypeError exception
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(Format('%s: argumentsList must be an array-like object', [AMethodName]));

  // Fast path: dense hole-free TGocciaArrayValue — direct element access.
  // Every other array (holes, or a length grown past the dense element count)
  // falls through to the generic path so that the argument count is
  // LengthOfArrayLike and every index is read with Get.
  if (AValue is TGocciaArrayValue) and
     IsDenseHoleFreeArgumentArray(TGocciaArrayValue(AValue)) then
  begin
    ArrVal := TGocciaArrayValue(AValue);
    Result := TGocciaArgumentsCollection.CreateWithCapacity(ArrVal.Elements.Count);
    for I := 0 to ArrVal.Elements.Count - 1 do
      Result.Add(ArrVal.Elements[I]);
    Exit;
  end;

  // ES2026 §7.3.19 step 3: Let len be ? LengthOfArrayLike(obj)
  // ES2026 §7.3.3 LengthOfArrayLike: ToLength(? Get(obj, "length"))
  // ES2026 §7.1.22 ToLength: NaN/negative → 0, spec caps at 2^53−1.
  ArrayObj := TGocciaObjectValue(AValue);
  LengthProp := ArrayObj.GetProperty(PROP_LENGTH);
  if not Assigned(LengthProp) or
     (LengthProp is TGocciaUndefinedLiteralValue) or
     (LengthProp is TGocciaNullLiteralValue) then
    Len := 0
  else
    Len := ToLengthValue(LengthProp);

  // Guard against pathological lengths before allocating
  if Len > MAX_ARGUMENTS_LIST_LENGTH then
    ThrowRangeError(Format('%s: arguments list length %d exceeds maximum of %d',
      [AMethodName, Len, MAX_ARGUMENTS_LIST_LENGTH]));

  // ES2026 §7.3.19 steps 4-6: Iterate and collect elements
  Result := TGocciaArgumentsCollection.CreateWithCapacity(Len);
  for I := 0 to Len - 1 do
  begin
    // ES2026 §7.3.19 step 6b: Let next be ? Get(obj, indexName)
    Element := ArrayObj.GetProperty(IntToStr(I));
    if not Assigned(Element) then
      Element := TGocciaUndefinedLiteralValue.UndefinedValue;
    Result.Add(Element);
  end;
end;

end.
