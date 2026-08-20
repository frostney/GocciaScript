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

// ES2026 §7.3.19 step 6b: Let next be ? Get(obj, indexName).
// Reads one index of a dense array with [[Get]] semantics: a hole is not an own
// property, so the read continues up the prototype chain (ES2026 §10.1.8.1
// OrdinaryGet step 2) and yields undefined only when nothing inherits the index.
// Argument-list materialization must never hand a hole sentinel to a callee.
function ArrayArgumentElement(const AArray: TGocciaArrayValue;
  const AIndex: Integer): TGocciaValue;

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

function ArrayArgumentElement(const AArray: TGocciaArrayValue;
  const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < AArray.Elements.Count) then
  begin
    Result := AArray.Elements[AIndex];
    if Result <> TGocciaHoleValue.HoleValue then
      Exit;
  end;

  // Hole or out-of-range: resolve through the full property lookup so
  // inherited index properties and accessors on Array.prototype are observed.
  Result := AArray.GetProperty(IntToStr(AIndex));
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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

  // Fast path: TGocciaArrayValue — direct element access
  if AValue is TGocciaArrayValue then
  begin
    ArrVal := TGocciaArrayValue(AValue);
    Result := TGocciaArgumentsCollection.CreateWithCapacity(ArrVal.Elements.Count);
    for I := 0 to ArrVal.Elements.Count - 1 do
      Result.Add(ArrayArgumentElement(ArrVal, I));
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
