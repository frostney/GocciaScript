unit Goccia.Arguments.ArrayLike;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

// ES2026 §7.3.18 CreateListFromArrayLike(obj [, elementTypes])
// Converts an array or array-like object to a TGocciaArgumentsCollection.
// Throws TypeError if the value is not an object.
function CreateListFromArrayLike(const AValue: TGocciaValue; const AMethodName: string): TGocciaArgumentsCollection;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue;

// ES2026 §7.3.18 CreateListFromArrayLike(obj [, elementTypes])
function CreateListFromArrayLike(const AValue: TGocciaValue; const AMethodName: string): TGocciaArgumentsCollection;
var
  ArrVal: TGocciaArrayValue;
  ArrayObj: TGocciaObjectValue;
  LengthProp: TGocciaValue;
  Len, I: Integer;
  Element: TGocciaValue;
begin
  // ES2026 §7.3.18 step 1: If obj is not an Object, throw a TypeError exception
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(Format('%s: argumentsList must be an array-like object', [AMethodName]));

  // Fast path: TGocciaArrayValue — direct element access
  if AValue is TGocciaArrayValue then
  begin
    ArrVal := TGocciaArrayValue(AValue);
    Result := TGocciaArgumentsCollection.CreateWithCapacity(ArrVal.Elements.Count);
    for I := 0 to ArrVal.Elements.Count - 1 do
      Result.Add(ArrVal.Elements[I]);
    Exit;
  end;

  // ES2026 §7.3.18 step 2: Let len be ? LengthOfArrayLike(obj)
  // ES2026 §7.3.3 LengthOfArrayLike: ToLength(? Get(obj, "length"))
  ArrayObj := TGocciaObjectValue(AValue);
  LengthProp := ArrayObj.GetProperty(PROP_LENGTH);
  if (LengthProp is TGocciaUndefinedLiteralValue) or
     (LengthProp is TGocciaNullLiteralValue) then
    Len := 0
  else
  begin
    Len := Trunc(LengthProp.ToNumberLiteral.Value);
    // ES2026 §7.1.22 ToLength step 2: If len <= 0, return +0
    if Len < 0 then
      Len := 0;
  end;

  // ES2026 §7.3.18 steps 3-5: Iterate and collect elements
  Result := TGocciaArgumentsCollection.CreateWithCapacity(Len);
  for I := 0 to Len - 1 do
  begin
    // ES2026 §7.3.18 step 5a: Let next be ? Get(obj, indexName)
    Element := ArrayObj.GetProperty(IntToStr(I));
    if not Assigned(Element) then
      Element := TGocciaUndefinedLiteralValue.UndefinedValue;
    Result.Add(Element);
  end;
end;

end.
