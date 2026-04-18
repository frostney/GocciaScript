unit Goccia.Evaluator.Comparison;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function IsStrictEqual(const ALeft, ARight: TGocciaValue): Boolean;
function IsNotStrictEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;

function IsSameValue(const ALeft, ARight: TGocciaValue): Boolean;
function IsSameValueZero(const ALeft, ARight: TGocciaValue): Boolean;

function IsDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
function IsPartialDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;

function GreaterThan(const ALeft, ARight: TGocciaValue): Boolean;
function GreaterThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
function LessThan(const ALeft, ARight: TGocciaValue): Boolean;
function LessThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;

implementation

uses
  BigInteger,

  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.HoleValue,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue;

type
  TComparedValuePair = record
    Actual: TGocciaValue;
    Expected: TGocciaValue;
  end;

  TComparedValuePairArray = array of TComparedValuePair;

function HasComparedPair(const AComparedPairs: TComparedValuePairArray;
  const AActual, AExpected: TGocciaValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AComparedPairs) do
    if (AComparedPairs[I].Actual = AActual) and
       (AComparedPairs[I].Expected = AExpected) then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

procedure AddComparedPair(var AComparedPairs: TComparedValuePairArray;
  const AActual, AExpected: TGocciaValue);
var
  PairIndex: Integer;
begin
  PairIndex := Length(AComparedPairs);
  SetLength(AComparedPairs, PairIndex + 1);
  AComparedPairs[PairIndex].Actual := AActual;
  AComparedPairs[PairIndex].Expected := AExpected;
end;

procedure CopyComparedPairs(const ASource: TComparedValuePairArray;
  out ADestination: TComparedValuePairArray);
var
  I: Integer;
begin
  SetLength(ADestination, Length(ASource));
  for I := 0 to High(ASource) do
    ADestination[I] := ASource[I];
end;

function IsDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray): Boolean; forward;
function IsPartialDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray): Boolean; forward;

function IsStrictEqual(const ALeft, ARight: TGocciaValue): Boolean;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) and (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  if (ALeft is TGocciaBooleanLiteralValue) and (ARight is TGocciaBooleanLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue(ALeft).Value = TGocciaBooleanLiteralValue(ARight).Value;
    Exit;
  end;

  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    if TGocciaNumberLiteralValue(ALeft).IsNaN or TGocciaNumberLiteralValue(ARight).IsNaN then
    begin
      Result := False;
      Exit;
    end;

    if (TGocciaNumberLiteralValue(ALeft).IsInfinity or TGocciaNumberLiteralValue(ALeft).IsNegativeInfinity) and
       (TGocciaNumberLiteralValue(ARight).IsInfinity or TGocciaNumberLiteralValue(ARight).IsNegativeInfinity) then
    begin
      // Both are infinity: check if same sign
      Result := (TGocciaNumberLiteralValue(ALeft).IsInfinity and TGocciaNumberLiteralValue(ARight).IsInfinity) or
                (TGocciaNumberLiteralValue(ALeft).IsNegativeInfinity and TGocciaNumberLiteralValue(ARight).IsNegativeInfinity);
      Exit;
    end;

    if (TGocciaNumberLiteralValue(ALeft).IsInfinity or TGocciaNumberLiteralValue(ALeft).IsNegativeInfinity) or
       (TGocciaNumberLiteralValue(ARight).IsInfinity or TGocciaNumberLiteralValue(ARight).IsNegativeInfinity) then
    begin
      Result := False;
      Exit;
    end;

    Result := TGocciaNumberLiteralValue(ALeft).Value = TGocciaNumberLiteralValue(ARight).Value;
    Exit;
  end;

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(ALeft).Value = TGocciaStringLiteralValue(ARight).Value;
    Exit;
  end;

  // ES2026 §7.2.16 BigInt strict equality
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue(ALeft).Value.Equal(TGocciaBigIntValue(ARight).Value);
    Exit;
  end;

  Result := ALeft = ARight; // Reference equality for objects
end;

function IsNotStrictEqual(const ALeft, ARight: TGocciaValue): Boolean;
begin
  Result := not IsStrictEqual(ALeft, ARight);
end;

function IsSameValue(const ALeft, ARight: TGocciaValue): Boolean;
begin
  // Handle undefined values
  if (ALeft is TGocciaUndefinedLiteralValue) and (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  // Handle null values
  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  // Handle boolean values
  if (ALeft is TGocciaBooleanLiteralValue) and (ARight is TGocciaBooleanLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue(ALeft).Value = TGocciaBooleanLiteralValue(ARight).Value;
    Exit;
  end;

  // Handle number values
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    // Both NaN values are considered equal
    if TGocciaNumberLiteralValue(ALeft).IsNaN and TGocciaNumberLiteralValue(ARight).IsNaN then
    begin
      Result := True;
      Exit;
    end;

    // If one is NaN and the other isn't, they're different
    if TGocciaNumberLiteralValue(ALeft).IsNaN or TGocciaNumberLiteralValue(ARight).IsNaN then
    begin
      Result := False;
      Exit;
    end;

    // Handle +0 and -0 (they are different in Object.is)
    if (TGocciaNumberLiteralValue(ALeft).Value = 0) and (TGocciaNumberLiteralValue(ARight).Value = 0) then
    begin
      // Use our safe signed zero detection
      Result := TGocciaNumberLiteralValue(ALeft).IsNegativeZero = TGocciaNumberLiteralValue(ARight).IsNegativeZero;
      Exit;
    end;

    // Regular number comparison
    Result := TGocciaNumberLiteralValue(ALeft).Value = TGocciaNumberLiteralValue(ARight).Value;
    Exit;
  end;

  // Handle string values
  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(ALeft).Value = TGocciaStringLiteralValue(ARight).Value;
    Exit;
  end;

  // Handle BigInt values
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue(ALeft).Value.Equal(TGocciaBigIntValue(ARight).Value);
    Exit;
  end;

  // For all other types, use reference equality
  Result := ALeft = ARight;
end;

function IsSameValueZero(const ALeft, ARight: TGocciaValue): Boolean;
var
  LeftNum, RightNum: Double;
begin
  // Use SameValue for most cases
  if IsSameValue(ALeft, ARight) then
  begin
    Result := True;
    Exit;
  end;

  // Handle +0 and -0 specifically
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    LeftNum := TGocciaNumberLiteralValue(ALeft).Value;
    RightNum := TGocciaNumberLiteralValue(ARight).Value;

    // Both are zero (either +0 or -0)
    if (LeftNum = 0) and (RightNum = 0) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function IsDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray): Boolean;
var
  ActualObj, ExpectedObj: TGocciaObjectValue;
  ActualArr, ExpectedArr: TGocciaArrayValue;
  ActualKeys, ExpectedKeys: TArray<string>;
  I: Integer;
  Key: string;
begin
  // Base case: strict equality (handles primitives and same object references)
  if IsStrictEqual(AActual, AExpected) then
  begin
    Result := True;
    Exit;
  end;

  // Type mismatch — allow TGocciaObjectValue/TGocciaInstanceValue interop
  if AActual.TypeName <> AExpected.TypeName then
  begin
    if AActual.IsCallable or AExpected.IsCallable then
    begin
      Result := False;
      Exit;
    end;
    if not ((AActual is TGocciaObjectValue) and (AExpected is TGocciaObjectValue)) then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Handle arrays
  if (AActual is TGocciaArrayValue) and (AExpected is TGocciaArrayValue) then
  begin
    ActualArr := TGocciaArrayValue(AActual);
    ExpectedArr := TGocciaArrayValue(AExpected);

    // Check lengths
    if ActualArr.Elements.Count <> ExpectedArr.Elements.Count then
    begin
      Result := False;
      Exit;
    end;

    // Recursively compare elements
    for I := 0 to ActualArr.Elements.Count - 1 do
    begin
      // Handle array holes via the internal hole sentinel.
      if (ActualArr.Elements[I] = TGocciaHoleValue.HoleValue) and
         (ExpectedArr.Elements[I] = TGocciaHoleValue.HoleValue) then
        Continue; // Both are holes, they're equal

      if (ActualArr.Elements[I] = TGocciaHoleValue.HoleValue) or
         (ExpectedArr.Elements[I] = TGocciaHoleValue.HoleValue) then
      begin
        Result := False; // One is hole, other isn't
        Exit;
      end;

      if not IsDeepEqualInternal(ActualArr.Elements[I], ExpectedArr.Elements[I],
        AComparedPairs) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  // Handle Sets
  if (AActual is TGocciaSetValue) and (AExpected is TGocciaSetValue) then
  begin
    if TGocciaSetValue(AActual).Items.Count <> TGocciaSetValue(AExpected).Items.Count then
    begin
      Result := False;
      Exit;
    end;
    if HasComparedPair(AComparedPairs, AActual, AExpected) then
    begin
      Result := True;
      Exit;
    end;
    AddComparedPair(AComparedPairs, AActual, AExpected);
    for I := 0 to TGocciaSetValue(AActual).Items.Count - 1 do
    begin
      if not IsDeepEqualInternal(TGocciaSetValue(AActual).Items[I],
        TGocciaSetValue(AExpected).Items[I], AComparedPairs) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    Exit;
  end;

  // Handle Maps
  if (AActual is TGocciaMapValue) and (AExpected is TGocciaMapValue) then
  begin
    if TGocciaMapValue(AActual).Entries.Count <> TGocciaMapValue(AExpected).Entries.Count then
    begin
      Result := False;
      Exit;
    end;
    if HasComparedPair(AComparedPairs, AActual, AExpected) then
    begin
      Result := True;
      Exit;
    end;
    AddComparedPair(AComparedPairs, AActual, AExpected);
    for I := 0 to TGocciaMapValue(AActual).Entries.Count - 1 do
    begin
      if not IsDeepEqualInternal(TGocciaMapValue(AActual).Entries[I].Key,
        TGocciaMapValue(AExpected).Entries[I].Key, AComparedPairs) then
      begin
        Result := False;
        Exit;
      end;
      if not IsDeepEqualInternal(TGocciaMapValue(AActual).Entries[I].Value,
        TGocciaMapValue(AExpected).Entries[I].Value, AComparedPairs) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    Exit;
  end;

  // Handle objects (arrays/sets/maps already handled by earlier branches)
  if (AActual is TGocciaObjectValue) and (AExpected is TGocciaObjectValue) then
  begin
    ActualObj := TGocciaObjectValue(AActual);
    ExpectedObj := TGocciaObjectValue(AExpected);

    // Get enumerable property names from both objects
    ActualKeys := ActualObj.GetEnumerablePropertyNames;
    ExpectedKeys := ExpectedObj.GetEnumerablePropertyNames;

    // Check if they have the same number of properties
    if Length(ActualKeys) <> Length(ExpectedKeys) then
    begin
      Result := False;
      Exit;
    end;
    if HasComparedPair(AComparedPairs, AActual, AExpected) then
    begin
      Result := True;
      Exit;
    end;
    AddComparedPair(AComparedPairs, AActual, AExpected);

    // Check if all keys exist in both objects and values are deeply equal
    for I := 0 to High(ActualKeys) do
    begin
      Key := ActualKeys[I];

      // Check if expected object has this key
      if not ExpectedObj.HasOwnProperty(Key) then
      begin
        Result := False;
        Exit;
      end;

      // Recursively compare property values
      if not IsDeepEqualInternal(ActualObj.GetProperty(Key),
        ExpectedObj.GetProperty(Key), AComparedPairs) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  // For other types (functions, etc.), fall back to strict equality
  Result := False;
end;

function IsDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
var
  ComparedPairs: TComparedValuePairArray;
begin
  Result := IsDeepEqualInternal(AActual, AExpected, ComparedPairs);
end;

function IsPartialDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray): Boolean;
var
  ActualObj, ExpectedObj: TGocciaObjectValue;
  DeepComparedPairs: TComparedValuePairArray;
  ExpectedKeys: TArray<string>;
  I: Integer;
  Key: string;
begin
  CopyComparedPairs(AComparedPairs, DeepComparedPairs);
  if IsDeepEqualInternal(AActual, AExpected, DeepComparedPairs) then
  begin
    Result := True;
    Exit;
  end;

  if (AActual is TGocciaObjectValue) and (AExpected is TGocciaObjectValue) then
  begin
    ActualObj := TGocciaObjectValue(AActual);
    ExpectedObj := TGocciaObjectValue(AExpected);
    ExpectedKeys := ExpectedObj.GetEnumerablePropertyNames;
    if HasComparedPair(AComparedPairs, AActual, AExpected) then
    begin
      Result := True;
      Exit;
    end;
    AddComparedPair(AComparedPairs, AActual, AExpected);

    for I := 0 to High(ExpectedKeys) do
    begin
      Key := ExpectedKeys[I];
      if not ActualObj.HasOwnProperty(Key) then
      begin
        Result := False;
        Exit;
      end;

      if not IsPartialDeepEqualInternal(ActualObj.GetProperty(Key),
        ExpectedObj.GetProperty(Key), AComparedPairs) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  Result := False;
end;

function IsPartialDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
var
  ComparedPairs: TComparedValuePairArray;
begin
  Result := IsPartialDeepEqualInternal(AActual, AExpected, ComparedPairs);
end;

function CompareNumbers(const ALeftNum, ARightNum: TGocciaNumberLiteralValue; const AIsGreater: Boolean): Boolean;
begin
  // Handle NaN - always false
  if ALeftNum.IsNaN or ARightNum.IsNaN then
  begin
    Result := False;
    Exit;
  end;

  if AIsGreater then
  begin
    if ALeftNum.IsInfinity then
      Result := not ARightNum.IsInfinity
    else if ALeftNum.IsNegativeInfinity then
      Result := False
    else if ARightNum.IsInfinity then
      Result := False
    else if ARightNum.IsNegativeInfinity then
      Result := True
    else
      Result := ALeftNum.Value > ARightNum.Value;
  end
  else
  begin
    if ALeftNum.IsInfinity then
      Result := False
    else if ALeftNum.IsNegativeInfinity then
      Result := not ARightNum.IsNegativeInfinity
    else if ARightNum.IsInfinity then
      Result := True
    else if ARightNum.IsNegativeInfinity then
      Result := False
    else
      Result := ALeftNum.Value < ARightNum.Value;
  end;
end;

// ES2026 §7.2.14 — cross-type BigInt/Number comparison
function CompareBigIntAndNumber(const ABigInt: TGocciaBigIntValue;
  const ANumber: TGocciaNumberLiteralValue; const ABigIntIsLeft: Boolean): Integer;
var
  D: Double;
begin
  // NaN is always unordered
  if ANumber.IsNaN then
    Exit(2); // unordered
  if ANumber.IsInfinity then
  begin
    Result := -1; // BigInt < +Infinity
    Exit;
  end;
  if ANumber.IsNegativeInfinity then
  begin
    Result := 1; // BigInt > -Infinity
    Exit;
  end;
  // Compare via Double (may lose precision for very large BigInts, but matches spec behavior)
  D := ABigInt.Value.ToDouble;
  if D > ANumber.Value then
    Result := 1
  else if D < ANumber.Value then
    Result := -1
  else
    Result := 0;
end;

function GreaterThan(const ALeft, ARight: TGocciaValue): Boolean;
var
  Cmp: Integer;
begin
  // undefined compared to anything is always false
  if (ALeft is TGocciaUndefinedLiteralValue) or (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // null compared to null is false; null compared to other non-numbers handled via coercion
  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // If both are strings, compare lexicographically
  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(ALeft).Value > TGocciaStringLiteralValue(ARight).Value;
    Exit;
  end;

  // ES2026 §7.2.14 — BigInt vs BigInt
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue(ALeft).Value.Compare(TGocciaBigIntValue(ARight).Value) > 0;
    Exit;
  end;

  // ES2026 §7.2.14 — BigInt vs Number (cross-type)
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    Cmp := CompareBigIntAndNumber(TGocciaBigIntValue(ALeft), TGocciaNumberLiteralValue(ARight), True);
    Result := Cmp = 1;
    Exit;
  end;
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaBigIntValue) then
  begin
    Cmp := CompareBigIntAndNumber(TGocciaBigIntValue(ARight), TGocciaNumberLiteralValue(ALeft), False);
    Result := Cmp = -1;
    Exit;
  end;

  // Otherwise, coerce both to numbers and compare (ECMAScript Abstract Relational Comparison)
  Result := CompareNumbers(ALeft.ToNumberLiteral, ARight.ToNumberLiteral, True);
end;

// ES2026 §13.10.1 Runtime Semantics: Evaluation — RelationalExpression : >=
function GreaterThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(ALeft).Value >= TGocciaStringLiteralValue(ARight).Value;
    Exit;
  end;

  // BigInt is never NaN, skip NaN check for BigInt operands
  if not ((ALeft is TGocciaBigIntValue) or (ARight is TGocciaBigIntValue)) then
  begin
    if ALeft.ToNumberLiteral.IsNaN or ARight.ToNumberLiteral.IsNaN then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := not LessThan(ALeft, ARight);
end;

function LessThan(const ALeft, ARight: TGocciaValue): Boolean;
var
  Cmp: Integer;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // If both are strings, compare lexicographically
  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(ALeft).Value < TGocciaStringLiteralValue(ARight).Value;
    Exit;
  end;

  // ES2026 §7.2.14 — BigInt comparisons
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
  begin
    Result := TGocciaBigIntValue(ALeft).Value.Compare(TGocciaBigIntValue(ARight).Value) < 0;
    Exit;
  end;
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    Cmp := CompareBigIntAndNumber(TGocciaBigIntValue(ALeft), TGocciaNumberLiteralValue(ARight), True);
    Result := Cmp = -1;
    Exit;
  end;
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaBigIntValue) then
  begin
    Cmp := CompareBigIntAndNumber(TGocciaBigIntValue(ARight), TGocciaNumberLiteralValue(ALeft), False);
    Result := Cmp = 1;
    Exit;
  end;

  // Otherwise, coerce both to numbers and compare (ECMAScript Abstract Relational Comparison)
  Result := CompareNumbers(ALeft.ToNumberLiteral, ARight.ToNumberLiteral, False);
end;

// ES2026 §13.10.1 Runtime Semantics: Evaluation — RelationalExpression : <=
function LessThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(ALeft).Value <= TGocciaStringLiteralValue(ARight).Value;
    Exit;
  end;

  // BigInt is never NaN, skip NaN check for BigInt operands
  if not ((ALeft is TGocciaBigIntValue) or (ARight is TGocciaBigIntValue)) then
  begin
    if ALeft.ToNumberLiteral.IsNaN or ARight.ToNumberLiteral.IsNaN then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := not GreaterThan(ALeft, ARight);
end;

end.
