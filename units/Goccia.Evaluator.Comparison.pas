unit Goccia.Evaluator.Comparison;

{$I Goccia.inc}

interface

uses
  Math,

  Goccia.Values.Primitives;

function IsStrictEqual(const ALeft, ARight: TGocciaValue): Boolean;
function IsNotStrictEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;

function IsSameValue(const ALeft, ARight: TGocciaValue): Boolean;
function IsSameValueZero(const ALeft, ARight: TGocciaValue): Boolean;

function IsDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;

function GreaterThan(const ALeft, ARight: TGocciaValue): Boolean;
function GreaterThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
function LessThan(const ALeft, ARight: TGocciaValue): Boolean;
function LessThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;

implementation

uses
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue;

function IsNegativeZero(const AValue: TGocciaNumberLiteralValue): Boolean; inline;
begin
  Result := AValue.IsNegativeZero;
end;

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
      Result := IsNegativeZero(TGocciaNumberLiteralValue(ALeft)) = IsNegativeZero(TGocciaNumberLiteralValue(ARight));
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

function IsDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
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

  // Type mismatch
  if AActual.TypeName <> AExpected.TypeName then
  begin
    Result := False;
    Exit;
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
      // Handle nil elements (holes in arrays)
      if (ActualArr.Elements[I] = nil) and (ExpectedArr.Elements[I] = nil) then
        Continue; // Both are holes, they're equal

      if (ActualArr.Elements[I] = nil) or (ExpectedArr.Elements[I] = nil) then
      begin
        Result := False; // One is hole, other isn't
        Exit;
      end;

      if not IsDeepEqual(ActualArr.Elements[I], ExpectedArr.Elements[I]) then
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
    for I := 0 to TGocciaSetValue(AActual).Items.Count - 1 do
    begin
      if not IsDeepEqual(TGocciaSetValue(AActual).Items[I], TGocciaSetValue(AExpected).Items[I]) then
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
    for I := 0 to TGocciaMapValue(AActual).Entries.Count - 1 do
    begin
      if not IsDeepEqual(TGocciaMapValue(AActual).Entries[I].Key, TGocciaMapValue(AExpected).Entries[I].Key) then
      begin
        Result := False;
        Exit;
      end;
      if not IsDeepEqual(TGocciaMapValue(AActual).Entries[I].Value, TGocciaMapValue(AExpected).Entries[I].Value) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    Exit;
  end;

  // Handle objects (but not arrays/sets/maps which inherit from TGocciaObjectValue)
  if (AActual is TGocciaObjectValue) and (AExpected is TGocciaObjectValue) and
     not (AActual is TGocciaArrayValue) and not (AExpected is TGocciaArrayValue) and
     not (AActual is TGocciaSetValue) and not (AExpected is TGocciaSetValue) and
     not (AActual is TGocciaMapValue) and not (AExpected is TGocciaMapValue) then
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
      if not IsDeepEqual(ActualObj.GetProperty(Key), ExpectedObj.GetProperty(Key)) then
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

function GreaterThan(const ALeft, ARight: TGocciaValue): Boolean;
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

  // Otherwise, coerce both to numbers and compare (ECMAScript Abstract Relational Comparison)
  Result := CompareNumbers(ALeft.ToNumberLiteral, ARight.ToNumberLiteral, True);
end;

function GreaterThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // NaN check after coercion
  if ALeft.ToNumberLiteral.IsNaN or ARight.ToNumberLiteral.IsNaN then
  begin
    Result := False;
    Exit;
  end;

  Result := not LessThan(ALeft, ARight);
end;

function LessThan(const ALeft, ARight: TGocciaValue): Boolean;
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

  // Otherwise, coerce both to numbers and compare (ECMAScript Abstract Relational Comparison)
  Result := CompareNumbers(ALeft.ToNumberLiteral, ARight.ToNumberLiteral, False);
end;

function LessThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or (ARight is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // NaN check after coercion
  if ALeft.ToNumberLiteral.IsNaN or ARight.ToNumberLiteral.IsNaN then
  begin
    Result := False;
    Exit;
  end;

  Result := not GreaterThan(ALeft, ARight);
end;

end.
