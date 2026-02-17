unit Goccia.Evaluator.Comparison;

{$I Goccia.inc}

interface

uses
  Math, Goccia.Values.Primitives;

function IsStrictEqual(Left, Right: TGocciaValue): Boolean;
function IsNotStrictEqual(Left, Right: TGocciaValue): Boolean; inline;

function IsSameValue(Left, Right: TGocciaValue): Boolean;
function IsSameValueZero(Left, Right: TGocciaValue): Boolean;

function IsDeepEqual(const Actual, Expected: TGocciaValue): Boolean;

function GreaterThan(Left, Right: TGocciaValue): Boolean;
function GreaterThanOrEqual(Left, Right: TGocciaValue): Boolean; inline;
function LessThan(Left, Right: TGocciaValue): Boolean;
function LessThanOrEqual(Left, Right: TGocciaValue): Boolean; inline;

implementation

uses Goccia.Values.ArrayValue, Goccia.Values.ObjectValue, Goccia.Values.ClassHelper,
     Goccia.Values.SetValue, Goccia.Values.MapValue;

function IsNegativeZero(Value: TGocciaNumberLiteralValue): Boolean; inline;
begin
  Result := Value.IsNegativeZero;
end;

function IsStrictEqual(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedLiteralValue) and (Right is TGocciaUndefinedLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  if (Left is TGocciaNullLiteralValue) and (Right is TGocciaNullLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  if (Left is TGocciaBooleanLiteralValue) and (Right is TGocciaBooleanLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue(Left).Value = TGocciaBooleanLiteralValue(Right).Value;
    Exit;
  end;

  if (Left is TGocciaNumberLiteralValue) and (Right is TGocciaNumberLiteralValue) then
  begin
    if TGocciaNumberLiteralValue(Left).IsNaN or TGocciaNumberLiteralValue(Right).IsNaN then
    begin
      Result := False;
      Exit;
    end;

    if (TGocciaNumberLiteralValue(Left).IsInfinity or TGocciaNumberLiteralValue(Left).IsNegativeInfinity) and
       (TGocciaNumberLiteralValue(Right).IsInfinity or TGocciaNumberLiteralValue(Right).IsNegativeInfinity) then
    begin
      // Both are infinity: check if same sign
      Result := (TGocciaNumberLiteralValue(Left).IsInfinity and TGocciaNumberLiteralValue(Right).IsInfinity) or
                (TGocciaNumberLiteralValue(Left).IsNegativeInfinity and TGocciaNumberLiteralValue(Right).IsNegativeInfinity);
      Exit;
    end;

    if (TGocciaNumberLiteralValue(Left).IsInfinity or TGocciaNumberLiteralValue(Left).IsNegativeInfinity) or
       (TGocciaNumberLiteralValue(Right).IsInfinity or TGocciaNumberLiteralValue(Right).IsNegativeInfinity) then
    begin
      Result := False;
      Exit;
    end;

    Result := TGocciaNumberLiteralValue(Left).Value = TGocciaNumberLiteralValue(Right).Value;
    Exit;
  end;

  if (Left is TGocciaStringLiteralValue) and (Right is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(Left).Value = TGocciaStringLiteralValue(Right).Value;
    Exit;
  end;

  Result := Left = Right; // Reference equality for objects
end;

function IsNotStrictEqual(Left, Right: TGocciaValue): Boolean;
begin
  Result := not IsStrictEqual(Left, Right);
end;

function IsSameValue(Left, Right: TGocciaValue): Boolean;
begin
  // Handle undefined values
  if (Left is TGocciaUndefinedLiteralValue) and (Right is TGocciaUndefinedLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  // Handle null values
  if (Left is TGocciaNullLiteralValue) and (Right is TGocciaNullLiteralValue) then
  begin
    Result := True;
    Exit;
  end;

  // Handle boolean values
  if (Left is TGocciaBooleanLiteralValue) and (Right is TGocciaBooleanLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue(Left).Value = TGocciaBooleanLiteralValue(Right).Value;
    Exit;
  end;

  // Handle number values
  if (Left is TGocciaNumberLiteralValue) and (Right is TGocciaNumberLiteralValue) then
  begin
    // Both NaN values are considered equal
    if TGocciaNumberLiteralValue(Left).IsNaN and TGocciaNumberLiteralValue(Right).IsNaN then
    begin
      Result := True;
      Exit;
    end;

    // If one is NaN and the other isn't, they're different
    if TGocciaNumberLiteralValue(Left).IsNaN or TGocciaNumberLiteralValue(Right).IsNaN then
    begin
      Result := False;
      Exit;
    end;

    // Handle +0 and -0 (they are different in Object.is)
    if (TGocciaNumberLiteralValue(Left).Value = 0) and (TGocciaNumberLiteralValue(Right).Value = 0) then
    begin
      // Use our safe signed zero detection
      Result := IsNegativeZero(TGocciaNumberLiteralValue(Left)) = IsNegativeZero(TGocciaNumberLiteralValue(Right));
      Exit;
    end;

    // Regular number comparison
    Result := TGocciaNumberLiteralValue(Left).Value = TGocciaNumberLiteralValue(Right).Value;
    Exit;
  end;

  // Handle string values
  if (Left is TGocciaStringLiteralValue) and (Right is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(Left).Value = TGocciaStringLiteralValue(Right).Value;
    Exit;
  end;

  // For all other types, use reference equality
  Result := Left = Right;
end;

function IsSameValueZero(Left, Right: TGocciaValue): Boolean;
var
  LeftNum, RightNum: Double;
begin
  // Use SameValue for most cases
  if IsSameValue(Left, Right) then
  begin
    Result := True;
    Exit;
  end;

  // Handle +0 and -0 specifically
  if (Left is TGocciaNumberLiteralValue) and (Right is TGocciaNumberLiteralValue) then
  begin
    LeftNum := TGocciaNumberLiteralValue(Left).Value;
    RightNum := TGocciaNumberLiteralValue(Right).Value;

    // Both are zero (either +0 or -0)
    if (LeftNum = 0) and (RightNum = 0) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function IsDeepEqual(const Actual, Expected: TGocciaValue): Boolean;
var
  ActualObj, ExpectedObj: TGocciaObjectValue;
  ActualArr, ExpectedArr: TGocciaArrayValue;
  ActualKeys, ExpectedKeys: TArray<string>;
  I: Integer;
  Key: string;
begin
  // Base case: strict equality (handles primitives and same object references)
  if IsStrictEqual(Actual, Expected) then
  begin
    Result := True;
    Exit;
  end;

  // Type mismatch
  if Actual.TypeName <> Expected.TypeName then
  begin
    Result := False;
    Exit;
  end;

  // Handle arrays
  if (Actual is TGocciaArrayValue) and (Expected is TGocciaArrayValue) then
  begin
    ActualArr := TGocciaArrayValue(Actual);
    ExpectedArr := TGocciaArrayValue(Expected);

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
  if (Actual is TGocciaSetValue) and (Expected is TGocciaSetValue) then
  begin
    if TGocciaSetValue(Actual).Items.Count <> TGocciaSetValue(Expected).Items.Count then
    begin
      Result := False;
      Exit;
    end;
    for I := 0 to TGocciaSetValue(Actual).Items.Count - 1 do
    begin
      if not IsDeepEqual(TGocciaSetValue(Actual).Items[I], TGocciaSetValue(Expected).Items[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    Exit;
  end;

  // Handle Maps
  if (Actual is TGocciaMapValue) and (Expected is TGocciaMapValue) then
  begin
    if TGocciaMapValue(Actual).Entries.Count <> TGocciaMapValue(Expected).Entries.Count then
    begin
      Result := False;
      Exit;
    end;
    for I := 0 to TGocciaMapValue(Actual).Entries.Count - 1 do
    begin
      if not IsDeepEqual(TGocciaMapValue(Actual).Entries[I].Key, TGocciaMapValue(Expected).Entries[I].Key) then
      begin
        Result := False;
        Exit;
      end;
      if not IsDeepEqual(TGocciaMapValue(Actual).Entries[I].Value, TGocciaMapValue(Expected).Entries[I].Value) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    Exit;
  end;

  // Handle objects (but not arrays/sets/maps which inherit from TGocciaObjectValue)
  if (Actual is TGocciaObjectValue) and (Expected is TGocciaObjectValue) and
     not (Actual is TGocciaArrayValue) and not (Expected is TGocciaArrayValue) and
     not (Actual is TGocciaSetValue) and not (Expected is TGocciaSetValue) and
     not (Actual is TGocciaMapValue) and not (Expected is TGocciaMapValue) then
  begin
    ActualObj := TGocciaObjectValue(Actual);
    ExpectedObj := TGocciaObjectValue(Expected);

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

function CompareNumbers(LeftNum, RightNum: TGocciaNumberLiteralValue; IsGreater: Boolean): Boolean;
begin
  // Handle NaN - always false
  if LeftNum.IsNaN or RightNum.IsNaN then
  begin
    Result := False;
    Exit;
  end;

  if IsGreater then
  begin
    if LeftNum.IsInfinity then
      Result := not RightNum.IsInfinity
    else if LeftNum.IsNegativeInfinity then
      Result := False
    else if RightNum.IsInfinity then
      Result := False
    else if RightNum.IsNegativeInfinity then
      Result := True
    else
      Result := LeftNum.Value > RightNum.Value;
  end
  else
  begin
    if LeftNum.IsInfinity then
      Result := False
    else if LeftNum.IsNegativeInfinity then
      Result := not RightNum.IsNegativeInfinity
    else if RightNum.IsInfinity then
      Result := True
    else if RightNum.IsNegativeInfinity then
      Result := False
    else
      Result := LeftNum.Value < RightNum.Value;
  end;
end;

function GreaterThan(Left, Right: TGocciaValue): Boolean;
begin
  // undefined compared to anything is always false
  if (Left is TGocciaUndefinedLiteralValue) or (Right is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // null compared to null is false; null compared to other non-numbers handled via coercion
  if (Left is TGocciaNullLiteralValue) and (Right is TGocciaNullLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // If both are strings, compare lexicographically
  if (Left is TGocciaStringLiteralValue) and (Right is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(Left).Value > TGocciaStringLiteralValue(Right).Value;
    Exit;
  end;

  // Otherwise, coerce both to numbers and compare (ECMAScript Abstract Relational Comparison)
  Result := CompareNumbers(Left.ToNumberLiteral, Right.ToNumberLiteral, True);
end;

function GreaterThanOrEqual(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedLiteralValue) or (Right is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // NaN check after coercion
  if Left.ToNumberLiteral.IsNaN or Right.ToNumberLiteral.IsNaN then
  begin
    Result := False;
    Exit;
  end;

  Result := not LessThan(Left, Right);
end;

function LessThan(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedLiteralValue) or (Right is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaNullLiteralValue) and (Right is TGocciaNullLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // If both are strings, compare lexicographically
  if (Left is TGocciaStringLiteralValue) and (Right is TGocciaStringLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue(Left).Value < TGocciaStringLiteralValue(Right).Value;
    Exit;
  end;

  // Otherwise, coerce both to numbers and compare (ECMAScript Abstract Relational Comparison)
  Result := CompareNumbers(Left.ToNumberLiteral, Right.ToNumberLiteral, False);
end;

function LessThanOrEqual(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedLiteralValue) or (Right is TGocciaUndefinedLiteralValue) then
  begin
    Result := False;
    Exit;
  end;

  // NaN check after coercion
  if Left.ToNumberLiteral.IsNaN or Right.ToNumberLiteral.IsNaN then
  begin
    Result := False;
    Exit;
  end;

  Result := not GreaterThan(Left, Right);
end;

end.
