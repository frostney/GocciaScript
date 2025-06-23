unit Goccia.Evaluator.Comparison;

{$I Goccia.inc}

interface

uses
  Math, Goccia.Values.Base, Goccia.Values.UndefinedValue, Goccia.Values.NullValue,
  Goccia.Values.BooleanValue, Goccia.Values.NumberValue, Goccia.Values.StringValue;

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

uses Goccia.Values.ArrayValue, Goccia.Values.ObjectValue;

// Helper function to check if a GocciaNumberValue is negative zero
function IsNegativeZero(Value: TGocciaNumberValue): Boolean;
begin
  Result := Value.IsNegativeZero;
end;

function IsStrictEqual(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedValue) and (Right is TGocciaUndefinedValue) then
  begin
    Result := True;
    Exit;
  end;

  if (Left is TGocciaNullValue) and (Right is TGocciaNullValue) then
  begin
    Result := True;
    Exit;
  end;

  if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
  begin
    Result := TGocciaBooleanValue(Left).ToBoolean = TGocciaBooleanValue(Right).ToBoolean;
    Exit;
  end;

  if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    if IsNaN(TGocciaNumberValue(Left).ToNumber) or IsNaN(TGocciaNumberValue(Right).ToNumber) then
    begin
      Result := False;
      Exit;
    end;

    if IsInfinite(TGocciaNumberValue(Left).ToNumber) and IsInfinite(TGocciaNumberValue(Right).ToNumber) then
    begin
      Result := Sign(TGocciaNumberValue(Left).ToNumber) = Sign(TGocciaNumberValue(Right).ToNumber);
      Exit;
    end;

    if IsInfinite(TGocciaNumberValue(Left).ToNumber) or IsInfinite(TGocciaNumberValue(Right).ToNumber) then
    begin
      Result := False;
      Exit;
    end;

    Result := TGocciaNumberValue(Left).ToNumber = TGocciaNumberValue(Right).ToNumber;
    Exit;
  end;

  if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
  begin
    Result := TGocciaStringValue(Left).ToString = TGocciaStringValue(Right).ToString;
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
  if (Left is TGocciaUndefinedValue) and (Right is TGocciaUndefinedValue) then
  begin
    Result := True;
    Exit;
  end;

  // Handle null values
  if (Left is TGocciaNullValue) and (Right is TGocciaNullValue) then
  begin
    Result := True;
    Exit;
  end;

  // Handle boolean values
  if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
  begin
    Result := TGocciaBooleanValue(Left).ToBoolean = TGocciaBooleanValue(Right).ToBoolean;
    Exit;
  end;

  // Handle number values
  if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    // Both NaN values are considered equal
    if TGocciaNumberValue(Left).IsNaN and TGocciaNumberValue(Right).IsNaN then
    begin
      Result := True;
      Exit;
    end;

    // If one is NaN and the other isn't, they're different
    if TGocciaNumberValue(Left).IsNaN or TGocciaNumberValue(Right).IsNaN then
    begin
      Result := False;
      Exit;
    end;

    // Handle +0 and -0 (they are different in Object.is)
    if (TGocciaNumberValue(Left).Value = 0) and (TGocciaNumberValue(Right).Value = 0) then
    begin
      // Use our safe signed zero detection
      Result := IsNegativeZero(TGocciaNumberValue(Left)) = IsNegativeZero(TGocciaNumberValue(Right));
      Exit;
    end;

    // Regular number comparison
    Result := TGocciaNumberValue(Left).Value = TGocciaNumberValue(Right).Value;
    Exit;
  end;

  // Handle string values
  if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
  begin
    Result := TGocciaStringValue(Left).ToString = TGocciaStringValue(Right).ToString;
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
  if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    LeftNum := TGocciaNumberValue(Left).ToNumber;
    RightNum := TGocciaNumberValue(Right).ToNumber;

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
      if not IsDeepEqual(ActualArr.Elements[I], ExpectedArr.Elements[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  // Handle objects (but not arrays which inherit from TGocciaObjectValue)
  if (Actual is TGocciaObjectValue) and (Expected is TGocciaObjectValue) and
     not (Actual is TGocciaArrayValue) and not (Expected is TGocciaArrayValue) then
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

function GreaterThan(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedValue) or (Right is TGocciaUndefinedValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaNullValue) or (Right is TGocciaNullValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
  begin
    Result := TGocciaBooleanValue(Left).ToBoolean > TGocciaBooleanValue(Right).ToBoolean;
    Exit;
  end;

  if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    Result := TGocciaNumberValue(Left).ToNumber > TGocciaNumberValue(Right).ToNumber;
    Exit;
  end;

  if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
  begin
    Result := TGocciaStringValue(Left).ToString > TGocciaStringValue(Right).ToString;
    Exit;
  end;

  Result := False; // Default for non-comparable types
end;

function GreaterThanOrEqual(Left, Right: TGocciaValue): Boolean;
begin
  Result := GreaterThan(Left, Right) or IsStrictEqual(Left, Right);
end;

function LessThan(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedValue) or (Right is TGocciaUndefinedValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaNullValue) or (Right is TGocciaNullValue) then
  begin
    Result := False;
    Exit;
  end;

  if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
  begin
    Result := TGocciaBooleanValue(Left).ToBoolean < TGocciaBooleanValue(Right).ToBoolean;
    Exit;
  end;

  if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
  begin
    Result := TGocciaNumberValue(Left).ToNumber < TGocciaNumberValue(Right).ToNumber;
    Exit;
  end;

  if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
  begin
    Result := TGocciaStringValue(Left).ToString < TGocciaStringValue(Right).ToString;
    Exit;
  end;

  Result := False; // Default for non-comparable types
end;

function LessThanOrEqual(Left, Right: TGocciaValue): Boolean;
begin
  Result := LessThan(Left, Right) or IsStrictEqual(Left, Right);
end;

end.
