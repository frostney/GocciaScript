unit Goccia.Evaluator.Comparison;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function IsDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
function IsPartialDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;

implementation

uses
  Goccia.Arithmetic,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
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

end.
