unit Goccia.Evaluator.Comparison;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function IsDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
function IsStrictDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
function IsPartialDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
function IsSnapshotPartialDeepEqual(const AActual,
  AExpected: TGocciaValue): Boolean;

implementation

uses
  Goccia.Arithmetic,
  Goccia.Values.ArrayValue,
  Goccia.Values.AsymmetricMatcher,
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
  var AComparedPairs: TComparedValuePairArray;
  const AStrict: Boolean): Boolean; forward;
function IsPartialDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray;
  const AIncludeInherited: Boolean): Boolean; forward;

function IsDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray;
  const AStrict: Boolean): Boolean;
var
  ActualObj, ExpectedObj: TGocciaObjectValue;
  ActualArr, ExpectedArr: TGocciaArrayValue;
  ActualKeys, ExpectedKeys: TArray<string>;
  I: Integer;
  Key: string;
  CursorA, CursorB: Integer;
  LeftKey, LeftValue, RightKey, RightValue: TGocciaValue;
begin
  // Vitest/Jest asymmetric matchers participate in every equality-based
  // assertion. When both operands are matchers, compare their stored matcher
  // state instead of invoking either matcher against the other.
  if (AActual is TGocciaAsymmetricMatcherValue) and
     (AExpected is TGocciaAsymmetricMatcherValue) then
  begin
    if AStrict then
      Result := TGocciaAsymmetricMatcherValue(AActual).IsEquivalentTo(
        TGocciaAsymmetricMatcherValue(AExpected), IsStrictDeepEqual)
    else
      Result := TGocciaAsymmetricMatcherValue(AActual).IsEquivalentTo(
        TGocciaAsymmetricMatcherValue(AExpected), IsDeepEqual);
    Exit;
  end;

  if (AActual is TGocciaAsymmetricMatcherValue) xor
     (AExpected is TGocciaAsymmetricMatcherValue) then
  begin
    if AActual is TGocciaAsymmetricMatcherValue then
    begin
      if AStrict then
        Result := TGocciaAsymmetricMatcherValue(AActual).AsymmetricMatch(
          AExpected, IsStrictDeepEqual)
      else
        Result := TGocciaAsymmetricMatcherValue(AActual).AsymmetricMatch(
          AExpected, IsDeepEqual);
    end
    else if AStrict then
      Result := TGocciaAsymmetricMatcherValue(AExpected).AsymmetricMatch(
        AActual, IsStrictDeepEqual)
    else
      Result := TGocciaAsymmetricMatcherValue(AExpected).AsymmetricMatch(
        AActual, IsDeepEqual);
    Exit;
  end;

  // Vitest's equality testers use Object.is for primitive leaves. This keeps
  // NaN equal to NaN and distinguishes +0 from -0 while still accepting the
  // same object reference.
  if IsSameValue(AActual, AExpected) then
  begin
    Result := True;
    Exit;
  end;

  // Distinct callable values are never deeply equal. Treating functions as
  // empty objects would also make different schema validators compare equal.
  if AActual.IsCallable or AExpected.IsCallable then
  begin
    Result := False;
    Exit;
  end;

  // Type mismatch — allow TGocciaObjectValue/TGocciaInstanceValue interop
  if AActual.TypeName <> AExpected.TypeName then
  begin
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

      { Vitest's loose equality treats an array hole like undefined. Its
        strict-equality matcher is the surface that preserves sparseness. }
      if (ActualArr.Elements[I] = TGocciaHoleValue.HoleValue) and
         AStrict then
      begin
        Result := False;
        Exit;
      end;
      if (ExpectedArr.Elements[I] = TGocciaHoleValue.HoleValue) and
         AStrict then
      begin
        Result := False;
        Exit;
      end;
      if (ActualArr.Elements[I] = TGocciaHoleValue.HoleValue) and
         not IsDeepEqualInternal(TGocciaUndefinedLiteralValue.UndefinedValue,
           ExpectedArr.Elements[I], AComparedPairs, AStrict) then
      begin
        Result := False;
        Exit;
      end;
      if (ExpectedArr.Elements[I] = TGocciaHoleValue.HoleValue) and
         not IsDeepEqualInternal(ActualArr.Elements[I],
           TGocciaUndefinedLiteralValue.UndefinedValue,
           AComparedPairs, AStrict) then
      begin
        Result := False;
        Exit;
      end;
      if (ActualArr.Elements[I] <> TGocciaHoleValue.HoleValue) and
         (ExpectedArr.Elements[I] <> TGocciaHoleValue.HoleValue) and
         not IsDeepEqualInternal(ActualArr.Elements[I], ExpectedArr.Elements[I],
           AComparedPairs, AStrict) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  // Handle Sets — compared by insertion order, element for element.
  if (AActual is TGocciaSetValue) and (AExpected is TGocciaSetValue) then
  begin
    if TGocciaSetValue(AActual).Count <> TGocciaSetValue(AExpected).Count then
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
    CursorA := 0;
    CursorB := 0;
    // Recursive comparison can run user getters that mutate either set; retain
    // both so cursors stay valid, and confirm the expected side advances before
    // comparing (avoids comparing stale out values).
    TGocciaSetValue(AActual).RetainIterator;
    TGocciaSetValue(AExpected).RetainIterator;
    try
      while TGocciaSetValue(AActual).NextItem(CursorA, LeftValue) do
      begin
        if not TGocciaSetValue(AExpected).NextItem(CursorB, RightValue) then
        begin
          Result := False;
          Exit;
        end;
        if not IsDeepEqualInternal(LeftValue, RightValue, AComparedPairs,
          AStrict) then
        begin
          Result := False;
          Exit;
        end;
      end;
    finally
      TGocciaSetValue(AExpected).ReleaseIterator;
      TGocciaSetValue(AActual).ReleaseIterator;
    end;
    Result := True;
    Exit;
  end;

  // Handle Maps — compared by insertion order, entry for entry.
  if (AActual is TGocciaMapValue) and (AExpected is TGocciaMapValue) then
  begin
    if TGocciaMapValue(AActual).Count <> TGocciaMapValue(AExpected).Count then
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
    CursorA := 0;
    CursorB := 0;
    // Recursive comparison can run user getters that mutate either map; retain
    // both so cursors stay valid, and confirm the expected side advances before
    // comparing (avoids comparing stale out values).
    TGocciaMapValue(AActual).RetainIterator;
    TGocciaMapValue(AExpected).RetainIterator;
    try
      while TGocciaMapValue(AActual).NextEntry(CursorA, LeftKey, LeftValue) do
      begin
        if not TGocciaMapValue(AExpected).NextEntry(CursorB, RightKey, RightValue) then
        begin
          Result := False;
          Exit;
        end;
        if not IsDeepEqualInternal(LeftKey, RightKey, AComparedPairs,
          AStrict) then
        begin
          Result := False;
          Exit;
        end;
        if not IsDeepEqualInternal(LeftValue, RightValue, AComparedPairs,
          AStrict) then
        begin
          Result := False;
          Exit;
        end;
      end;
    finally
      TGocciaMapValue(AExpected).ReleaseIterator;
      TGocciaMapValue(AActual).ReleaseIterator;
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
        ExpectedObj.GetProperty(Key), AComparedPairs, AStrict) then
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
  Result := IsDeepEqualInternal(AActual, AExpected, ComparedPairs, False);
end;

function IsStrictDeepEqual(const AActual, AExpected: TGocciaValue): Boolean;
var
  ComparedPairs: TComparedValuePairArray;
begin
  Result := IsDeepEqualInternal(AActual, AExpected, ComparedPairs, True);
end;

function IsPartialDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray;
  const AIncludeInherited: Boolean): Boolean;
var
  ActualObj, ExpectedObj: TGocciaObjectValue;
  DeepComparedPairs: TComparedValuePairArray;
  ExpectedKeys: TArray<string>;
  I: Integer;
  Key: string;
begin
  CopyComparedPairs(AComparedPairs, DeepComparedPairs);
  if IsDeepEqualInternal(AActual, AExpected, DeepComparedPairs, False) then
  begin
    Result := True;
    Exit;
  end;

  { Vitest's object-subset semantics do not make arrays partial: an array
    property shape must describe every element. }
  if (AActual is TGocciaArrayValue) or (AExpected is TGocciaArrayValue) then
    Exit(False);

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
      if (AIncludeInherited and not ActualObj.HasProperty(Key)) or
         (not AIncludeInherited and not ActualObj.HasOwnProperty(Key)) then
      begin
        Result := False;
        Exit;
      end;

      if not IsPartialDeepEqualInternal(ActualObj.GetProperty(Key),
        ExpectedObj.GetProperty(Key), AComparedPairs,
        AIncludeInherited) then
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
  Result := IsPartialDeepEqualInternal(AActual, AExpected, ComparedPairs,
    False);
end;

function IsSnapshotPartialDeepEqual(const AActual,
  AExpected: TGocciaValue): Boolean;
var
  ComparedPairs: TComparedValuePairArray;
begin
  Result := IsPartialDeepEqualInternal(AActual, AExpected, ComparedPairs,
    True);
end;

end.
