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
  Goccia.Values.ClassValue,
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

{ A missing property, an explicit undefined and an array hole are the three
  shapes loose equality collapses together: Jest ignores object keys whose
  value is undefined, and undefined array items past the shorter length. }
function IsUndefinedLike(const AValue: TGocciaValue): Boolean;
begin
  Result := (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) or
    (AValue = TGocciaHoleValue.HoleValue);
end;

{ Strict equality additionally requires the two objects to have the same type.
  Class instances only match instances of the same class, and never a plain
  object; every non-class object — including a null-prototype object or one
  built with Object.create(proto) — counts as plain. }
function HasSameObjectType(const AActual, AExpected: TGocciaValue): Boolean;
var
  ActualClass, ExpectedClass: TGocciaClassValue;
begin
  if AActual is TGocciaInstanceValue then
    ActualClass := TGocciaInstanceValue(AActual).ClassValue
  else
    ActualClass := nil;

  if AExpected is TGocciaInstanceValue then
    ExpectedClass := TGocciaInstanceValue(AExpected).ClassValue
  else
    ExpectedClass := nil;

  Result := ActualClass = ExpectedClass;
end;

{ An asymmetric matcher accepts a whole family of values, so it has to be
  paired off after the exact members have claimed their partners. }
function IsAsymmetricValue(const AValue: TGocciaValue): Boolean;
begin
  Result := AValue is TGocciaAsymmetricMatcherValue;
end;

{ Exactly one side being an array, Set or Map makes the two values different
  kinds of container, which never compare equal even loosely. }
function IsMismatchedContainer(const AActual, AExpected: TGocciaValue): Boolean;
begin
  Result :=
    ((AActual is TGocciaArrayValue) <> (AExpected is TGocciaArrayValue)) or
    ((AActual is TGocciaSetValue) <> (AExpected is TGocciaSetValue)) or
    ((AActual is TGocciaMapValue) <> (AExpected is TGocciaMapValue));
end;

function IsDeepEqualInternal(const AActual, AExpected: TGocciaValue;
  var AComparedPairs: TComparedValuePairArray;
  const AStrict: Boolean): Boolean; forward;

{ Snapshots of the members are taken before matching because the match is
  order-insensitive and needs random access, and because a recursive compare
  can run user getters that mutate either collection mid-walk. }
function SnapshotSetMembers(const ASet: TGocciaSetValue): TArray<TGocciaValue>;
var
  Cursor, Count: Integer;
  Item: TGocciaValue;
begin
  SetLength(Result, ASet.Count);
  Count := 0;
  Cursor := 0;
  ASet.RetainIterator;
  try
    while ASet.NextItem(Cursor, Item) and (Count < Length(Result)) do
    begin
      Result[Count] := Item;
      Inc(Count);
    end;
  finally
    ASet.ReleaseIterator;
  end;
  SetLength(Result, Count);
end;

procedure SnapshotMapEntries(const AMap: TGocciaMapValue;
  out AKeys, AValues: TArray<TGocciaValue>);
var
  Cursor, Count: Integer;
  Key, Value: TGocciaValue;
begin
  SetLength(AKeys, AMap.Count);
  SetLength(AValues, AMap.Count);
  Count := 0;
  Cursor := 0;
  AMap.RetainIterator;
  try
    while AMap.NextEntry(Cursor, Key, Value) and (Count < Length(AKeys)) do
    begin
      AKeys[Count] := Key;
      AValues[Count] := Value;
      Inc(Count);
    end;
  finally
    AMap.ReleaseIterator;
  end;
  SetLength(AKeys, Count);
  SetLength(AValues, Count);
end;

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
  I, J: Integer;
  Key: string;
  CommonCount: Integer;
  LeftValue, RightValue: TGocciaValue;
  ActualMembers, ExpectedMembers: TArray<TGocciaValue>;
  ActualMapKeys, ActualMapValues: TArray<TGocciaValue>;
  ExpectedMapKeys, ExpectedMapValues: TArray<TGocciaValue>;
  Used: TArray<Boolean>;
  Claimed: TArray<Boolean>;
  Pass: Integer;
  TrialPairs: TComparedValuePairArray;
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

  // Different kinds of container never compare equal, however similar their
  // contents look (a Set is not its array of members, a Map is not an object).
  if IsMismatchedContainer(AActual, AExpected) then
  begin
    Result := False;
    Exit;
  end;

  // Handle arrays
  if (AActual is TGocciaArrayValue) and (AExpected is TGocciaArrayValue) then
  begin
    ActualArr := TGocciaArrayValue(AActual);
    ExpectedArr := TGocciaArrayValue(AExpected);

    { Strict equality preserves length and sparseness. Loose equality ignores
      undefined items past the shorter length, so [2] equals [2, undefined],
      while a differing item inside the common prefix still fails. }
    if AStrict and
       (ActualArr.Elements.Count <> ExpectedArr.Elements.Count) then
    begin
      Result := False;
      Exit;
    end;

    // An array reachable from itself would otherwise recurse forever.
    if HasComparedPair(AComparedPairs, AActual, AExpected) then
    begin
      Result := True;
      Exit;
    end;
    AddComparedPair(AComparedPairs, AActual, AExpected);

    CommonCount := ActualArr.Elements.Count;
    if ExpectedArr.Elements.Count < CommonCount then
      CommonCount := ExpectedArr.Elements.Count;

    for I := 0 to CommonCount - 1 do
    begin
      LeftValue := ActualArr.Elements[I];
      RightValue := ExpectedArr.Elements[I];

      if AStrict then
      begin
        // A hole is only ever equal to another hole, never to undefined.
        if (LeftValue = TGocciaHoleValue.HoleValue) or
           (RightValue = TGocciaHoleValue.HoleValue) then
        begin
          if LeftValue <> RightValue then
          begin
            Result := False;
            Exit;
          end;
          Continue;
        end;
      end
      else
      begin
        if LeftValue = TGocciaHoleValue.HoleValue then
          LeftValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if RightValue = TGocciaHoleValue.HoleValue then
          RightValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      end;

      if not IsDeepEqualInternal(LeftValue, RightValue, AComparedPairs,
        AStrict) then
      begin
        Result := False;
        Exit;
      end;
    end;

    // Surplus items exist only under loose equality, and must be undefined.
    for I := CommonCount to ActualArr.Elements.Count - 1 do
      if not IsUndefinedLike(ActualArr.Elements[I]) then
      begin
        Result := False;
        Exit;
      end;
    for I := CommonCount to ExpectedArr.Elements.Count - 1 do
      if not IsUndefinedLike(ExpectedArr.Elements[I]) then
      begin
        Result := False;
        Exit;
      end;

    Result := True;
    Exit;
  end;

  { Sets and Maps compare without regard to insertion order, and membership
    uses deep equality so sets of objects behave like sets of values. Members
    are paired off greedily against the members not yet claimed. }
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

    ActualMembers := SnapshotSetMembers(TGocciaSetValue(AActual));
    ExpectedMembers := SnapshotSetMembers(TGocciaSetValue(AExpected));
    if Length(ActualMembers) <> Length(ExpectedMembers) then
    begin
      Result := False;
      Exit;
    end;

    { Two passes so a literal member is never stranded by a matcher that
      claimed its partner first: pass 0 pairs off the plain members, pass 1
      lets the asymmetric matchers take what is left. }
    SetLength(Used, Length(ExpectedMembers));
    SetLength(Claimed, Length(ActualMembers));
    for Pass := 0 to 1 do
      for I := 0 to High(ActualMembers) do
      begin
        if Claimed[I] then
          Continue;
        if (Pass = 0) and IsAsymmetricValue(ActualMembers[I]) then
          Continue;
        for J := 0 to High(ExpectedMembers) do
        begin
          if Used[J] then
            Continue;
          if (Pass = 0) and IsAsymmetricValue(ExpectedMembers[J]) then
            Continue;
          { A rejected candidate must leave no trace: the pairs it recorded on
            the way down would otherwise read as "already comparing" — and so
            as equal — when a later pass retries the same two members. }
          CopyComparedPairs(AComparedPairs, TrialPairs);
          if IsDeepEqualInternal(ActualMembers[I], ExpectedMembers[J],
            TrialPairs, AStrict) then
          begin
            CopyComparedPairs(TrialPairs, AComparedPairs);
            Used[J] := True;
            Claimed[I] := True;
            Break;
          end;
        end;
      end;

    for I := 0 to High(ActualMembers) do
      if not Claimed[I] then
      begin
        Result := False;
        Exit;
      end;

    Result := True;
    Exit;
  end;

  // Map entries pair off on both key and value, so two maps holding the same
  // entries in different insertion order are equal.
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

    SnapshotMapEntries(TGocciaMapValue(AActual), ActualMapKeys, ActualMapValues);
    SnapshotMapEntries(TGocciaMapValue(AExpected), ExpectedMapKeys,
      ExpectedMapValues);
    if Length(ActualMapKeys) <> Length(ExpectedMapKeys) then
    begin
      Result := False;
      Exit;
    end;

    { Same two passes as Sets: an entry whose key or value is a matcher only
      claims a partner once the exact entries have been paired off. }
    SetLength(Used, Length(ExpectedMapKeys));
    SetLength(Claimed, Length(ActualMapKeys));
    for Pass := 0 to 1 do
      for I := 0 to High(ActualMapKeys) do
      begin
        if Claimed[I] then
          Continue;
        if (Pass = 0) and (IsAsymmetricValue(ActualMapKeys[I]) or
           IsAsymmetricValue(ActualMapValues[I])) then
          Continue;
        for J := 0 to High(ExpectedMapKeys) do
        begin
          if Used[J] then
            Continue;
          if (Pass = 0) and (IsAsymmetricValue(ExpectedMapKeys[J]) or
             IsAsymmetricValue(ExpectedMapValues[J])) then
            Continue;
          // Same rejected-candidate isolation as the Set branch above.
          CopyComparedPairs(AComparedPairs, TrialPairs);
          if IsDeepEqualInternal(ActualMapKeys[I], ExpectedMapKeys[J],
            TrialPairs, AStrict) and
             IsDeepEqualInternal(ActualMapValues[I], ExpectedMapValues[J],
            TrialPairs, AStrict) then
          begin
            CopyComparedPairs(TrialPairs, AComparedPairs);
            Used[J] := True;
            Claimed[I] := True;
            Break;
          end;
        end;
      end;

    for I := 0 to High(ActualMapKeys) do
      if not Claimed[I] then
      begin
        Result := False;
        Exit;
      end;

    Result := True;
    Exit;
  end;

  // Handle objects (arrays/sets/maps already handled by earlier branches)
  if (AActual is TGocciaObjectValue) and (AExpected is TGocciaObjectValue) then
  begin
    ActualObj := TGocciaObjectValue(AActual);
    ExpectedObj := TGocciaObjectValue(AExpected);

    if AStrict and not HasSameObjectType(AActual, AExpected) then
    begin
      Result := False;
      Exit;
    end;

    // Get enumerable property names from both objects
    ActualKeys := ActualObj.GetEnumerablePropertyNames;
    ExpectedKeys := ExpectedObj.GetEnumerablePropertyNames;

    { Only strict equality requires the key sets to match exactly. Loose
      equality ignores a key whose value is undefined when the other side
      does not have it at all, in either direction. }
    if AStrict and (Length(ActualKeys) <> Length(ExpectedKeys)) then
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
        if AStrict or not IsUndefinedLike(ActualObj.GetProperty(Key)) then
        begin
          Result := False;
          Exit;
        end;
        Continue;
      end;

      // Recursively compare property values
      if not IsDeepEqualInternal(ActualObj.GetProperty(Key),
        ExpectedObj.GetProperty(Key), AComparedPairs, AStrict) then
      begin
        Result := False;
        Exit;
      end;
    end;

    // Keys only the expected side has are equally subject to the rule above.
    for I := 0 to High(ExpectedKeys) do
    begin
      Key := ExpectedKeys[I];
      if not ActualObj.HasOwnProperty(Key) then
        if AStrict or not IsUndefinedLike(ExpectedObj.GetProperty(Key)) then
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
  ActualArr, ExpectedArr: TGocciaArrayValue;
  DeepComparedPairs: TComparedValuePairArray;
  ExpectedKeys: TArray<string>;
  I: Integer;
  Key: string;
  LeftValue, RightValue: TGocciaValue;
begin
  CopyComparedPairs(AComparedPairs, DeepComparedPairs);
  if IsDeepEqualInternal(AActual, AExpected, DeepComparedPairs, False) then
  begin
    Result := True;
    Exit;
  end;

  { Object-subset semantics do not make arrays partial in length: the shape
    must describe every element. Each element is still matched partially, so
    an array of objects can be described by an array of subsets. }
  if (AActual is TGocciaArrayValue) and (AExpected is TGocciaArrayValue) then
  begin
    ActualArr := TGocciaArrayValue(AActual);
    ExpectedArr := TGocciaArrayValue(AExpected);
    if ActualArr.Elements.Count <> ExpectedArr.Elements.Count then
      Exit(False);
    if HasComparedPair(AComparedPairs, AActual, AExpected) then
      Exit(True);
    AddComparedPair(AComparedPairs, AActual, AExpected);

    for I := 0 to ActualArr.Elements.Count - 1 do
    begin
      LeftValue := ActualArr.Elements[I];
      RightValue := ExpectedArr.Elements[I];
      if LeftValue = TGocciaHoleValue.HoleValue then
        LeftValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      if RightValue = TGocciaHoleValue.HoleValue then
        RightValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      if not IsPartialDeepEqualInternal(LeftValue, RightValue, AComparedPairs,
        AIncludeInherited) then
        Exit(False);
    end;

    Result := True;
    Exit;
  end;

  { The shape drives the semantics: an expected plain object describes a subset
    of keys even when the actual value is an array, but an expected array can
    only describe an array. }
  if AExpected is TGocciaArrayValue then
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
