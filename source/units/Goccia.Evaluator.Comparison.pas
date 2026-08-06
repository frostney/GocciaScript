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
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
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
{ Strict equality additionally requires the two objects to resolve to the same
  constructor. Comparing the constructor the prototype chain yields — rather
  than any class the engine attached, or the prototype object itself — is what
  keeps Object.create(null) and Object.create(Array.prototype) apart from a
  plain literal while still equating Object.create(somePlainObject) with one,
  and what keeps two same-named anonymous classes distinct. }
function HasSameObjectType(const AActual, AExpected: TGocciaObjectValue): Boolean;
begin
  Result := AActual.GetProperty(PROP_CONSTRUCTOR) =
    AExpected.GetProperty(PROP_CONSTRUCTOR);
end;

{ An error's stack is incidental: two errors raised at different places are
  still the same error to a matcher, so an own `stack` never participates. }
function IsIgnoredErrorKey(const AKey: string;
  const ABothErrors: Boolean): Boolean;
begin
  Result := ABothErrors and (AKey = PROP_STACK);
end;

function CountComparableKeys(const AKeys: TArray<string>;
  const ABothErrors: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AKeys) do
    if not IsIgnoredErrorKey(AKeys[I], ABothErrors) then
      Inc(Result);
end;

{ Reads a property the comparison needs but the enumerable-key walk cannot
  see, normalizing a missing property to undefined. The value is returned raw:
  stringifying it here would both collapse distinct values and run user code
  inside the matcher. }
function ErrorFieldValue(const AObject: TGocciaObjectValue;
  const AName: string): TGocciaValue;
begin
  Result := AObject.GetProperty(AName);
  if Result = nil then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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

{ Reads the same property from both sides and compares the results.

  Every read can run a user getter, and a getter is a GC safe point. Written as
  one call expression — `Compare(Actual.GetProperty(K), Expected.GetProperty(K))`
  — the first read's result sits in an unrooted compiler temporary while the
  second getter runs, and neither result is rooted for the recursive walk, which
  re-enters user code again at every level. Rooting here roots the whole tree:
  each level holds its own two values while the level below it runs. }
function DeepEqualPropertyPair(const AActualObj, AExpectedObj: TGocciaObjectValue;
  const AName: string; var AComparedPairs: TComparedValuePairArray;
  const AStrict: Boolean): Boolean;
var
  ActualValue, ExpectedValue: TGocciaValue;
  ActualRoot, ExpectedRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(ActualRoot);
  InitializeTempRoot(ExpectedRoot);
  try
    ActualValue := ErrorFieldValue(AActualObj, AName);
    AddTempRootIfNeeded(ActualRoot, ActualValue);
    ExpectedValue := ErrorFieldValue(AExpectedObj, AName);
    AddTempRootIfNeeded(ExpectedRoot, ExpectedValue);
    Result := IsDeepEqualInternal(ActualValue, ExpectedValue, AComparedPairs,
      AStrict);
  finally
    RemoveTempRootIfNeeded(ExpectedRoot);
    RemoveTempRootIfNeeded(ActualRoot);
  end;
end;

{ Subset counterpart of DeepEqualPropertyPair, with the same rooting rationale. }
function PartialDeepEqualPropertyPair(const AActualObj,
  AExpectedObj: TGocciaObjectValue; const AName: string;
  var AComparedPairs: TComparedValuePairArray;
  const AIncludeInherited: Boolean): Boolean;
var
  ActualValue, ExpectedValue: TGocciaValue;
  ActualRoot, ExpectedRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(ActualRoot);
  InitializeTempRoot(ExpectedRoot);
  try
    ActualValue := ErrorFieldValue(AActualObj, AName);
    AddTempRootIfNeeded(ActualRoot, ActualValue);
    ExpectedValue := ErrorFieldValue(AExpectedObj, AName);
    AddTempRootIfNeeded(ExpectedRoot, ExpectedValue);
    Result := IsPartialDeepEqualInternal(ActualValue, ExpectedValue,
      AComparedPairs, AIncludeInherited);
  finally
    RemoveTempRootIfNeeded(ExpectedRoot);
    RemoveTempRootIfNeeded(ActualRoot);
  end;
end;

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
  Matched: Boolean;
  BothErrors: Boolean;
  ActualIsError, ExpectedIsError: Boolean;
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

    { Array equality is length-first for both matchers: [1] never equals
      [1, undefined]. Only sparseness is forgiven loosely, and only at equal
      length, where a hole reads as undefined. }
    if ActualArr.Elements.Count <> ExpectedArr.Elements.Count then
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

    for I := 0 to ActualArr.Elements.Count - 1 do
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

    { Membership is existential, not a pairing: every actual member has to
      deep-equal some expected member, and an expected member may match
      several actual members or none at all. }
    for I := 0 to High(ActualMembers) do
    begin
      Matched := False;
      for J := 0 to High(ExpectedMembers) do
      begin
        { A rejected candidate must leave no trace: the pairs it recorded on
          the way down would otherwise read as "already comparing" — and so
          as equal — when a later member is compared against the same pair. }
        CopyComparedPairs(AComparedPairs, TrialPairs);
        if IsDeepEqualInternal(ActualMembers[I], ExpectedMembers[J],
          TrialPairs, AStrict) then
        begin
          CopyComparedPairs(TrialPairs, AComparedPairs);
          Matched := True;
          Break;
        end;
      end;
      if not Matched then
      begin
        Result := False;
        Exit;
      end;
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

    { Same existential rule as Sets, over whole entries: an actual entry has
      to find some expected entry matching on both key and value. }
    for I := 0 to High(ActualMapKeys) do
    begin
      Matched := False;
      for J := 0 to High(ExpectedMapKeys) do
      begin
        // Same rejected-candidate isolation as the Set branch above.
        CopyComparedPairs(AComparedPairs, TrialPairs);
        if IsDeepEqualInternal(ActualMapKeys[I], ExpectedMapKeys[J],
          TrialPairs, AStrict) and
           IsDeepEqualInternal(ActualMapValues[I], ExpectedMapValues[J],
          TrialPairs, AStrict) then
        begin
          CopyComparedPairs(TrialPairs, AComparedPairs);
          Matched := True;
          Break;
        end;
      end;
      if not Matched then
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

    ActualIsError := IsErrorObject(AActual);
    ExpectedIsError := IsErrorObject(AExpected);
    BothErrors := ActualIsError and ExpectedIsError;

    { An error is never equal to a plain object, however similar their visible
      properties are: name and message are not enumerable, so without this the
      key walk below would call every error equal to every other one and to an
      empty object. }
    if ActualIsError <> ExpectedIsError then
    begin
      Result := False;
      Exit;
    end;

    if AStrict and not HasSameObjectType(ActualObj, ExpectedObj) then
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
    if AStrict and (CountComparableKeys(ActualKeys, BothErrors) <>
       CountComparableKeys(ExpectedKeys, BothErrors)) then
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

    { An error's identity is its name, message, cause and aggregated errors,
      none of which the enumerable-key walk below can see. Comparing them
      after the pair is registered lets a chain that loops back on itself
      terminate. }
    if BothErrors then
    begin
      if not DeepEqualPropertyPair(ActualObj, ExpectedObj, PROP_NAME,
        AComparedPairs, AStrict) or
         not DeepEqualPropertyPair(ActualObj, ExpectedObj, PROP_MESSAGE,
        AComparedPairs, AStrict) then
      begin
        Result := False;
        Exit;
      end;

      { Cause is expected-driven: it participates only when the expected error
        carries a defined one, so an actual error may hold a cause the
        expectation does not mention. }
      if ExpectedObj.HasOwnProperty(PROP_CAUSE) and
         not IsUndefinedLike(ErrorFieldValue(ExpectedObj, PROP_CAUSE)) then
      begin
        if not ActualObj.HasOwnProperty(PROP_CAUSE) then
        begin
          Result := False;
          Exit;
        end;
        if not DeepEqualPropertyPair(ActualObj, ExpectedObj, PROP_CAUSE,
          AComparedPairs, AStrict) then
        begin
          Result := False;
          Exit;
        end;
      end;

      // An AggregateError's collected errors participate the same way.
      if ExpectedObj.HasOwnProperty(PROP_ERRORS) then
      begin
        if not ActualObj.HasOwnProperty(PROP_ERRORS) then
        begin
          Result := False;
          Exit;
        end;
        if not DeepEqualPropertyPair(ActualObj, ExpectedObj, PROP_ERRORS,
          AComparedPairs, AStrict) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;

    // Check if all keys exist in both objects and values are deeply equal
    for I := 0 to High(ActualKeys) do
    begin
      Key := ActualKeys[I];
      if IsIgnoredErrorKey(Key, BothErrors) then
        Continue;

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
      if not DeepEqualPropertyPair(ActualObj, ExpectedObj, Key,
        AComparedPairs, AStrict) then
      begin
        Result := False;
        Exit;
      end;
    end;

    // Keys only the expected side has are equally subject to the rule above.
    for I := 0 to High(ExpectedKeys) do
    begin
      Key := ExpectedKeys[I];
      if IsIgnoredErrorKey(Key, BothErrors) then
        Continue;
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
  { Shapes the expectation cannot describe a subset of are compared in full:
    an asymmetric matcher runs its own match, and an expected error, Set or
    Map must equal the actual one outright rather than merely be contained
    by it. An expected plain object keeps subset semantics even when the
    actual value is one of these. }
  if (AActual is TGocciaAsymmetricMatcherValue) or
     (AExpected is TGocciaAsymmetricMatcherValue) or
     IsErrorObject(AExpected) or (AExpected is TGocciaSetValue) or
     (AExpected is TGocciaMapValue) then
  begin
    CopyComparedPairs(AComparedPairs, DeepComparedPairs);
    Result := IsDeepEqualInternal(AActual, AExpected, DeepComparedPairs,
      False);
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

      if not PartialDeepEqualPropertyPair(ActualObj, ExpectedObj, Key,
        AComparedPairs, AIncludeInherited) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
    Exit;
  end;

  // Primitives and everything else compare outright.
  CopyComparedPairs(AComparedPairs, DeepComparedPairs);
  Result := IsDeepEqualInternal(AActual, AExpected, DeepComparedPairs, False);
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
