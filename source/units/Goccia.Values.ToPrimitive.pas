unit Goccia.Values.ToPrimitive;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaToPrimitiveHint = (tphDefault, tphNumber, tphString);

// ES2026 §7.1.1 ToPrimitive(input [, preferredType])
function ToPrimitive(const AValue: TGocciaValue; const AHint: TGocciaToPrimitiveHint = tphDefault): TGocciaValue;

// ES2026 §7.1.19 ToPropertyKey(argument). Symbols are returned as-is; everything
// else is coerced via ToString. Callers must check the returned type.
function ToPropertyKey(const AValue: TGocciaValue): TGocciaValue;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

function TryCallMethod(const AObj: TGocciaObjectValue; const AMethodName: string; const AThisValue: TGocciaValue; out AResult: TGocciaValue): Boolean;
var
  Method: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  Result := False;
  Method := AObj.GetProperty(AMethodName);
  if Assigned(Method) and Method.IsCallable then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      AResult := TGocciaFunctionBase(Method).Call(Args, AThisValue);
      if (TGarbageCollector.Instance <> nil) then
        TGarbageCollector.Instance.AddTempRoot(AResult);
      try
        Result := AResult.IsPrimitive;
      finally
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.RemoveTempRoot(AResult);
      end;
    finally
      Args.Free;
    end;
  end;
end;

threadvar
  GHintString: TGocciaStringLiteralValue;
  GHintNumber: TGocciaStringLiteralValue;
  GHintDefault: TGocciaStringLiteralValue;

procedure EnsureHintValue(var ASlot: TGocciaStringLiteralValue; const AValue: string);
{$IFDEF FPC}inline;{$ENDIF}
begin
  if not Assigned(ASlot) then
  begin
    ASlot := TGocciaStringLiteralValue.Create(AValue);
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.PinObject(ASlot);
  end;
end;

function HintValue(const AHint: TGocciaToPrimitiveHint): TGocciaStringLiteralValue;
begin
  case AHint of
    tphString: begin EnsureHintValue(GHintString, 'string'); Result := GHintString; end;
    tphNumber: begin EnsureHintValue(GHintNumber, 'number'); Result := GHintNumber; end;
  else
    EnsureHintValue(GHintDefault, 'default'); Result := GHintDefault;
  end;
end;

// ES2026 §7.1.1 ToPrimitive(input [, preferredType])
function ToPrimitive(const AValue: TGocciaValue; const AHint: TGocciaToPrimitiveHint = tphDefault): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  ExoticToPrim: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  if AValue.IsPrimitive then
  begin
    Result := AValue;
    Exit;
  end;

  if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);

    // ES2026 §7.1.1 step 2.a + §7.3.10 GetMethod(input, @@toPrimitive)
    ExoticToPrim := Obj.GetSymbolProperty(TGocciaSymbolValue.WellKnownToPrimitive);
    if not ((ExoticToPrim is TGocciaUndefinedLiteralValue) or
            (ExoticToPrim is TGocciaNullLiteralValue)) then
    begin
      // GetMethod step 3: If IsCallable(func) is false, throw a TypeError.
      if not ExoticToPrim.IsCallable then
        ThrowTypeError(SErrorToPrimitiveNotCallable, SSuggestToPrimitiveNotCallable);
      // Step 2.b: Call(exoticToPrim, input, « hint »)
      Args := TGocciaArgumentsCollection.Create;
      try
        Args.Add(HintValue(AHint));
        Result := TGocciaFunctionBase(ExoticToPrim).Call(Args, AValue);
      finally
        Args.Free;
      end;
      if (TGarbageCollector.Instance <> nil) then
        TGarbageCollector.Instance.AddTempRoot(Result);
      try
        // Step 2.b.ii: If result is not an Object, return result.
        if Result.IsPrimitive then
          Exit;
        // Step 2.b.iii: Throw a TypeError exception.
        ThrowTypeError(SErrorToPrimitiveReturnedObject, SSuggestToPrimitiveReturnPrimitive);
      finally
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.RemoveTempRoot(Result);
      end;
    end;

    // Step 2.c: OrdinaryToPrimitive(input, hint)
    if AHint = tphString then
    begin
      if TryCallMethod(Obj, PROP_TO_STRING, AValue, Result) then Exit;
      if TryCallMethod(Obj, PROP_VALUE_OF, AValue, Result) then Exit;
    end
    else
    begin
      if TryCallMethod(Obj, PROP_VALUE_OF, AValue, Result) then Exit;
      if TryCallMethod(Obj, PROP_TO_STRING, AValue, Result) then Exit;
    end;

    // Step 2.c (OrdinaryToPrimitive step 6): Throw TypeError
    ThrowTypeError(SErrorCannotConvertToPrimitive, SSuggestToPrimitive);
  end;

  Result := AValue;
end;

// ES2026 §7.1.19 ToPropertyKey(argument). Symbols pass through; non-symbols
// are coerced via ToPrimitive(string) then ToString. The caller is
// responsible for dispatching string vs. symbol property storage.
function ToPropertyKey(const AValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaValue;
begin
  // Step 1: Let key be ? ToPrimitive(argument, string).
  Prim := ToPrimitive(AValue, tphString);
  // Step 2: If key is a Symbol, return key.
  if Prim is TGocciaSymbolValue then
  begin
    Result := Prim;
    Exit;
  end;
  // Step 3: Return ! ToString(key). Prim is a primitive here, so ToStringLiteral
  // on it cannot re-enter user code.
  Result := Prim.ToStringLiteral;
end;

end.
