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
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(AResult);
      try
        Result := AResult.IsPrimitive;
      finally
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.RemoveTempRoot(AResult);
      end;
    finally
      Args.Free;
    end;
  end;
end;

// ES2026 §7.1.1 ToPrimitive(input [, preferredType])
function ToPrimitive(const AValue: TGocciaValue; const AHint: TGocciaToPrimitiveHint = tphDefault): TGocciaValue;
var
  Obj: TGocciaObjectValue;
begin
  if AValue.IsPrimitive then
  begin
    Result := AValue;
    Exit;
  end;

  if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);

    if AHint = tphString then
    begin
      // String hint: toString() first, then valueOf()
      if TryCallMethod(Obj, PROP_TO_STRING, AValue, Result) then Exit;
      if TryCallMethod(Obj, PROP_VALUE_OF, AValue, Result) then Exit;
    end
    else
    begin
      // Default/number hint: valueOf() first, then toString()
      if TryCallMethod(Obj, PROP_VALUE_OF, AValue, Result) then Exit;
      if TryCallMethod(Obj, PROP_TO_STRING, AValue, Result) then Exit;
    end;

    // ES2026 §7.1.1 step 3e: Throw TypeError if no method returned a primitive
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
