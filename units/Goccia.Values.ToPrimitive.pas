unit Goccia.Values.ToPrimitive;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaToPrimitiveHint = (tphDefault, tphNumber, tphString);

// ES2026 §7.1.1 ToPrimitive(input [, preferredType])
function ToPrimitive(const AValue: TGocciaValue; const AHint: TGocciaToPrimitiveHint = tphDefault): TGocciaValue;

// ES2026 §7.1.17 ToString(argument)
function ToECMAString(const AValue: TGocciaValue): TGocciaStringLiteralValue;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue;

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
      Result := AResult.IsPrimitive;
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
    ThrowTypeError('Cannot convert object to primitive value');
  end;

  Result := AValue;
end;

// ES2026 §7.1.17 ToString(argument)
function ToECMAString(const AValue: TGocciaValue): TGocciaStringLiteralValue;
var
  Prim: TGocciaValue;
begin
  if AValue is TGocciaStringLiteralValue then
  begin
    Result := TGocciaStringLiteralValue(AValue);
    Exit;
  end;

  if AValue.IsPrimitive then
  begin
    Result := AValue.ToStringLiteral;
    Exit;
  end;

  // ES2026 §7.1.17 step 1: ToPrimitive(argument, string)
  Prim := ToPrimitive(AValue, tphString);
  Result := Prim.ToStringLiteral;
end;

end.
