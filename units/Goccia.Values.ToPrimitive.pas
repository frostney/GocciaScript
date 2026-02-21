unit Goccia.Values.ToPrimitive;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

{ ECMAScript Abstract Operation: ToPrimitive
  Converts a value to a primitive type. If the value is already a primitive,
  it is returned as-is. For objects, the operation tries valueOf() first,
  then toString(). If neither returns a primitive, falls back to the default
  ToStringLiteral conversion.

  This is a spec-level operation used by:
  - The addition operator (+)
  - Relational comparison operators (<, >, <=, >=)
  - Abstract equality (not used in GocciaScript since only === is supported)
  - Template literal interpolation }
function ToPrimitive(const AValue: TGocciaValue): TGocciaValue;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue;

function ToPrimitive(const AValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Method, CallResult: TGocciaValue;
  Args: TGocciaArgumentsCollection;
begin
  // Primitives return as-is (single VMT call instead of 5 `is` checks)
  if AValue.IsPrimitive then
  begin
    Result := AValue;
    Exit;
  end;

  // Objects: try valueOf first, then toString (default hint = "default"/"number")
  if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);

    // Try valueOf()
    Method := Obj.GetProperty(PROP_VALUE_OF);
    if Assigned(Method) and Method.IsCallable then
    begin
      Args := TGocciaArgumentsCollection.Create;
      try
        CallResult := TGocciaFunctionBase(Method).Call(Args, AValue);
        // If valueOf returned a primitive, use it
        if CallResult.IsPrimitive then
        begin
          Result := CallResult;
          Exit;
        end;
      finally
        Args.Free;
      end;
    end;

    // Try toString()
    Method := Obj.GetProperty(PROP_TO_STRING);
    if Assigned(Method) and Method.IsCallable then
    begin
      Args := TGocciaArgumentsCollection.Create;
      try
        CallResult := TGocciaFunctionBase(Method).Call(Args, AValue);
        if CallResult.IsPrimitive then
        begin
          Result := CallResult;
          Exit;
        end;
      finally
        Args.Free;
      end;
    end;

    // Neither valueOf nor toString returned a primitive — use default
    Result := AValue.ToStringLiteral;
    Exit;
  end;

  // Fallback for any other value type
  Result := AValue;
end;

end.
