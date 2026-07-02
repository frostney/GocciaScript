unit Goccia.Evaluator.TypeOperations;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TIsObjectInstanceOfClassFunction = function(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;

function EvaluateTypeof(const AOperand: TGocciaValue): TGocciaValue; inline;
function EvaluateInstanceof(const ALeft, ARight: TGocciaValue; const AIsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
function EvaluateInOperator(const ALeft, ARight: TGocciaValue): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Keywords.Reserved,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ProxyValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

function EvaluateTypeof(const AOperand: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AOperand.TypeOf);
end;

function IsPrototypeInChain(const AObj: TGocciaObjectValue; const ATargetProto: TGocciaObjectValue): Boolean;
var
  CurrentProto: TGocciaObjectValue;
begin
  Result := False;
  CurrentProto := AObj.Prototype;
  while Assigned(CurrentProto) do
  begin
    if CurrentProto = ATargetProto then
    begin
      Result := True;
      Exit;
    end;
    CurrentProto := CurrentProto.Prototype;
  end;
end;

function EvaluateInstanceof(const ALeft, ARight: TGocciaValue; const AIsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
begin
  // ES2026 §13.10.2 InstanceofOperator(value, target)
  if InstanceofOperatorResult(ALeft, ARight) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function EvaluateInOperator(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PropertyName: string;
  ResolvedKey: TGocciaValue;
begin
  // ECMAScript §13.10.1 step 5: If rval is not an Object, throw TypeError
  if ARight.IsPrimitive then
  begin
    if ALeft is TGocciaSymbolValue then
      ThrowTypeError(Format(SErrorCannotUseInOperator,
        [TGocciaSymbolValue(ALeft).ToDisplayString.Value, ARight.ToStringLiteral.Value]),
        SSuggestCheckNullBeforeAccess)
    else
      ThrowTypeError(Format(SErrorCannotUseInOperator,
        [ALeft.ToStringLiteral.Value, ARight.ToStringLiteral.Value]),
        SSuggestCheckNullBeforeAccess);
  end;

  // Step 6: Return ? HasProperty(rval, ? ToPropertyKey(lval))
  ResolvedKey := ToPropertyKey(ALeft);

  if ResolvedKey is TGocciaSymbolValue then
  begin
    if ARight is TGocciaProxyValue then
    begin
      if TGocciaProxyValue(ARight).HasSymbolTrap(TGocciaSymbolValue(ResolvedKey)) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;
    if ARight is TGocciaObjectValue then
    begin
      if TGocciaObjectValue(ARight).HasSymbolProperty(TGocciaSymbolValue(ResolvedKey)) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  PropertyName := TGocciaStringLiteralValue(ResolvedKey).Value;

  // Proxy intercept: has trap takes precedence over all other checks
  if ARight is TGocciaProxyValue then
  begin
    if TGocciaProxyValue(ARight).HasTrap(PropertyName) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if ARight is TGocciaInstanceValue then
  begin
    // Check if property exists in class instance
    if TGocciaInstanceValue(ARight).HasProperty(PropertyName) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else if ARight is TGocciaObjectValue then
  begin
    // Check if property exists in object (check after arrays and instances)
    if TGocciaObjectValue(ARight).HasProperty(PropertyName) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

end.
