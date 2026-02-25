unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Token,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

// Property definition with descriptor (falls back to SetProperty for non-objects)
procedure DefinePropertyOnValue(const AObj: TGocciaValue; const APropName: string; const AValue: TGocciaValue);

// Property assignment with error handling for non-objects
procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
procedure PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);

// Increment/Decrement operations
function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue; inline;

implementation

uses
  Goccia.Evaluator.Arithmetic,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

procedure DefinePropertyOnValue(const AObj: TGocciaValue; const APropName: string; const AValue: TGocciaValue);
begin
  if (AObj is TGocciaObjectValue) then
    TGocciaObjectValue(AObj).DefineProperty(APropName,
      TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable, pfConfigurable, pfWritable]))
  else
    AObj.SetProperty(APropName, AValue);
end;

procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
begin
  if (AObj is TGocciaInstanceValue) or (AObj is TGocciaObjectValue) or
     (AObj is TGocciaClassValue) or (AObj is TGocciaArrayValue) then
    AObj.SetProperty(APropertyName, AValue)
  else if Assigned(AOnError) then
    // AOnError is not invoked here — ThrowTypeError must be used because this is
    // a JavaScript-level TypeError (TGocciaThrowValue), not an interpreter-level
    // runtime error (TGocciaRuntimeError) which is what AOnError produces.
    // The Assigned check guards against raising in contexts without error handling.
    ThrowTypeError('Cannot set property on non-object');
end;

procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
begin
  CurrentValue := AObj.GetProperty(APropertyName);
  if CurrentValue = nil then
  begin
    if Assigned(AOnError) then
      AOnError('Cannot access property on non-object', ALine, AColumn);
    Exit;
  end;

  NewValue := PerformCompoundOperation(CurrentValue, AValue, AOperator);
  AObj.SetProperty(APropertyName, NewValue);
end;

procedure PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
begin
  if AObj is TGocciaClassValue then
    CurrentValue := TGocciaClassValue(AObj).GetSymbolProperty(ASymbol)
  else if AObj is TGocciaObjectValue then
    CurrentValue := TGocciaObjectValue(AObj).GetSymbolProperty(ASymbol)
  else
  begin
    if Assigned(AOnError) then
      AOnError('Cannot access property on non-object', ALine, AColumn);
    Exit;
  end;
  if CurrentValue = nil then
    CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  NewValue := PerformCompoundOperation(CurrentValue, AValue, AOperator);
  if AObj is TGocciaClassValue then
    TGocciaClassValue(AObj).AssignSymbolProperty(ASymbol, NewValue)
  else
    TGocciaObjectValue(AObj).AssignSymbolProperty(ASymbol, NewValue);
end;

function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue;
begin
  if AIsIncrement then
    Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value + 1)
  else
    Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value - 1);
end;

end.
