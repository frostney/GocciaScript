unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Token,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

// Property assignment with error handling for non-objects
procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
procedure AssignSymbolProperty(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
procedure PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);

// Increment/Decrement operations
function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue; inline;

implementation

uses
  SysUtils,

  Goccia.Arithmetic,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

procedure EnsureAssignableReceiver(const AObj: TGocciaValue; const APropertyName: string);
begin
  if AObj is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, [APropertyName]),
      SSuggestCheckNullBeforeAccess)
  else if AObj is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfUndefined, [APropertyName]),
      SSuggestCheckNullBeforeAccess)
  else if not ((AObj is TGocciaObjectValue) or (AObj is TGocciaClassValue)) then
    ThrowTypeError(SErrorCannotSetPropertyOnNonObject, SSuggestCheckNullBeforeAccess);
end;

procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
begin
  EnsureAssignableReceiver(AObj, APropertyName);
  AObj.SetProperty(APropertyName, AValue);
end;

procedure AssignSymbolProperty(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
begin
  EnsureAssignableReceiver(AObj, ASymbol.ToDisplayString.Value);
  if AObj is TGocciaClassValue then
    TGocciaClassValue(AObj).AssignSymbolProperty(ASymbol, AValue)
  else
    TGocciaObjectValue(AObj).AssignSymbolProperty(ASymbol, AValue);
end;

procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
begin
  EnsureAssignableReceiver(AObj, APropertyName);
  CurrentValue := AObj.GetProperty(APropertyName);
  if CurrentValue = nil then
    CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  NewValue := Goccia.Arithmetic.CompoundOperations(
    CurrentValue, AValue, AOperator);
  AObj.SetProperty(APropertyName, NewValue);
end;

procedure PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
begin
  EnsureAssignableReceiver(AObj, ASymbol.ToDisplayString.Value);
  if AObj is TGocciaClassValue then
    CurrentValue := TGocciaClassValue(AObj).GetSymbolProperty(ASymbol)
  else if AObj is TGocciaObjectValue then
    CurrentValue := TGocciaObjectValue(AObj).GetSymbolProperty(ASymbol)
  else
    CurrentValue := nil;
  if CurrentValue = nil then
    CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  NewValue := Goccia.Arithmetic.CompoundOperations(
    CurrentValue, AValue, AOperator);
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
