unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Math,
  SysUtils,

  Goccia.Error.ThrowErrorCallback,
  Goccia.Evaluator.Arithmetic,
  Goccia.Interfaces,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

// Property definition with descriptor (falls back to SetProperty for non-objects)
procedure DefinePropertyOnValue(const AObj: TGocciaValue; const APropName: string; const AValue: TGocciaValue);

// Property assignment with error handling for non-objects
procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);

// Increment/Decrement operations
function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue; inline;

implementation

uses
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper;

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

function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue;
begin
  if AIsIncrement then
    Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value + 1)
  else
    Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value - 1);
end;

end.
