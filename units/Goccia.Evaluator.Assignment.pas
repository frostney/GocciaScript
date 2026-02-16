unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue, Goccia.Values.ClassValue,
  Goccia.Values.ArrayValue, Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Token, Goccia.Interfaces, Goccia.Error.ThrowErrorCallback, SysUtils, Math, Goccia.Evaluator.Arithmetic;

// Unified property access helpers
function GetPropertyFromValue(Obj: TGocciaValue; const PropName: string): TGocciaValue;
procedure SetPropertyOnValue(Obj: TGocciaValue; const PropName: string; Value: TGocciaValue);
procedure DefinePropertyOnValue(Obj: TGocciaValue; const PropName: string; Value: TGocciaValue);

// Property assignment operations
procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);
procedure AssignComputedProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; Operator: TGocciaTokenType; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);

// Increment/Decrement operations
function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue; inline;

implementation

uses Goccia.Values.ClassHelper, Goccia.Values.ErrorHelper;

function GetPropertyFromValue(Obj: TGocciaValue; const PropName: string): TGocciaValue;
begin
  if (Obj is TGocciaInstanceValue) then
    Result := TGocciaInstanceValue(Obj).GetProperty(PropName)
  else if (Obj is TGocciaArrayValue) then
    Result := TGocciaArrayValue(Obj).GetProperty(PropName)
  else if (Obj is TGocciaClassValue) then
    Result := TGocciaClassValue(Obj).GetProperty(PropName)
  else if (Obj is TGocciaObjectValue) then
    Result := TGocciaObjectValue(Obj).GetProperty(PropName)
  else
    Result := nil;
end;

procedure SetPropertyOnValue(Obj: TGocciaValue; const PropName: string; Value: TGocciaValue);
begin
  if (Obj is TGocciaInstanceValue) then
    TGocciaInstanceValue(Obj).AssignProperty(PropName, Value)
  else if (Obj is TGocciaArrayValue) then
    TGocciaArrayValue(Obj).SetProperty(PropName, Value)
  else if (Obj is TGocciaObjectValue) then
    TGocciaObjectValue(Obj).AssignProperty(PropName, Value)
  else if (Obj is TGocciaClassValue) then
    TGocciaClassValue(Obj).SetProperty(PropName, Value);
end;

procedure DefinePropertyOnValue(Obj: TGocciaValue; const PropName: string; Value: TGocciaValue);
begin
  if (Obj is TGocciaInstanceValue) then
    TGocciaInstanceValue(Obj).DefineProperty(PropName,
      TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]))
  else if (Obj is TGocciaObjectValue) then
    TGocciaObjectValue(Obj).DefineProperty(PropName,
      TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]))
  else if (Obj is TGocciaClassValue) then
    TGocciaClassValue(Obj).SetProperty(PropName, Value);
end;

procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);
begin
  if (Obj is TGocciaInstanceValue) or (Obj is TGocciaObjectValue) or
     (Obj is TGocciaClassValue) or (Obj is TGocciaArrayValue) then
    SetPropertyOnValue(Obj, PropertyName, Value)
  else if Assigned(OnError) then
    ThrowTypeError('Cannot set property on non-object');
end;

procedure AssignComputedProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);
begin
  if (Obj is TGocciaInstanceValue) or (Obj is TGocciaObjectValue) or
     (Obj is TGocciaClassValue) or (Obj is TGocciaArrayValue) then
    SetPropertyOnValue(Obj, PropertyName, Value)
  else if Assigned(OnError) then
    ThrowTypeError('Cannot set property on non-object');
end;

procedure PerformPropertyCompoundAssignment(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; Operator: TGocciaTokenType; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
begin
  CurrentValue := GetPropertyFromValue(Obj, PropertyName);
  if CurrentValue = nil then
  begin
    if Assigned(OnError) then
      OnError('Cannot access property on non-object', Line, Column);
    Exit;
  end;

  NewValue := PerformCompoundOperation(CurrentValue, Value, Operator);
  SetPropertyOnValue(Obj, PropertyName, NewValue);
end;

function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue;
begin
  if IsIncrement then
    Result := TGocciaNumberLiteralValue.Create(OldValue.ToNumberLiteral.Value + 1)
  else
    Result := TGocciaNumberLiteralValue.Create(OldValue.ToNumberLiteral.Value - 1);
end;

end.
