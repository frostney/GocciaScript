unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue, Goccia.Values.ClassValue,
  Goccia.Values.ArrayValue, Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Token, Goccia.Interfaces, Goccia.Error.ThrowErrorCallback, SysUtils, Math, Goccia.Evaluator.Arithmetic;

// Property definition with descriptor (falls back to SetProperty for non-objects)
procedure DefinePropertyOnValue(Obj: TGocciaValue; const PropName: string; Value: TGocciaValue);

// Property assignment with error handling for non-objects
procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; Operator: TGocciaTokenType; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);

// Increment/Decrement operations
function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue; inline;

implementation

uses Goccia.Values.ClassHelper, Goccia.Values.ErrorHelper;

procedure DefinePropertyOnValue(Obj: TGocciaValue; const PropName: string; Value: TGocciaValue);
begin
  if (Obj is TGocciaObjectValue) then
    TGocciaObjectValue(Obj).DefineProperty(PropName,
      TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]))
  else
    Obj.SetProperty(PropName, Value);
end;

procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);
begin
  if (Obj is TGocciaInstanceValue) or (Obj is TGocciaObjectValue) or
     (Obj is TGocciaClassValue) or (Obj is TGocciaArrayValue) then
    Obj.SetProperty(PropertyName, Value)
  else if Assigned(OnError) then
    ThrowTypeError('Cannot set property on non-object');
end;

procedure PerformPropertyCompoundAssignment(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; Operator: TGocciaTokenType; OnError: TGocciaThrowErrorCallback; Line, Column: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
begin
  CurrentValue := Obj.GetProperty(PropertyName);
  if CurrentValue = nil then
  begin
    if Assigned(OnError) then
      OnError('Cannot access property on non-object', Line, Column);
    Exit;
  end;

  NewValue := PerformCompoundOperation(CurrentValue, Value, Operator);
  Obj.SetProperty(PropertyName, NewValue);
end;

function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue;
begin
  if IsIncrement then
    Result := TGocciaNumberLiteralValue.Create(OldValue.ToNumberLiteral.Value + 1)
  else
    Result := TGocciaNumberLiteralValue.Create(OldValue.ToNumberLiteral.Value - 1);
end;

end.
