unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Values.ObjectValue, Goccia.Values.ClassValue,
  Goccia.Values.ArrayValue, Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Token, Goccia.Interfaces, Goccia.Error, SysUtils, Math, Goccia.Evaluator.Arithmetic;

// Property assignment operations
procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowError; Line, Column: Integer);
procedure AssignComputedProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowError; Line, Column: Integer);

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; Operator: TGocciaTokenType; OnError: TGocciaThrowError; Line, Column: Integer);

// Increment/Decrement operations
function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue; inline;

implementation

uses Goccia.Values.ClassHelper;

procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowError; Line, Column: Integer);
begin
  // Handle different object types for property assignment
  // Direct property assignment (obj.prop = value) should call the object's AssignProperty method
  // which handles setters correctly
  if (Obj is TGocciaInstanceValue) then
  begin
    TGocciaInstanceValue(Obj).AssignProperty(PropertyName, Value);
  end
  else if (Obj is TGocciaObjectValue) then
  begin
    TGocciaObjectValue(Obj).AssignProperty(PropertyName, Value);
  end
  else if (Obj is TGocciaClassValue) then
  begin
    // Handle static property assignment
    TGocciaClassValue(Obj).SetProperty(PropertyName, Value);
  end
  else if Assigned(OnError) then
    OnError('Cannot set property on non-object', Line, Column);
end;

procedure AssignComputedProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowError; Line, Column: Integer);
begin
  // Handle different object types for computed property assignment
  if (Obj is TGocciaArrayValue) then
  begin
    TGocciaArrayValue(Obj).SetProperty(PropertyName, Value);
  end
  else if (Obj is TGocciaInstanceValue) then
  begin
    TGocciaInstanceValue(Obj).AssignProperty(PropertyName, Value);
  end
  else if (Obj is TGocciaObjectValue) then
  begin
    TGocciaObjectValue(Obj).AssignProperty(PropertyName, Value);
  end
  else if (Obj is TGocciaClassValue) then
  begin
    TGocciaClassValue(Obj).SetProperty(PropertyName, Value);
  end
  else if Assigned(OnError) then
    OnError('Cannot set property on non-object', Line, Column);
end;

procedure PerformPropertyCompoundAssignment(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; Operator: TGocciaTokenType; OnError: TGocciaThrowError; Line, Column: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
begin
  // Get current property value
  if (Obj is TGocciaInstanceValue) then
    CurrentValue := TGocciaInstanceValue(Obj).GetProperty(PropertyName)
  else if (Obj is TGocciaObjectValue) then
    CurrentValue := TGocciaObjectValue(Obj).GetProperty(PropertyName)
  else if (Obj is TGocciaClassValue) then
    CurrentValue := TGocciaClassValue(Obj).GetProperty(PropertyName)
  else if (Obj is TGocciaArrayValue) then
    CurrentValue := TGocciaArrayValue(Obj).GetProperty(PropertyName)
  else
  begin
    if Assigned(OnError) then
      OnError('Cannot access property on non-object', Line, Column);
    Exit;
  end;

  // Perform compound operation
  NewValue := PerformCompoundOperation(CurrentValue, Value, Operator);

  // Set the new value (use AssignProperty to handle setters correctly)
  if (Obj is TGocciaInstanceValue) then
    TGocciaInstanceValue(Obj).AssignProperty(PropertyName, NewValue)
  else if (Obj is TGocciaObjectValue) then
    TGocciaObjectValue(Obj).AssignProperty(PropertyName, NewValue)
  else if (Obj is TGocciaClassValue) then
    TGocciaClassValue(Obj).SetProperty(PropertyName, NewValue)
  else if (Obj is TGocciaArrayValue) then
    TGocciaArrayValue(Obj).SetProperty(PropertyName, NewValue);
end;

function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue;
begin
  if IsIncrement then
    Result := TGocciaNumberLiteralValue.Create(OldValue.ToNumberLiteral.Value + 1)
  else
    Result := TGocciaNumberLiteralValue.Create(OldValue.ToNumberLiteral.Value - 1);
end;

end.
