unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.ObjectValue, Goccia.Values.ClassValue,
  Goccia.Values.ArrayValue, Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.NumberValue, Goccia.Values.StringValue, Goccia.Values.UndefinedValue,
  Goccia.Token, Goccia.Interfaces, Goccia.Error, SysUtils, Math, Goccia.Evaluator.Arithmetic;

// Property assignment operations
procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowError; Line, Column: Integer);
procedure AssignComputedProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowError; Line, Column: Integer);

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; Operator: TGocciaTokenType; OnError: TGocciaThrowError; Line, Column: Integer);

// Increment/Decrement operations
function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue; inline;

implementation

procedure AssignProperty(Obj: TGocciaValue; const PropertyName: string; Value: TGocciaValue; OnError: TGocciaThrowError; Line, Column: Integer);
begin
  // Handle different object types for property assignment
  // Direct property assignment (obj.prop = value) should create the property if it doesn't exist
  if (Obj is TGocciaInstanceValue) then
  begin
    TGocciaInstanceValue(Obj).DefineProperty(PropertyName,
      TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]));
  end
  else if (Obj is TGocciaObjectValue) then
  begin
    TGocciaObjectValue(Obj).DefineProperty(PropertyName,
      TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]));
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
    TGocciaInstanceValue(Obj).DefineProperty(PropertyName,
      TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]));
  end
  else if (Obj is TGocciaObjectValue) then
  begin
    TGocciaObjectValue(Obj).DefineProperty(PropertyName,
      TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]));
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

  // Set the new value (create property if it doesn't exist, like JavaScript)
  if (Obj is TGocciaInstanceValue) then
    TGocciaInstanceValue(Obj).DefineProperty(PropertyName,
      TGocciaPropertyDescriptorData.Create(NewValue, [pfEnumerable, pfConfigurable, pfWritable]))
  else if (Obj is TGocciaObjectValue) then
    TGocciaObjectValue(Obj).DefineProperty(PropertyName,
      TGocciaPropertyDescriptorData.Create(NewValue, [pfEnumerable, pfConfigurable, pfWritable]))
  else if (Obj is TGocciaClassValue) then
    TGocciaClassValue(Obj).SetProperty(PropertyName, NewValue)
  else if (Obj is TGocciaArrayValue) then
    TGocciaArrayValue(Obj).SetProperty(PropertyName, NewValue);
end;

function PerformIncrement(OldValue: TGocciaValue; IsIncrement: Boolean): TGocciaValue;
begin
  if IsIncrement then
    Result := TGocciaNumberValue.Create(OldValue.ToNumber + 1)
  else
    Result := TGocciaNumberValue.Create(OldValue.ToNumber - 1);
end;

end.
