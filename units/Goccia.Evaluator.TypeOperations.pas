unit Goccia.Evaluator.TypeOperations;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.BooleanValue, Goccia.Values.StringValue,
  Goccia.Values.NumberValue, Goccia.Values.ArrayValue, Goccia.Values.ObjectValue,
  Goccia.Values.ClassValue, Goccia.Values.FunctionValue, Goccia.Values.NativeFunction,
  Goccia.Logger, SysUtils;

type
  TIsObjectInstanceOfClassFunction = function(Obj: TGocciaObjectValue; ClassValue: TGocciaClassValue): Boolean;

function EvaluateTypeof(Operand: TGocciaValue): TGocciaValue; inline;
function EvaluateInstanceof(Left, Right: TGocciaValue; IsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
function EvaluateInOperator(Left, Right: TGocciaValue): TGocciaValue;

implementation

function EvaluateTypeof(Operand: TGocciaValue): TGocciaValue;
begin
  Logger.Debug('EvaluateTypeof: typeof operator called with operand: %s', [Operand.ToString]);
  Result := TGocciaStringValue.Create(Operand.TypeName);
end;

function EvaluateInstanceof(Left, Right: TGocciaValue; IsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
begin
  // Implement instanceof operator according to JavaScript specification
  Logger.Debug('EvaluateInstanceof: instanceof operator called with Left: %s, Right: %s', [Left.ToString, Right.ToString]);

  // Right operand must be a constructor/class
  if not (Right is TGocciaClassValue) then
  begin
    // For built-in types, we need special handling
    Result := TGocciaBooleanValue.Create(False); // Default to false for now
  end
  else
  begin
    // Check if Left is an instance of the Right class
    if Left is TGocciaInstanceValue then
    begin
      // For class instances, check inheritance chain
      if TGocciaClassValue(Right).Name = 'Object' then
      begin
        // All class instances are instances of Object
        Result := TGocciaBooleanValue.Create(True);
      end
      else
      begin
        Result := TGocciaBooleanValue.Create(TGocciaInstanceValue(Left).IsInstanceOf(TGocciaClassValue(Right)));
      end;
    end
    else if (Left is TGocciaFunctionValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Functions are instances of Function
      Result := TGocciaBooleanValue.Create(True);
    end
    else if (Left is TGocciaNativeFunctionValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Native functions are also instances of Function
      Result := TGocciaBooleanValue.Create(True);
    end
    else if (Left is TGocciaClassValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Classes are also instances of Function (since classes are constructor functions)
      Result := TGocciaBooleanValue.Create(True);
    end
    else if (Left is TGocciaArrayValue) and (TGocciaClassValue(Right).Name = 'Array') then
    begin
      // Arrays are instances of Array
      Result := TGocciaBooleanValue.Create(True);
    end
    else if (Left is TGocciaArrayValue) and (TGocciaClassValue(Right).Name = 'Object') then
    begin
      // Arrays are also instances of Object (inheritance)
      Result := TGocciaBooleanValue.Create(True);
    end
    else if (Left is TGocciaObjectValue) and (TGocciaClassValue(Right).Name = 'Object') then
    begin
      // Objects are instances of Object
      Result := TGocciaBooleanValue.Create(True);
    end
    else if Left is TGocciaObjectValue then
    begin
      // General object instanceof check - walk the prototype chain
      Result := TGocciaBooleanValue.Create(IsObjectInstanceOfClass(TGocciaObjectValue(Left), TGocciaClassValue(Right)));
    end
    else
      Result := TGocciaBooleanValue.Create(False);
  end;
end;

function EvaluateInOperator(Left, Right: TGocciaValue): TGocciaValue;
var
  PropertyName: string;
  Index: Integer;
begin
  // Implement the 'in' operator: property/index in object/array/string
  Logger.Debug('EvaluateInOperator: in operator called with Left: %s, Right: %s', [Left.ToString, Right.ToString]);

  PropertyName := Left.ToString; // Left operand is the property name

  if Right is TGocciaArrayValue then
  begin
    // For arrays, first try to parse as integer index
    try
      Index := StrToInt(PropertyName);
      // Check if index is valid (in bounds and not a hole)
      if (Index >= 0) and (Index < TGocciaArrayValue(Right).Elements.Count) then
      begin
        // For sparse arrays, also check that the element is not nil (not a hole)
        if TGocciaArrayValue(Right).Elements[Index] <> nil then
          Result := TGocciaBooleanValue.Create(True)
        else
          Result := TGocciaBooleanValue.Create(False);
      end
      else
        Result := TGocciaBooleanValue.Create(False);
    except
      // If not a valid integer, check if it's a property in the prototype chain
      // This includes 'length', array methods like 'push', 'pop', etc.
      Result := TGocciaBooleanValue.Create(TGocciaArrayValue(Right).HasProperty(PropertyName));
    end;
  end
  else if Right is TGocciaStringValue then
  begin
    // Check if index exists in string
    if PropertyName = 'length' then
      Result := TGocciaBooleanValue.Create(True)
    else
    begin
      try
        Index := StrToInt(PropertyName);
        Result := TGocciaBooleanValue.Create((Index >= 0) and (Index < Length(Right.ToString)));
      except
        // If not a valid integer, always false for strings
        Result := TGocciaBooleanValue.Create(False);
      end;
    end;
  end
  else if Right is TGocciaInstanceValue then
  begin
    // Check if property exists in class instance
    Result := TGocciaBooleanValue.Create(TGocciaInstanceValue(Right).HasProperty(PropertyName));
  end
  else if Right is TGocciaObjectValue then
  begin
    // Check if property exists in object (check after arrays and instances)
    Result := TGocciaBooleanValue.Create(TGocciaObjectValue(Right).HasProperty(PropertyName));
  end
  else
  begin
    // For other types, return false
    Result := TGocciaBooleanValue.Create(False);
  end;
end;

end.
