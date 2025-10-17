unit Goccia.Evaluator.TypeOperations;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ArrayValue, Goccia.Values.ObjectValue,
  Goccia.Values.ClassValue, Goccia.Values.FunctionValue, Goccia.Values.NativeFunction,
  Goccia.Logger, SysUtils;

type
  TIsObjectInstanceOfClassFunction = function(Obj: TGocciaObjectValue; ClassValue: TGocciaClassValue): Boolean;

function EvaluateTypeof(Operand: TGocciaValue): TGocciaValue; inline;
function EvaluateInstanceof(Left, Right: TGocciaValue; IsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
function EvaluateInOperator(Left, Right: TGocciaValue): TGocciaValue;

implementation

uses Goccia.Values.ClassHelper;

function EvaluateTypeof(Operand: TGocciaValue): TGocciaValue;
begin
  Logger.Debug('EvaluateTypeof: typeof operator called with operand: %s', [Operand.ToStringLiteral.Value]);
  Result := TGocciaStringLiteralValue.Create(Operand.TypeOf);
end;

function EvaluateInstanceof(Left, Right: TGocciaValue; IsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
begin
  // Implement instanceof operator according to JavaScript specification
  Logger.Debug('EvaluateInstanceof: instanceof operator called with Left: %s, Right: %s', [Left.ToStringLiteral.Value, Right.ToStringLiteral.Value]);

  // Right operand must be a constructor/class
  if not (Right is TGocciaClassValue) then
  begin
    // For built-in types, we need special handling
    Result := TGocciaBooleanLiteralValue.Create(False); // Default to false for now
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
        Result := TGocciaBooleanLiteralValue.Create(True);
      end
      else
      begin
        Result := TGocciaBooleanLiteralValue.Create(TGocciaInstanceValue(Left).IsInstanceOf(TGocciaClassValue(Right)));
      end;
    end
    else if (Left is TGocciaFunctionValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Functions are instances of Function
      Result := TGocciaBooleanLiteralValue.Create(True);
    end
    else if (Left is TGocciaNativeFunctionValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Native functions are also instances of Function
      Result := TGocciaBooleanLiteralValue.Create(True);
    end
    else if (Left is TGocciaClassValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Classes are also instances of Function (since classes are constructor functions)
      Result := TGocciaBooleanLiteralValue.Create(True);
    end
    else if (Left is TGocciaArrayValue) and (TGocciaClassValue(Right).Name = 'Array') then
    begin
      // Arrays are instances of Array
      Result := TGocciaBooleanLiteralValue.Create(True);
    end
    else if (Left is TGocciaArrayValue) and (TGocciaClassValue(Right).Name = 'Object') then
    begin
      // Arrays are also instances of Object (inheritance)
      Result := TGocciaBooleanLiteralValue.Create(True);
    end
    else if (Left is TGocciaObjectValue) and (TGocciaClassValue(Right).Name = 'Object') then
    begin
      // Objects are instances of Object
      Result := TGocciaBooleanLiteralValue.Create(True);
    end
    else if Left is TGocciaObjectValue then
    begin
      // General object instanceof check - walk the prototype chain
      Result := TGocciaBooleanLiteralValue.Create(IsObjectInstanceOfClass(TGocciaObjectValue(Left), TGocciaClassValue(Right)));
    end
    else
      Result := TGocciaBooleanLiteralValue.Create(False);
  end;
end;

function EvaluateInOperator(Left, Right: TGocciaValue): TGocciaValue;
var
  PropertyName: string;
  Index: Integer;
begin
  // Implement the 'in' operator: property/index in object/array/string
  Logger.Debug('EvaluateInOperator: in operator called with Left: %s, Right: %s', [Left.ToStringLiteral.Value, Right.ToStringLiteral.Value]);

  PropertyName := Left.ToStringLiteral.Value; // Left operand is the property name

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
          Result := TGocciaBooleanLiteralValue.Create(True)
        else
          Result := TGocciaBooleanLiteralValue.Create(False);
      end
      else
        Result := TGocciaBooleanLiteralValue.Create(False);
    except
      // If not a valid integer, check if it's a property in the prototype chain
      // This includes 'length', array methods like 'push', 'pop', etc.
      Result := TGocciaBooleanLiteralValue.Create(TGocciaArrayValue(Right).HasProperty(PropertyName));
    end;
  end
  else if Right is TGocciaStringLiteralValue then
  begin
    // Check if index exists in string
    if PropertyName = 'length' then
      Result := TGocciaBooleanLiteralValue.Create(True)
    else
    begin
      try
        Index := StrToInt(PropertyName);
        Result := TGocciaBooleanLiteralValue.Create((Index >= 0) and (Index < Length(Right.ToStringLiteral.Value)));
      except
        // If not a valid integer, always false for strings
        Result := TGocciaBooleanLiteralValue.Create(False);
      end;
    end;
  end
  else if Right is TGocciaInstanceValue then
  begin
    // Check if property exists in class instance
    Result := TGocciaBooleanLiteralValue.Create(TGocciaInstanceValue(Right).HasProperty(PropertyName));
  end
  else if Right is TGocciaObjectValue then
  begin
    // Check if property exists in object (check after arrays and instances)
    Result := TGocciaBooleanLiteralValue.Create(TGocciaObjectValue(Right).HasProperty(PropertyName));
  end
  else
  begin
    // For other types, return false
    Result := TGocciaBooleanLiteralValue.Create(False);
  end;
end;

end.
