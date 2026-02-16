unit Goccia.Evaluator.TypeOperations;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ArrayValue, Goccia.Values.ObjectValue,
  Goccia.Values.ClassValue, Goccia.Values.FunctionValue, Goccia.Values.NativeFunction,
  Goccia.Values.FunctionBase, SysUtils;

type
  TIsObjectInstanceOfClassFunction = function(Obj: TGocciaObjectValue; ClassValue: TGocciaClassValue): Boolean;

function EvaluateTypeof(Operand: TGocciaValue): TGocciaValue; inline;
function EvaluateInstanceof(Left, Right: TGocciaValue; IsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
function EvaluateInOperator(Left, Right: TGocciaValue): TGocciaValue;

implementation

uses Goccia.Values.ClassHelper;

function EvaluateTypeof(Operand: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(Operand.TypeOf);
end;

function IsPrototypeInChain(Obj: TGocciaObjectValue; TargetProto: TGocciaObjectValue): Boolean;
var
  CurrentProto: TGocciaObjectValue;
begin
  Result := False;
  CurrentProto := Obj.Prototype;
  while Assigned(CurrentProto) do
  begin
    if CurrentProto = TargetProto then
    begin
      Result := True;
      Exit;
    end;
    CurrentProto := CurrentProto.Prototype;
  end;
end;

function EvaluateInstanceof(Left, Right: TGocciaValue; IsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
var
  ConstructorProto: TGocciaValue;
begin
  // Right operand must be callable (a constructor)
  if not (Right is TGocciaClassValue) then
  begin
    // Check for native function constructors (e.g. Error, TypeError)
    // ECMAScript: get Right.prototype and walk Left's prototype chain
    if (Right is TGocciaFunctionBase) and (Left is TGocciaObjectValue) then
    begin
      ConstructorProto := TGocciaFunctionBase(Right).GetProperty('prototype');
      if (ConstructorProto is TGocciaObjectValue) then
      begin
        if IsPrototypeInChain(TGocciaObjectValue(Left), TGocciaObjectValue(ConstructorProto)) then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;
    end;
    Result := TGocciaBooleanLiteralValue.FalseValue;
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
        Result := TGocciaBooleanLiteralValue.TrueValue;
      end
      else
      begin
        if TGocciaInstanceValue(Left).IsInstanceOf(TGocciaClassValue(Right)) then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
      end;
    end
    else if (Left is TGocciaFunctionValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Functions are instances of Function
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (Left is TGocciaNativeFunctionValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Native functions are also instances of Function
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (Left is TGocciaClassValue) and (TGocciaClassValue(Right).Name = 'Function') then
    begin
      // Classes are also instances of Function (since classes are constructor functions)
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (Left is TGocciaArrayValue) and (TGocciaClassValue(Right).Name = 'Array') then
    begin
      // Arrays are instances of Array
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (Left is TGocciaArrayValue) and (TGocciaClassValue(Right).Name = 'Object') then
    begin
      // Arrays are also instances of Object (inheritance)
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (Left is TGocciaObjectValue) and (TGocciaClassValue(Right).Name = 'Object') then
    begin
      // Objects are instances of Object
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if Left is TGocciaObjectValue then
    begin
      // General object instanceof check - walk the prototype chain
      if IsObjectInstanceOfClass(TGocciaObjectValue(Left), TGocciaClassValue(Right)) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

function EvaluateInOperator(Left, Right: TGocciaValue): TGocciaValue;
var
  PropertyName: string;
  Index: Integer;
begin
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
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
      end
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    except
      // If not a valid integer, check if it's a property in the prototype chain
      // This includes 'length', array methods like 'push', 'pop', etc.
      if TGocciaArrayValue(Right).HasProperty(PropertyName) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
  end
  else if Right is TGocciaStringLiteralValue then
  begin
    // Check if index exists in string
    if PropertyName = 'length' then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
    begin
      try
        Index := StrToInt(PropertyName);
        if (Index >= 0) and (Index < Length(Right.ToStringLiteral.Value)) then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
      except
        // If not a valid integer, always false for strings
        Result := TGocciaBooleanLiteralValue.FalseValue;
      end;
    end;
  end
  else if Right is TGocciaInstanceValue then
  begin
    // Check if property exists in class instance
    if TGocciaInstanceValue(Right).HasProperty(PropertyName) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else if Right is TGocciaObjectValue then
  begin
    // Check if property exists in object (check after arrays and instances)
    if TGocciaObjectValue(Right).HasProperty(PropertyName) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
  begin
    // For other types, return false
    Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

end.
