unit Goccia.Evaluator.TypeOperations;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TIsObjectInstanceOfClassFunction = function(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;

function EvaluateTypeof(const AOperand: TGocciaValue): TGocciaValue; inline;
function EvaluateInstanceof(const ALeft, ARight: TGocciaValue; const AIsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
function EvaluateInOperator(const ALeft, ARight: TGocciaValue): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.Keywords.Reserved,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.SymbolValue;

function EvaluateTypeof(const AOperand: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(AOperand.TypeOf);
end;

function IsPrototypeInChain(const AObj: TGocciaObjectValue; const ATargetProto: TGocciaObjectValue): Boolean;
var
  CurrentProto: TGocciaObjectValue;
begin
  Result := False;
  CurrentProto := AObj.Prototype;
  while Assigned(CurrentProto) do
  begin
    if CurrentProto = ATargetProto then
    begin
      Result := True;
      Exit;
    end;
    CurrentProto := CurrentProto.Prototype;
  end;
end;

function EvaluateInstanceof(const ALeft, ARight: TGocciaValue; const AIsObjectInstanceOfClass: TIsObjectInstanceOfClassFunction): TGocciaValue;
var
  ConstructorProto: TGocciaValue;
begin
  // ARight operand must be callable (a constructor)
  if not (ARight is TGocciaClassValue) then
  begin
    // Check for native function constructors (e.g. Error, TypeError)
    // ECMAScript: get ARight.prototype and walk ALeft's prototype chain
    if (ARight is TGocciaFunctionBase) and (ALeft is TGocciaObjectValue) then
    begin
      ConstructorProto := TGocciaFunctionBase(ARight).GetProperty('prototype');
      if (ConstructorProto is TGocciaObjectValue) then
      begin
        if IsPrototypeInChain(TGocciaObjectValue(ALeft), TGocciaObjectValue(ConstructorProto)) then
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
    // Check if ALeft is an instance of the ARight class
    if ALeft is TGocciaInstanceValue then
    begin
      // For class instances, check inheritance chain
      if TGocciaClassValue(ARight).Name = 'Object' then
      begin
        // All class instances are instances of Object
        Result := TGocciaBooleanLiteralValue.TrueValue;
      end
      else
      begin
        if TGocciaInstanceValue(ALeft).IsInstanceOf(TGocciaClassValue(ARight)) then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
      end;
    end
    else if (ALeft is TGocciaFunctionValue) and (TGocciaClassValue(ARight).Name = 'Function') then
    begin
      // Functions are instances of Function
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (ALeft is TGocciaNativeFunctionValue) and (TGocciaClassValue(ARight).Name = 'Function') then
    begin
      // Native functions are also instances of Function
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (ALeft is TGocciaClassValue) and (TGocciaClassValue(ARight).Name = 'Function') then
    begin
      // Classes are also instances of Function (since classes are constructor functions)
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (ALeft is TGocciaArrayValue) and (TGocciaClassValue(ARight).Name = 'Array') then
    begin
      // Arrays are instances of Array
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (ALeft is TGocciaArrayValue) and (TGocciaClassValue(ARight).Name = 'Object') then
    begin
      // Arrays are also instances of Object (inheritance)
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if (ALeft is TGocciaObjectValue) and (TGocciaClassValue(ARight).Name = 'Object') then
    begin
      // Objects are instances of Object
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if ALeft is TGocciaObjectValue then
    begin
      // General object instanceof check - walk the prototype chain
      if AIsObjectInstanceOfClass(TGocciaObjectValue(ALeft), TGocciaClassValue(ARight)) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

function EvaluateInOperator(const ALeft, ARight: TGocciaValue): TGocciaValue;
var
  PropertyName: string;
  Index: Integer;
begin
  // ECMAScript: right operand must be an object, not a primitive
  if ARight.IsPrimitive then
    ThrowTypeError('Cannot use ''' + KEYWORD_IN + ''' operator to search for ''' +
      ALeft.ToStringLiteral.Value + ''' in ' + ARight.ToStringLiteral.Value);

  if ALeft is TGocciaSymbolValue then
  begin
    if ARight is TGocciaObjectValue then
    begin
      if TGocciaObjectValue(ARight).HasSymbolProperty(TGocciaSymbolValue(ALeft)) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  PropertyName := ALeft.ToStringLiteral.Value;

  if ARight is TGocciaArrayValue then
  begin
    // For arrays, first try to parse as integer index
    try
      Index := StrToInt(PropertyName);
      // Check if index is valid (in bounds and not a hole)
      if (Index >= 0) and (Index < TGocciaArrayValue(ARight).Elements.Count) then
      begin
        // For sparse arrays, also check that the element is not nil (not a hole)
        if TGocciaArrayValue(ARight).Elements[Index] <> nil then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
      end
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    except
      // If not a valid integer, check if it's a property in the prototype chain
      // This includes 'length', array methods like 'push', 'pop', etc.
      if TGocciaArrayValue(ARight).HasProperty(PropertyName) then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    end;
  end
  else if ARight is TGocciaInstanceValue then
  begin
    // Check if property exists in class instance
    if TGocciaInstanceValue(ARight).HasProperty(PropertyName) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else if ARight is TGocciaObjectValue then
  begin
    // Check if property exists in object (check after arrays and instances)
    if TGocciaObjectValue(ARight).HasProperty(PropertyName) then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

end.
