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
  Goccia.Values.BigIntObjectValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.StringObjectValue;

procedure EnsureAssignableReceiver(const AObj: TGocciaValue; const APropertyName: string);
begin
  if AObj is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, [APropertyName]),
      SSuggestCheckNullBeforeAccess)
  else if AObj is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfUndefined, [APropertyName]),
      SSuggestCheckNullBeforeAccess);
end;

function BoxAssignablePrimitive(const AObj: TGocciaValue): TGocciaObjectValue;
begin
  if AObj is TGocciaBooleanLiteralValue then
    Exit(TGocciaBooleanObjectValue.Create(TGocciaBooleanLiteralValue(AObj)));
  if AObj is TGocciaNumberLiteralValue then
    Exit(TGocciaNumberObjectValue.Create(TGocciaNumberLiteralValue(AObj)));
  if AObj is TGocciaStringLiteralValue then
    Exit(TGocciaStringObjectValue.Create(TGocciaStringLiteralValue(AObj)));
  if AObj is TGocciaBigIntValue then
    Exit(TGocciaBigIntObjectValue.Create(AObj));
  Result := nil;
end;

procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  BoxedValue: TGocciaObjectValue;
begin
  EnsureAssignableReceiver(AObj, APropertyName);
  if (AObj is TGocciaObjectValue) or (AObj is TGocciaClassValue) then
  begin
    AObj.SetProperty(APropertyName, AValue);
    Exit;
  end;

  if (AObj is TGocciaSymbolValue) and
     (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
  begin
    BoxedValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype);
    if not BoxedValue.AssignPropertyWithReceiver(APropertyName, AValue,
      AObj) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    Exit;
  end;

  BoxedValue := BoxAssignablePrimitive(AObj);
  if Assigned(BoxedValue) then
  begin
    if not BoxedValue.AssignPropertyWithReceiver(APropertyName, AValue,
      AObj) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    Exit;
  end;

  if Assigned(AOnError) then
    // AOnError is not invoked here — ThrowTypeError must be used because this is
    // a JavaScript-level TypeError (TGocciaThrowValue), not an interpreter-level
    // runtime error (TGocciaRuntimeError) which is what AOnError produces.
    // The Assigned check guards against raising in contexts without error handling.
    ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
      SSuggestCheckNullBeforeAccess);
end;

procedure AssignSymbolProperty(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  BoxedValue: TGocciaObjectValue;
begin
  EnsureAssignableReceiver(AObj, ASymbol.ToDisplayString.Value);
  if AObj is TGocciaClassValue then
  begin
    TGocciaClassValue(AObj).AssignSymbolProperty(ASymbol, AValue);
    Exit;
  end;

  if AObj is TGocciaObjectValue then
  begin
    if not TGocciaObjectValue(AObj).AssignSymbolPropertyWithReceiver(ASymbol,
      AValue, AObj) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    Exit;
  end;

  if (AObj is TGocciaSymbolValue) and
     (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
  begin
    BoxedValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype);
    if not BoxedValue.AssignSymbolPropertyWithReceiver(ASymbol, AValue,
      AObj) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    Exit;
  end;

  BoxedValue := BoxAssignablePrimitive(AObj);
  if Assigned(BoxedValue) then
  begin
    if not BoxedValue.AssignSymbolPropertyWithReceiver(ASymbol, AValue,
      AObj) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    Exit;
  end;

  if Assigned(AOnError) then
    ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
      SSuggestCheckNullBeforeAccess);
end;

procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
  BoxedValue: TGocciaObjectValue;
begin
  EnsureAssignableReceiver(AObj, APropertyName);
  CurrentValue := AObj.GetProperty(APropertyName);
  if CurrentValue = nil then
  begin
    BoxedValue := BoxAssignablePrimitive(AObj);
    if Assigned(BoxedValue) then
    begin
      CurrentValue := BoxedValue.GetPropertyWithContext(APropertyName, AObj);
      if CurrentValue = nil then
        CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else if (AObj is TGocciaSymbolValue) and
            (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
    begin
      CurrentValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype)
        .GetPropertyWithContext(APropertyName, AObj);
      if CurrentValue = nil then
        CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
    begin
      if Assigned(AOnError) then
        AOnError('Cannot access property on non-object', ALine, AColumn);
      Exit;
    end;
  end;

  NewValue := Goccia.Arithmetic.CompoundOperations(
    CurrentValue, AValue, AOperator);
  AssignProperty(AObj, APropertyName, NewValue, AOnError, ALine, AColumn);
end;

procedure PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
  BoxedValue: TGocciaObjectValue;
begin
  EnsureAssignableReceiver(AObj, ASymbol.ToDisplayString.Value);
  if AObj is TGocciaClassValue then
    CurrentValue := TGocciaClassValue(AObj).GetSymbolProperty(ASymbol)
  else if AObj is TGocciaObjectValue then
    CurrentValue := TGocciaObjectValue(AObj).GetSymbolProperty(ASymbol)
  else if (AObj is TGocciaSymbolValue) and
          (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
    CurrentValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype)
      .GetSymbolPropertyWithReceiver(ASymbol, AObj)
  else
  begin
    CurrentValue := nil;
    BoxedValue := BoxAssignablePrimitive(AObj);
    if Assigned(BoxedValue) then
      CurrentValue := BoxedValue.GetSymbolPropertyWithReceiver(ASymbol, AObj)
    else if Assigned(AOnError) then
    begin
      AOnError('Cannot access property on non-object', ALine, AColumn);
      Exit;
    end;
  end;
  if CurrentValue = nil then
    CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  NewValue := Goccia.Arithmetic.CompoundOperations(
    CurrentValue, AValue, AOperator);
  AssignSymbolProperty(AObj, ASymbol, NewValue, AOnError, ALine, AColumn);
end;

function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue;
begin
  if AIsIncrement then
    Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value + 1)
  else
    Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value - 1);
end;

end.
