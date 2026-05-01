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

// Compound assignment operations
procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
procedure PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);

// Increment/Decrement operations
function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue; inline;

implementation

uses
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator.Arithmetic,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  BoxedValue: TGocciaObjectValue;
begin
  if (AObj is TGocciaObjectValue) or (AObj is TGocciaClassValue) then
    AObj.SetProperty(APropertyName, AValue)
  else
  begin
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

    BoxedValue := AObj.Box;
    if Assigned(BoxedValue) then
    begin
      if not BoxedValue.AssignPropertyWithReceiver(APropertyName, AValue,
        AObj) then
        ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
          SSuggestCheckNullBeforeAccess);
      Exit;
    end
    else if Assigned(AOnError) then
      // AOnError is not invoked here — ThrowTypeError must be used because this is
      // a JavaScript-level TypeError (TGocciaThrowValue), not an interpreter-level
      // runtime error (TGocciaRuntimeError) which is what AOnError produces.
      // The Assigned check guards against raising in contexts without error handling.
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
  end;
end;

procedure AssignSymbolProperty(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  BoxedValue: TGocciaObjectValue;
begin
  if AObj is TGocciaClassValue then
    TGocciaClassValue(AObj).AssignSymbolProperty(ASymbol, AValue)
  else if AObj is TGocciaObjectValue then
  begin
    if not TGocciaObjectValue(AObj).AssignSymbolPropertyWithReceiver(ASymbol,
      AValue, AObj) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
  end
  else
  begin
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

    BoxedValue := AObj.Box;
    if Assigned(BoxedValue) then
    begin
      if not BoxedValue.AssignSymbolPropertyWithReceiver(ASymbol, AValue,
        AObj) then
        ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
          SSuggestCheckNullBeforeAccess);
      Exit;
    end
    else if Assigned(AOnError) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
  end;
end;

procedure PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
  BoxedValue: TGocciaObjectValue;
begin
  CurrentValue := AObj.GetProperty(APropertyName);
  if CurrentValue = nil then
  begin
    BoxedValue := AObj.Box;
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

  NewValue := PerformCompoundOperation(CurrentValue, AValue, AOperator);
  AssignProperty(AObj, APropertyName, NewValue, AOnError, ALine, AColumn);
end;

procedure PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer);
var
  CurrentValue, NewValue: TGocciaValue;
  BoxedValue: TGocciaObjectValue;
begin
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
    BoxedValue := AObj.Box;
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
  NewValue := PerformCompoundOperation(CurrentValue, AValue, AOperator);
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
