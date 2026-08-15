unit Goccia.Evaluator.Assignment;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Token,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

// Property assignment with error handling for non-objects
procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False);
procedure AssignSymbolProperty(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False);

// Compound assignment operations
// ES2026 §13.15.2 step 3 performs GetValue(lref) once, before the right-hand
// side is evaluated, so the read is the caller's — it owns the evaluation order
// and the short-circuit operators consume the same read. The value it obtained
// is passed back in as ACurrentValue; reading the property again here would call
// an accessor's getter twice per compound assignment.
function ReadPropertyCompoundAssignmentValue(const AObj: TGocciaValue; const APropertyName: string; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; out ACurrentValue: TGocciaValue): Boolean;
function PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const ACurrentValue, AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False): TGocciaValue;
function PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const ACurrentValue, AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False): TGocciaValue;

// Increment/Decrement operations
function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Arithmetic,
  Goccia.Constants.PropertyNames,
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

function IsImmutableProjectGlobalProperty(const AObj: TGocciaObjectValue;
  const APropertyName: string): Boolean;
var
  GlobalThisValue: TGocciaValue;
begin
  if (APropertyName <> PROP_GOCCIA) and (APropertyName <> PROP_GLOBAL_THIS) then
    Exit(False);
  if not AObj.HasOwnProperty(PROP_GLOBAL_THIS) then
    Exit(False);

  GlobalThisValue := AObj.GetProperty(PROP_GLOBAL_THIS);
  Result := GlobalThisValue = AObj;
end;

procedure AssignProperty(const AObj: TGocciaValue; const APropertyName: string; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False);
var
  BoxedValue: TGocciaObjectValue;
begin
  EnsureAssignableReceiver(AObj, APropertyName);
  if AObj is TGocciaClassValue then
  begin
    if ANonStrictMode then
      TGocciaClassValue(AObj).AssignPropertyWithReceiver(APropertyName, AValue, AObj)
    else
      TGocciaClassValue(AObj).SetProperty(APropertyName, AValue);
    Exit;
  end;

  if AObj is TGocciaObjectValue then
  begin
    if IsImmutableProjectGlobalProperty(TGocciaObjectValue(AObj),
       APropertyName) then
      ThrowTypeError(Format(SErrorAssignToConstant, [APropertyName]),
        SSuggestUseLetNotConst);
    if ANonStrictMode then
      TGocciaObjectValue(AObj).AssignPropertyWithReceiver(APropertyName, AValue, AObj)
    else
      AObj.SetProperty(APropertyName, AValue);
    Exit;
  end;

  if (AObj is TGocciaSymbolValue) and
     (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
  begin
    BoxedValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype);
    if not BoxedValue.AssignPropertyWithReceiver(APropertyName, AValue,
      AObj) then
    begin
      if ANonStrictMode then
        Exit;
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    end;
    Exit;
  end;

  BoxedValue := BoxAssignablePrimitive(AObj);
  if Assigned(BoxedValue) then
  begin
    if not BoxedValue.AssignPropertyWithReceiver(APropertyName, AValue,
      AObj) then
    begin
      if ANonStrictMode then
        Exit;
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    end;
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

procedure AssignSymbolProperty(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False);
var
  BoxedValue: TGocciaObjectValue;
begin
  EnsureAssignableReceiver(AObj, ASymbol.ToDisplayString.Value);
  if AObj is TGocciaClassValue then
  begin
    if ANonStrictMode then
      TGocciaClassValue(AObj).AssignSymbolPropertyWithReceiver(ASymbol,
        AValue, AObj)
    else
      TGocciaClassValue(AObj).AssignSymbolProperty(ASymbol, AValue);
    Exit;
  end;

  if AObj is TGocciaObjectValue then
  begin
    if not TGocciaObjectValue(AObj).AssignSymbolPropertyWithReceiver(ASymbol,
      AValue, AObj) then
    begin
      if ANonStrictMode then
        Exit;
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    end;
    Exit;
  end;

  if (AObj is TGocciaSymbolValue) and
     (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
  begin
    BoxedValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype);
    if not BoxedValue.AssignSymbolPropertyWithReceiver(ASymbol, AValue,
      AObj) then
    begin
      if ANonStrictMode then
        Exit;
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    end;
    Exit;
  end;

  BoxedValue := BoxAssignablePrimitive(AObj);
  if Assigned(BoxedValue) then
  begin
    if not BoxedValue.AssignSymbolPropertyWithReceiver(ASymbol, AValue,
      AObj) then
    begin
      if ANonStrictMode then
        Exit;
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    end;
    Exit;
  end;

  if Assigned(AOnError) then
    ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
      SSuggestCheckNullBeforeAccess);
end;

// ES2026 §13.15.2 step 3 → §6.2.5.5 GetValue step 3: a primitive base is read
// through its wrapper object, so the fallbacks below mirror the boxing that
// GetValue performs. Returns False when the base cannot be read at all — the
// caller then has no current value to compute with.
function ReadPropertyCompoundAssignmentValue(const AObj: TGocciaValue; const APropertyName: string; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; out ACurrentValue: TGocciaValue): Boolean;
var
  BoxedValue: TGocciaObjectValue;
begin
  { This is the read step, so a nullish base fails as a read — the store's
    set-flavored message would misattribute the failure (and diverge from the
    VM, which reports the read). }
  if AObj is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull, [APropertyName]),
      SSuggestCheckNullBeforeAccess)
  else if AObj is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined,
      [APropertyName]), SSuggestCheckNullBeforeAccess);
  ACurrentValue := AObj.GetProperty(APropertyName);
  if ACurrentValue = nil then
  begin
    BoxedValue := BoxAssignablePrimitive(AObj);
    if Assigned(BoxedValue) then
      ACurrentValue := BoxedValue.GetPropertyWithContext(APropertyName, AObj)
    else if (AObj is TGocciaSymbolValue) and
            (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
      ACurrentValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype)
        .GetPropertyWithContext(APropertyName, AObj)
    else
    begin
      if Assigned(AOnError) then
        AOnError('Cannot access property on non-object', ALine, AColumn);
      ACurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit(False);
    end;
  end;

  if ACurrentValue = nil then
    ACurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  Result := True;
end;

// ES2026 §13.15.2 steps 5-7: ApplyStringOrNumericBinaryOperator on the value
// step 3 already read, then PutValue.
function PerformPropertyCompoundAssignment(const AObj: TGocciaValue; const APropertyName: string; const ACurrentValue, AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False): TGocciaValue;
begin
  Result := Goccia.Arithmetic.CompoundOperations(
    ACurrentValue, AValue, AOperator);
  AssignProperty(AObj, APropertyName, Result, AOnError, ALine, AColumn,
    ANonStrictMode);
end;

// ES2026 §13.15.2 steps 5-7 for a symbol-keyed target.
function PerformSymbolPropertyCompoundAssignment(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue; const ACurrentValue, AValue: TGocciaValue; const AOperator: TGocciaTokenType; const AOnError: TGocciaThrowErrorCallback; const ALine, AColumn: Integer; const ANonStrictMode: Boolean = False): TGocciaValue;
begin
  Result := Goccia.Arithmetic.CompoundOperations(
    ACurrentValue, AValue, AOperator);
  AssignSymbolProperty(AObj, ASymbol, Result, AOnError, ALine, AColumn,
    ANonStrictMode);
end;

function PerformIncrement(const AOldValue: TGocciaValue; const AIsIncrement: Boolean): TGocciaValue;
begin
  if AOldValue is TGocciaBigIntValue then
  begin
    if AIsIncrement then
      Result := TGocciaBigIntValue.Create(
        TGocciaBigIntValue(AOldValue).Value.Add(TBigInteger.One))
    else
      Result := TGocciaBigIntValue.Create(
        TGocciaBigIntValue(AOldValue).Value.Subtract(TBigInteger.One));
  end
  else
  begin
    if AIsIncrement then
      Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value + 1)
    else
      Result := TGocciaNumberLiteralValue.Create(AOldValue.ToNumberLiteral.Value - 1);
  end;
end;

end.
