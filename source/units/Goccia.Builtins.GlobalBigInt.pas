unit Goccia.Builtins.GlobalBigInt;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaGlobalBigInt = class(TGocciaBuiltin)
  private
    FBigIntFunction: TGocciaNativeFunctionValue;
  published
    // ES2026 §21.2.1.1 BigInt(value) — conversion function (not constructor)
    function BigIntConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    // ES2026 §21.2.2.1 BigInt.asIntN(bits, bigint)
    function BigIntAsIntN(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    // ES2026 §21.2.2.2 BigInt.asUintN(bits, bigint)
    function BigIntAsUintN(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Math,
  SysUtils,

  BigInteger,

  Goccia.Arguments.Validator,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.ObjectModel.Types,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HoleValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaGlobalBigInt.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  PrototypeInitializer: TGocciaBigIntValue;
  Proto: TGocciaObjectValue;
begin
  inherited Create(AName, AScope, AThrowError);

  // Initialize shared BigInt prototype
  PrototypeInitializer := TGocciaBigIntValue.BigIntZero;
  PrototypeInitializer.InitializePrototype;

  // Create the BigInt function (callable, converts values to BigInt)
  // BigInt implements [[Construct]] but rejects new target inside the body
  FBigIntFunction := TGocciaNativeFunctionValue.Create(BigIntConstructor, 'BigInt', 1);

  // Register static methods on the BigInt function
  with TGocciaMemberCollection.Create do
  try
    AddMethod(BigIntAsIntN, 2, gmkStaticMethod, [gmfNotConstructable]);
    AddMethod(BigIntAsUintN, 2, gmkStaticMethod, [gmfNotConstructable]);
    FStaticMembers := ToDefinitions;
  finally
    Free;
  end;
  FStaticMembers[0].ExposedName := 'asIntN';
  FStaticMembers[1].ExposedName := 'asUintN';
  RegisterMemberDefinitions(FBigIntFunction, FStaticMembers);

  // Set up BigInt.prototype
  Proto := TGocciaObjectValue(TGocciaBigIntValue.SharedPrototype);

  // ES2026 §21.2.3.1 BigInt.prototype.constructor
  Proto.DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(FBigIntFunction, [pfWritable, pfConfigurable]));

  // ES2026 §21.2.3.5 BigInt.prototype[@@toStringTag]
  Proto.DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create('BigInt'), [pfConfigurable]));

  // Expose BigInt.prototype as non-writable, non-enumerable, non-configurable
  FBigIntFunction.DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(Proto, []));

  // Bind BigInt in scope
  AScope.DefineLexicalBinding(AName, FBigIntFunction, dtLet);
end;

// ES2026 §21.2.1.1 BigInt(value) — conversion function
function TGocciaGlobalBigInt.BigIntConstructor(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  NumVal: Double;
  StrVal: string;
begin
  // ES2026 §21.2.1.1 Step 1: If NewTarget is not undefined, throw TypeError
  if AThisValue is TGocciaHoleValue then
    ThrowTypeError('Cannot use ''new'' with BigInt',
      'Use BigInt(value) without ''new'' to convert to BigInt');

  if AArgs.Length = 0 then
    ThrowTypeError(Format(SErrorBigIntInvalidConversion, ['undefined']),
      SSuggestBigIntNoImplicitConversion);

  Arg := AArgs.GetElement(0);

  // Already a BigInt — return as-is
  if Arg is TGocciaBigIntValue then
  begin
    Result := Arg;
    Exit;
  end;

  // Boolean → BigInt
  if Arg is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(Arg).Value then
      Result := TGocciaBigIntValue.BigIntOne
    else
      Result := TGocciaBigIntValue.BigIntZero;
    Exit;
  end;

  // Number → BigInt (must be an integer, no NaN/Infinity)
  if Arg is TGocciaNumberLiteralValue then
  begin
    NumVal := TGocciaNumberLiteralValue(Arg).Value;
    // ES2026 §21.2.1.1.1 NumberToBigInt: throws RangeError for non-integers
    if TGocciaNumberLiteralValue(Arg).IsNaN or
       TGocciaNumberLiteralValue(Arg).IsInfinite then
      ThrowRangeError(Format(SErrorBigIntNotInteger, [Arg.ToStringLiteral.Value]),
        SSuggestBigIntNoImplicitConversion);
    if Frac(NumVal) <> 0 then
      ThrowRangeError(Format(SErrorBigIntNotInteger, [Arg.ToStringLiteral.Value]),
        SSuggestBigIntNoImplicitConversion);
    Result := TGocciaBigIntValue.Create(TBigInteger.FromDouble(NumVal));
    Exit;
  end;

  // ES2026 §7.1.14 StringToBigInt: supports decimal, hex, octal, binary
  if Arg is TGocciaStringLiteralValue then
  begin
    StrVal := Trim(TGocciaStringLiteralValue(Arg).Value);
    if StrVal = '' then
    begin
      Result := TGocciaBigIntValue.BigIntZero;
      Exit;
    end;
    try
      if (Length(StrVal) >= 3) and (StrVal[1] = '0') then
      begin
        if (StrVal[2] = 'x') or (StrVal[2] = 'X') then
        begin
          Result := TGocciaBigIntValue.Create(
            TBigInteger.FromHexString(Copy(StrVal, 3, Length(StrVal) - 2)));
          Exit;
        end
        else if (StrVal[2] = 'o') or (StrVal[2] = 'O') then
        begin
          Result := TGocciaBigIntValue.Create(
            TBigInteger.FromOctalString(Copy(StrVal, 3, Length(StrVal) - 2)));
          Exit;
        end
        else if (StrVal[2] = 'b') or (StrVal[2] = 'B') then
        begin
          Result := TGocciaBigIntValue.Create(
            TBigInteger.FromBinaryString(Copy(StrVal, 3, Length(StrVal) - 2)));
          Exit;
        end;
      end;
      Result := TGocciaBigIntValue.Create(
        TBigInteger.FromDecimalString(StrVal));
    except
      ThrowSyntaxError(Format(SErrorBigIntInvalidConversion, [
        '''' + TGocciaStringLiteralValue(Arg).Value + '''']),
        SSuggestBigIntNoImplicitConversion);
      Result := nil;
    end;
    Exit;
  end;

  // All other types — TypeError
  ThrowTypeError(Format(SErrorBigIntInvalidConversion, [Arg.TypeName]),
    SSuggestBigIntNoImplicitConversion);
  Result := nil;
end;

// ES2026 §6.2.4.2 ToIndex — coerce bits parameter for asIntN/asUintN
function BigIntToIndex(const AValue: TGocciaValue): Integer;
var
  Num: TGocciaNumberLiteralValue;
  IntegerIndex: Double;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(0);

  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    IntegerIndex := 0
  else if Num.IsInfinite then
  begin
    ThrowRangeError(SErrorBigIntInvalidIndex, SSuggestBigIntInvalidIndex);
    Exit(0);
  end
  else
    IntegerIndex := Trunc(Num.Value);

  if (IntegerIndex < 0) or (IntegerIndex > 9007199254740991.0) or
     (IntegerIndex > High(Integer)) then
    ThrowRangeError(SErrorBigIntInvalidIndex, SSuggestBigIntInvalidIndex);

  Result := Trunc(IntegerIndex);
end;

// ES2026 §7.1.13 ToBigInt(argument) — forward declaration
function ToBigIntValue(const AValue: TGocciaValue): TGocciaBigIntValue; forward;

// ES2026 §7.1.13 ToBigInt — apply ToPrimitive then convert primitive to BigInt
function ToBigIntValue(const AValue: TGocciaValue): TGocciaBigIntValue;
var
  Prim: TGocciaValue;
  StrVal: string;
begin
  // Step 1: If argument is an object, apply ToPrimitive(argument, number)
  if (AValue is TGocciaObjectValue) and not (AValue is TGocciaBigIntValue) then
  begin
    Prim := ToPrimitive(AValue, tphNumber);
    Exit(ToBigIntValue(Prim));
  end;

  if AValue is TGocciaBigIntValue then
    Exit(TGocciaBigIntValue(AValue));

  if AValue is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(AValue).Value then
      Result := TGocciaBigIntValue.BigIntOne
    else
      Result := TGocciaBigIntValue.BigIntZero;
    Exit;
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    StrVal := Trim(TGocciaStringLiteralValue(AValue).Value);
    if StrVal = '' then
      Exit(TGocciaBigIntValue.BigIntZero);
    try
      if (Length(StrVal) >= 3) and (StrVal[1] = '0') then
      begin
        if (StrVal[2] = 'x') or (StrVal[2] = 'X') then
          Exit(TGocciaBigIntValue.Create(
            TBigInteger.FromHexString(Copy(StrVal, 3, Length(StrVal) - 2))))
        else if (StrVal[2] = 'o') or (StrVal[2] = 'O') then
          Exit(TGocciaBigIntValue.Create(
            TBigInteger.FromOctalString(Copy(StrVal, 3, Length(StrVal) - 2))))
        else if (StrVal[2] = 'b') or (StrVal[2] = 'B') then
          Exit(TGocciaBigIntValue.Create(
            TBigInteger.FromBinaryString(Copy(StrVal, 3, Length(StrVal) - 2))));
      end;
      Result := TGocciaBigIntValue.Create(TBigInteger.FromDecimalString(StrVal));
    except
      ThrowSyntaxError(Format(SErrorBigIntInvalidConversion, [
        '''' + TGocciaStringLiteralValue(AValue).Value + '''']),
        SSuggestBigIntNoImplicitConversion);
      Result := nil;
    end;
    Exit;
  end;

  // Number, undefined, null, symbol, object — all throw TypeError
  ThrowTypeError(Format(SErrorBigIntInvalidConversion, [AValue.TypeName]),
    SSuggestBigIntNoImplicitConversion);
  Result := nil;
end;

// ES2026 §21.2.2.1 BigInt.asIntN(bits, bigint)
function TGocciaGlobalBigInt.BigIntAsIntN(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Bits: Integer;
  BigIntVal: TGocciaBigIntValue;
begin
  Bits := BigIntToIndex(AArgs.GetElement(0));
  BigIntVal := ToBigIntValue(AArgs.GetElement(1));

  Result := TGocciaBigIntValue.Create(BigIntVal.Value.AsIntN(Bits));
end;

// ES2026 §21.2.2.2 BigInt.asUintN(bits, bigint)
function TGocciaGlobalBigInt.BigIntAsUintN(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Bits: Integer;
  BigIntVal: TGocciaBigIntValue;
begin
  Bits := BigIntToIndex(AArgs.GetElement(0));
  BigIntVal := ToBigIntValue(AArgs.GetElement(1));

  Result := TGocciaBigIntValue.Create(BigIntVal.Value.AsUintN(Bits));
end;

end.
