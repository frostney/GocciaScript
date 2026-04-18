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
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaGlobalBigInt.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  PrototypeInitializer: TGocciaBigIntValue;
begin
  inherited Create(AName, AScope, AThrowError);

  // Initialize shared BigInt prototype
  PrototypeInitializer := TGocciaBigIntValue.BigIntZero;
  PrototypeInitializer.InitializePrototype;

  // Create the BigInt function (callable, converts values to BigInt)
  FBigIntFunction := TGocciaNativeFunctionValue.Create(BigIntConstructor, 'BigInt', 1);

  // Register static methods on the BigInt function
  with TGocciaMemberCollection.Create do
  try
    AddMethod(BigIntAsIntN, 2, gmkStaticMethod);
    AddMethod(BigIntAsUintN, 2, gmkStaticMethod);
    FStaticMembers := ToDefinitions;
  finally
    Free;
  end;
  FStaticMembers[0].ExposedName := 'asIntN';
  FStaticMembers[1].ExposedName := 'asUintN';
  RegisterMemberDefinitions(FBigIntFunction, FStaticMembers);

  // Expose BigInt.prototype
  FBigIntFunction.AssignProperty(PROP_PROTOTYPE, TGocciaBigIntValue.SharedPrototype);

  // Bind BigInt in scope
  AScope.DefineLexicalBinding(AName, FBigIntFunction, dtLet);
end;

// ES2026 §21.2.1.1 BigInt(value) — conversion function
function TGocciaGlobalBigInt.BigIntConstructor(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  NumVal: Double;
begin
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
    if TGocciaNumberLiteralValue(Arg).IsNaN or
       TGocciaNumberLiteralValue(Arg).IsInfinite then
      ThrowTypeError(Format(SErrorBigIntNotInteger, [Arg.ToStringLiteral.Value]),
        SSuggestBigIntNoImplicitConversion);
    if Frac(NumVal) <> 0 then
      ThrowTypeError(Format(SErrorBigIntNotInteger, [Arg.ToStringLiteral.Value]),
        SSuggestBigIntNoImplicitConversion);
    Result := TGocciaBigIntValue.Create(TBigInteger.FromDouble(NumVal));
    Exit;
  end;

  // String → BigInt
  if Arg is TGocciaStringLiteralValue then
  begin
    try
      Result := TGocciaBigIntValue.Create(
        TBigInteger.FromDecimalString(Trim(TGocciaStringLiteralValue(Arg).Value)));
    except
      ThrowTypeError(Format(SErrorBigIntInvalidConversion, [
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

// ES2026 §21.2.2.1 BigInt.asIntN(bits, bigint)
function TGocciaGlobalBigInt.BigIntAsIntN(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Bits: Integer;
  BigIntArg: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'BigInt.asIntN', ThrowError);

  Bits := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  BigIntArg := AArgs.GetElement(1);
  if not (BigIntArg is TGocciaBigIntValue) then
    ThrowTypeError(Format(SErrorBigIntInvalidConversion, [BigIntArg.TypeName]),
      SSuggestBigIntNoImplicitConversion);

  Result := TGocciaBigIntValue.Create(
    TGocciaBigIntValue(BigIntArg).Value.AsIntN(Bits));
end;

// ES2026 §21.2.2.2 BigInt.asUintN(bits, bigint)
function TGocciaGlobalBigInt.BigIntAsUintN(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Bits: Integer;
  BigIntArg: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 2, 'BigInt.asUintN', ThrowError);

  Bits := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  BigIntArg := AArgs.GetElement(1);
  if not (BigIntArg is TGocciaBigIntValue) then
    ThrowTypeError(Format(SErrorBigIntInvalidConversion, [BigIntArg.TypeName]),
      SSuggestBigIntNoImplicitConversion);

  Result := TGocciaBigIntValue.Create(
    TGocciaBigIntValue(BigIntArg).Value.AsUintN(Bits));
end;

end.
