unit Goccia.Values.BigIntValue;

{$I Goccia.inc}

interface

uses
  BigInteger,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel.Types,
  Goccia.Values.Primitives;

type
  TGocciaBigIntValue = class(TGocciaValue)
  private class var
    FZeroValue: TGocciaBigIntValue;
    FOneValue: TGocciaBigIntValue;
  private
    FValue: TBigInteger;
  public
    constructor Create(const AValue: TBigInteger);

    class function BigIntZero: TGocciaBigIntValue;
    class function BigIntOne: TGocciaBigIntValue;

    procedure InitializePrototype;
    class function SharedPrototype: TGocciaValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: TBigInteger read FValue;
  published
    function BigIntToString(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BigIntValueOf(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BigIntToLocaleString(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.ObjectModel,
  Goccia.Threading,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

threadvar
  FSharedPrototype: TGocciaValue;
  FMethodHost: TGocciaBigIntValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

{ TGocciaBigIntValue }

constructor TGocciaBigIntValue.Create(const AValue: TBigInteger);
begin
  inherited Create;
  FValue := AValue;
end;

class function TGocciaBigIntValue.BigIntZero: TGocciaBigIntValue;
begin
  if not Assigned(FZeroValue) then
  begin
    Assert(not GIsWorkerThread, 'BigIntZero: must be initialised on main thread');
    FZeroValue := TGocciaBigIntValue.Create(TBigInteger.Zero);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FZeroValue);
  end;
  Result := FZeroValue;
end;

class function TGocciaBigIntValue.BigIntOne: TGocciaBigIntValue;
begin
  if not Assigned(FOneValue) then
  begin
    Assert(not GIsWorkerThread, 'BigIntOne: must be initialised on main thread');
    FOneValue := TGocciaBigIntValue.Create(TBigInteger.One);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FOneValue);
  end;
  Result := FOneValue;
end;

procedure TGocciaBigIntValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Proto: TGocciaObjectValue;
begin
  if Assigned(FSharedPrototype) then Exit;

  Proto := TGocciaObjectValue.Create;
  FSharedPrototype := Proto;
  FMethodHost := Self;

  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(BigIntToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddMethod(BigIntValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddMethod(BigIntToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
    FPrototypeMembers[0].ExposedName := PROP_TO_STRING;
    FPrototypeMembers[1].ExposedName := PROP_VALUE_OF;
    FPrototypeMembers[2].ExposedName := PROP_TO_LOCALE_STRING;
  end;

  RegisterMemberDefinitions(Proto, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedPrototype);
    TGarbageCollector.Instance.PinObject(FMethodHost);
  end;
end;

class function TGocciaBigIntValue.SharedPrototype: TGocciaValue;
begin
  Result := FSharedPrototype;
end;

function TGocciaBigIntValue.IsPrimitive: Boolean;
begin
  Result := True;
end;

function TGocciaBigIntValue.TypeName: string;
begin
  Result := BIGINT_TYPE_NAME;
end;

function TGocciaBigIntValue.TypeOf: string;
begin
  Result := BIGINT_TYPE_NAME;
end;

function TGocciaBigIntValue.GetProperty(const AName: string): TGocciaValue;
begin
  if Assigned(FSharedPrototype) then
  begin
    Result := TGocciaObjectValue(FSharedPrototype).GetPropertyWithContext(AName, Self);
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

function TGocciaBigIntValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  // ES2026 §7.1.2 ToBoolean: 0n is false, all others are true
  if FValue.IsZero then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaBigIntValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  // ES2026 §7.1.4 ToNumber: BigInt throws TypeError
  ThrowTypeError(SErrorBigIntToNumber, SSuggestBigIntNoImplicitConversion);
  Result := nil;
end;

function TGocciaBigIntValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create(FValue.ToString);
end;

{ Prototype methods }

// ES2026 §21.2.3.3 BigInt.prototype.toString([radix])
function TGocciaBigIntValue.BigIntToString(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Radix: Integer;
  RadixValue: TGocciaValue;
begin
  if not (AThisValue is TGocciaBigIntValue) then
    ThrowTypeError(Format(SErrorBigIntRequiresBigIntValue, ['BigInt.prototype.toString']),
      SSuggestBigIntRequiresBigIntValue);

  Radix := 10;
  if AArgs.Length > 0 then
  begin
    RadixValue := AArgs.GetElement(0);
    if not (RadixValue is TGocciaUndefinedLiteralValue) then
    begin
      Radix := Trunc(RadixValue.ToNumberLiteral.Value);
      if (Radix < 2) or (Radix > 36) then
        ThrowRangeError(SErrorBigIntInvalidRadix, SSuggestBigIntInvalidRadix);
    end;
  end;

  Result := TGocciaStringLiteralValue.Create(
    TGocciaBigIntValue(AThisValue).FValue.ToRadixString(Radix));
end;

// ES2026 §21.2.3.4 BigInt.prototype.valueOf()
function TGocciaBigIntValue.BigIntValueOf(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBigIntValue) then
    ThrowTypeError(Format(SErrorBigIntRequiresBigIntValue, ['BigInt.prototype.valueOf']),
      SSuggestBigIntRequiresBigIntValue);
  Result := AThisValue;
end;

// ES2026 §21.2.3.2 BigInt.prototype.toLocaleString()
function TGocciaBigIntValue.BigIntToLocaleString(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBigIntValue) then
    ThrowTypeError(Format(SErrorBigIntRequiresBigIntValue, ['BigInt.prototype.toLocaleString']),
      SSuggestBigIntRequiresBigIntValue);
  Result := TGocciaStringLiteralValue.Create(
    TGocciaBigIntValue(AThisValue).FValue.ToString);
end;

end.
