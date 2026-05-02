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

// ES2026 §7.1.14 StringToBigInt(argument)
function TryStringToBigInt(const AValue: string; out AResult: TBigInteger): Boolean;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.Threading,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

function TryStringToBigInt(const AValue: string; out AResult: TBigInteger): Boolean;
var
  Text: string;
begin
  Text := TrimECMAScriptWhitespace(AValue);
  try
    if Text = '' then
    begin
      AResult := TBigInteger.Zero;
      Exit(True);
    end;

    if (Length(Text) > 2) and (Text[1] = '0') then
    begin
      case Text[2] of
        'x', 'X':
          begin
            AResult := TBigInteger.FromHexString(Copy(Text, 3, Length(Text) - 2));
            Exit(True);
          end;
        'o', 'O':
          begin
            AResult := TBigInteger.FromOctalString(Copy(Text, 3, Length(Text) - 2));
            Exit(True);
          end;
        'b', 'B':
          begin
            AResult := TBigInteger.FromBinaryString(Copy(Text, 3, Length(Text) - 2));
            Exit(True);
          end;
      end;
    end;

    AResult := TBigInteger.FromDecimalString(Text);
    Result := True;
  except
    Result := False;
  end;
end;

// BigInt.prototype (the JS-visible prototype, shared by primitive 1n and the
// rare object wrapper) lives in a per-realm slot so JS-side mutations don't
// leak across realms.  Method host and member definitions are immutable
// process-wide.
var
  GBigIntPrimitivePrototypeSlot: TGocciaRealmSlotId;

threadvar
  FMethodHost: TGocciaBigIntValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetSharedBigIntPrimitivePrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GBigIntPrimitivePrototypeSlot))
  else
    Result := nil;
end;

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
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedBigIntPrimitivePrototype) then Exit;

  Proto := TGocciaObjectValue.Create;
  CurrentRealm.SetSlot(GBigIntPrimitivePrototypeSlot, Proto);

  if Length(FPrototypeMembers) = 0 then
  begin
    // First realm to initialize wins: pin Self as the singleton method host
    // for the lifetime of the process.  Method callbacks captured into
    // FPrototypeMembers bind to this host, so subsequent realms must reuse it
    // (otherwise the cached callbacks would reference a freed instance).
    FMethodHost := Self;
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FMethodHost);

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
end;

class function TGocciaBigIntValue.SharedPrototype: TGocciaValue;
begin
  Result := GetSharedBigIntPrimitivePrototype;
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
var
  Proto: TGocciaObjectValue;
begin
  Proto := GetSharedBigIntPrimitivePrototype;
  if Assigned(Proto) then
  begin
    Result := Proto.GetPropertyWithContext(AName, Self);
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

initialization
  GBigIntPrimitivePrototypeSlot := RegisterRealmSlot('BigInt.prototype');

end.
