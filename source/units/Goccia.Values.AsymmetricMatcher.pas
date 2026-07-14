unit Goccia.Values.AsymmetricMatcher;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaValueEqualityCallback = function(
    const AActual, AExpected: TGocciaValue): Boolean;

  TGocciaAsymmetricMatcherValue = class(TGocciaObjectValue)
  private
    FSample: TGocciaValue;
    FInverse: Boolean;
  protected
    function ApplyInverse(const AResult: Boolean): Boolean; inline;
    procedure SetSample(const AValue: TGocciaValue); inline;
  public
    constructor Create(const ASample: TGocciaValue;
      const AInverse: Boolean = False);
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; virtual; abstract;
    function AsymmetricDisplay: string; virtual; abstract;
    function IsEquivalentTo(const AOther: TGocciaAsymmetricMatcherValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; virtual;
    function MatcherName: string; virtual; abstract;
    procedure MarkReferences; override;
    property Inverse: Boolean read FInverse;
    property Sample: TGocciaValue read FSample;
  end;

  TGocciaAnythingMatcherValue = class(TGocciaAsymmetricMatcherValue)
  public
    constructor Create;
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function MatcherName: string; override;
  end;

  TGocciaAnyMatcherValue = class(TGocciaAsymmetricMatcherValue)
  private
    function ConstructorName: string;
  public
    constructor Create(const AConstructor: TGocciaValue);
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function IsEquivalentTo(const AOther: TGocciaAsymmetricMatcherValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function MatcherName: string; override;
  end;

  TGocciaArrayContainingMatcherValue = class(TGocciaAsymmetricMatcherValue)
  public
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function MatcherName: string; override;
  end;

  TGocciaObjectContainingMatcherValue = class(TGocciaAsymmetricMatcherValue)
  public
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function MatcherName: string; override;
  end;

  TGocciaStringContainingMatcherValue = class(TGocciaAsymmetricMatcherValue)
  public
    constructor Create(const ASample: TGocciaValue;
      const AInverse: Boolean = False);
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function MatcherName: string; override;
  end;

  TGocciaStringMatchingMatcherValue = class(TGocciaAsymmetricMatcherValue)
  public
    constructor Create(const ASample: TGocciaValue;
      const AInverse: Boolean = False);
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function IsEquivalentTo(const AOther: TGocciaAsymmetricMatcherValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function MatcherName: string; override;
  end;

  TGocciaCloseToMatcherValue = class(TGocciaAsymmetricMatcherValue)
  private
    FPrecision: Double;
  public
    constructor Create(const ASample, APrecision: TGocciaValue;
      const AInverse: Boolean = False);
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function IsEquivalentTo(const AOther: TGocciaAsymmetricMatcherValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function MatcherName: string; override;
    property Precision: Double read FPrecision;
  end;

  TGocciaSchemaMatchingMatcherValue = class(TGocciaAsymmetricMatcherValue)
  private
    FStandard: TGocciaObjectValue;
    FValidationResult: TGocciaValue;
  public
    constructor Create(const ASample: TGocciaValue;
      const AInverse: Boolean = False);
    function AsymmetricMatch(const AOther: TGocciaValue;
      const AEquality: TGocciaValueEqualityCallback): Boolean; override;
    function AsymmetricDisplay: string; override;
    function MatcherName: string; override;
    procedure MarkReferences; override;
    property ValidationResult: TGocciaValue read FValidationResult;
  end;

implementation

uses
  Generics.Collections,
  Math,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.RegExp.Runtime,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntObjectValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

const
  DEFAULT_CLOSE_TO_PRECISION = 2;

function NumberIsNegativeZero(const AValue: Double): Boolean; inline;
var
  Value: Double;
  Bits: Int64 absolute Value;
begin
  Value := AValue;
  Result := (Value = 0) and (Bits < 0);
end;

function TryGetStringValue(const AValue: TGocciaValue;
  out AText: string): Boolean;
begin
  if AValue is TGocciaStringLiteralValue then
  begin
    AText := TGocciaStringLiteralValue(AValue).Value;
    Exit(True);
  end;
  if AValue is TGocciaStringObjectValue then
  begin
    AText := TGocciaStringObjectValue(AValue).Primitive.Value;
    Exit(True);
  end;
  AText := '';
  Result := False;
end;

function TryGetNumberValue(const AValue: TGocciaValue;
  out ANumber: Double): Boolean;
begin
  if AValue is TGocciaNumberLiteralValue then
  begin
    ANumber := TGocciaNumberLiteralValue(AValue).Value;
    Exit(True);
  end;
  if AValue is TGocciaNumberObjectValue then
  begin
    ANumber := TGocciaNumberObjectValue(AValue).Primitive.Value;
    Exit(True);
  end;
  ANumber := 0;
  Result := False;
end;

function HasSymbolPropertyInChain(const AObject: TGocciaObjectValue;
  const ASymbol: TGocciaSymbolValue): Boolean;
var
  Current: TGocciaObjectValue;
begin
  Current := AObject;
  while Assigned(Current) do
  begin
    if Current.HasSymbolProperty(ASymbol) then
      Exit(True);
    Current := Current.Prototype;
  end;
  Result := False;
end;

constructor TGocciaAsymmetricMatcherValue.Create(const ASample: TGocciaValue;
  const AInverse: Boolean);
begin
  inherited Create;
  FSample := ASample;
  FInverse := AInverse;
end;

function TGocciaAsymmetricMatcherValue.ApplyInverse(
  const AResult: Boolean): Boolean;
begin
  Result := AResult xor FInverse;
end;

procedure TGocciaAsymmetricMatcherValue.SetSample(
  const AValue: TGocciaValue);
begin
  FSample := AValue;
end;

procedure TGocciaAsymmetricMatcherValue.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FSample) then
    FSample.MarkReferences;
end;

function TGocciaAsymmetricMatcherValue.IsEquivalentTo(
  const AOther: TGocciaAsymmetricMatcherValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
begin
  if not Assigned(AOther) or (ClassType <> AOther.ClassType) or
     (Inverse <> AOther.Inverse) then
    Exit(False);
  if not Assigned(Sample) or not Assigned(AOther.Sample) then
    Exit(Sample = AOther.Sample);
  Result := AEquality(Sample, AOther.Sample);
end;

constructor TGocciaAnythingMatcherValue.Create;
begin
  inherited Create(nil);
end;

function TGocciaAnythingMatcherValue.AsymmetricMatch(
  const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
begin
  Result := not (AOther is TGocciaNullLiteralValue) and
    not (AOther is TGocciaUndefinedLiteralValue);
end;

function TGocciaAnythingMatcherValue.AsymmetricDisplay: string;
begin
  Result := MatcherName;
end;

function TGocciaAnythingMatcherValue.MatcherName: string;
begin
  Result := 'Anything';
end;

constructor TGocciaAnyMatcherValue.Create(const AConstructor: TGocciaValue);
begin
  inherited Create(AConstructor);
  if AConstructor is TGocciaUndefinedLiteralValue then
    ThrowTypeError(
      'any() expects to be passed a constructor function. ' +
      'Please pass one or use anything() to match any object.');
end;

function TGocciaAnyMatcherValue.ConstructorName: string;
var
  NameValue: TGocciaValue;
begin
  if Sample is TGocciaClassValue then
    Exit(TGocciaClassValue(Sample).Name);
  if Sample is TGocciaObjectValue then
  begin
    NameValue := TGocciaObjectValue(Sample).GetProperty(PROP_NAME);
    if NameValue is TGocciaStringLiteralValue then
      Exit(TGocciaStringLiteralValue(NameValue).Value);
  end;
  Result := '<anonymous>';
end;

function TGocciaAnyMatcherValue.AsymmetricMatch(const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  NativeName: string;
begin
  if Sample is TGocciaStringClassValue then
    Exit((AOther is TGocciaStringLiteralValue) or
      (AOther is TGocciaStringObjectValue));
  if Sample is TGocciaNumberClassValue then
    Exit((AOther is TGocciaNumberLiteralValue) or
      (AOther is TGocciaNumberObjectValue));
  if Sample is TGocciaFunctionConstructorClassValue then
    Exit(AOther.IsCallable);
  if Sample is TGocciaBooleanClassValue then
    Exit((AOther is TGocciaBooleanLiteralValue) or
      (AOther is TGocciaBooleanObjectValue));
  if (Sample is TGocciaClassValue) and
     (TGocciaClassValue(Sample).Name = 'Object') and
     (TGocciaClassValue(Sample).Prototype =
       TGocciaObjectValue.SharedObjectPrototype) then
    Exit(AOther.TypeOf = 'object');

  if Sample is TGocciaNativeFunctionValue then
  begin
    NativeName := TGocciaNativeFunctionValue(Sample).Name;
    if NativeName = 'String' then
      Exit((AOther is TGocciaStringLiteralValue) or
        (AOther is TGocciaStringObjectValue));
    if NativeName = 'Number' then
      Exit((AOther is TGocciaNumberLiteralValue) or
        (AOther is TGocciaNumberObjectValue));
    if NativeName = 'Function' then
      Exit(AOther.IsCallable);
    if NativeName = 'Boolean' then
      Exit((AOther is TGocciaBooleanLiteralValue) or
        (AOther is TGocciaBooleanObjectValue));
    if NativeName = 'BigInt' then
      Exit((AOther is TGocciaBigIntValue) or
        (AOther is TGocciaBigIntObjectValue));
    if NativeName = 'Symbol' then
      Exit((AOther is TGocciaSymbolValue) or
        (AOther is TGocciaSymbolObjectValue));
    if NativeName = 'Object' then
      Exit(AOther.TypeOf = 'object');
  end;

  Result := InstanceofOperatorResult(AOther, Sample);
end;

function TGocciaAnyMatcherValue.AsymmetricDisplay: string;
begin
  Result := 'Any<' + ConstructorName + '>';
end;

function TGocciaAnyMatcherValue.IsEquivalentTo(
  const AOther: TGocciaAsymmetricMatcherValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
begin
  Result := Assigned(AOther) and (ClassType = AOther.ClassType) and
    (Sample = AOther.Sample);
end;

function TGocciaAnyMatcherValue.MatcherName: string;
begin
  Result := 'Any';
end;

function TGocciaArrayContainingMatcherValue.AsymmetricMatch(
  const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  ActualArray, ExpectedArray: TGocciaArrayValue;
  ExpectedIndex, ActualIndex: Integer;
  Found: Boolean;
begin
  if not (Sample is TGocciaArrayValue) then
    ThrowTypeError('You must provide an array to ' + MatcherName +
      ', not ''' + Sample.TypeOf + '''.');

  ExpectedArray := TGocciaArrayValue(Sample);
  if ExpectedArray.Elements.Count = 0 then
    Exit(ApplyInverse(True));
  if not (AOther is TGocciaArrayValue) then
    Exit(ApplyInverse(False));

  ActualArray := TGocciaArrayValue(AOther);
  for ExpectedIndex := 0 to ExpectedArray.Elements.Count - 1 do
  begin
    if ExpectedArray.Elements[ExpectedIndex] is TGocciaHoleValue then
      Continue;
    Found := False;
    for ActualIndex := 0 to ActualArray.Elements.Count - 1 do
      if AEquality(ActualArray.Elements[ActualIndex],
        ExpectedArray.Elements[ExpectedIndex]) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      Exit(ApplyInverse(False));
  end;
  Result := ApplyInverse(True);
end;

function TGocciaArrayContainingMatcherValue.AsymmetricDisplay: string;
begin
  Result := MatcherName;
end;

function TGocciaArrayContainingMatcherValue.MatcherName: string;
begin
  if Inverse then
    Result := 'ArrayNotContaining'
  else
    Result := 'ArrayContaining';
end;

function TGocciaObjectContainingMatcherValue.AsymmetricMatch(
  const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  ActualObject, ExpectedObject: TGocciaObjectValue;
  ExpectedNames: TArray<string>;
  ExpectedSymbols: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
  I: Integer;
  Name: string;
  ActualRoot: TGocciaTempRoot;
begin
  if not (Sample is TGocciaObjectValue) or (Sample.TypeOf <> 'object') then
    ThrowTypeError('You must provide an object to ' + MatcherName +
      ', not ''' + Sample.TypeOf + '''.');
  ExpectedObject := TGocciaObjectValue(Sample);
  ExpectedNames := ExpectedObject.GetEnumerablePropertyNames;
  ExpectedSymbols := ExpectedObject.GetEnumerableSymbolProperties;
  if (Length(ExpectedNames) = 0) and (Length(ExpectedSymbols) = 0) then
    Exit(ApplyInverse(True));
  if not AOther.ToBooleanLiteral.Value then
    Exit(ApplyInverse(False));

  InitializeTempRoot(ActualRoot);
  try
    ActualObject := ToObject(AOther);
    AddTempRootIfNeeded(ActualRoot, ActualObject);
    for I := 0 to High(ExpectedNames) do
    begin
      Name := ExpectedNames[I];
      if not ActualObject.HasProperty(Name) or
         not AEquality(ActualObject.GetProperty(Name),
           ExpectedObject.GetProperty(Name)) then
        Exit(ApplyInverse(False));
    end;

    for I := 0 to High(ExpectedSymbols) do
      if not HasSymbolPropertyInChain(ActualObject,
        ExpectedSymbols[I].Key) or
         not AEquality(ActualObject.GetSymbolProperty(ExpectedSymbols[I].Key),
           ExpectedSymbols[I].Value) then
        Exit(ApplyInverse(False));

    Result := ApplyInverse(True);
  finally
    RemoveTempRootIfNeeded(ActualRoot);
  end;
end;

function TGocciaObjectContainingMatcherValue.AsymmetricDisplay: string;
begin
  Result := MatcherName;
end;

function TGocciaObjectContainingMatcherValue.MatcherName: string;
begin
  if Inverse then
    Result := 'ObjectNotContaining'
  else
    Result := 'ObjectContaining';
end;

constructor TGocciaStringContainingMatcherValue.Create(
  const ASample: TGocciaValue; const AInverse: Boolean);
var
  Text: string;
begin
  inherited Create(ASample, AInverse);
  if not TryGetStringValue(ASample, Text) then
    ThrowError('Expected is not a string');
end;

function TGocciaStringContainingMatcherValue.AsymmetricMatch(
  const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  ActualText, ExpectedText: string;
begin
  if not TryGetStringValue(AOther, ActualText) then
    Exit(ApplyInverse(False));
  TryGetStringValue(Sample, ExpectedText);
  Result := ApplyInverse(Pos(ExpectedText, ActualText) > 0);
end;

function TGocciaStringContainingMatcherValue.AsymmetricDisplay: string;
begin
  Result := MatcherName;
end;

function TGocciaStringContainingMatcherValue.MatcherName: string;
begin
  if Inverse then
    Result := 'StringNotContaining'
  else
    Result := 'StringContaining';
end;

constructor TGocciaStringMatchingMatcherValue.Create(
  const ASample: TGocciaValue; const AInverse: Boolean);
var
  Pattern: string;
  RegExpValue: TGocciaValue;
begin
  inherited Create(ASample, AInverse);
  if TryGetStringValue(ASample, Pattern) then
    RegExpValue := CreateRegExpObject(Pattern, '')
  else if IsRegExpInstance(ASample) then
    RegExpValue := CloneRegExpObject(ASample)
  else
    ThrowError('Expected is not a String or a RegExp');
  SetSample(RegExpValue);
end;

function TGocciaStringMatchingMatcherValue.AsymmetricMatch(
  const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  ActualText: string;
  MatchValue: TGocciaValue;
begin
  if not TryGetStringValue(AOther, ActualText) then
    Exit(ApplyInverse(False));
  Result := ApplyInverse(MatchRegExpObjectOnce(Sample, ActualText,
    MatchValue));
end;

function TGocciaStringMatchingMatcherValue.AsymmetricDisplay: string;
begin
  Result := MatcherName;
end;

function TGocciaStringMatchingMatcherValue.IsEquivalentTo(
  const AOther: TGocciaAsymmetricMatcherValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
begin
  Result := Assigned(AOther) and (ClassType = AOther.ClassType) and
    (Inverse = AOther.Inverse) and
    (GetRegExpInternalSource(Sample) =
      GetRegExpInternalSource(AOther.Sample)) and
    (GetRegExpInternalFlags(Sample) = GetRegExpInternalFlags(AOther.Sample));
end;

function TGocciaStringMatchingMatcherValue.MatcherName: string;
begin
  if Inverse then
    Result := 'StringNotMatching'
  else
    Result := 'StringMatching';
end;

constructor TGocciaCloseToMatcherValue.Create(const ASample,
  APrecision: TGocciaValue; const AInverse: Boolean);
var
  SampleNumber: Double;
begin
  inherited Create(ASample, AInverse);
  if not TryGetNumberValue(ASample, SampleNumber) then
    ThrowError('Expected is not a Number');
  if APrecision is TGocciaUndefinedLiteralValue then
    FPrecision := DEFAULT_CLOSE_TO_PRECISION
  else if not TryGetNumberValue(APrecision, FPrecision) then
    ThrowError('Precision is not a Number');
end;

function TGocciaCloseToMatcherValue.AsymmetricMatch(
  const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  ActualNumber, ExpectedNumber, Tolerance: Double;
  I, IntegerPrecision: Integer;
  Matches: Boolean;
begin
  if not TryGetNumberValue(AOther, ActualNumber) then
    Exit(ApplyInverse(False));
  TryGetNumberValue(Sample, ExpectedNumber);

  Matches := (IsInfinite(ActualNumber) and IsInfinite(ExpectedNumber) and
    (Sign(ActualNumber) = Sign(ExpectedNumber)));
  if not Matches then
  begin
    if IsNan(FPrecision) then
      Tolerance := NaN
    else if IsInfinite(FPrecision) then
    begin
      if FPrecision > 0 then
        Tolerance := 0
      else
        Tolerance := Infinity;
    end
    else
    begin
      if Abs(FPrecision) <= 308 then
        IntegerPrecision := Trunc(FPrecision)
      else
        IntegerPrecision := 0;
      if (Abs(FPrecision) <= 308) and
         (FPrecision = IntegerPrecision) then
      begin
        Tolerance := 1;
        if IntegerPrecision >= 0 then
          for I := 1 to IntegerPrecision do
            Tolerance := Tolerance / 10
        else
          for I := -1 downto IntegerPrecision do
            Tolerance := Tolerance * 10;
        Tolerance := Tolerance / 2;
      end
      else
        Tolerance := Power(10, -FPrecision) / 2;
    end;
    Matches := Abs(ExpectedNumber - ActualNumber) < Tolerance;
  end;
  Result := ApplyInverse(Matches);
end;

function TGocciaCloseToMatcherValue.AsymmetricDisplay: string;
var
  UnitName: string;
begin
  if FPrecision = 1 then
    UnitName := 'digit'
  else
    UnitName := 'digits';
  Result := MatcherName + ' ' + Sample.ToStringLiteral.Value + ' (' +
    FormatDouble(FPrecision) + ' ' + UnitName + ')';
end;

function TGocciaCloseToMatcherValue.IsEquivalentTo(
  const AOther: TGocciaAsymmetricMatcherValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  OtherPrecision: Double;
begin
  if not inherited IsEquivalentTo(AOther, AEquality) then
    Exit(False);
  OtherPrecision := TGocciaCloseToMatcherValue(AOther).Precision;
  if IsNan(FPrecision) or IsNan(OtherPrecision) then
    Exit(IsNan(FPrecision) and IsNan(OtherPrecision));
  if (FPrecision = 0) and (OtherPrecision = 0) then
    Exit(NumberIsNegativeZero(FPrecision) =
      NumberIsNegativeZero(OtherPrecision));
  Result := FPrecision = OtherPrecision;
end;

function TGocciaCloseToMatcherValue.MatcherName: string;
begin
  if Inverse then
    Result := 'NumberNotCloseTo'
  else
    Result := 'NumberCloseTo';
end;

constructor TGocciaSchemaMatchingMatcherValue.Create(
  const ASample: TGocciaValue; const AInverse: Boolean);
var
  StandardValue, ValidateValue: TGocciaValue;
begin
  inherited Create(ASample, AInverse);
  if not (ASample is TGocciaObjectValue) then
    ThrowTypeError('SchemaMatching expected to receive a Standard Schema.');
  StandardValue := TGocciaObjectValue(ASample).GetProperty('~standard');
  if not (StandardValue is TGocciaObjectValue) then
    ThrowTypeError('SchemaMatching expected to receive a Standard Schema.');
  ValidateValue := TGocciaObjectValue(StandardValue).GetProperty('validate');
  if not ValidateValue.IsCallable then
    ThrowTypeError('SchemaMatching expected to receive a Standard Schema.');

  FStandard := TGocciaObjectValue(StandardValue);
end;

function TGocciaSchemaMatchingMatcherValue.AsymmetricMatch(
  const AOther: TGocciaValue;
  const AEquality: TGocciaValueEqualityCallback): Boolean;
var
  Arguments: TGocciaArgumentsCollection;
  Issues, LengthValue, ResultValue, ValidateValue: TGocciaValue;
  Passes: Boolean;
begin
  ValidateValue := FStandard.GetProperty('validate');
  Arguments := TGocciaArgumentsCollection.Create([AOther]);
  try
    ResultValue := DispatchCall(ValidateValue, Arguments, FStandard);
  finally
    Arguments.Free;
  end;

  if ResultValue is TGocciaPromiseValue then
    ThrowTypeError(
      'Async schema validation is not supported in asymmetric matchers.');

  FValidationResult := ResultValue;
  Issues := ResultValue.GetProperty('issues');
  Passes := not Issues.ToBooleanLiteral.Value;
  if not Passes then
  begin
    LengthValue := Issues.GetProperty(PROP_LENGTH);
    Passes := (LengthValue is TGocciaNumberLiteralValue) and
      (TGocciaNumberLiteralValue(LengthValue).Value = 0);
  end;
  Result := ApplyInverse(Passes);
end;

function TGocciaSchemaMatchingMatcherValue.AsymmetricDisplay: string;
begin
  Result := MatcherName;
end;

function TGocciaSchemaMatchingMatcherValue.MatcherName: string;
begin
  if Inverse then
    Result := 'SchemaNotMatching'
  else
    Result := 'SchemaMatching';
end;

procedure TGocciaSchemaMatchingMatcherValue.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FStandard) then
    FStandard.MarkReferences;
  if Assigned(FValidationResult) then
    FValidationResult.MarkReferences;
end;

end.
