unit Goccia.Builtins.Semver;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue;

const
  SEMVER_NAMESPACE_PROPERTY = 'semver';

function CreateSemverNamespace: TGocciaObjectValue;

implementation

uses
  Generics.Collections,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.ObjectModel,
  Goccia.Semver,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TSemverHostList = TObjectList<TObject>;

  TGocciaSemverNamespaceHost = class
  private
    FSemverPrototype: TGocciaObjectValue;
    FComparatorPrototype: TGocciaObjectValue;
    FRangePrototype: TGocciaObjectValue;

    function BuildOptionsObject(const AOptions: TGocciaSemverOptions): TGocciaObjectValue;
    function ParseOptionsValue(const AValue: TGocciaValue): TGocciaSemverOptions;
    function ParseOptionsAt(const AArgs: TGocciaArgumentsCollection;
      const AIndex: Integer): TGocciaSemverOptions;

    function StringOrNull(const AValue: string): TGocciaValue;
    function BooleanValue(const AValue: Boolean): TGocciaValue;
    function IntegerValue(const AValue: Int64): TGocciaValue;
    function IdentifierValue(const AIdentifier: TGocciaSemverIdentifier): TGocciaValue;
    function IdentifierArrayValue(
      const AIdentifiers: TGocciaSemverIdentifierArray): TGocciaArrayValue;
    function StringArrayValue(
      const AValues: TGocciaSemverStringArray): TGocciaArrayValue;
    function StringMatrixValue(
      const AValues: TGocciaSemverStringMatrix): TGocciaArrayValue;
    function StringListArgument(const AValue: TGocciaValue): TGocciaSemverStringArray;

    function ReadSemver(const AValue: TGocciaValue;
      const AOptions: TGocciaSemverOptions): TGocciaSemver;
    function ReadComparator(const AValue: TGocciaValue;
      const AOptions: TGocciaSemverOptions): TGocciaSemverComparator;
    function ReadRange(const AValue: TGocciaValue;
      const AOptions: TGocciaSemverOptions): TGocciaSemverRange;

    procedure WriteSemverProperties(const ATarget: TGocciaObjectValue;
      const ASemver: TGocciaSemver);
    function CreateSemverObject(const ASemver: TGocciaSemver): TGocciaObjectValue;
    function CreateComparatorObject(
      const AComparator: TGocciaSemverComparator): TGocciaObjectValue;
    function CreateRangeObject(const ARange: TGocciaSemverRange): TGocciaObjectValue;

    function WrapNullReturningString(const AValue: string): TGocciaValue;
    function HandleSemverException(const E: Exception): TGocciaValue;

    function RequireStringArgument(const AArgs: TGocciaArgumentsCollection;
      const AIndex: Integer; const AMethodName: string): string;
  published
    function SemVerConstructor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ComparatorConstructor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RangeConstructor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function SemVerFormat(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SemVerToStringMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SemVerCompareMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SemVerCompareMainMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SemVerComparePreMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SemVerCompareBuildMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SemVerIncMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function ComparatorToStringMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ComparatorTestMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ComparatorIntersectsMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function RangeFormatMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RangeToStringMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RangeTestMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RangeIntersectsMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function NamespaceValid(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceClean(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceInc(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespacePrerelease(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceMajor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceMinor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespacePatch(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceIntersects(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceGT(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceGTE(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceLT(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceLTE(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceEQ(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceNEQ(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceCMP(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceCompare(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceRCompare(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceCompareBuild(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceCompareLoose(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceDiff(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceSort(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceRSort(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceValidRange(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceSatisfies(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceMaxSatisfying(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceMinSatisfying(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceMinVersion(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceGTR(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceLTR(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceOutside(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceSimplifyRange(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceSubset(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceToComparators(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NamespaceCoerce(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create;
  end;

var
  GSemverHosts: TSemverHostList;

const
  PROP_BUILD              = 'build';
  PROP_INCLUDE_PRERELEASE = 'includePrerelease';
  PROP_LOOSE              = 'loose';
  PROP_MAJOR              = 'major';
  PROP_MINOR              = 'minor';
  PROP_OPTIONS            = 'options';
  PROP_OPERATOR           = 'operator';
  PROP_PATCH              = 'patch';
  PROP_PRERELEASE         = 'prerelease';
  PROP_RANGE              = 'range';
  PROP_RAW                = 'raw';
  PROP_RTL                = 'rtl';
  PROP_SEMVER             = SEMVER_NAMESPACE_PROPERTY;
  PROP_SET_OF_COMPARATORS = 'set';
  PROP_VERSION            = 'version';

constructor TGocciaSemverNamespaceHost.Create;
begin
  inherited Create;
end;

function TGocciaSemverNamespaceHost.BuildOptionsObject(
  const AOptions: TGocciaSemverOptions): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Result.AssignProperty(PROP_LOOSE, BooleanValue(AOptions.Loose));
  Result.AssignProperty(PROP_INCLUDE_PRERELEASE,
    BooleanValue(AOptions.IncludePrerelease));
  Result.AssignProperty(PROP_RTL, BooleanValue(AOptions.RTL));
end;

function TGocciaSemverNamespaceHost.ParseOptionsValue(
  const AValue: TGocciaValue): TGocciaSemverOptions;
var
  OptionsObject: TGocciaObjectValue;
  PropertyValue: TGocciaValue;
begin
  Result := DefaultSemverOptions;
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) or
    (AValue is TGocciaNullLiteralValue) then
    Exit;

  if not (AValue is TGocciaObjectValue) then
  begin
    if AValue.ToBooleanLiteral.Value then
      Result.Loose := True;
    Exit;
  end;

  OptionsObject := TGocciaObjectValue(AValue);
  PropertyValue := OptionsObject.GetProperty(PROP_LOOSE);
  if Assigned(PropertyValue) and not (PropertyValue is TGocciaUndefinedLiteralValue) then
    Result.Loose := PropertyValue.ToBooleanLiteral.Value;
  PropertyValue := OptionsObject.GetProperty(PROP_INCLUDE_PRERELEASE);
  if Assigned(PropertyValue) and not (PropertyValue is TGocciaUndefinedLiteralValue) then
    Result.IncludePrerelease := PropertyValue.ToBooleanLiteral.Value;
  PropertyValue := OptionsObject.GetProperty(PROP_RTL);
  if Assigned(PropertyValue) and not (PropertyValue is TGocciaUndefinedLiteralValue) then
    Result.RTL := PropertyValue.ToBooleanLiteral.Value;
end;

function TGocciaSemverNamespaceHost.ParseOptionsAt(
  const AArgs: TGocciaArgumentsCollection; const AIndex: Integer): TGocciaSemverOptions;
begin
  if AArgs.Length <= AIndex then
    Exit(DefaultSemverOptions);
  Result := ParseOptionsValue(AArgs.GetElement(AIndex));
end;

function TGocciaSemverNamespaceHost.StringOrNull(const AValue: string): TGocciaValue;
begin
  if AValue = '' then
    Result := TGocciaNullLiteralValue.NullValue
  else
    Result := TGocciaStringLiteralValue.Create(AValue);
end;

function TGocciaSemverNamespaceHost.BooleanValue(const AValue: Boolean): TGocciaValue;
begin
  if AValue then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaSemverNamespaceHost.IntegerValue(const AValue: Int64): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AValue);
end;

function TGocciaSemverNamespaceHost.IdentifierValue(
  const AIdentifier: TGocciaSemverIdentifier): TGocciaValue;
begin
  if AIdentifier.Kind = sikNumeric then
    Result := IntegerValue(AIdentifier.NumericValue)
  else
    Result := TGocciaStringLiteralValue.Create(AIdentifier.TextValue);
end;

function TGocciaSemverNamespaceHost.IdentifierArrayValue(
  const AIdentifiers: TGocciaSemverIdentifierArray): TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to High(AIdentifiers) do
    Result.Elements.Add(IdentifierValue(AIdentifiers[I]));
end;

function TGocciaSemverNamespaceHost.StringArrayValue(
  const AValues: TGocciaSemverStringArray): TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to High(AValues) do
    Result.Elements.Add(TGocciaStringLiteralValue.Create(AValues[I]));
end;

function TGocciaSemverNamespaceHost.StringMatrixValue(
  const AValues: TGocciaSemverStringMatrix): TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to High(AValues) do
    Result.Elements.Add(StringArrayValue(AValues[I]));
end;

function TGocciaSemverNamespaceHost.StringListArgument(
  const AValue: TGocciaValue): TGocciaSemverStringArray;
var
  ArrayValue: TGocciaArrayValue;
  I: Integer;
begin
  if not (AValue is TGocciaArrayValue) then
    ThrowTypeError('Expected an array of versions');
  ArrayValue := TGocciaArrayValue(AValue);
  SetLength(Result, ArrayValue.Elements.Count);
  for I := 0 to ArrayValue.Elements.Count - 1 do
    Result[I] := ArrayValue.Elements[I].ToStringLiteral.Value;
end;

function TGocciaSemverNamespaceHost.ReadSemver(const AValue: TGocciaValue;
  const AOptions: TGocciaSemverOptions): TGocciaSemver;
var
  OptionValue: TGocciaValue;
begin
  if AValue is TGocciaObjectValue then
  begin
    OptionValue := TGocciaObjectValue(AValue).GetProperty(PROP_RAW);
    if not Assigned(OptionValue) or (OptionValue is TGocciaUndefinedLiteralValue) then
      OptionValue := TGocciaObjectValue(AValue).GetProperty(PROP_VERSION);
    Exit(MustParseSemver(OptionValue.ToStringLiteral.Value,
      ParseOptionsValue(TGocciaObjectValue(AValue).GetProperty(PROP_OPTIONS))));
  end;
  Result := MustParseSemver(AValue.ToStringLiteral.Value, AOptions);
end;

function TGocciaSemverNamespaceHost.ReadComparator(const AValue: TGocciaValue;
  const AOptions: TGocciaSemverOptions): TGocciaSemverComparator;
var
  TextValue: TGocciaValue;
begin
  if AValue is TGocciaObjectValue then
  begin
    TextValue := TGocciaObjectValue(AValue).GetProperty(PROP_VALUE);
    if not Assigned(TextValue) or (TextValue is TGocciaUndefinedLiteralValue) then
      TextValue := TGocciaStringLiteralValue.Create('');
    Exit(ParseComparator(TextValue.ToStringLiteral.Value,
      ParseOptionsValue(TGocciaObjectValue(AValue).GetProperty(PROP_OPTIONS))));
  end;
  Result := ParseComparator(AValue.ToStringLiteral.Value, AOptions);
end;

function TGocciaSemverNamespaceHost.ReadRange(const AValue: TGocciaValue;
  const AOptions: TGocciaSemverOptions): TGocciaSemverRange;
var
  TextValue: TGocciaValue;
begin
  if AValue is TGocciaObjectValue then
  begin
    TextValue := TGocciaObjectValue(AValue).GetProperty(PROP_RAW);
    if not Assigned(TextValue) or (TextValue is TGocciaUndefinedLiteralValue) then
      TextValue := TGocciaObjectValue(AValue).GetProperty(PROP_RANGE);
    Exit(MustParseRange(TextValue.ToStringLiteral.Value,
      ParseOptionsValue(TGocciaObjectValue(AValue).GetProperty(PROP_OPTIONS))));
  end;
  Result := MustParseRange(AValue.ToStringLiteral.Value, AOptions);
end;

procedure TGocciaSemverNamespaceHost.WriteSemverProperties(
  const ATarget: TGocciaObjectValue; const ASemver: TGocciaSemver);
begin
  ATarget.AssignProperty(PROP_OPTIONS, BuildOptionsObject(ASemver.Options));
  ATarget.AssignProperty(PROP_LOOSE, BooleanValue(ASemver.Options.Loose));
  ATarget.AssignProperty(PROP_INCLUDE_PRERELEASE,
    BooleanValue(ASemver.Options.IncludePrerelease));
  ATarget.AssignProperty(PROP_RAW, TGocciaStringLiteralValue.Create(ASemver.Raw));
  ATarget.AssignProperty(PROP_VERSION, TGocciaStringLiteralValue.Create(ASemver.Version));
  ATarget.AssignProperty(PROP_MAJOR, IntegerValue(ASemver.Major));
  ATarget.AssignProperty(PROP_MINOR, IntegerValue(ASemver.Minor));
  ATarget.AssignProperty(PROP_PATCH, IntegerValue(ASemver.Patch));
  ATarget.AssignProperty(PROP_PRERELEASE, IdentifierArrayValue(ASemver.Prerelease));
  ATarget.AssignProperty(PROP_BUILD, StringArrayValue(ASemver.Build));
end;

function TGocciaSemverNamespaceHost.CreateSemverObject(
  const ASemver: TGocciaSemver): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(FSemverPrototype);
  WriteSemverProperties(Result, ASemver);
end;

function TGocciaSemverNamespaceHost.CreateComparatorObject(
  const AComparator: TGocciaSemverComparator): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(FComparatorPrototype);
  Result.AssignProperty(PROP_OPTIONS, BuildOptionsObject(AComparator.Semver.Options));
  Result.AssignProperty(PROP_LOOSE, BooleanValue(AComparator.Semver.Options.Loose));
  Result.AssignProperty(PROP_OPERATOR,
    TGocciaStringLiteralValue.Create(AComparator.Operator));
  if AComparator.IsAny then
    Result.AssignProperty(PROP_SEMVER, TGocciaNullLiteralValue.NullValue)
  else
    Result.AssignProperty(PROP_SEMVER, CreateSemverObject(AComparator.Semver));
  Result.AssignProperty(PROP_VALUE, TGocciaStringLiteralValue.Create(AComparator.Value));
end;

function TGocciaSemverNamespaceHost.CreateRangeObject(
  const ARange: TGocciaSemverRange): TGocciaObjectValue;
var
  I, J: Integer;
  SetArray: TGocciaArrayValue;
  ComparatorArray: TGocciaArrayValue;
begin
  Result := TGocciaObjectValue.Create(FRangePrototype);
  Result.AssignProperty(PROP_OPTIONS, BuildOptionsObject(ARange.Options));
  Result.AssignProperty(PROP_LOOSE, BooleanValue(ARange.Options.Loose));
  Result.AssignProperty(PROP_INCLUDE_PRERELEASE,
    BooleanValue(ARange.Options.IncludePrerelease));
  Result.AssignProperty(PROP_RAW, TGocciaStringLiteralValue.Create(ARange.Raw));
  Result.AssignProperty(PROP_RANGE, TGocciaStringLiteralValue.Create(RangeToString(ARange)));

  SetArray := TGocciaArrayValue.Create;
  for I := 0 to High(ARange.SetOfComparators) do
  begin
    ComparatorArray := TGocciaArrayValue.Create;
    for J := 0 to High(ARange.SetOfComparators[I]) do
      ComparatorArray.Elements.Add(CreateComparatorObject(ARange.SetOfComparators[I][J]));
    SetArray.Elements.Add(ComparatorArray);
  end;
  Result.AssignProperty(PROP_SET_OF_COMPARATORS, SetArray);
end;

function TGocciaSemverNamespaceHost.WrapNullReturningString(
  const AValue: string): TGocciaValue;
begin
  Result := StringOrNull(AValue);
end;

function TGocciaSemverNamespaceHost.HandleSemverException(
  const E: Exception): TGocciaValue;
begin
  if E is EGocciaSemverTypeError then
    ThrowTypeError(E.Message)
  else if E is EGocciaSemverError then
    ThrowError(E.Message)
  else
    raise E;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSemverNamespaceHost.RequireStringArgument(
  const AArgs: TGocciaArgumentsCollection; const AIndex: Integer;
  const AMethodName: string): string;
begin
  if AArgs.Length <= AIndex then
    ThrowTypeError(AMethodName + ' requires a string argument');
  Result := AArgs.GetElement(AIndex).ToStringLiteral.Value;
end;

function TGocciaSemverNamespaceHost.SemVerConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Options: TGocciaSemverOptions;
begin
  try
    Options := ParseOptionsAt(AArgs, 1);
    Result := CreateSemverObject(MustParseSemver(
      RequireStringArgument(AArgs, 0, 'SemVer'), Options));
  except
    on E: Exception do
      Result := HandleSemverException(E);
  end;
end;

function TGocciaSemverNamespaceHost.ComparatorConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Options: TGocciaSemverOptions;
begin
  try
    Options := ParseOptionsAt(AArgs, 1);
    Result := CreateComparatorObject(ParseComparator(
      RequireStringArgument(AArgs, 0, 'Comparator'), Options));
  except
    on E: Exception do
      Result := HandleSemverException(E);
  end;
end;

function TGocciaSemverNamespaceHost.RangeConstructor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Options: TGocciaSemverOptions;
begin
  try
    Options := ParseOptionsAt(AArgs, 1);
    Result := CreateRangeObject(MustParseRange(
      RequireStringArgument(AArgs, 0, 'Range'), Options));
  except
    on E: Exception do
      Result := HandleSemverException(E);
  end;
end;

function TGocciaSemverNamespaceHost.SemVerFormat(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ReadSemver(AThisValue,
    DefaultSemverOptions).Version);
end;

function TGocciaSemverNamespaceHost.SemVerToStringMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ReadSemver(AThisValue,
    DefaultSemverOptions).Version);
end;

function TGocciaSemverNamespaceHost.SemVerCompareMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSemver, OtherSemver: TGocciaSemver;
begin
  ThisSemver := ReadSemver(AThisValue, DefaultSemverOptions);
  OtherSemver := ReadSemver(AArgs.GetElement(0), ThisSemver.Options);
  Result := IntegerValue(Goccia.Semver.Compare(ThisSemver.Version,
    OtherSemver.Version, ThisSemver.Options));
end;

function TGocciaSemverNamespaceHost.SemVerCompareMainMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSemver, OtherSemver: TGocciaSemver;
begin
  ThisSemver := ReadSemver(AThisValue, DefaultSemverOptions);
  OtherSemver := ReadSemver(AArgs.GetElement(0), ThisSemver.Options);
  Result := IntegerValue(Compare(ThisSemver.Version, OtherSemver.Version,
    ThisSemver.Options));
end;

function TGocciaSemverNamespaceHost.SemVerComparePreMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := SemVerCompareMethod(AArgs, AThisValue);
end;

function TGocciaSemverNamespaceHost.SemVerCompareBuildMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSemver, OtherSemver: TGocciaSemver;
begin
  ThisSemver := ReadSemver(AThisValue, DefaultSemverOptions);
  OtherSemver := ReadSemver(AArgs.GetElement(0), ThisSemver.Options);
  Result := IntegerValue(Goccia.Semver.CompareBuild(ThisSemver.Version,
    OtherSemver.Version, ThisSemver.Options));
end;

function TGocciaSemverNamespaceHost.SemVerIncMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ThisSemver, UpdatedSemver: TGocciaSemver;
  Updated: string;
  Identifier: string;
begin
  try
    ThisSemver := ReadSemver(AThisValue, DefaultSemverOptions);
    if AArgs.Length > 1 then
      Identifier := AArgs.GetElement(1).ToStringLiteral.Value
    else
      Identifier := '';
    if not TryIncrement(ThisSemver.Version, AArgs.GetElement(0).ToStringLiteral.Value,
      ThisSemver.Options, Identifier, False, False, Updated) then
      ThrowError('invalid increment');
    UpdatedSemver := MustParseSemver(Updated, ThisSemver.Options);
    WriteSemverProperties(TGocciaObjectValue(AThisValue), UpdatedSemver);
    Result := AThisValue;
  except
    on E: Exception do
      Result := HandleSemverException(E);
  end;
end;

function TGocciaSemverNamespaceHost.ComparatorToStringMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ReadComparator(AThisValue,
    DefaultSemverOptions).Value);
end;

function TGocciaSemverNamespaceHost.ComparatorTestMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ComparatorValue: TGocciaSemverComparator;
  VersionValue: TGocciaSemver;
begin
  ComparatorValue := ReadComparator(AThisValue, DefaultSemverOptions);
  VersionValue := ReadSemver(AArgs.GetElement(0), ComparatorValue.Semver.Options);
  Result := BooleanValue(Cmp(VersionValue.Version, ComparatorValue.Operator,
    ComparatorValue.Semver.Version, ComparatorValue.Semver.Options));
end;

function TGocciaSemverNamespaceHost.ComparatorIntersectsMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LeftComparator, RightComparator: TGocciaSemverComparator;
  Options: TGocciaSemverOptions;
begin
  LeftComparator := ReadComparator(AThisValue, DefaultSemverOptions);
  Options := ParseOptionsAt(AArgs, 1);
  RightComparator := ReadComparator(AArgs.GetElement(0), Options);
  Result := BooleanValue(ComparatorIntersects(LeftComparator, RightComparator, Options));
end;

function TGocciaSemverNamespaceHost.RangeFormatMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ReadRange(AThisValue,
    DefaultSemverOptions).Formatted);
end;

function TGocciaSemverNamespaceHost.RangeToStringMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RangeFormatMethod(AArgs, AThisValue);
end;

function TGocciaSemverNamespaceHost.RangeTestMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  RangeValue: TGocciaSemverRange;
begin
  RangeValue := ReadRange(AThisValue, DefaultSemverOptions);
  Result := BooleanValue(Satisfies(AArgs.GetElement(0).ToStringLiteral.Value,
    RangeValue.Raw, RangeValue.Options));
end;

function TGocciaSemverNamespaceHost.RangeIntersectsMethod(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LeftRange, RightRange: TGocciaSemverRange;
  Options: TGocciaSemverOptions;
begin
  LeftRange := ReadRange(AThisValue, DefaultSemverOptions);
  Options := ParseOptionsAt(AArgs, 1);
  RightRange := ReadRange(AArgs.GetElement(0), Options);
  Result := BooleanValue(RangeIntersects(LeftRange.Raw, RightRange.Raw, Options));
end;

function TGocciaSemverNamespaceHost.NamespaceValid(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := WrapNullReturningString(Valid(
    RequireStringArgument(AArgs, 0, 'valid'), ParseOptionsAt(AArgs, 1)));
end;

function TGocciaSemverNamespaceHost.NamespaceClean(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := WrapNullReturningString(Clean(
    RequireStringArgument(AArgs, 0, 'clean'), ParseOptionsAt(AArgs, 1)));
end;

function TGocciaSemverNamespaceHost.NamespaceParse(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Options: TGocciaSemverOptions;
  Parsed: TGocciaSemver;
begin
  Options := ParseOptionsAt(AArgs, 1);
  if ParseSemver(RequireStringArgument(AArgs, 0, 'parse'), Options, Parsed) then
    Result := CreateSemverObject(Parsed)
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

function TGocciaSemverNamespaceHost.NamespaceInc(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  VersionText, ReleaseText, Identifier: string;
  Options: TGocciaSemverOptions;
  Incremented: string;
begin
  VersionText := RequireStringArgument(AArgs, 0, 'inc');
  ReleaseText := RequireStringArgument(AArgs, 1, 'inc');
  Identifier := '';
  Options := DefaultSemverOptions;
  if AArgs.Length > 2 then
  begin
    if AArgs.GetElement(2) is TGocciaObjectValue then
      Options := ParseOptionsValue(AArgs.GetElement(2))
    else
      Identifier := AArgs.GetElement(2).ToStringLiteral.Value;
  end;
  if (Identifier = '') and (AArgs.Length > 3) then
    Identifier := AArgs.GetElement(3).ToStringLiteral.Value;
  if TryIncrement(VersionText, ReleaseText, Options, Identifier, False, False,
    Incremented) then
    Result := TGocciaStringLiteralValue.Create(Incremented)
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

function TGocciaSemverNamespaceHost.NamespacePrerelease(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Identifiers: TGocciaSemverIdentifierArray;
begin
  if PrereleaseOf(RequireStringArgument(AArgs, 0, 'prerelease'),
    ParseOptionsAt(AArgs, 1), Identifiers) then
    Result := IdentifierArrayValue(Identifiers)
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

function TGocciaSemverNamespaceHost.NamespaceMajor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := IntegerValue(MajorOf(RequireStringArgument(AArgs, 0, 'major'),
    ParseOptionsAt(AArgs, 1)));
end;

function TGocciaSemverNamespaceHost.NamespaceMinor(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := IntegerValue(MinorOf(RequireStringArgument(AArgs, 0, 'minor'),
    ParseOptionsAt(AArgs, 1)));
end;

function TGocciaSemverNamespaceHost.NamespacePatch(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := IntegerValue(PatchOf(RequireStringArgument(AArgs, 0, 'patch'),
    ParseOptionsAt(AArgs, 1)));
end;

function TGocciaSemverNamespaceHost.NamespaceIntersects(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(RangeIntersects(
    RequireStringArgument(AArgs, 0, 'intersects'),
    RequireStringArgument(AArgs, 1, 'intersects'),
    ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceGT(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Compare(RequireStringArgument(AArgs, 0, 'gt'),
    RequireStringArgument(AArgs, 1, 'gt'), ParseOptionsAt(AArgs, 2)) > 0);
end;

function TGocciaSemverNamespaceHost.NamespaceGTE(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Compare(RequireStringArgument(AArgs, 0, 'gte'),
    RequireStringArgument(AArgs, 1, 'gte'), ParseOptionsAt(AArgs, 2)) >= 0);
end;

function TGocciaSemverNamespaceHost.NamespaceLT(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Compare(RequireStringArgument(AArgs, 0, 'lt'),
    RequireStringArgument(AArgs, 1, 'lt'), ParseOptionsAt(AArgs, 2)) < 0);
end;

function TGocciaSemverNamespaceHost.NamespaceLTE(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Compare(RequireStringArgument(AArgs, 0, 'lte'),
    RequireStringArgument(AArgs, 1, 'lte'), ParseOptionsAt(AArgs, 2)) <= 0);
end;

function TGocciaSemverNamespaceHost.NamespaceEQ(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Compare(RequireStringArgument(AArgs, 0, 'eq'),
    RequireStringArgument(AArgs, 1, 'eq'), ParseOptionsAt(AArgs, 2)) = 0);
end;

function TGocciaSemverNamespaceHost.NamespaceNEQ(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Compare(RequireStringArgument(AArgs, 0, 'neq'),
    RequireStringArgument(AArgs, 1, 'neq'), ParseOptionsAt(AArgs, 2)) <> 0);
end;

function TGocciaSemverNamespaceHost.NamespaceCMP(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Cmp(RequireStringArgument(AArgs, 0, 'cmp'),
    RequireStringArgument(AArgs, 1, 'cmp'),
    RequireStringArgument(AArgs, 2, 'cmp'), ParseOptionsAt(AArgs, 3)));
end;

function TGocciaSemverNamespaceHost.NamespaceCompare(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := IntegerValue(Compare(RequireStringArgument(AArgs, 0, 'compare'),
    RequireStringArgument(AArgs, 1, 'compare'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceRCompare(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := IntegerValue(Compare(RequireStringArgument(AArgs, 1, 'rcompare'),
    RequireStringArgument(AArgs, 0, 'rcompare'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceCompareBuild(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := IntegerValue(CompareBuild(
    RequireStringArgument(AArgs, 0, 'compareBuild'),
    RequireStringArgument(AArgs, 1, 'compareBuild'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceCompareLoose(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := IntegerValue(CompareLoose(
    RequireStringArgument(AArgs, 0, 'compareLoose'),
    RequireStringArgument(AArgs, 1, 'compareLoose')));
end;

function TGocciaSemverNamespaceHost.NamespaceDiff(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := WrapNullReturningString(Diff(
    RequireStringArgument(AArgs, 0, 'diff'),
    RequireStringArgument(AArgs, 1, 'diff'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceSort(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Values: TGocciaSemverStringArray;
  I, J: Integer;
  Temp: string;
begin
  Values := StringListArgument(AArgs.GetElement(0));
  for I := 0 to High(Values) do
    for J := I + 1 to High(Values) do
      if CompareBuild(Values[I], Values[J], ParseOptionsAt(AArgs, 1)) > 0 then
      begin
        Temp := Values[I];
        Values[I] := Values[J];
        Values[J] := Temp;
      end;
  Result := StringArrayValue(Values);
end;

function TGocciaSemverNamespaceHost.NamespaceRSort(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Values: TGocciaSemverStringArray;
  I, J: Integer;
  Temp: string;
begin
  Values := StringListArgument(AArgs.GetElement(0));
  for I := 0 to High(Values) do
    for J := I + 1 to High(Values) do
      if CompareBuild(Values[I], Values[J], ParseOptionsAt(AArgs, 1)) < 0 then
      begin
        Temp := Values[I];
        Values[I] := Values[J];
        Values[J] := Temp;
      end;
  Result := StringArrayValue(Values);
end;

function TGocciaSemverNamespaceHost.NamespaceValidRange(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := WrapNullReturningString(ValidRange(
    RequireStringArgument(AArgs, 0, 'validRange'), ParseOptionsAt(AArgs, 1)));
end;

function TGocciaSemverNamespaceHost.NamespaceSatisfies(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Satisfies(RequireStringArgument(AArgs, 0, 'satisfies'),
    RequireStringArgument(AArgs, 1, 'satisfies'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceMaxSatisfying(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := WrapNullReturningString(MaxSatisfying(
    StringListArgument(AArgs.GetElement(0)),
    RequireStringArgument(AArgs, 1, 'maxSatisfying'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceMinSatisfying(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := WrapNullReturningString(MinSatisfying(
    StringListArgument(AArgs.GetElement(0)),
    RequireStringArgument(AArgs, 1, 'minSatisfying'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceMinVersion(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Minimum: string;
  Parsed: TGocciaSemver;
begin
  Minimum := MinVersion(RequireStringArgument(AArgs, 0, 'minVersion'),
    ParseOptionsAt(AArgs, 1));
  if (Minimum <> '') and ParseSemver(Minimum, ParseOptionsAt(AArgs, 1), Parsed) then
    Result := CreateSemverObject(Parsed)
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

function TGocciaSemverNamespaceHost.NamespaceGTR(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(GreaterThanRange(
    RequireStringArgument(AArgs, 0, 'gtr'),
    RequireStringArgument(AArgs, 1, 'gtr'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceLTR(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(LessThanRange(
    RequireStringArgument(AArgs, 0, 'ltr'),
    RequireStringArgument(AArgs, 1, 'ltr'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceOutside(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(Outside(RequireStringArgument(AArgs, 0, 'outside'),
    RequireStringArgument(AArgs, 1, 'outside'),
    RequireStringArgument(AArgs, 2, 'outside'), ParseOptionsAt(AArgs, 3)));
end;

function TGocciaSemverNamespaceHost.NamespaceSimplifyRange(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(SimplifyRange(
    StringListArgument(AArgs.GetElement(0)),
    RequireStringArgument(AArgs, 1, 'simplifyRange'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceSubset(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := BooleanValue(IsSubset(RequireStringArgument(AArgs, 0, 'subset'),
    RequireStringArgument(AArgs, 1, 'subset'), ParseOptionsAt(AArgs, 2)));
end;

function TGocciaSemverNamespaceHost.NamespaceToComparators(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := StringMatrixValue(ToComparators(
    RequireStringArgument(AArgs, 0, 'toComparators'), ParseOptionsAt(AArgs, 1)));
end;

function TGocciaSemverNamespaceHost.NamespaceCoerce(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Parsed: TGocciaSemver;
begin
  if Coerce(RequireStringArgument(AArgs, 0, 'coerce'), ParseOptionsAt(AArgs, 1), Parsed) then
    Result := CreateSemverObject(Parsed)
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

function CreateSemverNamespace: TGocciaObjectValue;
var
  Host: TGocciaSemverNamespaceHost;
  NamespaceObject, ClassesObject, FunctionsObject, RangesObject: TGocciaObjectValue;
  Members: TGocciaMemberCollection;
  StaticMembers: TArray<TGocciaMemberDefinition>;
  ReleaseTypesArray: TGocciaSemverStringArray;
begin
  Host := TGocciaSemverNamespaceHost.Create;
  GSemverHosts.Add(Host);

  Host.FSemverPrototype := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod(PROP_TO_STRING, Host.SemVerToStringMethod, 0);
    Members.AddNamedMethod('format', Host.SemVerFormat, 0);
    Members.AddNamedMethod('compare', Host.SemVerCompareMethod, 1);
    Members.AddNamedMethod('compareMain', Host.SemVerCompareMainMethod, 1);
    Members.AddNamedMethod('comparePre', Host.SemVerComparePreMethod, 1);
    Members.AddNamedMethod('compareBuild', Host.SemVerCompareBuildMethod, 1);
    Members.AddNamedMethod('inc', Host.SemVerIncMethod, 1);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Host.FSemverPrototype, StaticMembers);

  Host.FComparatorPrototype := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod(PROP_TO_STRING, Host.ComparatorToStringMethod, 0);
    Members.AddNamedMethod('test', Host.ComparatorTestMethod, 1);
    Members.AddNamedMethod('intersects', Host.ComparatorIntersectsMethod, 1);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Host.FComparatorPrototype, StaticMembers);

  Host.FRangePrototype := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod(PROP_TO_STRING, Host.RangeToStringMethod, 0);
    Members.AddNamedMethod('format', Host.RangeFormatMethod, 0);
    Members.AddNamedMethod('test', Host.RangeTestMethod, 1);
    Members.AddNamedMethod('intersects', Host.RangeIntersectsMethod, 1);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Host.FRangePrototype, StaticMembers);

  NamespaceObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  NamespaceObject.AssignProperty('SemVer',
    TGocciaNativeFunctionValue.Create(Host.SemVerConstructor, CONSTRUCTOR_SEMVER, 2));
  NamespaceObject.AssignProperty('Comparator',
    TGocciaNativeFunctionValue.Create(Host.ComparatorConstructor, CONSTRUCTOR_COMPARATOR, 2));
  NamespaceObject.AssignProperty('Range',
    TGocciaNativeFunctionValue.Create(Host.RangeConstructor, CONSTRUCTOR_RANGE, 2));

  TGocciaObjectValue(NamespaceObject.GetProperty('SemVer')).AssignProperty(
    PROP_PROTOTYPE, Host.FSemverPrototype);
  TGocciaObjectValue(NamespaceObject.GetProperty('Comparator')).AssignProperty(
    PROP_PROTOTYPE, Host.FComparatorPrototype);
  TGocciaObjectValue(NamespaceObject.GetProperty('Range')).AssignProperty(
    PROP_PROTOTYPE, Host.FRangePrototype);

  NamespaceObject.AssignProperty('SEMVER_SPEC_VERSION',
    TGocciaStringLiteralValue.Create(SEMVER_SPEC_VERSION));
  SetLength(ReleaseTypesArray, Length(RELEASE_TYPES));
  ReleaseTypesArray[0] := RELEASE_TYPES[0];
  ReleaseTypesArray[1] := RELEASE_TYPES[1];
  ReleaseTypesArray[2] := RELEASE_TYPES[2];
  ReleaseTypesArray[3] := RELEASE_TYPES[3];
  ReleaseTypesArray[4] := RELEASE_TYPES[4];
  ReleaseTypesArray[5] := RELEASE_TYPES[5];
  ReleaseTypesArray[6] := RELEASE_TYPES[6];
  NamespaceObject.AssignProperty('RELEASE_TYPES', Host.StringArrayValue(ReleaseTypesArray));

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('valid', Host.NamespaceValid, 1, gmkStaticMethod);
    Members.AddNamedMethod('clean', Host.NamespaceClean, 1, gmkStaticMethod);
    Members.AddNamedMethod('parse', Host.NamespaceParse, 1, gmkStaticMethod);
    Members.AddNamedMethod('inc', Host.NamespaceInc, 2, gmkStaticMethod);
    Members.AddNamedMethod('prerelease', Host.NamespacePrerelease, 1, gmkStaticMethod);
    Members.AddNamedMethod('major', Host.NamespaceMajor, 1, gmkStaticMethod);
    Members.AddNamedMethod('minor', Host.NamespaceMinor, 1, gmkStaticMethod);
    Members.AddNamedMethod('patch', Host.NamespacePatch, 1, gmkStaticMethod);
    Members.AddNamedMethod('intersects', Host.NamespaceIntersects, 2, gmkStaticMethod);
    Members.AddNamedMethod('gt', Host.NamespaceGT, 2, gmkStaticMethod);
    Members.AddNamedMethod('gte', Host.NamespaceGTE, 2, gmkStaticMethod);
    Members.AddNamedMethod('lt', Host.NamespaceLT, 2, gmkStaticMethod);
    Members.AddNamedMethod('lte', Host.NamespaceLTE, 2, gmkStaticMethod);
    Members.AddNamedMethod('eq', Host.NamespaceEQ, 2, gmkStaticMethod);
    Members.AddNamedMethod('neq', Host.NamespaceNEQ, 2, gmkStaticMethod);
    Members.AddNamedMethod('cmp', Host.NamespaceCMP, 3, gmkStaticMethod);
    Members.AddNamedMethod('compare', Host.NamespaceCompare, 2, gmkStaticMethod);
    Members.AddNamedMethod('rcompare', Host.NamespaceRCompare, 2, gmkStaticMethod);
    Members.AddNamedMethod('compareBuild', Host.NamespaceCompareBuild, 2, gmkStaticMethod);
    Members.AddNamedMethod('compareLoose', Host.NamespaceCompareLoose, 2, gmkStaticMethod);
    Members.AddNamedMethod('diff', Host.NamespaceDiff, 2, gmkStaticMethod);
    Members.AddNamedMethod('sort', Host.NamespaceSort, 1, gmkStaticMethod);
    Members.AddNamedMethod('rsort', Host.NamespaceRSort, 1, gmkStaticMethod);
    Members.AddNamedMethod('validRange', Host.NamespaceValidRange, 1, gmkStaticMethod);
    Members.AddNamedMethod('satisfies', Host.NamespaceSatisfies, 2, gmkStaticMethod);
    Members.AddNamedMethod('maxSatisfying', Host.NamespaceMaxSatisfying, 2, gmkStaticMethod);
    Members.AddNamedMethod('minSatisfying', Host.NamespaceMinSatisfying, 2, gmkStaticMethod);
    Members.AddNamedMethod('minVersion', Host.NamespaceMinVersion, 1, gmkStaticMethod);
    Members.AddNamedMethod('gtr', Host.NamespaceGTR, 2, gmkStaticMethod);
    Members.AddNamedMethod('ltr', Host.NamespaceLTR, 2, gmkStaticMethod);
    Members.AddNamedMethod('outside', Host.NamespaceOutside, 3, gmkStaticMethod);
    Members.AddNamedMethod('simplifyRange', Host.NamespaceSimplifyRange, 2, gmkStaticMethod);
    Members.AddNamedMethod('subset', Host.NamespaceSubset, 2, gmkStaticMethod);
    Members.AddNamedMethod('toComparators', Host.NamespaceToComparators, 1, gmkStaticMethod);
    Members.AddNamedMethod('coerce', Host.NamespaceCoerce, 1, gmkStaticMethod);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(NamespaceObject, StaticMembers);

  ClassesObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  ClassesObject.AssignProperty('Comparator', NamespaceObject.GetProperty('Comparator'));
  ClassesObject.AssignProperty('Range', NamespaceObject.GetProperty('Range'));
  ClassesObject.AssignProperty('SemVer', NamespaceObject.GetProperty('SemVer'));
  ClassesObject.AssignProperty('comparator', NamespaceObject.GetProperty('Comparator'));
  ClassesObject.AssignProperty('range', NamespaceObject.GetProperty('Range'));
  ClassesObject.AssignProperty('semver', NamespaceObject.GetProperty('SemVer'));
  NamespaceObject.AssignProperty('classes', ClassesObject);

  FunctionsObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  FunctionsObject.AssignProperty('clean', NamespaceObject.GetProperty('clean'));
  FunctionsObject.AssignProperty('cmp', NamespaceObject.GetProperty('cmp'));
  FunctionsObject.AssignProperty('coerce', NamespaceObject.GetProperty('coerce'));
  FunctionsObject.AssignProperty('compare', NamespaceObject.GetProperty('compare'));
  FunctionsObject.AssignProperty('compareBuild', NamespaceObject.GetProperty('compareBuild'));
  FunctionsObject.AssignProperty('compareLoose', NamespaceObject.GetProperty('compareLoose'));
  FunctionsObject.AssignProperty('diff', NamespaceObject.GetProperty('diff'));
  FunctionsObject.AssignProperty('eq', NamespaceObject.GetProperty('eq'));
  FunctionsObject.AssignProperty('gt', NamespaceObject.GetProperty('gt'));
  FunctionsObject.AssignProperty('gte', NamespaceObject.GetProperty('gte'));
  FunctionsObject.AssignProperty('inc', NamespaceObject.GetProperty('inc'));
  FunctionsObject.AssignProperty('lt', NamespaceObject.GetProperty('lt'));
  FunctionsObject.AssignProperty('lte', NamespaceObject.GetProperty('lte'));
  FunctionsObject.AssignProperty('major', NamespaceObject.GetProperty('major'));
  FunctionsObject.AssignProperty('minor', NamespaceObject.GetProperty('minor'));
  FunctionsObject.AssignProperty('neq', NamespaceObject.GetProperty('neq'));
  FunctionsObject.AssignProperty('parse', NamespaceObject.GetProperty('parse'));
  FunctionsObject.AssignProperty('patch', NamespaceObject.GetProperty('patch'));
  FunctionsObject.AssignProperty('prerelease', NamespaceObject.GetProperty('prerelease'));
  FunctionsObject.AssignProperty('rcompare', NamespaceObject.GetProperty('rcompare'));
  FunctionsObject.AssignProperty('rsort', NamespaceObject.GetProperty('rsort'));
  FunctionsObject.AssignProperty('satisfies', NamespaceObject.GetProperty('satisfies'));
  FunctionsObject.AssignProperty('sort', NamespaceObject.GetProperty('sort'));
  FunctionsObject.AssignProperty('valid', NamespaceObject.GetProperty('valid'));
  NamespaceObject.AssignProperty('functions', FunctionsObject);

  RangesObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  RangesObject.AssignProperty('gtr', NamespaceObject.GetProperty('gtr'));
  RangesObject.AssignProperty('intersects', NamespaceObject.GetProperty('intersects'));
  RangesObject.AssignProperty('ltr', NamespaceObject.GetProperty('ltr'));
  RangesObject.AssignProperty('maxSatisfying', NamespaceObject.GetProperty('maxSatisfying'));
  RangesObject.AssignProperty('minSatisfying', NamespaceObject.GetProperty('minSatisfying'));
  RangesObject.AssignProperty('minVersion', NamespaceObject.GetProperty('minVersion'));
  RangesObject.AssignProperty('outside', NamespaceObject.GetProperty('outside'));
  RangesObject.AssignProperty('simplify', NamespaceObject.GetProperty('simplifyRange'));
  RangesObject.AssignProperty('subset', NamespaceObject.GetProperty('subset'));
  RangesObject.AssignProperty('toComparators', NamespaceObject.GetProperty('toComparators'));
  RangesObject.AssignProperty('valid', NamespaceObject.GetProperty('validRange'));
  NamespaceObject.AssignProperty('ranges', RangesObject);

  Result := NamespaceObject;
end;

initialization
  GSemverHosts := TObjectList<TObject>.Create(True);

finalization
  GSemverHosts.Free;

end.
