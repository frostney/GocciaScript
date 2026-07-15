unit Goccia.Builtins.Testing.SnapshotFormatting;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaSnapshotSerializerRegistry = class;

  { Host-replaceable formatter seam. The default implementation below matches
    the options Vitest uses when serializing snapshots. }
  IGocciaSnapshotFormatter = interface
    ['{A5C242B7-7DA8-4572-9F12-6B9CB05B3CB8}']
    function Format(const AValue: TGocciaValue;
      const ASerializers: TGocciaSnapshotSerializerRegistry): string;
  end;

  { Non-owning, newest-first registry for expect.addSnapshotSerializer. }
  TGocciaSnapshotSerializerRegistry = class
  private
    FSerializers: array of TGocciaValue;
  public
    function Add(const ASerializer: TGocciaValue): Boolean;
    procedure Clear;
    function Count: Integer;
    function SerializerAt(const AIndex: Integer): TGocciaValue;
    procedure MarkReferences;
  end;

  TGocciaVitestSnapshotFormatter = class(TInterfacedObject,
    IGocciaSnapshotFormatter)
  private
    FDateToISOString: TGocciaValue;
    FDateToISOStringRooted: Boolean;
  public
    destructor Destroy; override;
    procedure CaptureDateIntrinsics(const AGlobalObject: TGocciaObjectValue);
    function Format(const AValue: TGocciaValue;
      const ASerializers: TGocciaSnapshotSerializerRegistry): string;
  end;

  { Owns serializer ordering and delegates formatting through the replaceable
    Pascal adapter. }
  TGocciaSnapshotFormatting = class
  private
    FFormatter: IGocciaSnapshotFormatter;
    FDefaultFormatter: TGocciaVitestSnapshotFormatter;
    FSerializers: TGocciaSnapshotSerializerRegistry;
  public
    constructor Create(const AFormatter: IGocciaSnapshotFormatter = nil);
    destructor Destroy; override;

    function Format(const AValue: TGocciaValue): string;
    procedure CaptureDateIntrinsics(const AGlobalObject: TGocciaObjectValue);
    procedure SetFormatter(const AFormatter: IGocciaSnapshotFormatter);

    property Formatter: IGocciaSnapshotFormatter read FFormatter
      write SetFormatter;
    property Serializers: TGocciaSnapshotSerializerRegistry read FSerializers;
  end;

implementation

uses
  Generics.Collections,
  Math,
  SysUtils,

  StringBuffer,

  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.RegExp.Runtime,
  Goccia.Values.ArgumentsObjectValue,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.AsymmetricMatcher,
  Goccia.Values.BigIntObjectValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.ClassValue,
  Goccia.Values.DataViewValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.GeneratorValue,
  Goccia.Values.HoleValue,
  Goccia.Values.IntlCollator,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.IntlDisplayNames,
  Goccia.Values.IntlDurationFormat,
  Goccia.Values.IntlListFormat,
  Goccia.Values.IntlLocale,
  Goccia.Values.IntlNumberFormat,
  Goccia.Values.IntlPluralRules,
  Goccia.Values.IntlRelativeTimeFormat,
  Goccia.Values.IntlSegmenter,
  Goccia.Values.IteratorValue,
  Goccia.Values.MapValue,
  Goccia.Values.MockFunction,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TypedArrayValue,
  Goccia.Values.WeakMapValue,
  Goccia.Values.WeakSetValue;

const
  SNAPSHOT_INDENT_WIDTH = 2;
  SNAPSHOT_MAX_OUTPUT_LENGTH = 1 shl 27;
  LINE_FEED = #10;

type
  TGocciaSnapshotValueArray = array of TGocciaValue;

  TGocciaSnapshotFormatSettings = record
    CallToJSON: Boolean;
    EscapeRegex: Boolean;
    EscapeString: Boolean;
    Indent: string;
    MaxDepth: Double;
    MaxWidth: Double;
    Min: Boolean;
    PrintBasicPrototype: Boolean;
    PrintFunctionName: Boolean;
    SpacingInner: string;
    SpacingOuter: string;
    Plugins: TGocciaSnapshotValueArray;
  end;

  TGocciaVitestSnapshotFormatterInternal = class;

  TGocciaSnapshotPluginBridge = class
  private
    FFormatter: TGocciaVitestSnapshotFormatterInternal;
    FSettings: TGocciaSnapshotFormatSettings;
    FIndentation: string;
    FDepth: Integer;
    FReferences: TGocciaSnapshotValueArray;
    function ReadSettings(const AValue: TGocciaValue):
      TGocciaSnapshotFormatSettings;
    function ReadReferences(const AValue: TGocciaValue):
      TGocciaSnapshotValueArray;
  public
    constructor Create(const AFormatter: TGocciaVitestSnapshotFormatterInternal;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray);
    function PrintValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function PrintOldPluginValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function IndentText(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaVitestSnapshotFormatterInternal = class
  private
    FSerializers: TGocciaSnapshotSerializerRegistry;
    FDateToISOString: TGocciaValue;
    FBuiltinSerializers: array[0..6] of TGocciaObjectValue;
    FBuiltinSerializerRooted: array[0..6] of Boolean;

    function DefaultSettings: TGocciaSnapshotFormatSettings;
    function CreateBuiltinSerializer: TGocciaObjectValue;
    function BuiltinSerializerTest(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BuiltinSerializerSerialize(
      const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FormatAsymmetric(
      const AMatcher: TGocciaAsymmetricMatcherValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatRecursive(const AValue: TGocciaValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray;
      const AHasCalledToJSON: Boolean): string;
    function TryFormatWithSerializer(const AValue: TGocciaValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray;
      out AFormatted: string): Boolean;
    function FormatArray(const AArray: TGocciaArrayValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatArguments(const AArgumentsObject: TGocciaObjectValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatObject(const AObject: TGocciaObjectValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatMap(const AMap: TGocciaMapValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatSet(const ASet: TGocciaSetValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatMock(const AMock: TGocciaMockFunctionValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatTypedArray(const AArray: TGocciaTypedArrayValue;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer;
      const AReferences: TGocciaSnapshotValueArray): string;
    function FormatBytes(const AName: string; const ABytes: TBytes;
      const AStart, ACount: Integer;
      const ASettings: TGocciaSnapshotFormatSettings;
      const AIndentation: string; const ADepth: Integer): string;
    function MakeConfigObject(const ASettings: TGocciaSnapshotFormatSettings):
      TGocciaObjectValue;
    function MakeReferencesArray(const AReferences: TGocciaSnapshotValueArray):
      TGocciaArrayValue;
  public
    constructor Create(const ASerializers: TGocciaSnapshotSerializerRegistry;
      const ADateToISOString: TGocciaValue);
    destructor Destroy; override;
    function Format(const AValue: TGocciaValue): string;
  end;

function NormalizeNewlines(const AText: string): string;
begin
  Result := StringReplace(AText, #13#10, LINE_FEED, [rfReplaceAll]);
  Result := StringReplace(Result, #13, LINE_FEED, [rfReplaceAll]);
end;

function AppendReference(const AReferences: TGocciaSnapshotValueArray;
  const AValue: TGocciaValue): TGocciaSnapshotValueArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AReferences) + 1);
  for I := 0 to High(AReferences) do
    Result[I] := AReferences[I];
  Result[High(Result)] := AValue;
end;

function ContainsReference(const AReferences: TGocciaSnapshotValueArray;
  const AValue: TGocciaValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AReferences) do
    if AReferences[I] = AValue then
      Exit(True);
  Result := False;
end;

function QuoteString(const AValue: string;
  const AEscapeString: Boolean): string;
var
  Buffer: TStringBuffer;
  I: Integer;
begin
  Buffer := TStringBuffer.Create(Length(AValue) + 2);
  Buffer.AppendChar('"');
  for I := 1 to Length(AValue) do
  begin
    if AEscapeString and ((AValue[I] = '"') or (AValue[I] = '\')) then
      Buffer.AppendChar('\');
    Buffer.AppendChar(AnsiChar(AValue[I]));
  end;
  Buffer.AppendChar('"');
  Result := Buffer.ToString;
end;

function EscapeRegExp(const AValue: string): string;
var
  Buffer: TStringBuffer;
  I: Integer;
begin
  Buffer := TStringBuffer.Create(Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    if Pos(AValue[I], '$()*+.?[\]^{|}') > 0 then
      Buffer.AppendChar('\');
    Buffer.AppendChar(AnsiChar(AValue[I]));
  end;
  Result := Buffer.ToString;
end;

function HasDateToStringTag(const AObject: TGocciaObjectValue): Boolean;
var
  TagValue: TGocciaValue;
begin
  TagValue := AObject.GetSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag);
  Result := (TagValue is TGocciaStringLiteralValue) and
    (TGocciaStringLiteralValue(TagValue).Value = 'Date');
end;

function ObjectDisplayName(const AObject: TGocciaObjectValue): string;
var
  InstanceValue: TGocciaInstanceValue;
  Tag: string;
begin
  if AObject is TGocciaBooleanObjectValue then
    Exit('Boolean');
  if AObject is TGocciaNumberObjectValue then
    Exit('Number');
  if AObject is TGocciaStringObjectValue then
    Exit('String');
  if AObject is TGocciaBigIntObjectValue then
    Exit('BigInt');
  if AObject is TGocciaIntlCollatorValue then
    Exit('Collator');
  if AObject is TGocciaIntlDateTimeFormatValue then
    Exit('DateTimeFormat');
  if AObject is TGocciaIntlDisplayNamesValue then
    Exit('DisplayNames');
  if AObject is TGocciaIntlDurationFormatValue then
    Exit('DurationFormat');
  if AObject is TGocciaIntlListFormatValue then
    Exit('ListFormat');
  if AObject is TGocciaIntlLocaleValue then
    Exit('Locale');
  if AObject is TGocciaIntlNumberFormatValue then
    Exit('NumberFormat');
  if AObject is TGocciaIntlPluralRulesValue then
    Exit('PluralRules');
  if AObject is TGocciaIntlRelativeTimeFormatValue then
    Exit('RelativeTimeFormat');
  if AObject is TGocciaIntlSegmenterValue then
    Exit('Segmenter');
  if (AObject is TGocciaGeneratorBaseValue) or
     (AObject is TGocciaAsyncGeneratorBaseValue) then
    Exit('Object');
  if AObject is TGocciaIteratorValue then
    Exit('Iterator');
  if AObject is TGocciaInstanceValue then
  begin
    InstanceValue := TGocciaInstanceValue(AObject);
    if Assigned(InstanceValue.ClassValue) and
       (InstanceValue.ClassValue.Name <> '') then
      Exit(InstanceValue.ClassValue.Name);
  end;
  Tag := AObject.ToStringTag;
  if Pos('Iterator', Tag) > 0 then
    Exit('Iterator');
  if Tag = '' then
    Result := 'Object'
  else
    Result := Tag;
end;

function TGocciaSnapshotSerializerRegistry.Add(
  const ASerializer: TGocciaValue): Boolean;
var
  I: Integer;
begin
  SetLength(FSerializers, Length(FSerializers) + 1);
  for I := High(FSerializers) downto 1 do
    FSerializers[I] := FSerializers[I - 1];
  FSerializers[0] := ASerializer;
  Result := True;
end;

procedure TGocciaSnapshotSerializerRegistry.Clear;
begin
  SetLength(FSerializers, 0);
end;

function TGocciaSnapshotSerializerRegistry.Count: Integer;
begin
  Result := Length(FSerializers);
end;

function TGocciaSnapshotSerializerRegistry.SerializerAt(
  const AIndex: Integer): TGocciaValue;
begin
  Result := FSerializers[AIndex];
end;

procedure TGocciaSnapshotSerializerRegistry.MarkReferences;
var
  Serializer: TGocciaValue;
begin
  for Serializer in FSerializers do
    Serializer.MarkReferences;
end;

constructor TGocciaSnapshotFormatting.Create(
  const AFormatter: IGocciaSnapshotFormatter);
begin
  inherited Create;
  FSerializers := TGocciaSnapshotSerializerRegistry.Create;
  if Assigned(AFormatter) then
  begin
    FFormatter := AFormatter
  end
  else
  begin
    FDefaultFormatter := TGocciaVitestSnapshotFormatter.Create;
    FFormatter := FDefaultFormatter;
  end;
end;

destructor TGocciaSnapshotFormatting.Destroy;
begin
  FFormatter := nil;
  FSerializers.Free;
  inherited;
end;

function TGocciaSnapshotFormatting.Format(const AValue: TGocciaValue): string;
begin
  Result := FFormatter.Format(AValue, FSerializers);
end;

procedure TGocciaSnapshotFormatting.SetFormatter(
  const AFormatter: IGocciaSnapshotFormatter);
begin
  if Assigned(AFormatter) then
  begin
    FDefaultFormatter := nil;
    FFormatter := AFormatter
  end
  else
  begin
    FDefaultFormatter := TGocciaVitestSnapshotFormatter.Create;
    FFormatter := FDefaultFormatter;
  end;
end;

procedure TGocciaSnapshotFormatting.CaptureDateIntrinsics(
  const AGlobalObject: TGocciaObjectValue);
begin
  if Assigned(FDefaultFormatter) then
    FDefaultFormatter.CaptureDateIntrinsics(AGlobalObject);
end;

procedure TGocciaVitestSnapshotFormatter.CaptureDateIntrinsics(
  const AGlobalObject: TGocciaObjectValue);
var
  ConstructorValue, PrototypeValue: TGocciaValue;
begin
  if FDateToISOStringRooted and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveTempRoot(FDateToISOString);
  FDateToISOString := nil;
  FDateToISOStringRooted := False;
  if not Assigned(AGlobalObject) then
    Exit;
  ConstructorValue := AGlobalObject.GetProperty('Date');
  if not (ConstructorValue is TGocciaObjectValue) then
    Exit;
  PrototypeValue := TGocciaObjectValue(ConstructorValue).GetProperty(
    'prototype');
  if not (PrototypeValue is TGocciaObjectValue) then
    Exit;
  FDateToISOString := TGocciaObjectValue(PrototypeValue).GetProperty(
    'toISOString');
  if Assigned(TGarbageCollector.Instance) then
  begin
    if Assigned(FDateToISOString) and
       not TGarbageCollector.Instance.IsTempRoot(FDateToISOString) then
    begin
      TGarbageCollector.Instance.AddTempRoot(FDateToISOString);
      FDateToISOStringRooted := True;
    end;
  end;
end;

destructor TGocciaVitestSnapshotFormatter.Destroy;
begin
  if FDateToISOStringRooted and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveTempRoot(FDateToISOString);
  inherited;
end;

function TGocciaVitestSnapshotFormatter.Format(const AValue: TGocciaValue;
  const ASerializers: TGocciaSnapshotSerializerRegistry): string;
var
  InternalFormatter: TGocciaVitestSnapshotFormatterInternal;
begin
  InternalFormatter := TGocciaVitestSnapshotFormatterInternal.Create(
    ASerializers, FDateToISOString);
  try
    Result := InternalFormatter.Format(AValue);
  finally
    InternalFormatter.Free;
  end;
end;

constructor TGocciaVitestSnapshotFormatterInternal.Create(
  const ASerializers: TGocciaSnapshotSerializerRegistry;
  const ADateToISOString: TGocciaValue);
var
  I: Integer;
begin
  inherited Create;
  FSerializers := ASerializers;
  FDateToISOString := ADateToISOString;
  for I := 0 to High(FBuiltinSerializers) do
  begin
    FBuiltinSerializers[I] := CreateBuiltinSerializer;
    FBuiltinSerializerRooted[I] := False;
    if Assigned(TGarbageCollector.Instance) and
       not TGarbageCollector.Instance.IsTempRoot(FBuiltinSerializers[I]) then
    begin
      TGarbageCollector.Instance.AddTempRoot(FBuiltinSerializers[I]);
      FBuiltinSerializerRooted[I] := True;
    end;
  end;
end;

destructor TGocciaVitestSnapshotFormatterInternal.Destroy;
var
  I: Integer;
begin
  if Assigned(TGarbageCollector.Instance) then
    for I := 0 to High(FBuiltinSerializers) do
      if FBuiltinSerializerRooted[I] then
        TGarbageCollector.Instance.RemoveTempRoot(FBuiltinSerializers[I]);
  inherited;
end;

function TGocciaVitestSnapshotFormatterInternal.CreateBuiltinSerializer:
  TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.CreateDataPropertyOrThrow('test',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(BuiltinSerializerTest,
      'test', 1));
  Result.CreateDataPropertyOrThrow('serialize',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      BuiltinSerializerSerialize, 'serialize', 6));
end;

function TGocciaVitestSnapshotFormatterInternal.BuiltinSerializerTest(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaVitestSnapshotFormatterInternal.BuiltinSerializerSerialize(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('');
end;

function TGocciaVitestSnapshotFormatterInternal.DefaultSettings:
  TGocciaSnapshotFormatSettings;
var
  BuiltinIndex, I, PluginCount: Integer;
begin
  Result.CallToJSON := True;
  Result.EscapeRegex := True;
  Result.EscapeString := False;
  Result.Indent := StringOfChar(' ', SNAPSHOT_INDENT_WIDTH);
  Result.MaxDepth := Infinity;
  Result.MaxWidth := Infinity;
  Result.Min := False;
  Result.PrintBasicPrototype := False;
  Result.PrintFunctionName := False;
  Result.SpacingInner := LINE_FEED;
  Result.SpacingOuter := LINE_FEED;
  if Assigned(FSerializers) then
    PluginCount := FSerializers.Count
  else
    PluginCount := 0;
  SetLength(Result.Plugins, PluginCount + Length(FBuiltinSerializers));
  if Assigned(FSerializers) then
    for I := 0 to FSerializers.Count - 1 do
      Result.Plugins[I] := FSerializers.SerializerAt(I);
  for BuiltinIndex := 0 to High(FBuiltinSerializers) do
    Result.Plugins[PluginCount + BuiltinIndex] :=
      FBuiltinSerializers[BuiltinIndex];
end;

function TGocciaVitestSnapshotFormatterInternal.Format(
  const AValue: TGocciaValue): string;
var
  References: TGocciaSnapshotValueArray;
begin
  SetLength(References, 0);
  Result := NormalizeNewlines(FormatRecursive(AValue, DefaultSettings, '', 0,
    References, False));
end;

function TGocciaVitestSnapshotFormatterInternal.FormatRecursive(
  const AValue: TGocciaValue; const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray;
  const AHasCalledToJSON: Boolean): string;
var
  ObjectValue: TGocciaObjectValue;
  ToJSONFunction, JSONValue, NameValue, MessageValue, TimeValue,
    ToISOStringFunction: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  NextReferences: TGocciaSnapshotValueArray;
  DisplayName, Message: string;
  JSONRoot: TGocciaTempRoot;
begin
  if TryFormatWithSerializer(AValue, ASettings, AIndentation, ADepth,
    AReferences, Result) then
    Exit;

  if AValue is TGocciaAsymmetricMatcherValue then
    Exit(FormatAsymmetric(TGocciaAsymmetricMatcherValue(AValue), ASettings,
      AIndentation, ADepth, AReferences));

  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) or
     (AValue is TGocciaHoleValue) then
    Exit('undefined');
  if AValue is TGocciaNullLiteralValue then
    Exit('null');
  if AValue is TGocciaBooleanLiteralValue then
    Exit(LowerCase(BoolToStr(TGocciaBooleanLiteralValue(AValue).Value, True)));
  if AValue is TGocciaNumberLiteralValue then
  begin
    if TGocciaNumberLiteralValue(AValue).IsNegativeZero then
      Exit('-0');
    Exit(FormatDouble(TGocciaNumberLiteralValue(AValue).Value));
  end;
  if AValue is TGocciaBigIntValue then
    Exit(TGocciaBigIntValue(AValue).ToStringLiteral.Value + 'n');
  if AValue is TGocciaStringLiteralValue then
    Exit(QuoteString(TGocciaStringLiteralValue(AValue).Value,
      ASettings.EscapeString));
  if AValue is TGocciaSymbolValue then
    Exit(TGocciaSymbolValue(AValue).ToDisplayString.Value);
  if AValue is TGocciaMockFunctionValue then
    Exit(FormatMock(TGocciaMockFunctionValue(AValue), ASettings,
      AIndentation, ADepth, AReferences));
  if AValue.IsCallable then
    Exit('[Function]');
  if AValue is TGocciaSymbolObjectValue then
    Exit(TGocciaSymbolValue(
      TGocciaSymbolObjectValue(AValue).SymbolData).ToDisplayString.Value);
  if AValue is TGocciaWeakMapValue then
    Exit('WeakMap {}');
  if AValue is TGocciaWeakSetValue then
    Exit('WeakSet {}');

  if not (AValue is TGocciaObjectValue) then
    Exit(AValue.ToStringLiteral.Value);
  ObjectValue := TGocciaObjectValue(AValue);

  if ObjectValue.HasErrorData then
  begin
    NameValue := ObjectValue.GetProperty('name');
    MessageValue := ObjectValue.GetProperty('message');
    if Assigned(NameValue) and not (NameValue is TGocciaUndefinedLiteralValue) then
      DisplayName := NameValue.ToStringLiteral.Value
    else
      DisplayName := 'Error';
    if Assigned(MessageValue) and
       not (MessageValue is TGocciaUndefinedLiteralValue) then
      Message := MessageValue.ToStringLiteral.Value
    else
      Message := '';
    if DisplayName = '' then
      Exit('[' + Message + ']');
    if Message = '' then
      Exit('[' + DisplayName + ']');
    Exit('[' + DisplayName + ': ' + Message + ']');
  end;

  if ObjectValue.HasRegExpData then
  begin
    Result := RegExpObjectToString(ObjectValue);
    if ASettings.EscapeRegex then
      Result := EscapeRegExp(Result);
    Exit;
  end;

  { Date is implemented by the standard shim as a branded class instance.
    Its toStringTag is the only host-visible brand surface. }
  if (ObjectDisplayName(ObjectValue) = 'Date') or
     HasDateToStringTag(ObjectValue) then
  begin
    { pretty-format checks Number.isNaN(+value), so a user-defined valueOf
      participates in invalid-date detection even though ISO rendering uses
      the captured intrinsic. }
    if ObjectValue.ToNumberLiteral.IsNaN then
      Exit('Date { NaN }');
    ToISOStringFunction := FDateToISOString;
    if not Assigned(ToISOStringFunction) or
       not ToISOStringFunction.IsCallable then
      ToISOStringFunction := ObjectValue.GetProperty('toISOString');
    if Assigned(ToISOStringFunction) and ToISOStringFunction.IsCallable then
    begin
      Arguments := TGocciaArgumentsCollection.Create;
      try
        Exit(DispatchCall(ToISOStringFunction, Arguments,
          ObjectValue).ToStringLiteral.Value);
      finally
        Arguments.Free;
      end;
    end;
  end;

  if ContainsReference(AReferences, AValue) then
    Exit('[Circular]');
  NextReferences := AppendReference(AReferences, AValue);
  if ADepth + 1 > ASettings.MaxDepth then
    Exit('[' + ObjectDisplayName(ObjectValue) + ']');

  if ASettings.CallToJSON and not AHasCalledToJSON then
  begin
    ToJSONFunction := ObjectValue.GetProperty('toJSON');
    if Assigned(ToJSONFunction) and ToJSONFunction.IsCallable then
    begin
      Arguments := TGocciaArgumentsCollection.Create;
      try
        JSONValue := DispatchCall(ToJSONFunction, Arguments, ObjectValue);
      finally
        Arguments.Free;
      end;
      InitializeTempRoot(JSONRoot);
      AddTempRootIfNeeded(JSONRoot, JSONValue);
      try
        Exit(FormatRecursive(JSONValue, ASettings, AIndentation, ADepth + 1,
          NextReferences, True));
      finally
        RemoveTempRootIfNeeded(JSONRoot);
      end;
    end;
  end;

  if AValue is TGocciaArrayValue then
    Exit(FormatArray(TGocciaArrayValue(AValue), ASettings, AIndentation,
      ADepth, NextReferences));
  if IsArgumentsObjectValue(AValue) then
    Exit(FormatArguments(ObjectValue, ASettings, AIndentation, ADepth,
      NextReferences));
  if AValue is TGocciaMapValue then
    Exit(FormatMap(TGocciaMapValue(AValue), ASettings, AIndentation,
      ADepth, NextReferences));
  if AValue is TGocciaSetValue then
    Exit(FormatSet(TGocciaSetValue(AValue), ASettings, AIndentation,
      ADepth, NextReferences));
  if AValue is TGocciaTypedArrayValue then
    Exit(FormatTypedArray(TGocciaTypedArrayValue(AValue), ASettings,
      AIndentation, ADepth, NextReferences));
  if AValue is TGocciaArrayBufferValue then
    Exit(FormatBytes('ArrayBuffer', TGocciaArrayBufferValue(AValue).Data, 0,
      Length(TGocciaArrayBufferValue(AValue).Data), ASettings, AIndentation,
      ADepth));
  if AValue is TGocciaDataViewValue then
  begin
    Arguments := TGocciaArgumentsCollection.Create;
    try
      TimeValue := TGocciaDataViewValue(AValue).DataViewByteLengthGetter(
        Arguments, AValue);
    finally
      Arguments.Free;
    end;
    if not (TimeValue is TGocciaNumberLiteralValue) then
      Exit('DataView []');
    if TGocciaDataViewValue(AValue).BufferValue is TGocciaArrayBufferValue then
      Exit(FormatBytes('DataView',
        TGocciaArrayBufferValue(TGocciaDataViewValue(AValue).BufferValue).Data,
        TGocciaDataViewValue(AValue).ByteOffset,
        Trunc(TGocciaNumberLiteralValue(TimeValue).Value), ASettings,
        AIndentation, ADepth));
    if TGocciaDataViewValue(AValue).BufferValue is TGocciaSharedArrayBufferValue then
      Exit(FormatBytes('DataView', TGocciaSharedArrayBufferValue(
        TGocciaDataViewValue(AValue).BufferValue).Data,
        TGocciaDataViewValue(AValue).ByteOffset,
        Trunc(TGocciaNumberLiteralValue(TimeValue).Value), ASettings,
        AIndentation, ADepth));
  end;
  Exit(FormatObject(ObjectValue, ASettings, AIndentation, ADepth,
    NextReferences));
end;

function TGocciaVitestSnapshotFormatterInternal.FormatAsymmetric(
  const AMatcher: TGocciaAsymmetricMatcherValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Issues, ValidationResult: TGocciaValue;
begin
  if (AMatcher is TGocciaArrayContainingMatcherValue) or
     (AMatcher is TGocciaObjectContainingMatcherValue) or
     (AMatcher is TGocciaStringContainingMatcherValue) or
     (AMatcher is TGocciaStringMatchingMatcherValue) then
    Exit(AMatcher.MatcherName + ' ' + FormatRecursive(AMatcher.Sample,
      ASettings, AIndentation, ADepth, AReferences, False));

  if AMatcher is TGocciaSchemaMatchingMatcherValue then
  begin
    ValidationResult := TGocciaSchemaMatchingMatcherValue(
      AMatcher).ValidationResult;
    if ValidationResult is TGocciaObjectValue then
    begin
      Issues := TGocciaObjectValue(ValidationResult).GetProperty('issues');
      if (Issues is TGocciaArrayValue) and
         (TGocciaArrayValue(Issues).Elements.Count > 0) then
        Exit(AMatcher.MatcherName + ' ' + FormatRecursive(ValidationResult,
          ASettings, AIndentation, ADepth, AReferences, False));
    end;
    Exit(AMatcher.MatcherName);
  end;

  Result := AMatcher.AsymmetricDisplay;
end;

function TGocciaVitestSnapshotFormatterInternal.FormatArguments(
  const AArgumentsObject: TGocciaObjectValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Buffer: TStringBuffer;
  I, LengthValue: Integer;
  NextIndentation: string;
  RawLength: TGocciaValue;
begin
  RawLength := AArgumentsObject.GetProperty('length');
  if RawLength is TGocciaNumberLiteralValue then
    LengthValue := Max(0, Trunc(TGocciaNumberLiteralValue(RawLength).Value))
  else
    LengthValue := 0;
  if LengthValue = 0 then
    Exit('Arguments []');
  NextIndentation := AIndentation + ASettings.Indent;
  Buffer := TStringBuffer.Create;
  Buffer.Append('Arguments [' + ASettings.SpacingOuter);
  for I := 0 to LengthValue - 1 do
  begin
    Buffer.Append(NextIndentation);
    if I = ASettings.MaxWidth then
    begin
      Buffer.Append('…(' + IntToStr(LengthValue - I) + ')' +
        ASettings.SpacingOuter);
      Break;
    end;
    Buffer.Append(FormatRecursive(AArgumentsObject.GetProperty(IntToStr(I)),
      ASettings, NextIndentation, ADepth + 1, AReferences, False));
    Buffer.Append(',' + ASettings.SpacingInner);
  end;
  Buffer.Append(AIndentation + ']');
  Result := Buffer.ToString;
end;

function TGocciaVitestSnapshotFormatterInternal.FormatArray(
  const AArray: TGocciaArrayValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Buffer: TStringBuffer;
  Element: TGocciaValue;
  I: Integer;
  DisplayName, NextIndentation, Prefix: string;
begin
  DisplayName := ObjectDisplayName(AArray);
  if ASettings.Min or
     ((not ASettings.PrintBasicPrototype) and (DisplayName = 'Array')) then
    Prefix := ''
  else
    Prefix := DisplayName + ' ';
  if AArray.Elements.Count = 0 then
    Exit(Prefix + '[]');
  NextIndentation := AIndentation + ASettings.Indent;
  Buffer := TStringBuffer.Create;
  Buffer.Append(Prefix + '[' + ASettings.SpacingOuter);
  for I := 0 to AArray.Elements.Count - 1 do
  begin
    Buffer.Append(NextIndentation);
    if I = ASettings.MaxWidth then
    begin
      Buffer.Append('…(' + IntToStr(AArray.Elements.Count - I) + ')');
      Break;
    end;
    Element := AArray.Elements[I];
    if not (Element is TGocciaHoleValue) then
      Buffer.Append(FormatRecursive(Element, ASettings, NextIndentation,
        ADepth + 1, AReferences, False));
    if I < AArray.Elements.Count - 1 then
      Buffer.Append(',' + ASettings.SpacingInner)
    else if not ASettings.Min then
      Buffer.Append(',');
  end;
  Buffer.Append(ASettings.SpacingOuter + AIndentation + ']');
  Result := Buffer.ToString;
end;

function TGocciaVitestSnapshotFormatterInternal.FormatObject(
  const AObject: TGocciaObjectValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Buffer: TStringBuffer;
  Entries: TArray<TPair<string, TGocciaValue>>;
  Symbols: TArray<TPair<TGocciaSymbolValue, TGocciaValue>>;
  CurrentEntry: TPair<string, TGocciaValue>;
  TemporaryEntry: TPair<string, TGocciaValue>;
  DisplayName, Prefix, NextIndentation: string;
  I, J, UniqueCount, Width: Integer;
  SeenNames: TDictionary<string, Boolean>;
begin
  Entries := AObject.GetEnumerablePropertyEntries;
  SeenNames := TDictionary<string, Boolean>.Create;
  try
    UniqueCount := 0;
    for I := 0 to High(Entries) do
      if not SeenNames.ContainsKey(Entries[I].Key) then
      begin
        SeenNames.Add(Entries[I].Key, True);
        Entries[UniqueCount] := Entries[I];
        Inc(UniqueCount);
      end;
    SetLength(Entries, UniqueCount);
  finally
    SeenNames.Free;
  end;
  Symbols := AObject.GetEnumerableSymbolProperties;
  for I := 1 to High(Entries) do
  begin
    TemporaryEntry := Entries[I];
    J := I - 1;
    while (J >= 0) and (Entries[J].Key > TemporaryEntry.Key) do
    begin
      Entries[J + 1] := Entries[J];
      Dec(J);
    end;
    Entries[J + 1] := TemporaryEntry;
  end;

  DisplayName := ObjectDisplayName(AObject);
  if ASettings.Min or
     ((not ASettings.PrintBasicPrototype) and (DisplayName = 'Object')) then
    Prefix := ''
  else
    Prefix := DisplayName + ' ';
  if (Length(Entries) = 0) and (Length(Symbols) = 0) then
    Exit(Prefix + '{}');

  NextIndentation := AIndentation + ASettings.Indent;
  Buffer := TStringBuffer.Create;
  Buffer.Append(Prefix + '{' + ASettings.SpacingOuter);
  Width := 0;
  for CurrentEntry in Entries do
  begin
    Buffer.Append(NextIndentation);
    if Width = ASettings.MaxWidth then
    begin
      Buffer.Append('…(' + IntToStr(Length(Entries) + Length(Symbols) -
        Width) + ')' + ASettings.SpacingOuter);
      Break;
    end;
    Buffer.Append(QuoteString(CurrentEntry.Key,
      ASettings.EscapeString) + ': ');
    Buffer.Append(FormatRecursive(CurrentEntry.Value, ASettings,
      NextIndentation, ADepth + 1, AReferences, False));
    Buffer.Append(',' + ASettings.SpacingInner);
    Inc(Width);
  end;
  if Width = Length(Entries) then
    for I := 0 to High(Symbols) do
    begin
      Buffer.Append(NextIndentation);
      if Width = ASettings.MaxWidth then
      begin
        Buffer.Append('…(' + IntToStr(Length(Entries) + Length(Symbols) -
          Width) + ')' + ASettings.SpacingOuter);
        Break;
      end;
      Buffer.Append(Symbols[I].Key.ToDisplayString.Value + ': ');
      Buffer.Append(FormatRecursive(Symbols[I].Value, ASettings,
        NextIndentation, ADepth + 1, AReferences, False));
      Buffer.Append(',' + ASettings.SpacingInner);
      Inc(Width);
    end;
  Buffer.Append(AIndentation + '}');
  Result := Buffer.ToString;
end;

function TGocciaVitestSnapshotFormatterInternal.FormatMap(
  const AMap: TGocciaMapValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Buffer: TStringBuffer;
  Cursor, Width: Integer;
  Key, Value: TGocciaValue;
  NextIndentation: string;
begin
  if AMap.Count = 0 then
    Exit('Map {}');
  NextIndentation := AIndentation + ASettings.Indent;
  Buffer := TStringBuffer.Create;
  Buffer.Append('Map {' + ASettings.SpacingOuter);
  Cursor := 0;
  Width := 0;
  AMap.RetainIterator;
  try
    while AMap.NextEntry(Cursor, Key, Value) do
    begin
      Buffer.Append(NextIndentation);
      if Width = ASettings.MaxWidth then
      begin
        Buffer.Append('…(' + IntToStr(AMap.Count - Width) + ')' +
          ASettings.SpacingOuter);
        Break;
      end;
      Buffer.Append(FormatRecursive(Key, ASettings, NextIndentation,
        ADepth + 1, AReferences, False));
      Buffer.Append(' => ');
      Buffer.Append(FormatRecursive(Value, ASettings, NextIndentation,
        ADepth + 1, AReferences, False));
      Buffer.Append(',' + ASettings.SpacingInner);
      Inc(Width);
    end;
  finally
    AMap.ReleaseIterator;
  end;
  Buffer.Append(AIndentation + '}');
  Result := Buffer.ToString;
end;

function TGocciaVitestSnapshotFormatterInternal.FormatSet(
  const ASet: TGocciaSetValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Buffer: TStringBuffer;
  Cursor, Width: Integer;
  Value: TGocciaValue;
  NextIndentation: string;
begin
  if ASet.Count = 0 then
    Exit('Set {}');
  NextIndentation := AIndentation + ASettings.Indent;
  Buffer := TStringBuffer.Create;
  Buffer.Append('Set {' + ASettings.SpacingOuter);
  Cursor := 0;
  Width := 0;
  ASet.RetainIterator;
  try
    while ASet.NextItem(Cursor, Value) do
    begin
      Buffer.Append(NextIndentation);
      if Width = ASettings.MaxWidth then
      begin
        Buffer.Append('…(' + IntToStr(ASet.Count - Width) + ')' +
          ASettings.SpacingOuter);
        Break;
      end;
      Buffer.Append(FormatRecursive(Value, ASettings, NextIndentation,
        ADepth + 1, AReferences, False));
      Buffer.Append(',' + ASettings.SpacingInner);
      Inc(Width);
    end;
  finally
    ASet.ReleaseIterator;
  end;
  Buffer.Append(AIndentation + '}');
  Result := Buffer.ToString;
end;

function TGocciaVitestSnapshotFormatterInternal.FormatMock(
  const AMock: TGocciaMockFunctionValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Arguments: TGocciaArgumentsCollection;
  CallsValue, MockObject, NameValue, ResultsValue: TGocciaValue;
  Name, NameSuffix, NextIndentation: string;
begin
  Arguments := TGocciaArgumentsCollection.Create;
  try
    NameValue := AMock.DoGetMockName(Arguments, AMock);
  finally
    Arguments.Free;
  end;
  Name := NameValue.ToStringLiteral.Value;
  if Name = 'mock' then
    NameSuffix := ''
  else
    NameSuffix := ' ' + Name;
  if AMock.MockCalls.Count = 0 then
    Exit('[MockFunction' + NameSuffix + ']');

  MockObject := AMock.GetProperty('mock');
  CallsValue := TGocciaObjectValue(MockObject).GetProperty('calls');
  ResultsValue := TGocciaObjectValue(MockObject).GetProperty('results');
  NextIndentation := AIndentation + ASettings.Indent;
  Result := '[MockFunction' + NameSuffix + '] {' + ASettings.SpacingOuter +
    NextIndentation + '"calls": ' + FormatRecursive(CallsValue, ASettings,
      NextIndentation, ADepth, AReferences, False) + ',' +
    ASettings.SpacingOuter + NextIndentation + '"results": ' +
    FormatRecursive(ResultsValue, ASettings, NextIndentation, ADepth,
      AReferences, False);
  if not ASettings.Min then
    Result := Result + ',';
  Result := Result + ASettings.SpacingOuter + AIndentation + '}';
end;

function TGocciaVitestSnapshotFormatterInternal.FormatTypedArray(
  const AArray: TGocciaTypedArrayValue;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray): string;
var
  Buffer: TStringBuffer;
  Element: TGocciaValue;
  I: Integer;
  NextIndentation, Name: string;
begin
  Name := ObjectDisplayName(AArray);
  if TGocciaTypedArrayValue.IsBigIntKind(AArray.Kind) or
     (AArray.Kind = takFloat16) then
  begin
    if AArray.Length = 0 then
      Exit(Name + ' {}');
    NextIndentation := AIndentation + ASettings.Indent;
    Buffer := TStringBuffer.Create;
    Buffer.Append(Name + ' {' + ASettings.SpacingOuter);
    for I := 0 to AArray.Length - 1 do
    begin
      Buffer.Append(NextIndentation);
      if I = ASettings.MaxWidth then
      begin
        Buffer.Append('…(' + IntToStr(AArray.Length - I) + ')' +
          ASettings.SpacingOuter);
        Break;
      end;
      Element := AArray.GetProperty(IntToStr(I));
      Buffer.Append(QuoteString(IntToStr(I),
        ASettings.EscapeString) + ': ');
      Buffer.Append(FormatRecursive(Element, ASettings, NextIndentation,
        ADepth + 1, AReferences, False));
      Buffer.Append(',' + ASettings.SpacingInner);
    end;
    Buffer.Append(AIndentation + '}');
    Exit(Buffer.ToString);
  end;
  if AArray.Length = 0 then
    Exit(Name + ' []');
  NextIndentation := AIndentation + ASettings.Indent;
  Buffer := TStringBuffer.Create;
  Buffer.Append(Name + ' [' + ASettings.SpacingOuter);
  for I := 0 to AArray.Length - 1 do
  begin
    Element := AArray.GetProperty(IntToStr(I));
    Buffer.Append(NextIndentation);
    if I = ASettings.MaxWidth then
    begin
      Buffer.Append('…(' + IntToStr(AArray.Length - I) + ')' +
        ASettings.SpacingOuter);
      Break;
    end;
    Buffer.Append(FormatRecursive(Element, ASettings, NextIndentation,
      ADepth + 1, AReferences, False));
    Buffer.Append(',' + ASettings.SpacingInner);
  end;
  Buffer.Append(AIndentation + ']');
  Result := Buffer.ToString;
end;

function TGocciaVitestSnapshotFormatterInternal.FormatBytes(
  const AName: string; const ABytes: TBytes; const AStart, ACount: Integer;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer): string;
var
  Buffer: TStringBuffer;
  ByteValue, I, LastIndex: Integer;
  NextIndentation: string;
begin
  if ACount <= 0 then
    Exit(AName + ' []');
  NextIndentation := AIndentation + ASettings.Indent;
  Buffer := TStringBuffer.Create;
  Buffer.Append(AName + ' [' + ASettings.SpacingOuter);
  LastIndex := Min(Length(ABytes), AStart + ACount) - 1;
  for I := Max(0, AStart) to LastIndex do
  begin
    Buffer.Append(NextIndentation);
    if (I - Max(0, AStart)) = ASettings.MaxWidth then
    begin
      Buffer.Append('…(' + IntToStr(LastIndex - I + 1) + ')' +
        ASettings.SpacingOuter);
      Break;
    end;
    ByteValue := ABytes[I];
    if ByteValue > High(ShortInt) then
      Dec(ByteValue, 256);
    Buffer.Append(IntToStr(ByteValue) + ',' +
      ASettings.SpacingInner);
  end;
  Buffer.Append(AIndentation + ']');
  Result := Buffer.ToString;
end;

function TGocciaVitestSnapshotFormatterInternal.MakeConfigObject(
  const ASettings: TGocciaSnapshotFormatSettings): TGocciaObjectValue;

const
  ColorNames: array[0..4] of string = (
    'comment', 'content', 'prop', 'tag', 'value'
  );

  procedure AddBoolean(const AName: string; const AValue: Boolean);
  begin
    Result.CreateDataPropertyOrThrow(AName,
      TGocciaBooleanLiteralValue.FromBoolean(AValue));
  end;

  procedure AddNumber(const AName: string; const AValue: Double);
  begin
    Result.CreateDataPropertyOrThrow(AName,
      TGocciaNumberLiteralValue.Create(AValue));
  end;

  procedure AddString(const AName, AValue: string);
  begin
    Result.CreateDataPropertyOrThrow(AName,
      TGocciaStringLiteralValue.Create(AValue));
  end;

var
  Colors, Color: TGocciaObjectValue;
  OutputLengths, Plugins: TGocciaArrayValue;
  ColorName: string;
  I, PluginCount: Integer;
  ConfigRoot, ColorsRoot, ColorRoot, OutputLengthsRoot,
    PluginsRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(ConfigRoot);
  InitializeTempRoot(ColorsRoot);
  InitializeTempRoot(ColorRoot);
  InitializeTempRoot(OutputLengthsRoot);
  InitializeTempRoot(PluginsRoot);
  Result := TGocciaObjectValue.Create;
  AddTempRootIfNeeded(ConfigRoot, Result);
  try
    AddBoolean('callToJSON', ASettings.CallToJSON);
    Colors := TGocciaObjectValue.Create;
    AddTempRootIfNeeded(ColorsRoot, Colors);
    for ColorName in ColorNames do
    begin
      Color := TGocciaObjectValue.Create;
      AddTempRootIfNeeded(ColorRoot, Color);
      Color.CreateDataPropertyOrThrow('open',
        TGocciaStringLiteralValue.Create(''));
      Color.CreateDataPropertyOrThrow('close',
        TGocciaStringLiteralValue.Create(''));
      Colors.CreateDataPropertyOrThrow(ColorName, Color);
      RemoveTempRootIfNeeded(ColorRoot);
    end;
    Result.CreateDataPropertyOrThrow('colors', Colors);
    Result.CreateDataPropertyOrThrow('compareKeys',
      TGocciaUndefinedLiteralValue.UndefinedValue);
    AddBoolean('escapeRegex', ASettings.EscapeRegex);
    AddBoolean('escapeString', ASettings.EscapeString);
    AddString('indent', ASettings.Indent);
    AddNumber('maxDepth', ASettings.MaxDepth);
    AddNumber('maxWidth', ASettings.MaxWidth);
    AddBoolean('min', ASettings.Min);
    PluginCount := Length(ASettings.Plugins);
    Plugins := TGocciaArrayValue.Create(nil, PluginCount);
    AddTempRootIfNeeded(PluginsRoot, Plugins);
    for I := 0 to High(ASettings.Plugins) do
      Plugins.Elements.Add(ASettings.Plugins[I]);
    Result.CreateDataPropertyOrThrow('plugins', Plugins);
    AddBoolean('printBasicPrototype', ASettings.PrintBasicPrototype);
    AddBoolean('printFunctionName', ASettings.PrintFunctionName);
    AddBoolean('printShadowRoot', True);
    AddString('spacingInner', ASettings.SpacingInner);
    AddString('spacingOuter', ASettings.SpacingOuter);
    AddBoolean('singleQuote', False);
    AddBoolean('quoteKeys', True);
    AddNumber('maxOutputLength', SNAPSHOT_MAX_OUTPUT_LENGTH);
    OutputLengths := TGocciaArrayValue.Create;
    AddTempRootIfNeeded(OutputLengthsRoot, OutputLengths);
    Result.CreateDataPropertyOrThrow('_outputLengthPerDepth', OutputLengths);
  finally
    RemoveTempRootIfNeeded(PluginsRoot);
    RemoveTempRootIfNeeded(OutputLengthsRoot);
    RemoveTempRootIfNeeded(ColorRoot);
    RemoveTempRootIfNeeded(ColorsRoot);
    RemoveTempRootIfNeeded(ConfigRoot);
  end;
end;

function TGocciaVitestSnapshotFormatterInternal.MakeReferencesArray(
  const AReferences: TGocciaSnapshotValueArray): TGocciaArrayValue;
var
  Reference: TGocciaValue;
begin
  Result := TGocciaArrayValue.Create(nil, Length(AReferences));
  for Reference in AReferences do
    Result.Elements.Add(Reference);
end;

function TGocciaVitestSnapshotFormatterInternal.TryFormatWithSerializer(
  const AValue: TGocciaValue; const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray;
  out AFormatted: string): Boolean;
var
  Serializer, ConfigObject, OptionsObject, ColorsObject: TGocciaObjectValue;
  SerializerValue: TGocciaValue;
  TestFunction, SerializeFunction, PrintFunction, PrintedValue: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  Bridge: TGocciaSnapshotPluginBridge;
  PrinterFunction, IndentFunction: TGocciaNativeFunctionValue;
  ReferencesArray: TGocciaArrayValue;
  ConfigRoot, ReferencesRoot, PrinterRoot, IndentRoot,
    OptionsRoot: TGocciaTempRoot;
  I: Integer;
begin
  Result := False;
  for I := 0 to High(ASettings.Plugins) do
  begin
    SerializerValue := ASettings.Plugins[I];
    if not (SerializerValue is TGocciaObjectValue) then
      raise Exception.Create('Snapshot serializer must be an object');
    Serializer := TGocciaObjectValue(SerializerValue);
    TestFunction := Serializer.GetProperty('test');
    if not Assigned(TestFunction) or not TestFunction.IsCallable then
      raise Exception.Create('Snapshot serializer test must be callable');
    Arguments := TGocciaArgumentsCollection.Create([AValue]);
    try
      if not DispatchCall(TestFunction, Arguments,
        Serializer).ToBooleanLiteral.Value then
        Continue;
    finally
      Arguments.Free;
    end;

    Bridge := TGocciaSnapshotPluginBridge.Create(Self, ASettings,
      AIndentation, ADepth, AReferences);
    InitializeTempRoot(ConfigRoot);
    InitializeTempRoot(ReferencesRoot);
    InitializeTempRoot(PrinterRoot);
    InitializeTempRoot(IndentRoot);
    InitializeTempRoot(OptionsRoot);
    try
      PrinterFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
        Bridge.PrintValue, 'printer', 1);
      AddTempRootIfNeeded(PrinterRoot, PrinterFunction);
      SerializeFunction := Serializer.GetProperty('serialize');
      if Assigned(SerializeFunction) and SerializeFunction.IsCallable then
      begin
        ConfigObject := MakeConfigObject(ASettings);
        ReferencesArray := MakeReferencesArray(AReferences);
        AddTempRootIfNeeded(ConfigRoot, ConfigObject);
        AddTempRootIfNeeded(ReferencesRoot, ReferencesArray);
        Arguments := TGocciaArgumentsCollection.Create([
          AValue,
          ConfigObject,
          TGocciaStringLiteralValue.Create(AIndentation),
          TGocciaNumberLiteralValue.Create(ADepth),
          ReferencesArray,
          PrinterFunction
        ]);
        try
          PrintedValue := DispatchCall(SerializeFunction, Arguments,
            Serializer);
        finally
          Arguments.Free;
        end;
      end
      else
      begin
        PrintFunction := Serializer.GetProperty('print');
        PrinterFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Bridge.PrintOldPluginValue, 'print', 1);
        IndentFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Bridge.IndentText, 'indent', 1);
        AddTempRootIfNeeded(PrinterRoot, PrinterFunction);
        AddTempRootIfNeeded(IndentRoot, IndentFunction);
        OptionsObject := TGocciaObjectValue.Create;
        AddTempRootIfNeeded(OptionsRoot, OptionsObject);
        OptionsObject.CreateDataPropertyOrThrow('edgeSpacing',
          TGocciaStringLiteralValue.Create(ASettings.SpacingOuter));
        OptionsObject.CreateDataPropertyOrThrow('min',
          TGocciaBooleanLiteralValue.FromBoolean(ASettings.Min));
        OptionsObject.CreateDataPropertyOrThrow('spacing',
          TGocciaStringLiteralValue.Create(ASettings.SpacingInner));
        ConfigObject := MakeConfigObject(ASettings);
        ColorsObject := TGocciaObjectValue(ConfigObject.GetProperty('colors'));
        AddTempRootIfNeeded(ConfigRoot, ConfigObject);
        Arguments := TGocciaArgumentsCollection.Create([
          AValue, PrinterFunction, IndentFunction, OptionsObject, ColorsObject
        ]);
        try
          PrintedValue := DispatchCall(PrintFunction, Arguments, Serializer);
        finally
          Arguments.Free;
        end;
      end;
      if not (PrintedValue is TGocciaStringLiteralValue) then
        ThrowTypeError('pretty-format: Plugin must return type "string"');
      AFormatted := TGocciaStringLiteralValue(PrintedValue).Value;
      Exit(True);
    finally
      RemoveTempRootIfNeeded(OptionsRoot);
      RemoveTempRootIfNeeded(IndentRoot);
      RemoveTempRootIfNeeded(PrinterRoot);
      RemoveTempRootIfNeeded(ReferencesRoot);
      RemoveTempRootIfNeeded(ConfigRoot);
      Bridge.Free;
    end;
  end;
end;

constructor TGocciaSnapshotPluginBridge.Create(
  const AFormatter: TGocciaVitestSnapshotFormatterInternal;
  const ASettings: TGocciaSnapshotFormatSettings;
  const AIndentation: string; const ADepth: Integer;
  const AReferences: TGocciaSnapshotValueArray);
begin
  inherited Create;
  FFormatter := AFormatter;
  FSettings := ASettings;
  FIndentation := AIndentation;
  FDepth := ADepth;
  FReferences := AReferences;
end;

function TGocciaSnapshotPluginBridge.ReadSettings(
  const AValue: TGocciaValue): TGocciaSnapshotFormatSettings;
var
  Config: TGocciaObjectValue;
  Plugins: TGocciaArrayValue;
  I: Integer;
  Value: TGocciaValue;
begin
  Result := FSettings;
  if not (AValue is TGocciaObjectValue) then
    Exit;
  Config := TGocciaObjectValue(AValue);
  Value := Config.GetProperty('callToJSON');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.CallToJSON := Value.ToBooleanLiteral.Value;
  Value := Config.GetProperty('escapeRegex');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.EscapeRegex := Value.ToBooleanLiteral.Value;
  Value := Config.GetProperty('escapeString');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.EscapeString := Value.ToBooleanLiteral.Value;
  Value := Config.GetProperty('indent');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.Indent := Value.ToStringLiteral.Value;
  Value := Config.GetProperty('maxDepth');
  if Value is TGocciaNumberLiteralValue then
    Result.MaxDepth := TGocciaNumberLiteralValue(Value).Value;
  Value := Config.GetProperty('maxWidth');
  if Value is TGocciaNumberLiteralValue then
    Result.MaxWidth := TGocciaNumberLiteralValue(Value).Value;
  Value := Config.GetProperty('min');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.Min := Value.ToBooleanLiteral.Value;
  Value := Config.GetProperty('plugins');
  if Value is TGocciaArrayValue then
  begin
    Plugins := TGocciaArrayValue(Value);
    SetLength(Result.Plugins, Plugins.Elements.Count);
    for I := 0 to Plugins.Elements.Count - 1 do
      Result.Plugins[I] := Plugins.Elements[I];
  end;
  Value := Config.GetProperty('printBasicPrototype');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.PrintBasicPrototype := Value.ToBooleanLiteral.Value;
  Value := Config.GetProperty('printFunctionName');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.PrintFunctionName := Value.ToBooleanLiteral.Value;
  Value := Config.GetProperty('spacingInner');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.SpacingInner := Value.ToStringLiteral.Value;
  Value := Config.GetProperty('spacingOuter');
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    Result.SpacingOuter := Value.ToStringLiteral.Value;
end;

function TGocciaSnapshotPluginBridge.ReadReferences(
  const AValue: TGocciaValue): TGocciaSnapshotValueArray;
var
  ReferencesArray: TGocciaArrayValue;
  I: Integer;
begin
  if not (AValue is TGocciaArrayValue) then
    Exit(FReferences);
  ReferencesArray := TGocciaArrayValue(AValue);
  SetLength(Result, ReferencesArray.Elements.Count);
  for I := 0 to ReferencesArray.Elements.Count - 1 do
    Result[I] := ReferencesArray.Elements[I];
end;

function TGocciaSnapshotPluginBridge.PrintValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Settings: TGocciaSnapshotFormatSettings;
  References: TGocciaSnapshotValueArray;
  Indentation: string;
  Depth: Integer;
  HasCalledToJSON: Boolean;
begin
  Settings := FSettings;
  Indentation := FIndentation;
  Depth := FDepth;
  References := FReferences;
  HasCalledToJSON := False;
  if AArgs.Length > 1 then
    Settings := ReadSettings(AArgs.GetElement(1));
  if AArgs.Length > 2 then
    Indentation := AArgs.GetElement(2).ToStringLiteral.Value;
  if (AArgs.Length > 3) and
     (AArgs.GetElement(3) is TGocciaNumberLiteralValue) then
    Depth := Trunc(TGocciaNumberLiteralValue(AArgs.GetElement(3)).Value);
  if AArgs.Length > 4 then
    References := ReadReferences(AArgs.GetElement(4));
  if AArgs.Length > 5 then
    HasCalledToJSON := AArgs.GetElement(5).ToBooleanLiteral.Value;
  Result := TGocciaStringLiteralValue.Create(FFormatter.FormatRecursive(
    AArgs.GetElement(0), Settings, Indentation, Depth, References,
    HasCalledToJSON));
end;

function TGocciaSnapshotPluginBridge.PrintOldPluginValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(FFormatter.FormatRecursive(
    AArgs.GetElement(0), FSettings, FIndentation, FDepth, FReferences, False));
end;

function TGocciaSnapshotPluginBridge.IndentText(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Indentation, Text: string;
begin
  Indentation := FIndentation + FSettings.Indent;
  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  Text := StringReplace(Text, LINE_FEED, LINE_FEED + Indentation,
    [rfReplaceAll]);
  Result := TGocciaStringLiteralValue.Create(Indentation + Text);
end;

end.
