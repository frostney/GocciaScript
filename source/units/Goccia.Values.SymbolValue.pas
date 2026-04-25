unit Goccia.Values.SymbolValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel.Types,
  Goccia.Values.Primitives;

type
  TGocciaSymbolValue = class(TGocciaValue)
  private class var
    FWellKnownIterator: TGocciaSymbolValue;
    FWellKnownMatch: TGocciaSymbolValue;
    FWellKnownMatchAll: TGocciaSymbolValue;
    FWellKnownReplace: TGocciaSymbolValue;
    FWellKnownSearch: TGocciaSymbolValue;
    FWellKnownSplit: TGocciaSymbolValue;
    FWellKnownSpecies: TGocciaSymbolValue;
    FWellKnownHasInstance: TGocciaSymbolValue;
    FWellKnownToPrimitive: TGocciaSymbolValue;
    FWellKnownToStringTag: TGocciaSymbolValue;
    FWellKnownIsConcatSpreadable: TGocciaSymbolValue;
    FWellKnownAsyncIterator: TGocciaSymbolValue;
    FWellKnownMetadata: TGocciaSymbolValue;
    FWellKnownDispose: TGocciaSymbolValue;
    FWellKnownAsyncDispose: TGocciaSymbolValue;
    FWellKnownUnscopables: TGocciaSymbolValue;
  private
    FDescription: string;
    FHasDescription: Boolean;
    FId: Integer;

  public
    constructor Create(const ADescription: string = ''); overload;
    constructor Create(const ADescription: string; const AHasDescription: Boolean); overload;
    procedure InitializePrototype;

    class function SharedPrototype: TGocciaValue;
    class function WellKnownIterator: TGocciaSymbolValue;
    class function WellKnownMatch: TGocciaSymbolValue;
    class function WellKnownMatchAll: TGocciaSymbolValue;
    class function WellKnownReplace: TGocciaSymbolValue;
    class function WellKnownSearch: TGocciaSymbolValue;
    class function WellKnownSplit: TGocciaSymbolValue;
    class function WellKnownSpecies: TGocciaSymbolValue;
    class function WellKnownHasInstance: TGocciaSymbolValue;
    class function WellKnownToPrimitive: TGocciaSymbolValue;
    class function WellKnownToStringTag: TGocciaSymbolValue;
    class function WellKnownIsConcatSpreadable: TGocciaSymbolValue;
    class function WellKnownAsyncIterator: TGocciaSymbolValue;
    class function WellKnownMetadata: TGocciaSymbolValue;
    class function WellKnownDispose: TGocciaSymbolValue;
    class function WellKnownAsyncDispose: TGocciaSymbolValue;
    class function WellKnownUnscopables: TGocciaSymbolValue;

    function TypeName: string; override;
    function TypeOf: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToDisplayString: TGocciaStringLiteralValue;

    property Description: string read FDescription;
    property HasDescription: Boolean read FHasDescription;
    property Id: Integer read FId;
  published
    function GetDescription(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolToString(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolValueOf(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolToPrimitive(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  HashMap,

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
  FMethodHost: TGocciaSymbolValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

threadvar
  GNextSymbolId: Integer;
  GSymbolRegistry: THashMap<Integer, TGocciaSymbolValue>;

// ES2026 §20.4.3.4 Symbol.prototype.toString()
function TGocciaSymbolValue.SymbolToString(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolProtoToStringRequiresSymbol, SSuggestSymbolThisType);
  Result := TGocciaSymbolValue(AThisValue).ToDisplayString;
end;

// ES2026 §20.4.3.5 Symbol.prototype.valueOf()
function TGocciaSymbolValue.SymbolValueOf(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolProtoValueOfRequiresSymbol, SSuggestSymbolThisType);
  Result := AThisValue;
end;

// ES2026 §20.4.3.6 Symbol.prototype [ @@toPrimitive ] ( hint )
function TGocciaSymbolValue.SymbolToPrimitive(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolProtoToPrimitiveRequiresSymbol, SSuggestSymbolThisType);
  Result := AThisValue;
end;

function TGocciaSymbolValue.GetDescription(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolProtoDescriptionRequiresSymbol, SSuggestSymbolThisType);
  if TGocciaSymbolValue(AThisValue).FHasDescription then
    Result := TGocciaStringLiteralValue.Create(TGocciaSymbolValue(AThisValue).FDescription)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaSymbolValue.InitializePrototype;
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
      Members.AddAccessor(PROP_DESCRIPTION, GetDescription, nil, [pfConfigurable]);
      Members.AddNamedMethod(PROP_TO_STRING, SymbolToString, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddNamedMethod(PROP_VALUE_OF, SymbolValueOf, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownToPrimitive,
        '[Symbol.toPrimitive]',
        SymbolToPrimitive,
        1,
        [pfConfigurable],
        [gmfNotConstructable]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Symbol'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;

  RegisterMemberDefinitions(Proto, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedPrototype);
    TGarbageCollector.Instance.PinObject(FMethodHost);
  end;
end;

class function TGocciaSymbolValue.SharedPrototype: TGocciaValue;
begin
  Result := FSharedPrototype;
end;

class function TGocciaSymbolValue.WellKnownIterator: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownIterator) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownIterator: must be initialised on main thread');
    FWellKnownIterator := TGocciaSymbolValue.Create('Symbol.iterator');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownIterator);
  end;
  Result := FWellKnownIterator;
end;

class function TGocciaSymbolValue.WellKnownMatch: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownMatch) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownMatch: must be initialised on main thread');
    FWellKnownMatch := TGocciaSymbolValue.Create('Symbol.match');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownMatch);
  end;
  Result := FWellKnownMatch;
end;

class function TGocciaSymbolValue.WellKnownMatchAll: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownMatchAll) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownMatchAll: must be initialised on main thread');
    FWellKnownMatchAll := TGocciaSymbolValue.Create('Symbol.matchAll');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownMatchAll);
  end;
  Result := FWellKnownMatchAll;
end;

class function TGocciaSymbolValue.WellKnownReplace: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownReplace) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownReplace: must be initialised on main thread');
    FWellKnownReplace := TGocciaSymbolValue.Create('Symbol.replace');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownReplace);
  end;
  Result := FWellKnownReplace;
end;

class function TGocciaSymbolValue.WellKnownSearch: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownSearch) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownSearch: must be initialised on main thread');
    FWellKnownSearch := TGocciaSymbolValue.Create('Symbol.search');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownSearch);
  end;
  Result := FWellKnownSearch;
end;

class function TGocciaSymbolValue.WellKnownSplit: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownSplit) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownSplit: must be initialised on main thread');
    FWellKnownSplit := TGocciaSymbolValue.Create('Symbol.split');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownSplit);
  end;
  Result := FWellKnownSplit;
end;

class function TGocciaSymbolValue.WellKnownSpecies: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownSpecies) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownSpecies: must be initialised on main thread');
    FWellKnownSpecies := TGocciaSymbolValue.Create('Symbol.species');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownSpecies);
  end;
  Result := FWellKnownSpecies;
end;

class function TGocciaSymbolValue.WellKnownHasInstance: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownHasInstance) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownHasInstance: must be initialised on main thread');
    FWellKnownHasInstance := TGocciaSymbolValue.Create('Symbol.hasInstance');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownHasInstance);
  end;
  Result := FWellKnownHasInstance;
end;

class function TGocciaSymbolValue.WellKnownToPrimitive: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownToPrimitive) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownToPrimitive: must be initialised on main thread');
    FWellKnownToPrimitive := TGocciaSymbolValue.Create('Symbol.toPrimitive');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownToPrimitive);
  end;
  Result := FWellKnownToPrimitive;
end;

class function TGocciaSymbolValue.WellKnownToStringTag: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownToStringTag) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownToStringTag: must be initialised on main thread');
    FWellKnownToStringTag := TGocciaSymbolValue.Create('Symbol.toStringTag');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownToStringTag);
  end;
  Result := FWellKnownToStringTag;
end;

class function TGocciaSymbolValue.WellKnownIsConcatSpreadable: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownIsConcatSpreadable) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownIsConcatSpreadable: must be initialised on main thread');
    FWellKnownIsConcatSpreadable := TGocciaSymbolValue.Create('Symbol.isConcatSpreadable');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownIsConcatSpreadable);
  end;
  Result := FWellKnownIsConcatSpreadable;
end;

// ES2026 §20.1.2.1 Symbol.asyncIterator
class function TGocciaSymbolValue.WellKnownAsyncIterator: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownAsyncIterator) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownAsyncIterator: must be initialised on main thread');
    FWellKnownAsyncIterator := TGocciaSymbolValue.Create('Symbol.asyncIterator');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownAsyncIterator);
  end;
  Result := FWellKnownAsyncIterator;
end;

// TC39 proposal-decorator-metadata §2 Symbol.metadata
class function TGocciaSymbolValue.WellKnownMetadata: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownMetadata) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownMetadata: must be initialised on main thread');
    FWellKnownMetadata := TGocciaSymbolValue.Create('Symbol.metadata');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownMetadata);
  end;
  Result := FWellKnownMetadata;
end;

// ES2026 §20.1.2.3 Symbol.dispose
class function TGocciaSymbolValue.WellKnownDispose: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownDispose) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownDispose: must be initialised on main thread');
    FWellKnownDispose := TGocciaSymbolValue.Create('Symbol.dispose');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownDispose);
  end;
  Result := FWellKnownDispose;
end;

// ES2026 §20.1.2.1 Symbol.asyncDispose
class function TGocciaSymbolValue.WellKnownAsyncDispose: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownAsyncDispose) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownAsyncDispose: must be initialised on main thread');
    FWellKnownAsyncDispose := TGocciaSymbolValue.Create('Symbol.asyncDispose');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownAsyncDispose);
  end;
  Result := FWellKnownAsyncDispose;
end;

// ES2026 §20.4.2.18 Symbol.unscopables
class function TGocciaSymbolValue.WellKnownUnscopables: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownUnscopables) then
  begin
    Assert(not GIsWorkerThread, 'WellKnownUnscopables: must be initialised on main thread');
    FWellKnownUnscopables := TGocciaSymbolValue.Create('Symbol.unscopables');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownUnscopables);
  end;
  Result := FWellKnownUnscopables;
end;

constructor TGocciaSymbolValue.Create(const ADescription: string);
begin
  // Backwards-compatible default: a non-empty string is a real description,
  // an empty string keeps the legacy "no description" sentinel meaning.
  Create(ADescription, ADescription <> '');
end;

constructor TGocciaSymbolValue.Create(const ADescription: string;
  const AHasDescription: Boolean);
begin
  inherited Create;
  FDescription := ADescription;
  FHasDescription := AHasDescription;
  FId := GNextSymbolId;
  Inc(GNextSymbolId);
  if not Assigned(GSymbolRegistry) then
    GSymbolRegistry := THashMap<Integer, TGocciaSymbolValue>.Create;
  GSymbolRegistry.AddOrSetValue(FId, Self);
end;

function TGocciaSymbolValue.TypeName: string;
begin
  Result := SYMBOL_TYPE_NAME;
end;

function TGocciaSymbolValue.TypeOf: string;
begin
  Result := SYMBOL_TYPE_NAME;
end;

function TGocciaSymbolValue.GetProperty(const AName: string): TGocciaValue;
begin
  if Assigned(FSharedPrototype) then
  begin
    Result := TGocciaObjectValue(FSharedPrototype).GetPropertyWithContext(AName, Self);
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

function TGocciaSymbolValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaSymbolValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
  Result := nil;
end;

// ES2026 §7.1.17 ToString(argument) — Symbol throws TypeError on implicit coercion
function TGocciaSymbolValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
  Result := nil;
end;

// ES2026 §20.4.3.4 Symbol.prototype.toString() — explicit display representation
function TGocciaSymbolValue.ToDisplayString: TGocciaStringLiteralValue;
begin
  if FHasDescription then
    Result := TGocciaStringLiteralValue.Create('Symbol(' + FDescription + ')')
  else
    Result := TGocciaStringLiteralValue.Create('Symbol()');
end;

end.
