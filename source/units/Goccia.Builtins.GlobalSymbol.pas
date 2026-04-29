unit Goccia.Builtins.GlobalSymbol;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.ObjectModel.Types,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TGocciaGlobalSymbol = class(TGocciaBuiltin)
  private
    FGlobalRegistry: TOrderedStringMap<TGocciaSymbolValue>;
    FSymbolFunction: TGocciaNativeFunctionValue;

    // Well-known symbols
    FIteratorSymbol: TGocciaSymbolValue;

  published
    function SymbolConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolKeyFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    property GlobalRegistry: TOrderedStringMap<TGocciaSymbolValue> read FGlobalRegistry;
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Constants.SymbolNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaGlobalSymbol.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  PrototypeInitializer: TGocciaSymbolValue;
begin
  inherited Create(AName, AScope, AThrowError);

  FGlobalRegistry := TOrderedStringMap<TGocciaSymbolValue>.Create;

  // Initialize shared Symbol prototype
  PrototypeInitializer := TGocciaSymbolValue.Create;
  PrototypeInitializer.InitializePrototype;

  // Use the well-known Symbol.iterator singleton
  FIteratorSymbol := TGocciaSymbolValue.WellKnownIterator;

  // Create the Symbol function (callable, creates new symbols)
  // ES2026 §20.4.1.1: Symbol throws TypeError if invoked with new
  FSymbolFunction := TGocciaNativeFunctionValue.Create(SymbolConstructor, 'Symbol', 0);
  FSymbolFunction.NotConstructable := True;

  // Register static methods on the Symbol function
  // Static built-in methods are not constructors per ES2026
  with TGocciaMemberCollection.Create do
  try
    AddMethod(SymbolFor, 1, gmkStaticMethod, [gmfNotConstructable]);
    AddMethod(SymbolKeyFor, 1, gmkStaticMethod, [gmfNotConstructable]);
    FStaticMembers := ToDefinitions;
  finally
    Free;
  end;
  RegisterMemberDefinitions(FSymbolFunction, FStaticMembers);

  // Register well-known symbol constants
  FSymbolFunction.RegisterConstant(SYMBOL_MATCH, TGocciaSymbolValue.WellKnownMatch);
  FSymbolFunction.RegisterConstant(SYMBOL_MATCH_ALL, TGocciaSymbolValue.WellKnownMatchAll);
  FSymbolFunction.RegisterConstant(SYMBOL_REPLACE, TGocciaSymbolValue.WellKnownReplace);
  FSymbolFunction.RegisterConstant(SYMBOL_SEARCH, TGocciaSymbolValue.WellKnownSearch);
  FSymbolFunction.RegisterConstant(SYMBOL_SPLIT, TGocciaSymbolValue.WellKnownSplit);
  FSymbolFunction.RegisterConstant(SYMBOL_ITERATOR, FIteratorSymbol);
  FSymbolFunction.RegisterConstant(SYMBOL_SPECIES, TGocciaSymbolValue.WellKnownSpecies);
  FSymbolFunction.RegisterConstant(SYMBOL_HAS_INSTANCE, TGocciaSymbolValue.WellKnownHasInstance);
  FSymbolFunction.RegisterConstant(SYMBOL_TO_PRIMITIVE, TGocciaSymbolValue.WellKnownToPrimitive);
  FSymbolFunction.RegisterConstant(SYMBOL_TO_STRING_TAG, TGocciaSymbolValue.WellKnownToStringTag);
  FSymbolFunction.RegisterConstant(SYMBOL_ASYNC_ITERATOR, TGocciaSymbolValue.WellKnownAsyncIterator);
  FSymbolFunction.RegisterConstant(SYMBOL_IS_CONCAT_SPREADABLE, TGocciaSymbolValue.WellKnownIsConcatSpreadable);
  FSymbolFunction.RegisterConstant(SYMBOL_METADATA, TGocciaSymbolValue.WellKnownMetadata);
  FSymbolFunction.RegisterConstant(SYMBOL_DISPOSE, TGocciaSymbolValue.WellKnownDispose);
  FSymbolFunction.RegisterConstant(SYMBOL_ASYNC_DISPOSE, TGocciaSymbolValue.WellKnownAsyncDispose);
  FSymbolFunction.RegisterConstant(SYMBOL_UNSCOPABLES, TGocciaSymbolValue.WellKnownUnscopables);
  FSymbolFunction.RegisterConstant(SYMBOL_CUSTOM_MATCHER, TGocciaSymbolValue.WellKnownCustomMatcher);

  // Expose Symbol.prototype (ECMAScript compatible)
  FSymbolFunction.AssignProperty(PROP_PROTOTYPE, TGocciaSymbolValue.SharedPrototype);

  // ES2026 §20.4.3.1 Symbol.prototype.constructor = Symbol
  TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype).DefineProperty(
    PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(FSymbolFunction, [pfConfigurable, pfWritable]));

  // Bind Symbol in scope
  AScope.DefineLexicalBinding(AName, FSymbolFunction, dtLet);
end;

destructor TGocciaGlobalSymbol.Destroy;
var
  Pair: TOrderedStringMap<TGocciaSymbolValue>.TKeyValuePair;
begin
  if Assigned(TGarbageCollector.Instance) then
    for Pair in FGlobalRegistry do
      TGarbageCollector.Instance.UnpinObject(Pair.Value);
  FGlobalRegistry.Free;
  inherited;
end;

{ Symbol ( [ description ] ) — §20.4.1.1
  1. If NewTarget is not undefined, throw a TypeError exception.
  2. If description is undefined, let descString be undefined.
  3. Else, let descString be ? ToString(description).
  4. Return a new unique Symbol value whose [[Description]] is descString. }
function TGocciaGlobalSymbol.SymbolConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Description: string;
  HasDescription: Boolean;
  Arg: TGocciaValue;
begin
  { Step 2: If description is undefined, let descString be undefined.
    Step 3: Else, let descString be ? ToString(description). }
  HasDescription := False;
  Description := '';
  if AArgs.Length > 0 then
  begin
    Arg := AArgs.GetElement(0);
    if not (Arg is TGocciaUndefinedLiteralValue) then
    begin
      Description := Arg.ToStringLiteral.Value;
      HasDescription := True;
    end;
  end;

  { Step 4: Return a new unique Symbol with [[Description]] descString }
  Result := TGocciaSymbolValue.Create(Description, HasDescription);
end;

{ Symbol.for ( key ) — §20.4.2.2
  1. Let stringKey be ? ToString(key).
  2. For each element e of the GlobalSymbolRegistry List, do
     a. If SameValue(e.[[Key]], stringKey) is true, return e.[[Symbol]].
  3. Assert: GlobalSymbolRegistry does not currently contain an entry for stringKey.
  4. Let newSymbol be a new unique Symbol value whose [[Description]] is stringKey.
  5. Append the Record ([[Key]]: stringKey, [[Symbol]]: newSymbol) to
     the GlobalSymbolRegistry List.
  6. Return newSymbol. }
function TGocciaGlobalSymbol.SymbolFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Key: string;
  Symbol: TGocciaSymbolValue;
begin
  { Step 1: Let stringKey = ToString(key) }
  if AArgs.Length > 0 then
    Key := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Key := 'undefined';

  { Step 2: Search GlobalSymbolRegistry for existing entry }
  if FGlobalRegistry.TryGetValue(Key, Symbol) then
    Result := Symbol
  else
  begin
    { Steps 4-5: Create new symbol, append to GlobalSymbolRegistry }
    Symbol := TGocciaSymbolValue.Create(Key);
    Symbol.Registered := True;
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(Symbol);
    FGlobalRegistry.Add(Key, Symbol);
    { Step 6: Return newSymbol }
    Result := Symbol;
  end;
end;

{ Symbol.keyFor ( sym ) — §20.4.2.5
  1. If Type(sym) is not Symbol, throw a TypeError exception.
  2. For each element e of the GlobalSymbolRegistry List, do
     a. If SameValue(e.[[Symbol]], sym) is true, return e.[[Key]].
  3. Assert: GlobalSymbolRegistry does not currently contain an entry for sym.
  4. Return undefined. }
function TGocciaGlobalSymbol.SymbolKeyFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  Pair: TOrderedStringMap<TGocciaSymbolValue>.TKeyValuePair;
begin
  Arg := AArgs.GetElement(0);

  { Step 1: If Type(sym) is not Symbol, throw a TypeError }
  if not (Arg is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolKeyForRequiresSymbol, SSuggestSymbolKeyForArg);

  { Step 2: Search GlobalSymbolRegistry for matching symbol }
  for Pair in FGlobalRegistry do
  begin
    if Pair.Value = TGocciaSymbolValue(Arg) then
    begin
      { Step 2a: SameValue match found — return e.[[Key]] }
      Result := TGocciaStringLiteralValue.Create(Pair.Key);
      Exit;
    end;
  end;

  { Step 4: Return undefined }
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
