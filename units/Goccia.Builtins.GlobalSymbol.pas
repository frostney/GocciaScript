unit Goccia.Builtins.GlobalSymbol;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TGocciaGlobalSymbol = class(TGocciaBuiltin)
  private
    FGlobalRegistry: TDictionary<string, TGocciaSymbolValue>;
    FSymbolFunction: TGocciaNativeFunctionValue;

    // Well-known symbols
    FIteratorSymbol: TGocciaSymbolValue;

    function SymbolConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolKeyFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    property GlobalRegistry: TDictionary<string, TGocciaSymbolValue> read FGlobalRegistry;
    property IteratorSymbol: TGocciaSymbolValue read FIteratorSymbol;
  end;

implementation

uses
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.PropertyNames;

constructor TGocciaGlobalSymbol.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  PrototypeInitializer: TGocciaSymbolValue;
begin
  inherited Create(AName, AScope, AThrowError);

  FGlobalRegistry := TDictionary<string, TGocciaSymbolValue>.Create;

  // Initialize shared Symbol prototype
  PrototypeInitializer := TGocciaSymbolValue.Create;
  PrototypeInitializer.InitializePrototype;

  // Use the well-known Symbol.iterator singleton
  FIteratorSymbol := TGocciaSymbolValue.WellKnownIterator;

  // Create the Symbol function (callable, creates new symbols)
  FSymbolFunction := TGocciaNativeFunctionValue.Create(SymbolConstructor, 'Symbol', 0);

  // Register static methods on the Symbol function
  FSymbolFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(SymbolFor, 'for', 1));
  FSymbolFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(SymbolKeyFor, 'keyFor', 1));

  // Register well-known symbol constants
  FSymbolFunction.RegisterConstant('iterator', FIteratorSymbol);
  FSymbolFunction.RegisterConstant('species', TGocciaSymbolValue.WellKnownSpecies);
  FSymbolFunction.RegisterConstant('hasInstance', TGocciaSymbolValue.WellKnownHasInstance);
  FSymbolFunction.RegisterConstant('toPrimitive', TGocciaSymbolValue.WellKnownToPrimitive);
  FSymbolFunction.RegisterConstant('toStringTag', TGocciaSymbolValue.WellKnownToStringTag);
  FSymbolFunction.RegisterConstant('isConcatSpreadable', TGocciaSymbolValue.WellKnownIsConcatSpreadable);

  // Expose Symbol.prototype (ECMAScript compatible)
  FSymbolFunction.AssignProperty(PROP_PROTOTYPE, TGocciaSymbolValue.SharedPrototype);

  // Bind Symbol in scope
  AScope.DefineLexicalBinding(AName, FSymbolFunction, dtLet);
end;

destructor TGocciaGlobalSymbol.Destroy;
begin
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
begin
  { Steps 2-3: If description present, let descString = ToString(description) }
  if AArgs.Length > 0 then
    Description := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Description := '';

  { Step 4: Return a new unique Symbol with [[Description]] descString }
  Result := TGocciaSymbolValue.Create(Description);
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
  Pair: TPair<string, TGocciaSymbolValue>;
begin
  Arg := AArgs.GetElement(0);

  { Step 1: If Type(sym) is not Symbol, throw a TypeError }
  if not (Arg is TGocciaSymbolValue) then
    ThrowTypeError('Symbol.keyFor requires that the first argument be a symbol');

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
