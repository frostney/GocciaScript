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
  Goccia.Values.ObjectValue;

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
  FSymbolFunction.AssignProperty('prototype', TGocciaSymbolValue.SharedPrototype);

  // Bind Symbol in scope
  AScope.DefineLexicalBinding(AName, FSymbolFunction, dtLet);
end;

destructor TGocciaGlobalSymbol.Destroy;
begin
  FGlobalRegistry.Free;
  inherited;
end;

function TGocciaGlobalSymbol.SymbolConstructor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Description: string;
begin
  if AArgs.Length > 0 then
    Description := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Description := '';

  Result := TGocciaSymbolValue.Create(Description);
end;

function TGocciaGlobalSymbol.SymbolFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Key: string;
  Symbol: TGocciaSymbolValue;
begin
  if AArgs.Length > 0 then
    Key := AArgs.GetElement(0).ToStringLiteral.Value
  else
    Key := 'undefined';

  if FGlobalRegistry.TryGetValue(Key, Symbol) then
    Result := Symbol
  else
  begin
    Symbol := TGocciaSymbolValue.Create(Key);
    FGlobalRegistry.Add(Key, Symbol);
    Result := Symbol;
  end;
end;

function TGocciaGlobalSymbol.SymbolKeyFor(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  Pair: TPair<string, TGocciaSymbolValue>;
begin
  Arg := AArgs.GetElement(0);

  if not (Arg is TGocciaSymbolValue) then
    ThrowTypeError('Symbol.keyFor requires that the first argument be a symbol');

  // Search the global registry for this symbol instance
  for Pair in FGlobalRegistry do
  begin
    if Pair.Value = TGocciaSymbolValue(Arg) then
    begin
      Result := TGocciaStringLiteralValue.Create(Pair.Key);
      Exit;
    end;
  end;

  // Symbol not in global registry
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
