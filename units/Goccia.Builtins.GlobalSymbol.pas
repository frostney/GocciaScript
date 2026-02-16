unit Goccia.Builtins.GlobalSymbol;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error.ThrowErrorCallback,
  Goccia.Values.Primitives, Goccia.Values.SymbolValue,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Generics.Collections, SysUtils;

type
  TGocciaGlobalSymbol = class(TGocciaBuiltin)
  private
    FGlobalRegistry: TDictionary<string, TGocciaSymbolValue>;
    FSymbolFunction: TGocciaNativeFunctionValue;

    // Well-known symbols
    FIteratorSymbol: TGocciaSymbolValue;

    function SymbolConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function SymbolFor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function SymbolKeyFor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    property GlobalRegistry: TDictionary<string, TGocciaSymbolValue> read FGlobalRegistry;
    property IteratorSymbol: TGocciaSymbolValue read FIteratorSymbol;
  end;

implementation

uses
  Goccia.Values.ObjectValue;

constructor TGocciaGlobalSymbol.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FGlobalRegistry := TDictionary<string, TGocciaSymbolValue>.Create;

  // Create well-known symbols
  FIteratorSymbol := TGocciaSymbolValue.Create('Symbol.iterator');

  // Create the Symbol function (callable, creates new symbols)
  FSymbolFunction := TGocciaNativeFunctionValue.Create(SymbolConstructor, 'Symbol', 0);

  // Register static methods on the Symbol function
  FSymbolFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(SymbolFor, 'for', 1));
  FSymbolFunction.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(SymbolKeyFor, 'keyFor', 1));

  // Register well-known symbol constants
  FSymbolFunction.RegisterConstant('iterator', FIteratorSymbol);

  // Bind Symbol in scope
  AScope.DefineLexicalBinding(AName, FSymbolFunction, dtLet);
end;

destructor TGocciaGlobalSymbol.Destroy;
begin
  FGlobalRegistry.Free;
  inherited;
end;

function TGocciaGlobalSymbol.SymbolConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Description: string;
begin
  if Args.Length > 0 then
    Description := Args.GetElement(0).ToStringLiteral.Value
  else
    Description := '';

  Result := TGocciaSymbolValue.Create(Description);
end;

function TGocciaGlobalSymbol.SymbolFor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Key: string;
  Symbol: TGocciaSymbolValue;
begin
  if Args.Length > 0 then
    Key := Args.GetElement(0).ToStringLiteral.Value
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

function TGocciaGlobalSymbol.SymbolKeyFor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Arg: TGocciaValue;
  Pair: TPair<string, TGocciaSymbolValue>;
begin
  if Args.Length = 0 then
  begin
    Result := TGocciaNullLiteralValue.Create;
    Exit;
  end;

  Arg := Args.GetElement(0);

  if not (Arg is TGocciaSymbolValue) then
  begin
    // Non-symbol arguments return null (undefined for undefined)
    if Arg is TGocciaUndefinedLiteralValue then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      Result := TGocciaNullLiteralValue.Create;
    Exit;
  end;

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
