unit Goccia.Builtins.Console;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaConsole = class(TGocciaBuiltin)
  protected
    // Native methods
    function ConsoleLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Values.ClassHelper;

constructor TGocciaConsole.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleLog, 'log', -1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

function TGocciaConsole.ConsoleLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Output: string;
begin
  Output := '';
  for I := 0 to AArgs.Length - 1 do
  begin
    if I > 0 then
      Output := Output + ' ';
    Output := Output + AArgs.GetElement(I).ToStringLiteral.Value;
  end;
  WriteLn(Output);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;




end.
