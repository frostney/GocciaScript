unit Goccia.Builtins.Console;

{$I Goccia.inc}

interface

uses
  Goccia.Scope, Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Values.NativeFunction, Goccia.Values.Primitives,
  Goccia.Values.ObjectValue, Goccia.Values.ObjectPropertyDescriptor, Goccia.Arguments.Collection,
  Generics.Collections, Goccia.Builtins.Base;

type
  TGocciaConsole = class(TGocciaBuiltin)
  protected
    // Native methods
    function ConsoleLog(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses Goccia.Values.ClassHelper;

constructor TGocciaConsole.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleLog, 'log', -1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

function TGocciaConsole.ConsoleLog(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Output: string;
begin
  Output := '';
  for I := 0 to Args.Length - 1 do
  begin
    if I > 0 then
      Output := Output + ' ';
    Output := Output + Args.GetElement(I).ToStringLiteral.Value;
  end;
  WriteLn(Output);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;




end.
