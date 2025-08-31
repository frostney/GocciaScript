unit Goccia.Builtins.Console;

{$I Goccia.inc}

interface

uses
  Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.Primitives,
  Goccia.Values.ObjectValue, Goccia.Values.ObjectPropertyDescriptor, Goccia.Arguments, 
  Generics.Collections, Goccia.Builtins.Base;

type
  TGocciaConsole = class(TGocciaBuiltin)
  protected
    // Native methods
    function ConsoleLog(Args: TGocciaArguments; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

uses Goccia.Values.ClassHelper;

constructor TGocciaConsole.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleLog, 'log', -1));

  AScope.DefineBuiltin(AName, FBuiltinObject);
end;

function TGocciaConsole.ConsoleLog(Args: TGocciaArguments; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Output: string;
begin
  Output := '';
  for I := 0 to Args.Count - 1 do
  begin
    if I > 0 then
      Output := Output + ' ';
    Output := Output + Args.Get(I].ToStringLiteral.Value;
  end;
  WriteLn(Output);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;




end.
