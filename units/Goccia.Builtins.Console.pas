unit Goccia.Builtins.Console;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Generics.Collections, Goccia.Builtins.Base;

type
  TGocciaConsole = class(TGocciaBuiltin)
  protected
    // Native methods
    function ConsoleLog(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

constructor TGocciaConsole.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.SetProperty('log', TGocciaNativeFunctionValue.Create(ConsoleLog, 'log', -1));

  AScope.SetValue(AName, FBuiltinObject);
end;

function TGocciaConsole.ConsoleLog(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Output: string;
begin
  Output := '';
  for I := 0 to Args.Count - 1 do
  begin
    if I > 0 then
      Output := Output + ' ';
    Output := Output + Args[I].ToString;
  end;
  WriteLn(Output);
  Result := TGocciaUndefinedValue.Create;
end;




end.