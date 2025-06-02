unit Goccia.Builtins.Console;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Scope, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Generics.Collections;

type
  TGocciaConsole = class
  private
    FName: string;
    FConsole: TGocciaObjectValue;
  public
    constructor Create(const AName: string; AScope: TGocciaScope);
    destructor Destroy; override;

    // Native methods
    function ConsoleLog(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    property Name: string read FName;
  end;

implementation

constructor TGocciaConsole.Create(const AName: string; AScope: TGocciaScope);
begin
  FName := AName;
  FConsole := TGocciaObjectValue.Create;

  FConsole.SetProperty('log', TGocciaNativeFunctionValue.Create(ConsoleLog, 'log', -1));

  AScope.SetValue(AName, FConsole);
end;

destructor TGocciaConsole.Destroy;
begin
  FConsole.Free;
  inherited;
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