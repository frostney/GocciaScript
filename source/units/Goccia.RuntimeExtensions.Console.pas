unit Goccia.RuntimeExtensions.Console;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Console,
  Goccia.Runtime;

type
  TGocciaConsoleRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinConsole: TGocciaConsole;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;

    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
  end;

implementation

procedure TGocciaConsoleRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinConsole := TGocciaConsole.Create('console',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
end;

procedure TGocciaConsoleRuntimeExtension.Detach;
begin
  FBuiltinConsole.Free;
  FBuiltinConsole := nil;
  inherited;
end;

end.
