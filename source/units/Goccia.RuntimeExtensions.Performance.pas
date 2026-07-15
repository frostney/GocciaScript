unit Goccia.RuntimeExtensions.Performance;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Performance,
  Goccia.Runtime;

type
  TGocciaPerformanceRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinPerformance: TGocciaPerformance;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Scope;

procedure TGocciaPerformanceRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinPerformance := TGocciaPerformance.Create('performance',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
    Runtime.Engine.HostEnvironment);
  Runtime.Engine.Interpreter.GlobalScope.DefineLexicalBinding(
    CONSTRUCTOR_PERFORMANCE, TGocciaPerformance.CreateInterfaceObject,
    dtConst, True);
end;

procedure TGocciaPerformanceRuntimeExtension.Detach;
begin
  FBuiltinPerformance.Free;
  FBuiltinPerformance := nil;
  inherited;
end;

end.
