unit Goccia.RuntimeExtensions.Benchmark;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Benchmark,
  Goccia.Modules,
  Goccia.Runtime;

type
  TGocciaBenchmarkRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinBenchmark: TGocciaBenchmark;
    FMicrobenchModule: TGocciaModule;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;

    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
  end;

implementation

procedure TGocciaBenchmarkRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinBenchmark := TGocciaBenchmark.Create('Benchmark',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
  FMicrobenchModule := FBuiltinBenchmark.CreateModule;
  FMicrobenchModule.GetNamespaceObject;
  Runtime.Engine.ModuleLoader.GlobalModules.Add('goccia:microbench',
    FMicrobenchModule);
end;

procedure TGocciaBenchmarkRuntimeExtension.Detach;
begin
  if Assigned(Runtime) and Assigned(Runtime.Engine) then
    Runtime.Engine.ModuleLoader.GlobalModules.Remove('goccia:microbench');
  FMicrobenchModule.Free;
  FMicrobenchModule := nil;
  FBuiltinBenchmark.Free;
  FBuiltinBenchmark := nil;
  inherited;
end;

end.
