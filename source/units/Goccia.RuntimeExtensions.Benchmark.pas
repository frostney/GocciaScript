unit Goccia.RuntimeExtensions.Benchmark;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Benchmark,
  Goccia.Runtime;

type
  TGocciaBenchmarkRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinBenchmark: TGocciaBenchmark;
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
  Runtime.RegisterRuntimeBuiltinName('Benchmark');
end;

procedure TGocciaBenchmarkRuntimeExtension.Detach;
begin
  FBuiltinBenchmark.Free;
  FBuiltinBenchmark := nil;
  inherited;
end;

end.
