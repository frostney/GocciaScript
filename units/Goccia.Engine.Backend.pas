unit Goccia.Engine.Backend;

{$I Goccia.inc}

interface

uses
  Classes,

  Souffle.Bytecode.Module,
  Souffle.VM,
  Souffle.VM.RuntimeOperations,

  Goccia.AST.Node,
  Goccia.Builtins.Benchmark,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.Environment.Bootstrap,
  Goccia.Environment.Types,
  Goccia.Interpreter,
  Goccia.MicrotaskQueue,
  Goccia.Modules.Resolver,
  Goccia.Runtime.Operations,
  Goccia.Values.Primitives;

type
  TGocciaEngineBackend = (
    ebTreeWalk,
    ebSouffleVM
  );

  TGocciaSouffleBackend = class
  private
    FVM: TSouffleVM;
    FRuntime: TGocciaRuntimeOperations;
    FSourcePath: string;
    FBridgeSourceLines: TStringList;
    FBridgeInterpreter: TGocciaInterpreter;
    FBootstrap: TGocciaEnvironmentBootstrap;
    FModuleResolver: TGocciaModuleResolver;
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function GetBuiltinBenchmark: TGocciaBenchmark;
  public
    constructor Create(const ASourcePath: string);
    destructor Destroy; override;

    function CompileAndRun(const AProgram: TGocciaProgram): TGocciaValue;
    function CompileToModule(const AProgram: TGocciaProgram): TSouffleBytecodeModule;
    function RunModule(const AModule: TSouffleBytecodeModule): TGocciaValue;

    procedure RegisterGlobal(const AName: string; const AValue: TGocciaValue);
    procedure RegisterBuiltIns(const AGlobals: TGocciaGlobalBuiltins);

    property Runtime: TGocciaRuntimeOperations read FRuntime;
    property VM: TSouffleVM read FVM;
    property BridgeInterpreter: TGocciaInterpreter read FBridgeInterpreter;
    property BuiltinBenchmark: TGocciaBenchmark read GetBuiltinBenchmark;
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,
  Souffle.Bytecode.Chunk,
  Souffle.Value,
  Souffle.VM.Exception,

  Goccia.CallStack,
  Goccia.Error,
  Goccia.Values.Error;

{ TGocciaSouffleBackend }

constructor TGocciaSouffleBackend.Create(const ASourcePath: string);
begin
  inherited Create;
  TGarbageCollector.Initialize;
  TGocciaCallStack.Initialize;
  TGocciaMicrotaskQueue.Initialize;
  FSourcePath := ASourcePath;
  FBridgeSourceLines := TStringList.Create;
  FBridgeInterpreter := TGocciaInterpreter.Create(ASourcePath, FBridgeSourceLines);
  FModuleResolver := TGocciaModuleResolver.Create(
    ExtractFilePath(ExpandFileName(ASourcePath)));
  FBridgeInterpreter.Resolver := FModuleResolver;
  FRuntime := TGocciaRuntimeOperations.Create;
  FVM := TSouffleVM.Create(FRuntime);
  FRuntime.VM := FVM;
  FRuntime.Interpreter := FBridgeInterpreter;
  FRuntime.ModuleResolver := FModuleResolver;
end;

destructor TGocciaSouffleBackend.Destroy;
begin
  {$IFDEF BRIDGE_METRICS}
  TBridgeMetrics.DumpGlobal;
  {$ENDIF}
  FBootstrap.Free;
  FVM.Free;
  FRuntime.Free;
  FBridgeInterpreter.Free;
  FBridgeSourceLines.Free;
  FModuleResolver.Free;
  inherited;
end;

function TGocciaSouffleBackend.CompileAndRun(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Module: TSouffleBytecodeModule;
begin
  Module := CompileToModule(AProgram);
  try
    Result := RunModule(Module);
  finally
    Module.Free;
  end;
end;

function TGocciaSouffleBackend.CompileToModule(
  const AProgram: TGocciaProgram): TSouffleBytecodeModule;
var
  Compiler: TGocciaCompiler;
  Template: TSouffleFunctionTemplate;
begin
  Compiler := TGocciaCompiler.Create(FSourcePath);
  try
    Result := Compiler.Compile(AProgram);

    for Template in Compiler.FormalParameterCounts.Keys do
      FRuntime.RegisterFormalParameterCount(
        Template, Compiler.FormalParameterCounts[Template]);
  finally
    Compiler.Free;
  end;
end;

function TGocciaSouffleBackend.RunModule(
  const AModule: TSouffleBytecodeModule): TGocciaValue;
var
  SouffleResult: TSouffleValue;
  GC: TGarbageCollector;
  WasEnabled: Boolean;
begin
  GC := TGarbageCollector.Instance;
  WasEnabled := GC.Enabled;
  GC.Enabled := False;
  try
    try
      try
        SouffleResult := FVM.Execute(AModule);
        if Assigned(TGocciaMicrotaskQueue.Instance) then
          TGocciaMicrotaskQueue.Instance.DrainQueue;
        Result := FRuntime.UnwrapToGocciaValue(SouffleResult);
      except
        on E: ESouffleThrow do
          raise TGocciaThrowValue.Create(
            FRuntime.UnwrapToGocciaValue(E.ThrownValue));
      end;
    finally
      if Assigned(TGocciaMicrotaskQueue.Instance) then
        TGocciaMicrotaskQueue.Instance.ClearQueue;
    end;
  finally
    GC.Enabled := WasEnabled;
  end;
end;

procedure TGocciaSouffleBackend.RegisterGlobal(const AName: string;
  const AValue: TGocciaValue);
begin
  FRuntime.RegisterGlobal(AName, FRuntime.ToSouffleValue(AValue));
end;

procedure TGocciaSouffleBackend.RegisterBuiltIns(
  const AGlobals: TGocciaGlobalBuiltins);
begin
  FRuntime.SourcePath := FSourcePath;
  FreeAndNil(FBootstrap);
  FBootstrap := TGocciaEnvironmentBootstrap.Create(
    AGlobals, FBridgeInterpreter.GlobalScope, ThrowError);
  FBootstrap.MaterializeInterpreterEnvironment(True);
  FBootstrap.MaterializeSouffleEnvironment(FRuntime);
end;

procedure TGocciaSouffleBackend.ThrowError(const AMessage: string;
  const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FSourcePath,
    FBridgeSourceLines);
end;

function TGocciaSouffleBackend.GetBuiltinBenchmark: TGocciaBenchmark;
begin
  if Assigned(FBootstrap) then
    Result := FBootstrap.BuiltinBenchmark
  else
    Result := nil;
end;

end.
