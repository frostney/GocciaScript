unit Goccia.Engine.Backend;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Evaluator.Context,
  Goccia.Executor,
  Goccia.Modules.Loader,
  Goccia.Scope,
  Goccia.Values.Primitives,
  Goccia.VM;

type
  TGocciaBytecodeExecutor = class(TGocciaExecutor)
  private
    FVM: TGocciaVM;
    FGlobalBackedTopLevel: Boolean;
    FStrictTypes: Boolean;
    FModuleModules: TObjectList<TGocciaBytecodeModule>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(const AGlobalScope: TGocciaScope;
      const AModuleLoader: TGocciaModuleLoader;
      const ASourcePath: string); override;
    function ExecuteProgram(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    procedure EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext); override;
    function ExecuteDynamicFunction(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    procedure ClearTransientCaches; override;

    function CompileToModule(
      const AProgram: TGocciaProgram): TGocciaBytecodeModule;
    function RunModule(const AModule: TGocciaBytecodeModule): TGocciaValue;

    property VM: TGocciaVM read FVM;
    property GlobalBackedTopLevel: Boolean read FGlobalBackedTopLevel
      write FGlobalBackedTopLevel;
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
  end;

implementation

uses
  SysUtils,

  TimingUtils,

  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.Profiler,
  Goccia.Scope.Redeclaration;

{ TGocciaBytecodeExecutor }

constructor TGocciaBytecodeExecutor.Create;
begin
  inherited Create;
  FVM := TGocciaVM.Create;
  FModuleModules := TObjectList<TGocciaBytecodeModule>.Create(True);
end;

destructor TGocciaBytecodeExecutor.Destroy;
begin
  FModuleModules.Free;
  FVM.Free;
  inherited;
end;

procedure TGocciaBytecodeExecutor.Initialize(const AGlobalScope: TGocciaScope;
  const AModuleLoader: TGocciaModuleLoader; const ASourcePath: string);
begin
  inherited;
  FVM.GlobalScope := AGlobalScope;
  FVM.LoadModule := AModuleLoader.LoadModule;
  AModuleLoader.EvaluateModuleBody := EvaluateModuleBody;
end;

procedure TGocciaBytecodeExecutor.EvaluateModuleBody(
  const AProgram: TGocciaProgram; const AContext: TGocciaEvaluationContext);
var
  Compiler: TGocciaCompiler;
  BytecodeModule: TGocciaBytecodeModule;
  SavedGlobalScope: TGocciaScope;
begin
  Compiler := TGocciaCompiler.Create(AContext.CurrentFilePath);
  try
    Compiler.GlobalBackedTopLevel := True;
    Compiler.StrictTypes := FStrictTypes;
    BytecodeModule := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
  // Keep the module alive: closures created during execution reference its
  // function templates, so the module must outlive the exported values.
  FModuleModules.Add(BytecodeModule);
  SavedGlobalScope := FVM.GlobalScope;
  FVM.GlobalScope := AContext.Scope;
  try
    FVM.ExecuteModule(BytecodeModule);
  finally
    FVM.GlobalScope := SavedGlobalScope;
  end;
end;

function TGocciaBytecodeExecutor.ExecuteProgram(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Module: TGocciaBytecodeModule;
  Start: Int64;
begin
  Start := GetNanoseconds;
  Module := CompileToModule(AProgram);
  CompileTimeNanoseconds := GetNanoseconds - Start;
  try
    Start := GetNanoseconds;
    Result := RunModule(Module);
    ExecuteTimeNanoseconds := GetNanoseconds - Start;
  finally
    Module.Free;
  end;
end;

function TGocciaBytecodeExecutor.ExecuteDynamicFunction(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileToModule(AProgram);
  // Patch method template name to 'anonymous' for Function constructor
  if Module.TopLevel.FunctionCount > 0 then
    Module.TopLevel.GetFunction(0).Name := 'anonymous';
  // Retain the module: closures reference its function templates
  FModuleModules.Add(Module);
  Result := RunModule(Module);
end;

function TGocciaBytecodeExecutor.CompileToModule(
  const AProgram: TGocciaProgram): TGocciaBytecodeModule;
var
  Compiler: TGocciaCompiler;
begin
  if FGlobalBackedTopLevel and Assigned(FGlobalScope) then
    CheckTopLevelRedeclarations(AProgram, FGlobalScope, FSourcePath);

  Compiler := TGocciaCompiler.Create(FSourcePath);
  try
    Compiler.GlobalBackedTopLevel := FGlobalBackedTopLevel;
    Compiler.StrictTypes := FStrictTypes;
    Result := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
end;

function TGocciaBytecodeExecutor.RunModule(
  const AModule: TGocciaBytecodeModule): TGocciaValue;
var
  GC: TGarbageCollector;
  WasEnabled: Boolean;
begin
  GC := TGarbageCollector.Instance;
  WasEnabled := GC.Enabled;
  GC.Enabled := False;
  FVM.CoverageEnabled := Assigned(TGocciaCoverageTracker.Instance)
    and TGocciaCoverageTracker.Instance.Enabled;
  FVM.ProfilingOpcodes := Assigned(TGocciaProfiler.Instance)
    and TGocciaProfiler.Instance.Enabled
    and (pmOpcodes in TGocciaProfiler.Instance.Mode);
  FVM.ProfilingFunctions := Assigned(TGocciaProfiler.Instance)
    and TGocciaProfiler.Instance.Enabled
    and (pmFunctions in TGocciaProfiler.Instance.Mode);
  GProfilingAllocations := FVM.ProfilingFunctions;
  try
    Result := FVM.ExecuteModule(AModule);
  finally
    GProfilingAllocations := False;
    GC.Enabled := WasEnabled;
  end;
end;

procedure TGocciaBytecodeExecutor.ClearTransientCaches;
begin
  // The Goccia VM executes directly on TGocciaValue and does not maintain
  // bridge-side transient caches that need per-measurement clearing.
end;

end.
