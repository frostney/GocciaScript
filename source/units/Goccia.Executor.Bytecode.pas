unit Goccia.Executor.Bytecode;

{$I Goccia.inc}

interface

uses
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
    FNonStrictMode: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(const AGlobalScope: TGocciaScope;
      const AModuleLoader: TGocciaModuleLoader;
      const ASourcePath: string); override;
    function ExecuteProgram(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    function EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    function ExecuteDynamicFunction(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    procedure ClearTransientCaches; override;

    function CompileModule(
      const AProgram: TGocciaProgram): TGocciaCompiledModule; override;
    function RunCompiledModule(
      const AModule: TGocciaCompiledModule): TGocciaValue; override;
    function RunCompiledModuleInScope(
      const AModule: TGocciaCompiledModule;
      const AScope: TGocciaScope): TGocciaValue; override;

    property VM: TGocciaVM read FVM;
    property GlobalBackedTopLevel: Boolean read FGlobalBackedTopLevel
      write FGlobalBackedTopLevel;
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
    property NonStrictMode: Boolean read FNonStrictMode write FNonStrictMode;
  end;

implementation

uses
  SysUtils,

  TimingUtils,

  Goccia.Compiler.ConstantValue,
  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.Profiler,
  Goccia.Scope.Redeclaration;

{ TGocciaBytecodeExecutor }

constructor TGocciaBytecodeExecutor.Create;
begin
  inherited Create;
  FVM := TGocciaVM.Create;
end;

destructor TGocciaBytecodeExecutor.Destroy;
begin
  FVM.Free;
  inherited;
end;

procedure TGocciaBytecodeExecutor.Initialize(const AGlobalScope: TGocciaScope;
  const AModuleLoader: TGocciaModuleLoader; const ASourcePath: string);
begin
  inherited;
  FVM.Realm := FRealm;
  FVM.GlobalScope := AGlobalScope;
  FVM.GlobalThisValue := AGlobalScope.ThisValue;
  FVM.LoadModule := AModuleLoader.LoadModule;
  FVM.LoadModuleSource := AModuleLoader.LoadModuleSourceValue;
  FVM.LoadDeferredModule := AModuleLoader.LoadDeferredModuleNamespaceValue;
  AModuleLoader.EvaluateModuleBody := EvaluateModuleBody;
end;

function TGocciaBytecodeExecutor.EvaluateModuleBody(
  const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Compiler: TGocciaCompiler;
  BytecodeModule: TGocciaBytecodeModule;
  SavedGlobalScope: TGocciaScope;
  Options: TGocciaCompilerOptimizationOptions;
begin
  Compiler := TGocciaCompiler.Create(AContext.CurrentFilePath);
  try
    Compiler.GlobalBackedTopLevel := True;
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := AContext.NonStrictMode;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      Assigned(TGocciaCoverageTracker.Instance) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    BytecodeModule := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
  if Assigned(FRetainModule) then
    FRetainModule(BytecodeModule);
  SavedGlobalScope := FVM.GlobalScope;
  FVM.GlobalScope := AContext.Scope;
  try
    if Assigned(AContext.Realm) then
      FVM.Realm := AContext.Realm;
    Result := FVM.ExecuteModule(BytecodeModule);
  finally
    FVM.GlobalScope := SavedGlobalScope;
    FVM.Realm := FRealm;
  end;
end;

function TGocciaBytecodeExecutor.ExecuteProgram(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Module: TGocciaCompiledModule;
  Start: Int64;
begin
  Start := GetNanoseconds;
  Module := CompileModule(AProgram);
  CompileTimeNanoseconds := GetNanoseconds - Start;
  if Assigned(FRetainModule) then
    FRetainModule(Module);
  Start := GetNanoseconds;
  Result := RunCompiledModule(Module);
  ExecuteTimeNanoseconds := GetNanoseconds - Start;
end;

function TGocciaBytecodeExecutor.ExecuteDynamicFunction(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Module: TGocciaBytecodeModule;
begin
  Module := TGocciaBytecodeModule(CompileModule(AProgram));
  if Module.TopLevel.FunctionCount > 0 then
    Module.TopLevel.GetFunction(0).Name := 'anonymous';
  if Assigned(FRetainModule) then
    FRetainModule(Module);
  Result := RunCompiledModule(Module);
end;

function TGocciaBytecodeExecutor.CompileModule(
  const AProgram: TGocciaProgram): TGocciaCompiledModule;
var
  Compiler: TGocciaCompiler;
  Options: TGocciaCompilerOptimizationOptions;
begin
  if FGlobalBackedTopLevel and Assigned(FGlobalScope) then
    CheckTopLevelRedeclarations(AProgram, FGlobalScope, FSourcePath);

  Compiler := TGocciaCompiler.Create(FSourcePath);
  try
    Compiler.GlobalBackedTopLevel := FGlobalBackedTopLevel;
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := FNonStrictMode;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      Assigned(TGocciaCoverageTracker.Instance) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    Result := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
end;

function TGocciaBytecodeExecutor.RunCompiledModule(
  const AModule: TGocciaCompiledModule): TGocciaValue;
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
    Result := FVM.ExecuteModule(TGocciaBytecodeModule(AModule));
  finally
    GProfilingAllocations := False;
    GC.Enabled := WasEnabled;
  end;
end;

function TGocciaBytecodeExecutor.RunCompiledModuleInScope(
  const AModule: TGocciaCompiledModule;
  const AScope: TGocciaScope): TGocciaValue;
var
  SavedGlobalScope: TGocciaScope;
begin
  SavedGlobalScope := FVM.GlobalScope;
  FVM.GlobalScope := AScope;
  try
    Result := RunCompiledModule(AModule);
  finally
    FVM.GlobalScope := SavedGlobalScope;
  end;
end;

procedure TGocciaBytecodeExecutor.ClearTransientCaches;
begin
  // The Goccia VM executes directly on TGocciaValue and does not maintain
  // bridge-side transient caches that need per-measurement clearing.
end;

end.
