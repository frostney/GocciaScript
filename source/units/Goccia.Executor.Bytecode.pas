unit Goccia.Executor.Bytecode;

{$I Goccia.inc}

interface

uses
  OrderedStringMap,
  UnicodeStringList,

  Goccia.AST.Node,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.EvaluationContext,
  Goccia.Executor,
  Goccia.Modules,
  Goccia.Modules.Loader,
  Goccia.Realm,
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
    FArgumentsObjectEnabled: Boolean;
    FLinkedModules: TOrderedStringMap<TGocciaModule>;
    FLinkedReferrer: string;
    FEntryRequests: TGocciaModuleList;
    FEntryRequestsLeft: Integer;
    procedure LinkModuleRequests(const AModule: TGocciaBytecodeModule);
    procedure AwaitEntryRequests;
    procedure InitializeEntryLocalExports(const AModule: TGocciaBytecodeModule;
      const AEntry: TGocciaModule);
    procedure InitializeEntryLinkedExports(const AModule: TGocciaBytecodeModule;
      const AEntry: TGocciaModule);
    function LoadLinkedModule(const AModulePath,
      AImportingFilePath: string): TGocciaModule;
    function RunModule(const AModule: TGocciaCompiledModule;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure InstantiateCompiledModuleFunctions(
      const AModule: TGocciaBytecodeModule; const AScope: TGocciaScope;
      const ARealm: TGocciaRealm);
    function CompileDirectEvalCode(const AProgram: TGocciaProgram;
      const ASourcePath: string; const ASite: TGocciaDirectEvalEnvironment;
      const AStrictEval: Boolean;
      const ADeclaredVarNames: TUnicodeStringList): TGocciaFunctionTemplate;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(const AGlobalScope: TGocciaScope;
      const AModuleLoader: TGocciaModuleLoader;
      const ASourcePath: string); override;
    function ExecuteProgram(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    function EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext;
      out AProgramConsumed: Boolean): TGocciaValue; override;
    procedure InstantiateModuleFunctions(const AProgram: TGocciaProgram;
      var AContext: TGocciaEvaluationContext); override;
    function ExecuteDynamicFunction(
      const AProgram: TGocciaProgram): TGocciaValue; override;
    procedure EvaluateShim(const AProgram: TGocciaProgram;
      const AScope: TGocciaScope; const AFileName: string); override;
    function EvaluateGlobalEval(const AProgram: TGocciaProgram;
      const ASourcePath: string; const AStrictEval: Boolean;
      const ADeclarations: TGocciaGlobalEvalDeclarations;
      var AStage: TGocciaGlobalEvalStage): TGocciaValue; override;
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
    property ArgumentsObjectEnabled: Boolean
      read FArgumentsObjectEnabled write FArgumentsObjectEnabled;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Compiler.ConstantValue,
  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.Keywords.Reserved,
  Goccia.Profiler,
  Goccia.Scope.BindingMap,
  Goccia.Scope.Declarations,
  Goccia.Scope.Redeclaration,
  Goccia.Values.PromiseValue;

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
  FVM.EnsureStackRootRegistered;
  FVM.Realm := FRealm;
  FVM.GlobalScope := AGlobalScope;
  FVM.GlobalThisValue := AGlobalScope.ThisValue;
  FVM.LoadModule := AModuleLoader.LoadModule;
  FVM.LoadModuleSource := AModuleLoader.LoadModuleSourceValue;
  FVM.LoadDeferredModule := AModuleLoader.LoadDeferredModuleNamespaceValue;
  FVM.ResolveModuleURL := AModuleLoader.ResolveModuleURL;
  FVM.CompileDirectEval := CompileDirectEvalCode;
  AModuleLoader.EvaluateModuleBody := EvaluateModuleBody;
  AModuleLoader.InstantiateModuleFunctions := InstantiateModuleFunctions;
end;

{ ES2026 §16.2.1.7.3.1 InitializeEnvironment, the function declarations of a
  module: compile the whole module now, while it is linked, and instantiate
  each top-level function declaration from its compiled template in the
  module environment, so the functions exist, as compiled closures, before
  any module body runs (a module in an import cycle can call them first).
  The body is compiled with them and kept in AContext.CompiledModule for
  EvaluateModuleBody, which reads each function from the environment instead
  of creating it again. }
procedure TGocciaBytecodeExecutor.InstantiateModuleFunctions(
  const AProgram: TGocciaProgram; var AContext: TGocciaEvaluationContext);
var
  BytecodeModule: TGocciaBytecodeModule;
  Compiler: TGocciaCompiler;
  Options: TGocciaCompilerOptimizationOptions;
begin
  Compiler := TGocciaCompiler.Create(AContext.CurrentFilePath);
  try
    // The module's top-level bindings live in its Module Environment
    // Record, so the module body and its functions read them there by name:
    // a function instantiated now observes the body's later let/const
    // initialization and writes.
    Compiler.GlobalBackedTopLevel := True;
    Compiler.AsyncTopLevel := AProgram.HasTopLevelAwait;
    Compiler.PreinitializedTopLevelFunctions := True;
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := AContext.NonStrictMode;
    Compiler.ArgumentsObjectEnabled :=
      AContext.Scope.EffectiveArgumentsObjectEnabled;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      (TGocciaCoverageTracker.Instance <> nil) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    BytecodeModule := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
  if Assigned(FRetainModule) then
    FRetainModule(BytecodeModule);
  AContext.CompiledModule := BytecodeModule;
  InstantiateCompiledModuleFunctions(BytecodeModule, AContext.Scope,
    AContext.Realm);
end;

{ Create each top-level function declaration of AModule, compiled for
  linking, from its template in the module environment AScope. }
procedure TGocciaBytecodeExecutor.InstantiateCompiledModuleFunctions(
  const AModule: TGocciaBytecodeModule; const AScope: TGocciaScope;
  const ARealm: TGocciaRealm);
var
  GC: TGarbageCollector;
  I: Integer;
  SavedRealm: TGocciaRealm;
  Template: TGocciaFunctionTemplate;
  Value: TGocciaValue;
begin
  GC := TGarbageCollector.Instance;
  SavedRealm := FVM.Realm;
  if Assigned(ARealm) then
    FVM.Realm := ARealm;
  try
    for I := 0 to AModule.TopLevel.FunctionCount - 1 do
    begin
      Template := AModule.TopLevel.GetFunction(I);
      if Template.ModuleEnvironmentBinding = '' then
        Continue;
      Value := FVM.InstantiateModuleFunction(Template, AScope);
      if Assigned(GC) then
        GC.AddTempRoot(Value);
      try
        if AScope.ContainsOwnLexicalBinding(
           Template.ModuleEnvironmentBinding) then
          AScope.ForceUpdateBinding(Template.ModuleEnvironmentBinding,
            Value)
        else
          AScope.DefineLexicalBinding(
            Template.ModuleEnvironmentBinding, Value, dtLet);
      finally
        if Assigned(GC) then
          GC.RemoveTempRoot(Value);
      end;
    end;
  finally
    FVM.Realm := SavedRealm;
  end;
end;

{ A bootstrap shim runs as compiled bytecode, so every function it creates is
  a VM closure (#873, #911). The shim is a module: it is compiled the way the
  loader compiles one while linking it, with its top-level bindings read by
  name from its module environment AScope, where Goccia.Shims reads the
  export back. The language profile is the shim's own, not the engine's: a
  shim is strict module code that may use `arguments` (AScope enables it),
  and it is never compiled with strict types or for coverage. Its functions
  hide their source text, as built-in functions do, and the coverage tracker
  ignores AFileName. The shim can be materialized while the VM is running
  user code (a lazy global read), so the VM state the module run changes is
  restored afterwards, as EvaluateModuleBody does. }
procedure TGocciaBytecodeExecutor.EvaluateShim(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope; const AFileName: string);
var
  BytecodeModule: TGocciaBytecodeModule;
  Compiler: TGocciaCompiler;
  GC: TGarbageCollector;
  SavedGlobalBackedTopLevel: Boolean;
  SavedGlobalScope: TGocciaScope;
  SavedRealm: TGocciaRealm;
  SavedRuntimeModule: TGocciaModule;
begin
  if TGocciaCoverageTracker.Instance <> nil then
    TGocciaCoverageTracker.Instance.ExcludeFile(AFileName);
  Compiler := TGocciaCompiler.Create(AFileName);
  try
    Compiler.GlobalBackedTopLevel := True;
    Compiler.PreinitializedTopLevelFunctions := True;
    Compiler.ArgumentsObjectEnabled := AScope.EffectiveArgumentsObjectEnabled;
    Compiler.HideSourceText := True;
    BytecodeModule := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
  if Assigned(FRetainModule) then
    FRetainModule(BytecodeModule);

  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.AddTempRoot(AScope);
  SavedGlobalScope := FVM.GlobalScope;
  SavedRealm := FVM.Realm;
  SavedGlobalBackedTopLevel := FVM.GlobalBackedTopLevel;
  SavedRuntimeModule := FVM.CurrentRuntimeModule;
  try
    PredeclareModuleLexicalDeclarations(AProgram, AScope);
    InstantiateCompiledModuleFunctions(BytecodeModule, AScope, FRealm);
    FVM.GlobalScope := AScope;
    FVM.GlobalBackedTopLevel := False;
    FVM.CurrentRuntimeModule := nil;
    if Assigned(FRealm) then
      FVM.Realm := FRealm;
    FVM.ExecuteImportedModule(BytecodeModule);
  finally
    FVM.GlobalScope := SavedGlobalScope;
    FVM.Realm := SavedRealm;
    FVM.GlobalBackedTopLevel := SavedGlobalBackedTopLevel;
    FVM.CurrentRuntimeModule := SavedRuntimeModule;
    if Assigned(GC) then
      GC.RemoveTempRoot(AScope);
  end;
end;

function TGocciaBytecodeExecutor.EvaluateModuleBody(
  const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext;
  out AProgramConsumed: Boolean): TGocciaValue;
var
  BytecodeModule: TGocciaBytecodeModule;
  InstantiationContext: TGocciaEvaluationContext;
  SavedGlobalScope: TGocciaScope;
  SavedRealm: TGocciaRealm;
  SavedGlobalBackedTopLevel: Boolean;
  SavedLoadDeferredModule: TLoadDeferredModuleCallback;
  SavedLoadModule: TLoadModuleCallback;
  SavedLoadModuleSource: TLoadModuleSourceCallback;
  SavedRuntimeModule: TGocciaModule;
  SavedResolveModuleURL: TResolveModuleURLCallback;
begin
  AProgramConsumed := False;
  if AContext.CompiledModule is TGocciaBytecodeModule then
    BytecodeModule := TGocciaBytecodeModule(AContext.CompiledModule)
  else
  begin
    { A module environment no executor initialized gets its functions now,
      before its body runs. }
    InstantiationContext := AContext;
    InstantiateModuleFunctions(AProgram, InstantiationContext);
    BytecodeModule := TGocciaBytecodeModule(
      InstantiationContext.CompiledModule);
  end;
  SavedGlobalScope := FVM.GlobalScope;
  SavedRealm := FVM.Realm;
  SavedGlobalBackedTopLevel := FVM.GlobalBackedTopLevel;
  SavedLoadModule := FVM.LoadModule;
  SavedLoadModuleSource := FVM.LoadModuleSource;
  SavedLoadDeferredModule := FVM.LoadDeferredModule;
  SavedResolveModuleURL := FVM.ResolveModuleURL;
  SavedRuntimeModule := FVM.CurrentRuntimeModule;
  FVM.GlobalScope := AContext.Scope;
  FVM.GlobalBackedTopLevel := False;
  FVM.LoadModule := AContext.LoadModule;
  FVM.LoadModuleSource := AContext.LoadModuleSource;
  FVM.LoadDeferredModule := AContext.LoadDeferredModule;
  FVM.ResolveModuleURL := AContext.ResolveModuleURL;
  FVM.CurrentRuntimeModule := AContext.CurrentModule;
  try
    if Assigned(AContext.Realm) then
      FVM.Realm := AContext.Realm;
    Result := FVM.ExecuteImportedModule(BytecodeModule);
    if AProgram.HasTopLevelAwait and Assigned(AContext.CurrentModule) and
       (Result is TGocciaPromiseValue) then
      AContext.CurrentModule.EvaluationPromise := Result;
  finally
    FVM.GlobalScope := SavedGlobalScope;
    FVM.Realm := SavedRealm;
    FVM.GlobalBackedTopLevel := SavedGlobalBackedTopLevel;
    FVM.LoadModule := SavedLoadModule;
    FVM.LoadModuleSource := SavedLoadModuleSource;
    FVM.LoadDeferredModule := SavedLoadDeferredModule;
    FVM.ResolveModuleURL := SavedResolveModuleURL;
    FVM.CurrentRuntimeModule := SavedRuntimeModule;
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

{ ES2026 §19.2.1.1 PerformEval steps 17-33 with the realm's global
  environment as the variable environment, compiled (ADR 0134 Phase 2). The
  eval code compiles to a module of its own, kept for the engine's life like
  the Function constructor's code (ADR 0134, decision 1). §19.2.1.3
  EvalDeclarationInstantiation checks the global object first, then the
  template binds the eval's functions and vars and runs the body on this
  realm's VM, against the realm's global environment even when the eval was
  called from a module body. }
function TGocciaBytecodeExecutor.EvaluateGlobalEval(
  const AProgram: TGocciaProgram; const ASourcePath: string;
  const AStrictEval: Boolean;
  const ADeclarations: TGocciaGlobalEvalDeclarations;
  var AStage: TGocciaGlobalEvalStage): TGocciaValue;
var
  Compiler: TGocciaCompiler;
  Module: TGocciaBytecodeModule;
  Options: TGocciaCompilerOptimizationOptions;
  SavedGlobalScope: TGocciaScope;
  SavedGlobalBackedTopLevel: Boolean;
begin
  Compiler := TGocciaCompiler.Create(ASourcePath);
  try
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := FNonStrictMode;
    Compiler.ArgumentsObjectEnabled := FArgumentsObjectEnabled;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      (TGocciaCoverageTracker.Instance <> nil) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    Module := Compiler.CompileGlobalEval(AProgram, AStrictEval,
      ADeclarations.DeclaredVarNames);
  finally
    Compiler.Free;
  end;
  if Assigned(FRetainModule) then
    FRetainModule(Module);

  AStage := gesRun;
  if not AStrictEval then
    CheckGlobalEvalDeclarations(FGlobalScope, ADeclarations.VarNames,
      ADeclarations.FunctionNames, ADeclarations.DeclaredVarNames);

  // The eval can run inside a module body, which has the VM's global scope
  // and script-top-level flag set for itself (EvaluateModuleBody); RunModule
  // sets both for the eval, so the module body gets its own back.
  SavedGlobalScope := FVM.GlobalScope;
  SavedGlobalBackedTopLevel := FVM.GlobalBackedTopLevel;
  FVM.GlobalScope := FGlobalScope;
  try
    Result := RunModule(Module, FGlobalScope.ThisValue);
  finally
    FVM.GlobalBackedTopLevel := SavedGlobalBackedTopLevel;
    FVM.GlobalScope := SavedGlobalScope;
  end;
end;

{ The VM's compiler for direct eval at a strict call site (ADR 0134 Phase
  3): AProgram compiled as a function nested at the call that eval site
  record ASite describes, with this engine's language profile. The compiled
  module is kept for the engine's life like the Function constructor's code
  (ADR 0134, decision 1). }
function TGocciaBytecodeExecutor.CompileDirectEvalCode(
  const AProgram: TGocciaProgram; const ASourcePath: string;
  const ASite: TGocciaDirectEvalEnvironment; const AStrictEval: Boolean;
  const ADeclaredVarNames: TUnicodeStringList): TGocciaFunctionTemplate;
var
  Compiler: TGocciaCompiler;
  Module: TGocciaBytecodeModule;
  Options: TGocciaCompilerOptimizationOptions;
begin
  Compiler := TGocciaCompiler.Create(ASourcePath);
  try
    Compiler.StrictTypes := FStrictTypes;
    Compiler.NonStrictMode := FNonStrictMode;
    Compiler.ArgumentsObjectEnabled := FArgumentsObjectEnabled;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      (TGocciaCoverageTracker.Instance <> nil) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    Module := Compiler.CompileDirectEval(AProgram, ASite, AStrictEval,
      ADeclaredVarNames);
  finally
    Compiler.Free;
  end;
  if Assigned(FRetainModule) then
    FRetainModule(Module);
  Result := Module.TopLevel;
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
    Compiler.ArgumentsObjectEnabled := FArgumentsObjectEnabled;
    Options := Compiler.OptimizationOptions;
    Options.PreserveCoverageShape :=
      (TGocciaCoverageTracker.Instance <> nil) and
      TGocciaCoverageTracker.Instance.Enabled;
    Compiler.OptimizationOptions := Options;
    Result := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
end;

{ ES2026 §16.2.1.6.1.2 Link(): a program run here was linked by no module
  loader, so link the graph behind each of its static requests, in source
  order, before it runs. A name it imports or re-exports that does not
  resolve is a SyntaxError raised before any module of the graph evaluates.
  The linked modules are kept for the run, so the program's own OP_IMPORTs
  evaluate them without resolving their requests a second time. }
procedure TGocciaBytecodeExecutor.LinkModuleRequests(
  const AModule: TGocciaBytecodeModule);
var
  I, J: Integer;
  ModuleRequest: TGocciaModuleImport;
  Names: array of string;
begin
  if (AModule.ImportCount = 0) or not Assigned(FModuleLoader) then
    Exit;
  FLinkedModules := TOrderedStringMap<TGocciaModule>.Create;
  FLinkedReferrer := AModule.SourcePath;
  for I := 0 to AModule.ImportCount - 1 do
  begin
    ModuleRequest := AModule.GetImport(I);
    SetLength(Names, Length(ModuleRequest.Bindings));
    for J := 0 to High(ModuleRequest.Bindings) do
      Names[J] := ModuleRequest.Bindings[J].ExportName;
    FLinkedModules.AddOrSetValue(ModuleRequest.ModulePath,
      FModuleLoader.LinkModuleRequest(ModuleRequest.ModulePath,
        AModule.SourcePath, Names, ModuleRequest.Line, ModuleRequest.Column));
  end;
  FVM.LoadModule := LoadLinkedModule;
end;

{ ES2026 §16.2.1.7.3.1 InitializeEnvironment for a module-source entry, the
  Module Record the engine registered as the VM's current runtime module.
  The entry's local exports are bound before its requests are linked, as
  TGocciaEngine.Execute binds them: a module in a cycle with the entry that
  links meanwhile resolves each of them, and its `export * from` the entry
  forwards them (§16.2.1.7.2.1 GetExportedNames), since the loader settles a
  star export's names when the module is linked. A local export is a binding
  in its temporal dead zone, kept in a module environment of its own (the
  program's top-level bindings live in VM registers); the program's
  OP_EXPORTs initialize and update it. }
procedure TGocciaBytecodeExecutor.InitializeEntryLocalExports(
  const AModule: TGocciaBytecodeModule; const AEntry: TGocciaModule);
var
  ExportEnvironment: TGocciaScope;
  Export_: TGocciaModuleExport;
  I: Integer;
begin
  ExportEnvironment := nil;
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    if Export_.Kind <> mekLocal then
      Continue;
    if not Assigned(ExportEnvironment) then
      ExportEnvironment := TGocciaScope.Create(nil, skModule,
        'ModuleExports:' + AEntry.Path);
    ExportEnvironment.PredeclareLexicalBinding(Export_.Name, dtLet);
    AEntry.AddExportBinding(Export_.Name, Export_.Name, ExportEnvironment);
  end;
  if Assigned(ExportEnvironment) then
    AEntry.SetEnvironment(ExportEnvironment);
end;

{ The entry's other exports bind to the linked module of their request, so
  they are bound once its requests are linked and before any of them
  evaluates. }
procedure TGocciaBytecodeExecutor.InitializeEntryLinkedExports(
  const AModule: TGocciaBytecodeModule; const AEntry: TGocciaModule);
var
  Changed: Boolean;
  Export_: TGocciaModuleExport;
  I: Integer;
  SourceModule: TGocciaModule;
  StarName: string;
begin
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    case Export_.Kind of
      mekSource:
        AEntry.AddExportValue(Export_.Name,
          FModuleLoader.LoadModuleSourceValue(Export_.ModuleRequest,
            AModule.SourcePath));
      mekDeferredNamespace:
        AEntry.AddExportValue(Export_.Name,
          FModuleLoader.LoadDeferredModuleNamespaceValue(
            Export_.ModuleRequest, AModule.SourcePath));
      mekIndirect, mekNamespace:
        if Assigned(FLinkedModules) and
           FLinkedModules.TryGetValue(Export_.ModuleRequest, SourceModule) then
        begin
          if Export_.Kind = mekNamespace then
            AEntry.AddExportValue(Export_.Name,
              SourceModule.GetNamespaceObject)
          else
            AEntry.AddExportForwarding(Export_.Name, SourceModule,
              Export_.ImportName);
        end;
    end;
  end;

  { Star exports last: a name the entry exports explicitly is not replaced
    by one a star export would forward (§16.2.1.7.2.2 ResolveExport). }
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    if (Export_.Kind <> mekStar) or not Assigned(FLinkedModules) or
       not FLinkedModules.TryGetValue(Export_.ModuleRequest, SourceModule) then
      Continue;
    repeat
      Changed := False;
      for StarName in SourceModule.GetExportNames do
        if StarName <> KEYWORD_DEFAULT then
          Changed := AEntry.AddStarExportForwarding(StarName, SourceModule,
            StarName) or Changed;
    until not Changed;
  end;
end;

{ The program's OP_IMPORTs, which run in source order after its function
  declarations are instantiated and before any other statement of its body,
  evaluate its linked requests. For a module-source entry this is ES2026
  §16.2.1.6.1.3.1 InnerModuleEvaluation steps 11-12: once its last request
  has evaluated, the entry waits for each request whose evaluation is
  asynchronous (one of its [[PendingAsyncDependencies]]) before its body
  runs, and fails with the error of one that failed. The names the last
  import declaration binds are assigned after this wait. }
function TGocciaBytecodeExecutor.LoadLinkedModule(const AModulePath,
  AImportingFilePath: string): TGocciaModule;
begin
  if Assigned(FLinkedModules) and (AImportingFilePath = FLinkedReferrer) and
     FLinkedModules.TryGetValue(AModulePath, Result) then
  begin
    Result := FModuleLoader.EvaluateModule(Result);
    if Assigned(FEntryRequests) then
    begin
      FEntryRequests.Add(Result);
      Dec(FEntryRequestsLeft);
      if FEntryRequestsLeft = 0 then
        AwaitEntryRequests;
    end;
  end
  else
    Result := FModuleLoader.LoadModule(AModulePath, AImportingFilePath);
end;

procedure TGocciaBytecodeExecutor.AwaitEntryRequests;
var
  I: Integer;
  PendingPromises: TList<TGocciaPromiseValue>;
  Promise: TGocciaPromiseValue;
  Requests: TGocciaModuleList;
begin
  Requests := FEntryRequests;
  FEntryRequests := nil;
  PendingPromises := TList<TGocciaPromiseValue>.Create;
  try
    for I := 0 to Requests.Count - 1 do
    begin
      Promise := TGocciaModuleLoader.EvaluationWaitPromise(Requests[I],
        FVM.CurrentRuntimeModule);
      if Assigned(Promise) and (Promise.State <> gpsFulfilled) and
         (PendingPromises.IndexOf(Promise) < 0) then
        PendingPromises.Add(Promise);
    end;
    if PendingPromises.Count > 0 then
      FVM.AwaitModuleDependencies(
        TGocciaModuleLoader.AllEvaluationsSettled(PendingPromises));
  finally
    PendingPromises.Free;
    Requests.Free;
  end;
end;

function TGocciaBytecodeExecutor.RunCompiledModule(
  const AModule: TGocciaCompiledModule): TGocciaValue;
begin
  Result := RunModule(AModule, nil);
end;

function TGocciaBytecodeExecutor.RunModule(
  const AModule: TGocciaCompiledModule;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  GC: TGarbageCollector;
  HasEntryExports: Boolean;
  SavedLinkedModules: TOrderedStringMap<TGocciaModule>;
  SavedLinkedReferrer: string;
  SavedLoadModule: TLoadModuleCallback;
  SavedEntryRequests: TGocciaModuleList;
  SavedEntryRequestsLeft: Integer;
  WasEnabled: Boolean;
begin
  GC := TGarbageCollector.Instance;
  WasEnabled := GC.Enabled;
  GC.Enabled := False;
  FVM.CoverageEnabled := (TGocciaCoverageTracker.Instance <> nil)
    and TGocciaCoverageTracker.Instance.Enabled;
  FVM.ProfilingOpcodes := (TGocciaProfiler.Instance <> nil)
    and TGocciaProfiler.Instance.Enabled
    and (pmOpcodes in TGocciaProfiler.Instance.Mode);
  FVM.ProfilingFunctions := (TGocciaProfiler.Instance <> nil)
    and TGocciaProfiler.Instance.Enabled
    and (pmFunctions in TGocciaProfiler.Instance.Mode);
  FVM.GlobalBackedTopLevel := FGlobalBackedTopLevel;
  { A run can nest (the Function constructor runs its body here), so the
    outer run's linked modules are set aside, not overwritten. }
  SavedLinkedModules := FLinkedModules;
  SavedLinkedReferrer := FLinkedReferrer;
  SavedLoadModule := FVM.LoadModule;
  SavedEntryRequests := FEntryRequests;
  SavedEntryRequestsLeft := FEntryRequestsLeft;
  FLinkedModules := nil;
  FEntryRequests := nil;
  try
    HasEntryExports := Assigned(FVM.CurrentRuntimeModule) and
      (TGocciaBytecodeModule(AModule).ExportCount > 0);
    if HasEntryExports then
      InitializeEntryLocalExports(TGocciaBytecodeModule(AModule),
        FVM.CurrentRuntimeModule);
    LinkModuleRequests(TGocciaBytecodeModule(AModule));
    if HasEntryExports then
      InitializeEntryLinkedExports(TGocciaBytecodeModule(AModule),
        FVM.CurrentRuntimeModule);
    if Assigned(FVM.CurrentRuntimeModule) and Assigned(FLinkedModules) then
    begin
      FEntryRequests := TGocciaModuleList.Create;
      FEntryRequestsLeft := TGocciaBytecodeModule(AModule).ImportCount;
    end;
    Result := FVM.ExecuteModule(TGocciaBytecodeModule(AModule), AThisValue);
  finally
    FEntryRequests.Free;
    FEntryRequests := SavedEntryRequests;
    FEntryRequestsLeft := SavedEntryRequestsLeft;
    FVM.LoadModule := SavedLoadModule;
    FLinkedModules.Free;
    FLinkedModules := SavedLinkedModules;
    FLinkedReferrer := SavedLinkedReferrer;
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
