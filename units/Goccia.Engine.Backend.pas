unit Goccia.Engine.Backend;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.Interpreter,
  Goccia.JSON,
  Goccia.JSON5,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.Runtime.Bootstrap,
  Goccia.TOML,
  Goccia.Values.Primitives,
  Goccia.VM,
  Goccia.YAML;

type
  TGocciaEngineBackend = (
    ebTreeWalk,
    ebBytecode
  );

  TGocciaBytecodeBackend = class
  private
    FMinimalVM: TGocciaVM;
    FSourcePath: string;
    FInterpreter: TGocciaInterpreter;
    FBootstrap: TGocciaRuntimeBootstrap;
    FModuleLoader: TGocciaModuleLoader;
    FBootstrapSource: TStringList;
    FOwnsModuleLoader: Boolean;
    FInjectedGlobals: TStringList;
    FPreprocessors: TGocciaPreprocessors;
    FCompatibility: TGocciaCompatibilityFlags;
    FStrictTypes: Boolean;
    FShims: TStringList;
    FGlobalBackedTopLevel: Boolean;
    function GetASIEnabled: Boolean;
    procedure SetASIEnabled(const AValue: Boolean);
    function GetContentProvider: TGocciaModuleContentProvider;
    function GetModuleResolver: TGocciaModuleResolver;
    procedure RequireGlobalsBootstrapReady;
    procedure ExecuteShims;
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
  public
    constructor Create(const ASourcePath: string); overload;
    constructor Create(const ASourcePath: string;
      const AModuleLoader: TGocciaModuleLoader); overload;
    destructor Destroy; override;

    function CompileAndRun(const AProgram: TGocciaProgram): TGocciaValue;
    function CompileToModule(const AProgram: TGocciaProgram): TGocciaBytecodeModule;
    function RunModule(const AModule: TGocciaBytecodeModule): TGocciaValue;

    procedure RegisterGlobal(const AName: string; const AValue: TGocciaValue);
    procedure InjectGlobalsFromJSON(const AJsonString: string);
    procedure InjectGlobalsFromJSON5(const AJSON5String: UTF8String);
    procedure InjectGlobalsFromTOML(const ATOMLString: UTF8String);
    procedure InjectGlobalsFromYAML(const AYamlString: string);
    procedure InjectGlobalsFromModule(const APath: string);
    procedure RegisterBuiltIns(const AGlobals: TGocciaGlobalBuiltins);
    procedure ClearTransientCaches;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property Bootstrap: TGocciaRuntimeBootstrap read FBootstrap;
    property ContentProvider: TGocciaModuleContentProvider read GetContentProvider;
    property GlobalBackedTopLevel: Boolean read FGlobalBackedTopLevel
      write FGlobalBackedTopLevel;
    property Preprocessors: TGocciaPreprocessors read FPreprocessors write FPreprocessors;
    property Compatibility: TGocciaCompatibilityFlags read FCompatibility write FCompatibility;
    property ASIEnabled: Boolean read GetASIEnabled write SetASIEnabled;
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
    property Shims: TStringList read FShims;
    property ModuleLoader: TGocciaModuleLoader read FModuleLoader;
    property ModuleResolver: TGocciaModuleResolver read GetModuleResolver;
  end;

implementation

uses
  SysUtils,

  Goccia.CallStack,
  Goccia.Coverage,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Profiler,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Scope.Redeclaration,
  Goccia.Shims,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue;

{ TGocciaBytecodeBackend }

constructor TGocciaBytecodeBackend.Create(const ASourcePath: string);
begin
  inherited Create;
  TGarbageCollector.Initialize;
  TGocciaCallStack.Initialize;
  TGocciaMicrotaskQueue.Initialize;
  FSourcePath := ASourcePath;
  FModuleLoader := TGocciaModuleLoader.Create(ASourcePath);
  FOwnsModuleLoader := True;
  FMinimalVM := TGocciaVM.Create;
  FInterpreter := nil;
  FBootstrap := nil;
  FBootstrapSource := nil;
  FInjectedGlobals := TStringList.Create;
  FPreprocessors := TGocciaEngine.DefaultPreprocessors;
  FCompatibility := TGocciaEngine.DefaultCompatibility;
  FStrictTypes := True;
  FShims := TStringList.Create;
end;

constructor TGocciaBytecodeBackend.Create(const ASourcePath: string;
  const AModuleLoader: TGocciaModuleLoader);
begin
  inherited Create;
  TGarbageCollector.Initialize;
  TGocciaCallStack.Initialize;
  TGocciaMicrotaskQueue.Initialize;
  FSourcePath := ASourcePath;
  FModuleLoader := AModuleLoader;
  FOwnsModuleLoader := False;
  if not Assigned(FModuleLoader) then
  begin
    FModuleLoader := TGocciaModuleLoader.Create(ASourcePath);
    FOwnsModuleLoader := True;
  end;
  FMinimalVM := TGocciaVM.Create;
  FInterpreter := nil;
  FBootstrap := nil;
  FBootstrapSource := nil;
  FInjectedGlobals := TStringList.Create;
  FPreprocessors := TGocciaEngine.DefaultPreprocessors;
  FCompatibility := TGocciaEngine.DefaultCompatibility;
  FStrictTypes := True;
  FShims := TStringList.Create;
end;

destructor TGocciaBytecodeBackend.Destroy;
begin
  if Assigned(TGarbageCollector.Instance) and Assigned(FInterpreter) then
    TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);
  FMinimalVM.Free;
  FBootstrap.Free;
  FInterpreter.Free;
  FBootstrapSource.Free;
  FInjectedGlobals.Free;
  FShims.Free;
  if FOwnsModuleLoader then
    FModuleLoader.Free;
  inherited;
end;

procedure TGocciaBytecodeBackend.ThrowError(const AMessage: string; const ALine,
  AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FSourcePath, FBootstrapSource);
end;

function TGocciaBytecodeBackend.CompileAndRun(
  const AProgram: TGocciaProgram): TGocciaValue;
var
  Module: TGocciaBytecodeModule;
begin
  Module := CompileToModule(AProgram);
  try
    Result := RunModule(Module);
  finally
    Module.Free;
  end;
end;

function TGocciaBytecodeBackend.CompileToModule(
  const AProgram: TGocciaProgram): TGocciaBytecodeModule;
var
  Compiler: TGocciaCompiler;
begin
  if FGlobalBackedTopLevel and Assigned(FInterpreter) then
    CheckTopLevelRedeclarations(AProgram, FInterpreter.GlobalScope,
      FSourcePath);

  Compiler := TGocciaCompiler.Create(FSourcePath);
  try
    Compiler.GlobalBackedTopLevel := FGlobalBackedTopLevel;
    Result := Compiler.Compile(AProgram);
  finally
    Compiler.Free;
  end;
end;

function TGocciaBytecodeBackend.RunModule(
  const AModule: TGocciaBytecodeModule): TGocciaValue;
var
  GC: TGarbageCollector;
  WasEnabled: Boolean;
begin
  GC := TGarbageCollector.Instance;
  WasEnabled := GC.Enabled;
  GC.Enabled := False;
  FMinimalVM.CoverageEnabled := Assigned(TGocciaCoverageTracker.Instance)
    and TGocciaCoverageTracker.Instance.Enabled;
  FMinimalVM.ProfilingOpcodes := Assigned(TGocciaProfiler.Instance)
    and TGocciaProfiler.Instance.Enabled
    and (pmOpcodes in TGocciaProfiler.Instance.Mode);
  FMinimalVM.ProfilingFunctions := Assigned(TGocciaProfiler.Instance)
    and TGocciaProfiler.Instance.Enabled
    and (pmFunctions in TGocciaProfiler.Instance.Mode);
  GProfilingAllocations := FMinimalVM.ProfilingFunctions;
  try
    Result := FMinimalVM.ExecuteModule(AModule);
  finally
    GProfilingAllocations := False;
    GC.Enabled := WasEnabled;
  end;
end;

procedure TGocciaBytecodeBackend.RegisterGlobal(const AName: string;
  const AValue: TGocciaValue);
begin
  RequireGlobalsBootstrapReady;
  if FInterpreter.GlobalScope.ContainsOwnLexicalBinding(AName) then
  begin
    if FInjectedGlobals.IndexOf(AName) >= 0 then
      FInterpreter.GlobalScope.ForceUpdateBinding(AName, AValue)
    else
      raise TGocciaRuntimeError.Create(
        'Cannot override built-in global "' + AName + '" via globals injection.',
        0, 0, FSourcePath, FBootstrapSource);
  end
  else
  begin
    FInterpreter.GlobalScope.DefineLexicalBinding(AName, AValue, dtConst);
    FInjectedGlobals.Add(AName);
  end;
end;

procedure TGocciaBytecodeBackend.RequireGlobalsBootstrapReady;
begin
  if Assigned(FInterpreter) then
    Exit;
  raise TGocciaRuntimeError.Create(
    'RegisterBuiltIns must be called before globals injection.',
    0, 0, FSourcePath, nil);
end;

procedure TGocciaBytecodeBackend.InjectGlobalsFromJSON(const AJsonString: string);
var
  Parser: TGocciaJSONParser;
  ParsedValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Key: string;
begin
  RequireGlobalsBootstrapReady;
  Parser := TGocciaJSONParser.Create;
  try
    ParsedValue := Parser.Parse(AJsonString);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise TGocciaRuntimeError.Create('Globals JSON must be a top-level object.',
      0, 0, FSourcePath, nil);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Obj := TGocciaObjectValue(ParsedValue);
    for Key in Obj.GetOwnPropertyKeys do
      RegisterGlobal(Key, Obj.GetProperty(Key));
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaBytecodeBackend.InjectGlobalsFromJSON5(
  const AJSON5String: UTF8String);
var
  Key: string;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  Parser: TGocciaJSON5Parser;
begin
  RequireGlobalsBootstrapReady;
  Parser := TGocciaJSON5Parser.Create;
  try
    ParsedValue := Parser.Parse(AJSON5String);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise TGocciaRuntimeError.Create('Globals JSON5 must be a top-level object.',
      0, 0, FSourcePath, nil);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Obj := TGocciaObjectValue(ParsedValue);
    for Key in Obj.GetOwnPropertyKeys do
      RegisterGlobal(Key, Obj.GetProperty(Key));
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaBytecodeBackend.InjectGlobalsFromTOML(const ATOMLString: UTF8String);
var
  Key: string;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  Parser: TGocciaTOMLParser;
begin
  RequireGlobalsBootstrapReady;
  Parser := TGocciaTOMLParser.Create;
  try
    ParsedValue := Parser.Parse(ATOMLString);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise TGocciaRuntimeError.Create('Globals TOML must be a top-level object.',
      0, 0, FSourcePath, nil);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Obj := TGocciaObjectValue(ParsedValue);
    for Key in Obj.GetOwnPropertyKeys do
      RegisterGlobal(Key, Obj.GetProperty(Key));
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure TGocciaBytecodeBackend.InjectGlobalsFromYAML(const AYamlString: string);
var
  Documents: TGocciaArrayValue;
  ParsedDocument: TGocciaValue;
  Parser: TGocciaYAMLParser;
  Obj: TGocciaObjectValue;
  Key: string;
begin
  RequireGlobalsBootstrapReady;
  Parser := TGocciaYAMLParser.Create;
  try
    Documents := Parser.ParseDocuments(AYamlString);
  finally
    Parser.Free;
  end;

  try
    if Documents.Elements.Count <> 1 then
      raise TGocciaRuntimeError.Create(
        'Globals YAML must contain exactly one top-level document.',
        0, 0, FSourcePath, nil);

    ParsedDocument := Documents.Elements[0];
    if not (ParsedDocument is TGocciaObjectValue) then
      raise TGocciaRuntimeError.Create(
        'Globals YAML must be a top-level object.', 0, 0, FSourcePath, nil);

    TGarbageCollector.Instance.AddTempRoot(ParsedDocument);
    Obj := TGocciaObjectValue(ParsedDocument);
    try
      for Key in Obj.GetOwnPropertyKeys do
        RegisterGlobal(Key, Obj.GetProperty(Key));
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ParsedDocument);
    end;
  finally
    Documents.Free;
  end;
end;

procedure TGocciaBytecodeBackend.InjectGlobalsFromModule(const APath: string);
var
  Module: TGocciaModule;
  ExportPair: TGocciaValueMap.TKeyValuePair;
begin
  RequireGlobalsBootstrapReady;
  Module := FInterpreter.LoadModule(APath, FSourcePath);
  for ExportPair in Module.ExportsTable do
    RegisterGlobal(ExportPair.Key, ExportPair.Value);
end;

procedure TGocciaBytecodeBackend.ExecuteShims;
var
  I: Integer;
  Shim: TGocciaShimDefinition;
begin
  for I := 0 to DefaultShimCount - 1 do
  begin
    Shim := DefaultShim(I);
    FInterpreter.GlobalScope.DefineLexicalBinding(Shim.Name,
      LoadShimValue(FInterpreter, Shim), dtConst);
  end;
end;

procedure TGocciaBytecodeBackend.RegisterBuiltIns(
  const AGlobals: TGocciaGlobalBuiltins);
var
  EmptySource: TStringList;
begin
  FreeAndNil(FBootstrap);
  if Assigned(TGarbageCollector.Instance) and Assigned(FInterpreter) then
    TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);
  FreeAndNil(FInterpreter);
  FreeAndNil(FBootstrapSource);

  RegisterDefaultShimNames(FShims);

  EmptySource := TStringList.Create;
  EmptySource.Text := '';
  FBootstrapSource := EmptySource;
  FInterpreter := TGocciaInterpreter.Create(FSourcePath, FBootstrapSource,
    FModuleLoader);
  FInterpreter.JSXEnabled := ppJSX in FPreprocessors;
  FInterpreter.ASIEnabled := cfASI in FCompatibility;
  TGarbageCollector.Instance.AddRootObject(FInterpreter.GlobalScope);
  FBootstrap := TGocciaRuntimeBootstrap.Create(FInterpreter, AGlobals, ThrowError,
    FStrictTypes, FShims);

  FMinimalVM.GlobalScope := FInterpreter.GlobalScope;
  FMinimalVM.Interpreter := FInterpreter;

  ExecuteShims;
end;

function TGocciaBytecodeBackend.GetASIEnabled: Boolean;
begin
  Result := cfASI in FCompatibility;
end;

procedure TGocciaBytecodeBackend.SetASIEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfASI)
  else
    Exclude(FCompatibility, cfASI);
end;

function TGocciaBytecodeBackend.GetContentProvider: TGocciaModuleContentProvider;
begin
  Result := FModuleLoader.ContentProvider;
end;

function TGocciaBytecodeBackend.GetModuleResolver: TGocciaModuleResolver;
begin
  Result := FModuleLoader.Resolver;
end;

procedure TGocciaBytecodeBackend.ClearTransientCaches;
begin
  // The Goccia VM executes directly on TGocciaValue and does not maintain
  // bridge-side transient caches that need per-measurement clearing.
end;

end.
