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
    function GetContentProvider: TGocciaModuleContentProvider;
    function GetModuleResolver: TGocciaModuleResolver;
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
    procedure InjectGlobalsFromTOML(const ATOMLString: string);
    procedure InjectGlobalsFromYAML(const AYamlString: string);
    procedure InjectGlobalsFromModule(const APath: string);
    procedure RegisterBuiltIns(const AGlobals: TGocciaGlobalBuiltins);
    procedure ClearTransientCaches;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property Bootstrap: TGocciaRuntimeBootstrap read FBootstrap;
    property ContentProvider: TGocciaModuleContentProvider read GetContentProvider;
    property ModuleLoader: TGocciaModuleLoader read FModuleLoader;
    property ModuleResolver: TGocciaModuleResolver read GetModuleResolver;
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,

  Goccia.CallStack,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
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
  Compiler := TGocciaCompiler.Create(FSourcePath);
  try
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
  try
    Result := FMinimalVM.ExecuteModule(AModule);
  finally
    GC.Enabled := WasEnabled;
  end;
end;

procedure TGocciaBytecodeBackend.RegisterGlobal(const AName: string;
  const AValue: TGocciaValue);
begin
  if not Assigned(FInterpreter) then
    Exit;
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

procedure TGocciaBytecodeBackend.InjectGlobalsFromJSON(const AJsonString: string);
var
  Parser: TGocciaJSONParser;
  ParsedValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Key: string;
begin
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

procedure TGocciaBytecodeBackend.InjectGlobalsFromTOML(const ATOMLString: string);
var
  Key: string;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  Parser: TGocciaTOMLParser;
begin
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
  if not Assigned(FInterpreter) then
    Exit;
  Module := FInterpreter.LoadModule(APath, FSourcePath);
  for ExportPair in Module.ExportsTable do
    RegisterGlobal(ExportPair.Key, ExportPair.Value);
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

  EmptySource := TStringList.Create;
  EmptySource.Text := '';
  FBootstrapSource := EmptySource;
  FInterpreter := TGocciaInterpreter.Create(FSourcePath, FBootstrapSource,
    FModuleLoader);
  FInterpreter.JSXEnabled := ggJSX in AGlobals;
  TGarbageCollector.Instance.AddRootObject(FInterpreter.GlobalScope);
  FBootstrap := TGocciaRuntimeBootstrap.Create(FInterpreter, AGlobals, ThrowError);

  FMinimalVM.GlobalScope := FInterpreter.GlobalScope;
  FMinimalVM.Interpreter := FInterpreter;

  if FInterpreter.GlobalScope.GetValue('GocciaScript') is TGocciaObjectValue then
    TGocciaObjectValue(FInterpreter.GlobalScope.GetValue('GocciaScript'))
      .AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.TrueValue);
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
