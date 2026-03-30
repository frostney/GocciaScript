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
  Goccia.MicrotaskQueue,
  Goccia.Modules.Resolver,
  Goccia.Runtime.Bootstrap,
  Goccia.Values.Primitives,
  Goccia.VM;

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
    FModuleResolver: TGocciaModuleResolver;
    FBootstrapSource: TStringList;
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
  public
    constructor Create(const ASourcePath: string);
    destructor Destroy; override;

    function CompileAndRun(const AProgram: TGocciaProgram): TGocciaValue;
    function CompileToModule(const AProgram: TGocciaProgram): TGocciaBytecodeModule;
    function RunModule(const AModule: TGocciaBytecodeModule): TGocciaValue;

    procedure RegisterGlobal(const AName: string; const AValue: TGocciaValue);
    procedure RegisterBuiltIns(const AGlobals: TGocciaGlobalBuiltins);
    procedure ClearTransientCaches;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property Bootstrap: TGocciaRuntimeBootstrap read FBootstrap;
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,
  ModuleResolver,
  OrderedStringMap,

  Goccia.CallStack,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
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
  FModuleResolver := TGocciaModuleResolver.Create(
    ExtractFilePath(ExpandFileName(ASourcePath)));
  FMinimalVM := TGocciaVM.Create;
  FInterpreter := nil;
  FBootstrap := nil;
  FBootstrapSource := nil;
end;

destructor TGocciaBytecodeBackend.Destroy;
begin
  if Assigned(TGarbageCollector.Instance) and Assigned(FInterpreter) then
    TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);
  FMinimalVM.Free;
  FBootstrap.Free;
  FInterpreter.Free;
  FBootstrapSource.Free;
  FModuleResolver.Free;
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
  FInterpreter.GlobalScope.DefineLexicalBinding(AName, AValue, dtConst);
end;

procedure TGocciaBytecodeBackend.RegisterBuiltIns(
  const AGlobals: TGocciaGlobalBuiltins);
var
  EmptySource: TStringList;
  AliasPair: TOrderedStringMap<string>.TKeyValuePair;
begin
  FreeAndNil(FBootstrap);
  if Assigned(TGarbageCollector.Instance) and Assigned(FInterpreter) then
    TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);
  FreeAndNil(FInterpreter);
  FreeAndNil(FBootstrapSource);

  EmptySource := TStringList.Create;
  EmptySource.Text := '';
  FBootstrapSource := EmptySource;
  FInterpreter := TGocciaInterpreter.Create(FSourcePath, FBootstrapSource);
  FInterpreter.JSXEnabled := ggJSX in AGlobals;
  FInterpreter.Resolver := FModuleResolver;
  TGarbageCollector.Instance.AddRootObject(FInterpreter.GlobalScope);
  FBootstrap := TGocciaRuntimeBootstrap.Create(FInterpreter, AGlobals, ThrowError);

  FMinimalVM.GlobalScope := FInterpreter.GlobalScope;
  FMinimalVM.Interpreter := FInterpreter;

  if Assigned(FInterpreter.Resolver) then
    for AliasPair in FInterpreter.Resolver.Aliases do
      FModuleResolver.AddAlias(AliasPair.Key, AliasPair.Value);

  if FInterpreter.GlobalScope.GetValue('GocciaScript') is TGocciaObjectValue then
    TGocciaObjectValue(FInterpreter.GlobalScope.GetValue('GocciaScript'))
      .AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.TrueValue);
end;

procedure TGocciaBytecodeBackend.ClearTransientCaches;
begin
  // The Goccia VM executes directly on TGocciaValue and does not maintain
  // bridge-side transient caches that need per-measurement clearing.
end;

end.
