unit Goccia.Engine.Backend;

{$I Goccia.inc}

interface

uses
  Souffle.Bytecode.Module,
  Souffle.VM,
  Souffle.VM.RuntimeOperations,

  Goccia.AST.Node,
  Goccia.Compiler,
  Goccia.Engine,
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
    FEngine: TGocciaEngine;
    FModuleResolver: TGocciaModuleResolver;
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
    property Engine: TGocciaEngine read FEngine;
  end;

implementation

uses
  Classes,
  SysUtils,

  GarbageCollector.Generic,
  Modules.Resolver,
  OrderedStringMap,
  Souffle.Bytecode.Chunk,
  Souffle.Value,
  Souffle.VM.Exception,

  Goccia.FileExtensions,
  Goccia.Interpreter,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.Error;

{ TGocciaSouffleBackend }

constructor TGocciaSouffleBackend.Create(const ASourcePath: string);
begin
  inherited Create;
  TGarbageCollector.Initialize;
  FSourcePath := ASourcePath;
  FModuleResolver := TGocciaModuleResolver.Create(
    ExtractFilePath(ExpandFileName(ASourcePath)));
  FRuntime := TGocciaRuntimeOperations.Create;
  FVM := TSouffleVM.Create(FRuntime);
  FRuntime.VM := FVM;
  FRuntime.Engine := nil;
  FRuntime.ModuleResolver := FModuleResolver;
  FEngine := nil;
end;

destructor TGocciaSouffleBackend.Destroy;
begin
  {$IFDEF BRIDGE_METRICS}
  TBridgeMetrics.DumpGlobal;
  {$ENDIF}
  FVM.Free;
  FRuntime.Free;
  FEngine.Free;
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
var
  EmptySource: TStringList;
  GlobalScope: TGocciaScope;
  Names: TGocciaStringArray;
  AliasPair: TOrderedStringMap<string>.TKeyValuePair;
  I: Integer;
begin
  FreeAndNil(FEngine);
  FRuntime.Engine := nil;

  EmptySource := TStringList.Create;
  try
    FEngine := TGocciaEngine.Create(FSourcePath, EmptySource, AGlobals);
  finally
    EmptySource.Free;
  end;

  FRuntime.Engine := FEngine;
  FRuntime.SourcePath := FSourcePath;

  if Assigned(FEngine.Interpreter.Resolver) then
    for AliasPair in FEngine.Interpreter.Resolver.Aliases do
      FModuleResolver.AddAlias(AliasPair.Key, AliasPair.Value);

  GlobalScope := FEngine.Interpreter.GlobalScope;
  Names := GlobalScope.GetOwnBindingNames;
  for I := 0 to Length(Names) - 1 do
  begin
    if GlobalScope.GetLexicalBinding(Names[I]).DeclarationType = dtConst then
      FRuntime.RegisterConstGlobal(Names[I],
        FRuntime.ToSouffleValue(GlobalScope.GetValue(Names[I])))
    else
      RegisterGlobal(Names[I], GlobalScope.GetValue(Names[I]));
  end;

  FRuntime.PatchGocciaScriptStrictTypes;
  FRuntime.RegisterDelegates;
  FRuntime.RegisterTestNatives;
  FRuntime.RegisterNativeBuiltins;
end;

end.
