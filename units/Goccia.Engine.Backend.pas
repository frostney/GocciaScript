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

  Souffle.Heap,
  Souffle.Value,

  Goccia.AST.Statements,
  Goccia.Evaluator,
  Goccia.Interpreter,
  Goccia.Scope,
  Goccia.Scope.BindingMap;

{ TGocciaSouffleBackend }

constructor TGocciaSouffleBackend.Create(const ASourcePath: string);
begin
  inherited Create;
  FSourcePath := ASourcePath;
  FRuntime := TGocciaRuntimeOperations.Create;
  FVM := TSouffleVM.Create(FRuntime);
  FRuntime.VM := FVM;
  FRuntime.Engine := nil;
  FEngine := nil;
end;

destructor TGocciaSouffleBackend.Destroy;
begin
  FVM.Free;
  FRuntime.Free;
  FEngine.Free;
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
  I: Integer;
  Entry: TGocciaCompilerClassEntry;
  Context: TGocciaEvaluationContext;
  ClassValue: TGocciaValue;
begin
  Compiler := TGocciaCompiler.Create(FSourcePath);
  try
    Result := Compiler.Compile(AProgram);

    if Assigned(FEngine) and (Compiler.PendingClassCount > 0) then
    begin
      Context := FEngine.Interpreter.CreateEvaluationContext;
      for I := 0 to Compiler.PendingClassCount - 1 do
      begin
        Entry := Compiler.GetPendingClass(I);
        ClassValue := EvaluateClassDefinition(
          Entry.ClassDeclaration.ClassDefinition,
          Context, Entry.Line, Entry.Column);
        if Assigned(ClassValue) then
        begin
          RegisterGlobal(Entry.ClassDeclaration.ClassDefinition.Name, ClassValue);
          Context.Scope.DefineLexicalBinding(
            Entry.ClassDeclaration.ClassDefinition.Name, ClassValue, dtConst);
        end;
      end;
    end;
  finally
    Compiler.Free;
  end;
end;

function TGocciaSouffleBackend.RunModule(
  const AModule: TSouffleBytecodeModule): TGocciaValue;
var
  SouffleResult: TSouffleValue;
begin
  SouffleResult := FVM.Execute(AModule);
  Result := FRuntime.UnwrapToGocciaValue(SouffleResult);
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
  I: Integer;
begin
  EmptySource := TStringList.Create;
  try
    FEngine := TGocciaEngine.Create(FSourcePath, EmptySource, AGlobals);
  finally
    EmptySource.Free;
  end;

  FRuntime.Engine := FEngine;

  GlobalScope := FEngine.Interpreter.GlobalScope;
  Names := GlobalScope.GetOwnBindingNames;
  for I := 0 to Length(Names) - 1 do
    RegisterGlobal(Names[I], GlobalScope.GetValue(Names[I]));
end;

end.
