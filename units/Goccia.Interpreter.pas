unit Goccia.Interpreter;

{$I Goccia.inc}

interface

uses
  Classes,
  Math,
  SysUtils,

  OrderedStringMap,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.ControlFlow,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.Logger,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.Scope,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaInterpreter = class
  private
    FGlobalScope: TGocciaGlobalScope;
    FFileName: string;
    FSourceLines: TStringList;
    FJSXEnabled: Boolean;
    FModuleLoader: TGocciaModuleLoader;
    FOwnsModuleLoader: Boolean;

    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function GetContentProvider: TGocciaModuleContentProvider;
    function GetGlobalModules: TOrderedStringMap<TGocciaModule>;
    function GetResolver: TGocciaModuleResolver;
    procedure SetJSXEnabled(const AValue: Boolean);
    procedure SetResolver(const AValue: TGocciaModuleResolver);
  public
    function CreateEvaluationContext: TGocciaEvaluationContext;
    constructor Create(const AFileName: string; const ASourceLines: TStringList); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList;
      const AModuleLoader: TGocciaModuleLoader); overload;
    destructor Destroy; override;
    function Execute(const AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const AModulePath, AImportingFilePath: string): TGocciaModule;
    procedure CheckForModuleReload(const AModule: TGocciaModule);

    property GlobalScope: TGocciaGlobalScope read FGlobalScope;
    property JSXEnabled: Boolean read FJSXEnabled write SetJSXEnabled;
    property ContentProvider: TGocciaModuleContentProvider read GetContentProvider;
    property ModuleLoader: TGocciaModuleLoader read FModuleLoader;
    property Resolver: TGocciaModuleResolver read GetResolver write SetResolver;
    property GlobalModules: TOrderedStringMap<TGocciaModule> read GetGlobalModules;
  end;


implementation

uses
  GarbageCollector.Generic;

{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  const ASourceLines: TStringList);
begin
  Create(AFileName, ASourceLines, nil);
end;

constructor TGocciaInterpreter.Create(const AFileName: string;
  const ASourceLines: TStringList;
  const AModuleLoader: TGocciaModuleLoader);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobalScope := TGocciaGlobalScope.Create;
  if Assigned(AModuleLoader) then
  begin
    FModuleLoader := AModuleLoader;
    FOwnsModuleLoader := False;
  end
  else
  begin
    FModuleLoader := TGocciaModuleLoader.Create(AFileName);
    FOwnsModuleLoader := True;
  end;
  FModuleLoader.BindRuntime(FGlobalScope, ThrowError);
end;

destructor TGocciaInterpreter.Destroy;
begin
  if not Assigned(TGarbageCollector.Instance) then
    FGlobalScope.Free;
  if FOwnsModuleLoader then
    FModuleLoader.Free;

  inherited;
end;


function TGocciaInterpreter.CreateEvaluationContext: TGocciaEvaluationContext;
begin
  Result.Scope := FGlobalScope;
  Result.OnError := ThrowError;
  Result.LoadModule := LoadModule;
  Result.CurrentFilePath := FFileName;
end;

function TGocciaInterpreter.Execute(const AProgram: TGocciaProgram): TGocciaValue;
var
  I: Integer;
  CF: TGocciaControlFlow;
  Context: TGocciaEvaluationContext;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  Context := CreateEvaluationContext;

  for I := 0 to AProgram.Body.Count - 1 do
  begin
    CF := EvaluateStatement(AProgram.Body[I], Context);
    Result := CF.Value;
    if CF.Kind = cfkReturn then Exit;
  end;
end;

function TGocciaInterpreter.LoadModule(const AModulePath, AImportingFilePath: string): TGocciaModule;
begin
  Result := FModuleLoader.LoadModule(AModulePath, AImportingFilePath);
end;

procedure TGocciaInterpreter.CheckForModuleReload(const AModule: TGocciaModule);
begin
  FModuleLoader.CheckForModuleReload(AModule);
end;

function TGocciaInterpreter.GetContentProvider: TGocciaModuleContentProvider;
begin
  Result := FModuleLoader.ContentProvider;
end;

function TGocciaInterpreter.GetGlobalModules: TOrderedStringMap<TGocciaModule>;
begin
  Result := FModuleLoader.GlobalModules;
end;

function TGocciaInterpreter.GetResolver: TGocciaModuleResolver;
begin
  Result := FModuleLoader.Resolver;
end;

procedure TGocciaInterpreter.SetJSXEnabled(const AValue: Boolean);
begin
  FJSXEnabled := AValue;
  FModuleLoader.JSXEnabled := AValue;
end;

procedure TGocciaInterpreter.SetResolver(const AValue: TGocciaModuleResolver);
begin
  if Assigned(AValue) and (AValue <> FModuleLoader.Resolver) then
    raise Exception.Create(
      'TGocciaInterpreter resolver is owned by the module loader. ' +
      'Create the interpreter with a configured TGocciaModuleLoader instead.');
end;

procedure TGocciaInterpreter.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.
