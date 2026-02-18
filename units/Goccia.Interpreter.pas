unit Goccia.Interpreter;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Interfaces,
  Goccia.Lexer,
  Goccia.Logger,
  Goccia.Modules,
  Goccia.Parser,
  Goccia.Scope,
  Goccia.Token,
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
    FModules: TDictionary<string, TGocciaModule>;
    FFileName: string;
    FSourceLines: TStringList;

    // Helper methods
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function CreateEvaluationContext: TGocciaEvaluationContext;
  public
    // Filename and SourceLines are required for error messages with source context.
    // A future improvement could encapsulate these in a TGocciaSourceInfo record
    // shared by the lexer, parser, and interpreter.
    constructor Create(const AFileName: string; const ASourceLines: TStringList);
    destructor Destroy; override;
    function Execute(const AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const APath: string): TGocciaModule;
    procedure CheckForModuleReload(const AModule: TGocciaModule);

    property GlobalScope: TGocciaGlobalScope read FGlobalScope;
  end;


implementation

uses
  Goccia.GarbageCollector;

{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  const ASourceLines: TStringList);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobalScope := TGocciaGlobalScope.Create;
  FModules := TDictionary<string, TGocciaModule>.Create;
end;

destructor TGocciaInterpreter.Destroy;
begin
  // FGlobalScope is GC-managed when GC is active; only free manually otherwise
  if not Assigned(TGocciaGarbageCollector.Instance) then
    FGlobalScope.Free;
  FModules.Free;
  // Don't free FSourceLines - we don't own it, it's owned by the caller

  inherited;
end;


function TGocciaInterpreter.CreateEvaluationContext: TGocciaEvaluationContext;
begin
  Result.Scope := FGlobalScope;
  Result.OnError := ThrowError;
  Result.LoadModule := LoadModule;
end;

function TGocciaInterpreter.Execute(const AProgram: TGocciaProgram): TGocciaValue;
var
  I: Integer;
  Context: TGocciaEvaluationContext;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  Context := CreateEvaluationContext;

  for I := 0 to AProgram.Body.Count - 1 do
    Result := EvaluateStatement(AProgram.Body[I], Context);
end;

function TGocciaInterpreter.LoadModule(const APath: string): TGocciaModule;
var
  Source: TStringList;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Module: TGocciaModule;
  ModuleScope: TGocciaScope;
  I: Integer;
  Stmt: TGocciaStatement;
  ExportDecl: TGocciaExportDeclaration;
  ExportPair: TPair<string, string>;
  Value: TGocciaValue;
  Context: TGocciaEvaluationContext;
begin
  if FModules.TryGetValue(APath, Result) then
  begin
    CheckForModuleReload(Result);
    Exit;
  end;

  Source := TStringList.Create;
  try
    Source.LoadFromFile(APath);

    Lexer := TGocciaLexer.Create(Source.Text, APath);
    try
      Parser := TGocciaParser.Create(Lexer.ScanTokens, APath, Source);
      try
        ProgramNode := Parser.Parse;
        try
          Module := TGocciaModule.Create(APath);
          Module.LastModified := FileDateToDateTime(FileAge(APath));
          FModules.Add(APath, Module);

          // Execute module in its own isolated scope (child of global)
          ModuleScope := FGlobalScope.CreateChild(skModule, 'Module:' + APath);
          Context.Scope := ModuleScope;
          Context.OnError := ThrowError;
          Context.LoadModule := LoadModule;

          for I := 0 to ProgramNode.Body.Count - 1 do
            EvaluateStatement(ProgramNode.Body[I], Context);

          // Process exports from the module scope (not global)
          for I := 0 to ProgramNode.Body.Count - 1 do
          begin
            Stmt := ProgramNode.Body[I];
            if Stmt is TGocciaExportDeclaration then
            begin
              ExportDecl := TGocciaExportDeclaration(Stmt);
              for ExportPair in ExportDecl.ExportsTable do
              begin
                Value := ModuleScope.GetValue(ExportPair.Value);
                if Assigned(Value) then
                  Module.ExportsTable.AddOrSetValue(ExportPair.Key, Value);
              end;
            end;
          end;

          Result := Module;
        finally
          ProgramNode.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Lexer.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TGocciaInterpreter.CheckForModuleReload(const AModule: TGocciaModule);
var
  CurrentModified: TDateTime;
begin
  CurrentModified := FileDateToDateTime(FileAge(AModule.Path));
  if CurrentModified > AModule.LastModified then
  begin
    // Reload module
    AModule.ExportsTable.Clear;
    AModule.LastModified := CurrentModified;
    LoadModule(AModule.Path);
  end;
end;

procedure TGocciaInterpreter.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.
