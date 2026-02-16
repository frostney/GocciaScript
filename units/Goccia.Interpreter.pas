unit Goccia.Interpreter;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Modules,
  Goccia.Values.Primitives, Goccia.Values.ObjectValue,
  Goccia.Values.ArrayValue, Goccia.Values.FunctionValue, Goccia.Values.ClassValue,
  Goccia.Values.NativeFunction, Goccia.Token, Generics.Collections,
  Classes, SysUtils, Math, Goccia.Error, Goccia.Values.Error, Goccia.Utils, 
  Goccia.Parser, Goccia.Lexer, Goccia.Evaluator, Goccia.Scope, 
  Goccia.Interfaces, Goccia.Logger;

type
  TGocciaInterpreter = class
  private
    FGlobalScope: TGocciaScope;
    FModules: TDictionary<string, TGocciaModule>;
    FFileName: string;
    FSourceLines: TStringList;

    // Helper methods
    procedure ThrowError(const Message: string; Line, Column: Integer);
    function CreateEvaluationContext: TGocciaEvaluationContext;
  public
    // Filename and SourceLines are required for error messages with source context.
    // A future improvement could encapsulate these in a TGocciaSourceInfo record
    // shared by the lexer, parser, and interpreter.
    constructor Create(const AFileName: string; ASourceLines: TStringList);
    destructor Destroy; override;
    function Execute(AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const APath: string): TGocciaModule;
    procedure CheckForModuleReload(Module: TGocciaModule);

    property GlobalScope: TGocciaScope read FGlobalScope;
  end;


implementation

uses Goccia.GarbageCollector;

{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  ASourceLines: TStringList);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobalScope := TGocciaScope.Create(nil, skGlobal, 'GlobalScope');
  FGlobalScope.ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  FModules := TDictionary<string, TGocciaModule>.Create;
end;

destructor TGocciaInterpreter.Destroy;
begin
  // FGlobalScope is GC-managed when GC is active; only free manually otherwise
  if not Assigned(TGocciaGC.Instance) then
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

function TGocciaInterpreter.Execute(AProgram: TGocciaProgram): TGocciaValue;
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

procedure TGocciaInterpreter.CheckForModuleReload(Module: TGocciaModule);
var
  CurrentModified: TDateTime;
begin
  CurrentModified := FileDateToDateTime(FileAge(Module.Path));
  if CurrentModified > Module.LastModified then
  begin
    // Reload module
    Module.ExportsTable.Clear;
    Module.LastModified := CurrentModified;
    LoadModule(Module.Path);
  end;
end;

procedure TGocciaInterpreter.ThrowError(const Message: string; Line, Column: Integer);
begin
  raise TGocciaRuntimeError.Create(Message, Line, Column, FFileName, FSourceLines);
end;

end.
