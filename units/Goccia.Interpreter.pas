unit Goccia.Interpreter;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Modules,
  Goccia.Values.UndefinedValue, Goccia.Values.BooleanValue, Goccia.Values.NumberValue, Goccia.Values.ObjectValue,
  Goccia.Values.StringValue, Goccia.Values.ArrayValue, Goccia.Values.FunctionValue, Goccia.Values.ClassValue,
  Goccia.Values.NullValue, Goccia.Values.NativeFunction, Goccia.Token, Generics.Collections,
  Classes, SysUtils, Math, Goccia.Error, Goccia.Values.Error, Goccia.Utils, Goccia.Parser, Goccia.Lexer, Goccia.Evaluator, Goccia.Scope, Goccia.Builtins.Console, Goccia.Builtins.GlobalObject, Goccia.Builtins.Math, Goccia.Interfaces, Goccia.Logger;

type
  TGocciaInterpreter = class
  private
    FBuiltinConsole: TGocciaConsole;
    FBuiltinMath: TGocciaMath;
    FBuiltinGlobalObject: TGocciaGlobalObject;

    FGlobalScope: TGocciaScope;
    FModules: TDictionary<string, TGocciaModule>;
    FCurrentModule: TGocciaModule;
    FFileName: string;
    FSourceLines: TStringList;

    // Built-in functions
    procedure RegisterBuiltIns;
    procedure RegisterConsole;
    procedure RegisterMath;
    procedure RegisterPromise;
    procedure RegisterObjectMethods;

    // Helper methods
    procedure ThrowError(const Message: string; Line, Column: Integer);
    function CreateEvaluationContext: TGocciaEvaluationContext;
  public
    constructor Create(const AFileName: string; ASourceLines: TStringList);
    destructor Destroy; override;
    function Execute(AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const APath: string): TGocciaModule;
    procedure CheckForModuleReload(Module: TGocciaModule);
  end;

implementation


{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  ASourceLines: TStringList);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobalScope := TGocciaScope.Create(nil, skGlobal);
  FModules := TDictionary<string, TGocciaModule>.Create;
  RegisterBuiltIns;
end;

destructor TGocciaInterpreter.Destroy;
begin
  FBuiltinConsole.Free;
  FBuiltinMath.Free;
  FBuiltinGlobalObject.Free;
  FGlobalScope.Free;
  FModules.Free;
  FSourceLines.Free;
  inherited;
end;

procedure TGocciaInterpreter.RegisterBuiltIns;
begin
  RegisterConsole;
  RegisterMath;
  RegisterPromise;
  RegisterObjectMethods;
end;

procedure TGocciaInterpreter.RegisterConsole;
begin
  FBuiltinConsole := TGocciaConsole.Create('console', FGlobalScope);
end;

procedure TGocciaInterpreter.RegisterMath;
begin
  FBuiltinMath := TGocciaMath.Create('Math', FGlobalScope, ThrowError);
end;

procedure TGocciaInterpreter.RegisterPromise;
begin
  // Promise implementation would go here
  // For now, we'll just register a placeholder
end;

procedure TGocciaInterpreter.RegisterObjectMethods;
begin
  FBuiltinGlobalObject := TGocciaGlobalObject.Create('Object', FGlobalScope, ThrowError);
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
  Result := TGocciaUndefinedValue.Create;
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

          // Execute module in its own scope
          FCurrentModule := Module;
          Context := CreateEvaluationContext;
          Execute(ProgramNode);

          // Process exports
          for I := 0 to ProgramNode.Body.Count - 1 do
          begin
            Stmt := ProgramNode.Body[I];
            if Stmt is TGocciaExportDeclaration then
            begin
              ExportDecl := TGocciaExportDeclaration(Stmt);
              for ExportPair in ExportDecl.ExportsTable do
              begin
                Value := FGlobalScope.GetValue(ExportPair.Value);
                if Assigned(Value) then
                  Module.ExportsTable.Add(ExportPair.Key, Value);
              end;
            end;
          end;

          FCurrentModule := nil;
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
