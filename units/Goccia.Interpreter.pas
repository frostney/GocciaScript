unit Goccia.Interpreter;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Modules,
  Goccia.Values.UndefinedValue, Goccia.Values.BooleanValue, Goccia.Values.NumberValue, Goccia.Values.ObjectValue,
  Goccia.Values.StringValue, Goccia.Values.ArrayValue, Goccia.Values.FunctionValue, Goccia.Values.ClassValue,
  Goccia.Values.NullValue, Goccia.Values.NativeFunction, Goccia.Token, Generics.Collections,
  Classes, SysUtils, Math, Goccia.Error, Goccia.Values.Error, Goccia.Utils, Goccia.Parser, Goccia.Lexer, Goccia.Evaluator, Goccia.Scope, Goccia.Builtins.Console, Goccia.Builtins.GlobalObject, Goccia.Builtins.Math, Goccia.Interfaces, Goccia.Logger, Goccia.Builtins.GlobalArray, Goccia.Builtins.Globals, Goccia.Builtins.JSON, Goccia.Builtins.TestAssertions;

type
  TGocciaInterpreter = class
  private
    FBuiltinConsole: TGocciaConsole;
    FBuiltinMath: TGocciaMath;
    FBuiltinGlobalObject: TGocciaGlobalObject;
    FBuiltinGlobalArray: TGocciaGlobalArray;
    FBuiltinGlobals: TGocciaGlobals;
    FBuiltinJSON: TGocciaJSON;
    FBuiltinTestAssertions: TGocciaTestAssertions;

    FGlobalScope: TGocciaScope;
    FModules: TDictionary<string, TGocciaModule>;
    FCurrentModule: TGocciaModule;
    FFileName: string;
    FSourceLines: TStringList;

    // Built-in functions
    procedure RegisterBuiltIns;
    procedure RegisterConsole;
    procedure RegisterMath;
    procedure RegisterJSON;
    procedure RegisterTestAssertions;
    procedure RegisterPromise;
    procedure RegisterGlobalArray;
    procedure RegisterObjectMethods;
    procedure RegisterGlobals;
    procedure RegisterBuiltinConstructors;

    // Helper methods
    procedure ThrowError(const Message: string; Line, Column: Integer);
    function CreateEvaluationContext: TGocciaEvaluationContext;
  public
    constructor Create(const AFileName: string; ASourceLines: TStringList);
    destructor Destroy; override;
    function Execute(AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const APath: string): TGocciaModule;
    procedure CheckForModuleReload(Module: TGocciaModule);

    property GlobalScope: TGocciaScope read FGlobalScope;
    property BuiltinTestAssertions: TGocciaTestAssertions read FBuiltinTestAssertions;
    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinMath: TGocciaMath read FBuiltinMath;
    property BuiltinGlobalObject: TGocciaGlobalObject read FBuiltinGlobalObject;
    property BuiltinGlobalArray: TGocciaGlobalArray read FBuiltinGlobalArray;
    property BuiltinGlobals: TGocciaGlobals read FBuiltinGlobals;
    property BuiltinJSON: TGocciaJSON read FBuiltinJSON;
  end;

  TInterpreterResult = record
    Value: TGocciaValue;
    Interpreter: TGocciaInterpreter;
  end;

  function RunGocciaScriptFromStringList(const Source: TStringList; const FileName: string): TInterpreterResult;

implementation

function RunGocciaScriptFromStringList(const Source: TStringList; const FileName: string): TInterpreterResult;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  Interpreter: TGocciaInterpreter;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(Source.Text, FileName);
  try
    Tokens := Lexer.ScanTokens;
    try
    Parser := TGocciaParser.Create(Tokens, FileName, Lexer.SourceLines);
        try
        ProgramNode := Parser.Parse;
        try
            Interpreter := TGocciaInterpreter.Create(FileName, Lexer.SourceLines);

            Result.Value := Interpreter.Execute(ProgramNode);
            Result.Interpreter := Interpreter;
            // Note: Don't free Interpreter here - ownership is transferred to the caller

        finally
            ProgramNode.Free;
        end;
        finally
        Parser.Free;
        end;
    except
        on E: TGocciaError do
        begin
        WriteLn(E.GetDetailedMessage);
        ExitCode := 1;
        end;
        on E: Exception do
        begin
        WriteLn('Lexer Error: ', E.Message);
        ExitCode := 1;
        end;
    end;
    finally
      Lexer.Free;
    end;
end;


{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  ASourceLines: TStringList);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobalScope := TGocciaScope.Create(nil, skGlobal, 'GlobalScope');
  FGlobalScope.ThisValue := TGocciaUndefinedValue.Create;
  FModules := TDictionary<string, TGocciaModule>.Create;
  RegisterBuiltIns;
end;

destructor TGocciaInterpreter.Destroy;
begin
  // Free scope first to avoid accessing freed method pointers in builtin objects
  FGlobalScope.Free;
  FBuiltinConsole.Free;
  FBuiltinMath.Free;
  FBuiltinJSON.Free;
  FBuiltinTestAssertions.Free;
  FBuiltinGlobalObject.Free;
  FModules.Free;
  FSourceLines.Free;

  inherited;
end;

procedure TGocciaInterpreter.RegisterBuiltIns;
begin
  RegisterConsole;
  RegisterMath;
  RegisterJSON;
  RegisterTestAssertions;
  RegisterPromise;
  RegisterObjectMethods;
  RegisterGlobalArray;
  RegisterGlobals;
  RegisterBuiltinConstructors;
end;

procedure TGocciaInterpreter.RegisterConsole;
begin
  FBuiltinConsole := TGocciaConsole.Create('console', FGlobalScope, ThrowError);
end;

procedure TGocciaInterpreter.RegisterMath;
begin
  FBuiltinMath := TGocciaMath.Create('Math', FGlobalScope, ThrowError);
end;

procedure TGocciaInterpreter.RegisterJSON;
begin
  FBuiltinJSON := TGocciaJSON.Create('JSON', FGlobalScope, ThrowError);
end;

procedure TGocciaInterpreter.RegisterTestAssertions;
begin
  FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions', FGlobalScope, ThrowError);

  // The TestAssertions builtin should register itself and its functions globally
  // No additional registration needed here since it's handled in the constructor
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

procedure TGocciaInterpreter.RegisterGlobalArray;
begin
  FBuiltinGlobalArray := TGocciaGlobalArray.Create('Array', FGlobalScope, ThrowError);
end;

procedure TGocciaInterpreter.RegisterGlobals;
begin
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', FGlobalScope, ThrowError);
end;

procedure TGocciaInterpreter.RegisterBuiltinConstructors;
var
  ArrayConstructor, ObjectConstructor, StringConstructor, NumberConstructor, BooleanConstructor, FunctionConstructor, RangeErrorConstructor: TGocciaClassValue;
  ExistingArray, ExistingObject: TGocciaValue;
  ArrayObj, ObjectObj: TGocciaObjectValue;
  Key: string;
begin
  // Get existing built-in objects that have static methods
  ExistingArray := FGlobalScope.GetValue('Array');
  ExistingObject := FGlobalScope.GetValue('Object');

    // Create Array constructor and copy static methods
  ArrayConstructor := TGocciaClassValue.Create('Array', nil);
  if (ExistingArray is TGocciaObjectValue) then
  begin
    ArrayObj := TGocciaObjectValue(ExistingArray);
    // Copy all static methods from existing Array object
    for Key in ArrayObj.Properties.Keys do
      ArrayConstructor.SetProperty(Key, ArrayObj.Properties[Key]);
  end;
  FGlobalScope.SetValue('Array', ArrayConstructor);

  // Create Object constructor and copy static methods
  ObjectConstructor := TGocciaClassValue.Create('Object', nil);
  if (ExistingObject is TGocciaObjectValue) then
  begin
    ObjectObj := TGocciaObjectValue(ExistingObject);
    // Copy all static methods from existing Object
    for Key in ObjectObj.Properties.Keys do
      ObjectConstructor.SetProperty(Key, ObjectObj.Properties[Key]);
  end;
  FGlobalScope.SetValue('Object', ObjectConstructor);

  // Create other constructors
  StringConstructor := TGocciaClassValue.Create('String', nil);
  FGlobalScope.SetValue('String', StringConstructor);

  NumberConstructor := TGocciaClassValue.Create('Number', nil);
  FGlobalScope.SetValue('Number', NumberConstructor);

  BooleanConstructor := TGocciaClassValue.Create('Boolean', nil);
  FGlobalScope.SetValue('Boolean', BooleanConstructor);

  // Create Function constructor
  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  FGlobalScope.SetValue('Function', FunctionConstructor);

  // Create Error constructors
  RangeErrorConstructor := TGocciaClassValue.Create('RangeError', nil);
  FGlobalScope.SetValue('RangeError', RangeErrorConstructor);

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
