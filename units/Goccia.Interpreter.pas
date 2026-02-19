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
  Goccia.FileExtensions,
  Goccia.Interfaces,
  Goccia.Lexer,
  Goccia.Logger,
  Goccia.Modules,
  Goccia.Modules.Resolver,
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
    FLoadingModules: TDictionary<string, Boolean>;
    FGlobalModules: TDictionary<string, TGocciaModule>;
    FFileName: string;
    FSourceLines: TStringList;
    FJSXEnabled: Boolean;
    FResolver: TGocciaModuleResolver;

    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function CreateEvaluationContext: TGocciaEvaluationContext;
    function LoadJsonModule(const AResolvedPath: string): TGocciaModule;
  public
    constructor Create(const AFileName: string; const ASourceLines: TStringList);
    destructor Destroy; override;
    function Execute(const AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const AModulePath, AImportingFilePath: string): TGocciaModule;
    procedure CheckForModuleReload(const AModule: TGocciaModule);

    property GlobalScope: TGocciaGlobalScope read FGlobalScope;
    property JSXEnabled: Boolean read FJSXEnabled write FJSXEnabled;
    property Resolver: TGocciaModuleResolver read FResolver write FResolver;
    property GlobalModules: TDictionary<string, TGocciaModule> read FGlobalModules;
  end;


implementation

uses
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.JSX.SourceMap,
  Goccia.JSX.Transformer;

{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  const ASourceLines: TStringList);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobalScope := TGocciaGlobalScope.Create;
  FModules := TDictionary<string, TGocciaModule>.Create;
  FLoadingModules := TDictionary<string, Boolean>.Create;
  FGlobalModules := TDictionary<string, TGocciaModule>.Create;
end;

destructor TGocciaInterpreter.Destroy;
begin
  if not Assigned(TGocciaGarbageCollector.Instance) then
    FGlobalScope.Free;
  FModules.Free;
  FLoadingModules.Free;
  FGlobalModules.Free;

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
  Context: TGocciaEvaluationContext;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  Context := CreateEvaluationContext;

  for I := 0 to AProgram.Body.Count - 1 do
    Result := EvaluateStatement(AProgram.Body[I], Context);
end;

function TGocciaInterpreter.LoadModule(const AModulePath, AImportingFilePath: string): TGocciaModule;
var
  ResolvedPath: string;
  Source: TStringList;
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  JSXSourceMap: TGocciaSourceMap;
  OrigLine, OrigCol: Integer;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Module: TGocciaModule;
  ModuleScope: TGocciaScope;
  I: Integer;
  Stmt: TGocciaStatement;
  ExportDecl: TGocciaExportDeclaration;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  ReExportDecl: TGocciaReExportDeclaration;
  ExportPair: TPair<string, string>;
  Value: TGocciaValue;
  Context: TGocciaEvaluationContext;
  SourceModule: TGocciaModule;
  VarInfo: TGocciaVariableInfo;
begin
  if FGlobalModules.TryGetValue(AModulePath, Result) then
    Exit;

  try
    if Assigned(FResolver) then
      ResolvedPath := FResolver.Resolve(AModulePath, AImportingFilePath)
    else
      raise EGocciaModuleNotFound.CreateFmt(
        'No module resolver configured and cannot resolve "%s"', [AModulePath]);
  except
    on E: EGocciaModuleNotFound do
      raise TGocciaRuntimeError.Create(E.Message, 0, 0, AImportingFilePath, nil);
  end;

  if FModules.TryGetValue(ResolvedPath, Result) then
  begin
    if not FLoadingModules.ContainsKey(ResolvedPath) then
      CheckForModuleReload(Result);
    Exit;
  end;

  if LowerCase(ExtractFileExt(ResolvedPath)) = EXT_JSON then
  begin
    Result := LoadJsonModule(ResolvedPath);
    Exit;
  end;

  Source := TStringList.Create;
  try
    Source.LoadFromFile(ResolvedPath);

    SourceText := Source.Text;
    JSXSourceMap := nil;
    if FJSXEnabled then
    begin
      JSXResult := TGocciaJSXTransformer.Transform(SourceText);
      SourceText := JSXResult.Source;
      JSXSourceMap := JSXResult.SourceMap;
      if Assigned(JSXSourceMap) then
        WarnIfJSXExtensionMismatch(ResolvedPath);
    end;

    try
      try
        Lexer := TGocciaLexer.Create(SourceText, ResolvedPath);
        try
          Parser := TGocciaParser.Create(Lexer.ScanTokens, ResolvedPath, Lexer.SourceLines);
          try
            ProgramNode := Parser.Parse;
            try
              Module := TGocciaModule.Create(ResolvedPath);
              Module.LastModified := FileDateToDateTime(FileAge(ResolvedPath));
              FModules.Add(ResolvedPath, Module);
              FLoadingModules.Add(ResolvedPath, True);
              try
                ModuleScope := FGlobalScope.CreateChild(skModule, 'Module:' + ResolvedPath);
                Context.Scope := ModuleScope;
                Context.OnError := ThrowError;
                Context.LoadModule := LoadModule;
                Context.CurrentFilePath := ResolvedPath;

                for I := 0 to ProgramNode.Body.Count - 1 do
                  EvaluateStatement(ProgramNode.Body[I], Context);

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
                  end
                  else if Stmt is TGocciaExportVariableDeclaration then
                  begin
                    ExportVarDecl := TGocciaExportVariableDeclaration(Stmt);
                    for VarInfo in ExportVarDecl.Declaration.Variables do
                    begin
                      Value := ModuleScope.GetValue(VarInfo.Name);
                      if Assigned(Value) then
                        Module.ExportsTable.AddOrSetValue(VarInfo.Name, Value);
                    end;
                  end
                  else if Stmt is TGocciaReExportDeclaration then
                  begin
                    ReExportDecl := TGocciaReExportDeclaration(Stmt);
                    SourceModule := LoadModule(ReExportDecl.ModulePath, ResolvedPath);
                    for ExportPair in ReExportDecl.ExportsTable do
                    begin
                      if SourceModule.ExportsTable.TryGetValue(ExportPair.Value, Value) then
                        Module.ExportsTable.AddOrSetValue(ExportPair.Key, Value);
                    end;
                  end;
                end;

                Result := Module;
              finally
                FLoadingModules.Remove(ResolvedPath);
              end;
            finally
              ProgramNode.Free;
            end;
          finally
            Parser.Free;
          end;
        finally
          Lexer.Free;
        end;
      except
        on E: TGocciaError do
        begin
          if Assigned(JSXSourceMap) and
             JSXSourceMap.Translate(E.Line, E.Column, OrigLine, OrigCol) then
            E.TranslatePosition(OrigLine, OrigCol, Source);
          raise;
        end;
      end;
    finally
      JSXSourceMap.Free;
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
    AModule.ExportsTable.Clear;
    AModule.LastModified := CurrentModified;
    LoadModule(AModule.Path, AModule.Path);
  end;
end;

function TGocciaInterpreter.LoadJsonModule(const AResolvedPath: string): TGocciaModule;
var
  Source: TStringList;
  Parser: TGocciaJSONParser;
  ParsedValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Key: string;
  Module: TGocciaModule;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AResolvedPath);

    Parser := TGocciaJSONParser.Create;
    try
      try
        ParsedValue := Parser.Parse(Source.Text);
      except
        on E: EGocciaJSONParseError do
          raise TGocciaRuntimeError.Create(
            Format('Failed to parse JSON module "%s": %s', [AResolvedPath, E.Message]),
            0, 0, AResolvedPath, nil);
      end;

      Module := TGocciaModule.Create(AResolvedPath);
      Module.LastModified := FileDateToDateTime(FileAge(AResolvedPath));

      if ParsedValue is TGocciaObjectValue then
      begin
        Obj := TGocciaObjectValue(ParsedValue);
        for Key in Obj.GetOwnPropertyKeys do
          Module.ExportsTable.AddOrSetValue(Key, Obj.GetProperty(Key));
      end;

      FModules.Add(AResolvedPath, Module);
      Result := Module;
    finally
      Parser.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TGocciaInterpreter.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.
