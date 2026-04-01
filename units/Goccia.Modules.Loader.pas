unit Goccia.Modules.Loader;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Error.ThrowErrorCallback,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Resolver,
  Goccia.Scope;

type
  TGocciaModuleLoader = class
  private
    FContentProvider: TGocciaModuleContentProvider;
    FEntryFileName: string;
    FGlobalModules: TOrderedStringMap<TGocciaModule>;
    FGlobalScope: TGocciaGlobalScope;
    FJSXEnabled: Boolean;
    FLoadingModules: TOrderedStringMap<Boolean>;
    FModules: TOrderedStringMap<TGocciaModule>;
    FOnError: TGocciaThrowErrorCallback;
    FOwnsContentProvider: Boolean;
    FOwnsResolver: Boolean;
    FResolver: TGocciaModuleResolver;

    function LoadJsonModule(const AResolvedPath: string): TGocciaModule;
  public
    constructor Create(const AEntryFileName: string;
      const AResolver: TGocciaModuleResolver = nil;
      const AContentProvider: TGocciaModuleContentProvider = nil);
    destructor Destroy; override;

    procedure BindRuntime(const AGlobalScope: TGocciaGlobalScope;
      const AOnError: TGocciaThrowErrorCallback);
    procedure CheckForModuleReload(const AModule: TGocciaModule);
    function LoadModule(const AModulePath,
      AImportingFilePath: string): TGocciaModule;

    property ContentProvider: TGocciaModuleContentProvider
      read FContentProvider;
    property GlobalModules: TOrderedStringMap<TGocciaModule>
      read FGlobalModules;
    property JSXEnabled: Boolean read FJSXEnabled write FJSXEnabled;
    property Resolver: TGocciaModuleResolver read FResolver;
  end;

implementation

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.FileExtensions,
  Goccia.JSON,
  Goccia.JSX.SourceMap,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

constructor TGocciaModuleLoader.Create(const AEntryFileName: string;
  const AResolver: TGocciaModuleResolver;
  const AContentProvider: TGocciaModuleContentProvider);
begin
  inherited Create;
  FEntryFileName := AEntryFileName;
  FModules := TOrderedStringMap<TGocciaModule>.Create;
  FLoadingModules := TOrderedStringMap<Boolean>.Create;
  FGlobalModules := TOrderedStringMap<TGocciaModule>.Create;

  if Assigned(AResolver) then
  begin
    FResolver := AResolver;
    FOwnsResolver := False;
  end
  else
  begin
    FResolver := TGocciaModuleResolver.Create(
      ExtractFilePath(ExpandFileName(AEntryFileName)));
    FOwnsResolver := True;
  end;

  if Assigned(AContentProvider) then
  begin
    FContentProvider := AContentProvider;
    FOwnsContentProvider := False;
  end
  else
  begin
    FContentProvider := TGocciaFileSystemModuleContentProvider.Create;
    FOwnsContentProvider := True;
  end;
end;

destructor TGocciaModuleLoader.Destroy;
begin
  FModules.Free;
  FLoadingModules.Free;
  FGlobalModules.Free;
  if FOwnsResolver then
    FResolver.Free;
  if FOwnsContentProvider then
    FContentProvider.Free;
  inherited;
end;

procedure TGocciaModuleLoader.BindRuntime(const AGlobalScope: TGocciaGlobalScope;
  const AOnError: TGocciaThrowErrorCallback);
begin
  FGlobalScope := AGlobalScope;
  FOnError := AOnError;
end;

function TGocciaModuleLoader.LoadModule(const AModulePath,
  AImportingFilePath: string): TGocciaModule;
var
  Content: TGocciaModuleContent;
  Context: TGocciaEvaluationContext;
  ExportDecl: TGocciaExportDeclaration;
  ExportPair: TStringStringMap.TKeyValuePair;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  I: Integer;
  JSXResult: TGocciaJSXTransformResult;
  JSXSourceMap: TGocciaSourceMap;
  Lexer: TGocciaLexer;
  Module: TGocciaModule;
  ModuleScope: TGocciaScope;
  OrigLine: Integer;
  OrigCol: Integer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  ReExportDecl: TGocciaReExportDeclaration;
  ResolvedPath: string;
  SourceModule: TGocciaModule;
  SourceText: string;
  Stmt: TGocciaStatement;
  Value: TGocciaValue;
  VarInfo: TGocciaVariableInfo;
begin
  if not Assigned(FGlobalScope) then
    raise Exception.Create('Module loader runtime is not bound.');

  if FGlobalModules.TryGetValue(AModulePath, Result) then
    Exit;

  try
    if Assigned(FResolver) then
      ResolvedPath := FResolver.Resolve(AModulePath, AImportingFilePath)
    else
      raise EGocciaModuleNotFound.CreateFmt(
        'No module resolver configured and cannot resolve "%s"', [AModulePath]);
  except
    on E: TGocciaRuntimeError do
      raise;
    on E: Exception do
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

  Content := FContentProvider.LoadContent(ResolvedPath);
  try
    SourceText := Content.Text;
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
          Parser := TGocciaParser.Create(Lexer.ScanTokens, ResolvedPath,
            Lexer.SourceLines);
          try
            ProgramNode := Parser.Parse;
            try
              Module := TGocciaModule.Create(ResolvedPath);
              Module.LastModified := Content.LastModified;
              FModules.Add(ResolvedPath, Module);
              FLoadingModules.Add(ResolvedPath, True);
              try
                ModuleScope := FGlobalScope.CreateChild(skModule,
                  'Module:' + ResolvedPath);
                Context.Scope := ModuleScope;
                Context.OnError := FOnError;
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
                  else if Stmt is TGocciaExportEnumDeclaration then
                  begin
                    Value := ModuleScope.GetValue(
                      TGocciaExportEnumDeclaration(Stmt).Declaration.Name);
                    if Assigned(Value) then
                      Module.ExportsTable.AddOrSetValue(
                        TGocciaExportEnumDeclaration(Stmt).Declaration.Name,
                        Value);
                  end
                  else if Stmt is TGocciaReExportDeclaration then
                  begin
                    ReExportDecl := TGocciaReExportDeclaration(Stmt);
                    SourceModule := LoadModule(ReExportDecl.ModulePath,
                      ResolvedPath);
                    for ExportPair in ReExportDecl.ExportsTable do
                    begin
                      if SourceModule.ExportsTable.TryGetValue(ExportPair.Value,
                        Value) then
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
            E.TranslatePosition(OrigLine, OrigCol, Content.SourceLines);
          raise;
        end;
      end;
    finally
      JSXSourceMap.Free;
    end;
  finally
    Content.Free;
  end;
end;

procedure TGocciaModuleLoader.CheckForModuleReload(const AModule: TGocciaModule);
var
  CurrentModified: TDateTime;
begin
  if not FContentProvider.TryGetLastModified(AModule.Path, CurrentModified) then
    Exit;

  if CurrentModified > AModule.LastModified then
  begin
    AModule.ExportsTable.Clear;
    AModule.LastModified := CurrentModified;
    LoadModule(AModule.Path, AModule.Path);
  end;
end;

function TGocciaModuleLoader.LoadJsonModule(
  const AResolvedPath: string): TGocciaModule;
var
  Content: TGocciaModuleContent;
  Key: string;
  Module: TGocciaModule;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  Parser: TGocciaJSONParser;
begin
  Content := FContentProvider.LoadContent(AResolvedPath);
  try
    Parser := TGocciaJSONParser.Create;
    try
      try
        ParsedValue := Parser.Parse(Content.Text);
      except
        on E: EGocciaJSONParseError do
          raise TGocciaRuntimeError.Create(
            Format('Failed to parse JSON module "%s": %s',
              [AResolvedPath, E.Message]),
            0, 0, AResolvedPath, nil);
      end;

      Module := TGocciaModule.Create(AResolvedPath);
      Module.LastModified := Content.LastModified;

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
    Content.Free;
  end;
end;

end.
