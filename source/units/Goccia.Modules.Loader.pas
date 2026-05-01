unit Goccia.Modules.Loader;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Evaluator.Context,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Resolver,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  { Evaluate the body of a module-form program in AContext.
    Returns the last expression's completion value (or undefined for
    declaration-only bodies).  Module imports discard the return; the
    --source-type=module entry path uses it so the test runner can
    read the runTests(...) results object. }
  TGocciaModuleBodyEvaluator = function(const AProgram: TGocciaProgram;
    const AContext: TGocciaEvaluationContext): TGocciaValue of object;
  TGocciaRuntimeModuleLoader = function(const AResolvedPath: string;
    out AModule: TGocciaModule): Boolean of object;

  TGocciaModuleLoader = class
  private
    FASIEnabled: Boolean;
    FVarEnabled: Boolean;
    FFunctionEnabled: Boolean;
    FStrictTypesEnabled: Boolean;
    FContentProvider: TGocciaModuleContentProvider;
    FEvaluateModuleBody: TGocciaModuleBodyEvaluator;
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
    FRuntimeModuleLoader: TGocciaRuntimeModuleLoader;

    procedure CopyModuleContents(const ASourceModule,
      ATargetModule: TGocciaModule);
    function LoadJSONModule(const AResolvedPath: string): TGocciaModule;
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
    property EvaluateModuleBody: TGocciaModuleBodyEvaluator
      read FEvaluateModuleBody write FEvaluateModuleBody;
    property GlobalModules: TOrderedStringMap<TGocciaModule>
      read FGlobalModules;
    property ASIEnabled: Boolean read FASIEnabled write FASIEnabled;
    property JSXEnabled: Boolean read FJSXEnabled write FJSXEnabled;
    property VarEnabled: Boolean read FVarEnabled write FVarEnabled;
    property FunctionEnabled: Boolean read FFunctionEnabled write FFunctionEnabled;
    property StrictTypesEnabled: Boolean read FStrictTypesEnabled
      write FStrictTypesEnabled;
    property Resolver: TGocciaModuleResolver read FResolver;
    property RuntimeModuleLoader: TGocciaRuntimeModuleLoader
      read FRuntimeModuleLoader write FRuntimeModuleLoader;
  end;

implementation

uses
  TextSemantics,

  Goccia.AST.Expressions,
  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.JSON,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.SourceMap,
  Goccia.Values.ObjectValue;

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
  if Assigned(FGlobalScope) and (FGlobalScope <> AGlobalScope) then
    raise Exception.Create(
      'TGocciaModuleLoader instances are single-runtime; create a new loader per engine/backend.');
  FGlobalScope := AGlobalScope;
  FOnError := AOnError;
end;

procedure TGocciaModuleLoader.CopyModuleContents(const ASourceModule,
  ATargetModule: TGocciaModule);
var
  ExportPair: TGocciaValueMap.TKeyValuePair;
begin
  ATargetModule.InvalidateNamespaceObject;
  ATargetModule.ExportsTable.Clear;
  for ExportPair in ASourceModule.ExportsTable do
    ATargetModule.ExportsTable.AddOrSetValue(ExportPair.Key, ExportPair.Value);
  ATargetModule.LastModified := ASourceModule.LastModified;
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
  SourceText: UTF8String;
  Stmt: TGocciaStatement;
  Value: TGocciaValue;
  VarInfo: TGocciaVariableInfo;
  LoadSucceeded: Boolean;
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
    Result := LoadJSONModule(ResolvedPath);
    Exit;
  end;

  if Assigned(FRuntimeModuleLoader) and
     FRuntimeModuleLoader(ResolvedPath, Module) then
  begin
    FModules.Add(ResolvedPath, Module);
    Result := Module;
    Exit;
  end;

  Content := FContentProvider.LoadContent(ResolvedPath);
  try
    SourceText := NormalizeUTF8NewlinesToLF(Content.Text);
    JSXSourceMap := nil;
    if FJSXEnabled then
    begin
      JSXResult := TGocciaJSXTransformer.Transform(string(SourceText));
      SourceText := UTF8String(JSXResult.Source);
      JSXSourceMap := JSXResult.SourceMap;
      if Assigned(JSXSourceMap) then
        WarnIfJSXExtensionMismatch(ResolvedPath);
    end;

    try
      try
        Lexer := TGocciaLexer.Create(string(SourceText), ResolvedPath);
        try
          Parser := TGocciaParser.Create(Lexer.ScanTokens, ResolvedPath,
            Lexer.SourceLines);
          Parser.AutomaticSemicolonInsertion := FASIEnabled;
          Parser.VarDeclarationsEnabled := FVarEnabled;
          Parser.FunctionDeclarationsEnabled := FFunctionEnabled;
          try
            ProgramNode := Parser.Parse;
            try
              Module := TGocciaModule.Create(ResolvedPath);
              Module.LastModified := Content.LastModified;
              FModules.Add(ResolvedPath, Module);
              FLoadingModules.Add(ResolvedPath, True);
              LoadSucceeded := False;
              try
                ModuleScope := FGlobalScope.CreateChild(skModule,
                  'Module:' + ResolvedPath);
                // ES2026 §16.2.1.6.4 InitializeEnvironment: a Module
                // Environment Record's [[ThisValue]] is undefined.
                ModuleScope.ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
                ModuleScope.StrictTypes := FStrictTypesEnabled;
                Context.Scope := ModuleScope;
                Context.OnError := FOnError;
                Context.LoadModule := LoadModule;
                Context.CurrentFilePath := ResolvedPath;
                Context.CoverageEnabled := False;
                Context.StrictTypes := FStrictTypesEnabled;
                Context.DisposalTracker := nil;

                FEvaluateModuleBody(ProgramNode, Context);

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
                LoadSucceeded := True;
              finally
                FLoadingModules.Remove(ResolvedPath);
                if not LoadSucceeded then
                begin
                  FModules.Remove(ResolvedPath);
                  Module.Free;
                end;
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
  ReloadedModule: TGocciaModule;
begin
  if not FContentProvider.TryGetLastModified(AModule.Path, CurrentModified) then
    Exit;

  if CurrentModified > AModule.LastModified then
  begin
    FModules.Remove(AModule.Path);
    try
      ReloadedModule := LoadModule(AModule.Path, AModule.Path);
    except
      FModules.AddOrSetValue(AModule.Path, AModule);
      raise;
    end;

    if ReloadedModule <> AModule then
    begin
      try
        CopyModuleContents(ReloadedModule, AModule);
      finally
        FModules.Remove(AModule.Path);
        FModules.AddOrSetValue(AModule.Path, AModule);
        ReloadedModule.Free;
      end;
    end;
  end;
end;

function TGocciaModuleLoader.LoadJSONModule(
  const AResolvedPath: string): TGocciaModule;
var
  Content: TGocciaModuleContent;
  Key: string;
  Module: TGocciaModule;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  JSONParser: TGocciaJSONParser;
  LoadSucceeded: Boolean;
begin
  Content := FContentProvider.LoadContent(AResolvedPath);
  try
    JSONParser := TGocciaJSONParser.Create;
    try
      try
        ParsedValue := JSONParser.Parse(Content.Text);
      except
        on E: EGocciaJSONParseError do
          raise TGocciaRuntimeError.Create(
            Format('Failed to parse JSON module "%s": %s',
              [AResolvedPath, E.Message]),
            0, 0, AResolvedPath, nil);
      end;
    finally
      JSONParser.Free;
    end;

    Module := TGocciaModule.Create(AResolvedPath);
    Module.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      if ParsedValue is TGocciaObjectValue then
      begin
        Obj := TGocciaObjectValue(ParsedValue);
        for Key in Obj.GetOwnPropertyKeys do
          Module.ExportsTable.AddOrSetValue(Key, Obj.GetProperty(Key));
      end;

      FModules.Add(AResolvedPath, Module);
      Result := Module;
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
        Module.Free;
    end;
  finally
    Content.Free;
  end;
end;

end.
