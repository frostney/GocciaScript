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
  Goccia.SourcePipeline,
  Goccia.Values.Primitives;

type
  { Evaluate the body of a module-form program in AContext.
    Returns the last expression's completion value (or undefined for
    declaration-only bodies).  Module imports discard the return; the
    entry file using module source uses it so the test runner can
    read the runTests(...) results object. }
  TGocciaModuleBodyEvaluator = function(const AProgram: TGocciaProgram;
    const AContext: TGocciaEvaluationContext): TGocciaValue of object;
  TGocciaRuntimeModuleLoader = function(const AResolvedPath: string;
    out AModule: TGocciaModule): Boolean of object;

  TGocciaModuleLoader = class
  private
    FPreprocessors: TGocciaPreprocessors;
    FCompatibility: TGocciaCompatibilityFlags;
    FStrictTypesEnabled: Boolean;
    FContentProvider: TGocciaModuleContentProvider;
    FEvaluateModuleBody: TGocciaModuleBodyEvaluator;
    FEntryFileName: string;
    FGlobalModules: TOrderedStringMap<TGocciaModule>;
    FGlobalScope: TGocciaGlobalScope;
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
    function GetASIEnabled: Boolean;
    function GetJSXEnabled: Boolean;
    function GetVarEnabled: Boolean;
    function GetFunctionEnabled: Boolean;
    function GetTraditionalForLoopsEnabled: Boolean;
    function GetWhileLoopsEnabled: Boolean;
    function GetLooseEqualityEnabled: Boolean;
    function GetNonStrictModeEnabled: Boolean;
    procedure SetASIEnabled(const AValue: Boolean);
    procedure SetJSXEnabled(const AValue: Boolean);
    procedure SetVarEnabled(const AValue: Boolean);
    procedure SetFunctionEnabled(const AValue: Boolean);
    procedure SetTraditionalForLoopsEnabled(const AValue: Boolean);
    procedure SetWhileLoopsEnabled(const AValue: Boolean);
    procedure SetLooseEqualityEnabled(const AValue: Boolean);
    procedure SetNonStrictModeEnabled(const AValue: Boolean);
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
    procedure SetContentProvider(
      const AContentProvider: TGocciaModuleContentProvider;
      const AOwnsContentProvider: Boolean);

    property ContentProvider: TGocciaModuleContentProvider
      read FContentProvider;
    property EvaluateModuleBody: TGocciaModuleBodyEvaluator
      read FEvaluateModuleBody write FEvaluateModuleBody;
    property GlobalModules: TOrderedStringMap<TGocciaModule>
      read FGlobalModules;
    property Preprocessors: TGocciaPreprocessors
      read FPreprocessors write FPreprocessors;
    property Compatibility: TGocciaCompatibilityFlags
      read FCompatibility write FCompatibility;
    property ASIEnabled: Boolean read GetASIEnabled write SetASIEnabled;
    property JSXEnabled: Boolean read GetJSXEnabled write SetJSXEnabled;
    property VarEnabled: Boolean read GetVarEnabled write SetVarEnabled;
    property FunctionEnabled: Boolean read GetFunctionEnabled write SetFunctionEnabled;
    property TraditionalForLoopsEnabled: Boolean
      read GetTraditionalForLoopsEnabled write SetTraditionalForLoopsEnabled;
    property WhileLoopsEnabled: Boolean
      read GetWhileLoopsEnabled write SetWhileLoopsEnabled;
    property LooseEqualityEnabled: Boolean
      read GetLooseEqualityEnabled write SetLooseEqualityEnabled;
    property NonStrictModeEnabled: Boolean
      read GetNonStrictModeEnabled write SetNonStrictModeEnabled;
    property StrictTypesEnabled: Boolean read FStrictTypesEnabled
      write FStrictTypesEnabled;
    property Resolver: TGocciaModuleResolver read FResolver;
    property RuntimeModuleLoader: TGocciaRuntimeModuleLoader
      read FRuntimeModuleLoader write FRuntimeModuleLoader;
  end;

implementation

uses
  Goccia.AST.Expressions,
  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.Keywords.Reserved,
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
    FContentProvider := TGocciaUnavailableModuleContentProvider.Create;
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

function TGocciaModuleLoader.GetASIEnabled: Boolean;
begin
  Result := cfASI in FCompatibility;
end;

function TGocciaModuleLoader.GetJSXEnabled: Boolean;
begin
  Result := ppJSX in FPreprocessors;
end;

function TGocciaModuleLoader.GetVarEnabled: Boolean;
begin
  Result := cfVar in FCompatibility;
end;

function TGocciaModuleLoader.GetFunctionEnabled: Boolean;
begin
  Result := cfFunction in FCompatibility;
end;

function TGocciaModuleLoader.GetTraditionalForLoopsEnabled: Boolean;
begin
  Result := cfTraditionalFor in FCompatibility;
end;

function TGocciaModuleLoader.GetWhileLoopsEnabled: Boolean;
begin
  Result := cfWhileLoops in FCompatibility;
end;

function TGocciaModuleLoader.GetLooseEqualityEnabled: Boolean;
begin
  Result := cfLooseEquality in FCompatibility;
end;

function TGocciaModuleLoader.GetNonStrictModeEnabled: Boolean;
begin
  Result := cfNonStrictMode in FCompatibility;
end;

procedure TGocciaModuleLoader.SetASIEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfASI)
  else
    Exclude(FCompatibility, cfASI);
end;

procedure TGocciaModuleLoader.SetJSXEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FPreprocessors, ppJSX)
  else
    Exclude(FPreprocessors, ppJSX);
end;

procedure TGocciaModuleLoader.SetVarEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfVar)
  else
    Exclude(FCompatibility, cfVar);
end;

procedure TGocciaModuleLoader.SetFunctionEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfFunction)
  else
    Exclude(FCompatibility, cfFunction);
end;

procedure TGocciaModuleLoader.SetTraditionalForLoopsEnabled(
  const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfTraditionalFor)
  else
    Exclude(FCompatibility, cfTraditionalFor);
end;

procedure TGocciaModuleLoader.SetWhileLoopsEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfWhileLoops)
  else
    Exclude(FCompatibility, cfWhileLoops);
end;

procedure TGocciaModuleLoader.SetLooseEqualityEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfLooseEquality)
  else
    Exclude(FCompatibility, cfLooseEquality);
end;

procedure TGocciaModuleLoader.SetNonStrictModeEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfNonStrictMode)
  else
    Exclude(FCompatibility, cfNonStrictMode);
end;

procedure TGocciaModuleLoader.SetContentProvider(
  const AContentProvider: TGocciaModuleContentProvider;
  const AOwnsContentProvider: Boolean);
begin
  if not Assigned(AContentProvider) then
    raise Exception.Create('Module content provider cannot be nil.');

  if AContentProvider = FContentProvider then
  begin
    FOwnsContentProvider := AOwnsContentProvider;
    Exit;
  end;

  if FOwnsContentProvider then
    FContentProvider.Free;

  FContentProvider := AContentProvider;
  FOwnsContentProvider := AOwnsContentProvider;
end;

procedure TGocciaModuleLoader.BindRuntime(const AGlobalScope: TGocciaGlobalScope;
  const AOnError: TGocciaThrowErrorCallback);
begin
  if Assigned(FGlobalScope) and (FGlobalScope <> AGlobalScope) then
    raise Exception.Create(
      'TGocciaModuleLoader instances are single-runtime; create a new loader per engine/executor.');
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
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  ExportFuncDecl: TGocciaExportFunctionDeclaration;
  ExportPair: TStringStringMap.TKeyValuePair;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  I: Integer;
  Module: TGocciaModule;
  ModuleParseResult: TGocciaSourcePipelineModuleResult;
  ModuleWarning: TGocciaSourcePipelineWarning;
  ModuleScope: TGocciaScope;
  PipelineOptions: TGocciaSourcePipelineOptions;
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  ProgramNode: TGocciaProgram;
  ReExportDecl: TGocciaReExportDeclaration;
  ResolvedPath: string;
  SourceModule: TGocciaModule;
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

  Module := nil;
  if Assigned(FRuntimeModuleLoader) and
     FRuntimeModuleLoader(ResolvedPath, Module) then
  begin
    if Assigned(Module) then
    begin
      try
        FModules.Add(ResolvedPath, Module);
      except
        Module.Free;
        raise;
      end;
      Result := Module;
      Exit;
    end;
  end;

  Content := FContentProvider.LoadContent(ResolvedPath);
  try
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.SourceType := stModule;
    ModuleParseResult := TGocciaSourcePipeline.ParseModuleSource(Content.Text,
      ResolvedPath, PipelineOptions);
    try
      for I := 0 to ModuleParseResult.WarningCount - 1 do
      begin
        ModuleWarning := ModuleParseResult.Warnings[I];
        WriteLn(Format('Warning: %s', [ModuleWarning.Message]));
        if ModuleWarning.Suggestion <> '' then
          WriteLn(Format('  Suggestion: %s', [ModuleWarning.Suggestion]));
        WriteLn(Format('  --> %s:%d:%d', [ResolvedPath, ModuleWarning.Line,
          ModuleWarning.Column]));
      end;

      ProgramNode := ModuleParseResult.TakeProgramNode;
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
          ModuleScope.NonStrictMode := False;
          Context.Scope := ModuleScope;
          Context.OnError := FOnError;
          Context.LoadModule := LoadModule;
          Context.CurrentFilePath := ResolvedPath;
          Context.CoverageEnabled := False;
          Context.StrictTypes := FStrictTypesEnabled;
          Context.NonStrictMode := False;
          Context.DisposalTracker := nil;

          ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(
            PipelineOptions);
          try
            FEvaluateModuleBody(ProgramNode, Context);
          finally
            ActiveOptionsScope.Free;
          end;

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
            else if Stmt is TGocciaExportDefaultDeclaration then
            begin
              ExportDefaultDecl := TGocciaExportDefaultDeclaration(Stmt);
              Value := ModuleScope.GetValue(ExportDefaultDecl.LocalName);
              if Assigned(Value) then
                Module.ExportsTable.AddOrSetValue(KEYWORD_DEFAULT, Value);
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
            else if Stmt is TGocciaExportFunctionDeclaration then
            begin
              ExportFuncDecl := TGocciaExportFunctionDeclaration(Stmt);
              Value := ModuleScope.GetValue(ExportFuncDecl.Declaration.Name);
              if Assigned(Value) then
                Module.ExportsTable.AddOrSetValue(
                  ExportFuncDecl.Declaration.Name, Value);
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
      ModuleParseResult.Free;
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

    if Assigned(TGarbageCollector.Instance) and Assigned(ParsedValue) then
      TGarbageCollector.Instance.AddTempRoot(ParsedValue);
    try
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
      if Assigned(TGarbageCollector.Instance) and Assigned(ParsedValue) then
        TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
    end;
  finally
    Content.Free;
  end;
end;

end.
