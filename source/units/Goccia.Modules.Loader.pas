unit Goccia.Modules.Loader;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  StrUtils,
  SysUtils,

  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Evaluator.Context,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Resolver,
  Goccia.Modules.Virtual,
  Goccia.Scope,
  Goccia.SourcePipeline,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue;

type
  { Evaluate the body of a module-form program in AContext.
    Returns the last expression's completion value (or undefined for
    declaration-only bodies).  Module imports discard the return; the
    entry file using module source uses it so the test runner can
    read the runTests(...) results object. }
  TGocciaModuleBodyEvaluator = function(const AProgram: TGocciaProgram;
    const AContext: TGocciaEvaluationContext;
    out AProgramConsumed: Boolean): TGocciaValue of object;
  TGocciaRuntimeModuleLoader = function(const AResolvedPath: string;
    out AModule: TGocciaModule): Boolean of object;
  TGocciaGlobalModuleProvider = function: TGocciaModule of object;

  TGocciaModuleLoader = class
  private
    FPreprocessors: TGocciaPreprocessors;
    FCompatibility: TGocciaCompatibilityFlags;
    FLabelStatementsEnabled: Boolean;
    FForInLoopsEnabled: Boolean;
    FExperimentalJSModuleSourceEnabled: Boolean;
    FWarningUnsupportedFeatures: Boolean;
    FStrictTypesEnabled: Boolean;
    FContentProvider: TGocciaModuleContentProvider;
    FEvaluateModuleBody: TGocciaModuleBodyEvaluator;
    FEntryFileName: string;
    FDeferredModuleNamespaces: TOrderedStringMap<TGocciaValue>;
    FEvaluatingModules: TOrderedStringMap<Boolean>;
    FFailedModuleErrors: TOrderedStringMap<TGocciaValue>;
    FFailedModuleErrorModifiedTimes: TOrderedStringMap<TDateTime>;
    FGlobalModules: TOrderedStringMap<TGocciaModule>;
    FGlobalModuleProviders: TOrderedStringMap<TGocciaGlobalModuleProvider>;
    FGlobalScope: TGocciaGlobalScope;
    FLoadingModules: TOrderedStringMap<Boolean>;
    FModules: TOrderedStringMap<TGocciaModule>;
    FModuleSourceValues: TOrderedStringMap<TGocciaValue>;
    FVirtualModules: TGocciaVirtualModuleRegistry;
    FWarnedVirtualCollisions: TOrderedStringMap<Boolean>;
    FOnError: TGocciaThrowErrorCallback;
    FOwnsContentProvider: Boolean;
    FOwnsResolver: Boolean;
    FResolver: TGocciaModuleResolver;
    FRuntimeModuleLoader: TGocciaRuntimeModuleLoader;

    procedure CopyModuleContents(const ASourceModule,
      ATargetModule: TGocciaModule);
    function LoadJSONModule(const AResolvedPath,
      ACacheKey: string): TGocciaModule;
    function LoadTextModule(const AResolvedPath,
      ACacheKey: string; const ADefaultOnly: Boolean): TGocciaModule;
    function LoadBytesModule(const AResolvedPath,
      ACacheKey: string): TGocciaModule;
    function ResolveModuleRequestWithAttribute(const AModulePath,
      AAttributeType, AImportingFilePath: string): string;
    function LoadResolvedContent(
      const AResolvedPath: string): TGocciaModuleContent;
    function LoadResolvedContentBytes(const AResolvedPath: string): TBytes;
    function TryGetResolvedLastModified(const AResolvedPath: string;
      out ALastModified: TDateTime): Boolean;
    function IsJavaScriptModuleResource(const AResolvedPath: string): Boolean;
    procedure RecordFailedModuleError(const ACacheKey: string;
      const AValue: TGocciaValue; const ALastModified: TDateTime);
    procedure ClearFailedModuleError(const ACacheKey: string);
    function TryGetCachedFailedModuleError(const AResolvedPath,
      ACacheKey: string; out AValue: TGocciaValue): Boolean;
    function HasGlobalModuleRequest(const AModulePath: string): Boolean;
    function TryLoadGlobalModule(const AModulePath: string;
      out AModule: TGocciaModule): Boolean;
    function DeferredGraphTouchesEvaluating(const AResolvedPath: string;
      const ASeen: TOrderedStringMap<Boolean>): Boolean;
    procedure EvaluateDeferredAsyncDependencies(const AResolvedPath,
      AImportingFilePath: string; const ASeen: TOrderedStringMap<Boolean>;
      const ARequestedModules: TGocciaModuleList = nil);
    procedure ValidateDeferredModuleLinks(const AResolvedPath,
      AImportingFilePath: string; const ASeen: TOrderedStringMap<Boolean>);
    function GetASIEnabled: Boolean;
    function GetJSXEnabled: Boolean;
    function GetVarEnabled: Boolean;
    function GetFunctionEnabled: Boolean;
    function GetTraditionalForLoopsEnabled: Boolean;
    function GetWhileLoopsEnabled: Boolean;
    function GetLooseEqualityEnabled: Boolean;
    function GetNonStrictModeEnabled: Boolean;
    function GetArgumentsObjectEnabled: Boolean;
    procedure SetASIEnabled(const AValue: Boolean);
    procedure SetJSXEnabled(const AValue: Boolean);
    procedure SetVarEnabled(const AValue: Boolean);
    procedure SetFunctionEnabled(const AValue: Boolean);
    procedure SetTraditionalForLoopsEnabled(const AValue: Boolean);
    procedure SetWhileLoopsEnabled(const AValue: Boolean);
    procedure SetLooseEqualityEnabled(const AValue: Boolean);
    procedure SetNonStrictModeEnabled(const AValue: Boolean);
    procedure SetArgumentsObjectEnabled(const AValue: Boolean);
  public
    constructor Create(const AEntryFileName: string;
      const AResolver: TGocciaModuleResolver = nil;
      const AContentProvider: TGocciaModuleContentProvider = nil);
    destructor Destroy; override;

    procedure BindRuntime(const AGlobalScope: TGocciaGlobalScope;
      const AOnError: TGocciaThrowErrorCallback);
    procedure BeginEvaluatingModulePath(const APath: string);
    procedure CheckForModuleReload(const AModule: TGocciaModule;
      const ACacheKey: string = '');
    procedure EndEvaluatingModulePath(const APath: string);
    function IsEvaluatingModulePath(const APath: string): Boolean;
    procedure ValidateStaticNamedImports(const AProgram: TGocciaProgram;
      const AModule: TGocciaModule);
    function LoadModule(const AModulePath,
      AImportingFilePath: string): TGocciaModule;
    function LoadModuleSourceValue(const AModulePath,
      AImportingFilePath: string): TGocciaValue;
    function LoadDeferredModuleNamespaceValue(const AModulePath,
      AImportingFilePath: string): TGocciaValue;
    function LoadDeferredModuleNamespaceValueForEvaluation(
      const AModulePath, AImportingFilePath: string;
      const ARequestedModules: TGocciaModuleList): TGocciaValue;
    procedure InjectModule(const AAddress, AContent: string;
      const AContentType: string = ''; const ABaseAddress: string = '';
      const AProvenance: string = '');
    procedure CopyVirtualModulesFrom(const ASource: TGocciaModuleLoader);
    function ResolveModuleAddress(const AModulePath,
      AImportingFilePath: string): string;
    function ResolveModuleURL(const AModulePath,
      AImportingFilePath: string): string;
    procedure RegisterModule(const AResolvedPath: string;
      const AModule: TGocciaModule);
    procedure RegisterGlobalModuleProvider(const AModulePath: string;
      const AProvider: TGocciaGlobalModuleProvider);
    procedure UnregisterGlobalModuleProvider(const AModulePath: string);
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
    property LabelStatementsEnabled: Boolean
      read FLabelStatementsEnabled write FLabelStatementsEnabled;
    property ForInLoopsEnabled: Boolean
      read FForInLoopsEnabled write FForInLoopsEnabled;
    property ExperimentalJSModuleSourceEnabled: Boolean
      read FExperimentalJSModuleSourceEnabled
      write FExperimentalJSModuleSourceEnabled;
    property WarningUnsupportedFeatures: Boolean
      read FWarningUnsupportedFeatures write FWarningUnsupportedFeatures;
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
    property ArgumentsObjectEnabled: Boolean
      read GetArgumentsObjectEnabled write SetArgumentsObjectEnabled;
    property StrictTypesEnabled: Boolean read FStrictTypesEnabled
      write FStrictTypesEnabled;
    property Resolver: TGocciaModuleResolver read FResolver;
    property RuntimeModuleLoader: TGocciaRuntimeModuleLoader
      read FRuntimeModuleLoader write FRuntimeModuleLoader;
    property VirtualModules: TGocciaVirtualModuleRegistry
      read FVirtualModules;
  end;

implementation

uses
  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.AST.BindingPatterns,
  Goccia.AST.Expressions,
  Goccia.AST.Statements,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.JSON,
  Goccia.Keywords.Reserved,
  Goccia.Realm,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.Error,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.TypedArrayValue,
  Goccia.VM.Exception;

const
  MODULE_SOURCE_SENTINEL = '<module source>';

type
  TGocciaModuleEvaluationWaitState = class(TGocciaObjectValue)
  private
    FRemaining: Integer;
    FResultPromise: TGocciaPromiseValue;
    FSettled: Boolean;
  public
    constructor Create(const AResultPromise: TGocciaPromiseValue;
      const ACount: Integer);
    function Fulfill(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Reject(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TGocciaDeferredModuleBody = class(TGocciaObjectValue)
  private
    FContext: TGocciaEvaluationContext;
    FLoader: TGocciaModuleLoader;
    FModule: TGocciaModule;
    FHasTopLevelAwait: Boolean;
    FPipelineOptions: TGocciaSourcePipelineOptions;
    FProgramNode: TGocciaProgram;
  public
    constructor Create(const ALoader: TGocciaModuleLoader;
      const AProgramNode: TGocciaProgram;
      const AContext: TGocciaEvaluationContext;
      const AModule: TGocciaModule;
      const AHasTopLevelAwait: Boolean;
      const APipelineOptions: TGocciaSourcePipelineOptions);
    destructor Destroy; override;
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

{ TGocciaModuleEvaluationWaitState }

function EncodeReExportModuleRequest(
  const AReExportDecl: TGocciaReExportDeclaration): string;
begin
  Result := EncodeImportSpecifierAttribute(AReExportDecl.ModulePath,
    AReExportDecl.AttributeType);
end;

constructor TGocciaModuleEvaluationWaitState.Create(
  const AResultPromise: TGocciaPromiseValue; const ACount: Integer);
begin
  inherited Create(nil);
  FResultPromise := AResultPromise;
  FRemaining := ACount;
  FSettled := False;
end;

function TGocciaModuleEvaluationWaitState.Fulfill(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FSettled then
    Exit;
  Dec(FRemaining);
  if FRemaining = 0 then
  begin
    FSettled := True;
    FResultPromise.Resolve(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;
end;

function TGocciaModuleEvaluationWaitState.Reject(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FSettled then
    Exit;
  FSettled := True;
  FResultPromise.Reject(AArgs.GetElement(0));
end;

procedure TGocciaModuleEvaluationWaitState.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FResultPromise) then
    FResultPromise.MarkReferences;
end;

{ TGocciaDeferredModuleBody }

constructor TGocciaDeferredModuleBody.Create(
  const ALoader: TGocciaModuleLoader; const AProgramNode: TGocciaProgram;
  const AContext: TGocciaEvaluationContext; const AModule: TGocciaModule;
  const AHasTopLevelAwait: Boolean;
  const APipelineOptions: TGocciaSourcePipelineOptions);
begin
  inherited Create(nil);
  FLoader := ALoader;
  FProgramNode := AProgramNode;
  FContext := AContext;
  FModule := AModule;
  FHasTopLevelAwait := AHasTopLevelAwait;
  FPipelineOptions := APipelineOptions;
end;

destructor TGocciaDeferredModuleBody.Destroy;
begin
  FProgramNode.Free;
  inherited;
end;

function TGocciaDeferredModuleBody.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  BodyResult: TGocciaValue;
  ProgramConsumed: Boolean;
begin
  ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(
    FPipelineOptions);
  try
    try
      ProgramConsumed := False;
      try
        BodyResult := FLoader.FEvaluateModuleBody(FProgramNode, FContext,
          ProgramConsumed);
      finally
        if ProgramConsumed then
          FProgramNode := nil;
      end;
      if FHasTopLevelAwait then
        Result := BodyResult
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    except
      on E: EGocciaBytecodeThrow do
      begin
        if Assigned(FModule) then
          FLoader.RecordFailedModuleError(FModule.Path, E.ThrownValue,
            FModule.LastModified);
        raise;
      end;
      on E: TGocciaThrowValue do
      begin
        if Assigned(FModule) then
          FLoader.RecordFailedModuleError(FModule.Path, E.Value,
            FModule.LastModified);
        raise;
      end;
    end;
  finally
    ActiveOptionsScope.Free;
    FreeAndNil(FProgramNode);
  end;
end;

procedure TGocciaDeferredModuleBody.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FContext.Scope) then
    FContext.Scope.MarkReferences;
end;

constructor TGocciaModuleLoader.Create(const AEntryFileName: string;
  const AResolver: TGocciaModuleResolver;
  const AContentProvider: TGocciaModuleContentProvider);
begin
  inherited Create;
  FEntryFileName := AEntryFileName;
  FDeferredModuleNamespaces := TOrderedStringMap<TGocciaValue>.Create;
  FEvaluatingModules := TOrderedStringMap<Boolean>.Create;
  FFailedModuleErrors := TOrderedStringMap<TGocciaValue>.Create;
  FFailedModuleErrorModifiedTimes := TOrderedStringMap<TDateTime>.Create;
  FModules := TOrderedStringMap<TGocciaModule>.Create;
  FModuleSourceValues := TOrderedStringMap<TGocciaValue>.Create;
  FVirtualModules := TGocciaVirtualModuleRegistry.Create;
  FWarnedVirtualCollisions := TOrderedStringMap<Boolean>.Create;
  FLoadingModules := TOrderedStringMap<Boolean>.Create;
  FGlobalModules := TOrderedStringMap<TGocciaModule>.Create;
  FGlobalModuleProviders := TOrderedStringMap<TGocciaGlobalModuleProvider>.Create;

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
var
  NamespacePair: TOrderedStringMap<TGocciaValue>.TKeyValuePair;
  SourceValuePair: TOrderedStringMap<TGocciaValue>.TKeyValuePair;
begin
  if Assigned(TGarbageCollector.Instance) then
  begin
    for NamespacePair in FDeferredModuleNamespaces do
      if Assigned(NamespacePair.Value) then
        TGarbageCollector.Instance.RemoveRootObject(NamespacePair.Value);
    for SourceValuePair in FModuleSourceValues do
      if Assigned(SourceValuePair.Value) then
        TGarbageCollector.Instance.RemoveRootObject(SourceValuePair.Value);
  end;
  FDeferredModuleNamespaces.Free;
  FEvaluatingModules.Free;
  FFailedModuleErrors.Free;
  FFailedModuleErrorModifiedTimes.Free;
  FModules.Free;
  FModuleSourceValues.Free;
  FVirtualModules.Free;
  FWarnedVirtualCollisions.Free;
  FLoadingModules.Free;
  FGlobalModuleProviders.Free;
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

function TGocciaModuleLoader.GetArgumentsObjectEnabled: Boolean;
begin
  Result := cfArgumentsObject in FCompatibility;
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

procedure TGocciaModuleLoader.SetArgumentsObjectEnabled(const AValue: Boolean);
begin
  if AValue then
    Include(FCompatibility, cfArgumentsObject)
  else
    Exclude(FCompatibility, cfArgumentsObject);
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

function TGocciaModuleLoader.ResolveModuleRequestWithAttribute(
  const AModulePath, AAttributeType, AImportingFilePath: string): string;
begin
  if (AAttributeType = '') and HasGlobalModuleRequest(AModulePath) then
    Exit(AModulePath);

  Result := ResolveModuleAddress(AModulePath, AImportingFilePath);

  if AAttributeType <> '' then
    Result := EncodeImportSpecifierAttribute(Result, AAttributeType);
end;

function TGocciaModuleLoader.ResolveModuleAddress(const AModulePath,
  AImportingFilePath: string): string;
var
  AliasCandidate, FileSystemAddress: string;

  procedure WarnOnCollision(const AVirtualAddress: string);
  begin
    if not Assigned(FResolver) or
       FWarnedVirtualCollisions.ContainsKey(AVirtualAddress) then
      Exit;
    try
      FileSystemAddress := FResolver.Resolve(AModulePath,
        AImportingFilePath);
      WriteLn(StdErr, Format(
        'Warning: virtual module "%s" shadows module "%s".',
        [AVirtualAddress, FileSystemAddress]));
      FWarnedVirtualCollisions.Add(AVirtualAddress, True);
    except
      on Exception do;
    end;
  end;

begin
  if FVirtualModules.Resolve(AModulePath, AImportingFilePath, Result) then
  begin
    WarnOnCollision(Result);
    Exit;
  end;

  if Assigned(FResolver) then
  begin
    AliasCandidate := FResolver.ApplyAlias(AModulePath, AImportingFilePath);
    if (AliasCandidate <> AModulePath) and
       FVirtualModules.Resolve(AliasCandidate, AImportingFilePath, Result) then
    begin
      WarnOnCollision(Result);
      Exit;
    end;
    Result := FResolver.Resolve(AModulePath, AImportingFilePath);
    Exit;
  end;
  raise EGocciaModuleNotFound.CreateFmt(
    'No module resolver configured and cannot resolve "%s"', [AModulePath]);
end;

function TGocciaModuleLoader.LoadResolvedContent(
  const AResolvedPath: string): TGocciaModuleContent;
begin
  if FVirtualModules.Contains(AResolvedPath) then
    Exit(FVirtualModules.LoadContent(AResolvedPath));
  Result := FContentProvider.LoadContent(AResolvedPath);
end;

function TGocciaModuleLoader.LoadResolvedContentBytes(
  const AResolvedPath: string): TBytes;
begin
  if FVirtualModules.Contains(AResolvedPath) then
    Exit(FVirtualModules.LoadContentBytes(AResolvedPath));
  Result := FContentProvider.LoadContentBytes(AResolvedPath);
end;

function TGocciaModuleLoader.TryGetResolvedLastModified(
  const AResolvedPath: string; out ALastModified: TDateTime): Boolean;
begin
  if FVirtualModules.Contains(AResolvedPath) then
  begin
    ALastModified := 0;
    Exit(False);
  end;
  Result := FContentProvider.TryGetLastModified(AResolvedPath, ALastModified);
end;

function TGocciaModuleLoader.IsJavaScriptModuleResource(
  const AResolvedPath: string): Boolean;
var
  ContentType: TGocciaVirtualModuleContentType;
begin
  if FVirtualModules.GetContentType(AResolvedPath, ContentType) then
    Exit(ContentType = vmctJavaScript);
  Result := IsScriptExtension(ExtractFileExt(AResolvedPath));
end;

procedure TGocciaModuleLoader.InjectModule(const AAddress, AContent: string;
  const AContentType: string; const ABaseAddress: string;
  const AProvenance: string);
var
  CanonicalAddress: string;
  ConflictingModule: TGocciaModule;
  ParsedContentType: TGocciaVirtualModuleContentType;
begin
  CanonicalAddress := FVirtualModules.CanonicalAddress(AAddress, ABaseAddress);
  if StartsStr('goccia:', CanonicalAddress) and
     HasGlobalModuleRequest(CanonicalAddress) then
    raise EInvalidOperation.CreateFmt(
      'Virtual module "%s" conflicts with a registered runtime module.',
      [CanonicalAddress]);
  if HasGlobalModuleRequest(CanonicalAddress) then
    raise EInvalidOperation.CreateFmt(
      'Virtual module "%s" conflicts with a registered host module.',
      [CanonicalAddress]);
  ConflictingModule := nil;
  if StartsStr('goccia:', CanonicalAddress) and
     Assigned(FRuntimeModuleLoader) and
     FRuntimeModuleLoader(CanonicalAddress, ConflictingModule) then
  begin
    ConflictingModule.Free;
    raise EInvalidOperation.CreateFmt(
      'Virtual module "%s" conflicts with a registered runtime module.',
      [CanonicalAddress]);
  end;
  if not ParseVirtualModuleContentType(AContentType, ParsedContentType) then
    raise EArgumentException.CreateFmt(
      'Unsupported virtual module content type "%s".', [AContentType]);
  FVirtualModules.AddText(AAddress, ABaseAddress, ParsedContentType,
    AContent, AProvenance);
end;

procedure TGocciaModuleLoader.CopyVirtualModulesFrom(
  const ASource: TGocciaModuleLoader);
begin
  if Assigned(ASource) then
    FVirtualModules.CopyFrom(ASource.VirtualModules);
end;

function TGocciaModuleLoader.ResolveModuleURL(const AModulePath,
  AImportingFilePath: string): string;
var
  AliasCandidate, BaseDirectory, ResolvedAddress,
    VirtualCandidate: string;
begin
  if AModulePath = AImportingFilePath then
  begin
    if FVirtualModules.Contains(AModulePath) then
      Exit(AModulePath);
    if Assigned(FContentProvider) and FContentProvider.Exists(AModulePath) then
      Exit(FilePathToUrl(ExpandFileName(AModulePath)));
  end;

  if FVirtualModules.Resolve(AModulePath, AImportingFilePath,
     VirtualCandidate) then
    Exit(VirtualCandidate);
  if FVirtualModules.Contains(AImportingFilePath) then
    Exit(VirtualCandidate);
  if Assigned(FResolver) then
  begin
    AliasCandidate := FResolver.ApplyAlias(AModulePath, AImportingFilePath);
    if (AliasCandidate <> AModulePath) and
       FVirtualModules.Resolve(AliasCandidate, AImportingFilePath,
       VirtualCandidate) then
      Exit(VirtualCandidate);
  end;

  try
    ResolvedAddress := ResolveModuleAddress(AModulePath, AImportingFilePath);
    Exit(FilePathToUrl(ResolvedAddress));
  except
    on E: Exception do;
  end;

  if (AModulePath <> '') and
     ((AModulePath[1] = '/') or (AModulePath[1] = '\') or
     ((Length(AModulePath) >= 2) and (AModulePath[2] = ':'))) then
    ResolvedAddress := ExpandFileName(AModulePath)
  else
  begin
    BaseDirectory := ExtractFilePath(ExpandFileName(AImportingFilePath));
    ResolvedAddress := ExpandFileName(
      IncludeTrailingPathDelimiter(BaseDirectory) + AModulePath);
  end;
  Result := FilePathToUrl(ResolvedAddress);
end;

procedure TGocciaModuleLoader.RecordFailedModuleError(const ACacheKey: string;
  const AValue: TGocciaValue; const ALastModified: TDateTime);
begin
  FFailedModuleErrors.AddOrSetValue(ACacheKey, AValue);
  FFailedModuleErrorModifiedTimes.AddOrSetValue(ACacheKey, ALastModified);
end;

procedure TGocciaModuleLoader.ClearFailedModuleError(const ACacheKey: string);
begin
  FFailedModuleErrors.Remove(ACacheKey);
  FFailedModuleErrorModifiedTimes.Remove(ACacheKey);
end;

function TGocciaModuleLoader.TryGetCachedFailedModuleError(
  const AResolvedPath, ACacheKey: string; out AValue: TGocciaValue): Boolean;
var
  CurrentModified: TDateTime;
  FailedModified: TDateTime;
begin
  Result := FFailedModuleErrors.TryGetValue(ACacheKey, AValue);
  if not Result then
    Exit;

  if FFailedModuleErrorModifiedTimes.TryGetValue(ACacheKey, FailedModified) and
     TryGetResolvedLastModified(AResolvedPath, CurrentModified) and
     (CurrentModified > FailedModified) then
  begin
    ClearFailedModuleError(ACacheKey);
    Result := False;
  end;
end;

function TGocciaModuleLoader.HasGlobalModuleRequest(
  const AModulePath: string): Boolean;
begin
  Result := FGlobalModules.ContainsKey(AModulePath) or
    FGlobalModuleProviders.ContainsKey(AModulePath);
end;

function TGocciaModuleLoader.TryLoadGlobalModule(const AModulePath: string;
  out AModule: TGocciaModule): Boolean;
var
  Provider: TGocciaGlobalModuleProvider;
begin
  if FGlobalModules.TryGetValue(AModulePath, AModule) then
    Exit(True);

  if not FGlobalModuleProviders.TryGetValue(AModulePath, Provider) then
  begin
    AModule := nil;
    Exit(False);
  end;

  AModule := Provider();
  if not Assigned(AModule) then
    raise TGocciaRuntimeError.Create(
      Format('Global module provider for "%s" returned nil.', [AModulePath]),
      0, 0, AModulePath, nil);

  FGlobalModules.AddOrSetValue(AModulePath, AModule);
  Result := True;
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

procedure TGocciaModuleLoader.BeginEvaluatingModulePath(const APath: string);
begin
  if APath = '' then
    Exit;
  FEvaluatingModules.AddOrSetValue(APath, True);
  FEvaluatingModules.AddOrSetValue(ExpandFileName(APath), True);
  FLoadingModules.AddOrSetValue(APath, True);
  FLoadingModules.AddOrSetValue(ExpandFileName(APath), True);
end;

procedure TGocciaModuleLoader.EndEvaluatingModulePath(const APath: string);
begin
  if APath = '' then
    Exit;
  FEvaluatingModules.Remove(APath);
  FEvaluatingModules.Remove(ExpandFileName(APath));
  FLoadingModules.Remove(APath);
  FLoadingModules.Remove(ExpandFileName(APath));
end;

function TGocciaModuleLoader.IsEvaluatingModulePath(
  const APath: string): Boolean;
begin
  Result := (APath <> '') and
    (FEvaluatingModules.ContainsKey(APath) or
    FEvaluatingModules.ContainsKey(ExpandFileName(APath)) or
    FLoadingModules.ContainsKey(APath) or
    FLoadingModules.ContainsKey(ExpandFileName(APath)));
end;

// ES2026 §16.2.1.7.3.1 InitializeEnvironment: while linking a module, a named
// import whose ResolveExport result is null or ambiguous is a SyntaxError. The
// tree-walking interpreter also enforces this lazily as it evaluates each
// import declaration (Goccia.AST.Statements), but the bytecode compiler resolves
// import bindings at their use sites, so missing names must be rejected here at
// link time to keep both engines in lockstep (ADR 0014). Source modules still
// mid-evaluation in a cycle are skipped via IsEvaluatingModulePath, matching the
// re-export validation in RegisterStaticModuleExports.
procedure TGocciaModuleLoader.ValidateStaticNamedImports(
  const AProgram: TGocciaProgram; const AModule: TGocciaModule);
var
  Stmt: TGocciaStatement;
  ImportDecl: TGocciaImportDeclaration;
  ImportPair: TStringStringMap.TKeyValuePair;
  SourceModule: TGocciaModule;
  I: Integer;
begin
  if (not Assigned(AProgram)) or (not Assigned(AModule)) then
    Exit;

  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Stmt := AProgram.Body[I];
    if not (Stmt is TGocciaImportDeclaration) then
      Continue;

    ImportDecl := TGocciaImportDeclaration(Stmt);
    // Only evaluation-phase imports bind exported names; source- and defer-phase
    // imports bind the module source or namespace object, which always resolve.
    if (ImportDecl.Phase <> icpEvaluation) or (ImportDecl.Imports.Count = 0) then
      Continue;

    SourceModule := LoadModule(EncodeImportSpecifierAttribute(
      ImportDecl.ModulePath, ImportDecl.AttributeType), AModule.Path);
    for ImportPair in ImportDecl.Imports do
      if (not SourceModule.CanResolveExport(ImportPair.Value)) and
         (not IsEvaluatingModulePath(SourceModule.Path)) then
        raise TGocciaSyntaxError.Create(
          Format('Module "%s" has no export named "%s"',
            [ImportDecl.ModulePath, ImportPair.Value]),
          ImportDecl.Line, ImportDecl.Column, AModule.Path, nil);
  end;
end;

procedure TGocciaModuleLoader.RegisterModule(const AResolvedPath: string;
  const AModule: TGocciaModule);
var
  CacheKey: string;
begin
  if (AResolvedPath = '') or not Assigned(AModule) then
    Exit;

  CacheKey := ExpandFileName(AResolvedPath);
  FModules.AddOrSetValue(CacheKey, AModule);
  if CacheKey <> AResolvedPath then
    FModules.AddOrSetValue(AResolvedPath, AModule);
end;

procedure TGocciaModuleLoader.CopyModuleContents(const ASourceModule,
  ATargetModule: TGocciaModule);
begin
  ATargetModule.CopyExportsFrom(ASourceModule);
end;

function TGocciaModuleLoader.LoadModule(const AModulePath,
  AImportingFilePath: string): TGocciaModule;
var
  Content: TGocciaModuleContent;
  Context: TGocciaEvaluationContext;
  ExportClassDecl: TGocciaExportClassDeclaration;
  ExportDecl: TGocciaExportDeclaration;
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  ExportDestructuringDecl: TGocciaExportDestructuringDeclaration;
  ExportFuncDecl: TGocciaExportFunctionDeclaration;
  ExportPair: TStringStringMap.TKeyValuePair;
  ExportName: string;
  ExportVarDecl: TGocciaExportVariableDeclaration;
  AttributeType: string;
  CacheKey: string;
  FailedValue: TGocciaValue;
  I: Integer;
  ImportingFilePath: string;
  IsDeferredEvaluation: Boolean;
  Module: TGocciaModule;
  ModuleParseResult: TGocciaSourcePipelineModuleResult;
  ModuleWarning: TGocciaSourcePipelineWarning;
  ModuleScope: TGocciaScope;
  PipelineOptions: TGocciaSourcePipelineOptions;
  PendingDependencyPromise: TGocciaPromiseValue;
  RequestedModules: TGocciaModuleList;
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  ProgramNode: TGocciaProgram;
  ProgramConsumed: Boolean;
  ReExportDecl: TGocciaReExportDeclaration;
  RequestedModulePath: string;
  ResolvedPath: string;
  Seen: TOrderedStringMap<Boolean>;
  SourceModule: TGocciaModule;
  Stmt: TGocciaStatement;
  Value: TGocciaValue;
  VirtualContentType: TGocciaVirtualModuleContentType;
  LoadSucceeded: Boolean;
  Name: string;
  Names: TStringList;
  DeferredBody: TGocciaDeferredModuleBody;
  DeferredHandler: TGocciaNativeFunctionValue;

  function TryResolveImportedLocal(const ALocalName: string;
    out AValue: TGocciaValue): Boolean;
  var
    ImportDecl: TGocciaImportDeclaration;
    ImportPair: TStringStringMap.TKeyValuePair;
    J: Integer;
  begin
    AValue := nil;
    Result := False;

    for J := 0 to ProgramNode.Body.Count - 1 do
    begin
      if not (ProgramNode.Body[J] is TGocciaImportDeclaration) then
        Continue;

      ImportDecl := TGocciaImportDeclaration(ProgramNode.Body[J]);
      if ImportDecl.NamespaceName = ALocalName then
      begin
        case ImportDecl.Phase of
          icpDefer:
            AValue := LoadDeferredModuleNamespaceValue(
              EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
              ImportDecl.AttributeType), ResolvedPath);
          icpSource:
            AValue := LoadModuleSourceValue(EncodeImportSpecifierAttribute(
              ImportDecl.ModulePath, ImportDecl.AttributeType), ResolvedPath);
        else
          AValue := LoadModule(EncodeImportSpecifierAttribute(
            ImportDecl.ModulePath, ImportDecl.AttributeType),
            ResolvedPath).GetNamespaceObject;
        end;
        Exit(Assigned(AValue));
      end;

      for ImportPair in ImportDecl.Imports do
      begin
        if ImportPair.Key <> ALocalName then
          Continue;

        if ImportDecl.Phase = icpSource then
        begin
          AValue := LoadModuleSourceValue(EncodeImportSpecifierAttribute(
            ImportDecl.ModulePath, ImportDecl.AttributeType), ResolvedPath);
          Exit(Assigned(AValue));
        end;

        SourceModule := LoadModule(EncodeImportSpecifierAttribute(
          ImportDecl.ModulePath, ImportDecl.AttributeType), ResolvedPath);
        Result := SourceModule.TryGetExportValue(ImportPair.Value, AValue);
        Exit;
      end;
    end;
  end;

  function TryAddImportedLocalExport(const ALocalName,
    AExportName: string): Boolean;
  var
    ImportDecl: TGocciaImportDeclaration;
    ImportPair: TStringStringMap.TKeyValuePair;
    J: Integer;
  begin
    Result := False;

    for J := 0 to ProgramNode.Body.Count - 1 do
    begin
      if not (ProgramNode.Body[J] is TGocciaImportDeclaration) then
        Continue;

      ImportDecl := TGocciaImportDeclaration(ProgramNode.Body[J]);
      if ImportDecl.NamespaceName = ALocalName then
      begin
        case ImportDecl.Phase of
          icpDefer:
            Value := LoadDeferredModuleNamespaceValue(
              EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
              ImportDecl.AttributeType), ResolvedPath);
          icpSource:
            Value := LoadModuleSourceValue(EncodeImportSpecifierAttribute(
              ImportDecl.ModulePath, ImportDecl.AttributeType), ResolvedPath);
        else
          Value := LoadModule(EncodeImportSpecifierAttribute(
            ImportDecl.ModulePath, ImportDecl.AttributeType),
            ResolvedPath).GetNamespaceObject;
        end;
        Module.AddExportValue(AExportName, Value);
        Exit(True);
      end;

      for ImportPair in ImportDecl.Imports do
      begin
        if ImportPair.Key <> ALocalName then
          Continue;

        if ImportDecl.Phase = icpSource then
        begin
          Value := LoadModuleSourceValue(EncodeImportSpecifierAttribute(
            ImportDecl.ModulePath, ImportDecl.AttributeType), ResolvedPath);
          Module.AddExportValue(AExportName, Value);
        end
        else
        begin
          SourceModule := LoadModule(EncodeImportSpecifierAttribute(
            ImportDecl.ModulePath, ImportDecl.AttributeType), ResolvedPath);
          Module.AddExportForwarding(AExportName, SourceModule,
            ImportPair.Value);
        end;
        Exit(True);
      end;
    end;
  end;

  function IsImportedLocalName(const ALocalName: string): Boolean;
  var
    ImportDecl: TGocciaImportDeclaration;
    ImportPair: TStringStringMap.TKeyValuePair;
    J: Integer;
  begin
    Result := False;

    for J := 0 to ProgramNode.Body.Count - 1 do
    begin
      if not (ProgramNode.Body[J] is TGocciaImportDeclaration) then
        Continue;

      ImportDecl := TGocciaImportDeclaration(ProgramNode.Body[J]);
      if ImportDecl.NamespaceName = ALocalName then
        Exit(True);

      for ImportPair in ImportDecl.Imports do
        if ImportPair.Key = ALocalName then
          Exit(True);
    end;
  end;

  procedure EvaluateRequestedModulesInSourceOrder;
  var
    ImportDecl: TGocciaImportDeclaration;
    J: Integer;
    RequestedModule: TGocciaModule;
  begin
    for J := 0 to ProgramNode.Body.Count - 1 do
    begin
      Stmt := ProgramNode.Body[J];
      if Stmt is TGocciaImportDeclaration then
      begin
        ImportDecl := TGocciaImportDeclaration(Stmt);
        case ImportDecl.Phase of
          icpEvaluation:
          begin
            RequestedModule := LoadModule(EncodeImportSpecifierAttribute(
              ImportDecl.ModulePath, ImportDecl.AttributeType), ResolvedPath);
            if Assigned(RequestedModule) then
            begin
              if (RequestedModule <> Module) and
                 FEvaluatingModules.ContainsKey(RequestedModule.Path) then
                Module.AsyncCycleRoot := RequestedModule;
              RequestedModules.Add(RequestedModule);
            end;
          end;
          icpDefer:
            LoadDeferredModuleNamespaceValueForEvaluation(
              EncodeImportSpecifierAttribute(ImportDecl.ModulePath,
              ImportDecl.AttributeType), ResolvedPath, RequestedModules);
        end;
      end
      else if Stmt is TGocciaReExportDeclaration then
      begin
        RequestedModule := LoadModule(
          EncodeReExportModuleRequest(TGocciaReExportDeclaration(Stmt)),
          ResolvedPath);
        if Assigned(RequestedModule) then
        begin
          if (RequestedModule <> Module) and
             FEvaluatingModules.ContainsKey(RequestedModule.Path) then
            Module.AsyncCycleRoot := RequestedModule;
          RequestedModules.Add(RequestedModule);
        end;
      end;
    end;
  end;

  procedure DrainRequestedModuleEvaluationPromises;
  var
    AggregatePromise: TGocciaPromiseValue;
    EvalPromise: TGocciaPromiseValue;
    FulfillHandler: TGocciaNativeFunctionValue;
    J: Integer;
    PendingPromises: TList<TGocciaPromiseValue>;
    RejectHandler: TGocciaNativeFunctionValue;
    WaitState: TGocciaModuleEvaluationWaitState;

    function WaitPromiseFor(const ARequestedModule: TGocciaModule): TGocciaPromiseValue;
    var
      WaitModule: TGocciaModule;
    begin
      Result := nil;
      if not Assigned(ARequestedModule) then
        Exit;

      WaitModule := ARequestedModule.AsyncCycleRoot;
      if Assigned(WaitModule) and (WaitModule <> Module) and
         (WaitModule.EvaluationPromise is TGocciaPromiseValue) then
        Exit(TGocciaPromiseValue(WaitModule.EvaluationPromise));

      if ARequestedModule.EvaluationPromise is TGocciaPromiseValue then
        Result := TGocciaPromiseValue(ARequestedModule.EvaluationPromise);
    end;
  begin
    PendingDependencyPromise := nil;

    PendingPromises := TList<TGocciaPromiseValue>.Create;
    try
      for J := 0 to RequestedModules.Count - 1 do
      begin
        EvalPromise := WaitPromiseFor(RequestedModules[J]);
        if not Assigned(EvalPromise) then
          Continue;

        case EvalPromise.State of
          gpsRejected:
            raise TGocciaThrowValue.Create(EvalPromise.PromiseResult);
          gpsPending:
            PendingPromises.Add(EvalPromise);
        end;
      end;

      if PendingPromises.Count = 0 then
        Exit;
      if PendingPromises.Count = 1 then
      begin
        PendingDependencyPromise := PendingPromises[0];
        Exit;
      end;

      AggregatePromise := TGocciaPromiseValue.Create;
      WaitState := TGocciaModuleEvaluationWaitState.Create(AggregatePromise,
        PendingPromises.Count);
      FulfillHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
        WaitState.Fulfill, '<module-dependency-fulfill>', 1);
      FulfillHandler.CapturedRoot := WaitState;
      RejectHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
        WaitState.Reject, '<module-dependency-reject>', 1);
      RejectHandler.CapturedRoot := WaitState;
      for J := 0 to PendingPromises.Count - 1 do
        PendingPromises[J].InvokeThen(FulfillHandler, RejectHandler);
      PendingDependencyPromise := AggregatePromise;
    finally
      PendingPromises.Free;
    end;
  end;

  procedure RegisterStaticModuleExports(const ARegisterIndirectExports: Boolean);
  var
    J: Integer;

    procedure AddStarExportForwardings(const ASourceModule: TGocciaModule);
    var
      Changed: Boolean;
      ExportName: string;
    begin
      repeat
        Changed := False;
        for ExportName in ASourceModule.GetExportNames do
          if ExportName <> KEYWORD_DEFAULT then
            Changed := Module.AddStarExportForwarding(ExportName,
              ASourceModule, ExportName) or Changed;
      until not Changed;
    end;

    procedure ValidateIndirectReExports;
    var
      ExportPair: TStringStringMap.TKeyValuePair;
      ReExportDecl: TGocciaReExportDeclaration;
      SourceModule: TGocciaModule;
      Stmt: TGocciaStatement;
      ValidationIndex: Integer;
    begin
      for ValidationIndex := 0 to ProgramNode.Body.Count - 1 do
      begin
        Stmt := ProgramNode.Body[ValidationIndex];
        if not (Stmt is TGocciaReExportDeclaration) then
          Continue;

        ReExportDecl := TGocciaReExportDeclaration(Stmt);
        if ReExportDecl.IsStarExport then
          Continue;

        SourceModule := LoadModule(EncodeReExportModuleRequest(ReExportDecl),
          ResolvedPath);
        for ExportPair in ReExportDecl.ExportsTable do
          if (not Module.CanResolveExport(ExportPair.Key)) and
             (not IsEvaluatingModulePath(SourceModule.Path)) then
            raise TGocciaSyntaxError.Create(
              Format('Module "%s" has no export named "%s"',
                [ReExportDecl.ModulePath, ExportPair.Value]),
              ReExportDecl.Line, ReExportDecl.Column, ResolvedPath, nil);
      end;
    end;
  begin
    if not ARegisterIndirectExports then
    begin
      Module.ClearExports;

      for J := 0 to ProgramNode.Body.Count - 1 do
      begin
        Stmt := ProgramNode.Body[J];

        if Stmt is TGocciaExportDeclaration then
        begin
          ExportDecl := TGocciaExportDeclaration(Stmt);
          for ExportPair in ExportDecl.ExportsTable do
            if (not IsImportedLocalName(ExportPair.Value)) and
               ModuleScope.Contains(ExportPair.Value) then
              Module.AddExportBinding(ExportPair.Key, ExportPair.Value,
                ModuleScope);
        end
        else if Stmt is TGocciaExportDefaultDeclaration then
        begin
          ExportDefaultDecl := TGocciaExportDefaultDeclaration(Stmt);
          Module.AddExportBinding(KEYWORD_DEFAULT,
            ExportDefaultDecl.LocalName, ModuleScope);
          if ExportDefaultDecl.IsDirectDeclaration and
             (ExportDefaultDecl.Expression is TGocciaFunctionExpression) then
          begin
            Value := ExportDefaultDecl.Expression.Evaluate(Context);
            if (Value is TGocciaFunctionValue) and
               (TGocciaFunctionValue(Value).Name = '') then
              TGocciaFunctionValue(Value).Name := KEYWORD_DEFAULT;
            ModuleScope.ForceUpdateBinding(ExportDefaultDecl.LocalName, Value);
          end;
        end
        else if Stmt is TGocciaExportVariableDeclaration then
        begin
          ExportVarDecl := TGocciaExportVariableDeclaration(Stmt);
          Names := TStringList.Create;
          try
            Names.CaseSensitive := True;
            CollectVariableDeclarationBindingNames(
              ExportVarDecl.Declaration, Names, True);
            for Name in Names do
              Module.AddExportBinding(Name, Name, ModuleScope);
          finally
            Names.Free;
          end;
        end
        else if Stmt is TGocciaExportDestructuringDeclaration then
        begin
          ExportDestructuringDecl :=
            TGocciaExportDestructuringDeclaration(Stmt);
          Names := TStringList.Create;
          try
            Names.CaseSensitive := True;
            CollectPatternBindingNames(
              ExportDestructuringDecl.Declaration.Pattern, Names, True);
            for Name in Names do
              Module.AddExportBinding(Name, Name, ModuleScope);
          finally
            Names.Free;
          end;
        end
        else if Stmt is TGocciaExportFunctionDeclaration then
        begin
          ExportFuncDecl := TGocciaExportFunctionDeclaration(Stmt);
          Module.AddExportBinding(ExportFuncDecl.Declaration.Name,
            ExportFuncDecl.Declaration.Name, ModuleScope);
        end
        else if Stmt is TGocciaExportClassDeclaration then
        begin
          ExportClassDecl := TGocciaExportClassDeclaration(Stmt);
          Module.AddExportBinding(
            ExportClassDecl.Declaration.ClassDefinition.Name,
            ExportClassDecl.Declaration.ClassDefinition.Name, ModuleScope);
        end
        else if Stmt is TGocciaExportEnumDeclaration then
        begin
          Module.AddExportBinding(
            TGocciaExportEnumDeclaration(Stmt).Declaration.Name,
            TGocciaExportEnumDeclaration(Stmt).Declaration.Name, ModuleScope);
        end;
      end;
    end;

    if not ARegisterIndirectExports then
    begin
      Module.InvalidateNamespaceObject;
      Exit;
    end;

    for J := 0 to ProgramNode.Body.Count - 1 do
    begin
      Stmt := ProgramNode.Body[J];

      if Stmt is TGocciaExportDeclaration then
      begin
        ExportDecl := TGocciaExportDeclaration(Stmt);
        for ExportPair in ExportDecl.ExportsTable do
        begin
          if TryAddImportedLocalExport(ExportPair.Value, ExportPair.Key) then
            Continue
          else if (not Module.HasExport(ExportPair.Key)) and
                  TryResolveImportedLocal(ExportPair.Value, Value) then
            Module.AddExportValue(ExportPair.Key, Value);
        end;
      end
      else if Stmt is TGocciaReExportDeclaration then
      begin
        ReExportDecl := TGocciaReExportDeclaration(Stmt);
        SourceModule := LoadModule(EncodeReExportModuleRequest(ReExportDecl),
          ResolvedPath);
        if ReExportDecl.IsStarExport then
        begin
          if ReExportDecl.NamespaceName <> '' then
            Module.AddExportValue(ReExportDecl.NamespaceName,
              SourceModule.GetNamespaceObject)
          else
            AddStarExportForwardings(SourceModule);
        end
        else
        begin
          for ExportPair in ReExportDecl.ExportsTable do
          begin
            if SourceModule.IsAmbiguousExport(ExportPair.Value) then
              raise TGocciaSyntaxError.Create(
                Format('Module "%s" has ambiguous export named "%s"',
                  [ReExportDecl.ModulePath, ExportPair.Value]),
                ReExportDecl.Line, ReExportDecl.Column, ResolvedPath, nil);
            if (not SourceModule.HasExport(ExportPair.Value)) and
               (not IsEvaluatingModulePath(SourceModule.Path)) then
              raise TGocciaSyntaxError.Create(
                Format('Module "%s" has no export named "%s"',
                  [ReExportDecl.ModulePath, ExportPair.Value]),
                ReExportDecl.Line, ReExportDecl.Column, ResolvedPath, nil);
            Module.AddExportForwarding(ExportPair.Key, SourceModule,
              ExportPair.Value);
          end;
        end;
      end;
    end;

    ValidateIndirectReExports;
    ValidateStaticNamedImports(ProgramNode, Module);
    Module.InvalidateNamespaceObject;
  end;
begin
  if not Assigned(FGlobalScope) then
    raise Exception.Create('Module loader runtime is not bound.');

  ImportingFilePath := AImportingFilePath;
  IsDeferredEvaluation := StartsStr(DEFERRED_EVALUATION_REFERRER_PREFIX,
    ImportingFilePath);
  if IsDeferredEvaluation then
    Delete(ImportingFilePath, 1,
      Length(DEFERRED_EVALUATION_REFERRER_PREFIX));

  DecodeImportSpecifierAttribute(AModulePath, RequestedModulePath,
    AttributeType);
  if (AttributeType <> '') and (AttributeType <> 'json') and
     (AttributeType <> 'text') and (AttributeType <> 'bytes') then
    raise TGocciaSyntaxError.Create(
      Format('Unsupported import attribute type "%s"', [AttributeType]),
      0, 0, AImportingFilePath, nil);

  if (AttributeType = '') and TryLoadGlobalModule(RequestedModulePath,
     Result) then
    Exit;

  try
    ResolvedPath := ResolveModuleAddress(RequestedModulePath,
      ImportingFilePath);
  except
    on E: TGocciaRuntimeError do
      raise;
    on E: Exception do
      raise TGocciaRuntimeError.Create(E.Message, 0, 0, ImportingFilePath, nil);
  end;

  CacheKey := ResolvedPath;
  if AttributeType <> '' then
    CacheKey := EncodeImportSpecifierAttribute(ResolvedPath, AttributeType);

  if TryGetCachedFailedModuleError(ResolvedPath, CacheKey, FailedValue) then
    raise TGocciaThrowValue.Create(FailedValue);

  if IsDeferredEvaluation then
  begin
    Seen := TOrderedStringMap<Boolean>.Create;
    try
      if DeferredGraphTouchesEvaluating(CacheKey, Seen) then
        raise EGocciaDeferredModuleNotReady.Create(
          DEFERRED_MODULE_NOT_READY_MESSAGE);
    finally
      Seen.Free;
    end;
  end;

  if FModules.TryGetValue(CacheKey, Result) then
  begin
    if not FLoadingModules.ContainsKey(CacheKey) then
      CheckForModuleReload(Result, CacheKey);
    Exit;
  end;

  if AttributeType = 'json' then
  begin
    Result := LoadJSONModule(ResolvedPath, CacheKey);
    Exit;
  end;

  if AttributeType = 'text' then
  begin
    Result := LoadTextModule(ResolvedPath, CacheKey, True);
    Exit;
  end;

  if AttributeType = 'bytes' then
  begin
    Result := LoadBytesModule(ResolvedPath, CacheKey);
    Exit;
  end;

  if (AttributeType = '') and
     FVirtualModules.GetContentType(ResolvedPath, VirtualContentType) then
  begin
    case VirtualContentType of
      vmctJSON:
        begin
          Result := LoadJSONModule(ResolvedPath, CacheKey);
          Exit;
        end;
      vmctText:
        begin
          Result := LoadTextModule(ResolvedPath, CacheKey, False);
          Exit;
        end;
      vmctBytes:
        begin
          Result := LoadBytesModule(ResolvedPath, CacheKey);
          Exit;
        end;
    end;
  end;

  if LowerCase(ExtractFileExt(ResolvedPath)) = EXT_JSON then
  begin
    Result := LoadJSONModule(ResolvedPath, CacheKey);
    Exit;
  end;

  if IsTextAssetExtension(ExtractFileExt(ResolvedPath)) then
  begin
    Result := LoadTextModule(ResolvedPath, CacheKey, False);
    Exit;
  end;

  Module := nil;
  if Assigned(FRuntimeModuleLoader) and
     FRuntimeModuleLoader(ResolvedPath, Module) then
  begin
    if Assigned(Module) then
    begin
      try
        FModules.Add(CacheKey, Module);
        ClearFailedModuleError(CacheKey);
      except
        Module.Free;
        raise;
      end;
      Result := Module;
      Exit;
    end;
  end;

  Content := LoadResolvedContent(ResolvedPath);
  try
    PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.LabelStatementsEnabled := FLabelStatementsEnabled;
    PipelineOptions.ForInLoopsEnabled := FForInLoopsEnabled;
    PipelineOptions.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
    PipelineOptions.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
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
        RequestedModules := TGocciaModuleList.Create;
      try
        Module := TGocciaModule.Create(ResolvedPath);
        Module.LastModified := Content.LastModified;
        FModules.Add(CacheKey, Module);
        FLoadingModules.Add(CacheKey, True);
        FEvaluatingModules.AddOrSetValue(CacheKey, True);
        FEvaluatingModules.AddOrSetValue(ResolvedPath, True);
        LoadSucceeded := False;
        try
          ModuleScope := FGlobalScope.CreateChild(skModule,
            'Module:' + ResolvedPath);
          Module.SetEnvironment(ModuleScope);
          // ES2026 §16.2.1.6.4 InitializeEnvironment: a Module
          // Environment Record's [[ThisValue]] is undefined.
          ModuleScope.ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          ModuleScope.StrictTypes := FStrictTypesEnabled;
          ModuleScope.NonStrictMode := False;
          ModuleScope.ArgumentsObjectEnabled :=
            cfArgumentsObject in FCompatibility;
          Context.Realm := CurrentRealm;
          Context.Scope := ModuleScope;
          Context.OnError := FOnError;
          Context.LoadModule := LoadModule;
          Context.LoadModuleSource := LoadModuleSourceValue;
          Context.LoadDeferredModule := LoadDeferredModuleNamespaceValue;
          Context.ResolveModuleURL := ResolveModuleURL;
          Context.CurrentFilePath := ResolvedPath;
          Context.CurrentModule := Module;
          Context.CoverageEnabled := False;
          Context.StrictTypes := FStrictTypesEnabled;
          Context.NonStrictMode := False;
          Context.CompatibilityNonStrictMode := False;
          Context.DisposalTracker := nil;

          PredeclareModuleLexicalDeclarations(ProgramNode, ModuleScope);
          HoistFunctionDeclarations(ProgramNode.Body, Context, True);
          if GetVarEnabled then
            HoistVarDeclarations(ProgramNode.Body, ModuleScope, Context);
          Context.ModuleEnvironmentInitialized := True;
          RegisterStaticModuleExports(False);
          EvaluateRequestedModulesInSourceOrder;
          RegisterStaticModuleExports(True);
          DrainRequestedModuleEvaluationPromises;

          if Assigned(PendingDependencyPromise) then
          begin
            DeferredBody := TGocciaDeferredModuleBody.Create(Self,
              ProgramNode, Context, Module, ProgramNode.HasTopLevelAwait,
              PipelineOptions);
            ProgramNode := nil;
            DeferredHandler := TGocciaNativeFunctionValue.CreateWithoutPrototype(
              DeferredBody.Invoke, '<module-evaluation-continuation>', 1);
            DeferredHandler.CapturedRoot := DeferredBody;
            Module.EvaluationPromise := PendingDependencyPromise.InvokeThen(
              DeferredHandler, nil);
            Result := Module;
            LoadSucceeded := True;
            Exit;
          end;

          ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(
            PipelineOptions);
          try
            try
              ProgramConsumed := False;
              try
                FEvaluateModuleBody(ProgramNode, Context, ProgramConsumed);
              finally
                if ProgramConsumed then
                  ProgramNode := nil;
              end;
            except
              on E: EGocciaBytecodeThrow do
              begin
                RecordFailedModuleError(CacheKey, E.ThrownValue,
                  Content.LastModified);
                raise;
              end;
              on E: TGocciaThrowValue do
              begin
                RecordFailedModuleError(CacheKey, E.Value,
                  Content.LastModified);
                raise;
              end;
            end;
          finally
            ActiveOptionsScope.Free;
          end;
          Result := Module;
          LoadSucceeded := True;
          ClearFailedModuleError(CacheKey);
        finally
          FEvaluatingModules.Remove(CacheKey);
          FEvaluatingModules.Remove(ResolvedPath);
          FLoadingModules.Remove(CacheKey);
          if not LoadSucceeded then
          begin
            FModules.Remove(CacheKey);
            Module.Free;
          end;
        end;
      finally
        RequestedModules.Free;
        ProgramNode.Free;
      end;
    finally
      ModuleParseResult.Free;
    end;
  finally
    Content.Free;
  end;
end;

function TGocciaModuleLoader.LoadModuleSourceValue(const AModulePath,
  AImportingFilePath: string): TGocciaValue;
var
  AttributeType: string;
  CacheKey: string;
  Content: TGocciaModuleContent;
  I: Integer;
  ModuleParseResult: TGocciaSourcePipelineModuleResult;
  ModuleWarning: TGocciaSourcePipelineWarning;
  PipelineOptions: TGocciaSourcePipelineOptions;
  RequestedModulePath: string;
  ResolvedPath: string;
  SourceValue: TGocciaValue;
  VirtualContentType: TGocciaVirtualModuleContentType;
begin
  DecodeImportSpecifierAttribute(AModulePath, RequestedModulePath,
    AttributeType);
  // Import Bytes non-goal: bytes modules are not exposed as source-phase
  // imports, so reject with a clear message rather than the generic
  // unsupported-attribute error.
  if AttributeType = 'bytes' then
    raise TGocciaSyntaxError.Create(
      'Source phase imports are not supported for bytes modules',
      0, 0, AImportingFilePath, nil);
  if (AttributeType <> '') and (AttributeType <> 'json') and
     (AttributeType <> 'text') then
    raise TGocciaSyntaxError.Create(
      Format('Unsupported import attribute type "%s"', [AttributeType]),
      0, 0, AImportingFilePath, nil);

  if RequestedModulePath = MODULE_SOURCE_SENTINEL then
  begin
    if AttributeType <> '' then
      raise TGocciaSyntaxError.Create(
        'Module source is not available for attributed test262 sentinel',
        0, 0, AImportingFilePath, nil);
    if FModuleSourceValues.TryGetValue(RequestedModulePath, SourceValue) then
      Exit(SourceValue);
    SourceValue := TGocciaModuleSourceValue.Create(RequestedModulePath, '');
    FModuleSourceValues.Add(RequestedModulePath, SourceValue);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddRootObject(SourceValue);
    Exit(SourceValue);
  end;

  try
    ResolvedPath := ResolveModuleAddress(RequestedModulePath,
      AImportingFilePath);
  except
    on E: TGocciaRuntimeError do
      raise;
    on E: Exception do
      raise TGocciaRuntimeError.Create(E.Message, 0, 0, AImportingFilePath, nil);
  end;

  CacheKey := ResolvedPath;
  if AttributeType <> '' then
    CacheKey := EncodeImportSpecifierAttribute(ResolvedPath, AttributeType);
  if FModuleSourceValues.TryGetValue(CacheKey, SourceValue) then
    Exit(SourceValue);

  if (AttributeType <> '') or
     ((not FVirtualModules.GetContentType(ResolvedPath,
       VirtualContentType)) and
      (not IsScriptExtension(ExtractFileExt(ResolvedPath)))) or
     (FVirtualModules.GetContentType(ResolvedPath, VirtualContentType) and
      (VirtualContentType <> vmctJavaScript)) then
    raise TGocciaSyntaxError.Create(
      Format('Module source is not available for "%s"', [ResolvedPath]),
      0, 0, AImportingFilePath, nil);

  if not (FExperimentalJSModuleSourceEnabled or
     (cfExperimentalJSModuleSource in FCompatibility)) then
    raise TGocciaSyntaxError.Create(
      'JavaScript ModuleSource objects require --experimental-js-module-source',
      0, 0, AImportingFilePath, nil);

  Content := LoadResolvedContent(ResolvedPath);
  try
    PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.LabelStatementsEnabled := FLabelStatementsEnabled;
    PipelineOptions.ForInLoopsEnabled := FForInLoopsEnabled;
    PipelineOptions.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
    PipelineOptions.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
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

      SourceValue := TGocciaModuleSourceValue.Create(ResolvedPath,
        Content.Text);
      FModuleSourceValues.Add(CacheKey, SourceValue);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddRootObject(SourceValue);
      Result := SourceValue;
    finally
      ModuleParseResult.Free;
    end;
  finally
    Content.Free;
  end;
end;

function TGocciaModuleLoader.DeferredGraphTouchesEvaluating(
  const AResolvedPath: string; const ASeen: TOrderedStringMap<Boolean>): Boolean;
var
  AttributeType: string;
  Content: TGocciaModuleContent;
  ExistingModule: TGocciaModule;
  ExistingPromise: TGocciaPromiseValue;
  ModuleParseResult: TGocciaSourcePipelineModuleResult;
  PhysicalPath: string;
  PipelineOptions: TGocciaSourcePipelineOptions;
  ProgramNode: TGocciaProgram;
  RequestedAttributeType: string;
  RequestedPath: string;
  ResolvedPath: string;
  Stmt: TGocciaStatement;
  I: Integer;
begin
  DecodeImportSpecifierAttribute(AResolvedPath, PhysicalPath, AttributeType);

  if ASeen.ContainsKey(AResolvedPath) then
    Exit(False);
  ASeen.Add(AResolvedPath, True);

  if FEvaluatingModules.ContainsKey(AResolvedPath) or
     FEvaluatingModules.ContainsKey(PhysicalPath) or
     FLoadingModules.ContainsKey(AResolvedPath) or
     FLoadingModules.ContainsKey(PhysicalPath) then
    Exit(True);

  if FModules.TryGetValue(AResolvedPath, ExistingModule) and
     (ExistingModule.EvaluationPromise is TGocciaPromiseValue) then
  begin
    ExistingPromise := TGocciaPromiseValue(ExistingModule.EvaluationPromise);
    if ExistingPromise.State = gpsPending then
      Exit(True);
  end;

  if AttributeType <> '' then
    Exit(False);

  if not IsJavaScriptModuleResource(PhysicalPath) then
    Exit(False);

  Content := LoadResolvedContent(PhysicalPath);
  try
    PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.LabelStatementsEnabled := FLabelStatementsEnabled;
    PipelineOptions.ForInLoopsEnabled := FForInLoopsEnabled;
    PipelineOptions.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
    PipelineOptions.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
    PipelineOptions.SourceType := stModule;
    ModuleParseResult := TGocciaSourcePipeline.ParseModuleSource(Content.Text,
      PhysicalPath, PipelineOptions);
    try
      ProgramNode := ModuleParseResult.TakeProgramNode;
      try
        for I := 0 to ProgramNode.Body.Count - 1 do
        begin
          Stmt := ProgramNode.Body[I];
          if Stmt is TGocciaImportDeclaration then
          begin
            RequestedPath := TGocciaImportDeclaration(Stmt).ModulePath;
            RequestedAttributeType :=
              TGocciaImportDeclaration(Stmt).AttributeType;
          end
          else if Stmt is TGocciaReExportDeclaration then
          begin
            RequestedPath := TGocciaReExportDeclaration(Stmt).ModulePath;
            RequestedAttributeType :=
              TGocciaReExportDeclaration(Stmt).AttributeType;
          end
          else
            Continue;

          ResolvedPath := ResolveModuleRequestWithAttribute(RequestedPath,
            RequestedAttributeType, PhysicalPath);
          if DeferredGraphTouchesEvaluating(ResolvedPath, ASeen) then
            Exit(True);
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

  Result := False;
end;

procedure TGocciaModuleLoader.EvaluateDeferredAsyncDependencies(
  const AResolvedPath, AImportingFilePath: string;
  const ASeen: TOrderedStringMap<Boolean>;
  const ARequestedModules: TGocciaModuleList);
var
  AttributeType: string;
  Content: TGocciaModuleContent;
  LoadedModule: TGocciaModule;
  ModuleParseResult: TGocciaSourcePipelineModuleResult;
  PhysicalPath: string;
  PipelineOptions: TGocciaSourcePipelineOptions;
  ProgramNode: TGocciaProgram;
  RequestedAttributeType: string;
  RequestedPath: string;
  ResolvedPath: string;
  Stmt: TGocciaStatement;
  I: Integer;
begin
  DecodeImportSpecifierAttribute(AResolvedPath, PhysicalPath, AttributeType);

  if ASeen.ContainsKey(AResolvedPath) then
    Exit;
  ASeen.Add(AResolvedPath, True);

  if AttributeType <> '' then
    Exit;

  if not IsJavaScriptModuleResource(PhysicalPath) then
    Exit;

  Content := LoadResolvedContent(PhysicalPath);
  try
    PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.LabelStatementsEnabled := FLabelStatementsEnabled;
    PipelineOptions.ForInLoopsEnabled := FForInLoopsEnabled;
    PipelineOptions.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
    PipelineOptions.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
    PipelineOptions.SourceType := stModule;
    ModuleParseResult := TGocciaSourcePipeline.ParseModuleSource(Content.Text,
      PhysicalPath, PipelineOptions);
    try
      ProgramNode := ModuleParseResult.TakeProgramNode;
      try
        if ProgramNode.HasTopLevelAwait then
        begin
          LoadedModule := LoadModule(AResolvedPath, AImportingFilePath);
          if Assigned(ARequestedModules) and Assigned(LoadedModule) then
            ARequestedModules.Add(LoadedModule)
          else if Assigned(LoadedModule) and
                  (LoadedModule.EvaluationPromise is TGocciaPromiseValue) and
                  (TGocciaPromiseValue(LoadedModule.EvaluationPromise).State =
                  gpsRejected) then
            raise TGocciaThrowValue.Create(
              TGocciaPromiseValue(LoadedModule.EvaluationPromise).PromiseResult);
          Exit;
        end;

        for I := 0 to ProgramNode.Body.Count - 1 do
        begin
          Stmt := ProgramNode.Body[I];
          if Stmt is TGocciaImportDeclaration then
          begin
            RequestedPath := TGocciaImportDeclaration(Stmt).ModulePath;
            RequestedAttributeType :=
              TGocciaImportDeclaration(Stmt).AttributeType;
          end
          else if Stmt is TGocciaReExportDeclaration then
          begin
            RequestedPath := TGocciaReExportDeclaration(Stmt).ModulePath;
            RequestedAttributeType :=
              TGocciaReExportDeclaration(Stmt).AttributeType;
          end
          else
            Continue;

          ResolvedPath := ResolveModuleRequestWithAttribute(RequestedPath,
            RequestedAttributeType, PhysicalPath);
          EvaluateDeferredAsyncDependencies(ResolvedPath, PhysicalPath, ASeen,
            ARequestedModules);
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

procedure TGocciaModuleLoader.ValidateDeferredModuleLinks(
  const AResolvedPath, AImportingFilePath: string;
  const ASeen: TOrderedStringMap<Boolean>);
var
  AttributeType: string;
  Content: TGocciaModuleContent;
  ImportDecl: TGocciaImportDeclaration;
  ModuleParseResult: TGocciaSourcePipelineModuleResult;
  PhysicalPath: string;
  PipelineOptions: TGocciaSourcePipelineOptions;
  ProgramNode: TGocciaProgram;
  ReExportDecl: TGocciaReExportDeclaration;
  RequestedAttributeType: string;
  RequestedPath: string;
  ResolvedPath: string;
  Stmt: TGocciaStatement;
  I: Integer;
begin
  DecodeImportSpecifierAttribute(AResolvedPath, PhysicalPath, AttributeType);

  if ASeen.ContainsKey(AResolvedPath) then
    Exit;
  ASeen.Add(AResolvedPath, True);

  if AttributeType <> '' then
    Exit;

  if not IsJavaScriptModuleResource(PhysicalPath) then
    Exit;

  Content := LoadResolvedContent(PhysicalPath);
  try
    PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
    PipelineOptions.Preprocessors := FPreprocessors;
    PipelineOptions.Compatibility := FCompatibility;
    PipelineOptions.LabelStatementsEnabled := FLabelStatementsEnabled;
    PipelineOptions.ForInLoopsEnabled := FForInLoopsEnabled;
    PipelineOptions.ExperimentalJSModuleSourceEnabled :=
      FExperimentalJSModuleSourceEnabled;
    PipelineOptions.WarningUnsupportedFeatures := FWarningUnsupportedFeatures;
    PipelineOptions.SourceType := stModule;
    ModuleParseResult := TGocciaSourcePipeline.ParseModuleSource(Content.Text,
      PhysicalPath, PipelineOptions);
    try
      ProgramNode := ModuleParseResult.TakeProgramNode;
      try
        for I := 0 to ProgramNode.Body.Count - 1 do
        begin
          Stmt := ProgramNode.Body[I];
          if Stmt is TGocciaImportDeclaration then
          begin
            RequestedPath := TGocciaImportDeclaration(Stmt).ModulePath;
            RequestedAttributeType :=
              TGocciaImportDeclaration(Stmt).AttributeType;
          end
          else if Stmt is TGocciaReExportDeclaration then
          begin
            RequestedPath := TGocciaReExportDeclaration(Stmt).ModulePath;
            RequestedAttributeType :=
              TGocciaReExportDeclaration(Stmt).AttributeType;
          end
          else
            Continue;

          ResolvedPath := ResolveModuleRequestWithAttribute(RequestedPath,
            RequestedAttributeType, PhysicalPath);
          ValidateDeferredModuleLinks(ResolvedPath, PhysicalPath, ASeen);
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

function TGocciaModuleLoader.LoadDeferredModuleNamespaceValue(
  const AModulePath, AImportingFilePath: string): TGocciaValue;
begin
  Result := LoadDeferredModuleNamespaceValueForEvaluation(AModulePath,
    AImportingFilePath, nil);
end;

function TGocciaModuleLoader.LoadDeferredModuleNamespaceValueForEvaluation(
  const AModulePath, AImportingFilePath: string;
  const ARequestedModules: TGocciaModuleList): TGocciaValue;
var
  AttributeType: string;
  CacheKey: string;
  DeferredModulePath: string;
  RequestedModulePath: string;
  ResolvedPath: string;
  Seen: TOrderedStringMap<Boolean>;
begin
  DecodeImportSpecifierAttribute(AModulePath, RequestedModulePath,
    AttributeType);
  if (AttributeType <> '') and (AttributeType <> 'json') and
     (AttributeType <> 'text') and (AttributeType <> 'bytes') then
    raise TGocciaSyntaxError.Create(
      Format('Unsupported import attribute type "%s"', [AttributeType]),
      0, 0, AImportingFilePath, nil);

  if (AttributeType = '') and HasGlobalModuleRequest(RequestedModulePath) then
    ResolvedPath := RequestedModulePath
  else
  begin
    try
      if Assigned(FResolver) then
        ResolvedPath := ResolveModuleAddress(RequestedModulePath,
          AImportingFilePath)
      else
        ResolvedPath := ResolveModuleAddress(RequestedModulePath,
          AImportingFilePath);
    except
      on E: TGocciaRuntimeError do
        raise;
      on E: Exception do
        raise TGocciaRuntimeError.Create(E.Message, 0, 0, AImportingFilePath,
          nil);
    end;
  end;

  CacheKey := ResolvedPath;
  if AttributeType <> '' then
    CacheKey := EncodeImportSpecifierAttribute(ResolvedPath, AttributeType);

  if FDeferredModuleNamespaces.TryGetValue(CacheKey, Result) then
    Exit;

  Seen := TOrderedStringMap<Boolean>.Create;
  try
    ValidateDeferredModuleLinks(CacheKey, AImportingFilePath, Seen);
  finally
    Seen.Free;
  end;

  Seen := TOrderedStringMap<Boolean>.Create;
  try
    EvaluateDeferredAsyncDependencies(CacheKey, AImportingFilePath, Seen,
      ARequestedModules);
  finally
    Seen.Free;
  end;

  DeferredModulePath := EncodeImportSpecifierAttribute(ResolvedPath,
    AttributeType);
  Result := TGocciaDeferredModuleNamespaceObject.Create(DeferredModulePath,
    AImportingFilePath, LoadModule);
  FDeferredModuleNamespaces.Add(CacheKey, Result);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddRootObject(Result);
end;

procedure TGocciaModuleLoader.CheckForModuleReload(const AModule: TGocciaModule;
  const ACacheKey: string);
var
  CurrentModified: TDateTime;
  ReloadCacheKey: string;
  ReloadedModule: TGocciaModule;
begin
  if not TryGetResolvedLastModified(AModule.Path, CurrentModified) then
    Exit;

  if CurrentModified > AModule.LastModified then
  begin
    ReloadCacheKey := ACacheKey;
    if ReloadCacheKey = '' then
      ReloadCacheKey := AModule.Path;

    FModules.Remove(ReloadCacheKey);
    try
      ReloadedModule := LoadModule(ReloadCacheKey, AModule.Path);
    except
      FModules.AddOrSetValue(ReloadCacheKey, AModule);
      raise;
    end;

    if ReloadedModule <> AModule then
    begin
      try
        CopyModuleContents(ReloadedModule, AModule);
      finally
        FModules.Remove(ReloadCacheKey);
        FModules.AddOrSetValue(ReloadCacheKey, AModule);
        ReloadedModule.Free;
      end;
    end;
  end;
end;

procedure TGocciaModuleLoader.RegisterGlobalModuleProvider(
  const AModulePath: string; const AProvider: TGocciaGlobalModuleProvider);
begin
  if not Assigned(AProvider) then
    raise Exception.Create('Global module provider cannot be nil.');
  if FVirtualModules.Contains(AModulePath) then
    raise EInvalidOperation.CreateFmt(
      'Host module "%s" conflicts with a configured virtual module.',
      [AModulePath]);
  FGlobalModules.Remove(AModulePath);
  FGlobalModuleProviders.AddOrSetValue(AModulePath, AProvider);
end;

procedure TGocciaModuleLoader.UnregisterGlobalModuleProvider(
  const AModulePath: string);
begin
  FGlobalModuleProviders.Remove(AModulePath);
  FGlobalModules.Remove(AModulePath);
end;

function TGocciaModuleLoader.LoadJSONModule(const AResolvedPath,
  ACacheKey: string): TGocciaModule;
var
  Content: TGocciaModuleContent;
  HasDefaultKey: Boolean;
  Key: string;
  Module: TGocciaModule;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  JSONParser: TGocciaJSONParser;
  LoadSucceeded: Boolean;
begin
  Content := LoadResolvedContent(AResolvedPath);
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
        HasDefaultKey := False;
        if ParsedValue is TGocciaObjectValue then
        begin
          Obj := TGocciaObjectValue(ParsedValue);
          for Key in Obj.GetOwnPropertyKeys do
          begin
            HasDefaultKey := HasDefaultKey or (Key = KEYWORD_DEFAULT);
            Module.AddExportValue(Key, Obj.GetProperty(Key));
          end;
        end;
        if not HasDefaultKey then
          Module.AddExportValue(KEYWORD_DEFAULT, ParsedValue);

        FModules.Add(ACacheKey, Module);
        ClearFailedModuleError(ACacheKey);
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

function TGocciaModuleLoader.LoadTextModule(const AResolvedPath,
  ACacheKey: string; const ADefaultOnly: Boolean): TGocciaModule;
var
  Content: TGocciaModuleContent;
  LoadSucceeded: Boolean;
  Metadata: TGocciaObjectValue;
  Module: TGocciaModule;
  NormalizedText: UTF8String;
  TextValue: TGocciaValue;
begin
  Content := LoadResolvedContent(AResolvedPath);
  try
    NormalizedText := NormalizeUTF8NewlinesToLF(Content.Text);
    TextValue := TGocciaStringLiteralValue.FromUTF8(NormalizedText);

    Metadata := nil;
    if not ADefaultOnly then
    begin
      Metadata := TGocciaObjectValue.Create(
        TGocciaObjectValue.SharedObjectPrototype, 5);
      Metadata.SetProperty(PROP_KIND, TGocciaStringLiteralValue.Create('text'));
      Metadata.SetProperty(PROP_PATH,
        TGocciaStringLiteralValue.Create(AResolvedPath));
      Metadata.SetProperty(PROP_FILE_NAME,
        TGocciaStringLiteralValue.Create(ExtractFileName(AResolvedPath)));
      Metadata.SetProperty(PROP_EXTENSION,
        TGocciaStringLiteralValue.Create(ExtractFileExt(AResolvedPath)));
      Metadata.SetProperty(PROP_BYTE_LENGTH,
        TGocciaNumberLiteralValue.Create(Length(Content.Text)));
      Metadata.Freeze;
    end;

    Module := TGocciaModule.Create(AResolvedPath);
    Module.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      if not ADefaultOnly then
      begin
        Module.AddExportValue(PROP_METADATA, Metadata);
        Module.AddExportValue(PROP_CONTENT, TextValue);
      end;
      Module.AddExportValue(KEYWORD_DEFAULT, TextValue);
      FModules.Add(ACacheKey, Module);
      ClearFailedModuleError(ACacheKey);
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

// Import Bytes proposal: load the resolved file as raw bytes, ignoring file
// extension/MIME, and expose a single default export that is a Uint8Array
// backed by an immutable ArrayBuffer. Named imports are rejected naturally
// because the synthetic module declares only the default export.
function TGocciaModuleLoader.LoadBytesModule(const AResolvedPath,
  ACacheKey: string): TGocciaModule;
var
  Buffer: TGocciaArrayBufferValue;
  Bytes: TBytes;
  LastModified: TDateTime;
  LoadSucceeded: Boolean;
  Module: TGocciaModule;
  TypedArray: TGocciaTypedArrayValue;
begin
  Bytes := LoadResolvedContentBytes(AResolvedPath);
  if not TryGetResolvedLastModified(AResolvedPath, LastModified) then
    LastModified := 0;

  Buffer := TGocciaArrayBufferValue.CreateImmutableFromBytes(Bytes);
  TypedArray := TGocciaTypedArrayValue.Create(takUint8, Buffer);

  // Root the view (which marks its backing buffer) until the module owns it.
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(TypedArray);
  try
    Module := TGocciaModule.Create(AResolvedPath);
    Module.LastModified := LastModified;
    LoadSucceeded := False;
    try
      Module.AddExportValue(KEYWORD_DEFAULT, TypedArray);
      FModules.Add(ACacheKey, Module);
      ClearFailedModuleError(ACacheKey);
      Result := Module;
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
        Module.Free;
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(TypedArray);
  end;
end;

end.
