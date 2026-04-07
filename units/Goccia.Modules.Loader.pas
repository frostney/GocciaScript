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

    procedure CopyModuleContents(const ASourceModule,
      ATargetModule: TGocciaModule);
    function LoadStructuredDataModule(const AResolvedPath: string): TGocciaModule;
    function LoadTextAssetModule(const AResolvedPath: string): TGocciaModule;
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
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.FileExtensions,
  Goccia.JSON,
  Goccia.JSON5,
  Goccia.JSONL,
  Goccia.JSX.SourceMap,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.TextFiles,
  Goccia.TOML,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.YAML;

const
  TEXT_ASSET_KIND = 'text';

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

  if IsStructuredDataExtension(ExtractFileExt(ResolvedPath)) then
  begin
    Result := LoadStructuredDataModule(ResolvedPath);
    Exit;
  end;

  if IsTextAssetExtension(ExtractFileExt(ResolvedPath)) then
  begin
    Result := LoadTextAssetModule(ResolvedPath);
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

function TGocciaModuleLoader.LoadTextAssetModule(
  const AResolvedPath: string): TGocciaModule;
var
  Content: TGocciaModuleContent;
  Metadata: TGocciaObjectValue;
  Module: TGocciaModule;
  LoadSucceeded: Boolean;
  NormalizedText: UTF8String;
begin
  Content := FContentProvider.LoadContent(AResolvedPath);
  try
    NormalizedText := NormalizeUTF8NewlinesToLF(Content.Text);
    Metadata := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype, 5);
    Metadata.SetProperty(PROP_KIND, TGocciaStringLiteralValue.Create(TEXT_ASSET_KIND));
    Metadata.SetProperty(PROP_PATH, TGocciaStringLiteralValue.Create(AResolvedPath));
    Metadata.SetProperty(PROP_FILE_NAME,
      TGocciaStringLiteralValue.Create(ExtractFileName(AResolvedPath)));
    Metadata.SetProperty(PROP_EXTENSION,
      TGocciaStringLiteralValue.Create(LowerCase(ExtractFileExt(AResolvedPath))));
    Metadata.SetProperty(PROP_BYTE_LENGTH,
      TGocciaNumberLiteralValue.Create(Length(Content.Text)));
    Metadata.Freeze;

    Module := TGocciaModule.Create(AResolvedPath);
    Module.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      Module.ExportsTable.AddOrSetValue(PROP_METADATA, Metadata);
      Module.ExportsTable.AddOrSetValue(PROP_CONTENT,
        TGocciaStringLiteralValue.Create(NormalizedText));
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

function TGocciaModuleLoader.LoadStructuredDataModule(
  const AResolvedPath: string): TGocciaModule;
var
  Content: TGocciaModuleContent;
  Documents: TGocciaArrayValue;
  DocumentIndex: Integer;
  Extension: string;
  Key: string;
  Module: TGocciaModule;
  Obj: TGocciaObjectValue;
  ParsedDocument: TGocciaValue;
  ParsedValue: TGocciaValue;
  JSONParser: TGocciaJSONParser;
  JSON5Parser: TGocciaJSON5Parser;
  TOMLParser: TGocciaTOMLParser;
  JSONLParser: TGocciaJSONLParser;
  JSONLRecords: TGocciaArrayValue;
  YAMLParser: TGocciaYAMLParser;
  LoadSucceeded: Boolean;
begin
  Content := FContentProvider.LoadContent(AResolvedPath);
  try
    Documents := nil;
    JSONLRecords := nil;
    ParsedDocument := nil;
    ParsedValue := nil;
    Extension := LowerCase(ExtractFileExt(AResolvedPath));
    if Extension = EXT_JSON then
    begin
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
    end
    else if IsJSON5Extension(Extension) then
    begin
      JSON5Parser := TGocciaJSON5Parser.Create;
      try
        try
          ParsedValue := JSON5Parser.Parse(Content.Text);
        except
          on E: EGocciaJSON5ParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse JSON5 module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        JSON5Parser.Free;
      end;
    end
    else if IsJSONLExtension(Extension) then
    begin
      JSONLParser := TGocciaJSONLParser.Create;
      try
        try
          JSONLRecords := JSONLParser.Parse(Content.Text);
        except
          on E: EGocciaJSONLParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse JSONL module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        JSONLParser.Free;
      end;
    end
    else if IsTOMLExtension(Extension) then
    begin
      TOMLParser := TGocciaTOMLParser.Create;
      try
        try
          ParsedValue := TOMLParser.Parse(Content.Text);
        except
          on E: EGocciaTOMLParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse TOML module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        TOMLParser.Free;
      end;
    end
    else
    begin
      YAMLParser := TGocciaYAMLParser.Create;
      try
        try
          Documents := YAMLParser.ParseDocuments(Content.Text);
        except
          on E: EGocciaYAMLParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse YAML module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        YAMLParser.Free;
      end;

      if Documents.Elements.Count = 0 then
        raise TGocciaRuntimeError.Create(
          Format('YAML module "%s" must contain at least one top-level document.',
            [AResolvedPath]),
          0, 0, AResolvedPath, nil);

      if Documents.Elements.Count = 1 then
        ParsedDocument := Documents.Elements[0];
    end;

    Module := TGocciaModule.Create(AResolvedPath);
    Module.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      if (Extension = EXT_JSON) or IsJSON5Extension(Extension) or
         IsTOMLExtension(Extension) then
        ParsedDocument := ParsedValue;

      if Assigned(JSONLRecords) then
      begin
        for DocumentIndex := 0 to JSONLRecords.Elements.Count - 1 do
          Module.ExportsTable.AddOrSetValue(IntToStr(DocumentIndex),
            JSONLRecords.Elements[DocumentIndex]);
      end
      else if Assigned(Documents) and (Documents.Elements.Count > 1) then
      begin
        // Multi-document YAML modules expose each document by its string index.
        for DocumentIndex := 0 to Documents.Elements.Count - 1 do
          Module.ExportsTable.AddOrSetValue(IntToStr(DocumentIndex),
            Documents.Elements[DocumentIndex]);
      end
      else if ParsedDocument is TGocciaObjectValue then
      begin
        Obj := TGocciaObjectValue(ParsedDocument);
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
    JSONLRecords.Free;
    Documents.Free;
    Content.Free;
  end;
end;

end.
