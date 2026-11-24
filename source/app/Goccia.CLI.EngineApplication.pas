unit Goccia.CLI.EngineApplication;

{ The old engine's half of the command-line application: TGocciaCLIApplication
  (Goccia.CLI.Application, engine-free) with the members that make and
  configure a TGocciaEngine for a file. The apps on the old engine derive from
  it; the G2 apps derive from TGocciaCLIApplication. }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,

  Goccia.Builtins.GlobalShadowRealm,
  Goccia.CLI.Application,
  Goccia.CLI.FileSettings,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Trust,
  Goccia.Engine,
  Goccia.Values.Primitives;

type
  TGocciaCLIEngineApplication = class(TGocciaCLIApplication)
  protected
    { the old engine's stack and inspect depths, then the base's services }
    procedure InitializeSingletons; override;
    { a value the old engine threw, with its source context }
    procedure HandleError(const AException: Exception); override;
    { A worker a thread pool abandoned may still be running its file, and
      that reads this application's objects and the process-wide runtime:
      end the process before App.Free and unit finalization can free them
      under it, on every path out of Run. The report is already written. }
    class procedure ProcessEnding(const AExitCode: LongInt); override;
    { Every engine is gone by now. Shutting the main thread's runtime down
      destroys what its collector still manages, as each worker's does when
      it exits, so a run ends with nothing left allocated (#1592). That
      includes the process-wide singletons (primitives, well-known symbols,
      the array hole), whose cached pointers are not reset: this is the
      process's last runtime, and no engine may start after it (ADR 0143).
      A worker a thread pool abandoned still runs and reads those values, so
      then the runtime is left to the process exit. }
    class procedure ProcessEnded; override;
    procedure ConfigureCreatedEngine(const AEngine: TGocciaEngine;
      const AFileConfig: TConfigEntryArray); virtual;
    procedure ConfigureCapabilityAudit(const AEngine: TGocciaEngine);
    procedure ApplyVirtualModulesToEngine(const AEngine: TGocciaEngine;
      const AFileConfigPath: string);
    { The command line's --module and --modules alone, without any config's
      modules (GocciaRunner's sandbox mode). }
    procedure ApplyCommandLineVirtualModulesToEngine(
      const AEngine: TGocciaEngine);
    function CreateEngine(const AFileName: string;
      const ASource: TStringList): TGocciaEngine;
  end;

{ Injects the globals of a file or module a config at AConfigPath names
  (AWritten, relative to the config): read under AEngine's capability set
  with the config's directory as the project; a data file is parsed, and a
  module runs in an engine of its own whose named exports cross as data. }
procedure InjectConfiguredGlobals(const AEngine: TGocciaEngine;
  const AWritten, AConfigPath: string);
{ Configures AEngine's clock and random source from a host-environment module
  a config at AConfigPath names: read under AEngine's capability set, loaded
  as a guest module of AEngine, never host-owned. }
procedure ConfigureConfiguredHostEnvironment(const AEngine: TGocciaEngine;
  const AWritten, AConfigPath: string);

function ResolveSourceTypeOption(
  const AOption: TEnumOption<Goccia.CLI.Options.TGocciaSourceType>;
  const AFileConfig: TConfigEntryArray;
  const AFileName: string): Goccia.Engine.TGocciaSourceType;
{ A file's resolved settings (Goccia.CLI.FileSettings) applied to the old
  engine. }
procedure ApplyFileSettingsToEngine(const AEngine: TGocciaEngine;
  const ASettings: TGocciaFileSettings);
{ The single place engine-affecting options reach an engine.  CreateEngine
  calls it for every binary that builds its engine through the base class;
  engines that must be constructed elsewhere — GocciaRunner's sandbox mode
  needs its own module resolver — call it directly rather than restating
  the option set, which is how the sandbox runner once lost --max-memory
  and the fetch policy.  Pass an empty AFileConfig when there is no host
  file to discover a per-file config for. AAcceptedUnsafe are the
  unsafe-* requests of the file's config that are trusted or accepted for
  the run (TGocciaConfigTrustVerdict.AcceptedUnsafe); the command-line flags
  apply regardless. }
procedure ApplyFileConfigToEngine(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray; const AFileName: string;
  const AAcceptedUnsafe: TGocciaUnsafeRequests);

{ A --global's value text as the old engine's value: JSON when it parses as
  JSON, else the text as a string }
function ParseInlineGlobalValue(const AValueText: string): TGocciaValue;

implementation

uses
  Generics.Collections,
  Math,

  CLI.Parser,
  FileUtils,
  TextEncoding,
  TextSemantics,

  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.DataTree.Values,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Error.Suggestions,
  Goccia.FileExtensions,
  Goccia.FloatingPoint,
  Goccia.GarbageCollector,
  Goccia.HostEnvironment.JavaScript,
  Goccia.JSON,
  Goccia.JSON.Utils,
  Goccia.JSON5,
  Goccia.Keywords.Reserved,
  Goccia.Modules,
  Goccia.Modules.Configuration,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.ScriptLoader.Globals,
  Goccia.ScriptLoader.Input,
  Goccia.StackLimit,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Formatting,
  Goccia.Values.HoleValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.VM,
  Goccia.VM.Exception;

function IsAbsoluteFilePath(const APath: string): Boolean;
begin
  Result := (APath <> '') and ((APath[1] = PathDelim) or
    ((Length(APath) >= 2) and (APath[2] = ':')));
end;

function ParseInlineGlobalValue(const AValueText: string): TGocciaValue;
var
  Parser: TGocciaJSONParser;
begin
  Parser := TGocciaJSONParser.Create;
  try
    try
      Result := Parser.Parse(AValueText);
    except
      on Exception do
        Result := TGocciaStringLiteralValue.Create(AValueText);
    end;
  finally
    Parser.Free;
  end;
end;

procedure TGocciaCLIEngineApplication.InitializeSingletons;
begin
  SetMaxStackDepth(DEFAULT_MAX_STACK_DEPTH);
  SetInspectDepth(DEFAULT_INSPECT_DEPTH);
  if Assigned(EngineOptions) then
  begin
    { MaxStack.Maximum keeps the value within Integer. }
    SetMaxStackDepth(Integer(EngineOptions.MaxStack.ValueOr(
      DEFAULT_MAX_STACK_DEPTH)));
    SetInspectDepth(EngineOptions.InspectDepth.ValueOr(DEFAULT_INSPECT_DEPTH));
  end;
  inherited InitializeSingletons;
end;

procedure TGocciaCLIEngineApplication.HandleError(const AException: Exception);
begin
  if AException is TGocciaThrowValue then
    WriteLn(FormatThrowDetail(TGocciaThrowValue(AException).Value, '', nil,
      IsColorTerminal, 0, TGocciaThrowValue(AException).Suggestion))
  else if AException is EGocciaBytecodeThrow then
    WriteLn(FormatThrowDetail(EGocciaBytecodeThrow(AException).ThrownValue,
      '', nil, IsColorTerminal, 0, EGocciaBytecodeThrow(AException).Suggestion))
  else
    inherited HandleError(AException);
end;

class procedure TGocciaCLIEngineApplication.ProcessEnding(
  const AExitCode: LongInt);
begin
  EndProcessIfWorkerAbandoned(AExitCode);
end;

class procedure TGocciaCLIEngineApplication.ProcessEnded;
begin
  ShutdownMainThreadRuntime;
end;

{ --source-type / config "source-type" as the old engine's enum
  (Goccia.CLI.FileSettings.ResolveFileSourceType has the rules) }
function ResolveSourceTypeOption(
  const AOption: TEnumOption<Goccia.CLI.Options.TGocciaSourceType>;
  const AFileConfig: TConfigEntryArray;
  const AFileName: string): Goccia.Engine.TGocciaSourceType;
begin
  case ResolveFileSourceType(AOption, AFileConfig, AFileName) of
    fstScript: Result := Goccia.Engine.stScript;
    fstModule: Result := Goccia.Engine.stModule;
  else
    Result := Goccia.Engine.stDetect;
  end;
end;

procedure ApplyFileSettingsToEngine(const AEngine: TGocciaEngine;
  const ASettings: TGocciaFileSettings);
var
  GC: TGarbageCollector;
begin
  case ASettings.SourceType of
    fstScript: AEngine.SourceType := Goccia.Engine.stScript;
    fstModule: AEngine.SourceType := Goccia.Engine.stModule;
  else
    AEngine.SourceType := Goccia.Engine.stDetect;
  end;
  AEngine.Compatibility := ASettings.Compatibility;
  AEngine.LabelStatementsEnabled := ASettings.LabelStatements;
  AEngine.ForInLoopsEnabled := ASettings.ForInLoops;
  AEngine.ExperimentalJSModuleSourceEnabled :=
    ASettings.ExperimentalJSModuleSource;
  AEngine.WarningUnsupportedFeatures := ASettings.WarningUnsupportedFeatures;
  AEngine.StrictTypes := ASettings.StrictTypes;
  AEngine.FunctionConstructor.Enabled := ASettings.FunctionConstructor;
  if ASettings.ShadowRealm then
    EnableShadowRealm(AEngine);
  { Always set explicitly so a previous file's per-file override does not
    leak into subsequent files (GC.MaxBytes is process-global). }
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    if ASettings.MaxMemorySet then
      GC.MaxBytes := ASettings.MaxMemory
    else
      GC.MaxBytes := GC.SuggestedMaxBytes;
  end;
  AEngine.FetchMaxResponseBytes := ASettings.FetchMaxResponseBytes;
end;

procedure ApplyFileConfigToEngine(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray; const AFileName: string;
  const AAcceptedUnsafe: TGocciaUnsafeRequests);
begin
  if not Assigned(AEngineOptions) then
    Exit;
  ApplyFileSettingsToEngine(AEngine, ResolveFileSettings(AEngineOptions,
    AFileConfig, AFileName, AAcceptedUnsafe));
end;

procedure TGocciaCLIEngineApplication.ConfigureCreatedEngine(
  const AEngine: TGocciaEngine; const AFileConfig: TConfigEntryArray);
begin
end;

procedure TGocciaCLIEngineApplication.ConfigureCapabilityAudit(
  const AEngine: TGocciaEngine);
begin
  if AuditLogOpen then
  begin
    AEngine.CapabilityAuditSink := HandleCapabilityAudit;
    AEngine.AuditEffectiveCapabilities;
  end;
end;

procedure InjectModulesFromFileSystemModule(const AEngine: TGocciaEngine;
  const APath: string);
var
  DefaultValue: TGocciaValue;
  ManifestJSON: string;
  ManifestLoader: TGocciaModuleLoader;
  ManifestModule: TGocciaModule;
  Stringifier: TGocciaJSONStringifier;
begin
  ManifestLoader := TGocciaModuleLoader.Create(APath);
  try
    ManifestLoader.SetContentProvider(
      TGocciaFileSystemModuleContentProvider.Create, True);
    ManifestLoader.Preprocessors := AEngine.ModuleLoader.Preprocessors;
    ManifestLoader.Compatibility := AEngine.ModuleLoader.Compatibility;
    ManifestLoader.LabelStatementsEnabled :=
      AEngine.ModuleLoader.LabelStatementsEnabled;
    ManifestLoader.ForInLoopsEnabled := AEngine.ModuleLoader.ForInLoopsEnabled;
    ManifestLoader.ExperimentalJSModuleSourceEnabled :=
      AEngine.ModuleLoader.ExperimentalJSModuleSourceEnabled;
    ManifestLoader.WarningUnsupportedFeatures :=
      AEngine.ModuleLoader.WarningUnsupportedFeatures;
    ManifestLoader.SuppressWarnings := AEngine.ModuleLoader.SuppressWarnings;
    ManifestLoader.StrictTypesEnabled :=
      AEngine.ModuleLoader.StrictTypesEnabled;
    ManifestLoader.EvaluateModuleBody :=
      AEngine.ModuleLoader.EvaluateModuleBody;
    ManifestLoader.InstantiateModuleFunctions :=
      AEngine.ModuleLoader.InstantiateModuleFunctions;
    ManifestLoader.BindRuntime(AEngine.GlobalScope,
      AEngine.ThrowError);

    ManifestModule := ManifestLoader.LoadHostModule(APath, APath);
    if not ManifestModule.TryGetExportValue(KEYWORD_DEFAULT, DefaultValue) then
      raise EArgumentException.Create(
        'Virtual modules manifest module must have a default export.');
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.AddTempRoot(DefaultValue);
    try
      Stringifier := TGocciaJSONStringifier.Create;
      try
        ManifestJSON := Stringifier.Stringify(DefaultValue);
      finally
        Stringifier.Free;
      end;
      AEngine.InjectModulesFromJSON(ManifestJSON, ManifestModule.Path);
    finally
      if (TGarbageCollector.Instance <> nil) then
        TGarbageCollector.Instance.RemoveTempRoot(DefaultValue);
    end;
  finally
    ManifestLoader.Free;
  end;
end;

procedure InjectModulesFromManifestFile(const AEngine: TGocciaEngine;
  const APath: string);
var
  Content, Extension: string;
  JSON5Parser: TGocciaJSON5Parser;
  ParsedValue: TGocciaValue;
  Stringifier: TGocciaJSONStringifier;
begin
  Extension := LowerCase(ExtractFileExt(APath));
  if (Extension = '.js') or (Extension = '.mjs') or
     (Extension = '.ts') then
  begin
    { The manifest is a host file named by the host, so it always loads from
      the host filesystem. An engine whose own provider does not read the
      host (GocciaRunner's sandbox mode) cannot evaluate it in place.
      Evaluated through the engine's own loader, the manifest and its
      imports become host-owned there, so anything it leaves behind (a global function that calls
      import(), say) would import as the host; under an outright read deny
      (--deny-read) it is therefore evaluated in an isolated loader
      too, and a later import made from its code is a guest read the deny
      refuses. }
    if AEngine.Capabilities.DeniesAll(gcRead) or
       not AEngine.ContentProvider.ReadsHostFileSystem then
      InjectModulesFromFileSystemModule(AEngine, APath)
    else
      AEngine.InjectModulesFromModule(APath);
    Exit;
  end;

  Content := ReadUTF8FileText(APath);
  if Extension = EXT_JSON then
  begin
    AEngine.InjectModulesFromJSON(Content, APath);
    Exit;
  end;

  ParsedValue := nil;
  if Extension = EXT_JSON5 then
  begin
    JSON5Parser := TGocciaJSON5Parser.Create;
    try
      ParsedValue := JSON5Parser.Parse(Content);
    finally
      JSON5Parser.Free;
    end;
  end
  else if Extension = EXT_TOML then
    ParsedValue := TOMLTextToValue(Content)
  else if (Extension = '.yaml') or (Extension = '.yml') then
    ParsedValue := YAMLTextToValue(string(Content))
  else
    raise Exception.CreateFmt(
      'Unsupported virtual modules manifest format: %s', [APath]);

  if (TGarbageCollector.Instance <> nil) and Assigned(ParsedValue) then
    TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Stringifier := TGocciaJSONStringifier.Create;
    try
      AEngine.InjectModulesFromJSON(Stringifier.Stringify(ParsedValue), APath);
    finally
      Stringifier.Free;
    end;
  finally
    if (TGarbageCollector.Instance <> nil) and Assigned(ParsedValue) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure InjectInlineModuleDefinition(const AEngine: TGocciaEngine;
  const ADefinition, ABaseAddress: string); forward;

{ ── Inputs a config file names ──────────────────────────────── }

{ A module manifest, a globals file or module, or a host-environment module a
  config file names is the repository's choice, not the user's, so it is read
  under the capability set of the script the config governs (ADR 0122). The
  config's own directory plays the project's part: a file inside it is
  covered as the module graph is, anything else needs a read grant, and a read
  deny refuses it. ASpecifier is the path as the config wrote it, which is all
  the refusal names. }
procedure CheckConfigInputRead(const AEngine: TGocciaEngine;
  const APath, ASpecifier, AConfigPath: string);
var
  CanonicalPath: string;
  InConfigDirectory: Boolean;
begin
  CanonicalPath := CanonicalCapabilityPath(APath);
  InConfigDirectory := IsPathWithinScope(CanonicalPath,
    CanonicalCapabilityPath(ExtractFileDir(AConfigPath)));
  if AEngine.Capabilities.DeniesPath(gcRead, CanonicalPath) then
  begin
    AEngine.EmitCapabilityAudit(gckReadFile, gcdDeny, CanonicalPath,
      'read is denied for this path');
    ThrowPermissionDenied(CapabilityName(gcRead), ASpecifier,
      Format(SSuggestReadDenied, [CanonicalPath]));
  end;
  if InConfigDirectory then
    Exit;
  if not AEngine.Capabilities.AllowsPath(gcRead, CanonicalPath) then
  begin
    AEngine.EmitCapabilityAudit(gckReadFile, gcdDeny, CanonicalPath,
      'the path is outside the config''s directory and no read grant ' +
      'covers it');
    ThrowPermissionDenied(CapabilityName(gcRead), ASpecifier,
      Format(SSuggestReadNotGranted, [CanonicalPath,
        ExtractFileDir(CanonicalPath)]));
  end;
  AEngine.EmitCapabilityAudit(gckReadFile, gcdAllow, CanonicalPath,
    'a read grant covers the path');
end;

function ConfigInputPath(const AWritten, AConfigPath: string): string;
begin
  if IsAbsoluteFilePath(AWritten) then
    Result := ExpandFileName(AWritten)
  else
    Result := ExpandFileName(IncludeTrailingPathDelimiter(
      ExtractFileDir(AConfigPath)) + AWritten);
end;

{ Why AValue, reached at APath inside a globals module's exports, is not data,
  or '' when it and everything under it is: data is what JSON carries as is
  (null, booleans, finite numbers, strings, arrays, and plain objects), so
  nothing is dropped, rewritten, or run on the way into the script. AVisiting
  holds the objects being walked, so a cycle is reported, not followed. AAt
  receives the path of the value that is not data. }
function NonDataReason(const AValue: TGocciaValue; const APath: string;
  const AVisiting: TList<TGocciaObjectValue>; out AAt: string): string;
var
  Key: string;
  I, Count: Integer;
begin
  AAt := APath;
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) or
     (AValue is TGocciaHoleValue) then
    Exit('undefined');
  if (AValue is TGocciaNullLiteralValue) or
     (AValue is TGocciaBooleanLiteralValue) or
     (AValue is TGocciaStringLiteralValue) then
    Exit('');
  if AValue is TGocciaNumberLiteralValue then
  begin
    if TGocciaNumberLiteralValue(AValue).IsNaN or
       TGocciaNumberLiteralValue(AValue).IsInfinity or
       TGocciaNumberLiteralValue(AValue).IsNegativeInfinity then
      Exit('a number JSON cannot hold');
    Exit('');
  end;
  if AValue.IsCallable then
    Exit('a function');
  if AValue is TGocciaSymbolValue then
    Exit('a symbol');
  if AValue is TGocciaBigIntValue then
    Exit('a BigInt');
  { A natively built object is a TGocciaObjectValue and an object literal a
    TGocciaVMLiteralObjectValue; both are plain objects. }
  if not (AValue is TGocciaObjectValue) or
     ((AValue.ClassType <> TGocciaObjectValue) and
      (AValue.ClassType <> TGocciaVMLiteralObjectValue) and
      (AValue.ClassType <> TGocciaArrayValue)) then
    Exit('an object that is not a plain object or array');
  if AVisiting.IndexOf(TGocciaObjectValue(AValue)) >= 0 then
    Exit('a circular reference');
  AVisiting.Add(TGocciaObjectValue(AValue));
  try
    if AValue is TGocciaArrayValue then
    begin
      Count := TGocciaArrayValue(AValue).GetLength;
      for I := 0 to Count - 1 do
      begin
        Result := NonDataReason(TGocciaArrayValue(AValue).GetProperty(
          IntToStr(I)), APath + '[' + IntToStr(I) + ']', AVisiting, AAt);
        if Result <> '' then
          Exit;
      end;
    end
    else
      for Key in TGocciaObjectValue(AValue).GetEnumerablePropertyNames do
      begin
        Result := NonDataReason(TGocciaObjectValue(AValue).GetProperty(Key),
          APath + '.' + Key, AVisiting, AAt);
        if Result <> '' then
          Exit;
      end;
    Result := '';
  finally
    AVisiting.Remove(TGocciaObjectValue(AValue));
  end;
end;

type
  { What crosses back from an isolated module: its default export, or all of
    its named exports as one object. }
  TIsolatedModuleResult = (imrDefaultExport, imrNamedExports);

{ Evaluates the JavaScript or TypeScript module at APath in an engine of its
  own with AEngine's capability set, the config's directory as its project,
  and AEngine's language settings: its imports are guest reads, judged like
  the script's, and whatever it leaves on its global object stays in that
  engine. A throw from it is located exactly as the same module passed with
  --globals is.
  Returns the requested exports as JSON, so only data reaches the script.
  AModulePath receives the module's canonical path. }
function EvaluateIsolatedConfigModule(const AEngine: TGocciaEngine;
  const APath, AConfigPath: string; const AResult: TIsolatedModuleResult;
  out AModulePath: string): string;
var
  Isolated: TGocciaEngine;
  Source: TStringList;
  Module: TGocciaModule;
  ExportValue, ExportedValue: TGocciaValue;
  NamedExports: TGocciaObjectValue;
  ExportName, Reason, ReasonPath: string;
  Visiting: TList<TGocciaObjectValue>;
  Stringifier: TGocciaJSONStringifier;
  FloatingPointState: TGocciaFloatingPointState;
begin
  { Loading the module evaluates it outside Execute, so it needs the
    engine's floating-point environment itself: compiling and running
    NaN or Infinity must not raise a host FPU exception. }
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Source := TStringList.Create;
    try
      Isolated := TGocciaEngine.Create(APath, Source, AEngine.Capabilities);
      try
        Isolated.ProjectRoot := ExtractFileDir(AConfigPath);
        Isolated.ConfigureCapabilityAuditAsChildOf(AEngine);
        Isolated.Preprocessors := AEngine.Preprocessors;
        Isolated.Compatibility := AEngine.Compatibility;
        Isolated.LabelStatementsEnabled := AEngine.LabelStatementsEnabled;
        Isolated.ForInLoopsEnabled := AEngine.ForInLoopsEnabled;
        Isolated.StrictTypes := AEngine.StrictTypes;
        Isolated.SuppressWarnings := AEngine.SuppressWarnings;
        { The filesystem content provider, with every read it makes checked. }
        AttachRuntime(Isolated);
        Module := Isolated.ModuleLoader.LoadModule(APath, APath);
        AModulePath := Module.Path;
        if AResult = imrDefaultExport then
        begin
          if not Module.TryGetExportValue(KEYWORD_DEFAULT, ExportValue) then
            raise EArgumentException.Create(
              'Virtual modules manifest module must have a default export.');
        end
        else
        begin
          NamedExports := TGocciaObjectValue.Create;
          if TGarbageCollector.Instance <> nil then
            TGarbageCollector.Instance.AddTempRoot(NamedExports);
          Visiting := TList<TGocciaObjectValue>.Create;
          try
            try
              for ExportName in Module.GetExportNames do
                if Module.TryGetExportValue(ExportName, ExportedValue) then
                begin
                  { Only data crosses into the script's engine, and all of it:
                    JSON would drop a nested function or rewrite NaN silently. }
                  Reason := NonDataReason(ExportedValue, ExportName, Visiting,
                    ReasonPath);
                  if Reason <> '' then
                    raise EArgumentException.CreateFmt(
                      '%s: export "%s" is %s; a config''s globals module may ' +
                      'export data only (pass it with --globals on the ' +
                      'command line to inject code)',
                      [APath, ReasonPath, Reason]);
                  NamedExports.SetProperty(ExportName, ExportedValue);
                end;
            except
              { The stringify step's finally removes the root on success; a
                refused export must not leave it pinning the copied values. }
              if TGarbageCollector.Instance <> nil then
                TGarbageCollector.Instance.RemoveTempRoot(NamedExports);
              raise;
            end;
          finally
            Visiting.Free;
          end;
          ExportValue := NamedExports;
        end;
        if TGarbageCollector.Instance <> nil then
          TGarbageCollector.Instance.AddTempRoot(ExportValue);
        try
          Stringifier := TGocciaJSONStringifier.Create;
          try
            Result := Stringifier.Stringify(ExportValue);
          finally
            Stringifier.Free;
          end;
        finally
          if TGarbageCollector.Instance <> nil then
          begin
            TGarbageCollector.Instance.RemoveTempRoot(ExportValue);
            if AResult = imrNamedExports then
              TGarbageCollector.Instance.RemoveTempRoot(NamedExports);
          end;
        end;
      finally
        Isolated.Free;
      end;
    finally
      Source.Free;
    end;
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

function IsScriptModuleFile(const APath: string): Boolean;
var
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(APath));
  Result := (Extension = EXT_JS) or (Extension = EXT_MJS) or
    (Extension = EXT_TS);
end;

{ A manifest path from a config file's "modules" key. }
procedure InjectConfiguredManifest(const AEngine: TGocciaEngine;
  const APath, ASpecifier, AConfigPath: string);
var
  ModulePath: string;
  ManifestJSON: string;
begin
  CheckConfigInputRead(AEngine, APath, ASpecifier, AConfigPath);
  if IsScriptModuleFile(APath) then
  begin
    ManifestJSON := EvaluateIsolatedConfigModule(AEngine, APath, AConfigPath,
      imrDefaultExport, ModulePath);
    AEngine.InjectModulesFromJSON(ManifestJSON, ModulePath);
  end
  else
    InjectModulesFromManifestFile(AEngine, APath);
end;

procedure InjectConfiguredGlobals(const AEngine: TGocciaEngine;
  const AWritten, AConfigPath: string);
var
  Path, ModulePath: string;
begin
  Path := ConfigInputPath(AWritten, AConfigPath);
  CheckConfigInputRead(AEngine, Path, AWritten, AConfigPath);
  if IsStructuredGlobalsFile(Path) then
  begin
    if IsYAMLGlobalsFile(Path) then
      AEngine.InjectGlobalsFromYAML(ReadFileText(Path))
    else if IsJSON5GlobalsFile(Path) then
      AEngine.InjectGlobalsFromJSON5(ReadFileText(Path))
    else if IsTOMLGlobalsFile(Path) then
      AEngine.InjectGlobalsFromTOML(ReadFileText(Path))
    else
      AEngine.InjectGlobalsFromJSON(ReadFileText(Path));
  end
  else
    AEngine.InjectGlobalsFromJSON(EvaluateIsolatedConfigModule(AEngine, Path,
      AConfigPath, imrNamedExports, ModulePath));
end;

procedure ConfigureConfiguredHostEnvironment(const AEngine: TGocciaEngine;
  const AWritten, AConfigPath: string);
var
  Path, ScriptProjectRoot: string;
  Module: TGocciaModule;
begin
  Path := ConfigInputPath(AWritten, AConfigPath);
  CheckConfigInputRead(AEngine, Path, AWritten, AConfigPath);
  { The providers are functions the engine calls for as long as it runs, so
    they cannot cross from another engine as data. The module is a guest
    module of the script's own engine instead: never host-owned, so its
    imports, now and later, are guest reads under the script's capability
    set, with the config's directory as its project while it loads. }
  ScriptProjectRoot := AEngine.ProjectRoot;
  AEngine.ProjectRoot := ExtractFileDir(AConfigPath);
  try
    Module := AEngine.ModuleLoader.LoadModule(Path, AEngine.SourcePath);
  finally
    AEngine.ProjectRoot := ScriptProjectRoot;
  end;
  ConfigureHostEnvironmentFromLoadedModule(AEngine, Module);
end;

procedure InjectModulesFromConfigFile(const AEngine: TGocciaEngine;
  const APath: string; const ADepth: Integer = 0);
var
  Content, Extension, ExtendsPath, ItemPath, Written: string;
  I: Integer;
  ArrayValue: TGocciaArrayValue;
  ModulesValue, ParsedValue: TGocciaValue;
  ObjectValue: TGocciaObjectValue;
  JSONParser: TGocciaJSONParser;
  JSON5Parser: TGocciaJSON5Parser;
  Stringifier: TGocciaJSONStringifier;
begin
  if (APath = '') or not FileExists(APath) then
    Exit;
  if ADepth > 32 then
    raise Exception.Create(
      'Circular or too-deeply-nested extends in virtual module config: ' +
      APath);
  Content := ReadUTF8FileText(APath);
  Extension := LowerCase(ExtractFileExt(APath));
  ParsedValue := nil;
  if Extension = EXT_JSON then
  begin
    JSONParser := TGocciaJSONParser.Create;
    try
      ParsedValue := JSONParser.Parse(Content);
    finally
      JSONParser.Free;
    end;
  end
  else if Extension = EXT_JSON5 then
  begin
    JSON5Parser := TGocciaJSON5Parser.Create;
    try
      ParsedValue := JSON5Parser.Parse(Content);
    finally
      JSON5Parser.Free;
    end;
  end
  else if Extension = EXT_TOML then
    ParsedValue := TOMLTextToValue(Content);

  if not (ParsedValue is TGocciaObjectValue) then
    Exit;
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    ObjectValue := TGocciaObjectValue(ParsedValue);
    if ObjectValue.HasOwnProperty('extends') and
       (ObjectValue.GetProperty('extends') is TGocciaStringLiteralValue) then
    begin
      ExtendsPath := TGocciaStringLiteralValue(
        ObjectValue.GetProperty('extends')).Value;
      if not IsAbsoluteFilePath(ExtendsPath) then
        ExtendsPath := ExpandFileName(
          IncludeTrailingPathDelimiter(ExtractFilePath(APath)) + ExtendsPath);
      InjectModulesFromConfigFile(AEngine, ExtendsPath, ADepth + 1);
    end;

    if ObjectValue.HasOwnProperty('modules') then
    begin
      ModulesValue := ObjectValue.GetProperty('modules');
      if ModulesValue is TGocciaStringLiteralValue then
      begin
        ItemPath := TGocciaStringLiteralValue(ModulesValue).Value;
        Written := ItemPath;
        if not IsAbsoluteFilePath(ItemPath) then
          ItemPath := ExpandFileName(
            IncludeTrailingPathDelimiter(ExtractFilePath(APath)) + ItemPath);
        InjectConfiguredManifest(AEngine, ItemPath, Written, APath);
      end
      else if ModulesValue is TGocciaArrayValue then
      begin
        ArrayValue := TGocciaArrayValue(ModulesValue);
        for I := 0 to ArrayValue.GetLength - 1 do
        begin
          ModulesValue := ArrayValue.GetElement(I);
          if not (ModulesValue is TGocciaStringLiteralValue) then
            raise EArgumentException.Create(
              'Config modules manifest paths must be strings.');
          ItemPath := TGocciaStringLiteralValue(ModulesValue).Value;
          Written := ItemPath;
          if not IsAbsoluteFilePath(ItemPath) then
            ItemPath := ExpandFileName(
              IncludeTrailingPathDelimiter(ExtractFilePath(APath)) +
              ItemPath);
          InjectConfiguredManifest(AEngine, ItemPath, Written, APath);
        end;
      end
      else if ModulesValue is TGocciaObjectValue then
      begin
        Stringifier := TGocciaJSONStringifier.Create;
        try
          AEngine.InjectModulesFromJSON(Stringifier.Stringify(ModulesValue),
            APath);
        finally
          Stringifier.Free;
        end;
      end
      else
        raise EArgumentException.Create(
          'Config modules must be a descriptor object, path, or path array.');
    end;

    if ObjectValue.HasOwnProperty('module') then
    begin
      ModulesValue := ObjectValue.GetProperty('module');
      if ModulesValue is TGocciaStringLiteralValue then
        InjectInlineModuleDefinition(AEngine,
          TGocciaStringLiteralValue(ModulesValue).Value, APath)
      else if ModulesValue is TGocciaArrayValue then
      begin
        ArrayValue := TGocciaArrayValue(ModulesValue);
        for I := 0 to ArrayValue.GetLength - 1 do
        begin
          ModulesValue := ArrayValue.GetElement(I);
          if not (ModulesValue is TGocciaStringLiteralValue) then
            raise EArgumentException.Create(
              'Config module definitions must be strings.');
          InjectInlineModuleDefinition(AEngine,
            TGocciaStringLiteralValue(ModulesValue).Value, APath);
        end;
      end
      else
        raise EArgumentException.Create(
          'Config module must be a definition string or string array.');
    end;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

procedure InjectInlineModuleDefinition(const AEngine: TGocciaEngine;
  const ADefinition, ABaseAddress: string);
var
  Address, Content: string;
  Separator: NativeInt;
begin
  Separator := Pos('=', ADefinition);
  if Separator <= 1 then
    raise Exception.CreateFmt(
      'Invalid --module value "%s"; expected name=source or name={descriptor}.',
      [ADefinition]);
  Address := Copy(ADefinition, 1, Separator - 1);
  Content := Copy(ADefinition, Separator + 1, MaxInt);
  if (Content <> '') and (Content[1] = '{') then
    AEngine.InjectModulesFromJSON('{"' + EscapeJSONString(Address) + '":' +
      Content + '}', ABaseAddress)
  else
    AEngine.InjectModule(Address, Content, 'javascript', ABaseAddress);
end;

procedure ApplyConfiguredVirtualModules(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions; const ARootConfigPath,
  AFileConfigPath: string);
var
  ManifestPath: string;

  procedure ApplyManifests;
  var
    ManifestIndex: Integer;
  begin
    for ManifestIndex := 0 to
      AEngineOptions.ModuleManifests.Values.Count - 1 do
    begin
      ManifestPath :=
        AEngineOptions.ModuleManifests.Values[ManifestIndex];
      if not IsAbsoluteFilePath(ManifestPath) then
        ManifestPath := ExpandFileName(ManifestPath);
      InjectModulesFromManifestFile(AEngine, ManifestPath);
    end;
  end;

  procedure ApplyDefinitions;
  var
    DefinitionIndex: Integer;
  begin
    for DefinitionIndex := 0 to
      AEngineOptions.ModuleDefinitions.Values.Count - 1 do
      InjectInlineModuleDefinition(AEngine,
        AEngineOptions.ModuleDefinitions.Values[DefinitionIndex],
        GetCurrentDir);
  end;
begin
  InjectModulesFromConfigFile(AEngine, ARootConfigPath);

  if (AFileConfigPath <> '') and
     (ExpandFileName(AFileConfigPath) <> ExpandFileName(ARootConfigPath)) then
    InjectModulesFromConfigFile(AEngine, AFileConfigPath);

  if not Assigned(AEngineOptions) then
    Exit;
  if AEngineOptions.ModuleManifests.FromCommandLine then
    ApplyManifests;
  if AEngineOptions.ModuleDefinitions.FromCommandLine then
    ApplyDefinitions;
end;

procedure TGocciaCLIEngineApplication.ApplyCommandLineVirtualModulesToEngine(
  const AEngine: TGocciaEngine);
begin
  ApplyConfiguredVirtualModules(AEngine, EngineOptions, '', '');
end;

procedure TGocciaCLIEngineApplication.ApplyVirtualModulesToEngine(
  const AEngine: TGocciaEngine; const AFileConfigPath: string);
begin
  ApplyConfiguredVirtualModules(AEngine, EngineOptions, RootConfigPath,
    AFileConfigPath);
end;

function TGocciaCLIEngineApplication.CreateEngine(const AFileName: string;
  const ASource: TStringList): TGocciaEngine;
var
  AliasBaseDirectory: string;
  FileConfig: TConfigEntryArray;
  FileConfigPath: string;
  Verdict: TGocciaConfigTrustVerdict;
begin
  FileConfigPath := DiscoverFileConfigPath(AFileName);
  if FileConfigPath <> '' then
    FileConfig := LoadFileConfig(FileConfigPath)
  else
    SetLength(FileConfig, 0);
  Verdict := FileConfigVerdict(FileConfigPath, AFileName);
  { The capability set is fixed when the engine is created (ADR 0122). }
  Result := TGocciaEngine.Create(AFileName, ASource,
    VerdictCapabilities(Verdict));
  try
    Result.CapabilityProvenance := CapabilityProvenance(Verdict);
    ConfigureCapabilityAudit(Result);
    if Assigned(EngineOptions) then
    begin
      if EngineOptions.Aliases.FromCommandLine or
         (RootConfigPath = '') then
        AliasBaseDirectory := GetCurrentDir
      else
        AliasBaseDirectory := ExtractFilePath(RootConfigPath);
      Result.Resolver.ProviderCachedOnly := ResolveFlagOption(
        EngineOptions.CachedOnly, FileConfig);
      ConfigureModuleResolver(Result.Resolver, AFileName,
        EngineOptions.ImportMap.ValueOr(''), EngineOptions.Aliases.Values,
        AliasBaseDirectory);
      if ResolveFlagOption(EngineOptions.Deterministic, FileConfig) then
        Result.HostEnvironment.UseDeterministicProfile;
    end;
    ConfigureCreatedEngine(Result, FileConfig);
    if Assigned(EngineOptions) then
      { One config per file: a file with its own config takes its unsafe-*
        keys from that config (and its extends chain) alone; the root config
        fills in only for a file without one, inside its tree. }
      ApplyFileConfigToEngine(Result, EngineOptions, FileConfig, AFileName,
        Verdict.AcceptedUnsafe);
    ApplyVirtualModulesToEngine(Result, FileConfigPath);
    { A detected entry is configured as a Script; the engine keeps the top
      level of one that turns out a Module off the global object. }
    Result.GlobalBackedTopLevel :=
      Result.SourceType <> Goccia.Engine.stModule;
  except
    Result.Free;
    raise;
  end;
end;

end.
