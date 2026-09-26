unit Goccia.CLI.SandboxHost;

{ Runs a program inside a sandbox virtual filesystem for GocciaRunner's
  sandbox mode (ADR 0122): owns the top-level sandbox context, builds and
  runs an engine over it, nests `runScript` / `goccia` children, classifies
  how a run ended, and renders the filesystem diff.

  What an engine may do is not decided here. The application configures each
  engine it creates through OnConfigureEngine (runtime, audit, host
  environment, file-level options), and gives the root engine's capability
  set through RootCapabilities; a nested child inherits its parent's set, so
  a child never reaches more than its parent. }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  SandboxHostInputs,

  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.HostEnvironment,
  Goccia.Sandbox.Context,
  Goccia.Sandbox.Modules,
  Goccia.Values.Primitives;

const
  { Each nesting level holds an engine, a realm, and a virtual filesystem
    alive on the native stack, so the depth a guest may ask for is a resource
    ceiling like the memory budget, not a taste judgement about how deep
    orchestration should go. }
  MAX_RUN_SCRIPT_DEPTH = 32;

type
  { Configures an engine the host just created over AContext to run
    AEntryPath. AParentEngine and AParentHostEnvironment are nil for the root
    run and the calling engine's for a nested one. }
  TGocciaSandboxEngineConfigurator = procedure(const AEngine: TGocciaEngine;
    const AContext: TGocciaSandboxContext; const AEntryPath: string;
    const AParentEngine: TGocciaEngine;
    const AParentHostEnvironment: TGocciaHostEnvironment) of object;

  TGocciaSandboxHost = class
  private
    FContext: TGocciaSandboxContext;
    FInputs: TSandboxHostInputs;
    FBytecode: Boolean;
    FTimeoutMilliseconds: Integer;
    FMaxInstructions: Int64;
    FImportMapPath: string;
    FAliases: TStringList;
    FRootCapabilities: TGocciaCapabilities;
    FOnConfigureEngine: TGocciaSandboxEngineConfigurator;
    FRunScriptDepth: Integer;
    FCurrentOutputLines: TStrings;
    FCurrentHostEnvironment: TGocciaHostEnvironment;
    FCurrentCapabilities: TGocciaCapabilities;
    FHasCurrentCapabilities: Boolean;
    FCurrentEngine: TGocciaEngine;

    procedure ConfigureResolver(const AResolver: TGocciaSandboxModuleResolver);
    procedure CopyNestedInputs(const AParentContext,
      AChildContext: TGocciaSandboxContext;
      const AOptions: TGocciaSandboxRunOptions);
    function ExecuteInContext(const AContext: TGocciaSandboxContext;
      const AEntryPath: string): TGocciaSandboxRunResult;
    function ExecuteNested(const AContext: TGocciaSandboxContext;
      const AEntryPath: string; const AOptions: TGocciaSandboxRunOptions):
      TGocciaSandboxRunResult;
  public
    constructor Create(const AQuotaBytes: Int64; const ANodeQuota: Integer);
    destructor Destroy; override;

    { Copies a host input into the sandbox; see TSandboxHostInputs.CopyIn. }
    function CopyIn(const AHostPath, ASandboxPath: string;
      const AReadWrite: Boolean): string;
    { Records the filesystem as the run starts, for the diff and the
      write-back. Call after the inputs are copied. }
    procedure CaptureBaseline;
    { Runs AEntryPath, a sandbox path, as the root program. A failure is
      returned, classified, never raised. }
    function Run(const AEntryPath: string): TGocciaSandboxRunResult;
    { The changes since CaptureBaseline: JSON always with timestamp metadata,
      unified never. }
    function DiffText(const AUnified: Boolean): string;
    { For the console output callback OnConfigureEngine installs. }
    procedure CaptureConsoleLine(const AMethod, ALine: string);

    property Context: TGocciaSandboxContext read FContext;
    property Inputs: TSandboxHostInputs read FInputs;
    property Bytecode: Boolean read FBytecode write FBytecode;
    property TimeoutMilliseconds: Integer read FTimeoutMilliseconds
      write FTimeoutMilliseconds;
    property MaxInstructions: Int64 read FMaxInstructions
      write FMaxInstructions;
    property ImportMapPath: string read FImportMapPath write FImportMapPath;
    { `key=value` module aliases. }
    property Aliases: TStringList read FAliases;
    property RootCapabilities: TGocciaCapabilities read FRootCapabilities
      write FRootCapabilities;
    property OnConfigureEngine: TGocciaSandboxEngineConfigurator
      read FOnConfigureEngine write FOnConfigureEngine;
  end;

implementation

uses
  SandboxVirtualFileSystem,
  TextSemantics,

  Goccia.CapabilityAudit,
  Goccia.Diagnostics.SourceRegistry,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Realm,
  Goccia.Timeout,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.VM.Exception;

const
  SANDBOX_ROOT = '/';

function EnsureSandboxAbsolute(const APath: string): string;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  if Path = '' then
    Exit(SANDBOX_ROOT);
  if Path[1] = SANDBOX_ROOT then
    Result := Path
  else
    Result := SANDBOX_ROOT + Path;
end;

function SandboxPathParent(const APath: string): string;
var
  Path: string;
  SlashIndex: Integer;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := SANDBOX_ROOT;
  SlashIndex := Length(Path);
  while (SlashIndex > 1) and (Path[SlashIndex] <> SANDBOX_ROOT) do
    Dec(SlashIndex);
  if SlashIndex > 1 then
    Result := Copy(Path, 1, SlashIndex - 1);
end;

function SandboxPathHasTrailingSeparator(const APath: string): Boolean;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := (Path <> '') and (Path[Length(Path)] = SANDBOX_ROOT);
end;

function SandboxJoinPath(const ABase, AName: string): string;
var
  Base: string;
begin
  Base := NormalizeSandboxPathSeparators(ABase);
  if Base = SANDBOX_ROOT then
    Result := SANDBOX_ROOT + AName
  else
    Result := Base + SANDBOX_ROOT + AName;
end;

procedure ImportVirtualFile(const ASourceFs,
  ADestinationFs: TSandboxVirtualFileSystem; const ASourcePath,
  ADestinationPath: string; const ADestinationIsDirectory: Boolean);
var
  SourceStat: TSandboxFsStat;
  TargetPath: string;
begin
  SourceStat := ASourceFs.Stat(ASourcePath);
  TargetPath := ADestinationFs.Normalize(EnsureSandboxAbsolute(
    ADestinationPath));
  if ADestinationIsDirectory or (TargetPath = SANDBOX_ROOT) or
     SandboxPathHasTrailingSeparator(ADestinationPath) or
     ADestinationFs.IsDirectory(TargetPath) then
    TargetPath := ADestinationFs.Normalize(SandboxJoinPath(TargetPath,
      SourceStat.Name));
  ADestinationFs.MakeDirectory(SandboxPathParent(TargetPath), True);
  ADestinationFs.WriteAllBytes(TargetPath, ASourceFs.ReadAllBytes(
    ASourcePath));
end;

procedure ImportVirtualDirectoryContents(const ASourceFs,
  ADestinationFs: TSandboxVirtualFileSystem; const ASourceDirectory,
  ADestinationDirectory: string);
var
  Entries: TSandboxFsStatArray;
  TargetDirectory: string;
  TargetChild: string;
  I: Integer;
begin
  TargetDirectory := ADestinationFs.Normalize(EnsureSandboxAbsolute(
    ADestinationDirectory));
  ADestinationFs.MakeDirectory(TargetDirectory, True);
  Entries := ASourceFs.List(ASourceDirectory);
  for I := 0 to High(Entries) do
  begin
    TargetChild := SandboxJoinPath(TargetDirectory, Entries[I].Name);
    if Entries[I].Kind = nkDirectory then
      ImportVirtualDirectoryContents(ASourceFs, ADestinationFs,
        Entries[I].Path, TargetChild)
    else
      ImportVirtualFile(ASourceFs, ADestinationFs, Entries[I].Path,
        TargetChild, False);
  end;
end;

procedure ImportVirtualPath(const ASourceFs,
  ADestinationFs: TSandboxVirtualFileSystem; const ASourcePath,
  ADestinationPath: string; const ADestinationIsDirectory: Boolean);
var
  SourceStat: TSandboxFsStat;
  TargetPath: string;
begin
  SourceStat := ASourceFs.Stat(ASourcePath);
  TargetPath := ADestinationFs.Normalize(EnsureSandboxAbsolute(
    ADestinationPath));
  if SourceStat.Kind = nkDirectory then
    ImportVirtualDirectoryContents(ASourceFs, ADestinationFs, ASourcePath,
      TargetPath)
  else
    ImportVirtualFile(ASourceFs, ADestinationFs, ASourcePath, TargetPath,
      ADestinationIsDirectory);
end;

function CloneResultValueRecursive(const AValue: TGocciaValue;
  const ASeen: TList): TGocciaValue;
var
  ArrayValue, ClonedArray: TGocciaArrayValue;
  ObjectValue, ClonedObject: TGocciaObjectValue;
  Key: string;
  I: Integer;
begin
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if AValue is TGocciaNullLiteralValue then
    Exit(TGocciaNullLiteralValue.NullValue);
  if AValue is TGocciaBooleanLiteralValue then
    Exit(TGocciaBooleanLiteralValue.Create(
      TGocciaBooleanLiteralValue(AValue).Value));
  if AValue is TGocciaNumberLiteralValue then
    Exit(TGocciaNumberLiteralValue.Create(
      TGocciaNumberLiteralValue(AValue).Value));
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue.Create(
      TGocciaStringLiteralValue(AValue).Value));

  if ASeen.IndexOf(AValue) >= 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if AValue is TGocciaArrayValue then
  begin
    ASeen.Add(AValue);
    try
      ArrayValue := TGocciaArrayValue(AValue);
      ClonedArray := TGocciaArrayValue.Create(nil, ArrayValue.GetLength);
      for I := 0 to ArrayValue.GetLength - 1 do
        ClonedArray.SetElement(I,
          CloneResultValueRecursive(ArrayValue.GetElement(I), ASeen));
      Exit(ClonedArray);
    finally
      ASeen.Remove(AValue);
    end;
  end;

  if AValue is TGocciaObjectValue then
  begin
    ASeen.Add(AValue);
    try
      ObjectValue := TGocciaObjectValue(AValue);
      ClonedObject := TGocciaObjectValue.Create(
        TGocciaObjectValue.SharedObjectPrototype, 8);
      for Key in ObjectValue.GetOwnPropertyKeys do
        ClonedObject.SetProperty(Key,
          CloneResultValueRecursive(ObjectValue.GetProperty(Key), ASeen));
      Exit(ClonedObject);
    finally
      ASeen.Remove(AValue);
    end;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ A copy of a run's result that outlives the engine that produced it:
  primitives, arrays, and plain own data properties, with cycles cut. }
function CloneResultValue(const AValue: TGocciaValue): TGocciaValue;
var
  Seen: TList;
begin
  Seen := TList.Create;
  try
    Result := CloneResultValueRecursive(AValue, Seen);
  finally
    Seen.Free;
  end;
end;

{ TGocciaSandboxHost }

constructor TGocciaSandboxHost.Create(const AQuotaBytes: Int64;
  const ANodeQuota: Integer);
begin
  inherited Create;
  FContext := TGocciaSandboxContext.Create(AQuotaBytes, ANodeQuota);
  FContext.RunScriptCallback := ExecuteNested;
  FInputs := TSandboxHostInputs.Create(FContext.Fs);
  FAliases := TStringList.Create;
  FRootCapabilities := TGocciaCapabilities.None;
end;

destructor TGocciaSandboxHost.Destroy;
begin
  FAliases.Free;
  FInputs.Free;
  FContext.Free;
  inherited Destroy;
end;

function TGocciaSandboxHost.CopyIn(const AHostPath, ASandboxPath: string;
  const AReadWrite: Boolean): string;
begin
  Result := FInputs.CopyIn(AHostPath, ASandboxPath, AReadWrite);
end;

procedure TGocciaSandboxHost.CaptureBaseline;
begin
  FContext.CaptureBaseline;
end;

function TGocciaSandboxHost.DiffText(const AUnified: Boolean): string;
begin
  if AUnified then
    Result := FContext.DiffUnified(False)
  else
    Result := FContext.DiffJson(True);
end;

procedure TGocciaSandboxHost.CaptureConsoleLine(const AMethod, ALine: string);
begin
  if Assigned(FCurrentOutputLines) then
    FCurrentOutputLines.Add(ALine);
end;

procedure TGocciaSandboxHost.ConfigureResolver(
  const AResolver: TGocciaSandboxModuleResolver);
var
  AliasSpec, AliasKey, AliasValue: string;
  I, SeparatorIndex: Integer;
begin
  if not Assigned(AResolver) then
    Exit;

  if FImportMapPath <> '' then
    AResolver.LoadImportMap(FImportMapPath);

  for I := 0 to FAliases.Count - 1 do
  begin
    AliasSpec := FAliases[I];
    SeparatorIndex := Pos('=', AliasSpec);
    if SeparatorIndex <= 1 then
      raise Exception.Create('Invalid --alias argument. Use --alias key=value.');

    AliasKey := Copy(AliasSpec, 1, SeparatorIndex - 1);
    AliasValue := Copy(AliasSpec, SeparatorIndex + 1, MaxInt);
    if AliasValue = '' then
      raise Exception.Create('Invalid --alias argument. Use --alias key=value.');

    AResolver.AddAlias(AliasKey, AliasValue);
  end;
end;

procedure TGocciaSandboxHost.CopyNestedInputs(const AParentContext,
  AChildContext: TGocciaSandboxContext;
  const AOptions: TGocciaSandboxRunOptions);
var
  CopySpec: TGocciaSandboxCopySpec;
begin
  for CopySpec in AOptions.Copies do
  begin
    case CopySpec.Kind of
      sckParentPath:
        ImportVirtualPath(AParentContext.Fs, AChildContext.Fs,
          CopySpec.FromPath, CopySpec.ToPath, CopySpec.ToDirectory);
      sckText:
      begin
        AChildContext.Fs.MakeDirectory(SandboxPathParent(CopySpec.Path),
          True);
        AChildContext.Fs.WriteAllText(CopySpec.Path, CopySpec.Text);
      end;
      sckBytes:
      begin
        AChildContext.Fs.MakeDirectory(SandboxPathParent(CopySpec.Path),
          True);
        AChildContext.Fs.WriteAllBytes(CopySpec.Path, CopySpec.Bytes);
      end;
    end;
  end;
end;

function TGocciaSandboxHost.Run(const AEntryPath: string):
  TGocciaSandboxRunResult;
begin
  Result := ExecuteInContext(FContext, FContext.Fs.Normalize(AEntryPath));
end;

function TGocciaSandboxHost.ExecuteInContext(
  const AContext: TGocciaSandboxContext; const AEntryPath: string):
  TGocciaSandboxRunResult;
var
  Source: TStringList;
  OutputLines: TStringList;
  Executor: TGocciaExecutor;
  BytecodeExecutor: TGocciaBytecodeExecutor;
  Resolver: TGocciaSandboxModuleResolver;
  Provider: TGocciaSandboxModuleContentProvider;
  Engine: TGocciaEngine;
  ScriptResult: TGocciaScriptResult;
  CloneRealm, ExecutionRealm: TGocciaRealm;
  PreviousOutputLines: TStrings;
  PreviousHostEnvironment: TGocciaHostEnvironment;
  PreviousCapabilities: TGocciaCapabilities;
  PreviousHasCapabilities: Boolean;
  PreviousEngine: TGocciaEngine;
  EngineCapabilities: TGocciaCapabilities;
  RenderScope: TGocciaDiagnosticSourceScope;
  ExpectedPrincipal: Int64;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Ok := False;
  Result.ExitCode := 1;
  { Only a fall-through default: every path below assigns a kind, so this
    stands for "the runner returned without classifying", which is a runner
    defect by definition. }
  Result.FailureKind := sfkHostError;

  if not AContext.Fs.IsFile(AEntryPath) then
  begin
    { The entry path is an argument, and for a nested run the guest chose
      it — sfkHostError here would let the guest name the runner as the
      party at fault by passing a path that does not exist. }
    Result.ErrorMessage := 'sandbox entry file not found: ' + AEntryPath;
    Result.FailureKind := sfkScriptError;
    Exit;
  end;

  Source := CreateFileTextLines(AContext.Fs.SnapshotReadAllText(AEntryPath));
  OutputLines := TStringList.Create;
  Resolver := TGocciaSandboxModuleResolver.Create(AContext.Fs, SANDBOX_ROOT);
  Provider := TGocciaSandboxModuleContentProvider.Create(AContext.Fs);
  Engine := nil;
  Executor := nil;
  CloneRealm := CurrentRealm;
  PreviousOutputLines := FCurrentOutputLines;
  PreviousHostEnvironment := FCurrentHostEnvironment;
  PreviousCapabilities := FCurrentCapabilities;
  PreviousHasCapabilities := FHasCurrentCapabilities;
  PreviousEngine := FCurrentEngine;
  FCurrentOutputLines := OutputLines;
  try
    try
      ConfigureResolver(Resolver);

      { The root run's set comes from the application; a nested run
        inherits its parent's. }
      if FHasCurrentCapabilities then
        EngineCapabilities := FCurrentCapabilities
      else
        EngineCapabilities := FRootCapabilities;

      if FBytecode then
      begin
        BytecodeExecutor := TGocciaBytecodeExecutor.Create;
        BytecodeExecutor.GlobalBackedTopLevel := True;
        Executor := BytecodeExecutor;
      end
      else
        Executor := TGocciaInterpreterExecutor.Create;

      Engine := TGocciaEngine.Create(AEntryPath, Source, Resolver, Executor,
        EngineCapabilities);
      FCurrentCapabilities := Engine.Capabilities;
      FHasCurrentCapabilities := True;
      Engine.ModuleLoader.SetContentProvider(Provider, True);
      Provider := nil;
      if Assigned(FOnConfigureEngine) then
        FOnConfigureEngine(Engine, AContext, AEntryPath, PreviousEngine,
          PreviousHostEnvironment);
      FCurrentHostEnvironment := Engine.HostEnvironment;
      FCurrentEngine := Engine;

      { The recipient owns render authorization. A top-level run explicitly
        authorizes the engine it just created. During nested runScript, the
        parent scope is active before the child transition, so the returned
        error string is authorized only for the parent and the child's
        excerpt is withheld. Engine.Execute restores that same scope before
        its exception reaches the formatter below. }
      RenderScope := TGocciaDiagnosticSourceRegistry.Current;
      if Assigned(RenderScope) then
        ExpectedPrincipal := RenderScope.Principal
      else
        ExpectedPrincipal := Engine.ModuleLoader.DiagnosticScope.Principal;

      try
        PushTimeoutScope(tsFile, FTimeoutMilliseconds);
        PushInstructionLimitScope(FMaxInstructions);
        ScriptResult := Engine.Execute;
        ExecutionRealm := CurrentRealm;
        try
          SetCurrentRealm(CloneRealm);
          Result.ResultValue := CloneResultValue(ScriptResult.Result);
        finally
          SetCurrentRealm(ExecutionRealm);
        end;
        Result.Ok := True;
        Result.ExitCode := 0;
        Result.FailureKind := sfkNone;
      finally
        PopTimeoutScope;
        PopInstructionLimitScope;
      end;
    except
      on E: EGocciaCapabilityAuditDeliveryError do
        raise;
      { Each branch classifies as well as formats. The order is what does the
        classifying: every kind the guest can steer is named ahead of the
        generic Exception branch, so a ceiling is reported as the ceiling it
        is and a guest throw as the guest's, rather than either being folded
        into "some native error happened". }
      on E: TGocciaMemoryLimitError do
      begin
        Result.ErrorMessage := 'memory limit exceeded: ' + E.Message;
        Result.FailureKind := sfkResourceLimit;
      end;
      on E: TGocciaInstructionLimitError do
      begin
        Result.ErrorMessage := E.Message;
        Result.FailureKind := sfkResourceLimit;
      end;
      { Reached from a nested isolated runScript, which checks the inherited
        quota before it builds the child context and raises out through the
        calling guest rather than returning a result; this frame is where
        that lands. }
      on E: ESandboxFsQuotaExceeded do
      begin
        Result.ErrorMessage := E.Message;
        Result.FailureKind := sfkResourceLimit;
      end;
      on E: EGocciaSandboxNestingLimitExceeded do
      begin
        Result.ErrorMessage := E.Message;
        Result.FailureKind := sfkResourceLimit;
      end;
      on E: TGocciaTimeoutError do
      begin
        Result.ErrorMessage := E.Message;
        Result.FailureKind := sfkTimeout;
      end;
      on E: TGocciaError do
      begin
        Result.ErrorMessage := E.GetDetailedMessage(False);
        Result.FailureKind := sfkScriptError;
      end;
      on E: TGocciaThrowValue do
      begin
        Result.ErrorMessage := FormatThrowDetail(E.Value, AEntryPath, Source,
          False, ExpectedPrincipal, E.Suggestion, True,
          E.SuggestionIsHostOnly);
        Result.FailureKind := sfkScriptError;
      end;
      { The same guest throw, as the bytecode VM delivers it. Without this
        branch a bytecode run reported an uncaught throw as a host fault and
        printed the bare message where the interpreter printed the frame —
        the guest picking both the classification and the format. }
      on E: EGocciaBytecodeThrow do
      begin
        Result.ErrorMessage := FormatThrowDetail(E.ThrownValue, AEntryPath,
          Source, False, ExpectedPrincipal, E.Suggestion, True,
          E.SuggestionIsHostOnly);
        Result.FailureKind := sfkScriptError;
      end;
      { Whatever is left is a native error the engine does not model. Every
        failure the guest can steer is named above, so reaching here means
        the runner malfunctioned; if a guest-reachable condition ever lands
        here it belongs in a branch of its own rather than in this one. }
      on E: Exception do
      begin
        Result.ErrorMessage := E.Message;
        Result.FailureKind := sfkHostError;
      end;
    end;

    Result.Output := OutputLines.Text;
    if Result.ErrorMessage <> '' then
      Result.ErrorOutput := Result.ErrorMessage + sLineBreak;
  finally
    Engine.Free;
    Executor.Free;
    Resolver.Free;
    Provider.Free;
    FCurrentHostEnvironment := PreviousHostEnvironment;
    FCurrentCapabilities := PreviousCapabilities;
    FHasCurrentCapabilities := PreviousHasCapabilities;
    FCurrentEngine := PreviousEngine;
    FCurrentOutputLines := PreviousOutputLines;
    OutputLines.Free;
    Source.Free;
  end;
end;

function TGocciaSandboxHost.ExecuteNested(
  const AContext: TGocciaSandboxContext; const AEntryPath: string;
  const AOptions: TGocciaSandboxRunOptions): TGocciaSandboxRunResult;
var
  ChildContext: TGocciaSandboxContext;
  RemainingBytes: Int64;
  RemainingNodes: Integer;
begin
  if FRunScriptDepth >= MAX_RUN_SCRIPT_DEPTH then
    raise EGocciaSandboxNestingLimitExceeded.Create(
      'sandbox runScript nesting limit exceeded');
  Inc(FRunScriptDepth);
  try
    if not AOptions.Isolated then
      Exit(ExecuteInContext(AContext, AEntryPath));

    FillChar(Result, SizeOf(Result), 0);
    Result.Ok := False;
    Result.ExitCode := 1;
    { Fall-through default, as in ExecuteInContext: the child run replaces
      the whole record and every except branch assigns a kind, so this only
      survives if the runner returned unclassified. }
    Result.FailureKind := sfkHostError;
    RemainingBytes := AContext.Fs.QuotaBytes - AContext.Fs.UsedBytes;
    RemainingNodes := AContext.Fs.NodeQuota - AContext.Fs.NodeCount;
    if RemainingBytes <= 0 then
      raise ESandboxFsQuotaExceeded.Create('sandbox byte quota exhausted');
    if RemainingNodes <= 0 then
      raise ESandboxFsQuotaExceeded.Create('sandbox node quota exhausted');
    ChildContext := TGocciaSandboxContext.Create(RemainingBytes,
      RemainingNodes);
    try
      try
        ChildContext.RunScriptCallback := ExecuteNested;
        CopyNestedInputs(AContext, ChildContext, AOptions);
        ChildContext.CaptureBaseline;
        Result := ExecuteInContext(ChildContext,
          ChildContext.Fs.Normalize(AEntryPath));
        if AOptions.IncludeDiff then
        begin
          Result.DiffRequested := True;
          if AOptions.DiffFormat = SANDBOX_DIFF_FORMAT_UNIFIED then
            Result.Diff := ChildContext.DiffUnified(False)
          else
            Result.Diff := ChildContext.DiffJson(True);
        end;
      except
        on E: EGocciaCapabilityAuditDeliveryError do
          raise;
        { Copying into and diffing the child context, not the child
          program — but the guest supplies the copy list, so most of what
          fails here is still its own doing: the inherited quotas are a
          ceiling like any other, and a copy path that is missing, is not a
          directory, or is otherwise unusable is the guest naming a path the
          parent filesystem does not have. }
        on E: ESandboxFsQuotaExceeded do
        begin
          Result.Ok := False;
          Result.ExitCode := 1;
          Result.ErrorMessage := E.Message;
          Result.ErrorOutput := E.Message + sLineBreak;
          Result.FailureKind := sfkResourceLimit;
        end;
        on E: ESandboxFsError do
        begin
          Result.Ok := False;
          Result.ExitCode := 1;
          Result.ErrorMessage := E.Message;
          Result.ErrorOutput := E.Message + sLineBreak;
          Result.FailureKind := sfkScriptError;
        end;
        { What is left is copying or diffing failing for a reason the guest
          did not name — a native error in the runner's own copy or diff
          machinery. }
        on E: Exception do
        begin
          Result.Ok := False;
          Result.ExitCode := 1;
          Result.ErrorMessage := E.Message;
          Result.ErrorOutput := E.Message + sLineBreak;
          Result.FailureKind := sfkHostError;
        end;
      end;
    finally
      ChildContext.Free;
    end;
  finally
    Dec(FRunScriptDepth);
  end;
end;

end.
