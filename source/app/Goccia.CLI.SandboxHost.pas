unit Goccia.CLI.SandboxHost;

{ GocciaRunner's sandbox mode (ADR 0122): owns the top-level sandbox context, runs each program
  over a context (the root, and every runScript or goccia child) on a thread
  of its own with a fresh engine, nests the children, classifies how a run
  ended, and renders the filesystem diff.

  An engine's state is its thread's, so a nested run cannot share its
  caller's thread: the caller's thread waits in the runScript native while
  the child runs on a new one, and the child's completion value comes back
  as text (Goccia.Sandbox EncodeSandboxValue) that the caller decodes.

  What an engine may do is not decided here. The application configures each
  engine through OnConfigureEngine and formats a failed run's error through
  OnFormatFailure; a nested child inherits its parent's capability set, so a
  child never reaches more than its parent. }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  SandboxHostInputs,
  SandboxVirtualFileSystem,

  Goccia.Capabilities,
  Goccia.Embed,
  Goccia.Packages.Address,
  Goccia.Sandbox.Context;

const
  { Each nesting level holds an engine, a thread and a virtual filesystem, so
    the depth a guest may ask for is a resource ceiling, as on the old
    engine. }
  MAX_RUN_SCRIPT_DEPTH = 32;

type
  TGocciaSandboxHost = class;

  { One program's run: its context, entry and engine, its parent run (nil
    for the root), and the console lines it printed. }
  TGocciaSandboxRun = class
  private
    FHost: TGocciaSandboxHost;
    FPreviousClamp: TSandboxRootClampCallback;
    procedure HandleRootClamp(const APath, ABase, ACanonicalPath: string);
  public
    Context: TGocciaSandboxContext;
    EntryPath: string;
    Source: TStringList;
    Engine: TGocciaEngine;
    Parent: TGocciaSandboxRun;
    Depth: Integer;
    Capabilities: TGocciaCapabilities;
    Output: TStringList;
    { the run's timeout in milliseconds (0 none): the host's, or what is left of the caller's when that is less }
    TimeoutMilliseconds: Int64;
    Deterministic: Boolean;
    { --silent: the console prints nothing; LogLine: each console line to the --log file too }
    Silent: Boolean;
    LogLine: procedure(const AMethod, ALine: string) of object;
    constructor Create;
    destructor Destroy; override;
    procedure CaptureConsole(const AMethod, ALine: UnicodeString);
    property Host: TGocciaSandboxHost read FHost;
  end;

  { Configures the engine ARun.Engine was just given (language profile,
    settings, audit, deterministic profile, console). }
  TGocciaSandboxEngineConfigurator = procedure(const ARun: TGocciaSandboxRun) of object;
  { The error text of a run that failed with a script error. }
  TGocciaSandboxFailureFormatter = function(const ARun: TGocciaSandboxRun;
    const AResult: TGocciaRunResult): string of object;

  TGocciaSandboxHost = class
  private
    FContext: TGocciaSandboxContext;
    FInputs: TSandboxHostInputs;
    FTimeoutMilliseconds: Integer;
    FMaxInstructions: Int64;
    FInspectDepth: Integer;
    FImportMapPath: string;
    FAliases: TStringList;
    FRootCapabilities: TGocciaCapabilities;
    FOnConfigureEngine: TGocciaSandboxEngineConfigurator;
    FOnFormatFailure: TGocciaSandboxFailureFormatter;
    FPrintRootResult: Boolean;
    FDeterministic: Boolean;
    function ExecuteInContext(const AContext: TGocciaSandboxContext;
      const AEntryPath: string; const AParent: TGocciaSandboxRun): TGocciaSandboxRunResult;
    function ExecuteNested(const AContext: TGocciaSandboxContext;
      const AEntryPath: string; const AOptions: TGocciaSandboxRunOptions):
      TGocciaSandboxRunResult;
    procedure CopyNestedInputs(const AParentContext,
      AChildContext: TGocciaSandboxContext;
      const AOptions: TGocciaSandboxRunOptions);
  public
    { AClock stamps the files of the host's sandbox filesystem and of every
      isolated nested run's; nil reads the wall clock. Not owned. }
    constructor Create(const AQuotaBytes: Int64; const ANodeQuota: Integer;
      const AClock: TSandboxFsClock = nil);
    destructor Destroy; override;
    function CopyIn(const AHostPath, ASandboxPath: string;
      const AReadWrite: Boolean): string; overload;
    function CopyIn(const AHostPath, ASandboxPath: string;
      const AReadWrite: Boolean; const APin: TSandboxHostPin): string;
      overload;
    procedure CaptureBaseline;
    { Runs AEntryPath, a sandbox path, as the root program. A failure is
      returned, classified; only a capability audit the sink could not take
      is raised (EGocciaCapabilityAuditDeliveryError). }
    function Run(const AEntryPath: string): TGocciaSandboxRunResult;
    function DiffText(const AUnified: Boolean): string;
    { the resolver's import map and aliases, for a run's module resolver }
    procedure ConfigureResolver(const AResolver: TObject);
    { sandbox mode never grants a provider package: the refusal is the PermissionDenied a missing import
      capability is (TGocciaSandboxModuleResolver asks before it refuses) }
    procedure RefuseProvider(const ASpecifier: string; const AAddress: TGocciaProviderAddress);

    property Context: TGocciaSandboxContext read FContext;
    property Inputs: TSandboxHostInputs read FInputs;
    property TimeoutMilliseconds: Integer read FTimeoutMilliseconds
      write FTimeoutMilliseconds;
    property MaxInstructions: Int64 read FMaxInstructions
      write FMaxInstructions;
    { console output's inspection depth in each run (--inspect-depth), 0 for
      the default }
    property InspectDepth: Integer read FInspectDepth write FInspectDepth;
    property ImportMapPath: string read FImportMapPath write FImportMapPath;
    property Aliases: TStringList read FAliases;
    property RootCapabilities: TGocciaCapabilities read FRootCapabilities
      write FRootCapabilities;
    { the deterministic profile (--deterministic) for every run }
    property Deterministic: Boolean read FDeterministic write FDeterministic;
    property OnConfigureEngine: TGocciaSandboxEngineConfigurator
      read FOnConfigureEngine write FOnConfigureEngine;
    property OnFormatFailure: TGocciaSandboxFailureFormatter
      read FOnFormatFailure write FOnFormatFailure;
    { the root run's completion value as --print shows it (ResultText) }
    property PrintRootResult: Boolean read FPrintRootResult
      write FPrintRootResult;
  end;

implementation

uses
  TextSemantics,

  Goccia.CapabilityAudit,
  Goccia.EngineFault,
  Goccia.Error.Suggestions,
  Goccia.HostAPI,
  Goccia.HostEnvironment,
  Goccia.Sandbox,
  Goccia.Sandbox.Modules,
  Goccia.ThreadCleanupRegistry,
  Goccia.VM;

const
  SANDBOX_ROOT = '/';
  { reserved, not committed: room for native recursion as deep as a file thread's. A 32-bit process has 2 GB of
    address space, which the nesting ceiling's 33 run threads would fill at 64 MB each before the ceiling is
    reached, so there each reserves 16 MB (the native stack guard reads the thread's own bounds) }
  {$IFDEF CPU64}
  RUN_THREAD_STACK = 64 * 1024 * 1024;
  {$ELSE}
  RUN_THREAD_STACK = 16 * 1024 * 1024;
  {$ENDIF}

threadvar
  { the run this thread is carrying out: a nested run's parent }
  CurrentRun: TGocciaSandboxRun;

type
  { one run on a thread of its own }
  TGocciaSandboxRunThread = class(TThread)
  public
    Host: TGocciaSandboxHost;
    Run: TGocciaSandboxRun;
    Source: UnicodeString;
    RandomRoot: IGocciaHostRandom;
    Result: TGocciaSandboxRunResult;
    AuditFailure: string;
    AuditFailed: Boolean;
  protected
    procedure Execute; override;
  end;

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

{ TGocciaSandboxRun }

constructor TGocciaSandboxRun.Create;
begin
  inherited Create;
  Output := TStringList.Create;
end;

destructor TGocciaSandboxRun.Destroy;
begin
  Output.Free;
  Source.Free;
  inherited Destroy;
end;

procedure TGocciaSandboxRun.CaptureConsole(const AMethod, ALine: UnicodeString);
begin
  if Assigned(LogLine) then
    LogLine(AMethod, ALine);
  if not Silent then
    Output.Add(ALine);
end;

{ a path that left the sandbox root was clamped back into it: audited as a
  denial at the script's call site }
procedure TGocciaSandboxRun.HandleRootClamp(const APath, ABase,
  ACanonicalPath: string);
begin
  if Assigned(Engine) then
    Engine.Audit(gckSandboxFileSystem, False, APath,
      Format('path from %s escaped the sandbox root and was clamped to %s',
        [ABase, ACanonicalPath]));
end;

{ TGocciaSandboxRunThread }

procedure TGocciaSandboxRunThread.Execute;
var
  Resolver: TGocciaSandboxModuleResolver;
  R: TGocciaRunResult;
  Message_: string;
begin
  { a fault anywhere in the thread's body, its teardown included, stops the process: escaping Execute it would
    end the thread silently in FatalException (ADR 0201) }
  try
    try
      Result := Default(TGocciaSandboxRunResult);
      Result.Ok := False;
      Result.ExitCode := 1;
      { every path below assigns a kind; this one stands for the runner returning
        without classifying }
      Result.FailureKind := sfkHostError;
      Resolver := nil;
      CurrentRun := Run;
      SetSandbox(Run.Context);
      try
        try
          Resolver := TGocciaSandboxModuleResolver.Create(Run.Context.Fs,
            SANDBOX_ROOT);
          Host.ConfigureResolver(Resolver);
          Run.Engine := TGocciaEngine.Create;
          Run.Engine.Resolver := Resolver;
          Run.Engine.Capabilities := Run.Capabilities;
          Run.Engine.Deterministic := Run.Deterministic;
          if RandomRoot <> nil then
            Run.Engine.RandomRoot := RandomRoot;
          Run.Engine.OnConsole := Run.CaptureConsole;
          Run.Engine.FailOnUnhandledRejection := True;
          Run.Engine.ExposeTimers := False;
          Run.Engine.TimeoutMs := Run.TimeoutMilliseconds;
          Run.Engine.MaxInstructions := Host.MaxInstructions;
          Run.Engine.InspectDepth := Host.InspectDepth;
          if Run.Parent <> nil then
            Run.Engine.ResultForm := grfSandbox
          else if Host.PrintRootResult then
            Run.Engine.ResultForm := grfString;
          if Assigned(Host.OnConfigureEngine) then
            Host.OnConfigureEngine(Run);
          Run.FPreviousClamp := Run.Context.Fs.RootClampCallback;
          Run.Context.Fs.RootClampCallback := Run.HandleRootClamp;
          try
            R := Run.Engine.Run(Run.EntryPath, Source);
          finally
            Run.Context.Fs.RootClampCallback := Run.FPreviousClamp;
          end;
          if R.Ok then
          begin
            Result.Ok := True;
            Result.ExitCode := 0;
            Result.FailureKind := sfkNone;
            if Run.Parent <> nil then
            begin
              Result.ResultEncoded := R.ResultText;
              Result.HasResultEncoded := R.HasResult;
            end
            else if R.HasResult then
            begin
              Result.ResultText := R.ResultText;
              Result.HasResultText := True;
            end;
          end
          else
          begin
            { each kind the guest can steer is named before the script error }
            case R.Aborted of
              gaTimeout:
                begin
                  Result.ErrorMessage := Format('file timed out after %dms',
                    [Host.TimeoutMilliseconds]);
                  Result.FailureKind := sfkTimeout;
                end;
              gaInstructionLimit:
                begin
                  Result.ErrorMessage := Format(
                    'Execution exceeded instruction limit of %d',
                    [Host.MaxInstructions]);
                  Result.FailureKind := sfkResourceLimit;
                end;
              gaMemoryLimit:
                begin
                  Result.ErrorMessage := 'memory limit exceeded: ' + R.ErrorText;
                  Result.FailureKind := sfkResourceLimit;
                end;
              gaHostAbort:
                { a ceiling of the sandbox's or the embedder's natives, a capability audit (this run's or a nested
                  run's) that could not be delivered, or else the embedder's own abort }
                if TakeSandboxCeiling then
                begin
                  Result.ErrorMessage := R.ErrorText;
                  Result.FailureKind := sfkResourceLimit;
                end
                else if TakeSandboxAuditFailure or Run.Engine.AuditDeliveryFailed then
                begin
                  { a capability audit the sink could not take: the host's
                    failure, raised in the caller }
                  AuditFailed := True;
                  AuditFailure := R.ErrorText;
                end
                else
                begin
                  { the embedder's own abort (an abort handle it captured
                    in OnConfigureEngine): the run ends, as the embedder
                    asked, and the embedder knows why }
                  Result.ErrorMessage := R.ErrorText;
                  Result.FailureKind := sfkHostError;
                end;
            else
              if (R.ErrorSuggestion = SSuggestMemoryLimitExceeded) or
                 (R.ErrorSuggestion = SSuggestStackMemoryLimitExceeded) then
              begin
                { --max-memory's refusal (ADR 0185: a catchable RangeError, which the engine marks with its suggestion
                  in a slot the script cannot set) left uncaught: the host's ceiling, as the run the limit ends }
                Result.ErrorMessage := 'memory limit exceeded: ' + R.ErrorText;
                Result.FailureKind := sfkResourceLimit;
              end
              else
              begin
                if Assigned(Host.OnFormatFailure) then
                  Message_ := Host.OnFormatFailure(Run, R)
                else
                  Message_ := R.ErrorMessage;
                Result.ErrorMessage := Message_;
                Result.FailureKind := sfkScriptError;
              end;
            end;
          end;
        except
          on E: EOutOfMemory do
          begin
            { a nested run's engine the process has no memory for (on a 32-bit build the nesting limit's engines,
              each with its register stack and frames, fill the address space first): the host's ceiling, which a
              guest reaches by nesting, as the nesting depth is }
            Result.Ok := False;
            Result.ExitCode := 1;
            Result.ErrorMessage := E.Message;
            if Run.Parent <> nil then
              Result.FailureKind := sfkResourceLimit
            else
              Result.FailureKind := sfkHostError;
          end;
          on E: Exception do
          begin
            if IsEngineFault(E) then
              AbortOnEngineFault(Run.EntryPath, E);
            Result.Ok := False;
            Result.ExitCode := 1;
            Result.ErrorMessage := E.Message;
            Result.FailureKind := sfkHostError;
          end;
        end;
        Result.Output := Run.Output.Text;
        if Result.ErrorMessage <> '' then
          Result.ErrorOutput := Result.ErrorMessage + sLineBreak;
      finally
        FreeAndNil(Run.Engine);
        Resolver.Free;
        SetSandbox(nil);
        CurrentRun := nil;
      end;
    finally
      { this thread's engine state: FPC does not finalize managed threadvars }
      RunThreadvarCleanupsOrAbort(Run.EntryPath);
    end;
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then
        AbortOnEngineFault(Run.EntryPath, E);
      raise;
    end;
  end;
end;

{ TGocciaSandboxHost }

constructor TGocciaSandboxHost.Create(const AQuotaBytes: Int64;
  const ANodeQuota: Integer; const AClock: TSandboxFsClock);
begin
  inherited Create;
  FContext := TGocciaSandboxContext.Create(AQuotaBytes, ANodeQuota, AClock);
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

function TGocciaSandboxHost.CopyIn(const AHostPath, ASandboxPath: string;
  const AReadWrite: Boolean; const APin: TSandboxHostPin): string;
begin
  Result := FInputs.CopyIn(AHostPath, ASandboxPath, AReadWrite, APin);
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

procedure TGocciaSandboxHost.RefuseProvider(const ASpecifier: string; const AAddress: TGocciaProviderAddress);
begin
  raise EGocciaPermissionDenied.CreateDenied(CapabilityName(gcImport), AAddress.PackageKey, '');
end;

procedure TGocciaSandboxHost.ConfigureResolver(const AResolver: TObject);
var
  Resolver: TGocciaSandboxModuleResolver;
  AliasSpec, AliasKey, AliasValue: string;
  I, SeparatorIndex: Integer;
begin
  Resolver := AResolver as TGocciaSandboxModuleResolver;
  Resolver.ProviderGrant := RefuseProvider;
  if FImportMapPath <> '' then
    Resolver.LoadImportMap(FImportMapPath);
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
    Resolver.AddAlias(AliasKey, AliasValue);
  end;
end;

procedure TGocciaSandboxHost.CopyNestedInputs(const AParentContext,
  AChildContext: TGocciaSandboxContext;
  const AOptions: TGocciaSandboxRunOptions);
var
  CopySpec: TGocciaSandboxCopySpec;
begin
  for CopySpec in AOptions.Copies do
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

function TGocciaSandboxHost.Run(const AEntryPath: string): TGocciaSandboxRunResult;
begin
  Result := ExecuteInContext(FContext, FContext.Fs.Normalize(AEntryPath), nil);
end;

function TGocciaSandboxHost.ExecuteInContext(
  const AContext: TGocciaSandboxContext; const AEntryPath: string;
  const AParent: TGocciaSandboxRun): TGocciaSandboxRunResult;
var
  Run: TGocciaSandboxRun;
  Thread: TGocciaSandboxRunThread;
  Remaining: Int64;
  Text, AuditFailure: string;
  AuditFailed: Boolean;
  Paused: Int64;
begin
  Result := Default(TGocciaSandboxRunResult);
  Result.Ok := False;
  Result.ExitCode := 1;
  if not AContext.Fs.IsFile(AEntryPath) then
  begin
    { the entry is an argument, and for a nested run the guest chose it }
    Result.ErrorMessage := 'sandbox entry file not found: ' + AEntryPath;
    Result.ErrorOutput := Result.ErrorMessage + sLineBreak;
    Result.FailureKind := sfkScriptError;
    Exit;
  end;
  Text := AContext.Fs.SnapshotReadAllText(AEntryPath);
  Run := TGocciaSandboxRun.Create;
  try
    Run.FHost := Self;
    Run.Context := AContext;
    Run.EntryPath := AEntryPath;
    Run.Source := CreateFileTextLines(Text);
    Run.Parent := AParent;
    Run.Deterministic := FDeterministic;
    Run.TimeoutMilliseconds := FTimeoutMilliseconds;
    if AParent = nil then
    begin
      Run.Depth := 0;
      Run.Capabilities := FRootCapabilities;
    end
    else
    begin
      { a child inherits its parent's capabilities, and at most the time its
        parent has left }
      Run.Depth := AParent.Depth + 1;
      Run.Capabilities := AParent.Capabilities;
      Remaining := LimitRemainingMs;
      if (Remaining > 0) and ((Run.TimeoutMilliseconds = 0) or
         (Remaining < Run.TimeoutMilliseconds)) then
        Run.TimeoutMilliseconds := Remaining;
    end;
    try
      Thread := TGocciaSandboxRunThread.Create(True, RUN_THREAD_STACK);
    except
      on E: EThread do
      begin
        { no room for another run's thread: for a nested run a ceiling of the host's, as the nesting depth is,
          which a guest reaches by nesting and so never a host fault }
        if AParent = nil then
          raise;
        raise EGocciaSandboxNestingLimitExceeded.Create(
          'sandbox runScript nesting limit exceeded: no room for another run (' + E.Message + ')');
      end;
    end;
    try
      Thread.FreeOnTerminate := False;
      Thread.Host := Self;
      Thread.Run := Run;
      Thread.Source := UnicodeString(Text);
      { a deterministic child draws a stream forked from its caller's, on the
        caller's thread }
      if (AParent <> nil) and Assigned(AParent.Engine) and FDeterministic then
        Thread.RandomRoot := AParent.Engine.ForkRandom;
      { the caller's deadline waits while the child, which has what is left of it, runs }
      Paused := LimitsSuspend;
      try
        Thread.Start;
        Thread.WaitFor;
      finally
        LimitsResume(Paused);
      end;
      Result := Thread.Result;
      AuditFailed := Thread.AuditFailed;
      AuditFailure := Thread.AuditFailure;
    finally
      Thread.Free;
    end;
  finally
    Run.Free;
  end;
  if AuditFailed then
    raise EGocciaCapabilityAuditDeliveryError.Create(AuditFailure);
end;

function TGocciaSandboxHost.ExecuteNested(const AContext: TGocciaSandboxContext;
  const AEntryPath: string; const AOptions: TGocciaSandboxRunOptions):
  TGocciaSandboxRunResult;
var
  Parent: TGocciaSandboxRun;
  ChildContext: TGocciaSandboxContext;
  RemainingBytes: Int64;
  RemainingNodes: Integer;
begin
  Parent := CurrentRun;
  if (Parent <> nil) and (Parent.Depth >= MAX_RUN_SCRIPT_DEPTH) then
    raise EGocciaSandboxNestingLimitExceeded.Create(
      'sandbox runScript nesting limit exceeded');
  if not AOptions.Isolated then
    Exit(ExecuteInContext(AContext, AEntryPath, Parent));

  Result := Default(TGocciaSandboxRunResult);
  Result.Ok := False;
  Result.ExitCode := 1;
  Result.FailureKind := sfkHostError;
  RemainingBytes := AContext.Fs.QuotaBytes - AContext.Fs.UsedBytes;
  RemainingNodes := AContext.Fs.NodeQuota - AContext.Fs.NodeCount;
  if RemainingBytes <= 0 then
    raise ESandboxFsQuotaExceeded.Create('sandbox byte quota exhausted');
  if RemainingNodes <= 0 then
    raise ESandboxFsQuotaExceeded.Create('sandbox node quota exhausted');
  { the child's filesystem keeps its parent's clock, as its quotas are
    inherited }
  ChildContext := TGocciaSandboxContext.Create(RemainingBytes,
    RemainingNodes, AContext.Clock);
  try
    try
      ChildContext.RunScriptCallback := ExecuteNested;
      CopyNestedInputs(AContext, ChildContext, AOptions);
      ChildContext.CaptureBaseline;
      Result := ExecuteInContext(ChildContext,
        ChildContext.Fs.Normalize(AEntryPath), Parent);
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
      on E: Exception do
      begin
        if IsEngineFault(E) then
          AbortOnEngineFault(AEntryPath, E);
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
end;

end.
