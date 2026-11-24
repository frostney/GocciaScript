unit Goccia.CLI.G2SandboxHost;

{ GocciaRunner's sandbox mode on G2 (ADR 0122), as Goccia.CLI.SandboxHost runs
  it on the old engine: owns the top-level sandbox context, runs each program
  over a context (the root, and every runScript or goccia child) on a thread
  of its own with a fresh G2 engine, nests the children, classifies how a run
  ended, and renders the filesystem diff.

  A G2 engine's state is its thread's, so a nested run cannot share its
  caller's thread: the caller's thread waits in the runScript native while
  the child runs on a new one, and the child's completion value comes back
  as text (G2.Sandbox EncodeSandboxValue) that the caller decodes.

  What an engine may do is not decided here. The application configures each
  engine through OnConfigureEngine and formats a failed run's error through
  OnFormatFailure; a nested child inherits its parent's capability set, so a
  child never reaches more than its parent. }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  G2.Embed,
  SandboxHostInputs,
  SandboxVirtualFileSystem,

  Goccia.Capabilities,
  Goccia.Packages.Address,
  Goccia.Sandbox.Context;

const
  { Each nesting level holds an engine, a thread and a virtual filesystem, so
    the depth a guest may ask for is a resource ceiling, as on the old
    engine. }
  MAX_G2_RUN_SCRIPT_DEPTH = 32;

type
  TG2SandboxHost = class;

  { One program's run: its context, entry and engine, its parent run (nil
    for the root), and the console lines it printed. }
  TG2SandboxRun = class
  private
    FHost: TG2SandboxHost;
    FPreviousClamp: TSandboxRootClampCallback;
    procedure HandleRootClamp(const APath, ABase, ACanonicalPath: string);
  public
    Context: TGocciaSandboxContext;
    EntryPath: string;
    Source: TStringList;
    Engine: TG2Engine;
    Parent: TG2SandboxRun;
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
    property Host: TG2SandboxHost read FHost;
  end;

  { Configures the engine ARun.Engine was just given (language profile,
    settings, audit, deterministic profile, console). }
  TG2SandboxEngineConfigurator = procedure(const ARun: TG2SandboxRun) of object;
  { The error text of a run that failed with a script error. }
  TG2SandboxFailureFormatter = function(const ARun: TG2SandboxRun;
    const AResult: TG2RunResult): string of object;

  TG2SandboxHost = class
  private
    FContext: TGocciaSandboxContext;
    FInputs: TSandboxHostInputs;
    FTimeoutMilliseconds: Integer;
    FMaxInstructions: Int64;
    FImportMapPath: string;
    FAliases: TStringList;
    FRootCapabilities: TGocciaCapabilities;
    FOnConfigureEngine: TG2SandboxEngineConfigurator;
    FOnFormatFailure: TG2SandboxFailureFormatter;
    FPrintRootResult: Boolean;
    FDeterministic: Boolean;
    function ExecuteInContext(const AContext: TGocciaSandboxContext;
      const AEntryPath: string; const AParent: TG2SandboxRun): TGocciaSandboxRunResult;
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
    property ImportMapPath: string read FImportMapPath write FImportMapPath;
    property Aliases: TStringList read FAliases;
    property RootCapabilities: TGocciaCapabilities read FRootCapabilities
      write FRootCapabilities;
    { the deterministic profile (--deterministic) for every run }
    property Deterministic: Boolean read FDeterministic write FDeterministic;
    property OnConfigureEngine: TG2SandboxEngineConfigurator
      read FOnConfigureEngine write FOnConfigureEngine;
    property OnFormatFailure: TG2SandboxFailureFormatter
      read FOnFormatFailure write FOnFormatFailure;
    { the root run's completion value as --print shows it (ResultText) }
    property PrintRootResult: Boolean read FPrintRootResult
      write FPrintRootResult;
  end;

implementation

uses
  G2.HostAPI,
  G2.Sandbox,
  G2.VM,
  TextSemantics,

  Goccia.CapabilityAudit,
  Goccia.HostEnvironment,
  Goccia.Sandbox.Modules,
  Goccia.ThreadCleanupRegistry;

const
  SANDBOX_ROOT = '/';
  { reserved, not committed: room for native recursion as deep as a file thread's }
  RUN_THREAD_STACK = 64 * 1024 * 1024;

threadvar
  { the run this thread is carrying out: a nested run's parent }
  CurrentRun: TG2SandboxRun;

type
  { one run on a thread of its own }
  TG2SandboxRunThread = class(TThread)
  public
    Host: TG2SandboxHost;
    Run: TG2SandboxRun;
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

{ TG2SandboxRun }

constructor TG2SandboxRun.Create;
begin
  inherited Create;
  Output := TStringList.Create;
end;

destructor TG2SandboxRun.Destroy;
begin
  Output.Free;
  Source.Free;
  inherited Destroy;
end;

procedure TG2SandboxRun.CaptureConsole(const AMethod, ALine: UnicodeString);
begin
  if Assigned(LogLine) then
    LogLine(AMethod, ALine);
  if not Silent then
    Output.Add(ALine);
end;

{ a path that left the sandbox root was clamped back into it: audited as a
  denial at the script's call site }
procedure TG2SandboxRun.HandleRootClamp(const APath, ABase,
  ACanonicalPath: string);
begin
  if Assigned(Engine) then
    Engine.Audit(gckSandboxFileSystem, False, APath,
      Format('path from %s escaped the sandbox root and was clamped to %s',
        [ABase, ACanonicalPath]));
end;

{ TG2SandboxRunThread }

procedure TG2SandboxRunThread.Execute;
var
  Resolver: TGocciaSandboxModuleResolver;
  R: TG2RunResult;
  Message_: string;
begin
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
        Run.Engine := TG2Engine.Create;
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
        if Run.Parent <> nil then
          Run.Engine.ResultForm := g2rfSandbox
        else if Host.PrintRootResult then
          Run.Engine.ResultForm := g2rfString;
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
            g2aTimeout:
              begin
                Result.ErrorMessage := Format('file timed out after %dms',
                  [Host.TimeoutMilliseconds]);
                Result.FailureKind := sfkTimeout;
              end;
            g2aInstructionLimit:
              begin
                Result.ErrorMessage := Format(
                  'Execution exceeded instruction limit of %d',
                  [Host.MaxInstructions]);
                Result.FailureKind := sfkResourceLimit;
              end;
            g2aMemoryLimit:
              begin
                Result.ErrorMessage := 'memory limit exceeded: ' + R.ErrorText;
                Result.FailureKind := sfkResourceLimit;
              end;
            g2aHostAbort:
              if TakeSandboxCeiling then
              begin
                Result.ErrorMessage := R.ErrorText;
                Result.FailureKind := sfkResourceLimit;
              end
              else
              begin
                { a capability audit the sink could not take: the host's
                  failure, raised in the caller }
                AuditFailed := True;
                AuditFailure := R.ErrorText;
              end;
          else
            if Assigned(Host.OnFormatFailure) then
              Message_ := Host.OnFormatFailure(Run, R)
            else
              Message_ := R.ErrorMessage;
            Result.ErrorMessage := Message_;
            Result.FailureKind := sfkScriptError;
          end;
        end;
      except
        on E: Exception do
        begin
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
    RunThreadvarCleanups;
  end;
end;

{ TG2SandboxHost }

constructor TG2SandboxHost.Create(const AQuotaBytes: Int64;
  const ANodeQuota: Integer; const AClock: TSandboxFsClock);
begin
  inherited Create;
  FContext := TGocciaSandboxContext.Create(AQuotaBytes, ANodeQuota, AClock);
  FContext.RunScriptCallback := ExecuteNested;
  FInputs := TSandboxHostInputs.Create(FContext.Fs);
  FAliases := TStringList.Create;
  FRootCapabilities := TGocciaCapabilities.None;
end;

destructor TG2SandboxHost.Destroy;
begin
  FAliases.Free;
  FInputs.Free;
  FContext.Free;
  inherited Destroy;
end;

function TG2SandboxHost.CopyIn(const AHostPath, ASandboxPath: string;
  const AReadWrite: Boolean): string;
begin
  Result := FInputs.CopyIn(AHostPath, ASandboxPath, AReadWrite);
end;

function TG2SandboxHost.CopyIn(const AHostPath, ASandboxPath: string;
  const AReadWrite: Boolean; const APin: TSandboxHostPin): string;
begin
  Result := FInputs.CopyIn(AHostPath, ASandboxPath, AReadWrite, APin);
end;

procedure TG2SandboxHost.CaptureBaseline;
begin
  FContext.CaptureBaseline;
end;

function TG2SandboxHost.DiffText(const AUnified: Boolean): string;
begin
  if AUnified then
    Result := FContext.DiffUnified(False)
  else
    Result := FContext.DiffJson(True);
end;

procedure TG2SandboxHost.RefuseProvider(const ASpecifier: string; const AAddress: TGocciaProviderAddress);
begin
  raise EG2PermissionDenied.CreateDenied(CapabilityName(gcImport), AAddress.PackageKey, '');
end;

procedure TG2SandboxHost.ConfigureResolver(const AResolver: TObject);
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

procedure TG2SandboxHost.CopyNestedInputs(const AParentContext,
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

function TG2SandboxHost.Run(const AEntryPath: string): TGocciaSandboxRunResult;
begin
  Result := ExecuteInContext(FContext, FContext.Fs.Normalize(AEntryPath), nil);
end;

function TG2SandboxHost.ExecuteInContext(
  const AContext: TGocciaSandboxContext; const AEntryPath: string;
  const AParent: TG2SandboxRun): TGocciaSandboxRunResult;
var
  Run: TG2SandboxRun;
  Thread: TG2SandboxRunThread;
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
  Run := TG2SandboxRun.Create;
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
    Thread := TG2SandboxRunThread.Create(True, RUN_THREAD_STACK);
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

function TG2SandboxHost.ExecuteNested(const AContext: TGocciaSandboxContext;
  const AEntryPath: string; const AOptions: TGocciaSandboxRunOptions):
  TGocciaSandboxRunResult;
var
  Parent: TG2SandboxRun;
  ChildContext: TGocciaSandboxContext;
  RemainingBytes: Int64;
  RemainingNodes: Integer;
begin
  Parent := CurrentRun;
  if (Parent <> nil) and (Parent.Depth >= MAX_G2_RUN_SCRIPT_DEPTH) then
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
