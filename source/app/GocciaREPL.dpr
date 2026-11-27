program GocciaREPL;

{ GocciaREPL: the options and config, one
  engine for the session on a thread of its own (an engine's state is its thread's), each input run on it as a
  Script, so its declarations and imports stay for the next one. `./build.pas repl` builds it as
  build/GocciaREPL. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  TimingUtils,

  Goccia.Application,
  Goccia.Capabilities,
  Goccia.CLI.Application,
  Goccia.CLI.FileSettings,
  Goccia.CLI.FileEngine,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.Stdin,
  Goccia.CLI.Trust,
  Goccia.FileExtensions,
  Goccia.Modules.Configuration,
  Goccia.Modules.Resolver,
  Goccia.REPL.LineEditor,
  Goccia.Terminal.Colors,
  Goccia.Version,

  Goccia.Embed,
  Goccia.EngineFault,
  Goccia.VM,
  Goccia.ThreadCleanupRegistry;

const
  { the name an input is run and reported as (GocciaREPL.dpr's REPL_FILE_NAME) }
  REPL_FILE_NAME = 'REPL';

type
  TGocciaREPLApp = class(TGocciaCLIApplication)
  private
    FTiming: TFlagOption;
    { the build's prelude snapshot run (GOCCIA_SNAPSHOT_OUT): this file run once, no prompt }
    FSnapshotFile: string;
    { the session, run by the REPL thread }
    procedure RunSession;
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    function HonoredCapabilities: TGocciaHonoredCapabilities; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

  { the session on a thread of its own, with the stack a file thread has }
  TGocciaREPLThread = class(TThread)
  private
    FApp: TGocciaREPLApp;
    FErrorClass: ExceptClass;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  end;

procedure TGocciaREPLThread.Execute;
begin
  try
    try
      JitTakeThread;
      FApp.RunSession;
    except
      on E: Exception do
      begin
        { the session ends here, on its own thread: the next input must not run on the state the fault calls into
          question (ADR 0196) }
        if IsEngineFault(E) then
          AbortOnEngineFault(REPL_FILE_NAME, E);
        FErrorClass := ExceptClass(E.ClassType);
        FErrorMessage := E.Message;
      end;
    end;
  finally
    { this thread's engine state: FPC does not finalize managed threadvars }
    RunThreadvarCleanups;
  end;
end;

procedure TGocciaREPLApp.Configure;
begin
  { an input is not an entry file, so its goal is not detected (ADR 0140) }
  AddEngineOptions.SourceType.HelpText := 'Source loading kind (default: script)';
  FTiming := AddFlag('timing', 'Show per-line timing');
end;

function TGocciaREPLApp.UsageLine: string;
begin
  Result := '[options]';
end;

function TGocciaREPLApp.HonoredCapabilities: TGocciaHonoredCapabilities;
begin
  Result := ALL_CAPABILITIES;
end;

procedure TGocciaREPLApp.ExecuteWithPaths(const APaths: TStringList);
var
  Worker: TGocciaREPLThread;
  ErrorClass: ExceptClass;
  ErrorMessage: string;
begin
  { build.pas compiles the preludes into the binary from a run of an empty file with GOCCIA_SNAPSHOT_OUT set }
  if (APaths.Count = 1) and (GetEnvironmentVariable('GOCCIA_SNAPSHOT_OUT') <> '') then
    FSnapshotFile := APaths[0]
  else if APaths.Count > 0 then
  begin
    WriteLn('Error: Unexpected argument "', APaths[0], '". The REPL does not accept file arguments.');
    ExitCode := 1;
    Exit;
  end;
  { the working directory's config governs the session: refuse before the prompt when its permission requests are
    not trusted }
  ValidateFileConfig(REPL_FILE_NAME);
  if FSnapshotFile = '' then
    WriteLn('Goccia REPL v' + GetVersion + ' (bytecode)');
  { each thread buffers its own output: the banner goes out before the session thread writes }
  Flush(Output);
  Worker := TGocciaREPLThread.Create(True, FILE_THREAD_STACK);
  try
    Worker.FApp := Self;
    Worker.Start;
    Worker.WaitFor;
    ErrorClass := Worker.FErrorClass;
    ErrorMessage := Worker.FErrorMessage;
  finally
    Worker.Free;
  end;
  if ErrorClass <> nil then
    raise ErrorClass.Create(ErrorMessage);
end;

procedure TGocciaREPLApp.RunSession;
var
  FileConfigPath, AliasBaseDirectory, Line, SetupError: string;
  FileConfig: TConfigEntryArray;
  Verdict: TGocciaConfigTrustVerdict;
  Capabilities: TGocciaCapabilities;
  Grant: TGocciaImportGrant;
  Resolver: TGocciaModuleResolver;
  Engine: TGocciaEngine;
  Editor: TLineEditor;
  Source: TStringList;
  ReadResult: TLineReadResult;
  RunResult: TGocciaRunResult;
  Start: Int64;
  Settings: TGocciaFileSettings;
begin
  FileConfigPath := DiscoverFileConfigPath(REPL_FILE_NAME);
  if FileConfigPath <> '' then
    FileConfig := LoadFileConfig(FileConfigPath)
  else
    SetLength(FileConfig, 0);
  Verdict := FileConfigVerdict(FileConfigPath, REPL_FILE_NAME);
  Capabilities := ResolveEngineCapabilities(FileConfigPath, REPL_FILE_NAME);
  Grant := TGocciaImportGrant.Create(Capabilities);
  Resolver := TGocciaModuleResolver.Create(IncludeTrailingPathDelimiter(GetCurrentDir));
  Grant.Resolver := Resolver;
  Resolver.SetExtensions(ModuleImportExtensions);
  Resolver.NodeModulesGrant := Grant.Grant;
  Resolver.ProviderGrant := Grant.GrantProvider;
  Resolver.ProviderAudit := Grant.AuditProvider;
  Resolver.ProviderCachedOnly := ResolveFlagOption(EngineOptions.CachedOnly, FileConfig);
  if EngineOptions.Aliases.FromCommandLine or (RootConfigPath = '') then
    AliasBaseDirectory := GetCurrentDir
  else
    AliasBaseDirectory := ExtractFilePath(RootConfigPath);
  ConfigureModuleResolver(Resolver, REPL_FILE_NAME, EngineOptions.ImportMap.ValueOr(''),
    EngineOptions.Aliases.Values, AliasBaseDirectory);
  Source := TStringList.Create;
  Editor := TLineEditor.Create;
  Engine := TGocciaEngine.Create;
  try
    Engine.Capabilities := Capabilities;
    Engine.ProjectRoot := GetCurrentDir;
    Engine.ConfigJson := HostConfigJson(EngineOptions, FileConfig);
    Engine.Resolver := Resolver;
    Engine.ModuleContentCheck := Grant.CheckContent;
    Engine.ProviderGraph := Grant.ProviderGraph;
    if AuditLogOpen then
    begin
      Engine.AuditSink := HandleCapabilityAudit;
      Grant.Engine := Engine;
    end;
    Settings := ResolveFileSettings(EngineOptions, FileConfig, REPL_FILE_NAME, Verdict.AcceptedUnsafe);
    ApplySettings(Engine, Settings);
    { an input is not an entry file, so its goal is not detected from its source (ADR 0140): without an explicit
      --source-type the REPL runs Script }
    if Engine.SourceKind = gskDetect then
      Engine.SourceKind := gskScript;
    if EngineOptions.Timeout.Present then
      Engine.TimeoutMs := EngineOptions.Timeout.Value;
    if EngineOptions.MaxInstructions.Present then
      Engine.MaxInstructions := EngineOptions.MaxInstructions.Value;
    if EngineOptions.MaxStack.Present then
      Engine.MaxFrames := Integer(EngineOptions.MaxStack.Value);
    Engine.Deterministic := ResolveFlagOption(EngineOptions.Deterministic, FileConfig);
    { an input that leaves a rejection nothing handles reports it as its error, and the session goes on }
    Engine.FailOnUnhandledRejection := True;
    if IsColorTerminal then
      Engine.ResultForm := grfREPLColor
    else
      Engine.ResultForm := grfREPL;
    { the configs' and the command line's virtual modules, as GocciaREPL's CreateEngine applies them }
    if not DefineVirtualModules(Engine, RootConfigPath, FileConfigPath, EngineOptions, SetupError) then
    begin
      WriteLn('Error: ', SetupError);
      ExitCode := 1;
      Exit;
    end;
    if FSnapshotFile <> '' then
    begin
      RunResult := Engine.Run(FSnapshotFile, '');
      if not RunResult.Ok then
        ExitCode := 1;
      Exit;
    end;
    while True do
    begin
      ReadResult := Editor.ReadLine('> ', Line);
      if ReadResult = lrExit then
        Break;
      if Line = 'exit' then
        Break;
      if Line = '' then
        Continue;
      Editor.AddToHistory(Line);
      Source.Clear;
      Source.Add(Line);
      Start := GetNanoseconds;
      RunResult := Engine.Run(REPL_FILE_NAME, Line);
      if RunResult.Ok then
      begin
        if RunResult.HasResult then
          WriteLn(UTF8Encode(RunResult.ResultText));
      end
      else
        WriteLn(FailureText(REPL_FILE_NAME, RunResult, Source, Engine, '', nil, True, True, IsColorTerminal,
          EngineOptions.Timeout.ValueOr(0), EngineOptions.MaxInstructions.ValueOr(0)));
      if FTiming.Present then
        WriteLn(Format('  Lex: (in Compile) | Parse: (in Compile) | Compile: %s | Execute: %s | Total: %s',
          [FormatDuration(RunResult.CompileNanoseconds), FormatDuration(RunResult.ExecuteNanoseconds),
           FormatDuration(GetNanoseconds - Start)]));
    end;
  finally
    Engine.Free;
    Editor.Free;
    Source.Free;
    Resolver.Free;
    Grant.Free;
  end;
end;

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TGocciaREPLApp, 'GocciaREPL');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
