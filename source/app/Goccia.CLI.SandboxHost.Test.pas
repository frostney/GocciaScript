program Goccia.CLI.SandboxHost.Test;

{ The sandbox host: an engine it builds over a virtual filesystem takes
  nothing from the host filesystem that the entry's sandbox path happens to
  name. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.CLI.SandboxHost,
  Goccia.Engine,
  Goccia.HostEnvironment,
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Sandbox,
  Goccia.Sandbox.Context,
  Goccia.TestSetup;

type
  TSandboxHostTests = class(TTestSuite)
  private
    FProjectRoots: TStringList;
    procedure RecordEngine(const AEngine: TGocciaEngine;
      const AContext: TGocciaSandboxContext; const AEntryPath: string;
      const AParentEngine: TGocciaEngine;
      const AParentHostEnvironment: TGocciaHostEnvironment);
    procedure InstallSandboxModules(const AEngine: TGocciaEngine;
      const AContext: TGocciaSandboxContext; const AEntryPath: string;
      const AParentEngine: TGocciaEngine;
      const AParentHostEnvironment: TGocciaHostEnvironment);
    procedure TestEntryPathNamesNoHostProject;
    procedure TestNestedRunsAndReadOnlyPaths;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

procedure TSandboxHostTests.SetupTests;
begin
  Test('A sandbox entry path does not make a host directory the project',
    TestEntryPathNamesNoHostProject);
  Test('A shared runScript child is refused under a read-only path, an ' +
    'isolated one writes its own copy', TestNestedRunsAndReadOnlyPaths);
end;

procedure TSandboxHostTests.BeforeEach;
begin
  inherited BeforeEach;
  FProjectRoots := TStringList.Create;
end;

procedure TSandboxHostTests.AfterEach;
begin
  FProjectRoots.Free;
  inherited AfterEach;
end;

procedure TSandboxHostTests.RecordEngine(const AEngine: TGocciaEngine;
  const AContext: TGocciaSandboxContext; const AEntryPath: string;
  const AParentEngine: TGocciaEngine;
  const AParentHostEnvironment: TGocciaHostEnvironment);
begin
  FProjectRoots.Add(AEngine.ProjectRoot);
end;

procedure TSandboxHostTests.TestEntryPathNamesNoHostProject;
var
  Host: TGocciaSandboxHost;
  Entry: string;
  RunResult: TGocciaSandboxRunResult;
begin
  { A sandbox path that is also an existing host directory's file: taken as
    a host path, its directory would become the project and exempt host
    reads under it. }
  Entry := '/' + ExtractFileName(ExcludeTrailingPathDelimiter(
    GetTempDir(False))) + '/entry.js';
  Host := TGocciaSandboxHost.Create(DEFAULT_SANDBOX_BYTE_QUOTA,
    DEFAULT_SANDBOX_NODE_QUOTA);
  try
    Host.Context.Fs.MakeDirectory(ExtractFilePath(Entry), True);
    Host.Context.Fs.WriteAllText(Entry, '1 + 1;');
    Host.OnConfigureEngine := RecordEngine;
    RunResult := Host.Run(Entry);
    Expect<Boolean>(RunResult.Ok).ToBe(True);
    Expect<Integer>(FProjectRoots.Count).ToBe(1);
    Expect<string>(FProjectRoots[0]).ToBe('');
  finally
    Host.Free;
  end;
end;

procedure TSandboxHostTests.InstallSandboxModules(
  const AEngine: TGocciaEngine; const AContext: TGocciaSandboxContext;
  const AEntryPath: string; const AParentEngine: TGocciaEngine;
  const AParentHostEnvironment: TGocciaHostEnvironment);
begin
  AEngine.SourceType := stModule;
  AttachRuntime(AEngine).Install(
    TGocciaSandboxRuntimeExtension.Create(AContext));
end;

procedure TSandboxHostTests.TestNestedRunsAndReadOnlyPaths;
const
  CHILD_SOURCE =
    'import fs from "fs";' + LineEnding +
    'fs.writeFileSync("/data/a.txt", "child");' + LineEnding +
    'fs.writeFileSync("/data/new.txt", "child");' + LineEnding;
  MAIN_SOURCE =
    'import fs from "fs";' + LineEnding +
    'import { runScript } from "goccia";' + LineEnding +
    'const shared = runScript("/child.js");' + LineEnding +
    'const isolated = runScript("/child.js", ' +
      '{ sandbox: true, copy: ["/child.js", "/data"] });' + LineEnding +
    'fs.writeFileSync("/out/log.txt", [' + LineEnding +
    '  "shared " + shared.ok + " " + shared.failureKind + " " + ' +
      'shared.error.includes("EROFS"),' + LineEnding +
    '  "isolated " + isolated.ok + " " + isolated.failureKind,' +
      LineEnding +
    '].join("\n"));' + LineEnding;
var
  Host: TGocciaSandboxHost;
  RunResult: TGocciaSandboxRunResult;
  Lines: TStringList;
begin
  Host := TGocciaSandboxHost.Create(DEFAULT_SANDBOX_BYTE_QUOTA,
    DEFAULT_SANDBOX_NODE_QUOTA);
  Lines := TStringList.Create;
  try
    Host.Context.Fs.MakeDirectory('/data');
    Host.Context.Fs.WriteAllText('/data/a.txt', 'original');
    Host.Context.Fs.MakeDirectory('/out');
    Host.Context.Fs.WriteAllText('/child.js', CHILD_SOURCE);
    Host.Context.Fs.WriteAllText('/main.js', MAIN_SOURCE);
    Host.Context.Fs.AddReadOnlyPath('/data');
    Host.OnConfigureEngine := InstallSandboxModules;

    RunResult := Host.Run('/main.js');
    Expect<Boolean>(RunResult.Ok).ToBe(True);
    Lines.Text := Host.Context.Fs.ReadAllText('/out/log.txt');
    Expect<Integer>(Lines.Count).ToBe(2);
    if Lines.Count = 2 then
    begin
      { The shared child runs on the caller's filesystem and is refused;
        the isolated child's copies carry no marks, and its writes stay in
        its own filesystem. }
      Expect<string>(Lines[0]).ToBe('shared false script-error true');
      Expect<string>(Lines[1]).ToBe('isolated true none');
    end;
    Expect<string>(Host.Context.Fs.ReadAllText('/data/a.txt')).ToBe(
      'original');
    Expect<Boolean>(Host.Context.Fs.Exists('/data/new.txt')).ToBe(False);
  finally
    Lines.Free;
    Host.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TSandboxHostTests.Create('CLI Sandbox Host'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
