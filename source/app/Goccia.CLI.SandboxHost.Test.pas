program Goccia.CLI.SandboxHost.Test;

{ The sandbox host: an engine it builds over a virtual filesystem takes
  nothing from the host filesystem that the entry's sandbox path happens to
  name. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
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
    procedure TestRootResultPrintedOnlyWhenAsked;
    procedure TestClockReachesIsolatedChildren;
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
  Test('The root run''s last value is converted for printing only when ' +
    'asked, in its own realm; a nested run''s is always copied',
    TestRootResultPrintedOnlyWhenAsked);
  Test('The host''s filesystem clock stamps its inputs, its run''s files and ' +
    'an isolated child''s', TestClockReachesIsolatedChildren);
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

procedure TSandboxHostTests.TestRootResultPrintedOnlyWhenAsked;
const
  MAIN_SOURCE =
    'import fs from "fs";' + LineEnding +
    'import { runScript } from "goccia";' + LineEnding +
    'const child = runScript("/child.js");' + LineEnding +
    'fs.writeFileSync("/out.txt", JSON.stringify(child.result));' +
      LineEnding +
    '({ a: [1, 2], toString() { return `a=${this.a}`; } });' + LineEnding;
var
  Host: TGocciaSandboxHost;
  RunResult: TGocciaSandboxRunResult;
begin
  Host := TGocciaSandboxHost.Create(DEFAULT_SANDBOX_BYTE_QUOTA,
    DEFAULT_SANDBOX_NODE_QUOTA);
  try
    Host.Context.Fs.WriteAllText('/child.js', '[3, { a: 4 }];');
    Host.Context.Fs.WriteAllText('/main.js', MAIN_SOURCE);
    Host.OnConfigureEngine := InstallSandboxModules;

    Expect<Boolean>(Host.PrintRootResult).ToBe(False);
    RunResult := Host.Run('/main.js');
    Expect<Boolean>(RunResult.Ok).ToBe(True);
    Expect<Boolean>(RunResult.HasResultText).ToBe(False);
    Expect<Boolean>(Assigned(RunResult.ResultValue)).ToBe(False);

    { The object's own toString runs in the run's realm, before its engine
      is freed; a copy made afterwards had no prototype to find it on. }
    Host.PrintRootResult := True;
    RunResult := Host.Run('/main.js');
    Expect<Boolean>(RunResult.Ok).ToBe(True);
    Expect<Boolean>(RunResult.HasResultText).ToBe(True);
    Expect<string>(RunResult.ResultText).ToBe('a=1,2');
    Expect<Boolean>(Assigned(RunResult.ResultValue)).ToBe(False);
    { The child's value still reaches the program that ran it. }
    Expect<string>(Host.Context.Fs.ReadAllText('/out.txt')).ToBe(
      '[3,{"a":4}]');
  finally
    Host.Free;
  end;
end;

procedure TSandboxHostTests.TestClockReachesIsolatedChildren;
const
  CHILD_SOURCE =
    'import fs from "fs";' + LineEnding +
    'fs.writeFileSync("/c.txt", "c");' + LineEnding +
    'const ms = (path) => Math.floor(fs.statSync(path).mtimeMs + 0.5);' +
      LineEnding +
    'ms("/c.txt") + " " + ms("/child.js");' + LineEnding;
  MAIN_SOURCE =
    'import fs from "fs";' + LineEnding +
    'import { runScript } from "goccia";' + LineEnding +
    'fs.writeFileSync("/w.txt", "w");' + LineEnding +
    'const child = runScript("/child.js", ' +
      '{ sandbox: true, copy: ["/child.js"] });' + LineEnding +
    'const ms = (path) => Math.floor(fs.statSync(path).mtimeMs + 0.5);' +
      LineEnding +
    'fs.writeFileSync("/out.txt", [child.result, ms("/in.txt"), ' +
      'ms("/w.txt")].join(" "));' + LineEnding;
var
  Clock: TGocciaSandboxFsHostClock;
  Host: TGocciaSandboxHost;
  RunResult: TGocciaSandboxRunResult;
begin
  Clock := TGocciaSandboxFsHostClock.Create(TGocciaFixedHostClock.Create(
    Int64(1700000000123000000), 0, 'UTC'));
  try
    Host := TGocciaSandboxHost.Create(DEFAULT_SANDBOX_BYTE_QUOTA,
      DEFAULT_SANDBOX_NODE_QUOTA, Clock);
    try
      Expect<Boolean>(Host.Context.Clock = Clock).ToBe(True);
      Host.Context.Fs.WriteAllText('/in.txt', 'input');
      Host.Context.Fs.WriteAllText('/child.js', CHILD_SOURCE);
      Host.Context.Fs.WriteAllText('/main.js', MAIN_SOURCE);
      Host.OnConfigureEngine := InstallSandboxModules;
      Host.CaptureBaseline;

      RunResult := Host.Run('/main.js');
      Expect<Boolean>(RunResult.Ok).ToBe(True);
      { The child ran on a filesystem of its own, created during the run:
        it keeps its parent's clock. }
      Expect<string>(Host.Context.Fs.ReadAllText('/out.txt')).ToBe(
        '1700000000123 1700000000123 1700000000123 1700000000123');
    finally
      Host.Free;
    end;
  finally
    { The host does not own the clock. }
    Clock.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TSandboxHostTests.Create('CLI Sandbox Host'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
