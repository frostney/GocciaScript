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
    procedure TestEntryPathNamesNoHostProject;
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

begin
  TestRunnerProgram.AddSuite(TSandboxHostTests.Create('CLI Sandbox Host'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
