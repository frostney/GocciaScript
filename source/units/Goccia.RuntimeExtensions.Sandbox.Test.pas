program Goccia.RuntimeExtensions.Sandbox.Test;

{ The sandbox runtime extension as an embedder installs it: what a script
  sees through the "fs" and "goccia" modules of a context whose virtual
  filesystem the embedder prepared. }

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.HostEnvironment,
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Sandbox,
  Goccia.Sandbox.Context,
  Goccia.TestSetup;

type
  TSandboxRuntimeExtensionTests = class(TTestSuite)
  private
    procedure RunModule(const AContext: TGocciaSandboxContext;
      const ASource: string);
    procedure TestReadOnlyPathsRefuseEveryWriteForm;
    procedure TestRefusedAppendChangesNoMetadata;
    procedure TestContextClockStampsEveryFile;
    procedure TestDeterministicClockFreezesTimestampsAtTheEpoch;
  public
    procedure SetupTests; override;
  end;

const
  { Runs every fs write form and the shell's writing commands against the
    read-only /data, and records each outcome in /out/log.txt. }
  READ_ONLY_PROBE_SOURCE =
    'import fs, { writeFile, appendFile, mkdir, rm, rename, copyFile } ' +
      'from "fs";' + LineEnding +
    'import { $ } from "goccia";' + LineEnding +
    'const seen = [];' + LineEnding +
    'const outcome = (error) => error ? error.code + " " + error.syscall : ' +
      '"ok";' + LineEnding +
    'const sync = (name, run) => {' + LineEnding +
    '  try { run(); seen.push(name + " ok"); }' + LineEnding +
    '  catch (error) { seen.push(name + " " + outcome(error)); }' +
      LineEnding +
    '};' + LineEnding +
    'const callback = (name, start) => new Promise((resolve) => {' +
      LineEnding +
    '  start((error) => { seen.push(name + " " + outcome(error)); ' +
      'resolve(); });' + LineEnding +
    '});' + LineEnding +
    'const promised = async (name, run) => {' + LineEnding +
    '  try { await run(); seen.push(name + " ok"); }' + LineEnding +
    '  catch (error) { seen.push(name + " " + outcome(error)); }' +
      LineEnding +
    '};' + LineEnding +
    'const shell = async (name, command) => {' + LineEnding +
    '  const result = await $(command).nothrow().run();' + LineEnding +
    '  seen.push(name + " " + result.exitCode + " " + ' +
      'result.stderr.includes("read-only file system"));' + LineEnding +
    '};' + LineEnding +
    'try { fs.writeFileSync("/data/a.txt", "x"); }' + LineEnding +
    'catch (error) {' + LineEnding +
    '  seen.push(String(error instanceof Error) + " " + error.message);' +
      LineEnding +
    '  seen.push("errno " + (error.errno < 0) + " path " + error.path);' +
      LineEnding +
    '}' + LineEnding +
    'sync("writeFileSync", () => fs.writeFileSync("/data/new.txt", "x"));' +
      LineEnding +
    'sync("appendFileSync", () => fs.appendFileSync("/data/a.txt", "x"));' +
      LineEnding +
    'sync("mkdirSync", () => fs.mkdirSync("/data/dir"));' + LineEnding +
    'sync("mkdirSync-recursive", () => fs.mkdirSync("/data/x/y", ' +
      '{ recursive: true }));' + LineEnding +
    'sync("rmSync", () => fs.rmSync("/data/a.txt"));' + LineEnding +
    'sync("rmSync-recursive", () => fs.rmSync("/data", ' +
      '{ recursive: true }));' + LineEnding +
    'sync("renameSync-from", () => fs.renameSync("/data/a.txt", ' +
      '"/out/a.txt"));' + LineEnding +
    'sync("renameSync-into", () => fs.renameSync("/out/w.txt", ' +
      '"/data/w.txt"));' + LineEnding +
    'sync("copyFileSync-into", () => fs.copyFileSync("/out/w.txt", ' +
      '"/data/w.txt"));' + LineEnding +
    'sync("copyFileSync-from", () => fs.copyFileSync("/data/a.txt", ' +
      '"/out/copy.txt"));' + LineEnding +
    'await callback("writeFile", (done) => writeFile("/data/a.txt", "x", ' +
      'done));' + LineEnding +
    'await callback("appendFile", (done) => appendFile("/data/a.txt", ' +
      '"x", done));' + LineEnding +
    'await callback("mkdir", (done) => mkdir("/data/dir", done));' +
      LineEnding +
    'await callback("rm", (done) => rm("/data/a.txt", done));' +
      LineEnding +
    'await callback("rename", (done) => rename("/out/w.txt", ' +
      '"/data/w.txt", done));' + LineEnding +
    'await callback("copyFile", (done) => copyFile("/out/w.txt", ' +
      '"/data/w.txt", done));' + LineEnding +
    'await promised("promises.writeFile", () => ' +
      'fs.promises.writeFile("/data/a.txt", "x"));' + LineEnding +
    'await promised("promises.appendFile", () => ' +
      'fs.promises.appendFile("/data/a.txt", "x"));' + LineEnding +
    'await promised("promises.mkdir", () => ' +
      'fs.promises.mkdir("/data/dir"));' + LineEnding +
    'await promised("promises.rm", () => ' +
      'fs.promises.rm("/data", { recursive: true }));' + LineEnding +
    'await promised("promises.rename", () => ' +
      'fs.promises.rename("/data/a.txt", "/out/a.txt"));' + LineEnding +
    'await promised("promises.copyFile", () => ' +
      'fs.promises.copyFile("/out/w.txt", "/data/w.txt"));' + LineEnding +
    'await shell("echo", "echo x > /data/a.txt");' + LineEnding +
    'await shell("echo-append", "echo x >> /data/a.txt");' + LineEnding +
    'await shell("touch", "touch /data/t.txt");' + LineEnding +
    'await shell("mkdir", "mkdir /data/dir");' + LineEnding +
    'await shell("rm", "rm -r /data");' + LineEnding +
    'await shell("cp", "cp /out/w.txt /data/w.txt");' + LineEnding +
    'await shell("mv", "mv /data/a.txt /out/a.txt");' + LineEnding +
    'seen.push("read " + fs.readFileSync("/data/a.txt", "utf8"));' +
      LineEnding +
    'fs.writeFileSync("/out/log.txt", seen.join("\n"));' + LineEnding;

procedure TSandboxRuntimeExtensionTests.SetupTests;
begin
  Test('A script sees EROFS from every fs write form and the shell under ' +
    'a read-only path', TestReadOnlyPathsRefuseEveryWriteForm);
  Test('A refused append leaves even the file''s timestamps unchanged',
    TestRefusedAppendChangesNoMetadata);
  Test('A context''s clock stamps every file, its baseline keeps it, and a ' +
    'script reads its time', TestContextClockStampsEveryFile);
  Test('Over the deterministic clock every timestamp is the epoch and the ' +
    'diff reports content changes only',
    TestDeterministicClockFreezesTimestampsAtTheEpoch);
end;

procedure TSandboxRuntimeExtensionTests.RunModule(
  const AContext: TGocciaSandboxContext; const ASource: string);
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Runtime: TGocciaRuntimeCore;
begin
  Source := TStringList.Create;
  try
    Source.Text := ASource;
    Engine := TGocciaEngine.Create('/main.js', Source);
    try
      Engine.SourceType := stModule;
      Runtime := AttachRuntime(Engine);
      Runtime.Install(TGocciaSandboxRuntimeExtension.Create(AContext));
      Engine.Execute;
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TSandboxRuntimeExtensionTests.TestReadOnlyPathsRefuseEveryWriteForm;
const
  EXPECTED: array[0..31] of string = (
    'true EROFS: read-only file system, writeFile ''/data/a.txt''',
    'errno true path /data/a.txt',
    'writeFileSync EROFS writeFile',
    'appendFileSync EROFS appendFile',
    'mkdirSync EROFS mkdir',
    'mkdirSync-recursive EROFS mkdir',
    'rmSync EROFS rm',
    'rmSync-recursive EROFS rm',
    'renameSync-from EROFS rename',
    'renameSync-into EROFS rename',
    'copyFileSync-into EROFS copyFile',
    'copyFileSync-from ok',
    'writeFile EROFS writeFile',
    'appendFile EROFS appendFile',
    'mkdir EROFS mkdir',
    'rm EROFS rm',
    'rename EROFS rename',
    'copyFile EROFS copyFile',
    'promises.writeFile EROFS writeFile',
    'promises.appendFile EROFS appendFile',
    'promises.mkdir EROFS mkdir',
    'promises.rm EROFS rm',
    'promises.rename EROFS rename',
    'promises.copyFile EROFS copyFile',
    'echo 1 true',
    'echo-append 1 true',
    'touch 1 true',
    'mkdir 1 true',
    'rm 1 true',
    'cp 1 true',
    'mv 1 true',
    'read original');
var
  Context: TGocciaSandboxContext;
  Lines: TStringList;
  Index: Integer;
  Diff: string;
begin
  Context := TGocciaSandboxContext.Create;
  Lines := TStringList.Create;
  try
    Context.Fs.MakeDirectory('/data/sub', True);
    Context.Fs.WriteAllText('/data/a.txt', 'original');
    Context.Fs.MakeDirectory('/out');
    Context.Fs.WriteAllText('/out/w.txt', 'w');
    Context.Fs.AddReadOnlyPath('/data');
    Context.CaptureBaseline;

    RunModule(Context, READ_ONLY_PROBE_SOURCE);

    Lines.Text := Context.Fs.ReadAllText('/out/log.txt');
    Expect<Integer>(Lines.Count).ToBe(Length(EXPECTED));
    for Index := 0 to High(EXPECTED) do
      if Index < Lines.Count then
        Expect<string>(Lines[Index]).ToBe(EXPECTED[Index]);

    { Nothing under /data changed, and the one write that read from it went
      to the writable /out. }
    Expect<string>(Context.Fs.ReadAllText('/data/a.txt')).ToBe('original');
    Expect<Integer>(Length(Context.Fs.List('/data'))).ToBe(2);
    Expect<Boolean>(Context.Fs.Exists('/data/sub')).ToBe(True);
    Expect<string>(Context.Fs.ReadAllText('/out/copy.txt')).ToBe('original');
    Expect<string>(Context.Fs.ReadAllText('/out/w.txt')).ToBe('w');
    { The refused writes changed nothing, so the diff does not list them. }
    Diff := Context.DiffJson;
    Expect<Boolean>(Pos('/out/copy.txt', Diff) > 0).ToBe(True);
    Expect<Boolean>(Pos('/data', Diff) > 0).ToBe(False);
  finally
    Lines.Free;
    Context.Free;
  end;
end;

procedure TSandboxRuntimeExtensionTests.TestRefusedAppendChangesNoMetadata;
const
  APPEND_SOURCE =
    'import fs from "fs";' + LineEnding +
    'let code = "none";' + LineEnding +
    'try { fs.appendFileSync("/data/a.txt", "x"); }' + LineEnding +
    'catch (error) { code = error.code; }' + LineEnding +
    'fs.writeFileSync("/out/code.txt", code);' + LineEnding;
var
  Context: TGocciaSandboxContext;
  Diff: string;
begin
  Context := TGocciaSandboxContext.Create;
  try
    Context.Fs.MakeDirectory('/data');
    Context.Fs.WriteAllText('/data/a.txt', 'original');
    Context.Fs.MakeDirectory('/out');
    Context.Fs.AddReadOnlyPath('/data');
    Context.CaptureBaseline;
    { The filesystem reads the wall clock, which moves in steps of about
      16 ms on Windows: wait out a step so an access-time change would
      show. }
    Sleep(50);

    RunModule(Context, APPEND_SOURCE);

    Expect<string>(Context.Fs.ReadAllText('/out/code.txt')).ToBe('EROFS');
    { With metadata: the access time is not advanced either. }
    Diff := Context.DiffJson(True);
    Expect<Boolean>(Pos('/out/code.txt', Diff) > 0).ToBe(True);
    Expect<Boolean>(Pos('/data', Diff) > 0).ToBe(False);
  finally
    Context.Free;
  end;
end;

const
  { Writes a file, then records the four timestamps of an embedder-written
    file and of the file it wrote, to the millisecond: the filesystem keeps
    a TDateTime, which is exact at the epoch but not to the nanosecond for
    every instant. }
  STAT_PROBE_SOURCE =
    'import fs from "fs";' + LineEnding +
    'fs.writeFileSync("/out/written.txt", "w");' + LineEnding +
    'fs.readFileSync("/data/input.txt", "utf8");' + LineEnding +
    'const times = (path) => {' + LineEnding +
    '  const stat = fs.statSync(path);' + LineEnding +
    '  return [stat.atimeMs, stat.mtimeMs, stat.ctimeMs, stat.birthtimeMs]' +
      '.map((ms) => Math.floor(ms + 0.5)).join(" ");' + LineEnding +
    '};' + LineEnding +
    'fs.writeFileSync("/out/log.txt", [times("/data/input.txt"), ' +
      'times("/out/written.txt")].join("\n"));' + LineEnding;

procedure TSandboxRuntimeExtensionTests.TestContextClockStampsEveryFile;
const
  FIXED_EPOCH_NANOSECONDS = Int64(1700000000123000000);
  FIXED_MILLISECONDS = '1700000000123';
  FIXED_TIMES = FIXED_MILLISECONDS + ' ' + FIXED_MILLISECONDS + ' ' +
    FIXED_MILLISECONDS + ' ' + FIXED_MILLISECONDS;
var
  Clock: TGocciaSandboxFsHostClock;
  Context: TGocciaSandboxContext;
  DefaultContext: TGocciaSandboxContext;
  Lines: TStringList;
  Stat: TSandboxFsStat;
begin
  DefaultContext := TGocciaSandboxContext.Create;
  try
    Expect<Boolean>(DefaultContext.Clock = nil).ToBe(True);
  finally
    DefaultContext.Free;
  end;

  Clock := TGocciaSandboxFsHostClock.Create(TGocciaFixedHostClock.Create(
    FIXED_EPOCH_NANOSECONDS, 0, 'UTC'));
  try
    Context := TGocciaSandboxContext.Create(DEFAULT_SANDBOX_BYTE_QUOTA,
      DEFAULT_SANDBOX_NODE_QUOTA, Clock);
    Lines := TStringList.Create;
    try
      Expect<Boolean>(Context.Clock = Clock).ToBe(True);
      { The root directory exists before the embedder writes anything, and
        is stamped by the clock too. }
      Expect<Double>(Round(SandboxDateTimeToUnixMilliseconds(
        Context.Fs.Stat('/').BirthTime))).ToBe(1700000000123);
      Context.Fs.MakeDirectory('/data');
      Context.Fs.WriteAllText('/data/input.txt', 'input');
      Context.Fs.MakeDirectory('/out');
      Context.CaptureBaseline;

      { The baseline is a fork, and a fork keeps the clock. }
      Context.Baseline.WriteAllText('/baseline-only.txt', 'b');
      Stat := Context.Baseline.Stat('/baseline-only.txt');
      Expect<Double>(Round(SandboxDateTimeToUnixMilliseconds(
        Stat.BirthTime))).ToBe(1700000000123);

      RunModule(Context, STAT_PROBE_SOURCE);

      Lines.Text := Context.Fs.ReadAllText('/out/log.txt');
      Expect<Integer>(Lines.Count).ToBe(2);
      if Lines.Count = 2 then
      begin
        Expect<string>(Lines[0]).ToBe(FIXED_TIMES);
        Expect<string>(Lines[1]).ToBe(FIXED_TIMES);
      end;
    finally
      Lines.Free;
      Context.Free;
    end;
  finally
    { The context does not own its clock. }
    Clock.Free;
  end;
end;

procedure TSandboxRuntimeExtensionTests.
  TestDeterministicClockFreezesTimestampsAtTheEpoch;
const
  REWRITE_SOURCE =
    'import fs from "fs";' + LineEnding +
    'fs.writeFileSync("/data/input.txt", "changed");' + LineEnding;
var
  Clock: TGocciaSandboxFsHostClock;
  Context: TGocciaSandboxContext;
  Lines: TStringList;
  Diff: string;
begin
  Clock := TGocciaSandboxFsHostClock.Create(
    TGocciaHostEnvironment.DeterministicClock);
  try
    Context := TGocciaSandboxContext.Create(DEFAULT_SANDBOX_BYTE_QUOTA,
      DEFAULT_SANDBOX_NODE_QUOTA, Clock);
    Lines := TStringList.Create;
    try
      Context.Fs.MakeDirectory('/data');
      Context.Fs.WriteAllText('/data/input.txt', 'input');
      Context.Fs.MakeDirectory('/out');
      Context.CaptureBaseline;

      RunModule(Context, STAT_PROBE_SOURCE);
      Lines.Text := Context.Fs.ReadAllText('/out/log.txt');
      Expect<Integer>(Lines.Count).ToBe(2);
      if Lines.Count = 2 then
      begin
        Expect<string>(Lines[0]).ToBe('0 0 0 0');
        Expect<string>(Lines[1]).ToBe('0 0 0 0');
      end;

      RunModule(Context, REWRITE_SOURCE);
      { Every timestamp is the epoch before and after, so the metadata
        dimension is empty; the content changes are still reported. }
      Diff := Context.DiffJson(True);
      Expect<Boolean>(Pos('"kind": "modify", "path": "/data/input.txt"',
        Diff) > 0).ToBe(True);
      Expect<Boolean>(Pos('"kind": "create", "path": "/out/written.txt"',
        Diff) > 0).ToBe(True);
      Expect<Boolean>(Pos('"metadataChanges": [' + LineEnding + '  ]',
        Diff) > 0).ToBe(True);
    finally
      Lines.Free;
      Context.Free;
    end;
  finally
    Clock.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TSandboxRuntimeExtensionTests.Create('Sandbox Runtime Extension'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
