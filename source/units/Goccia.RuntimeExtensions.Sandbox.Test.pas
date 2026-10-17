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

begin
  TestRunnerProgram.AddSuite(
    TSandboxRuntimeExtensionTests.Create('Sandbox Runtime Extension'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
