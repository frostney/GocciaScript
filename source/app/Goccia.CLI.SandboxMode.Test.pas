program Goccia.CLI.SandboxMode.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  CLI.Parser,
  FileUtils,
  TestingPascalLibrary,

  Goccia.CLI.Application,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.CLI.SandboxMode;

type
  TSandboxModeTests = class(TTestSuite)
  private
    FRoot: string;
    FPaths: TStringList;
    function WriteFile(const ARelativePath, AText: string): string;
    function ParseSandboxOptions(
      const AArgs: array of string): TGocciaSandboxOptions;
    function CommandLine: TGocciaSandboxCommandLine;
    function ConfigSection(const ARelativePath,
      AText: string): TGocciaSandboxRequest;
    function ResolveError(const AArgs: array of string;
      const AConfig: TGocciaSandboxRequest;
      const ACommandLine: TGocciaSandboxCommandLine): string;
    procedure TestCopySpecDefaultTargets;
    procedure TestCopySpecExplicitTargets;
    procedure TestCopySpecWithoutName;
    procedure TestCopySpecTargetsMustBeAbsolute;
    procedure TestDuplicateCommandLineTargetsRejected;
    procedure TestConfigEntryYieldsToPositional;
    procedure TestDiffFormatInference;
    procedure TestDiffFormatValues;
    procedure TestMergeReplacesSameTarget;
    procedure TestHostModeByDefault;
    procedure TestSandboxOnlyOptionsNeedSandboxMode;
    procedure TestActivationReasons;
    procedure TestEntryRules;
    procedure TestHostCapabilitiesAndOptionsRejected;
    procedure TestConfigSectionNeedsAcceptance;
    procedure TestConfigInputsMergeWithCommandLine;
    procedure TestConfigOutputPathsConfined;
    procedure TestDiffResolution;
    procedure TestLimits;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

procedure TSandboxModeTests.SetupTests;
begin
  Test('--copy lands at /<basename> by default', TestCopySpecDefaultTargets);
  Test('--copy host=sandbox names the target', TestCopySpecExplicitTargets);
  Test('A host path with no name needs an explicit target',
    TestCopySpecWithoutName);
  Test('Copy targets are absolute, non-empty, and stay in the sandbox',
    TestCopySpecTargetsMustBeAbsolute);
  Test('Two command-line inputs with one target are refused',
    TestDuplicateCommandLineTargetsRejected);
  Test('A positional entry beats the config''s entry, with a note',
    TestConfigEntryYieldsToPositional);
  Test('--diff-file infers only .json and .diff', TestDiffFormatInference);
  Test('--diff accepts json and unified', TestDiffFormatValues);
  Test('A command-line input replaces the config input with its target',
    TestMergeReplacesSameTarget);
  Test('No sandbox option and no section is host mode',
    TestHostModeByDefault);
  Test('Sandbox-only options need sandbox mode',
    TestSandboxOnlyOptionsNeedSandboxMode);
  Test('The reason names what switched sandbox mode on',
    TestActivationReasons);
  Test('Entry: one host file or --entry, never stdin or a directory',
    TestEntryRules);
  Test('Host capabilities and host-mode options are rejected',
    TestHostCapabilitiesAndOptionsRejected);
  Test('A config section counts only once accepted',
    TestConfigSectionNeedsAcceptance);
  Test('Config inputs resolve against the config and merge with the CLI',
    TestConfigInputsMergeWithCommandLine);
  Test('Config copy-rw and diff-file stay inside the config directory',
    TestConfigOutputPathsConfined);
  Test('Diff: explicit format beats the file extension',
    TestDiffResolution);
  Test('Filesystem limits have defaults and reject zero', TestLimits);
end;

procedure DeleteDirectoryTree(const APath: string);
var
  SearchRec: TSearchRec;
  EntryPath: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
    SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  RemoveDir(APath);
end;

procedure TSandboxModeTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FRoot := ExcludeTrailingPathDelimiter(ExpandFileName(
    IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-sandbox-mode-' +
    IntToStr(Random(MaxInt))));
  ForceDirectories(FRoot);
  EnsureConfigParsersRegistered;
  FPaths := TStringList.Create;
  WriteFile('project/main.js', 'console.log(1);');
  WriteFile('project/src/lib.js', 'export const x = 1;');
  WriteFile('project/program.gbc', '');
end;

procedure TSandboxModeTests.AfterAll;
begin
  FPaths.Free;
  DeleteDirectoryTree(FRoot);
  inherited AfterAll;
end;

function TSandboxModeTests.WriteFile(const ARelativePath,
  AText: string): string;
begin
  Result := FRoot + PathDelim + ARelativePath;
  ForceDirectories(ExtractFileDir(Result));
  WriteUTF8FileText(Result, AText);
end;

function TSandboxModeTests.ParseSandboxOptions(
  const AArgs: array of string): TGocciaSandboxOptions;
var
  Positionals: TStringList;
  Options: TOptionArray;
  I: Integer;
begin
  Result := TGocciaSandboxOptions.Create;
  Options := Result.Options;
  try
    Positionals := ParseArguments(AArgs, Options);
  except
    Result.Free;
    raise;
  end;
  try
    FPaths.Assign(Positionals);
  finally
    Positionals.Free;
  end;
  for I := 0 to High(Options) do
    if Options[I].Present then
      Options[I].MarkFromCommandLine;
end;

function TSandboxModeTests.CommandLine: TGocciaSandboxCommandLine;
begin
  Result := Default(TGocciaSandboxCommandLine);
  Result.Paths := FPaths;
  Result.WorkingDirectory := FRoot + PathDelim + 'project';
end;

function TSandboxModeTests.ConfigSection(const ARelativePath,
  AText: string): TGocciaSandboxRequest;
var
  Path: string;
begin
  Path := WriteFile(ARelativePath, AText);
  Result := ReadConfigPermissionRequest(ParseConfigFile(Path), Path).Sandbox;
end;

function TSandboxModeTests.ResolveError(const AArgs: array of string;
  const AConfig: TGocciaSandboxRequest;
  const ACommandLine: TGocciaSandboxCommandLine): string;
var
  Options: TGocciaSandboxOptions;
begin
  Result := '';
  Options := nil;
  try
    try
      Options := ParseSandboxOptions(AArgs);
      ResolveSandboxMode(Options, AConfig, True, ACommandLine);
    except
      on E: Exception do
        Result := E.ClassName + ': ' + E.Message;
    end;
  finally
    Options.Free;
  end;
end;

procedure TSandboxModeTests.TestCopySpecDefaultTargets;
var
  Input: TGocciaSandboxInput;
begin
  Input := ParseCopySpec('src', FRoot, '--copy', False);
  Expect<string>(Input.HostPath).ToBe(FRoot + PathDelim + 'src');
  Expect<string>(Input.SandboxPath).ToBe('/src');
  Expect<Boolean>(Input.ReadWrite).ToBe(False);

  Input := ParseCopySpec('data/task.txt', FRoot, '--copy-rw', True);
  Expect<string>(Input.SandboxPath).ToBe('/task.txt');
  Expect<Boolean>(Input.ReadWrite).ToBe(True);

  { A trailing separator does not change the name. }
  Input := ParseCopySpec('src/', FRoot, '--copy', False);
  Expect<string>(Input.SandboxPath).ToBe('/src');
end;

procedure TSandboxModeTests.TestCopySpecExplicitTargets;
var
  Input: TGocciaSandboxInput;
begin
  Input := ParseCopySpec('fixtures=/data', FRoot, '--copy', False);
  Expect<string>(Input.HostPath).ToBe(FRoot + PathDelim + 'fixtures');
  Expect<string>(Input.SandboxPath).ToBe('/data');

  Input := ParseCopySpec('dir=/', FRoot, '--copy', False);
  Expect<string>(Input.SandboxPath).ToBe('/');

  Input := ParseCopySpec('a.txt=/inbox/', FRoot, '--copy', False);
  Expect<string>(Input.SandboxPath).ToBe('/inbox/');

  Input := ParseCopySpec('a.txt=\inbox\a.txt', FRoot, '--copy', False);
  Expect<string>(Input.SandboxPath).ToBe('/inbox/a.txt');
end;

function CopySpecError(const ASpec, AOrigin: string): string;
begin
  Result := '';
  try
    ParseCopySpec(ASpec, GetTempDir(False), AOrigin, False);
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TSandboxModeTests.TestCopySpecTargetsMustBeAbsolute;
begin
  Expect<string>(CopySpecError('rw=', '--copy')).ToBe('TCLIUsageError: ' +
    '--copy rw=: the sandbox path is empty; give an absolute sandbox path ' +
    'such as /name');
  Expect<string>(CopySpecError('rw=rel', '--copy-rw')).ToBe(
    'TCLIUsageError: --copy-rw rw=rel: "rel" is not an absolute sandbox ' +
    'path; start it with /');
  Expect<string>(CopySpecError('x=/../../etc', '--copy')).ToBe(
    'TCLIUsageError: --copy x=/../../etc: "/../../etc" climbs above the ' +
    'sandbox root');
  Expect<string>(CopySpecError('x=/a/../b', '--copy')).ToBe('');
end;

procedure TSandboxModeTests.TestDuplicateCommandLineTargetsRejected;
begin
  WriteFile('dup/a.txt', 'a');
  WriteFile('dup/b/a.txt', 'b');
  WriteFile('dup/x/y.txt', 'y');
  Expect<string>(ResolveError(['--sandbox', '--entry=/a.txt', '--copy',
    '../dup/a.txt', '--copy', '../dup/b/a.txt'],
    Default(TGocciaSandboxRequest), CommandLine)).ToBe('TCLIUsageError: ' +
    '--copy ../dup/a.txt and --copy ../dup/b/a.txt both copy to /a.txt; ' +
    'give one of them an explicit =<sandbox> path');
  Expect<string>(ResolveError(['--sandbox', '--entry=/x/y.txt', '--copy',
    '../dup/x', '--copy-rw', '../dup/x'], Default(TGocciaSandboxRequest),
    CommandLine)).ToBe('TCLIUsageError: --copy ../dup/x and --copy-rw ' +
    '../dup/x both copy to /x; give one of them an explicit =<sandbox> path');
  { Files into one directory land on different paths. }
  Expect<string>(ResolveError(['--sandbox', '--entry=/in/a.txt', '--copy',
    '../dup/a.txt=/in/', '--copy', '../dup/x/y.txt=/in/'],
    Default(TGocciaSandboxRequest), CommandLine)).ToBe('');
end;

procedure TSandboxModeTests.TestCopySpecWithoutName;
var
  Message: string;
begin
  Message := '';
  try
    ParseCopySpec('/', FRoot, '--copy', False);
  except
    on E: TCLIUsageError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe('--copy /: the host path has no name to use ' +
    'as its sandbox path; give one with /=<sandbox>');

  Message := '';
  try
    ParseCopySpec('=/x', FRoot, '--copy-rw', True);
  except
    on E: TCLIUsageError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe(
    '--copy-rw needs a host path: --copy-rw <host>[=<sandbox>]');
end;

procedure TSandboxModeTests.TestDiffFormatInference;
var
  Format: TGocciaSandboxDiffFormat;
begin
  Expect<Boolean>(TryInferDiffFormat('out/changes.json', Format)).ToBe(True);
  Expect<Boolean>(Format = sdfJson).ToBe(True);
  Expect<Boolean>(TryInferDiffFormat('changes.DIFF', Format)).ToBe(True);
  Expect<Boolean>(Format = sdfUnified).ToBe(True);
  { ADR 0119: a unified diff here is not a patch, so .patch implies
    nothing (maintainer decision R1). }
  Expect<Boolean>(TryInferDiffFormat('changes.patch', Format)).ToBe(False);
  Expect<Boolean>(TryInferDiffFormat('changes.txt', Format)).ToBe(False);
  Expect<Boolean>(TryInferDiffFormat('changes', Format)).ToBe(False);
end;

procedure TSandboxModeTests.TestDiffFormatValues;
var
  Format: TGocciaSandboxDiffFormat;
begin
  Expect<Boolean>(TryParseDiffFormat('json', Format)).ToBe(True);
  Expect<Boolean>(Format = sdfJson).ToBe(True);
  Expect<Boolean>(TryParseDiffFormat('unified', Format)).ToBe(True);
  Expect<Boolean>(Format = sdfUnified).ToBe(True);
  Expect<Boolean>(TryParseDiffFormat('patch', Format)).ToBe(False);
  Expect<Boolean>(TryParseDiffFormat('JSON', Format)).ToBe(False);
end;

procedure TSandboxModeTests.TestMergeReplacesSameTarget;
var
  ConfigInputs, CommandLineInputs, Merged: TGocciaSandboxInputs;
begin
  SetLength(ConfigInputs, 2);
  ConfigInputs[0] := ParseCopySpec('out', FRoot, 'config', True);
  ConfigInputs[1] := ParseCopySpec('src', FRoot, 'config', False);
  SetLength(CommandLineInputs, 1);
  CommandLineInputs[0] := ParseCopySpec('out=/out/', FRoot, '--copy', False);

  Merged := MergeSandboxInputs(ConfigInputs, CommandLineInputs);
  Expect<Integer>(Length(Merged)).ToBe(2);
  Expect<string>(Merged[0].SandboxPath).ToBe('/src');
  Expect<string>(Merged[1].SandboxPath).ToBe('/out/');
  { The dry run: the command line's read-only copy replaced copy-rw. }
  Expect<Boolean>(Merged[1].ReadWrite).ToBe(False);
end;

procedure TSandboxModeTests.TestHostModeByDefault;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
begin
  Options := ParseSandboxOptions(['main.js']);
  try
    Request := ResolveSandboxMode(Options, Default(TGocciaSandboxRequest),
      True, CommandLine);
    Expect<Boolean>(Request.Active).ToBe(False);
  finally
    Options.Free;
  end;
end;

procedure TSandboxModeTests.TestSandboxOnlyOptionsNeedSandboxMode;
begin
  Expect<string>(ResolveError(['--entry=/main.js'],
    Default(TGocciaSandboxRequest), CommandLine)).ToBe(
    'TCLIUsageError: --entry only applies in sandbox mode; enable sandbox ' +
    'mode with --sandbox or --copy <host>[=<sandbox>]');
  Expect<string>(ResolveError(['main.js', '--diff'],
    Default(TGocciaSandboxRequest), CommandLine)).ToBe(
    'TCLIUsageError: --diff only applies in sandbox mode; enable sandbox ' +
    'mode with --sandbox or --copy <host>[=<sandbox>]');
  Expect<string>(ResolveError(['main.js', '--diff-file=x.json'],
    Default(TGocciaSandboxRequest), CommandLine)).ToBe(
    'TCLIUsageError: --diff-file only applies in sandbox mode; enable ' +
    'sandbox mode with --sandbox or --copy <host>[=<sandbox>]');
  Expect<string>(ResolveError(['main.js', '--max-fs-bytes=1MiB'],
    Default(TGocciaSandboxRequest), CommandLine)).ToBe(
    'TCLIUsageError: --max-fs-bytes only applies in sandbox mode; enable ' +
    'sandbox mode with --sandbox or --copy <host>[=<sandbox>]');
end;

procedure TSandboxModeTests.TestActivationReasons;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
  Config: TGocciaSandboxRequest;
begin
  Options := ParseSandboxOptions(['main.js', '--sandbox']);
  try
    Request := ResolveSandboxMode(Options, Default(TGocciaSandboxRequest),
      True, CommandLine);
    Expect<Boolean>(Request.Active).ToBe(True);
    Expect<string>(Request.Reason).ToBe('--sandbox');
    Expect<Integer>(Length(Request.Inputs)).ToBe(0);
    Expect<string>(Request.EntryHost).ToBe(FRoot + PathDelim + 'project' +
      PathDelim + 'main.js');
    Expect<string>(Request.EntrySandbox).ToBe('');
  finally
    Options.Free;
  end;

  Options := ParseSandboxOptions(['main.js', '--copy-rw', 'src']);
  try
    Request := ResolveSandboxMode(Options, Default(TGocciaSandboxRequest),
      True, CommandLine);
    Expect<string>(Request.Reason).ToBe('--copy-rw');
    Expect<Integer>(Length(Request.Inputs)).ToBe(1);
    Expect<string>(Request.Inputs[0].SandboxPath).ToBe('/src');
    Expect<Boolean>(Request.Inputs[0].ReadWrite).ToBe(True);
  finally
    Options.Free;
  end;

  Config := ConfigSection('activation/goccia.json', '{"sandbox": {}}');
  Options := ParseSandboxOptions(['main.js']);
  try
    Request := ResolveSandboxMode(Options, Config, True, CommandLine);
    Expect<Boolean>(Request.Active).ToBe(True);
    Expect<string>(Request.Reason).ToBe('the "sandbox" section of ' +
      FRoot + PathDelim + 'activation' + PathDelim + 'goccia.json');
  finally
    Options.Free;
  end;
end;

procedure TSandboxModeTests.TestEntryRules;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
  Empty: TGocciaSandboxRequest;
begin
  Empty := Default(TGocciaSandboxRequest);
  Options := ParseSandboxOptions(['--sandbox', '--entry', '/tools/run.js']);
  try
    Request := ResolveSandboxMode(Options, Empty, True, CommandLine);
    Expect<string>(Request.EntrySandbox).ToBe('/tools/run.js');
    Expect<string>(Request.EntryHost).ToBe('');
  finally
    Options.Free;
  end;
  Expect<string>(ResolveError(['--sandbox', '--entry=main.js'], Empty,
    CommandLine)).ToBe('TCLIUsageError: --entry: "main.js" is not an ' +
    'absolute sandbox path; start it with /');
  Expect<string>(ResolveError(['--sandbox', '--entry=/../main.js'], Empty,
    CommandLine)).ToBe('TCLIUsageError: --entry: "/../main.js" climbs ' +
    'above the sandbox root');

  Expect<string>(ResolveError(['main.js', '--sandbox', '--entry=/x.js'],
    Empty, CommandLine)).ToBe('TCLIUsageError: --entry names the sandbox ' +
    'entry, so a host file cannot be given too; drop main.js or --entry');
  Expect<string>(ResolveError(['--sandbox'], Empty, CommandLine)).ToBe(
    'TCLIUsageError: sandbox mode needs an entry: pass a host file, or ' +
    '--entry <sandbox-path> for a copied one (sandbox mode does not read ' +
    'stdin)');
  Expect<string>(ResolveError(['-', '--sandbox'], Empty, CommandLine)).ToBe(
    'TCLIUsageError: sandbox mode does not read stdin; pass a host file, ' +
    'or --entry <sandbox-path> for a copied one');
  Expect<string>(ResolveError(['main.js', 'src/lib.js', '--sandbox'], Empty,
    CommandLine)).ToBe('TCLIUsageError: sandbox mode runs one entry file; ' +
    'got 2 inputs');
  Expect<string>(ResolveError(['src', '--sandbox'], Empty, CommandLine)).ToBe(
    'TCLIUsageError: sandbox mode runs one entry file, not a directory: ' +
    'src (copy it with --copy src and name the entry with --entry)');
  Expect<string>(ResolveError(['program.gbc', '--sandbox'], Empty,
    CommandLine)).ToBe('TCLIUsageError: sandbox mode runs source files, ' +
    'not bytecode: program.gbc');
  Expect<string>(ResolveError(['missing.js', '--sandbox'], Empty,
    CommandLine)).ToBe('Exception: Path not found: missing.js');
end;

procedure TSandboxModeTests.TestHostCapabilitiesAndOptionsRejected;
var
  Line: TGocciaSandboxCommandLine;
begin
  Line := CommandLine;
  SetLength(Line.DeniedAllowFlags, 1);
  Line.DeniedAllowFlags[0] := '--allow-read';
  Expect<string>(ResolveError(['main.js', '--copy', 'src'],
    Default(TGocciaSandboxRequest), Line)).ToBe('TCLIUsageError: ' +
    '--allow-read cannot be used in sandbox mode (enabled by --copy): the ' +
    'sandbox has no host filesystem; copy inputs with --copy');

  Line := CommandLine;
  SetLength(Line.HostOnlyOptions, 1);
  Line.HostOnlyOptions[0] := '--output';
  Expect<string>(ResolveError(['main.js', '--sandbox'],
    Default(TGocciaSandboxRequest), Line)).ToBe('TCLIUsageError: --output ' +
    'cannot be used in sandbox mode (enabled by --sandbox)');
end;

procedure TSandboxModeTests.TestConfigSectionNeedsAcceptance;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
  Config: TGocciaSandboxRequest;
begin
  Config := ConfigSection('unaccepted/goccia.json',
    '{"sandbox": {"copy": ["."]}}');
  Options := ParseSandboxOptions(['main.js']);
  try
    Request := ResolveSandboxMode(Options, Config, False, CommandLine);
    Expect<Boolean>(Request.Active).ToBe(False);
  finally
    Options.Free;
  end;
end;

procedure TSandboxModeTests.TestConfigInputsMergeWithCommandLine;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
  Config: TGocciaSandboxRequest;
begin
  WriteFile('merge/src/a.js', '');
  WriteFile('merge/out/b.txt', '');
  WriteFile('merge/fixtures/c.json', '{}');
  Config := ConfigSection('merge/goccia.json', '{"sandbox": {' +
    '"copy": ["src", "fixtures=/data"], "copy-rw": ["out"], ' +
    '"entry": "/src/a.js"}}');
  Options := ParseSandboxOptions(['--copy',
    FRoot + PathDelim + 'merge' + PathDelim + 'out']);
  try
    Request := ResolveSandboxMode(Options, Config, True, CommandLine);
    Expect<string>(Request.Reason).ToBe('--copy');
    Expect<string>(Request.EntrySandbox).ToBe('/src/a.js');
    Expect<Integer>(Length(Request.Inputs)).ToBe(3);
    Expect<string>(Request.Inputs[0].HostPath).ToBe(FRoot + PathDelim +
      'merge' + PathDelim + 'src');
    Expect<string>(Request.Inputs[0].SandboxPath).ToBe('/src');
    Expect<Boolean>(Request.Inputs[0].FromConfig).ToBe(True);
    Expect<string>(Request.Inputs[1].SandboxPath).ToBe('/data');
    { The command line's --copy out replaced the config's copy-rw. }
    Expect<string>(Request.Inputs[2].SandboxPath).ToBe('/out');
    Expect<Boolean>(Request.Inputs[2].ReadWrite).ToBe(False);
    Expect<Boolean>(Request.Inputs[2].FromConfig).ToBe(False);
  finally
    Options.Free;
  end;
end;

procedure TSandboxModeTests.TestConfigOutputPathsConfined;
var
  Config: TGocciaSandboxRequest;
  ConfigPath: string;
begin
  WriteFile('confined/project/goccia.json', '{}');
  WriteFile('confined/outside/x.txt', '');
  ConfigPath := FRoot + PathDelim + 'confined' + PathDelim + 'project' +
    PathDelim + 'goccia.json';

  Config := ConfigSection('confined/project/goccia.json',
    '{"sandbox": {"copy-rw": ["../outside"]}}');
  Expect<string>(ResolveError(['main.js'], Config, CommandLine)).ToBe(
    'TParseError: ' + ConfigPath + ': "sandbox.copy-rw" writes to ' + FRoot +
    PathDelim + 'confined' + PathDelim + 'outside, which is outside ' +
    ExtractFileDir(ConfigPath) + '; a config may only write inside its ' +
    'own directory (pass --copy-rw on the command line to write ' +
    'elsewhere)');

  { Reading from outside is not a write. }
  Config := ConfigSection('confined/project/goccia.json',
    '{"sandbox": {"copy": ["../outside"]}}');
  Expect<string>(ResolveError(['main.js'], Config, CommandLine)).ToBe('');

  Config := ConfigSection('confined/project/goccia.json',
    '{"sandbox": {"diff-file": "../changes.json"}}');
  Expect<string>(ResolveError(['main.js'], Config, CommandLine)).ToBe(
    'TParseError: ' + ConfigPath + ': "sandbox.diff-file" writes to ' +
    FRoot + PathDelim + 'confined' + PathDelim + 'changes.json, which is ' +
    'outside ' + ExtractFileDir(ConfigPath) + '; a config may only write ' +
    'inside its own directory (pass --diff-file on the command line to ' +
    'write elsewhere)');
end;

procedure TSandboxModeTests.TestDiffResolution;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
  Empty: TGocciaSandboxRequest;
begin
  Empty := Default(TGocciaSandboxRequest);
  Options := ParseSandboxOptions(['main.js', '--sandbox', '--diff']);
  try
    Request := ResolveSandboxMode(Options, Empty, True, CommandLine);
    Expect<Boolean>(Request.DiffRequested).ToBe(True);
    Expect<Boolean>(Request.DiffFormat = sdfJson).ToBe(True);
    Expect<string>(Request.DiffFile).ToBe('');
  finally
    Options.Free;
  end;

  Options := ParseSandboxOptions(['main.js', '--sandbox',
    '--diff-file=out/changes.diff']);
  try
    Request := ResolveSandboxMode(Options, Empty, True, CommandLine);
    Expect<Boolean>(Request.DiffRequested).ToBe(True);
    Expect<Boolean>(Request.DiffFormat = sdfUnified).ToBe(True);
    Expect<string>(Request.DiffFile).ToBe(FRoot + PathDelim + 'project' +
      PathDelim + 'out' + PathDelim + 'changes.diff');
  finally
    Options.Free;
  end;

  Options := ParseSandboxOptions(['main.js', '--sandbox', '--diff=json',
    '--diff-file=changes.patch']);
  try
    Request := ResolveSandboxMode(Options, Empty, True, CommandLine);
    Expect<Boolean>(Request.DiffFormat = sdfJson).ToBe(True);
  finally
    Options.Free;
  end;

  Expect<string>(ResolveError(['main.js', '--sandbox',
    '--diff-file=changes.patch'], Empty, CommandLine)).ToBe('TParseError: ' +
    'Cannot tell the diff format from "changes.patch": name the file .json ' +
    'or .diff, or pass --diff=json or --diff=unified');
  Expect<string>(ResolveError(['main.js', '--sandbox', '--diff=patch'],
    Empty, CommandLine)).ToBe('TParseError: Invalid value for --diff: ' +
    'patch (use --diff=json or --diff=unified)');
  Expect<string>(ResolveError(['main.js', '--sandbox', '--diff='], Empty,
    CommandLine)).ToBe('TParseError: --diff= needs a format: use ' +
    '--diff=json or --diff=unified, or --diff alone for json');
end;

procedure TSandboxModeTests.TestLimits;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
begin
  Options := ParseSandboxOptions(['main.js', '--sandbox']);
  try
    Request := ResolveSandboxMode(Options, Default(TGocciaSandboxRequest),
      True, CommandLine);
    Expect<Int64>(Request.MaxFsBytes).ToBe(16 * 1024 * 1024);
    Expect<Integer>(Request.MaxFsNodes).ToBe(4096);
  finally
    Options.Free;
  end;

  Options := ParseSandboxOptions(['main.js', '--sandbox',
    '--max-fs-bytes=2MiB', '--max-fs-nodes=10']);
  try
    Request := ResolveSandboxMode(Options, Default(TGocciaSandboxRequest),
      True, CommandLine);
    Expect<Int64>(Request.MaxFsBytes).ToBe(2 * 1024 * 1024);
    Expect<Integer>(Request.MaxFsNodes).ToBe(10);
  finally
    Options.Free;
  end;

  Expect<string>(ResolveError(['main.js', '--sandbox', '--max-fs-bytes=0'],
    Default(TGocciaSandboxRequest), CommandLine)).ToBe(
    'TParseError: --max-fs-bytes must be greater than 0.');
end;


procedure TSandboxModeTests.TestConfigEntryYieldsToPositional;
var
  Options: TGocciaSandboxOptions;
  Request: TGocciaSandboxModeRequest;
  Config: TGocciaSandboxRequest;
begin
  Config := ConfigSection('entry-note/goccia.json',
    '{"sandbox": {"entry": "/app/main.js"}}');
  Options := ParseSandboxOptions(['main.js']);
  try
    Request := ResolveSandboxMode(Options, Config, True, CommandLine);
    Expect<string>(Request.EntrySandbox).ToBe('');
    Expect<Integer>(Length(Request.Notes)).ToBe(1);
    Expect<string>(Request.Notes[0]).ToBe('Note: ' + FRoot + PathDelim +
      'entry-note' + PathDelim + 'goccia.json: "sandbox.entry" /app/main.js ' +
      'is not used; the command line names the entry (main.js)');
  finally
    Options.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TSandboxModeTests.Create('CLI Sandbox Mode'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
