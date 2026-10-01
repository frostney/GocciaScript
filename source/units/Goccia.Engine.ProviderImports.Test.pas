program Goccia.Engine.ProviderImports.Test;

{ Provider imports end to end through an engine (ADR 0122): an import-map
  entry naming a `github:` package is granted by an `import` scope,
  materialized from its lockfile pins through a fixture transport, and
  loaded with every file verified against its pin, in both executors. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  SHA256,
  TestingPascalLibrary,
  TextEncoding,

  Goccia.Arguments.Collection,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.Packages.Address,
  Goccia.Packages.Store,
  Goccia.Packages.Transport,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.FFI,
  Goccia.RuntimeExtensions.URL,
  Goccia.RuntimeProfiles.Loader,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

const
  COMMIT = 'abcdefabcdefabcdefabcdefabcdefabcdefabcd';
  PACKAGE_KEY = 'github:frostney/raylib@v1.0.0';
  OTHER_KEY = 'github:someone/other@v2';
  {$IFDEF DARWIN}
  LIBRARY_SUFFIX = '.dylib';
  {$ELSE}
  {$IFDEF MSWINDOWS}
  LIBRARY_SUFFIX = '.dll';
  {$ELSE}
  LIBRARY_SUFFIX = '.so';
  {$ENDIF}
  {$ENDIF}

type
  { A package file that is imported as data rather than compiled. Read is the
    expression that yields Name from the imported namespace `m`. }
  TRuntimeModuleFixture = record
    Path: string;
    Content: string;
    Tampered: string;
    Read: string;
    Name: string;
  end;

const
  { One per file extension that the engine or the loader profile loads as a
    data module. The verify-on-load test fails for such an extension that has
    no fixture here. }
  RUNTIME_MODULE_FIXTURES: array[0..10] of TRuntimeModuleFixture = (
    (Path: 'vendor/config.json'; Content: '{"name": "json"}';
     Tampered: '{"name": "evil"}'; Read: 'm.name'; Name: 'json'),
    (Path: 'vendor/config.toml'; Content: 'name = "toml"';
     Tampered: 'name = "evil"'; Read: 'm.name'; Name: 'toml'),
    (Path: 'vendor/config.yaml'; Content: 'name: yaml';
     Tampered: 'name: evil'; Read: 'm.name'; Name: 'yaml'),
    (Path: 'vendor/config.yml'; Content: 'name: yml';
     Tampered: 'name: evil'; Read: 'm.name'; Name: 'yml'),
    (Path: 'vendor/config.json5'; Content: '{ name: "json5" }';
     Tampered: '{ name: "evil" }'; Read: 'm.name'; Name: 'json5'),
    (Path: 'vendor/config.jsonc'; Content: '{ "name": "jsonc" } // pinned';
     Tampered: '{ "name": "evil" }'; Read: 'm.name'; Name: 'jsonc'),
    (Path: 'vendor/rows.csv'; Content: 'name' + #10 + 'csv';
     Tampered: 'name' + #10 + 'evil'; Read: 'm[0].name'; Name: 'csv'),
    (Path: 'vendor/rows.tsv'; Content: 'name' + #10 + 'tsv';
     Tampered: 'name' + #10 + 'evil'; Read: 'm[0].name'; Name: 'tsv'),
    (Path: 'vendor/rows.jsonl'; Content: '{"name": "jsonl"}';
     Tampered: '{"name": "evil"}'; Read: 'm[0].name'; Name: 'jsonl'),
    (Path: 'vendor/notes.txt'; Content: 'txt';
     Tampered: 'evil'; Read: 'm.content'; Name: 'txt'),
    (Path: 'vendor/notes.md'; Content: 'md';
     Tampered: 'evil'; Read: 'm.content'; Name: 'md'));

type
  TRunOutcome = record
    Result: string;
    ErrorName: string;
    ErrorMessage: string;
    ErrorCapability: string;
    Suggestion: string;
  end;

  TFixtureTransport = class(TGocciaProviderTransport)
  private
    FFiles: TStringList;
    FBinary: TStringList;
    FRequests: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const AURL, AHost: string;
      const AMaxBytes: Integer): TGocciaProviderResponse; override;
    property Requests: Integer read FRequests write FRequests;
  end;

  TProviderImportTests = class(TTestSuite)
  private
    FRoot: string;
    FProject: string;
    FFixtureLibrary: string;
    FTransport: TFixtureTransport;
    FEvents: TStringList;
    FCachedOnly: Boolean;
    FInstallFFI: Boolean;
    FInstallLoaderProfile: Boolean;
    FSwapPath: string;
    FSwapText: string;
    procedure RecordEvent(const AEvent: TGocciaCapabilityAuditEvent);
    function Swap(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ProjectPath(const AName: string): string;
    function CachePath(const AName: string): string;
    function Run(const ASource: string;
      const ACapabilities: TGocciaCapabilities;
      const ABytecode: Boolean): TRunOutcome;
    function HasEvent(const AEvent: string): Boolean;
    function DataModuleExtensions: TStringList;
    procedure WritePackage;
    procedure TestExactEntryRunsWithGrant;
    procedure TestCachedPackageNeedsNoNetwork;
    procedure TestWithoutGrantIsPermissionDenied;
    procedure TestDenyWinsOverAllow;
    procedure TestRepositoryScopeCoversOnlyItsRepository;
    procedure TestPrefixEntryResolvesPinnedFiles;
    procedure TestPrefixCannotEscapeItsPackagePath;
    procedure TestRootEntryResolvesIndex;
    procedure TestPackageImportsOnlyItsOwnFiles;
    procedure TestCacheDirectoryIsNotModuleGraph;
    procedure TestVerifyOnLoadCatchesASwap;
    procedure TestCachedOnlyRefusesTheNetwork;
    procedure TestFFIOpensAVerifiedPackageLibraryByURL;
    procedure TestFFIRefusesATamperedPackageLibrary;
    procedure TestReloadOfATamperedFileReportsTheChange;
    procedure TestVerifyOnLoadCoversEveryDataModuleExtension;
    procedure TestVerifyOnLoadCoversTextAndBytesImports;
    procedure TestImportMetaResolveOfAProviderKeyIsLexical;
    procedure TestComputedImportsThroughAProviderNeedOnlyImport;
    procedure TestLockKeysCompareOwnerAndRepositoryCaseInsensitively;
    procedure TestImportMapAliasesDoNotApplyInsidePackages;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
  public
    procedure SetupTests; override;
  end;

procedure WriteFile(const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  FileUtils.WriteUTF8FileText(APath, AText);
end;

procedure DeleteTree(const APath: string);
var
  SearchRec: TSearchRec;
  EntryPath: string;
begin
  if not DirectoryExists(APath) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
     SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  RemoveDir(APath);
end;

function TextBytes(const AText: string): TBytes;
var
  ErrorOffset: Integer;
begin
  if not TryEncodeUTF8(AText, Result, ErrorOffset) then
    raise Exception.Create('fixture encoding');
end;

{ TFixtureTransport }

constructor TFixtureTransport.Create;
begin
  inherited Create;
  FFiles := TStringList.Create;
  FBinary := TStringList.Create;
end;

destructor TFixtureTransport.Destroy;
begin
  FBinary.Free;
  FFiles.Free;
  inherited;
end;

function TFixtureTransport.Get(const AURL, AHost: string;
  const AMaxBytes: Integer): TGocciaProviderResponse;
var
  Index: Integer;
begin
  Inc(FRequests);
  if AHost <> GITHUB_RAW_HOST then
    raise EGocciaProviderTransportError.Create('wrong host ' + AHost);
  Result.StatusCode := 200;
  Index := FBinary.IndexOfName(AURL);
  if Index >= 0 then
  begin
    Result.Body := ReadFileBytes(FBinary.ValueFromIndex[Index]);
    Exit;
  end;
  Index := FFiles.IndexOfName(AURL);
  if Index < 0 then
  begin
    Result.StatusCode := 404;
    Result.Body := nil;
    Exit;
  end;
  Result.Body := TextBytes(FFiles.ValueFromIndex[Index]);
end;

{ TProviderImportTests }

procedure TProviderImportTests.SetupTests;
begin
  Test('An exact entry runs with an import grant, in both executors',
    TestExactEntryRunsWithGrant);
  Test('A cached package runs without network access',
    TestCachedPackageNeedsNoNetwork);
  Test('Without a grant the import is PermissionDenied and nothing is ' +
    'fetched', TestWithoutGrantIsPermissionDenied);
  Test('An import deny wins over an allow', TestDenyWinsOverAllow);
  Test('A repository scope covers only its repository',
    TestRepositoryScopeCoversOnlyItsRepository);
  Test('A prefix entry resolves pinned files with extension probing',
    TestPrefixEntryResolvesPinnedFiles);
  Test('A prefix tail cannot climb out of the package path',
    TestPrefixCannotEscapeItsPackagePath);
  Test('An entry without a path resolves the package index',
    TestRootEntryResolvesIndex);
  Test('A package imports only its own files',
    TestPackageImportsOnlyItsOwnFiles);
  Test('The .goccia cache is not part of the module graph',
    TestCacheDirectoryIsNotModuleGraph);
  Test('Verify on load refuses a file swapped after verification',
    TestVerifyOnLoadCatchesASwap);
  Test('Cached-only refuses the network', TestCachedOnlyRefusesTheNetwork);
  Test('FFI.open opens a verified package library by file URL',
    TestFFIOpensAVerifiedPackageLibraryByURL);
  Test('FFI.open refuses a tampered package library',
    TestFFIRefusesATamperedPackageLibrary);
  Test('Reloading a file tampered after it was loaded reports the change',
    TestReloadOfATamperedFileReportsTheChange);
  Test('Verify on load covers every data module extension',
    TestVerifyOnLoadCoversEveryDataModuleExtension);
  Test('Verify on load covers text and bytes imports',
    TestVerifyOnLoadCoversTextAndBytesImports);
  Test('import.meta.resolve of a provider key answers with its address',
    TestImportMetaResolveOfAProviderKeyIsLexical);
  Test('A computed import through a provider needs only the import grant',
    TestComputedImportsThroughAProviderNeedOnlyImport);
  Test('Lock keys compare owner and repository case-insensitively',
    TestLockKeysCompareOwnerAndRepositoryCaseInsensitively);
  Test('Import-map aliases do not apply to a package''s imports',
    TestImportMapAliasesDoNotApplyInsidePackages);
end;

procedure TProviderImportTests.BeforeAll;
begin
  inherited BeforeAll;
  FRoot := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-provider-imports-' + IntToStr(GetProcessID);
  FProject := IncludeTrailingPathDelimiter(FRoot) + 'project';
  FFixtureLibrary := ExpandFileName('fixtures' + PathDelim + 'ffi' +
    PathDelim + 'libfixture' + LIBRARY_SUFFIX);
  FEvents := TStringList.Create;
  FTransport := TFixtureTransport.Create;
  WritePackage;
end;

procedure TProviderImportTests.AfterAll;
begin
  FTransport.Free;
  FEvents.Free;
  DeleteTree(FRoot);
  inherited AfterAll;
end;

procedure TProviderImportTests.BeforeEach;
begin
  inherited BeforeEach;
  FEvents.Clear;
  FTransport.Requests := 0;
  FCachedOnly := False;
  FInstallFFI := False;
  FInstallLoaderProfile := False;
  FSwapPath := '';
end;

procedure TProviderImportTests.WritePackage;
var
  Artifacts: string;
  I: Integer;

  procedure AddFile(const APath, AText: string);
  begin
    FTransport.FFiles.Values[PackageArtifactURL('frostney', 'raylib', COMMIT,
      APath)] := AText;
    if Artifacts <> '' then
      Artifacts := Artifacts + ', ';
    Artifacts := Artifacts + '"' + APath + '": {"sha256": "' +
      SHA256Hex(TextBytes(AText)) + '"}';
  end;

  procedure AddBinary(const APath, AFile: string);
  begin
    FTransport.FBinary.Values[PackageArtifactURL('frostney', 'raylib', COMMIT,
      APath)] := AFile;
    Artifacts := Artifacts + ', "' + APath + '": {"sha256": "' +
      SHA256Hex(ReadFileBytes(AFile)) + '"}';
  end;

begin
  Artifacts := '';
  AddFile('bindings/raylib.ts',
    'import { detail } from "./lib/detail.ts";' + sLineBreak +
    'import data from "../vendor/data.json" with { type: "json" };' +
    sLineBreak + 'export const value = detail + ":" + data.name;');
  AddFile('bindings/lib/detail.ts', 'export const detail = "pkg";');
  AddFile('vendor/data.json', '{"name": "data"}');
  AddFile('bindings/late.ts', 'export const late = "late";');
  AddFile('bindings/escape.ts',
    'import { value } from "../../../../../../../lib.js";' + sLineBreak +
    'export const escaped = value;');
  AddFile('bindings/bare.ts',
    'import { value } from "raylib";' + sLineBreak +
    'export const bare = value;');
  AddFile('bindings/native.ts',
    'export const open = () => FFI.open(new URL("../native/libfixture" + ' +
    'FFI.suffix, import.meta.url));');
  AddFile('index.ts', 'export const root = "root";');
  { Files imported as data, one per extension that loads that way. }
  for I := Low(RUNTIME_MODULE_FIXTURES) to High(RUNTIME_MODULE_FIXTURES) do
    AddFile(RUNTIME_MODULE_FIXTURES[I].Path,
      RUNTIME_MODULE_FIXTURES[I].Content);
  AddFile('bindings/dyn.ts',
    'export const load = (name) => import("./" + name);');
  { Relative specifiers that leave the package and land where a path key of
    the project's import map would catch them. }
  AddFile('bindings/viaalias.ts',
    'import { value } from "../../../../../../../src/x.js";' + sLineBreak +
    'export const viaAlias = value;');
  AddFile('bindings/viagithub.ts',
    'export * from "../../../../../../../other/x.ts";');
  if FileExists(FFixtureLibrary) then
    AddBinary('native/libfixture' + LIBRARY_SUFFIX, FFixtureLibrary);

  WriteFile(ProjectPath('goccia.json'),
    '{"imports": {' +
    '"raylib": "github:frostney/raylib@v1.0.0/bindings/raylib.ts", ' +
    '"ray/": "github:frostney/raylib@v1.0.0/bindings/", ' +
    '"rayroot": "github:frostney/raylib@v1.0.0", ' +
    '"raypkg/": "github:frostney/raylib@v1.0.0/", ' +
    '"./src/": "./src/", ' +
    '"./other/": "github:someone/other@v2/", ' +
    '"other": "github:someone/other@v2/x.ts"}}');
  WriteFile(ProjectPath('goccia.lock.json'),
    '{"version": 1, "packages": {' +
    '"' + PACKAGE_KEY + '": {"ref": "tag", "commit": "' + COMMIT +
    '", "artifacts": {' + Artifacts + '}}, ' +
    '"' + OTHER_KEY + '": {"ref": "tag", "commit": "' + COMMIT +
    '", "artifacts": {"x.ts": {"sha256": "' +
    SHA256Hex(TextBytes('export {};')) + '"}}}}}');
  WriteFile(ProjectPath('lib.js'), 'export const value = "inside";');
  WriteFile(ProjectPath('src/x.js'), 'export const value = "project";');
  WriteFile(ProjectPath('.goccia/stray.js'), 'export const value = "stray";');
end;

function TProviderImportTests.ProjectPath(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FProject) +
    StringReplace(AName, '/', PathDelim, [rfReplaceAll]);
end;

function TProviderImportTests.CachePath(const AName: string): string;
begin
  Result := ProjectPath('.goccia/packages/github/frostney/raylib/' + COMMIT +
    '/' + AName);
end;

procedure TProviderImportTests.RecordEvent(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  FEvents.Add(CapabilityKindName(AEvent.Kind) + '|' +
    CapabilityDecisionName(AEvent.Decision) + '|' + AEvent.Subject);
end;

function TProviderImportTests.HasEvent(const AEvent: string): Boolean;
begin
  Result := FEvents.IndexOf(AEvent) >= 0;
end;

function TProviderImportTests.Swap(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if FSwapPath <> '' then
  begin
    WriteFile(FSwapPath, FSwapText);
    { Later than anything the loader recorded, whatever the filesystem's
      timestamp resolution. }
    FileSetDate(FSwapPath, DateTimeToFileDate(Now + 1));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure CaptureThrown(const AValue: TGocciaValue; var AOutcome: TRunOutcome);
var
  ErrorObject: TGocciaObjectValue;
begin
  if not (AValue is TGocciaObjectValue) then
  begin
    AOutcome.ErrorMessage := AValue.ToStringLiteral.Value;
    Exit;
  end;
  ErrorObject := TGocciaObjectValue(AValue);
  AOutcome.ErrorName := ErrorObject.GetProperty('name').ToStringLiteral.Value;
  AOutcome.ErrorMessage :=
    ErrorObject.GetProperty('message').ToStringLiteral.Value;
  AOutcome.ErrorCapability :=
    ErrorObject.GetProperty('capability').ToStringLiteral.Value;
end;

function TProviderImportTests.Run(const ASource: string;
  const ACapabilities: TGocciaCapabilities;
  const ABytecode: Boolean): TRunOutcome;
var
  Source: TStringList;
  Executor: TGocciaExecutor;
  Runtime: TGocciaRuntimeCore;
  Engine: TGocciaEngine;
  ResultValue: TGocciaValue;
begin
  Result := Default(TRunOutcome);
  Source := TStringList.Create;
  Source.Text := ASource;
  if ABytecode then
    Executor := TGocciaBytecodeExecutor.Create
  else
    Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.mjs'), Source, Executor,
    ACapabilities);
  try
    Engine.CapabilityAuditSink := RecordEvent;
    Runtime := AttachRuntime(Engine);
    if FInstallFFI then
    begin
      Runtime.Install(TGocciaURLRuntimeExtension.Create);
      InstallFFIIfGranted(Runtime);
    end;
    if FInstallLoaderProfile then
      ApplyLoaderRuntimeProfile(Runtime);
    Engine.RegisterGlobal('swap',
      TGocciaNativeFunctionValue.Create(Swap, 'swap', 0));
    Engine.Resolver.ProviderTransport := FTransport;
    Engine.Resolver.ProviderCachedOnly := FCachedOnly;
    Engine.Resolver.LoadImportMap(ProjectPath('goccia.json'));
    try
      Engine.Execute;
      Engine.WaitForRuntimeIdle;
    except
      on E: TGocciaThrowValue do
      begin
        CaptureThrown(E.Value, Result);
        Result.Suggestion := E.Suggestion;
      end;
      on E: EGocciaBytecodeThrow do
      begin
        CaptureThrown(E.ThrownValue, Result);
        Result.Suggestion := E.Suggestion;
      end;
      on E: Exception do
        Result.ErrorMessage := E.Message;
    end;
    ResultValue := TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('result');
    if Assigned(ResultValue) then
      Result.Result := ResultValue.ToStringLiteral.Value;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TProviderImportTests.TestExactEntryRunsWithGrant;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
  begin
    FEvents.Clear;
    Outcome := Run('import { value } from "raylib"; globalThis.result = value;',
      TGocciaCapabilities.None.Allow(gcImport, 'github:frostney'), Bytecode);
    Expect<string>(Outcome.ErrorMessage).ToBe('');
    Expect<string>(Outcome.Result).ToBe('pkg:data');
    Expect<Boolean>(HasEvent('import.provider|allow|' + PACKAGE_KEY))
      .ToBe(True);
    Expect<Boolean>(HasEvent('import.provider|allow|' + PACKAGE_KEY +
      '/bindings/lib/detail.ts')).ToBe(True);
    { Package files are the module graph: no read decision is made. }
    Expect<Boolean>(Pos('read.file', FEvents.Text) = 0).ToBe(True);
  end;
end;

procedure TProviderImportTests.TestCachedPackageNeedsNoNetwork;
var
  Outcome: TRunOutcome;
begin
  Run('import { value } from "raylib";',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), False);
  FTransport.Requests := 0;
  Outcome := Run('import { value } from "raylib"; globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), True);
  Expect<string>(Outcome.Result).ToBe('pkg:data');
  Expect<Integer>(FTransport.Requests).ToBe(0);
end;

procedure TProviderImportTests.TestWithoutGrantIsPermissionDenied;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  DeleteTree(ProjectPath('.goccia/packages'));
  for Bytecode := False to True do
  begin
    FEvents.Clear;
    Outcome := Run('import { value } from "raylib"; globalThis.result = value;',
      TGocciaCapabilities.None, Bytecode);
    Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
    Expect<string>(Outcome.ErrorMessage).ToBe('import: ' + PACKAGE_KEY);
    Expect<string>(Outcome.ErrorCapability).ToBe('import');
    Expect<Boolean>(Pos('--allow-import=github:frostney/raylib',
      Outcome.Suggestion) > 0).ToBe(True);
    Expect<Boolean>(HasEvent('import.provider|deny|' + PACKAGE_KEY))
      .ToBe(True);
  end;
  { Refused before anything was fetched or written. }
  Expect<Integer>(FTransport.Requests).ToBe(0);
  Expect<Boolean>(DirectoryExists(ProjectPath('.goccia/packages')))
    .ToBe(False);
end;

procedure TProviderImportTests.TestDenyWinsOverAllow;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { value } from "raylib";',
    TGocciaCapabilities.None.Allow(gcImport, 'github')
      .Deny(gcImport, 'github:frostney'), False);
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.Suggestion).ToBe(
    'refused by the import deny github:frostney');
end;

procedure TProviderImportTests.TestRepositoryScopeCoversOnlyItsRepository;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import "other";',
    TGocciaCapabilities.None.Allow(gcImport, 'github:frostney/raylib'), False);
  Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
  Expect<string>(Outcome.ErrorMessage).ToBe('import: ' + OTHER_KEY);
end;

procedure TProviderImportTests.TestPrefixEntryResolvesPinnedFiles;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
  begin
    Outcome := Run('import { late } from "ray/late.ts";' + sLineBreak +
      'import { detail } from "ray/lib/detail";' + sLineBreak +
      'globalThis.result = late + ":" + detail;',
      TGocciaCapabilities.None.Allow(gcImport, 'github:frostney/raylib'),
      Bytecode);
    Expect<string>(Outcome.ErrorMessage).ToBe('');
    Expect<string>(Outcome.Result).ToBe('late:pkg');
  end;
  { A file the lockfile does not pin does not exist for resolution, even
    when it is on disk. }
  WriteFile(CachePath('bindings/planted.ts'), 'export const x = 1;');
  Outcome := Run('import { x } from "ray/planted.ts";',
    TGocciaCapabilities.None.Allow(gcImport, 'github:frostney/raylib'), False);
  Expect<Boolean>(Pos('Module not found', Outcome.ErrorMessage) > 0)
    .ToBe(True);
  DeleteFile(CachePath('bindings/planted.ts'));
end;

procedure TProviderImportTests.TestPrefixCannotEscapeItsPackagePath;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import data from "ray/../vendor/data.json" with ' +
    '{ type: "json" };',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), False);
  Expect<Boolean>(Outcome.ErrorMessage <> '').ToBe(True);
  Expect<Integer>(FTransport.Requests).ToBe(0);
end;

procedure TProviderImportTests.TestRootEntryResolvesIndex;
var
  Outcome: TRunOutcome;
begin
  Outcome := Run('import { root } from "rayroot"; globalThis.result = root;',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), True);
  Expect<string>(Outcome.ErrorMessage).ToBe('');
  Expect<string>(Outcome.Result).ToBe('root');
end;

procedure TProviderImportTests.TestPackageImportsOnlyItsOwnFiles;
var
  Outcome: TRunOutcome;
begin
  { ../../../../../../../lib.js from the package names the project's own
    lib.js; a package may not reach it. }
  Outcome := Run('import { escaped } from "ray/escape.ts";',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), False);
  Expect<Boolean>(Pos('imports only its own files', Outcome.ErrorMessage) > 0)
    .ToBe(True);
  Outcome := Run('import { bare } from "ray/bare.ts";',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), True);
  Expect<Boolean>(Pos('imports only its own files', Outcome.ErrorMessage) > 0)
    .ToBe(True);
end;

procedure TProviderImportTests.TestCacheDirectoryIsNotModuleGraph;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
  begin
    Outcome := Run('import { value } from "./.goccia/stray.js";' + sLineBreak +
      'globalThis.result = value;', TGocciaCapabilities.None, Bytecode);
    Expect<string>(Outcome.ErrorName).ToBe('PermissionDenied');
    Expect<string>(Outcome.ErrorMessage).ToBe('read: ./.goccia/stray.js');
  end;
  { With a read grant it is an ordinary read. }
  Outcome := Run('import { value } from "./.goccia/stray.js";' + sLineBreak +
    'globalThis.result = value;',
    TGocciaCapabilities.None.Allow(gcRead, ProjectPath('.goccia')), False);
  Expect<string>(Outcome.Result).ToBe('stray');
end;

procedure TProviderImportTests.TestVerifyOnLoadCatchesASwap;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
  begin
    { The package is verified when "raylib" resolves; late.ts is swapped
      after that and before the dynamic import loads it. }
    FSwapPath := CachePath('bindings/late.ts');
    FSwapText := 'export const late = "evil";';
    Outcome := Run('import { value } from "raylib";' + sLineBreak +
      'swap();' + sLineBreak +
      'try { const { late } = await import("ray/late.ts");' +
      ' globalThis.result = late; }' + sLineBreak +
      'catch (error) { globalThis.result = error.message; }',
      TGocciaCapabilities.None.Allow(gcImport, 'github'), Bytecode);
    Expect<string>(Outcome.Result).ToBe('Provider package file ' +
      PACKAGE_KEY + '/bindings/late.ts changed after it was verified');
    Expect<Boolean>(HasEvent('import.provider|deny|' + PACKAGE_KEY +
      '/bindings/late.ts')).ToBe(True);
  end;
end;

procedure TProviderImportTests.TestCachedOnlyRefusesTheNetwork;
var
  Outcome: TRunOutcome;
begin
  DeleteTree(ProjectPath('.goccia/packages'));
  FCachedOnly := True;
  Outcome := Run('import { value } from "raylib";',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), False);
  Expect<Boolean>(Pos('--cached-only', Outcome.ErrorMessage) > 0).ToBe(True);
  Expect<Integer>(FTransport.Requests).ToBe(0);
end;

procedure TProviderImportTests.TestFFIOpensAVerifiedPackageLibraryByURL;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  if not FileExists(FFixtureLibrary) then
  begin
    Fail('FFI fixture not found: ' + FFixtureLibrary +
      ' (build it with ./build.pas testrunner or fixtures/ffi/build.sh)');
    Exit;
  end;
  FInstallFFI := True;
  for Bytecode := False to True do
  begin
    Outcome := Run('import { open } from "ray/native.ts";' + sLineBreak +
      'const lib = open(); globalThis.result = lib.closed; lib.close();',
      TGocciaCapabilities.None.Allow(gcImport, 'github:frostney')
        .Allow(gcFFI, ProjectPath('.goccia')), Bytecode);
    Expect<string>(Outcome.ErrorMessage).ToBe('');
    Expect<string>(Outcome.Result).ToBe('false');
  end;
  { Importing never implies FFI: without an ffi grant the global is absent. }
  Outcome := Run('import { open } from "ray/native.ts"; open();',
    TGocciaCapabilities.None.Allow(gcImport, 'github:frostney'), False);
  Expect<Boolean>(Pos('FFI', Outcome.ErrorMessage) > 0).ToBe(True);
end;

procedure TProviderImportTests.TestFFIRefusesATamperedPackageLibrary;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
  Tampered: TBytes;
begin
  if not FileExists(FFixtureLibrary) then
  begin
    Fail('FFI fixture not found: ' + FFixtureLibrary);
    Exit;
  end;
  FInstallFFI := True;
  Tampered := ReadFileBytes(FFixtureLibrary);
  SetLength(Tampered, Length(Tampered) + 1);
  Tampered[High(Tampered)] := 0;
  for Bytecode := False to True do
  begin
    { Materialize, then swap the library before FFI.open hashes it. }
    FSwapPath := CachePath('native/libfixture' + LIBRARY_SUFFIX);
    FSwapText := 'not the pinned library';
    FInstallFFI := True;
    Outcome := Run('import { open } from "ray/native.ts";' + sLineBreak +
      'swap();' + sLineBreak +
      'try { open(); globalThis.result = "opened"; }' +
      ' catch (error) { globalThis.result = error.message; }',
      TGocciaCapabilities.None.Allow(gcImport, 'github:frostney')
        .Allow(gcFFI, ProjectPath('.goccia')), Bytecode);
    Expect<Boolean>(Pos('changed after it was verified', Outcome.Result) > 0)
      .ToBe(True);
  end;
end;

{ A module already loaded, tampered with, and imported again is reloaded by
  its own resolved path; the reload must report the change, not blame the
  package for importing a host path. }
procedure TProviderImportTests.TestReloadOfATamperedFileReportsTheChange;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
  begin
    FSwapPath := CachePath('vendor/data.json');
    FSwapText := '{"name": "evil"}';
    Outcome := Run('import { value } from "raylib";' + sLineBreak +
      'swap();' + sLineBreak +
      'try { const m = await import("raypkg/vendor/data.json", ' +
      '{ with: { type: "json" } }); globalThis.result = m.default.name; }' +
      sLineBreak + 'catch (error) { globalThis.result = error.message; }',
      TGocciaCapabilities.None.Allow(gcImport, 'github'), Bytecode);
    Expect<string>(Outcome.Result).ToBe('Provider package file ' +
      PACKAGE_KEY + '/vendor/data.json changed after it was verified');
    WriteFile(CachePath('vendor/data.json'), '{"name": "data"}');
  end;
end;

{ The file extensions that load as data modules: the engine's own that are
  not scripts, and those the loader profile's runtime extensions register.
  The caller owns the list. }
function TProviderImportTests.DataModuleExtensions: TStringList;
var
  Engine: TGocciaEngine;
  Executor: TGocciaExecutor;
  I: Integer;
  Runtime: TGocciaRuntimeCore;
  Source: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  Source := TStringList.Create;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := TGocciaEngine.Create(ProjectPath('app.mjs'), Source, Executor,
    TGocciaCapabilities.None);
  try
    try
      for I := Low(EngineModuleImportExtensions) to
          High(EngineModuleImportExtensions) do
        if not IsScriptExtension(EngineModuleImportExtensions[I]) then
          Result.Add(EngineModuleImportExtensions[I]);
      Runtime := AttachRuntime(Engine);
      ApplyLoaderRuntimeProfile(Runtime);
      Runtime.CollectModuleExtensions(Result);
    except
      Result.Free;
      raise;
    end;
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

{ A file imported as data is parsed from the bytes verified against the pin,
  like every other package file, whether a runtime extension loads it or the
  module loader does. The cases come from the extensions that load this way,
  so a loader added to the loader profile fails here until it has a fixture,
  and then has to pass. }
procedure TProviderImportTests.TestVerifyOnLoadCoversEveryDataModuleExtension;
var
  Bytecode: Boolean;
  Covered: TStringList;
  Extension: string;
  Extensions: TStringList;
  Failures: TStringList;
  Fixture: TRuntimeModuleFixture;
  I: Integer;
  Mode: string;
  Outcome: TRunOutcome;
begin
  Failures := TStringList.Create;
  try
    Extensions := DataModuleExtensions;
    Covered := TStringList.Create;
    try
      for I := Low(RUNTIME_MODULE_FIXTURES) to High(RUNTIME_MODULE_FIXTURES) do
      begin
        Extension := ExtractFileExt(RUNTIME_MODULE_FIXTURES[I].Path);
        Covered.Add(Extension);
        { A fixture for an extension nothing loads as data proves nothing. }
        if Extensions.IndexOf(Extension) < 0 then
          Failures.Add('Nothing loads ' + Extension +
            ' as a data module; its fixture tests nothing');
      end;
      for I := 0 to Extensions.Count - 1 do
        if Covered.IndexOf(Extensions[I]) < 0 then
          Failures.Add('No verify-on-load fixture for the module extension ' +
            Extensions[I] + '; add one to RUNTIME_MODULE_FIXTURES');
    finally
      Covered.Free;
      Extensions.Free;
    end;

    FInstallLoaderProfile := True;
    for Bytecode := False to True do
      for I := Low(RUNTIME_MODULE_FIXTURES) to High(RUNTIME_MODULE_FIXTURES) do
      begin
        Fixture := RUNTIME_MODULE_FIXTURES[I];
        if Bytecode then
          Mode := ' (bytecode)'
        else
          Mode := ' (interpreted)';
        { Untampered, the pinned bytes load. }
        FSwapPath := '';
        Outcome := Run('import { value } from "raylib";' + sLineBreak +
          'const m = await import("raypkg/' + Fixture.Path + '");' +
          sLineBreak + 'globalThis.result = ' + Fixture.Read + ';',
          TGocciaCapabilities.None.Allow(gcImport, 'github'), Bytecode);
        if (Outcome.ErrorMessage <> '') or (Outcome.Result <> Fixture.Name) then
          Failures.Add(Fixture.Path + Mode +
            ' did not load its pinned content: got "' + Outcome.Result +
            '", error "' + Outcome.ErrorMessage + '"');

        { Tampered after materialization, it is refused before parsing. }
        FSwapPath := CachePath(Fixture.Path);
        FSwapText := Fixture.Tampered;
        Outcome := Run('import { value } from "raylib";' + sLineBreak +
          'swap();' + sLineBreak +
          'try { const m = await import("raypkg/' + Fixture.Path + '");' +
          sLineBreak + 'globalThis.result = ' + Fixture.Read + '; }' +
          sLineBreak + 'catch (error) { globalThis.result = error.message; }',
          TGocciaCapabilities.None.Allow(gcImport, 'github'), Bytecode);
        if Outcome.Result <> 'Provider package file ' + PACKAGE_KEY + '/' +
           Fixture.Path + ' changed after it was verified' then
          Failures.Add(Fixture.Path + Mode +
            ' was not refused after it changed: got "' + Outcome.Result + '"');
        { Materialize afresh for the next file. }
        DeleteTree(ProjectPath('.goccia/packages'));
      end;

    { Fail carries the list; Expect could only report its length. }
    if Failures.Count > 0 then
      Fail(Trim(Failures.Text));
    Expect<Integer>(Failures.Count).ToBe(0);
  finally
    Failures.Free;
  end;
end;

{ An import attribute picks the loader whatever the file's extension, and
  `bytes` reads through its own path. }
procedure TProviderImportTests.TestVerifyOnLoadCoversTextAndBytesImports;
const
  ATTRIBUTE_TYPES: array[0..1] of string = ('text', 'bytes');
var
  Bytecode: Boolean;
  I: Integer;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
    for I := Low(ATTRIBUTE_TYPES) to High(ATTRIBUTE_TYPES) do
    begin
      FSwapPath := CachePath('bindings/late.ts');
      FSwapText := 'export const late = "evil";';
      Outcome := Run('import { value } from "raylib";' + sLineBreak +
        'swap();' + sLineBreak +
        'try { const m = await import("ray/late.ts", { with: { type: "' +
        ATTRIBUTE_TYPES[I] + '" } }); globalThis.result = "loaded " + ' +
        'm.default.length; }' + sLineBreak +
        'catch (error) { globalThis.result = error.message; }',
        TGocciaCapabilities.None.Allow(gcImport, 'github'), Bytecode);
      Expect<string>(Outcome.Result).ToBe('Provider package file ' +
        PACKAGE_KEY + '/bindings/late.ts changed after it was verified');
      DeleteTree(ProjectPath('.goccia/packages'));
    end;
end;

procedure TProviderImportTests.TestImportMetaResolveOfAProviderKeyIsLexical;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  DeleteTree(ProjectPath('.goccia/packages'));
  for Bytecode := False to True do
  begin
    FEvents.Clear;
    Outcome := Run('globalThis.result = import.meta.resolve("raylib") + ' +
      '"|" + import.meta.resolve("ray/lib/detail.ts");',
      TGocciaCapabilities.None, Bytecode);
    Expect<string>(Outcome.ErrorMessage).ToBe('');
    Expect<string>(Outcome.Result).ToBe(
      'github:frostney/raylib@v1.0.0/bindings/raylib.ts|' +
      'github:frostney/raylib@v1.0.0/bindings/lib/detail.ts');
    Expect<Boolean>(Pos('import.provider', FEvents.Text) = 0).ToBe(True);
  end;
  Expect<Integer>(FTransport.Requests).ToBe(0);
end;

procedure TProviderImportTests.TestComputedImportsThroughAProviderNeedOnlyImport;
const
  SOURCE_TEXT =
    'const key = "ray" + "/late.ts";' + sLineBreak +
    'const { late } = await import(key);' + sLineBreak +
    'const { load } = await import("ray/" + "dyn.ts");' + sLineBreak +
    'const again = await load("late" + ".ts");' + sLineBreak +
    'globalThis.result = late + ":" + again.late;';
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
  begin
    FEvents.Clear;
    Outcome := Run(SOURCE_TEXT,
      TGocciaCapabilities.None.Allow(gcImport, 'github:frostney'), Bytecode);
    Expect<string>(Outcome.ErrorMessage).ToBe('');
    Expect<string>(Outcome.Result).ToBe('late:late');
    Expect<Boolean>(Pos('read.file', FEvents.Text) = 0).ToBe(True);

    Outcome := Run('try { await import("ray" + "/late.ts"); }' + sLineBreak +
      'catch (error) { globalThis.result = error.name + " " + ' +
      'error.message; }', TGocciaCapabilities.None, Bytecode);
    Expect<string>(Outcome.Result).ToBe('PermissionDenied import: ' +
      PACKAGE_KEY);
  end;
  { A computed path into the cache from project code is still a read. }
  Outcome := Run('try { await import("./.goccia/packages/github/frostney/' +
    'raylib/" + "' + COMMIT + '/bindings/late.ts"); }' + sLineBreak +
    'catch (error) { globalThis.result = error.name; }',
    TGocciaCapabilities.None.Allow(gcImport, 'github'), False);
  Expect<string>(Outcome.Result).ToBe('PermissionDenied');
end;

procedure TProviderImportTests.TestLockKeysCompareOwnerAndRepositoryCaseInsensitively;
var
  Lock: string;
  Outcome: TRunOutcome;
begin
  Lock := ReadUTF8FileText(ProjectPath('goccia.lock.json'));
  try
    WriteFile(ProjectPath('goccia.lock.json'), StringReplace(Lock,
      '"' + PACKAGE_KEY + '"', '"github:Frostney/RayLib@v1.0.0"', []));
    Outcome := Run('import { value } from "raylib"; globalThis.result = value;',
      TGocciaCapabilities.None.Allow(gcImport, 'github'), False);
    Expect<string>(Outcome.ErrorMessage).ToBe('');
    Expect<string>(Outcome.Result).ToBe('pkg:data');
    { The ref still compares exactly. }
    WriteFile(ProjectPath('goccia.lock.json'), StringReplace(Lock,
      '"' + PACKAGE_KEY + '"', '"github:frostney/raylib@V1.0.0"', []));
    Outcome := Run('import { value } from "raylib";',
      TGocciaCapabilities.None.Allow(gcImport, 'github'), False);
    Expect<Boolean>(Pos('is not pinned', Outcome.ErrorMessage) > 0)
      .ToBe(True);
  finally
    WriteFile(ProjectPath('goccia.lock.json'), Lock);
  end;
end;

procedure TProviderImportTests.TestImportMapAliasesDoNotApplyInsidePackages;
var
  Bytecode: Boolean;
  Outcome: TRunOutcome;
begin
  for Bytecode := False to True do
  begin
    Outcome := Run('import { viaAlias } from "ray/viaalias.ts";' +
      sLineBreak + 'globalThis.result = viaAlias;',
      TGocciaCapabilities.None.Allow(gcImport, 'github'), Bytecode);
    Expect<Boolean>(Pos('imports only its own files', Outcome.ErrorMessage) >
      0).ToBe(True);
    Expect<string>(Outcome.Result).ToBe('undefined');

    FEvents.Clear;
    Outcome := Run('import * as other from "ray/viagithub.ts";' +
      sLineBreak + 'globalThis.result = "loaded";',
      TGocciaCapabilities.None.Allow(gcImport, 'github'), Bytecode);
    Expect<Boolean>(Pos('imports only its own files', Outcome.ErrorMessage) >
      0).ToBe(True);
    { The second package was never granted, fetched, or loaded. }
    Expect<Boolean>(Pos(OTHER_KEY, FEvents.Text) = 0).ToBe(True);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TProviderImportTests.Create('Provider imports'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
