program Goccia.Modules.Resolver.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  FileUtils,
  TestingPascalLibrary,

  Goccia.CapabilityAudit,
  Goccia.Modules.Resolver,
  Goccia.TestSetup;

const
  PACKAGE_REFERENCE = 'github:frostney/GocciaScript-Raylib@v0.10.0';

type
  TFixtureRemotePackageResolver = class(TGocciaRemotePackageResolver)
  private
    FResolveCount: Integer;
    FResolvedPath: string;
  public
    function ResolvePackage(const AReference,
      AImportMapPath: string): string; override;
    property ResolveCount: Integer read FResolveCount;
    property ResolvedPath: string read FResolvedPath write FResolvedPath;
  end;

  TModuleResolverTests = class(TTestSuite)
  private
    FAuditDecision: TGocciaCapabilityDecision;
    FAuditKind: TGocciaCapabilityKind;
    FAuditReason: string;
    FAuditSubject: string;
    FAuditCount: Integer;
    FTempDirectories: TStringList;
    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure RecordAudit(const AKind: TGocciaCapabilityKind;
      const ADecision: TGocciaCapabilityDecision;
      const ASubject, AReason: string);
    procedure ResetAudit;
    procedure WriteTextFile(const APath, AText: string);
    procedure TestMixedLocalAndRemoteImportMap;
    procedure TestDeniesRemoteEntryBeforeConsultingCache;
    procedure TestRequiresCapabilityAgainForCachedEntry;
    procedure TestRejectsRawHTTPSImportMapAddress;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

function TFixtureRemotePackageResolver.ResolvePackage(
  const AReference, AImportMapPath: string): string;
begin
  Inc(FResolveCount);
  if AReference <> PACKAGE_REFERENCE then
    raise Exception.Create('unexpected package reference');
  if AImportMapPath = '' then
    raise Exception.Create('missing import map path');
  Result := FResolvedPath;
end;

procedure TModuleResolverTests.SetupTests;
begin
  Test('One import map resolves local and remote entries',
    TestMixedLocalAndRemoteImportMap);
  Test('Remote imports are denied before consulting a cache',
    TestDeniesRemoteEntryBeforeConsultingCache);
  Test('Cached remote entries require the capability again',
    TestRequiresCapabilityAgainForCachedEntry);
  Test('Raw HTTPS import map addresses remain unsupported',
    TestRejectsRawHTTPSImportMapAddress);
end;

procedure TModuleResolverTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FTempDirectories := TStringList.Create;
end;

procedure TModuleResolverTests.AfterAll;
var
  I: Integer;
begin
  for I := 0 to FTempDirectories.Count - 1 do
    DeleteDirectoryTree(FTempDirectories[I]);
  FTempDirectories.Free;
  inherited AfterAll;
end;

function TModuleResolverTests.CreateTempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-module-resolver-' + IntToStr(Random(MaxInt));
  ForceDirectories(Result);
  FTempDirectories.Add(Result);
end;

procedure TModuleResolverTests.DeleteDirectoryTree(const APath: string);
var
  EntryPath: string;
  SearchRecord: TSearchRec;
begin
  if not DirectoryExists(APath) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
    SearchRecord) = 0 then
  begin
    repeat
      if (SearchRecord.Name = '.') or (SearchRecord.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRecord.Name;
      if (SearchRecord.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRecord) <> 0;
    FindClose(SearchRecord);
  end;
  RemoveDir(APath);
end;

procedure TModuleResolverTests.WriteTextFile(
  const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  WriteUTF8FileText(APath, AText);
end;

procedure TModuleResolverTests.RecordAudit(
  const AKind: TGocciaCapabilityKind;
  const ADecision: TGocciaCapabilityDecision;
  const ASubject, AReason: string);
begin
  Inc(FAuditCount);
  FAuditKind := AKind;
  FAuditDecision := ADecision;
  FAuditSubject := ASubject;
  FAuditReason := AReason;
end;

procedure TModuleResolverTests.ResetAudit;
begin
  FAuditCount := 0;
  FAuditKind := gckFetchHost;
  FAuditDecision := gcdDeny;
  FAuditSubject := '';
  FAuditReason := '';
end;

procedure TModuleResolverTests.TestMixedLocalAndRemoteImportMap;
var
  EntryPath, ImportMapPath, LocalPath, ProjectDirectory,
    RemotePath: string;
  RemoteResolver: TFixtureRemotePackageResolver;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  EntryPath := IncludeTrailingPathDelimiter(ProjectDirectory) + 'app.js';
  LocalPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'local.js';
  RemotePath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    '.goccia' + PathDelim + 'remote.js';
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(EntryPath, 'export {};');
  WriteTextFile(LocalPath, 'export const local = true;');
  WriteTextFile(RemotePath, 'export const remote = true;');
  WriteTextFile(ImportMapPath,
    '{"imports":{"local-package":"./local.js",' +
    '"remote-package":"' + PACKAGE_REFERENCE + '"}}');

  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  RemoteResolver := TFixtureRemotePackageResolver.Create;
  try
    RemoteResolver.ResolvedPath := RemotePath;
    Resolver.RemotePackageResolver := RemoteResolver;
    Resolver.RemoteImportsEnabled := True;
    Resolver.CapabilityAuditEmitter := RecordAudit;
    ResetAudit;
    Resolver.LoadImportMap(ImportMapPath);

    Expect<string>(Resolver.Resolve(
      'local-package', EntryPath)).ToBe(LocalPath);
    Expect<string>(Resolver.Resolve(
      'remote-package', EntryPath)).ToBe(RemotePath);
    Expect<Integer>(RemoteResolver.ResolveCount).ToBe(1);
    Expect<Integer>(FAuditCount).ToBe(1);
    Expect<string>(CapabilityKindName(FAuditKind)).ToBe(
      'remote-import.resolve');
    Expect<Boolean>(FAuditDecision = gcdAllow).ToBe(True);
    Expect<string>(FAuditSubject).ToBe(PACKAGE_REFERENCE);
  finally
    Resolver.Free;
    RemoteResolver.Free;
  end;
end;

procedure TModuleResolverTests.TestDeniesRemoteEntryBeforeConsultingCache;
var
  ErrorRaised: Boolean;
  ImportMapPath, ProjectDirectory, RemotePath: string;
  RemoteResolver: TFixtureRemotePackageResolver;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  RemotePath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    '.goccia' + PathDelim + 'remote.js';
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(RemotePath, 'export const remote = true;');
  WriteTextFile(ImportMapPath,
    '{"imports":{"remote-package":"' + PACKAGE_REFERENCE + '"}}');

  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  RemoteResolver := TFixtureRemotePackageResolver.Create;
  try
    RemoteResolver.ResolvedPath := RemotePath;
    Resolver.RemotePackageResolver := RemoteResolver;
    Resolver.RemoteImportsEnabled := False;
    Resolver.CapabilityAuditEmitter := RecordAudit;
    ResetAudit;
    ErrorRaised := False;
    try
      Resolver.LoadImportMap(ImportMapPath);
    except
      on Exception do
        ErrorRaised := True;
    end;

    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Integer>(RemoteResolver.ResolveCount).ToBe(0);
    Expect<Integer>(FAuditCount).ToBe(1);
    Expect<Boolean>(FAuditDecision = gcdDeny).ToBe(True);
    Expect<string>(FAuditSubject).ToBe(PACKAGE_REFERENCE);
  finally
    Resolver.Free;
    RemoteResolver.Free;
  end;
end;

procedure TModuleResolverTests.TestRequiresCapabilityAgainForCachedEntry;
var
  ErrorRaised: Boolean;
  ImportMapPath, ProjectDirectory, RemotePath: string;
  RemoteResolver: TFixtureRemotePackageResolver;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  RemotePath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    '.goccia' + PathDelim + 'remote.js';
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(RemotePath, 'export const remote = true;');
  WriteTextFile(ImportMapPath,
    '{"imports":{"remote-package":"' + PACKAGE_REFERENCE + '"}}');

  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  RemoteResolver := TFixtureRemotePackageResolver.Create;
  try
    RemoteResolver.ResolvedPath := RemotePath;
    Resolver.RemotePackageResolver := RemoteResolver;
    Resolver.RemoteImportsEnabled := True;
    Resolver.LoadImportMap(ImportMapPath);
    Expect<Integer>(RemoteResolver.ResolveCount).ToBe(1);

    Resolver.RemoteImportsEnabled := False;
    Resolver.CapabilityAuditEmitter := RecordAudit;
    ResetAudit;
    ErrorRaised := False;
    try
      Resolver.LoadImportMap(ImportMapPath);
    except
      on Exception do
        ErrorRaised := True;
    end;
    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Integer>(RemoteResolver.ResolveCount).ToBe(1);
    Expect<Integer>(FAuditCount).ToBe(1);
    Expect<Boolean>(FAuditDecision = gcdDeny).ToBe(True);
  finally
    Resolver.Free;
    RemoteResolver.Free;
  end;
end;

procedure TModuleResolverTests.TestRejectsRawHTTPSImportMapAddress;
var
  ErrorRaised: Boolean;
  ImportMapPath, ProjectDirectory: string;
  RemoteResolver: TFixtureRemotePackageResolver;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json';
  WriteTextFile(ImportMapPath,
    '{"imports":{"raw":"https://example.com/package.js"}}');
  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  RemoteResolver := TFixtureRemotePackageResolver.Create;
  try
    Resolver.RemotePackageResolver := RemoteResolver;
    Resolver.RemoteImportsEnabled := True;
    ResetAudit;
    Resolver.CapabilityAuditEmitter := RecordAudit;
    ErrorRaised := False;
    try
      Resolver.LoadImportMap(ImportMapPath);
    except
      on Exception do
        ErrorRaised := True;
    end;
    Expect<Boolean>(ErrorRaised).ToBe(True);
    Expect<Integer>(RemoteResolver.ResolveCount).ToBe(0);
    Expect<Integer>(FAuditCount).ToBe(0);
  finally
    Resolver.Free;
    RemoteResolver.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TModuleResolverTests.Create('Goccia Module Resolver'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
