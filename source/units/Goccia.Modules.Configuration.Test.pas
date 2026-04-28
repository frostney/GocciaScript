program Goccia.Modules.Configuration.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,
  TextSemantics,

  Goccia.Modules.Configuration,
  Goccia.Modules.Resolver,
  Goccia.TestSetup;

type
  TModuleConfigurationTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;

    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteRawFile(const APath: string; const ABytes: RawByteString);
    procedure WriteTextFile(const APath, AText: string);

    procedure TestConfigureModuleResolverLoadsExplicitImportMap;
    procedure TestConfigureModuleResolverPreservesUTF8ImportMap;
    procedure TestConfigureModuleResolverDiscoversProjectConfig;
    procedure TestConfigureModuleResolverAppliesInlineAliasAfterImportMap;
    procedure TestConfigureModuleResolverPrefersExplicitImportMap;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

procedure TModuleConfigurationTests.SetupTests;
begin
  Test('ConfigureModuleResolver loads explicit import map',
    TestConfigureModuleResolverLoadsExplicitImportMap);
  Test('ConfigureModuleResolver preserves UTF-8 import map entries',
    TestConfigureModuleResolverPreservesUTF8ImportMap);
  Test('ConfigureModuleResolver discovers goccia.json from the entry path',
    TestConfigureModuleResolverDiscoversProjectConfig);
  Test('ConfigureModuleResolver applies inline aliases after the import map',
    TestConfigureModuleResolverAppliesInlineAliasAfterImportMap);
  Test('ConfigureModuleResolver prefers an explicit import map over discovered goccia.json',
    TestConfigureModuleResolverPrefersExplicitImportMap);
end;

procedure TModuleConfigurationTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FTempDirectories := TStringList.Create;
end;

procedure TModuleConfigurationTests.AfterAll;
var
  I: Integer;
begin
  for I := 0 to FTempDirectories.Count - 1 do
    DeleteDirectoryTree(FTempDirectories[I]);
  FTempDirectories.Free;
  inherited AfterAll;
end;

function TModuleConfigurationTests.CreateTempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-import-map-' +
    IntToStr(Random(MaxInt));
  ForceDirectories(Result);
  FTempDirectories.Add(Result);
end;

procedure TModuleConfigurationTests.DeleteDirectoryTree(const APath: string);
var
  EntryPath: string;
  SearchRec: TSearchRec;
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
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  RemoveDir(APath);
end;

procedure TModuleConfigurationTests.WriteTextFile(const APath, AText: string);
var
  Source: TStringList;
begin
  ForceDirectories(ExtractFileDir(APath));
  Source := TStringList.Create;
  try
    Source.Text := AText;
    Source.SaveToFile(APath);
  finally
    Source.Free;
  end;
end;

procedure TModuleConfigurationTests.WriteRawFile(const APath: string;
  const ABytes: RawByteString);
var
  Stream: TFileStream;
begin
  ForceDirectories(ExtractFileDir(APath));
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(ABytes) > 0 then
      Stream.WriteBuffer(Pointer(ABytes)^, Length(ABytes));
  finally
    Stream.Free;
  end;
end;

procedure TModuleConfigurationTests.TestConfigureModuleResolverLoadsExplicitImportMap;
var
  EntryPath, ImportMapPath, ProjectDirectory, ResolvedPath: string;
  InlineAliases: TStringList;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  EntryPath := IncludeTrailingPathDelimiter(ProjectDirectory) + 'src' +
    PathDelim + 'app.js';
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'imports.json';

  WriteTextFile(EntryPath, 'import { value } from "lodash";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'vendor' +
    PathDelim + 'lodash' + PathDelim + 'index.js', 'export const value = 1;');
  WriteTextFile(ImportMapPath,
    '{' + LineEnding +
    '  "imports": {' + LineEnding +
    '    "lodash": "./vendor/lodash/index.js"' + LineEnding +
    '  }' + LineEnding +
    '}');

  InlineAliases := TStringList.Create;
  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  try
    ConfigureModuleResolver(Resolver, EntryPath, ImportMapPath, InlineAliases);
    ResolvedPath := Resolver.Resolve('lodash', EntryPath);
  finally
    InlineAliases.Free;
    Resolver.Free;
  end;

  Expect<string>(ResolvedPath).ToBe(IncludeTrailingPathDelimiter(
    ProjectDirectory) + 'vendor' + PathDelim + 'lodash' + PathDelim +
    'index.js');
end;

procedure TModuleConfigurationTests.TestConfigureModuleResolverPreservesUTF8ImportMap;
const
  SPECIFIER_BYTES = 'caf' + #$C3#$A9;
  TARGET_FILE_BYTES = 'd' + #$C3#$A9 + 'j' + #$C3#$A0 + '.js';
  IMPORT_MAP_BYTES = '{"imports":{"' + SPECIFIER_BYTES + '":"./src/' +
    TARGET_FILE_BYTES + '"}}';
var
  EntryPath: string;
  ExpectedPath: string;
  ImportMapPath: string;
  InlineAliases: TStringList;
  ProjectDirectory: string;
  ResolvedPath: string;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  EntryPath := IncludeTrailingPathDelimiter(ProjectDirectory) + 'src' +
    PathDelim + 'app.js';
  ImportMapPath := IncludeTrailingPathDelimiter(ProjectDirectory) +
    'imports.json';
  ExpectedPath := IncludeTrailingPathDelimiter(ProjectDirectory) + 'src' +
    PathDelim + RetagUTF8Text(TARGET_FILE_BYTES);

  WriteTextFile(EntryPath, 'import { value } from "caf' + #$C3#$A9 + '";');
  WriteTextFile(ExpectedPath, 'export const value = 1;');
  WriteRawFile(ImportMapPath, IMPORT_MAP_BYTES);

  InlineAliases := TStringList.Create;
  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  try
    ConfigureModuleResolver(Resolver, EntryPath, ImportMapPath, InlineAliases);
    ResolvedPath := Resolver.Resolve(RetagUTF8Text(SPECIFIER_BYTES), EntryPath);
  finally
    InlineAliases.Free;
    Resolver.Free;
  end;

  Expect<string>(ResolvedPath).ToBe(ExpectedPath);
end;

procedure TModuleConfigurationTests.TestConfigureModuleResolverDiscoversProjectConfig;
var
  EntryPath, ProjectDirectory, ResolvedPath: string;
  Resolver: TGocciaModuleResolver;
  InlineAliases: TStringList;
begin
  ProjectDirectory := CreateTempDirectory;
  EntryPath := IncludeTrailingPathDelimiter(ProjectDirectory) + 'src' +
    PathDelim + 'feature' + PathDelim + 'main.js';

  WriteTextFile(EntryPath, 'import { add } from "@/utils/math";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'src' +
    PathDelim + 'utils' + PathDelim + 'math.js',
    'export const add = (a, b) => a + b;');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json',
    '{' + LineEnding +
    '  "imports": {' + LineEnding +
    '    "@/": "./src/"' + LineEnding +
    '  }' + LineEnding +
    '}');

  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  InlineAliases := TStringList.Create;
  try
    ConfigureModuleResolver(Resolver, EntryPath, '', InlineAliases);
    ResolvedPath := Resolver.Resolve('@/utils/math', EntryPath);
  finally
    InlineAliases.Free;
    Resolver.Free;
  end;

  Expect<string>(ResolvedPath).ToBe(IncludeTrailingPathDelimiter(
    ProjectDirectory) + 'src' + PathDelim + 'utils' + PathDelim + 'math.js');
end;

procedure TModuleConfigurationTests.TestConfigureModuleResolverAppliesInlineAliasAfterImportMap;
var
  EntryPath, ProjectDirectory, ResolvedPath: string;
  InlineAliases: TStringList;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  EntryPath := IncludeTrailingPathDelimiter(ProjectDirectory) + 'app.js';

  WriteTextFile(EntryPath, 'import { value } from "config";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'config' +
    PathDelim + 'default.js', 'export const value = "default";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'config' +
    PathDelim + 'override.js', 'export const value = "override";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'imports.json',
    '{' + LineEnding +
    '  "imports": {' + LineEnding +
    '    "config": "./config/default.js"' + LineEnding +
    '  }' + LineEnding +
    '}');

  InlineAliases := TStringList.Create;
  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  try
    InlineAliases.Add('config=./config/override.js');
    ConfigureModuleResolver(Resolver, EntryPath,
      IncludeTrailingPathDelimiter(ProjectDirectory) + 'imports.json',
      InlineAliases);
    ResolvedPath := Resolver.Resolve('config', EntryPath);
  finally
    Resolver.Free;
    InlineAliases.Free;
  end;

  Expect<string>(ResolvedPath).ToBe(IncludeTrailingPathDelimiter(
    ProjectDirectory) + 'config' + PathDelim + 'override.js');
end;

procedure TModuleConfigurationTests.TestConfigureModuleResolverPrefersExplicitImportMap;
var
  EntryPath, ProjectDirectory, ResolvedPath: string;
  InlineAliases: TStringList;
  Resolver: TGocciaModuleResolver;
begin
  ProjectDirectory := CreateTempDirectory;
  EntryPath := IncludeTrailingPathDelimiter(ProjectDirectory) + 'src' +
    PathDelim + 'app.js';

  WriteTextFile(EntryPath, 'import { value } from "config";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'config' +
    PathDelim + 'explicit.js', 'export const value = "explicit";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'config' +
    PathDelim + 'discovered.js', 'export const value = "discovered";');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) + 'imports.json',
    '{' + LineEnding +
    '  "imports": {' + LineEnding +
    '    "config": "./config/explicit.js"' + LineEnding +
    '  }' + LineEnding +
    '}');
  WriteTextFile(IncludeTrailingPathDelimiter(ProjectDirectory) +
    'goccia.json',
    '{' + LineEnding +
    '  "imports": {' + LineEnding +
    '    "config": "./config/discovered.js"' + LineEnding +
    '  }' + LineEnding +
    '}');

  InlineAliases := TStringList.Create;
  Resolver := TGocciaModuleResolver.Create(ProjectDirectory);
  try
    ConfigureModuleResolver(Resolver, EntryPath,
      IncludeTrailingPathDelimiter(ProjectDirectory) + 'imports.json',
      InlineAliases);
    ResolvedPath := Resolver.Resolve('config', EntryPath);
  finally
    Resolver.Free;
    InlineAliases.Free;
  end;

  Expect<string>(ResolvedPath).ToBe(IncludeTrailingPathDelimiter(
    ProjectDirectory) + 'config' + PathDelim + 'explicit.js');
end;

begin
  TestRunnerProgram.AddSuite(TModuleConfigurationTests.Create(
    'Module Configuration'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
