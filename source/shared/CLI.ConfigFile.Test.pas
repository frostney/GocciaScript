program CLI.ConfigFile.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  FileUtils,
  TestingPascalLibrary,
  TextSemantics;

type
  TConfigFileTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;

    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteFixtureFile(const APath, AText: string);
    procedure WriteTextFile(const APath, AText: string);

    { ParseJSONConfig (built-in) }
    procedure TestJSONStringValue;
    procedure TestJSONIntegerValue;
    procedure TestJSONBooleanTrueIncluded;
    procedure TestJSONBooleanFalseOmitted;
    procedure TestJSONArrayProducesMultipleEntries;
    procedure TestJSONNestedObjectSkipped;
    procedure TestJSONNullSkipped;
    procedure TestJSONNestedObjectFlattened;
    procedure TestJSONValueKinds;
    procedure TestJSONUnrepresentableValuesMarked;

    { SourcePath }
    procedure TestSourcePathOnOwnAndBaseEntries;

    { Validation }
    procedure TestRemovedKeyRaises;
    procedure TestCommandLineOnlyKeyRaises;
    procedure TestFlagValueMustBeBoolean;
    procedure TestRequiresTrustKeySkipped;
    procedure TestValidateConfigEntries;
    procedure TestInvalidOptionValueNamesConfig;

    { ApplyConfigEntries }
    procedure TestApplyFlagOption;
    procedure TestApplyFlagFalseNotSet;
    procedure TestApplyStringOption;
    procedure TestApplyIntegerOption;
    procedure TestApplyEnumOption;
    procedure TestApplyRepeatableOption;
    procedure TestApplyUnknownKeySkipped;

    { ApplyConfigFile }
    procedure TestApplyConfigFileJSON;
    procedure TestApplyConfigFileJSONPreservesUTF8;
    procedure TestApplyConfigFileUnregisteredExtensionRaises;

    { CLI override }
    procedure TestOptionCanBeModifiedAfterConfigApply;
    procedure TestApplySkipsAlreadyPresentOption;

    { extends }
    procedure TestExtendsLoadsBaseConfig;
    procedure TestExtendsChildOverridesParent;
    procedure TestExtendsCircularRaises;

    { RegisterConfigParser }
    procedure TestRegisterAndUseCustomParser;

    { DiscoverConfigFile }
    procedure TestDiscoverFindsFileInStartDirectory;
    procedure TestDiscoverWalksUpToParent;
    procedure TestDiscoverRespectsExtensionPriority;
    procedure TestDiscoverReturnsEmptyWhenNotFound;

    { FindConfigEntry }
    procedure TestFindConfigEntryReturnsMatch;
    procedure TestFindConfigEntryReturnsFalseWhenMissing;
    procedure TestFindConfigEntryFirstMatchWins;

    { ResolveFlagOption }
    procedure TestResolveFlagOptionCLIWins;
    procedure TestResolveFlagOptionPerFileOverridesRoot;
    procedure TestResolveFlagOptionPerFileFalseOverridesRoot;
    procedure TestResolveFlagOptionFallsBackToRoot;
    procedure TestResolveFlagOptionOnlyTrueEnablesFlag;
    procedure TestResolveFlagOptionUsesConfigName;
    procedure TestResolveFlagOptionMixedAliasExtendsPrecedence;
    procedure TestResolveFlagOptionDefaultsFalse;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

procedure TConfigFileTests.SetupTests;
begin
  Test('JSON: string value produces entry', TestJSONStringValue);
  Test('JSON: integer value produces entry', TestJSONIntegerValue);
  Test('JSON: boolean true is included', TestJSONBooleanTrueIncluded);
  Test('JSON: boolean false is omitted', TestJSONBooleanFalseOmitted);
  Test('JSON: array produces multiple entries', TestJSONArrayProducesMultipleEntries);
  Test('JSON: nested object is skipped', TestJSONNestedObjectSkipped);
  Test('JSON: null for a known option is an error', TestJSONNullSkipped);
  Test('JSON: nested objects flatten one level', TestJSONNestedObjectFlattened);
  Test('JSON: entries record their value kind', TestJSONValueKinds);
  Test('JSON: null, deep objects, and nested arrays are marked unsupported',
    TestJSONUnrepresentableValuesMarked);
  Test('SourcePath names the declaring file through extends',
    TestSourcePathOnOwnAndBaseEntries);
  Test('A removed key raises a usage error naming its replacement',
    TestRemovedKeyRaises);
  Test('A command-line-only key raises a usage error',
    TestCommandLineOnlyKeyRaises);
  Test('A flag value must be exactly true or false', TestFlagValueMustBeBoolean);
  Test('RequiresTrust keys are not applied', TestRequiresTrustKeySkipped);
  Test('ValidateConfigEntries checks without applying',
    TestValidateConfigEntries);
  Test('An invalid option value names the config file',
    TestInvalidOptionValueNamesConfig);

  Test('ApplyConfigEntries sets flag option', TestApplyFlagOption);
  Test('ApplyConfigEntries does not set flag for false', TestApplyFlagFalseNotSet);
  Test('ApplyConfigEntries sets string option', TestApplyStringOption);
  Test('ApplyConfigEntries sets integer option', TestApplyIntegerOption);
  Test('ApplyConfigEntries sets enum option', TestApplyEnumOption);
  Test('ApplyConfigEntries accumulates repeatable values', TestApplyRepeatableOption);
  Test('ApplyConfigEntries skips unknown keys', TestApplyUnknownKeySkipped);

  Test('ApplyConfigFile loads a JSON file', TestApplyConfigFileJSON);
  Test('ApplyConfigFile preserves UTF-8 JSON file values',
    TestApplyConfigFileJSONPreservesUTF8);
  Test('ApplyConfigFile raises for unregistered extension', TestApplyConfigFileUnregisteredExtensionRaises);

  Test('Option can be modified after config apply', TestOptionCanBeModifiedAfterConfigApply);
  Test('ApplyConfigEntries skips already-present options', TestApplySkipsAlreadyPresentOption);

  Test('extends loads base config entries', TestExtendsLoadsBaseConfig);
  Test('extends child overrides parent values', TestExtendsChildOverridesParent);
  Test('extends circular chain raises', TestExtendsCircularRaises);

  Test('RegisterConfigParser enables custom format', TestRegisterAndUseCustomParser);

  Test('DiscoverConfigFile finds file in start directory', TestDiscoverFindsFileInStartDirectory);
  Test('DiscoverConfigFile walks up to parent', TestDiscoverWalksUpToParent);
  Test('DiscoverConfigFile respects extension priority', TestDiscoverRespectsExtensionPriority);
  Test('DiscoverConfigFile returns empty when not found', TestDiscoverReturnsEmptyWhenNotFound);

  Test('FindConfigEntry returns matching value', TestFindConfigEntryReturnsMatch);
  Test('FindConfigEntry returns false when key missing', TestFindConfigEntryReturnsFalseWhenMissing);
  Test('FindConfigEntry first match wins for duplicate keys', TestFindConfigEntryFirstMatchWins);

  Test('ResolveFlagOption returns True when flag is from CLI', TestResolveFlagOptionCLIWins);
  Test('ResolveFlagOption uses per-file config over root config', TestResolveFlagOptionPerFileOverridesRoot);
  Test('ResolveFlagOption per-file false overrides root true', TestResolveFlagOptionPerFileFalseOverridesRoot);
  Test('ResolveFlagOption falls back to root when no per-file config', TestResolveFlagOptionFallsBackToRoot);
  Test('ResolveFlagOption enables a flag only for true', TestResolveFlagOptionOnlyTrueEnablesFlag);
  Test('ResolveFlagOption uses option ConfigName', TestResolveFlagOptionUsesConfigName);
  Test('ResolveFlagOption preserves extends precedence across aliases',
    TestResolveFlagOptionMixedAliasExtendsPrecedence);
  Test('ResolveFlagOption defaults to False when nothing is set', TestResolveFlagOptionDefaultsFalse);
end;

procedure TConfigFileTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FTempDirectories := TStringList.Create;
end;

procedure TConfigFileTests.AfterAll;
var
  I: Integer;
begin
  for I := 0 to FTempDirectories.Count - 1 do
    DeleteDirectoryTree(FTempDirectories[I]);
  FTempDirectories.Free;
  inherited AfterAll;
end;

function TConfigFileTests.CreateTempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'cli-config-test-' + IntToStr(Random(MaxInt));
  ForceDirectories(Result);
  FTempDirectories.Add(Result);
end;

procedure TConfigFileTests.DeleteDirectoryTree(const APath: string);
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

procedure TConfigFileTests.WriteTextFile(const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  FileUtils.WriteUTF8FileText(APath, AText);
end;

procedure TConfigFileTests.WriteFixtureFile(const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  FileUtils.WriteUTF8FileText(APath, AText);
end;

function ConfigErrorMessage(const AEntries: TConfigEntryArray;
  const AOptions: TOptionArray; const AValidateOnly: Boolean): string;
begin
  Result := '';
  try
    if AValidateOnly then
      ValidateConfigEntries(AEntries, AOptions)
    else
      ApplyConfigEntries(AEntries, AOptions);
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

{ ── JSON parsing tests ─────────────────────────────────────── }

procedure TConfigFileTests.TestJSONStringValue;
var
  Dir, Path: string;
  Flag: TFlagOption;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"mode": "bytecode"}');

  Mode := TStringOption.Create('mode', 'Execution mode');
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 2);
    Options[0] := Mode;
    Options[1] := Flag;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Mode.Present).ToBe(True);
    Expect<string>(Mode.Value).ToBe('bytecode');
    Expect<Boolean>(Flag.Present).ToBe(False);
  finally
    Mode.Free;
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestJSONIntegerValue;
var
  Dir, Path: string;
  Timeout: TIntegerOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"timeout": 5000}');

  Timeout := TIntegerOption.Create('timeout', 'Timeout');
  try
    SetLength(Options, 1);
    Options[0] := Timeout;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Timeout.Present).ToBe(True);
    Expect<Integer>(Timeout.Value).ToBe(5000);
  finally
    Timeout.Free;
  end;
end;

procedure TConfigFileTests.TestJSONBooleanTrueIncluded;
var
  Dir, Path: string;
  Flag: TFlagOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"feature": true}');

  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 1);
    Options[0] := Flag;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Flag.Present).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestJSONBooleanFalseOmitted;
var
  Dir, Path: string;
  Flag: TFlagOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"feature": false}');

  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 1);
    Options[0] := Flag;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Flag.Present).ToBe(False);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestJSONArrayProducesMultipleEntries;
var
  Dir, Path: string;
  Aliases: TRepeatableOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"alias": ["@/=./src/", "config=./config.js"]}');

  Aliases := TRepeatableOption.Create('alias', 'Aliases');
  try
    SetLength(Options, 1);
    Options[0] := Aliases;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Aliases.Present).ToBe(True);
    Expect<Integer>(Aliases.Values.Count).ToBe(2);
    Expect<string>(Aliases.Values[0]).ToBe('@/=./src/');
    Expect<string>(Aliases.Values[1]).ToBe('config=./config.js');
  finally
    Aliases.Free;
  end;
end;

procedure TConfigFileTests.TestJSONNestedObjectSkipped;
var
  Dir, Path: string;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"imports": {"@/": "./src/"}, "mode": "bytecode"}');

  Mode := TStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 1);
    Options[0] := Mode;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Mode.Present).ToBe(True);
    Expect<string>(Mode.Value).ToBe('bytecode');
  finally
    Mode.Free;
  end;
end;

procedure TConfigFileTests.TestJSONNullSkipped;
var
  Dir, Path: string;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"mode": null, "other": null}');

  Mode := TStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 1);
    Options[0] := Mode;

    Expect<string>(ConfigErrorMessage(ParseConfigFile(Path), Options, False))
      .ToBe('TParseError: ' + Path + ': "mode" must be a single value, not ' +
        'null or an object');
    Expect<Boolean>(Mode.Present).ToBe(False);
  finally
    Mode.Free;
  end;
end;

{ ── ApplyConfigEntries tests ───────────────────────────────── }

procedure TConfigFileTests.TestApplyFlagOption;
var
  Flag: TFlagOption;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(Entries, 1);
    Entries[0].Key := 'feature';
    Entries[0].Value := 'true';
    Entries[0].Kind := cvkBoolean;

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Flag.Present).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestApplyFlagFalseNotSet;
var
  Flag: TFlagOption;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(Entries, 1);
    Entries[0].Key := 'feature';
    Entries[0].Value := 'false';
    Entries[0].Kind := cvkBoolean;

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Flag.Present).ToBe(False);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestApplyStringOption;
var
  Opt: TStringOption;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TStringOption.Create('output', 'Output path');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    SetLength(Entries, 1);
    Entries[0].Key := 'output';
    Entries[0].Value := 'results.json';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<string>(Opt.Value).ToBe('results.json');
  finally
    Opt.Free;
  end;
end;

procedure TConfigFileTests.TestApplyIntegerOption;
var
  Opt: TIntegerOption;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TIntegerOption.Create('timeout', 'Timeout');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    SetLength(Entries, 1);
    Entries[0].Key := 'timeout';
    Entries[0].Value := '3000';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Integer>(Opt.Value).ToBe(3000);
  finally
    Opt.Free;
  end;
end;

type
  TTestMode = (tmInterpreted, tmBytecode);

procedure TConfigFileTests.TestApplyEnumOption;
var
  Opt: TEnumOption<TTestMode>;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TEnumOption<TTestMode>.Create('mode', 'Execution mode');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    SetLength(Entries, 1);
    Entries[0].Key := 'mode';
    Entries[0].Value := 'bytecode';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Boolean>(Opt.Matches(tmBytecode)).ToBe(True);
  finally
    Opt.Free;
  end;
end;

procedure TConfigFileTests.TestApplyRepeatableOption;
var
  Opt: TRepeatableOption;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TRepeatableOption.Create('alias', 'Aliases');
  try
    SetLength(Options, 1);
    Options[0] := Opt;
    SetLength(Entries, 2);
    Entries[0].Key := 'alias';
    Entries[0].Value := '@/=./src/';
    Entries[1].Key := 'alias';
    Entries[1].Value := 'utils=./lib/';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Opt.Present).ToBe(True);
    Expect<Integer>(Opt.Values.Count).ToBe(2);
    Expect<string>(Opt.Values[0]).ToBe('@/=./src/');
    Expect<string>(Opt.Values[1]).ToBe('utils=./lib/');
  finally
    Opt.Free;
  end;
end;

procedure TConfigFileTests.TestApplyUnknownKeySkipped;
var
  Flag: TFlagOption;
  Options: TOptionArray;
  Entries: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(Entries, 1);
    Entries[0].Key := 'unknown-key';
    Entries[0].Value := 'value';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Flag.Present).ToBe(False);
  finally
    Flag.Free;
  end;
end;

{ ── ApplyConfigFile tests ──────────────────────────────────── }

procedure TConfigFileTests.TestApplyConfigFileJSON;
var
  Dir, Path: string;
  Flag: TFlagOption;
  Timeout: TIntegerOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteTextFile(Path, '{"feature": true, "timeout": 2000}');

  Flag := TFlagOption.Create('feature', 'Flag');
  Timeout := TIntegerOption.Create('timeout', 'Timeout');
  try
    SetLength(Options, 2);
    Options[0] := Flag;
    Options[1] := Timeout;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Flag.Present).ToBe(True);
    Expect<Boolean>(Timeout.Present).ToBe(True);
    Expect<Integer>(Timeout.Value).ToBe(2000);
  finally
    Flag.Free;
    Timeout.Free;
  end;
end;

procedure TConfigFileTests.TestApplyConfigFileJSONPreservesUTF8;
const
  JSON_TEXT = '{"mode":"Jos' + #$00E9 + '","alias":["caf' + #$00E9 +
    '=./d' + #$00E9 + 'j' + #$00E0 + '.js"]}';
var
  Alias: TRepeatableOption;
  Dir, Path: string;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteFixtureFile(Path, JSON_TEXT);

  Alias := TRepeatableOption.Create('alias', 'Alias');
  Mode := TStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 2);
    Options[0] := Mode;
    Options[1] := Alias;

    ApplyConfigFile(Path, Options);

    Expect<string>(Mode.Value).ToBe('Jos' + #$00E9);
    Expect<Integer>(Alias.Values.Count).ToBe(1);
    Expect<string>(Alias.Values[0]).ToBe('caf' + #$00E9 +
      '=./d' + #$00E9 + 'j' + #$00E0 + '.js');
  finally
    Alias.Free;
    Mode.Free;
  end;
end;

procedure TConfigFileTests.TestApplyConfigFileUnregisteredExtensionRaises;
var
  Dir, Path: string;
  Flag: TFlagOption;
  Options: TOptionArray;
  Raised: Boolean;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.xyz';
  WriteTextFile(Path, 'whatever');

  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 1);
    Options[0] := Flag;

    Raised := False;
    try
      ApplyConfigFile(Path, Options);
    except
      on E: Exception do
        Raised := True;
    end;

    Expect<Boolean>(Raised).ToBe(True);
  finally
    Flag.Free;
  end;
end;

{ ── CLI override test ──────────────────────────────────────── }

procedure TConfigFileTests.TestOptionCanBeModifiedAfterConfigApply;
var
  Dir, Path: string;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteTextFile(Path, '{"mode": "bytecode"}');

  Mode := TStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 1);
    Options[0] := Mode;

    { Apply config first }
    ApplyConfigFile(Path, Options);
    Expect<string>(Mode.Value).ToBe('bytecode');

    { CLI override }
    Mode.Apply('interpreted');
    Expect<string>(Mode.Value).ToBe('interpreted');
  finally
    Mode.Free;
  end;
end;

procedure TConfigFileTests.TestApplySkipsAlreadyPresentOption;
var
  Dir, Path: string;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteTextFile(Path, '{"mode": "bytecode"}');

  Mode := TStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 1);
    Options[0] := Mode;

    { Simulate CLI setting the option first }
    Mode.Apply('interpreted');

    { Config should NOT override it }
    ApplyConfigFile(Path, Options);
    Expect<string>(Mode.Value).ToBe('interpreted');
  finally
    Mode.Free;
  end;
end;

{ ── extends tests ──────────────────────────────────────────── }

procedure TConfigFileTests.TestExtendsLoadsBaseConfig;
var
  Dir, BasePath, ChildPath: string;
  Flag: TFlagOption;
  Timeout: TIntegerOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  BasePath := IncludeTrailingPathDelimiter(Dir) + 'base.json';
  ChildPath := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(BasePath, '{"feature": true, "timeout": 3000}');
  WriteTextFile(ChildPath, '{"extends": "base.json"}');

  Flag := TFlagOption.Create('feature', 'Flag');
  Timeout := TIntegerOption.Create('timeout', 'Timeout');
  try
    SetLength(Options, 2);
    Options[0] := Flag;
    Options[1] := Timeout;

    ApplyConfigFile(ChildPath, Options);

    Expect<Boolean>(Flag.Present).ToBe(True);
    Expect<Boolean>(Timeout.Present).ToBe(True);
    Expect<Integer>(Timeout.Value).ToBe(3000);
  finally
    Flag.Free;
    Timeout.Free;
  end;
end;

procedure TConfigFileTests.TestExtendsChildOverridesParent;
var
  Dir, BasePath, ChildPath: string;
  Mode: TStringOption;
  Flag: TFlagOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  BasePath := IncludeTrailingPathDelimiter(Dir) + 'base.json';
  ChildPath := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(BasePath, '{"mode": "interpreted", "feature": true}');
  WriteTextFile(ChildPath, '{"extends": "base.json", "mode": "bytecode"}');

  Mode := TStringOption.Create('mode', 'Mode');
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 2);
    Options[0] := Mode;
    Options[1] := Flag;

    ApplyConfigFile(ChildPath, Options);

    { Child's mode overrides parent's }
    Expect<string>(Mode.Value).ToBe('bytecode');
    { Parent's feature is inherited }
    Expect<Boolean>(Flag.Present).ToBe(True);
  finally
    Mode.Free;
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestExtendsCircularRaises;
var
  Dir, PathA, PathB: string;
  Flag: TFlagOption;
  Options: TOptionArray;
  Raised: Boolean;
begin
  Dir := CreateTempDirectory;
  PathA := IncludeTrailingPathDelimiter(Dir) + 'a.json';
  PathB := IncludeTrailingPathDelimiter(Dir) + 'b.json';
  WriteTextFile(PathA, '{"extends": "b.json"}');
  WriteTextFile(PathB, '{"extends": "a.json"}');

  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(Options, 1);
    Options[0] := Flag;

    Raised := False;
    try
      ApplyConfigFile(PathA, Options);
    except
      on E: Exception do
        Raised := True;
    end;

    Expect<Boolean>(Raised).ToBe(True);
  finally
    Flag.Free;
  end;
end;

{ ── Custom parser test ─────────────────────────────────────── }

function DummyTOMLParser(const AContent: string): TConfigEntryArray;
begin
  SetLength(Result, 1);
  Result[0].Key := 'mode';
  Result[0].Value := 'bytecode';
end;

procedure TConfigFileTests.TestRegisterAndUseCustomParser;
var
  Dir, Path: string;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.toml';
  WriteTextFile(Path, 'mode = "bytecode"');

  RegisterConfigParser('.toml', @DummyTOMLParser);

  Mode := TStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 1);
    Options[0] := Mode;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Mode.Present).ToBe(True);
    Expect<string>(Mode.Value).ToBe('bytecode');
  finally
    Mode.Free;
  end;
end;

{ ── DiscoverConfigFile tests ───────────────────────────────── }

procedure TConfigFileTests.TestDiscoverFindsFileInStartDirectory;
var
  Dir, Path, Found: string;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'myapp.json';
  WriteTextFile(Path, '{}');

  Found := DiscoverConfigFile(Dir, ['myapp'], ['.json']);

  Expect<string>(Found).ToBe(ExpandFileName(Path));
end;

procedure TConfigFileTests.TestDiscoverWalksUpToParent;
var
  ParentDir, ChildDir, ConfigPath, Found: string;
begin
  ParentDir := CreateTempDirectory;
  ChildDir := IncludeTrailingPathDelimiter(ParentDir) + 'sub';
  ForceDirectories(ChildDir);

  ConfigPath := IncludeTrailingPathDelimiter(ParentDir) + 'myapp.json';
  WriteTextFile(ConfigPath, '{}');

  Found := DiscoverConfigFile(ChildDir, ['myapp'], ['.json']);

  Expect<string>(Found).ToBe(ExpandFileName(ConfigPath));
end;

procedure TConfigFileTests.TestDiscoverRespectsExtensionPriority;
var
  Dir, JSONPath, TOMLPath, Found: string;
begin
  Dir := CreateTempDirectory;
  JSONPath := IncludeTrailingPathDelimiter(Dir) + 'myapp.json';
  TOMLPath := IncludeTrailingPathDelimiter(Dir) + 'myapp.toml';
  WriteTextFile(JSONPath, '{}');
  WriteTextFile(TOMLPath, '');

  { TOML listed first → higher priority }
  Found := DiscoverConfigFile(Dir, ['myapp'], ['.toml', '.json']);

  Expect<string>(Found).ToBe(ExpandFileName(TOMLPath));
end;

procedure TConfigFileTests.TestDiscoverReturnsEmptyWhenNotFound;
var
  Dir, Found: string;
begin
  Dir := CreateTempDirectory;

  Found := DiscoverConfigFile(Dir, ['nonexistent'], ['.json']);

  Expect<string>(Found).ToBe('');
end;

{ ── FindConfigEntry tests ──────────────────────────────────── }

procedure TConfigFileTests.TestFindConfigEntryReturnsMatch;
var
  Entries: TConfigEntryArray;
  Value: string;
begin
  SetLength(Entries, 2);
  Entries[0].Key := 'mode';
  Entries[0].Value := 'bytecode';
  Entries[1].Key := 'feature';
  Entries[1].Value := 'true';
  Entries[1].Kind := cvkBoolean;

  Expect<Boolean>(FindConfigEntry(Entries, 'feature', Value)).ToBe(True);
  Expect<string>(Value).ToBe('true');
end;

procedure TConfigFileTests.TestFindConfigEntryReturnsFalseWhenMissing;
var
  Entries: TConfigEntryArray;
  Value: string;
begin
  SetLength(Entries, 1);
  Entries[0].Key := 'mode';
  Entries[0].Value := 'bytecode';

  Expect<Boolean>(FindConfigEntry(Entries, 'timeout', Value)).ToBe(False);
end;

procedure TConfigFileTests.TestFindConfigEntryFirstMatchWins;
var
  Entries: TConfigEntryArray;
  Value: string;
begin
  SetLength(Entries, 2);
  Entries[0].Key := 'alias';
  Entries[0].Value := '@/=./src/';
  Entries[1].Key := 'alias';
  Entries[1].Value := 'utils=./lib/';

  Expect<Boolean>(FindConfigEntry(Entries, 'alias', Value)).ToBe(True);
  Expect<string>(Value).ToBe('@/=./src/');
end;

{ ── ResolveFlagOption tests ─────────────────────────────────── }

procedure TConfigFileTests.TestResolveFlagOptionCLIWins;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    Flag.Apply('');
    Flag.MarkFromCommandLine;

    { Per-file config says false, but CLI should win }
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'feature';
    FileConfig[0].Value := 'false';
    FileConfig[0].Kind := cvkBoolean;

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionPerFileOverridesRoot;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
  Options: TOptionArray;
  RootEntries: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    { Root config sets feature='false' — ApplyConfigEntries skips Apply for
      flag value 'false', so Present remains False. }
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(RootEntries, 1);
    RootEntries[0].Key := 'feature';
    RootEntries[0].Value := 'false';
    RootEntries[0].Kind := cvkBoolean;
    ApplyConfigEntries(RootEntries, Options);

    { Per-file config says true — should override root }
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'feature';
    FileConfig[0].Value := 'true';
    FileConfig[0].Kind := cvkBoolean;

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionPerFileFalseOverridesRoot;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
  Options: TOptionArray;
  RootEntries: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    { Simulate root config setting feature=true (Present but not FromCommandLine) }
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(RootEntries, 1);
    RootEntries[0].Key := 'feature';
    RootEntries[0].Value := 'true';
    RootEntries[0].Kind := cvkBoolean;
    ApplyConfigEntries(RootEntries, Options);
    Expect<Boolean>(Flag.Present).ToBe(True);

    { Per-file config says false — should override root }
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'feature';
    FileConfig[0].Value := 'false';
    FileConfig[0].Kind := cvkBoolean;

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(False);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionFallsBackToRoot;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
  Options: TOptionArray;
  RootEntries: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    { Simulate root config setting feature=true }
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(RootEntries, 1);
    RootEntries[0].Key := 'feature';
    RootEntries[0].Value := 'true';
    RootEntries[0].Kind := cvkBoolean;
    ApplyConfigEntries(RootEntries, Options);

    { No per-file config — should fall back to root (Present=True) }
    SetLength(FileConfig, 0);

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionOnlyTrueEnablesFlag;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'feature';
    FileConfig[0].Value := '';
    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(False);
    FileConfig[0].Value := 'true';
    FileConfig[0].Kind := cvkBoolean;
    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(True);
  finally
    Flag.Free;
  end;
end;

{ ── Flattening, kinds, and SourcePath ──────────────────────── }

function EntryText(const AEntries: TConfigEntryArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AEntries) do
  begin
    if I > 0 then
      Result := Result + ';';
    if AEntries[I].Kind = cvkObject then
      Result := Result + AEntries[I].Key + '={}'
    else if AEntries[I].Kind = cvkUnsupported then
      Result := Result + AEntries[I].Key + '=?'
    else
      Result := Result + AEntries[I].Key + '=' + AEntries[I].Value;
  end;
end;

procedure TConfigFileTests.TestJSONNestedObjectFlattened;
var
  Dir, Path: string;
  Entries: TConfigEntryArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(Path, '{"timeout": "5s", "permissions": {"allow-net": ' +
    '["a.test", "b.test"], "allow-ffi": true, "deny-read": [], ' +
    '"deep": {"x": 1}}, "list": [{"y": 2}, "z"], "after": 1}');
  Entries := ParseConfigFile(Path);
  Expect<string>(EntryText(Entries)).ToBe('timeout=5s;permissions={};' +
    'permissions.allow-net=a.test;permissions.allow-net=b.test;' +
    'permissions.allow-ffi=true;permissions.deny-read=;' +
    'permissions.deep=?;list=?;list=z;after=1');
  WriteTextFile(Path, '{"max-memory": {"a": 1}, "timeout": {}}');
  Expect<string>(EntryText(ParseConfigFile(Path))).ToBe('max-memory={};' +
    'max-memory.a=1;timeout={}');
end;

procedure TConfigFileTests.TestJSONUnrepresentableValuesMarked;
var
  Dir, Path: string;
  Entries: TConfigEntryArray;
  Mode: TStringOption;
  Options: TOptionArray;
  Found: string;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(Path, '{"a": null, "b": {"c": null, "d": [["x"], "y"], ' +
    '"e": {"f": 1}}, "mode": "bytecode", "g": [null]}');
  Entries := ParseConfigFile(Path);
  Expect<string>(EntryText(Entries)).ToBe('a=?;b={};b.c=?;b.d=?;b.d=y;' +
    'b.e=?;mode=bytecode;g=?');
  { extends must name a path; any other value is an error, not ignored. }
  WriteTextFile(Path, '{"extends": {"path": "base.json"}}');
  Found := '';
  try
    ParseConfigFile(Path);
  except
    on E: TParseError do
      Found := E.Message;
  end;
  Expect<string>(Found).ToBe(Path + ': "extends" must be a path');
  WriteTextFile(Path, '{"extends": ["base.json"]}');
  Found := '';
  try
    ParseConfigFile(Path);
  except
    on E: TParseError do
      Found := E.Message;
  end;
  Expect<string>(Found).ToBe(Path + ': "extends" must be a path');
  { A number too large for Int64 keeps its digits, so a unit parser can say
    it is too large rather than misreading an exponent. }
  WriteTextFile(Path, '{"max-memory": 100000000000000000000}');
  Entries := ParseConfigFile(Path);
  Expect<string>(Entries[0].Value).ToBe('100000000000000000000');
  { Lookups and option application never see an unrepresentable value. }
  Expect<Boolean>(FindConfigEntry(Entries, 'a', Found)).ToBe(False);
  Mode := TStringOption.Create('a', 'A');
  try
    SetLength(Options, 1);
    Options[0] := Mode;
    ApplyConfigEntries(Entries, Options);
    Expect<Boolean>(Mode.Present).ToBe(False);
  finally
    Mode.Free;
  end;
end;

procedure TConfigFileTests.TestJSONValueKinds;
var
  Dir, Path: string;
  Entries: TConfigEntryArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(Path, '{"a": "s", "b": 1, "c": true, "d": [], ' +
    '"e": ["x"]}');
  Entries := ParseConfigFile(Path);
  Expect<Integer>(Length(Entries)).ToBe(5);
  Expect<Boolean>(Entries[0].Kind = cvkString).ToBe(True);
  Expect<Boolean>(Entries[0].InArray).ToBe(False);
  Expect<Boolean>(Entries[1].Kind = cvkNumber).ToBe(True);
  Expect<Boolean>(Entries[2].Kind = cvkBoolean).ToBe(True);
  Expect<Boolean>(Entries[3].Kind = cvkEmptyArray).ToBe(True);
  Expect<Boolean>(Entries[3].InArray).ToBe(True);
  Expect<Boolean>(Entries[4].Kind = cvkString).ToBe(True);
  Expect<Boolean>(Entries[4].InArray).ToBe(True);
end;

procedure TConfigFileTests.TestSourcePathOnOwnAndBaseEntries;
var
  Dir, BasePath, ChildPath: string;
  Entries: TConfigEntryArray;
begin
  Dir := CreateTempDirectory;
  BasePath := IncludeTrailingPathDelimiter(Dir) + 'base' + PathDelim +
    'goccia.json';
  ChildPath := IncludeTrailingPathDelimiter(Dir) + 'child' + PathDelim +
    'goccia.json';
  WriteTextFile(BasePath, '{"permissions": {"allow-read": ["./data"]}}');
  WriteTextFile(ChildPath, '{"extends": "../base/goccia.json", ' +
    '"mode": "bytecode"}');
  Entries := ParseConfigFile(ChildPath);
  Expect<Integer>(Length(Entries)).ToBe(3);
  Expect<string>(Entries[0].Key).ToBe('mode');
  Expect<string>(Entries[0].SourcePath).ToBe(ExpandFileName(ChildPath));
  Expect<string>(Entries[1].Key).ToBe('permissions');
  Expect<string>(Entries[1].SourcePath).ToBe(ExpandFileName(BasePath));
  Expect<string>(Entries[2].Key).ToBe('permissions.allow-read');
  Expect<string>(Entries[2].SourcePath).ToBe(ExpandFileName(BasePath));
end;

{ ── Validation ─────────────────────────────────────────────── }

function SingleEntry(const AKey, AValue: string;
  const AKind: TConfigValueKind = cvkString;
  const AInArray: Boolean = False): TConfigEntryArray;
begin
  SetLength(Result, 1);
  Result[0].Key := AKey;
  Result[0].Value := AValue;
  Result[0].SourcePath := '/project/goccia.json';
  Result[0].Kind := AKind;
  Result[0].InArray := AInArray;
end;

procedure TConfigFileTests.TestRemovedKeyRaises;
var
  Removed: TRemovedOption;
  Options: TOptionArray;
  Message: string;
  IsUsageError: Boolean;
begin
  Removed := TRemovedOption.Create('allowed-host', 'allowed-hosts',
    'use --allow-net instead', 'use "permissions" instead');
  try
    SetLength(Options, 1);
    Options[0] := Removed;
    Message := '';
    IsUsageError := False;
    try
      ApplyConfigEntries(SingleEntry('allowed-hosts', 'example.com'),
        Options);
    except
      on E: TCLIUsageError do
      begin
        IsUsageError := True;
        Message := E.Message;
      end;
    end;
    Expect<Boolean>(IsUsageError).ToBe(True);
    Expect<string>(Message).ToBe('/project/goccia.json: "allowed-hosts" ' +
      'was removed in GocciaScript 0.14.0; use "permissions" instead');
  finally
    Removed.Free;
  end;
end;

procedure TConfigFileTests.TestCommandLineOnlyKeyRaises;
var
  Scopes: TScopeListOption;
  Options: TOptionArray;
  Message: string;
begin
  Scopes := TScopeListOption.Create('allow-net', 'Allow', '<host>');
  try
    Scopes.CommandLineOnly := True;
    Scopes.ConfigHint := '; declare it under "permissions" instead';
    SetLength(Options, 1);
    Options[0] := Scopes;
    Message := '';
    try
      ApplyConfigEntries(SingleEntry('allow-net', 'example.com'), Options);
    except
      on E: TCLIUsageError do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe('/project/goccia.json: "allow-net" can ' +
      'only be given on the command line; declare it under "permissions" ' +
      'instead');
    Expect<Boolean>(Scopes.Present).ToBe(False);
  finally
    Scopes.Free;
  end;
end;

procedure TConfigFileTests.TestFlagValueMustBeBoolean;
var
  Flag: TFlagOption;
  Options: TOptionArray;
  Message: string;
  IsUsageError: Boolean;
begin
  Flag := TFlagOption.Create('strict-types', 'Strict');
  try
    SetLength(Options, 1);
    Options[0] := Flag;
    Message := '';
    IsUsageError := False;
    try
      ApplyConfigEntries(SingleEntry('strict-types', 'yes'), Options);
    except
      on E: TParseError do
      begin
        Message := E.Message;
        IsUsageError := E is TCLIUsageError;
      end;
    end;
    Expect<string>(Message).ToBe('/project/goccia.json: "strict-types" ' +
      'must be true or false, got "yes"');
    Expect<Boolean>(IsUsageError).ToBe(False);
    ApplyConfigEntries(SingleEntry('strict-types', 'false', cvkBoolean),
      Options);
    Expect<Boolean>(Flag.Present).ToBe(False);
    { Only a boolean, never a string that spells one or null. }
    Expect<string>(ConfigErrorMessage(SingleEntry('strict-types', 'true'),
      Options, False)).ToBe('TParseError: /project/goccia.json: ' +
      '"strict-types" must be true or false, got "true"');
    Expect<string>(ConfigErrorMessage(SingleEntry('strict-types', '',
      cvkUnsupported), Options, True)).ToBe('TParseError: ' +
      '/project/goccia.json: "strict-types" must be true or false, got null');
    Expect<string>(ConfigErrorMessage(SingleEntry('strict-types', 'true',
      cvkBoolean, True), Options, True)).ToBe('TParseError: ' +
      '/project/goccia.json: "strict-types" must be true or false, got an ' +
      'array');
    Expect<Boolean>(Flag.Present).ToBe(False);
    ApplyConfigEntries(SingleEntry('strict-types', 'true', cvkBoolean),
      Options);
    Expect<Boolean>(Flag.Present).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestRequiresTrustKeySkipped;
var
  Flag: TFlagOption;
  Options: TOptionArray;
begin
  Flag := TFlagOption.Create('unsafe-thing', 'Unsafe');
  try
    Flag.RequiresTrust := True;
    SetLength(Options, 1);
    Options[0] := Flag;
    ApplyConfigEntries(SingleEntry('unsafe-thing', 'true', cvkBoolean),
      Options);
    Expect<Boolean>(Flag.Present).ToBe(False);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestValidateConfigEntries;
var
  Flag: TFlagOption;
  Removed: TRemovedOption;
  Options: TOptionArray;
  Raised: Boolean;
begin
  Flag := TFlagOption.Create('strict-types', 'Strict');
  Removed := TRemovedOption.Create('unsafe-ffi', 'unsafe-ffi',
    'use --allow-ffi instead', 'use "permissions" instead');
  try
    SetLength(Options, 2);
    Options[0] := Flag;
    Options[1] := Removed;
    ValidateConfigEntries(SingleEntry('strict-types', 'true', cvkBoolean),
      Options);
    Expect<Boolean>(Flag.Present).ToBe(False);
    ValidateConfigEntries(SingleEntry('unknown-key', 'x'), Options);
    Raised := False;
    try
      ValidateConfigEntries(SingleEntry('unsafe-ffi', 'true'), Options);
    except
      on E: TCLIUsageError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Flag.Free;
    Removed.Free;
  end;
end;

procedure TConfigFileTests.TestInvalidOptionValueNamesConfig;
var
  MaxMemory: TByteSizeOption;
  Options: TOptionArray;
  Message: string;
begin
  MaxMemory := TByteSizeOption.Create('max-memory', 'Memory');
  try
    SetLength(Options, 1);
    Options[0] := MaxMemory;
    Message := '';
    try
      ApplyConfigEntries(SingleEntry('max-memory', '64MB'), Options);
    except
      on E: TParseError do
        Message := E.Message;
    end;
    Expect<string>(Message).ToBe('Invalid value for "max-memory" in ' +
      '/project/goccia.json: 64MB ("MB" is ambiguous; use KiB, MiB, or GiB, ' +
      'or a plain byte count)');
    Expect<string>(ConfigErrorMessage(SingleEntry('max-memory', '64MB'),
      Options, True)).ToBe('TParseError: Invalid value for "max-memory" in ' +
      '/project/goccia.json: 64MB ("MB" is ambiguous; use KiB, MiB, or GiB, ' +
      'or a plain byte count)');
    { An object for a scalar option is rejected, not flattened away. }
    Expect<string>(ConfigErrorMessage(SingleEntry('max-memory', '',
      cvkObject), Options, True)).ToBe('TParseError: /project/goccia.json: ' +
      '"max-memory" must be a single value, not null or an object');
    { A scalar option takes one value, not an array. }
    Expect<string>(ConfigErrorMessage(SingleEntry('max-memory', '1', cvkNumber,
      True), Options, True)).ToBe('TParseError: /project/goccia.json: ' +
      '"max-memory" must be a single value, not an array');
    Expect<string>(ConfigErrorMessage(SingleEntry('max-memory', '',
      cvkUnsupported), Options, True)).ToBe('TParseError: ' +
      '/project/goccia.json: "max-memory" must be a single value, not null ' +
      'or an object');
    ApplyConfigEntries(SingleEntry('max-memory', '64MiB'), Options);
    Expect<Int64>(MaxMemory.Value).ToBe(64 * 1024 * 1024);
  finally
    MaxMemory.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionUsesConfigName;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('enable-ffi', 'FFI');
  try
    Flag.ConfigName := 'unsafe-ffi';
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'unsafe-ffi';
    FileConfig[0].Value := 'true';
    FileConfig[0].Kind := cvkBoolean;

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionMixedAliasExtendsPrecedence;
var
  Dir, BasePath, ChildPath: string;
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
begin
  Dir := CreateTempDirectory;
  BasePath := IncludeTrailingPathDelimiter(Dir) + 'base.json';
  ChildPath := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(BasePath, '{"unsafe-ffi": true}');
  WriteTextFile(ChildPath, '{"extends": "base.json", "enable-ffi": false}');

  Flag := TFlagOption.Create('enable-ffi', 'FFI');
  try
    Flag.ConfigName := 'unsafe-ffi';
    FileConfig := ParseConfigFile(ChildPath);

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(False);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionDefaultsFalse;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    { No CLI, no root config, no per-file config — should default to False }
    SetLength(FileConfig, 0);

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(False);
  finally
    Flag.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TConfigFileTests.Create('CLI ConfigFile'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
