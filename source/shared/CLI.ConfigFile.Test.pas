program CLI.ConfigFile.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  TestingPascalLibrary;

type
  TConfigFileTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;

    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteTextFile(const APath, AText: string);

    { ParseJSONConfig (built-in) }
    procedure TestJSONStringValue;
    procedure TestJSONIntegerValue;
    procedure TestJSONBooleanTrueIncluded;
    procedure TestJSONBooleanFalseOmitted;
    procedure TestJSONArrayProducesMultipleEntries;
    procedure TestJSONNestedObjectSkipped;
    procedure TestJSONNullSkipped;

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
    procedure TestApplyConfigFileUnregisteredExtensionRaises;

    { CLI override }
    procedure TestCLIOverridesConfigValue;
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
  Test('JSON: null is skipped', TestJSONNullSkipped);

  Test('ApplyConfigEntries sets flag option', TestApplyFlagOption);
  Test('ApplyConfigEntries does not set flag for false', TestApplyFlagFalseNotSet);
  Test('ApplyConfigEntries sets string option', TestApplyStringOption);
  Test('ApplyConfigEntries sets integer option', TestApplyIntegerOption);
  Test('ApplyConfigEntries sets enum option', TestApplyEnumOption);
  Test('ApplyConfigEntries accumulates repeatable values', TestApplyRepeatableOption);
  Test('ApplyConfigEntries skips unknown keys', TestApplyUnknownKeySkipped);

  Test('ApplyConfigFile loads a JSON file', TestApplyConfigFileJSON);
  Test('ApplyConfigFile raises for unregistered extension', TestApplyConfigFileUnregisteredExtensionRaises);

  Test('CLI argument overrides config file value', TestCLIOverridesConfigValue);
  Test('ApplyConfigEntries skips already-present options', TestApplySkipsAlreadyPresentOption);

  Test('extends loads base config entries', TestExtendsLoadsBaseConfig);
  Test('extends child overrides parent values', TestExtendsChildOverridesParent);
  Test('extends circular chain raises', TestExtendsCircularRaises);

  Test('RegisterConfigParser enables custom format', TestRegisterAndUseCustomParser);

  Test('DiscoverConfigFile finds file in start directory', TestDiscoverFindsFileInStartDirectory);
  Test('DiscoverConfigFile walks up to parent', TestDiscoverWalksUpToParent);
  Test('DiscoverConfigFile respects extension priority', TestDiscoverRespectsExtensionPriority);
  Test('DiscoverConfigFile returns empty when not found', TestDiscoverReturnsEmptyWhenNotFound);
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
var
  Lines: TStringList;
begin
  ForceDirectories(ExtractFileDir(APath));
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    Lines.SaveToFile(APath);
  finally
    Lines.Free;
  end;
end;

{ ── JSON parsing tests ─────────────────────────────────────── }

procedure TConfigFileTests.TestJSONStringValue;
var
  Dir, Path: string;
  Flag: TGocciaFlagOption;
  Mode: TGocciaStringOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"mode": "bytecode"}');

  Mode := TGocciaStringOption.Create('mode', 'Execution mode');
  Flag := TGocciaFlagOption.Create('asi', 'ASI');
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
  Timeout: TGocciaIntegerOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"timeout": 5000}');

  Timeout := TGocciaIntegerOption.Create('timeout', 'Timeout');
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
  ASI: TGocciaFlagOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"asi": true}');

  ASI := TGocciaFlagOption.Create('asi', 'ASI');
  try
    SetLength(Options, 1);
    Options[0] := ASI;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(ASI.Present).ToBe(True);
  finally
    ASI.Free;
  end;
end;

procedure TConfigFileTests.TestJSONBooleanFalseOmitted;
var
  Dir, Path: string;
  ASI: TGocciaFlagOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"asi": false}');

  ASI := TGocciaFlagOption.Create('asi', 'ASI');
  try
    SetLength(Options, 1);
    Options[0] := ASI;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(ASI.Present).ToBe(False);
  finally
    ASI.Free;
  end;
end;

procedure TConfigFileTests.TestJSONArrayProducesMultipleEntries;
var
  Dir, Path: string;
  Aliases: TGocciaRepeatableOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"alias": ["@/=./src/", "config=./config.js"]}');

  Aliases := TGocciaRepeatableOption.Create('alias', 'Aliases');
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
  Mode: TGocciaStringOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"imports": {"@/": "./src/"}, "mode": "bytecode"}');

  Mode := TGocciaStringOption.Create('mode', 'Mode');
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
  Mode: TGocciaStringOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'test.json';
  WriteTextFile(Path, '{"mode": null}');

  Mode := TGocciaStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 1);
    Options[0] := Mode;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(Mode.Present).ToBe(False);
  finally
    Mode.Free;
  end;
end;

{ ── ApplyConfigEntries tests ───────────────────────────────── }

procedure TConfigFileTests.TestApplyFlagOption;
var
  Flag: TGocciaFlagOption;
  Options: TGocciaOptionArray;
  Entries: TConfigEntryArray;
begin
  Flag := TGocciaFlagOption.Create('asi', 'ASI');
  try
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(Entries, 1);
    Entries[0].Key := 'asi';
    Entries[0].Value := 'true';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Flag.Present).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestApplyFlagFalseNotSet;
var
  Flag: TGocciaFlagOption;
  Options: TGocciaOptionArray;
  Entries: TConfigEntryArray;
begin
  Flag := TGocciaFlagOption.Create('asi', 'ASI');
  try
    SetLength(Options, 1);
    Options[0] := Flag;
    SetLength(Entries, 1);
    Entries[0].Key := 'asi';
    Entries[0].Value := 'false';

    ApplyConfigEntries(Entries, Options);

    Expect<Boolean>(Flag.Present).ToBe(False);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestApplyStringOption;
var
  Opt: TGocciaStringOption;
  Options: TGocciaOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TGocciaStringOption.Create('output', 'Output path');
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
  Opt: TGocciaIntegerOption;
  Options: TGocciaOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TGocciaIntegerOption.Create('timeout', 'Timeout');
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
  Opt: TGocciaEnumOption<TTestMode>;
  Options: TGocciaOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TGocciaEnumOption<TTestMode>.Create('mode', 'Execution mode');
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
  Opt: TGocciaRepeatableOption;
  Options: TGocciaOptionArray;
  Entries: TConfigEntryArray;
begin
  Opt := TGocciaRepeatableOption.Create('alias', 'Aliases');
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
  Flag: TGocciaFlagOption;
  Options: TGocciaOptionArray;
  Entries: TConfigEntryArray;
begin
  Flag := TGocciaFlagOption.Create('asi', 'ASI');
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
  ASI: TGocciaFlagOption;
  Timeout: TGocciaIntegerOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteTextFile(Path, '{"asi": true, "timeout": 2000}');

  ASI := TGocciaFlagOption.Create('asi', 'ASI');
  Timeout := TGocciaIntegerOption.Create('timeout', 'Timeout');
  try
    SetLength(Options, 2);
    Options[0] := ASI;
    Options[1] := Timeout;

    ApplyConfigFile(Path, Options);

    Expect<Boolean>(ASI.Present).ToBe(True);
    Expect<Boolean>(Timeout.Present).ToBe(True);
    Expect<Integer>(Timeout.Value).ToBe(2000);
  finally
    ASI.Free;
    Timeout.Free;
  end;
end;

procedure TConfigFileTests.TestApplyConfigFileUnregisteredExtensionRaises;
var
  Dir, Path: string;
  Flag: TGocciaFlagOption;
  Options: TGocciaOptionArray;
  Raised: Boolean;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.xyz';
  WriteTextFile(Path, 'whatever');

  Flag := TGocciaFlagOption.Create('asi', 'ASI');
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

procedure TConfigFileTests.TestCLIOverridesConfigValue;
var
  Dir, Path: string;
  Mode: TGocciaStringOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteTextFile(Path, '{"mode": "bytecode"}');

  Mode := TGocciaStringOption.Create('mode', 'Mode');
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
  Mode: TGocciaStringOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteTextFile(Path, '{"mode": "bytecode"}');

  Mode := TGocciaStringOption.Create('mode', 'Mode');
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
  ASI: TGocciaFlagOption;
  Timeout: TGocciaIntegerOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  BasePath := IncludeTrailingPathDelimiter(Dir) + 'base.json';
  ChildPath := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(BasePath, '{"asi": true, "timeout": 3000}');
  WriteTextFile(ChildPath, '{"extends": "base.json"}');

  ASI := TGocciaFlagOption.Create('asi', 'ASI');
  Timeout := TGocciaIntegerOption.Create('timeout', 'Timeout');
  try
    SetLength(Options, 2);
    Options[0] := ASI;
    Options[1] := Timeout;

    ApplyConfigFile(ChildPath, Options);

    Expect<Boolean>(ASI.Present).ToBe(True);
    Expect<Boolean>(Timeout.Present).ToBe(True);
    Expect<Integer>(Timeout.Value).ToBe(3000);
  finally
    ASI.Free;
    Timeout.Free;
  end;
end;

procedure TConfigFileTests.TestExtendsChildOverridesParent;
var
  Dir, BasePath, ChildPath: string;
  Mode: TGocciaStringOption;
  ASI: TGocciaFlagOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  BasePath := IncludeTrailingPathDelimiter(Dir) + 'base.json';
  ChildPath := IncludeTrailingPathDelimiter(Dir) + 'goccia.json';
  WriteTextFile(BasePath, '{"mode": "interpreted", "asi": true}');
  WriteTextFile(ChildPath, '{"extends": "base.json", "mode": "bytecode"}');

  Mode := TGocciaStringOption.Create('mode', 'Mode');
  ASI := TGocciaFlagOption.Create('asi', 'ASI');
  try
    SetLength(Options, 2);
    Options[0] := Mode;
    Options[1] := ASI;

    ApplyConfigFile(ChildPath, Options);

    { Child's mode overrides parent's }
    Expect<string>(Mode.Value).ToBe('bytecode');
    { Parent's asi is inherited }
    Expect<Boolean>(ASI.Present).ToBe(True);
  finally
    Mode.Free;
    ASI.Free;
  end;
end;

procedure TConfigFileTests.TestExtendsCircularRaises;
var
  Dir, PathA, PathB: string;
  Flag: TGocciaFlagOption;
  Options: TGocciaOptionArray;
  Raised: Boolean;
begin
  Dir := CreateTempDirectory;
  PathA := IncludeTrailingPathDelimiter(Dir) + 'a.json';
  PathB := IncludeTrailingPathDelimiter(Dir) + 'b.json';
  WriteTextFile(PathA, '{"extends": "b.json"}');
  WriteTextFile(PathB, '{"extends": "a.json"}');

  Flag := TGocciaFlagOption.Create('asi', 'ASI');
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
  Mode: TGocciaStringOption;
  Options: TGocciaOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.toml';
  WriteTextFile(Path, 'mode = "bytecode"');

  RegisterConfigParser('.toml', @DummyTOMLParser);

  Mode := TGocciaStringOption.Create('mode', 'Mode');
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

begin
  TestRunnerProgram.AddSuite(TConfigFileTests.Create('CLI ConfigFile'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
