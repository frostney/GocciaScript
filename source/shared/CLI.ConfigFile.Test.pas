program CLI.ConfigFile.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  TestingPascalLibrary,
  TextSemantics;

type
  TConfigFileTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;

    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteRawFile(const APath: string; const ABytes: RawByteString);
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
    procedure TestResolveFlagOptionEmptyStringEnablesFlag;
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
  Test('JSON: null is skipped', TestJSONNullSkipped);

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
  Test('ResolveFlagOption treats empty string as enabled', TestResolveFlagOptionEmptyStringEnablesFlag);
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

procedure TConfigFileTests.WriteRawFile(const APath: string;
  const ABytes: RawByteString);
var
  Stream: TFileStream;
{$IFDEF LAKON}
  Buffer: TBytes;
  Index: Integer;
{$ENDIF}
begin
  ForceDirectories(ExtractFileDir(APath));
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(ABytes) > 0 then
{$IFDEF LAKON}
    begin
      // Lakon's RawByteString aliases the one string type (bytes
      // ride one per code unit), and Pointer(S) is the string BLOCK,
      // not the payload — copy the low bytes out explicitly.
      SetLength(Buffer, Length(ABytes));
      for Index := 1 to Length(ABytes) do
        Buffer[Index - 1] := Ord(ABytes[Index]) and $FF;
      Stream.WriteBuffer(Buffer[0], Length(Buffer));
    end;
{$ELSE}
      Stream.WriteBuffer(Pointer(ABytes)^, Length(ABytes));
{$ENDIF}
  finally
    Stream.Free;
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
  WriteTextFile(Path, '{"mode": null}');

  Mode := TStringOption.Create('mode', 'Mode');
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
  JSON_BYTES = '{"mode":"Jos' + #$C3#$A9 + '","alias":["caf' + #$C3#$A9 +
    '=./d' + #$C3#$A9 + 'j' + #$C3#$A0 + '.js"]}';
var
  Alias: TRepeatableOption;
  Dir, Path: string;
  Mode: TStringOption;
  Options: TOptionArray;
begin
  Dir := CreateTempDirectory;
  Path := IncludeTrailingPathDelimiter(Dir) + 'config.json';
  WriteRawFile(Path, JSON_BYTES);

  Alias := TRepeatableOption.Create('alias', 'Alias');
  Mode := TStringOption.Create('mode', 'Mode');
  try
    SetLength(Options, 2);
    Options[0] := Mode;
    Options[1] := Alias;

    ApplyConfigFile(Path, Options);

    Expect<string>(Mode.Value).ToBe(RetagUTF8Text('Jos' + #$C3#$A9));
    Expect<Integer>(Alias.Values.Count).ToBe(1);
    Expect<string>(Alias.Values[0]).ToBe(RetagUTF8Text('caf' + #$C3#$A9 +
      '=./d' + #$C3#$A9 + 'j' + #$C3#$A0 + '.js'));
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
    ApplyConfigEntries(RootEntries, Options);

    { Per-file config says true — should override root }
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'feature';
    FileConfig[0].Value := 'true';

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
    ApplyConfigEntries(RootEntries, Options);
    Expect<Boolean>(Flag.Present).ToBe(True);

    { Per-file config says false — should override root }
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'feature';
    FileConfig[0].Value := 'false';

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
    ApplyConfigEntries(RootEntries, Options);

    { No per-file config — should fall back to root (Present=True) }
    SetLength(FileConfig, 0);

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(True);
  finally
    Flag.Free;
  end;
end;

procedure TConfigFileTests.TestResolveFlagOptionEmptyStringEnablesFlag;
var
  Flag: TFlagOption;
  FileConfig: TConfigEntryArray;
begin
  Flag := TFlagOption.Create('feature', 'Flag');
  try
    { Per-file config with empty string — matches ApplyConfigEntries behavior }
    SetLength(FileConfig, 1);
    FileConfig[0].Key := 'feature';
    FileConfig[0].Value := '';

    Expect<Boolean>(ResolveFlagOption(Flag, FileConfig)).ToBe(True);
  finally
    Flag.Free;
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
