program Goccia.Packages.ImportMapEdit.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Packages.ImportMapEdit,
  Goccia.TestSetup;

const
  NL = #10;

type
  TImportMapEditTests = class(TTestSuite)
  private
    function SetEntry(const AText, AKey, AValue: string): string;
    function RemoveEntry(const AText, AKey: string): string;
    procedure TestReplacesAValueInPlace;
    procedure TestAppendsKeepingIndentation;
    procedure TestCreatesImports;
    procedure TestRemovesEntries;
    procedure TestReadsEntries;
    procedure TestRefusesWhatItCannotEdit;
    procedure TestKeepsCRLF;
  public
    procedure SetupTests; override;
  end;

procedure TImportMapEditTests.SetupTests;
begin
  Test('Replaces a value in place', TestReplacesAValueInPlace);
  Test('Appends a member with the existing indentation',
    TestAppendsKeepingIndentation);
  Test('Creates imports when missing', TestCreatesImports);
  Test('Removes the first, a middle, the last, and the only entry',
    TestRemovesEntries);
  Test('Reads entries', TestReadsEntries);
  Test('Refuses text it cannot edit', TestRefusesWhatItCannotEdit);
  Test('Keeps CRLF line endings', TestKeepsCRLF);
end;

function TImportMapEditTests.SetEntry(const AText, AKey,
  AValue: string): string;
var
  Error: string;
begin
  if not TrySetImportMapEntry(AText, AKey, AValue, Result, Error) then
    Result := 'ERROR ' + Error;
end;

function TImportMapEditTests.RemoveEntry(const AText, AKey: string): string;
var
  Error: string;
begin
  if not TryRemoveImportMapEntry(AText, AKey, Result, Error) then
    Result := 'ERROR ' + Error;
end;

procedure TImportMapEditTests.TestReplacesAValueInPlace;
begin
  Expect<string>(SetEntry(
    '{ "mode":"bytecode",  "imports" : {"a":   "./a.js", "b": "./b.js"}}',
    'a', 'github:o/r@v2')).ToBe(
    '{ "mode":"bytecode",  "imports" : {"a":   "github:o/r@v2", ' +
    '"b": "./b.js"}}');
end;

procedure TImportMapEditTests.TestAppendsKeepingIndentation;
begin
  Expect<string>(SetEntry(
    '{' + NL + '    "imports": {' + NL + '        "@/": "./src/"' + NL +
    '    },' + NL + '    "mode": "bytecode"' + NL + '}' + NL,
    'raylib', 'github:o/r@v1/x.ts')).ToBe(
    '{' + NL + '    "imports": {' + NL + '        "@/": "./src/",' + NL +
    '        "raylib": "github:o/r@v1/x.ts"' + NL +
    '    },' + NL + '    "mode": "bytecode"' + NL + '}' + NL);
  Expect<string>(SetEntry('{"imports": {"a": "./a.js"}}', 'b', './b.js'))
    .ToBe('{"imports": {"a": "./a.js", "b": "./b.js"}}');
  Expect<string>(SetEntry('{"imports": {}}', 'b', './b.js'))
    .ToBe('{"imports": {' + NL + '  "b": "./b.js"' + NL + '}}');
end;

procedure TImportMapEditTests.TestCreatesImports;
begin
  Expect<string>(SetEntry('{' + NL + '  "mode": "bytecode"' + NL + '}' + NL,
    'k', 'v')).ToBe('{' + NL + '  "mode": "bytecode",' + NL +
    '  "imports": {' + NL + '    "k": "v"' + NL + '  }' + NL + '}' + NL);
  Expect<string>(NewImportMapText('k', 'v')).ToBe('{' + NL +
    '  "imports": {' + NL + '    "k": "v"' + NL + '  }' + NL + '}' + NL);
end;

procedure TImportMapEditTests.TestRemovesEntries;
var
  Text: string;
begin
  Text := '{' + NL + '  "imports": {' + NL + '    "a": "1",' + NL +
    '    "b": "2",' + NL + '    "c": "3"' + NL + '  }' + NL + '}';
  Expect<string>(RemoveEntry(Text, 'a')).ToBe('{' + NL + '  "imports": {' +
    NL + '    "b": "2",' + NL + '    "c": "3"' + NL + '  }' + NL + '}');
  Expect<string>(RemoveEntry(Text, 'b')).ToBe('{' + NL + '  "imports": {' +
    NL + '    "a": "1",' + NL + '    "c": "3"' + NL + '  }' + NL + '}');
  Expect<string>(RemoveEntry(Text, 'c')).ToBe('{' + NL + '  "imports": {' +
    NL + '    "a": "1",' + NL + '    "b": "2"' + NL + '  }' + NL + '}');
  Expect<string>(RemoveEntry('{"imports": {"a": "1"}, "x": 1}', 'a'))
    .ToBe('{"imports": {}, "x": 1}');
  Expect<Boolean>(Pos('ERROR', RemoveEntry(Text, 'z')) = 1).ToBe(True);
end;

procedure TImportMapEditTests.TestReadsEntries;
var
  Entries: TGocciaImportMapEntries;
  Error: string;
begin
  Expect<Boolean>(TryReadImportMapEntries(
    '{"x": [1, {"y": null}], "imports": {"ab": "v\"1", "c": "d"}}',
    Entries, Error)).ToBe(True);
  Expect<Integer>(Length(Entries)).ToBe(2);
  Expect<string>(Entries[0].Key).ToBe('ab');
  Expect<string>(Entries[0].Value).ToBe('v"1');
  Expect<Boolean>(TryReadImportMapEntries('{"mode": "x"}', Entries, Error))
    .ToBe(True);
  Expect<Integer>(Length(Entries)).ToBe(0);
end;

procedure TImportMapEditTests.TestRefusesWhatItCannotEdit;
var
  Entries: TGocciaImportMapEntries;
  Error: string;
begin
  Expect<Boolean>(TryReadImportMapEntries('[]', Entries, Error)).ToBe(False);
  Expect<Boolean>(TryReadImportMapEntries('{"imports": []}', Entries, Error))
    .ToBe(False);
  Expect<Boolean>(TryReadImportMapEntries('{"imports": {"a": 1}}', Entries,
    Error)).ToBe(False);
  Expect<Boolean>(TryReadImportMapEntries(
    '{"imports": {"a": "1", "a": "2"}}', Entries, Error)).ToBe(False);
  Expect<Boolean>(TryReadImportMapEntries('{"imports": {}} x', Entries,
    Error)).ToBe(False);
  Expect<Boolean>(TryReadImportMapEntries('{"imports": {}, }', Entries,
    Error)).ToBe(False);
  Expect<Boolean>(Pos('ERROR', SetEntry('{// comment' + NL + '}', 'k', 'v')) =
    1).ToBe(True);
end;

procedure TImportMapEditTests.TestKeepsCRLF;
begin
  Expect<string>(SetEntry('{' + #13#10 + '  "imports": {' + #13#10 +
    '    "a": "1"' + #13#10 + '  }' + #13#10 + '}', 'b', '2')).ToBe(
    '{' + #13#10 + '  "imports": {' + #13#10 + '    "a": "1",' + #13#10 +
    '    "b": "2"' + #13#10 + '  }' + #13#10 + '}');
end;

begin
  TestRunnerProgram.AddSuite(TImportMapEditTests.Create('Import map edits'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
