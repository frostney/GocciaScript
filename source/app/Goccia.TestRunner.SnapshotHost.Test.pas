program Goccia.TestRunner.SnapshotHost.Test;

{$I Goccia.inc}

uses
  Classes,
  StrUtils,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Builtins.Testing.Snapshots,
  Goccia.TestRunner.SnapshotHost,
  Goccia.TextFiles;

type
  TSnapshotHostTests = class(TTestSuite)
  private
    FTempDir: string;

    procedure WriteSource(const APath, ASource: string);
    procedure RemoveTree(const APath: string);
    function MethodCallColumn(const ALine: string): Integer;

    procedure TestWritesExternalSnapshotBesideSource;
    procedure TestInsertsInlineSnapshot;
    procedure TestReplacesExistingInlineSnapshot;
    procedure TestPreservesPropertyShapeArgument;
    procedure TestRewritesTwoCallsOnSameLine;
    procedure TestResolvesArgumentDebugLocation;
    procedure TestRewritesSchemaShapeWithArrowRegExp;
    procedure TestRewritesCapturedModuleSource;
    procedure TestResolvesExpressionDebugLocation;
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;
    procedure AfterEach; override;
  end;

procedure TSnapshotHostTests.SetupTests;
begin
  Test('Writes external snapshot beside source',
    TestWritesExternalSnapshotBesideSource);
  Test('Inserts an inline snapshot', TestInsertsInlineSnapshot);
  Test('Replaces an existing inline snapshot',
    TestReplacesExistingInlineSnapshot);
  Test('Preserves property shape argument',
    TestPreservesPropertyShapeArgument);
  Test('Rewrites two calls on the same line',
    TestRewritesTwoCallsOnSameLine);
  Test('Resolves bytecode argument debug location',
    TestResolvesArgumentDebugLocation);
  Test('Rewrites schema shape containing an arrow regular expression',
    TestRewritesSchemaShapeWithArrowRegExp);
  Test('Rewrites the source captured at the assertion call site',
    TestRewritesCapturedModuleSource);
  Test('Resolves bytecode expression-start debug locations',
    TestResolvesExpressionDebugLocation);
end;

procedure TSnapshotHostTests.BeforeEach;
begin
  ResetPendingInlineSnapshots;
  FTempDir := GetTempDir + 'goccia_snapshot_host_' + IntToStr(Random(MaxInt));
  ForceDirectories(FTempDir);
end;

procedure TSnapshotHostTests.RemoveTree(const APath: string);
var
  ChildPath: string;
  Search: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
    Search) = 0 then
  try
    repeat
      if (Search.Name = '.') or (Search.Name = '..') then
        Continue;
      ChildPath := IncludeTrailingPathDelimiter(APath) + Search.Name;
      if (Search.Attr and faDirectory) <> 0 then
        RemoveTree(ChildPath)
      else
        DeleteFile(ChildPath);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(APath);
end;

procedure TSnapshotHostTests.AfterEach;
begin
  if DirectoryExists(FTempDir) then
    RemoveTree(FTempDir);
end;

procedure TSnapshotHostTests.WriteSource(const APath, ASource: string);
var
  Stream: TFileStream;
  Content: UTF8String;
begin
  Content := UTF8String(ASource);
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(Content) > 0 then
      Stream.WriteBuffer(Content[1], Length(Content));
  finally
    Stream.Free;
  end;
end;

function TSnapshotHostTests.MethodCallColumn(const ALine: string): Integer;
begin
  Result := Pos('toMatchInlineSnapshot(', ALine) +
    Length('toMatchInlineSnapshot');
end;

procedure TSnapshotHostTests.TestWritesExternalSnapshotBesideSource;
var
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
  SnapshotPath, SourcePath: string;
begin
  SourcePath := FTempDir + PathDelim + 'example.test.js';
  WriteSource(SourcePath, 'test("example", () => {});' + #10);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    Host.WriteSnapshotFile('// snapshot' + #10);
    SnapshotPath := FTempDir + PathDelim + '__snapshots__' + PathDelim +
      'example.test.js.snap';
    Expect<Boolean>(FileExists(SnapshotPath)).ToBe(True);
    Expect<string>(string(ReadUTF8FileText(SnapshotPath))).ToBe(
      '// snapshot' + #10);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestInsertsInlineSnapshot;
var
  Edit: TGocciaInlineSnapshotEdit;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
  Line, Source, SourcePath: string;
begin
  Line := '  expect("hello").toMatchInlineSnapshot();';
  Source := 'test("example", () => {' + #10 + Line + #10 + '});' + #10;
  SourcePath := FTempDir + PathDelim + 'inline.test.js';
  WriteSource(SourcePath, Source);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    Edit.Line := 2;
    Edit.Column := MethodCallColumn(Line);
    Edit.SnapshotArgumentIndex := 0;
    Edit.Snapshot := '"hello"';
    Host.QueueInlineSnapshot(Edit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    Source := string(ReadUTF8FileText(SourcePath));
    Expect<Boolean>(Pos('toMatchInlineSnapshot(`"hello"`)', Source) > 0)
      .ToBe(True);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestReplacesExistingInlineSnapshot;
var
  Edit: TGocciaInlineSnapshotEdit;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
  Line, Source, SourcePath: string;
begin
  Line := '  expect("new").toMatchInlineSnapshot(`"old"`);';
  Source := 'test("example", () => {' + #10 + Line + #10 + '});' + #10;
  SourcePath := FTempDir + PathDelim + 'replace.test.js';
  WriteSource(SourcePath, Source);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    Edit.Line := 2;
    Edit.Column := MethodCallColumn(Line);
    Edit.SnapshotArgumentIndex := 0;
    Edit.Snapshot := '"new"';
    Host.QueueInlineSnapshot(Edit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    Source := string(ReadUTF8FileText(SourcePath));
    Expect<Boolean>(Pos('toMatchInlineSnapshot(`"new"`)', Source) > 0)
      .ToBe(True);
    Expect<Boolean>(Pos('"old"', Source) = 0).ToBe(True);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestPreservesPropertyShapeArgument;
var
  Edit: TGocciaInlineSnapshotEdit;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
  Line, Source, SourcePath: string;
begin
  Line := '  expect(value).toMatchInlineSnapshot({ value: ' +
    'expect.stringMatching(/a[)]/) });';
  Source := 'test("example", () => {' + #10 + Line + #10 + '});' + #10;
  SourcePath := FTempDir + PathDelim + 'properties.test.js';
  WriteSource(SourcePath, Source);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    Edit.Line := 2;
    Edit.Column := MethodCallColumn(Line);
    Edit.SnapshotArgumentIndex := 1;
    Edit.Snapshot := '{' + #10 + '  "value": StringMatching /a[)]/,' + #10 + '}';
    Host.QueueInlineSnapshot(Edit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    Source := string(ReadUTF8FileText(SourcePath));
    Expect<Boolean>(Pos('expect.stringMatching(/a[)]/) }, `', Source) > 0)
      .ToBe(True);
    Expect<Boolean>(Pos('  {' + #10 + '    "value"', Source) > 0).ToBe(True);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestRewritesTwoCallsOnSameLine;
var
  FirstEdit, SecondEdit: TGocciaInlineSnapshotEdit;
  FirstName, Line, Source, SourcePath: string;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
begin
  FirstName := 'toMatchInlineSnapshot(';
  Line := 'expect("left").toMatchInlineSnapshot(); ' +
    'expect("right").toMatchInlineSnapshot();';
  SourcePath := FTempDir + PathDelim + 'same-line.test.js';
  WriteSource(SourcePath, Line + #10);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    FirstEdit.Line := 1;
    FirstEdit.Column := Pos(FirstName, Line) + Length(FirstName) - 1;
    FirstEdit.SnapshotArgumentIndex := 0;
    FirstEdit.Snapshot := '"left"';
    SecondEdit := FirstEdit;
    SecondEdit.Column := PosEx(FirstName, Line,
      FirstEdit.Column + 1) + Length(FirstName) - 1;
    SecondEdit.Snapshot := '"right"';
    Host.QueueInlineSnapshot(FirstEdit);
    Host.QueueInlineSnapshot(SecondEdit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    Source := string(ReadUTF8FileText(SourcePath));
    Expect<Boolean>(Pos('toMatchInlineSnapshot(`"left"`)', Source) > 0)
      .ToBe(True);
    Expect<Boolean>(Pos('toMatchInlineSnapshot(`"right"`)', Source) > 0)
      .ToBe(True);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestResolvesArgumentDebugLocation;
var
  Edit: TGocciaInlineSnapshotEdit;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
  Line, Source, SourcePath: string;
begin
  Line := '  expect("new").toMatchInlineSnapshot(`"old"`);';
  Source := 'test("example", () => {' + #10 + Line + #10 + '});' + #10;
  SourcePath := FTempDir + PathDelim + 'debug-location.test.js';
  WriteSource(SourcePath, Source);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    Edit.Line := 2;
    Edit.Column := Pos('`"old"`', Line);
    Edit.SnapshotArgumentIndex := 0;
    Edit.Snapshot := '"new"';
    Host.QueueInlineSnapshot(Edit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    Source := string(ReadUTF8FileText(SourcePath));
    Expect<Boolean>(Pos('toMatchInlineSnapshot(`"new"`)', Source) > 0)
      .ToBe(True);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestRewritesSchemaShapeWithArrowRegExp;
var
  Edit: TGocciaInlineSnapshotEdit;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
  Line, Source, SourcePath: string;
begin
  Line := '  expect(value).toMatchInlineSnapshot({ value: ' +
    'expect.schemaMatching({"~standard": { validate: value => ' +
    '/}/.test(value) ? { value } : { issues: [1] }, ' +
    'check(value) { return /[,)]/.test(value); } }}) });';
  Source := 'test("example", () => {' + #10 + Line + #10 + '});' + #10;
  SourcePath := FTempDir + PathDelim + 'schema-regexp.test.js';
  WriteSource(SourcePath, Source);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    Edit.Line := 2;
    Edit.Column := MethodCallColumn(Line);
    Edit.SnapshotArgumentIndex := 1;
    Edit.Snapshot := '{' + #10 + '  "value": SchemaMatching,' + #10 + '}';
    Host.QueueInlineSnapshot(Edit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    Source := string(ReadUTF8FileText(SourcePath));
    Expect<Boolean>(Pos('/}/.test(value)', Source) > 0).ToBe(True);
    Expect<Boolean>(Pos('/[,)]/.test(value)', Source) > 0).ToBe(True);
    Expect<Boolean>(Pos('}), `' + #10 + '    {', Source) > 0).ToBe(True);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestRewritesCapturedModuleSource;
var
  Edit: TGocciaInlineSnapshotEdit;
  EntryPath, HelperPath, HelperSource, Line: string;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
begin
  EntryPath := FTempDir + PathDelim + 'entry.test.js';
  HelperPath := FTempDir + PathDelim + 'helper.js';
  WriteSource(EntryPath, 'import "./helper.js";' + #10);
  Line := 'expect("helper").toMatchInlineSnapshot();';
  WriteSource(HelperPath, Line + #10);
  Host := TGocciaTestRunnerSnapshotHost.Create(EntryPath);
  HostRef := Host;
  try
    Edit.SourcePath := HelperPath;
    Edit.Line := 1;
    Edit.Column := MethodCallColumn(Line);
    Edit.SnapshotArgumentIndex := 0;
    Edit.Snapshot := '"helper"';
    Host.QueueInlineSnapshot(Edit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    HelperSource := string(ReadUTF8FileText(HelperPath));
    Expect<Boolean>(Pos('toMatchInlineSnapshot(`"helper"`)',
      HelperSource) > 0).ToBe(True);
    Expect<string>(string(ReadUTF8FileText(EntryPath))).ToBe(
      'import "./helper.js";' + #10);
  finally
    HostRef := nil;
  end;
end;

procedure TSnapshotHostTests.TestResolvesExpressionDebugLocation;
var
  Edit: TGocciaInlineSnapshotEdit;
  Host: TGocciaTestRunnerSnapshotHost;
  HostRef: IGocciaSnapshotHost;
  Line, Source, SourcePath: string;
begin
  Line := '  expect("new").toMatchInlineSnapshot();';
  Source := 'test("example", () => {' + #10 + Line + #10 + '});' + #10;
  SourcePath := FTempDir + PathDelim + 'expression-location.test.js';
  WriteSource(SourcePath, Source);
  Host := TGocciaTestRunnerSnapshotHost.Create(SourcePath);
  HostRef := Host;
  try
    Edit.Line := 2;
    Edit.Column := Pos('expect(', Line);
    Edit.SnapshotArgumentIndex := 0;
    Edit.Snapshot := '"new"';
    Host.QueueInlineSnapshot(Edit);
    Host.FlushInlineSnapshots;
    FlushPendingInlineSnapshots;
    Source := string(ReadUTF8FileText(SourcePath));
    Expect<Boolean>(Pos('toMatchInlineSnapshot(`"new"`)', Source) > 0)
      .ToBe(True);
  finally
    HostRef := nil;
  end;
end;

begin
  Randomize;
  TestRunnerProgram.AddSuite(TSnapshotHostTests.Create('Snapshot file host'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
