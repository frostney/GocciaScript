program Goccia.Builtins.Testing.Snapshots.Test;

{$I Goccia.inc}

uses
  Classes,

  TestingPascalLibrary,

  Goccia.Builtins.Testing.Snapshots;

type
  TSnapshotStateTests = class(TTestSuite)
  private
    procedure TestLocalRunCreatesMissingSnapshot;
    procedure TestNoUpdateFailsMissingSnapshot;
    procedure TestNoUpdateFailsStaleSnapshot;
    procedure TestUpdateReplacesStaleAndRemovesObsolete;
    procedure TestSnapshotFileRoundTripsEscapedContent;
    procedure TestPropertyMismatchDoesNotWriteSnapshot;
    procedure TestInlineSnapshotQueuesSourceEdit;
    procedure TestExistingInlineSnapshotWorksWithoutPersistentSource;
    procedure TestMissingInlineSnapshotRejectsStdin;
    procedure TestSkippedTestPreservesSnapshots;
    procedure TestIncompleteRunPreservesUncheckedSnapshots;
    procedure TestUpdateDeletesEmptySnapshotFile;
    procedure TestSortsArbitrarilyLongNumericKeys;
    procedure TestSortsPunctuationLikeVitest;
    procedure TestNormalizesCRLFSnapshotValues;
    procedure TestIgnoresCommentedExports;
    procedure TestMalformedSnapshotDataBecomesEmpty;
  public
    procedure SetupTests; override;
  end;

procedure TSnapshotStateTests.SetupTests;
begin
  Test('Local run creates a missing snapshot',
    TestLocalRunCreatesMissingSnapshot);
  Test('No-update mode fails a missing snapshot',
    TestNoUpdateFailsMissingSnapshot);
  Test('No-update mode fails a stale snapshot',
    TestNoUpdateFailsStaleSnapshot);
  Test('Update mode replaces stale and removes obsolete snapshots',
    TestUpdateReplacesStaleAndRemovesObsolete);
  Test('Snapshot files round-trip escaped template content',
    TestSnapshotFileRoundTripsEscapedContent);
  Test('Property mismatch does not write a snapshot',
    TestPropertyMismatchDoesNotWriteSnapshot);
  Test('Inline snapshot queues a source edit',
    TestInlineSnapshotQueuesSourceEdit);
  Test('Existing inline snapshot works without persistent source',
    TestExistingInlineSnapshotWorksWithoutPersistentSource);
  Test('Missing inline snapshot rejects stdin',
    TestMissingInlineSnapshotRejectsStdin);
  Test('Skipped tests preserve their snapshots',
    TestSkippedTestPreservesSnapshots);
  Test('Incomplete runs preserve unchecked snapshots',
    TestIncompleteRunPreservesUncheckedSnapshots);
  Test('Update mode deletes an empty snapshot file',
    TestUpdateDeletesEmptySnapshotFile);
  Test('Snapshot sorting accepts arbitrarily long numeric keys',
    TestSortsArbitrarilyLongNumericKeys);
  Test('Snapshot sorting uses Vitest punctuation order',
    TestSortsPunctuationLikeVitest);
  Test('Snapshot values normalize CRLF line endings',
    TestNormalizesCRLFSnapshotValues);
  Test('Commented exports are ignored', TestIgnoresCommentedExports);
  Test('Malformed snapshot data is treated as empty',
    TestMalformedSnapshotDataBecomesEmpty);
end;

procedure TSnapshotStateTests.TestNormalizesCRLFSnapshotValues;
var
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile('// Snapshot v1' + #13#10#13#10 +
    'exports[`crlf 1`] = `' + #13#10 + 'value' + #13#10 + '`;' + #13#10);
  State := TGocciaSnapshotState.Create(HostRef, sumNone);
  try
    Expect<Boolean>(State.MatchExternal('crlf', '', #10 + 'value' + #10,
      True, Failure)).ToBe(True);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestIgnoresCommentedExports;
var
  Errors: TStringList;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile('// exports[`ignored 1`] = `ignored`;' + #10);
  State := TGocciaSnapshotState.Create(HostRef, sumNone);
  Errors := TStringList.Create;
  try
    State.Finish(Errors);
    Expect<Integer>(Errors.Count).ToBe(0);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestMalformedSnapshotDataBecomesEmpty;
var
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile('exports[`valid 1`] = `old`;' + #10 +
    '/* unterminated');
  State := TGocciaSnapshotState.Create(HostRef, sumNone);
  try
    Expect<Boolean>(State.MatchExternal('valid', '', 'old', True,
      Failure)).ToBe(False);
    Expect<Boolean>(Pos('is missing', Failure) > 0).ToBe(True);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestSortsArbitrarilyLongNumericKeys;
var
  Errors: TStringList;
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  Errors := TStringList.Create;
  try
    Expect<Boolean>(State.MatchExternal(
      'key 99999999999999999999999999999999999999999999999999', '',
      'large', True, Failure)).ToBe(True);
    Expect<Boolean>(State.MatchExternal('key 2', '', 'small', True,
      Failure)).ToBe(True);
    State.Finish(Errors);
    Expect<Integer>(Errors.Count).ToBe(0);
    Expect<Boolean>(Pos('key 2', Host.SnapshotContent) < Pos(
      'key 99999999999999999999999999999999999999999999999999',
      Host.SnapshotContent)).ToBe(True);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestSortsPunctuationLikeVitest;
var
  Errors: TStringList;
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  Errors := TStringList.Create;
  try
    Expect<Boolean>(State.MatchExternal('t -', '', 'dash', True,
      Failure)).ToBe(True);
    Expect<Boolean>(State.MatchExternal('t .', '', 'dot', True,
      Failure)).ToBe(True);
    State.Finish(Errors);
    Expect<Integer>(Errors.Count).ToBe(0);
    Expect<Boolean>(Pos('t . 1', Host.SnapshotContent) <
      Pos('t - 1', Host.SnapshotContent)).ToBe(True);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestSkippedTestPreservesSnapshots;
var
  Errors: TStringList;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile('// Snapshot v1' + #10#10 +
    'exports[`suite > skipped 1`] = `one`;' + #10#10 +
    'exports[`suite > skipped > hint 1`] = `two`;' + #10);
  State := TGocciaSnapshotState.Create(HostRef, sumAll);
  Errors := TStringList.Create;
  try
    State.PreserveTestSnapshots('suite > skipped');
    State.Finish(Errors);
    Expect<Boolean>(Host.SnapshotExists).ToBe(True);
    Expect<Boolean>(Pos('suite > skipped 1', Host.SnapshotContent) > 0)
      .ToBe(True);
    Expect<Boolean>(Pos('suite > skipped > hint 1', Host.SnapshotContent) > 0)
      .ToBe(True);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestIncompleteRunPreservesUncheckedSnapshots;
var
  Errors: TStringList;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile('// Snapshot v1' + #10#10 +
    'exports[`not run 1`] = `value`;' + #10);
  State := TGocciaSnapshotState.Create(HostRef, sumAll);
  Errors := TStringList.Create;
  try
    State.Finish(Errors, False);
    Expect<Boolean>(Host.SnapshotExists).ToBe(True);
    Expect<Boolean>(Pos('not run 1', Host.SnapshotContent) > 0).ToBe(True);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestUpdateDeletesEmptySnapshotFile;
var
  Errors: TStringList;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile(
    '// Vitest Snapshot v1, https://vitest.dev/guide/snapshot.html' + #10);
  State := TGocciaSnapshotState.Create(HostRef, sumAll);
  Errors := TStringList.Create;
  try
    State.Finish(Errors);
    Expect<Boolean>(Host.SnapshotExists).ToBe(False);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestLocalRunCreatesMissingSnapshot;
var
  Errors: TStringList;
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  Errors := TStringList.Create;
  try
    Expect<Boolean>(State.MatchExternal('suite > creates', '', '42', True,
      Failure)).ToBe(True);
    State.Finish(Errors);
    Expect<Integer>(Errors.Count).ToBe(0);
    Expect<Boolean>(Host.SnapshotExists).ToBe(True);
    Expect<Boolean>(Pos('exports[`suite > creates 1`] = `42`;',
      Host.SnapshotContent) > 0).ToBe(True);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestNoUpdateFailsMissingSnapshot;
var
  Failure: string;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  HostRef := TGocciaMemorySnapshotHost.Create;
  State := TGocciaSnapshotState.Create(HostRef, sumNone);
  try
    Expect<Boolean>(State.MatchExternal('missing', '', '42', True,
      Failure)).ToBe(False);
    Expect<Boolean>(Pos('is missing', Failure) > 0).ToBe(True);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestNoUpdateFailsStaleSnapshot;
var
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile('// Snapshot v1' + #10#10 +
    'exports[`stale 1`] = `old`;' + #10);
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  try
    Expect<Boolean>(State.MatchExternal('stale', '', 'new', True,
      Failure)).ToBe(False);
    Expect<Boolean>(Pos('mismatched', Failure) > 0).ToBe(True);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestUpdateReplacesStaleAndRemovesObsolete;
var
  Errors: TStringList;
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  Host.WriteSnapshotFile('// Snapshot v1' + #10#10 +
    'exports[`kept 1`] = `old`;' + #10#10 +
    'exports[`obsolete 1`] = `gone`;' + #10);
  State := TGocciaSnapshotState.Create(HostRef, sumAll);
  Errors := TStringList.Create;
  try
    Expect<Boolean>(State.MatchExternal('kept', '', 'new', True,
      Failure)).ToBe(True);
    State.Finish(Errors);
    Expect<Integer>(Errors.Count).ToBe(0);
    Expect<Boolean>(Pos('exports[`kept 1`] = `new`;',
      Host.SnapshotContent) > 0).ToBe(True);
    Expect<Boolean>(Pos('obsolete', Host.SnapshotContent) = 0).ToBe(True);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestSnapshotFileRoundTripsEscapedContent;
var
  Errors: TStringList;
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  Errors := TStringList.Create;
  try
    Expect<Boolean>(State.MatchExternal('escaped', '',
      'a`b\c${value}', True, Failure)).ToBe(True);
    State.Finish(Errors);
  finally
    Errors.Free;
    State.Free;
  end;

  State := TGocciaSnapshotState.Create(HostRef, sumNone);
  try
    Expect<Boolean>(State.MatchExternal('escaped', '',
      'a`b\c${value}', True, Failure)).ToBe(True);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestPropertyMismatchDoesNotWriteSnapshot;
var
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  State := TGocciaSnapshotState.Create(HostRef, sumAll);
  try
    Expect<Boolean>(State.MatchExternal('properties', '', '', False,
      Failure)).ToBe(False);
    Expect<string>(Failure).ToBe('Snapshot properties mismatched');
    Expect<Boolean>(Host.SnapshotExists).ToBe(False);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestInlineSnapshotQueuesSourceEdit;
var
  Errors: TStringList;
  Failure: string;
  Host: TGocciaMemorySnapshotHost;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  Host := TGocciaMemorySnapshotHost.Create;
  HostRef := Host;
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  Errors := TStringList.Create;
  try
    Expect<Boolean>(State.MatchInline('inline', '', 'value', '', False,
      True, 'inline.test.js', 4, 28, 0, Failure)).ToBe(True);
    State.Finish(Errors);
    Expect<Integer>(Host.InlineEditCount).ToBe(1);
    Expect<Integer>(Host.InlineEdit(0).Line).ToBe(4);
    Expect<string>(Host.InlineEdit(0).SourcePath).ToBe('inline.test.js');
    Expect<Integer>(Host.InlineEdit(0).Column).ToBe(28);
    Expect<Integer>(Host.InlineEdit(0).SnapshotArgumentIndex).ToBe(0);
    Expect<string>(Host.InlineEdit(0).Snapshot).ToBe('value');
    Expect<Boolean>(Host.Flushed).ToBe(True);
  finally
    Errors.Free;
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestExistingInlineSnapshotWorksWithoutPersistentSource;
var
  Failure: string;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  HostRef := TGocciaMemorySnapshotHost.Create(False);
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  try
    Expect<Boolean>(State.MatchInline('stdin', '', 'value', 'value', True,
      True, '', 1, 1, 0, Failure)).ToBe(True);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

procedure TSnapshotStateTests.TestMissingInlineSnapshotRejectsStdin;
var
  Failure: string;
  HostRef: IGocciaSnapshotHost;
  State: TGocciaSnapshotState;
begin
  HostRef := TGocciaMemorySnapshotHost.Create(False);
  State := TGocciaSnapshotState.Create(HostRef, sumNew);
  try
    Expect<Boolean>(State.MatchInline('stdin', '', 'value', '', False,
      True, '', 1, 1, 0, Failure)).ToBe(False);
    Expect<Boolean>(Pos('stdin', Failure) > 0).ToBe(True);
  finally
    State.Free;
    HostRef := nil;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TSnapshotStateTests.Create('Snapshot state'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
