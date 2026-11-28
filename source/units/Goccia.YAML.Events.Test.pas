program Goccia.YAML.Events.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.YAML.Events,
  Goccia.YAML.Scanner;

type
  TYAMLEventParserTests = class(TTestSuite)
  private
    function EventLines(const AText: string): string;
    function ErrorMessage(const AText: string): string;
    procedure TestBlockMappingWithAnchorAndAlias;
    procedure TestFlowSequencePairIsAFlowMapping;
    procedure TestExplicitKeyHoldsABlockSequence;
    procedure TestNodePropertiesGoToTheFirstKey;
    procedure TestCRLFBreaksReadAsLineFeeds;
    procedure TestSurrogatePairEscapesSpellOneCharacter;
    procedure TestDirectivesNeedADocumentStart;
    procedure TestErrorsNameTheirLine;
    procedure TestUnknownAliasIsAnError;
  public
    procedure SetupTests; override;
  end;

procedure TYAMLEventParserTests.SetupTests;
begin
  Test('a block mapping with an anchor and an alias',
    TestBlockMappingWithAnchorAndAlias);
  Test('a pair in a flow sequence is a flow mapping',
    TestFlowSequencePairIsAFlowMapping);
  Test('an explicit key holds a block sequence',
    TestExplicitKeyHoldsABlockSequence);
  Test('node properties before a compact mapping go to its first key',
    TestNodePropertiesGoToTheFirstKey);
  Test('CRLF breaks read as line feeds', TestCRLFBreaksReadAsLineFeeds);
  Test('surrogate pair escapes spell one character',
    TestSurrogatePairEscapesSpellOneCharacter);
  Test('directives need a document start', TestDirectivesNeedADocumentStart);
  Test('errors name their line', TestErrorsNameTheirLine);
  Test('an unknown alias is an error', TestUnknownAliasIsAnError);
end;

{ the stream's events in test.event notation, one per line }
function TYAMLEventParserTests.EventLines(const AText: string): string;
var
  Event: TGocciaYAMLEvent;
  Events: TGocciaYAMLEventParser;
begin
  Result := '';
  Events := TGocciaYAMLEventParser.Create(AText);
  try
    repeat
      Event := Events.NextEvent;
      Result := Result + YAMLEventText(Event) + #10;
    until Event.Kind = yekStreamEnd;
  finally
    Events.Free;
  end;
end;

{ the message the event parser rejects AText with, '' when it reads it }
function TYAMLEventParserTests.ErrorMessage(const AText: string): string;
begin
  Result := '';
  try
    EventLines(AText);
  except
    on E: EGocciaYAMLParseError do
      Result := E.Message;
  end;
end;

procedure TYAMLEventParserTests.TestBlockMappingWithAnchorAndAlias;
begin
  Expect<string>(EventLines('a: &x 1' + #10 + 'b: *x' + #10)).ToBe(
    '+STR' + #10 + '+DOC' + #10 + '+MAP' + #10 + '=VAL :a' + #10 +
    '=VAL &x :1' + #10 + '=VAL :b' + #10 + '=ALI *x' + #10 + '-MAP' + #10 +
    '-DOC' + #10 + '-STR' + #10);
end;

procedure TYAMLEventParserTests.TestFlowSequencePairIsAFlowMapping;
begin
  Expect<string>(EventLines('[a: b, c]')).ToBe(
    '+STR' + #10 + '+DOC' + #10 + '+SEQ []' + #10 + '+MAP {}' + #10 +
    '=VAL :a' + #10 + '=VAL :b' + #10 + '-MAP' + #10 + '=VAL :c' + #10 +
    '-SEQ' + #10 + '-DOC' + #10 + '-STR' + #10);
end;

procedure TYAMLEventParserTests.TestExplicitKeyHoldsABlockSequence;
begin
  Expect<string>(EventLines('? - a' + #10 + ': b' + #10)).ToBe(
    '+STR' + #10 + '+DOC' + #10 + '+MAP' + #10 + '+SEQ' + #10 +
    '=VAL :a' + #10 + '-SEQ' + #10 + '=VAL :b' + #10 + '-MAP' + #10 +
    '-DOC' + #10 + '-STR' + #10);
end;

procedure TYAMLEventParserTests.TestNodePropertiesGoToTheFirstKey;
begin
  Expect<string>(EventLines('- &k !!str a: 1' + #10)).ToBe(
    '+STR' + #10 + '+DOC' + #10 + '+SEQ' + #10 + '+MAP' + #10 +
    '=VAL &k <tag:yaml.org,2002:str> :a' + #10 + '=VAL :1' + #10 +
    '-MAP' + #10 + '-SEQ' + #10 + '-DOC' + #10 + '-STR' + #10);
end;

procedure TYAMLEventParserTests.TestCRLFBreaksReadAsLineFeeds;
begin
  Expect<string>(EventLines('k: |' + #13#10 + '  one' + #13#10 + '  two' + #13#10)).ToBe(
    '+STR' + #10 + '+DOC' + #10 + '+MAP' + #10 + '=VAL :k' + #10 +
    '=VAL |one\ntwo\n' + #10 + '-MAP' + #10 + '-DOC' + #10 + '-STR' + #10);
end;

procedure TYAMLEventParserTests.TestSurrogatePairEscapesSpellOneCharacter;
begin
  Expect<string>(EventLines('"😀 \U0001F600"')).ToBe(
    '+STR' + #10 + '+DOC' + #10 + '=VAL "' + #$D83D#$DE00 + ' ' +
    #$D83D#$DE00 + #10 + '-DOC' + #10 + '-STR' + #10);
end;

procedure TYAMLEventParserTests.TestDirectivesNeedADocumentStart;
begin
  Expect<Boolean>(ErrorMessage('%YAML 1.2' + #10 + 'a: 1' + #10) <> '').ToBe(True);
  Expect<string>(ErrorMessage('%YAML 1.2' + #10 + '---' + #10 + 'a: 1' + #10)).ToBe('');
  Expect<Boolean>(ErrorMessage('%YAML 2.0' + #10 + '---' + #10 + 'a' + #10) <> '').ToBe(True);
end;

procedure TYAMLEventParserTests.TestErrorsNameTheirLine;
begin
  // a plain scalar cannot hold ": ", and an implicit key ends on its line
  Expect<Boolean>(Pos('Line 2, column', ErrorMessage('a: 1' + #10 + ' b: 2' + #10)) = 1).ToBe(True);
end;

procedure TYAMLEventParserTests.TestUnknownAliasIsAnError;
begin
  Expect<Boolean>(ErrorMessage('a: *missing' + #10) <> '').ToBe(True);
  // anchors belong to their own document
  Expect<Boolean>(ErrorMessage('--- &a x' + #10 + '--- *a' + #10) <> '').ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TYAMLEventParserTests.Create('YAML event parser'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
