program TextSemantics.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,
  TextEncoding,
  TextSemantics;

type
  TTextSemanticsTests = class(TTestSuite)
  private
    procedure TestTrimECMAScriptWhitespace;
    procedure TestUnicodeCaseMapping;
    procedure TestUTF8WellFormedChecks;
    procedure TestUTF16WellFormedChecks;
    procedure TestUTF16CodePointIndexing;
    procedure TestUTF16CodeUnitIndexing;
    procedure TestUTF16StringIdentity;
    procedure TestReplacementPatternExpansion;
    procedure TestECMAScriptSourceLinesSplitUnicodeLineTerminators;
    procedure TestTextLineHelpersPreserveUnicode;
    procedure TestSourceTextFallsBackAfterMutation;
  public
    procedure SetupTests; override;
  end;

procedure TTextSemanticsTests.SetupTests;
begin
  Test('TrimECMAScriptWhitespace removes Unicode whitespace',
    TestTrimECMAScriptWhitespace);
  Test('Unicode case mapping handles non-ASCII text',
    TestUnicodeCaseMapping);
  Test('UTF-8 decoder rejects invalid byte sequences',
    TestUTF8WellFormedChecks);
  Test('UTF-16 well-formed helpers follow surrogate pairing',
    TestUTF16WellFormedChecks);
  Test('Code point helpers index UTF-16 text',
    TestUTF16CodePointIndexing);
  Test('UTF-16 code unit helpers count astral characters as surrogate pairs',
    TestUTF16CodeUnitIndexing);
  Test('UTF-16 string identity compares canonical code units',
    TestUTF16StringIdentity);
  Test('Replacement pattern expansion follows GetSubstitution tokens',
    TestReplacementPatternExpansion);
  Test('ECMAScript source lines split Unicode line terminators',
    TestECMAScriptSourceLinesSplitUnicodeLineTerminators);
  Test('Text line helpers preserve Unicode text',
    TestTextLineHelpersPreserveUnicode);
  Test('Preserved source text falls back after line-list mutation',
    TestSourceTextFallsBackAfterMutation);
end;

procedure TTextSemanticsTests.TestTrimECMAScriptWhitespace;
const
  NO_BREAK_SPACE = #$00A0;
  EN_QUAD = #$2000;
  OGHAM_SPACE_MARK = #$1680;
  BYTE_ORDER_MARK = #$FEFF;
var
  Text: string;
begin
  Text := NO_BREAK_SPACE + EN_QUAD + 'hello' + OGHAM_SPACE_MARK +
    BYTE_ORDER_MARK;
  Expect<string>(TrimECMAScriptWhitespace(Text)).ToBe('hello');
  Expect<string>(TrimECMAScriptWhitespaceStart(Text)).ToBe(
    'hello' + OGHAM_SPACE_MARK + BYTE_ORDER_MARK);
  Expect<string>(TrimECMAScriptWhitespaceEnd(Text)).ToBe(
    NO_BREAK_SPACE + EN_QUAD + 'hello');
end;

procedure TTextSemanticsTests.TestUnicodeCaseMapping;
const
  UPPER_TEXT = #$00C9 + #$00D6 + #$03A3;
  LOWER_TEXT = #$00E9 + #$00F6 + #$03C2;
  NONCHARACTER = #$FDD0;
  HIGH_SURROGATE = #$D800;
begin
  Expect<string>(ECMAScriptDefaultLowerCase(UPPER_TEXT)).ToBe(LOWER_TEXT);
  Expect<string>(ECMAScriptDefaultUpperCase(LOWER_TEXT)).ToBe(UPPER_TEXT);
  Expect<string>(ECMAScriptDefaultLowerCase(#$0130)).ToBe('i' + #$0307);
  Expect<string>(ECMAScriptDefaultLowerCase('A' + #$03A3)).ToBe(
    'a' + #$03C2);
  Expect<string>(ECMAScriptDefaultUpperCase(
    'a' + NONCHARACTER + #$00DF)).ToBe(
    'A' + NONCHARACTER + 'SS');
  Expect<string>(ECMAScriptDefaultLowerCase(
    'A' + NONCHARACTER + #$0130)).ToBe(
    'a' + NONCHARACTER + 'i' + #$0307);
  Expect<string>(ECMAScriptDefaultLowerCase(
    'A' + HIGH_SURROGATE)).ToBe(
    'a' + HIGH_SURROGATE);
  Expect<string>(ECMAScriptDefaultUpperCase(
    'a' + HIGH_SURROGATE)).ToBe(
    'A' + HIGH_SURROGATE);
  Expect<string>(ECMAScriptDefaultUpperCase(#$D801#$DC28)).ToBe(
    #$D801#$DC00);
  Expect<string>(ECMAScriptDefaultLowerCase(#$D801#$DC00)).ToBe(
    #$D801#$DC28);
  Expect<string>(ECMAScriptDefaultUpperCase(#$FB03)).ToBe('FFI');
  Expect<string>(ECMAScriptDefaultLowerCase('A' + #$03A3 + #$0301)).ToBe(
    'a' + #$03C2 + #$0301);
  Expect<string>(ECMAScriptDefaultLowerCase(
    'A' + #$03A3 + #$0301 + 'B')).ToBe(
    'a' + #$03C3 + #$0301 + 'b');
end;

procedure TTextSemanticsTests.TestUTF8WellFormedChecks;
var
  Bytes: TBytes;
  ErrorOffset: Integer;
  Text: string;
begin
  Expect<Boolean>(TryDecodeUTF8(TBytes.Create($F0, $9F, $98, $80), Text,
    ErrorOffset))
    .ToBe(True);
  Expect<Integer>(ErrorOffset).ToBe(-1);
  Expect<string>(Text).ToBe(#$D83D#$DE00);
  Expect<Boolean>(TryDecodeUTF8(TBytes.Create(Ord('a'), $C0, $80), Text,
    ErrorOffset)).ToBe(False);
  Expect<Integer>(ErrorOffset).ToBe(1);
  Expect<Boolean>(TryDecodeUTF8(TBytes.Create($ED, $A0, $80), Text,
    ErrorOffset))
    .ToBe(False);
  Expect<Integer>(ErrorOffset).ToBe(0);
  Expect<Boolean>(TryEncodeUTF8('a' + #$D800, Bytes, ErrorOffset))
    .ToBe(False);
  Expect<Integer>(ErrorOffset).ToBe(1);
  Expect<string>(DecodeUTF8WithReplacement(
    TBytes.Create(Ord('a'), $C0, $80, Ord('b')))).ToBe(
    'a' + #$FFFD + #$FFFD + 'b');
  Expect<string>(DecodeUTF8WithReplacement(
    TBytes.Create($E2, Ord('A'), Ord('B')))).ToBe(#$FFFD + 'AB');
  Expect<string>(DecodeUTF8WithReplacement(
    TBytes.Create($E2, $80, Ord('A')))).ToBe(#$FFFD + 'A');
end;

procedure TTextSemanticsTests.TestUTF16WellFormedChecks;
const
  HIGH_SURROGATE = #$D83D;
  LOW_SURROGATE = #$DCA9;
  PILE_OF_POO = #$D83D#$DCA9;
begin
  Expect<Boolean>(IsWellFormedUTF16('a' + HIGH_SURROGATE +
    LOW_SURROGATE + 'd')).ToBe(True);
  Expect<Boolean>(IsWellFormedUTF16(PILE_OF_POO)).ToBe(True);
  Expect<Boolean>(IsWellFormedUTF16(HIGH_SURROGATE)).ToBe(False);
  Expect<Boolean>(IsWellFormedUTF16(LOW_SURROGATE)).ToBe(False);
  Expect<Boolean>(IsWellFormedUTF16(HIGH_SURROGATE +
    HIGH_SURROGATE)).ToBe(False);
  Expect<string>(ToWellFormedUTF16('a' + HIGH_SURROGATE +
    LOW_SURROGATE + 'd')).ToBe('a' + PILE_OF_POO + 'd');
  Expect<string>(ToWellFormedUTF16('a' + HIGH_SURROGATE + 'd')).ToBe(
    'a' + UNICODE_REPLACEMENT_CHARACTER + 'd');
  Expect<string>(ToWellFormedUTF16(LOW_SURROGATE +
    HIGH_SURROGATE)).ToBe(
    UNICODE_REPLACEMENT_CHARACTER + UNICODE_REPLACEMENT_CHARACTER);
end;

procedure TTextSemanticsTests.TestUTF16CodePointIndexing;
const
  LINE_SEPARATOR = #$2028;
  PARAGRAPH_SEPARATOR = #$2029;
  NONCHARACTER = #$FFFF;
  HIGH_SURROGATE = #$D800;
var
  Text: string;
begin
  Text := 'a' + LINE_SEPARATOR + PARAGRAPH_SEPARATOR + NONCHARACTER + 'b';
  Expect<Integer>(UnicodeCodePointLength(Text)).ToBe(5);
  Expect<string>(UnicodeCodePointAt(Text, 0)).ToBe('a');
  Expect<string>(UnicodeCodePointAt(Text, 1)).ToBe(LINE_SEPARATOR);
  Expect<string>(UnicodeCodePointAt(Text, 2)).ToBe(PARAGRAPH_SEPARATOR);
  Expect<string>(UnicodeCodePointAt(Text, 3)).ToBe(NONCHARACTER);
  Expect<string>(UnicodeCodePointAt(Text, 4)).ToBe('b');
  Expect<string>(UnicodeCodePointAt(Text, 5)).ToBe('');
  Expect<Integer>(UnicodeCodePointLength(HIGH_SURROGATE)).ToBe(1);
  Text := 'a' + HIGH_SURROGATE + 'b';
  Expect<Integer>(UnicodeCodePointLength(Text)).ToBe(3);
  Expect<string>(UnicodeCodePointAt(Text, 0)).ToBe('a');
  Expect<string>(UnicodeCodePointAt(Text, 2)).ToBe('b');
end;

procedure TTextSemanticsTests.TestUTF16CodeUnitIndexing;
const
  LINE_SEPARATOR = #$2028;
  GRINNING_FACE = #$D83D#$DE00;
  HIGH_SURROGATE = #$D800;
var
  CodePoint: Cardinal;
  Text1, Text2: string;
begin
  Expect<Integer>(UTF16CodeUnitLength('a' + LINE_SEPARATOR + 'b')).ToBe(3);
  Expect<string>(UTF16CodeUnitAt('a' + LINE_SEPARATOR + 'b', 1)).ToBe(
    LINE_SEPARATOR);
  Expect<Integer>(UTF16CodeUnitLength('a' + GRINNING_FACE + 'b')).ToBe(4);
  Expect<string>(UTF16CodeUnitAt('a' + GRINNING_FACE + 'b', 4)).ToBe('');
  Expect<Boolean>(TryUTF16CodePointValueAt(GRINNING_FACE, 0, CodePoint))
    .ToBe(True);
  Expect<Integer>(CodePoint).ToBe($1F600);
  Expect<Boolean>(TryUTF16CodePointValueAt(GRINNING_FACE, 1, CodePoint))
    .ToBe(True);
  Expect<Integer>(CodePoint).ToBe($DE00);
  Expect<Integer>(UTF16CodeUnitLength(HIGH_SURROGATE)).ToBe(1);
  Expect<string>(UTF16CodeUnitAt(HIGH_SURROGATE, 0)).ToBe(HIGH_SURROGATE);
  Expect<Boolean>(TryUTF16CodePointValueAt(HIGH_SURROGATE, 0, CodePoint))
    .ToBe(True);
  Expect<Integer>(CodePoint).ToBe($D800);
  Expect<string>(UTF16Substring('a' + GRINNING_FACE + 'b', 1, 2)).ToBe(
    GRINNING_FACE);
  Expect<string>(UTF16Substring('a' + GRINNING_FACE + 'b', 1, 1)).ToBe(
    #$D83D);
  Expect<Integer>(UTF16IndexOf('a' + GRINNING_FACE + 'b',
    GRINNING_FACE)).ToBe(1);
  Expect<Integer>(UTF16LastIndexOf(GRINNING_FACE + 'a' +
    GRINNING_FACE, GRINNING_FACE, 4)).ToBe(3);
  Text1 := 'a' + GRINNING_FACE + 'b';
  Text2 := LINE_SEPARATOR + GRINNING_FACE;
  Expect<Integer>(UTF16CodeUnitLength(Text1)).ToBe(4);
  Expect<Integer>(UTF16CodeUnitLength(Text2)).ToBe(3);
  Expect<Integer>(UTF16CodeUnitLength(Text1)).ToBe(4);
end;

procedure TTextSemanticsTests.TestUTF16StringIdentity;
const
  GRINNING_FACE = #$D83D#$DE00;
  HIGH_SURROGATE = #$D83D;
  LOW_SURROGATE = #$DE00;
var
  SurrogatePair: string;
begin
  SurrogatePair := HIGH_SURROGATE + LOW_SURROGATE;
  Expect<Boolean>(UTF16StringsEqual(GRINNING_FACE, SurrogatePair))
    .ToBe(True);
  Expect<Integer>(UTF16StringHash(GRINNING_FACE)).ToBe(
    UTF16StringHash(SurrogatePair));
  Expect<Boolean>(UTF16StringsEqual('a', 'b')).ToBe(False);
  Expect<Boolean>(UTF16StringsEqual(#$00E9, #$00E8)).ToBe(False);
  Expect<Boolean>(UTF16StringsEqual(GRINNING_FACE,
    HIGH_SURROGATE)).ToBe(False);
end;

procedure TTextSemanticsTests.TestReplacementPatternExpansion;
var
  Captures: array[0..1] of TReplacementCapture;
  NamedCaptures: array[0..0] of TReplacementNamedCapture;
begin
  Captures[0].Text := 'b';
  Captures[0].Matched := True;
  Captures[1].Text := '';
  Captures[1].Matched := False;
  NamedCaptures[0].Name := 'letter';
  NamedCaptures[0].Text := 'b';
  NamedCaptures[0].Matched := True;

  Expect<string>(ExpandReplacementPattern('$$ $& $` $'' $1 $2 $3 $12',
    'bc', 'abcde', 1, Captures, NamedCaptures)).ToBe(
    '$ bc a de b  $3 b2');
  Expect<string>(ExpandReplacementPattern('$<letter>:$<missing>:$<',
    'bc', 'abcde', 1, Captures, NamedCaptures)).ToBe('b::$<');
  Expect<string>(ExpandReplacementPattern('$<letter>', 'a', 'a', 0, [],
    [])).ToBe('$<letter>');
end;

procedure TTextSemanticsTests.TestECMAScriptSourceLinesSplitUnicodeLineTerminators;
const
  LINE_SEPARATOR = #$2028;
  PARAGRAPH_SEPARATOR = #$2029;
var
  Lines: TStringList;
begin
  Lines := CreateECMAScriptSourceLines(
    'var' + LINE_SEPARATOR + 'x' + PARAGRAPH_SEPARATOR + '= 1;');
  try
    Expect<Integer>(Lines.Count).ToBe(3);
    Expect<string>(Lines[0]).ToBe('var');
    Expect<string>(Lines[1]).ToBe('x');
    Expect<string>(Lines[2]).ToBe('= 1;');
  finally
    Lines.Free;
  end;
end;

procedure TTextSemanticsTests.TestTextLineHelpersPreserveUnicode;
const
  UNICODE_WORD = 'na' + #$00EF + 've';
var
  I: Integer;
  Lines: TStringList;
  Text: string;
begin
  Lines := CreateTextLines('first' + #13#10 + UNICODE_WORD + #13);
  try
    Text := StringListToLFText(Lines);
    Expect<Integer>(Length(Text)).ToBe(
      Length('first' + #10 + UNICODE_WORD + #10));
    for I := 1 to Length(Text) do
      Expect<Integer>(Ord(Text[I])).ToBe(
        Ord(('first' + #10 + UNICODE_WORD + #10)[I]));
  finally
    Lines.Free;
  end;
end;

procedure TTextSemanticsTests.TestSourceTextFallsBackAfterMutation;
var
  Lines: TStringList;
begin
  Lines := CreateECMAScriptSourceLines('test("one", () => {});' + #13#10 +
    'test("two", () => {});');
  try
    Expect<string>(StringListToSourceText(Lines)).ToBe(
      'test("one", () => {});' + #13#10 + 'test("two", () => {});');
    Lines.Add('runTests();');
    Expect<string>(StringListToSourceText(Lines)).ToBe(
      'test("one", () => {});' + #10 + 'test("two", () => {});' + #10 +
      'runTests();');
  finally
    Lines.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTextSemanticsTests.Create('TextSemantics'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
