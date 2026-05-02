program TextSemantics.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,
  TextSemantics;

type
  TTextSemanticsTests = class(TTestSuite)
  private
    procedure TestTrimECMAScriptWhitespace;
    procedure TestUnicodeCaseMapping;
    procedure TestUTF8WellFormedChecks;
    procedure TestUTF8CodePointIndexing;
    procedure TestUTF16CodeUnitIndexing;
    procedure TestReplacementPatternExpansion;
    procedure TestECMAScriptSourceLinesSplitUnicodeLineTerminators;
    procedure TestTextLineHelpersPreserveUTF8;
  public
    procedure SetupTests; override;
  end;

procedure TTextSemanticsTests.SetupTests;
begin
  Test('TrimECMAScriptWhitespace removes Unicode whitespace',
    TestTrimECMAScriptWhitespace);
  Test('Unicode case mapping handles non-ASCII UTF-8',
    TestUnicodeCaseMapping);
  Test('UTF-8 well-formed helpers reject invalid sequences',
    TestUTF8WellFormedChecks);
  Test('UTF-8 code point helpers index multibyte characters',
    TestUTF8CodePointIndexing);
  Test('UTF-16 code unit helpers count astral characters as surrogate pairs',
    TestUTF16CodeUnitIndexing);
  Test('Replacement pattern expansion follows GetSubstitution tokens',
    TestReplacementPatternExpansion);
  Test('ECMAScript source lines split Unicode line terminators',
    TestECMAScriptSourceLinesSplitUnicodeLineTerminators);
  Test('Text line helpers preserve UTF-8 bytes',
    TestTextLineHelpersPreserveUTF8);
end;

procedure TTextSemanticsTests.TestTrimECMAScriptWhitespace;
const
  NO_BREAK_SPACE = #$C2#$A0;
  EN_QUAD = #$E2#$80#$80;
  OGHAM_SPACE_MARK = #$E1#$9A#$80;
  BYTE_ORDER_MARK = #$EF#$BB#$BF;
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
  UPPER_TEXT = #$C3#$89 + #$C3#$96 + #$CE#$A3;
  LOWER_TEXT = #$C3#$A9 + #$C3#$B6 + #$CF#$83;
begin
  Expect<string>(UnicodeLowerCaseUTF8(UPPER_TEXT)).ToBe(LOWER_TEXT);
  Expect<string>(UnicodeUpperCaseUTF8(LOWER_TEXT)).ToBe(UPPER_TEXT);
end;

procedure TTextSemanticsTests.TestUTF8WellFormedChecks;
const
  GRINNING_FACE = #$F0#$9F#$98#$80;
  OVERLONG_NULL = #$C0#$80;
  SURROGATE_CODE_POINT = #$ED#$A0#$80;
begin
  Expect<Boolean>(IsWellFormedUTF8('ok' + GRINNING_FACE)).ToBe(True);
  Expect<Boolean>(IsWellFormedUTF8(OVERLONG_NULL)).ToBe(False);
  Expect<Boolean>(IsWellFormedUTF8(SURROGATE_CODE_POINT)).ToBe(False);
  Expect<string>(ToWellFormedUTF8('a' + OVERLONG_NULL + 'b')).ToBe(
    'a' + UTF8_REPLACEMENT_CHARACTER + UTF8_REPLACEMENT_CHARACTER + 'b');
  Expect<string>(ToWellFormedUTF8(#$E2 + 'AB')).ToBe(
    UTF8_REPLACEMENT_CHARACTER + 'AB');
  Expect<string>(ToWellFormedUTF8(#$E2#$80 + 'A')).ToBe(
    UTF8_REPLACEMENT_CHARACTER + 'A');
end;

procedure TTextSemanticsTests.TestUTF8CodePointIndexing;
const
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
  UTF8_NONCHARACTER = #$EF#$BF#$BF;
  UTF8_HIGH_SURROGATE = #$ED#$A0#$80;
var
  Text: string;
begin
  Text := 'a' + UTF8_LINE_SEPARATOR + UTF8_PARAGRAPH_SEPARATOR +
    UTF8_NONCHARACTER + 'b';
  Expect<Integer>(UTF8CodePointLength(Text)).ToBe(5);
  Expect<string>(UTF8CodePointAt(Text, 0)).ToBe('a');
  Expect<string>(UTF8CodePointAt(Text, 1)).ToBe(UTF8_LINE_SEPARATOR);
  Expect<string>(UTF8CodePointAt(Text, 2)).ToBe(UTF8_PARAGRAPH_SEPARATOR);
  Expect<string>(UTF8CodePointAt(Text, 3)).ToBe(UTF8_NONCHARACTER);
  Expect<string>(UTF8CodePointAt(Text, 4)).ToBe('b');
  Expect<string>(UTF8CodePointAt(Text, 5)).ToBe('');
  Expect<Integer>(UTF8CodePointLength(UTF8_HIGH_SURROGATE)).ToBe(1);
  Text := 'a' + UTF8_HIGH_SURROGATE + 'b';
  Expect<Integer>(UTF8CodePointLength(Text)).ToBe(3);
  Expect<string>(UTF8CodePointAt(Text, 0)).ToBe('a');
  Expect<string>(UTF8CodePointAt(Text, 2)).ToBe('b');
end;

procedure TTextSemanticsTests.TestUTF16CodeUnitIndexing;
const
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_GRINNING_FACE = #$F0#$9F#$98#$80;
  UTF8_HIGH_SURROGATE = #$ED#$A0#$80;
var
  CodePoint: Cardinal;
begin
  Expect<Integer>(UTF16CodeUnitLength('a' + UTF8_LINE_SEPARATOR + 'b')).ToBe(3);
  Expect<string>(UTF16CodeUnitAt('a' + UTF8_LINE_SEPARATOR + 'b', 1)).ToBe(
    UTF8_LINE_SEPARATOR);
  Expect<Integer>(UTF16CodeUnitLength('a' + UTF8_GRINNING_FACE + 'b')).ToBe(4);
  Expect<string>(UTF16CodeUnitAt('a' + UTF8_GRINNING_FACE + 'b', 4)).ToBe('');
  Expect<Boolean>(TryUTF16CodePointValueAt(UTF8_GRINNING_FACE, 0, CodePoint))
    .ToBe(True);
  Expect<Integer>(CodePoint).ToBe($1F600);
  Expect<Boolean>(TryUTF16CodePointValueAt(UTF8_GRINNING_FACE, 1, CodePoint))
    .ToBe(True);
  Expect<Integer>(CodePoint).ToBe($DE00);
  Expect<Integer>(UTF16CodeUnitLength(UTF8_HIGH_SURROGATE)).ToBe(1);
  Expect<string>(UTF16CodeUnitAt(UTF8_HIGH_SURROGATE, 0)).ToBe(
    UTF8_HIGH_SURROGATE);
  Expect<Boolean>(TryUTF16CodePointValueAt(UTF8_HIGH_SURROGATE, 0, CodePoint))
    .ToBe(True);
  Expect<Integer>(CodePoint).ToBe($D800);
  Expect<string>(UTF16Substring('a' + UTF8_GRINNING_FACE + 'b', 1, 2)).ToBe(
    UTF8_GRINNING_FACE);
  Expect<string>(UTF16Substring('a' + UTF8_GRINNING_FACE + 'b', 1, 1)).ToBe(
    #$ED#$A0#$BD);
  Expect<Integer>(UTF16IndexOf('a' + UTF8_GRINNING_FACE + 'b',
    UTF8_GRINNING_FACE)).ToBe(1);
  Expect<Integer>(UTF16LastIndexOf(UTF8_GRINNING_FACE + 'a' +
    UTF8_GRINNING_FACE, UTF8_GRINNING_FACE, 4)).ToBe(3);
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
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
var
  Lines: TStringList;
begin
  Lines := CreateECMAScriptSourceLines(
    'var' + UTF8_LINE_SEPARATOR + 'x' + UTF8_PARAGRAPH_SEPARATOR + '= 1;');
  try
    Expect<Integer>(Lines.Count).ToBe(3);
    Expect<string>(Lines[0]).ToBe('var');
    Expect<string>(Lines[1]).ToBe('x');
    Expect<string>(Lines[2]).ToBe('= 1;');
  finally
    Lines.Free;
  end;
end;

procedure TTextSemanticsTests.TestTextLineHelpersPreserveUTF8;
const
  UTF8_WORD_BYTES = 'na' + #$C3#$AF + 've';
var
  I: Integer;
  Lines: TStringList;
  Text: string;
begin
  Lines := CreateUTF8StringList('first' + #13#10 + UTF8_WORD_BYTES + #13);
  try
    Text := StringListToLFText(Lines);
    Expect<Integer>(Length(Text)).ToBe(
      Length('first' + #10 + UTF8_WORD_BYTES + #10));
    for I := 1 to Length(Text) do
      Expect<Integer>(Ord(Text[I])).ToBe(
        Ord(('first' + #10 + UTF8_WORD_BYTES + #10)[I]));
  finally
    Lines.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTextSemanticsTests.Create('TextSemantics'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
