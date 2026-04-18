program Goccia.JSON.Utils.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.JSON.Utils;

type
  TJSONUtilsTests = class(TTestSuite)
  private
    procedure TestEscapesDoubleQuote;
    procedure TestEscapesBackslash;
    procedure TestEscapesSolidus;
    procedure TestEscapesNul;
    procedure TestEscapesUnitSeparator;
    procedure TestShortEscapes;
    procedure TestCombinedSpecialChars;
    procedure TestPreservesMultibyteUTF8;
    procedure TestQuoteJSONStringWraps;
    procedure TestPlainASCIIPassthrough;
  public
    procedure SetupTests; override;
  end;

procedure TJSONUtilsTests.SetupTests;
begin
  Test('Escapes double quote as \"', TestEscapesDoubleQuote);
  Test('Escapes backslash as \\', TestEscapesBackslash);
  Test('Escapes solidus as \/', TestEscapesSolidus);
  Test('Escapes NUL (#0) as \u0000', TestEscapesNul);
  Test('Escapes unit separator (#31) as \u001F', TestEscapesUnitSeparator);
  Test('Uses short escapes for \b \t \n \f \r', TestShortEscapes);
  Test('Handles multiple special chars in one string', TestCombinedSpecialChars);
  Test('Preserves non-control multibyte UTF-8 characters', TestPreservesMultibyteUTF8);
  Test('QuoteJSONString wraps escaped result in double quotes', TestQuoteJSONStringWraps);
  Test('Plain ASCII passes through unchanged', TestPlainASCIIPassthrough);
end;

procedure TJSONUtilsTests.TestEscapesDoubleQuote;
begin
  Expect<string>(EscapeJSONString('"')).ToBe('\"');
  Expect<string>(EscapeJSONString('say "hello"')).ToBe('say \"hello\"');
end;

procedure TJSONUtilsTests.TestEscapesBackslash;
begin
  Expect<string>(EscapeJSONString('\')).ToBe('\\');
  Expect<string>(EscapeJSONString('a\b')).ToBe('a\\b');
end;

procedure TJSONUtilsTests.TestEscapesSolidus;
begin
  Expect<string>(EscapeJSONString('/')).ToBe('\/');
  Expect<string>(EscapeJSONString('</script>')).ToBe('<\/script>');
end;

// #0 (U+0000) is a C0 control char with no short escape
procedure TJSONUtilsTests.TestEscapesNul;
begin
  Expect<string>(EscapeJSONString(#0)).ToBe('\u0000');
end;

// #31 (U+001F) is the last C0 control char, no short escape
procedure TJSONUtilsTests.TestEscapesUnitSeparator;
begin
  Expect<string>(EscapeJSONString(#31)).ToBe('\u001F');
end;

// All five named short escapes: \b \t \n \f \r
procedure TJSONUtilsTests.TestShortEscapes;
begin
  Expect<string>(EscapeJSONString(#8)).ToBe('\b');
  Expect<string>(EscapeJSONString(#9)).ToBe('\t');
  Expect<string>(EscapeJSONString(#10)).ToBe('\n');
  Expect<string>(EscapeJSONString(#12)).ToBe('\f');
  Expect<string>(EscapeJSONString(#13)).ToBe('\r');
end;

// Mix of quote, backslash, control chars, and a hex-escaped control char
// to exercise buffering across multiple escape types
procedure TJSONUtilsTests.TestCombinedSpecialChars;
begin
  Expect<string>(EscapeJSONString('"' + '\' + #10 + #0 + 'ok')).ToBe('\"\\' + '\n' + '\u0000ok');
  Expect<string>(EscapeJSONString(#9 + '/' + #31)).ToBe('\t' + '\/' + '\u001F');
end;

// Non-ASCII characters above U+001F must pass through unescaped.
// U+00E9 (e-acute) is the two-byte UTF-8 sequence #$C3#$A9.
// U+1F600 (grinning face emoji) is a four-byte UTF-8 sequence.
procedure TJSONUtilsTests.TestPreservesMultibyteUTF8;
var
  EAcute: string;
  Grinning: string;
begin
  EAcute := #$C3#$A9; // UTF-8 for U+00E9 (e with acute accent)
  Expect<string>(EscapeJSONString(EAcute)).ToBe(EAcute);
  Expect<string>(EscapeJSONString('caf' + EAcute)).ToBe('caf' + EAcute);

  Grinning := #$F0#$9F#$98#$80; // UTF-8 for U+1F600 (grinning face emoji)
  Expect<string>(EscapeJSONString(Grinning)).ToBe(Grinning);
  Expect<string>(EscapeJSONString('face' + Grinning)).ToBe('face' + Grinning);
end;

// QuoteJSONString should wrap the escaped body in literal double-quote chars
procedure TJSONUtilsTests.TestQuoteJSONStringWraps;
begin
  Expect<string>(QuoteJSONString('hello')).ToBe('"hello"');
  Expect<string>(QuoteJSONString('say "hi"')).ToBe('"say \"hi\""');
  Expect<string>(QuoteJSONString(#10)).ToBe('"\n"');
  Expect<string>(QuoteJSONString(#0)).ToBe('"\u0000"');
end;

// Ordinary printable ASCII is never escaped
procedure TJSONUtilsTests.TestPlainASCIIPassthrough;
begin
  Expect<string>(EscapeJSONString('')).ToBe('');
  Expect<string>(EscapeJSONString('abc 123 !@#$%')).ToBe('abc 123 !@#$%');
end;

begin
  TestRunnerProgram.AddSuite(TJSONUtilsTests.Create('JSON Utils'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
