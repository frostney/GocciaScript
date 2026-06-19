program Goccia.Lexer.Test;

{$I Goccia.inc}

uses
  Generics.Collections,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.TestSetup,
  Goccia.Token;

type
  TExpectedKeywordToken = record
    Keyword: string;
    TokenType: TGocciaTokenType;
  end;

  TLexerTests = class(TTestSuite)
  private
    procedure ScanAllTokens(const ALexer: TGocciaLexer;
      const ALexicalGoal: TGocciaLexicalGoal);
    procedure ScanGoalSequence(const ALexer: TGocciaLexer;
      const AGoals: array of TGocciaLexicalGoal);
    procedure AssertIgnoresLeadingHashbang(const ALineBreak: string);
    procedure AssertPreservesLineNumbersAfterHashbang(const ALineBreak: string);
    procedure AssertCommentTerminatedBy(const ALineBreak: string);
    procedure AssertTokenizesKeywordToken(const AKeyword: string; const ATokenType: TGocciaTokenType);
    procedure AssertContextualKeywordUsesDivGoal(const AKeyword: string;
      const ATokenType: TGocciaTokenType);
    procedure AssertTokenizesIdentifier(const ASource: string;
      const AExpectedLexeme: string);
    procedure AssertIdentifierRaisesLexerError(const ASource: string;
      const AExpectedMessage: string);
    procedure TestIgnoresLeadingHashbang;
    procedure TestPreservesLineNumbersAfterHashbang;
    procedure TestHashbangRequiresHashbangLexicalGoal;
    procedure TestCommentTerminatedByUnicodeLineTerminators;
    procedure TestTokenizesKeywordTokens;
    procedure TestContextualKeywordTokensUseDivGoal;
    procedure TestUnicodeIdentifierStartAndPartCodePoints;
    procedure TestEscapedIdentifiersUseECMAScriptIdentifierRules;
    procedure TestCRIncrementsLineInWhitespace;
    procedure TestECMAScriptUnicodeWhitespaceCodePoints;
    procedure TestCRIncrementsLineInBlockComment;
    procedure TestUnicodeLineTerminatorsInBlockComment;
    procedure TestUnicodeLineTerminatorsBetweenTokens;
    procedure TestRegexLiteralPreservesRawPattern;
    procedure TestLexicalGoalSelectsSlashTokenization;
    procedure TestRegexUnterminatedAtEOF;
    procedure TestAwaitSlashLookaheadStopsAtUnicodeLineTerminators;
    procedure TestNumericSeparatorsNormalize;
    procedure TestTemplateInterpolationTracksLineTerminators;
    procedure TestUnicodeEscapeOverflowRaisesLexerError;
  public
    procedure SetupTests; override;
  end;

procedure TLexerTests.SetupTests;
begin
  Test('Ignores leading hashbang', TestIgnoresLeadingHashbang);
  Test('Preserves line numbers after hashbang', TestPreservesLineNumbersAfterHashbang);
  Test('Hashbang requires hashbang lexical goal',
    TestHashbangRequiresHashbangLexicalGoal);
  Test('Terminates comment on Unicode line terminators', TestCommentTerminatedByUnicodeLineTerminators);
  Test('Tokenizes keyword tokens', TestTokenizesKeywordTokens);
  Test('Contextual keyword tokens use explicit division lexical goal',
    TestContextualKeywordTokensUseDivGoal);
  Test('Unicode identifier start and part code points',
    TestUnicodeIdentifierStartAndPartCodePoints);
  Test('Escaped identifiers use ECMAScript identifier rules',
    TestEscapedIdentifiersUseECMAScriptIdentifierRules);
  Test('CR increments line counter in whitespace', TestCRIncrementsLineInWhitespace);
  Test('ECMAScript Unicode whitespace separates tokens',
    TestECMAScriptUnicodeWhitespaceCodePoints);
  Test('CR increments line counter in block comments', TestCRIncrementsLineInBlockComment);
  Test('Unicode line terminators increment line counter in block comments', TestUnicodeLineTerminatorsInBlockComment);
  Test('Unicode line terminators split tokens', TestUnicodeLineTerminatorsBetweenTokens);
  Test('Regex literal preserves raw pattern', TestRegexLiteralPreservesRawPattern);
  Test('Lexical goal selects slash tokenization', TestLexicalGoalSelectsSlashTokenization);
  Test('Unterminated regex at EOF raises lexer error', TestRegexUnterminatedAtEOF);
  Test('Await slash lookahead stops at Unicode line terminators',
    TestAwaitSlashLookaheadStopsAtUnicodeLineTerminators);
  Test('Numeric separators normalize', TestNumericSeparatorsNormalize);
  Test('Template interpolation tracks line terminators', TestTemplateInterpolationTracksLineTerminators);
  Test('Unicode escape overflow raises lexer error', TestUnicodeEscapeOverflowRaisesLexerError);
end;

procedure TLexerTests.ScanAllTokens(const ALexer: TGocciaLexer;
  const ALexicalGoal: TGocciaLexicalGoal);
var
  Token: TGocciaToken;
begin
  repeat
    Token := ALexer.ScanNextToken(ALexicalGoal);
  until Token.TokenType = gttEOF;
end;

procedure TLexerTests.ScanGoalSequence(const ALexer: TGocciaLexer;
  const AGoals: array of TGocciaLexicalGoal);
var
  Goal: TGocciaLexicalGoal;
begin
  for Goal in AGoals do
    ALexer.ScanNextToken(Goal);
end;

procedure TLexerTests.AssertIgnoresLeadingHashbang(const ALineBreak: string);
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('#!/usr/bin/env goccia' + ALineBreak + 'const value = 1 / 2;', '<test>');
  try
    Lexer.ScanNextToken(glgInputElementHashbangOrRegExp);
    ScanAllTokens(Lexer, glgInputElementDiv);
    Tokens := Lexer.Tokens;
    Expect<Integer>(Tokens.Count).ToBe(8);
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<TGocciaTokenType>(Tokens[1].TokenType).ToBe(gttIdentifier);
    Expect<string>(Tokens[1].Lexeme).ToBe('value');
    Expect<TGocciaTokenType>(Tokens[4].TokenType).ToBe(gttSlash);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.AssertPreservesLineNumbersAfterHashbang(const ALineBreak: string);
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('#!/usr/bin/env goccia' + ALineBreak + 'const value = 1;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementHashbangOrRegExp);
    Tokens := Lexer.Tokens;
    Expect<Integer>(Tokens[0].Line).ToBe(2);
    Expect<Integer>(Tokens[0].Column).ToBe(1);
    Expect<Integer>(Tokens[1].Line).ToBe(2);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestHashbangRequiresHashbangLexicalGoal;
var
  Lexer: TGocciaLexer;
  Token: TGocciaToken;
begin
  Lexer := TGocciaLexer.Create('#!/usr/bin/env goccia' + LineEnding +
    'const value = 1;', '<test>');
  try
    Token := Lexer.ScanNextToken(glgInputElementDiv);
    Expect<TGocciaTokenType>(Token.TokenType).ToBe(gttHash);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.AssertCommentTerminatedBy(const ALineBreak: string);
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('// comment' + ALineBreak + 'const x = 1;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<Integer>(Tokens.Count).ToBe(6);
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.AssertTokenizesKeywordToken(const AKeyword: string; const ATokenType: TGocciaTokenType);
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(AKeyword, '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<Integer>(Tokens.Count).ToBe(2);
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(ATokenType);
    Expect<string>(Tokens[0].Lexeme).ToBe(AKeyword);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.AssertContextualKeywordUsesDivGoal(const AKeyword: string;
  const ATokenType: TGocciaTokenType);
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(AKeyword + ' /', '<test>');
  try
    ScanGoalSequence(Lexer, [glgInputElementRegExp, glgInputElementDiv]);
    Tokens := Lexer.Tokens;
    Expect<Integer>(Tokens.Count).ToBe(2);
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(ATokenType);
    Expect<string>(Tokens[0].Lexeme).ToBe(AKeyword);
    Expect<TGocciaTokenType>(Tokens[1].TokenType).ToBe(gttSlash);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.AssertTokenizesIdentifier(const ASource: string;
  const AExpectedLexeme: string);
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(ASource, '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<Integer>(Tokens.Count).ToBe(2);
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttIdentifier);
    Expect<string>(Tokens[0].Lexeme).ToBe(AExpectedLexeme);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.AssertIdentifierRaisesLexerError(const ASource: string;
  const AExpectedMessage: string);
var
  Lexer: TGocciaLexer;
  MessageMatches: Boolean;
  Raised: Boolean;
begin
  Lexer := TGocciaLexer.Create(ASource, '<test>');
  try
    MessageMatches := False;
    Raised := False;
    try
      ScanAllTokens(Lexer, glgInputElementRegExp);
    except
      on E: TGocciaLexerError do
      begin
        MessageMatches := Pos(AExpectedMessage, E.Message) > 0;
        Raised := True;
      end;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Boolean>(MessageMatches).ToBe(True);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestIgnoresLeadingHashbang;
const
  CRLF = #13#10;
  // ES2026 §12.3 Line Terminators — UTF-8 encoding of LS and PS
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
begin
  AssertIgnoresLeadingHashbang(sLineBreak);
  AssertIgnoresLeadingHashbang(CRLF);
  AssertIgnoresLeadingHashbang(#13);
  AssertIgnoresLeadingHashbang(UTF8_LINE_SEPARATOR);
  AssertIgnoresLeadingHashbang(UTF8_PARAGRAPH_SEPARATOR);
end;

procedure TLexerTests.TestPreservesLineNumbersAfterHashbang;
const
  CRLF = #13#10;
  // ES2026 §12.3 Line Terminators — UTF-8 encoding of LS and PS
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
begin
  AssertPreservesLineNumbersAfterHashbang(sLineBreak);
  AssertPreservesLineNumbersAfterHashbang(CRLF);
  AssertPreservesLineNumbersAfterHashbang(#13);
  AssertPreservesLineNumbersAfterHashbang(UTF8_LINE_SEPARATOR);
  AssertPreservesLineNumbersAfterHashbang(UTF8_PARAGRAPH_SEPARATOR);
end;

procedure TLexerTests.TestCommentTerminatedByUnicodeLineTerminators;
const
  // ES2026 §12.3 Line Terminators — UTF-8 encoding of LS and PS
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
begin
  AssertCommentTerminatedBy(#13);
  AssertCommentTerminatedBy(UTF8_LINE_SEPARATOR);
  AssertCommentTerminatedBy(UTF8_PARAGRAPH_SEPARATOR);
end;

procedure TLexerTests.TestTokenizesKeywordTokens;
const
  ExpectedKeywordTokens: array[0..38] of TExpectedKeywordToken = (
    (Keyword: KEYWORD_AS; TokenType: gttAs),
    (Keyword: KEYWORD_DO; TokenType: gttDo),
    (Keyword: KEYWORD_IF; TokenType: gttIf),
    (Keyword: KEYWORD_IN; TokenType: gttIn),
    (Keyword: KEYWORD_FOR; TokenType: gttFor),
    (Keyword: KEYWORD_LET; TokenType: gttLet),
    (Keyword: KEYWORD_NEW; TokenType: gttNew),
    (Keyword: KEYWORD_TRY; TokenType: gttTry),
    (Keyword: KEYWORD_VAR; TokenType: gttVar),
    (Keyword: KEYWORD_CASE; TokenType: gttCase),
    (Keyword: KEYWORD_ELSE; TokenType: gttElse),
    (Keyword: KEYWORD_ENUM; TokenType: gttEnum),
    (Keyword: KEYWORD_FROM; TokenType: gttFrom),
    (Keyword: KEYWORD_NULL; TokenType: gttNull),
    (Keyword: KEYWORD_THIS; TokenType: gttThis),
    (Keyword: KEYWORD_TRUE; TokenType: gttTrue),
    (Keyword: KEYWORD_VOID; TokenType: gttVoid),
    (Keyword: KEYWORD_WITH; TokenType: gttWith),
    (Keyword: KEYWORD_BREAK; TokenType: gttBreak),
    (Keyword: KEYWORD_CATCH; TokenType: gttCatch),
    (Keyword: KEYWORD_CLASS; TokenType: gttClass),
    (Keyword: KEYWORD_CONST; TokenType: gttConst),
    (Keyword: KEYWORD_FALSE; TokenType: gttFalse),
    (Keyword: KEYWORD_SUPER; TokenType: gttSuper),
    (Keyword: KEYWORD_THROW; TokenType: gttThrow),
    (Keyword: KEYWORD_WHILE; TokenType: gttWhile),
    (Keyword: KEYWORD_DELETE; TokenType: gttDelete),
    (Keyword: KEYWORD_EXPORT; TokenType: gttExport),
    (Keyword: KEYWORD_IMPORT; TokenType: gttImport),
    (Keyword: KEYWORD_RETURN; TokenType: gttReturn),
    (Keyword: KEYWORD_STATIC; TokenType: gttStatic),
    (Keyword: KEYWORD_SWITCH; TokenType: gttSwitch),
    (Keyword: KEYWORD_TYPEOF; TokenType: gttTypeof),
    (Keyword: KEYWORD_DEFAULT; TokenType: gttDefault),
    (Keyword: KEYWORD_EXTENDS; TokenType: gttExtends),
    (Keyword: KEYWORD_FINALLY; TokenType: gttFinally),
    (Keyword: KEYWORD_CONTINUE; TokenType: gttContinue),
    (Keyword: KEYWORD_FUNCTION; TokenType: gttFunction),
    (Keyword: KEYWORD_INSTANCEOF; TokenType: gttInstanceof)
  );
var
  I: Integer;
begin
  for I := Low(ExpectedKeywordTokens) to High(ExpectedKeywordTokens) do
    AssertTokenizesKeywordToken(ExpectedKeywordTokens[I].Keyword, ExpectedKeywordTokens[I].TokenType);
end;

procedure TLexerTests.TestContextualKeywordTokensUseDivGoal;
const
  ExpectedContextualKeywordTokens: array[0..2] of TExpectedKeywordToken = (
    (Keyword: KEYWORD_AS; TokenType: gttAs),
    (Keyword: KEYWORD_FROM; TokenType: gttFrom),
    (Keyword: KEYWORD_STATIC; TokenType: gttStatic)
  );
var
  I: Integer;
begin
  for I := Low(ExpectedContextualKeywordTokens) to High(ExpectedContextualKeywordTokens) do
    AssertContextualKeywordUsesDivGoal(ExpectedContextualKeywordTokens[I].Keyword,
      ExpectedContextualKeywordTokens[I].TokenType);
end;

procedure TLexerTests.TestUnicodeIdentifierStartAndPartCodePoints;
const
  UTF8_GREEK_SMALL_LETTER_PI = #$CF#$80;
  UTF8_ROCKET = #$F0#$9F#$9A#$80;
  UTF8_ZERO_WIDTH_NON_JOINER = #$E2#$80#$8C;
begin
  AssertTokenizesIdentifier(UTF8_GREEK_SMALL_LETTER_PI,
    UTF8_GREEK_SMALL_LETTER_PI);
  AssertTokenizesIdentifier('\u03C0', UTF8_GREEK_SMALL_LETTER_PI);
  AssertTokenizesIdentifier('a\u200C', 'a' + UTF8_ZERO_WIDTH_NON_JOINER);
  AssertTokenizesIdentifier(UTF8_ROCKET, UTF8_ROCKET);
end;

procedure TLexerTests.TestEscapedIdentifiersUseECMAScriptIdentifierRules;
begin
  AssertIdentifierRaisesLexerError('\u200C', 'Invalid identifier escape');
  AssertIdentifierRaisesLexerError('\u200D', 'Invalid identifier escape');
  AssertIdentifierRaisesLexerError('\u{1F680}', 'Invalid identifier escape');
end;

procedure TLexerTests.TestCRIncrementsLineInWhitespace;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  // Standalone CR between statements
  Lexer := TGocciaLexer.Create('const a = 1;' + #13 + 'const b = 2;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    // Second 'const' at index 5 should be on line 2, column 1
    Expect<TGocciaTokenType>(Tokens[5].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[5].Line).ToBe(2);
    Expect<Integer>(Tokens[5].Column).ToBe(1);
  finally
    Lexer.Free;
  end;

  // CRLF counts as a single line break
  Lexer := TGocciaLexer.Create('const a = 1;' + #13#10 + 'const b = 2;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[5].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[5].Line).ToBe(2);
    Expect<Integer>(Tokens[5].Column).ToBe(1);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestECMAScriptUnicodeWhitespaceCodePoints;
const
  UTF8_OGHAM_SPACE = #$E1#$9A#$80;
  UTF8_EM_SPACE = #$E2#$80#$83;
  UTF8_NARROW_NO_BREAK_SPACE = #$E2#$80#$AF;
  UTF8_MEDIUM_MATHEMATICAL_SPACE = #$E2#$81#$9F;
  UTF8_IDEOGRAPHIC_SPACE = #$E3#$80#$80;
  UTF8_ZERO_WIDTH_NO_BREAK_SPACE = #$EF#$BB#$BF;
  WhitespaceSamples: array[0..5] of string = (
    UTF8_OGHAM_SPACE,
    UTF8_EM_SPACE,
    UTF8_NARROW_NO_BREAK_SPACE,
    UTF8_MEDIUM_MATHEMATICAL_SPACE,
    UTF8_IDEOGRAPHIC_SPACE,
    UTF8_ZERO_WIDTH_NO_BREAK_SPACE
  );
var
  I: Integer;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  for I := Low(WhitespaceSamples) to High(WhitespaceSamples) do
  begin
    Lexer := TGocciaLexer.Create('/x/g' + WhitespaceSamples[I] +
      '; var' + WhitespaceSamples[I] + 'name;', '<test>');
    try
      ScanAllTokens(Lexer, glgInputElementRegExp);
      Tokens := Lexer.Tokens;
      Expect<Integer>(Tokens.Count).ToBe(6);
      Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttRegex);
      Expect<TGocciaTokenType>(Tokens[1].TokenType).ToBe(gttSemicolon);
      Expect<TGocciaTokenType>(Tokens[2].TokenType).ToBe(gttVar);
      Expect<TGocciaTokenType>(Tokens[3].TokenType).ToBe(gttIdentifier);
      Expect<string>(Tokens[3].Lexeme).ToBe('name');
      Expect<TGocciaTokenType>(Tokens[4].TokenType).ToBe(gttSemicolon);
    finally
      Lexer.Free;
    end;
  end;
end;

procedure TLexerTests.TestCRIncrementsLineInBlockComment;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  // Standalone CR inside block comment
  Lexer := TGocciaLexer.Create('/* a' + #13 + 'b */const x = 1;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
  finally
    Lexer.Free;
  end;

  // CRLF inside block comment counts as single line break
  Lexer := TGocciaLexer.Create('/* a' + #13#10 + 'b */const x = 1;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestUnicodeLineTerminatorsInBlockComment;
const
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('/* a' + UTF8_LINE_SEPARATOR + 'b */const x = 1;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
  finally
    Lexer.Free;
  end;

  Lexer := TGocciaLexer.Create('/* a' + UTF8_PARAGRAPH_SEPARATOR + 'b */const x = 1;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestUnicodeLineTerminatorsBetweenTokens;
const
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(
    'var' + UTF8_LINE_SEPARATOR + 'x' + UTF8_PARAGRAPH_SEPARATOR +
    '=' + UTF8_LINE_SEPARATOR + '1' + UTF8_PARAGRAPH_SEPARATOR + ';',
    '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttVar);
    Expect<TGocciaTokenType>(Tokens[1].TokenType).ToBe(gttIdentifier);
    Expect<string>(Tokens[1].Lexeme).ToBe('x');
    Expect<TGocciaTokenType>(Tokens[2].TokenType).ToBe(gttAssign);
    Expect<TGocciaTokenType>(Tokens[3].TokenType).ToBe(gttNumber);
    Expect<TGocciaTokenType>(Tokens[4].TokenType).ToBe(gttSemicolon);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestRegexLiteralPreservesRawPattern;
const
  REGEX_SEPARATOR = #0;
  PATTERN = 'a\/b[\/]';
  FLAGS = 'gi';
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('const value = /a\/b[\/]/gi;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[3].TokenType).ToBe(gttRegex);
    Expect<string>(Tokens[3].Lexeme).ToBe(
      IntToStr(Length(PATTERN)) + REGEX_SEPARATOR + PATTERN + FLAGS);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestLexicalGoalSelectsSlashTokenization;
const
  REGEX_SEPARATOR = #0;
var
  Lexer: TGocciaLexer;
  Token: TGocciaToken;
begin
  Lexer := TGocciaLexer.Create('/=', '<test>');
  try
    Token := Lexer.ScanNextToken(glgInputElementDiv);
    Expect<TGocciaTokenType>(Token.TokenType).ToBe(gttSlashAssign);
  finally
    Lexer.Free;
  end;

  Lexer := TGocciaLexer.Create('/=/', '<test>');
  try
    Token := Lexer.ScanNextToken(glgInputElementRegExp);
    Expect<TGocciaTokenType>(Token.TokenType).ToBe(gttRegex);
    Expect<string>(Token.Lexeme).ToBe('1' + REGEX_SEPARATOR + '=');
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestRegexUnterminatedAtEOF;
var
  Lexer: TGocciaLexer;
  Raised: Boolean;
  MessageMatches: Boolean;
begin
  Lexer := TGocciaLexer.Create('const value = /', '<test>');
  try
    Raised := False;
    MessageMatches := False;
    try
      ScanAllTokens(Lexer, glgInputElementRegExp);
    except
      on E: TGocciaLexerError do
      begin
        Raised := True;
        MessageMatches := Pos('Unterminated regular expression literal', E.Message) > 0;
      end;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Boolean>(MessageMatches).ToBe(True);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestAwaitSlashLookaheadStopsAtUnicodeLineTerminators;
const
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(
    'const half = (await) => await/' + UTF8_LINE_SEPARATOR + '/2/;',
    '<test>');
  try
    ScanGoalSequence(Lexer, [
      glgInputElementRegExp, glgInputElementRegExp, glgInputElementRegExp,
      glgInputElementRegExp, glgInputElementRegExp, glgInputElementRegExp,
      glgInputElementRegExp, glgInputElementRegExp, glgInputElementDiv,
      glgInputElementRegExp
    ]);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[8].TokenType).ToBe(gttSlash);
    Expect<TGocciaTokenType>(Tokens[9].TokenType).ToBe(gttRegex);
  finally
    Lexer.Free;
  end;

  Lexer := TGocciaLexer.Create(
    'const half = (await) => await/' + UTF8_PARAGRAPH_SEPARATOR + '/2/;',
    '<test>');
  try
    ScanGoalSequence(Lexer, [
      glgInputElementRegExp, glgInputElementRegExp, glgInputElementRegExp,
      glgInputElementRegExp, glgInputElementRegExp, glgInputElementRegExp,
      glgInputElementRegExp, glgInputElementRegExp, glgInputElementDiv,
      glgInputElementRegExp
    ]);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[8].TokenType).ToBe(gttSlash);
    Expect<TGocciaTokenType>(Tokens[9].TokenType).ToBe(gttRegex);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestNumericSeparatorsNormalize;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('const a = 1_234.5_6e7_8; const b = 0xFF_FFn;', '<test>');
  try
    ScanAllTokens(Lexer, glgInputElementRegExp);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[3].TokenType).ToBe(gttNumber);
    Expect<string>(Tokens[3].Lexeme).ToBe('1234.56e78');
    Expect<TGocciaTokenType>(Tokens[8].TokenType).ToBe(gttBigInt);
    Expect<string>(Tokens[8].Lexeme).ToBe('0xFFFF');
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestTemplateInterpolationTracksLineTerminators;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('`a${/* x' + #13#10 + '*/ 1}b`;const x = 1;', '<test>');
  try
    ScanGoalSequence(Lexer, [
      glgInputElementRegExp,
      glgInputElementRegExp,
      glgInputElementRegExp,
      glgInputElementTemplateTail,
      glgInputElementRegExp,
      glgInputElementRegExp
    ]);
    Tokens := Lexer.Tokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttTemplateHead);
    Expect<TGocciaTokenType>(Tokens[3].TokenType).ToBe(gttTemplateTail);
    Expect<TGocciaTokenType>(Tokens[5].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[5].Line).ToBe(2);
    Expect<Integer>(Tokens[5].Column).ToBe(9);
  finally
    Lexer.Free;
  end;
end;

procedure TLexerTests.TestUnicodeEscapeOverflowRaisesLexerError;
var
  Lexer: TGocciaLexer;
  Raised: Boolean;
  MessageMatches: Boolean;
begin
  Lexer := TGocciaLexer.Create(
    'const value = "\u{FFFFFFFFFFFFFFFFFFFFFFFF}";',
    '<test>');
  try
    Raised := False;
    MessageMatches := False;
    try
      ScanAllTokens(Lexer, glgInputElementRegExp);
    except
      on E: TGocciaLexerError do
      begin
        Raised := True;
        MessageMatches := Pos('Invalid unicode code point', E.Message) > 0;
      end;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    Expect<Boolean>(MessageMatches).ToBe(True);
  finally
    Lexer.Free;
  end;
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TLexerTests.Create('Lexer'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
