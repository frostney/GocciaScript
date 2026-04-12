program Goccia.Lexer.Test;

{$I Goccia.inc}

uses
  Generics.Collections,
  SysUtils,

  TestRunner,

  Goccia.GarbageCollector,
  Goccia.Lexer,
  Goccia.TestSetup,
  Goccia.Token;

type
  TLexerTests = class(TTestSuite)
  private
    procedure AssertIgnoresLeadingHashbang(const ALineBreak: string);
    procedure AssertPreservesLineNumbersAfterHashbang(const ALineBreak: string);
    procedure AssertCommentTerminatedBy(const ALineBreak: string);
    procedure TestIgnoresLeadingHashbang;
    procedure TestPreservesLineNumbersAfterHashbang;
    procedure TestCommentTerminatedByUnicodeLineTerminators;
    procedure TestCRIncrementsLineInWhitespace;
    procedure TestCRIncrementsLineInBlockComment;
  public
    procedure SetupTests; override;
  end;

procedure TLexerTests.SetupTests;
begin
  Test('Ignores leading hashbang', TestIgnoresLeadingHashbang);
  Test('Preserves line numbers after hashbang', TestPreservesLineNumbersAfterHashbang);
  Test('Terminates comment on Unicode line terminators', TestCommentTerminatedByUnicodeLineTerminators);
  Test('CR increments line counter in whitespace', TestCRIncrementsLineInWhitespace);
  Test('CR increments line counter in block comments', TestCRIncrementsLineInBlockComment);
end;

procedure TLexerTests.AssertIgnoresLeadingHashbang(const ALineBreak: string);
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create('#!/usr/bin/env goccia' + ALineBreak + 'const value = 1 / 2;', '<test>');
  try
    Tokens := Lexer.ScanTokens;
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
    Tokens := Lexer.ScanTokens;
    Expect<Integer>(Tokens[0].Line).ToBe(2);
    Expect<Integer>(Tokens[0].Column).ToBe(1);
    Expect<Integer>(Tokens[1].Line).ToBe(2);
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
    Tokens := Lexer.ScanTokens;
    Expect<Integer>(Tokens.Count).ToBe(6);
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
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

procedure TLexerTests.TestCRIncrementsLineInWhitespace;
var
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
begin
  // Standalone CR between statements
  Lexer := TGocciaLexer.Create('const a = 1;' + #13 + 'const b = 2;', '<test>');
  try
    Tokens := Lexer.ScanTokens;
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
    Tokens := Lexer.ScanTokens;
    Expect<TGocciaTokenType>(Tokens[5].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[5].Line).ToBe(2);
    Expect<Integer>(Tokens[5].Column).ToBe(1);
  finally
    Lexer.Free;
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
    Tokens := Lexer.ScanTokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
  finally
    Lexer.Free;
  end;

  // CRLF inside block comment counts as single line break
  Lexer := TGocciaLexer.Create('/* a' + #13#10 + 'b */const x = 1;', '<test>');
  try
    Tokens := Lexer.ScanTokens;
    Expect<TGocciaTokenType>(Tokens[0].TokenType).ToBe(gttConst);
    Expect<Integer>(Tokens[0].Line).ToBe(2);
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
