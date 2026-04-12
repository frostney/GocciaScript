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
  public
    procedure SetupTests; override;
  end;

procedure TLexerTests.SetupTests;
begin
  Test('Ignores leading hashbang', TestIgnoresLeadingHashbang);
  Test('Preserves line numbers after hashbang', TestPreservesLineNumbersAfterHashbang);
  Test('Terminates comment on Unicode line terminators', TestCommentTerminatedByUnicodeLineTerminators);
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
  AssertPreservesLineNumbersAfterHashbang(UTF8_LINE_SEPARATOR);
  AssertPreservesLineNumbersAfterHashbang(UTF8_PARAGRAPH_SEPARATOR);
end;

procedure TLexerTests.TestCommentTerminatedByUnicodeLineTerminators;
const
  // ES2026 §12.3 Line Terminators — UTF-8 encoding of LS and PS
  UTF8_LINE_SEPARATOR = #$E2#$80#$A8;
  UTF8_PARAGRAPH_SEPARATOR = #$E2#$80#$A9;
begin
  AssertCommentTerminatedBy(UTF8_LINE_SEPARATOR);
  AssertCommentTerminatedBy(UTF8_PARAGRAPH_SEPARATOR);
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
