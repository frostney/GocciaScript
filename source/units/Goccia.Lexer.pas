unit Goccia.Lexer;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  StringBuffer,

  Goccia.Token;

type
  TGocciaLexicalGoal = (
    glgInputElementDiv,
    glgInputElementRegExp,
    glgInputElementRegExpOrTemplateTail,
    glgInputElementTemplateTail,
    glgInputElementHashbangOrRegExp
  );

  TGocciaLexerCheckpoint = record
    Current: Integer;
    Line: Integer;
    Column: Integer;
    Start: Integer;
    StartColumn: Integer;
    TokenCount: Integer;
    HashbangSkipped: Boolean;
    EOFEmitted: Boolean;
  end;

  TGocciaLexer = class
  private
    FSource: string;
    FTokens: TObjectList<TGocciaToken>;
    FCurrent: Integer;
    FLine: Integer;
    FColumn: Integer;
    FStart: Integer;
    FStartColumn: Integer;
    FFileName: string;
    FSourceLines: TStringList;
    FHashbangSkipped: Boolean;
    FEOFEmitted: Boolean;
    FScanTimeNanoseconds: Int64;
    function GetSourceLines: TStringList;

    function IsAtEnd: Boolean; inline;
    function Advance: Char; inline;
    function Peek: Char; inline;
    function PeekNext: Char; inline;
    function Match(const AExpected: Char): Boolean; inline;
    function IsSourceIdentifierStartCodePoint(ACodePoint: Cardinal): Boolean; inline;
    function IsSourceIdentifierPartCodePoint(ACodePoint: Cardinal): Boolean; inline;
    function IsValidEscapedIdentifierText(const AText: string;
      const AAtStart: Boolean): Boolean;
    function TryReadIdentifierCodePoint(out ACodePoint: Cardinal;
      out AText: string; out AByteLength: Integer): Boolean;
    procedure AddToken(const ATokenType: TGocciaTokenType); overload;
    procedure AddToken(const ATokenType: TGocciaTokenType; const ALiteral: string;
      const AContainsEscape: Boolean = False); overload;
    procedure ScanToken(const ALexicalGoal: TGocciaLexicalGoal);
    procedure ScanString;
    procedure ScanTemplateStart;
    procedure ScanTemplateContinuation;
    procedure ScanTemplateSpan(const AContinuationTokenType,
      AEndTokenType: TGocciaTokenType);
    procedure ScanRegexLiteral;
    procedure ScanNumber;
    procedure ScanIdentifier;
    function ScanUnicodeEscape: string;
    function ScanHexEscape: string;
    procedure ProcessEscapeSequence(var ASB: TStringBuffer);
    // TC39 Template Literal Revision — template-aware variants that return
    // False on malformed escapes instead of raising, allowing tagged templates
    // to set cooked=undefined while preserving the raw source text.
    function ScanUnicodeEscapeForTemplate(var ASB: TStringBuffer): Boolean;
    function ScanHexEscapeForTemplate(var ASB: TStringBuffer): Boolean;
    procedure ProcessTemplateEscapeSequence(var ASB: TStringBuffer; var ASegmentValid: Boolean);
    procedure SkipWhitespace;
    procedure SkipHashbang;
    procedure SkipComment;
    procedure SkipBlockComment;
    procedure SkipUntilLineTerminator;
    function ConsumeWhitespaceCodePoint: Boolean; inline;
    function IsLineTerminator: Boolean; inline;
    function IsUnicodeLineTerminator: Boolean; inline;
    function ConsumeLineTerminator: Boolean; inline;
    function AppendLineTerminator(var ASB, ARawSB: TStringBuffer): Boolean; inline;
    procedure ConsumeUnicodeLineTerminator; inline;
  public
    constructor Create(const ASource, AFileName: string);
    destructor Destroy; override;
    function CreateCheckpoint: TGocciaLexerCheckpoint;
    procedure RestoreCheckpoint(const ACheckpoint: TGocciaLexerCheckpoint);
    // True when a token at or after ACount is goal-sensitive ('/' or a
    // template-tail '}'), i.e. when a speculative scan's look-ahead tokens
    // cannot be reused and must be re-lexed under the parser's real goal
    // (issue #808).
    function HasGoalSensitiveTokenSince(const ACount: Integer): Boolean;
    function ScanNextToken(const ALexicalGoal: TGocciaLexicalGoal): TGocciaToken;
    property ScanTimeNanoseconds: Int64 read FScanTimeNanoseconds;
    property Tokens: TObjectList<TGocciaToken> read FTokens;
    property SourceLines: TStringList read GetSourceLines;
  end;

implementation

uses
  TextSemantics,
  TimingUtils,

  Goccia.Error,
  Goccia.Error.Suggestions,
  Goccia.Identifier,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved;

const
  // ES2026 §12.3 Line Terminators — UTF-8 byte components for LS (U+2028) and PS (U+2029)
  UTF8_LINE_TERMINATOR_LEAD_BYTE = #$E2;
  UTF8_LINE_TERMINATOR_CONTINUATION_BYTE = #$80;
  UTF8_LINE_SEPARATOR_FINAL_BYTE = #$A8;
  UTF8_PARAGRAPH_SEPARATOR_FINAL_BYTE = #$A9;
  UTF8_LINE_TERMINATOR_BYTE_LENGTH = 3;
  LINE_FEED_CODE_POINT = $000A;
  CARRIAGE_RETURN_CODE_POINT = $000D;
  LINE_SEPARATOR_CODE_POINT = $2028;
  PARAGRAPH_SEPARATOR_CODE_POINT = $2029;
  MAX_UNICODE_CODE_POINT = $10FFFF;

type
  TKeywordTokenEntry = record
    Keyword: string;
    TokenType: TGocciaTokenType;
  end;

  TKeywordTokenLengthRange = record
    KeywordLength: Integer;
    First: Integer;
    Last: Integer;
  end;

const
  KEYWORD_TOKEN_INDEX_AS         = 0;
  KEYWORD_TOKEN_INDEX_IN         = 3;
  KEYWORD_TOKEN_INDEX_FOR        = 4;
  KEYWORD_TOKEN_INDEX_VAR        = 8;
  KEYWORD_TOKEN_INDEX_CASE       = 9;
  KEYWORD_TOKEN_INDEX_WITH       = 17;
  KEYWORD_TOKEN_INDEX_BREAK      = 18;
  KEYWORD_TOKEN_INDEX_WHILE      = 25;
  KEYWORD_TOKEN_INDEX_DELETE     = 26;
  KEYWORD_TOKEN_INDEX_TYPEOF     = 32;
  KEYWORD_TOKEN_INDEX_DEFAULT    = 33;
  KEYWORD_TOKEN_INDEX_FINALLY    = 35;
  KEYWORD_TOKEN_INDEX_CONTINUE   = 36;
  KEYWORD_TOKEN_INDEX_FUNCTION   = 37;
  KEYWORD_TOKEN_INDEX_INSTANCEOF = 38;

  KEYWORD_TOKEN_COUNT = KEYWORD_TOKEN_INDEX_INSTANCEOF + 1;
  KEYWORD_TOKEN_RANGE_COUNT = 8;

  // Grouped by keyword length so KeywordTokenRanges can point to a
  // small contiguous slice for each candidate identifier length.
  KeywordTokens: array[0..KEYWORD_TOKEN_COUNT - 1] of TKeywordTokenEntry = (
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

  KeywordTokenRanges: array[0..KEYWORD_TOKEN_RANGE_COUNT - 1] of TKeywordTokenLengthRange = (
    (KeywordLength: Length(KEYWORD_AS); First: KEYWORD_TOKEN_INDEX_AS; Last: KEYWORD_TOKEN_INDEX_IN),
    (KeywordLength: Length(KEYWORD_FOR); First: KEYWORD_TOKEN_INDEX_FOR; Last: KEYWORD_TOKEN_INDEX_VAR),
    (KeywordLength: Length(KEYWORD_CASE); First: KEYWORD_TOKEN_INDEX_CASE; Last: KEYWORD_TOKEN_INDEX_WITH),
    (KeywordLength: Length(KEYWORD_BREAK); First: KEYWORD_TOKEN_INDEX_BREAK; Last: KEYWORD_TOKEN_INDEX_WHILE),
    (KeywordLength: Length(KEYWORD_DELETE); First: KEYWORD_TOKEN_INDEX_DELETE; Last: KEYWORD_TOKEN_INDEX_TYPEOF),
    (KeywordLength: Length(KEYWORD_DEFAULT); First: KEYWORD_TOKEN_INDEX_DEFAULT; Last: KEYWORD_TOKEN_INDEX_FINALLY),
    (KeywordLength: Length(KEYWORD_CONTINUE); First: KEYWORD_TOKEN_INDEX_CONTINUE; Last: KEYWORD_TOKEN_INDEX_FUNCTION),
    (KeywordLength: Length(KEYWORD_INSTANCEOF); First: KEYWORD_TOKEN_INDEX_INSTANCEOF;
      Last: KEYWORD_TOKEN_INDEX_INSTANCEOF)
  );

function IsValidHexString(const AValue: string): Boolean;
var
  I: Integer;
begin
  if Length(AValue) = 0 then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not CharInSet(AValue[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
      Exit(False);
  Result := True;
end;

function TryParseUnicodeCodePointHex(const AValue: string;
  out ACodePoint: Cardinal): Boolean;
var
  CodePointValue: QWord;
  SignificantHex: string;
  StartIndex: Integer;
begin
  ACodePoint := 0;
  StartIndex := 1;
  while (StartIndex <= Length(AValue)) and (AValue[StartIndex] = '0') do
    Inc(StartIndex);

  if StartIndex > Length(AValue) then
    Exit(True);

  SignificantHex := Copy(AValue, StartIndex, Length(AValue) - StartIndex + 1);
  if Length(SignificantHex) > 6 then
    Exit(False);

  Result := TryStrToQWord('$' + SignificantHex, CodePointValue) and
    (CodePointValue <= MAX_UNICODE_CODE_POINT);
  if Result then
    ACodePoint := Cardinal(CodePointValue);
end;

function TryKeywordToken(const AText: string; out ATokenType: TGocciaTokenType): Boolean; inline;
var
  Len: Integer;
  Range: TKeywordTokenLengthRange;
  RangeIndex: Integer;
  TokenIndex: Integer;
begin
  Len := Length(AText);

  for RangeIndex := Low(KeywordTokenRanges) to High(KeywordTokenRanges) do
    if Len = KeywordTokenRanges[RangeIndex].KeywordLength then
    begin
      Range := KeywordTokenRanges[RangeIndex];
      for TokenIndex := Range.First to Range.Last do
        if AText = KeywordTokens[TokenIndex].Keyword then
        begin
          ATokenType := KeywordTokens[TokenIndex].TokenType;
          Exit(True);
        end;
      Exit(False);
    end;

  Result := False;
end;

constructor TGocciaLexer.Create(const ASource, AFileName: string);
begin
  FSource := ASource;
  FFileName := AFileName;
  FTokens := TObjectList<TGocciaToken>.Create(True);
  FSourceLines := nil;
  FCurrent := 1;
  FLine := 1;
  FColumn := 1;
  FHashbangSkipped := False;
  FEOFEmitted := False;
  FScanTimeNanoseconds := 0;
end;

destructor TGocciaLexer.Destroy;
begin
  FTokens.Free;
  FSourceLines.Free;
  inherited;
end;

function TGocciaLexer.CreateCheckpoint: TGocciaLexerCheckpoint;
begin
  Result.Current := FCurrent;
  Result.Line := FLine;
  Result.Column := FColumn;
  Result.Start := FStart;
  Result.StartColumn := FStartColumn;
  Result.TokenCount := FTokens.Count;
  Result.HashbangSkipped := FHashbangSkipped;
  Result.EOFEmitted := FEOFEmitted;
end;

procedure TGocciaLexer.RestoreCheckpoint(
  const ACheckpoint: TGocciaLexerCheckpoint);
begin
  FCurrent := ACheckpoint.Current;
  FLine := ACheckpoint.Line;
  FColumn := ACheckpoint.Column;
  FStart := ACheckpoint.Start;
  FStartColumn := ACheckpoint.StartColumn;
  FHashbangSkipped := ACheckpoint.HashbangSkipped;
  FEOFEmitted := ACheckpoint.EOFEmitted;

  while FTokens.Count > ACheckpoint.TokenCount do
    FTokens.Delete(FTokens.Count - 1);
end;

// Whether any token at or after index ACount is goal-sensitive — would be
// classified differently under another lexical goal — so the speculative
// parenthesized-group scans (TGocciaParser) must drop their look-ahead tokens
// and let the real parse re-lex them, rather than reuse them (issue #808).
//
// Only two lexer branches depend on the goal:
//   * '/' — a regex literal (gttRegex) vs a division operator (gttSlash /
//     gttSlashAssign); always goal-sensitive.
//   * a '}' that closes a `${...}` template substitution — gttTemplateMiddle /
//     gttTemplateTail under the template-tail goal vs an ordinary gttRightBrace
//     otherwise. A probe scans a template's opener (gttTemplateHead) before
//     reaching its closer, so a '}' only matters once a substitution is open;
//     object, block, and destructuring braces with no template in scope stay
//     reusable.
function TGocciaLexer.HasGoalSensitiveTokenSince(
  const ACount: Integer): Boolean;
var
  I: Integer;
  InTemplateSubstitution: Boolean;
begin
  InTemplateSubstitution := False;
  for I := ACount to FTokens.Count - 1 do
    case FTokens[I].TokenType of
      gttRegex, gttSlash, gttSlashAssign:
        Exit(True);
      gttTemplateHead, gttTemplateMiddle:
        InTemplateSubstitution := True;
      gttRightBrace, gttTemplateTail:
        if InTemplateSubstitution then
          Exit(True);
    end;
  Result := False;
end;

function TGocciaLexer.GetSourceLines: TStringList;
begin
  if not Assigned(FSourceLines) then
    FSourceLines := CreateECMAScriptSourceLines(FSource);
  Result := FSourceLines;
end;

function TGocciaLexer.IsAtEnd: Boolean; inline;
begin
  Result := FCurrent > Length(FSource);
end;

function TGocciaLexer.Advance: Char; inline;
begin
  Result := FSource[FCurrent];
  Inc(FCurrent);
  Inc(FColumn);
end;

function TGocciaLexer.Peek: Char; inline;
begin
  if IsAtEnd then
    Result := #0
  else
    Result := FSource[FCurrent];
end;

function TGocciaLexer.PeekNext: Char; inline;
begin
  if FCurrent + 1 > Length(FSource) then
    Result := #0
  else
    Result := FSource[FCurrent + 1];
end;

function TGocciaLexer.Match(const AExpected: Char): Boolean; inline;
begin
  if IsAtEnd then
  begin
    Result := False;
    Exit;
  end;

  if FSource[FCurrent] <> AExpected then
  begin
    Result := False;
    Exit;
  end;

  Inc(FCurrent);
  Inc(FColumn);
  Result := True;
end;

procedure TGocciaLexer.AddToken(const ATokenType: TGocciaTokenType);
begin
  AddToken(ATokenType, Copy(FSource, FStart, FCurrent - FStart));
end;

procedure TGocciaLexer.AddToken(const ATokenType: TGocciaTokenType; const ALiteral: string;
  const AContainsEscape: Boolean);
begin
  FTokens.Add(TGocciaToken.Create(ATokenType, ALiteral, FLine, FStartColumn,
    FColumn - 1, AContainsEscape));
end;

// ES2026 §12.3 LineTerminator :: <LF> | <CR> | <LS> | <PS>
function TGocciaLexer.IsLineTerminator: Boolean; inline;
begin
  if IsAtEnd then
    Exit(False);
  case FSource[FCurrent] of
    #10, #13:
      Result := True;
  else
    Result := IsUnicodeLineTerminator;
  end;
end;

// ES2026 §12.3 LS (U+2028) = UTF-8 E2 80 A8, PS (U+2029) = UTF-8 E2 80 A9
function TGocciaLexer.IsUnicodeLineTerminator: Boolean; inline;
begin
  Result := (FSource[FCurrent] = UTF8_LINE_TERMINATOR_LEAD_BYTE) and
            (FCurrent + 2 <= Length(FSource)) and
            (FSource[FCurrent + 1] = UTF8_LINE_TERMINATOR_CONTINUATION_BYTE) and
            ((FSource[FCurrent + 2] = UTF8_LINE_SEPARATOR_FINAL_BYTE) or
             (FSource[FCurrent + 2] = UTF8_PARAGRAPH_SEPARATOR_FINAL_BYTE));
end;

procedure TGocciaLexer.ConsumeUnicodeLineTerminator; inline;
begin
  Inc(FCurrent, UTF8_LINE_TERMINATOR_BYTE_LENGTH);
  Inc(FLine);
  FColumn := 1;
end;

function TGocciaLexer.ConsumeLineTerminator: Boolean; inline;
begin
  if IsAtEnd then
    Exit(False);

  case FSource[FCurrent] of
    #13:
      begin
        Inc(FCurrent);
        if (not IsAtEnd) and (FSource[FCurrent] = #10) then
          Inc(FCurrent);
        Inc(FLine);
        FColumn := 1;
        Result := True;
      end;
    #10:
      begin
        Inc(FCurrent);
        Inc(FLine);
        FColumn := 1;
        Result := True;
      end;
    UTF8_LINE_TERMINATOR_LEAD_BYTE:
      begin
        Result := IsUnicodeLineTerminator;
        if Result then
          ConsumeUnicodeLineTerminator;
      end;
  else
    Result := False;
  end;
end;

function TGocciaLexer.AppendLineTerminator(var ASB, ARawSB: TStringBuffer): Boolean; inline;
var
  C: Char;
  I: Integer;
begin
  if IsAtEnd then
    Exit(False);

  case FSource[FCurrent] of
    #13:
      begin
        Inc(FCurrent);
        if (not IsAtEnd) and (FSource[FCurrent] = #10) then
          Inc(FCurrent);
        Inc(FLine);
        FColumn := 1;
        ASB.AppendChar(#10);
        ARawSB.AppendChar(#10);
        Result := True;
      end;
    #10:
      begin
        Inc(FCurrent);
        Inc(FLine);
        FColumn := 1;
        ASB.AppendChar(#10);
        ARawSB.AppendChar(#10);
        Result := True;
      end;
    UTF8_LINE_TERMINATOR_LEAD_BYTE:
      begin
        Result := IsUnicodeLineTerminator;
        if Result then
        begin
          for I := 0 to UTF8_LINE_TERMINATOR_BYTE_LENGTH - 1 do
          begin
            C := FSource[FCurrent + I];
            ASB.AppendChar(C);
            ARawSB.AppendChar(C);
          end;
          ConsumeUnicodeLineTerminator;
        end;
      end;
  else
    Result := False;
  end;
end;

// ES2026 §12.2 WhiteSpace :: <TAB> | <VT> | <FF> | <ZWNBSP> | <USP>
function TGocciaLexer.ConsumeWhitespaceCodePoint: Boolean; inline;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
begin
  Result := False;
  if IsAtEnd then
    Exit;
  if not TryReadUTF8CodePoint(FSource, FCurrent, CodePoint, ByteLength) then
    Exit;
  if not IsECMAScriptWhitespaceCodePoint(CodePoint) then
    Exit;

  // Line terminators must go through ConsumeLineTerminator so line/column
  // state stays correct.
  case CodePoint of
    LINE_FEED_CODE_POINT,
    CARRIAGE_RETURN_CODE_POINT,
    LINE_SEPARATOR_CODE_POINT,
    PARAGRAPH_SEPARATOR_CODE_POINT:
      Exit;
  end;

  Inc(FCurrent, ByteLength);
  Inc(FColumn, ByteLength);
  Result := True;
end;

procedure TGocciaLexer.SkipWhitespace;
begin
  while not IsAtEnd do
  begin
    if ConsumeLineTerminator then
      Continue;

    case Peek of
      ' ', #9, #11, #12:
        Advance;
      '/':
        if PeekNext = '/' then
          SkipComment
        else if PeekNext = '*' then
          SkipBlockComment
        else
          Break;
    else
      if not ConsumeWhitespaceCodePoint then
        Break;
    end;
  end;
end;

procedure TGocciaLexer.SkipUntilLineTerminator;
var
  C: Char;
begin
  while not IsAtEnd do
  begin
    C := FSource[FCurrent];
    if (C = #10) or (C = #13) or
       ((C = UTF8_LINE_TERMINATOR_LEAD_BYTE) and IsUnicodeLineTerminator) then
      Break;
    Inc(FCurrent);
    Inc(FColumn);
  end;
end;

// ES2026 §12.5 HashbangComment :: #! SingleLineCommentCharsₒₚₜ
procedure TGocciaLexer.SkipHashbang;
begin
  if (FCurrent <> 1) or (Length(FSource) < 2) or (Copy(FSource, 1, 2) <> '#!') then
    Exit;

  SkipUntilLineTerminator;
end;

// ES2026 §12.4 SingleLineComment :: // SingleLineCommentCharsₒₚₜ
procedure TGocciaLexer.SkipComment;
begin
  // Skip '//'
  Advance;
  Advance;

  SkipUntilLineTerminator;
end;

procedure TGocciaLexer.SkipBlockComment;
var
  C: Char;
begin
  // Skip '/*'
  Advance;
  Advance;

  while not IsAtEnd do
  begin
    C := FSource[FCurrent];
    if C = '*' then
    begin
      Inc(FCurrent);
      Inc(FColumn);
      if (not IsAtEnd) and (FSource[FCurrent] = '/') then
      begin
        Inc(FCurrent);
        Inc(FColumn);
        Exit;
      end;
    end
    else if (C = #13) or (C = #10) or
            ((C = UTF8_LINE_TERMINATOR_LEAD_BYTE) and IsUnicodeLineTerminator) then
      ConsumeLineTerminator
    else
    begin
      Inc(FCurrent);
      Inc(FColumn);
    end;
  end;

  // If we reach here, we hit end of file without finding closing */
  raise TGocciaLexerError.Create('Unterminated block comment', FLine, FColumn,
    FFileName, GetSourceLines, SSuggestCloseBlockComment);
end;

function TGocciaLexer.ScanUnicodeEscape: string;
var
  CodePoint, LowSurrogate: Cardinal;
  HexStr: string;
  I, HexStart, SavedCurrent, SavedColumn: Integer;
begin
  // Called after consuming '\u', Peek is the next character
  if Peek = '{' then
  begin
    Advance; // consume '{'
    HexStart := FCurrent;
    while (Peek <> '}') and not IsAtEnd do
      Advance;
    if IsAtEnd then
      raise TGocciaLexerError.Create('Unterminated unicode escape', FLine, FColumn, FFileName, GetSourceLines,
        SSuggestUnicodeEscapeFormat);
    HexStr := Copy(FSource, HexStart, FCurrent - HexStart);
    if not IsValidHexString(HexStr) then
      raise TGocciaLexerError.Create('Invalid unicode escape', FLine, FColumn, FFileName, GetSourceLines,
        SSuggestUnicodeHexDigits);
    Advance; // consume '}'
    if not TryParseUnicodeCodePointHex(HexStr, CodePoint) then
      raise TGocciaLexerError.Create('Invalid unicode code point', FLine, FColumn, FFileName, GetSourceLines,
        SSuggestUnicodeCodePointRange);
  end
  else
  begin
    HexStart := FCurrent;
    for I := 1 to 4 do
    begin
      if IsAtEnd then
        raise TGocciaLexerError.Create('Invalid unicode escape', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestUnicodeHexDigits);
      Advance;
    end;
    HexStr := Copy(FSource, HexStart, FCurrent - HexStart);
    if not IsValidHexString(HexStr) then
      raise TGocciaLexerError.Create('Invalid unicode escape', FLine, FColumn, FFileName, GetSourceLines,
        SSuggestUnicodeHexDigits);
    CodePoint := StrToInt('$' + HexStr);
  end;

  // ES2026 §12.9.4: Combine UTF-16 surrogate pairs into a single code point
  if (CodePoint >= $D800) and (CodePoint <= $DBFF) and (Peek = '\') and (PeekNext = 'u') then
  begin
    SavedCurrent := FCurrent;
    SavedColumn := FColumn;
    Advance; // consume '\'
    Advance; // consume 'u'
    HexStart := FCurrent;
    for I := 1 to 4 do
    begin
      if IsAtEnd then
      begin
        FCurrent := SavedCurrent;
        FColumn := SavedColumn;
        Break;
      end;
      Advance;
    end;
    HexStr := Copy(FSource, HexStart, FCurrent - HexStart);
    if (Length(HexStr) = 4) and IsValidHexString(HexStr) then
    begin
      LowSurrogate := StrToInt('$' + HexStr);
      if (LowSurrogate >= $DC00) and (LowSurrogate <= $DFFF) then
        CodePoint := $10000 + ((CodePoint - $D800) shl 10) + (LowSurrogate - $DC00)
      else
      begin
        FCurrent := SavedCurrent;
        FColumn := SavedColumn;
      end;
    end;
  end;

  if CodePoint > MAX_UNICODE_CODE_POINT then
    raise TGocciaLexerError.Create('Invalid unicode code point', FLine, FColumn, FFileName, GetSourceLines,
      SSuggestUnicodeCodePointRange);
  Result := TextSemantics.CodePointToUTF8(CodePoint);
end;

function TGocciaLexer.ScanHexEscape: string;
var
  HexStr: string;
  HexStart: Integer;
  CodePoint: Cardinal;
begin
  // Called after consuming '\x', Peek is the first hex digit
  if IsAtEnd then
    raise TGocciaLexerError.Create('Invalid hex escape', FLine, FColumn, FFileName, GetSourceLines,
      SSuggestHexEscapeFormat);
  HexStart := FCurrent;
  Advance;
  if IsAtEnd then
    raise TGocciaLexerError.Create('Invalid hex escape', FLine, FColumn, FFileName, GetSourceLines,
      SSuggestHexEscapeFormat);
  Advance;
  HexStr := Copy(FSource, HexStart, 2);
  if not IsValidHexString(HexStr) then
    raise TGocciaLexerError.Create('Invalid hex escape', FLine, FColumn, FFileName, GetSourceLines,
      SSuggestHexEscapeFormat);

  CodePoint := StrToInt('$' + HexStr);
  Result := TextSemantics.CodePointToUTF8(CodePoint);
end;

procedure TGocciaLexer.ProcessEscapeSequence(var ASB: TStringBuffer);
var
  FirstDigit: Char;
  OctalValue: Integer;
begin
  if IsLineTerminator then
  begin
    ConsumeLineTerminator;
    Exit;
  end;

  case Peek of
    'b': begin ASB.AppendChar(#8); Advance; end;
    'f': begin ASB.AppendChar(#12); Advance; end;
    'n': begin ASB.AppendChar(#10); Advance; end;
    'r': begin ASB.AppendChar(#13); Advance; end;
    't': begin ASB.AppendChar(#9); Advance; end;
    'v': begin ASB.AppendChar(#11); Advance; end;
    '\': begin ASB.AppendChar('\'); Advance; end;
    '0'..'7':
      begin
        FirstDigit := Peek;
        OctalValue := Ord(FirstDigit) - Ord('0');
        Advance;
        if CharInSet(FirstDigit, ['0'..'3']) and
           CharInSet(Peek, ['0'..'7']) then
        begin
          OctalValue := OctalValue * 8 + Ord(Peek) - Ord('0');
          Advance;
          if CharInSet(Peek, ['0'..'7']) then
          begin
            OctalValue := OctalValue * 8 + Ord(Peek) - Ord('0');
            Advance;
          end;
        end
        else if CharInSet(FirstDigit, ['4'..'7']) and
                CharInSet(Peek, ['0'..'7']) then
        begin
          OctalValue := OctalValue * 8 + Ord(Peek) - Ord('0');
          Advance;
        end;
        ASB.Append(TextSemantics.CodePointToUTF8(OctalValue));
      end;
    'u': begin Advance; ASB.Append(ScanUnicodeEscape); end;
    'x': begin Advance; ASB.Append(ScanHexEscape); end;
  else
    ASB.AppendChar(Peek);
    Advance;
  end;
end;

// TC39 Template Literal Revision — template-aware Unicode escape scanner.
// Returns True on success (resolved value appended to ASB), False on malformed
// escape (cursor advanced past the consumed characters, nothing appended).
// Does not raise TGocciaLexerError — the caller marks the segment as invalid.
function TGocciaLexer.ScanUnicodeEscapeForTemplate(var ASB: TStringBuffer): Boolean;
var
  CodePoint, LowSurrogate: Cardinal;
  HexStr: string;
  I, HexStart, SavedCurrent, SavedColumn: Integer;
begin
  // Called after consuming '\u', Peek is the next character
  if Peek = '{' then
  begin
    Advance; // consume '{'
    HexStart := FCurrent;
    // Scan hex digits, stopping at '}', backtick, or EOF.
    // Non-hex characters also stop the scan (invalid escape).
    while (Peek <> '}') and (Peek <> '`') and not IsAtEnd do
    begin
      if not CharInSet(Peek, ['0'..'9', 'a'..'f', 'A'..'F']) then
        Break;
      Advance;
    end;
    HexStr := Copy(FSource, HexStart, FCurrent - HexStart);
    if (Peek <> '}') or (HexStr = '') then
      Exit(False); // unterminated or empty
    if not IsValidHexString(HexStr) then
      Exit(False);
    Advance; // consume '}'
    if not TryParseUnicodeCodePointHex(HexStr, CodePoint) then
      Exit(False);
  end
  else
  begin
    HexStart := FCurrent;
    for I := 1 to 4 do
    begin
      if IsAtEnd or (Peek = '`') then
        Exit(False);
      Advance;
    end;
    HexStr := Copy(FSource, HexStart, FCurrent - HexStart);
    if not IsValidHexString(HexStr) then
      Exit(False);
    CodePoint := StrToInt('$' + HexStr);
  end;

  // ES2026 §12.9.4: Combine UTF-16 surrogate pairs into a single code point.
  // Guard against scanning past the closing backtick when the low surrogate
  // probe is incomplete or malformed — restore the cursor on every failure path.
  if (CodePoint >= $D800) and (CodePoint <= $DBFF) and (Peek = '\') and (PeekNext = 'u') then
  begin
    SavedCurrent := FCurrent;
    SavedColumn := FColumn;
    Advance; // consume '\'
    Advance; // consume 'u'
    HexStart := FCurrent;
    for I := 1 to 4 do
    begin
      if IsAtEnd or (Peek = '`') then
      begin
        FCurrent := SavedCurrent;
        FColumn := SavedColumn;
        Break;
      end;
      Advance;
    end;
    HexStr := Copy(FSource, HexStart, FCurrent - HexStart);
    if (Length(HexStr) = 4) and IsValidHexString(HexStr) then
    begin
      LowSurrogate := StrToInt('$' + HexStr);
      if (LowSurrogate >= $DC00) and (LowSurrogate <= $DFFF) then
        CodePoint := $10000 + ((CodePoint - $D800) shl 10) + (LowSurrogate - $DC00)
      else
      begin
        FCurrent := SavedCurrent;
        FColumn := SavedColumn;
      end;
    end
    else
    begin
      FCurrent := SavedCurrent;
      FColumn := SavedColumn;
    end;
  end;

  if CodePoint > MAX_UNICODE_CODE_POINT then
    Exit(False);
  ASB.Append(TextSemantics.CodePointToUTF8(CodePoint));
  Result := True;
end;

// TC39 Template Literal Revision — template-aware hex escape scanner.
// Returns True on success (resolved character appended to ASB), False on
// malformed escape (cursor advanced past consumed characters).
function TGocciaLexer.ScanHexEscapeForTemplate(var ASB: TStringBuffer): Boolean;
var
  HexStr: string;
  HexStart: Integer;
  CodePoint: Cardinal;
begin
  // Called after consuming '\x', Peek is the first hex digit
  if IsAtEnd or (Peek = '`') then
    Exit(False);
  HexStart := FCurrent;
  Advance;
  if IsAtEnd or (Peek = '`') then
    Exit(False);
  Advance;
  HexStr := Copy(FSource, HexStart, 2);
  if not IsValidHexString(HexStr) then
    Exit(False);

  CodePoint := StrToInt('$' + HexStr);
  ASB.Append(TextSemantics.CodePointToUTF8(CodePoint));
  Result := True;
end;

// TC39 Template Literal Revision — process an escape sequence in template
// context. On valid escapes, appends the resolved value to ASB. On invalid
// \u or \x escapes, sets ASegmentValid to False and does not append to ASB.
procedure TGocciaLexer.ProcessTemplateEscapeSequence(var ASB: TStringBuffer;
  var ASegmentValid: Boolean);
begin
  case Peek of
    'n': begin ASB.AppendChar(#10); Advance; end;
    'r': begin ASB.AppendChar(#13); Advance; end;
    't': begin ASB.AppendChar(#9); Advance; end;
    '\': begin ASB.AppendChar('\'); Advance; end;
    '0': begin ASB.AppendChar(#0); Advance; end;
    'u':
    begin
      Advance;
      if not ScanUnicodeEscapeForTemplate(ASB) then
        ASegmentValid := False;
    end;
    'x':
    begin
      Advance;
      if not ScanHexEscapeForTemplate(ASB) then
        ASegmentValid := False;
    end;
  else
    ASB.AppendChar(Peek);
    Advance;
  end;
end;

procedure TGocciaLexer.ScanString;
var
  SB: TStringBuffer;
  Quote: Char;
begin
  Quote := FSource[FStart];
  SB := TStringBuffer.Create;
  while (Peek <> Quote) and not IsAtEnd do
  begin
    if (Peek = #10) or (Peek = #13) then
      raise TGocciaLexerError.Create('String literals cannot contain unescaped line terminators',
        FLine, FColumn, FFileName, GetSourceLines, SSuggestStringLineTerminator);

    if Peek = '\' then
    begin
      Advance;
      if not IsAtEnd then
      begin
        case Peek of
          '''': begin SB.AppendChar(''''); Advance; end;
          '"': begin SB.AppendChar('"'); Advance; end;
        else
          ProcessEscapeSequence(SB);
        end;
      end;
    end
    else
      SB.AppendChar(Advance);
  end;

  if IsAtEnd then
    raise TGocciaLexerError.Create('Unterminated string', FLine, FColumn,
      FFileName, GetSourceLines, SSuggestCloseString);

  Advance; // Closing quote
  AddToken(gttString, SB.ToString);
end;

procedure TGocciaLexer.ScanTemplateStart;
begin
  ScanTemplateSpan(gttTemplateHead, gttTemplate);
end;

procedure TGocciaLexer.ScanTemplateContinuation;
begin
  FStart := FCurrent;
  FStartColumn := FColumn;
  ScanTemplateSpan(gttTemplateMiddle, gttTemplateTail);
end;

// ES2026 §12.9.6 Template Literal Lexical Components.
// Scans one template span only.  A span ends at `${` (continuation token) or
// at the closing backtick (end token); `${...}` expressions are parsed by the
// parser in the main token stream.
procedure TGocciaLexer.ScanTemplateSpan(const AContinuationTokenType,
  AEndTokenType: TGocciaTokenType);
const
  TEMPLATE_RAW_SEPARATOR = #1;
  TEMPLATE_INVALID_ESCAPE_SEPARATOR = #2;
var
  SB: TStringBuffer;
  RawSB: TStringBuffer;
  C: Char;
  RawStart, J: Integer;
  SegmentValid, HasInvalidEscape: Boolean;
  Separator: Char;
begin
  SB := TStringBuffer.Create;
  RawSB := TStringBuffer.Create;
  HasInvalidEscape := False;
  SegmentValid := True;
  while (Peek <> '`') and not IsAtEnd do
  begin
    C := Peek;
    // ES2026 §12.9.6: CR/CRLF normalize to LF; LS/PS preserve their bytes.
    if ((C = #13) or (C = #10) or (C = UTF8_LINE_TERMINATOR_LEAD_BYTE)) and
       AppendLineTerminator(SB, RawSB) then
      Continue
    else if C = '\' then
    begin
      Advance; // consume '\'
      if not IsAtEnd then
      begin
        case Peek of
          '`':
          begin
            SB.AppendChar('`');
            RawSB.AppendChar('\');
            RawSB.AppendChar('`');
            Advance;
          end;
          '$':
          begin
            SB.AppendChar('$');
            RawSB.AppendChar('\');
            RawSB.AppendChar('$');
            Advance;
          end;
        else
          begin
            // Capture raw escape sequence by tracking source positions.
            // Use the template-aware variant that tolerates malformed escapes.
            RawSB.AppendChar('\');
            RawStart := FCurrent;
            ProcessTemplateEscapeSequence(SB, SegmentValid);
            if not SegmentValid then
              HasInvalidEscape := True;
            // Copy the raw source text consumed by the escape sequence
            for J := RawStart to FCurrent - 1 do
              RawSB.AppendChar(FSource[J]);
          end;
        end;
      end;
    end
    else if (C = '$') and (PeekNext = '{') then
    begin
      Advance; // consume $
      Advance; // consume {
      if HasInvalidEscape then
        Separator := TEMPLATE_INVALID_ESCAPE_SEPARATOR
      else
        Separator := TEMPLATE_RAW_SEPARATOR;
      AddToken(AContinuationTokenType, SB.ToString + Separator + RawSB.ToString);
      Exit;
    end
    else
    begin
      C := Advance;
      SB.AppendChar(C);
      RawSB.AppendChar(C);
    end;
  end;

  if IsAtEnd then
    raise TGocciaLexerError.Create('Unterminated template literal', FLine, FColumn,
      FFileName, GetSourceLines, SSuggestCloseTemplate);

  Advance; // Closing backtick
  // TC39 Template Literal Revision: use #2 separator when the template contains
  // invalid escape sequences, signalling the parser to re-cook from raw.
  if HasInvalidEscape then
    Separator := TEMPLATE_INVALID_ESCAPE_SEPARATOR
  else
    Separator := TEMPLATE_RAW_SEPARATOR;
  AddToken(AEndTokenType, SB.ToString + Separator + RawSB.ToString);
end;

procedure TGocciaLexer.ScanRegexLiteral;
const
  REGEX_SEPARATOR = #0;
var
  PatternStart: Integer;
  Pattern: string;
  FlagsStart: Integer;
  Flags: string;
  InCharacterClass: Boolean;
  SawClosingDelimiter: Boolean;
  C: Char;
  I: Integer;
begin
  PatternStart := FCurrent;
  InCharacterClass := False;
  SawClosingDelimiter := False;

  // ES2026 §12.9.5: RegularExpressionNonTerminator :: SourceCharacter but not LineTerminator
  while not IsAtEnd do
  begin
    if IsLineTerminator then
      raise TGocciaLexerError.Create('Unterminated regular expression literal',
        FLine, FColumn, FFileName, GetSourceLines,
        SSuggestCloseRegex);

    C := Advance;

    if C = '\' then
    begin
      if IsAtEnd or IsLineTerminator then
        raise TGocciaLexerError.Create('Unterminated regular expression literal',
          FLine, FColumn, FFileName, GetSourceLines,
          SSuggestCloseRegex);
      Advance;
      Continue;
    end;

    if C = '[' then
      InCharacterClass := True
    else if (C = ']') and InCharacterClass then
      InCharacterClass := False
    else if (C = '/') and not InCharacterClass then
    begin
      SawClosingDelimiter := True;
      Break;
    end;
  end;

  if not SawClosingDelimiter then
    raise TGocciaLexerError.Create('Unterminated regular expression literal',
      FLine, FColumn, FFileName, GetSourceLines,
      SSuggestCloseRegex);

  Pattern := Copy(FSource, PatternStart, FCurrent - PatternStart - 1);

  FlagsStart := FCurrent;
  while CharInSet(Peek, ['a'..'z', 'A'..'Z']) do
    Advance;
  Flags := Copy(FSource, FlagsStart, FCurrent - FlagsStart);

  for I := 1 to Length(Flags) do
  begin
    if not CharInSet(Flags[I], ['d', 'g', 'i', 'm', 's', 'u', 'v', 'y']) then
      raise TGocciaLexerError.Create('Invalid regular expression flag: ' + Flags[I],
        FLine, FColumn, FFileName, GetSourceLines,
        SSuggestValidRegexFlags);
    if Pos(Flags[I], Copy(Flags, 1, I - 1)) > 0 then
      raise TGocciaLexerError.Create('Duplicate regular expression flag: ' + Flags[I],
        FLine, FColumn, FFileName, GetSourceLines,
        SSuggestDuplicateRegexFlag);
  end;

  if CharInSet(Peek, ['0'..'9', '_', '$', 'a'..'z', 'A'..'Z']) then
    raise TGocciaLexerError.Create('Invalid regular expression literal suffix',
      FLine, FColumn, FFileName, GetSourceLines,
      SSuggestRegexSuffixFlags);

  AddToken(gttRegex, IntToStr(Length(Pattern)) + REGEX_SEPARATOR + Pattern + Flags);
end;

// ES2026 §12.9.3 NumericLiteral
procedure TGocciaLexer.ScanNumber;
var
  Ch: Char;
  HasSeparator: Boolean;
  HasDecimalOrExponent: Boolean;
  IsDecimal: Boolean;
  Lexeme: string;

  procedure ConsumeDigitsWithSeparators(const AValidDigits: TSysCharSet); inline;
  var
    Digit: Char;
  begin
    while True do
    begin
      Digit := Peek;
      if CharInSet(Digit, AValidDigits) then
        Advance
      else if Digit = '_' then
      begin
        HasSeparator := True;
        Advance;
        if not CharInSet(Peek, AValidDigits) then
          raise TGocciaLexerError.Create('Numeric separator must be between digits',
            FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
      end
      else
        Break;
    end;
  end;
begin
  HasSeparator := False;
  HasDecimalOrExponent := False;
  IsDecimal := True;
  Ch := Peek;

  if Ch = '0' then
  begin
    Advance;
    Ch := Peek;

    if (Ch = 'x') or (Ch = 'X') then
    begin
      IsDecimal := False;
      Advance;
      if not CharInSet(Peek, ['0'..'9', 'a'..'f', 'A'..'F']) then
        raise TGocciaLexerError.Create('Invalid hexadecimal number', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestHexNumberFormat);
      ConsumeDigitsWithSeparators(['0'..'9', 'a'..'f', 'A'..'F']);
    end
    else if (Ch = 'b') or (Ch = 'B') then
    begin
      IsDecimal := False;
      Advance;
      if not CharInSet(Peek, ['0', '1']) then
        raise TGocciaLexerError.Create('Invalid binary number', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestBinaryNumberFormat);
      ConsumeDigitsWithSeparators(['0', '1']);
    end
    else if (Ch = 'o') or (Ch = 'O') then
    begin
      IsDecimal := False;
      Advance;
      if not CharInSet(Peek, ['0'..'7']) then
        raise TGocciaLexerError.Create('Invalid octal number', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestOctalNumberFormat);
      ConsumeDigitsWithSeparators(['0'..'7']);
    end
    else
    begin
      // ES2021 §12.9.3: numeric separators are not allowed after a leading 0
      // in DecimalIntegerLiteral (0 is a standalone production).
      if Peek = '_' then
        raise TGocciaLexerError.Create('Numeric separator cannot be used after leading 0',
          FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
      while CharInSet(Peek, ['0'..'9']) do
        Advance;
    end;
  end
  else
    ConsumeDigitsWithSeparators(['0'..'9']);

  // Fraction, trailing dot, and exponent are only valid for decimal literals.
  // Hex (0x), binary (0b), and octal (0o) literals must not consume a
  // following dot — it belongs to the property-access tokeniser.
  if IsDecimal then
  begin
    if (Peek = '.') and (PeekNext = '_') then
      raise TGocciaLexerError.Create('Numeric separator must be between digits',
        FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator)
    else if (Peek = '.') and CharInSet(PeekNext, ['0'..'9']) then
    begin
      HasDecimalOrExponent := True;
      Advance;
      ConsumeDigitsWithSeparators(['0'..'9']);
    end
    else if Peek = '.' then
    begin
      // ES2026 §12.9.3: trailing dot with no fractional digits is a valid
      // DecimalLiteral (e.g. `10.`).  This enables `10..toString(16)` by
      // tokenising `10.` as a numeric literal, leaving the second dot for
      // the property-access tokeniser.
      HasDecimalOrExponent := True;
      Advance;
    end;

    if CharInSet(Peek, ['e', 'E']) then
    begin
      HasDecimalOrExponent := True;
      Advance;
      if CharInSet(Peek, ['+', '-']) then
        Advance;
      if not CharInSet(Peek, ['0'..'9']) then
        raise TGocciaLexerError.Create('Invalid scientific notation', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestScientificNotation);
      ConsumeDigitsWithSeparators(['0'..'9']);
    end;
  end;

  // ES2026 §12.9.3 BigIntLiteralSuffix: check for 'n' suffix
  if (Peek = 'n') and (not HasDecimalOrExponent) then
  begin
    Advance;
    Lexeme := Copy(FSource, FStart, FCurrent - FStart - 1); // Exclude 'n'
    if HasSeparator then
      Lexeme := StringReplace(Lexeme, '_', '', [rfReplaceAll]);
    AddToken(gttBigInt, Lexeme);
  end
  else
  begin
    Lexeme := Copy(FSource, FStart, FCurrent - FStart);
    if HasSeparator then
      Lexeme := StringReplace(Lexeme, '_', '', [rfReplaceAll]);
    AddToken(gttNumber, Lexeme);
  end;
end;

function TGocciaLexer.IsSourceIdentifierStartCodePoint(
  ACodePoint: Cardinal): Boolean;
begin
  if ACodePoint = $180E then
    Exit(False);

  if IsECMAScriptWhitespaceCodePoint(ACodePoint) then
    Exit(False);

  // GocciaScript intentionally keeps its historical non-ASCII identifier
  // extension, including emoji identifiers, while using the standard helper
  // for ordinary Unicode identifier code points.
  Result := IsIdentifierStartCodePoint(ACodePoint) or (ACodePoint > $7F);
end;

function TGocciaLexer.IsSourceIdentifierPartCodePoint(
  ACodePoint: Cardinal): Boolean;
begin
  if ACodePoint = $180E then
    Exit(False);

  if IsECMAScriptWhitespaceCodePoint(ACodePoint) then
    Exit(False);

  Result := IsIdentifierPartCodePoint(ACodePoint) or (ACodePoint > $7F);
end;

function TGocciaLexer.TryReadIdentifierCodePoint(out ACodePoint: Cardinal;
  out AText: string; out AByteLength: Integer): Boolean;
begin
  Result := False;
  ACodePoint := 0;
  AText := '';
  AByteLength := 0;

  if IsAtEnd then
    Exit;

  if Ord(FSource[FCurrent]) <= $7F then
  begin
    ACodePoint := Ord(FSource[FCurrent]);
    AText := FSource[FCurrent];
    AByteLength := 1;
    Exit(True);
  end;

  Result := TryReadUTF8CodePoint(FSource, FCurrent, ACodePoint, AByteLength);
  if Result then
    AText := Copy(FSource, FCurrent, AByteLength);
end;

function TGocciaLexer.IsValidEscapedIdentifierText(const AText: string;
  const AAtStart: Boolean): Boolean;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
begin
  if AText = '' then
    Exit(False);

  if not TryReadUTF8CodePoint(AText, 1, CodePoint, ByteLength) or
     (ByteLength <> Length(AText)) then
    Exit(False);

  // ES2026 §12.7.1.1 keeps escaped IdentifierStart/IdentifierPart on the
  // standard Unicode ID_Start/ID_Continue path, even though raw source text
  // also accepts GocciaScript's historical non-ASCII identifier extension.
  if AAtStart then
    Result := IsIdentifierStartCodePoint(CodePoint)
  else
    Result := IsIdentifierPartCodePoint(CodePoint);
end;

procedure TGocciaLexer.ScanIdentifier;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  IdentifierText: string;
  SB: TStringBuffer;
  EscapedText: string;
  Text: string;
  HadEscape: Boolean;
  TokenType: TGocciaTokenType;
begin
  SB := TStringBuffer.Create(32);
  HadEscape := False;
  while not IsAtEnd and not IsLineTerminator do
  begin
    if (Peek = '\') and (PeekNext = 'u') then
    begin
      Advance;
      Advance;
      EscapedText := ScanUnicodeEscape;
      if not IsValidEscapedIdentifierText(EscapedText, SB.Length = 0) then
        raise TGocciaLexerError.Create('Invalid identifier escape', FLine,
          FColumn, FFileName, GetSourceLines, SSuggestInvalidCharacter);
      SB.Append(EscapedText);
      HadEscape := True;
    end
    else if TryReadIdentifierCodePoint(CodePoint, IdentifierText,
       ByteLength) and IsSourceIdentifierPartCodePoint(CodePoint) then
    begin
      SB.Append(IdentifierText);
      Inc(FCurrent, ByteLength);
      Inc(FColumn, ByteLength);
    end
    else
      Break;
  end;

  Text := SB.ToString;

  if (not HadEscape) and TryKeywordToken(Text, TokenType) then
    AddToken(TokenType, Text)
  else
    AddToken(gttIdentifier, Text, HadEscape);
end;

function LexicalGoalAllowsRegularExpression(
  const ALexicalGoal: TGocciaLexicalGoal): Boolean; inline;
begin
  Result := ALexicalGoal in [
    glgInputElementRegExp,
    glgInputElementRegExpOrTemplateTail,
    glgInputElementHashbangOrRegExp
  ];
end;

function LexicalGoalRequiresTemplateContinuation(
  const ALexicalGoal: TGocciaLexicalGoal): Boolean; inline;
begin
  Result := ALexicalGoal = glgInputElementTemplateTail;
end;

procedure TGocciaLexer.ScanToken(const ALexicalGoal: TGocciaLexicalGoal);
var
  ByteLength: Integer;
  C: Char;
  CodePoint: Cardinal;
  IdentifierText: string;
begin
  FStart := FCurrent;
  FStartColumn := FColumn;
  C := Advance;

  case C of
    '(': AddToken(gttLeftParen);
    ')': AddToken(gttRightParen);
    '{': AddToken(gttLeftBrace);
    '}': AddToken(gttRightBrace);
    '[': AddToken(gttLeftBracket);
    ']': AddToken(gttRightBracket);
    ',': AddToken(gttComma);
    ';': AddToken(gttSemicolon);
    '?':
      if Match('?') then
      begin
        if Match('=') then
          AddToken(gttNullishCoalescingAssign)
        else
          AddToken(gttNullishCoalescing);
      end
      else if (Peek = '.') and not CharInSet(PeekNext, ['0'..'9']) then
      begin
        Advance;
        AddToken(gttOptionalChaining)
      end
      else
        AddToken(gttQuestion);
    ':': AddToken(gttColon);
    '+':
      if Match('=') then
        AddToken(gttPlusAssign)
      else if Match('+') then
        AddToken(gttIncrement)
      else
        AddToken(gttPlus);
    '-':
      if Match('=') then
        AddToken(gttMinusAssign)
      else if Match('-') then
        AddToken(gttDecrement)
      else
        AddToken(gttMinus);
    '*':
      if Match('*') then
      begin
        if Match('=') then
          AddToken(gttPowerAssign)
        else
          AddToken(gttPower);
      end
      else if Match('=') then
        AddToken(gttStarAssign)
      else
        AddToken(gttStar);
    '/':
      if LexicalGoalAllowsRegularExpression(ALexicalGoal) then
        ScanRegexLiteral
      else if Match('=') then
        AddToken(gttSlashAssign)
      else
        AddToken(gttSlash);
    '%':
      if Match('=') then
        AddToken(gttPercentAssign)
      else
        AddToken(gttPercent);
    '!':
      if Match('=') then
      begin
        if Match('=') then
          AddToken(gttNotEqual)
        else
          AddToken(gttLooseNotEqual);
      end
      else
        AddToken(gttNot);
    '=':
      if Match('=') then
      begin
        if Match('=') then
          AddToken(gttEqual)
        else
          AddToken(gttLooseEqual);
      end
      else if Match('>') then
        AddToken(gttArrow)
      else
        AddToken(gttAssign);
    '<':
      if Match('<') then
      begin
        if Match('=') then
          AddToken(gttLeftShiftAssign)
        else
          AddToken(gttLeftShift);
      end
      else if Match('=') then
        AddToken(gttLessEqual)
      else
        AddToken(gttLess);
    '>':
      if Match('>') then
      begin
        if Match('>') then
        begin
          if Match('=') then
            AddToken(gttUnsignedRightShiftAssign)
          else
            AddToken(gttUnsignedRightShift);
        end
        else if Match('=') then
          AddToken(gttRightShiftAssign)
        else
          AddToken(gttRightShift);
      end
      else if Match('=') then
        AddToken(gttGreaterEqual)
      else
        AddToken(gttGreater);
    '&':
      if Match('&') then
      begin
        if Match('=') then
          AddToken(gttLogicalAndAssign)
        else
          AddToken(gttAnd);
      end
      else if Match('=') then
        AddToken(gttBitwiseAndAssign)
      else
        AddToken(gttBitwiseAnd);
    '|':
      if Match('|') then
      begin
        if Match('=') then
          AddToken(gttLogicalOrAssign)
        else
          AddToken(gttOr);
      end
      else if Match('=') then
        AddToken(gttBitwiseOrAssign)
      else
        AddToken(gttBitwiseOr);
    '^':
      if Match('=') then
        AddToken(gttBitwiseXorAssign)
      else
        AddToken(gttBitwiseXor);
    '~':
      AddToken(gttBitwiseNot);
    '.':
      if (Peek = '.') and (PeekNext = '.') then
      begin
        Advance; // consume second dot
        Advance; // consume third dot
        AddToken(gttSpread);
      end
      else if CharInSet(Peek, ['0'..'9']) then
      begin
        Dec(FCurrent);
        Dec(FColumn);
        ScanNumber;
      end
      else
        AddToken(gttDot);
    '#': AddToken(gttHash);
    '@': AddToken(gttAt);
    '`':
      ScanTemplateStart;
    '''', '"':
      ScanString;
  else
    if CharInSet(C, ['0'..'9']) then
    begin
      Dec(FCurrent);
      Dec(FColumn);
      ScanNumber;
    end
    else if (C = '\') and (Peek = 'u') then
    begin
      Dec(FCurrent);
      Dec(FColumn);
      ScanIdentifier;
    end
    else
    begin
      Dec(FCurrent);
      Dec(FColumn);
      if TryReadIdentifierCodePoint(CodePoint, IdentifierText, ByteLength) and
         IsSourceIdentifierStartCodePoint(CodePoint) then
        ScanIdentifier
      else
        raise TGocciaLexerError.Create(Format('Unexpected character: %s', [C]),
          FLine, FStartColumn, FFileName, GetSourceLines,
          SSuggestInvalidCharacter);
    end
  end;
end;

function TGocciaLexer.ScanNextToken(
  const ALexicalGoal: TGocciaLexicalGoal): TGocciaToken;
var
  StartTime: Int64;
begin
  StartTime := GetNanoseconds;
  try
    if FEOFEmitted then
      Exit(FTokens[FTokens.Count - 1]);

    if not FHashbangSkipped then
    begin
      if ALexicalGoal = glgInputElementHashbangOrRegExp then
        SkipHashbang;
      FHashbangSkipped := True;
    end;

    if LexicalGoalRequiresTemplateContinuation(ALexicalGoal) then
    begin
      ScanTemplateContinuation;
      Result := FTokens[FTokens.Count - 1];
      Exit;
    end;

    SkipWhitespace;
    if IsAtEnd then
    begin
      FEOFEmitted := True;
      FTokens.Add(TGocciaToken.Create(gttEOF, '', FLine, FColumn, FColumn));
      Exit(FTokens[FTokens.Count - 1]);
    end;

    ScanToken(ALexicalGoal);
    Result := FTokens[FTokens.Count - 1];
  finally
    Inc(FScanTimeNanoseconds, GetNanoseconds - StartTime);
  end;
end;

end.
