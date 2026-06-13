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
    FCanStartRegex: Boolean;
    FParenRegexContext: array of Boolean;
    FParenRegexContextCount: Integer;
    FHashbangSkipped: Boolean;
    FEOFEmitted: Boolean;
    FLegacyRegexContextEnabled: Boolean;
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
    procedure AppendCurrent(var ASB, ARawSB: TStringBuffer); inline;
    procedure AddToken(const ATokenType: TGocciaTokenType); overload;
    procedure AddToken(const ATokenType: TGocciaTokenType; const ALiteral: string;
      const AContainsEscape: Boolean = False); overload;
    procedure ScanToken(const ACanStartRegex: Boolean);
    procedure ScanString;
    procedure ScanTemplate;
    procedure ScanRegexLiteral;
    procedure ScanNumber;
    procedure ScanIdentifier;
    function CanScanRegexAfterAwait: Boolean;
    function ScanUnicodeEscape: string;
    function ScanHexEscape: string;
    procedure ProcessEscapeSequence(var ASB: TStringBuffer);
    // TC39 Template Literal Revision — template-aware variants that return
    // False on malformed escapes instead of raising, allowing tagged templates
    // to set cooked=undefined while preserving the raw source text.
    function ScanUnicodeEscapeForTemplate(var ASB: TStringBuffer): Boolean;
    function ScanHexEscapeForTemplate(var ASB: TStringBuffer): Boolean;
    procedure ProcessTemplateEscapeSequence(var ASB: TStringBuffer; var ASegmentValid: Boolean);
    // Lexically-aware ${...} boundary detection for template literals.
    // ScanInterpolationExpression scans an expression body after ${ has been
    // consumed, tracking braces, strings, comments, and nested templates.
    // Stops without consuming the matching }.
    procedure ScanInterpolationExpression(var ASB, ARawSB: TStringBuffer);
    // Scans a nested template literal body within an interpolation expression.
    // Called after the opening backtick has been consumed and appended.
    // Handles \`, \$, and nested ${...} via mutual recursion.
    procedure ScanNestedTemplateInExpression(var ASB, ARawSB: TStringBuffer);
    procedure UpdateRegexContext(const ATokenType: TGocciaTokenType);
    procedure PushParenRegexContext(const AAllowsRegexAfter: Boolean);
    function PopParenRegexContext: Boolean;
    procedure SkipWhitespace;
    procedure SkipHashbang;
    procedure SkipComment;
    procedure SkipBlockComment;
    procedure SkipUntilLineTerminator;
    function IsLineTerminator: Boolean; inline;
    function IsUnicodeLineTerminator: Boolean; inline;
    function ConsumeLineTerminator: Boolean; inline;
    function AppendLineTerminator(var ASB, ARawSB: TStringBuffer): Boolean; inline;
    procedure ConsumeUnicodeLineTerminator; inline;
  public
    constructor Create(const ASource, AFileName: string);
    destructor Destroy; override;
    function ScanNextToken(const ACanStartRegex: Boolean): TGocciaToken;
    function ScanTokens: TObjectList<TGocciaToken>;
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
  UTF8_NO_BREAK_SPACE_LEAD_BYTE = #$C2;
  UTF8_NO_BREAK_SPACE_FINAL_BYTE = #$A0;
  NO_BREAK_SPACE_CODE_POINT = $00A0;
  ZERO_WIDTH_NO_BREAK_SPACE_CODE_POINT = $FEFF;

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
  FCanStartRegex := True;
  FParenRegexContextCount := 0;
  FHashbangSkipped := False;
  FEOFEmitted := False;
  FLegacyRegexContextEnabled := False;
  FScanTimeNanoseconds := 0;
end;

destructor TGocciaLexer.Destroy;
begin
  FTokens.Free;
  FSourceLines.Free;
  inherited;
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

procedure TGocciaLexer.AppendCurrent(var ASB, ARawSB: TStringBuffer); inline;
var
  C: Char;
begin
  C := Advance;
  ASB.AppendChar(C);
  ARawSB.AppendChar(C);
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
  if FLegacyRegexContextEnabled then
    UpdateRegexContext(ATokenType);
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

procedure TGocciaLexer.SkipWhitespace;
begin
  while not IsAtEnd do
  begin
    case Peek of
      ' ', #9, #11, #12:
        Advance;
      // ES2026 §12.2 White Space — NBSP (U+00A0)
      UTF8_NO_BREAK_SPACE_LEAD_BYTE:
        if (FCurrent < Length(FSource)) and
           (FSource[FCurrent + 1] = UTF8_NO_BREAK_SPACE_FINAL_BYTE) then
        begin
          Inc(FCurrent, 2);
          Inc(FColumn, 2);
        end
        else
          Break;
      // ES2026 §12.3 LineTerminator — CR and CRLF
      #13, #10:
        ConsumeLineTerminator;
      // ES2026 §12.3 Line Terminators — LS (U+2028) and PS (U+2029)
      UTF8_LINE_TERMINATOR_LEAD_BYTE:
        if not ConsumeLineTerminator then
          Break;
      '/':
        if PeekNext = '/' then
          SkipComment
        else if PeekNext = '*' then
          SkipBlockComment
        else
          Break;
    else
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
  CodePointValue: QWord;
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
    if not TryStrToQWord('$' + HexStr, CodePointValue) or
       (CodePointValue > $10FFFF) then
      raise TGocciaLexerError.Create('Invalid unicode code point', FLine, FColumn, FFileName, GetSourceLines,
        SSuggestUnicodeCodePointRange);
    CodePoint := Cardinal(CodePointValue);
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

  if CodePoint > $10FFFF then
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
  Result := Chr(CodePoint);
end;

procedure TGocciaLexer.ProcessEscapeSequence(var ASB: TStringBuffer);
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
    '0': begin ASB.AppendChar(#0); Advance; end;
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
  CodePointValue: QWord;
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
    // Use TryStrToQWord to safely handle arbitrarily long hex payloads
    // without raising on overflow (e.g. \u{FFFFFFFF}).
    if not TryStrToQWord('$' + HexStr, CodePointValue) or
       (CodePointValue > $10FFFF) then
      Exit(False);
    CodePoint := Cardinal(CodePointValue);
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

  if CodePoint > $10FFFF then
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
  ASB.AppendChar(Chr(CodePoint));
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

// Scans a nested template literal body within an interpolation expression.
// Called after the opening backtick has been consumed and appended to the
// buffers.  Consumes characters up to and including the closing backtick,
// appending all content to both ASB and ARawSB verbatim.  Handles escaped
// backticks (\`), escaped dollars (\$), and nested ${...} interpolations
// via mutual recursion with ScanInterpolationExpression.
procedure TGocciaLexer.ScanNestedTemplateInExpression(var ASB, ARawSB: TStringBuffer);
var
  C: Char;
begin
  while not IsAtEnd do
  begin
    C := Peek;
    if ((C = #13) or (C = #10) or (C = UTF8_LINE_TERMINATOR_LEAD_BYTE)) and
       AppendLineTerminator(ASB, ARawSB) then
      Continue;

    case C of
      '`':
      begin
        // Closing backtick of nested template
        AppendCurrent(ASB, ARawSB);
        Exit;
      end;
      '\':
      begin
        // Escape sequence — consume \ and next char verbatim
        AppendCurrent(ASB, ARawSB);
        if not IsAtEnd then
          AppendCurrent(ASB, ARawSB);
      end;
      '$':
      begin
        if PeekNext = '{' then
        begin
          // Nested interpolation within nested template
          AppendCurrent(ASB, ARawSB); // consume $
          AppendCurrent(ASB, ARawSB); // consume {
          ScanInterpolationExpression(ASB, ARawSB);
          // ScanInterpolationExpression leaves the closing } unconsumed
          if not IsAtEnd then
            AppendCurrent(ASB, ARawSB); // consume }
        end
        else
          AppendCurrent(ASB, ARawSB);
      end;
    else
      AppendCurrent(ASB, ARawSB);
    end;
  end;
end;

// ES2026 §12.9.4 Template Literal Lexical Components
//
// Scans a template interpolation expression with full lexical awareness.
// Called after ${ has been consumed.  Tracks brace depth starting at 1 and
// stops when the matching } is found WITHOUT consuming it — the caller is
// responsible for consuming the closing }.  All characters within the
// expression body are appended verbatim to both ASB and ARawSB.
//
// Lexical constructs handled:
//   - Nested braces {…}
//   - String literals '…' and "…" (with \ escapes)
//   - Template literals `…` (with nested ${…} via mutual recursion)
//   - Regex literals /…/ (with [\ ] classes and \ escapes), so quotes and
//     braces inside a pattern cannot flip the string/brace tracking
//   - Line comments //…
//   - Block comments /*…*/
//   - Line terminators (CR, CRLF, LF, LS, PS) with line/column tracking
//
// Regex-vs-division uses the same classification as UpdateRegexContext: a
// slash after a completed primary expression (identifier, literal, `]`,
// closing quote/backtick, postfix ++/--) divides; everywhere else it starts
// a regex.  The scanner tracks the last significant character and the last
// word so keyword boundaries (`return /re/`) classify like the main lexer.
procedure TGocciaLexer.ScanInterpolationExpression(var ASB, ARawSB: TStringBuffer);
var
  BraceCount: Integer;
  C, Quote: Char;
  LastSignificant, PrevSignificant: Char;
  LastWord: string;
  WordActive: Boolean;
  // Mirrors the main lexer's paren-regex context: each '(' records whether
  // it opens an if/while/for head, so the matching ')' can re-allow a regex.
  ParenAllowsRegex: array of Boolean;
  ParenDepth: Integer;
  LastParenAllowsRegex: Boolean;

  procedure PushParenContext(const AAllowsRegexAfter: Boolean);
  begin
    if ParenDepth >= Length(ParenAllowsRegex) then
      SetLength(ParenAllowsRegex, ParenDepth * 2 + 8);
    ParenAllowsRegex[ParenDepth] := AAllowsRegexAfter;
    Inc(ParenDepth);
  end;

  function PopParenContext: Boolean;
  begin
    if ParenDepth = 0 then
      Exit(False);
    Dec(ParenDepth);
    Result := ParenAllowsRegex[ParenDepth];
  end;

  function WordIsStatementHeadKeyword: Boolean;
  var
    KeywordType: TGocciaTokenType;
  begin
    Result := (LastSignificant in ['a'..'z', 'A'..'Z']) and
      TryKeywordToken(LastWord, KeywordType) and
      (KeywordType in [gttIf, gttWhile, gttFor]);
  end;

  procedure NoteSignificant(const AChar: Char);
  begin
    PrevSignificant := LastSignificant;
    LastSignificant := AChar;
    if (AChar in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) or
       (AChar > #127) then
    begin
      if WordActive then
        LastWord := LastWord + AChar
      else
        LastWord := AChar;
      WordActive := True;
    end
    else
      WordActive := False;
  end;

  function SlashStartsRegex: Boolean;
  var
    KeywordType: TGocciaTokenType;
  begin
    if LastSignificant = #0 then
      Exit(True); // expression start
    if (LastSignificant in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) or
       (LastSignificant > #127) then
    begin
      // Mirror UpdateRegexContext: keywords allow a regex except the
      // identifier-like ones that end a primary expression.
      if TryKeywordToken(LastWord, KeywordType) then
        Exit(not (KeywordType in [gttTrue, gttFalse, gttNull, gttAs,
          gttFrom, gttStatic, gttThis, gttSuper]));
      Exit(False);
    end;
    // A trailing-dot numeric literal (`1.`) is one Number token in the real
    // lexer, so a slash after it divides; a bare member-access dot keeps the
    // regex-allowed default the main lexer uses for gttDot.
    if (LastSignificant = '.') and (PrevSignificant in ['0'..'9']) then
      Exit(False);
    if (LastSignificant in ['+', '-']) and
       (PrevSignificant = LastSignificant) then
      Exit(False); // postfix ++/-- ends a primary expression
    // Mirror PushParenRegexContext: the ')' closing an if/while/for head
    // allows a regex statement to follow; any other ')' ends a primary
    // expression and divides.
    if LastSignificant = ')' then
      Exit(LastParenAllowsRegex);
    Result := not (LastSignificant in [']', '''', '"', '`']);
  end;

  // Pure lookahead from the character after the already-consumed '/': does a
  // regex literal body (with \ escapes and [..] classes) close with '/' on
  // this line?  Misclassified division would otherwise swallow state-bearing
  // characters ('}', quotes) up to end of line; when no same-line closing
  // slash exists we keep the slash as division and degrade gracefully.
  function RegexBodyClosesOnLine: Boolean;
  var
    Idx: Integer;
    InClass: Boolean;
  begin
    Result := False;
    InClass := False;
    Idx := FCurrent;
    while Idx <= Length(FSource) do
    begin
      case FSource[Idx] of
        #10, #13:
          Exit(False);
        UTF8_LINE_TERMINATOR_LEAD_BYTE:
          if (Idx + 2 <= Length(FSource)) and
             (FSource[Idx + 1] = UTF8_LINE_TERMINATOR_CONTINUATION_BYTE) and
             ((FSource[Idx + 2] = UTF8_LINE_SEPARATOR_FINAL_BYTE) or
              (FSource[Idx + 2] = UTF8_PARAGRAPH_SEPARATOR_FINAL_BYTE)) then
            Exit(False);
        '\':
          Inc(Idx); // skip the escaped character
        '[':
          InClass := True;
        ']':
          InClass := False;
        '/':
          if not InClass then
            Exit(True);
      end;
      Inc(Idx);
    end;
  end;

  // Called with the opening / already appended.  Consumes the regex body
  // verbatim.  Bails at a line terminator: regex literals cannot span
  // lines, so a misclassified division degrades to ordinary character
  // scanning instead of swallowing the rest of the source.
  procedure ScanRegexBody;
  var
    R: Char;
    InClass: Boolean;
  begin
    InClass := False;
    while not IsAtEnd and not IsLineTerminator do
    begin
      R := Peek;
      if R = '\' then
      begin
        AppendCurrent(ASB, ARawSB);
        if not IsAtEnd and not IsLineTerminator then
          AppendCurrent(ASB, ARawSB);
        Continue;
      end;
      if InClass then
      begin
        if R = ']' then
          InClass := False;
        AppendCurrent(ASB, ARawSB);
        Continue;
      end;
      if R = '[' then
      begin
        InClass := True;
        AppendCurrent(ASB, ARawSB);
        Continue;
      end;
      AppendCurrent(ASB, ARawSB);
      if R = '/' then
        Exit; // closing delimiter; flags are scanned by the main loop
    end;
  end;

begin
  BraceCount := 1;
  LastSignificant := #0;
  PrevSignificant := #0;
  LastWord := '';
  WordActive := False;
  ParenDepth := 0;
  LastParenAllowsRegex := False;
  while (BraceCount > 0) and not IsAtEnd do
  begin
    C := Peek;
    if ((C = #13) or (C = #10) or (C = UTF8_LINE_TERMINATOR_LEAD_BYTE)) and
       AppendLineTerminator(ASB, ARawSB) then
    begin
      WordActive := False;
      Continue;
    end;

    case C of
      '{':
      begin
        Inc(BraceCount);
        AppendCurrent(ASB, ARawSB);
        NoteSignificant('{');
      end;
      '}':
      begin
        Dec(BraceCount);
        if BraceCount > 0 then
        begin
          AppendCurrent(ASB, ARawSB);
          NoteSignificant('}');
        end;
        // When BraceCount = 0, leave the closing } unconsumed for the caller
      end;
      '''', '"':
      begin
        // String literal — scan until matching unescaped quote
        Quote := C;
        AppendCurrent(ASB, ARawSB);
        while not IsAtEnd do
        begin
          C := Peek;
          if C = '\' then
          begin
            // Escape — consume \ and the next character verbatim
            AppendCurrent(ASB, ARawSB);
            if not IsAtEnd then
              AppendCurrent(ASB, ARawSB);
          end
          else if C = Quote then
          begin
            AppendCurrent(ASB, ARawSB);
            Break;
          end
          else if ((C = #13) or (C = #10) or
                   (C = UTF8_LINE_TERMINATOR_LEAD_BYTE)) and
                  AppendLineTerminator(ASB, ARawSB) then
            Continue
          else
            AppendCurrent(ASB, ARawSB);
        end;
        NoteSignificant(Quote);
      end;
      '`':
      begin
        // Nested template literal — delegate to mutual recursion
        AppendCurrent(ASB, ARawSB);
        ScanNestedTemplateInExpression(ASB, ARawSB);
        NoteSignificant('`');
      end;
      '/':
      begin
        AppendCurrent(ASB, ARawSB);
        if not IsAtEnd then
        begin
          if Peek = '/' then
          begin
            // Line comment — consume until line terminator (LF, CR, LS, PS)
            AppendCurrent(ASB, ARawSB);
            while not IsAtEnd and not IsLineTerminator do
              AppendCurrent(ASB, ARawSB);
          end
          else if Peek = '*' then
          begin
            // Block comment — consume until */
            AppendCurrent(ASB, ARawSB); // consume *
            while not IsAtEnd do
            begin
              C := Peek;
              if C = '*' then
              begin
                AppendCurrent(ASB, ARawSB);
                if not IsAtEnd and (Peek = '/') then
                begin
                  AppendCurrent(ASB, ARawSB);
                  Break;
                end;
              end
              else if ((C = #13) or (C = #10) or
                       (C = UTF8_LINE_TERMINATOR_LEAD_BYTE)) and
                      AppendLineTerminator(ASB, ARawSB) then
                Continue
              else
                AppendCurrent(ASB, ARawSB);
            end;
          end
          else if SlashStartsRegex and RegexBodyClosesOnLine then
          begin
            ScanRegexBody;
            // A regex is a completed primary expression; ')' marks that, and
            // clearing the paren flag keeps a following slash as division.
            LastParenAllowsRegex := False;
            NoteSignificant(')');
          end
          else
            NoteSignificant('/');
        end
        else
          NoteSignificant('/');
      end;
    else
      begin
        AppendCurrent(ASB, ARawSB);
        // Capture the paren-regex context before NoteSignificant resets the
        // word state: '(' records whether it opens an if/while/for head.
        if C = '(' then
          PushParenContext(WordIsStatementHeadKeyword)
        else if C = ')' then
          LastParenAllowsRegex := PopParenContext;
        if C > ' ' then
          NoteSignificant(C)
        else
          WordActive := False;
      end;
    end;
  end;
end;

procedure TGocciaLexer.PushParenRegexContext(const AAllowsRegexAfter: Boolean);
begin
  if FParenRegexContextCount >= Length(FParenRegexContext) then
    SetLength(FParenRegexContext, FParenRegexContextCount * 2 + 8);
  FParenRegexContext[FParenRegexContextCount] := AAllowsRegexAfter;
  Inc(FParenRegexContextCount);
end;

function TGocciaLexer.PopParenRegexContext: Boolean;
begin
  if FParenRegexContextCount = 0 then
    Exit(False);
  Dec(FParenRegexContextCount);
  Result := FParenRegexContext[FParenRegexContextCount];
end;

procedure TGocciaLexer.UpdateRegexContext(const ATokenType: TGocciaTokenType);
var
  PreviousTokenType: TGocciaTokenType;
begin
  case ATokenType of
    gttLeftParen:
      begin
        if FTokens.Count >= 2 then
          PreviousTokenType := FTokens[FTokens.Count - 2].TokenType
        else
          PreviousTokenType := gttEOF;
        PushParenRegexContext(PreviousTokenType in [gttIf, gttWhile, gttFor]);
        FCanStartRegex := True;
      end;
    gttRightParen:
      FCanStartRegex := PopParenRegexContext();
    gttNumber,
    gttBigInt,
    gttString,
    gttTemplate,
    gttRegex,
    gttTrue,
    gttFalse,
    gttNull,
    gttIdentifier,
    gttAs,
    gttFrom,
    gttStatic,
    gttThis,
    gttSuper,
    gttRightBracket,
    gttIncrement,
    gttDecrement:
      FCanStartRegex := False;
  else
    FCanStartRegex := True;
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

// TC39 Template Literal Revision: template scanning tolerates malformed escape
// sequences.  When an invalid \u or \x escape is encountered, the segment is
// flagged and the token separator changes from #1 to #2 so the parser can
// detect that cooked values must be re-derived per-segment from the raw string.
procedure TGocciaLexer.ScanTemplate;
const
  TEMPLATE_RAW_SEPARATOR = #1;
  TEMPLATE_INVALID_ESCAPE_SEPARATOR = #2;
  TEMPLATE_EXPRESSION_BOUNDARY = #3;
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
      // Interpolation expression boundary — detect and record with lexical
      // awareness so braces inside strings, comments, and nested templates
      // do not confuse the boundary detection.
      Advance; // consume $
      Advance; // consume {
      SB.AppendChar(TEMPLATE_EXPRESSION_BOUNDARY);
      RawSB.AppendChar(TEMPLATE_EXPRESSION_BOUNDARY);
      ScanInterpolationExpression(SB, RawSB);
      // ScanInterpolationExpression leaves the closing } unconsumed
      if not IsAtEnd then
        Advance; // consume closing }
      SB.AppendChar(TEMPLATE_EXPRESSION_BOUNDARY);
      RawSB.AppendChar(TEMPLATE_EXPRESSION_BOUNDARY);
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
  AddToken(gttTemplate, SB.ToString + Separator + RawSB.ToString);
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

  AddToken(gttRegex, Pattern + REGEX_SEPARATOR + Flags);
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
  if (ACodePoint = NO_BREAK_SPACE_CODE_POINT) or
     (ACodePoint = ZERO_WIDTH_NO_BREAK_SPACE_CODE_POINT) then
    Exit(False);

  // GocciaScript intentionally keeps its historical non-ASCII identifier
  // extension, including emoji identifiers, while using the standard helper
  // for ordinary Unicode identifier code points.
  Result := IsIdentifierStartCodePoint(ACodePoint) or (ACodePoint > $7F);
end;

function TGocciaLexer.IsSourceIdentifierPartCodePoint(
  ACodePoint: Cardinal): Boolean;
begin
  if (ACodePoint = NO_BREAK_SPACE_CODE_POINT) or
     (ACodePoint = ZERO_WIDTH_NO_BREAK_SPACE_CODE_POINT) then
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

  if AAtStart then
    Result := IsSourceIdentifierStartCodePoint(CodePoint)
  else
    Result := IsSourceIdentifierPartCodePoint(CodePoint);
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

procedure TGocciaLexer.ScanToken(const ACanStartRegex: Boolean);
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
      else if Match('.') then
        AddToken(gttOptionalChaining)
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
      if Match('=') then
        AddToken(gttSlashAssign)
      else if ACanStartRegex or
              (FLegacyRegexContextEnabled and CanScanRegexAfterAwait) then
        ScanRegexLiteral
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
      ScanTemplate;
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

function TGocciaLexer.CanScanRegexAfterAwait: Boolean;
var
  I: Integer;
  InClass, Escaped: Boolean;
  Ch: Char;

  function IsLineTerminatorAt(const AIndex: Integer): Boolean; inline;
  begin
    Result := (FSource[AIndex] = #10) or (FSource[AIndex] = #13) or
              ((FSource[AIndex] = UTF8_LINE_TERMINATOR_LEAD_BYTE) and
              (AIndex + 2 <= Length(FSource)) and
              (FSource[AIndex + 1] = UTF8_LINE_TERMINATOR_CONTINUATION_BYTE) and
              ((FSource[AIndex + 2] = UTF8_LINE_SEPARATOR_FINAL_BYTE) or
              (FSource[AIndex + 2] = UTF8_PARAGRAPH_SEPARATOR_FINAL_BYTE)));
  end;
begin
  Result := False;
  if (FTokens.Count = 0) or
     (FTokens[FTokens.Count - 1].TokenType <> gttIdentifier) or
     (FTokens[FTokens.Count - 1].Lexeme <> KEYWORD_AWAIT) then
    Exit;

  if IsAtEnd or (Peek = '/') or (Peek = '*') then
    Exit;

  InClass := False;
  Escaped := False;
  I := FCurrent;
  while I <= Length(FSource) do
  begin
    Ch := FSource[I];
    if IsLineTerminatorAt(I) then
      Exit;

    if Escaped then
    begin
      Escaped := False;
      Inc(I);
      Continue;
    end;

    case Ch of
      '\':
        Escaped := True;
      '[':
        InClass := True;
      ']':
        InClass := False;
      '/':
        if not InClass then
          Exit(True);
    end;

    Inc(I);
  end;
end;

function TGocciaLexer.ScanNextToken(const ACanStartRegex: Boolean): TGocciaToken;
var
  StartTime: Int64;
begin
  StartTime := GetNanoseconds;
  try
    if FEOFEmitted then
      Exit(FTokens[FTokens.Count - 1]);

    if not FHashbangSkipped then
    begin
      SkipHashbang;
      FHashbangSkipped := True;
    end;

    SkipWhitespace;
    if IsAtEnd then
    begin
      FEOFEmitted := True;
      FTokens.Add(TGocciaToken.Create(gttEOF, '', FLine, FColumn, FColumn));
      Exit(FTokens[FTokens.Count - 1]);
    end;

    ScanToken(ACanStartRegex);
    Result := FTokens[FTokens.Count - 1];
  finally
    Inc(FScanTimeNanoseconds, GetNanoseconds - StartTime);
  end;
end;

function TGocciaLexer.ScanTokens: TObjectList<TGocciaToken>;
var
  Token: TGocciaToken;
begin
  FLegacyRegexContextEnabled := True;
  try
    repeat
      Token := ScanNextToken(FCanStartRegex);
    until Token.TokenType = gttEOF;
  finally
    FLegacyRegexContextEnabled := False;
  end;
  Result := FTokens;
end;

end.
