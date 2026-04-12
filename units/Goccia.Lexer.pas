unit Goccia.Lexer;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  OrderedStringMap,
  StringBuffer,

  Goccia.Token;

type
  TGocciaLexer = class
  const ValidIdentifierChars: set of Char = ['a'..'z', 'A'..'Z', '0'..'9', '_', '$'];
  private class var
    FKeywords: TOrderedStringMap<TGocciaTokenType>;
    class procedure InitKeywords;
    class destructor DestroyClass;
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
    function GetSourceLines: TStringList;

    function IsAtEnd: Boolean; inline;
    function Advance: Char;
    function Peek: Char; inline;
    function PeekNext: Char; inline;
    function Match(const AExpected: Char): Boolean; inline;
    function IsValidIdentifierChar(const C: Char): Boolean;
    procedure AddToken(const ATokenType: TGocciaTokenType); overload;
    procedure AddToken(const ATokenType: TGocciaTokenType; const ALiteral: string); overload;
    procedure ScanToken;
    procedure ScanString;
    procedure ScanTemplate;
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
    function IsLineTerminator: Boolean; inline;
    function IsUnicodeLineTerminator: Boolean; inline;
    procedure ConsumeUnicodeLineTerminator;
  public
    constructor Create(const ASource, AFileName: string);
    destructor Destroy; override;
    function ScanTokens: TObjectList<TGocciaToken>;
    property SourceLines: TStringList read GetSourceLines;
  end;

implementation

uses
  Goccia.Error,
  Goccia.Error.Suggestions,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved,
  Goccia.TextFiles;

const
  // ES2026 §12.3 Line Terminators — UTF-8 byte components for LS (U+2028) and PS (U+2029)
  UTF8_LINE_TERMINATOR_LEAD_BYTE = #$E2;
  UTF8_LINE_TERMINATOR_CONTINUATION_BYTE = #$80;
  UTF8_LINE_SEPARATOR_FINAL_BYTE = #$A8;
  UTF8_PARAGRAPH_SEPARATOR_FINAL_BYTE = #$A9;
  UTF8_LINE_TERMINATOR_BYTE_LENGTH = 3;

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

constructor TGocciaLexer.Create(const ASource, AFileName: string);
begin
  InitKeywords;
  FSource := ASource;
  FFileName := AFileName;
  FTokens := TObjectList<TGocciaToken>.Create(True);
  FSourceLines := nil;
  FCurrent := 1;
  FLine := 1;
  FColumn := 1;
  FCanStartRegex := True;
  FParenRegexContextCount := 0;
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
    FSourceLines := CreateUTF8StringList(FSource);
  Result := FSourceLines;
end;

function TGocciaLexer.IsAtEnd: Boolean;
begin
  Result := FCurrent > Length(FSource);
end;

function TGocciaLexer.Advance: Char;
begin
  Result := FSource[FCurrent];
  Inc(FCurrent);
  Inc(FColumn);
end;

function TGocciaLexer.Peek: Char;
begin
  if IsAtEnd then
    Result := #0
  else
    Result := FSource[FCurrent];
end;

function TGocciaLexer.PeekNext: Char;
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

procedure TGocciaLexer.AddToken(const ATokenType: TGocciaTokenType; const ALiteral: string);
begin
  FTokens.Add(TGocciaToken.Create(ATokenType, ALiteral, FLine, FStartColumn));
  UpdateRegexContext(ATokenType);
end;

// ES2026 §12.3 LineTerminator :: <LF> | <CR> | <LS> | <PS>
function TGocciaLexer.IsLineTerminator: Boolean;
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
function TGocciaLexer.IsUnicodeLineTerminator: Boolean;
begin
  Result := (FSource[FCurrent] = UTF8_LINE_TERMINATOR_LEAD_BYTE) and
            (FCurrent + 2 <= Length(FSource)) and
            (FSource[FCurrent + 1] = UTF8_LINE_TERMINATOR_CONTINUATION_BYTE) and
            ((FSource[FCurrent + 2] = UTF8_LINE_SEPARATOR_FINAL_BYTE) or
             (FSource[FCurrent + 2] = UTF8_PARAGRAPH_SEPARATOR_FINAL_BYTE));
end;

procedure TGocciaLexer.ConsumeUnicodeLineTerminator;
begin
  Inc(FCurrent, UTF8_LINE_TERMINATOR_BYTE_LENGTH);
  Inc(FLine);
  FColumn := 1;
end;

procedure TGocciaLexer.SkipWhitespace;
begin
  while not IsAtEnd do
  begin
    case Peek of
      ' ', #9:
        Advance;
      // ES2026 §12.3 LineTerminator — CR and CRLF
      #13:
        begin
          Advance;
          if (not IsAtEnd) and (Peek = #10) then
            Advance;
          Inc(FLine);
          FColumn := 1;
        end;
      #10:
        begin
          Inc(FLine);
          FColumn := 0;
          Advance;
        end;
      // ES2026 §12.3 Line Terminators — LS (U+2028) and PS (U+2029)
      UTF8_LINE_TERMINATOR_LEAD_BYTE:
        if IsUnicodeLineTerminator then
          ConsumeUnicodeLineTerminator
        else
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

// ES2026 §12.5 HashbangComment :: #! SingleLineCommentCharsₒₚₜ
procedure TGocciaLexer.SkipHashbang;
begin
  if (FCurrent <> 1) or (Length(FSource) < 2) or (Copy(FSource, 1, 2) <> '#!') then
    Exit;

  while not IsAtEnd and not IsLineTerminator do
    Advance;
end;

// ES2026 §12.4 SingleLineComment :: // SingleLineCommentCharsₒₚₜ
procedure TGocciaLexer.SkipComment;
begin
  // Skip '//'
  Advance;
  Advance;

  while not IsAtEnd and not IsLineTerminator do
    Advance;
end;

procedure TGocciaLexer.SkipBlockComment;
begin
  // Skip '/*'
  Advance;
  Advance;

  while not IsAtEnd do
  begin
    if Peek = '*' then
    begin
      Advance;
      if Peek = '/' then
      begin
        Advance; // Skip the closing '/'
        Exit;
      end;
    end
    // ES2026 §12.3 LineTerminator — CR and CRLF
    else if Peek = #13 then
    begin
      Advance;
      if (not IsAtEnd) and (Peek = #10) then
        Advance;
      Inc(FLine);
      FColumn := 1;
    end
    else if Peek = #10 then
    begin
      Inc(FLine);
      FColumn := 0;
      Advance;
    end
    // ES2026 §12.3 Line Terminators — LS (U+2028) and PS (U+2029)
    else if (Peek = UTF8_LINE_TERMINATOR_LEAD_BYTE) and IsUnicodeLineTerminator then
      ConsumeUnicodeLineTerminator
    else
      Advance;
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
  end;

  CodePoint := StrToInt('$' + HexStr);

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

  // Convert code point to UTF-8
  if CodePoint <= $7F then
    Result := Chr(CodePoint)
  else if CodePoint <= $7FF then
    Result := Chr($C0 or (CodePoint shr 6)) + Chr($80 or (CodePoint and $3F))
  else if CodePoint <= $FFFF then
    Result := Chr($E0 or (CodePoint shr 12)) + Chr($80 or ((CodePoint shr 6) and $3F)) + Chr($80 or (CodePoint and $3F))
  else if CodePoint <= $10FFFF then
    Result := Chr($F0 or (CodePoint shr 18)) + Chr($80 or ((CodePoint shr 12) and $3F)) + Chr($80 or ((CodePoint shr 6) and $3F)) + Chr($80 or (CodePoint and $3F))
  else
    raise TGocciaLexerError.Create('Invalid unicode code point', FLine, FColumn, FFileName, GetSourceLines,
      SSuggestUnicodeCodePointRange);
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
  case Peek of
    'n': begin ASB.AppendChar(#10); Advance; end;
    'r': begin ASB.AppendChar(#13); Advance; end;
    't': begin ASB.AppendChar(#9); Advance; end;
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

  // Convert code point to UTF-8
  if CodePoint <= $7F then
    ASB.AppendChar(Chr(CodePoint))
  else if CodePoint <= $7FF then
  begin
    ASB.AppendChar(Chr($C0 or (CodePoint shr 6)));
    ASB.AppendChar(Chr($80 or (CodePoint and $3F)));
  end
  else if CodePoint <= $FFFF then
  begin
    ASB.AppendChar(Chr($E0 or (CodePoint shr 12)));
    ASB.AppendChar(Chr($80 or ((CodePoint shr 6) and $3F)));
    ASB.AppendChar(Chr($80 or (CodePoint and $3F)));
  end
  else if CodePoint <= $10FFFF then
  begin
    ASB.AppendChar(Chr($F0 or (CodePoint shr 18)));
    ASB.AppendChar(Chr($80 or ((CodePoint shr 12) and $3F)));
    ASB.AppendChar(Chr($80 or ((CodePoint shr 6) and $3F)));
    ASB.AppendChar(Chr($80 or (CodePoint and $3F)));
  end
  else
    Exit(False);
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
  J: Integer;
begin
  while not IsAtEnd do
  begin
    C := Peek;
    case C of
      '`':
      begin
        // Closing backtick of nested template
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
        Exit;
      end;
      '\':
      begin
        // Escape sequence — consume \ and next char verbatim
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
        if not IsAtEnd then
        begin
          C := Advance;
          ASB.AppendChar(C);
          ARawSB.AppendChar(C);
        end;
      end;
      '$':
      begin
        if PeekNext = '{' then
        begin
          // Nested interpolation within nested template
          Advance; // consume $
          ASB.AppendChar('$');
          ARawSB.AppendChar('$');
          Advance; // consume {
          ASB.AppendChar('{');
          ARawSB.AppendChar('{');
          ScanInterpolationExpression(ASB, ARawSB);
          // ScanInterpolationExpression leaves the closing } unconsumed
          if not IsAtEnd then
          begin
            Advance; // consume }
            ASB.AppendChar('}');
            ARawSB.AppendChar('}');
          end;
        end
        else
        begin
          Advance;
          ASB.AppendChar(C);
          ARawSB.AppendChar(C);
        end;
      end;
      #13:
      begin
        Advance;
        if Peek = #10 then
          Advance;
        Inc(FLine);
        FColumn := 1;
        ASB.AppendChar(#10);
        ARawSB.AppendChar(#10);
      end;
      #10:
      begin
        Inc(FLine);
        FColumn := 0;
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
      end;
      UTF8_LINE_TERMINATOR_LEAD_BYTE:
      begin
        if IsUnicodeLineTerminator then
        begin
          for J := 0 to UTF8_LINE_TERMINATOR_BYTE_LENGTH - 1 do
          begin
            C := FSource[FCurrent + J];
            ASB.AppendChar(C);
            ARawSB.AppendChar(C);
          end;
          Inc(FCurrent, UTF8_LINE_TERMINATOR_BYTE_LENGTH);
          Inc(FLine);
          FColumn := 1;
        end
        else
        begin
          Advance;
          ASB.AppendChar(C);
          ARawSB.AppendChar(C);
        end;
      end;
    else
      begin
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
      end;
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
//   - Line comments //…
//   - Block comments /*…*/
//   - Line terminators (CR, CRLF, LF, LS, PS) with line/column tracking
procedure TGocciaLexer.ScanInterpolationExpression(var ASB, ARawSB: TStringBuffer);
var
  BraceCount, J: Integer;
  C, Quote: Char;
begin
  BraceCount := 1;
  while (BraceCount > 0) and not IsAtEnd do
  begin
    C := Peek;
    case C of
      '{':
      begin
        Inc(BraceCount);
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
      end;
      '}':
      begin
        Dec(BraceCount);
        if BraceCount > 0 then
        begin
          Advance;
          ASB.AppendChar(C);
          ARawSB.AppendChar(C);
        end;
        // When BraceCount = 0, leave the closing } unconsumed for the caller
      end;
      '''', '"':
      begin
        // String literal — scan until matching unescaped quote
        Quote := C;
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
        while not IsAtEnd do
        begin
          C := Peek;
          if C = '\' then
          begin
            // Escape — consume \ and the next character verbatim
            Advance;
            ASB.AppendChar(C);
            ARawSB.AppendChar(C);
            if not IsAtEnd then
            begin
              C := Advance;
              ASB.AppendChar(C);
              ARawSB.AppendChar(C);
            end;
          end
          else if C = Quote then
          begin
            Advance;
            ASB.AppendChar(C);
            ARawSB.AppendChar(C);
            Break;
          end
          else
          begin
            // Track line terminators inside strings
            if C = #13 then
            begin
              Advance;
              if Peek = #10 then
                Advance;
              Inc(FLine);
              FColumn := 1;
              ASB.AppendChar(#10);
              ARawSB.AppendChar(#10);
            end
            else if C = #10 then
            begin
              Inc(FLine);
              FColumn := 0;
              Advance;
              ASB.AppendChar(C);
              ARawSB.AppendChar(C);
            end
            else
            begin
              Advance;
              ASB.AppendChar(C);
              ARawSB.AppendChar(C);
            end;
          end;
        end;
      end;
      '`':
      begin
        // Nested template literal — delegate to mutual recursion
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
        ScanNestedTemplateInExpression(ASB, ARawSB);
      end;
      '/':
      begin
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
        if not IsAtEnd then
        begin
          if Peek = '/' then
          begin
            // Line comment — consume until line terminator (LF, CR, LS, PS)
            C := Advance;
            ASB.AppendChar(C);
            ARawSB.AppendChar(C);
            while not IsAtEnd and not IsLineTerminator do
            begin
              C := Advance;
              ASB.AppendChar(C);
              ARawSB.AppendChar(C);
            end;
          end
          else if Peek = '*' then
          begin
            // Block comment — consume until */
            C := Advance; // consume *
            ASB.AppendChar(C);
            ARawSB.AppendChar(C);
            while not IsAtEnd do
            begin
              C := Peek;
              if C = '*' then
              begin
                Advance;
                ASB.AppendChar(C);
                ARawSB.AppendChar(C);
                if not IsAtEnd and (Peek = '/') then
                begin
                  C := Advance;
                  ASB.AppendChar(C);
                  ARawSB.AppendChar(C);
                  Break;
                end;
              end
              else if C = #13 then
              begin
                Advance;
                if Peek = #10 then
                  Advance;
                Inc(FLine);
                FColumn := 1;
                ASB.AppendChar(#10);
                ARawSB.AppendChar(#10);
              end
              else if C = #10 then
              begin
                Inc(FLine);
                FColumn := 0;
                Advance;
                ASB.AppendChar(C);
                ARawSB.AppendChar(C);
              end
              else
              begin
                Advance;
                ASB.AppendChar(C);
                ARawSB.AppendChar(C);
              end;
            end;
          end;
          // If neither // nor /*, the / was already appended — continue
        end;
      end;
      #13:
      begin
        Advance;
        if Peek = #10 then
          Advance;
        Inc(FLine);
        FColumn := 1;
        ASB.AppendChar(#10);
        ARawSB.AppendChar(#10);
      end;
      #10:
      begin
        Inc(FLine);
        FColumn := 0;
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
      end;
      UTF8_LINE_TERMINATOR_LEAD_BYTE:
      begin
        if IsUnicodeLineTerminator then
        begin
          for J := 0 to UTF8_LINE_TERMINATOR_BYTE_LENGTH - 1 do
          begin
            C := FSource[FCurrent + J];
            ASB.AppendChar(C);
            ARawSB.AppendChar(C);
          end;
          Inc(FCurrent, UTF8_LINE_TERMINATOR_BYTE_LENGTH);
          Inc(FLine);
          FColumn := 1;
        end
        else
        begin
          Advance;
          ASB.AppendChar(C);
          ARawSB.AppendChar(C);
        end;
      end;
    else
      begin
        Advance;
        ASB.AppendChar(C);
        ARawSB.AppendChar(C);
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
    gttString,
    gttTemplate,
    gttRegex,
    gttTrue,
    gttFalse,
    gttNull,
    gttIdentifier,
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
    if Peek = #10 then
    begin
      Inc(FLine);
      FColumn := 0;
    end;

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
    if Peek = #13 then
    begin
      Advance;
      if Peek = #10 then
        Advance;
      Inc(FLine);
      FColumn := 0;
      // ES2026 §12.9.6: TV and TRV both normalize CR and CRLF to LF
      SB.AppendChar(#10);
      RawSB.AppendChar(#10);
    end
    else if Peek = #10 then
    begin
      Inc(FLine);
      FColumn := 0;
      C := Advance;
      SB.AppendChar(C);
      RawSB.AppendChar(C);
    end
    // ES2026 §12.9.6: TV = TRV; TRV of LS/PS preserves the original code point
    else if (Peek = UTF8_LINE_TERMINATOR_LEAD_BYTE) and IsUnicodeLineTerminator then
    begin
      for J := 0 to UTF8_LINE_TERMINATOR_BYTE_LENGTH - 1 do
      begin
        C := FSource[FCurrent + J];
        SB.AppendChar(C);
        RawSB.AppendChar(C);
      end;
      Inc(FCurrent, UTF8_LINE_TERMINATOR_BYTE_LENGTH);
      Inc(FLine);
      FColumn := 0;
    end
    else if Peek = '\' then
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
    else if (Peek = '$') and (PeekNext = '{') then
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
  PatternBuffer: TStringBuffer;
  FlagsStart: Integer;
  Flags: string;
  InCharacterClass: Boolean;
  C: Char;
  I: Integer;
begin
  PatternBuffer := TStringBuffer.Create;
  InCharacterClass := False;

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
      PatternBuffer.AppendChar(C);
      if IsAtEnd or IsLineTerminator then
        raise TGocciaLexerError.Create('Unterminated regular expression literal',
          FLine, FColumn, FFileName, GetSourceLines,
          SSuggestCloseRegex);
      PatternBuffer.AppendChar(Advance);
      Continue;
    end;

    if C = '[' then
      InCharacterClass := True
    else if (C = ']') and InCharacterClass then
      InCharacterClass := False
    else if (C = '/') and not InCharacterClass then
      Break;

    PatternBuffer.AppendChar(C);
  end;

  if IsAtEnd and ((FCurrent = 1) or (FSource[FCurrent - 1] <> '/')) then
    raise TGocciaLexerError.Create('Unterminated regular expression literal',
      FLine, FColumn, FFileName, GetSourceLines,
      SSuggestCloseRegex);

  FlagsStart := FCurrent;
  while CharInSet(Peek, ['a'..'z', 'A'..'Z']) do
    Advance;
  Flags := Copy(FSource, FlagsStart, FCurrent - FlagsStart);

  for I := 1 to Length(Flags) do
  begin
    if not CharInSet(Flags[I], ['g', 'i', 'm', 's', 'u', 'y']) then
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

  AddToken(gttRegex, PatternBuffer.ToString + REGEX_SEPARATOR + Flags);
end;

// ES2026 §12.9.3 NumericLiteral
procedure TGocciaLexer.ScanNumber;
var
  Ch: Char;
  HasSeparator: Boolean;
  Lexeme: string;
begin
  HasSeparator := False;
  Ch := Peek;

  if Ch = '0' then
  begin
    Advance;
    Ch := Peek;

    if (Ch = 'x') or (Ch = 'X') then
    begin
      Advance;
      if not CharInSet(Peek, ['0'..'9', 'a'..'f', 'A'..'F']) then
        raise TGocciaLexerError.Create('Invalid hexadecimal number', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestHexNumberFormat);
      while CharInSet(Peek, ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
      begin
        if Peek = '_' then
        begin
          HasSeparator := True;
          Advance;
          if not CharInSet(Peek, ['0'..'9', 'a'..'f', 'A'..'F']) then
            raise TGocciaLexerError.Create('Numeric separator must be between digits',
              FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
        end
        else
          Advance;
      end;
    end
    else if (Ch = 'b') or (Ch = 'B') then
    begin
      Advance;
      if not CharInSet(Peek, ['0', '1']) then
        raise TGocciaLexerError.Create('Invalid binary number', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestBinaryNumberFormat);
      while CharInSet(Peek, ['0', '1', '_']) do
      begin
        if Peek = '_' then
        begin
          HasSeparator := True;
          Advance;
          if not CharInSet(Peek, ['0', '1']) then
            raise TGocciaLexerError.Create('Numeric separator must be between digits',
              FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
        end
        else
          Advance;
      end;
    end
    else if (Ch = 'o') or (Ch = 'O') then
    begin
      Advance;
      if not CharInSet(Peek, ['0'..'7']) then
        raise TGocciaLexerError.Create('Invalid octal number', FLine, FColumn, FFileName, GetSourceLines,
          SSuggestOctalNumberFormat);
      while CharInSet(Peek, ['0'..'7', '_']) do
      begin
        if Peek = '_' then
        begin
          HasSeparator := True;
          Advance;
          if not CharInSet(Peek, ['0'..'7']) then
            raise TGocciaLexerError.Create('Numeric separator must be between digits',
              FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
        end
        else
          Advance;
      end;
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
  begin
    while CharInSet(Peek, ['0'..'9', '_']) do
    begin
      if Peek = '_' then
      begin
        HasSeparator := True;
        Advance;
        if not CharInSet(Peek, ['0'..'9']) then
          raise TGocciaLexerError.Create('Numeric separator must be between digits',
            FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
      end
      else
        Advance;
    end;
  end;

  if (Peek = '.') and (PeekNext = '_') then
    raise TGocciaLexerError.Create('Numeric separator must be between digits',
      FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator)
  else if (Peek = '.') and CharInSet(PeekNext, ['0'..'9']) then
  begin
    Advance;
    while CharInSet(Peek, ['0'..'9', '_']) do
    begin
      if Peek = '_' then
      begin
        HasSeparator := True;
        Advance;
        if not CharInSet(Peek, ['0'..'9']) then
          raise TGocciaLexerError.Create('Numeric separator must be between digits',
            FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
      end
      else
        Advance;
    end;
  end;

  if CharInSet(Peek, ['e', 'E']) then
  begin
    Advance;
    if CharInSet(Peek, ['+', '-']) then
      Advance;
    if not CharInSet(Peek, ['0'..'9']) then
      raise TGocciaLexerError.Create('Invalid scientific notation', FLine, FColumn, FFileName, GetSourceLines,
        SSuggestScientificNotation);
    while CharInSet(Peek, ['0'..'9', '_']) do
    begin
      if Peek = '_' then
      begin
        HasSeparator := True;
        Advance;
        if not CharInSet(Peek, ['0'..'9']) then
          raise TGocciaLexerError.Create('Numeric separator must be between digits',
            FLine, FColumn, FFileName, GetSourceLines, SSuggestNumericSeparator);
      end
      else
        Advance;
    end;
  end;

  Lexeme := Copy(FSource, FStart, FCurrent - FStart);
  if HasSeparator then
    Lexeme := StringReplace(Lexeme, '_', '', [rfReplaceAll]);
  AddToken(gttNumber, Lexeme);
end;

function TGocciaLexer.IsValidIdentifierChar(const C: Char): Boolean;
begin
  Result := CharInSet(C, ValidIdentifierChars) or (Ord(C) > 127);
end;

class procedure TGocciaLexer.InitKeywords;
begin
  if Assigned(FKeywords) then Exit;
  FKeywords := TOrderedStringMap<TGocciaTokenType>.Create(40);
  // Reserved keywords
  FKeywords.Add(KEYWORD_BREAK, gttBreak);
  FKeywords.Add(KEYWORD_CASE, gttCase);
  FKeywords.Add(KEYWORD_CATCH, gttCatch);
  FKeywords.Add(KEYWORD_CLASS, gttClass);
  FKeywords.Add(KEYWORD_CONST, gttConst);
  FKeywords.Add(KEYWORD_DEFAULT, gttDefault);
  FKeywords.Add(KEYWORD_DELETE, gttDelete);
  FKeywords.Add(KEYWORD_DO, gttDo);
  FKeywords.Add(KEYWORD_ELSE, gttElse);
  FKeywords.Add(KEYWORD_ENUM, gttEnum);
  FKeywords.Add(KEYWORD_EXPORT, gttExport);
  FKeywords.Add(KEYWORD_EXTENDS, gttExtends);
  FKeywords.Add(KEYWORD_FALSE, gttFalse);
  FKeywords.Add(KEYWORD_FINALLY, gttFinally);
  FKeywords.Add(KEYWORD_FOR, gttFor);
  FKeywords.Add(KEYWORD_FUNCTION, gttFunction);
  FKeywords.Add(KEYWORD_IF, gttIf);
  FKeywords.Add(KEYWORD_IMPORT, gttImport);
  FKeywords.Add(KEYWORD_IN, gttIn);
  FKeywords.Add(KEYWORD_INSTANCEOF, gttInstanceof);
  FKeywords.Add(KEYWORD_LET, gttLet);
  FKeywords.Add(KEYWORD_NEW, gttNew);
  FKeywords.Add(KEYWORD_NULL, gttNull);
  FKeywords.Add(KEYWORD_RETURN, gttReturn);
  FKeywords.Add(KEYWORD_SUPER, gttSuper);
  FKeywords.Add(KEYWORD_SWITCH, gttSwitch);
  FKeywords.Add(KEYWORD_THIS, gttThis);
  FKeywords.Add(KEYWORD_THROW, gttThrow);
  FKeywords.Add(KEYWORD_TRUE, gttTrue);
  FKeywords.Add(KEYWORD_TRY, gttTry);
  FKeywords.Add(KEYWORD_TYPEOF, gttTypeof);
  FKeywords.Add(KEYWORD_VAR, gttVar);
  FKeywords.Add(KEYWORD_WHILE, gttWhile);
  FKeywords.Add(KEYWORD_WITH, gttWith);

  // Contextual keywords
  FKeywords.Add(KEYWORD_AS, gttAs);
  FKeywords.Add(KEYWORD_FROM, gttFrom);
  FKeywords.Add(KEYWORD_STATIC, gttStatic);
end;

class destructor TGocciaLexer.DestroyClass;
begin
  FKeywords.Free;
end;

procedure TGocciaLexer.ScanIdentifier;
var
  Text: string;
  TokenType: TGocciaTokenType;
begin
  while not IsAtEnd and IsValidIdentifierChar(Peek) do
    Advance;

  Text := Copy(FSource, FStart, FCurrent - FStart);

  if FKeywords.TryGetValue(Text, TokenType) then
    AddToken(TokenType, Text)
  else
    AddToken(gttIdentifier, Text);
end;

procedure TGocciaLexer.ScanToken;
var
  C: Char;
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
      else if FCanStartRegex then
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
      if Match('.') then
      begin
        if Match('.') then
          AddToken(gttSpread)
        else
          raise TGocciaLexerError.Create('Invalid token ..',
            FLine, FStartColumn, FFileName, GetSourceLines,
            SSuggestInvalidDoubleDot);
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
    else if IsValidIdentifierChar(C) then
    begin
      Dec(FCurrent);
      Dec(FColumn);
      ScanIdentifier;
    end
    else
      raise TGocciaLexerError.Create(Format('Unexpected character: %s', [C]),
        FLine, FStartColumn, FFileName, GetSourceLines,
        SSuggestInvalidCharacter);
  end;
end;

function TGocciaLexer.ScanTokens: TObjectList<TGocciaToken>;
begin
  SkipHashbang;
  while not IsAtEnd do
  begin
    SkipWhitespace;
    if not IsAtEnd then
      ScanToken;
  end;

  FTokens.Add(TGocciaToken.Create(gttEOF, '', FLine, FColumn));
  Result := FTokens;
end;

end.
