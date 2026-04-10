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
    procedure UpdateRegexContext(const ATokenType: TGocciaTokenType);
    procedure PushParenRegexContext(const AAllowsRegexAfter: Boolean);
    function PopParenRegexContext: Boolean;
    procedure SkipWhitespace;
    procedure SkipHashbang;
    procedure SkipComment;
    procedure SkipBlockComment;
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

procedure TGocciaLexer.SkipWhitespace;
begin
  while not IsAtEnd do
  begin
    case Peek of
      ' ', #13, #9:
        Advance;
      #10:
        begin
          Inc(FLine);
          FColumn := 0;
          Advance;
        end;
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

procedure TGocciaLexer.SkipHashbang;
begin
  if (FCurrent <> 1) or (Length(FSource) < 2) or (Copy(FSource, 1, 2) <> '#!') then
    Exit;

  while not IsAtEnd and not CharInSet(Peek, [#10, #13]) do
    Advance;
end;

procedure TGocciaLexer.SkipComment;
begin
  // Skip '//'
  Advance;
  Advance;

  while (Peek <> #10) and not IsAtEnd do
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
    else if Peek = #10 then
    begin
      Inc(FLine);
      FColumn := 0;
      Advance;
    end
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

procedure TGocciaLexer.ScanTemplate;
var
  SB: TStringBuffer;
begin
  SB := TStringBuffer.Create;
  while (Peek <> '`') and not IsAtEnd do
  begin
    if Peek = #13 then
    begin
      Advance;
      if Peek = #10 then
        Advance;
      Inc(FLine);
      FColumn := 0;
      SB.AppendChar(#10);
    end
    else if Peek = #10 then
    begin
      Inc(FLine);
      FColumn := 0;
      SB.AppendChar(Advance);
    end
    else if Peek = '\' then
    begin
      Advance;
      if not IsAtEnd then
      begin
        case Peek of
          '`': begin SB.AppendChar('`'); Advance; end;
          '$': begin SB.AppendChar('$'); Advance; end;
        else
          ProcessEscapeSequence(SB);
        end;
      end;
    end
    else
      SB.AppendChar(Advance);
  end;

  if IsAtEnd then
    raise TGocciaLexerError.Create('Unterminated template literal', FLine, FColumn,
      FFileName, GetSourceLines, SSuggestCloseTemplate);

  Advance; // Closing backtick
  AddToken(gttTemplate, SB.ToString);
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

  while not IsAtEnd do
  begin
    C := Advance;

    if C = #10 then
      raise TGocciaLexerError.Create('Unterminated regular expression literal',
        FLine, FColumn, FFileName, GetSourceLines,
        SSuggestCloseRegex);

    if C = '\' then
    begin
      PatternBuffer.AppendChar(C);
      if IsAtEnd then
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

// ES2021 §12.9.3 NumericLiteral
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

  if (Peek = '.') and CharInSet(PeekNext, ['0'..'9']) then
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
        AddToken(gttAnd)
      else if Match('=') then
        AddToken(gttBitwiseAndAssign)
      else
        AddToken(gttBitwiseAnd);
    '|':
      if Match('|') then
        AddToken(gttOr)
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
