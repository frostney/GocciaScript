unit Goccia.Lexer;

{$I Goccia.inc}

interface

uses
  Goccia.Token, Goccia.Error, Generics.Collections, Classes, SysUtils;

type
  TGocciaLexer = class
  const ValidIdentifierChars: set of Char = ['a'..'z', 'A'..'Z', '0'..'9', '_', '$'];
  private class var
    FKeywords: TDictionary<string, TGocciaTokenType>;
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

    function IsAtEnd: Boolean; inline;
    function Advance: Char;
    function Peek: Char; inline;
    function PeekNext: Char; inline;
    function Match(Expected: Char): Boolean; inline;
    function IsValidIdentifierChar(C: Char): Boolean;
    procedure AddToken(TokenType: TGocciaTokenType); overload;
    procedure AddToken(TokenType: TGocciaTokenType; const Literal: string); overload;
    procedure ScanToken;
    procedure ScanString;
    procedure ScanTemplate;
    procedure ScanNumber;
    procedure ScanIdentifier;
    function ScanUnicodeEscape: string;
    function ScanHexEscape: string;
    procedure SkipWhitespace;
    procedure SkipComment;
    procedure SkipBlockComment;
  public
    constructor Create(const ASource, AFileName: string);
    destructor Destroy; override;
    function ScanTokens: TObjectList<TGocciaToken>;
    property SourceLines: TStringList read FSourceLines;
  end;

implementation

constructor TGocciaLexer.Create(const ASource, AFileName: string);
begin
  InitKeywords;
  FSource := ASource;
  FFileName := AFileName;
  FTokens := TObjectList<TGocciaToken>.Create(True);
  FSourceLines := TStringList.Create;
  FSourceLines.Text := ASource;
  FCurrent := 1;
  FLine := 1;
  FColumn := 1;
end;

destructor TGocciaLexer.Destroy;
begin
  FTokens.Free;
  FSourceLines.Free;
  inherited;
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

function TGocciaLexer.Match(Expected: Char): Boolean; inline;
begin
  if IsAtEnd then
  begin
    Result := False;
    Exit;
  end;

  if FSource[FCurrent] <> Expected then
  begin
    Result := False;
    Exit;
  end;

  Inc(FCurrent);
  Inc(FColumn);
  Result := True;
end;

procedure TGocciaLexer.AddToken(TokenType: TGocciaTokenType);
begin
  AddToken(TokenType, Copy(FSource, FStart, FCurrent - FStart));
end;

procedure TGocciaLexer.AddToken(TokenType: TGocciaTokenType; const Literal: string);
begin
  FTokens.Add(TGocciaToken.Create(TokenType, Literal, FLine, FStartColumn));
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
    FFileName, FSourceLines);
end;

function TGocciaLexer.ScanUnicodeEscape: string;
var
  CodePoint: Cardinal;
  HexStr: string;
  I: Integer;
begin
  // Called after consuming '\u', Peek is the next character
  if Peek = '{' then
  begin
    // Variable-length: \u{XXXX} or \u{XXXXX}
    Advance; // consume '{'
    HexStr := '';
    while (Peek <> '}') and not IsAtEnd do
      HexStr := HexStr + Advance;
    if IsAtEnd then
      raise TGocciaLexerError.Create('Unterminated unicode escape', FLine, FColumn, FFileName, FSourceLines);
    Advance; // consume '}'
  end
  else
  begin
    // Fixed 4-digit: \uXXXX
    HexStr := '';
    for I := 1 to 4 do
    begin
      if IsAtEnd then
        raise TGocciaLexerError.Create('Invalid unicode escape', FLine, FColumn, FFileName, FSourceLines);
      HexStr := HexStr + Advance;
    end;
  end;

  CodePoint := StrToInt('$' + HexStr);

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
    raise TGocciaLexerError.Create('Invalid unicode code point', FLine, FColumn, FFileName, FSourceLines);
end;

function TGocciaLexer.ScanHexEscape: string;
var
  HexStr: string;
  CodePoint: Cardinal;
begin
  // Called after consuming '\x', Peek is the first hex digit
  HexStr := '';
  if IsAtEnd then
    raise TGocciaLexerError.Create('Invalid hex escape', FLine, FColumn, FFileName, FSourceLines);
  HexStr := HexStr + Advance;
  if IsAtEnd then
    raise TGocciaLexerError.Create('Invalid hex escape', FLine, FColumn, FFileName, FSourceLines);
  HexStr := HexStr + Advance;

  CodePoint := StrToInt('$' + HexStr);
  Result := Chr(CodePoint);
end;

procedure TGocciaLexer.ScanString;
var
  SB: TStringBuilder;
  Quote: Char;
begin
  Quote := FSource[FStart];
  SB := TStringBuilder.Create;
  try
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
            'n': begin SB.Append(#10); Advance; end;
            'r': begin SB.Append(#13); Advance; end;
            't': begin SB.Append(#9); Advance; end;
            '\': begin SB.Append('\'); Advance; end;
            '''': begin SB.Append(''''); Advance; end;
            '"': begin SB.Append('"'); Advance; end;
            '0': begin SB.Append(#0); Advance; end;
            'u': begin Advance; SB.Append(ScanUnicodeEscape); end;
            'x': begin Advance; SB.Append(ScanHexEscape); end;
          else
            SB.Append(Peek);
            Advance;
          end;
        end;
      end
      else
        SB.Append(Advance);
    end;

    if IsAtEnd then
      raise TGocciaLexerError.Create('Unterminated string', FLine, FColumn,
        FFileName, FSourceLines);

    Advance; // Closing quote
    AddToken(gttString, SB.ToString);
  finally
    SB.Free;
  end;
end;

procedure TGocciaLexer.ScanTemplate;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    while (Peek <> '`') and not IsAtEnd do
    begin
      if Peek = #13 then
      begin
        // ECMAScript spec: normalize CR and CRLF to LF in template literals
        Advance;
        if Peek = #10 then
          Advance;
        Inc(FLine);
        FColumn := 0;
        SB.Append(#10);
      end
      else if Peek = #10 then
      begin
        Inc(FLine);
        FColumn := 0;
        SB.Append(Advance);
      end
      else if Peek = '\' then
      begin
        Advance;
        if not IsAtEnd then
        begin
          case Peek of
            'n': begin SB.Append(#10); Advance; end;
            'r': begin SB.Append(#13); Advance; end;
            't': begin SB.Append(#9); Advance; end;
            '\': begin SB.Append('\'); Advance; end;
            '`': begin SB.Append('`'); Advance; end;
            '$': begin SB.Append('$'); Advance; end;
            '0': begin SB.Append(#0); Advance; end;
            'u': begin Advance; SB.Append(ScanUnicodeEscape); end;
            'x': begin Advance; SB.Append(ScanHexEscape); end;
          else
            SB.Append(Peek);
            Advance;
          end;
        end;
      end
      else
        SB.Append(Advance);
    end;

    if IsAtEnd then
      raise TGocciaLexerError.Create('Unterminated template literal', FLine, FColumn,
        FFileName, FSourceLines);

    Advance; // Closing backtick
    AddToken(gttTemplate, SB.ToString);
  finally
    SB.Free;
  end;
end;

procedure TGocciaLexer.ScanNumber;
var
  Value: string;
  Ch: Char;
begin
  Value := '';
  Ch := Peek;

  // Handle different number formats
  if Ch = '0' then
  begin
    Value := Value + Advance; // consume '0'
    Ch := Peek;

    if (Ch = 'x') or (Ch = 'X') then
    begin
      // Hexadecimal: 0x10, 0X10
      Value := Value + Advance; // consume 'x' or 'X'
      if not CharInSet(Peek, ['0'..'9', 'a'..'f', 'A'..'F']) then
        raise TGocciaLexerError.Create('Invalid hexadecimal number', FLine, FColumn, FFileName, FSourceLines);

      while CharInSet(Peek, ['0'..'9', 'a'..'f', 'A'..'F']) do
        Value := Value + Advance;
    end
    else if (Ch = 'b') or (Ch = 'B') then
    begin
      // Binary: 0b1010, 0B1010
      Value := Value + Advance; // consume 'b' or 'B'
      if not CharInSet(Peek, ['0', '1']) then
        raise TGocciaLexerError.Create('Invalid binary number', FLine, FColumn, FFileName, FSourceLines);

      while CharInSet(Peek, ['0', '1']) do
        Value := Value + Advance;
    end
    else if (Ch = 'o') or (Ch = 'O') then
    begin
      // Octal: 0o12, 0O12
      Value := Value + Advance; // consume 'o' or 'O'
      if not CharInSet(Peek, ['0'..'7']) then
        raise TGocciaLexerError.Create('Invalid octal number', FLine, FColumn, FFileName, FSourceLines);

      while CharInSet(Peek, ['0'..'7']) do
        Value := Value + Advance;
    end
    else
    begin
      // Handle regular numbers starting with 0 (including 0.xxx)
      // Continue consuming digits if present (for cases like 000123)
      while CharInSet(Peek, ['0'..'9']) do
        Value := Value + Advance;
    end;
  end
  else
  begin
    // Regular decimal number (not starting with 0)
    while CharInSet(Peek, ['0'..'9']) do
      Value := Value + Advance;
  end;

  // Handle decimal part (e.g., 0.69314, 1.5)
  if (Peek = '.') and CharInSet(PeekNext, ['0'..'9']) then
  begin
    Value := Value + Advance; // Consume '.'
    while CharInSet(Peek, ['0'..'9']) do
      Value := Value + Advance;
  end;

  // Handle scientific notation (e.g., 1e3, 2.5E-10, 0.69314e2)
  if CharInSet(Peek, ['e', 'E']) then
  begin
    Value := Value + Advance; // Consume 'e' or 'E'
    if CharInSet(Peek, ['+', '-']) then
      Value := Value + Advance; // Consume optional '+' or '-'

    if not CharInSet(Peek, ['0'..'9']) then
      raise TGocciaLexerError.Create('Invalid scientific notation', FLine, FColumn, FFileName, FSourceLines);

    while CharInSet(Peek, ['0'..'9']) do
      Value := Value + Advance;
  end;

  AddToken(gttNumber, Value);
end;

function TGocciaLexer.IsValidIdentifierChar(C: Char): Boolean;
begin
  Result := CharInSet(C, ValidIdentifierChars) or (Ord(C) > 127); // Allow Unicode characters
end;

class procedure TGocciaLexer.InitKeywords;
begin
  if Assigned(FKeywords) then Exit;
  FKeywords := TDictionary<string, TGocciaTokenType>.Create(40);
  FKeywords.Add('const', gttConst);
  FKeywords.Add('let', gttLet);
  FKeywords.Add('class', gttClass);
  FKeywords.Add('extends', gttExtends);
  FKeywords.Add('new', gttNew);
  FKeywords.Add('this', gttThis);
  FKeywords.Add('super', gttSuper);
  FKeywords.Add('static', gttStatic);
  FKeywords.Add('return', gttReturn);
  FKeywords.Add('if', gttIf);
  FKeywords.Add('else', gttElse);
  FKeywords.Add('for', gttFor);
  FKeywords.Add('while', gttWhile);
  FKeywords.Add('do', gttDo);
  FKeywords.Add('switch', gttSwitch);
  FKeywords.Add('case', gttCase);
  FKeywords.Add('default', gttDefault);
  FKeywords.Add('break', gttBreak);
  FKeywords.Add('throw', gttThrow);
  FKeywords.Add('try', gttTry);
  FKeywords.Add('catch', gttCatch);
  FKeywords.Add('finally', gttFinally);
  FKeywords.Add('import', gttImport);
  FKeywords.Add('export', gttExport);
  FKeywords.Add('from', gttFrom);
  FKeywords.Add('as', gttAs);
  FKeywords.Add('typeof', gttTypeof);
  FKeywords.Add('instanceof', gttInstanceof);
  FKeywords.Add('in', gttIn);
  FKeywords.Add('delete', gttDelete);
  FKeywords.Add('true', gttTrue);
  FKeywords.Add('false', gttFalse);
  FKeywords.Add('null', gttNull);
  FKeywords.Add('undefined', gttUndefined);
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
        AddToken(gttNullishCoalescing)
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
          raise TGocciaLexerError.Create('Use !== for inequality',
            FLine, FStartColumn, FFileName, FSourceLines);
      end
      else
        AddToken(gttNot);
    '=':
      if Match('=') then
      begin
        if Match('=') then
          AddToken(gttEqual)
        else
          raise TGocciaLexerError.Create('Use === for equality',
            FLine, FStartColumn, FFileName, FSourceLines);
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
            FLine, FStartColumn, FFileName, FSourceLines);
      end
      else
        AddToken(gttDot);
    '#': AddToken(gttHash);
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
        FLine, FStartColumn, FFileName, FSourceLines);
  end;
end;

function TGocciaLexer.ScanTokens: TObjectList<TGocciaToken>;
begin
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
