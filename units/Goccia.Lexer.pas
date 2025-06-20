unit Goccia.Lexer;

{$I Goccia.inc}

interface

uses
  Goccia.Token, Goccia.Error, Generics.Collections, Classes, SysUtils;

type
  TGocciaLexer = class
  const ValidIdentifierChars: set of Char = ['a'..'z', 'A'..'Z', '0'..'9', '_', '$'];
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

procedure TGocciaLexer.ScanString;
var
  Value: string;
  Quote: Char;
begin
  Quote := FSource[FStart];
  Value := '';

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
          'n': Value := Value + #10;
          'r': Value := Value + #13;
          't': Value := Value + #9;
          '\': Value := Value + '\';
          '''': Value := Value + '''';
          '"': Value := Value + '"';
        else
          Value := Value + Peek;
        end;
        Advance;
      end;
    end
    else
      Value := Value + Advance;
  end;

  if IsAtEnd then
    raise TGocciaLexerError.Create('Unterminated string', FLine, FColumn,
      FFileName, FSourceLines);

  Advance; // Closing quote
  AddToken(gttString, Value);
end;

procedure TGocciaLexer.ScanTemplate;
var
  Value: string;
begin
  Value := '';

  while (Peek <> '`') and not IsAtEnd do
  begin
    if Peek = #10 then
    begin
      Inc(FLine);
      FColumn := 0;
      Value := Value + Advance; // Template literals preserve newlines
    end
    else if Peek = '\' then
    begin
      Advance;
      if not IsAtEnd then
      begin
        case Peek of
          'n': Value := Value + #10;
          'r': Value := Value + #13;
          't': Value := Value + #9;
          '\': Value := Value + '\';
          '`': Value := Value + '`';
          '$': Value := Value + '$';
        else
          Value := Value + Peek;
        end;
        Advance;
      end;
    end
    else
      Value := Value + Advance;
  end;

  if IsAtEnd then
    raise TGocciaLexerError.Create('Unterminated template literal', FLine, FColumn,
      FFileName, FSourceLines);

  Advance; // Closing backtick
  AddToken(gttTemplate, Value);
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

procedure TGocciaLexer.ScanIdentifier;
var
  Text: string;
begin
  while not IsAtEnd and IsValidIdentifierChar(Peek) do
    Advance;

  Text := Copy(FSource, FStart, FCurrent - FStart);

  // Check for keywords
  if Text = 'const' then AddToken(gttConst, Text)
  else if Text = 'let' then AddToken(gttLet, Text)
  else if Text = 'class' then AddToken(gttClass, Text)
  else if Text = 'extends' then AddToken(gttExtends, Text)
  else if Text = 'new' then AddToken(gttNew, Text)
  else if Text = 'this' then AddToken(gttThis, Text)
  else if Text = 'super' then AddToken(gttSuper, Text)
    else if Text = 'static' then AddToken(gttStatic, Text)
  else if Text = 'return' then AddToken(gttReturn, Text)
  else if Text = 'if' then AddToken(gttIf, Text)
  else if Text = 'else' then AddToken(gttElse, Text)
  else if Text = 'for' then AddToken(gttFor, Text)
  else if Text = 'while' then AddToken(gttWhile, Text)
  else if Text = 'do' then AddToken(gttDo, Text)
  else if Text = 'switch' then AddToken(gttSwitch, Text)
  else if Text = 'case' then AddToken(gttCase, Text)
  else if Text = 'default' then AddToken(gttDefault, Text)
  else if Text = 'break' then AddToken(gttBreak, Text)
  else if Text = 'throw' then AddToken(gttThrow, Text)
  else if Text = 'try' then AddToken(gttTry, Text)
  else if Text = 'catch' then AddToken(gttCatch, Text)
  else if Text = 'finally' then AddToken(gttFinally, Text)
  else if Text = 'import' then AddToken(gttImport, Text)
  else if Text = 'export' then AddToken(gttExport, Text)
  else if Text = 'from' then AddToken(gttFrom, Text)
  else if Text = 'as' then AddToken(gttAs, Text)
  else if Text = 'typeof' then AddToken(gttTypeof, Text)
  else if Text = 'instanceof' then AddToken(gttInstanceof, Text)
  else if Text = 'in' then AddToken(gttIn, Text)
  else if Text = 'true' then AddToken(gttTrue, Text)
  else if Text = 'false' then AddToken(gttFalse, Text)
  else if Text = 'null' then AddToken(gttNull, Text)
  else if Text = 'undefined' then AddToken(gttUndefined, Text)
  else AddToken(gttIdentifier, Text);
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
