unit G2.Scan;

{ G2's scanner (ES2026 §12 ECMAScript Language: Lexical Grammar) for the single-pass front end: it reads UTF-16
  source in place and produces one token at a time on the parser's request, the parser naming the lexical goal
  (a slash starts a RegularExpressionLiteral or is division; a right brace continues a template or is
  punctuation).
  Identifiers are G2 atoms interned straight from the source; a string's or template's cooked text is in Buf
  until the next token. Nothing per token is allocated on the heap. }

{$mode objfpc}{$H+}{$modeswitch advancedrecords}

interface

uses
  SysUtils, G2.Core;

type
  TTok = (
    tkEOF, tkIdent, tkPrivateName, tkNumber, tkBigInt, tkString,
    tkTemplate, tkTemplateHead, tkTemplateMiddle, tkTemplateTail, tkRegExp,
    { punctuators }
    tkLBrace, tkRBrace, tkLParen, tkRParen, tkLBracket, tkRBracket, tkDot, tkEllipsis, tkSemi, tkComma,
    tkLt, tkGt, tkLe, tkGe, tkEq, tkNe, tkSEq, tkSNe, tkPlus, tkMinus, tkStar, tkPercent, tkStarStar,
    tkInc, tkDec, tkShl, tkSar, tkShr, tkAmp, tkBar, tkCaret, tkBang, tkTilde, tkAndAnd, tkOrOr, tkQQ,
    tkQuestion, tkQDot, tkColon, tkAssign, tkPlusAssign, tkMinusAssign, tkStarAssign, tkPercentAssign,
    tkStarStarAssign, tkShlAssign, tkSarAssign, tkShrAssign, tkAmpAssign, tkBarAssign, tkCaretAssign,
    tkAndAndAssign, tkOrOrAssign, tkQQAssign, tkArrow, tkSlash, tkSlashAssign, tkAt,
    { ReservedWord (§12.7.2) }
    tkBreak, tkCase, tkCatch, tkClass, tkConst, tkContinue, tkDebugger, tkDefault, tkDelete, tkDo, tkElse,
    tkEnum, tkExport, tkExtends, tkFalse, tkFinally, tkFor, tkFunction, tkIf, tkImport, tkIn, tkInstanceof,
    tkNew, tkNull, tkReturn, tkSuper, tkSwitch, tkThis, tkThrow, tkTrue, tkTry, tkTypeof, tkVar, tkVoid,
    tkWhile, tkWith
  );

  { the goal of the next token: a slash begins a regular expression, or is division; a right brace ends a
    template substitution (the template continues) or is a plain brace }
  TScanGoal = (sgDiv, sgRegExp, sgTemplateTail);

  EScanError = class(Exception)
  public
    Pos: Integer;   { 0-based source offset }
    constructor CreatePos(const Msg: string; APos: Integer);
  end;

  TScanState = record
    Pos, Line, LineStart: Integer;
    AfterLine: Boolean;   { the current position is preceded on its line only by whitespace and comments }
  end;

  TScanner = class
  private
    FSrc: PWideChar;
    FLen: Integer;
    FPos: Integer;          { next code unit to read }
    FLine, FLineStart: Integer;
    FAtLineStart: Boolean;  { for '-->' (B.1.1): only whitespace and comments since the line began }
    procedure Fail(const Msg: string; APos: Integer);
    procedure SkipTrivia;
    procedure SkipBlockComment;
    procedure SkipLineRest;
    procedure NewLine(Width: Integer); inline;
    procedure ScanIdentOrKeyword;
    procedure ScanIdentSlow(Start: Integer; IsPrivate: Boolean);
    function ReadIdentEscape: Cardinal;
    procedure ScanNumber;
    procedure ScanString(Quote: WideChar);
    procedure ScanTemplate(Continuation: Boolean);
    procedure ScanRegExp;
    procedure BufAdd(C: WideChar); inline;
    procedure BufAddCP(CP: Cardinal);
    function ReadHexDigits(Count: Integer; out V: Cardinal): Boolean;
    function ReadBracedCodePoint(out V: Cardinal): Boolean;
  public
    { HTML-like comments (B.1.1): on for Scripts, eval and Function code, off for modules }
    HTMLComments: Boolean;
    { the current token }
    Kind: TTok;
    TokStart, TokEnd: Integer;   { 0-based offsets, TokEnd exclusive }
    PrevEnd: Integer;            { the previous token's TokEnd }
    TokLine, TokCol: Integer;    { 1-based start line and column (UTF-16 units) }
    NewlineBefore: Boolean;      { a LineTerminator (or a multi-line comment holding one) precedes it }
    Escaped: Boolean;            { an identifier or keyword written with a \u escape }
    EscapedReserved: TTok;       { an escaped identifier: the reserved word it spells, else tkIdent }
    Atom: PStr;                  { tkIdent, tkPrivateName (without '#'): the name; also set for keywords }
    Num: Double;                 { tkNumber }
    LegacyOctal: Boolean;        { a LegacyOctal / NonOctalDecimal literal, or a string with a legacy escape }
    Buf: array of WideChar;      { tkString / templates: the cooked text; tkBigInt: the digits (no separators) }
    BufLen: Integer;
    CookedValid: Boolean;        { templates: the cooked value exists (no invalid escape) }
    RegexBodyStart, RegexBodyLen, RegexFlagsStart, RegexFlagsLen: Integer;
    constructor Create(ASrc: PWideChar; ALen: Integer);
    procedure Next(Goal: TScanGoal);
    function Save: TScanState;
    procedure Restore(const S: TScanState);
    { scans the current token again with another goal (a slash read as division that starts a regular
      expression, a right brace that continues a template) }
    procedure Rescan(Goal: TScanGoal);
    function BufText: UnicodeString;
    function TokText: UnicodeString;
    { the atom of an identifier or reserved word (reserved words get theirs on demand) }
    function TokAtom: PStr;
    { a template's raw text (§13.2.8.6 TV / TRV): CR and CRLF become LF }
    function TemplateRaw: UnicodeString;
    property Src: PWideChar read FSrc;
    property Len: Integer read FLen;
  end;

function TokName(K: TTok): string;

implementation

uses
  Goccia.Identifier, TextSemantics, NumericText;

const
  LS = #$2028;
  PS = #$2029;
  { ASCII character classes: a WideChar in a set of Char would go through a code page conversion }
  ccStart = 1; ccPart = 2; ccDigit = 4; ccHex = 8; ccOctal = 16;

var
  CClass: array[0..127] of Byte;

procedure InitCClass;
var C: Integer;
begin
  for C := 0 to 127 do
  begin
    CClass[C] := 0;
    if Char(C) in ['a'..'z', 'A'..'Z', '$', '_'] then CClass[C] := ccStart or ccPart;
    if Char(C) in ['0'..'9'] then CClass[C] := CClass[C] or ccPart or ccDigit or ccHex;
    if Char(C) in ['a'..'f', 'A'..'F'] then CClass[C] := CClass[C] or ccHex;
    if Char(C) in ['0'..'7'] then CClass[C] := CClass[C] or ccOctal;
  end;
end;

function CCls(C: WideChar): Byte; inline;
begin
  if Ord(C) < 128 then Result := CClass[Ord(C)] else Result := 0;
end;

function IsDigitW(C: WideChar): Boolean; inline;
begin
  Result := (Ord(C) >= Ord('0')) and (Ord(C) <= Ord('9'));
end;

type
  TKw = record S: string; K: TTok; end;

const
  KEYWORDS: array[0..35] of TKw = (
    (S: 'break'; K: tkBreak), (S: 'case'; K: tkCase), (S: 'catch'; K: tkCatch), (S: 'class'; K: tkClass),
    (S: 'const'; K: tkConst), (S: 'continue'; K: tkContinue), (S: 'debugger'; K: tkDebugger),
    (S: 'default'; K: tkDefault), (S: 'delete'; K: tkDelete), (S: 'do'; K: tkDo), (S: 'else'; K: tkElse),
    (S: 'enum'; K: tkEnum), (S: 'export'; K: tkExport), (S: 'extends'; K: tkExtends), (S: 'false'; K: tkFalse),
    (S: 'finally'; K: tkFinally), (S: 'for'; K: tkFor), (S: 'function'; K: tkFunction), (S: 'if'; K: tkIf),
    (S: 'import'; K: tkImport), (S: 'in'; K: tkIn), (S: 'instanceof'; K: tkInstanceof), (S: 'new'; K: tkNew),
    (S: 'null'; K: tkNull), (S: 'return'; K: tkReturn), (S: 'super'; K: tkSuper), (S: 'switch'; K: tkSwitch),
    (S: 'this'; K: tkThis), (S: 'throw'; K: tkThrow), (S: 'true'; K: tkTrue), (S: 'try'; K: tkTry),
    (S: 'typeof'; K: tkTypeof), (S: 'var'; K: tkVar), (S: 'void'; K: tkVoid), (S: 'while'; K: tkWhile),
    (S: 'with'; K: tkWith));

var
  { a reserved word's kind by its atom: the atoms of KEYWORDS, filled on first use }
  KwAtoms: array[0..35] of PStr;
  KwReady: Boolean = False;
  { the reserved words by first letter, as UTF-16 text, for KeywordOfChars }
  KwText: array[0..35] of UnicodeString;
  KwByFirst: array[0..25] of array of Integer;

procedure InitKeywords;
var I, F: Integer; W: UnicodeString;
begin
  for I := 0 to High(KEYWORDS) do
  begin
    W := UnicodeString(KEYWORDS[I].S);
    KwText[I] := W;
    KwAtoms[I] := AtomOfChars(PWideChar(W), Length(W));
    F := Ord(W[1]) - Ord('a');
    SetLength(KwByFirst[F], Length(KwByFirst[F]) + 1);
    KwByFirst[F][High(KwByFirst[F])] := I;
  end;
  KwReady := True;
end;

{ the reserved word spelled by the L characters at P (ASCII, no escapes), or tkIdent }
function KeywordOfChars(P: PWideChar; L: Integer): TTok; inline;
var F, J, I: Integer;
begin
  Result := tkIdent;
  if (L < 2) or (L > 10) then Exit;
  F := Ord(P[0]) - Ord('a');
  if (F < 1) or (F > 22) then Exit;   { b .. w }
  for J := 0 to High(KwByFirst[F]) do
  begin
    I := KwByFirst[F][J];
    if (Length(KwText[I]) = L) and (CompareWord(P^, PWideChar(KwText[I])^, L) = 0) then Exit(KEYWORDS[I].K);
  end;
end;

function KeywordKind(A: PStr; out K: TTok): Boolean;
var I: Integer;
begin
  if (A^.Len < 2) or (A^.Len > 10) then Exit(False);
  for I := 0 to High(KwAtoms) do
    if KwAtoms[I] = A then
    begin
      K := KEYWORDS[I].K;
      Exit(True);
    end;
  Result := False;
end;

function TokName(K: TTok): string;
begin
  WriteStr(Result, K);
end;

constructor EScanError.CreatePos(const Msg: string; APos: Integer);
begin
  inherited Create(Msg);
  Pos := APos;
end;

constructor TScanner.Create(ASrc: PWideChar; ALen: Integer);
begin
  inherited Create;
  if not KwReady then
  begin
    InitKeywords;
    InitCClass;
  end;
  FSrc := ASrc;
  FLen := ALen;
  FPos := 0;
  FLine := 1;
  FLineStart := 0;
  FAtLineStart := True;
  HTMLComments := True;
  SetLength(Buf, 64);
  { a Hashbang Comment (§12.5) at the very start }
  if (FLen >= 2) and (FSrc[0] = '#') and (FSrc[1] = '!') then SkipLineRest;
end;

procedure TScanner.Fail(const Msg: string; APos: Integer);
begin
  raise EScanError.CreatePos(Msg, APos);
end;

function TScanner.Save: TScanState;
begin
  Result.Pos := FPos;
  Result.Line := FLine;
  Result.LineStart := FLineStart;
  Result.AfterLine := FAtLineStart;
end;

procedure TScanner.Restore(const S: TScanState);
begin
  FPos := S.Pos;
  FLine := S.Line;
  FLineStart := S.LineStart;
  FAtLineStart := S.AfterLine;
end;

procedure TScanner.Rescan(Goal: TScanGoal);
var NL: Boolean; PE: Integer;
begin
  NL := NewlineBefore;
  PE := PrevEnd;
  FPos := TokStart;
  FLine := TokLine;
  FLineStart := TokStart - TokCol + 1;
  FAtLineStart := False;
  Next(Goal);
  NewlineBefore := NL;
  PrevEnd := PE;
end;

procedure TScanner.NewLine(Width: Integer);
begin
  Inc(FPos, Width);
  Inc(FLine);
  FLineStart := FPos;
  FAtLineStart := True;
end;

procedure TScanner.BufAdd(C: WideChar);
begin
  if BufLen >= Length(Buf) then SetLength(Buf, BufLen * 2 + 16);
  Buf[BufLen] := C;
  Inc(BufLen);
end;

procedure TScanner.BufAddCP(CP: Cardinal);
begin
  if CP < $10000 then BufAdd(WideChar(CP))
  else
  begin
    Dec(CP, $10000);
    BufAdd(WideChar($D800 + (CP shr 10)));
    BufAdd(WideChar($DC00 + (CP and $3FF)));
  end;
end;

function TScanner.BufText: UnicodeString;
begin
  SetLength(Result, BufLen);
  if BufLen > 0 then Move(Buf[0], Result[1], BufLen * SizeOf(WideChar));
end;

function TScanner.TokText: UnicodeString;
begin
  SetLength(Result, TokEnd - TokStart);
  if TokEnd > TokStart then Move(FSrc[TokStart], Result[1], (TokEnd - TokStart) * SizeOf(WideChar));
end;

function TScanner.TemplateRaw: UnicodeString;
var P, E, N: Integer; C: WideChar;
begin
  { between the opening delimiter (a backtick or the right brace) and the closing one (a backtick or
    the dollar-and-brace) }
  P := TokStart + 1;
  if (Kind = tkTemplateHead) or (Kind = tkTemplateMiddle) then E := TokEnd - 2 else E := TokEnd - 1;
  SetLength(Result, E - P);
  N := 0;
  while P < E do
  begin
    C := FSrc[P];
    if C = #13 then
    begin
      C := #10;
      if (P + 1 < E) and (FSrc[P + 1] = #10) then Inc(P);
    end;
    Inc(N);
    Result[N] := C;
    Inc(P);
  end;
  SetLength(Result, N);
end;

{ ---- whitespace and comments (§12.2, §12.3, §12.4, B.1.1) ---- }

procedure TScanner.SkipLineRest;
var C: WideChar;
begin
  while FPos < FLen do
  begin
    C := FSrc[FPos];
    if (C = #10) or (C = #13) or (C = LS) or (C = PS) then Break;
    Inc(FPos);
  end;
end;

procedure TScanner.SkipBlockComment;
var Start: Integer; C: WideChar;
begin
  Start := FPos;
  Inc(FPos, 2);
  while True do
  begin
    if FPos >= FLen then Fail('Unterminated block comment', Start);
    C := FSrc[FPos];
    if (C = '*') and (FPos + 1 < FLen) and (FSrc[FPos + 1] = '/') then
    begin
      Inc(FPos, 2);
      Exit;
    end;
    if C = #13 then
    begin
      if (FPos + 1 < FLen) and (FSrc[FPos + 1] = #10) then NewLine(2) else NewLine(1);
      NewlineBefore := True;
    end
    else if (C = #10) or (C = LS) or (C = PS) then
    begin
      NewLine(1);
      NewlineBefore := True;
    end
    else Inc(FPos);
  end;
end;

procedure TScanner.SkipTrivia;
var C: WideChar;
begin
  while FPos < FLen do
  begin
    C := FSrc[FPos];
    case C of
      ' ', #9, #11, #12, #$A0, #$FEFF: Inc(FPos);
      #10:
        begin
          NewLine(1);
          NewlineBefore := True;
        end;
      #13:
        begin
          if (FPos + 1 < FLen) and (FSrc[FPos + 1] = #10) then NewLine(2) else NewLine(1);
          NewlineBefore := True;
        end;
      LS, PS:
        begin
          NewLine(1);
          NewlineBefore := True;
        end;
      '/':
        if FPos + 1 < FLen then
        begin
          if FSrc[FPos + 1] = '/' then SkipLineRest
          else if FSrc[FPos + 1] = '*' then SkipBlockComment
          else Exit;
        end
        else Exit;
      '<':
        { B.1.1 SingleLineHTMLOpenComment }
        if HTMLComments and (FPos + 3 < FLen) and (FSrc[FPos + 1] = '!') and (FSrc[FPos + 2] = '-') and
           (FSrc[FPos + 3] = '-') then SkipLineRest
        else Exit;
      '-':
        { B.1.1 HTMLCloseComment: at the start of the input or after a line terminator, with only whitespace and
          comments between }
        if HTMLComments and FAtLineStart and (FPos + 2 < FLen) and (FSrc[FPos + 1] = '-') and
           (FSrc[FPos + 2] = '>') then SkipLineRest
        else Exit;
    else
      if (Ord(C) > $7F) and IsECMAScriptWhitespaceCodePoint(Ord(C)) then Inc(FPos)
      else Exit;
    end;
  end;
end;

{ ---- identifiers (§12.7) ---- }

function TScanner.ReadHexDigits(Count: Integer; out V: Cardinal): Boolean;
var I, D: Integer; C: WideChar;
begin
  V := 0;
  for I := 1 to Count do
  begin
    if FPos >= FLen then Exit(False);
    C := FSrc[FPos];
    case C of
      '0'..'9': D := Ord(C) - Ord('0');
      'a'..'f': D := Ord(C) - Ord('a') + 10;
      'A'..'F': D := Ord(C) - Ord('A') + 10;
    else
      Exit(False);
    end;
    V := V * 16 + Cardinal(D);
    Inc(FPos);
  end;
  Result := True;
end;

{ after the braced form's opening: hex digits and the closing brace, at most 10FFFF }
function TScanner.ReadBracedCodePoint(out V: Cardinal): Boolean;
var C: WideChar; D, N: Integer;
begin
  V := 0;
  N := 0;
  while True do
  begin
    if FPos >= FLen then Exit(False);
    C := FSrc[FPos];
    case C of
      '0'..'9': D := Ord(C) - Ord('0');
      'a'..'f': D := Ord(C) - Ord('a') + 10;
      'A'..'F': D := Ord(C) - Ord('A') + 10;
      '}':
        begin
          Inc(FPos);
          Exit(N > 0);
        end;
    else
      Exit(False);
    end;
    V := V * 16 + Cardinal(D);
    if V > $10FFFF then Exit(False);
    Inc(N);
    Inc(FPos);
  end;
end;

{ at '\': a UnicodeEscapeSequence in an identifier (§12.7.1) }
function TScanner.ReadIdentEscape: Cardinal;
var Start: Integer;
begin
  Start := FPos;
  Inc(FPos);
  if (FPos >= FLen) or (FSrc[FPos] <> 'u') then Fail('Invalid escape in identifier', Start);
  Inc(FPos);
  if (FPos < FLen) and (FSrc[FPos] = '{') then
  begin
    Inc(FPos);
    if not ReadBracedCodePoint(Result) then Fail('Invalid Unicode escape sequence', Start);
  end
  else if not ReadHexDigits(4, Result) then Fail('Invalid Unicode escape sequence', Start);
end;

procedure TScanner.ScanIdentSlow(Start: Integer; IsPrivate: Boolean);
var C: WideChar; CP: Cardinal; First: Boolean;
begin
  { the cooked name in Buf; FPos is back at the name's first code unit }
  BufLen := 0;
  First := True;
  while FPos < FLen do
  begin
    C := FSrc[FPos];
    if C = '\' then
    begin
      CP := ReadIdentEscape;
      Escaped := True;
      if First then
      begin
        if not (IsIdentifierStartCodePoint(CP) or (CP = Ord('$')) or (CP = Ord('_'))) then
          Fail('Invalid identifier start', Start);
      end
      else if not (IsIdentifierPartCodePoint(CP) or (CP = Ord('$')) or (CP = Ord('_')) or (CP = $200C) or
                   (CP = $200D)) then
        Fail('Invalid identifier part', Start);
      BufAddCP(CP);
    end
    else
    begin
      CP := Ord(C);
      if (CP >= $D800) and (CP <= $DBFF) and (FPos + 1 < FLen) and (Ord(FSrc[FPos + 1]) >= $DC00) and
         (Ord(FSrc[FPos + 1]) <= $DFFF) then
        CP := $10000 + ((CP - $D800) shl 10) + (Ord(FSrc[FPos + 1]) - $DC00);
      if First then
      begin
        if not (((CP < $80) and (Char(CP) in ['a'..'z', 'A'..'Z', '$', '_'])) or
                ((CP >= $80) and IsIdentifierStartCodePoint(CP))) then Break;
      end
      else if not (((CP < $80) and (Char(CP) in ['a'..'z', 'A'..'Z', '0'..'9', '$', '_'])) or
                   ((CP >= $80) and (IsIdentifierPartCodePoint(CP) or (CP = $200C) or (CP = $200D)))) then Break;
      BufAddCP(CP);
      if CP >= $10000 then Inc(FPos, 2) else Inc(FPos);
    end;
    First := False;
  end;
  if BufLen = 0 then Fail('Invalid or unexpected token', Start);
  Atom := AtomOfChars(@Buf[0], BufLen);
  if IsPrivate then Kind := tkPrivateName
  else if KeywordKind(Atom, Kind) then
  begin
    { an escaped reserved word is an identifier token the parser rejects where a keyword or identifier is
      expected (§12.7.2 static semantics); Escaped tells it }
    if Escaped then
    begin
      EscapedReserved := Kind;
      Kind := tkIdent;
    end;
  end
  else Kind := tkIdent;
end;

procedure TScanner.ScanIdentOrKeyword;
var Start: Integer; C: WideChar;
begin
  Start := FPos;
  while FPos < FLen do
  begin
    C := FSrc[FPos];
    if (CCls(C) and ccPart) <> 0 then Inc(FPos)
    else if (C = '\') or (Ord(C) >= $80) then
    begin
      FPos := Start;
      ScanIdentSlow(Start, False);
      Exit;
    end
    else Break;
  end;
  { a reserved word needs no atom (TokAtom makes one where it is a property name); an identifier is interned }
  Kind := KeywordOfChars(@FSrc[Start], FPos - Start);
  if Kind = tkIdent then Atom := AtomOfChars(@FSrc[Start], FPos - Start);
end;

function TScanner.TokAtom: PStr;
begin
  if Atom = nil then Atom := AtomOfChars(@FSrc[TokStart], TokEnd - TokStart);
  Result := Atom;
end;

{ ---- numbers (§12.9.3) ---- }

{ the conversions of a collected literal (ASCII in P[0..L-1]): out of ScanNumber, whose frame then holds no managed
  temporary }
function CharsToAnsi(P: PWideChar; L: Integer): AnsiString;
var I: Integer;
begin
  SetLength(Result, L);
  for I := 0 to L - 1 do Result[I + 1] := AnsiChar(Ord(P[I]));
end;

function RadixText(P: PWideChar; L, Radix: Integer): Double;
begin
  case Radix of
    16: Result := StringToNumber('0x' + string(CharsToAnsi(P, L)));
    8: Result := StringToNumber('0o' + string(CharsToAnsi(P, L)));
  else Result := StringToNumber('0b' + string(CharsToAnsi(P, L)));
  end;
end;

function DecimalText(P: PWideChar; L: Integer): Double;
begin
  if (L > 0) and (P[0] = '.') then Result := DecimalTextToNumber('0' + string(CharsToAnsi(P, L)))
  else Result := DecimalTextToNumber(string(CharsToAnsi(P, L)));
end;

procedure TScanner.ScanNumber;
var Start, I, P0: Integer; C, First: WideChar; Radix: Integer; IntV: Int64;
  Simple, SawDigit, PrevSep: Boolean;

  { the literal's characters without separators collect in Buf (a field: no managed local in this frame) }
  procedure Put(A: AnsiChar); inline;
  begin
    BufAdd(WideChar(Ord(A)));
  end;

  procedure Digits(AllowSep: Boolean; Lo, Hi: Char; HexLetters: Boolean);
  begin
    SawDigit := False;
    PrevSep := False;
    while FPos < FLen do
    begin
      C := FSrc[FPos];
      if C = '_' then
      begin
        if not AllowSep or not SawDigit or PrevSep then Fail('Invalid numeric separator', FPos);
        PrevSep := True;
        Inc(FPos);
        Continue;
      end;
      if not (((Ord(C) >= Ord(Lo)) and (Ord(C) <= Ord(Hi))) or
              (HexLetters and ((CCls(C) and ccHex) <> 0))) then Break;
      Put(AnsiChar(Ord(C)));
      SawDigit := True;
      PrevSep := False;
      Inc(FPos);
    end;
    if PrevSep then Fail('Numeric separators are not allowed at the end of numeric literals', FPos - 1);
  end;

  procedure Exponent;
  begin
    Put('e');
    Inc(FPos);
    if (FPos < FLen) and ((FSrc[FPos] = '+') or (FSrc[FPos] = '-')) then
    begin
      Put(AnsiChar(Ord(FSrc[FPos])));
      Inc(FPos);
    end;
    if (FPos < FLen) and (FSrc[FPos] = '_') then Fail('Invalid numeric separator', FPos);
    Digits(True, '0', '9', False);
    if not SawDigit then Fail('Invalid number', Start);
  end;

begin
  Start := FPos;
  LegacyOctal := False;
  BufLen := 0;
  Kind := tkNumber;
  C := FSrc[FPos];
  if (C = '0') and (FPos + 1 < FLen) and ((FSrc[FPos + 1] = 'x') or (FSrc[FPos + 1] = 'X') or (FSrc[FPos + 1] = 'o') or
     (FSrc[FPos + 1] = 'O') or (FSrc[FPos + 1] = 'b') or (FSrc[FPos + 1] = 'B')) then
  begin
    case FSrc[FPos + 1] of
      'x', 'X': Radix := 16;
      'o', 'O': Radix := 8;
    else Radix := 2;
    end;
    Inc(FPos, 2);
    case Radix of
      16: Digits(True, '0', '9', True);
      8: Digits(True, '0', '7', False);
    else Digits(True, '0', '1', False);
    end;
    if BufLen = 0 then Fail('Invalid number', Start);
    if (FPos < FLen) and (FSrc[FPos] = 'n') then
    begin
      { the BigInt's text: the prefix, then the digits }
      Inc(FPos);
      Kind := tkBigInt;
      P0 := BufLen;
      BufAdd('0');
      BufAdd('0');
      Move(Buf[0], Buf[2], P0 * SizeOf(WideChar));
      Buf[0] := '0';
      case Radix of 16: Buf[1] := 'x'; 8: Buf[1] := 'o'; else Buf[1] := 'b'; end;
    end
    else Num := RadixText(@Buf[0], BufLen, Radix);
  end
  else if (C = '0') and (FPos + 1 < FLen) and (IsDigitW(FSrc[FPos + 1]) or (FSrc[FPos + 1] = '_')) then
  begin
    { LegacyOctalIntegerLiteral or NonOctalDecimalIntegerLiteral (no separators, no BigInt suffix) }
    if FSrc[FPos + 1] = '_' then Fail('Numeric separator can not be used after leading 0', FPos + 1);
    LegacyOctal := True;
    Inc(FPos);
    Simple := True;   { all octal digits so far }
    while (FPos < FLen) and IsDigitW(FSrc[FPos]) do
    begin
      if (FSrc[FPos] = '8') or (FSrc[FPos] = '9') then Simple := False;
      Put(AnsiChar(Ord(FSrc[FPos])));
      Inc(FPos);
    end;
    if (FPos < FLen) and (FSrc[FPos] = '_') then Fail('Invalid numeric separator', FPos);
    if Simple then
    begin
      { 07.x is a member access on 7; an identifier character right after (07e1, 07n) fails below }
      Num := 0;
      for I := 0 to BufLen - 1 do Num := Num * 8 + (Ord(Buf[I]) - Ord('0'));
    end
    else
    begin
      { NonOctalDecimalIntegerLiteral: a decimal, which may have a fraction or exponent }
      if (FPos < FLen) and (FSrc[FPos] = '.') then
      begin
        Put('.');
        Inc(FPos);
        Digits(True, '0', '9', False);
      end;
      if (FPos < FLen) and ((FSrc[FPos] = 'e') or (FSrc[FPos] = 'E')) then Exponent;
      if (FPos < FLen) and (FSrc[FPos] = 'n') then Fail('Invalid BigInt literal', FPos);
      Num := DecimalText(@Buf[0], BufLen);
    end;
  end
  else
  begin
    { DecimalLiteral, DecimalBigIntegerLiteral (Digits reuses C: the first character is kept apart) }
    First := C;
    Simple := First <> '.';
    if First <> '.' then Digits(True, '0', '9', False);
    if (FPos < FLen) and (FSrc[FPos] = 'n') and Simple then
    begin
      { the BigInt's digits are in Buf already }
      Inc(FPos);
      Kind := tkBigInt;
    end
    else
    begin
      if (FPos < FLen) and (FSrc[FPos] = '.') then
      begin
        Simple := False;
        Put('.');
        Inc(FPos);
        if (FPos < FLen) and (FSrc[FPos] = '_') then Fail('Invalid numeric separator', FPos);
        Digits(True, '0', '9', False);
        if (First = '.') and not SawDigit then Fail('Invalid number', Start);
      end;
      if (FPos < FLen) and ((FSrc[FPos] = 'e') or (FSrc[FPos] = 'E')) then
      begin
        Simple := False;
        Exponent;
      end;
      if (FPos < FLen) and (FSrc[FPos] = 'n') then Fail('Invalid BigInt literal', FPos);
      if Simple and (BufLen <= 15) then
      begin
        IntV := 0;
        for I := 0 to BufLen - 1 do IntV := IntV * 10 + (Ord(Buf[I]) - Ord('0'));
        Num := IntV;
      end
      else Num := DecimalText(@Buf[0], BufLen);
    end;
  end;
  { §12.9.3: the SourceCharacter after a NumericLiteral is not an IdentifierStart or DecimalDigit }
  if FPos < FLen then
  begin
    C := FSrc[FPos];
    if ((CCls(C) and ccPart) <> 0) or (C = '\') or
       ((Ord(C) >= $80) and IsIdentifierStartCodePoint(Ord(C))) then
      Fail('Identifier starts immediately after numeric literal', FPos);
  end;
end;

{ ---- strings (§12.9.4) ---- }

procedure TScanner.ScanString(Quote: WideChar);
var Start: Integer; C: WideChar; V: Cardinal; D: Integer;
begin
  Start := FPos;
  Inc(FPos);
  BufLen := 0;
  LegacyOctal := False;
  while True do
  begin
    if FPos >= FLen then Fail('Unterminated string literal', Start);
    C := FSrc[FPos];
    if C = Quote then
    begin
      Inc(FPos);
      Break;
    end;
    if (C = #10) or (C = #13) then Fail('Unterminated string literal', Start);
    if C <> '\' then
    begin
      BufAdd(C);
      { U+2028 and U+2029 may appear in a string (§12.9.4); they still end a source line }
      if (C = LS) or (C = PS) then NewLine(1) else Inc(FPos);
      Continue;
    end;
    Inc(FPos);
    if FPos >= FLen then Fail('Unterminated string literal', Start);
    C := FSrc[FPos];
    Inc(FPos);
    case C of
      'n': BufAdd(#10);
      't': BufAdd(#9);
      'r': BufAdd(#13);
      'b': BufAdd(#8);
      'f': BufAdd(#12);
      'v': BufAdd(#11);
      #13:
        begin
          { LineContinuation }
          if (FPos < FLen) and (FSrc[FPos] = #10) then Inc(FPos);
          Inc(FLine);
          FLineStart := FPos;
        end;
      #10, LS, PS:
        begin
          Inc(FLine);
          FLineStart := FPos;
        end;
      'x':
        begin
          if not ReadHexDigits(2, V) then Fail('Invalid hexadecimal escape sequence', FPos - 2);
          BufAdd(WideChar(V));
        end;
      'u':
        begin
          if (FPos < FLen) and (FSrc[FPos] = '{') then
          begin
            Inc(FPos);
            if not ReadBracedCodePoint(V) then Fail('Invalid Unicode escape sequence', FPos);
            BufAddCP(V);
          end
          else
          begin
            if not ReadHexDigits(4, V) then Fail('Invalid Unicode escape sequence', FPos - 2);
            BufAdd(WideChar(V));
          end;
        end;
      '0'..'7':
        begin
          { \0 alone is NUL; otherwise a LegacyOctalEscapeSequence (Annex B; strict mode rejects it) }
          if (C = '0') and not ((FPos < FLen) and IsDigitW(FSrc[FPos])) then BufAdd(#0)
          else
          begin
            LegacyOctal := True;
            V := Ord(C) - Ord('0');
            D := 1;
            while (D < 3) and (FPos < FLen) and ((CCls(FSrc[FPos]) and ccOctal) <> 0) and (V * 8 + Cardinal(Ord(FSrc[FPos]) - Ord('0')) <= 255) do
            begin
              V := V * 8 + Cardinal(Ord(FSrc[FPos]) - Ord('0'));
              Inc(FPos);
              Inc(D);
            end;
            BufAdd(WideChar(V));
          end;
        end;
      '8', '9':
        begin
          { NonOctalDecimalEscapeSequence: rejected in strict mode like a legacy octal escape }
          LegacyOctal := True;
          BufAdd(C);
        end;
    else
      BufAdd(C);
    end;
  end;
  Kind := tkString;
end;

{ ---- templates (§12.9.6) ---- }

procedure TScanner.ScanTemplate(Continuation: Boolean);
var Start: Integer; C: WideChar; V: Cardinal; Bad: Boolean;
begin
  { at a backtick or at the right brace ending a substitution }
  Start := FPos;
  Inc(FPos);
  BufLen := 0;
  CookedValid := True;
  while True do
  begin
    if FPos >= FLen then Fail('Unterminated template literal', Start);
    C := FSrc[FPos];
    if C = '`' then
    begin
      Inc(FPos);
      if Continuation then Kind := tkTemplateTail else Kind := tkTemplate;
      Exit;
    end;
    if (C = '$') and (FPos + 1 < FLen) and (FSrc[FPos + 1] = '{') then
    begin
      Inc(FPos, 2);
      if Continuation then Kind := tkTemplateMiddle else Kind := tkTemplateHead;
      Exit;
    end;
    if C = #13 then
    begin
      { TV: CR and CRLF are LF }
      BufAdd(#10);
      if (FPos + 1 < FLen) and (FSrc[FPos + 1] = #10) then NewLine(2) else NewLine(1);
      Continue;
    end;
    if (C = #10) or (C = LS) or (C = PS) then
    begin
      BufAdd(C);
      NewLine(1);
      Continue;
    end;
    if C <> '\' then
    begin
      BufAdd(C);
      Inc(FPos);
      Continue;
    end;
    Inc(FPos);
    if FPos >= FLen then Fail('Unterminated template literal', Start);
    C := FSrc[FPos];
    Inc(FPos);
    Bad := False;
    case C of
      'n': BufAdd(#10);
      't': BufAdd(#9);
      'r': BufAdd(#13);
      'b': BufAdd(#8);
      'f': BufAdd(#12);
      'v': BufAdd(#11);
      #13:
        begin
          if (FPos < FLen) and (FSrc[FPos] = #10) then Inc(FPos);
          Inc(FLine);
          FLineStart := FPos;
        end;
      #10, LS, PS:
        begin
          Inc(FLine);
          FLineStart := FPos;
        end;
      'x':
        if ReadHexDigits(2, V) then BufAdd(WideChar(V)) else Bad := True;
      'u':
        if (FPos < FLen) and (FSrc[FPos] = '{') then
        begin
          Inc(FPos);
          if ReadBracedCodePoint(V) then BufAddCP(V) else Bad := True;
        end
        else if ReadHexDigits(4, V) then BufAdd(WideChar(V))
        else Bad := True;
      '0':
        if (FPos < FLen) and IsDigitW(FSrc[FPos]) then Bad := True else BufAdd(#0);
      '1'..'9': Bad := True;
    else
      BufAdd(C);
    end;
    { NotEscapeSequence: a template's cooked value is undefined (a tagged template's), the raw text stays }
    if Bad then CookedValid := False;
  end;
end;

{ ---- regular expressions (§12.9.5); the pattern itself is checked when it is compiled ---- }

procedure TScanner.ScanRegExp;
var Start, I, Seen, Bit: Integer; C: WideChar; InClass: Boolean;
begin
  Start := FPos;
  Inc(FPos);
  InClass := False;
  while True do
  begin
    if FPos >= FLen then Fail('Unterminated regular expression', Start);
    C := FSrc[FPos];
    if (C = #10) or (C = #13) or (C = LS) or (C = PS) then Fail('Unterminated regular expression', Start);
    if C = '\' then
    begin
      Inc(FPos);
      if FPos >= FLen then Fail('Unterminated regular expression', Start);
      C := FSrc[FPos];
      if (C = #10) or (C = #13) or (C = LS) or (C = PS) then Fail('Unterminated regular expression', Start);
      Inc(FPos);
      Continue;
    end;
    if C = '[' then InClass := True
    else if C = ']' then InClass := False
    else if (C = '/') and not InClass then Break;
    Inc(FPos);
  end;
  RegexBodyStart := Start + 1;
  RegexBodyLen := FPos - Start - 1;
  Inc(FPos);
  RegexFlagsStart := FPos;
  while FPos < FLen do
  begin
    C := FSrc[FPos];
    if ((CCls(C) and ccPart) <> 0) or
       ((Ord(C) >= $80) and IsIdentifierPartCodePoint(Ord(C))) then Inc(FPos)
    else if C = '\' then Fail('Invalid regular expression flags', FPos)
    else Break;
  end;
  RegexFlagsLen := FPos - RegexFlagsStart;
  { §22.2.3.1 early errors: the flags are among dgimsuvy, none twice, not both u and v }
  Seen := 0;
  for I := RegexFlagsStart to FPos - 1 do
  begin
    case FSrc[I] of
      'd': Bit := 1; 'g': Bit := 2; 'i': Bit := 4; 'm': Bit := 8;
      's': Bit := 16; 'u': Bit := 32; 'v': Bit := 64; 'y': Bit := 128;
    else
      Fail('Invalid regular expression flag', I);
    end;
    if (Seen and Bit) <> 0 then Fail('Duplicate regular expression flag', I);
    Seen := Seen or Bit;
  end;
  if (Seen and 96) = 96 then Fail('Regular expression flags u and v together', RegexFlagsStart);
  Kind := tkRegExp;
end;

{ ---- the next token ---- }

procedure TScanner.Next(Goal: TScanGoal);
var C, C1: WideChar;

  procedure Op(K: TTok; W: Integer); inline;
  begin
    Kind := K;
    Inc(FPos, W);
  end;

  function At(I: Integer): WideChar; inline;
  begin
    if FPos + I < FLen then Result := FSrc[FPos + I] else Result := #0;
  end;

begin
  PrevEnd := TokEnd;
  NewlineBefore := False;
  Escaped := False;
  EscapedReserved := tkIdent;
  Atom := nil;
  SkipTrivia;
  if (Goal = sgTemplateTail) and (FPos < FLen) and (FSrc[FPos] = '}') then
  begin
    TokStart := FPos;
    TokLine := FLine;
    TokCol := FPos - FLineStart + 1;
    ScanTemplate(True);
    TokEnd := FPos;
    FAtLineStart := False;
    Exit;
  end;
  TokStart := FPos;
  TokLine := FLine;
  TokCol := FPos - FLineStart + 1;
  if FPos >= FLen then
  begin
    Kind := tkEOF;
    TokEnd := FPos;
    Exit;
  end;
  C := FSrc[FPos];
  C1 := At(1);
  case C of
    'a'..'z', 'A'..'Z', '$', '_': ScanIdentOrKeyword;
    '\': ScanIdentSlow(FPos, False);
    '0'..'9': ScanNumber;
    '"', '''': ScanString(C);
    '`': ScanTemplate(False);
    '#':
      begin
        if ((CCls(C1) and ccStart) <> 0) or (C1 = '\') or (Ord(C1) >= $80) then
        begin
          Inc(FPos);
          ScanIdentSlow(FPos - 1, True);
        end
        else Fail('Invalid or unexpected token', FPos);
      end;
    '{': Op(tkLBrace, 1);
    '}': Op(tkRBrace, 1);
    '(': Op(tkLParen, 1);
    ')': Op(tkRParen, 1);
    '[': Op(tkLBracket, 1);
    ']': Op(tkRBracket, 1);
    ';': Op(tkSemi, 1);
    ',': Op(tkComma, 1);
    ':': Op(tkColon, 1);
    '~': Op(tkTilde, 1);
    '@': Op(tkAt, 1);
    '.':
      if IsDigitW(C1) then ScanNumber
      else if (C1 = '.') and (At(2) = '.') then Op(tkEllipsis, 3)
      else Op(tkDot, 1);
    '<':
      if C1 = '<' then
      begin
        if At(2) = '=' then Op(tkShlAssign, 3) else Op(tkShl, 2);
      end
      else if C1 = '=' then Op(tkLe, 2)
      else Op(tkLt, 1);
    '>':
      if C1 = '>' then
      begin
        if At(2) = '>' then
        begin
          if At(3) = '=' then Op(tkShrAssign, 4) else Op(tkShr, 3);
        end
        else if At(2) = '=' then Op(tkSarAssign, 3)
        else Op(tkSar, 2);
      end
      else if C1 = '=' then Op(tkGe, 2)
      else Op(tkGt, 1);
    '=':
      if C1 = '=' then
      begin
        if At(2) = '=' then Op(tkSEq, 3) else Op(tkEq, 2);
      end
      else if C1 = '>' then Op(tkArrow, 2)
      else Op(tkAssign, 1);
    '!':
      if C1 = '=' then
      begin
        if At(2) = '=' then Op(tkSNe, 3) else Op(tkNe, 2);
      end
      else Op(tkBang, 1);
    '+':
      if C1 = '+' then Op(tkInc, 2) else if C1 = '=' then Op(tkPlusAssign, 2) else Op(tkPlus, 1);
    '-':
      if C1 = '-' then Op(tkDec, 2) else if C1 = '=' then Op(tkMinusAssign, 2) else Op(tkMinus, 1);
    '*':
      if C1 = '*' then
      begin
        if At(2) = '=' then Op(tkStarStarAssign, 3) else Op(tkStarStar, 2);
      end
      else if C1 = '=' then Op(tkStarAssign, 2)
      else Op(tkStar, 1);
    '%':
      if C1 = '=' then Op(tkPercentAssign, 2) else Op(tkPercent, 1);
    '&':
      if C1 = '&' then
      begin
        if At(2) = '=' then Op(tkAndAndAssign, 3) else Op(tkAndAnd, 2);
      end
      else if C1 = '=' then Op(tkAmpAssign, 2)
      else Op(tkAmp, 1);
    '|':
      if C1 = '|' then
      begin
        if At(2) = '=' then Op(tkOrOrAssign, 3) else Op(tkOrOr, 2);
      end
      else if C1 = '=' then Op(tkBarAssign, 2)
      else Op(tkBar, 1);
    '^':
      if C1 = '=' then Op(tkCaretAssign, 2) else Op(tkCaret, 1);
    '?':
      if C1 = '?' then
      begin
        if At(2) = '=' then Op(tkQQAssign, 3) else Op(tkQQ, 2);
      end
      { OptionalChainingPunctuator: ?. not followed by a DecimalDigit }
      else if (C1 = '.') and not IsDigitW(At(2)) then Op(tkQDot, 2)
      else Op(tkQuestion, 1);
    '/':
      if Goal = sgRegExp then ScanRegExp
      else if C1 = '=' then Op(tkSlashAssign, 2)
      else Op(tkSlash, 1);
  else
    if (Ord(C) >= $80) and (IsIdentifierStartCodePoint(Ord(C)) or ((Ord(C) >= $D800) and (Ord(C) <= $DBFF))) then
      ScanIdentSlow(FPos, False)
    else Fail('Invalid or unexpected token', FPos);
  end;
  TokEnd := FPos;
  FAtLineStart := False;
end;

end.
