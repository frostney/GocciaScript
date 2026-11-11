program scancmp;

{ Differential check of G2.Scan against Goccia's lexer: both scan each file with the same lexical goals (chosen
  from the previous token, with a stack for template substitutions) and every token's kind, start and value are
  compared; one failing where the other does not is a mismatch too. Usage: scancmp [--module] FILE... (a
  development tool, not part of the G2 build). Prints one line per mismatching file and a summary. }

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math, G2.Core, G2.Scan, Goccia.Lexer, Goccia.Token, NumericText;

function DecodeUTF8(const Raw: RawByteString): UnicodeString;
begin
  Result := UTF8Decode(Raw);
end;

function ReadFileText(const FN: string): UnicodeString;
var FS: TFileStream; Raw: RawByteString;
begin
  FS := TFileStream.Create(FN, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Raw, FS.Size);
    if FS.Size > 0 then FS.ReadBuffer(Raw[1], FS.Size);
  finally
    FS.Free;
  end;
  if (Length(Raw) >= 3) and (Raw[1] = #$EF) and (Raw[2] = #$BB) and (Raw[3] = #$BF) then Delete(Raw, 1, 3);
  Result := DecodeUTF8(Raw);
end;

{ Goccia's token type for one of ours, by kind and (identifiers) text }
function ExpectedGoccia(S: TScanner): TGocciaTokenType;
var Name: UnicodeString;
begin
  case S.Kind of
    tkEOF: Result := gttEOF;
    tkIdent:
      begin
        Name := StrToUnicode(S.TokAtom);
        if S.Escaped then Result := gttIdentifier
        else if Name = 'let' then Result := gttLet
        else if Name = 'static' then Result := gttStatic
        else if Name = 'from' then Result := gttFrom
        else if Name = 'as' then Result := gttAs
        else Result := gttIdentifier;
      end;
    tkPrivateName: Result := gttHash;
    tkNumber: Result := gttNumber;
    tkBigInt: Result := gttBigInt;
    tkString: Result := gttString;
    tkTemplate: Result := gttTemplate;
    tkTemplateHead: Result := gttTemplateHead;
    tkTemplateMiddle: Result := gttTemplateMiddle;
    tkTemplateTail: Result := gttTemplateTail;
    tkRegExp: Result := gttRegex;
    tkLBrace: Result := gttLeftBrace; tkRBrace: Result := gttRightBrace;
    tkLParen: Result := gttLeftParen; tkRParen: Result := gttRightParen;
    tkLBracket: Result := gttLeftBracket; tkRBracket: Result := gttRightBracket;
    tkDot: Result := gttDot; tkEllipsis: Result := gttSpread; tkSemi: Result := gttSemicolon;
    tkComma: Result := gttComma; tkLt: Result := gttLess; tkGt: Result := gttGreater;
    tkLe: Result := gttLessEqual; tkGe: Result := gttGreaterEqual; tkEq: Result := gttLooseEqual;
    tkNe: Result := gttLooseNotEqual; tkSEq: Result := gttEqual; tkSNe: Result := gttNotEqual;
    tkPlus: Result := gttPlus; tkMinus: Result := gttMinus; tkStar: Result := gttStar;
    tkPercent: Result := gttPercent; tkStarStar: Result := gttPower; tkInc: Result := gttIncrement;
    tkDec: Result := gttDecrement; tkShl: Result := gttLeftShift; tkSar: Result := gttRightShift;
    tkShr: Result := gttUnsignedRightShift; tkAmp: Result := gttBitwiseAnd; tkBar: Result := gttBitwiseOr;
    tkCaret: Result := gttBitwiseXor; tkBang: Result := gttNot; tkTilde: Result := gttBitwiseNot;
    tkAndAnd: Result := gttAnd; tkOrOr: Result := gttOr; tkQQ: Result := gttNullishCoalescing;
    tkQuestion: Result := gttQuestion; tkQDot: Result := gttOptionalChaining; tkColon: Result := gttColon;
    tkAssign: Result := gttAssign; tkPlusAssign: Result := gttPlusAssign; tkMinusAssign: Result := gttMinusAssign;
    tkStarAssign: Result := gttStarAssign; tkPercentAssign: Result := gttPercentAssign;
    tkStarStarAssign: Result := gttPowerAssign; tkShlAssign: Result := gttLeftShiftAssign;
    tkSarAssign: Result := gttRightShiftAssign; tkShrAssign: Result := gttUnsignedRightShiftAssign;
    tkAmpAssign: Result := gttBitwiseAndAssign; tkBarAssign: Result := gttBitwiseOrAssign;
    tkCaretAssign: Result := gttBitwiseXorAssign; tkAndAndAssign: Result := gttLogicalAndAssign;
    tkOrOrAssign: Result := gttLogicalOrAssign; tkQQAssign: Result := gttNullishCoalescingAssign;
    tkArrow: Result := gttArrow; tkSlash: Result := gttSlash; tkSlashAssign: Result := gttSlashAssign;
    tkAt: Result := gttAt;
    tkConst: Result := gttConst; tkClass: Result := gttClass; tkEnum: Result := gttEnum;
    tkExtends: Result := gttExtends; tkNew: Result := gttNew; tkThis: Result := gttThis;
    tkSuper: Result := gttSuper; tkReturn: Result := gttReturn; tkIf: Result := gttIf; tkElse: Result := gttElse;
    tkFor: Result := gttFor; tkWhile: Result := gttWhile; tkDo: Result := gttDo; tkSwitch: Result := gttSwitch;
    tkCase: Result := gttCase; tkDefault: Result := gttDefault; tkBreak: Result := gttBreak;
    tkContinue: Result := gttContinue; tkThrow: Result := gttThrow; tkTry: Result := gttTry;
    tkCatch: Result := gttCatch; tkFinally: Result := gttFinally; tkImport: Result := gttImport;
    tkExport: Result := gttExport; tkVar: Result := gttVar; tkWith: Result := gttWith;
    tkFunction: Result := gttFunction; tkTrue: Result := gttTrue; tkFalse: Result := gttFalse;
    tkNull: Result := gttNull; tkTypeof: Result := gttTypeof; tkVoid: Result := gttVoid;
    tkInstanceof: Result := gttInstanceof; tkIn: Result := gttIn; tkDelete: Result := gttDelete;
    tkDebugger: Result := gttIdentifier;
  else
    Result := gttEOF;
  end;
end;

function EndsExpression(K: TTok): Boolean;
begin
  Result := K in [tkIdent, tkPrivateName, tkNumber, tkBigInt, tkString, tkTemplate, tkTemplateTail, tkRegExp,
    tkRParen, tkRBracket, tkRBrace, tkThis, tkSuper, tkNull, tkTrue, tkFalse, tkInc, tkDec];
end;

var
  Mismatches, Files, Tokens: Integer;

procedure CheckFile(const FN: string; Module: Boolean);
var Src: UnicodeString; L: TGocciaLexer; S: TScanner; T: TGocciaToken; G: TScanGoal; GG: TGocciaLexicalGoal;
  Stack: array of Integer; SP, N: Integer; Prev: TTok; First: Boolean; MyErr, GErr, Msg: string; Q: Integer;
  GText, MText: UnicodeString;

  procedure Mismatch(const Why: string);
  begin
    Inc(Mismatches);
    WriteLn(FN, ': token ', N, ' at ', S.TokLine, ':', S.TokCol, ': ', Why);
  end;

begin
  Inc(Files);
  try
    Src := ReadFileText(FN);
  except
    on E: Exception do begin WriteLn(FN, ': read failed: ', E.Message); Exit; end;
  end;
  L := TGocciaLexer.Create(Src, FN);
  S := TScanner.Create(PWideChar(Src), Length(Src));
  try
    L.HTMLComments := not Module;
    L.StandardIdentifiers := True;
    S.HTMLComments := not Module;
    SetLength(Stack, 0);
    SP := 0;
    Prev := tkEOF;
    First := True;
    N := 0;
    while True do
    begin
      Inc(N);
      { the goal: a right brace closing a substitution continues the template; a slash is a RegExp where no
        expression ended }
      Q := S.Save.Pos;
      while (Q < S.Len) and ((S.Src[Q] = ' ') or (S.Src[Q] = #9) or (S.Src[Q] = #10) or (S.Src[Q] = #13)) do Inc(Q);
      if (SP > 0) and (Stack[SP - 1] = 0) and (Q < S.Len) and (S.Src[Q] = '}') then
      begin
        G := sgTemplateTail;
        GG := glgInputElementTemplateTail;
      end
      else if First then
      begin
        G := sgRegExp;
        GG := glgInputElementHashbangOrRegExp;
      end
      else if EndsExpression(Prev) then
      begin
        G := sgDiv;
        GG := glgInputElementDiv;
      end
      else
      begin
        G := sgRegExp;
        GG := glgInputElementRegExp;
      end;
      First := False;
      MyErr := '';
      GErr := '';
      try
        S.Next(G);
      except
        on E: Exception do MyErr := E.Message;
      end;
      try
        T := L.ScanNextToken(GG);
      except
        on E: Exception do GErr := E.Message;
      end;
      if (MyErr <> '') or (GErr <> '') then
      begin
        if (MyErr = '') or (GErr = '') then
          Mismatch(Format('error differs: g2 "%s" goccia "%s"', [MyErr, GErr]));
        Break;
      end;
      Inc(Tokens);
      if (S.Kind = tkPrivateName) and (T^.TokenType = gttHash) then
      begin
        { Goccia: '#' and the name as two tokens }
        try
          T := L.ScanNextToken(glgInputElementDiv);
        except
          on E: Exception do begin Mismatch('goccia failed on a private name: ' + E.Message); Break; end;
        end;
        if StrToUnicode(S.Atom) <> T^.Lexeme then
        begin
          Mismatch(Format('private name g2 %s goccia %s', [string(StrToUnicode(S.Atom)), string(T^.Lexeme)]));
          Break;
        end;
        Prev := S.Kind;
        Continue;
      end;
      if T^.TokenType <> ExpectedGoccia(S) then
      begin
        Mismatch(Format('kind g2 %s (%s) goccia %d (%s)', [TokName(S.Kind), string(S.TokText), Ord(T^.TokenType),
          string(T^.Lexeme)]));
        Break;
      end;
      if not (S.Kind in [tkEOF, tkTemplateMiddle, tkTemplateTail]) and ((T^.StartLine <> S.TokLine) or (T^.Column <> S.TokCol)) then
      begin
        Mismatch(Format('position g2 %d:%d goccia %d:%d (%s)', [S.TokLine, S.TokCol, T^.StartLine, T^.Column,
          string(S.TokText)]));
        Break;
      end;
      { values }
      case S.Kind of
        tkIdent, tkPrivateName:
          if StrToUnicode(S.Atom) <> T^.Lexeme then
          begin
            Mismatch(Format('name g2 %s goccia %s', [string(StrToUnicode(S.Atom)), string(T^.Lexeme)]));
            Break;
          end;
        tkString:
          if S.BufText <> T^.Lexeme then
          begin
            Mismatch(Format('string value differs (%s)', [string(S.TokText)]));
            Break;
          end;
        tkTemplate, tkTemplateHead, tkTemplateMiddle, tkTemplateTail:
          begin
          { Goccia keeps a continuation's leading right brace in its values (its parser drops it) }
          GText := T^.TemplateValue.Cooked;
          MText := T^.TemplateValue.Raw;
          if S.Kind in [tkTemplateMiddle, tkTemplateTail] then
          begin
            { and, scanned from before the brace, the whitespace in front of it }
            if Pos(UnicodeString('}'), MText) > 0 then Delete(MText, 1, Pos(UnicodeString('}'), MText));
            if Pos(UnicodeString('}'), GText) > 0 then Delete(GText, 1, Pos(UnicodeString('}'), GText));
          end;
          if (S.CookedValid <> T^.TemplateValue.CookedValid) or
             (S.CookedValid and (S.BufText <> GText)) or (S.TemplateRaw <> MText) then
          begin
            Mismatch(Format('template value differs (%s): g2 valid %s cooked [%s] raw [%s]; goccia valid %s cooked [%s] raw [%s]',
              [string(S.TokText), BoolToStr(S.CookedValid, True), string(S.BufText), string(S.TemplateRaw),
               BoolToStr(T^.TemplateValue.CookedValid, True), string(T^.TemplateValue.Cooked), string(T^.TemplateValue.Raw)]));
            Break;
          end;
          end;
        tkRegExp:
          begin
            { Goccia's lexeme is the pattern and the flags, without the slashes }
            SetLength(MText, S.RegexBodyLen + S.RegexFlagsLen);
            if S.RegexBodyLen > 0 then Move(S.Src[S.RegexBodyStart], MText[1], S.RegexBodyLen * 2);
            if S.RegexFlagsLen > 0 then Move(S.Src[S.RegexFlagsStart], MText[S.RegexBodyLen + 1], S.RegexFlagsLen * 2);
            { Goccia's regex lexeme is the pattern length, a NUL, the pattern and the flags }
            MText := UnicodeString(IntToStr(S.RegexBodyLen)) + #0 + MText;
            GText := T^.Lexeme;
            if MText <> GText then
            begin
              Mismatch(Format('regexp g2 %s goccia %s', [string(MText), string(GText)]));
              Break;
            end;
          end;
      end;
      { template substitution stack: 0 a substitution, 1 a plain brace }
      case S.Kind of
        tkTemplateHead:
          begin
            if SP >= Length(Stack) then SetLength(Stack, SP * 2 + 8);
            Stack[SP] := 0;
            Inc(SP);
          end;
        tkTemplateTail:
          Dec(SP);
        tkLBrace:
          begin
            if SP >= Length(Stack) then SetLength(Stack, SP * 2 + 8);
            Stack[SP] := 1;
            Inc(SP);
          end;
        tkRBrace:
          if SP > 0 then Dec(SP);
      end;
      if S.Kind = tkEOF then Break;
      Prev := S.Kind;
    end;
  finally
    S.Free;
    L.Free;
  end;
end;

{ --bench FILE: each scanner alone over the file, 5 times (goals as in CheckFile, without the comparison) }
procedure Bench(const FN: string);
var Src: UnicodeString; S: TScanner; L: TGocciaLexer; R, N, Q, SP: Integer; T0, TG2, TGoccia: QWord; Prev: TTok;
  T: TGocciaToken; Stack: array of Integer; G: TScanGoal; GG: TGocciaLexicalGoal; Goals: array of TScanGoal;
begin
  Src := ReadFileText(FN);
  { the goals, from one G2 scan with the substitution stack; then both scanners replay them }
  S := TScanner.Create(PWideChar(Src), Length(Src));
  Prev := tkEOF; SP := 0; SetLength(Stack, 0); N := 0; SetLength(Goals, 0);
  repeat
    Q := S.Save.Pos;
    while (Q < S.Len) and ((S.Src[Q] = ' ') or (S.Src[Q] = #9) or (S.Src[Q] = #10) or (S.Src[Q] = #13)) do Inc(Q);
    if (SP > 0) and (Stack[SP - 1] = 0) and (Q < S.Len) and (S.Src[Q] = '}') then G := sgTemplateTail
    else if (N = 0) or not EndsExpression(Prev) then G := sgRegExp else G := sgDiv;
    S.Next(G);
    if N >= Length(Goals) then SetLength(Goals, N * 2 + 1024);
    Goals[N] := G;
    Inc(N);
    case S.Kind of
      tkTemplateHead, tkLBrace:
        begin
          if SP >= Length(Stack) then SetLength(Stack, SP * 2 + 8);
          if S.Kind = tkTemplateHead then Stack[SP] := 0 else Stack[SP] := 1;
          Inc(SP);
        end;
      tkTemplateTail, tkRBrace: if SP > 0 then Dec(SP);
    end;
    Prev := S.Kind;
  until S.Kind = tkEOF;
  S.Free;
  TG2 := 0;
  TGoccia := 0;
  for R := 1 to 5 do
  begin
    T0 := GetTickCount64;
    S := TScanner.Create(PWideChar(Src), Length(Src));
    for Q := 0 to N - 1 do S.Next(Goals[Q]);
    S.Free;
    Inc(TG2, GetTickCount64 - T0);
    T0 := GetTickCount64;
    L := TGocciaLexer.Create(Src, FN);
    L.HTMLComments := True;
    L.StandardIdentifiers := True;
    for Q := 0 to N - 1 do
    begin
      case Goals[Q] of
        sgTemplateTail: GG := glgInputElementTemplateTail;
        sgDiv: GG := glgInputElementDiv;
      else if Q = 0 then GG := glgInputElementHashbangOrRegExp else GG := glgInputElementRegExp;
      end;
      T := L.ScanNextToken(GG);
      if T^.TokenType = gttHash then L.ScanNextToken(glgInputElementDiv);
    end;
    L.Free;
    Inc(TGoccia, GetTickCount64 - T0);
  end;
  WriteLn(Format('%s: %d tokens, %d chars; G2.Scan %.1f ms, Goccia lexer %.1f ms (mean of 5), %.2fx',
    [ExtractFileName(FN), N, Length(Src), TG2 / 5, TGoccia / 5, TGoccia / Max(1, TG2)]));
end;

var I: Integer; Module, DoBench: Boolean; Start: QWord;
begin
  Module := False;
  DoBench := False;
  Start := GetTickCount64;
  for I := 1 to ParamCount do
    if ParamStr(I) = '--module' then Module := True
    else if ParamStr(I) = '--bench' then DoBench := True
    else if DoBench then Bench(ParamStr(I))
    else CheckFile(ParamStr(I), Module);
  WriteLn(Format('files %d, tokens %d, mismatching files %d, %d ms', [Files, Tokens, Mismatches,
    GetTickCount64 - Start]));
end.
