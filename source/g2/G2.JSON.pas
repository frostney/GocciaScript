unit G2.JSON;
{ 25.5 The JSON object: JSON.parse (25.5.1, with the JSON.parse source text access proposal's reviver context),
  JSON.stringify (25.5.2), JSON.rawJSON and JSON.isRawJSON (the same proposal).

  parse reads the JSON grammar (ECMA-404) straight from the source text. With a callable reviver it also keeps a
  parse record per value (its source span, and the records of its elements or entries), the values themselves in
  a rooted array since the collector moves them. stringify appends to one growable buffer; a property whose
  value serializes to undefined is cut back off it. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{ FPC 3.3.1's data flow analysis does not terminate on this unit at -O3 }
{$OPTIMIZATION NODFA}

interface

procedure InitJSON;

implementation

uses
  SysUtils, Math, NumericText, G2.Core, G2.VM, G2.ObjModel, G2.BigIntV;

const
  okRawJSON = 28;   { 25.5.3 an object with [[IsRawJSON]]: frozen, null prototype, its rawJSON property }
  MAX_DEPTH = 10000;

type
  TArgs = PValue;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

{ 7.2.2 IsArray }
function IsArrayV(V: TValue; var R: Boolean): Boolean;
begin
  R := False;
  while IsObject(V) do
  begin
    if ObjOf(V)^.OKind = okArray then begin R := True; Break; end;
    if ObjOf(V)^.OKind <> okProxy then Break;
    if not IsObject(ObjOf(V)^.Aux2) then
      Exit(ThrowTypeError('Cannot perform ''IsArray'' on a proxy that has been revoked'));
    V := ObjOf(V)^.Aux1;
  end;
  Result := True;
end;

{ 7.3.23 EnumerableOwnProperties(O, key): the string keys, through a proxy's traps }
function EnumerableKeys(O: TValue; var Res: TValue): Boolean;
var Keys, KV: TValue; I, N: Integer; D: TPropDesc; Found: Boolean;
begin
  if ObjOf(O)^.OKind <> okProxy then
  begin
    Res := OwnKeys(ObjOf(O), True);
    if OwnKeysFailed then begin OwnKeysFailed := False; Exit(False); end;
    Exit(True);
  end;
  PushRoot(O);
  if not OwnPropertyKeysV(O, Keys) then begin PopRoots(1); Exit(False); end;
  PushRoot(Keys);
  PushRoot(MkPtr(NewArray(0, 4)));
  N := ArrayLen(ObjOf(RootAt(TempTop - 2)));
  for I := 0 to N - 1 do
  begin
    KV := ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I];
    if not IsString(KV) then Continue;
    if not GetOwnPropertyV(RootAt(TempTop - 3), Atomize(StrOf(KV)), D, Found) then begin PopRoots(3); Exit(False); end;
    if Found and D.Enumerable then
      ArrayPush(ObjOf(RootAt(TempTop - 1)), ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I]);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(3);
  Result := True;
end;

{ 7.3.5 CreateDataProperty: False (no exception) when the object refuses }
function CreateDataProp(O: TValue; Key: PStr; V: TValue): Boolean;
var D: TPropDesc; Ok: Boolean;
begin
  DataDesc(D, V, True, True, True);
  Result := DefineOwnPropertyV(O, Key, D, Ok);
end;

{ O.[[Delete]](Key), its result ignored }
function DeleteProp(O: TValue; Key: PStr): Boolean;
var Ok: Boolean;
begin
  if ObjOf(O)^.OKind = okProxy then Exit(ProxyDelete(O, Key, Ok));
  DeleteIfConfigurable(ObjOf(O), Key);
  Result := True;
end;

{ ---------------- parse ---------------- }

type
  TJRec = class
    ValIdx: Integer;          { its value: the parser's rooted value array }
    SrcStart, SrcLen: Integer;
    IsObj: Boolean;
    Elements: array of TJRec;
    Keys: array of UnicodeString;
    Entries: array of TJRec;
    destructor Destroy; override;
  end;

  TJParser = record
    S: UnicodeString;
    P, N: Integer;            { P: the next character (1-based); N: the length }
    Recs: Boolean;            { keep parse records (a reviver will run) }
    RecVals: Integer;         { TempRoots index of the record value array }
    Depth: Integer;
  end;

destructor TJRec.Destroy;
var I: Integer;
begin
  for I := 0 to High(Elements) do Elements[I].Free;
  for I := 0 to High(Entries) do Entries[I].Free;
  inherited;
end;

function JErr(var J: TJParser; const What: UnicodeString): Boolean;
begin
  if J.P > J.N then Result := ThrowError('SyntaxError', 'Unexpected end of JSON input')
  else Result := ThrowError('SyntaxError', What + ' in JSON at position ' + UnicodeString(IntToStr(J.P - 1)));
end;

procedure JWs(var J: TJParser); inline;
begin
  while (J.P <= J.N) and ((J.S[J.P] = ' ') or (J.S[J.P] = #9) or (J.S[J.P] = #10) or (J.S[J.P] = #13)) do Inc(J.P);
end;

function HexDigit(C: WideChar): Integer; inline;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'a'..'f': Result := Ord(C) - Ord('a') + 10;
    'A'..'F': Result := Ord(C) - Ord('A') + 10;
  else Result := -1;
  end;
end;

{ a JSON string at J.P (the opening quote) }
function JString(var J: TJParser; var Res: UnicodeString): Boolean;
var Start, I, H, K, Code: Integer; C: WideChar; Buf: UnicodeString; BL: Integer;
begin
  Inc(J.P);
  Start := J.P;
  { no escape: one copy }
  I := J.P;
  while (I <= J.N) and (J.S[I] <> '"') and (J.S[I] <> '\') and (Ord(J.S[I]) >= $20) do Inc(I);
  if (I <= J.N) and (J.S[I] = '"') then
  begin
    Res := Copy(J.S, Start, I - Start);
    J.P := I + 1;
    Exit(True);
  end;
  SetLength(Buf, J.N - Start + 1);
  BL := I - Start;
  if BL > 0 then Move(J.S[Start], Buf[1], BL * SizeOf(WideChar));
  J.P := I;
  while True do
  begin
    if J.P > J.N then Exit(JErr(J, 'Unterminated string'));
    C := J.S[J.P];
    if C = '"' then Break;
    if Ord(C) < $20 then Exit(JErr(J, 'Bad control character in string literal'));
    if C = '\' then
    begin
      Inc(J.P);
      if J.P > J.N then Exit(JErr(J, 'Bad escaped character'));
      case J.S[J.P] of
        '"': C := '"';
        '\': C := '\';
        '/': C := '/';
        'b': C := #8;
        'f': C := #12;
        'n': C := #10;
        'r': C := #13;
        't': C := #9;
        'u':
          begin
            Code := 0;
            for K := 1 to 4 do
            begin
              if J.P + K > J.N then begin J.P := J.N + 1; Exit(JErr(J, 'Bad Unicode escape')); end;
              H := HexDigit(J.S[J.P + K]);
              if H < 0 then begin J.P := J.P + K; Exit(JErr(J, 'Bad Unicode escape')); end;
              Code := Code * 16 + H;
            end;
            Inc(J.P, 4);
            C := WideChar(Code);
          end;
      else
        Exit(JErr(J, 'Bad escaped character'));
      end;
    end;
    Inc(BL);
    Buf[BL] := C;
    Inc(J.P);
  end;
  Inc(J.P);
  SetLength(Buf, BL);
  Res := Buf;
  Result := True;
end;

function JNumber(var J: TJParser; var Res: TValue): Boolean;
var Start, IntVal, Digits: Integer; Neg, Simple: Boolean;
begin
  Start := J.P;
  Neg := False;
  if J.S[J.P] = '-' then begin Neg := True; Inc(J.P); end;
  if J.P > J.N then Exit(JErr(J, 'No number after minus sign'));
  IntVal := 0;
  Digits := 0;
  Simple := True;
  if J.S[J.P] = '0' then Inc(J.P)
  else if (J.S[J.P] >= '1') and (J.S[J.P] <= '9') then
    while (J.P <= J.N) and (J.S[J.P] >= '0') and (J.S[J.P] <= '9') do
    begin
      if Digits < 9 then IntVal := IntVal * 10 + Ord(J.S[J.P]) - Ord('0');
      Inc(Digits);
      Inc(J.P);
    end
  else Exit(JErr(J, 'Unexpected token'));
  if (J.P <= J.N) and (J.S[J.P] = '.') then
  begin
    Simple := False;
    Inc(J.P);
    if (J.P > J.N) or (J.S[J.P] < '0') or (J.S[J.P] > '9') then Exit(JErr(J, 'Unterminated fractional number'));
    while (J.P <= J.N) and (J.S[J.P] >= '0') and (J.S[J.P] <= '9') do Inc(J.P);
  end;
  if (J.P <= J.N) and ((J.S[J.P] = 'e') or (J.S[J.P] = 'E')) then
  begin
    Simple := False;
    Inc(J.P);
    if (J.P <= J.N) and ((J.S[J.P] = '+') or (J.S[J.P] = '-')) then Inc(J.P);
    if (J.P > J.N) or (J.S[J.P] < '0') or (J.S[J.P] > '9') then Exit(JErr(J, 'Exponent part is missing a number'));
    while (J.P <= J.N) and (J.S[J.P] >= '0') and (J.S[J.P] <= '9') do Inc(J.P);
  end;
  if Simple and (Digits <= 9) and not (Neg and (IntVal = 0)) then
  begin
    if Neg then Res := MkInt(-IntVal) else Res := MkInt(IntVal);
  end
  else Res := MkNumber(StringToNumber(Copy(J.S, Start, J.P - Start)));
  Result := True;
end;

function JLiteral(var J: TJParser; const Word: UnicodeString): Boolean;
var I: Integer;
begin
  for I := 1 to Length(Word) do
  begin
    if (J.P > J.N) or (J.S[J.P] <> Word[I]) then Exit(JErr(J, 'Unexpected token'));
    Inc(J.P);
  end;
  Result := True;
end;

function NewRec(var J: TJParser; Start: Integer; V: TValue): TJRec;
begin
  Result := TJRec.Create;
  Result.SrcStart := Start;
  Result.SrcLen := J.P - Start;
  Result.ValIdx := ArrayLen(ObjOf(TempRoots[J.RecVals]));
  ArrayPush(ObjOf(TempRoots[J.RecVals]), V);
end;

{ a JSON value at J.P (white space before it skipped); Rec its parse record when J.Recs }
function JValue(var J: TJParser; var Res: TValue; out Rec: TJRec): Boolean;
var Start: Integer; T: UnicodeString; V: TValue; K: PStr; Sub: TJRec; Elems, Ents: array of TJRec;
  Keys: array of UnicodeString; NE: Integer;
  procedure FreeSubs;
  var X: Integer;
  begin
    for X := 0 to NE - 1 do
      if Elems <> nil then Elems[X].Free else Ents[X].Free;
  end;
begin
  Rec := nil;
  JWs(J);
  if J.P > J.N then Exit(JErr(J, 'Unexpected end'));
  Start := J.P;
  case J.S[J.P] of
    '{':
      begin
        if J.Depth >= MAX_DEPTH then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
        Inc(J.Depth);
        Inc(J.P);
        PushRoot(MkPtr(NewPlainObject(ObjectProto, 4)));
        NE := 0;
        Elems := nil;
        JWs(J);
        if (J.P <= J.N) and (J.S[J.P] = '}') then Inc(J.P)
        else
          while True do
          begin
            JWs(J);
            if (J.P > J.N) or (J.S[J.P] <> '"') then begin FreeSubs; PopRoots(1); Exit(JErr(J, 'Expected property name')); end;
            if not JString(J, T) then begin FreeSubs; PopRoots(1); Exit(False); end;
            JWs(J);
            if (J.P > J.N) or (J.S[J.P] <> ':') then begin FreeSubs; PopRoots(1); Exit(JErr(J, 'Expected '':'' after property name')); end;
            Inc(J.P);
            if not JValue(J, V, Sub) then begin FreeSubs; PopRoots(1); Exit(False); end;
            K := AtomOf(T);
            { 25.5.1.1: CreateDataProperty, the later of two equal names winning (__proto__ is an own property) }
            DefineOwn(RootAt(TempTop - 1), K, V, pfDefault);
            if J.Recs then
            begin
              SetLength(Ents, NE + 1); SetLength(Keys, NE + 1);
              Ents[NE] := Sub; Keys[NE] := T;
              Inc(NE);
            end;
            JWs(J);
            if (J.P <= J.N) and (J.S[J.P] = ',') then begin Inc(J.P); Continue; end;
            if (J.P <= J.N) and (J.S[J.P] = '}') then begin Inc(J.P); Break; end;
            FreeSubs;
            PopRoots(1);
            Exit(JErr(J, 'Expected '','' or ''}'' after property value'));
          end;
        Res := RootAt(TempTop - 1);
        PopRoots(1);
        Dec(J.Depth);
        if J.Recs then
        begin
          Rec := NewRec(J, Start, Res);
          Rec.IsObj := True;
          Rec.Entries := Ents;
          Rec.Keys := Keys;
        end;
        Exit(True);
      end;
    '[':
      begin
        if J.Depth >= MAX_DEPTH then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
        Inc(J.Depth);
        Inc(J.P);
        PushRoot(MkPtr(NewArray(0, 4)));
        NE := 0;
        Ents := nil;
        JWs(J);
        if (J.P <= J.N) and (J.S[J.P] = ']') then Inc(J.P)
        else
          while True do
          begin
            if not JValue(J, V, Sub) then begin FreeSubs; PopRoots(1); Exit(False); end;
            ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
            if J.Recs then
            begin
              SetLength(Elems, NE + 1);
              Elems[NE] := Sub;
              Inc(NE);
            end;
            JWs(J);
            if (J.P <= J.N) and (J.S[J.P] = ',') then begin Inc(J.P); Continue; end;
            if (J.P <= J.N) and (J.S[J.P] = ']') then begin Inc(J.P); Break; end;
            FreeSubs;
            PopRoots(1);
            Exit(JErr(J, 'Expected '','' or '']'' after array element'));
          end;
        Res := RootAt(TempTop - 1);
        PopRoots(1);
        Dec(J.Depth);
        if J.Recs then
        begin
          Rec := NewRec(J, Start, Res);
          Rec.Elements := Elems;
        end;
        Exit(True);
      end;
    '"':
      begin
        if not JString(J, T) then Exit(False);
        Res := StrFromUnicode(T);
      end;
    't': begin if not JLiteral(J, 'true') then Exit(False); Res := VTrue; end;
    'f': begin if not JLiteral(J, 'false') then Exit(False); Res := VFalse; end;
    'n': begin if not JLiteral(J, 'null') then Exit(False); Res := VNull; end;
    '-', '0'..'9': if not JNumber(J, Res) then Exit(False);
  else
    Exit(JErr(J, 'Unexpected token'));
  end;
  if J.Recs then Rec := NewRec(J, Start, Res);
  Result := True;
end;

threadvar
  JSrc: UnicodeString;   { the text a reviver's parse records point into }

{ the JSON.parse source text access proposal's InternalizeJSONProperty(holder, name, reviver, parseRecord) }
function Internalize(Holder: TValue; Name: PStr; Reviver: TValue; Rec: TJRec; RecVals: Integer;
  var Res: TValue): Boolean;
var Val, Ctx, NewEl: TValue; IsArr: Boolean; Len, I: Int64; Keys: TValue; K: PStr; N, X: Integer;
  Elems, Ents: array of TJRec; EKeys: array of UnicodeString; Sub: TJRec; CA: array[0..2] of TValue;
  KS: UnicodeString;
begin
  PushRoot(Holder); PushRoot(Reviver);
  if not GetV(Holder, Name, Val) then begin PopRoots(2); Exit(False); end;
  PushRoot(Val);
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 2)));   { the context }
  Elems := nil; Ents := nil; EKeys := nil;
  if (Rec <> nil) and SameValueV(ObjOf(TempRoots[RecVals])^.Elements^.Data[Rec.ValIdx], RootAt(TempTop - 2)) then
  begin
    if not IsObject(RootAt(TempTop - 2)) then
      DefineOwn(RootAt(TempTop - 1), AtomOf('source'), StrFromUnicode(Copy(JSrc, Rec.SrcStart, Rec.SrcLen)), pfDefault);
    Elems := Rec.Elements;
    Ents := Rec.Entries;
    EKeys := Rec.Keys;
  end;
  Val := RootAt(TempTop - 2);
  if IsObject(Val) then
  begin
    if not IsArrayV(Val, IsArr) then begin PopRoots(4); Exit(False); end;
    if IsArr then
    begin
      if not LengthOfArrayLike(RootAt(TempTop - 2), Len) then begin PopRoots(4); Exit(False); end;
      I := 0;
      while I < Len do
      begin
        K := IntAtom(I);
        if I <= High(Elems) then Sub := Elems[I] else Sub := nil;
        if not Internalize(RootAt(TempTop - 2), K, RootAt(TempTop - 3), Sub, RecVals, NewEl) then begin PopRoots(4); Exit(False); end;
        if NewEl = VUndefined then
        begin
          if not DeleteProp(RootAt(TempTop - 2), K) then begin PopRoots(4); Exit(False); end;
        end
        else if not CreateDataProp(RootAt(TempTop - 2), K, NewEl) then begin PopRoots(4); Exit(False); end;
        Inc(I);
      end;
    end
    else
    begin
      if not EnumerableKeys(RootAt(TempTop - 2), Keys) then begin PopRoots(4); Exit(False); end;
      PushRoot(Keys);
      N := ArrayLen(ObjOf(Keys));
      for I := 0 to N - 1 do
      begin
        K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
        Sub := nil;
        if EKeys <> nil then
        begin
          KS := StrToUnicode(K);
          for X := High(EKeys) downto 0 do
            if EKeys[X] = KS then begin Sub := Ents[X]; Break; end;
        end;
        if not Internalize(RootAt(TempTop - 3), K, RootAt(TempTop - 4), Sub, RecVals, NewEl) then begin PopRoots(5); Exit(False); end;
        if NewEl = VUndefined then
        begin
          if not DeleteProp(RootAt(TempTop - 3), K) then begin PopRoots(5); Exit(False); end;
        end
        else if not CreateDataProp(RootAt(TempTop - 3), K, NewEl) then begin PopRoots(5); Exit(False); end;
      end;
      PopRoots(1);
    end;
  end;
  CA[0] := MkPtr(Name);
  CA[1] := RootAt(TempTop - 2);
  CA[2] := RootAt(TempTop - 1);
  Result := CallValue(RootAt(TempTop - 3), RootAt(TempTop - 4), @CA[0], 3, Res);
  PopRoots(4);
end;

{ 25.5.1 JSON.parse(text, reviver) }
function JSON_Parse(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var SV, V: TValue; J: TJParser; Rec: TJRec; Saved: UnicodeString;
begin
  if not ToStringV(Arg(Args, Argc, 1), SV) then Exit(False);
  J.S := StrToUnicode(StrOf(SV));
  J.P := 1;
  J.N := Length(J.S);
  J.Depth := 0;
  J.Recs := IsCallable(Arg(Args, Argc, 2));
  PushRoot(Arg(Args, Argc, 2));
  PushRoot(MkPtr(NewArray(0, 4)));
  J.RecVals := TempTop - 1;
  Rec := nil;
  try
    if not JValue(J, V, Rec) then begin PopRoots(2); Exit(False); end;
    JWs(J);
    if J.P <= J.N then begin PopRoots(2); Exit(JErr(J, 'Unexpected non-whitespace character after JSON')); end;
    if not J.Recs then begin Res := V; PopRoots(2); Exit(True); end;
    { 25.5.1 step 11: the root's holder }
    PushRoot(V);
    PushRoot(MkPtr(NewPlainObject(ObjectProto, 2)));
    DefineOwn(RootAt(TempTop - 1), AtomOf(''), RootAt(TempTop - 2), pfDefault);
    Saved := JSrc;
    JSrc := J.S;
    try
      Result := Internalize(RootAt(TempTop - 1), AtomOf(''), RootAt(TempTop - 4), Rec, J.RecVals, Res);
    finally
      JSrc := Saved;
    end;
    PopRoots(4);
  finally
    Rec.Free;
  end;
end;

{ ---------------- stringify ---------------- }

type
  TJOut = record
    Buf: UnicodeString;
    Len: Integer;
    Replacer: Integer;        { TempRoots index of the replacer function, -1 none }
    Stack: Integer;           { TempRoots index of the array of objects being serialized }
    PropList: array of PStr;
    HasPropList: Boolean;
    Gap, Indent: UnicodeString;
    Depth: Integer;
  end;

procedure Put(var O: TJOut; const S: UnicodeString); inline;
var L: Integer;
begin
  L := Length(S);
  if L = 0 then Exit;
  if O.Len + L > Length(O.Buf) then SetLength(O.Buf, (O.Len + L) * 2 + 64);
  Move(S[1], O.Buf[O.Len + 1], L * SizeOf(WideChar));
  Inc(O.Len, L);
end;

procedure PutC(var O: TJOut; C: WideChar); inline;
begin
  if O.Len + 1 > Length(O.Buf) then SetLength(O.Buf, O.Len * 2 + 64);
  Inc(O.Len);
  O.Buf[O.Len] := C;
end;

const
  HexDig: array[0..15] of WideChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

procedure PutU(var O: TJOut; C: Integer);
begin
  PutC(O, '\'); PutC(O, 'u');
  PutC(O, HexDig[(C shr 12) and 15]); PutC(O, HexDig[(C shr 8) and 15]);
  PutC(O, HexDig[(C shr 4) and 15]); PutC(O, HexDig[C and 15]);
end;

{ 25.5.2.3 QuoteJSONString: lone surrogates escaped }
procedure Quote(var O: TJOut; S: PWideChar; N: Integer);
var I, Run: Integer; C: WideChar;
begin
  PutC(O, '"');
  I := 0;
  while I < N do
  begin
    Run := I;
    while (Run < N) and (Ord(S[Run]) >= $20) and (S[Run] <> '"') and (S[Run] <> '\') and
          ((Ord(S[Run]) < $D800) or (Ord(S[Run]) > $DFFF)) do Inc(Run);
    if Run > I then
    begin
      if O.Len + (Run - I) > Length(O.Buf) then SetLength(O.Buf, (O.Len + Run - I) * 2 + 64);
      Move(S[I], O.Buf[O.Len + 1], (Run - I) * SizeOf(WideChar));
      Inc(O.Len, Run - I);
      I := Run;
      if I >= N then Break;
    end;
    C := S[I];
    case C of
      '"': begin PutC(O, '\'); PutC(O, '"'); end;
      '\': begin PutC(O, '\'); PutC(O, '\'); end;
      #8: begin PutC(O, '\'); PutC(O, 'b'); end;
      #9: begin PutC(O, '\'); PutC(O, 't'); end;
      #10: begin PutC(O, '\'); PutC(O, 'n'); end;
      #12: begin PutC(O, '\'); PutC(O, 'f'); end;
      #13: begin PutC(O, '\'); PutC(O, 'r'); end;
    else
      if Ord(C) < $20 then PutU(O, Ord(C))
      else if (Ord(C) <= $DBFF) and (I + 1 < N) and (Ord(S[I + 1]) >= $DC00) and (Ord(S[I + 1]) <= $DFFF) then
      begin
        PutC(O, C);
        PutC(O, S[I + 1]);
        Inc(I);
      end
      else PutU(O, Ord(C));   { a lone surrogate }
    end;
    Inc(I);
  end;
  PutC(O, '"');
end;

procedure QuoteStr(var O: TJOut; S: PStr);
begin
  Quote(O, StrChars(S), S^.Len);
end;

function SerializeProp(var O: TJOut; Key: PStr; Holder: TValue; var Skip: Boolean): Boolean; forward;

function InStack(var O: TJOut; V: TValue): Boolean;
var A: PObj; I: Integer;
begin
  A := ObjOf(TempRoots[O.Stack]);
  for I := 0 to ArrayLen(A) - 1 do
    if A^.Elements^.Data[I] = V then Exit(True);
  Result := False;
end;

procedure NewLine(var O: TJOut); inline;
begin
  PutC(O, #10);
  Put(O, O.Indent);
end;

{ 25.5.2.6 SerializeJSONObject }
function SerializeObject(var O: TJOut; V: TValue): Boolean;
var Keys: TValue; I, N, Mark: Integer; Stepback: UnicodeString; Any, Skip: Boolean; K: PStr;
begin
  if InStack(O, V) then Exit(ThrowTypeError('Converting circular structure to JSON'));
  if O.Depth >= MAX_DEPTH then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  Inc(O.Depth);
  PushRoot(V);
  ArrayPush(ObjOf(TempRoots[O.Stack]), V);
  Stepback := O.Indent;
  O.Indent := O.Indent + O.Gap;
  if O.HasPropList then begin Keys := VUndefined; N := Length(O.PropList); end
  else
  begin
    if not EnumerableKeys(RootAt(TempTop - 1), Keys) then begin PopRoots(1); Exit(False); end;
    N := ArrayLen(ObjOf(Keys));
  end;
  PushRoot(Keys);
  PutC(O, '{');
  Any := False;
  for I := 0 to N - 1 do
  begin
    if O.HasPropList then K := O.PropList[I]
    else K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
    Mark := O.Len;
    if Any then PutC(O, ',');
    if O.Gap <> '' then NewLine(O);
    QuoteStr(O, K);
    PutC(O, ':');
    if O.Gap <> '' then PutC(O, ' ');
    if not SerializeProp(O, K, RootAt(TempTop - 2), Skip) then begin PopRoots(2); Exit(False); end;
    if Skip then O.Len := Mark else Any := True;
  end;
  if Any and (O.Gap <> '') then
  begin
    PutC(O, #10);
    Put(O, Stepback);
  end;
  PutC(O, '}');
  O.Indent := Stepback;
  with ObjOf(TempRoots[O.Stack])^ do Dec(Elements^.Len);
  PopRoots(2);
  Dec(O.Depth);
  Result := True;
end;

{ 25.5.2.7 SerializeJSONArray }
function SerializeArray(var O: TJOut; V: TValue): Boolean;
var Len, I: Int64; Stepback: UnicodeString; Skip: Boolean;
begin
  if InStack(O, V) then Exit(ThrowTypeError('Converting circular structure to JSON'));
  if O.Depth >= MAX_DEPTH then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  Inc(O.Depth);
  PushRoot(V);
  ArrayPush(ObjOf(TempRoots[O.Stack]), V);
  Stepback := O.Indent;
  O.Indent := O.Indent + O.Gap;
  if not LengthOfArrayLike(RootAt(TempTop - 1), Len) then begin PopRoots(1); Exit(False); end;
  PutC(O, '[');
  I := 0;
  while I < Len do
  begin
    if I > 0 then PutC(O, ',');
    if O.Gap <> '' then NewLine(O);
    if not SerializeProp(O, IntAtom(I), RootAt(TempTop - 1), Skip) then begin PopRoots(1); Exit(False); end;
    if Skip then Put(O, 'null');
    Inc(I);
  end;
  if (Len > 0) and (O.Gap <> '') then
  begin
    PutC(O, #10);
    Put(O, Stepback);
  end;
  PutC(O, ']');
  O.Indent := Stepback;
  with ObjOf(TempRoots[O.Stack])^ do Dec(Elements^.Len);
  PopRoots(1);
  Dec(O.Depth);
  Result := True;
end;

{ 25.5.2.2 SerializeJSONProperty(state, key, holder); Skip when it is undefined }
function SerializeProp(var O: TJOut; Key: PStr; Holder: TValue; var Skip: Boolean): Boolean;
var V, F, S: TValue; CA: array[0..1] of TValue; D: Double; IsArr: Boolean; Ob: PObj;
begin
  Skip := False;
  PushRoot(Holder);
  if not GetV(Holder, Key, V) then begin PopRoots(1); Exit(False); end;
  if IsObject(V) or IsBigInt(V) then
  begin
    PushRoot(V);
    if not GetV(V, AtomOf('toJSON'), F) then begin PopRoots(2); Exit(False); end;
    if IsCallable(F) then
    begin
      CA[0] := MkPtr(Key);
      if not CallValue(F, RootAt(TempTop - 1), @CA[0], 1, V) then begin PopRoots(2); Exit(False); end;
    end
    else V := RootAt(TempTop - 1);
    PopRoots(1);
  end;
  if O.Replacer >= 0 then
  begin
    CA[0] := MkPtr(Key);
    CA[1] := V;
    if not CallValue(TempRoots[O.Replacer], RootAt(TempTop - 1), @CA[0], 2, V) then begin PopRoots(1); Exit(False); end;
  end;
  PopRoots(1);
  if IsObject(V) then
  begin
    Ob := ObjOf(V);
    case Ob^.OKind of
      okNumber: begin if not ToNumberV(V, D) then Exit(False); V := MkNumber(D); end;
      okStringObj: begin if not ToStringV(V, S) then Exit(False); V := S; end;
      okBoolean: V := Ob^.Aux1;
      okRawJSON:
        begin
          if not GetV(V, AtomOf('rawJSON'), S) then Exit(False);
          if IsString(S) then Put(O, StrToUnicode(StrOf(S)));
          Exit(True);
        end;
    else
      if (Ob^.OKind = okPlain) and IsBigInt(Ob^.Aux1) then V := Ob^.Aux1;   { a BigInt wrapper }
    end;
  end;
  if V = VNull then begin Put(O, 'null'); Exit(True); end;
  if V = VTrue then begin Put(O, 'true'); Exit(True); end;
  if V = VFalse then begin Put(O, 'false'); Exit(True); end;
  if IsString(V) then begin QuoteStr(O, StrOf(V)); Exit(True); end;
  if IsInt(V) then begin Put(O, UnicodeString(IntToStr(IntOf(V)))); Exit(True); end;
  if IsNumber(V) then
  begin
    D := NumOf(V);
    if IsNaN(D) or IsInfinite(D) then Put(O, 'null')
    else Put(O, StrToUnicode(StrOf(NumberToStr(D))));
    Exit(True);
  end;
  if IsBigInt(V) then Exit(ThrowTypeError('Do not know how to serialize a BigInt'));
  if IsObject(V) and not IsCallable(V) then
  begin
    if not IsArrayV(V, IsArr) then Exit(False);
    if IsArr then Exit(SerializeArray(O, V));
    Exit(SerializeObject(O, V));
  end;
  Skip := True;
  Result := True;
end;

{ 25.5.2 JSON.stringify(value, replacer, space) }
function JSON_Stringify(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: TJOut; R, Sp, V: TValue; IsArr, Skip, Dup: Boolean; Len, I: Int64; D: Double; K: PStr;
  X, N: Integer; S: UnicodeString;
begin
  O.Len := 0;
  O.Replacer := -1;
  O.HasPropList := False;
  O.Gap := '';
  O.Indent := '';
  O.Depth := 0;
  SetLength(O.Buf, 64);
  R := Arg(Args, Argc, 2);
  PushRoot(Arg(Args, Argc, 1));
  PushRoot(R);
  PushRoot(Arg(Args, Argc, 3));
  N := 3;
  if IsCallable(R) then O.Replacer := TempTop - 2
  else if IsObject(R) then
  begin
    if not IsArrayV(R, IsArr) then begin PopRoots(N); Exit(False); end;
    if IsArr then
    begin
      { the property list: strings and numbers (and their wrappers), each once, in order }
      O.HasPropList := True;
      if not LengthOfArrayLike(RootAt(TempTop - 2), Len) then begin PopRoots(N); Exit(False); end;
      I := 0;
      while I < Len do
      begin
        if not GetV(RootAt(TempTop - 2), IntAtom(I), V) then begin PopRoots(N); Exit(False); end;
        K := nil;
        if IsString(V) then K := Atomize(StrOf(V))
        else if IsNumber(V) or (IsObject(V) and (ObjOf(V)^.OKind in [okNumber, okStringObj])) then
        begin
          if not ToStringV(V, V) then begin PopRoots(N); Exit(False); end;
          K := Atomize(StrOf(V));
        end;
        if K <> nil then
        begin
          Dup := False;
          for X := 0 to High(O.PropList) do if O.PropList[X] = K then begin Dup := True; Break; end;
          if not Dup then
          begin
            SetLength(O.PropList, Length(O.PropList) + 1);
            O.PropList[High(O.PropList)] := K;
          end;
        end;
        Inc(I);
      end;
    end;
  end;
  Sp := RootAt(TempTop - 1);
  if IsObject(Sp) then
  begin
    if ObjOf(Sp)^.OKind = okNumber then
    begin
      if not ToNumberV(Sp, D) then begin PopRoots(N); Exit(False); end;
      Sp := MkNumber(D);
    end
    else if ObjOf(Sp)^.OKind = okStringObj then
    begin
      if not ToStringV(Sp, V) then begin PopRoots(N); Exit(False); end;
      Sp := V;
    end;
  end;
  if IsNumber(Sp) then
  begin
    D := NumOf(Sp);
    if IsNaN(D) then D := 0;
    if D > 10 then D := 10;
    if D >= 1 then O.Gap := UnicodeString(StringOfChar(' ', Trunc(D)));
  end
  else if IsString(Sp) then
  begin
    S := StrToUnicode(StrOf(Sp));
    O.Gap := Copy(S, 1, 10);
  end;
  PushRoot(MkPtr(NewArray(0, 8)));
  Inc(N);
  O.Stack := TempTop - 1;
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 1)));
  Inc(N);
  DefineOwn(RootAt(TempTop - 1), AtomOf(''), RootAt(TempTop - 5), pfDefault);
  if not SerializeProp(O, AtomOf(''), RootAt(TempTop - 1), Skip) then begin PopRoots(N); Exit(False); end;
  PopRoots(N);
  if Skip then Res := VUndefined
  else Res := StrFromUnicode(Copy(O.Buf, 1, O.Len));
  Result := True;
end;

{ ---------------- rawJSON / isRawJSON ---------------- }

{ JSON.rawJSON(text): a frozen null-prototype object whose rawJSON is the text of one JSON primitive }
function JSON_RawJSON(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var SV, V: TValue; J: TJParser; Rec: TJRec; S: UnicodeString; Ob: PObj; Ok: Boolean;
begin
  if not ToStringV(Arg(Args, Argc, 1), SV) then Exit(False);
  S := StrToUnicode(StrOf(SV));
  if (S = '') or CharInSet(S[1], [#9, #10, #13, ' ']) or CharInSet(S[Length(S)], [#9, #10, #13, ' ']) then
    Exit(ThrowError('SyntaxError', 'Invalid value for JSON.rawJSON'));
  if (S[1] = '{') or (S[1] = '[') then Exit(ThrowError('SyntaxError', 'Invalid value for JSON.rawJSON'));
  PushRoot(SV);
  J.S := S;
  J.P := 1;
  J.N := Length(S);
  J.Depth := 0;
  J.Recs := False;
  if not JValue(J, V, Rec) then begin PopRoots(1); Exit(False); end;
  if J.P <= J.N then begin PopRoots(1); Exit(JErr(J, 'Unexpected non-whitespace character after JSON')); end;
  Ob := NewObject(RootShapeFor(VNull), 1, okRawJSON);
  PushRoot(MkPtr(Ob));
  DefineOwn(RootAt(TempTop - 1), AtomOf('rawJSON'), RootAt(TempTop - 2), pfDefault);
  if not SetIntegrityLevel(RootAt(TempTop - 1), True, Ok) then begin PopRoots(2); Exit(False); end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function JSON_IsRawJSON(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  Res := MkBool(IsObject(V) and (ObjOf(V)^.OKind = okRawJSON));
  Result := True;
end;

{ JSONTextHook: ParseJSONModule's JSON.parse of the source text }
function JSONText(const S: UnicodeString; var Res: TValue): Boolean;
var A: array[0..1] of TValue;
begin
  A[0] := VUndefined;
  A[1] := StrFromUnicode(S);
  PushRoot(A[1]);
  A[1] := RootAt(TempTop - 1);
  Result := JSON_Parse(@A[0], 1, Res);
  PopRoots(1);
end;

procedure InitJSON;
var JS: TValue;
begin
  JSONTextHook := @JSONText;
  if not GetV(GlobalObj, AtomOf('JSON'), JS) then Exit;
  PushRoot(JS);
  DefineNative(RootAt(TempTop - 1), 'parse', JSON_Parse, 2);
  DefineNative(RootAt(TempTop - 1), 'stringify', JSON_Stringify, 3);
  DefineNative(RootAt(TempTop - 1), 'rawJSON', JSON_RawJSON, 1);
  DefineNative(RootAt(TempTop - 1), 'isRawJSON', JSON_IsRawJSON, 1);
  PopRoots(1);
end;

end.
