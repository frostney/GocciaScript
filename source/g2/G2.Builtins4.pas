unit G2.Builtins4;
{ The remaining standard library the tooling bundles reach: number formatting, Math, URI coding,
  Date setters, eval, and a JS prelude for the parts that compose existing built-ins. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

procedure InitBuiltins4;

implementation

uses
  DateUtils,
  Math,
  SysUtils,

  BigInteger,
  G2.Builtins,
  G2.Bytecode,
  G2.Compiler,
  G2.Core,
  G2.Eval,
  G2.Snapshot,
  G2.VM;

type
  TArgs = PValue;

threadvar
  Scratch: TValue;

function Arg(AArgs: TArgs; AArgc, I: Integer): TValue; inline;
begin
  if I <= AArgc then Result := AArgs[I] else Result := VUndefined;
end;

function NumArg(AArgs: TArgs; AArgc, I: Integer; var D: Double): Boolean;
begin
  Result := ToNumberV(Arg(AArgs, AArgc, I), D);
end;

function ThisNumber(AArgs: TArgs; var D: Double): Boolean;
begin
  if IsNumber(AArgs[0]) then begin D := NumOf(AArgs[0]); Exit(True); end;
  if IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind = okNumber) then begin D := NumOf(ObjOf(AArgs[0])^.Aux1); Exit(True); end;
  Result := ThrowTypeError('Number.prototype method called on incompatible receiver');
end;

{ ---------------- Number.prototype.toExponential ---------------- }

{ decimal digits and exponent of |D|: D = 0.d1d2... x 10^(E+1), digits from the shortest round trip }
procedure ShortestDigits(D: Double; out ADigits: UnicodeString; out E: Integer);
var S: UnicodeString; P, I, Exp10, DotPos: Integer; Mant: UnicodeString;
begin
  S := StrToUnicode(StrOf(NumberToStr(Abs(D))));
  Exp10 := 0;
  P := Pos('e', S);
  if P > 0 then
  begin
    Exp10 := StrToInt(string(Copy(S, P + 1, MaxInt)));
    Mant := Copy(S, 1, P - 1);
  end
  else Mant := S;
  DotPos := Pos('.', Mant);
  if DotPos = 0 then DotPos := Length(Mant) + 1
  else Delete(Mant, DotPos, 1);
  { strip leading zeros, moving the point }
  I := 1;
  while (I < Length(Mant)) and (Mant[I] = '0') do Inc(I);
  ADigits := Copy(Mant, I, MaxInt);
  E := DotPos - 1 - I + Exp10;   { exponent of the first digit }
  while (Length(ADigits) > 1) and (ADigits[Length(ADigits)] = '0') do Delete(ADigits, Length(ADigits), 1);
end;

{ the exact decimal expansion of D > 0: D = 0.Dig x 10^(E+1), so Dig[1] is the 10^E digit }
procedure ExactDecimal(D: Double; out ADig: string; out E: Integer);
var Bits: QWord; BE: Integer; M: Int64; N: TBigInteger;
begin
  Bits := PQWord(@D)^;
  BE := Integer((Bits shr 52) and $7FF);
  M := Int64(Bits and ((QWord(1) shl 52) - 1));
  if BE = 0 then BE := -1074 else begin M := M or (Int64(1) shl 52); BE := BE - 1075; end;
  if BE >= 0 then
  begin
    N := TBigInteger.FromInt64(M).Multiply(TBigInteger.FromInt64(2).Power(TBigInteger.FromInt64(BE)));
    ADig := N.ToString;
    E := Length(ADig) - 1;
  end
  else
  begin
    N := TBigInteger.FromInt64(M).Multiply(TBigInteger.FromInt64(5).Power(TBigInteger.FromInt64(-BE)));
    ADig := N.ToString;
    E := Length(ADig) - 1 + BE;
  end;
  while (Length(ADig) > 1) and (ADig[Length(ADig)] = '0') do Delete(ADig, Length(ADig), 1);
end;

{ the first Keep digits of Dig rounded half up (the larger n on a tie); Carry when it became 10..0 }
function RoundDigits(const ADig: string; AKeep: Integer; out ACarry: Boolean): string;
var I: Integer; Up: Boolean;
begin
  ACarry := False;
  if AKeep <= 0 then
  begin
    { everything is below the kept place: 1 at that place or nothing }
    ACarry := (AKeep = 0) and (ADig[1] >= '5');
    Exit('');
  end;
  if AKeep >= Length(ADig) then Exit(ADig + StringOfChar('0', AKeep - Length(ADig)));
  Result := Copy(ADig, 1, AKeep);
  Up := ADig[AKeep + 1] >= '5';
  if not Up then Exit;
  I := AKeep;
  while (I >= 1) and (Result[I] = '9') do begin Result[I] := '0'; Dec(I); end;
  if I >= 1 then Result[I] := Chr(Ord(Result[I]) + 1)
  else begin Result := '1' + Result; ACarry := True; end;
end;

function NumProto_ToExponential(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D, FD: Double; F, E: Integer; Digits: UnicodeString; S, Sign, ExpStr: UnicodeString; Dig, R: string; Carry: Boolean;
begin
  if not ThisNumber(AArgs, D) then Exit(False);
  if Arg(AArgs, AArgc, 1) = VUndefined then F := -1
  else
  begin
    if not NumArg(AArgs, AArgc, 1, FD) then Exit(False);
    if IsNaN(FD) then FD := 0;
    F := Trunc(FD);
  end;
  if IsNaN(D) or IsInfinite(D) then begin ARes := NumberToStr(D); Exit(True); end;
  if (F <> -1) and ((F < 0) or (F > 100)) then Exit(ThrowError('RangeError', 'toExponential() argument must be between 0 and 100'));
  Sign := '';
  if D < 0 then begin Sign := '-'; D := -D; end;
  if D = 0 then
  begin
    S := '0';
    if F > 0 then S := S + '.' + StringOfChar('0', F);
    ARes := StrFromUnicode(Sign + S + 'e+0');
    Exit(True);
  end;
  if F = -1 then
  begin
    ShortestDigits(D, Digits, E);
    S := Digits[1];
    if Length(Digits) > 1 then S := S + '.' + Copy(Digits, 2, MaxInt);
  end
  else
  begin
    ExactDecimal(D, Dig, E);
    R := RoundDigits(Dig, F + 1, Carry);
    if Carry then begin Inc(E); R := Copy(R, 1, F + 1); end;
    S := UnicodeString(R[1]);
    if F > 0 then S := S + '.' + UnicodeString(Copy(R, 2, MaxInt));
  end;
  if E < 0 then ExpStr := 'e-' + UnicodeString(IntToStr(-E)) else ExpStr := 'e+' + UnicodeString(IntToStr(E));
  ARes := StrFromUnicode(Sign + S + ExpStr);
  Result := True;
end;

{ Number.prototype.toFixed: round(x * 10^f) from the exact expansion }
function NumProto_ToFixed4(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D, FD: Double; F, E, Keep: Integer; Dig, N: string; Carry: Boolean; Sign: UnicodeString;
begin
  if not ThisNumber(AArgs, D) then Exit(False);
  FD := 0;
  if Arg(AArgs, AArgc, 1) <> VUndefined then
    if not NumArg(AArgs, AArgc, 1, FD) then Exit(False);
  if IsNaN(FD) then FD := 0;
  F := Trunc(FD);
  if (F < 0) or (F > 100) then Exit(ThrowError('RangeError', 'toFixed() digits argument must be between 0 and 100'));
  if IsNaN(D) or IsInfinite(D) or (Abs(D) >= 1e21) then begin ARes := NumberToStr(D); Exit(True); end;
  Sign := '';
  if D < 0 then begin Sign := '-'; D := -D; end;
  if D = 0 then N := '0'
  else
  begin
    ExactDecimal(D, Dig, E);
    Keep := E + 1 + F;   { digits of round(x * 10^f) }
    N := RoundDigits(Dig, Keep, Carry);
    if Keep <= 0 then
    begin
      if Carry then N := '1' else N := '0';
    end;
  end;
  if Length(N) <= F then N := StringOfChar('0', F + 1 - Length(N)) + N;
  if F > 0 then N := Copy(N, 1, Length(N) - F) + '.' + Copy(N, Length(N) - F + 1, F);
  ARes := StrFromUnicode(Sign + UnicodeString(N));
  Result := True;
end;

{ ---------------- Boolean.prototype ---------------- }

function ThisBool(AArgs: TArgs; var B: TValue): Boolean;
begin
  if (AArgs[0] = VTrue) or (AArgs[0] = VFalse) then begin B := AArgs[0]; Exit(True); end;
  if IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind = okBoolean) then begin B := ObjOf(AArgs[0])^.Aux1; Exit(True); end;
  Result := ThrowTypeError('Boolean.prototype method called on incompatible receiver');
end;

function BoolProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: TValue;
begin
  if not ThisBool(AArgs, B) then Exit(False);
  if B = VTrue then ARes := MkPtr(AtomOf('true')) else ARes := MkPtr(AtomOf('false'));
  Result := True;
end;

function BoolProto_ValueOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  Result := ThisBool(AArgs, ARes);
end;

{ ---------------- Math ---------------- }

function MathFn(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AWhich: Integer): Boolean;
var X: Double;
begin
  if not NumArg(AArgs, AArgc, 1, X) then Exit(False);
  case AWhich of
    0: if Abs(X) < 1e-5 then X := X - X * X / 2 + X * X * X / 3 else X := Ln(1 + X);   { log1p }
    1: if Abs(X) < 1e-5 then X := X + X * X / 2 + X * X * X / 6 else X := Exp(X) - 1;   { expm1 }
    2: X := Math.Cosh(X);
    3: if X = 0 then X := X else X := Math.Sinh(X);
    4: if X = 0 then X := X else if X > 20 then X := 1 else if X < -20 then X := -1 else X := Math.Tanh(X);
    5: X := Math.ArcCosh(X);
    6: if X = 0 then X := X else X := Math.ArcSinh(X);
    7: if X = 0 then X := X else X := Math.ArcTanh(X);
    8: X := Single(X);   { fround }
    9: X := Ln(X) / Ln(2);   { fallback log2 }
  end;
  ARes := MkNumber(X);
  Result := True;
end;
function M_Log1p(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 0); end;
function M_Expm1(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 1); end;
function M_Cosh(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 2); end;
function M_Sinh(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 3); end;
function M_Tanh(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 4); end;
function M_Acosh(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 5); end;
function M_Asinh(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 6); end;
function M_Atanh(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 7); end;
function M_Fround(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MathFn(AArgs, AArgc, ARes, 8); end;

function ToUint32D(D: Double): UInt32;
begin
  if IsNaN(D) or IsInfinite(D) then Exit(0);
  D := Int(D);
  D := D - DFloor(D / 4294967296.0) * 4294967296.0;
  Result := UInt32(Trunc(D));
end;

function M_Imul(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A, B: Double;
begin
  if not NumArg(AArgs, AArgc, 1, A) then Exit(False);
  if not NumArg(AArgs, AArgc, 2, B) then Exit(False);
  ARes := MkInt(Int32(UInt32(QWord(ToUint32D(A)) * QWord(ToUint32D(B)) and $FFFFFFFF)));
  Result := True;
end;

function M_Clz32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: Double; U: UInt32; N: Integer;
begin
  if not NumArg(AArgs, AArgc, 1, A) then Exit(False);
  U := ToUint32D(A);
  N := 32;
  while U <> 0 do begin U := U shr 1; Dec(N); end;
  ARes := MkInt(N);
  Result := True;
end;

{ ---------------- URI coding (ECMA-262 Encode / Decode) ---------------- }

const
  URIUnreservedMarks = '-_.!~*''()';
  URIReserved = ';/?:@&=+$,#';

function URIEncode(const S: UnicodeString; const AExtra: UnicodeString; var ARes: TValue): Boolean;
var I, CP: Integer; C: WideChar; Out_, Hex: UnicodeString; U8: RawByteString; J: Integer;
begin
  Out_ := '';
  I := 1;
  while I <= Length(S) do
  begin
    C := S[I];
    if ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z')) or ((C >= '0') and (C <= '9')) or
       (Pos(C, URIUnreservedMarks) > 0) or (Pos(C, AExtra) > 0) then
    begin
      Out_ := Out_ + C;
      Inc(I);
      Continue;
    end;
    CP := Ord(C);
    if (CP >= $DC00) and (CP <= $DFFF) then Exit(ThrowError('URIError', 'URI malformed'));
    if (CP >= $D800) and (CP <= $DBFF) then
    begin
      if (I = Length(S)) or (Ord(S[I + 1]) < $DC00) or (Ord(S[I + 1]) > $DFFF) then Exit(ThrowError('URIError', 'URI malformed'));
      U8 := UTF8Encode(Copy(S, I, 2));
      Inc(I, 2);
    end
    else
    begin
      U8 := UTF8Encode(UnicodeString(C));
      Inc(I);
    end;
    for J := 1 to Length(U8) do
    begin
      Hex := UnicodeString(IntToHex(Ord(U8[J]), 2));
      Out_ := Out_ + '%' + Hex;
    end;
  end;
  ARes := StrFromUnicode(Out_);
  Result := True;
end;

function HexVal(C: WideChar): Integer;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'a'..'f': Result := Ord(C) - Ord('a') + 10;
    'A'..'F': Result := Ord(C) - Ord('A') + 10;
  else Result := -1;
  end;
end;

function URIDecode(const S: UnicodeString; const AKeep: UnicodeString; var ARes: TValue): Boolean;
var I, B, N, K, Start: Integer; Out_: UnicodeString; Bytes: RawByteString; Dec_: UnicodeString;
begin
  Out_ := '';
  I := 1;
  while I <= Length(S) do
  begin
    if S[I] <> '%' then begin Out_ := Out_ + S[I]; Inc(I); Continue; end;
    Start := I;
    if (I + 2 > Length(S)) or (HexVal(S[I + 1]) < 0) or (HexVal(S[I + 2]) < 0) then Exit(ThrowError('URIError', 'URI malformed'));
    B := HexVal(S[I + 1]) * 16 + HexVal(S[I + 2]);
    Inc(I, 3);
    if B < $80 then
    begin
      if Pos(WideChar(B), AKeep) > 0 then Out_ := Out_ + Copy(S, Start, 3) else Out_ := Out_ + WideChar(B);
      Continue;
    end;
    if (B and $E0) = $C0 then N := 1 else if (B and $F0) = $E0 then N := 2 else if (B and $F8) = $F0 then N := 3
    else Exit(ThrowError('URIError', 'URI malformed'));
    Bytes := AnsiChar(B);
    for K := 1 to N do
    begin
      if (I + 2 > Length(S)) or (S[I] <> '%') or (HexVal(S[I + 1]) < 0) or (HexVal(S[I + 2]) < 0) then
        Exit(ThrowError('URIError', 'URI malformed'));
      B := HexVal(S[I + 1]) * 16 + HexVal(S[I + 2]);
      if (B and $C0) <> $80 then Exit(ThrowError('URIError', 'URI malformed'));
      Bytes := Bytes + AnsiChar(B);
      Inc(I, 3);
    end;
    Dec_ := UTF8Decode(Bytes);
    if Dec_ = '' then Exit(ThrowError('URIError', 'URI malformed'));
    Out_ := Out_ + Dec_;
  end;
  ARes := StrFromUnicode(Out_);
  Result := True;
end;

function StrArg(AArgs: TArgs; AArgc, I: Integer; var S: UnicodeString): Boolean;
var V: TValue;
begin
  if not ToStringV(Arg(AArgs, AArgc, I), V) then Exit(False);
  S := StrToUnicode(StrOf(V));
  Result := True;
end;

function G_EncodeURIComponent(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  Result := URIEncode(S, '', ARes);
end;

function G_EncodeURI(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  Result := URIEncode(S, URIReserved, ARes);
end;

function G_DecodeURIComponent(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  Result := URIDecode(S, '', ARes);
end;

function G_DecodeURI(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  Result := URIDecode(S, URIReserved, ARes);
end;

{ ---------------- indirect eval ---------------- }

{ a direct eval in function code: compiled against its caller's bindings (goal 4), run as their closure }
function EvalInCaller(const ASrc: UnicodeString; ADesc: TValue; var ARes: TValue): Boolean;
var T: PFuncTemplate; Err: UnicodeString; Clo: TValue; Ok: Boolean;
begin
  EvalOuterDesc := StrToUnicode(StrOf(ADesc));
  try
    Ok := ParseAndCompile(ASrc, '<eval>', True, T, Err, 4);
  finally
    EvalOuterDesc := '';
  end;
  if not Ok then Exit(ThrowError('SyntaxError', Err));
  if not MakeEvalClosure(T, ADesc, Clo) then Exit(False);
  Result := CallValue(Clo, VUndefined, nil, 0, ARes);
end;

function G_Eval(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T: PFuncTemplate; Err, Src: UnicodeString; Clo: TValue; Goal, P: Integer;
begin
  { indirect eval, and direct eval at a non-strict Script's top level, declare globals (goal 2); other direct
    evals keep their declarations to themselves until G2 gives them their caller's scope }
  Goal := TakeEvalHint;
  if Goal < 0 then Goal := 2;
  if not IsString(Arg(AArgs, AArgc, 1)) then begin ARes := Arg(AArgs, AArgc, 1); Exit(True); end;
  if (Goal in [0, 1]) and IsString(EvalDesc) then
  begin
    Src := StrToUnicode(StrOf(AArgs[1]));
    if Goal = 1 then
    begin
      P := 1;
      if (Length(Src) >= 2) and (Src[1] = '#') and (Src[2] = '!') then
        while (P <= Length(Src)) and not CharInSet(Src[P], [#10, #13]) and (Src[P] <> #$2028) and (Src[P] <> #$2029) do Inc(P);
      Insert('"use strict";', Src, P);
    end;
    Exit(EvalInCaller(Src, EvalDesc, ARes));
  end;
  if Goal = 1 then
  begin
    { a direct eval from strict code: strict eval code, its declarations its own }
    Src := StrToUnicode(StrOf(AArgs[1]));
    { after a hashbang comment, which only the very start of the source may hold }
    P := 1;
    if (Length(Src) >= 2) and (Src[1] = '#') and (Src[2] = '!') then
    begin
      while (P <= Length(Src)) and not CharInSet(Src[P], [#10, #13]) and (Src[P] <> #$2028) and (Src[P] <> #$2029) do Inc(P);
    end;
    Insert('"use strict";', Src, P);
    if not ParseAndCompile(Src, '<eval>', True, T, Err, 0) then Exit(ThrowError('SyntaxError', Err));
  end
  else if not ParseAndCompile(StrToUnicode(StrOf(AArgs[1])), '<eval>', True, T, Err, Goal) then Exit(ThrowError('SyntaxError', Err));
  if not MakeScriptClosure(T, Clo) then Exit(False);
  Result := CallValue(Clo, GlobalObj, nil, 0, ARes);   { indirect eval: the global environment's this }
end;

{ ---------------- Date: UTC fields, setters, parse ---------------- }

const
  okDate = 18;

function DateTimeOf(T: Double): TDateTime;
begin
  Result := UnixToDateTime(Trunc(T / 1000.0), True) + (T - Trunc(T / 1000.0) * 1000.0) / 86400000.0;
end;

function MsOf(ADT: TDateTime): Double;
begin
  Result := Round((ADT - UnixDateDelta) * 86400000.0);
end;

function ThisDate(AArgs: TArgs; var T: Double): Boolean;
begin
  if not IsObject(AArgs[0]) or (ObjOf(AArgs[0])^.OKind <> okDate) then Exit(ThrowTypeError('this is not a Date object.'));
  T := NumOf(ObjOf(AArgs[0])^.Aux1);
  Result := True;
end;

{ MakeDate from components; months and days may overflow, as in the spec }
function MakeTime(Y, AMo, D, H, AMi, S, AMs: Double): Double;
var YY, MM: Integer; Days: Double;
begin
  if IsNaN(Y) or IsNaN(AMo) or IsNaN(D) or IsNaN(H) or IsNaN(AMi) or IsNaN(S) or IsNaN(AMs) or
     IsInfinite(Y) or IsInfinite(AMo) or IsInfinite(D) then Exit(NaN);
  YY := Trunc(Y) + Floor(Trunc(AMo) / 12);
  MM := Trunc(AMo) - Floor(Trunc(AMo) / 12) * 12;
  if (YY < 1) or (YY > 9999) then Exit(NaN);
  Days := EncodeDate(YY, MM + 1, 1) - UnixDateDelta + Trunc(D) - 1;
  Result := Days * 86400000.0 + Trunc(H) * 3600000.0 + Trunc(AMi) * 60000.0 + Trunc(S) * 1000.0 + Trunc(AMs);
end;

function Date_UTC(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var C: array[1..7] of Double; I: Integer;
begin
  C[1] := NaN; C[2] := 0; C[3] := 1; C[4] := 0; C[5] := 0; C[6] := 0; C[7] := 0;
  for I := 1 to 7 do
    if I <= AArgc then if not NumArg(AArgs, AArgc, I, C[I]) then Exit(False);
  if (not IsNaN(C[1])) and (Trunc(C[1]) >= 0) and (Trunc(C[1]) <= 99) then C[1] := 1900 + Trunc(C[1]);
  ARes := MkNumber(MakeTime(C[1], C[2], C[3], C[4], C[5], C[6], C[7]));
  Result := True;
end;

function Date_Parse(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: UnicodeString; DT: TDateTime;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  if TryISO8601ToDate(string(S), DT, True) then ARes := MkNumber(MsOf(DT)) else ARes := MkDouble(NaN);
  Result := True;
end;

{ which: 0 time, 1 ms, 2 seconds, 3 minutes, 4 hours, 5 date, 6 month, 7 full year }
function DateSet(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AWhich: Integer): Boolean;
var T, V: Double; DT: TDateTime; Y, Mo, D, H, Mi, S, Ms: Word; C: array[0..6] of Double; I, N, K: Integer;
begin
  if not ThisDate(AArgs, T) then Exit(False);
  if AWhich = 0 then
  begin
    if not NumArg(AArgs, AArgc, 1, V) then Exit(False);
    if IsNaN(V) or (Abs(V) > 8.64e15) then V := NaN else V := Trunc(V);
  end
  else
  begin
    if IsNaN(T) then
    begin
      if AWhich <> 7 then begin ARes := MkDouble(NaN); Exit(True); end;
      T := 0;
    end;
    DT := DateTimeOf(T);
    DecodeDateTime(DT, Y, Mo, D, H, Mi, S, Ms);
    C[0] := Y; C[1] := Mo - 1; C[2] := D; C[3] := H; C[4] := Mi; C[5] := S; C[6] := Ms;
    { the setter's first argument and the optional ones after it, from most to least significant }
    case AWhich of
      1: begin I := 6; N := 1; end;
      2: begin I := 5; N := 2; end;
      3: begin I := 4; N := 3; end;
      4: begin I := 3; N := 4; end;
      5: begin I := 2; N := 1; end;
      6: begin I := 1; N := 2; end;
    else begin I := 0; N := 3; end;
    end;
    for K := 0 to N - 1 do
      if (K + 1 <= AArgc) and (I + K <= 6) then
        if not NumArg(AArgs, AArgc, K + 1, C[I + K]) then Exit(False);
    V := MakeTime(C[0], C[1], C[2], C[3], C[4], C[5], C[6]);
  end;
  ObjOf(AArgs[0])^.Aux1 := MkNumber(V);
  ARes := MkNumber(V);
  Result := True;
end;
function Date_SetTime(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 0); end;
function Date_SetMs(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 1); end;
function Date_SetSeconds(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 2); end;
function Date_SetMinutes(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 3); end;
function Date_SetHours(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 4); end;
function Date_SetDate(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 5); end;
function Date_SetMonth(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 6); end;
function Date_SetFullYear(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateSet(AArgs, AArgc, ARes, 7); end;

function DateGet(AArgs: TArgs; var ARes: TValue; AWhich: Integer): Boolean;
var T: Double; DT: TDateTime;
const Days: array[1..7] of UnicodeString = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  Months: array[1..12] of UnicodeString = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
  if not ThisDate(AArgs, T) then Exit(False);
  if IsNaN(T) then
  begin
    if AWhich >= 2 then ARes := StrFromUnicode('Invalid Date') else ARes := MkDouble(NaN);
    Exit(True);
  end;
  DT := DateTimeOf(T);
  case AWhich of
    0: ARes := MkInt(DayOfWeek(DT) - 1);
    1: ARes := MkInt(Trunc(T) mod 1000);
    2: ARes := StrFromUnicode(Days[DayOfWeek(DT)] + ', ' + UnicodeString(FormatDateTime('dd', DT)) + ' ' +
         Months[MonthOf(DT)] + ' ' + UnicodeString(FormatDateTime('yyyy hh:nn:ss', DT)) + ' GMT');
    3: ARes := StrFromUnicode(Days[DayOfWeek(DT)] + ' ' + Months[MonthOf(DT)] + ' ' +
         UnicodeString(FormatDateTime('dd yyyy', DT)));
    4: ARes := StrFromUnicode(UnicodeString(FormatDateTime('hh:nn:ss', DT)) + ' GMT+0000 (Coordinated Universal Time)');
    5: ARes := StrFromUnicode(Days[DayOfWeek(DT)] + ' ' + Months[MonthOf(DT)] + ' ' +
         UnicodeString(FormatDateTime('dd yyyy hh:nn:ss', DT)) + ' GMT+0000 (Coordinated Universal Time)');
  end;
  Result := True;
end;
function Date_GetUTCDay(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateGet(AArgs, ARes, 0); end;
function Date_GetUTCMs(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateGet(AArgs, ARes, 1); end;
function Date_ToUTCString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateGet(AArgs, ARes, 2); end;
function Date_ToDateString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateGet(AArgs, ARes, 3); end;
function Date_ToTimeString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateGet(AArgs, ARes, 4); end;
function Date_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DateGet(AArgs, ARes, 5); end;

const
  Prelude =
    '(function (G) {'#10 +
    '  function def(o, name, fn) { Object.defineProperty(o, name, {value: fn, writable: true, configurable: true}); }'#10 +
    '  var AP = Array.prototype, OP = Object.prototype, hasOwn = OP.hasOwnProperty;'#10 +
    '  function toInt(v) { v = +v; return v !== v ? 0 : (v < 0 ? Math.ceil(v) : Math.floor(v)); }'#10 +
    '  def(Object, "fromEntries", function (it) { var o = {}; for (var e of it) o[e[0]] = e[1]; return o; });'#10 +
    '  def(Object, "hasOwn", function (o, k) { return hasOwn.call(Object(o), k); });'#10 +
    '  def(Object, "groupBy", function (it, f) { var o = Object.create(null), i = 0; for (var x of it) { var k = f(x, i++); (o[k] || (o[k] = [])).push(x); } return o; });'#10 +
    '  def(OP, "__defineGetter__", function (k, f) { Object.defineProperty(this, k, {get: f, enumerable: true, configurable: true}); });'#10 +
    '  def(OP, "__defineSetter__", function (k, f) { Object.defineProperty(this, k, {set: f, enumerable: true, configurable: true}); });'#10 +
    '  def(OP, "__lookupGetter__", function (k) { for (var o = this; o; o = Object.getPrototypeOf(o)) { var d = Object.getOwnPropertyDescriptor(o, k); if (d) return d.get; } });'#10 +
    '  def(OP, "__lookupSetter__", function (k) { for (var o = this; o; o = Object.getPrototypeOf(o)) { var d = Object.getOwnPropertyDescriptor(o, k); if (d) return d.set; } });'#10 +
    '  def(AP, "at", function (i) { var n = this.length >>> 0; i = toInt(i); if (i < 0) i += n; return i < 0 || i >= n ? undefined : this[i]; });'#10 +
    '  def(AP, "keys", function () { var a = []; for (var i = 0, n = this.length >>> 0; i < n; i++) a.push(i); return a[Symbol.iterator](); });'#10 +
    '  def(AP, "entries", function () { var a = []; for (var i = 0, n = this.length >>> 0; i < n; i++) a.push([i, this[i]]); return a[Symbol.iterator](); });'#10 +
    '  function flat(src, out, depth) { for (var i = 0, n = src.length >>> 0; i < n; i++) { if (!(i in src)) continue; var v = src[i]; if (depth > 0 && Array.isArray(v)) flat(v, out, depth - 1); else out.push(v); } return out; }'#10 +
    '  def(AP, "flat", function (d) { return flat(this, [], d === undefined ? 1 : toInt(d)); });'#10 +
    '  def(AP, "flatMap", function (f, t) { var out = []; for (var i = 0, n = this.length >>> 0; i < n; i++) { if (!(i in this)) continue; var v = f.call(t, this[i], i, this); if (Array.isArray(v)) for (var j = 0; j < v.length; j++) out.push(v[j]); else out.push(v); } return out; });'#10 +
    '  def(AP, "copyWithin", function (t, s, e) { var n = this.length >>> 0; t = toInt(t); s = toInt(s); e = e === undefined ? n : toInt(e);'#10 +
    '    if (t < 0) t = Math.max(n + t, 0); else t = Math.min(t, n); if (s < 0) s = Math.max(n + s, 0); else s = Math.min(s, n); if (e < 0) e = Math.max(n + e, 0); else e = Math.min(e, n);'#10 +
    '    var c = Math.min(e - s, n - t), tmp = this.slice(s, s + c); for (var i = 0; i < c; i++) this[t + i] = tmp[i]; return this; });'#10 +
    '  def(AP, "toReversed", function () { return this.slice().reverse(); });'#10 +
    '  def(AP, "toSorted", function (f) { return this.slice().sort(f); });'#10 +
    '  def(AP, "toSpliced", function () { var a = this.slice(); a.splice.apply(a, arguments); return a; });'#10 +
    '  def(AP, "with", function (i, v) { var a = this.slice(), n = a.length; i = toInt(i); if (i < 0) i += n; if (i < 0 || i >= n) throw new RangeError("Invalid index"); a[i] = v; return a; });'#10 +
    '  def(AP, "toLocaleString", function () { return this.map(function (x) { return x == null ? "" : x.toLocaleString(); }).join(","); });'#10 +
    '  def(String, "fromCodePoint", function () { var s = ""; for (var i = 0; i < arguments.length; i++) { var c = +arguments[i];'#10 +
    '    if (c !== toInt(c) || c < 0 || c > 0x10FFFF) throw new RangeError("Invalid code point " + c);'#10 +
    '    if (c < 0x10000) s += String.fromCharCode(c); else { c -= 0x10000; s += String.fromCharCode(0xD800 + (c >> 10), 0xDC00 + (c & 0x3FF)); } } return s; });'#10 +
    '  def(String, "raw", function (strs) { var r = strs.raw, s = ""; for (var i = 0; i < r.length; i++) { s += r[i]; if (i + 1 < r.length && i + 1 < arguments.length) s += arguments[i + 1]; } return s; });'#10 +
    '  def(String.prototype, "isWellFormed", function () { return !/[\uD800-\uDBFF](?![\uDC00-\uDFFF])|(^|[^\uD800-\uDBFF])[\uDC00-\uDFFF]/.test(this); });'#10 +
    '  def(Number.prototype, "toPrecision", function (p) { var x = Number(this.valueOf()); if (p === undefined) return String(x); p = toInt(p);'#10 +
    '    if (x !== x || x === Infinity || x === -Infinity) return String(x); if (p < 1 || p > 100) throw new RangeError("toPrecision() argument must be between 1 and 100");'#10 +
    '    if (x === 0) return p > 1 ? "0." + "0".repeat(p - 1) : "0";'#10 +
    '    var s = x.toExponential(p - 1), e = +s.slice(s.indexOf("e") + 1); if (e < -6 || e >= p) return s; return x.toFixed(Math.max(0, p - 1 - e)); });'#10 +
    '  def(Number.prototype, "toLocaleString", function () { return String(this.valueOf()); });'#10 +
    '  Object.defineProperty(Number, "MIN_VALUE", {value: 5e-324});'#10 +
    '  ["LOG10E", Math.LOG10E || 0.4342944819032518, "LOG2E", 1.4426950408889634, "SQRT1_2", 0.7071067811865476].forEach(function (v, i, a) { if (i % 2 === 0) Object.defineProperty(Math, v, {value: a[i + 1]}); });'#10 +
    '  var SP = Set.prototype;'#10 +
    '  def(SP, "union", function (o) { var r = new Set(this); for (var x of o.keys()) r.add(x); return r; });'#10 +
    '  def(SP, "intersection", function (o) { var r = new Set(); for (var x of this) if (o.has(x)) r.add(x); return r; });'#10 +
    '  def(SP, "difference", function (o) { var r = new Set(); for (var x of this) if (!o.has(x)) r.add(x); return r; });'#10 +
    '  def(SP, "symmetricDifference", function (o) { var r = new Set(this); for (var x of o.keys()) { if (this.has(x)) r.delete(x); else r.add(x); } return r; });'#10 +
    '  def(SP, "isSubsetOf", function (o) { for (var x of this) if (!o.has(x)) return false; return true; });'#10 +
    '  def(SP, "isSupersetOf", function (o) { for (var x of o.keys()) if (!this.has(x)) return false; return true; });'#10 +
    '  def(SP, "isDisjointFrom", function (o) { for (var x of this) if (o.has(x)) return false; return true; });'#10 +
    '  ["species", "toPrimitive", "isConcatSpreadable", "match", "matchAll", "replace", "search", "split", "unscopables"].forEach(function (n) {'#10 +
    '    if (!(n in Symbol)) Object.defineProperty(Symbol, n, {value: Symbol("Symbol." + n)}); });'#10 +
    '  var R = Reflect;'#10 +
    '  def(R, "defineProperty", function (o, k, d) { try { Object.defineProperty(o, k, d); return true; } catch (e) { return false; } });'#10 +
    '  def(R, "deleteProperty", function (o, k) { return delete o[k]; });'#10 +
    '  def(R, "get", function (o, k, r) { if (r === undefined || r === o) return o[k]; for (var p = o; p; p = Object.getPrototypeOf(p)) { var d = Object.getOwnPropertyDescriptor(p, k); if (d) return d.get ? d.get.call(r) : d.value; } });'#10 +
    '  def(R, "set", function (o, k, v) { o[k] = v; return true; });'#10 +
    '  def(R, "getOwnPropertyDescriptor", Object.getOwnPropertyDescriptor);'#10 +
    '  def(R, "isExtensible", Object.isExtensible);'#10 +
    '  def(R, "preventExtensions", function (o) { Object.preventExtensions(o); return true; });'#10 +
    '  def(R, "setPrototypeOf", function (o, p) { try { Object.setPrototypeOf(o, p); return true; } catch (e) { return false; } });'#10 +
    '  function errType(name) { var C = function (m) { var e = new Error(m); Object.setPrototypeOf(e, this instanceof C ? Object.getPrototypeOf(this) : C.prototype); return e; };'#10 +
    '    C.prototype = Object.create(Error.prototype); def(C.prototype, "constructor", C); def(C.prototype, "name", name); def(C, "name", name); Object.setPrototypeOf(C, Error); def(G, name, C); return C; }'#10 +
    '  errType("EvalError"); errType("URIError");'#10 +
    '  var AE = errType("AggregateError"); def(G, "AggregateError", function (errs, m) { var e = new Error(m); var p = new.target ? new.target.prototype : AE.prototype; Object.setPrototypeOf(e, p !== null && (typeof p === "object" || typeof p === "function") ? p : AE.prototype); e.errors = Array.from(errs); return e; });'#10 +
    '  G.AggregateError.prototype = AE.prototype;'#10 +
    '  Object.defineProperty(G, "NaN", {value: NaN}); Object.defineProperty(G, "Infinity", {value: Infinity}); Object.defineProperty(G, "undefined", {value: undefined});'#10 +
    '})';

procedure InitBuiltins4;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue;
  function Lookup(AObj: TValue; const AName: UnicodeString): TValue;
  begin
    if not IsObject(AObj) or not GetV(AObj, AtomOf(AName), Result) then Result := VUndefined;
  end;
begin
  AddRoot(@Scratch);
  Scratch := Lookup(Lookup(GlobalObj, 'Number'), 'prototype');
  DefineNative(Scratch, 'toExponential', NumProto_ToExponential, 1);
  DefineNative(Scratch, 'toFixed', NumProto_ToFixed4, 1);
  Scratch := Lookup(Lookup(GlobalObj, 'Boolean'), 'prototype');
  DefineNative(Scratch, 'toString', BoolProto_ToString, 0);
  DefineNative(Scratch, 'valueOf', BoolProto_ValueOf, 0);
  Scratch := Lookup(GlobalObj, 'Math');
  DefineNative(Scratch, 'log1p', M_Log1p, 1);
  DefineNative(Scratch, 'expm1', M_Expm1, 1);
  DefineNative(Scratch, 'cosh', M_Cosh, 1);
  DefineNative(Scratch, 'sinh', M_Sinh, 1);
  DefineNative(Scratch, 'tanh', M_Tanh, 1);
  DefineNative(Scratch, 'acosh', M_Acosh, 1);
  DefineNative(Scratch, 'asinh', M_Asinh, 1);
  DefineNative(Scratch, 'atanh', M_Atanh, 1);
  DefineNative(Scratch, 'fround', M_Fround, 1);
  DefineNative(Scratch, 'imul', M_Imul, 2);
  DefineNative(Scratch, 'clz32', M_Clz32, 1);
  DefineNative(GlobalObj, 'encodeURIComponent', G_EncodeURIComponent, 1);
  DefineNative(GlobalObj, 'encodeURI', G_EncodeURI, 1);
  DefineNative(GlobalObj, 'decodeURIComponent', G_DecodeURIComponent, 1);
  DefineNative(GlobalObj, 'decodeURI', G_DecodeURI, 1);
  DefineNative(GlobalObj, 'eval', G_Eval, 1);
  Scratch := Lookup(GlobalObj, 'Date');
  DefineNative(Scratch, 'UTC', Date_UTC, 7);
  DefineNative(Scratch, 'parse', Date_Parse, 1);
  Scratch := Lookup(Scratch, 'prototype');
  DefineNative(Scratch, 'setTime', Date_SetTime, 1);
  DefineNative(Scratch, 'setMilliseconds', Date_SetMs, 1);
  DefineNative(Scratch, 'setUTCMilliseconds', Date_SetMs, 1);
  DefineNative(Scratch, 'setSeconds', Date_SetSeconds, 2);
  DefineNative(Scratch, 'setUTCSeconds', Date_SetSeconds, 2);
  DefineNative(Scratch, 'setMinutes', Date_SetMinutes, 3);
  DefineNative(Scratch, 'setUTCMinutes', Date_SetMinutes, 3);
  DefineNative(Scratch, 'setHours', Date_SetHours, 4);
  DefineNative(Scratch, 'setUTCHours', Date_SetHours, 4);
  DefineNative(Scratch, 'setDate', Date_SetDate, 1);
  DefineNative(Scratch, 'setUTCDate', Date_SetDate, 1);
  DefineNative(Scratch, 'setMonth', Date_SetMonth, 2);
  DefineNative(Scratch, 'setUTCMonth', Date_SetMonth, 2);
  DefineNative(Scratch, 'setFullYear', Date_SetFullYear, 3);
  DefineNative(Scratch, 'setUTCFullYear', Date_SetFullYear, 3);
  DefineNative(Scratch, 'getUTCDay', Date_GetUTCDay, 0);
  DefineNative(Scratch, 'getUTCMilliseconds', Date_GetUTCMs, 0);
  DefineNative(Scratch, 'toUTCString', Date_ToUTCString, 0);
  DefineNative(Scratch, 'toGMTString', Date_ToUTCString, 0);
  DefineNative(Scratch, 'toDateString', Date_ToDateString, 0);
  DefineNative(Scratch, 'toTimeString', Date_ToTimeString, 0);
  DefineNative(Scratch, 'toString', Date_ToString, 0);
  DefineNative(Scratch, 'toLocaleString', Date_ToString, 0);
  DefineNative(Scratch, 'toLocaleDateString', Date_ToDateString, 0);
  DefineNative(Scratch, 'toLocaleTimeString', Date_ToTimeString, 0);

  if not CompilePrelude('<builtins4>', Prelude, T, Err) then
    raise Exception.Create('builtins4 prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('builtins4 prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('builtins4 prelude run');
  Scratch := Fn;
  if not CallValue(Scratch, VUndefined, @GlobalObj, 1, R) then
  begin
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('builtins4 prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('builtins4 prelude call');
  end;
  { the prelude created Symbol.toPrimitive if it was missing: ToPrimitive can look it up from now on }
  if GetV(GlobalObj, AtomOf('Symbol'), R) and GetV(R, AtomOf('toPrimitive'), R) and IsPtr(R) then
    ToPrimitiveKey := PStr(PPointer(PByte(PtrOf(R)) + 16)^);
end;

end.
