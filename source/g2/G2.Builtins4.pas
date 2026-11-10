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
  SysUtils, Math, DateUtils, BigInteger, G2.Core, G2.Bytecode, G2.VM, G2.Builtins, G2.Eval, G2.Snapshot, G2.Compiler;

type
  TArgs = PValue;

threadvar
  Scratch: TValue;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function NumArg(Args: TArgs; Argc, I: Integer; var D: Double): Boolean;
begin
  Result := ToNumberV(Arg(Args, Argc, I), D);
end;

function ThisNumber(Args: TArgs; var D: Double): Boolean;
begin
  if IsNumber(Args[0]) then begin D := NumOf(Args[0]); Exit(True); end;
  if IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okNumber) then begin D := NumOf(ObjOf(Args[0])^.Aux1); Exit(True); end;
  Result := ThrowTypeError('Number.prototype method called on incompatible receiver');
end;

{ ---------------- Number.prototype.toExponential ---------------- }

{ decimal digits and exponent of |D|: D = 0.d1d2... x 10^(E+1), digits from the shortest round trip }
procedure ShortestDigits(D: Double; out Digits: UnicodeString; out E: Integer);
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
  Digits := Copy(Mant, I, MaxInt);
  E := DotPos - 1 - I + Exp10;   { exponent of the first digit }
  while (Length(Digits) > 1) and (Digits[Length(Digits)] = '0') do Delete(Digits, Length(Digits), 1);
end;

{ the exact decimal expansion of D > 0: D = 0.Dig x 10^(E+1), so Dig[1] is the 10^E digit }
procedure ExactDecimal(D: Double; out Dig: string; out E: Integer);
var Bits: QWord; BE: Integer; M: Int64; N: TBigInteger;
begin
  Bits := PQWord(@D)^;
  BE := Integer((Bits shr 52) and $7FF);
  M := Int64(Bits and ((QWord(1) shl 52) - 1));
  if BE = 0 then BE := -1074 else begin M := M or (Int64(1) shl 52); BE := BE - 1075; end;
  if BE >= 0 then
  begin
    N := TBigInteger.FromInt64(M).Multiply(TBigInteger.FromInt64(2).Power(TBigInteger.FromInt64(BE)));
    Dig := N.ToString;
    E := Length(Dig) - 1;
  end
  else
  begin
    N := TBigInteger.FromInt64(M).Multiply(TBigInteger.FromInt64(5).Power(TBigInteger.FromInt64(-BE)));
    Dig := N.ToString;
    E := Length(Dig) - 1 + BE;
  end;
  while (Length(Dig) > 1) and (Dig[Length(Dig)] = '0') do Delete(Dig, Length(Dig), 1);
end;

{ the first Keep digits of Dig rounded half up (the larger n on a tie); Carry when it became 10..0 }
function RoundDigits(const Dig: string; Keep: Integer; out Carry: Boolean): string;
var I: Integer; Up: Boolean;
begin
  Carry := False;
  if Keep <= 0 then
  begin
    { everything is below the kept place: 1 at that place or nothing }
    Carry := (Keep = 0) and (Dig[1] >= '5');
    Exit('');
  end;
  if Keep >= Length(Dig) then Exit(Dig + StringOfChar('0', Keep - Length(Dig)));
  Result := Copy(Dig, 1, Keep);
  Up := Dig[Keep + 1] >= '5';
  if not Up then Exit;
  I := Keep;
  while (I >= 1) and (Result[I] = '9') do begin Result[I] := '0'; Dec(I); end;
  if I >= 1 then Result[I] := Chr(Ord(Result[I]) + 1)
  else begin Result := '1' + Result; Carry := True; end;
end;

function NumProto_ToExponential(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D, FD: Double; F, E: Integer; Digits: UnicodeString; S, Sign, ExpStr: UnicodeString; Dig, R: string; Carry: Boolean;
begin
  if not ThisNumber(Args, D) then Exit(False);
  if Arg(Args, Argc, 1) = VUndefined then F := -1
  else
  begin
    if not NumArg(Args, Argc, 1, FD) then Exit(False);
    if IsNaN(FD) then FD := 0;
    if Abs(FD) > 1000 then F := 1000 else F := Trunc(FD);   { ToIntegerOrInfinity; anything past 100 is a RangeError }
    if F = -1 then F := -2;
  end;
  if IsNaN(D) or IsInfinite(D) then begin Res := NumberToStr(D); Exit(True); end;
  if (F <> -1) and ((F < 0) or (F > 100)) then Exit(ThrowError('RangeError', 'toExponential() argument must be between 0 and 100'));
  Sign := '';
  if D < 0 then begin Sign := '-'; D := -D; end;
  if D = 0 then
  begin
    S := '0';
    if F > 0 then S := S + '.' + StringOfChar('0', F);
    Res := StrFromUnicode(Sign + S + 'e+0');
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
  Res := StrFromUnicode(Sign + S + ExpStr);
  Result := True;
end;

{ Number.prototype.toFixed: round(x * 10^f) from the exact expansion }
function NumProto_ToFixed4(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D, FD: Double; F, E, Keep: Integer; Dig, N: string; Carry: Boolean; Sign: UnicodeString;
begin
  if not ThisNumber(Args, D) then Exit(False);
  FD := 0;
  if Arg(Args, Argc, 1) <> VUndefined then
    if not NumArg(Args, Argc, 1, FD) then Exit(False);
  if IsNaN(FD) then FD := 0;
  if Abs(FD) > 1000 then F := 1000 else F := Trunc(FD);
  if (F < 0) or (F > 100) then Exit(ThrowError('RangeError', 'toFixed() digits argument must be between 0 and 100'));
  if IsNaN(D) or IsInfinite(D) or (Abs(D) >= 1e21) then begin Res := NumberToStr(D); Exit(True); end;
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
  Res := StrFromUnicode(Sign + UnicodeString(N));
  Result := True;
end;

{ 21.1.3.5 Number.prototype.toPrecision }
function NumProto_ToPrecision(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D, PD: Double; P, E: Integer; Dig, M: string; Carry: Boolean; Sign, S: UnicodeString;
begin
  if not ThisNumber(Args, D) then Exit(False);
  if Arg(Args, Argc, 1) = VUndefined then begin Res := NumberToStr(D); Exit(True); end;
  if not NumArg(Args, Argc, 1, PD) then Exit(False);
  if IsNaN(PD) then PD := 0;
  if Abs(PD) > 1000 then P := 1000 else P := Trunc(PD);
  if IsNaN(D) or IsInfinite(D) then begin Res := NumberToStr(D); Exit(True); end;
  if (P < 1) or (P > 100) then Exit(ThrowError('RangeError', 'toPrecision() argument must be between 1 and 100'));
  Sign := '';
  if D < 0 then begin Sign := '-'; D := -D; end;
  if D = 0 then
  begin
    M := StringOfChar('0', P);
    E := 0;
  end
  else
  begin
    ExactDecimal(D, Dig, E);
    M := RoundDigits(Dig, P, Carry);
    if Carry then begin Inc(E); M := Copy(M, 1, P); end;
  end;
  if (E < -6) or (E >= P) then
  begin
    S := UnicodeString(M[1]);
    if P > 1 then S := S + '.' + UnicodeString(Copy(M, 2, MaxInt));
    if E < 0 then S := S + 'e-' + UnicodeString(IntToStr(-E)) else S := S + 'e+' + UnicodeString(IntToStr(E));
  end
  else if E = P - 1 then S := UnicodeString(M)
  else if E >= 0 then S := UnicodeString(Copy(M, 1, E + 1) + '.' + Copy(M, E + 2, MaxInt))
  else S := '0.' + UnicodeString(StringOfChar('0', -(E + 1)) + M);
  Res := StrFromUnicode(Sign + S);
  Result := True;
end;

function NextDoubleUp(X: Double): Double;
var B: QWord;
begin
  B := PQWord(@X)^ + 1;   { X > 0 and finite }
  Result := PDouble(@B)^;
end;

{ 21.1.3.6 Number.prototype.toString(radix): for a radix other than 10, the digits of the shortest-enough
  expansion (V8's DoubleToRadixCString: fraction digits up to the input's precision, rounded to even) }
function NumProto_ToString4(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
const Chars: string = '0123456789abcdefghijklmnopqrstuvwxyz';
  BufSize = 2200;
var D, RD, IntPart, Frac_, Delta, Rem: Double; Radix, Digit, IC, FC: Integer; Neg: Boolean;
  Buf: array[0..BufSize - 1] of Char; C: Char; S: string;
begin
  if not ThisNumber(Args, D) then Exit(False);
  if Arg(Args, Argc, 1) = VUndefined then RD := 10
  else
  begin
    if not NumArg(Args, Argc, 1, RD) then Exit(False);
    if IsNaN(RD) then RD := 0;
    RD := Int(RD);
  end;
  if (RD < 2) or (RD > 36) then Exit(ThrowError('RangeError', 'toString() radix must be between 2 and 36'));
  Radix := Trunc(RD);
  if (Radix = 10) or IsNaN(D) or IsInfinite(D) or (D = 0) then begin Res := NumberToStr(D); Exit(True); end;
  Neg := D < 0;
  if Neg then D := -D;
  IC := BufSize div 2;
  FC := IC;
  IntPart := Int(D);
  Frac_ := D - IntPart;
  Delta := 0.5 * (NextDoubleUp(D) - D);
  if Delta < 4.9406564584124654e-324 then Delta := 4.9406564584124654e-324;
  if Frac_ >= Delta then
  begin
    Buf[FC] := '.'; Inc(FC);
    repeat
      Frac_ := Frac_ * Radix;
      Delta := Delta * Radix;
      Digit := Trunc(Frac_);
      Buf[FC] := Chars[Digit + 1]; Inc(FC);
      Frac_ := Frac_ - Digit;
      if (Frac_ > 0.5) or ((Frac_ = 0.5) and ((Digit and 1) <> 0)) then
        if Frac_ + Delta > 1 then
        begin
          { round up, carrying through the digits already written }
          while True do
          begin
            Dec(FC);
            if FC = BufSize div 2 then begin IntPart := IntPart + 1; Break; end;
            C := Buf[FC];
            if C > '9' then Digit := Ord(C) - Ord('a') + 10 else Digit := Ord(C) - Ord('0');
            if Digit + 1 < Radix then
            begin
              Buf[FC] := Chars[Digit + 2]; Inc(FC);
              Break;
            end;
          end;
          Break;
        end;
    until Frac_ < Delta;
  end;
  { the integer digits; those below the double's precision are zeros }
  while IntPart / Radix >= 9007199254740992.0 do
  begin
    IntPart := IntPart / Radix;
    Dec(IC); Buf[IC] := '0';
  end;
  repeat
    Rem := IntPart - Int(IntPart / Radix) * Radix;
    if Rem < 0 then Rem := Rem + Radix;
    if Rem >= Radix then Rem := Rem - Radix;
    Dec(IC); Buf[IC] := Chars[Trunc(Rem) + 1];
    IntPart := (IntPart - Rem) / Radix;
  until IntPart <= 0;
  SetString(S, PChar(@Buf[IC]), FC - IC);
  if Neg then S := '-' + S;
  Res := StrFromUnicode(UnicodeString(S));
  Result := True;
end;

{ ---------------- Boolean.prototype ---------------- }

function ThisBool(Args: TArgs; var B: TValue): Boolean;
begin
  if (Args[0] = VTrue) or (Args[0] = VFalse) then begin B := Args[0]; Exit(True); end;
  if IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okBoolean) then begin B := ObjOf(Args[0])^.Aux1; Exit(True); end;
  Result := ThrowTypeError('Boolean.prototype method called on incompatible receiver');
end;

function BoolProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: TValue;
begin
  if not ThisBool(Args, B) then Exit(False);
  if B = VTrue then Res := MkPtr(AtomOf('true')) else Res := MkPtr(AtomOf('false'));
  Result := True;
end;

function BoolProto_ValueOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ThisBool(Args, Res);
end;

{ ---------------- Math ---------------- }

{$linklib m}
function c_log1p(X: Double): Double; cdecl; external 'm' name 'log1p';
function c_expm1(X: Double): Double; cdecl; external 'm' name 'expm1';
function c_cosh(X: Double): Double; cdecl; external 'm' name 'cosh';
function c_sinh(X: Double): Double; cdecl; external 'm' name 'sinh';
function c_tanh(X: Double): Double; cdecl; external 'm' name 'tanh';
function c_acosh(X: Double): Double; cdecl; external 'm' name 'acosh';
function c_asinh(X: Double): Double; cdecl; external 'm' name 'asinh';
function c_atanh(X: Double): Double; cdecl; external 'm' name 'atanh';

{ the C library's functions (IEEE 754 signed zeros and infinities, nearly correctly rounded) }
function MathFn(Args: TArgs; Argc: Integer; var Res: TValue; Which: Integer): Boolean;
var X: Double;
begin
  if not NumArg(Args, Argc, 1, X) then Exit(False);
  case Which of
    0: X := c_log1p(X);
    1: X := c_expm1(X);
    2: X := c_cosh(X);
    3: X := c_sinh(X);
    4: X := c_tanh(X);
    5: X := c_acosh(X);
    6: X := c_asinh(X);
    7: X := c_atanh(X);
    8: X := Single(X);   { fround }
    9: X := Ln(X) / Ln(2);   { fallback log2 }
  end;
  Res := MkNumber(X);
  Result := True;
end;
function M_Log1p(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 0); end;
function M_Expm1(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 1); end;
function M_Cosh(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 2); end;
function M_Sinh(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 3); end;
function M_Tanh(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 4); end;
function M_Acosh(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 5); end;
function M_Asinh(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 6); end;
function M_Atanh(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 7); end;
function M_Fround(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MathFn(Args, Argc, Res, 8); end;

function ToUint32D(D: Double): UInt32;
begin
  Result := DoubleToUint32Mod(D);
end;

function M_Imul(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A, B: Double;
begin
  if not NumArg(Args, Argc, 1, A) then Exit(False);
  if not NumArg(Args, Argc, 2, B) then Exit(False);
  Res := MkInt(Int32(UInt32(QWord(ToUint32D(A)) * QWord(ToUint32D(B)) and $FFFFFFFF)));
  Result := True;
end;

function M_Clz32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: Double; U: UInt32; N: Integer;
begin
  if not NumArg(Args, Argc, 1, A) then Exit(False);
  U := ToUint32D(A);
  N := 32;
  while U <> 0 do begin U := U shr 1; Dec(N); end;
  Res := MkInt(N);
  Result := True;
end;

{ ---------------- URI coding (ECMA-262 Encode / Decode) ---------------- }

const
  URIUnreservedMarks = '-_.!~*''()';
  URIReserved = ';/?:@&=+$,#';

function URIEncode(const S: UnicodeString; const Extra: UnicodeString; var Res: TValue): Boolean;
var I, CP: Integer; C: WideChar; Out_, Hex: UnicodeString; U8: RawByteString; J: Integer;
begin
  Out_ := '';
  I := 1;
  while I <= Length(S) do
  begin
    C := S[I];
    if ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z')) or ((C >= '0') and (C <= '9')) or
       (Pos(C, URIUnreservedMarks) > 0) or (Pos(C, Extra) > 0) then
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
  Res := StrFromUnicode(Out_);
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

{ 19.2.6.5 Decode: each escape sequence is one UTF-8 encoded code point (no overlong form, no surrogate, at most
  U+10FFFF); a single byte in Keep stays escaped }
function URIDecode(const S: UnicodeString; const Keep: UnicodeString; var Res: TValue): Boolean;
var I, B, N, K, Start, O: Integer; CP: Cardinal; Out_: UnicodeString;
  function HexAt(P: Integer): Integer;
  begin
    if (P + 2 > Length(S)) or (S[P] <> '%') or (HexVal(S[P + 1]) < 0) or (HexVal(S[P + 2]) < 0) then Exit(-1);
    Result := HexVal(S[P + 1]) * 16 + HexVal(S[P + 2]);
  end;
  procedure Put(C: WideChar); inline;
  begin
    Inc(O);
    Out_[O] := C;
  end;
begin
  SetLength(Out_, Length(S));   { each escape sequence decodes to fewer code units than it has }
  O := 0;
  I := 1;
  while I <= Length(S) do
  begin
    if S[I] <> '%' then begin Put(S[I]); Inc(I); Continue; end;
    Start := I;
    B := HexAt(I);
    if B < 0 then Exit(ThrowError('URIError', 'URI malformed'));
    Inc(I, 3);
    if B < $80 then
    begin
      if Pos(WideChar(B), Keep) > 0 then
      begin
        Put(S[Start]); Put(S[Start + 1]); Put(S[Start + 2]);
      end
      else Put(WideChar(B));
      Continue;
    end;
    if (B and $E0) = $C0 then begin N := 1; CP := B and $1F; end
    else if (B and $F0) = $E0 then begin N := 2; CP := B and $0F; end
    else if (B and $F8) = $F0 then begin N := 3; CP := B and $07; end
    else Exit(ThrowError('URIError', 'URI malformed'));
    for K := 1 to N do
    begin
      B := HexAt(I);
      if (B < 0) or ((B and $C0) <> $80) then Exit(ThrowError('URIError', 'URI malformed'));
      CP := (CP shl 6) or Cardinal(B and $3F);
      Inc(I, 3);
    end;
    if ((N = 1) and (CP < $80)) or ((N = 2) and (CP < $800)) or ((N = 3) and (CP < $10000)) or
       ((CP >= $D800) and (CP <= $DFFF)) or (CP > $10FFFF) then
      Exit(ThrowError('URIError', 'URI malformed'));
    if CP < $10000 then Put(WideChar(CP))
    else
    begin
      Dec(CP, $10000);
      Put(WideChar($D800 + (CP shr 10)));
      Put(WideChar($DC00 + (CP and $3FF)));
    end;
  end;
  SetLength(Out_, O);
  Res := StrFromUnicode(Out_);
  Result := True;
end;

function StrArg(Args: TArgs; Argc, I: Integer; var S: UnicodeString): Boolean;
var V: TValue;
begin
  if not ToStringV(Arg(Args, Argc, I), V) then Exit(False);
  S := StrToUnicode(StrOf(V));
  Result := True;
end;

function G_EncodeURIComponent(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  Result := URIEncode(S, '', Res);
end;

function G_EncodeURI(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  Result := URIEncode(S, URIReserved, Res);
end;

function G_DecodeURIComponent(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  Result := URIDecode(S, '', Res);
end;

function G_DecodeURI(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: UnicodeString;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  Result := URIDecode(S, URIReserved, Res);
end;

{ ---------------- indirect eval ---------------- }

{ a direct eval in function code: compiled against its caller's bindings (goal 4), run as their closure }
function EvalInCaller(const Src: UnicodeString; Desc: TValue; var Res: TValue): Boolean;
var T: PFuncTemplate; Err: UnicodeString; Clo: TValue; Ok: Boolean;
begin
  EvalOuterDesc := StrToUnicode(StrOf(Desc));
  try
    Ok := ParseAndCompile(Src, '<eval>', True, T, Err, 4);
  finally
    EvalOuterDesc := '';
  end;
  if not Ok then Exit(ThrowError('SyntaxError', Err));
  if not MakeEvalClosure(T, Desc, Clo) then Exit(False);
  Result := CallValue(Clo, VUndefined, nil, 0, Res);
end;

{ where a directive goes after a hashbang comment: past its line terminator (one is added when the comment ends
  the source) }
function AfterHashbang(var Src: UnicodeString): Integer;
begin
  Result := 1;
  while (Result <= Length(Src)) and not CharInSet(Src[Result], [#10, #13]) and (Src[Result] <> #$2028) and
        (Src[Result] <> #$2029) do Inc(Result);
  if Result > Length(Src) then Src := Src + #10;
  Inc(Result);
end;

function G_Eval(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: PFuncTemplate; Err, Src: UnicodeString; Clo: TValue; Goal, P: Integer;
begin
  { indirect eval, and direct eval at a non-strict Script's top level, declare globals (goal 2); other direct
    evals keep their declarations to themselves until G2 gives them their caller's scope }
  Goal := TakeEvalHint;
  if Goal < 0 then Goal := 2;
  if not IsString(Arg(Args, Argc, 1)) then begin Res := Arg(Args, Argc, 1); Exit(True); end;
  if (Goal in [0, 1]) and IsString(EvalDesc) then
  begin
    Src := StrToUnicode(StrOf(Args[1]));
    if Goal = 1 then
    begin
      P := 1;
      if (Length(Src) >= 2) and (Src[1] = '#') and (Src[2] = '!') then P := AfterHashbang(Src);
      Insert('"use strict";', Src, P);
    end;
    EvalStrictPrefix := Goal = 1;
    try
      Exit(EvalInCaller(Src, EvalDesc, Res));
    finally
      EvalStrictPrefix := False;
    end;
  end;
  if Goal = 1 then
  begin
    { a direct eval from strict code: strict eval code, its declarations its own }
    Src := StrToUnicode(StrOf(Args[1]));
    { after a hashbang comment, which only the very start of the source may hold }
    P := 1;
    if (Length(Src) >= 2) and (Src[1] = '#') and (Src[2] = '!') then P := AfterHashbang(Src);
    Insert('"use strict";', Src, P);
    EvalStrictPrefix := True;
    try
      if not ParseAndCompile(Src, '<eval>', True, T, Err, 0) then Exit(ThrowError('SyntaxError', Err));
    finally
      EvalStrictPrefix := False;
    end;
  end
  else if not ParseAndCompile(StrToUnicode(StrOf(Args[1])), '<eval>', True, T, Err, Goal) then Exit(ThrowError('SyntaxError', Err));
  if not MakeScriptClosure(T, Clo) then Exit(False);
  Result := CallValue(Clo, GlobalObj, nil, 0, Res);   { indirect eval: the global environment's this }
end;

{ ---------------- Date: UTC fields, setters, parse ---------------- }

const
  okDate = 18;

function DateTimeOf(T: Double): TDateTime;
begin
  Result := UnixToDateTime(Trunc(T / 1000.0), True) + (T - Trunc(T / 1000.0) * 1000.0) / 86400000.0;
end;

function MsOf(DT: TDateTime): Double;
begin
  Result := Round((DT - UnixDateDelta) * 86400000.0);
end;

function ThisDate(Args: TArgs; var T: Double): Boolean;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okDate) then Exit(ThrowTypeError('this is not a Date object.'));
  T := NumOf(ObjOf(Args[0])^.Aux1);
  Result := True;
end;

{ MakeDate from components; months and days may overflow, as in the spec }
function MakeTime(Y, Mo, D, H, Mi, S, Ms: Double): Double;
var YY, MM: Integer; Days: Double;
begin
  if IsNaN(Y) or IsNaN(Mo) or IsNaN(D) or IsNaN(H) or IsNaN(Mi) or IsNaN(S) or IsNaN(Ms) or
     IsInfinite(Y) or IsInfinite(Mo) or IsInfinite(D) then Exit(NaN);
  YY := Trunc(Y) + Floor(Trunc(Mo) / 12);
  MM := Trunc(Mo) - Floor(Trunc(Mo) / 12) * 12;
  if (YY < 1) or (YY > 9999) then Exit(NaN);
  Days := EncodeDate(YY, MM + 1, 1) - UnixDateDelta + Trunc(D) - 1;
  Result := Days * 86400000.0 + Trunc(H) * 3600000.0 + Trunc(Mi) * 60000.0 + Trunc(S) * 1000.0 + Trunc(Ms);
end;

function Date_UTC(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var C: array[1..7] of Double; I: Integer;
begin
  C[1] := NaN; C[2] := 0; C[3] := 1; C[4] := 0; C[5] := 0; C[6] := 0; C[7] := 0;
  for I := 1 to 7 do
    if I <= Argc then if not NumArg(Args, Argc, I, C[I]) then Exit(False);
  if (not IsNaN(C[1])) and (Trunc(C[1]) >= 0) and (Trunc(C[1]) <= 99) then C[1] := 1900 + Trunc(C[1]);
  Res := MkNumber(MakeTime(C[1], C[2], C[3], C[4], C[5], C[6], C[7]));
  Result := True;
end;

function Date_Parse(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: UnicodeString; DT: TDateTime;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  if TryISO8601ToDate(string(S), DT, True) then Res := MkNumber(MsOf(DT)) else Res := MkDouble(NaN);
  Result := True;
end;

{ which: 0 time, 1 ms, 2 seconds, 3 minutes, 4 hours, 5 date, 6 month, 7 full year }
function DateSet(Args: TArgs; Argc: Integer; var Res: TValue; Which: Integer): Boolean;
var T, V: Double; DT: TDateTime; Y, Mo, D, H, Mi, S, Ms: Word; C: array[0..6] of Double; I, N, K: Integer;
begin
  if not ThisDate(Args, T) then Exit(False);
  if Which = 0 then
  begin
    if not NumArg(Args, Argc, 1, V) then Exit(False);
    if IsNaN(V) or (Abs(V) > 8.64e15) then V := NaN else V := Trunc(V);
  end
  else
  begin
    if IsNaN(T) then
    begin
      if Which <> 7 then begin Res := MkDouble(NaN); Exit(True); end;
      T := 0;
    end;
    DT := DateTimeOf(T);
    DecodeDateTime(DT, Y, Mo, D, H, Mi, S, Ms);
    C[0] := Y; C[1] := Mo - 1; C[2] := D; C[3] := H; C[4] := Mi; C[5] := S; C[6] := Ms;
    { the setter's first argument and the optional ones after it, from most to least significant }
    case Which of
      1: begin I := 6; N := 1; end;
      2: begin I := 5; N := 2; end;
      3: begin I := 4; N := 3; end;
      4: begin I := 3; N := 4; end;
      5: begin I := 2; N := 1; end;
      6: begin I := 1; N := 2; end;
    else begin I := 0; N := 3; end;
    end;
    for K := 0 to N - 1 do
      if (K + 1 <= Argc) and (I + K <= 6) then
        if not NumArg(Args, Argc, K + 1, C[I + K]) then Exit(False);
    V := MakeTime(C[0], C[1], C[2], C[3], C[4], C[5], C[6]);
  end;
  ObjOf(Args[0])^.Aux1 := MkNumber(V);
  Res := MkNumber(V);
  Result := True;
end;
function Date_SetTime(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 0); end;
function Date_SetMs(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 1); end;
function Date_SetSeconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 2); end;
function Date_SetMinutes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 3); end;
function Date_SetHours(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 4); end;
function Date_SetDate(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 5); end;
function Date_SetMonth(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 6); end;
function Date_SetFullYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateSet(Args, Argc, Res, 7); end;

function DateGet(Args: TArgs; var Res: TValue; Which: Integer): Boolean;
var T: Double; DT: TDateTime;
const Days: array[1..7] of UnicodeString = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  Months: array[1..12] of UnicodeString = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
  if not ThisDate(Args, T) then Exit(False);
  if IsNaN(T) then
  begin
    if Which >= 2 then Res := StrFromUnicode('Invalid Date') else Res := MkDouble(NaN);
    Exit(True);
  end;
  DT := DateTimeOf(T);
  case Which of
    0: Res := MkInt(DayOfWeek(DT) - 1);
    1: Res := MkInt(Trunc(T) mod 1000);
    2: Res := StrFromUnicode(Days[DayOfWeek(DT)] + ', ' + UnicodeString(FormatDateTime('dd', DT)) + ' ' +
         Months[MonthOf(DT)] + ' ' + UnicodeString(FormatDateTime('yyyy hh:nn:ss', DT)) + ' GMT');
    3: Res := StrFromUnicode(Days[DayOfWeek(DT)] + ' ' + Months[MonthOf(DT)] + ' ' +
         UnicodeString(FormatDateTime('dd yyyy', DT)));
    4: Res := StrFromUnicode(UnicodeString(FormatDateTime('hh:nn:ss', DT)) + ' GMT+0000 (Coordinated Universal Time)');
    5: Res := StrFromUnicode(Days[DayOfWeek(DT)] + ' ' + Months[MonthOf(DT)] + ' ' +
         UnicodeString(FormatDateTime('dd yyyy hh:nn:ss', DT)) + ' GMT+0000 (Coordinated Universal Time)');
  end;
  Result := True;
end;
function Date_GetUTCDay(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateGet(Args, Res, 0); end;
function Date_GetUTCMs(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateGet(Args, Res, 1); end;
function Date_ToUTCString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateGet(Args, Res, 2); end;
function Date_ToDateString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateGet(Args, Res, 3); end;
function Date_ToTimeString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateGet(Args, Res, 4); end;
function Date_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DateGet(Args, Res, 5); end;

const
  Prelude =
    '(function (G) {'#10 +
    '  function def(o, name, fn) { Object.defineProperty(o, name, {value: fn, writable: true, configurable: true}); }'#10 +
    '  var AP = Array.prototype, OP = Object.prototype, hasOwn = OP.hasOwnProperty;'#10 +
    '  function toInt(v) { v = +v; return v !== v ? 0 : (v < 0 ? Math.ceil(v) : Math.floor(v)); }'#10 +
    '  def(Object, "groupBy", function (it, f) { var o = Object.create(null), i = 0; for (var x of it) { var k = f(x, i++); (o[k] || (o[k] = [])).push(x); } return o; });'#10 +
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
    '  def(AP, "toLocaleString", function toLocaleString(locales = undefined, options = undefined) { var a = Object(this), n = a.length >>> 0, r = "";'#10 +
    '    for (var i = 0; i < n; i++) { if (i > 0) r += ","; var x = a[i]; if (x != null) r += String(x.toLocaleString(locales, options)); } return r; });'#10 +
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
    '  Object.defineProperty(G, "NaN", {value: NaN}); Object.defineProperty(G, "Infinity", {value: Infinity}); Object.defineProperty(G, "undefined", {value: undefined});'#10 +
    '})';

procedure InitBuiltins4;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue;
  function Lookup(Obj: TValue; const Name: UnicodeString): TValue;
  begin
    if not IsObject(Obj) or not GetV(Obj, AtomOf(Name), Result) then Result := VUndefined;
  end;
begin
  AddRoot(@Scratch);
  Scratch := Lookup(Lookup(GlobalObj, 'Number'), 'prototype');
  DefineNative(Scratch, 'toExponential', NumProto_ToExponential, 1);
  DefineNative(Scratch, 'toFixed', NumProto_ToFixed4, 1);
  DefineNative(Scratch, 'toPrecision', NumProto_ToPrecision, 1);
  DefineNative(Scratch, 'toString', NumProto_ToString4, 1);
  { 21.1.2.12 / 21.1.2.13: Number.parseFloat and Number.parseInt are the global functions }
  Scratch := Lookup(GlobalObj, 'Number');
  DefineOwn(Scratch, AtomOf('parseFloat'), Lookup(GlobalObj, 'parseFloat'), pfHidden);
  DefineOwn(Scratch, AtomOf('parseInt'), Lookup(GlobalObj, 'parseInt'), pfHidden);
  Scratch := Lookup(Lookup(GlobalObj, 'Number'), 'prototype');
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
  AddRoot(@RealmEvalFn);
  RealmVals([@RealmEvalFn]);
  RealmEvalFn := Lookup(GlobalObj, 'eval');
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
