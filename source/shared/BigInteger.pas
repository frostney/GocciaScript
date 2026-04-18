unit BigInteger;

{$I Shared.inc}

interface

type
  TLimbArray = array of Cardinal;

  TBigInteger = record
  private
    FLimbs: TLimbArray;
    FNegative: Boolean;

    procedure Normalize;
    class function AddMagnitudes(const AA, AB: TLimbArray): TLimbArray; static;
    class function SubMagnitudes(const ALarger, ASmaller: TLimbArray): TLimbArray; static;
    class function CompareMagnitudes(const AA, AB: TLimbArray): Integer; static;
    class procedure DivModMagnitudes(const AA, AB: TLimbArray;
      out AQuotient, ARemainder: TLimbArray); static;
  public
    class function Zero: TBigInteger; static; inline;
    class function One: TBigInteger; static;
    class function NegativeOne: TBigInteger; static;
    class function FromInt64(const AValue: Int64): TBigInteger; static;
    class function FromDouble(const AValue: Double): TBigInteger; static;
    class function FromDecimalString(const AValue: string): TBigInteger; static;
    class function FromHexString(const AValue: string): TBigInteger; static;
    class function FromBinaryString(const AValue: string): TBigInteger; static;
    class function FromOctalString(const AValue: string): TBigInteger; static;

    function IsZero: Boolean; inline;
    function IsNegative: Boolean; inline;
    function IsPositive: Boolean; inline;
    function IsOne: Boolean;
    function IsMinusOne: Boolean;
    function BitLength: Integer;
    function GetBit(const AIndex: Integer): Boolean;

    function ToDouble: Double;
    function ToInt64: Int64;
    function ToString: string;
    function ToRadixString(const ARadix: Integer): string;

    function Negate: TBigInteger;
    function AbsValue: TBigInteger;

    function Add(const AOther: TBigInteger): TBigInteger;
    function Subtract(const AOther: TBigInteger): TBigInteger;
    function Multiply(const AOther: TBigInteger): TBigInteger;
    function Divide(const AOther: TBigInteger): TBigInteger;
    function Modulo(const AOther: TBigInteger): TBigInteger;
    function Power(const AExponent: TBigInteger): TBigInteger;

    function BitwiseAnd(const AOther: TBigInteger): TBigInteger;
    function BitwiseOr(const AOther: TBigInteger): TBigInteger;
    function BitwiseXor(const AOther: TBigInteger): TBigInteger;
    function BitwiseNot: TBigInteger;
    function ShiftLeft(const ACount: Integer): TBigInteger;
    function ShiftRight(const ACount: Integer): TBigInteger;

    function Compare(const AOther: TBigInteger): Integer;
    function Equal(const AOther: TBigInteger): Boolean;

    function AsIntN(const ABits: Integer): TBigInteger;
    function AsUintN(const ABits: Integer): TBigInteger;
  end;

implementation

uses
  Math,
  SysUtils;

const
  LIMB_BITS = 32;
  LIMB_BASE: QWord = QWord(1) shl LIMB_BITS;

{ --- Internal helpers --- }

procedure TBigInteger.Normalize;
var
  Len: Integer;
begin
  Len := Length(FLimbs);
  while (Len > 0) and (FLimbs[Len - 1] = 0) do
    Dec(Len);
  SetLength(FLimbs, Len);
  if Len = 0 then
    FNegative := False;
end;

class function TBigInteger.CompareMagnitudes(const AA, AB: TLimbArray): Integer;
var
  I: Integer;
begin
  if Length(AA) <> Length(AB) then
  begin
    if Length(AA) > Length(AB) then
      Exit(1)
    else
      Exit(-1);
  end;
  for I := High(AA) downto 0 do
  begin
    if AA[I] > AB[I] then
      Exit(1);
    if AA[I] < AB[I] then
      Exit(-1);
  end;
  Result := 0;
end;

{$Q-}{$R-}
class function TBigInteger.AddMagnitudes(const AA, AB: TLimbArray): TLimbArray;
var
  MaxLen, I: Integer;
  Carry, Sum: QWord;
  LA, LB: Cardinal;
begin
  MaxLen := Length(AA);
  if Length(AB) > MaxLen then
    MaxLen := Length(AB);
  SetLength(Result, MaxLen + 1);
  Carry := 0;
  for I := 0 to MaxLen - 1 do
  begin
    if I < Length(AA) then LA := AA[I] else LA := 0;
    if I < Length(AB) then LB := AB[I] else LB := 0;
    Sum := QWord(LA) + QWord(LB) + Carry;
    Result[I] := Cardinal(Sum);
    Carry := Sum shr LIMB_BITS;
  end;
  if Carry <> 0 then
    Result[MaxLen] := Cardinal(Carry)
  else
    SetLength(Result, MaxLen);
end;

class function TBigInteger.SubMagnitudes(const ALarger, ASmaller: TLimbArray): TLimbArray;
var
  I, Len: Integer;
  Borrow: Int64;
  Diff: Int64;
  LA, LB: Cardinal;
begin
  Len := Length(ALarger);
  SetLength(Result, Len);
  Borrow := 0;
  for I := 0 to Len - 1 do
  begin
    LA := ALarger[I];
    if I < Length(ASmaller) then LB := ASmaller[I] else LB := 0;
    Diff := Int64(LA) - Int64(LB) - Borrow;
    if Diff < 0 then
    begin
      Diff := Diff + Int64(LIMB_BASE);
      Borrow := 1;
    end
    else
      Borrow := 0;
    Result[I] := Cardinal(Diff);
  end;
  // Normalize trailing zeros
  Len := Length(Result);
  while (Len > 0) and (Result[Len - 1] = 0) do
    Dec(Len);
  SetLength(Result, Len);
end;

class procedure TBigInteger.DivModMagnitudes(const AA, AB: TLimbArray;
  out AQuotient, ARemainder: TLimbArray);
var
  N, M, I, J, Len: Integer;
  U, V: TLimbArray;
  QHat, RHat, P: QWord;
  D, VNMinus1, VNMinus2: Cardinal;
  Carry: QWord;
  T: Int64;
begin
  N := Length(AB);
  M := Length(AA) - N;

  if N = 0 then
    raise Exception.Create('Division by zero');

  // Single limb divisor - fast path
  if N = 1 then
  begin
    SetLength(AQuotient, Length(AA));
    Carry := 0;
    for I := High(AA) downto 0 do
    begin
      Carry := (Carry shl LIMB_BITS) + QWord(AA[I]);
      AQuotient[I] := Cardinal(Carry div QWord(AB[0]));
      Carry := Carry mod QWord(AB[0]);
    end;
    Len := Length(AQuotient);
    while (Len > 0) and (AQuotient[Len - 1] = 0) do Dec(Len);
    SetLength(AQuotient, Len);
    if Carry <> 0 then
    begin
      SetLength(ARemainder, 1);
      ARemainder[0] := Cardinal(Carry);
    end
    else
      SetLength(ARemainder, 0);
    Exit;
  end;

  // Knuth Algorithm D
  // D1: Normalize
  D := 0;
  while (QWord(AB[N - 1]) shl (D + 1)) < LIMB_BASE do
    Inc(D);

  SetLength(V, N);
  Carry := 0;
  for I := 0 to N - 1 do
  begin
    Carry := Carry + (QWord(AB[I]) shl D);
    V[I] := Cardinal(Carry);
    Carry := Carry shr LIMB_BITS;
  end;

  SetLength(U, Length(AA) + 1);
  Carry := 0;
  for I := 0 to High(AA) do
  begin
    Carry := Carry + (QWord(AA[I]) shl D);
    U[I] := Cardinal(Carry);
    Carry := Carry shr LIMB_BITS;
  end;
  U[Length(AA)] := Cardinal(Carry);

  SetLength(AQuotient, M + 1);
  VNMinus1 := V[N - 1];
  VNMinus2 := V[N - 2];

  // D2-D7: Main loop
  for J := M downto 0 do
  begin
    // D3: Calculate QHat
    QHat := (QWord(U[J + N]) shl LIMB_BITS + QWord(U[J + N - 1])) div QWord(VNMinus1);
    RHat := (QWord(U[J + N]) shl LIMB_BITS + QWord(U[J + N - 1])) mod QWord(VNMinus1);

    while (QHat >= LIMB_BASE) or (QHat * QWord(VNMinus2) > LIMB_BASE * RHat + QWord(U[J + N - 2])) do
    begin
      Dec(QHat);
      RHat := RHat + QWord(VNMinus1);
      if RHat >= LIMB_BASE then
        Break;
    end;

    // D4: Multiply and subtract
    Carry := 0;
    for I := 0 to N - 1 do
    begin
      P := QHat * QWord(V[I]);
      T := Int64(U[I + J]) - Int64(Cardinal(P)) - Int64(Carry);
      U[I + J] := Cardinal(T);
      Carry := (P shr LIMB_BITS);
      if T < 0 then
        Carry := Carry + 1;
    end;
    T := Int64(U[J + N]) - Int64(Carry);
    U[J + N] := Cardinal(T);

    AQuotient[J] := Cardinal(QHat);

    // D5-D6: Add back if negative
    if T < 0 then
    begin
      Dec(AQuotient[J]);
      Carry := 0;
      for I := 0 to N - 1 do
      begin
        Carry := Carry + QWord(U[I + J]) + QWord(V[I]);
        U[I + J] := Cardinal(Carry);
        Carry := Carry shr LIMB_BITS;
      end;
      U[J + N] := Cardinal(QWord(U[J + N]) + Carry);
    end;
  end;

  // D8: Unnormalize remainder
  SetLength(ARemainder, N);
  if D > 0 then
  begin
    Carry := 0;
    for I := N - 1 downto 0 do
    begin
      Carry := (Carry shl LIMB_BITS) + QWord(U[I]);
      ARemainder[I] := Cardinal(Carry shr D);
      Carry := Carry and (QWord(1) shl D - 1);
    end;
  end
  else
    for I := 0 to N - 1 do
      ARemainder[I] := U[I];

  // Normalize results
  Len := Length(AQuotient);
  while (Len > 0) and (AQuotient[Len - 1] = 0) do Dec(Len);
  SetLength(AQuotient, Len);
  Len := Length(ARemainder);
  while (Len > 0) and (ARemainder[Len - 1] = 0) do Dec(Len);
  SetLength(ARemainder, Len);
end;
{$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

{ --- Constructors --- }

class function TBigInteger.Zero: TBigInteger;
begin
  Result.FNegative := False;
  Result.FLimbs := nil;
end;

class function TBigInteger.One: TBigInteger;
begin
  Result.FNegative := False;
  SetLength(Result.FLimbs, 1);
  Result.FLimbs[0] := 1;
end;

class function TBigInteger.NegativeOne: TBigInteger;
begin
  Result.FNegative := True;
  SetLength(Result.FLimbs, 1);
  Result.FLimbs[0] := 1;
end;

class function TBigInteger.FromInt64(const AValue: Int64): TBigInteger;
var
  Mag: QWord;
begin
  if AValue = 0 then
    Exit(Zero);

  Result.FNegative := AValue < 0;
  if AValue = Low(Int64) then
    Mag := QWord(High(Int64)) + 1
  else if AValue < 0 then
    Mag := QWord(-AValue)
  else
    Mag := QWord(AValue);

  if Mag <= High(Cardinal) then
  begin
    SetLength(Result.FLimbs, 1);
    Result.FLimbs[0] := Cardinal(Mag);
  end
  else
  begin
    SetLength(Result.FLimbs, 2);
    {$Q-}{$R-}
    Result.FLimbs[0] := Cardinal(Mag);
    Result.FLimbs[1] := Cardinal(Mag shr LIMB_BITS);
    {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
  end;
end;

class function TBigInteger.FromDouble(const AValue: Double): TBigInteger;
var
  V: Double;
  Limb: Cardinal;
begin
  if IsNaN(AValue) or IsInfinite(AValue) then
    raise Exception.Create('Cannot convert non-finite number to BigInt');

  if Frac(AValue) <> 0 then
    raise Exception.Create('Cannot convert non-integer to BigInt');

  if AValue = 0 then
    Exit(Zero);

  // All exact IEEE 754 integers fit in Int64 — use direct conversion to avoid
  // FPC 3.2.2 AArch64 floating-point precision bugs with FMod/LIMB_BASE arithmetic
  if (AValue >= -9007199254740992.0) and (AValue <= 9007199254740992.0) then
    Exit(FromInt64(Trunc(AValue)));

  Result.FNegative := AValue < 0;
  V := Abs(AValue);
  Result.FLimbs := nil;

  while V >= 1 do
  begin
    {$Q-}{$R-}
    Limb := Cardinal(Trunc(FMod(V, LIMB_BASE)));
    {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
    SetLength(Result.FLimbs, Length(Result.FLimbs) + 1);
    Result.FLimbs[High(Result.FLimbs)] := Limb;
    V := Int(V / LIMB_BASE);
  end;
  Result.Normalize;
end;

class function TBigInteger.FromDecimalString(const AValue: string): TBigInteger;
var
  I, StartIndex: Integer;
  Ch: Char;
  Digit: Cardinal;
  Carry: QWord;
  J: Integer;
begin
  if Length(AValue) = 0 then
    raise Exception.Create('Invalid BigInt string');

  Result := Zero;
  StartIndex := 1;

  if AValue[1] = '-' then
  begin
    Result.FNegative := True;
    StartIndex := 2;
  end
  else if AValue[1] = '+' then
    StartIndex := 2;

  if StartIndex > Length(AValue) then
    raise Exception.Create('Invalid BigInt string');

  for I := StartIndex to Length(AValue) do
  begin
    Ch := AValue[I];
    if (Ch < '0') or (Ch > '9') then
      raise Exception.Create('Invalid BigInt string');
    Digit := Ord(Ch) - Ord('0');

    // Multiply by 10 and add digit
    {$Q-}{$R-}
    Carry := Digit;
    for J := 0 to High(Result.FLimbs) do
    begin
      Carry := Carry + QWord(Result.FLimbs[J]) * 10;
      Result.FLimbs[J] := Cardinal(Carry);
      Carry := Carry shr LIMB_BITS;
    end;
    if Carry <> 0 then
    begin
      SetLength(Result.FLimbs, Length(Result.FLimbs) + 1);
      Result.FLimbs[High(Result.FLimbs)] := Cardinal(Carry);
    end;
    {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
  end;

  Result.Normalize;
end;

class function TBigInteger.FromHexString(const AValue: string): TBigInteger;
var
  I, BitPos, LimbIndex: Integer;
  Ch: Char;
  Nibble: Cardinal;
begin
  if Length(AValue) = 0 then
    raise Exception.Create('Invalid BigInt hex string');

  Result := Zero;
  // Pre-allocate limbs
  SetLength(Result.FLimbs, (Length(AValue) * 4 + LIMB_BITS - 1) div LIMB_BITS);
  for I := 0 to High(Result.FLimbs) do
    Result.FLimbs[I] := 0;

  BitPos := 0;
  for I := Length(AValue) downto 1 do
  begin
    Ch := AValue[I];
    case Ch of
      '0'..'9': Nibble := Ord(Ch) - Ord('0');
      'a'..'f': Nibble := Ord(Ch) - Ord('a') + 10;
      'A'..'F': Nibble := Ord(Ch) - Ord('A') + 10;
    else
      raise Exception.Create('Invalid BigInt hex string');
    end;
    LimbIndex := BitPos div LIMB_BITS;
    {$Q-}{$R-}
    Result.FLimbs[LimbIndex] := Result.FLimbs[LimbIndex] or (Nibble shl (BitPos mod LIMB_BITS));
    {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
    Inc(BitPos, 4);
  end;
  Result.Normalize;
end;

class function TBigInteger.FromBinaryString(const AValue: string): TBigInteger;
var
  I, BitPos, LimbIndex: Integer;
begin
  if Length(AValue) = 0 then
    raise Exception.Create('Invalid BigInt binary string');

  Result := Zero;
  SetLength(Result.FLimbs, (Length(AValue) + LIMB_BITS - 1) div LIMB_BITS);
  for I := 0 to High(Result.FLimbs) do
    Result.FLimbs[I] := 0;

  BitPos := 0;
  for I := Length(AValue) downto 1 do
  begin
    if AValue[I] = '1' then
    begin
      LimbIndex := BitPos div LIMB_BITS;
      {$Q-}{$R-}
      Result.FLimbs[LimbIndex] := Result.FLimbs[LimbIndex] or (Cardinal(1) shl (BitPos mod LIMB_BITS));
      {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
    end
    else if AValue[I] <> '0' then
      raise Exception.Create('Invalid BigInt binary string');
    Inc(BitPos);
  end;
  Result.Normalize;
end;

class function TBigInteger.FromOctalString(const AValue: string): TBigInteger;
var
  I, BitPos, LimbIndex, Bit: Integer;
  Ch: Char;
  OctDigit: Cardinal;
begin
  if Length(AValue) = 0 then
    raise Exception.Create('Invalid BigInt octal string');

  Result := Zero;
  SetLength(Result.FLimbs, (Length(AValue) * 3 + LIMB_BITS - 1) div LIMB_BITS);
  for I := 0 to High(Result.FLimbs) do
    Result.FLimbs[I] := 0;

  BitPos := 0;
  for I := Length(AValue) downto 1 do
  begin
    Ch := AValue[I];
    if (Ch < '0') or (Ch > '7') then
      raise Exception.Create('Invalid BigInt octal string');
    OctDigit := Ord(Ch) - Ord('0');
    for Bit := 0 to 2 do
    begin
      if (OctDigit and (1 shl Bit)) <> 0 then
      begin
        LimbIndex := BitPos div LIMB_BITS;
        if LimbIndex <= High(Result.FLimbs) then
        begin
          {$Q-}{$R-}
          Result.FLimbs[LimbIndex] := Result.FLimbs[LimbIndex] or (Cardinal(1) shl (BitPos mod LIMB_BITS));
          {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
        end;
      end;
      Inc(BitPos);
    end;
  end;
  Result.Normalize;
end;

{ --- Predicates --- }

function TBigInteger.IsZero: Boolean;
begin
  Result := Length(FLimbs) = 0;
end;

function TBigInteger.IsNegative: Boolean;
begin
  Result := FNegative and (Length(FLimbs) > 0);
end;

function TBigInteger.IsPositive: Boolean;
begin
  Result := (not FNegative) and (Length(FLimbs) > 0);
end;

function TBigInteger.IsOne: Boolean;
begin
  Result := (not FNegative) and (Length(FLimbs) = 1) and (FLimbs[0] = 1);
end;

function TBigInteger.IsMinusOne: Boolean;
begin
  Result := FNegative and (Length(FLimbs) = 1) and (FLimbs[0] = 1);
end;

function TBigInteger.BitLength: Integer;
var
  TopLimb: Cardinal;
begin
  if IsZero then
    Exit(0);
  Result := (Length(FLimbs) - 1) * LIMB_BITS;
  TopLimb := FLimbs[High(FLimbs)];
  while TopLimb <> 0 do
  begin
    Inc(Result);
    TopLimb := TopLimb shr 1;
  end;
end;

function TBigInteger.GetBit(const AIndex: Integer): Boolean;
var
  LimbIndex: Integer;
begin
  LimbIndex := AIndex div LIMB_BITS;
  if LimbIndex >= Length(FLimbs) then
    Exit(False);
  {$Q-}{$R-}
  Result := (FLimbs[LimbIndex] and (Cardinal(1) shl (AIndex mod LIMB_BITS))) <> 0;
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
end;

{ --- Conversions --- }

function TBigInteger.ToDouble: Double;
var
  I: Integer;
  Base: Double;
begin
  if IsZero then
    Exit(0.0);
  Result := 0.0;
  Base := 1.0;
  for I := 0 to High(FLimbs) do
  begin
    Result := Result + FLimbs[I] * Base;
    Base := Base * LIMB_BASE;
  end;
  if FNegative then
    Result := -Result;
end;

function TBigInteger.ToInt64: Int64;
var
  Mag: QWord;
begin
  if IsZero then
    Exit(0);
  {$Q-}{$R-}
  Mag := QWord(FLimbs[0]);
  if Length(FLimbs) > 1 then
    Mag := Mag or (QWord(FLimbs[1]) shl LIMB_BITS);
  if FNegative then
    Result := -Int64(Mag)
  else
    Result := Int64(Mag);
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
end;

function TBigInteger.ToString: string;
begin
  Result := ToRadixString(10);
end;

function TBigInteger.ToRadixString(const ARadix: Integer): string;
var
  Temp: TLimbArray;
  Carry, Remainder: QWord;
  I, Len: Integer;
  Digits: string;
  Digit: Integer;
begin
  if IsZero then
    Exit('0');

  if (ARadix < 2) or (ARadix > 36) then
    raise Exception.Create('Invalid radix');

  // Copy limbs for destructive division
  SetLength(Temp, Length(FLimbs));
  for I := 0 to High(FLimbs) do
    Temp[I] := FLimbs[I];

  Digits := '';

  {$Q-}{$R-}
  while True do
  begin
    // Check if all limbs are zero
    Len := Length(Temp);
    while (Len > 0) and (Temp[Len - 1] = 0) do
      Dec(Len);
    if Len = 0 then
      Break;

    // Divide by radix
    Remainder := 0;
    for I := Len - 1 downto 0 do
    begin
      Carry := (Remainder shl LIMB_BITS) + QWord(Temp[I]);
      Temp[I] := Cardinal(Carry div QWord(ARadix));
      Remainder := Carry mod QWord(ARadix);
    end;

    Digit := Integer(Remainder);
    if Digit < 10 then
      Digits := Chr(Ord('0') + Digit) + Digits
    else
      Digits := Chr(Ord('a') + Digit - 10) + Digits;
  end;
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

  if FNegative then
    Result := '-' + Digits
  else
    Result := Digits;
end;

{ --- Unary operations --- }

function TBigInteger.Negate: TBigInteger;
begin
  Result.FLimbs := Copy(FLimbs);
  if Length(FLimbs) > 0 then
    Result.FNegative := not FNegative
  else
    Result.FNegative := False;
end;

function TBigInteger.AbsValue: TBigInteger;
begin
  Result.FLimbs := Copy(FLimbs);
  Result.FNegative := False;
end;

{ --- Arithmetic --- }

function TBigInteger.Add(const AOther: TBigInteger): TBigInteger;
var
  Cmp: Integer;
begin
  if IsZero then Exit(AOther);
  if AOther.IsZero then Exit(Self);

  // Same sign: add magnitudes, keep sign
  if FNegative = AOther.FNegative then
  begin
    Result.FLimbs := AddMagnitudes(FLimbs, AOther.FLimbs);
    Result.FNegative := FNegative;
  end
  else
  begin
    // Different signs: subtract smaller from larger magnitude
    Cmp := CompareMagnitudes(FLimbs, AOther.FLimbs);
    if Cmp = 0 then
      Exit(Zero);
    if Cmp > 0 then
    begin
      Result.FLimbs := SubMagnitudes(FLimbs, AOther.FLimbs);
      Result.FNegative := FNegative;
    end
    else
    begin
      Result.FLimbs := SubMagnitudes(AOther.FLimbs, FLimbs);
      Result.FNegative := AOther.FNegative;
    end;
  end;
  Result.Normalize;
end;

function TBigInteger.Subtract(const AOther: TBigInteger): TBigInteger;
begin
  Result := Add(AOther.Negate);
end;

function TBigInteger.Multiply(const AOther: TBigInteger): TBigInteger;
var
  I, J: Integer;
  Carry: QWord;
  Prod: QWord;
begin
  if IsZero or AOther.IsZero then
    Exit(Zero);

  SetLength(Result.FLimbs, Length(FLimbs) + Length(AOther.FLimbs));
  for I := 0 to High(Result.FLimbs) do
    Result.FLimbs[I] := 0;

  {$Q-}{$R-}
  for I := 0 to High(FLimbs) do
  begin
    Carry := 0;
    for J := 0 to High(AOther.FLimbs) do
    begin
      Prod := QWord(FLimbs[I]) * QWord(AOther.FLimbs[J]) +
              QWord(Result.FLimbs[I + J]) + Carry;
      Result.FLimbs[I + J] := Cardinal(Prod);
      Carry := Prod shr LIMB_BITS;
    end;
    if Carry <> 0 then
      Result.FLimbs[I + Length(AOther.FLimbs)] := Cardinal(Carry);
  end;
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

  Result.FNegative := FNegative xor AOther.FNegative;
  Result.Normalize;
end;

// ES2026 §6.1.6.2.6 BigInt::divide — truncates toward zero
function TBigInteger.Divide(const AOther: TBigInteger): TBigInteger;
var
  Q, R: TLimbArray;
begin
  if AOther.IsZero then
    raise Exception.Create('Division by zero');
  if IsZero then
    Exit(Zero);

  if CompareMagnitudes(FLimbs, AOther.FLimbs) < 0 then
    Exit(Zero);

  DivModMagnitudes(FLimbs, AOther.FLimbs, Q, R);
  Result.FLimbs := Q;
  Result.FNegative := FNegative xor AOther.FNegative;
  Result.Normalize;
end;

// ES2026 §6.1.6.2.7 BigInt::remainder — sign follows dividend
function TBigInteger.Modulo(const AOther: TBigInteger): TBigInteger;
var
  Q, R: TLimbArray;
begin
  if AOther.IsZero then
    raise Exception.Create('Division by zero');
  if IsZero then
    Exit(Zero);

  if CompareMagnitudes(FLimbs, AOther.FLimbs) < 0 then
    Exit(Self);

  DivModMagnitudes(FLimbs, AOther.FLimbs, Q, R);
  Result.FLimbs := R;
  Result.FNegative := FNegative;
  Result.Normalize;
end;

// ES2026 §6.1.6.2.8 BigInt::exponentiate
function TBigInteger.Power(const AExponent: TBigInteger): TBigInteger;
var
  Base, Exp: TBigInteger;
begin
  if AExponent.IsNegative then
    raise Exception.Create('Exponent must be positive');
  if AExponent.IsZero then
    Exit(One);
  if IsZero then
    Exit(Zero);

  Base := Self;
  Exp := AExponent;
  Result := One;

  while not Exp.IsZero do
  begin
    if Exp.GetBit(0) then
      Result := Result.Multiply(Base);
    Base := Base.Multiply(Base);
    Exp := Exp.ShiftRight(1);
  end;
end;

{ --- Bitwise operations (two's complement semantics) --- }

// Helper: convert magnitude+sign to two's complement limbs
// For negative numbers: ~magnitude + 1 (infinite sign extension with 1s)
function ToTwosComplement(const ABI: TBigInteger; const ALen: Integer): TLimbArray;
var
  I: Integer;
  Carry: QWord;
begin
  SetLength(Result, ALen);
  for I := 0 to ALen - 1 do
  begin
    if I < Length(ABI.FLimbs) then
      Result[I] := ABI.FLimbs[I]
    else
      Result[I] := 0;
  end;

  if ABI.FNegative then
  begin
    {$Q-}{$R-}
    // Invert all bits
    for I := 0 to ALen - 1 do
      Result[I] := not Result[I];
    // Add 1
    Carry := 1;
    for I := 0 to ALen - 1 do
    begin
      Carry := Carry + QWord(Result[I]);
      Result[I] := Cardinal(Carry);
      Carry := Carry shr LIMB_BITS;
    end;
    {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
  end;
end;

function FromTwosComplement(const ALimbs: TLimbArray; const ANegative: Boolean): TBigInteger;
var
  Temp: TLimbArray;
  I, Len: Integer;
  Carry: QWord;
begin
  if ANegative then
  begin
    SetLength(Temp, Length(ALimbs));
    {$Q-}{$R-}
    // Invert all bits
    for I := 0 to High(ALimbs) do
      Temp[I] := not ALimbs[I];
    // Add 1
    Carry := 1;
    for I := 0 to High(Temp) do
    begin
      Carry := Carry + QWord(Temp[I]);
      Temp[I] := Cardinal(Carry);
      Carry := Carry shr LIMB_BITS;
    end;
    {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
    Result.FLimbs := Temp;
    Result.FNegative := True;
  end
  else
  begin
    Result.FLimbs := Copy(ALimbs);
    Result.FNegative := False;
  end;
  // Normalize
  Len := Length(Result.FLimbs);
  while (Len > 0) and (Result.FLimbs[Len - 1] = 0) do
    Dec(Len);
  SetLength(Result.FLimbs, Len);
  if Len = 0 then
    Result.FNegative := False;
end;

function TBigInteger.BitwiseAnd(const AOther: TBigInteger): TBigInteger;
var
  Len, I: Integer;
  A, B: TLimbArray;
  ResultNeg: Boolean;
  ResultLimbs: TLimbArray;
begin
  if IsZero then Exit(Zero);
  if AOther.IsZero then Exit(Zero);

  Len := Length(FLimbs);
  if Length(AOther.FLimbs) > Len then
    Len := Length(AOther.FLimbs);
  Inc(Len); // Extra limb for sign

  A := ToTwosComplement(Self, Len);
  B := ToTwosComplement(AOther, Len);

  SetLength(ResultLimbs, Len);
  {$Q-}{$R-}
  for I := 0 to Len - 1 do
    ResultLimbs[I] := A[I] and B[I];
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

  ResultNeg := FNegative and AOther.FNegative;
  Result := FromTwosComplement(ResultLimbs, ResultNeg);
end;

function TBigInteger.BitwiseOr(const AOther: TBigInteger): TBigInteger;
var
  Len, I: Integer;
  A, B: TLimbArray;
  ResultNeg: Boolean;
  ResultLimbs: TLimbArray;
begin
  if IsZero then Exit(AOther);
  if AOther.IsZero then Exit(Self);

  Len := Length(FLimbs);
  if Length(AOther.FLimbs) > Len then
    Len := Length(AOther.FLimbs);
  Inc(Len);

  A := ToTwosComplement(Self, Len);
  B := ToTwosComplement(AOther, Len);

  SetLength(ResultLimbs, Len);
  {$Q-}{$R-}
  for I := 0 to Len - 1 do
    ResultLimbs[I] := A[I] or B[I];
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

  ResultNeg := FNegative or AOther.FNegative;
  Result := FromTwosComplement(ResultLimbs, ResultNeg);
end;

function TBigInteger.BitwiseXor(const AOther: TBigInteger): TBigInteger;
var
  Len, I: Integer;
  A, B: TLimbArray;
  ResultNeg: Boolean;
  ResultLimbs: TLimbArray;
begin
  Len := Length(FLimbs);
  if Length(AOther.FLimbs) > Len then
    Len := Length(AOther.FLimbs);
  Inc(Len);

  A := ToTwosComplement(Self, Len);
  B := ToTwosComplement(AOther, Len);

  SetLength(ResultLimbs, Len);
  {$Q-}{$R-}
  for I := 0 to Len - 1 do
    ResultLimbs[I] := A[I] xor B[I];
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

  ResultNeg := FNegative xor AOther.FNegative;
  Result := FromTwosComplement(ResultLimbs, ResultNeg);
end;

// ES2026 §6.1.6.2.11 BigInt::bitwiseNOT — ~x = -(x + 1)
function TBigInteger.BitwiseNot: TBigInteger;
begin
  Result := Add(One).Negate;
end;

function TBigInteger.ShiftLeft(const ACount: Integer): TBigInteger;
var
  LimbShift, BitShift, NewLen, I: Integer;
begin
  if IsZero or (ACount = 0) then
    Exit(Self);

  if ACount < 0 then
    Exit(ShiftRight(-ACount));

  LimbShift := ACount div LIMB_BITS;
  BitShift := ACount mod LIMB_BITS;

  NewLen := Length(FLimbs) + LimbShift + 1;
  SetLength(Result.FLimbs, NewLen);
  for I := 0 to NewLen - 1 do
    Result.FLimbs[I] := 0;

  {$Q-}{$R-}
  for I := 0 to High(FLimbs) do
  begin
    Result.FLimbs[I + LimbShift] := Result.FLimbs[I + LimbShift] or
      (FLimbs[I] shl BitShift);
    if (BitShift > 0) and (I + LimbShift + 1 < NewLen) then
      Result.FLimbs[I + LimbShift + 1] := Result.FLimbs[I + LimbShift + 1] or
        (FLimbs[I] shr (LIMB_BITS - BitShift));
  end;
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

  Result.FNegative := FNegative;
  Result.Normalize;
end;

// ES2026 §6.1.6.2.10 BigInt::signedRightShift — floor division semantics
function TBigInteger.ShiftRight(const ACount: Integer): TBigInteger;
var
  LimbShift, BitShift, NewLen, I: Integer;
  RoundDown: Boolean;
begin
  if IsZero or (ACount = 0) then
    Exit(Self);

  if ACount < 0 then
    Exit(ShiftLeft(-ACount));

  LimbShift := ACount div LIMB_BITS;
  BitShift := ACount mod LIMB_BITS;

  if LimbShift >= Length(FLimbs) then
  begin
    // All bits shifted out
    if FNegative then
      Exit(NegativeOne)
    else
      Exit(Zero);
  end;

  NewLen := Length(FLimbs) - LimbShift;
  SetLength(Result.FLimbs, NewLen);

  {$Q-}{$R-}
  for I := 0 to NewLen - 1 do
  begin
    Result.FLimbs[I] := FLimbs[I + LimbShift] shr BitShift;
    if (BitShift > 0) and (I + LimbShift + 1 < Length(FLimbs)) then
      Result.FLimbs[I] := Result.FLimbs[I] or
        (FLimbs[I + LimbShift + 1] shl (LIMB_BITS - BitShift));
  end;
  {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}

  Result.FNegative := FNegative;
  Result.Normalize;

  // For negative numbers, round toward -Infinity (floor division)
  if FNegative and (not Result.IsZero or True) then
  begin
    // Check if any shifted-out bits were set
    RoundDown := False;
    for I := 0 to LimbShift - 1 do
    begin
      if FLimbs[I] <> 0 then
      begin
        RoundDown := True;
        Break;
      end;
    end;
    if (not RoundDown) and (BitShift > 0) then
    begin
      {$Q-}{$R-}
      if (FLimbs[LimbShift] and (Cardinal(1) shl BitShift - 1)) <> 0 then
        RoundDown := True;
      {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
    end;
    if RoundDown then
      Result := Result.Subtract(One);
  end;
end;

{ --- Comparison --- }

function TBigInteger.Compare(const AOther: TBigInteger): Integer;
begin
  // Different signs
  if FNegative and (not AOther.FNegative) then
  begin
    if IsZero and AOther.IsZero then
      Exit(0);
    Exit(-1);
  end;
  if (not FNegative) and AOther.FNegative then
  begin
    if IsZero and AOther.IsZero then
      Exit(0);
    Exit(1);
  end;

  // Same sign
  Result := CompareMagnitudes(FLimbs, AOther.FLimbs);
  if FNegative then
    Result := -Result;
end;

function TBigInteger.Equal(const AOther: TBigInteger): Boolean;
begin
  Result := Compare(AOther) = 0;
end;

{ --- AsIntN / AsUintN --- }

// ES2026 §21.2.1 BigInt.asIntN(bits, bigint)
function TBigInteger.AsIntN(const ABits: Integer): TBigInteger;
var
  Mod2N: TBigInteger;
begin
  if ABits <= 0 then
    Exit(Zero);

  Mod2N := AsUintN(ABits);

  // If the top bit (bit ABits-1) is set, subtract 2^ABits
  if Mod2N.GetBit(ABits - 1) then
    Result := Mod2N.Subtract(One.ShiftLeft(ABits))
  else
    Result := Mod2N;
end;

// ES2026 §21.2.2 BigInt.asUintN(bits, bigint)
function TBigInteger.AsUintN(const ABits: Integer): TBigInteger;
var
  LimbCount, I, RemBits: Integer;
begin
  if ABits <= 0 then
    Exit(Zero);

  // Compute value mod 2^ABits using two's complement
  LimbCount := (ABits + LIMB_BITS - 1) div LIMB_BITS;

  // Get two's complement representation
  if FNegative then
  begin
    Result := FromTwosComplement(
      ToTwosComplement(Self, LimbCount + 1), False);
    // Truncate to LimbCount limbs
    if Length(Result.FLimbs) > LimbCount then
      SetLength(Result.FLimbs, LimbCount);
  end
  else
  begin
    Result.FNegative := False;
    SetLength(Result.FLimbs, LimbCount);
    for I := 0 to LimbCount - 1 do
    begin
      if I < Length(FLimbs) then
        Result.FLimbs[I] := FLimbs[I]
      else
        Result.FLimbs[I] := 0;
    end;
  end;

  // Mask off the top limb
  RemBits := ABits mod LIMB_BITS;
  if (RemBits > 0) and (Length(Result.FLimbs) > 0) then
  begin
    {$Q-}{$R-}
    Result.FLimbs[LimbCount - 1] := Result.FLimbs[LimbCount - 1] and
      (Cardinal(1) shl RemBits - 1);
    {$IFDEF PRODUCTION}{$Q-}{$R-}{$ELSE}{$Q+}{$R+}{$ENDIF}
  end;

  Result.Normalize;
end;

end.
