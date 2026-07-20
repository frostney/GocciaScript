unit Goccia.NumberExponentiation;

{$I Goccia.inc}

interface

function NumberExponentiation(const ABase, AExponent: Double): Double;

implementation

uses
  Math,

  NumberBits;

function IsOddIntegralNumber(const AValue: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := not IsNan(AValue) and not IsInfinite(AValue) and
    (Frac(AValue) = 0.0) and (Frac(AValue / 2.0) <> 0.0);
end;

function CanonicalNaN: Double; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := BitsToDouble(CANONICAL_FLOAT64_NAN_BITS);
end;

// ES2026 §6.1.6.1.3 Number::exponentiate(base, exponent)
function NumberExponentiation(const ABase, AExponent: Double): Double;
var
  AbsoluteBase: Double;
begin
  // ES2026 §6.1.6.1.3 steps 1-3: exponent zero takes precedence.
  if IsNan(AExponent) then
    Exit(CanonicalNaN);
  if AExponent = 0.0 then
    Exit(1.0);
  if IsNan(ABase) then
    Exit(CanonicalNaN);

  // ES2026 §6.1.6.1.3 steps 4-5: infinite bases preserve odd signs.
  if IsInfinite(ABase) then
  begin
    if ABase > 0.0 then
    begin
      if AExponent > 0.0 then
        Exit(Infinity);
      Exit(0.0);
    end;

    if AExponent > 0.0 then
    begin
      if IsOddIntegralNumber(AExponent) then
        Exit(NegInfinity);
      Exit(Infinity);
    end;
    if IsOddIntegralNumber(AExponent) then
      Exit(BitsToDouble(UInt64(1) shl 63));
    Exit(0.0);
  end;

  // ES2026 §6.1.6.1.3 steps 6-7: signed-zero bases preserve odd signs.
  if ABase = 0.0 then
  begin
    if not IsNegativeZero(ABase) then
    begin
      if AExponent > 0.0 then
        Exit(0.0);
      Exit(Infinity);
    end;

    if AExponent > 0.0 then
    begin
      if IsOddIntegralNumber(AExponent) then
        Exit(BitsToDouble(UInt64(1) shl 63));
      Exit(0.0);
    end;
    if IsOddIntegralNumber(AExponent) then
      Exit(NegInfinity);
    Exit(Infinity);
  end;

  // ES2026 §6.1.6.1.3 steps 9-10: infinite exponents compare |base| to 1.
  if IsInfinite(AExponent) then
  begin
    AbsoluteBase := Abs(ABase);
    if AbsoluteBase = 1.0 then
      Exit(CanonicalNaN);
    if (AbsoluteBase > 1.0) = (AExponent > 0.0) then
      Exit(Infinity);
    Exit(0.0);
  end;

  // ES2026 §6.1.6.1.3 step 12: negative base requires an integral exponent.
  if (ABase < 0.0) and (Frac(AExponent) <> 0.0) then
    Exit(CanonicalNaN);

  // ES2026 §6.1.6.1.3 step 13: implementation-approximated exponentiation.
  Result := Power(ABase, AExponent);
end;

end.
