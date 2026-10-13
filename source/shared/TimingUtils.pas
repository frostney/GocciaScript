unit TimingUtils;

{$I Shared.inc}

interface

function GetMilliseconds: Int64;
function GetNanoseconds: Int64;
function GetEpochNanoseconds: Int64;
function FormatDuration(const ANanoseconds: Int64): string;

implementation

uses
  SysUtils,
  DateUtils
  {$IFDEF UNIX},
  ctypes{$ENDIF}
  {$IFDEF MSWINDOWS},
  Windows{$ENDIF};

{$IFDEF UNIX}
const
  CLOCK_REALTIME_ID = 0;
  {$IFDEF DARWIN}
  // CLOCK_MONOTONIC_RAW. Darwin's CLOCK_MONOTONIC (6) advances in whole
  // microseconds, so an interval shorter than that measures as 0 or 1000 ns:
  // a benchmark sample under half a microsecond then has a median of zero.
  // The raw clock counts Mach ticks at nanosecond resolution, is monotonic,
  // and likewise keeps advancing while the system sleeps.
  CLOCK_MONOTONIC_ID = 4;
  {$ELSE}
  CLOCK_MONOTONIC_ID = 1;
  {$ENDIF}

type
  TNanoTimeSpec = record
    tv_sec: clong;
    tv_nsec: clong;
  end;

function clock_gettime(clk_id: cint; tp: Pointer): cint; cdecl; external 'c';
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  QPCFrequency: Int64;
{$ENDIF}

function GetNanoseconds: Int64;
{$IFDEF UNIX}
var
  ts: TNanoTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC_ID, @ts);
  Result := Int64(ts.tv_sec) * 1000000000 + Int64(ts.tv_nsec);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  Counter, Whole, Remainder: Int64;
begin
  QueryPerformanceCounter(Counter);
  Whole := Counter div QPCFrequency;
  Remainder := Counter mod QPCFrequency;
  Result := Whole * 1000000000 + (Remainder * 1000000000) div QPCFrequency;
end;
{$ELSE}
begin
  Result := GetTickCount64 * 1000000;
end;
{$ENDIF}
{$ENDIF}

function GetMilliseconds: Int64;
begin
  Result := GetNanoseconds div 1000000;
end;

function GetEpochNanoseconds: Int64;
{$IFDEF UNIX}
var
  ts: TNanoTimeSpec;
begin
  clock_gettime(CLOCK_REALTIME_ID, @ts);
  Result := Int64(ts.tv_sec) * 1000000000 + Int64(ts.tv_nsec);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  FT: TFileTime;
  Ticks: Int64;
begin
  GetSystemTimeAsFileTime(FT);
  Ticks := (Int64(FT.dwHighDateTime) shl 32) or Int64(FT.dwLowDateTime);
  Result := (Ticks - 116444736000000000) * 100;
end;
{$ELSE}
begin
  Result := Int64(DateTimeToUnix(Now)) * 1000000000;
end;
{$ENDIF}
{$ENDIF}

const
  MICRO_SIGN = #$00B5;
  // Format writes an integer part of up to this many digits in fixed
  // notation; FormatDuration leaves anything larger to it.
  FIXED_INTEGER_PART_LIMIT = Int64(1000000000000);
  // How close, relative to the value, the exact value may come to halfway
  // between two hundredths before FormatDuration leaves the rounding to
  // Format, whose digit generation is exact only to about 1e-15.
  HALFWAY_MARGIN_SHIFT = 40;

type
  TDurationText = record
    Chars: array[0..31] of Char;
    Start: Integer;
  end;

procedure PrependChar(var AText: TDurationText; const AChar: Char); inline;
begin
  Dec(AText.Start);
  AText.Chars[AText.Start] := AChar;
end;

procedure PrependSuffix(var AText: TDurationText; const ASuffix: string);
var
  I: Integer;
begin
  for I := Length(ASuffix) downto 1 do
    PrependChar(AText, ASuffix[I]);
end;

// Prepends the decimal digits of AValue, with a sign when negative.
procedure PrependInteger(var AText: TDurationText; const AValue: Int64);
var
  Remaining: UInt64;
begin
  if AValue < 0 then
    Remaining := UInt64(-(AValue + 1)) + 1
  else
    Remaining := UInt64(AValue);
  repeat
    PrependChar(AText, Char(Ord('0') + Remaining mod 10));
    Remaining := Remaining div 10;
  until Remaining = 0;
  if AValue < 0 then
    PrependChar(AText, '-');
end;

function DurationTextString(const AText: TDurationText): string;
begin
  SetString(Result, PChar(@AText.Chars[AText.Start]),
    Length(AText.Chars) - AText.Start);
end;

// Writes AValue with two decimals and ASuffix as Format's '%.2f' does,
// without the floating-point formatter or the codepage manager. AValue is
// split into its binary mantissa and exponent, so hundredths and the
// remainder below them are exact. Returns False, leaving the value to
// Format, when AValue is not a positive normal number below
// FIXED_INTEGER_PART_LIMIT or lies too close to halfway between two
// hundredths for the exact rounding to be sure to agree with Format's.
function TryFormatFixedHundredths(const AValue: Double;
  const ASuffix: string; out AResult: string): Boolean;
var
  Bits, Mantissa, Scaled, Hundredths, Remainder, Half, Distance: UInt64;
  Exponent, Shift: Integer;
  Text: TDurationText;
begin
  Result := False;
  Move(AValue, Bits, SizeOf(Bits));
  if (Bits shr 63) <> 0 then
    Exit;
  Exponent := Integer((Bits shr 52) and $7FF);
  if (Exponent = 0) or (Exponent = $7FF) then
    Exit;
  Mantissa := (Bits and UInt64($000FFFFFFFFFFFFF)) or
    UInt64($0010000000000000);
  // AValue = Mantissa / 2^Shift.
  Shift := 1075 - Exponent;
  if (Shift < 1) or (Shift > 62) then
    Exit;
  Scaled := Mantissa * 100;
  Hundredths := Scaled shr Shift;
  Remainder := Scaled - (Hundredths shl Shift);
  Half := UInt64(1) shl (Shift - 1);
  if Remainder >= Half then
    Distance := Remainder - Half
  else
    Distance := Half - Remainder;
  if Distance <= (Scaled shr HALFWAY_MARGIN_SHIFT) + 1 then
    Exit;
  if Remainder > Half then
    Inc(Hundredths);
  if Hundredths div 100 >= UInt64(FIXED_INTEGER_PART_LIMIT) then
    Exit;
  Text.Start := Length(Text.Chars);
  PrependSuffix(Text, ASuffix);
  PrependChar(Text, Char(Ord('0') + Hundredths mod 10));
  PrependChar(Text, Char(Ord('0') + (Hundredths div 10) mod 10));
  PrependChar(Text, '.');
  PrependInteger(Text, Int64(Hundredths div 100));
  AResult := DurationTextString(Text);
  Result := True;
end;

// Format writes the same characters only when the decimal separator is '.',
// and the same bytes reach the output only when strings round-trip through
// the system codepage unchanged (UTF-8).
function DirectFormattingMatchesFormat: Boolean;
begin
  Result := (DefaultSystemCodePage = CP_UTF8) and
    (DefaultFormatSettings.DecimalSeparator = '.');
end;

function FormatDuration(const ANanoseconds: Int64): string;
var
  Us: Double;
  Ms: Double;
  S: Double;
  Direct: Boolean;
  Text: TDurationText;
begin
  // The unit values keep their original arithmetic, including the precision
  // of the 1000.0 constants, so the digits match those Format wrote.
  Us := ANanoseconds / 1000.0;
  Ms := Us / 1000.0;
  S := Ms / 1000.0;
  Direct := DirectFormattingMatchesFormat;
  if S >= 10.0 then
  begin
    if not (Direct and TryFormatFixedHundredths(S, 's', Result)) then
      Result := SysUtils.Format('%.2fs', [S]);
  end
  else if Ms >= 0.5 then
  begin
    if not (Direct and TryFormatFixedHundredths(Ms, 'ms', Result)) then
      Result := SysUtils.Format('%.2fms', [Ms]);
  end
  else if Us >= 0.5 then
  begin
    if not (Direct and
       TryFormatFixedHundredths(Us, MICRO_SIGN + 's', Result)) then
      Result := SysUtils.Format('%.2f' + MICRO_SIGN + 's', [Us]);
  end
  else if Direct then
  begin
    Text.Start := Length(Text.Chars);
    PrependSuffix(Text, 'ns');
    PrependInteger(Text, ANanoseconds);
    Result := DurationTextString(Text);
  end
  else
    Result := SysUtils.Format('%dns', [ANanoseconds]);
end;

{$IFDEF MSWINDOWS}
initialization
  QueryPerformanceFrequency(QPCFrequency);
{$ENDIF}

end.
