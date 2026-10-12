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
  // Above this many nanoseconds (a little over a day) the seconds value has
  // too many digits for the integer path below to be sure it rounds as
  // Format does, so FormatDuration hands it to Format.
  INTEGER_FORMAT_LIMIT_NANOSECONDS = Int64(100000000000000);

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

// Writes ANanoseconds / (AUnit * 100) with two decimals, as Format's '%.2f'
// does, without converting through the floating-point formatter or the
// codepage manager. Returns False for a value exactly halfway between two
// hundredths, whose rounding FormatDuration leaves to Format.
function TryFormatHundredths(const ANanoseconds, AUnit: Int64;
  const ASuffix: string; out AResult: string): Boolean;
var
  Hundredths, Remainder: Int64;
  Text: TDurationText;
begin
  Hundredths := ANanoseconds div AUnit;
  Remainder := ANanoseconds mod AUnit;
  Result := Remainder * 2 <> AUnit;
  if not Result then
    Exit;
  if Remainder * 2 > AUnit then
    Inc(Hundredths);
  Text.Start := Length(Text.Chars);
  PrependSuffix(Text, ASuffix);
  PrependChar(Text, Char(Ord('0') + Hundredths mod 10));
  PrependChar(Text, Char(Ord('0') + (Hundredths div 10) mod 10));
  PrependChar(Text, '.');
  PrependInteger(Text, Hundredths div 100);
  AResult := DurationTextString(Text);
end;

function FormatNanoseconds(const ANanoseconds: Int64): string;
var
  Text: TDurationText;
begin
  Text.Start := Length(Text.Chars);
  PrependSuffix(Text, 'ns');
  PrependInteger(Text, ANanoseconds);
  Result := DurationTextString(Text);
end;

// The integer path writes the same characters as Format only when the
// decimal separator is '.', and the same bytes reach the output only when
// strings round-trip through the system codepage unchanged (UTF-8).
function IntegerFormatMatchesFormat: Boolean;
begin
  Result := (DefaultSystemCodePage = CP_UTF8) and
    (DefaultFormatSettings.DecimalSeparator = '.');
end;

function FormatDuration(const ANanoseconds: Int64): string;
var
  Us: Double;
  Ms: Double;
  S: Double;
begin
  if (ANanoseconds < INTEGER_FORMAT_LIMIT_NANOSECONDS) and
     IntegerFormatMatchesFormat then
  begin
    // The same unit thresholds as the Double comparisons below: S >= 10.0,
    // Ms >= 0.5 and Us >= 0.5.
    if ANanoseconds >= 10000000000 then
    begin
      if TryFormatHundredths(ANanoseconds, 10000000, 's', Result) then
        Exit;
    end
    else if ANanoseconds >= 500000 then
    begin
      if TryFormatHundredths(ANanoseconds, 10000, 'ms', Result) then
        Exit;
    end
    else if ANanoseconds >= 500 then
    begin
      if TryFormatHundredths(ANanoseconds, 10, MICRO_SIGN + 's', Result) then
        Exit;
    end
    else
      Exit(FormatNanoseconds(ANanoseconds));
  end;

  Us := ANanoseconds / 1000.0;
  Ms := Us / 1000.0;
  S := Ms / 1000.0;
  if S >= 10.0 then
    Result := SysUtils.Format('%.2fs', [S])
  else if Ms >= 0.5 then
    Result := SysUtils.Format('%.2fms', [Ms])
  else if Us >= 0.5 then
    Result := SysUtils.Format('%.2f' + #$00B5 + 's', [Us])
  else
    Result := SysUtils.Format('%dns', [ANanoseconds]);
end;

{$IFDEF MSWINDOWS}
initialization
  QueryPerformanceFrequency(QPCFrequency);
{$ENDIF}

end.
