unit G2.Date;
{ 21.4 Date objects, following the spec's steps: time values are Numbers of milliseconds since the epoch, the
  day and time arithmetic of 21.4.1 works on Doubles, and the local time zone is the system's (TZ, else
  /etc/localtime), read through the C library: localtime_r gives the offset at an instant, and UTC(t) picks
  among the offsets around a local time as 21.4.1.26 says (the earlier instant of a repeated local time, the
  offset before a skipped one).

  Date.parse reads the Date Time String Format (21.4.1.32, with expanded years) and the formats toString and
  toUTCString produce. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

procedure InitDate;

implementation

uses
  BaseUnix, Unix, Linux, UnixType, SysUtils, Math, G2.Core, G2.VM;

{$linklib c}

const
  okDate = 18;
  { typed: an untyped float constant is Extended, and the spec's arithmetic is Double's }
  msPerSecond: Double = 1000.0;
  msPerMinute: Double = 60000.0;
  msPerHour: Double = 3600000.0;
  msPerDay: Double = 86400000.0;
  MaxTime: Double = 8.64e15;

type
  TArgs = PValue;
  Ttm = record
    tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_yday, tm_isdst: Int32;
    tm_gmtoff: Int64;
    tm_zone: PAnsiChar;
  end;
  Ptm = ^Ttm;

function localtime_r(const T: PInt64; Res: Ptm): Ptm; cdecl; external 'c';
procedure tzset; cdecl; external 'c';

threadvar
  DateProtoV: TValue;

const
  DayNames: array[0..6] of UnicodeString = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MonthNames: array[0..11] of UnicodeString = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
    'Oct', 'Nov', 'Dec');

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

{ ---------------- 21.4.1 time arithmetic ---------------- }

{ Math.Floor returns an Integer: time values need the whole Double range }
function FloorD(X: Double): Double; inline;
begin
  Result := Int(X);
  if Result > X then Result := Result - 1;
end;

function FloorDiv(A, B: Double): Double; inline;
begin
  Result := FloorD(A / B);
end;

function PosMod(A, B: Double): Double; inline;
begin
  Result := A - FloorD(A / B) * B;
end;

function Day(T: Double): Double; inline;
begin
  Result := FloorD(T / msPerDay);
end;

function TimeWithinDay(T: Double): Double; inline;
begin
  Result := PosMod(T, msPerDay);
end;

function DaysInYear(Y: Double): Integer;
begin
  if PosMod(Y, 4) <> 0 then Exit(365);
  if PosMod(Y, 100) <> 0 then Exit(366);
  if PosMod(Y, 400) <> 0 then Exit(365);
  Result := 366;
end;

function DayFromYear(Y: Double): Double; inline;
begin
  Result := 365.0 * (Y - 1970) + FloorD((Y - 1969) / 4) - FloorD((Y - 1901) / 100) + FloorD((Y - 1601) / 400);
end;

function TimeFromYear(Y: Double): Double; inline;
begin
  Result := msPerDay * DayFromYear(Y);
end;

function YearFromTime(T: Double): Double;
var Y: Double;
begin
  Y := FloorD(T / (msPerDay * 365.2425)) + 1970;
  while TimeFromYear(Y) > T do Y := Y - 1;
  while TimeFromYear(Y + 1) <= T do Y := Y + 1;
  Result := Y;
end;

function InLeapYear(T: Double): Boolean; inline;
begin
  Result := DaysInYear(YearFromTime(T)) = 366;
end;

const
  CumDays: array[Boolean, 0..12] of Integer = (
    (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
    (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366));

{ month (0..11) and date (1..31) of T }
procedure MonthDate(T: Double; out Month, Date: Integer);
var DWY: Integer; Leap: Boolean;
begin
  DWY := Trunc(Day(T) - DayFromYear(YearFromTime(T)));
  Leap := InLeapYear(T);
  Month := 0;
  while (Month < 11) and (DWY >= CumDays[Leap, Month + 1]) do Inc(Month);
  Date := DWY - CumDays[Leap, Month] + 1;
end;

function MonthFromTime(T: Double): Integer;
var D: Integer;
begin
  MonthDate(T, Result, D);
end;

function DateFromTime(T: Double): Integer;
var M: Integer;
begin
  MonthDate(T, M, Result);
end;

function WeekDay(T: Double): Integer; inline;
begin
  Result := Trunc(PosMod(Day(T) + 4, 7));
end;

function HourFromTime(T: Double): Integer; inline;
begin
  Result := Trunc(PosMod(FloorD(T / msPerHour), 24));
end;

function MinFromTime(T: Double): Integer; inline;
begin
  Result := Trunc(PosMod(FloorD(T / msPerMinute), 60));
end;

function SecFromTime(T: Double): Integer; inline;
begin
  Result := Trunc(PosMod(FloorD(T / msPerSecond), 60));
end;

function MsFromTime(T: Double): Integer; inline;
begin
  Result := Trunc(PosMod(T, msPerSecond));
end;

function ToInteger(D: Double): Double; inline;
begin
  if IsNaN(D) then Exit(0);
  Result := Int(D);
end;

function Finite(D: Double): Boolean; inline;
begin
  Result := not IsNaN(D) and not IsInfinite(D);
end;

{ 21.4.1.27 MakeTime }
function MakeTime(H, M, S, Ms: Double): Double;
var A, B: Double;
begin
  if not (Finite(H) and Finite(M) and Finite(S) and Finite(Ms)) then Exit(NaN);
  { each step rounded to a Double, in the spec's order }
  A := Int(H) * msPerHour;
  B := Int(M) * msPerMinute;
  A := A + B;
  B := Int(S) * msPerSecond;
  A := A + B;
  Result := A + Int(Ms);
end;

{ 21.4.1.28 MakeDay }
function MakeDay(Year, Month, Date: Double): Double;
var Y, M, Dt, YM, Mn, T: Double;
begin
  if not (Finite(Year) and Finite(Month) and Finite(Date)) then Exit(NaN);
  Y := Int(Year); M := Int(Month); Dt := Int(Date);
  YM := Y + FloorD(M / 12);
  if not Finite(YM) or (Abs(YM) > 400000) then Exit(NaN);   { past any time value TimeClip keeps }
  Mn := PosMod(M, 12);
  T := DayFromYear(YM) + CumDays[DaysInYear(YM) = 366, Trunc(Mn)];
  Result := T + Dt - 1;
end;

{ 21.4.1.29 MakeDate }
function MakeDate(D, T: Double): Double;
begin
  if not (Finite(D) and Finite(T)) then Exit(NaN);
  Result := D * msPerDay;
  Result := Result + T;
  if not Finite(Result) then Result := NaN;
end;

{ 21.4.1.31 TimeClip }
function TimeClip(T: Double): Double;
begin
  if not Finite(T) or (Abs(T) > MaxTime) then Exit(NaN);
  Result := Int(T) + 0.0;   { -0 becomes +0 }
end;

{ ---------------- local time ---------------- }

{ the system zone's offset from UTC at instant T (ms), and its abbreviation }
function OffsetAt(T: Double; out Abbr: UnicodeString): Double;
var S: Int64; TM: Ttm;
begin
  Abbr := '';
  S := Trunc(FloorD(T / 1000));
  FillChar(TM, SizeOf(TM), 0);
  if localtime_r(@S, @TM) = nil then Exit(0);
  if TM.tm_zone <> nil then Abbr := UnicodeString(AnsiString(TM.tm_zone));
  Result := TM.tm_gmtoff * 1000.0;
end;

function Offset(T: Double): Double;
var A: UnicodeString;
begin
  Result := OffsetAt(T, A);
end;

{ 21.4.1.25 LocalTime }
function LocalTime(T: Double): Double; inline;
begin
  Result := T + Offset(T);
end;

{ 21.4.1.26 UTC(t): the instants whose local time is T; the earliest of them, else (a skipped local time) the
  offset in effect before the transition }
function UTCOf(T: Double): Double;
var OB, OA, ON, Best: Double; Found: Boolean;
  procedure Try_(O: Double);
  begin
    if Offset(T - O) = O then
      if not Found or (T - O < Best) then begin Best := T - O; Found := True; end;
  end;
begin
  if not Finite(T) then Exit(NaN);
  OB := Offset(T - msPerDay);
  OA := Offset(T + msPerDay);
  ON := Offset(T);
  Found := False;
  Best := 0;
  Try_(OB); Try_(OA); Try_(ON);
  if Found then Result := Best else Result := T - OB;
end;

{ ---------------- the Date object ---------------- }

function ThisTime(Args: TArgs; var T: Double): Boolean;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okDate) then
    Exit(ThrowTypeError('this is not a Date object.'));
  T := NumOf(ObjOf(Args[0])^.Aux1);
  Result := True;
end;

procedure SetThisTime(Args: TArgs; T: Double);
begin
  ObjOf(Args[0])^.Aux1 := MkNumber(T);
end;

function NowMs: Double;
var TV: TTimeVal;
begin
  TV.tv_sec := 0; TV.tv_usec := 0;
  fpgettimeofday(@TV, nil);
  Result := Int64(TV.tv_sec) * 1000.0 + Int64(TV.tv_usec) div 1000;
end;

function Pad(N: Int64; W: Integer): UnicodeString;
begin
  Result := UnicodeString(IntToStr(Abs(N)));
  while Length(Result) < W do Result := '0' + Result;
end;

function YearString(Y: Double): UnicodeString;
begin
  if Y >= 0 then Result := Pad(Trunc(Y), 4) else Result := '-' + Pad(Trunc(-Y), 4);
end;

{ 21.4.4.41.2 DateString: "Tue Feb 01 2022" of a local time }
function DateStr(TV: Double): UnicodeString;
var M, D: Integer;
begin
  MonthDate(TV, M, D);
  Result := DayNames[WeekDay(TV)] + ' ' + MonthNames[M] + ' ' + Pad(D, 2) + ' ' + YearString(YearFromTime(TV));
end;

{ 21.4.4.41.1 TimeString: "00:00:00 GMT" }
function TimeStr(TV: Double): UnicodeString;
begin
  Result := Pad(HourFromTime(TV), 2) + ':' + Pad(MinFromTime(TV), 2) + ':' + Pad(SecFromTime(TV), 2) + ' GMT';
end;

{ 21.4.4.41.3 TimeZoneString: "+0100 (CET)" of an instant }
function TZStr(TV: Double): UnicodeString;
var O: Double; Abbr: UnicodeString; Mins: Int64;
begin
  O := OffsetAt(TV, Abbr);
  Mins := Round(O / msPerMinute);
  if Mins >= 0 then Result := '+' else Result := '-';
  Result := Result + Pad(Abs(Mins) div 60, 2) + Pad(Abs(Mins) mod 60, 2);
  if Abbr <> '' then Result := Result + ' (' + Abbr + ')';
end;

{ 21.4.4.41.4 ToDateString }
function ToDateString(TV: Double): UnicodeString;
var T: Double;
begin
  if IsNaN(TV) then Exit('Invalid Date');
  T := LocalTime(TV);
  Result := DateStr(T) + ' ' + TimeStr(T) + TZStr(TV);
end;

function ISOString(TV: Double): UnicodeString;
var Y: Double; M, D: Integer;
begin
  Y := YearFromTime(TV);
  MonthDate(TV, M, D);
  if (Y >= 0) and (Y <= 9999) then Result := Pad(Trunc(Y), 4)
  else if Y < 0 then Result := '-' + Pad(Trunc(-Y), 6)
  else Result := '+' + Pad(Trunc(Y), 6);
  Result := Result + '-' + Pad(M + 1, 2) + '-' + Pad(D, 2) + 'T' + Pad(HourFromTime(TV), 2) + ':' +
    Pad(MinFromTime(TV), 2) + ':' + Pad(SecFromTime(TV), 2) + '.' + Pad(MsFromTime(TV), 3) + 'Z';
end;

function UTCString(TV: Double): UnicodeString;
var M, D: Integer;
begin
  MonthDate(TV, M, D);
  Result := DayNames[WeekDay(TV)] + ', ' + Pad(D, 2) + ' ' + MonthNames[M] + ' ' + YearString(YearFromTime(TV)) +
    ' ' + TimeStr(TV);
end;

{ ---------------- parsing ---------------- }

type
  TDP = record
    S: UnicodeString;
    P: Integer;
  end;

function Digits(var R: TDP; N: Integer; out V: Integer): Boolean;
var I: Integer;
begin
  V := 0;
  for I := 1 to N do
  begin
    if (R.P > Length(R.S)) or (R.S[R.P] < '0') or (R.S[R.P] > '9') then Exit(False);
    V := V * 10 + Ord(R.S[R.P]) - Ord('0');
    Inc(R.P);
  end;
  Result := True;
end;

function Peek(var R: TDP): WideChar; inline;
begin
  if R.P <= Length(R.S) then Result := R.S[R.P] else Result := #0;
end;

function DaysInMonth(Y, M: Integer): Integer;
begin
  Result := CumDays[DaysInYear(Y) = 366, M] - CumDays[DaysInYear(Y) = 366, M - 1];
end;

{ 21.4.1.32 Date Time String Format, with expanded years; NaN when S is not an instance of it }
function ParseISO(const S: UnicodeString): Double;
var R: TDP; Y, Mo, D, H, Mi, Sec, Ms, OH, OM, Frac, I: Integer; Sign: Integer; HasTime, HasOffset: Boolean;
  OffMs, T: Double;
begin
  Result := NaN;
  R.S := S; R.P := 1;
  Sign := 1;
  if (Peek(R) = '+') or (Peek(R) = '-') then
  begin
    if Peek(R) = '-' then Sign := -1;
    Inc(R.P);
    if not Digits(R, 6, Y) then Exit;
    if (Sign = -1) and (Y = 0) then Exit;   { -000000 is not a year }
    Y := Y * Sign;
  end
  else if not Digits(R, 4, Y) then Exit;
  Mo := 1; D := 1; H := 0; Mi := 0; Sec := 0; Ms := 0;
  if Peek(R) = '-' then
  begin
    Inc(R.P);
    if not Digits(R, 2, Mo) then Exit;
    if Peek(R) = '-' then
    begin
      Inc(R.P);
      if not Digits(R, 2, D) then Exit;
    end;
  end;
  if (Mo < 1) or (Mo > 12) or (D < 1) or (D > DaysInMonth(Y, Mo)) then Exit;
  HasTime := False;
  HasOffset := False;
  OffMs := 0;
  if Peek(R) = 'T' then
  begin
    HasTime := True;
    Inc(R.P);
    if not Digits(R, 2, H) then Exit;
    if Peek(R) <> ':' then Exit;
    Inc(R.P);
    if not Digits(R, 2, Mi) then Exit;
    if Peek(R) = ':' then
    begin
      Inc(R.P);
      if not Digits(R, 2, Sec) then Exit;
      if Peek(R) = '.' then
      begin
        Inc(R.P);
        if (Peek(R) < '0') or (Peek(R) > '9') then Exit;
        Ms := 0;
        I := 0;
        while (Peek(R) >= '0') and (Peek(R) <= '9') do
        begin
          Frac := Ord(Peek(R)) - Ord('0');
          if I < 3 then Ms := Ms * 10 + Frac;
          Inc(I);
          Inc(R.P);
        end;
        while I < 3 do begin Ms := Ms * 10; Inc(I); end;
      end;
    end;
    if (H > 24) or (Mi > 59) or (Sec > 59) then Exit;
    if (H = 24) and ((Mi <> 0) or (Sec <> 0) or (Ms <> 0)) then Exit;
    if Peek(R) = 'Z' then begin HasOffset := True; Inc(R.P); end
    else if (Peek(R) = '+') or (Peek(R) = '-') then
    begin
      if Peek(R) = '-' then Sign := -1 else Sign := 1;
      Inc(R.P);
      if not Digits(R, 2, OH) then Exit;
      if Peek(R) <> ':' then Exit;
      Inc(R.P);
      if not Digits(R, 2, OM) then Exit;
      if (OH > 23) or (OM > 59) then Exit;
      HasOffset := True;
      OffMs := Sign * (OH * msPerHour + OM * msPerMinute);
    end;
  end;
  if R.P <= Length(R.S) then Exit;
  T := MakeDate(MakeDay(Y, Mo - 1, D), MakeTime(H, Mi, Sec, Ms));
  if HasTime and not HasOffset then T := UTCOf(T)   { a date-time form without an offset is local time }
  else T := T - OffMs;
  Result := TimeClip(T);
end;

function MonthIndex(const S: UnicodeString): Integer;
var I: Integer;
begin
  for I := 0 to 11 do if MonthNames[I] = S then Exit(I);
  Result := -1;
end;

{ the formats toString ("Tue Feb 01 2022 00:00:00 GMT+0100 (CET)") and toUTCString ("Tue, 01 Feb 2022 00:00:00
  GMT") produce, the year signed and of any length }
function ParseToStringForm(const S: UnicodeString): Double;
var R: TDP; Mo, D, H, Mi, Sec, OH, OM, Sign, YSign, Y: Integer; T: Double; Name: UnicodeString; UTCForm: Boolean;
  function Word3: UnicodeString;
  begin
    if R.P + 2 > Length(R.S) then Exit('');
    Result := Copy(R.S, R.P, 3);
    Inc(R.P, 3);
  end;
  function Sp: Boolean;
  begin
    Result := Peek(R) = ' ';
    if Result then Inc(R.P);
  end;
  function Year(out V: Integer): Boolean;
  var N: Integer;
  begin
    YSign := 1;
    if Peek(R) = '-' then begin YSign := -1; Inc(R.P); end;
    V := 0; N := 0;
    while (Peek(R) >= '0') and (Peek(R) <= '9') and (N < 7) do
    begin
      V := V * 10 + Ord(Peek(R)) - Ord('0');
      Inc(R.P); Inc(N);
    end;
    V := V * YSign;
    Result := N >= 4;
  end;
begin
  Result := NaN;
  R.S := S; R.P := 1;
  if Word3 = '' then Exit;   { the weekday }
  UTCForm := Peek(R) = ',';
  if UTCForm then
  begin
    Inc(R.P);
    if not Sp then Exit;
    if not Digits(R, 2, D) then Exit;
    if not Sp then Exit;
    Mo := MonthIndex(Word3);
    if Mo < 0 then Exit;
    if not Sp then Exit;
    if not Year(Y) then Exit;
  end
  else
  begin
    if not Sp then Exit;
    Mo := MonthIndex(Word3);
    if Mo < 0 then Exit;
    if not Sp then Exit;
    if not Digits(R, 2, D) then Exit;
    if not Sp then Exit;
    if not Year(Y) then Exit;
  end;
  H := 0; Mi := 0; Sec := 0;
  if Peek(R) = #0 then
  begin
    T := MakeDate(MakeDay(Y, Mo, D), 0);
    if UTCForm then Exit(TimeClip(T));
    Exit(TimeClip(UTCOf(T)));
  end;
  if not Sp then Exit;
  if not Digits(R, 2, H) then Exit;
  if Peek(R) <> ':' then Exit;
  Inc(R.P);
  if not Digits(R, 2, Mi) then Exit;
  if Peek(R) <> ':' then Exit;
  Inc(R.P);
  if not Digits(R, 2, Sec) then Exit;
  T := MakeDate(MakeDay(Y, Mo, D), MakeTime(H, Mi, Sec, 0));
  if Peek(R) = #0 then Exit(TimeClip(UTCOf(T)));
  if not Sp then Exit;
  Name := Word3;
  if Name <> 'GMT' then Exit;
  if (Peek(R) = '+') or (Peek(R) = '-') then
  begin
    if Peek(R) = '-' then Sign := -1 else Sign := 1;
    Inc(R.P);
    if not Digits(R, 2, OH) then Exit;
    if not Digits(R, 2, OM) then Exit;
    T := T - Sign * (OH * msPerHour + OM * msPerMinute);
  end;
  { anything after: the zone's name in parentheses }
  Result := TimeClip(T);
end;

function ParseDate(const S: UnicodeString): Double;
var T: UnicodeString;
begin
  T := Trim(S);
  Result := ParseISO(T);
  if IsNaN(Result) then Result := ParseToStringForm(T);
end;

{ ---------------- constructor and statics ---------------- }

function NumArg(Args: TArgs; Argc, I: Integer; var D: Double): Boolean; inline;
begin
  Result := ToNumberV(Arg(Args, Argc, I), D);
end;

{ the year of the multi-argument forms: 0..99 mean 1900..1999 }
function FullYear(Y: Double): Double;
var YI: Double;
begin
  if IsNaN(Y) then Exit(NaN);
  YI := ToInteger(Y);
  if (YI >= 0) and (YI <= 99) then Result := 1900 + YI else Result := Y;
end;

{ the components of Date(y, m, ...) and Date.UTC: a local or UTC time value }
function ComponentsTime(Args: TArgs; Argc: Integer; var T: Double): Boolean;
var C: array[0..6] of Double; I: Integer;
begin
  C[0] := NaN; C[1] := 0; C[2] := 1; C[3] := 0; C[4] := 0; C[5] := 0; C[6] := 0;
  for I := 0 to 6 do
    if Argc > I then
      if not ToNumberV(Args[I + 1], C[I]) then Exit(False);
  T := MakeDate(MakeDay(FullYear(C[0]), C[1], C[2]), MakeTime(C[3], C[4], C[5], C[6]));
  Result := True;
end;

{ 21.4.2.1 Date(...values) }
function Date_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var TV, D: Double; V, P: TValue; O: PObj;
begin
  if CurNewTarget = VUndefined then
  begin
    Res := StrFromUnicode(ToDateString(NowMs));
    Exit(True);
  end;
  PushRoot(CurNewTarget);
  if Argc = 0 then TV := NowMs
  else if Argc = 1 then
  begin
    if IsObject(Args[1]) and (ObjOf(Args[1])^.OKind = okDate) then TV := NumOf(ObjOf(Args[1])^.Aux1)
    else
    begin
      if not ToPrimitiveHint(Args[1], phDefault, V) then begin PopRoots(1); Exit(False); end;
      if IsString(V) then TV := ParseDate(StrToUnicode(StrOf(V)))
      else
      begin
        if not ToNumberV(V, D) then begin PopRoots(1); Exit(False); end;
        TV := D;
      end;
    end;
    TV := TimeClip(TV);
  end
  else
  begin
    if not ComponentsTime(Args, Argc, TV) then begin PopRoots(1); Exit(False); end;
    TV := TimeClip(UTCOf(TV));
  end;
  { OrdinaryCreateFromConstructor comes after the arguments' conversions }
  CurNewTarget := RootAt(TempTop - 1);
  PopRoots(1);
  if not ProtoFromNewTarget(DateProtoV, P) then Exit(False);
  O := NewObject(RootShapeFor(P), 0, okDate);
  O^.Aux1 := MkNumber(TV);
  Res := MkPtr(O);
  Result := True;
end;

function Date_Now(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(NowMs);
  Result := True;
end;

function Date_Parse(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: TValue;
begin
  if not ToStringV(Arg(Args, Argc, 1), S) then Exit(False);
  Res := MkNumber(ParseDate(StrToUnicode(StrOf(S))));
  Result := True;
end;

{ 21.4.3.4 Date.UTC }
function Date_UTC(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ComponentsTime(Args, Argc, T) then Exit(False);
  Res := MkNumber(TimeClip(T));
  Result := True;
end;

{ ---------------- getters ---------------- }

type
  TPart = (dpYear, dpMonth, dpDate, dpDay, dpHours, dpMinutes, dpSeconds, dpMs);

function GetPart(Args: TArgs; var Res: TValue; Part: TPart; Local: Boolean): Boolean;
var T: Double; M, D: Integer;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then begin Res := MkNumber(NaN); Exit(True); end;
  if Local then T := LocalTime(T);
  case Part of
    dpYear: Res := MkNumber(YearFromTime(T));
    dpMonth: Res := MkInt(MonthFromTime(T));
    dpDate: Res := MkInt(DateFromTime(T));
    dpDay: Res := MkInt(WeekDay(T));
    dpHours: Res := MkInt(HourFromTime(T));
    dpMinutes: Res := MkInt(MinFromTime(T));
    dpSeconds: Res := MkInt(SecFromTime(T));
    dpMs: Res := MkInt(MsFromTime(T));
  end;
  Result := True;
end;

function D_GetFullYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpYear, True); end;
function D_GetMonth(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpMonth, True); end;
function D_GetDate(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpDate, True); end;
function D_GetDay(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpDay, True); end;
function D_GetHours(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpHours, True); end;
function D_GetMinutes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpMinutes, True); end;
function D_GetSeconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpSeconds, True); end;
function D_GetMilliseconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpMs, True); end;
function D_GetUTCFullYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpYear, False); end;
function D_GetUTCMonth(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpMonth, False); end;
function D_GetUTCDate(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpDate, False); end;
function D_GetUTCDay(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpDay, False); end;
function D_GetUTCHours(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpHours, False); end;
function D_GetUTCMinutes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpMinutes, False); end;
function D_GetUTCSeconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpSeconds, False); end;
function D_GetUTCMilliseconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GetPart(Args, Res, dpMs, False); end;

function D_GetTime(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  Res := MkNumber(T);
  Result := True;
end;

function D_GetTimezoneOffset(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then begin Res := MkNumber(NaN); Exit(True); end;
  Res := MkNumber((T - LocalTime(T)) / msPerMinute);
  Result := True;
end;

{ B.2.3.1 getYear }
function D_GetYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then begin Res := MkNumber(NaN); Exit(True); end;
  Res := MkNumber(YearFromTime(LocalTime(T)) - 1900);
  Result := True;
end;

{ ---------------- setters (21.4.4.20 - 21.4.4.31) ---------------- }

function D_SetTime(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T, V: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if not NumArg(Args, Argc, 1, V) then Exit(False);
  V := TimeClip(V);
  SetThisTime(Args, V);
  Res := MkNumber(V);
  Result := True;
end;

type
  TSetter = (stMs, stSeconds, stMinutes, stHours, stDate, stMonth, stFullYear);

{ the setters: the arguments converted in order (all of them, even for an invalid date), then the new time
  from the fields of the old one the arguments do not give }
function SetFields(Args: TArgs; Argc: Integer; var Res: TValue; Which: TSetter; Local: Boolean): Boolean;
var T, U: Double; A: array[0..3] of Double; Have: array[0..3] of Boolean; N, I: Integer;
  Y, Mo, Dt, H, Mi, S, Ms: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  case Which of
    stMs, stDate: N := 1;
    stSeconds, stMonth: N := 2;
    stMinutes, stFullYear: N := 3;
  else N := 4;
  end;
  for I := 0 to 3 do begin Have[I] := False; A[I] := NaN; end;
  for I := 0 to N - 1 do
    if (I = 0) or (Argc > I) then
    begin
      if not NumArg(Args, Argc, I + 1, A[I]) then Exit(False);
      Have[I] := True;
    end;
  if IsNaN(T) then
  begin
    if Which <> stFullYear then begin Res := MkNumber(NaN); Exit(True); end;
    T := 0;   { setFullYear on an invalid date starts from +0 }
  end
  else if Local then T := LocalTime(T);
  Y := YearFromTime(T); Mo := MonthFromTime(T); Dt := DateFromTime(T);
  H := HourFromTime(T); Mi := MinFromTime(T); S := SecFromTime(T); Ms := MsFromTime(T);
  case Which of
    stMs: Ms := A[0];
    stSeconds: begin S := A[0]; if Have[1] then Ms := A[1]; end;
    stMinutes: begin Mi := A[0]; if Have[1] then S := A[1]; if Have[2] then Ms := A[2]; end;
    stHours: begin H := A[0]; if Have[1] then Mi := A[1]; if Have[2] then S := A[2]; if Have[3] then Ms := A[3]; end;
    stDate: Dt := A[0];
    stMonth: begin Mo := A[0]; if Have[1] then Dt := A[1]; end;
    stFullYear: begin Y := A[0]; if Have[1] then Mo := A[1]; if Have[2] then Dt := A[2]; end;
  end;
  if Which in [stDate, stMonth, stFullYear] then
    U := MakeDate(MakeDay(Y, Mo, Dt), TimeWithinDay(T))
  else
    U := MakeDate(Day(T), MakeTime(H, Mi, S, Ms));
  if Local then U := UTCOf(U);
  U := TimeClip(U);
  SetThisTime(Args, U);
  Res := MkNumber(U);
  Result := True;
end;

function D_SetMilliseconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stMs, True); end;
function D_SetSeconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stSeconds, True); end;
function D_SetMinutes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stMinutes, True); end;
function D_SetHours(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stHours, True); end;
function D_SetDate(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stDate, True); end;
function D_SetMonth(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stMonth, True); end;
function D_SetFullYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stFullYear, True); end;
function D_SetUTCMilliseconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stMs, False); end;
function D_SetUTCSeconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stSeconds, False); end;
function D_SetUTCMinutes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stMinutes, False); end;
function D_SetUTCHours(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stHours, False); end;
function D_SetUTCDate(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stDate, False); end;
function D_SetUTCMonth(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stMonth, False); end;
function D_SetUTCFullYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := SetFields(Args, Argc, Res, stFullYear, False); end;

{ B.2.3.2 setYear }
function D_SetYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T, Y, YYYY, D: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if not NumArg(Args, Argc, 1, Y) then Exit(False);
  if IsNaN(T) then T := 0 else T := LocalTime(T);
  YYYY := FullYear(Y);
  if IsNaN(YYYY) then
  begin
    SetThisTime(Args, NaN);
    Res := MkNumber(NaN);
    Exit(True);
  end;
  D := MakeDay(YYYY, MonthFromTime(T), DateFromTime(T));
  D := TimeClip(UTCOf(MakeDate(D, TimeWithinDay(T))));
  SetThisTime(Args, D);
  Res := MkNumber(D);
  Result := True;
end;

{ ---------------- string forms ---------------- }

function D_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  Res := StrFromUnicode(ToDateString(T));
  Result := True;
end;

function D_ToDateString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then Res := StrFromUnicode('Invalid Date')
  else Res := StrFromUnicode(DateStr(LocalTime(T)));
  Result := True;
end;

function D_ToTimeString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then Res := StrFromUnicode('Invalid Date')
  else Res := StrFromUnicode(TimeStr(LocalTime(T)) + TZStr(T));
  Result := True;
end;

function D_ToUTCString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then Res := StrFromUnicode('Invalid Date')
  else Res := StrFromUnicode(UTCString(T));
  Result := True;
end;

function D_ToISOString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  if not Finite(T) then Exit(ThrowError('RangeError', 'Invalid time value'));
  Res := StrFromUnicode(ISOString(T));
  Result := True;
end;

{ 21.4.4.37 toJSON: generic }
function D_ToJSON(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O, TV, F: TValue;
begin
  if IsNullish(Args[0]) then Exit(ThrowTypeError('Date.prototype.toJSON called on null or undefined'));
  if not ToObjectHook(Args[0], O) then Exit(False);
  PushRoot(O);
  if not ToPrimitiveHint(O, phNumber, TV) then begin PopRoots(1); Exit(False); end;
  if IsNumber(TV) and not Finite(NumOf(TV)) then begin PopRoots(1); Res := VNull; Exit(True); end;
  if not GetV(RootAt(TempTop - 1), AtomOf('toISOString'), F) then begin PopRoots(1); Exit(False); end;
  if not IsCallable(F) then begin PopRoots(1); Exit(ThrowTypeError('toISOString is not a function')); end;
  Result := CallValue(F, RootAt(TempTop - 1), nil, 0, Res);
  PopRoots(1);
end;

{ 21.4.4.45 Date.prototype[@@toPrimitive](hint) }
function D_ToPrimitive(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var H: UnicodeString; Hint: TPrimHint;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('Date.prototype[Symbol.toPrimitive] called on a non-object'));
  if not IsString(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Invalid hint'));
  H := StrToUnicode(StrOf(Args[1]));
  if (H = 'string') or (H = 'default') then Hint := phString
  else if H = 'number' then Hint := phNumber
  else Exit(ThrowTypeError('Invalid hint: ' + H));
  Result := OrdinaryToPrimitive(Args[0], Hint = phString, Res);
end;

{ Temporal: Date.prototype.toTemporalInstant }
function D_ToTemporalInstant(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double; Temporal, Instant, F, A: TValue;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then Exit(ThrowError('RangeError', 'Invalid time value'));
  if not GetV(GlobalObj, AtomOf('Temporal'), Temporal) or not IsObject(Temporal) then Exit(ThrowTypeError('Temporal is not available'));
  if not GetV(Temporal, AtomOf('Instant'), Instant) then Exit(False);
  PushRoot(Instant);
  if not GetV(Instant, AtomOf('fromEpochMilliseconds'), F) then begin PopRoots(1); Exit(False); end;
  A := MkNumber(T);
  Result := CallValue(F, RootAt(TempTop - 1), @A, 1, Res);
  PopRoots(1);
end;

procedure InitDate;
var C, F: TValue;   { Date.prototype is read from its rooted threadvar: a collection can move it }
  procedure N(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Len: Integer);
  begin
    DefineNative(O, Name, Fn, Len);
  end;
begin
  tzset;
  AddRoot(@DateProtoV);
  RealmVals([@DateProtoV]);
  DateProtoV := MkPtr(NewPlainObject(ObjectProto, 48));
  C := NewNativeFunction(Date_Ctor, 'Date', 7);
  PushRoot(C);
  DefineOwn(RootAt(TempTop - 1), AtomPrototype, DateProtoV, 0);
  DefineOwn(DateProtoV, AtomConstructor, RootAt(TempTop - 1), pfHidden);
  DefineOwn(GlobalObj, AtomOf('Date'), RootAt(TempTop - 1), pfHidden);
  N(RootAt(TempTop - 1), 'now', Date_Now, 0);
  N(RootAt(TempTop - 1), 'parse', Date_Parse, 1);
  N(RootAt(TempTop - 1), 'UTC', Date_UTC, 7);
  PopRoots(1);
  N(DateProtoV, 'getTime', D_GetTime, 0);
  N(DateProtoV, 'valueOf', D_GetTime, 0);
  N(DateProtoV, 'getTimezoneOffset', D_GetTimezoneOffset, 0);
  N(DateProtoV, 'getFullYear', D_GetFullYear, 0); N(DateProtoV, 'getUTCFullYear', D_GetUTCFullYear, 0);
  N(DateProtoV, 'getMonth', D_GetMonth, 0); N(DateProtoV, 'getUTCMonth', D_GetUTCMonth, 0);
  N(DateProtoV, 'getDate', D_GetDate, 0); N(DateProtoV, 'getUTCDate', D_GetUTCDate, 0);
  N(DateProtoV, 'getDay', D_GetDay, 0); N(DateProtoV, 'getUTCDay', D_GetUTCDay, 0);
  N(DateProtoV, 'getHours', D_GetHours, 0); N(DateProtoV, 'getUTCHours', D_GetUTCHours, 0);
  N(DateProtoV, 'getMinutes', D_GetMinutes, 0); N(DateProtoV, 'getUTCMinutes', D_GetUTCMinutes, 0);
  N(DateProtoV, 'getSeconds', D_GetSeconds, 0); N(DateProtoV, 'getUTCSeconds', D_GetUTCSeconds, 0);
  N(DateProtoV, 'getMilliseconds', D_GetMilliseconds, 0); N(DateProtoV, 'getUTCMilliseconds', D_GetUTCMilliseconds, 0);
  N(DateProtoV, 'getYear', D_GetYear, 0);
  N(DateProtoV, 'setTime', D_SetTime, 1);
  N(DateProtoV, 'setMilliseconds', D_SetMilliseconds, 1); N(DateProtoV, 'setUTCMilliseconds', D_SetUTCMilliseconds, 1);
  N(DateProtoV, 'setSeconds', D_SetSeconds, 2); N(DateProtoV, 'setUTCSeconds', D_SetUTCSeconds, 2);
  N(DateProtoV, 'setMinutes', D_SetMinutes, 3); N(DateProtoV, 'setUTCMinutes', D_SetUTCMinutes, 3);
  N(DateProtoV, 'setHours', D_SetHours, 4); N(DateProtoV, 'setUTCHours', D_SetUTCHours, 4);
  N(DateProtoV, 'setDate', D_SetDate, 1); N(DateProtoV, 'setUTCDate', D_SetUTCDate, 1);
  N(DateProtoV, 'setMonth', D_SetMonth, 2); N(DateProtoV, 'setUTCMonth', D_SetUTCMonth, 2);
  N(DateProtoV, 'setFullYear', D_SetFullYear, 3); N(DateProtoV, 'setUTCFullYear', D_SetUTCFullYear, 3);
  N(DateProtoV, 'setYear', D_SetYear, 1);
  N(DateProtoV, 'toString', D_ToString, 0);
  N(DateProtoV, 'toDateString', D_ToDateString, 0);
  N(DateProtoV, 'toTimeString', D_ToTimeString, 0);
  N(DateProtoV, 'toISOString', D_ToISOString, 0);
  N(DateProtoV, 'toUTCString', D_ToUTCString, 0);
  N(DateProtoV, 'toJSON', D_ToJSON, 1);
  N(DateProtoV, 'toLocaleString', D_ToString, 0);
  N(DateProtoV, 'toLocaleDateString', D_ToDateString, 0);
  N(DateProtoV, 'toLocaleTimeString', D_ToTimeString, 0);
  N(DateProtoV, 'toTemporalInstant', D_ToTemporalInstant, 0);
  { B.2.3.3: toGMTString is the toUTCString function object }
  GetV(DateProtoV, AtomOf('toUTCString'), F);
  DefineOwn(DateProtoV, AtomOf('toGMTString'), F, pfHidden);
  { 21.4.4.45: [@@toPrimitive] is configurable, not writable }
  F := NewNativeFunction(D_ToPrimitive, '[Symbol.toPrimitive]', 1);
  DefineOwn(DateProtoV, SymKey(SymToPrimitive), F, pfConfigurable);
end;

end.
