unit G2.Temporal;
{ Temporal: the spec's operations and objects are written in JS (prelude/temporal.js -> G2.Temporal.inc) and
  snapshotted; this unit gives the prelude what JS cannot reach: the clock, the time zone database and the system
  time zone. Zones come as TZif files from GOCCIA_TZDIR or /usr/share/zoneinfo, the zone and link names from that
  directory's tzdata.zi, as in Goccia (ADR 0034); the prelude parses them and caches each zone. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}

interface

uses
  G2.Core;

procedure InitTemporal;
{ the internals the Temporal prelude returns, for the Intl prelude (undefined before InitTemporal) }
function TemporalInternals: TValue;

implementation

uses
  {$IFDEF UNIX}BaseUnix, Unix, Linux, UnixType, {$ENDIF}SysUtils, Classes, BigInteger, G2.Bytecode, G2.VM, G2.BigIntV,
  G2.Snapshot, Goccia.Temporal.Utils, Goccia.Temporal.Calendar;

{$I G2.Temporal.inc}

function ZoneDir: string;
begin
  Result := GetEnvironmentVariable('GOCCIA_TZDIR');
  if Result = '' then Result := '/usr/share/zoneinfo';
  Result := IncludeTrailingPathDelimiter(Result);
end;

{ the file's bytes as a string of code units 0..255, or undefined when it cannot be read }
function FileBytes(const Path: string): TValue;
var F: TFileStream; B: TBytes; S: UnicodeString; I: Integer;
begin
  Result := VUndefined;
  if not FileExists(Path) then Exit;
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    try
      SetLength(B, F.Size);
      if Length(B) > 0 then F.ReadBuffer(B[0], Length(B));
    finally
      F.Free;
    end;
  except
    Exit;
  end;
  SetLength(S, Length(B));
  for I := 0 to High(B) do S[I + 1] := WideChar(B[I]);
  Result := StrFromUnicode(S);
end;

{ nowNs(): the current time, nanoseconds since the epoch, as a BigInt }
function Temporal_NowNs(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var TS: TTimeSpec; B: TBigInteger;
begin
  clock_gettime(CLOCK_REALTIME, @TS);
  B := TBigInteger.FromInt64(Int64(TS.tv_sec)).Multiply(TBigInteger.FromInt64(1000000000)).Add(
    TBigInteger.FromInt64(Int64(TS.tv_nsec)));
  Res := BigToValue(B);
  Result := True;
end;

{ tzData(name): the zone's TZif file; only names made of the characters of IANA zone names, so no path can
  leave the zone directory }
function Temporal_TzData(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Name: UnicodeString; I: Integer; C: WideChar;
begin
  Res := VUndefined;
  Result := True;
  if (Argc < 1) or not IsString(Args[1]) then Exit;
  Name := StrToUnicode(StrOf(Args[1]));
  if (Name = '') or (Length(Name) > 256) or (Name[1] = '/') or (Pos('..', Name) > 0) then Exit;
  for I := 1 to Length(Name) do
  begin
    C := Name[I];
    if not (CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '/', '_', '-', '+', '.'])) then Exit;
  end;
  Res := FileBytes(ZoneDir + string(Name));
end;

{ tzIndex(): the zone directory's tzdata.zi (its Z and L lines name every zone and link) }
function Temporal_TzIndex(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := FileBytes(ZoneDir + 'tzdata.zi');
  Result := True;
end;

{ systemTZ(): TZ, else the zone /etc/localtime links to, else undefined }
function Temporal_SystemTZ(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: string; P: Integer;
begin
  Res := VUndefined;
  Result := True;
  S := GetEnvironmentVariable('TZ');
  if (S <> '') and (S[1] = ':') then Delete(S, 1, 1);
  if S = '' then
  begin
    {$IFDEF UNIX}
    S := fpReadLink('/etc/localtime');
    {$ENDIF}
    P := Pos('zoneinfo/', S);
    if P > 0 then S := Copy(S, P + Length('zoneinfo/'), MaxInt) else S := '';
  end;
  if S <> '' then Res := StrFromUnicode(UnicodeString(S));
end;

{ calOp(op, calendar, ...): the non-ISO calendars' arithmetic (Goccia.Temporal.Calendar, over ICU), all in ISO
  and calendar year/month/day integers; null where the calendar cannot represent the date (reject) or ICU fails
    0 ISOToDate(cal, y, m, d)  -> [year, month, monthCode, day, era, eraYear, dayOfWeek, dayOfYear, daysInMonth,
                                    daysInYear, monthsInYear, inLeapYear] (era and eraYear undefined without eras)
    1 DateToISO(cal, year, monthOrCode, isCode, isLeap, day, constrain) -> [y, m, d]
    2 Add(cal, y, m, d, years, months, weeks, days, constrain)          -> [y, m, d]
    3 MonthDayRef(cal, refYear, monthOrCode, isCode, isLeap, day)        -> [y, m, d]
    4 YearFromEra(cal, era, eraYear)                                     -> year
    5 MonthsInYear(cal, year)                                            -> count
    6 UsesEra(cal)                                                        -> boolean
    7 EraFromYear(cal, year)                                              -> [era, eraYear] }
function Temporal_CalOp(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Op: Integer; Cal: string; D: TTemporalDateRecord; Info: TTemporalCalendarDateInfo; A: PObj; N, I: Integer;
  Era: string; EraYear: Integer; HasEra: Boolean;
  function IArg(K: Integer): Int64;
  begin
    if (K <= Argc) and IsNumber(Args[K]) then Result := Trunc(NumOf(Args[K])) else Result := 0;
  end;
  function BArg(K: Integer): Boolean;
  begin
    Result := (K <= Argc) and ToBool(Args[K]);
  end;
  function SArg(K: Integer): string;
  begin
    if (K <= Argc) and IsString(Args[K]) then Result := string(StrToUnicode(StrOf(Args[K]))) else Result := '';
  end;
  procedure Triple(const R: TTemporalDateRecord);
  begin
    A := NewArray(3, 3);
    A^.Elements^.Data[0] := MkInt(R.Year);
    A^.Elements^.Data[1] := MkInt(R.Month);
    A^.Elements^.Data[2] := MkInt(R.Day);
    Res := MkPtr(A);
  end;
begin
  Result := True;
  Res := VNull;
  Op := IArg(1);
  Cal := SArg(2);
  case Op of
    0:
      if TryGetCalendarDateInfo(Cal, IArg(3), IArg(4), IArg(5), Info) then
      begin
        PushRoot(MkPtr(NewArray(12, 12)));
        A := ObjOf(RootAt(TempTop - 1));
        for I := 0 to 11 do A^.Elements^.Data[I] := VUndefined;
        A^.Elements^.Data[0] := MkInt(Info.Date.Year);
        A^.Elements^.Data[1] := MkInt(Info.Date.Month);
        A^.Elements^.Data[3] := MkInt(Info.Date.Day);
        A^.Elements^.Data[5] := MkInt(Info.EraYear);
        A^.Elements^.Data[6] := MkInt(Info.DayOfWeek);
        A^.Elements^.Data[7] := MkInt(Info.DayOfYear);
        A^.Elements^.Data[8] := MkInt(Info.DaysInMonth);
        A^.Elements^.Data[9] := MkInt(Info.DaysInYear);
        A^.Elements^.Data[10] := MkInt(Info.MonthsInYear);
        A^.Elements^.Data[11] := MkBool(Info.InLeapYear);
        if not Info.HasEra then A^.Elements^.Data[5] := VUndefined;
        { strings last: each allocation may move the array }
        ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, 2, StrFromUnicode(UnicodeString(Info.MonthCode)));
        if Info.HasEra then
          ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, 4, StrFromUnicode(UnicodeString(Info.Era)));
        Res := RootAt(TempTop - 1);
        PopRoots(1);
      end;
    1:
      if TryResolveCalendarDateToISO(Cal, IArg(3), IArg(4), IArg(7), BArg(5), BArg(6), BArg(8), D) then Triple(D);
    2:
      if TryAddCalendarDate(Cal, IArg(3), IArg(4), IArg(5), IArg(6), IArg(7), IArg(8), IArg(9), BArg(10), D) then
        Triple(D);
    3:
      if TryCalendarMonthDayInISOYear(Cal, IArg(3), IArg(4), IArg(7), BArg(5), BArg(6), D) then Triple(D);
    4:
      if TryCalendarYearFromEra(Cal, SArg(3), IArg(4), N) then Res := MkInt(N);
    5:
      if TryCalendarMonthsInYear(Cal, IArg(3), N) then Res := MkInt(N);
    6:
      Res := MkBool(TemporalCalendarUsesEra(Cal));
    7:
      if TryCalendarEraFromYear(Cal, IArg(3), Era, EraYear, HasEra) and HasEra then
      begin
        PushRoot(MkPtr(NewArray(2, 2)));
        ObjOf(RootAt(TempTop - 1))^.Elements^.Data[1] := MkInt(EraYear);
        ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, 0, StrFromUnicode(UnicodeString(Era)));
        Res := RootAt(TempTop - 1);
        PopRoots(1);
      end;
  end;
end;

threadvar
  TemporalInternalsV: TValue;
  TemporalRooted: Boolean;

function TemporalInternals: TValue;
begin
  if TemporalRooted then Result := TemporalInternalsV else Result := VUndefined;
end;

procedure InitTemporal;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue; CA: array[0..6] of TValue; I: Integer; Ok: Boolean;
begin
  if not TemporalRooted then begin AddRoot(@TemporalInternalsV); TemporalRooted := True; end;
  RealmVals([@TemporalInternalsV]);
  TemporalInternalsV := VUndefined;
  if not CompilePrelude('<temporal>', TemporalSrc, T, Err) then raise Exception.Create('temporal prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('temporal prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('temporal prelude run');
  PushRoot(Fn);
  { each native stays rooted while the next one is allocated }
  PushRoot(NewNativeFunction(Temporal_NowNs, 'nowNs', 0));
  PushRoot(NewNativeFunction(Temporal_TzData, 'tzData', 1));
  PushRoot(NewNativeFunction(Temporal_TzIndex, 'tzIndex', 0));
  PushRoot(NewNativeFunction(Temporal_SystemTZ, 'systemTZ', 0));
  PushRoot(NewNativeFunction(MarkIntrinsicClass, 'markIntrinsic', 1));
  PushRoot(NewNativeFunction(Temporal_CalOp, 'calOp', 8));
  CA[0] := GlobalObj;
  for I := 1 to 6 do CA[I] := RootAt(TempTop - 7 + I);
  Ok := CallValue(RootAt(TempTop - 7), VUndefined, @CA[0], 7, R);
  PopRoots(6);
  if Ok then TemporalInternalsV := R;
  if not Ok then
  begin
    PopRoots(1);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('temporal prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('temporal prelude call');
  end;
  PopRoots(1);
end;

end.
