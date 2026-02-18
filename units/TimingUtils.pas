unit TimingUtils;

{$I Goccia.inc}

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
  {$IFDEF WINDOWS},
  Windows{$ENDIF};

{$IFDEF UNIX}
const
  CLOCK_REALTIME_ID = 0;
  {$IFDEF DARWIN}
  CLOCK_MONOTONIC_ID = 6;
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

{$IFDEF WINDOWS}
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
{$IFDEF WINDOWS}
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
{$IFDEF WINDOWS}
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

function FormatDuration(const ANanoseconds: Int64): string;
var
  Us: Double;
  Ms: Double;
  S: Double;
begin
  Us := ANanoseconds / 1000.0;
  Ms := Us / 1000.0;
  S := Ms / 1000.0;
  if S >= 10.0 then
    Result := SysUtils.Format('%.2fs', [S])
  else if Ms >= 0.5 then
    Result := SysUtils.Format('%.2fms', [Ms])
  else if Us >= 0.5 then
    Result := SysUtils.Format('%.2f' + #$C2#$B5 + 's', [Us])
  else
    Result := SysUtils.Format('%dns', [ANanoseconds]);
end;

{$IFDEF WINDOWS}
initialization
  QueryPerformanceFrequency(QPCFrequency);
{$ENDIF}

end.
