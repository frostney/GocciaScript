unit TimingUtils;

{$I Goccia.inc}

interface

function GetMicroseconds: Int64;
function FormatDuration(Microseconds: Int64): string;

implementation

uses
  SysUtils
  {$IFDEF UNIX}, Unix{$ENDIF}
  {$IFDEF WINDOWS}, Windows{$ENDIF};

{$IFDEF WINDOWS}
var
  QPCFrequency: Int64;
{$ENDIF}

function GetMicroseconds: Int64;
{$IFDEF UNIX}
var
  tv: TTimeVal;
begin
  fpGetTimeOfDay(@tv, nil);
  Result := Int64(tv.tv_sec) * 1000000 + Int64(tv.tv_usec);
end;
{$ELSE}
{$IFDEF WINDOWS}
var
  Counter, Whole, Remainder: Int64;
begin
  QueryPerformanceCounter(Counter);
  Whole := Counter div QPCFrequency;
  Remainder := Counter mod QPCFrequency;
  Result := Whole * 1000000 + (Remainder * 1000000) div QPCFrequency;
end;
{$ELSE}
begin
  Result := GetTickCount64 * 1000;
end;
{$ENDIF}
{$ENDIF}

function FormatDuration(Microseconds: Int64): string;
var
  Ms: Double;
  S: Double;
begin
  Ms := Microseconds / 1000.0;
  S := Ms / 1000.0;
  if S >= 10.0 then
    Result := SysUtils.Format('%.2fs', [S])
  else if Ms >= 0.5 then
    Result := SysUtils.Format('%.2fms', [Ms])
  else
    Result := SysUtils.Format('%d' + #$C2#$B5 + 's', [Microseconds]);
end;

{$IFDEF WINDOWS}
initialization
  QueryPerformanceFrequency(QPCFrequency);
{$ENDIF}

end.
