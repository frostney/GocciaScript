unit Goccia.Timeout;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaTimeoutError = class(Exception);

procedure StartExecutionTimeout(const ATimeoutMilliseconds: Integer);
procedure ClearExecutionTimeout;
procedure CheckExecutionTimeout;

implementation

uses
  TimingUtils;

const
  TIMEOUT_CHECK_INTERVAL = 1024;
  TIMEOUT_ALWAYS_CHECK_THRESHOLD_MS = 16;

threadvar
  GTimeoutMilliseconds: Integer;
  GStartNanoseconds: Int64;
  GCheckCounter: Integer;

procedure StartExecutionTimeout(const ATimeoutMilliseconds: Integer);
begin
  GTimeoutMilliseconds := ATimeoutMilliseconds;
  GStartNanoseconds := GetNanoseconds;
  GCheckCounter := 0;
end;

procedure ClearExecutionTimeout;
begin
  GTimeoutMilliseconds := 0;
  GStartNanoseconds := 0;
  GCheckCounter := 0;
end;

procedure CheckExecutionTimeout;
begin
  if GTimeoutMilliseconds <= 0 then
    Exit;

  Inc(GCheckCounter);
  if (GTimeoutMilliseconds > TIMEOUT_ALWAYS_CHECK_THRESHOLD_MS) and
     ((GCheckCounter and (TIMEOUT_CHECK_INTERVAL - 1)) <> 0) then
    Exit;

  if ((GetNanoseconds - GStartNanoseconds) div 1000000) >= GTimeoutMilliseconds then
    raise TGocciaTimeoutError.CreateFmt('Execution timed out after %dms',
      [GTimeoutMilliseconds]);
end;

end.
