unit Goccia.FloatingPoint;

{$I Goccia.inc}

interface

uses
  Math;

type
  {$IFDEF FPC}
  TGocciaExceptionMask = TFPUExceptionMask;
  TGocciaRoundingMode = TFPURoundingMode;
  {$ELSE}
  TGocciaExceptionMask = TArithmeticExceptionMask;
  TGocciaRoundingMode = TRoundingMode;
  {$ENDIF}

  TGocciaFloatingPointState = record
    ExceptionMask: TGocciaExceptionMask;
    RoundingMode: TGocciaRoundingMode;
    PrecisionMode: TFPUPrecisionMode;
  end;

procedure EnterGocciaFloatingPointScope(
  out AState: TGocciaFloatingPointState);
procedure LeaveGocciaFloatingPointScope(
  const AState: TGocciaFloatingPointState);

implementation

procedure EnterGocciaFloatingPointScope(
  out AState: TGocciaFloatingPointState);
begin
  AState.ExceptionMask := GetExceptionMask;
  AState.RoundingMode := GetRoundMode;
  AState.PrecisionMode := GetPrecisionMode;

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
  SetRoundMode(rmNearest);
  SetPrecisionMode(pmDouble);
end;

procedure LeaveGocciaFloatingPointScope(
  const AState: TGocciaFloatingPointState);
begin
  SetPrecisionMode(AState.PrecisionMode);
  SetRoundMode(AState.RoundingMode);
  SetExceptionMask(AState.ExceptionMask);
end;

end.
