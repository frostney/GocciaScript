unit Goccia.TestSetup;

{$I Goccia.inc}

interface

implementation

uses
  Math;

initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

end.
