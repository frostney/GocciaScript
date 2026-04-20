unit Goccia.VM.CallFrame;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk,
  Goccia.VM.Closure;

type
  TGocciaVMCallFrame = record
    Template: TGocciaFunctionTemplate;
    IP: Integer;
    ReturnRegister: Integer;
    RegisterBase: Integer;
    RegisterCount: Integer;
    LocalCellBase: Integer;
    LocalCellCount: Integer;
    ArgCount: Integer;
    Closure: TGocciaBytecodeClosure;
    HandlerCount: Integer;
    PrevCovLine: UInt32;
    ProfileEntryTimestamp: Int64;
  end;

implementation

end.
