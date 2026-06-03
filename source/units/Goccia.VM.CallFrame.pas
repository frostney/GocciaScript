unit Goccia.VM.CallFrame;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk,
  Goccia.VM.Closure,
  Goccia.VM.Registers;

type
  TGocciaVMCallFrame = record
    Template: TGocciaFunctionTemplate;
    IP: Integer;
    ReturnRegister: Integer;
    RegisterBase: Integer;
    RegisterCount: Integer;
    LocalCellBase: Integer;
    LocalCellCount: Integer;
    Arguments: TGocciaRegisterArray;
    ArgCount: Integer;
    Closure: TGocciaBytecodeClosure;
    HandlerCount: Integer;
    PrevCovLine: UInt32;
    ProfileEntryTimestamp: Int64;
    NewTarget: Pointer;
    ExecutionContextPushed: Boolean;
  end;

implementation

end.
