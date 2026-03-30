unit Goccia.VM.CallFrame;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk,
  Goccia.VM.Registers;

type
  TGocciaVMCallFrame = record
    Template: TGocciaFunctionTemplate;
    IP: Integer;
    Base: Integer;
    ReturnRegister: Integer;
  end;

implementation

end.
