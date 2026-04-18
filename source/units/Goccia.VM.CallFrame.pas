unit Goccia.VM.CallFrame;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk;

type
  TGocciaVMCallFrame = record
    Template: TGocciaFunctionTemplate;
    IP: Integer;
    Base: Integer;
    ReturnRegister: Integer;
  end;

implementation

end.
