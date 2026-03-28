unit Goccia.VM.CallFrame;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk,
  Goccia.Values.Primitives;

type
  TGocciaRegister = TGocciaValue;
  TGocciaRegisterArray = array of TGocciaRegister;

  TGocciaVMCallFrame = record
    Template: TGocciaFunctionTemplate;
    IP: Integer;
    Base: Integer;
    ReturnRegister: Integer;
  end;

implementation

end.
