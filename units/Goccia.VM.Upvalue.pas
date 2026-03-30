unit Goccia.VM.Upvalue;

{$I Goccia.inc}

interface

uses
  Goccia.VM.Registers;

type
  TGocciaBytecodeCell = class
  private
    FValue: TGocciaRegister;
  public
    constructor Create(const AValue: TGocciaRegister);
    property Value: TGocciaRegister read FValue write FValue;
  end;

  TGocciaBytecodeUpvalue = class
  private
    FCell: TGocciaBytecodeCell;
  public
    constructor Create(const ACell: TGocciaBytecodeCell);
    property Cell: TGocciaBytecodeCell read FCell;
  end;

implementation

constructor TGocciaBytecodeCell.Create(const AValue: TGocciaRegister);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TGocciaBytecodeUpvalue.Create(const ACell: TGocciaBytecodeCell);
begin
  inherited Create;
  FCell := ACell;
end;

end.
