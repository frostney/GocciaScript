unit Goccia.VM.Upvalue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaBytecodeCell = class
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue write FValue;
  end;

  TGocciaBytecodeUpvalue = class
  private
    FCell: TGocciaBytecodeCell;
  public
    constructor Create(const ACell: TGocciaBytecodeCell);
    property Cell: TGocciaBytecodeCell read FCell;
  end;

implementation

constructor TGocciaBytecodeCell.Create(const AValue: TGocciaValue);
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
