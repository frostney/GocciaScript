unit Goccia.Values.Error;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, SysUtils;

type
  // TODO: Is it worth to merge this with Goccia.Error?
  TGocciaReturnValue = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

  TGocciaThrowValue = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

implementation

{ TGocciaReturnValue }

constructor TGocciaReturnValue.Create(AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

{ TGocciaThrowValue }

constructor TGocciaThrowValue.Create(AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

end.