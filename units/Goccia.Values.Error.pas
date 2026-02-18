unit Goccia.Values.Error;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.Primitives;

type
  // These are flow-control exceptions (return/throw), distinct from the compilation
  // and runtime errors in Goccia.Error. Kept separate intentionally.
  TGocciaReturnValue = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

  TGocciaThrowValue = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

  TGocciaBreakSignal = class(Exception)
  public
    constructor Create;
  end;

implementation

{ TGocciaReturnValue }

constructor TGocciaReturnValue.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

{ TGocciaThrowValue }

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

{ TGocciaBreakSignal }

constructor TGocciaBreakSignal.Create;
begin
  inherited Create('break');
end;

end.
