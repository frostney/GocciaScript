unit Goccia.Values.NullValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Math;

type
  TGocciaNullValue = class(TGocciaValue)
  public
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
  end;

implementation

function TGocciaNullValue.ToString: string;
begin
  Result := 'null';
end;

function TGocciaNullValue.ToBoolean: Boolean;
begin
  Result := False;
end;

function TGocciaNullValue.ToNumber: Double;
begin
  Result := 0;
end;

function TGocciaNullValue.TypeName: string;
begin
  Result := 'object';
end;

end.