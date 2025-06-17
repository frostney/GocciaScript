unit Goccia.Values.UndefinedValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Math;

type
  TGocciaUndefinedValue = class(TGocciaValue)
  public
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
  end;

implementation

function TGocciaUndefinedValue.ToString: string;
begin
  Result := 'undefined';
end;

function TGocciaUndefinedValue.ToBoolean: Boolean;
begin
  Result := False;
end;

function TGocciaUndefinedValue.ToNumber: Double;
begin
  Result := NaN;
end;

function TGocciaUndefinedValue.TypeName: string;
begin
  Result := 'undefined';
end;

end.
