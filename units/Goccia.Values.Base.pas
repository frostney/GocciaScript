unit Goccia.Values.Base;

{$I Goccia.inc}

interface

uses
  Classes;

type
  // TODO: Do we want to refactor this as an interface?
  TGocciaValue = class
  public
    function ToString: string; virtual; abstract;
    function ToBoolean: Boolean; virtual;
    function ToNumber: Double; virtual; abstract;
    function TypeName: string; virtual; abstract;
  end;

implementation

function TGocciaValue.ToBoolean: Boolean;
begin
  Result := True;
end;

end.
