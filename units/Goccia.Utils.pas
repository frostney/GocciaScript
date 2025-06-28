unit Goccia.Utils;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function ValueOrUndefined(Value: TGocciaValue): TGocciaValue;

implementation

function ValueOrUndefined(Value: TGocciaValue): TGocciaValue;
begin
  if Value = nil then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := Value;
end;

end.
