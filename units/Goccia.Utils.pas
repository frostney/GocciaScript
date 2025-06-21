unit Goccia.Utils;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.UndefinedValue, Goccia.Values.NullValue, Goccia.Values.BooleanValue,
  Goccia.Values.NumberValue, Goccia.Values.StringValue, Goccia.Values.ObjectValue;

function ValueOrUndefined(Value: TGocciaValue): TGocciaValue;

implementation

function ValueOrUndefined(Value: TGocciaValue): TGocciaValue;
begin
  if Value = nil then
    Result := TGocciaUndefinedValue.Create
  else
    Result := Value;
end;

end.