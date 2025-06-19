unit Goccia.Utils;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.UndefinedValue, Goccia.Values.NullValue, Goccia.Values.BooleanValue,
  Goccia.Values.NumberValue, Goccia.Values.StringValue, Goccia.Values.ObjectValue;

// TODO: Do we want to implement this on the class level as class operators or functions?
function IsEqual(Left, Right: TGocciaValue): Boolean;

function ValueOrUndefined(Value: TGocciaValue): TGocciaValue;

implementation

// TODO: Might be worth to rename to StrictEquals and integrate into Goccia.Evaluator
function IsEqual(Left, Right: TGocciaValue): Boolean;
begin
  if (Left is TGocciaUndefinedValue) and (Right is TGocciaUndefinedValue) then
    Result := True
  else if (Left is TGocciaNullValue) and (Right is TGocciaNullValue) then
    Result := True
  else if (Left is TGocciaBooleanValue) and (Right is TGocciaBooleanValue) then
    Result := TGocciaBooleanValue(Left).ToBoolean = TGocciaBooleanValue(Right).ToBoolean
  else if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
    Result := TGocciaNumberValue(Left).ToNumber = TGocciaNumberValue(Right).ToNumber
  else if (Left is TGocciaStringValue) and (Right is TGocciaStringValue) then
    Result := TGocciaStringValue(Left).ToString = TGocciaStringValue(Right).ToString
  else
    Result := Left = Right; // Reference equality for objects
end;

function ValueOrUndefined(Value: TGocciaValue): TGocciaValue;
begin
  if Value = nil then
    Result := TGocciaUndefinedValue.Create
  else
    Result := Value;
end;

end.