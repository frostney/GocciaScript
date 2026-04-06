unit Goccia.Values.WrapperPrimitives;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function UnboxWrappedPrimitive(const AValue: TGocciaValue): TGocciaValue;

implementation

uses
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.StringObjectValue;

function UnboxWrappedPrimitive(const AValue: TGocciaValue): TGocciaValue;
begin
  if AValue is TGocciaNumberObjectValue then
    Result := TGocciaNumberObjectValue(AValue).Primitive
  else if AValue is TGocciaStringObjectValue then
    Result := TGocciaStringObjectValue(AValue).Primitive
  else if AValue is TGocciaBooleanObjectValue then
    Result := TGocciaBooleanObjectValue(AValue).Primitive
  else
    Result := AValue;
end;

end.
