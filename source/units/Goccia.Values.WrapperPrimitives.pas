unit Goccia.Values.WrapperPrimitives;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function UnboxWrappedPrimitive(const AValue: TGocciaValue): TGocciaValue;

// ES2026 §25.5.4.2 SerializeJSONProperty steps 4.b-4.d: Number wrappers
// coerce via ToNumber and String wrappers via ToString — both route through
// ToPrimitive, so user-defined valueOf/toString are honored and may throw —
// while Boolean wrappers read [[BooleanData]] directly.
function CoerceWrappedPrimitive(const AValue: TGocciaValue): TGocciaValue;

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

function CoerceWrappedPrimitive(const AValue: TGocciaValue): TGocciaValue;
begin
  if AValue is TGocciaNumberObjectValue then
    Result := AValue.ToNumberLiteral
  else if AValue is TGocciaStringObjectValue then
    Result := AValue.ToStringLiteral
  else
    Result := UnboxWrappedPrimitive(AValue);
end;

end.
