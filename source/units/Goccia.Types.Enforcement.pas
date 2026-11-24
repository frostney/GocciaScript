unit Goccia.Types.Enforcement;

{$I Goccia.inc}

interface

uses
  Goccia.Types.LocalType,
  Goccia.Values.Primitives;

{ Throw a TypeError when AValue does not satisfy the AExpected strict
  type.  Used by both the bytecode VM (OP_CHECK_TYPE) and typed scope
  bindings to provide a single source of truth for strict-types
  enforcement. }
procedure EnforceStrictType(const AValue: TGocciaValue;
  const AExpected: TGocciaLocalType);

{ Combined helper: parse the annotation, then enforce the resulting
  type on the value.  No-op when the annotation does not produce a
  strict type (sltUntyped). }
procedure EnforceStrictTypeAnnotation(const AValue: TGocciaValue;
  const AAnnotation: string);

implementation

uses
  Math,
  SysUtils,

  Goccia.Constants.TypeNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper;

// ES2026 Types-as-comments: runtime guard shared by the bytecode VM and
// typed scope bindings.
procedure EnforceStrictType(const AValue: TGocciaValue;
  const AExpected: TGocciaLocalType);
begin
  case AExpected of
    sltInteger:
      begin
        if not (AValue is TGocciaNumberLiteralValue) or
           AValue.ToNumberLiteral.IsNaN or
           AValue.ToNumberLiteral.IsInfinite or
           (Frac(AValue.ToNumberLiteral.Value) <> 0.0) then
          ThrowTypeError(Format(SErrorTypeNotAssignable, [AValue.TypeName, INTEGER_TYPE_NAME]),
            SSuggestTypeEnforcement);
      end;
    sltFloat:
      begin
        if not (AValue is TGocciaNumberLiteralValue) then
          ThrowTypeError(Format(SErrorTypeNotAssignable, [AValue.TypeName, NUMBER_TYPE_NAME]),
            SSuggestTypeEnforcement);
      end;
    sltBoolean:
      begin
        if not (AValue is TGocciaBooleanLiteralValue) then
          ThrowTypeError(Format(SErrorTypeNotAssignable, [AValue.TypeName, BOOLEAN_TYPE_NAME]),
            SSuggestTypeEnforcement);
      end;
    sltString:
      begin
        if not (AValue is TGocciaStringLiteralValue) then
          ThrowTypeError(Format(SErrorTypeNotAssignable, [AValue.TypeName, STRING_TYPE_NAME]),
            SSuggestTypeEnforcement);
      end;
    sltReference:
      begin
        if AValue.IsPrimitive then
          ThrowTypeError(Format(SErrorTypeNotAssignable, [AValue.TypeName, OBJECT_TYPE_NAME]),
            SSuggestTypeEnforcement);
      end;
  else
    // sltUntyped: no runtime check
  end;
end;

procedure EnforceStrictTypeAnnotation(const AValue: TGocciaValue;
  const AAnnotation: string);
var
  Expected: TGocciaLocalType;
begin
  Expected := TypeAnnotationToLocalType(AAnnotation);
  if Expected <> sltUntyped then
    EnforceStrictType(AValue, Expected);
end;

end.
