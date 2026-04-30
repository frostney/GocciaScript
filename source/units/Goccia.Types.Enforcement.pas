unit Goccia.Types.Enforcement;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Bytecode.Chunk,
  Goccia.Values.Primitives;

{ Convert a TypeScript-style type annotation string into the strict
  TGocciaLocalType enum used by the compiler and runtime guards.
  Returns sltUntyped when the annotation is empty, references a
  union type, or names an unsupported type. }
function TypeAnnotationToLocalType(const AAnnotation: string): TGocciaLocalType;

{ Infer a strict type from an initializer AST expression.  Returns
  sltUntyped for everything that is not a self-evident literal /
  reference constructor.  Matches the heuristics used by the
  bytecode compiler so the interpreter and bytecode VM agree on
  which untyped declarations have an inferred type. }
function InferLocalType(const AExpr: TGocciaExpression): TGocciaLocalType;

{ Throw a TypeError when AValue does not satisfy the AExpected strict
  type.  Used by both the bytecode VM (OP_CHECK_TYPE) and the
  interpreter to provide a single source of truth for strict-types
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

function TypeAnnotationToLocalType(const AAnnotation: string): TGocciaLocalType;
var
  Ann: string;
begin
  Result := sltUntyped;
  Ann := Trim(AAnnotation);
  if Ann = '' then
    Exit;
  if Pos('|', Ann) > 0 then
    Exit;
  if Ann = NUMBER_TYPE_NAME then
    Result := sltFloat
  else if Ann = STRING_TYPE_NAME then
    Result := sltString
  else if Ann = BOOLEAN_TYPE_NAME then
    Result := sltBoolean
  else if (Ann = OBJECT_TYPE_NAME) or (Ann = 'Object')
       or (Ann = 'Function')
       or (Pos('<', Ann) > 0)
       or (Pos('[', Ann) > 0)
       or (Pos('{', Ann) > 0)
       or (Pos('=>', Ann) > 0) then
    Result := sltReference;
end;

function InferLocalType(const AExpr: TGocciaExpression): TGocciaLocalType;
var
  Lit: TGocciaLiteralExpression;
begin
  Result := sltUntyped;
  if AExpr is TGocciaLiteralExpression then
  begin
    Lit := TGocciaLiteralExpression(AExpr);
    if Lit.Value is TGocciaNumberLiteralValue then
      Result := sltFloat
    else if Lit.Value is TGocciaBooleanLiteralValue then
      Result := sltBoolean
    else if Lit.Value is TGocciaStringLiteralValue then
      Result := sltString;
  end
  else if AExpr is TGocciaTemplateLiteralExpression then
    Result := sltString
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
    Result := sltString
  else if AExpr is TGocciaObjectExpression then
    Result := sltReference
  else if AExpr is TGocciaArrayExpression then
    Result := sltReference
  else if AExpr is TGocciaNewExpression then
    Result := sltReference
  else if AExpr is TGocciaArrowFunctionExpression then
    Result := sltReference
  else if AExpr is TGocciaMethodExpression then
    Result := sltReference;
end;

// ES2026 Types-as-comments: runtime guard shared by interpreter and bytecode VM.
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
