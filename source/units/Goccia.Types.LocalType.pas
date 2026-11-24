unit Goccia.Types.LocalType;

{$I Goccia.inc}

// The strict types of types-as-comments (--strict-types) as the compilers see
// them: what an annotation or an initializer says a binding holds. Engine-free;
// Goccia.Types.Enforcement checks values against them at run time.

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node;

type
  TGocciaLocalType = (
    sltUntyped,
    sltInteger,
    sltFloat,
    sltBoolean,
    sltString,
    sltReference
  );

{ Convert a TypeScript-style type annotation string into the strict
  TGocciaLocalType enum used by the compiler and runtime guards.
  Returns sltUntyped when the annotation is empty, references a
  union type, or names an unsupported type. }
function TypeAnnotationToLocalType(const AAnnotation: string): TGocciaLocalType;

{ Infer a strict type from an initializer AST expression.  Returns
  sltUntyped for everything that is not a self-evident literal /
  reference constructor.  The bytecode compiler uses it to decide
  which untyped declarations have an inferred type. }
function InferLocalType(const AExpr: TGocciaExpression): TGocciaLocalType;

implementation

uses
  SysUtils,

  Goccia.Constants.TypeNames;

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
    case Lit.Kind of
      lkNumber:
        Result := sltFloat;
      lkBoolean:
        Result := sltBoolean;
      lkString:
        Result := sltString;
    else
      ;
    end;
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
  else if AExpr is TGocciaFunctionExpression then
    Result := sltReference
  else if AExpr is TGocciaObjectMethodDefinition then
    Result := sltReference;
end;

end.
