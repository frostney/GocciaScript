unit Goccia.Types.Enforcement;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Bytecode.Chunk,
  Goccia.Values.Primitives;

{ Convert a TypeScript-style type annotation string into the strict
  TGocciaLocalType enum used by the compiler and runtime guards, read from
  the type's outermost form. Returns sltUntyped when the annotation is
  empty or its type can hold a value outside every strict type: a union,
  an indexed access, a conditional or mapped type, `keyof`, an object type
  literal, a literal type, or a name it does not know. }
function TypeAnnotationToLocalType(const AAnnotation: string): TGocciaLocalType;

{ Infer a strict type from an initializer AST expression.  Returns
  sltUntyped for everything that is not a self-evident literal /
  reference constructor.  The bytecode compiler uses it to decide
  which untyped declarations have an inferred type. }
function InferLocalType(const AExpr: TGocciaExpression): TGocciaLocalType;

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

type
  TTypeAnnotationTokens = array of string;

const
  { Generic types whose every value is an object: a primitive has none of the
    members their interfaces require, so `tsc` admits no primitive for them.
    A homomorphic mapped type such as Partial, Readonly or Record over a
    primitive is that primitive, and an object type literal admits any
    primitive with the members it names, so neither is listed. }
  REFERENCE_GENERIC_NAMES: array[0..9] of string = (
    'Array', 'ReadonlyArray', 'Map', 'ReadonlyMap', 'Set', 'ReadonlySet',
    'WeakMap', 'WeakSet', 'WeakRef', 'Promise');

{ Splits a collected annotation into its tokens. The parser joins token
  lexemes with spaces and keeps string and template literal types as their
  source text, so a literal is one token however many spaces, brackets or
  keywords its text holds. Punctuation splits even without spaces, so a
  compact spelling such as `number[]` reads the same. }
function TokenizeTypeAnnotation(const AAnnotation: string): TTypeAnnotationTokens;
var
  Count, I, Len, Start, TemplateDepth: Integer;
  Quote: Char;

  procedure Add(const AToken: string);
  begin
    if Count = Length(Result) then
      SetLength(Result, Count * 2 + 8);
    Result[Count] := AToken;
    Inc(Count);
  end;

begin
  Result := nil;
  Count := 0;
  Len := Length(AAnnotation);
  I := 1;
  while I <= Len do
  begin
    Start := I;
    case AAnnotation[I] of
      ' ', #9, #10, #13:
        begin
          Inc(I);
          Continue;
        end;
      '''', '"':
        begin
          Quote := AAnnotation[I];
          Inc(I);
          while (I <= Len) and (AAnnotation[I] <> Quote) do
          begin
            if AAnnotation[I] = '\' then
              Inc(I);
            Inc(I);
          end;
          Inc(I);
        end;
      '`':
        begin
          { A substitution holds a type, which may hold another template;
            the literal ends at the backtick outside every substitution. }
          TemplateDepth := 0;
          Inc(I);
          while I <= Len do
          begin
            if AAnnotation[I] = '\' then
              Inc(I)
            else if (AAnnotation[I] = '$') and (I < Len) and
                    (AAnnotation[I + 1] = '{') then
            begin
              Inc(TemplateDepth);
              Inc(I);
            end
            else if (AAnnotation[I] = '}') and (TemplateDepth > 0) then
              Dec(TemplateDepth)
            else if (AAnnotation[I] = '`') and (TemplateDepth = 0) then
              Break;
            Inc(I);
          end;
          Inc(I);
        end;
      '=':
        if (I < Len) and (AAnnotation[I + 1] = '>') then
          Inc(I, 2)
        else
          Inc(I);
      'A'..'Z', 'a'..'z', '0'..'9', '_', '$', '.', '-', #128..#255:
        while (I <= Len) and
              (AAnnotation[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$',
                '.', '-', #128..#255]) do
          Inc(I);
    else
      Inc(I);
    end;
    if I > Len + 1 then
      I := Len + 1;
    Add(Copy(AAnnotation, Start, I - Start));
  end;
  SetLength(Result, Count);
end;

function IsOpeningTypeToken(const AToken: string): Boolean;
begin
  Result := (AToken = '(') or (AToken = '[') or (AToken = '{') or
    (AToken = '<');
end;

function IsClosingTypeToken(const AToken: string): Boolean;
begin
  Result := (AToken = ')') or (AToken = ']') or (AToken = '}') or
    (AToken = '>');
end;

{ The index of the token that closes the bracket opened at AOpen, or -1. }
function MatchingTypeToken(const ATokens: TTypeAnnotationTokens;
  const AOpen, ALast: Integer): Integer;
var
  Depth, I: Integer;
begin
  Depth := 0;
  for I := AOpen to ALast do
  begin
    if IsOpeningTypeToken(ATokens[I]) then
      Inc(Depth)
    else if IsClosingTypeToken(ATokens[I]) then
    begin
      Dec(Depth);
      if Depth = 0 then
        Exit(I);
    end;
  end;
  Result := -1;
end;

{ Whether a token outside every bracket of ATokens[AFirst..ALast] is one of
  ATargets. }
function HasTopLevelTypeToken(const ATokens: TTypeAnnotationTokens;
  const AFirst, ALast: Integer; const ATargets: array of string): Boolean;
var
  Depth, I, J: Integer;
begin
  Depth := 0;
  for I := AFirst to ALast do
  begin
    if IsOpeningTypeToken(ATokens[I]) then
      Inc(Depth)
    else if IsClosingTypeToken(ATokens[I]) then
      Dec(Depth)
    else if Depth = 0 then
      for J := 0 to High(ATargets) do
        if ATokens[I] = ATargets[J] then
          Exit(True);
  end;
  Result := False;
end;

function IsReferenceGenericName(const AName: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(REFERENCE_GENERIC_NAMES) do
    if AName = REFERENCE_GENERIC_NAMES[I] then
      Exit(True);
  Result := False;
end;

{ The strict type every value of the type spelled by ATokens[AFirst..ALast]
  has, read from the type's outermost form. Only a form whose every value
  passes the check gets one: the check runs where `tsc` would accept the
  program, so a form that can hold a value outside the strict type stays
  unenforced (sltUntyped). }
function ClassifyTypeTokens(const ATokens: TTypeAnnotationTokens;
  AFirst: Integer; const ALast: Integer): TGocciaLocalType;
var
  Close: Integer;
begin
  Result := sltUntyped;
  if AFirst > ALast then
    Exit;
  // A union or intersection may start with its operator.
  if (ATokens[AFirst] = '|') or (ATokens[AFirst] = '&') then
    Inc(AFirst);
  if AFirst > ALast then
    Exit;

  { A union may hold values of several kinds, and an intersection or a
    conditional type is read from its members, which this does not do. }
  if HasTopLevelTypeToken(ATokens, AFirst, ALast,
       ['|', '&', '?', 'extends']) then
    Exit;

  // `(a: T) => R`, `<T>(a: T) => R`, `new (a: T) => R`, `abstract new ...`
  if HasTopLevelTypeToken(ATokens, AFirst, ALast, ['=>']) then
  begin
    if (ATokens[AFirst] = '(') or (ATokens[AFirst] = '<') or
       (ATokens[AFirst] = 'new') or (ATokens[AFirst] = 'abstract') then
      Result := sltReference;
    Exit;
  end;

  // `readonly T[]` and `readonly [A, B]` are arrays.
  if ATokens[AFirst] = 'readonly' then
  begin
    if ClassifyTypeTokens(ATokens, AFirst + 1, ALast) = sltReference then
      Result := sltReference;
    Exit;
  end;

  if ATokens[ALast] = ']' then
  begin
    // `[A, B]`: a tuple is an array.
    if (ATokens[AFirst] = '[') and
       (MatchingTypeToken(ATokens, AFirst, ALast) = ALast) then
      Exit(sltReference);
    { `T[]` is an array; `T[K]` is an indexed access type, which is whatever
      type the property K has. }
    if (ALast - 1 > AFirst) and (ATokens[ALast - 1] = '[') then
      Result := sltReference;
    Exit;
  end;

  if ATokens[AFirst] = '(' then
  begin
    Close := MatchingTypeToken(ATokens, AFirst, ALast);
    if Close = ALast then
      Result := ClassifyTypeTokens(ATokens, AFirst + 1, ALast - 1);
    Exit;
  end;

  // `Array<T>`, `Map<K, V>`, `Promise<T>`
  if (AFirst < ALast) and (ATokens[AFirst + 1] = '<') and
     (ATokens[ALast] = '>') and
     (MatchingTypeToken(ATokens, AFirst + 1, ALast) = ALast) then
  begin
    if IsReferenceGenericName(ATokens[AFirst]) then
      Result := sltReference;
    Exit;
  end;

  if AFirst <> ALast then
    Exit;
  { A single name. A string literal type is quoted and a template literal
    type starts with a backtick, so neither reads as a keyword. `Object`, like
    the empty object type, admits every value but null and undefined, so it
    is not listed. }
  if ATokens[AFirst] = NUMBER_TYPE_NAME then
    Result := sltFloat
  else if ATokens[AFirst] = STRING_TYPE_NAME then
    Result := sltString
  else if ATokens[AFirst] = BOOLEAN_TYPE_NAME then
    Result := sltBoolean
  else if (ATokens[AFirst] = OBJECT_TYPE_NAME) or
          (ATokens[AFirst] = 'Function') then
    Result := sltReference;
end;

function TypeAnnotationToLocalType(const AAnnotation: string): TGocciaLocalType;
var
  Tokens: TTypeAnnotationTokens;
begin
  Tokens := TokenizeTypeAnnotation(AAnnotation);
  Result := ClassifyTypeTokens(Tokens, 0, High(Tokens));
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
  else if AExpr is TGocciaFunctionExpression then
    Result := sltReference
  else if AExpr is TGocciaObjectMethodDefinition then
    Result := sltReference;
end;

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
