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
  the type's outermost form. Only `number`, `string`, `boolean`, `object`,
  array and tuple types and function and constructor types get one; every
  other form, names such as `Array<T>` included, returns sltUntyped. }
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
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved,
  Goccia.Values.ErrorHelper;

type
  TTypeAnnotationTokens = array of string;

const
  // Extra token slots TokenizeTypeAnnotation allocates each time it grows.
  TOKEN_CAPACITY_STEP = 8;

{ Splits a collected annotation into its tokens. The parser joins token
  lexemes with spaces, keeps a string literal type as its source text and a
  template literal type as one backtick-quoted token with every backslash and
  backtick inside it escaped, so a literal is one token whatever its text
  holds. Punctuation splits even without spaces, so a compact spelling such as
  `number[]` reads the same. }
function TokenizeTypeAnnotation(const AAnnotation: string): TTypeAnnotationTokens;
var
  Count, I, Len, Start: Integer;
  Quote: Char;

  procedure Add(const AToken: string);
  begin
    if Count = Length(Result) then
      SetLength(Result, Count * 2 + TOKEN_CAPACITY_STEP);
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
      '''', '"', '`':
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

{ The index of the bracket that opens the one closed at AClose, or -1. }
function OpeningTypeToken(const ATokens: TTypeAnnotationTokens;
  const AFirst, AClose: Integer): Integer;
var
  Depth, I: Integer;
begin
  Depth := 0;
  for I := AClose downto AFirst do
  begin
    if IsClosingTypeToken(ATokens[I]) then
      Inc(Depth)
    else if IsOpeningTypeToken(ATokens[I]) then
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

{ Whether ATokens[AFirst..ALast] is a function or constructor type. Its
  return type runs to the end, so what follows its `=>` is part of it:
  `() => A & B` is a function type, not an intersection. }
function IsFunctionTypeTokens(const ATokens: TTypeAnnotationTokens;
  const AFirst, ALast: Integer): Boolean;
var
  Close: Integer;
begin
  Result := False;
  if not HasTopLevelTypeToken(ATokens, AFirst, ALast, ['=>']) then
    Exit;
  // `<T>(a: T) => T`, `new (a: T) => R`, `abstract new (a: T) => R`
  if (ATokens[AFirst] = '<') or (ATokens[AFirst] = KEYWORD_NEW) or
     (ATokens[AFirst] = KEYWORD_ABSTRACT) then
    Exit(True);
  // `(a: T) => R`, but not `(A)[]` or `(A) | B`
  if ATokens[AFirst] = '(' then
  begin
    Close := MatchingTypeToken(ATokens, AFirst, ALast);
    Result := (Close > 0) and (Close < ALast) and
      (ATokens[Close + 1] = '=>');
  end;
end;

{ The strict type every value of the type spelled by ATokens[AFirst..ALast]
  has, read from the type's outermost form in TypeScript's precedence. Only a
  form whose every value passes the check gets one: the check must never
  reject a value `tsc` accepts for the annotation. A name is never enforced,
  `Array<T>` and `Function` included, because a type alias, interface or
  import in the program can rebind it; the predefined type keywords cannot be
  rebound. }
function ClassifyTypeTokens(const ATokens: TTypeAnnotationTokens;
  const AFirst, ALast: Integer): TGocciaLocalType;
var
  Open: Integer;
begin
  Result := sltUntyped;
  if AFirst > ALast then
    Exit;

  if IsFunctionTypeTokens(ATokens, AFirst, ALast) then
    Exit(sltReference);

  { A union may hold values of several kinds, and an intersection or a
    conditional type is read from its members, which this does not do. A
    leading `|` or `&` counts as one. }
  if HasTopLevelTypeToken(ATokens, AFirst, ALast,
       ['|', '&', '?', KEYWORD_EXTENDS]) then
    Exit;

  // `readonly T[]` and `readonly [A, B]` are arrays.
  if ATokens[AFirst] = KEYWORD_READONLY then
  begin
    if ClassifyTypeTokens(ATokens, AFirst + 1, ALast) = sltReference then
      Result := sltReference;
    Exit;
  end;

  { `keyof T[]` is the keys of an array, and `unique symbol` a symbol. A type
    query (`typeof x`) is a primary type, so `typeof x[]` is an array and
    reads on. }
  if (ATokens[AFirst] = KEYWORD_KEYOF) or (ATokens[AFirst] = KEYWORD_UNIQUE) or
     (ATokens[AFirst] = KEYWORD_INFER) or (ATokens[AFirst] = KEYWORD_ASSERTS) then
    Exit;

  if ATokens[ALast] = ']' then
  begin
    Open := OpeningTypeToken(ATokens, AFirst, ALast);
    // `[A, B]`: a tuple is an array.
    if Open = AFirst then
      Exit(sltReference);
    { `T[]` is an array; `T[K]` is an indexed access type, which is whatever
      type the property K has. }
    if (Open > AFirst) and (Open = ALast - 1) then
      Result := sltReference;
    Exit;
  end;

  if (ATokens[AFirst] = '(') and
     (MatchingTypeToken(ATokens, AFirst, ALast) = ALast) then
    Exit(ClassifyTypeTokens(ATokens, AFirst + 1, ALast - 1));

  if AFirst <> ALast then
    Exit;
  { A single token. A string literal type is quoted and a template literal
    type starts with a backtick, so neither reads as a keyword. }
  if ATokens[AFirst] = NUMBER_TYPE_NAME then
    Result := sltFloat
  else if ATokens[AFirst] = STRING_TYPE_NAME then
    Result := sltString
  else if ATokens[AFirst] = BOOLEAN_TYPE_NAME then
    Result := sltBoolean
  else if ATokens[AFirst] = OBJECT_TYPE_NAME then
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
