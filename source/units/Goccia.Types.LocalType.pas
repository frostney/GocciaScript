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

implementation

uses
  Goccia.Constants.TypeNames,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved;

type
  TTypeAnnotationTokens = array of string;
  TTypeBracketPairs = array of Integer;

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

{ The index of the bracket that pairs with each token of ATokens, or -1 for a
  token that is no bracket or has no partner. One pass, so the classifier
  never rescans a range to find a bracket's partner. }
function PairTypeBrackets(const ATokens: TTypeAnnotationTokens): TTypeBracketPairs;
var
  Stack: array of Integer;
  Depth, I: Integer;
begin
  SetLength(Result, Length(ATokens));
  SetLength(Stack, Length(ATokens));
  Depth := 0;
  for I := 0 to High(ATokens) do
  begin
    Result[I] := -1;
    if IsOpeningTypeToken(ATokens[I]) then
    begin
      Stack[Depth] := I;
      Inc(Depth);
    end
    else if IsClosingTypeToken(ATokens[I]) and (Depth > 0) then
    begin
      Dec(Depth);
      Result[I] := Stack[Depth];
      Result[Stack[Depth]] := I;
    end;
  end;
end;

{ Whether a token outside every bracket of ATokens[AFirst..ALast] is one of
  ATargets. A bracket group is stepped over through its partner. }
function HasTopLevelTypeToken(const ATokens: TTypeAnnotationTokens;
  const APairs: TTypeBracketPairs; const AFirst, ALast: Integer;
  const ATargets: array of string): Boolean;
var
  I, J: Integer;
begin
  I := AFirst;
  while I <= ALast do
  begin
    if IsOpeningTypeToken(ATokens[I]) and (APairs[I] > I) then
      I := APairs[I]
    else
      for J := 0 to High(ATargets) do
        if ATokens[I] = ATargets[J] then
          Exit(True);
    Inc(I);
  end;
  Result := False;
end;

{ Whether ATokens[AFirst..ALast] is a function or constructor type, as
  TypeScript starts one: `(` or `<` opening a signature whose parameter list
  is followed by `=>`, `new`, or `abstract new`. Its return type runs to the
  end, so what follows the `=>` is part of it: `() => A & B` is a function
  type, not an intersection. }
function IsFunctionTypeTokens(const ATokens: TTypeAnnotationTokens;
  const APairs: TTypeBracketPairs; const AFirst, ALast: Integer): Boolean;
var
  Start: Integer;
begin
  Start := AFirst;
  if (ATokens[Start] = KEYWORD_ABSTRACT) and (Start < ALast) and
     (ATokens[Start + 1] = KEYWORD_NEW) then
    Inc(Start);
  if ATokens[Start] = KEYWORD_NEW then
    Inc(Start);
  if Start > ALast then
    Exit(False);
  // `<T>(a: T) => T`: type parameters, then the parameter list.
  if (ATokens[Start] = '<') and (APairs[Start] > Start) then
    Start := APairs[Start] + 1;
  Result := (Start < ALast) and (ATokens[Start] = '(') and
    (APairs[Start] > Start) and (APairs[Start] < ALast) and
    (ATokens[APairs[Start] + 1] = '=>');
end;

{ The strict type every value of the type spelled by ATokens[AFirst..ALast]
  has, read from the type's outermost form in TypeScript's precedence. Only a
  form whose every value passes the check gets one: the check must never
  reject a value `tsc` accepts for the annotation, given the built-in
  interfaces of the primitive wrappers (Number, String, Boolean, Symbol,
  BigInt) as the standard library declares them; a program that augments one
  to extend an array or function type can be refused by an array or function
  type check. A name is never enforced, `Array<T>` and `Function` included,
  because a type alias, interface or import in the program can rebind it;
  the predefined type keywords cannot be rebound. }
function ClassifyTypeTokens(const ATokens: TTypeAnnotationTokens;
  const APairs: TTypeBracketPairs; AFirst, ALast: Integer): TGocciaLocalType;
var
  Open: Integer;
  ReadOnlyArray: Boolean;
begin
  Result := sltUntyped;
  ReadOnlyArray := False;
  // Parentheses and `readonly` are peeled without recursion.
  while AFirst <= ALast do
  begin
    if (ATokens[AFirst] = '(') and (APairs[AFirst] = ALast) then
    begin
      Inc(AFirst);
      Dec(ALast);
    end
    // `readonly T[]` and `readonly [A, B]` are arrays.
    else if ATokens[AFirst] = KEYWORD_READONLY then
    begin
      ReadOnlyArray := True;
      Inc(AFirst);
    end
    else
      Break;
  end;
  if AFirst > ALast then
    Exit;

  if IsFunctionTypeTokens(ATokens, APairs, AFirst, ALast) then
  begin
    if not ReadOnlyArray then
      Result := sltReference;
    Exit;
  end;

  { A union may hold values of several kinds, and an intersection or a
    conditional type (the only form with a top-level `?`) is read from its
    members, which this does not do. A leading `|` or `&` counts as one. }
  if HasTopLevelTypeToken(ATokens, APairs, AFirst, ALast, ['|', '&', '?']) then
    Exit;

  { `keyof T[]` is the keys of an array. TypeScript reads `keyof`, `unique`
    and `infer` as operators wherever a type starts. A type query
    (`typeof x`) is a primary type, so `typeof x[]` is an array and reads
    on. }
  if (ATokens[AFirst] = KEYWORD_KEYOF) or (ATokens[AFirst] = KEYWORD_UNIQUE) or
     (ATokens[AFirst] = KEYWORD_INFER) then
    Exit;

  if ATokens[ALast] = ']' then
  begin
    Open := APairs[ALast];
    // `[A, B]`: a tuple is an array.
    if Open = AFirst then
      Exit(sltReference);
    { `T[]` is an array; `T[K]` is an indexed access type, which is whatever
      type the property K has. }
    if (Open > AFirst) and (Open = ALast - 1) then
      Result := sltReference;
    Exit;
  end;

  if ReadOnlyArray or (AFirst <> ALast) then
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
  Result := ClassifyTypeTokens(Tokens, PairTypeBrackets(Tokens), 0,
    High(Tokens));
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
