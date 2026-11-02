unit Goccia.Parser.TypeRanges;

{$I Goccia.inc}

{ Reads where each piece of a type is, from tokens the parser has already
  consumed and is about to erase, for `goccia:ast`'s expression view (ADR 0172).

  The parser collects a type as a run of tokens it skips, balancing brackets
  and nothing more. Once it has committed to that run, this reads it again and
  records the structure a formatter lays out: the members of a union, the
  elements of a tuple, the members of an object type, the arguments and
  parameters of a generic, the parameters of a function type, and each literal.
  It lexes nothing, never reads past the run, and never raises: a shape it does
  not recognise is passed over, and the parse it follows is unaffected. Ranges
  nest by construction, so a misread type is reported with the wrong kinds,
  never with overlapping or escaping ranges. Names and values stay out; a rule
  slices them from the source. }

interface

uses
  Generics.Collections,

  Goccia.AST.Node,
  Goccia.Token;

type
  { What a run of tokens is, which decides where reading starts. }
  TGocciaTypeSyntax = (
    tsAnnotation,     // `: T`; ATokens[AStartIndex - 1] is the colon
    tsType,           // `T`, as after `as` or `satisfies`
    tsTypeParameters, // `<T extends U = V>`
    tsTypeArguments,  // `<A, B>`
    tsImplements,     // `A, B<C>`, after `implements`
    tsTypeAlias,      // `type T<P> = ...`, with any modifier before `type`
    tsInterface       // `interface I<P> extends A { ... }`, likewise
  );

{ The ranges in ATokens[AStartIndex] up to, not including, ATokens[AEndIndex]. }
function ReadTypeRanges(const ATokens: TObjectList<TGocciaToken>;
  const ASyntax: TGocciaTypeSyntax;
  const AStartIndex, AEndIndex: Integer): TGocciaTypeRangeArray;

implementation

uses
  Goccia.Constants.TypeNames,
  Goccia.Keywords.Contextual;

const
  { Past this a type's remaining structure is not read. A type nested this
    deep is generated text, and reading it would recurse without bound. }
  MAX_TYPE_DEPTH = 256;

  { Tokens that read as a name in a type: an identifier, and every reserved
    word, since a member or a namespace may be named with one. }
  WORD_TOKENS = [gttTrue, gttFalse, gttNull, gttIdentifier,
    gttConst..gttFunction, gttTypeof, gttVoid, gttInstanceof, gttIn,
    gttDelete];

  { The predefined types that are spelled as identifiers. `null`, `void` and
    `this` are reserved words, read before these. }
  TYPE_KEYWORD_COUNT = 11;
  TYPE_KEYWORD_NAMES: array[0..TYPE_KEYWORD_COUNT - 1] of string = (
    KEYWORD_ANY, KEYWORD_UNKNOWN, NUMBER_TYPE_NAME, BIGINT_TYPE_NAME,
    BOOLEAN_TYPE_NAME, STRING_TYPE_NAME, SYMBOL_TYPE_NAME, OBJECT_TYPE_NAME,
    KEYWORD_NEVER, UNDEFINED_TYPE_NAME, KEYWORD_INTRINSIC);
  TYPE_KEYWORD_KINDS: array[0..TYPE_KEYWORD_COUNT - 1] of TGocciaASTPartKind = (
    apkAnyKeyword, apkUnknownKeyword, apkNumberKeyword, apkBigIntKeyword,
    apkBooleanKeyword, apkStringKeyword, apkSymbolKeyword, apkObjectKeyword,
    apkNeverKeyword, apkUndefinedKeyword, apkIntrinsicKeyword);

type
  { Where reading is, so a loop can tell whether an iteration moved. }
  TTypeReaderMark = record
    Position: Integer;
    GreaterUsed: Integer;
    Count: Integer;
    LastEnd: Integer;
  end;

  TTypeReader = class
  private
    FTokens: TObjectList<TGocciaToken>;
    FPosition: Integer;
    FEnd: Integer;
    { How many `>` of a `>>` or `>>>` token have been read: one closes each
      type argument or type parameter list. }
    FGreaterUsed: Integer;
    { The end offset of the last token, or `>`, read. }
    FLastEnd: Integer;
    FRanges: TGocciaTypeRangeArray;
    FCount: Integer;
    FDepth: Integer;
    { Reading the `extends` clause of a conditional type, directly: there an
      `infer U extends C` keeps its constraint even when a `?` follows. }
    FInExtendsClause: Boolean;

    function TokenAt(const AOffset: Integer): TGocciaToken;
    function TypeAt(const AOffset: Integer): TGocciaTokenType;
    function IsWord(const AOffset: Integer): Boolean;
    function WordIs(const AOffset: Integer; const AText: string): Boolean;
    function BreakBefore(const AOffset: Integer): Boolean;
    function CurrentStart: Integer;
    function AtEnd: Boolean;
    procedure Advance;
    function ClosesSubstitution(const AIndex: Integer): Boolean;
    function GroupEnd(const AIndex: Integer): Integer;
    procedure SkipBalanced;
    procedure SkipOpaqueType;
    procedure ParseQualifiedNameTail;
    function Here: TTypeReaderMark;
    function MovedFrom(const AMark: TTypeReaderMark): Boolean;
    procedure Restore(const AMark: TTypeReaderMark);

    function Open(const AKind: TGocciaASTPartKind; const AStartOffset: Integer;
      const AFlags: TGocciaASTPartFlags = []): Integer;
    procedure Close(const AIndex: Integer);
    function Wrap(const AKind: TGocciaASTPartKind; const AFirst: Integer;
      const AStartOffset: Integer = -1): Integer;
    procedure Leaf(const AKind: TGocciaASTPartKind);

    function StartsType(const AOffset: Integer): Boolean;
    function StartsFunctionType: Boolean;
    function StartsMappedType: Boolean;
    function StartsNamedTupleMember: Boolean;

    function ParseType(const AAllowConditional: Boolean = True): Boolean;
    function ParseUnionOrIntersection(const AKind: TGocciaASTPartKind;
      const AOperator: TGocciaTokenType): Boolean;
    function ParseTypeOperator: Boolean;
    function ParseInfer: Boolean;
    function ParsePostfix: Boolean;
    function ParsePrimary: Boolean;
    procedure ParseLiteralType(const ALiteralKind: TGocciaASTPartKind);
    procedure ParseTypeReference;
    procedure ParseTemplateLiteralType;
    procedure ParseTypeQuery;
    procedure ParseImportType;
    procedure ParseTuple;
    procedure ParseTupleElement;
    procedure ParseNamedTupleMember;
    procedure ParseObjectType(const AKind: TGocciaASTPartKind);
    procedure ParseMember;
    procedure ParseMappedType;
    procedure ParseFunctionType(const AKind: TGocciaASTPartKind);
    procedure ParseSignatureTail;
    procedure ParseParameters;
    procedure ParseParameter;
    procedure ParseAnnotation;
    procedure ParseReturnType;
    procedure ParseTypeParameters;
    procedure ParseTypeArguments;
    procedure ParseNamedType(const AKind: TGocciaASTPartKind);
    procedure ParseHeritageList(const AKind: TGocciaASTPartKind);
    procedure SkipToWord(const AText: string);
  public
    constructor Create(const ATokens: TObjectList<TGocciaToken>;
      const AStartIndex, AEndIndex: Integer);
    function Read(const ASyntax: TGocciaTypeSyntax): TGocciaTypeRangeArray;
  end;

function KeyKindOf(const AToken: TGocciaToken): TGocciaASTPartKind;
begin
  case AToken.TokenType of
    gttString: Result := apkKeyString;
    gttNumber: Result := apkKeyNumeric;
    gttBigInt: Result := apkKeyBigInt;
  else
    Result := apkKeyIdentifier;
  end;
end;

{ TTypeReader }

constructor TTypeReader.Create(const ATokens: TObjectList<TGocciaToken>;
  const AStartIndex, AEndIndex: Integer);
begin
  inherited Create;
  FTokens := ATokens;
  FPosition := AStartIndex;
  FEnd := AEndIndex;
  if FEnd > FTokens.Count then
    FEnd := FTokens.Count;
  if (FPosition >= 0) and (FPosition < FEnd) then
    FLastEnd := FTokens[FPosition].Span.StartOffset
  else
    FLastEnd := 0;
end;

function TTypeReader.TokenAt(const AOffset: Integer): TGocciaToken;
var
  Index: Integer;
begin
  Index := FPosition + AOffset;
  if (Index >= 0) and (Index < FEnd) then
    Result := FTokens[Index]
  else
    Result := nil;
end;

{ A `>>` or `>>>` in a type closes two or three lists, so it reads as `>`. }
function TTypeReader.TypeAt(const AOffset: Integer): TGocciaTokenType;
var
  Token: TGocciaToken;
begin
  Token := TokenAt(AOffset);
  if not Assigned(Token) then
    Exit(gttEOF);
  Result := Token.TokenType;
  if Result in [gttRightShift, gttUnsignedRightShift] then
    Result := gttGreater;
end;

function TTypeReader.IsWord(const AOffset: Integer): Boolean;
var
  Token: TGocciaToken;
begin
  Token := TokenAt(AOffset);
  Result := Assigned(Token) and (Token.TokenType in WORD_TOKENS);
end;

function TTypeReader.WordIs(const AOffset: Integer;
  const AText: string): Boolean;
var
  Token: TGocciaToken;
begin
  Token := TokenAt(AOffset);
  Result := Assigned(Token) and (Token.TokenType in WORD_TOKENS) and
    not Token.ContainsEscape and (Token.Lexeme = AText);
end;

function TTypeReader.BreakBefore(const AOffset: Integer): Boolean;
var
  Index: Integer;
begin
  Index := FPosition + AOffset;
  if (Index <= 0) or (Index >= FEnd) then
    Exit(False);
  Result := FTokens[Index - 1].Line < FTokens[Index].StartLine;
end;

function TTypeReader.CurrentStart: Integer;
begin
  if AtEnd then
    Exit(FLastEnd);
  Result := FTokens[FPosition].Span.StartOffset + FGreaterUsed;
end;

function TTypeReader.AtEnd: Boolean;
begin
  Result := FPosition >= FEnd;
end;

procedure TTypeReader.Advance;
var
  Token: TGocciaToken;
  Greaters: Integer;
begin
  if AtEnd then
    Exit;
  Token := FTokens[FPosition];
  case Token.TokenType of
    gttRightShift: Greaters := 2;
    gttUnsignedRightShift: Greaters := 3;
  else
    Greaters := 0;
  end;
  if Greaters = 0 then
  begin
    FLastEnd := Token.Span.EndOffset;
    Inc(FPosition);
    Exit;
  end;
  Inc(FGreaterUsed);
  FLastEnd := Token.Span.StartOffset + FGreaterUsed;
  if FGreaterUsed >= Greaters then
  begin
    FGreaterUsed := 0;
    Inc(FPosition);
  end;
end;

{ The closing brace of a template substitution is a token of its own,
  before the template's next part, and closes no bracket. }
function TTypeReader.ClosesSubstitution(const AIndex: Integer): Boolean;
begin
  Result := (FTokens[AIndex].TokenType = gttRightBrace) and
    (AIndex + 1 < FEnd) and
    (FTokens[AIndex + 1].TokenType in [gttTemplateMiddle, gttTemplateTail]);
end;

{ The index just past the bracketed group that ATokens[AIndex] opens, or
  FEnd when the run ends first. }
function TTypeReader.GroupEnd(const AIndex: Integer): Integer;
var
  Depth: Integer;
begin
  Depth := 0;
  Result := AIndex;
  repeat
    case FTokens[Result].TokenType of
      gttLeftParen, gttLeftBracket, gttLeftBrace: Inc(Depth);
      gttRightParen, gttRightBracket: Dec(Depth);
      gttRightBrace:
        if not ClosesSubstitution(Result) then
          Dec(Depth);
    end;
    Inc(Result);
  until (Result >= FEnd) or (Depth <= 0);
end;

{ Passes over a bracketed group, the current token opening it. }
procedure TTypeReader.SkipBalanced;
var
  Last: Integer;
begin
  if AtEnd then
    Exit;
  Last := GroupEnd(FPosition);
  while FPosition < Last do
    Advance;
end;

{ Passes over a type too deeply nested to read, up to the token that ends
  it in the list or brackets around it, so what holds it still ends where
  its text does. }
procedure TTypeReader.SkipOpaqueType;
var
  Depth: Integer;
begin
  Depth := 0;
  while not AtEnd do
  begin
    case TypeAt(0) of
      gttLeftParen, gttLeftBracket, gttLeftBrace, gttLess, gttTemplateHead:
        Inc(Depth);
      gttRightParen, gttRightBracket, gttRightBrace, gttGreater,
      gttTemplateTail:
        if not ((FGreaterUsed = 0) and ClosesSubstitution(FPosition)) then
        begin
          if Depth = 0 then
            Exit;
          Dec(Depth);
        end;
      gttComma, gttSemicolon, gttTemplateMiddle:
        if Depth = 0 then
          Exit;
    end;
    Advance;
  end;
end;

{ The rest of a name after its first word, `.b.c`, and the type arguments
  after it. A `<` on the next line is not a type argument list. }
procedure TTypeReader.ParseQualifiedNameTail;
begin
  while (TypeAt(0) = gttDot) and IsWord(1) do
  begin
    Advance;
    Advance;
  end;
  if (TypeAt(0) = gttLess) and not BreakBefore(0) then
    ParseTypeArguments;
end;

function TTypeReader.Here: TTypeReaderMark;
begin
  Result.Position := FPosition;
  Result.GreaterUsed := FGreaterUsed;
  Result.Count := FCount;
  Result.LastEnd := FLastEnd;
end;

function TTypeReader.MovedFrom(const AMark: TTypeReaderMark): Boolean;
begin
  Result := (FPosition <> AMark.Position) or
    (FGreaterUsed <> AMark.GreaterUsed);
end;

{ Ranges are only ever added after the mark, so going back drops them. }
procedure TTypeReader.Restore(const AMark: TTypeReaderMark);
begin
  FPosition := AMark.Position;
  FGreaterUsed := AMark.GreaterUsed;
  FCount := AMark.Count;
  FLastEnd := AMark.LastEnd;
end;

function TTypeReader.Open(const AKind: TGocciaASTPartKind;
  const AStartOffset: Integer; const AFlags: TGocciaASTPartFlags): Integer;
begin
  if FCount = Length(FRanges) then
    if FCount = 0 then
      SetLength(FRanges, 8)
    else
      SetLength(FRanges, FCount * 2);
  Result := FCount;
  Inc(FCount);
  FRanges[Result].Kind := AKind;
  FRanges[Result].StartOffset := AStartOffset;
  FRanges[Result].EndOffset := AStartOffset;
  FRanges[Result].Flags := AFlags;
  FRanges[Result].SubtreeEnd := Result + 1;
end;

{ A range ends at what was read last, and holds every range added since it
  opened. }
procedure TTypeReader.Close(const AIndex: Integer);
begin
  if FLastEnd > FRanges[AIndex].StartOffset then
    FRanges[AIndex].EndOffset := FLastEnd;
  FRanges[AIndex].SubtreeEnd := FCount;
end;

{ Opens a range around the ones from AFirst on, which are one complete type:
  the first member of a union, the element type of `T[]`. }
function TTypeReader.Wrap(const AKind: TGocciaASTPartKind;
  const AFirst: Integer; const AStartOffset: Integer): Integer;
var
  I: Integer;
begin
  if FCount = Length(FRanges) then
    SetLength(FRanges, FCount * 2 + 8);
  for I := FCount downto AFirst + 1 do
    FRanges[I] := FRanges[I - 1];
  Inc(FCount);
  for I := AFirst + 1 to FCount - 1 do
    Inc(FRanges[I].SubtreeEnd);
  FRanges[AFirst].Kind := AKind;
  if AStartOffset >= 0 then
    FRanges[AFirst].StartOffset := AStartOffset
  else
    FRanges[AFirst].StartOffset := FRanges[AFirst + 1].StartOffset;
  FRanges[AFirst].EndOffset := FRanges[AFirst].StartOffset;
  FRanges[AFirst].Flags := [];
  FRanges[AFirst].SubtreeEnd := AFirst + 1;
  Result := AFirst;
end;

procedure TTypeReader.Leaf(const AKind: TGocciaASTPartKind);
var
  Index: Integer;
begin
  Index := Open(AKind, CurrentStart);
  Advance;
  Close(Index);
end;

function TTypeReader.StartsType(const AOffset: Integer): Boolean;
begin
  Result := IsWord(AOffset) or (TypeAt(AOffset) in [gttString, gttNumber,
    gttBigInt, gttTemplate, gttTemplateHead, gttMinus, gttLeftParen,
    gttLeftBracket, gttLeftBrace, gttLess]);
end;

{ `(` opens a function type's parameters when an `=>` follows the group it
  opens, and a parenthesized type otherwise. }
function TTypeReader.StartsFunctionType: Boolean;
var
  Index: Integer;
begin
  if TypeAt(0) = gttLess then
    Exit(True);
  if TypeAt(0) <> gttLeftParen then
    Exit(False);
  Index := GroupEnd(FPosition);
  Result := (Index < FEnd) and (FTokens[Index].TokenType = gttArrow);
end;

function TTypeReader.StartsMappedType: Boolean;
var
  Offset: Integer;
begin
  Offset := 1;
  if TypeAt(Offset) in [gttPlus, gttMinus] then
    Inc(Offset);
  if WordIs(Offset, KEYWORD_READONLY) then
    Inc(Offset);
  Result := (TypeAt(Offset) = gttLeftBracket) and IsWord(Offset + 1) and
    (TypeAt(Offset + 2) = gttIn);
end;

function TTypeReader.StartsNamedTupleMember: Boolean;
begin
  Result := IsWord(0) and ((TypeAt(1) = gttColon) or
    ((TypeAt(1) = gttQuestion) and (TypeAt(2) = gttColon)));
end;

{ A type: a function or constructor type, or a union, which a conditional
  type extends. }
function TTypeReader.ParseType(const AAllowConditional: Boolean): Boolean;
var
  First, Node: Integer;
  Mark: TTypeReaderMark;
  SavedInExtendsClause: Boolean;
begin
  Result := False;
  if AtEnd then
    Exit;
  if FDepth >= MAX_TYPE_DEPTH then
  begin
    Mark := Here;
    SkipOpaqueType;
    Exit(MovedFrom(Mark));
  end;
  Inc(FDepth);
  SavedInExtendsClause := FInExtendsClause;
  FInExtendsClause := not AAllowConditional;
  try
    if StartsFunctionType then
    begin
      ParseFunctionType(apkFunctionType);
      Exit(True);
    end;
    if (TypeAt(0) = gttNew) or
       (WordIs(0, KEYWORD_ABSTRACT) and (TypeAt(1) = gttNew)) then
    begin
      ParseFunctionType(apkConstructorType);
      Exit(True);
    end;

    First := FCount;
    if not ParseUnionOrIntersection(apkUnionType, gttBitwiseOr) then
      Exit;
    Result := True;
    if not AAllowConditional or (TypeAt(0) <> gttExtends) or
       (FCount = First) then
      Exit;

    Node := Wrap(apkConditionalType, First);
    Advance;
    ParseType(False);
    if TypeAt(0) = gttQuestion then
    begin
      Advance;
      ParseType(True);
    end;
    if TypeAt(0) = gttColon then
    begin
      Advance;
      ParseType(True);
    end;
    Close(Node);
  finally
    FInExtendsClause := SavedInExtendsClause;
    Dec(FDepth);
  end;
end;

{ A union of intersections, or an intersection of operator types. A leading
  operator belongs to the list, which is a list even with one member then. }
function TTypeReader.ParseUnionOrIntersection(const AKind: TGocciaASTPartKind;
  const AOperator: TGocciaTokenType): Boolean;

  function ParseMember: Boolean;
  begin
    if AKind = apkUnionType then
      Result := ParseUnionOrIntersection(apkIntersectionType, gttBitwiseAnd)
    else
      Result := ParseTypeOperator;
  end;

var
  First, Node, LeadingStart: Integer;
begin
  LeadingStart := -1;
  if TypeAt(0) = AOperator then
  begin
    LeadingStart := CurrentStart;
    Advance;
  end;
  First := FCount;
  if not ParseMember or (FCount = First) then
    Exit(LeadingStart >= 0);
  Result := True;
  if (TypeAt(0) <> AOperator) and (LeadingStart < 0) then
    Exit;
  Node := Wrap(AKind, First, LeadingStart);
  while TypeAt(0) = AOperator do
  begin
    Advance;
    if not ParseMember then
      Break;
  end;
  Close(Node);
end;

function TTypeReader.ParseTypeOperator: Boolean;
var
  Node: Integer;
begin
  if (WordIs(0, KEYWORD_KEYOF) or WordIs(0, KEYWORD_UNIQUE) or
      WordIs(0, KEYWORD_READONLY)) and StartsType(1) then
  begin
    { `keyof keyof T` nests without passing through ParseType. }
    if FDepth >= MAX_TYPE_DEPTH then
    begin
      SkipOpaqueType;
      Exit(True);
    end;
    Node := Open(apkTypeOperator, CurrentStart);
    Advance;
    Inc(FDepth);
    try
      ParseTypeOperator();
    finally
      Dec(FDepth);
    end;
    Close(Node);
    Exit(True);
  end;
  if WordIs(0, KEYWORD_INFER) and IsWord(1) then
    Exit(ParseInfer);
  Result := ParsePostfix;
end;

{ `infer U`, and `infer U extends C`. Outside a conditional's `extends`
  clause, a `?` after the constraint makes `extends` begin a conditional
  type instead, as TypeScript reads it. }
function TTypeReader.ParseInfer: Boolean;
var
  Node, Parameter: Integer;
  Mark: TTypeReaderMark;
begin
  Node := Open(apkInferType, CurrentStart);
  Advance;
  Parameter := Open(apkTypeParameter, CurrentStart);
  Advance;
  if TypeAt(0) = gttExtends then
  begin
    Mark := Here;
    Advance;
    ParseType(False);
    if (TypeAt(0) = gttQuestion) and not FInExtendsClause then
      Restore(Mark);
  end;
  Close(Parameter);
  Close(Node);
  Result := True;
end;

{ `T[]` and `T[K]`, applied to the type before them, left to right. Each
  one nests the type before it a level deeper, so they count towards the
  depth limit. }
function TTypeReader.ParsePostfix: Boolean;
var
  First, Node, Levels: Integer;
begin
  First := FCount;
  if not ParsePrimary then
    Exit(False);
  Result := True;
  if FCount = First then
    Exit;
  Levels := 0;
  while (TypeAt(0) = gttLeftBracket) and not BreakBefore(0) and
        (FDepth + Levels < MAX_TYPE_DEPTH) do
  begin
    Inc(Levels);
    if TypeAt(1) = gttRightBracket then
    begin
      Node := Wrap(apkArrayType, First);
      Advance;
      Advance;
    end
    else
    begin
      Node := Wrap(apkIndexedAccessType, First);
      Advance;
      ParseType(True);
      if TypeAt(0) = gttRightBracket then
        Advance;
    end;
    Close(Node);
  end;
  while (TypeAt(0) = gttLeftBracket) and not BreakBefore(0) do
    SkipBalanced;
end;

function TTypeReader.ParsePrimary: Boolean;
var
  Node: Integer;
begin
  Result := True;
  case TypeAt(0) of
    gttString: ParseLiteralType(apkKeyString);
    gttNumber: ParseLiteralType(apkKeyNumeric);
    gttBigInt: ParseLiteralType(apkKeyBigInt);
    gttTrue, gttFalse: ParseLiteralType(apkBooleanLiteral);
    gttTemplate: ParseLiteralType(apkTemplateLiteral);
    gttMinus:
      begin
        if not (TypeAt(1) in [gttNumber, gttBigInt]) then
          Exit(False);
        { The literal is the number; the `-` is the literal type's. }
        Node := Open(apkLiteralType, CurrentStart);
        Advance;
        Leaf(KeyKindOf(TokenAt(0)));
        Close(Node);
      end;
    gttTemplateHead: ParseTemplateLiteralType;
    gttNull: Leaf(apkNullKeyword);
    gttVoid: Leaf(apkVoidKeyword);
    gttThis: Leaf(apkThisType);
    gttTypeof: ParseTypeQuery;
    gttImport: ParseImportType;
    gttLeftParen:
      begin
        Node := Open(apkParenthesizedType, CurrentStart);
        Advance;
        ParseType(True);
        if TypeAt(0) = gttRightParen then
          Advance;
        Close(Node);
      end;
    gttLeftBracket: ParseTuple;
    gttLeftBrace:
      if StartsMappedType then
        ParseMappedType
      else
        ParseObjectType(apkTypeLiteral);
  else
    if not IsWord(0) then
      Exit(False);
    ParseTypeReference;
  end;
end;

procedure TTypeReader.ParseLiteralType(const ALiteralKind: TGocciaASTPartKind);
var
  Node: Integer;
begin
  Node := Open(apkLiteralType, CurrentStart);
  Leaf(ALiteralKind);
  Close(Node);
end;

{ A predefined type such as `string`, or a name, qualified or not, with its
  type arguments. The name itself is not a node: a rule slices it. }
procedure TTypeReader.ParseTypeReference;
var
  I: Integer;
  Token: TGocciaToken;
begin
  Token := TokenAt(0);
  if (Token.TokenType = gttIdentifier) and not Token.ContainsEscape and
     (TypeAt(1) <> gttDot) then
    for I := 0 to TYPE_KEYWORD_COUNT - 1 do
      if Token.Lexeme = TYPE_KEYWORD_NAMES[I] then
      begin
        Leaf(TYPE_KEYWORD_KINDS[I]);
        Exit;
      end;

  ParseNamedType(apkTypeReference);
end;

procedure TTypeReader.ParseTemplateLiteralType;
var
  Node: Integer;
  Mark: TTypeReaderMark;
begin
  Node := Open(apkTemplateLiteralType, CurrentStart);
  Advance;
  while not AtEnd do
  begin
    Mark := Here;
    ParseType(True);
    if TypeAt(0) = gttTemplateMiddle then
      Advance
    else if TypeAt(0) = gttTemplateTail then
    begin
      Advance;
      Break;
    end
    else if not MovedFrom(Mark) then
      Advance;
  end;
  Close(Node);
end;

procedure TTypeReader.ParseTypeQuery;
var
  Node: Integer;
begin
  Node := Open(apkTypeQuery, CurrentStart);
  Advance;
  if TypeAt(0) = gttImport then
    ParseImportType
  else if IsWord(0) then
  begin
    Advance;
    ParseQualifiedNameTail;
  end;
  Close(Node);
end;

{ `import("m").T<A>`. The module path is a string literal type, as
  typescript-estree has it. }
procedure TTypeReader.ParseImportType;
var
  Node: Integer;
begin
  Node := Open(apkImportType, CurrentStart);
  Advance;
  if TypeAt(0) = gttLeftParen then
  begin
    Advance;
    if TypeAt(0) = gttString then
      ParseLiteralType(apkKeyString);
    while not AtEnd and (TypeAt(0) <> gttRightParen) do
      if TypeAt(0) in [gttLeftParen, gttLeftBracket, gttLeftBrace] then
        SkipBalanced
      else
        Advance;
    if TypeAt(0) = gttRightParen then
      Advance;
  end;
  ParseQualifiedNameTail;
  Close(Node);
end;

procedure TTypeReader.ParseTuple;
var
  Node: Integer;
  Mark: TTypeReaderMark;
begin
  Node := Open(apkTupleType, CurrentStart);
  Advance;
  while not AtEnd and (TypeAt(0) <> gttRightBracket) do
  begin
    Mark := Here;
    ParseTupleElement;
    if TypeAt(0) = gttComma then
      Advance
    else if not MovedFrom(Mark) then
      Advance;
  end;
  if TypeAt(0) = gttRightBracket then
    Advance;
  Close(Node);
end;

procedure TTypeReader.ParseTupleElement;
var
  First, Node: Integer;
begin
  if TypeAt(0) = gttSpread then
  begin
    Node := Open(apkRestType, CurrentStart);
    Advance;
    if StartsNamedTupleMember then
      ParseNamedTupleMember
    else
      ParseType(True);
    Close(Node);
  end
  else if StartsNamedTupleMember then
    ParseNamedTupleMember
  else
  begin
    First := FCount;
    if ParseType and (FCount > First) and (TypeAt(0) = gttQuestion) then
    begin
      Node := Wrap(apkOptionalType, First);
      Advance;
      Close(Node);
    end;
  end;
end;

procedure TTypeReader.ParseNamedTupleMember;
var
  Node: Integer;
begin
  Node := Open(apkNamedTupleMember, CurrentStart);
  Advance;
  if TypeAt(0) = gttQuestion then
    Advance;
  Advance;
  ParseType(True);
  Close(Node);
end;

{ An object type or an interface body. A member's range stops before the
  `,` or `;` after it, as an object literal's property stops before its
  comma, so what lies between the last member and the closing brace is the separator
  a formatter looks for. }
procedure TTypeReader.ParseObjectType(const AKind: TGocciaASTPartKind);
var
  Node: Integer;
  Mark: TTypeReaderMark;
begin
  Node := Open(AKind, CurrentStart);
  Advance;
  while not AtEnd and (TypeAt(0) <> gttRightBrace) do
  begin
    if TypeAt(0) in [gttSemicolon, gttComma] then
    begin
      Advance;
      Continue;
    end;
    Mark := Here;
    ParseMember;
    if not MovedFrom(Mark) then
      Advance;
  end;
  if TypeAt(0) = gttRightBrace then
    Advance;
  Close(Node);
end;

procedure TTypeReader.ParseMember;

  { `readonly`, `get` and `set` modify a member when a key follows them, and
    are the key otherwise: `readonly: boolean`, `get(): T`. }
  function IsModifier: Boolean;
  begin
    Result := (WordIs(0, KEYWORD_READONLY) or WordIs(0, KEYWORD_GET) or
      WordIs(0, KEYWORD_SET)) and
      (IsWord(1) or (TypeAt(1) in [gttString, gttNumber, gttBigInt,
        gttLeftBracket]));
  end;

var
  Node, KeyEnd, After, StartOffset: Integer;
  Flags: TGocciaASTPartFlags;
  IsComputed, IsMethod: Boolean;
begin
  StartOffset := CurrentStart;
  if TypeAt(0) in [gttLeftParen, gttLess] then
  begin
    Node := Open(apkCallSignature, StartOffset);
    ParseSignatureTail;
    Close(Node);
    Exit;
  end;
  if (TypeAt(0) = gttNew) and (TypeAt(1) in [gttLeftParen, gttLess]) then
  begin
    Node := Open(apkConstructSignature, StartOffset);
    Advance;
    ParseSignatureTail;
    Close(Node);
    Exit;
  end;

  while IsModifier do
    Advance;

  if (TypeAt(0) = gttLeftBracket) and IsWord(1) and (TypeAt(2) = gttColon) then
  begin
    Node := Open(apkIndexSignature, StartOffset);
    Advance;
    while not AtEnd and (TypeAt(0) <> gttRightBracket) do
      if IsWord(0) then
      begin
        ParseParameter;
        if TypeAt(0) = gttComma then
          Advance;
      end
      else
        Advance;
    if TypeAt(0) = gttRightBracket then
      Advance;
    if TypeAt(0) = gttColon then
      ParseAnnotation;
    Close(Node);
    Exit;
  end;

  { The key is one token, or a bracketed expression. What follows it, past
    an optional `?`, says whether the member is a method. }
  IsComputed := TypeAt(0) = gttLeftBracket;
  if IsComputed then
    KeyEnd := GroupEnd(FPosition)
  else if IsWord(0) or (TypeAt(0) in [gttString, gttNumber, gttBigInt]) then
    KeyEnd := FPosition + 1
  else
    Exit;
  After := KeyEnd - FPosition;
  if TypeAt(After) = gttQuestion then
    Inc(After);
  IsMethod := TypeAt(After) in [gttLeftParen, gttLess];

  Flags := [];
  if IsComputed then
    Include(Flags, apfComputed);
  if IsMethod then
    Node := Open(apkMethodSignature, StartOffset, Flags)
  else
    Node := Open(apkPropertySignature, StartOffset, Flags);
  { A computed key that is one literal, `['k']`, reports the literal; any
    other is an expression, which the reader does not read. }
  if not IsComputed then
    Leaf(KeyKindOf(TokenAt(0)))
  else if (KeyEnd - FPosition = 3) and
          (TypeAt(1) in [gttString, gttNumber, gttBigInt]) then
  begin
    Advance;
    Leaf(KeyKindOf(TokenAt(0)));
    Advance;
  end
  else
    while FPosition < KeyEnd do
      Advance;
  if TypeAt(0) = gttQuestion then
    Advance;
  if IsMethod then
    ParseSignatureTail
  else if TypeAt(0) = gttColon then
    ParseAnnotation;
  Close(Node);
end;

{ A mapped type, `readonly [K in keyof T as N]?: V` in braces: the
  constraint, the name type and the value type, in that order. }
procedure TTypeReader.ParseMappedType;
var
  Node: Integer;
begin
  Node := Open(apkMappedType, CurrentStart);
  Advance;
  if TypeAt(0) in [gttPlus, gttMinus] then
    Advance;
  if WordIs(0, KEYWORD_READONLY) then
    Advance;
  Advance;
  Advance;
  Advance;
  ParseType(True);
  if TypeAt(0) = gttAs then
  begin
    Advance;
    ParseType(True);
  end;
  if TypeAt(0) = gttRightBracket then
    Advance;
  if TypeAt(0) in [gttPlus, gttMinus] then
    Advance;
  if TypeAt(0) = gttQuestion then
    Advance;
  if TypeAt(0) = gttColon then
  begin
    Advance;
    ParseType(True);
  end;
  while TypeAt(0) in [gttSemicolon, gttComma] do
    Advance;
  if TypeAt(0) = gttRightBrace then
    Advance;
  Close(Node);
end;

{ `<T>(a: T) => R` and `abstract new (a: T) => R`. The `=> R` is the
  function type's TSTypeAnnotation, as typescript-estree has it. }
procedure TTypeReader.ParseFunctionType(const AKind: TGocciaASTPartKind);
var
  Node, Annotation: Integer;
begin
  Node := Open(AKind, CurrentStart);
  if WordIs(0, KEYWORD_ABSTRACT) then
    Advance;
  if TypeAt(0) = gttNew then
    Advance;
  if TypeAt(0) = gttLess then
    ParseTypeParameters;
  if TypeAt(0) = gttLeftParen then
    ParseParameters;
  if TypeAt(0) = gttArrow then
  begin
    Annotation := Open(apkTypeAnnotation, CurrentStart);
    Advance;
    ParseReturnType;
    Close(Annotation);
  end;
  Close(Node);
end;

procedure TTypeReader.ParseSignatureTail;
begin
  if TypeAt(0) = gttLess then
    ParseTypeParameters;
  if TypeAt(0) = gttLeftParen then
    ParseParameters;
  if TypeAt(0) = gttColon then
    ParseAnnotation;
end;

procedure TTypeReader.ParseParameters;
var
  Mark: TTypeReaderMark;
begin
  Advance;
  while not AtEnd and (TypeAt(0) <> gttRightParen) do
  begin
    Mark := Here;
    ParseParameter;
    if TypeAt(0) = gttComma then
      Advance
    else if not MovedFrom(Mark) then
      Advance;
  end;
  if TypeAt(0) = gttRightParen then
    Advance;
end;

{ A parameter of a function type or a signature: `a?: T`, `...rest: T[]`,
  or a pattern with its annotation. Its binding is not a node; its
  annotation is. }
procedure TTypeReader.ParseParameter;
var
  Node: Integer;
  Flags: TGocciaASTPartFlags;
  Mark: TTypeReaderMark;
begin
  Mark := Here;
  Flags := [];
  if TypeAt(0) = gttSpread then
    Include(Flags, apfRest);
  Node := Open(apkParameter, CurrentStart, Flags);
  if apfRest in Flags then
    Advance;
  if TypeAt(0) in [gttLeftBrace, gttLeftBracket] then
    SkipBalanced
  else if IsWord(0) then
    Advance
  else
  begin
    Restore(Mark);
    Exit;
  end;
  if TypeAt(0) = gttQuestion then
    Advance;
  if TypeAt(0) = gttColon then
    ParseAnnotation;
  Close(Node);
end;

procedure TTypeReader.ParseAnnotation;
var
  Node: Integer;
begin
  Node := Open(apkTypeAnnotation, CurrentStart);
  Advance;
  ParseReturnType;
  Close(Node);
end;

{ A type, or a type predicate where a return type may hold one: `x is T`,
  `this is T`, `asserts x`, `asserts x is T`. }
procedure TTypeReader.ParseReturnType;
var
  Node: Integer;
begin
  if IsWord(0) and WordIs(1, KEYWORD_IS) and
     not BreakBefore(1) then
  begin
    Node := Open(apkTypePredicate, CurrentStart);
    Advance;
    Advance;
    ParseType(True);
    Close(Node);
  end
  else if WordIs(0, KEYWORD_ASSERTS) and IsWord(1) and not BreakBefore(1) then
  begin
    Node := Open(apkTypePredicate, CurrentStart);
    Advance;
    Advance;
    if WordIs(0, KEYWORD_IS) and not BreakBefore(0) then
    begin
      Advance;
      ParseType(True);
    end;
    Close(Node);
  end
  else
    ParseType(True);
end;

procedure TTypeReader.ParseTypeParameters;
var
  Node, Parameter: Integer;
  Mark: TTypeReaderMark;
begin
  Node := Open(apkTypeParameterDeclaration, CurrentStart);
  Advance;
  while not AtEnd and (TypeAt(0) <> gttGreater) do
  begin
    Mark := Here;
    if IsWord(0) then
    begin
      Parameter := Open(apkTypeParameter, CurrentStart);
      while ((TypeAt(0) in [gttConst, gttIn]) or WordIs(0, KEYWORD_OUT)) and
            IsWord(1) and (TypeAt(1) <> gttExtends) do
        Advance;
      Advance;
      if TypeAt(0) = gttExtends then
      begin
        Advance;
        ParseType(True);
      end;
      if TypeAt(0) = gttAssign then
      begin
        Advance;
        ParseType(True);
      end;
      Close(Parameter);
    end;
    if TypeAt(0) = gttComma then
      Advance
    else if not MovedFrom(Mark) then
      Advance;
  end;
  if TypeAt(0) = gttGreater then
    Advance;
  Close(Node);
end;

procedure TTypeReader.ParseTypeArguments;
var
  Node: Integer;
  Mark: TTypeReaderMark;
begin
  Node := Open(apkTypeParameterInstantiation, CurrentStart);
  Advance;
  while not AtEnd and (TypeAt(0) <> gttGreater) do
  begin
    Mark := Here;
    ParseType(True);
    if TypeAt(0) = gttComma then
      Advance
    else if not MovedFrom(Mark) then
      Advance;
  end;
  if TypeAt(0) = gttGreater then
    Advance;
  Close(Node);
end;

{ A name, qualified or not, with its type arguments: a type reference, or
  `Base<T>` in an `extends` or `implements` list. }
procedure TTypeReader.ParseNamedType(const AKind: TGocciaASTPartKind);
var
  Node: Integer;
begin
  if not IsWord(0) then
    Exit;
  Node := Open(AKind, CurrentStart);
  Advance;
  ParseQualifiedNameTail;
  Close(Node);
end;

procedure TTypeReader.ParseHeritageList(const AKind: TGocciaASTPartKind);
var
  Mark: TTypeReaderMark;
begin
  while not AtEnd and (TypeAt(0) <> gttLeftBrace) do
  begin
    Mark := Here;
    ParseNamedType(AKind);
    if TypeAt(0) = gttComma then
      Advance
    else if not MovedFrom(Mark) then
      Advance;
  end;
end;

{ Passes over modifiers such as `export` and `declare`, up to and including
  the word that opens the declaration, and the declaration's name. }
procedure TTypeReader.SkipToWord(const AText: string);
begin
  while not AtEnd and not WordIs(0, AText) do
    Advance;
  Advance;
  if IsWord(0) then
    Advance;
end;

function TTypeReader.Read(const ASyntax: TGocciaTypeSyntax): TGocciaTypeRangeArray;
begin
  case ASyntax of
    tsAnnotation:
      if (FPosition > 0) and (FTokens[FPosition - 1].TokenType = gttColon) then
      begin
        Dec(FPosition);
        FLastEnd := FTokens[FPosition].Span.StartOffset;
        ParseAnnotation;
      end
      else
        ParseReturnType;
    tsType:
      ParseType(True);
    tsTypeParameters:
      if TypeAt(0) = gttLess then
        ParseTypeParameters;
    tsTypeArguments:
      if TypeAt(0) = gttLess then
        ParseTypeArguments;
    tsImplements:
      ParseHeritageList(apkClassImplements);
    tsTypeAlias:
      begin
        SkipToWord(KEYWORD_TYPE);
        if TypeAt(0) = gttLess then
          ParseTypeParameters;
        if TypeAt(0) = gttAssign then
        begin
          Advance;
          ParseType(True);
        end;
      end;
    tsInterface:
      begin
        SkipToWord(KEYWORD_INTERFACE);
        if TypeAt(0) = gttLess then
          ParseTypeParameters;
        if TypeAt(0) = gttExtends then
        begin
          Advance;
          ParseHeritageList(apkInterfaceHeritage);
        end;
        if TypeAt(0) = gttLeftBrace then
          ParseObjectType(apkInterfaceBody);
      end;
  end;
  Result := Copy(FRanges, 0, FCount);
end;

function ReadTypeRanges(const ATokens: TObjectList<TGocciaToken>;
  const ASyntax: TGocciaTypeSyntax;
  const AStartIndex, AEndIndex: Integer): TGocciaTypeRangeArray;
var
  Reader: TTypeReader;
begin
  Result := nil;
  if not Assigned(ATokens) or (AStartIndex < 0) or (AEndIndex <= AStartIndex) then
    Exit;
  Reader := TTypeReader.Create(ATokens, AStartIndex, AEndIndex);
  try
    Result := Reader.Read(ASyntax);
  finally
    Reader.Free;
  end;
end;

end.
