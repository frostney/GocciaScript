unit Goccia.Builtins.AST;

{$I Goccia.inc}

{ The `goccia:ast` namespace: one `parse` function that turns source text into
  a JavaScript-visible tree.

  What is exposed and why is [ADR 0117](../../docs/adr/0117-javascript-visible-ast-module.md)
  for the statement view and [ADR 0162](../../docs/adr/0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)
  for the expression view `expressions: true` adds. By default the tree holds
  statements, the three kinds that own a statement list (`Program`,
  `BlockStatement`, `SwitchCase`), their offsets and positions, and comment
  trivia. It never holds names or literal values — a rule that needs those
  has the offsets and the source text the result carries.

  The tree is walked only through TGocciaASTNode.ForEachChild. Expressions are
  always walked, because that is the only way to reach the block of an arrow
  function assigned to a `const`; without the option they are walked without
  being emitted.

  With `tokens: true` the result also carries the committed token stream,
  comments included, as typed ranges; see
  [ADR 0163](../../docs/adr/0163-goccia-ast-reports-the-committed-token-stream.md).
  A type-level declaration is reported by the kind it was written as, and
  `typeOnly` marks the module statements that carry no code; see
  [ADR 0161](../../docs/adr/0161-goccia-ast-reports-type-level-declarations-by-kind.md).
  The module forms are the exception to ADR 0161's "no children": they report
  their specifiers and module path, see
  [ADR 0168](../../docs/adr/0168-goccia-ast-reports-the-children-of-type-only-module-statements.md).
  With `expressions: true` the parser also records erased type syntax, and
  every type is reported as nodes beneath what it annotates, type
  declarations included; see
  [ADR 0172](../../docs/adr/0172-goccia-ast-reports-erased-type-syntax-recorded-on-request.md). }

interface

uses
  Goccia.Values.ObjectValue;

function CreateASTNamespace: TGocciaObjectValue;

implementation

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Report,
  Goccia.AST.Statements,
  Goccia.Constants.ErrorNames,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.NestingLimit,
  Goccia.ObjectModel,
  Goccia.OriginMap,
  Goccia.SourcePipeline,
  Goccia.SourceSpan,
  Goccia.ThreadCleanupRegistry,
  Goccia.Token,
  Goccia.Utils.Arrays,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

const
  DEFAULT_FILE_NAME = '<source>';

{ Materializing }

const
  { What an assignment to a fresh key of an ordinary object creates. The
    result's objects are made here and their keys are distinct, so each is
    defined directly, without the lookups an assignment makes first. }
  RESULT_PROPERTY_FLAGS = [pfEnumerable, pfConfigurable, pfWritable];
  NUMBER_CACHE_LENGTH = 1 shl 18;
  POSITION_PROPERTY_COUNT = 2;
  LOC_PROPERTY_COUNT = 2;
  { children, kind, start, end, loc. }
  NODE_BASE_PROPERTY_COUNT = 5;
  { kind, start, end, loc. }
  COMMENT_PROPERTY_COUNT = 4;

type
  { The numbers one result holds. Every offset, line and column in it lies
    between 0 and one past the source's length, and a file repeats them
    many times over — a node starts where its first child and first token
    do — so each is made once and shared: a number is a primitive, and which
    instance holds it is not observable. Making an object registers it but
    never collects, and a cached value is attached to the object being built
    before anything that can collect runs, so every value the cache holds is
    reachable from the result while the cache lives. The table is capped, so
    past NUMBER_CACHE_LENGTH a large source gets a fresh value per use rather
    than a pointer per character that the memory budget does not count. }
  TASTNumberValues = class
  private
    FValues: array of TGocciaNumberLiteralValue;
  public
    constructor Create(const ASourceLength: Integer);
    function ValueOf(const AValue: Integer): TGocciaNumberLiteralValue;
  end;

constructor TASTNumberValues.Create(const ASourceLength: Integer);
begin
  inherited Create;
  if ASourceLength + 2 < NUMBER_CACHE_LENGTH then
    SetLength(FValues, ASourceLength + 2)
  else
    SetLength(FValues, NUMBER_CACHE_LENGTH);
end;

function TASTNumberValues.ValueOf(
  const AValue: Integer): TGocciaNumberLiteralValue;
begin
  if (AValue < 0) or (AValue > High(FValues)) then
    Exit(TGocciaNumberLiteralValue.Create(AValue));
  Result := FValues[AValue];
  if Result = nil then
  begin
    Result := TGocciaNumberLiteralValue.Create(AValue);
    FValues[AValue] := Result;
  end;
end;

function CreatePositionObject(const ANumbers: TASTNumberValues;
  const ALine, AColumn: Integer): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(nil, POSITION_PROPERTY_COUNT);
  Result.DefineFreshDataProperty('line', ANumbers.ValueOf(ALine),
    RESULT_PROPERTY_FLAGS);
  Result.DefineFreshDataProperty('column', ANumbers.ValueOf(AColumn),
    RESULT_PROPERTY_FLAGS);
end;

{ Making an object registers it but never collects, so the loc object and
  its positions need no rooting while they are made; the caller attaches the
  loc object before anything that can collect runs. }
function CreateLocObject(const ANumbers: TASTNumberValues;
  const AStartLine, AStartColumn, AEndLine,
  AEndColumn: Integer): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(nil, LOC_PROPERTY_COUNT);
  Result.DefineFreshDataProperty('start',
    CreatePositionObject(ANumbers, AStartLine, AStartColumn),
    RESULT_PROPERTY_FLAGS);
  Result.DefineFreshDataProperty('end',
    CreatePositionObject(ANumbers, AEndLine, AEndColumn),
    RESULT_PROPERTY_FLAGS);
end;

const
  FLAG_NAMES: array[TASTNodeFlag] of string = ('computed', 'optional',
    'async', 'generator', 'delegate', 'prefix', 'shorthand', 'method',
    'rest', 'static');

{ Every array in the result is filled in index order as it is made, so an
  element goes straight onto the end of its dense elements, where
  ArrayCreateDataProperty would spell the index as a property key and parse
  it back. }
procedure AppendElement(const AArray: TGocciaArrayValue; const AIndex: Integer;
  const AValue: TGocciaValue);
begin
  if not AArray.TryAppendDenseElementFast(AIndex, AValue) then
    ArrayCreateDataProperty(AArray, AIndex, AValue);
end;

{ Nodes are materialized parent-first. A node is held only by this call (its
  parent attaches it on return) while its kind string and its children are
  made, and each of those can collect, so it is rooted until it returns; every
  object it has attached by then is reachable from it. }
function MaterializeNode(const AFlattener: TASTFlattener;
  const ACoordinates: IGocciaSourceCoordinates;
  const ANumbers: TASTNumberValues;
  const AIndex: Integer): TGocciaObjectValue;
var
  Children: TGocciaArrayValue;
  Node: PASTFlatNode;
  ChildIndex, Position, PropertyCount: Integer;
  StartLine, StartColumn, EndLine, EndColumn: Integer;
  Flag: TASTNodeFlag;
  Roots: TGocciaActiveRootFrame;
begin
  Node := AFlattener.NodeAt(AIndex);
  { The line and column are read off the caller's text at the mapped
    offsets — deriving them rather than translating them separately is what
    keeps `loc` and the range the same answer. }
  ACoordinates.PositionAtOffset(Node^.StartOffset, StartLine, StartColumn);
  ACoordinates.PositionAtOffset(Node^.EndOffset, EndLine, EndColumn);
  Roots.Initialize;
  try
    PropertyCount := NODE_BASE_PROPERTY_COUNT + Ord(Node^.OperatorText <> '') +
      Ord(Node^.BodyKind <> '') + Ord(Node^.HasTypeOnly);
    for Flag := Low(TASTNodeFlag) to High(TASTNodeFlag) do
      if Flag in Node^.FlagKinds then
        Inc(PropertyCount);
    Result := TGocciaObjectValue.Create(nil, PropertyCount);
    Roots.Add(Result);
    Children := TGocciaArrayValue.Create;
    Result.DefineFreshDataProperty('children', Children, RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('kind',
      TGocciaStringLiteralValue.Create(Node^.Kind), RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('start',
      ANumbers.ValueOf(Node^.StartOffset), RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('end',
      ANumbers.ValueOf(Node^.EndOffset), RESULT_PROPERTY_FLAGS);
    Result.DefineFreshDataProperty('loc', CreateLocObject(ANumbers, StartLine,
      StartColumn, EndLine, EndColumn), RESULT_PROPERTY_FLAGS);
    if Node^.OperatorText <> '' then
      Result.DefineFreshDataProperty('operator',
        TGocciaStringLiteralValue.Create(Node^.OperatorText),
        RESULT_PROPERTY_FLAGS);
    if Node^.BodyKind <> '' then
      Result.DefineFreshDataProperty('bodyKind',
        TGocciaStringLiteralValue.Create(Node^.BodyKind), RESULT_PROPERTY_FLAGS);
    for Flag := Low(TASTNodeFlag) to High(TASTNodeFlag) do
      if Flag in Node^.FlagKinds then
        if Flag in Node^.Flags then
          Result.DefineFreshDataProperty(FLAG_NAMES[Flag],
            TGocciaBooleanLiteralValue.TrueValue, RESULT_PROPERTY_FLAGS)
        else
          Result.DefineFreshDataProperty(FLAG_NAMES[Flag],
            TGocciaBooleanLiteralValue.FalseValue, RESULT_PROPERTY_FLAGS);
    if Node^.HasTypeOnly then
      if Node^.TypeOnly then
        Result.DefineFreshDataProperty('typeOnly',
          TGocciaBooleanLiteralValue.TrueValue, RESULT_PROPERTY_FLAGS)
      else
        Result.DefineFreshDataProperty('typeOnly',
          TGocciaBooleanLiteralValue.FalseValue, RESULT_PROPERTY_FLAGS);

    Position := 0;
    ChildIndex := Node^.FirstChild;
    while ChildIndex >= 0 do
    begin
      AppendElement(Children, Position,
        MaterializeNode(AFlattener, ACoordinates, ANumbers, ChildIndex));
      Inc(Position);
      ChildIndex := AFlattener.NodeAt(ChildIndex)^.NextSibling;
    end;
  finally
    Roots.Clear;
  end;
end;

{ Tokens }

{ One object per token, attached to the array before its properties are made
  so each is rooted through the result while they allocate. The type strings
  are immutable and shared: a file has tens of thousands of tokens and
  sixteen types. }
procedure MaterializeTokens(const ATarget: TGocciaArrayValue;
  const ATokens: TASTFlatTokenArray; const ANumbers: TASTNumberValues);
var
  TypeValues: array[TASTTokenType] of TGocciaStringLiteralValue;
  TokenType: TASTTokenType;
  TokenObject: TGocciaObjectValue;
  I: Integer;
begin
  for TokenType := Low(TASTTokenType) to High(TASTTokenType) do
    TypeValues[TokenType] := nil;

  for I := 0 to High(ATokens) do
  begin
    TokenObject := TGocciaObjectValue.Create(
      TGocciaObjectValue.SharedObjectPrototype, TOKEN_PROPERTY_COUNT);
    AppendElement(ATarget, I, TokenObject);
    TokenType := ATokens[I].TokenType;
    if TypeValues[TokenType] = nil then
      TypeValues[TokenType] :=
        TGocciaStringLiteralValue.Create(TOKEN_TYPE_NAMES[TokenType]);
    // The object was made just above, so each key is a fresh data property
    // and needs none of the checks an assignment makes.
    TokenObject.DefineFreshDataProperty(PROP_TOKEN_TYPE, TypeValues[TokenType],
      RESULT_PROPERTY_FLAGS);
    TokenObject.DefineFreshDataProperty(PROP_TOKEN_START,
      ANumbers.ValueOf(ATokens[I].StartOffset),
      RESULT_PROPERTY_FLAGS);
    TokenObject.DefineFreshDataProperty(PROP_TOKEN_END,
      ANumbers.ValueOf(ATokens[I].EndOffset),
      RESULT_PROPERTY_FLAGS);
  end;
end;

type
  TGocciaASTNamespaceHost = class
  published
    function Parse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

{ The host carries no state, so every namespace on a thread shares one. It
  lives until thread teardown rather than being released with a namespace: a
  `parse` function the guest kept is bound to it and may still be called.
  Per thread because worker threads materialize the module concurrently. }
threadvar
  GASTHost: TGocciaASTNamespaceHost;

function ReadBooleanOption(const AOptions: TGocciaValue;
  const AName: string; const ADefault: Boolean): Boolean;
var
  Value: TGocciaValue;
begin
  Result := ADefault;
  if not (AOptions is TGocciaObjectValue) then
    Exit;
  Value := TGocciaObjectValue(AOptions).GetProperty(AName);
  if (Value = nil) or (Value is TGocciaUndefinedLiteralValue) then
    Exit;
  Result := Value.ToBooleanLiteral.Value;
end;

function ReadStringOption(const AOptions: TGocciaValue;
  const AName, ADefault: string): string;
var
  Value: TGocciaValue;
begin
  Result := ADefault;
  if not (AOptions is TGocciaObjectValue) then
    Exit;
  Value := TGocciaObjectValue(AOptions).GetProperty(AName);
  if not (Value is TGocciaStringLiteralValue) then
    Exit;
  Result := Value.ToStringLiteral.Value;
end;

function CommentKindName(const AKind: TGocciaCommentKind): string;
begin
  if AKind = gckLine then
    Result := 'Line'
  else
    Result := 'Block';
end;

function TGocciaASTNamespaceHost.Parse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  SourceText: string;
  SourceLines: TStringList;
  Options: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  Flattener: TASTFlattener;
  ResultObject: TGocciaObjectValue;
  CommentObject: TGocciaObjectValue;
  Comments: TGocciaArrayValue;
  CommentSpans: TGocciaCommentSpanArray;
  Coordinates: IGocciaSourceCoordinates;
  StartOffset, EndOffset: Integer;
  StartLine, StartColumn, EndLine, EndColumn: Integer;
  ResultRoot: TGocciaTempRoot;
  I, CommentIndex: Integer;
  WantTokens, WantExpressions: Boolean;
  TokenCollector: TASTTokenCollector;
  FlatTokens: TASTFlatTokenArray;
  Tokens: TGocciaArrayValue;
  Numbers: TASTNumberValues;
begin
  { Normalized up front so the text handed back is the text the offsets index:
    the pipeline normalizes line terminators before it lexes, and a CRLF file
    reported with its CRs would put every offset after the first line one byte
    out for anything slicing `source`. }
  if AArgs.GetElement(0) is TGocciaStringLiteralValue then
    SourceText := NormalizeNewlinesToLF(AArgs.GetElement(0).ToStringLiteral.Value)
  else
    ThrowTypeError('parse expects the source text as a string',
      'Pass the file contents as a string');

  { The language profile is the host's, not a second one configured here:
    `parse` should accept exactly the JavaScript this engine runs, and the
    compatibility flags that decides are already on the active scope. `eval`
    and the Function constructor inherit them the same way. The options
    argument therefore only says what is about the *text*, not about the
    language. }
  Options := TGocciaSourcePipeline.CurrentOptionsOrDefault;
  Options.CollectComments := True;
  Options.WarningUnsupportedFeatures := False;
  Options.InheritedStrictMode := False;
  if ReadBooleanOption(AArgs.GetElement(1), 'jsx', False) then
    Options.Preprocessors := Options.Preprocessors + [ppJSX]
  else
    Options.Preprocessors := Options.Preprocessors - [ppJSX];
  if ReadBooleanOption(AArgs.GetElement(1), 'module', True) then
    Options.SourceType := stModule
  else
    Options.SourceType := stScript;
  { Opt-in, so a caller that wants only the tree pays nothing for tokens:
    the pipeline copies none and the JSX transformer records none. }
  WantTokens := ReadBooleanOption(AArgs.GetElement(1), OPTION_TOKENS, False);
  Options.CollectTokens := WantTokens;
  { Erased type syntax is reported only in the expression view, so only it
    asks the parser to record where it was. }
  WantExpressions := ReadBooleanOption(AArgs.GetElement(1), 'expressions',
    False);
  Options.RecordTypeRanges := WantExpressions;

  { CreateTextLines keeps the exact text alongside the split lines, so the
    string the pipeline parses is character for character the one returned as
    `source` — a plain TStringList round trip loses a trailing newline, and
    every offset would then be into a string the caller never sees. }
  SourceLines := CreateTextLines(SourceText);
  try
    try
      // The file name decides whether the JSX preprocessor warns about the
      // extension, and appears in a syntax error's location.
      PipelineResult := TGocciaSourcePipeline.Parse(SourceLines,
        ReadStringOption(AArgs.GetElement(1), 'fileName', DEFAULT_FILE_NAME),
        Options);
    except
      on E: TGocciaError do
        ThrowSyntaxError(Format('%s (%d:%d)', [E.Message, E.Line, E.Column]));
    end;
  finally
    SourceLines.Free;
  end;

  Numbers := nil;
  InitializeTempRoot(ResultRoot);
  try
    Numbers := TASTNumberValues.Create(Length(SourceText));
    ResultObject := TGocciaObjectValue.Create;
    AddTempRootIfNeeded(ResultRoot, ResultObject);

    { The caller's own text, which is also the one every offset indexes. A
      preprocessor may have rewritten it on the way to the parser, but nothing
      the result carries is measured in that rewrite. }
    ResultObject.AssignProperty('source',
      TGocciaStringLiteralValue.Create(SourceText));

    Comments := TGocciaArrayValue.Create;
    ResultObject.AssignProperty('comments', Comments);

    Coordinates := TGocciaSourceCoordinates.Create(SourceText);

    Flattener := TASTFlattener.Create(PipelineResult.OriginMap,
      WantExpressions);
    try
      Flattener.Run(PipelineResult.ProgramNode);
      ResultObject.AssignProperty('root',
        MaterializeNode(Flattener, Coordinates, Numbers, 0));
    finally
      Flattener.Free;
    end;

    CommentSpans := PipelineResult.Comments;
    CommentIndex := 0;
    for I := 0 to High(CommentSpans) do
    begin
      // A hashbang is reported as a token only; `comments` keeps the two
      // kinds it has always had.
      if CommentSpans[I].Kind = gckHashbang then
        Continue;
      CommentObject := TGocciaObjectValue.Create(nil, COMMENT_PROPERTY_COUNT);
      AppendElement(Comments, CommentIndex, CommentObject);
      Inc(CommentIndex);
      StartOffset := MappedOffset(PipelineResult.OriginMap,
        CommentSpans[I].StartOffset, obStart);
      EndOffset := MappedOffset(PipelineResult.OriginMap,
        CommentSpans[I].EndOffset, obEnd);
      Coordinates.PositionAtOffset(StartOffset, StartLine, StartColumn);
      Coordinates.PositionAtOffset(EndOffset, EndLine, EndColumn);
      CommentObject.DefineFreshDataProperty('kind',
        TGocciaStringLiteralValue.Create(CommentKindName(CommentSpans[I].Kind)),
        RESULT_PROPERTY_FLAGS);
      CommentObject.DefineFreshDataProperty('start',
        Numbers.ValueOf(StartOffset), RESULT_PROPERTY_FLAGS);
      CommentObject.DefineFreshDataProperty('end',
        Numbers.ValueOf(EndOffset), RESULT_PROPERTY_FLAGS);
      CommentObject.DefineFreshDataProperty('loc',
        CreateLocObject(Numbers, StartLine, StartColumn, EndLine, EndColumn),
        RESULT_PROPERTY_FLAGS);
    end;

    if WantTokens then
    begin
      TokenCollector := TASTTokenCollector.Create;
      try
        FlatTokens := TokenCollector.Collect(PipelineResult);
      finally
        TokenCollector.Free;
      end;
      Tokens := TGocciaArrayValue.Create;
      ResultObject.AssignProperty(PROP_TOKENS, Tokens);
      MaterializeTokens(Tokens, FlatTokens, Numbers);
    end;

    Result := ResultObject;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
    Numbers.Free;
    PipelineResult.Free;
  end;
end;

function CreateASTNamespace: TGocciaObjectValue;
var
  Members: TGocciaMemberCollection;
begin
  if not Assigned(GASTHost) then
    GASTHost := TGocciaASTNamespaceHost.Create;

  Result := TGocciaObjectValue.Create;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('parse', GASTHost.Parse, 2, gmkStaticMethod);
    RegisterMemberDefinitions(Result, Members.ToDefinitions);
  finally
    Members.Free;
  end;
end;

procedure ClearASTHost;
begin
  FreeAndNil(GASTHost);
end;

initialization
  RegisterThreadvarCleanup(@ClearASTHost);

end.
