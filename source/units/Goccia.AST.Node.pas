unit Goccia.AST.Node;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.SourceSpan;

type
  { ADR 0134 static detection. What the parser found in a function's own code,
    its parameter list included and the bodies of nested functions excluded:
    a call whose callee is the plain identifier `eval` (ES2026 §13.3.6.1 makes
    it a direct eval when the callee is %eval% at run time), and whether one
    such call is in non-strict code, so that a var it declares may land in the
    function's variable environment (§19.2.1.3 EvalDeclarationInstantiation). }
  TGocciaDirectEvalUsageFlag = (deuDirectEval, deuSloppyDirectEval);
  TGocciaDirectEvalUsage = set of TGocciaDirectEvalUsageFlag;

  TGocciaASTArena = class;

  { Every object a parse allocates for its tree: the nodes, and the four
    helper classes that are not nodes (TGocciaClassDefinition,
    TGocciaDestructuringProperty, TGocciaObjectMatchProperty,
    TGocciaMatchClause).

    Ownership rule (ADR 0143): an AST object created while a parse runs belongs
    to that parse's arena, and only the arena frees it. The arena is handed to
    the TGocciaProgram the parse returns, so freeing the program frees the
    whole tree, and a parse that raises frees what it built. A node therefore
    never frees another AST object, and a container a node holds (a list, a
    map) never owns its elements: the node frees the container alone.
    Sharing a node between two parents, or reusing it in a rewritten tree, is
    safe for the same reason.

    An object freed explicitly before its arena (a node the parser discards)
    leaves the arena first, so the arena does not free it twice. An object
    created with no parse running (a compiler temporary, a unit test's
    hand-built node) has no arena; whoever creates it frees it. }
  TGocciaASTObject = class
  private
    FArena: TGocciaASTArena;
    { Its slot in FArena, so leaving the arena costs O(1). }
    FArenaIndex: Integer;
  protected
    { False for an object that owns its arena instead of belonging to one:
      TGocciaProgram. }
    class function JoinsArena: Boolean; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { Owns the AST objects of one parse. Freeing it frees every object still in
    it, in no particular order: no AST object's destructor touches another. }
  TGocciaASTArena = class
  private
    FObjects: TFPList;
    procedure Adopt(const AObject: TGocciaASTObject);
    procedure Forget(const AObject: TGocciaASTObject);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
  end;

  TGocciaASTNode = class;

  { A piece of syntax that has a range but no node of its own: the parser
    keeps an object literal's members in maps, a function's parameters in
    records, and an import's module path as a string, so none of them is a
    TGocciaASTNode. ForEachChild reports one between EnterPart and
    LeavePart, with the nodes inside it in between. }
  TGocciaASTPartKind = (
    apkProperty,        // A member of an object literal
    apkParameter,       // A function's parameter
    apkModuleSpecifier, // The module path of an import or re-export
    apkClassMethod,     // A class's method, getter or setter
    apkClassField,      // A class's field
    apkClassAccessor,   // A class's `accessor` field
    apkStaticBlock      // A class's `static { }` block
  );

  TGocciaASTPartFlag = (
    apfComputed,  // Property, class member: `[key]`
    apfShorthand, // Property: `{ key }`
    apfMethod,    // Property: `key() {}`, `get key() {}`, `set key(v) {}`
    apfRest,      // Parameter: `...rest`
    apfStatic     // Class member: `static`
  );
  TGocciaASTPartFlags = set of TGocciaASTPartFlag;

  { Receives a node's children from TGocciaASTNode.ForEachChild. }
  TGocciaASTChildVisitor = class
  public
    { ANode may be nil; a visitor ignores it. }
    procedure VisitChild(const ANode: TGocciaASTNode); virtual; abstract;
    procedure EnterPart(const AKind: TGocciaASTPartKind;
      const AStartOffset, AEndOffset: Integer;
      const AFlags: TGocciaASTPartFlags); virtual; abstract;
    procedure LeavePart; virtual; abstract;
  end;

  TGocciaASTNode = class(TGocciaASTObject)
  private
    FSpan: TGocciaSourceSpan;
    function GetLine: Integer;
    function GetColumn: Integer;
  public
    constructor Create(const ASpan: TGocciaSourceSpan);
    { Reports the nodes nested immediately beneath this one, in source order,
      to AVisitor (ADR 0162). The one walk over the tree's syntax that a tool
      outside the evaluator and the compiler shares, so that a new node class
      describes its own children here rather than in every such tool. A node
      with no children keeps the default, which reports none. }
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); virtual;
    property Span: TGocciaSourceSpan read FSpan;
    property Line: Integer read GetLine;
    property Column: Integer read GetColumn;
  end;

  { A TypeScript assertion erased from the tree: `x as T` (and `as const`),
    `x satisfies T`, `x!`. }
  TGocciaTypeAssertionKind = (takAs, takSatisfies, takNonNull);

  { Where an erased assertion's text is, as offsets into the parsed text:
    the `as`, `satisfies` or `!` and the type after it, or from the opening
    parenthesis when the expression was parenthesized before it. }
  TGocciaTypeAssertion = record
    Kind: TGocciaTypeAssertionKind;
    StartOffset: Integer;
    EndOffset: Integer;
  end;

  TGocciaExpression = class(TGocciaASTNode)
  private
    FParenthesized: Boolean;
    FParenStartOffset: Integer;
    FParenEndOffset: Integer;
    FTypeAssertions: array of TGocciaTypeAssertion;
    function GetTypeAssertionCount: Integer;
    function GetTypeAssertion(const AIndex: Integer): TGocciaTypeAssertion;
  public
    { Records an assertion applied to this expression. The parser leaves the
      expression itself in the tree and drops the assertion, so this is the
      only place its extent survives. Recorded in source order, so each one
      ends after the one before it. }
    procedure AddTypeAssertion(const AKind: TGocciaTypeAssertionKind;
      const AStartOffset, AEndOffset: Integer);
    { Records the outermost pair of parentheses around this expression, as
      offsets into the parsed text. The parser drops the parentheses from the
      tree, so this is the only place their extent survives. }
    procedure MarkParenthesized(const AStartOffset, AEndOffset: Integer);
    { True when MarkParenthesized recorded where the parentheses are. An
      expression can be Parenthesized without it: the parser also marks
      synthesized wrappers. }
    function HasParenRange: Boolean;
    property Parenthesized: Boolean read FParenthesized write FParenthesized;
    property ParenStartOffset: Integer read FParenStartOffset;
    property ParenEndOffset: Integer read FParenEndOffset;
    property TypeAssertionCount: Integer read GetTypeAssertionCount;
    property TypeAssertions[const AIndex: Integer]: TGocciaTypeAssertion
      read GetTypeAssertion;
  end;

  TGocciaStatement = class(TGocciaASTNode)
  private
    FLabels: TStringList;
    function GetLabelCount: Integer;
    function GetLabel(const AIndex: Integer): string;
  public
    destructor Destroy; override;
    procedure AddLabel(const AName: string);
    property LabelCount: Integer read GetLabelCount;
    property Labels[const AIndex: Integer]: string read GetLabel;
  end;

  // Program node. It owns the arena of the parse that produced it, so freeing
  // the program frees the whole tree (see TGocciaASTObject).
  TGocciaProgram = class(TGocciaASTNode)
  private
    FBody: TObjectList<TGocciaStatement>;
    FHasTopLevelAwait: Boolean;
    FDirectEvalUsage: TGocciaDirectEvalUsage;
    FNodeArena: TGocciaASTArena;
  protected
    class function JoinsArena: Boolean; override;
  public
    { ABody must not own its statements; the program frees the list. }
    constructor Create(const ABody: TObjectList<TGocciaStatement>);
    destructor Destroy; override;
    { Takes ownership of the arena holding this program's nodes. }
    procedure AdoptArena(const AArena: TGocciaASTArena);
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    property Body: TObjectList<TGocciaStatement> read FBody;
    property HasTopLevelAwait: Boolean read FHasTopLevelAwait write FHasTopLevelAwait;
    property DirectEvalUsage: TGocciaDirectEvalUsage read FDirectEvalUsage
      write FDirectEvalUsage;
  end;

{ The arena AST objects created on this thread join, or nil. }
function ActiveASTArena: TGocciaASTArena;
{ Makes AArena the active arena and returns the one it replaces, which the
  caller restores with RestoreASTArena when its parse ends. }
function ActivateASTArena(const AArena: TGocciaASTArena): TGocciaASTArena;
procedure RestoreASTArena(const APrevious: TGocciaASTArena);

// Conservative pre-scan the bytecode compiler uses to decide whether a
// non-arrow function must materialize an implicit arguments object (only
// relevant under --compat-arguments-object). The arguments object
// is observable only through the identifier `arguments` or through direct
// `eval`, and a function's source text spans its whole body — including any
// nested arrow functions, which share the enclosing `arguments`. A body that
// references neither can skip the costly allocation.
//
// The scan must account for Unicode-escaped identifiers: the lexer decodes
// escape sequences inside identifiers, so an escaped spelling of `arguments` or
// `eval` references the object even though the decoded word never appears
// literally in the raw source. Every IdentifierName escape is a \uXXXX /
// \u{...} sequence, so the presence of \u anywhere forces creation. All three
// substring checks
// over-approximate (a comment, string literal, regex, or longer identifier that
// merely contains "arguments"/"eval"/"\u" forces creation), which is always
// safe: it may keep a needless object but never drops a needed one. Empty
// source (synthetic functions) is treated conservatively as "may reference".
function FunctionSourceMayReferenceArgumentsObject(
  const ASourceText: string): Boolean;

implementation

threadvar
  GActiveASTArena: TGocciaASTArena;

function ActiveASTArena: TGocciaASTArena;
begin
  Result := GActiveASTArena;
end;

function ActivateASTArena(const AArena: TGocciaASTArena): TGocciaASTArena;
begin
  Result := GActiveASTArena;
  GActiveASTArena := AArena;
end;

procedure RestoreASTArena(const APrevious: TGocciaASTArena);
begin
  GActiveASTArena := APrevious;
end;

{ TGocciaASTObject }

class function TGocciaASTObject.JoinsArena: Boolean;
begin
  Result := True;
end;

procedure TGocciaASTObject.AfterConstruction;
begin
  inherited;
  if Assigned(GActiveASTArena) and JoinsArena then
    GActiveASTArena.Adopt(Self);
end;

procedure TGocciaASTObject.BeforeDestruction;
begin
  if Assigned(FArena) then
    FArena.Forget(Self);
  inherited;
end;

{ TGocciaASTArena }

constructor TGocciaASTArena.Create;
begin
  inherited Create;
  FObjects := TFPList.Create;
end;

destructor TGocciaASTArena.Destroy;
var
  I: Integer;
  AObject: TGocciaASTObject;
begin
  for I := 0 to FObjects.Count - 1 do
  begin
    AObject := TGocciaASTObject(FObjects[I]);
    if not Assigned(AObject) then
      Continue;
    AObject.FArena := nil;
    AObject.Free;
  end;
  FObjects.Free;
  inherited;
end;

procedure TGocciaASTArena.Adopt(const AObject: TGocciaASTObject);
begin
  AObject.FArena := Self;
  AObject.FArenaIndex := FObjects.Add(AObject);
end;

procedure TGocciaASTArena.Forget(const AObject: TGocciaASTObject);
var
  Index: Integer;
begin
  { By slot, not by search: a syntax error inside a long list frees each
    element with the list, and a search from the end would scan past every
    node built after it, quadratic in the size of the parse. }
  Index := AObject.FArenaIndex;
  if (Index < 0) or (Index >= FObjects.Count) or
     (FObjects[Index] <> Pointer(AObject)) then
    Exit;
  if Index = FObjects.Count - 1 then
    FObjects.Count := Index
  else
    FObjects[Index] := nil;
  AObject.FArena := nil;
end;

function TGocciaASTArena.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function FunctionSourceMayReferenceArgumentsObject(
  const ASourceText: string): Boolean;
begin
  Result := (ASourceText = '') or
            (Pos('arguments', ASourceText) > 0) or
            (Pos('eval', ASourceText) > 0) or
            (Pos('\u', ASourceText) > 0);
end;

constructor TGocciaASTNode.Create(const ASpan: TGocciaSourceSpan);
begin
  FSpan := ASpan;
end;

procedure TGocciaASTNode.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
end;

{ TGocciaExpression }

procedure TGocciaExpression.MarkParenthesized(const AStartOffset,
  AEndOffset: Integer);
begin
  FParenthesized := True;
  { `((x))` marks the inner pair first; the outer pair, seen last, wins. }
  FParenStartOffset := AStartOffset;
  FParenEndOffset := AEndOffset;
end;

function TGocciaExpression.HasParenRange: Boolean;
begin
  Result := FParenEndOffset > FParenStartOffset;
end;

procedure TGocciaExpression.AddTypeAssertion(
  const AKind: TGocciaTypeAssertionKind; const AStartOffset,
  AEndOffset: Integer);
var
  Index: Integer;
begin
  Index := Length(FTypeAssertions);
  SetLength(FTypeAssertions, Index + 1);
  FTypeAssertions[Index].Kind := AKind;
  FTypeAssertions[Index].StartOffset := AStartOffset;
  FTypeAssertions[Index].EndOffset := AEndOffset;
end;

function TGocciaExpression.GetTypeAssertionCount: Integer;
begin
  Result := Length(FTypeAssertions);
end;

function TGocciaExpression.GetTypeAssertion(
  const AIndex: Integer): TGocciaTypeAssertion;
begin
  Result := FTypeAssertions[AIndex];
end;

function TGocciaASTNode.GetLine: Integer;
begin
  Result := FSpan.StartLine;
end;

function TGocciaASTNode.GetColumn: Integer;
begin
  Result := FSpan.StartColumn;
end;

{ TGocciaStatement }

destructor TGocciaStatement.Destroy;
begin
  FLabels.Free;
  inherited;
end;

function TGocciaStatement.GetLabelCount: Integer;
begin
  if Assigned(FLabels) then
    Result := FLabels.Count
  else
    Result := 0;
end;

function TGocciaStatement.GetLabel(const AIndex: Integer): string;
begin
  Result := FLabels[AIndex];
end;

procedure TGocciaStatement.AddLabel(const AName: string);
begin
  if not Assigned(FLabels) then
  begin
    FLabels := TStringList.Create;
    FLabels.CaseSensitive := True;
  end;
  FLabels.Add(AName);
end;

{ TGocciaProgram }

constructor TGocciaProgram.Create(const ABody: TObjectList<TGocciaStatement>);
begin
  if ABody.Count > 0 then
    inherited Create(ABody[0].Span.Cover(ABody[ABody.Count - 1].Span))
  else
    inherited Create(TGocciaSourceSpan.Empty);
  FBody := ABody;
end;

destructor TGocciaProgram.Destroy;
begin
  if Assigned(FBody) then
    FBody.OwnsObjects := False;
  FBody.Free;
  FNodeArena.Free;
  inherited;
end;

class function TGocciaProgram.JoinsArena: Boolean;
begin
  Result := False;
end;

procedure TGocciaProgram.AdoptArena(const AArena: TGocciaASTArena);
begin
  FNodeArena := AArena;
end;

procedure TGocciaProgram.ForEachChild(const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  for I := 0 to FBody.Count - 1 do
    AVisitor.VisitChild(FBody[I]);
end;

end.
