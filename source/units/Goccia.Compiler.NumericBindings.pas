unit Goccia.Compiler.NumericBindings;

{$I Goccia.inc}

// Finds the let and const bindings of one function body that hold a Number
// whenever their value can be read.
//
// The compiler emits a typed arithmetic instruction for a binding whose type
// hint says Number, and those instructions do not check their operands. A hint
// read from the code compiled so far is not enough for a binding that can be
// assigned: a later assignment reaches an earlier read through the back edge of
// a loop, and an assignment in one branch of an if, a switch or a conditional
// expression reaches a read after the join. So a binding that can be assigned
// keeps a Number hint only when every value it can be given is a Number, and
// that is decided here, for the whole body, before the body is compiled.
//
// A binding qualifies when its initializer and every assignment to it in the
// body produce a Number, given that the bindings it reads from qualify too. The
// answer is the largest set of bindings for which that holds, found by starting
// from every candidate and dropping the ones with an assignment that might not
// produce a Number until nothing changes.
//
// Names are resolved with the lexical scoping of the body. Assignments inside a
// nested function or class body are attributed to the binding visible where
// that function or class appears, without regard to bindings it declares
// itself, and count as producing a value of unknown type. A construct the scan
// does not list (with, direct eval, pattern matching, a node class added to the
// AST later) proves nothing for the whole body.

interface

uses
  Goccia.AST.Node,
  Goccia.Compiler.Context,
  Goccia.Compiler.Scope;

// Adds to AProofs, for each TGocciaVariableDeclaration in ABody, the bit
// (1 shl I) of each of its variables I that holds only Numbers. AScope is the
// scope the body is compiled in, with the parameters already declared; it
// answers for names the body does not declare.
procedure DiscoverNumberBindings(const ABody: TGocciaASTNode;
  const AScope: TGocciaCompilerScope;
  const AAllowBlockFunctionVarBindings: Boolean;
  const AProofs: TNumberBindingProofMap);

procedure DiscoverProgramNumberBindings(const AProgram: TGocciaProgram;
  const AScope: TGocciaCompilerScope;
  const AAllowBlockFunctionVarBindings: Boolean;
  const AProofs: TNumberBindingProofMap);

implementation

uses
  Generics.Collections,

  HashMap,
  OrderedStringMap,

  Goccia.AST.Expressions,
  Goccia.AST.Statements,
  Goccia.Compiler.TypeRules,
  Goccia.Token,
  Goccia.Values.Primitives;

const
  // FReads value for a name the body does not declare that resolves to a
  // binding holding only Numbers.
  OUTER_NUMBER_BINDING = -2;
  // Each round drops at least one binding; a body that needs more rounds than
  // this proves nothing.
  MAX_SOLVE_ROUNDS = 64;
  PROOF_BIT_COUNT = 64;

type
  TNumberBinding = record
    Frame: Integer;
    Declaration: TGocciaVariableDeclaration;
    Index: Integer;
    Initializer: TGocciaExpression;
    HoldsNumbers: Boolean;
  end;

  TNumberBindingWrite = record
    Binding: Integer;
    Operator: TGocciaTokenType;
    Value: TGocciaExpression;
  end;

  TShadowedName = record
    Name: string;
    Previous: Integer;
  end;

  TNumberBindingScan = class
  private
    FScope: TGocciaCompilerScope;
    FAllowBlockFunctionVarBindings: Boolean;
    FBindings: array of TNumberBinding;
    FBindingCount: Integer;
    FWrites: array of TNumberBindingWrite;
    FWriteCount: Integer;
    FShadowed: array of TShadowedName;
    FShadowedCount: Integer;
    FVisible: TOrderedStringMap<Integer>;
    FOuterNumbers: TOrderedStringMap<Boolean>;
    FReads: THashMap<TGocciaExpression, Integer>;
    FFrame: Integer;
    FClosureDepth: Integer;
    FFailed: Boolean;

    function PushFrame: Integer;
    procedure PopFrame(const AMark: Integer);
    procedure Declare(const AName: string;
      const ADeclaration: TGocciaVariableDeclaration; const AIndex: Integer;
      const AInitializer: TGocciaExpression);
    procedure DeclareOpaque(const AName: string);
    procedure DeclarePattern(const APattern: TGocciaDestructuringPattern);
    function Resolve(const AName: string): Integer;
    function OuterHoldsNumber(const AName: string): Boolean;
    procedure RecordRead(const AIdentifier: TGocciaIdentifierExpression);
    procedure RecordWrite(const AName: string;
      const AOperator: TGocciaTokenType; const AValue: TGocciaExpression);

    procedure HoistVarNames(const ANode: TGocciaASTNode;
      const ANested: Boolean);
    procedure PredeclareLexical(const ANode: TGocciaASTNode);
    procedure WalkNode(const ANode: TGocciaASTNode);
    procedure WalkList(const AList: TObjectList<TGocciaExpression>);
    procedure WalkBlock(const ANodes: TObjectList<TGocciaASTNode>);
    procedure WalkStatement(const AStmt: TGocciaStatement);
    procedure WalkVariables(const AVariables: TArray<TGocciaVariableInfo>);
    procedure WalkExpression(const AExpr: TGocciaExpression);
    procedure WalkPattern(const APattern: TGocciaDestructuringPattern;
      const AAssigns: Boolean);
    procedure WalkFunction(const AParameters: TGocciaParameterArray;
      const ABody: TGocciaASTNode);
    procedure WalkObject(const AObject: TGocciaObjectExpression);
    procedure WalkClass(const ADefinition: TGocciaClassDefinition);
    procedure WalkEnum(const AEnum: TGocciaEnumDeclaration);

    function IsNumber(const AExpr: TGocciaExpression): Boolean;
    function WriteKeepsNumber(const AWrite: TNumberBindingWrite): Boolean;
    function Solve: Boolean;
    procedure Publish(const AProofs: TNumberBindingProofMap);
  public
    constructor Create(const AScope: TGocciaCompilerScope;
      const AAllowBlockFunctionVarBindings: Boolean);
    destructor Destroy; override;
    procedure Run(const ANodes: array of TGocciaASTNode;
      const AProofs: TNumberBindingProofMap);
  end;

{ TNumberBindingScan }

constructor TNumberBindingScan.Create(const AScope: TGocciaCompilerScope;
  const AAllowBlockFunctionVarBindings: Boolean);
begin
  inherited Create;
  FScope := AScope;
  FAllowBlockFunctionVarBindings := AAllowBlockFunctionVarBindings;
  FVisible := TOrderedStringMap<Integer>.Create;
  FOuterNumbers := TOrderedStringMap<Boolean>.Create;
  FReads := THashMap<TGocciaExpression, Integer>.Create;
end;

destructor TNumberBindingScan.Destroy;
begin
  FReads.Free;
  FOuterNumbers.Free;
  FVisible.Free;
  inherited;
end;

function TNumberBindingScan.PushFrame: Integer;
begin
  Result := FShadowedCount;
  Inc(FFrame);
end;

procedure TNumberBindingScan.PopFrame(const AMark: Integer);
begin
  while FShadowedCount > AMark do
  begin
    Dec(FShadowedCount);
    if FShadowed[FShadowedCount].Previous < 0 then
      FVisible.Remove(FShadowed[FShadowedCount].Name)
    else
      FVisible[FShadowed[FShadowedCount].Name] :=
        FShadowed[FShadowedCount].Previous;
  end;
  Dec(FFrame);
end;

procedure TNumberBindingScan.Declare(const AName: string;
  const ADeclaration: TGocciaVariableDeclaration; const AIndex: Integer;
  const AInitializer: TGocciaExpression);
var
  Previous: Integer;
begin
  if (AName = '') or (FClosureDepth > 0) then
    Exit;
  if not FVisible.TryGetValue(AName, Previous) then
    Previous := -1
  else if FBindings[Previous].Frame = FFrame then
  begin
    // Two declarations of one name in one scope (a var and a function, say)
    // are one binding with both kinds of value.
    FBindings[Previous].HoldsNumbers := False;
    Exit;
  end;

  if FBindingCount = Length(FBindings) then
    SetLength(FBindings, FBindingCount * 2 + 8);
  FBindings[FBindingCount].Frame := FFrame;
  FBindings[FBindingCount].Declaration := ADeclaration;
  FBindings[FBindingCount].Index := AIndex;
  FBindings[FBindingCount].Initializer := AInitializer;
  FBindings[FBindingCount].HoldsNumbers := Assigned(ADeclaration) and
    Assigned(AInitializer);

  if FShadowedCount = Length(FShadowed) then
    SetLength(FShadowed, FShadowedCount * 2 + 8);
  FShadowed[FShadowedCount].Name := AName;
  FShadowed[FShadowedCount].Previous := Previous;
  Inc(FShadowedCount);

  FVisible[AName] := FBindingCount;
  Inc(FBindingCount);
end;

procedure TNumberBindingScan.DeclareOpaque(const AName: string);
begin
  Declare(AName, nil, -1, nil);
end;

procedure TNumberBindingScan.DeclarePattern(
  const APattern: TGocciaDestructuringPattern);
var
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    DeclareOpaque(TGocciaIdentifierDestructuringPattern(APattern).Name)
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    for I := 0 to TGocciaArrayDestructuringPattern(APattern).Elements.Count - 1 do
      DeclarePattern(TGocciaArrayDestructuringPattern(APattern).Elements[I]);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    for I := 0 to TGocciaObjectDestructuringPattern(APattern).Properties.Count - 1 do
      DeclarePattern(
        TGocciaObjectDestructuringPattern(APattern).Properties[I].Pattern);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
    DeclarePattern(TGocciaAssignmentDestructuringPattern(APattern).Left)
  else if APattern is TGocciaRestDestructuringPattern then
    DeclarePattern(TGocciaRestDestructuringPattern(APattern).Argument);
end;

function TNumberBindingScan.Resolve(const AName: string): Integer;
begin
  if not FVisible.TryGetValue(AName, Result) then
    Result := -1;
end;

// A name the body does not declare is a parameter, a binding of an enclosing
// function, or a global. It counts as a Number only where the compiler already
// trusts its hint from inside this function: a const, an enforced binding, or a
// binding proven to hold only Numbers by the scan of its own function, which
// counted this function's assignments to it as unknown values.
function TNumberBindingScan.OuterHoldsNumber(const AName: string): Boolean;
var
  Scope: TGocciaCompilerScope;
  LocalIndex: Integer;
  Local: TGocciaCompilerLocal;
begin
  if FOuterNumbers.TryGetValue(AName, Result) then
    Exit;
  Result := False;
  if not FScope.DirectEvalMayShadow(AName) then
  begin
    Scope := FScope;
    while Assigned(Scope) do
    begin
      if Scope.WithBindingCount > 0 then
        Break;
      LocalIndex := Scope.ResolveLocal(AName);
      if LocalIndex >= 0 then
      begin
        Local := Scope.GetLocal(LocalIndex);
        Result := IsKnownNumeric(Local.TypeHint) and not Local.IsGlobalBacked and
          (Local.IsConst or Local.IsStrictlyTyped or Local.HoldsOnlyNumbers);
        Break;
      end;
      Scope := Scope.Parent;
    end;
  end;
  FOuterNumbers[AName] := Result;
end;

procedure TNumberBindingScan.RecordRead(
  const AIdentifier: TGocciaIdentifierExpression);
var
  Binding: Integer;
begin
  if FClosureDepth > 0 then
    Exit;
  Binding := Resolve(AIdentifier.Name);
  if Binding >= 0 then
    FReads[AIdentifier] := Binding
  else if OuterHoldsNumber(AIdentifier.Name) then
    FReads[AIdentifier] := OUTER_NUMBER_BINDING;
end;

procedure TNumberBindingScan.RecordWrite(const AName: string;
  const AOperator: TGocciaTokenType; const AValue: TGocciaExpression);
var
  Binding: Integer;
begin
  Binding := Resolve(AName);
  if Binding < 0 then
    Exit;
  if FClosureDepth > 0 then
  begin
    // Code in a nested function or class can run at any point after it is
    // created, and the scan does not follow its own bindings.
    FBindings[Binding].HoldsNumbers := False;
    Exit;
  end;
  if FWriteCount = Length(FWrites) then
    SetLength(FWrites, FWriteCount * 2 + 8);
  FWrites[FWriteCount].Binding := Binding;
  FWrites[FWriteCount].Operator := AOperator;
  FWrites[FWriteCount].Value := AValue;
  Inc(FWriteCount);
end;

// ES2026 §10.2.11 FunctionDeclarationInstantiation and §16.1.7
// GlobalDeclarationInstantiation create every var binding holding undefined
// before the body runs, so it can be read before any assignment to it has
// run. It never qualifies.
procedure TNumberBindingScan.HoistVarNames(const ANode: TGocciaASTNode;
  const ANested: Boolean);
var
  I, J: Integer;
  VarDecl: TGocciaVariableDeclaration;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  TryStmt: TGocciaTryStatement;
  Switch: TGocciaSwitchStatement;
begin
  if not Assigned(ANode) then
    Exit;

  if ANode is TGocciaExportVariableDeclaration then
    HoistVarNames(TGocciaExportVariableDeclaration(ANode).Declaration, ANested)
  else if ANode is TGocciaExportDestructuringDeclaration then
    HoistVarNames(TGocciaExportDestructuringDeclaration(ANode).Declaration,
      ANested)
  else if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      for I := 0 to High(VarDecl.Variables) do
        if Assigned(VarDecl.Variables[I].Pattern) then
          DeclarePattern(VarDecl.Variables[I].Pattern)
        else
          DeclareOpaque(VarDecl.Variables[I].Name);
  end
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    if TGocciaDestructuringDeclaration(ANode).IsVar then
      DeclarePattern(TGocciaDestructuringDeclaration(ANode).Pattern);
  end
  else if ANode is TGocciaFunctionDeclaration then
  begin
    // ES2026 §B.3.2: in non-strict code a function declared in a block also
    // creates a var binding.
    if ANested and FAllowBlockFunctionVarBindings then
      DeclareOpaque(TGocciaFunctionDeclaration(ANode).Name);
  end
  else if ANode is TGocciaBlockStatement then
  begin
    for I := 0 to TGocciaBlockStatement(ANode).Nodes.Count - 1 do
      HoistVarNames(TGocciaBlockStatement(ANode).Nodes[I], True);
  end
  else if ANode is TGocciaIfStatement then
  begin
    HoistVarNames(TGocciaIfStatement(ANode).Consequent, True);
    HoistVarNames(TGocciaIfStatement(ANode).Alternate, True);
  end
  else if ANode is TGocciaForStatement then
  begin
    HoistVarNames(TGocciaForStatement(ANode).Init, True);
    HoistVarNames(TGocciaForStatement(ANode).Body, True);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(ANode);
    if ForOf.IsVar then
    begin
      DeclareOpaque(ForOf.BindingName);
      if Assigned(ForOf.BindingPattern) then
        DeclarePattern(ForOf.BindingPattern);
    end;
    HoistVarNames(ForOf.Body, True);
  end
  else if ANode is TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(ANode);
    if ForIn.IsVar then
    begin
      DeclareOpaque(ForIn.BindingName);
      if Assigned(ForIn.BindingPattern) then
        DeclarePattern(ForIn.BindingPattern);
    end;
    HoistVarNames(ForIn.Body, True);
  end
  else if ANode is TGocciaWhileStatement then
    HoistVarNames(TGocciaWhileStatement(ANode).Body, True)
  else if ANode is TGocciaDoWhileStatement then
    HoistVarNames(TGocciaDoWhileStatement(ANode).Body, True)
  else if ANode is TGocciaWithStatement then
    HoistVarNames(TGocciaWithStatement(ANode).Body, True)
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    HoistVarNames(TryStmt.Block, True);
    HoistVarNames(TryStmt.CatchBlock, True);
    HoistVarNames(TryStmt.FinallyBlock, True);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    Switch := TGocciaSwitchStatement(ANode);
    for I := 0 to Switch.Cases.Count - 1 do
      for J := 0 to Switch.Cases[I].Consequent.Count - 1 do
        HoistVarNames(Switch.Cases[I].Consequent[J], True);
  end;
end;

// ES2026 §14.2.3 BlockDeclarationInstantiation: the lexical declarations of a
// block exist from its start, so a name read earlier in the block, or from a
// function declared in it, already resolves to them.
procedure TNumberBindingScan.PredeclareLexical(const ANode: TGocciaASTNode);
var
  I: Integer;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  Import: TGocciaImportDeclaration;
  ImportPair: TStringStringMap.TKeyValuePair;
  ExportDefault: TGocciaExportDefaultDeclaration;
begin
  if ANode is TGocciaExportVariableDeclaration then
    PredeclareLexical(TGocciaExportVariableDeclaration(ANode).Declaration)
  else if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if not VarDecl.IsVar then
      for I := 0 to High(VarDecl.Variables) do
        if VarDecl.Variables[I].IsPattern or
           Assigned(VarDecl.Variables[I].Pattern) then
          DeclarePattern(VarDecl.Variables[I].Pattern)
        else if VarDecl.Variables[I].HasInitializer then
          Declare(VarDecl.Variables[I].Name, VarDecl, I,
            VarDecl.Variables[I].Initializer)
        else
          DeclareOpaque(VarDecl.Variables[I].Name);
  end
  else if ANode is TGocciaExportDestructuringDeclaration then
    PredeclareLexical(TGocciaExportDestructuringDeclaration(ANode).Declaration)
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(ANode);
    if not DestructDecl.IsVar then
      DeclarePattern(DestructDecl.Pattern);
  end
  else if ANode is TGocciaUsingDeclaration then
  begin
    for I := 0 to High(TGocciaUsingDeclaration(ANode).Variables) do
      if Assigned(TGocciaUsingDeclaration(ANode).Variables[I].Pattern) then
        DeclarePattern(TGocciaUsingDeclaration(ANode).Variables[I].Pattern)
      else
        DeclareOpaque(TGocciaUsingDeclaration(ANode).Variables[I].Name);
  end
  else if ANode is TGocciaExportFunctionDeclaration then
    DeclareOpaque(TGocciaExportFunctionDeclaration(ANode).Declaration.Name)
  else if ANode is TGocciaFunctionDeclaration then
    DeclareOpaque(TGocciaFunctionDeclaration(ANode).Name)
  else if ANode is TGocciaExportClassDeclaration then
    DeclareOpaque(TGocciaExportClassDeclaration(ANode).Declaration
      .ClassDefinition.Name)
  else if ANode is TGocciaClassDeclaration then
    DeclareOpaque(TGocciaClassDeclaration(ANode).ClassDefinition.Name)
  else if ANode is TGocciaExportEnumDeclaration then
    DeclareOpaque(TGocciaExportEnumDeclaration(ANode).Declaration.Name)
  else if ANode is TGocciaEnumDeclaration then
    DeclareOpaque(TGocciaEnumDeclaration(ANode).Name)
  else if ANode is TGocciaExportDefaultDeclaration then
  begin
    ExportDefault := TGocciaExportDefaultDeclaration(ANode);
    if ExportDefault.LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING then
      DeclareOpaque(ExportDefault.LocalName);
  end
  else if ANode is TGocciaImportDeclaration then
  begin
    Import := TGocciaImportDeclaration(ANode);
    DeclareOpaque(Import.NamespaceName);
    for ImportPair in Import.Imports do
      DeclareOpaque(ImportPair.Key);
  end;
end;

procedure TNumberBindingScan.WalkNode(const ANode: TGocciaASTNode);
begin
  if FFailed or not Assigned(ANode) then
    Exit;
  if ANode is TGocciaExpression then
    WalkExpression(TGocciaExpression(ANode))
  else if ANode is TGocciaStatement then
    WalkStatement(TGocciaStatement(ANode))
  else
    FFailed := True;
end;

procedure TNumberBindingScan.WalkList(
  const AList: TObjectList<TGocciaExpression>);
var
  I: Integer;
begin
  if Assigned(AList) then
    for I := 0 to AList.Count - 1 do
      WalkNode(AList[I]);
end;

procedure TNumberBindingScan.WalkBlock(
  const ANodes: TObjectList<TGocciaASTNode>);
var
  I, Mark: Integer;
begin
  Mark := PushFrame;
  if FClosureDepth = 0 then
    for I := 0 to ANodes.Count - 1 do
      PredeclareLexical(ANodes[I]);
  for I := 0 to ANodes.Count - 1 do
    WalkNode(ANodes[I]);
  PopFrame(Mark);
end;

procedure TNumberBindingScan.WalkVariables(
  const AVariables: TArray<TGocciaVariableInfo>);
var
  I: Integer;
begin
  for I := 0 to High(AVariables) do
  begin
    WalkNode(AVariables[I].Initializer);
    if Assigned(AVariables[I].Pattern) then
      WalkPattern(AVariables[I].Pattern, False);
  end;
end;

procedure TNumberBindingScan.WalkStatement(const AStmt: TGocciaStatement);
var
  Kind: TClass;
  I, J, Mark: Integer;
  ForStmt: TGocciaForStatement;
  ForOf: TGocciaForOfStatement;
  ForIn: TGocciaForInStatement;
  TryStmt: TGocciaTryStatement;
  Switch: TGocciaSwitchStatement;
begin
  Kind := AStmt.ClassType;

  if Kind = TGocciaExpressionStatement then
    WalkNode(TGocciaExpressionStatement(AStmt).Expression)
  else if Kind = TGocciaVariableDeclaration then
    WalkVariables(TGocciaVariableDeclaration(AStmt).Variables)
  else if Kind = TGocciaUsingDeclaration then
    WalkVariables(TGocciaUsingDeclaration(AStmt).Variables)
  else if Kind = TGocciaDestructuringDeclaration then
  begin
    WalkNode(TGocciaDestructuringDeclaration(AStmt).Initializer);
    WalkPattern(TGocciaDestructuringDeclaration(AStmt).Pattern, False);
  end
  else if Kind = TGocciaFunctionDeclaration then
    WalkNode(TGocciaFunctionDeclaration(AStmt).FunctionExpression)
  else if Kind = TGocciaBlockStatement then
    WalkBlock(TGocciaBlockStatement(AStmt).Nodes)
  else if Kind = TGocciaIfStatement then
  begin
    WalkNode(TGocciaIfStatement(AStmt).Condition);
    WalkNode(TGocciaIfStatement(AStmt).Consequent);
    WalkNode(TGocciaIfStatement(AStmt).Alternate);
  end
  else if Kind = TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    Mark := PushFrame;
    if FClosureDepth = 0 then
      PredeclareLexical(ForStmt.Init);
    WalkNode(ForStmt.Init);
    WalkNode(ForStmt.Condition);
    WalkNode(ForStmt.Update);
    WalkNode(ForStmt.Body);
    PopFrame(Mark);
  end
  else if Kind = TGocciaWhileStatement then
  begin
    WalkNode(TGocciaWhileStatement(AStmt).Condition);
    WalkNode(TGocciaWhileStatement(AStmt).Body);
  end
  else if Kind = TGocciaDoWhileStatement then
  begin
    WalkNode(TGocciaDoWhileStatement(AStmt).Body);
    WalkNode(TGocciaDoWhileStatement(AStmt).Condition);
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOf := TGocciaForOfStatement(AStmt);
    if Assigned(ForOf.MatchPattern) then
    begin
      FFailed := True;
      Exit;
    end;
    Mark := PushFrame;
    if (FClosureDepth = 0) and not ForOf.IsVar then
    begin
      DeclareOpaque(ForOf.BindingName);
      if Assigned(ForOf.BindingPattern) then
        DeclarePattern(ForOf.BindingPattern);
    end;
    if Assigned(ForOf.BindingPattern) then
      WalkPattern(ForOf.BindingPattern, False);
    if Assigned(ForOf.AssignmentTarget) then
      WalkPattern(ForOf.AssignmentTarget, True);
    WalkNode(ForOf.Iterable);
    WalkNode(ForOf.Body);
    PopFrame(Mark);
  end
  else if Kind = TGocciaForInStatement then
  begin
    ForIn := TGocciaForInStatement(AStmt);
    Mark := PushFrame;
    if (FClosureDepth = 0) and not ForIn.IsVar then
    begin
      DeclareOpaque(ForIn.BindingName);
      if Assigned(ForIn.BindingPattern) then
        DeclarePattern(ForIn.BindingPattern);
    end;
    if Assigned(ForIn.BindingPattern) then
      WalkPattern(ForIn.BindingPattern, False);
    if Assigned(ForIn.AssignmentTarget) then
      WalkPattern(ForIn.AssignmentTarget, True);
    WalkNode(ForIn.ObjectExpression);
    WalkNode(ForIn.Body);
    PopFrame(Mark);
  end
  else if Kind = TGocciaReturnStatement then
    WalkNode(TGocciaReturnStatement(AStmt).Value)
  else if Kind = TGocciaThrowStatement then
    WalkNode(TGocciaThrowStatement(AStmt).Value)
  else if Kind = TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    if Assigned(TryStmt.CatchPattern) then
    begin
      FFailed := True;
      Exit;
    end;
    WalkNode(TryStmt.Block);
    Mark := PushFrame;
    if FClosureDepth = 0 then
    begin
      DeclareOpaque(TryStmt.CatchParam);
      if Assigned(TryStmt.CatchBindingPattern) then
        DeclarePattern(TryStmt.CatchBindingPattern);
    end;
    if Assigned(TryStmt.CatchBindingPattern) then
      WalkPattern(TryStmt.CatchBindingPattern, False);
    WalkNode(TryStmt.CatchBlock);
    PopFrame(Mark);
    WalkNode(TryStmt.FinallyBlock);
  end
  else if Kind = TGocciaSwitchStatement then
  begin
    Switch := TGocciaSwitchStatement(AStmt);
    WalkNode(Switch.Discriminant);
    Mark := PushFrame;
    if FClosureDepth = 0 then
      for I := 0 to Switch.Cases.Count - 1 do
        for J := 0 to Switch.Cases[I].Consequent.Count - 1 do
          PredeclareLexical(Switch.Cases[I].Consequent[J]);
    for I := 0 to Switch.Cases.Count - 1 do
    begin
      WalkNode(Switch.Cases[I].Test);
      for J := 0 to Switch.Cases[I].Consequent.Count - 1 do
        WalkNode(Switch.Cases[I].Consequent[J]);
    end;
    PopFrame(Mark);
  end
  else if Kind = TGocciaClassDeclaration then
    WalkClass(TGocciaClassDeclaration(AStmt).ClassDefinition)
  else if Kind = TGocciaEnumDeclaration then
    WalkEnum(TGocciaEnumDeclaration(AStmt))
  else if Kind = TGocciaExportEnumDeclaration then
    WalkEnum(TGocciaExportEnumDeclaration(AStmt).Declaration)
  else if Kind = TGocciaExportDefaultDeclaration then
    WalkNode(TGocciaExportDefaultDeclaration(AStmt).Expression)
  else if Kind = TGocciaExportVariableDeclaration then
    WalkNode(TGocciaExportVariableDeclaration(AStmt).Declaration)
  else if Kind = TGocciaExportDestructuringDeclaration then
    WalkNode(TGocciaExportDestructuringDeclaration(AStmt).Declaration)
  else if Kind = TGocciaExportFunctionDeclaration then
    WalkNode(TGocciaExportFunctionDeclaration(AStmt).Declaration)
  else if Kind = TGocciaExportClassDeclaration then
    WalkNode(TGocciaExportClassDeclaration(AStmt).Declaration)
  else if (Kind = TGocciaImportDeclaration) or
          (Kind = TGocciaExportDeclaration) or
          (Kind = TGocciaReExportDeclaration) or
          (Kind = TGocciaEmptyStatement) or
          (Kind = TGocciaBreakStatement) or
          (Kind = TGocciaContinueStatement) then
    // Nothing to evaluate.
  else
    // A with statement makes every name in its body a possible property of an
    // object; any other statement class is unknown here.
    FFailed := True;
end;

procedure TNumberBindingScan.WalkExpression(const AExpr: TGocciaExpression);
var
  Kind: TClass;
  Call: TGocciaCallExpression;
  Increment: TGocciaIncrementExpression;
begin
  Kind := AExpr.ClassType;

  if (Kind = TGocciaLiteralExpression) or
     (Kind = TGocciaTemplateLiteralExpression) or
     (Kind = TGocciaRegexLiteralExpression) or
     (Kind = TGocciaThisExpression) or
     (Kind = TGocciaSuperExpression) or
     (Kind = TGocciaImportMetaExpression) or
     (Kind = TGocciaNewTargetExpression) or
     (Kind = TGocciaHoleExpression) then
    // A leaf.
  else if Kind = TGocciaIdentifierExpression then
    RecordRead(TGocciaIdentifierExpression(AExpr))
  else if Kind = TGocciaBinaryExpression then
  begin
    WalkNode(TGocciaBinaryExpression(AExpr).Left);
    WalkNode(TGocciaBinaryExpression(AExpr).Right);
  end
  else if Kind = TGocciaUnaryExpression then
    WalkNode(TGocciaUnaryExpression(AExpr).Operand)
  else if Kind = TGocciaSequenceExpression then
    WalkList(TGocciaSequenceExpression(AExpr).Expressions)
  else if Kind = TGocciaConditionalExpression then
  begin
    WalkNode(TGocciaConditionalExpression(AExpr).Condition);
    WalkNode(TGocciaConditionalExpression(AExpr).Consequent);
    WalkNode(TGocciaConditionalExpression(AExpr).Alternate);
  end
  else if Kind = TGocciaAssignmentExpression then
  begin
    WalkNode(TGocciaAssignmentExpression(AExpr).Value);
    RecordWrite(TGocciaAssignmentExpression(AExpr).Name, gttAssign,
      TGocciaAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaCompoundAssignmentExpression then
  begin
    WalkNode(TGocciaCompoundAssignmentExpression(AExpr).Value);
    RecordWrite(TGocciaCompoundAssignmentExpression(AExpr).Name,
      TGocciaCompoundAssignmentExpression(AExpr).Operator,
      TGocciaCompoundAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaIncrementExpression then
  begin
    Increment := TGocciaIncrementExpression(AExpr);
    WalkNode(Increment.Operand);
    if Increment.Operand is TGocciaIdentifierExpression then
      RecordWrite(TGocciaIdentifierExpression(Increment.Operand).Name,
        gttIncrement, nil);
  end
  else if Kind = TGocciaDestructuringAssignmentExpression then
  begin
    WalkNode(TGocciaDestructuringAssignmentExpression(AExpr).Right);
    WalkPattern(TGocciaDestructuringAssignmentExpression(AExpr).Left, True);
  end
  else if Kind = TGocciaCallExpression then
  begin
    Call := TGocciaCallExpression(AExpr);
    // A direct eval runs code that can assign any binding in scope.
    if (Call.Callee is TGocciaIdentifierExpression) and
       (TGocciaIdentifierExpression(Call.Callee).Name = 'eval') then
    begin
      FFailed := True;
      Exit;
    end;
    WalkNode(Call.Callee);
    WalkList(Call.Arguments);
  end
  else if Kind = TGocciaMemberExpression then
  begin
    WalkNode(TGocciaMemberExpression(AExpr).ObjectExpr);
    WalkNode(TGocciaMemberExpression(AExpr).PropertyExpression);
  end
  else if Kind = TGocciaPrivateMemberExpression then
    WalkNode(TGocciaPrivateMemberExpression(AExpr).ObjectExpr)
  else if Kind = TGocciaPropertyAssignmentExpression then
  begin
    WalkNode(TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr);
    WalkNode(TGocciaPropertyAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaComputedPropertyAssignmentExpression then
  begin
    WalkNode(TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr);
    WalkNode(
      TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression);
    WalkNode(TGocciaComputedPropertyAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaPropertyCompoundAssignmentExpression then
  begin
    WalkNode(TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    WalkNode(TGocciaPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    WalkNode(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    WalkNode(TGocciaComputedPropertyCompoundAssignmentExpression(AExpr)
      .PropertyExpression);
    WalkNode(TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaPrivatePropertyAssignmentExpression then
  begin
    WalkNode(TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr);
    WalkNode(TGocciaPrivatePropertyAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    WalkNode(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    WalkNode(TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if Kind = TGocciaNewExpression then
  begin
    WalkNode(TGocciaNewExpression(AExpr).Callee);
    WalkList(TGocciaNewExpression(AExpr).Arguments);
  end
  else if Kind = TGocciaArrayExpression then
    WalkList(TGocciaArrayExpression(AExpr).Elements)
  else if Kind = TGocciaObjectExpression then
    WalkObject(TGocciaObjectExpression(AExpr))
  else if Kind = TGocciaSpreadExpression then
    WalkNode(TGocciaSpreadExpression(AExpr).Argument)
  else if Kind = TGocciaTemplateWithInterpolationExpression then
    WalkList(TGocciaTemplateWithInterpolationExpression(AExpr).Parts)
  else if Kind = TGocciaTaggedTemplateExpression then
  begin
    WalkNode(TGocciaTaggedTemplateExpression(AExpr).Tag);
    WalkList(TGocciaTaggedTemplateExpression(AExpr).Expressions);
  end
  else if Kind = TGocciaImportCallExpression then
  begin
    WalkNode(TGocciaImportCallExpression(AExpr).Specifier);
    WalkNode(TGocciaImportCallExpression(AExpr).Options);
  end
  else if Kind = TGocciaAwaitExpression then
    WalkNode(TGocciaAwaitExpression(AExpr).Operand)
  else if Kind = TGocciaYieldExpression then
    WalkNode(TGocciaYieldExpression(AExpr).Operand)
  else if Kind = TGocciaArrowFunctionExpression then
    WalkFunction(TGocciaArrowFunctionExpression(AExpr).Parameters,
      TGocciaArrowFunctionExpression(AExpr).Body)
  else if Kind = TGocciaFunctionExpression then
    WalkFunction(TGocciaFunctionExpression(AExpr).Parameters,
      TGocciaFunctionExpression(AExpr).Body)
  else if Kind = TGocciaObjectMethodDefinition then
    WalkNode(TGocciaObjectMethodDefinition(AExpr).FunctionExpression)
  else if Kind = TGocciaClassMethod then
    WalkFunction(TGocciaClassMethod(AExpr).Parameters,
      TGocciaClassMethod(AExpr).Body)
  else if Kind = TGocciaGetterExpression then
    WalkFunction(nil, TGocciaGetterExpression(AExpr).Body)
  else if Kind = TGocciaSetterExpression then
    WalkFunction(TGocciaSetterExpression(AExpr).Parameters,
      TGocciaSetterExpression(AExpr).Body)
  else if Kind = TGocciaClassExpression then
    WalkClass(TGocciaClassExpression(AExpr).ClassDefinition)
  else
    // Pattern matching binds names in scopes of its own; any other
    // expression class is unknown here.
    FFailed := True;
end;

procedure TNumberBindingScan.WalkPattern(
  const APattern: TGocciaDestructuringPattern; const AAssigns: Boolean);
var
  I: Integer;
  ObjectPattern: TGocciaObjectDestructuringPattern;
begin
  if FFailed or not Assigned(APattern) then
    Exit;
  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    // A destructuring assignment stores a value read from an object.
    if AAssigns then
      RecordWrite(TGocciaIdentifierDestructuringPattern(APattern).Name,
        gttEOF, nil);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    for I := 0 to TGocciaArrayDestructuringPattern(APattern).Elements.Count - 1 do
      WalkPattern(TGocciaArrayDestructuringPattern(APattern).Elements[I],
        AAssigns);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      WalkNode(ObjectPattern.Properties[I].KeyExpression);
      WalkPattern(ObjectPattern.Properties[I].Pattern, AAssigns);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    WalkPattern(TGocciaAssignmentDestructuringPattern(APattern).Left,
      AAssigns);
    WalkNode(TGocciaAssignmentDestructuringPattern(APattern).Right);
  end
  else if APattern is TGocciaRestDestructuringPattern then
    WalkPattern(TGocciaRestDestructuringPattern(APattern).Argument, AAssigns)
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    WalkNode(TGocciaMemberExpressionDestructuringPattern(APattern).Expression)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    WalkNode(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression)
  else
    FFailed := True;
end;

procedure TNumberBindingScan.WalkFunction(
  const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode);
var
  I: Integer;
begin
  Inc(FClosureDepth);
  for I := 0 to High(AParameters) do
  begin
    WalkNode(AParameters[I].DefaultValue);
    WalkPattern(AParameters[I].Pattern, False);
  end;
  WalkNode(ABody);
  Dec(FClosureDepth);
end;

procedure TNumberBindingScan.WalkObject(const AObject: TGocciaObjectExpression);
var
  I: Integer;
  Order: TArray<TGocciaPropertySourceOrder>;
  ValuePair: TGocciaExpressionMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
begin
  Order := AObject.PropertySourceOrder;
  for I := 0 to High(Order) do
    WalkNode(Order[I].Expression);
  if Assigned(AObject.Properties) then
    for ValuePair in AObject.Properties do
      WalkNode(ValuePair.Value);
  for I := 0 to High(AObject.ComputedPropertiesInOrder) do
  begin
    WalkNode(AObject.ComputedPropertiesInOrder[I].Key);
    WalkNode(AObject.ComputedPropertiesInOrder[I].Value);
  end;
  if Assigned(AObject.Getters) then
    for GetterPair in AObject.Getters do
      WalkNode(GetterPair.Value);
  if Assigned(AObject.Setters) then
    for SetterPair in AObject.Setters do
      WalkNode(SetterPair.Value);
end;

// A class body is treated as one nested function: methods, accessors, field
// initializers and static blocks run after the class is created, and computed
// keys, decorators and the heritage expression are walked with them. Each
// place the parser can keep class code is visited.
procedure TNumberBindingScan.WalkClass(
  const ADefinition: TGocciaClassDefinition);

  procedure WalkMethods(const AMethods: TGocciaClassMethodMap);
  var
    Pair: TGocciaClassMethodMap.TKeyValuePair;
  begin
    if Assigned(AMethods) then
      for Pair in AMethods do
        WalkNode(Pair.Value);
  end;

  procedure WalkGetters(const AGetters: TGocciaGetterExpressionMap);
  var
    Pair: TGocciaGetterExpressionMap.TKeyValuePair;
  begin
    if Assigned(AGetters) then
      for Pair in AGetters do
        WalkNode(Pair.Value);
  end;

  procedure WalkSetters(const ASetters: TGocciaSetterExpressionMap);
  var
    Pair: TGocciaSetterExpressionMap.TKeyValuePair;
  begin
    if Assigned(ASetters) then
      for Pair in ASetters do
        WalkNode(Pair.Value);
  end;

  procedure WalkProperties(const AProperties: TGocciaExpressionMap);
  var
    Pair: TGocciaExpressionMap.TKeyValuePair;
  begin
    if Assigned(AProperties) then
      for Pair in AProperties do
        WalkNode(Pair.Value);
  end;

  procedure WalkDecorators(const ADecorators: TGocciaDecoratorList);
  var
    I: Integer;
  begin
    for I := 0 to High(ADecorators) do
      WalkNode(ADecorators[I]);
  end;

var
  I: Integer;
begin
  Inc(FClosureDepth);
  WalkNode(ADefinition.SuperClassExpression);
  WalkDecorators(ADefinition.FDecorators);
  WalkMethods(ADefinition.Methods);
  WalkMethods(ADefinition.StaticMethods);
  WalkMethods(ADefinition.PrivateMethods);
  WalkGetters(ADefinition.Getters);
  WalkSetters(ADefinition.Setters);
  WalkGetters(ADefinition.StaticGetters);
  WalkSetters(ADefinition.StaticSetters);
  WalkProperties(ADefinition.StaticProperties);
  WalkProperties(ADefinition.InstanceProperties);
  WalkProperties(ADefinition.PrivateInstanceProperties);
  WalkProperties(ADefinition.PrivateStaticProperties);
  for I := 0 to High(ADefinition.FComputedStaticGetters) do
  begin
    WalkNode(ADefinition.FComputedStaticGetters[I].KeyExpression);
    WalkNode(ADefinition.FComputedStaticGetters[I].GetterExpression);
  end;
  for I := 0 to High(ADefinition.FComputedInstanceGetters) do
  begin
    WalkNode(ADefinition.FComputedInstanceGetters[I].KeyExpression);
    WalkNode(ADefinition.FComputedInstanceGetters[I].GetterExpression);
  end;
  for I := 0 to High(ADefinition.FComputedStaticSetters) do
  begin
    WalkNode(ADefinition.FComputedStaticSetters[I].KeyExpression);
    WalkNode(ADefinition.FComputedStaticSetters[I].SetterExpression);
  end;
  for I := 0 to High(ADefinition.FComputedInstanceSetters) do
  begin
    WalkNode(ADefinition.FComputedInstanceSetters[I].KeyExpression);
    WalkNode(ADefinition.FComputedInstanceSetters[I].SetterExpression);
  end;
  for I := 0 to High(ADefinition.FElements) do
  begin
    WalkNode(ADefinition.FElements[I].ComputedKeyExpression);
    WalkDecorators(ADefinition.FElements[I].Decorators);
    WalkNode(ADefinition.FElements[I].MethodNode);
    WalkNode(ADefinition.FElements[I].GetterNode);
    WalkNode(ADefinition.FElements[I].SetterNode);
    WalkNode(ADefinition.FElements[I].FieldInitializer);
    WalkNode(ADefinition.FElements[I].StaticBlockBody);
  end;
  for I := 0 to High(ADefinition.FFieldOrder) do
  begin
    WalkNode(ADefinition.FFieldOrder[I].ComputedKeyExpression);
    WalkNode(ADefinition.FFieldOrder[I].FieldInitializer);
  end;
  Dec(FClosureDepth);
end;

procedure TNumberBindingScan.WalkEnum(const AEnum: TGocciaEnumDeclaration);
var
  I: Integer;
begin
  Inc(FClosureDepth);
  for I := 0 to High(AEnum.Members) do
    WalkNode(AEnum.Members[I].Initializer);
  Dec(FClosureDepth);
end;

// True when evaluating AExpr either produces a Number or throws, given the
// bindings that still hold only Numbers.
function TNumberBindingScan.IsNumber(const AExpr: TGocciaExpression): Boolean;
var
  Binding: Integer;
  Binary: TGocciaBinaryExpression;
  Sequence: TGocciaSequenceExpression;
begin
  Result := False;
  if not Assigned(AExpr) then
    Exit;

  if AExpr is TGocciaLiteralExpression then
    Result := TGocciaLiteralExpression(AExpr).Value is TGocciaNumberLiteralValue
  else if AExpr is TGocciaIdentifierExpression then
    Result := FReads.TryGetValue(AExpr, Binding) and
      ((Binding = OUTER_NUMBER_BINDING) or FBindings[Binding].HoldsNumbers)
  else if AExpr is TGocciaUnaryExpression then
    case TGocciaUnaryExpression(AExpr).Operator of
      // ES2026 §13.5.4 Unary + Operator: ToNumber, which throws for a BigInt.
      gttPlus:
        Result := True;
      // ES2026 §13.5.5 and §13.5.6: ToNumeric keeps a Number a Number.
      gttMinus, gttBitwiseNot:
        Result := IsNumber(TGocciaUnaryExpression(AExpr).Operand);
    end
  else if AExpr is TGocciaBinaryExpression then
  begin
    Binary := TGocciaBinaryExpression(AExpr);
    case Binary.Operator of
      // ES2026 §13.15.3 ApplyStringOrNumericBinaryOperator: apart from +, both
      // operands go through ToNumeric and a Number and a BigInt together
      // throw, so one Number operand makes the result a Number.
      gttMinus, gttStar, gttSlash, gttPercent, gttPower, gttBitwiseAnd,
      gttBitwiseOr, gttBitwiseXor, gttLeftShift, gttRightShift,
      gttUnsignedRightShift:
        Result := IsNumber(Binary.Left) or IsNumber(Binary.Right);
      // + concatenates when either primitive is a String; &&, || and ?? yield
      // one of their operands.
      gttPlus, gttAnd, gttOr, gttNullishCoalescing:
        Result := IsNumber(Binary.Left) and IsNumber(Binary.Right);
    end;
  end
  else if AExpr is TGocciaConditionalExpression then
    Result := IsNumber(TGocciaConditionalExpression(AExpr).Consequent) and
      IsNumber(TGocciaConditionalExpression(AExpr).Alternate)
  else if AExpr is TGocciaSequenceExpression then
  begin
    Sequence := TGocciaSequenceExpression(AExpr);
    Result := (Sequence.Expressions.Count > 0) and
      IsNumber(Sequence.Expressions[Sequence.Expressions.Count - 1]);
  end
  else if AExpr is TGocciaAssignmentExpression then
    Result := IsNumber(TGocciaAssignmentExpression(AExpr).Value)
  else if AExpr is TGocciaCompoundAssignmentExpression then
    case TGocciaCompoundAssignmentExpression(AExpr).Operator of
      gttMinusAssign, gttStarAssign, gttSlashAssign, gttPercentAssign,
      gttPowerAssign, gttBitwiseAndAssign, gttBitwiseOrAssign,
      gttBitwiseXorAssign, gttLeftShiftAssign, gttRightShiftAssign,
      gttUnsignedRightShiftAssign:
        Result := IsNumber(TGocciaCompoundAssignmentExpression(AExpr).Value);
    end
  else if AExpr is TGocciaIncrementExpression then
    // ES2026 §13.4.2 to §13.4.5: the result is ToNumeric of the old value,
    // or that plus or minus one.
    Result := (TGocciaIncrementExpression(AExpr).Operand is
      TGocciaIdentifierExpression) and
      IsNumber(TGocciaIncrementExpression(AExpr).Operand);
end;

// True when AWrite stores a Number in a binding that holds a Number before it.
function TNumberBindingScan.WriteKeepsNumber(
  const AWrite: TNumberBindingWrite): Boolean;
begin
  case AWrite.Operator of
    // ES2026 §13.15.2: = and the logical assignments store the right-hand
    // value; += concatenates unless it is a Number too.
    gttAssign, gttPlusAssign, gttLogicalAndAssign, gttLogicalOrAssign,
    gttNullishCoalescingAssign:
      Result := IsNumber(AWrite.Value);
    // The other compound assignments apply
    // ApplyStringOrNumericBinaryOperator to the old value, a Number, and
    // ++ and -- apply ToNumeric to it.
    gttMinusAssign, gttStarAssign, gttSlashAssign, gttPercentAssign,
    gttPowerAssign, gttBitwiseAndAssign, gttBitwiseOrAssign,
    gttBitwiseXorAssign, gttLeftShiftAssign, gttRightShiftAssign,
    gttUnsignedRightShiftAssign, gttIncrement:
      Result := True;
  else
    Result := False;
  end;
end;

function TNumberBindingScan.Solve: Boolean;
var
  Round, I: Integer;
  Changed: Boolean;
begin
  for Round := 1 to MAX_SOLVE_ROUNDS do
  begin
    Changed := False;
    for I := 0 to FBindingCount - 1 do
      if FBindings[I].HoldsNumbers and
         not IsNumber(FBindings[I].Initializer) then
      begin
        FBindings[I].HoldsNumbers := False;
        Changed := True;
      end;
    for I := 0 to FWriteCount - 1 do
      if FBindings[FWrites[I].Binding].HoldsNumbers and
         not WriteKeepsNumber(FWrites[I]) then
      begin
        FBindings[FWrites[I].Binding].HoldsNumbers := False;
        Changed := True;
      end;
    if not Changed then
      Exit(True);
  end;
  Result := False;
end;

procedure TNumberBindingScan.Publish(const AProofs: TNumberBindingProofMap);
var
  I: Integer;
  Mask: UInt64;
begin
  for I := 0 to FBindingCount - 1 do
    if FBindings[I].HoldsNumbers and (FBindings[I].Index < PROOF_BIT_COUNT) then
    begin
      if not AProofs.TryGetValue(FBindings[I].Declaration, Mask) then
        Mask := 0;
      AProofs[FBindings[I].Declaration] :=
        Mask or (UInt64(1) shl FBindings[I].Index);
    end;
end;

procedure TNumberBindingScan.Run(const ANodes: array of TGocciaASTNode;
  const AProofs: TNumberBindingProofMap);
var
  I: Integer;
begin
  PushFrame;
  for I := 0 to High(ANodes) do
    HoistVarNames(ANodes[I], False);
  for I := 0 to High(ANodes) do
    PredeclareLexical(ANodes[I]);
  for I := 0 to High(ANodes) do
    WalkNode(ANodes[I]);
  if not FFailed and Solve then
    Publish(AProofs);
end;

// True when ANode, outside nested functions, declares a let with an
// initializer: the only kind of binding whose proof the compiler asks for.
// Bodies without one skip the scan.
function DeclaresInitializedLet(const ANode: TGocciaASTNode): Boolean;
var
  I, J: Integer;
  VarDecl: TGocciaVariableDeclaration;
  TryStmt: TGocciaTryStatement;
  Switch: TGocciaSwitchStatement;
begin
  Result := False;
  if not Assigned(ANode) then
    Exit;
  if ANode is TGocciaExportVariableDeclaration then
    Result := DeclaresInitializedLet(
      TGocciaExportVariableDeclaration(ANode).Declaration)
  else if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if not VarDecl.IsVar and not VarDecl.IsConst then
      for I := 0 to High(VarDecl.Variables) do
        if VarDecl.Variables[I].HasInitializer then
          Exit(True);
  end
  else if ANode is TGocciaBlockStatement then
  begin
    for I := 0 to TGocciaBlockStatement(ANode).Nodes.Count - 1 do
      if DeclaresInitializedLet(TGocciaBlockStatement(ANode).Nodes[I]) then
        Exit(True);
  end
  else if ANode is TGocciaIfStatement then
    Result := DeclaresInitializedLet(TGocciaIfStatement(ANode).Consequent) or
      DeclaresInitializedLet(TGocciaIfStatement(ANode).Alternate)
  else if ANode is TGocciaForStatement then
    Result := DeclaresInitializedLet(TGocciaForStatement(ANode).Init) or
      DeclaresInitializedLet(TGocciaForStatement(ANode).Body)
  else if ANode is TGocciaForOfStatement then
    Result := DeclaresInitializedLet(TGocciaForOfStatement(ANode).Body)
  else if ANode is TGocciaForInStatement then
    Result := DeclaresInitializedLet(TGocciaForInStatement(ANode).Body)
  else if ANode is TGocciaWhileStatement then
    Result := DeclaresInitializedLet(TGocciaWhileStatement(ANode).Body)
  else if ANode is TGocciaDoWhileStatement then
    Result := DeclaresInitializedLet(TGocciaDoWhileStatement(ANode).Body)
  else if ANode is TGocciaWithStatement then
    Result := DeclaresInitializedLet(TGocciaWithStatement(ANode).Body)
  else if ANode is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(ANode);
    Result := DeclaresInitializedLet(TryStmt.Block) or
      DeclaresInitializedLet(TryStmt.CatchBlock) or
      DeclaresInitializedLet(TryStmt.FinallyBlock);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    Switch := TGocciaSwitchStatement(ANode);
    for I := 0 to Switch.Cases.Count - 1 do
      for J := 0 to Switch.Cases[I].Consequent.Count - 1 do
        if DeclaresInitializedLet(Switch.Cases[I].Consequent[J]) then
          Exit(True);
  end;
end;

procedure DiscoverNumberBindings(const ABody: TGocciaASTNode;
  const AScope: TGocciaCompilerScope;
  const AAllowBlockFunctionVarBindings: Boolean;
  const AProofs: TNumberBindingProofMap);
var
  Scan: TNumberBindingScan;
  Block: TGocciaBlockStatement;
  Nodes: array of TGocciaASTNode;
  I: Integer;
begin
  if not (ABody is TGocciaBlockStatement) then
    Exit;
  Block := TGocciaBlockStatement(ABody);
  if not DeclaresInitializedLet(Block) then
    Exit;
  SetLength(Nodes, Block.Nodes.Count);
  for I := 0 to Block.Nodes.Count - 1 do
    Nodes[I] := Block.Nodes[I];
  Scan := TNumberBindingScan.Create(AScope, AAllowBlockFunctionVarBindings);
  try
    Scan.Run(Nodes, AProofs);
  finally
    Scan.Free;
  end;
end;

procedure DiscoverProgramNumberBindings(const AProgram: TGocciaProgram;
  const AScope: TGocciaCompilerScope;
  const AAllowBlockFunctionVarBindings: Boolean;
  const AProofs: TNumberBindingProofMap);
var
  Scan: TNumberBindingScan;
  Nodes: array of TGocciaASTNode;
  I: Integer;
  Found: Boolean;
begin
  SetLength(Nodes, AProgram.Body.Count);
  Found := False;
  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Nodes[I] := AProgram.Body[I];
    Found := Found or DeclaresInitializedLet(Nodes[I]);
  end;
  if not Found then
    Exit;
  Scan := TNumberBindingScan.Create(AScope, AAllowBlockFunctionVarBindings);
  try
    Scan.Run(Nodes, AProofs);
  finally
    Scan.Free;
  end;
end;

end.
