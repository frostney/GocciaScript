program Goccia.StatementSpans.Test;

{$I Goccia.inc}

{ A statement's span is what a tool slices the file with, so every one of them
  has to select the statement and nothing else. These are the shapes where the
  parser's position used to come from somewhere other than the statement's own
  first token. }

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,
  TextSemantics,

  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.GarbageCollector,
  Goccia.SourcePipeline,
  Goccia.TestSetup;

type
  TStatementSpanTests = class(TTestSuite)
  private
    procedure ExpectStatementText(const ASource: string;
      const AIndex: Integer; const AExpected: string;
      const ACompatibility: TGocciaCompatibilityFlags = []);
    procedure ExpectNestedText(const ASource: string;
      const AExpected: string;
      const ACompatibility: TGocciaCompatibilityFlags = []);

    procedure TestCallExpressionStatement;
    procedure TestAssignmentExpressionStatement;
    procedure TestUpdateExpressionStatement;
    procedure TestMemberCallExpressionStatement;
    procedure TestAsyncFunctionDeclaration;
    procedure TestExportedAsyncFunctionDeclaration;
    procedure TestFunctionBodyIsTheBraces;
    procedure TestMethodBodyIsTheBraces;
    procedure TestCaseClauseIsItsOwnClause;
    procedure TestDefaultClauseIsItsOwnClause;
  public
    procedure SetupTests; override;
  end;

procedure TStatementSpanTests.SetupTests;
begin
  Test('A call expression statement starts at the callee',
    TestCallExpressionStatement);
  Test('An assignment expression statement starts at the target',
    TestAssignmentExpressionStatement);
  Test('A postfix update statement starts at the operand',
    TestUpdateExpressionStatement);
  Test('A member call statement starts at the object',
    TestMemberCallExpressionStatement);
  Test('An async function declaration starts at async',
    TestAsyncFunctionDeclaration);
  Test('An exported async function declaration covers the export',
    TestExportedAsyncFunctionDeclaration);
  Test('A function body covers its braces', TestFunctionBodyIsTheBraces);
  Test('A method body covers its braces', TestMethodBodyIsTheBraces);
  Test('A case clause covers the clause, not the switch',
    TestCaseClauseIsItsOwnClause);
  Test('A default clause covers the clause, not the switch',
    TestDefaultClauseIsItsOwnClause);
end;

function ParseSource(const ASource: string;
  const ACompatibility: TGocciaCompatibilityFlags): TGocciaSourcePipelineResult;
var
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
begin
  Options := TGocciaSourcePipeline.DefaultOptions;
  Options.Compatibility := ACompatibility;
  Options.SourceType := stModule;
  Lines := CreateTextLines(ASource);
  try
    Result := TGocciaSourcePipeline.Parse(Lines, 'spans.ts', Options);
  finally
    Lines.Free;
  end;
end;

function TextOfSpan(const ASource: string;
  const ANode: TGocciaASTNode): string;
begin
  Result := Copy(ASource, ANode.Span.StartOffset + 1,
    ANode.Span.EndOffset - ANode.Span.StartOffset);
end;

procedure TStatementSpanTests.ExpectStatementText(const ASource: string;
  const AIndex: Integer; const AExpected: string;
  const ACompatibility: TGocciaCompatibilityFlags);
var
  Parsed: TGocciaSourcePipelineResult;
begin
  Parsed := ParseSource(ASource, ACompatibility);
  try
    Expect<string>(TextOfSpan(ASource,
      Parsed.ProgramNode.Body[AIndex])).ToBe(AExpected);
  finally
    Parsed.Free;
  end;
end;

{ The first block statement anywhere under the first top-level statement. The
  shapes under test each have exactly one. }
function FirstBlock(const ANode: TGocciaASTNode): TGocciaBlockStatement; forward;

procedure TStatementSpanTests.ExpectNestedText(const ASource: string;
  const AExpected: string;
  const ACompatibility: TGocciaCompatibilityFlags);
var
  Parsed: TGocciaSourcePipelineResult;
  Block: TGocciaBlockStatement;
begin
  Parsed := ParseSource(ASource, ACompatibility);
  try
    Block := FirstBlock(Parsed.ProgramNode.Body[0]);
    Expect<Boolean>(Assigned(Block)).ToBe(True);
    if Assigned(Block) then
      Expect<string>(TextOfSpan(ASource, Block)).ToBe(AExpected);
  finally
    Parsed.Free;
  end;
end;

procedure TStatementSpanTests.TestCallExpressionStatement;
begin
  ExpectStatementText('use(next);' + #10, 0, 'use(next);');
end;

procedure TStatementSpanTests.TestAssignmentExpressionStatement;
begin
  ExpectStatementText('let x = 0;' + #10 + 'x = 1;' + #10, 1, 'x = 1;');
end;

procedure TStatementSpanTests.TestUpdateExpressionStatement;
begin
  ExpectStatementText('let x = 0;' + #10 + 'x++;' + #10, 1, 'x++;');
end;

procedure TStatementSpanTests.TestMemberCallExpressionStatement;
begin
  ExpectStatementText('obj.m().n();' + #10, 0, 'obj.m().n();');
end;

procedure TStatementSpanTests.TestAsyncFunctionDeclaration;
begin
  ExpectStatementText(
    'async function f() {' + #10 + '  await g();' + #10 + '}' + #10,
    0, 'async function f() {' + #10 + '  await g();' + #10 + '}',
    [cfFunction]);
end;

procedure TStatementSpanTests.TestExportedAsyncFunctionDeclaration;
begin
  ExpectStatementText(
    'export async function f() {' + #10 + '  await g();' + #10 + '}' + #10,
    0, 'export async function f() {' + #10 + '  await g();' + #10 + '}',
    [cfFunction]);
end;

procedure TStatementSpanTests.TestFunctionBodyIsTheBraces;
begin
  ExpectNestedText(
    'async function f() {' + #10 + '  await g();' + #10 + '}' + #10,
    '{' + #10 + '  await g();' + #10 + '}', [cfFunction]);
end;

procedure TStatementSpanTests.TestMethodBodyIsTheBraces;
begin
  ExpectNestedText(
    'class C {' + #10 + '  m() {' + #10 + '    return 1;' + #10 + '  }' + #10 +
    '}' + #10,
    '{' + #10 + '    return 1;' + #10 + '  }');
end;

function SwitchClauseText(const ASource: string;
  const AIndex: Integer): string;
var
  Parsed: TGocciaSourcePipelineResult;
  SwitchNode: TGocciaSwitchStatement;
begin
  Parsed := ParseSource(ASource, []);
  try
    SwitchNode := Parsed.ProgramNode.Body[0] as TGocciaSwitchStatement;
    Result := TextOfSpan(ASource, SwitchNode.Cases[AIndex]);
  finally
    Parsed.Free;
  end;
end;

const
  SWITCH_SOURCE =
    'switch (x) {' + #10 +
    '  case 1:' + #10 +
    '    a();' + #10 +
    '    break;' + #10 +
    '  default:' + #10 +
    '    b();' + #10 +
    '}' + #10;

procedure TStatementSpanTests.TestCaseClauseIsItsOwnClause;
begin
  Expect<string>(SwitchClauseText(SWITCH_SOURCE, 0)).ToBe(
    'case 1:' + #10 + '    a();' + #10 + '    break;');
end;

procedure TStatementSpanTests.TestDefaultClauseIsItsOwnClause;
begin
  Expect<string>(SwitchClauseText(SWITCH_SOURCE, 1)).ToBe(
    'default:' + #10 + '    b();');
end;

function FirstBlock(const ANode: TGocciaASTNode): TGocciaBlockStatement;
var
  I: Integer;
  Found: TGocciaBlockStatement;
begin
  Result := nil;
  if not Assigned(ANode) then
    Exit;
  if ANode is TGocciaBlockStatement then
    Exit(TGocciaBlockStatement(ANode));

  if ANode is TGocciaFunctionDeclaration then
    Exit(FirstBlock(TGocciaFunctionDeclaration(ANode).FunctionExpression.Body));
  if ANode is TGocciaExportFunctionDeclaration then
    Exit(FirstBlock(TGocciaExportFunctionDeclaration(ANode).Declaration));
  if ANode is TGocciaClassDeclaration then
  begin
    for I := 0 to High(TGocciaClassDeclaration(ANode).ClassDefinition.FElements) do
    begin
      Found := FirstBlock(
        TGocciaClassDeclaration(ANode).ClassDefinition.FElements[I].MethodNode);
      if Assigned(Found) then
        Exit(Found);
    end;
    Exit;
  end;
  if ANode is TGocciaClassMethod then
    Exit(FirstBlock(TGocciaClassMethod(ANode).Body));
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TStatementSpanTests.Create('StatementSpans'));
    RunGocciaTests;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
