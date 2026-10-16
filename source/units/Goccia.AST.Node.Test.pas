{ The AST arena (#1592, ADR 0143): every AST object a parse creates belongs to
  that parse's arena, and only the arena frees it. }

program Goccia.AST.Node.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.AST.Node,
  Goccia.Parser,
  Goccia.SourcePipeline,
  Goccia.SourceSpan,
  Goccia.TestSetup;

type
  TCountedNode = class(TGocciaExpression)
  public
    destructor Destroy; override;
  end;

  TTestASTArena = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestArenaFreesWhatJoinedIt;
    procedure TestExplicitFreeLeavesTheArena;
    procedure TestNodeOutsideAnArenaIsTheCreatorsToFree;
    procedure TestParseHandsItsArenaToTheProgram;
    procedure TestFailedParseLeavesNoArenaActive;
  end;

var
  GCountedNodesDestroyed: Integer;

destructor TCountedNode.Destroy;
begin
  Inc(GCountedNodesDestroyed);
  inherited;
end;

procedure TTestASTArena.SetupTests;
begin
  Test('An arena frees every node created while it was active',
    TestArenaFreesWhatJoinedIt);
  Test('A node freed before its arena leaves it and is freed once',
    TestExplicitFreeLeavesTheArena);
  Test('A node created with no arena active joins none',
    TestNodeOutsideAnArenaIsTheCreatorsToFree);
  Test('A parse hands its arena to the program it returns',
    TestParseHandsItsArenaToTheProgram);
  Test('A parse that raises frees its arena and restores the outer one',
    TestFailedParseLeavesNoArenaActive);
end;

procedure TTestASTArena.TestArenaFreesWhatJoinedIt;
var
  Arena, Previous: TGocciaASTArena;
  I: Integer;
begin
  GCountedNodesDestroyed := 0;
  Arena := TGocciaASTArena.Create;
  Previous := ActivateASTArena(Arena);
  try
    for I := 1 to 5 do
      TCountedNode.Create(TGocciaSourceSpan.Empty);
  finally
    RestoreASTArena(Previous);
  end;
  Expect<Integer>(Arena.Count).ToBe(5);
  Expect<Integer>(GCountedNodesDestroyed).ToBe(0);
  Arena.Free;
  Expect<Integer>(GCountedNodesDestroyed).ToBe(5);
end;

procedure TTestASTArena.TestExplicitFreeLeavesTheArena;
var
  Arena, Previous: TGocciaASTArena;
  First, Last: TCountedNode;
begin
  GCountedNodesDestroyed := 0;
  Arena := TGocciaASTArena.Create;
  Previous := ActivateASTArena(Arena);
  try
    First := TCountedNode.Create(TGocciaSourceSpan.Empty);
    TCountedNode.Create(TGocciaSourceSpan.Empty);
    Last := TCountedNode.Create(TGocciaSourceSpan.Empty);
  finally
    RestoreASTArena(Previous);
  end;
  { A discarded node, as the parser frees one, from either end. }
  Last.Free;
  First.Free;
  Expect<Integer>(GCountedNodesDestroyed).ToBe(2);
  Arena.Free;
  Expect<Integer>(GCountedNodesDestroyed).ToBe(3);
end;

procedure TTestASTArena.TestNodeOutsideAnArenaIsTheCreatorsToFree;
var
  Node: TCountedNode;
begin
  GCountedNodesDestroyed := 0;
  Expect<Boolean>(Assigned(ActiveASTArena)).ToBe(False);
  Node := TCountedNode.Create(TGocciaSourceSpan.Empty);
  Node.Free;
  Expect<Integer>(GCountedNodesDestroyed).ToBe(1);
end;

function ParseSource(const ASource: string): TGocciaProgram;
var
  Lines: TStringList;
  PipelineResult: TGocciaSourcePipelineResult;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    PipelineResult := TGocciaSourcePipeline.Parse(Lines, 'arena-test.js',
      TGocciaSourcePipeline.DefaultOptions);
    try
      Result := PipelineResult.TakeProgramNode;
    finally
      PipelineResult.Free;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TTestASTArena.TestParseHandsItsArenaToTheProgram;
var
  ProgramNode: TGocciaProgram;
begin
  ProgramNode := ParseSource(
    'const a = { b: [1, 2, 3], c() { return a.b.map((x) => x * 2); } };' +
    #10'switch (a.b.length) { case 3: a.c(); break; default: }');
  try
    Expect<Integer>(ProgramNode.Body.Count).ToBe(2);
    Expect<Boolean>(Assigned(ActiveASTArena)).ToBe(False);
  finally
    { Frees the whole tree; a node freed twice or left behind would show
      under heaptrc, which the memory-safety job runs. }
    ProgramNode.Free;
  end;
end;

procedure TTestASTArena.TestFailedParseLeavesNoArenaActive;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    ParseSource('const a = { b: [1, 2, 3 };').Free;
  except
    on Exception do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
  Expect<Boolean>(Assigned(ActiveASTArena)).ToBe(False);
end;

begin
  TestRunnerProgram.AddSuite(TTestASTArena.Create('AST arena'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
