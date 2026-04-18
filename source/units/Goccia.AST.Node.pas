unit Goccia.AST.Node;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.ControlFlow,
  Goccia.Evaluator.Context,
  Goccia.Values.Primitives;

type
  TGocciaASTNode = class
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const ALine, AColumn: Integer);
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

  // Expressions — virtual Evaluate replaces the `is` dispatch chain in Goccia.Evaluator
  TGocciaExpression = class(TGocciaASTNode)
  public
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; virtual; abstract;
  end;

  // Statements — virtual Execute replaces the `is` dispatch chain in Goccia.Evaluator
  TGocciaStatement = class(TGocciaASTNode)
  public
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; virtual; abstract;
  end;

  // Program node
  TGocciaProgram = class(TGocciaASTNode)
  private
    FBody: TObjectList<TGocciaStatement>;
  public
    constructor Create(const ABody: TObjectList<TGocciaStatement>);
    property Body: TObjectList<TGocciaStatement> read FBody;
  end;

implementation

constructor TGocciaASTNode.Create(const ALine, AColumn: Integer);
begin
  FLine := ALine;
  FColumn := AColumn;
end;

{ TGocciaProgram }

constructor TGocciaProgram.Create(const ABody: TObjectList<TGocciaStatement>);
begin
  inherited Create(0, 0);
  FBody := ABody;
end;

end.