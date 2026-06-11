unit Goccia.AST.Node;

{$I Goccia.inc}

interface

uses
  Classes,
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
  private
    FLabels: TStringList;
    function GetLabelCount: Integer;
    function GetLabel(const AIndex: Integer): string;
  public
    destructor Destroy; override;
    procedure AddLabel(const AName: string);
    function HasLabel(const AName: string): Boolean;
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; virtual; abstract;
    property LabelCount: Integer read GetLabelCount;
    property Labels[const AIndex: Integer]: string read GetLabel;
  end;

  // Program node
  TGocciaProgram = class(TGocciaASTNode)
  private
    FBody: TObjectList<TGocciaStatement>;
    FHasTopLevelAwait: Boolean;
  public
    constructor Create(const ABody: TObjectList<TGocciaStatement>);
    property Body: TObjectList<TGocciaStatement> read FBody;
    property HasTopLevelAwait: Boolean read FHasTopLevelAwait write FHasTopLevelAwait;
  end;

implementation

constructor TGocciaASTNode.Create(const ALine, AColumn: Integer);
begin
  FLine := ALine;
  FColumn := AColumn;
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

function TGocciaStatement.HasLabel(const AName: string): Boolean;
begin
  Result := Assigned(FLabels) and (FLabels.IndexOf(AName) >= 0);
end;

{ TGocciaProgram }

constructor TGocciaProgram.Create(const ABody: TObjectList<TGocciaStatement>);
begin
  inherited Create(0, 0);
  FBody := ABody;
end;

end.
