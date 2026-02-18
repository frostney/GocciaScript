unit Goccia.AST.Node;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  StrUtils,
  SysUtils;

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

  // Expressions
  TGocciaExpression = class(TGocciaASTNode);

  // Statements
  TGocciaStatement = class(TGocciaASTNode);

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