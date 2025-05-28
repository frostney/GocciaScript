unit Goccia.AST.Node;

{$I Goccia.inc}

interface

uses
  SysUtils, StrUtils, Classes, Generics.Collections;

type
  TGocciaASTNode = class
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(ALine, AColumn: Integer);
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
    constructor Create(ABody: TObjectList<TGocciaStatement>);
    property Body: TObjectList<TGocciaStatement> read FBody;
  end;

implementation

constructor TGocciaASTNode.Create(ALine, AColumn: Integer);
begin
  FLine := ALine;
  FColumn := AColumn;
end;

{ TGocciaProgram }

constructor TGocciaProgram.Create(ABody: TObjectList<TGocciaStatement>);
begin
  inherited Create(0, 0);
  FBody := ABody;
end;

end.