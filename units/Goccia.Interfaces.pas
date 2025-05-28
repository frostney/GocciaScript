unit Goccia.Interfaces;

interface

{$I Goccia.inc}

uses
  Goccia.Values.Base,
  Goccia.AST.Node;

type
  IGocciaInterpreter = interface
    ['{52c77df2-cfb9-4cc6-afc5-cd3c7c7b007f}']
    function Evaluate(Node: TGocciaASTNode): TGocciaValue;
  end;

implementation

end.
