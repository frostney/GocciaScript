unit Goccia.Interfaces;

interface

{$I Goccia.inc}

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

type
  IGocciaCallable = interface
    ['{52c77df2-cfb9-4cc6-afc5-cd3c7c7b007f}']
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

end.
