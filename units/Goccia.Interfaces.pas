unit Goccia.Interfaces;

interface

{$I Goccia.inc}

uses
  Goccia.Values.Core,
  Generics.Collections;

type
  IGocciaCallable = interface
    ['{52c77df2-cfb9-4cc6-afc5-cd3c7c7b007f}']
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

end.
