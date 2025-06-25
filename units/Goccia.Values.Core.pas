unit Goccia.Values.Core;

{$I Goccia.inc}

interface

uses
  Classes;

type
  TGocciaValue = class(TInterfacedObject)
  public
    function TypeName: string; virtual; abstract;
    function TypeOf: string; virtual; abstract;
  end;

implementation

end.
