unit Goccia.Values.NativeFunctionCallback;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection, Goccia.Values.Primitives;

type
  TGocciaNativeFunctionCallback = function(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue of object;

implementation

end.
