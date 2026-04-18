unit Goccia.Values.NativeFunctionCallback;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

type
  TGocciaNativeFunctionCallback = function(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue of object;

implementation

end.
