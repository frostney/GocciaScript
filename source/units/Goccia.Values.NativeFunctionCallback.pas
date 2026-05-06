unit Goccia.Values.NativeFunctionCallback;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

type
  TGocciaNativeFunctionCallback = function(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue of object;

  // ES2026 §10.2.2 [[Construct]] — separate from [[Call]] so native
  // constructors receive newTarget without changing the 620+ [[Call]] callbacks.
  TGocciaNativeConstructorCallback = function(const AArgs: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue of object;

implementation

end.
