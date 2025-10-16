unit Goccia.Error.ThrowErrorCallback;

{$I Goccia.inc}

interface

type
  TGocciaThrowErrorCallback = procedure(const Message: string; Line, Column: Integer) of object;

implementation

end.
