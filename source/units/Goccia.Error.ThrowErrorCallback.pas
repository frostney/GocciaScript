unit Goccia.Error.ThrowErrorCallback;

{$I Goccia.inc}

interface

type
  TGocciaThrowErrorCallback = procedure(const AMessage: string; const ALine, AColumn: Integer) of object;

implementation

end.
