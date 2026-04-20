unit Goccia.StackLimit;

{$I Goccia.inc}

interface

const
  DEFAULT_MAX_STACK_DEPTH = 3500;

procedure SetMaxStackDepth(const AMaxDepth: Integer);
procedure CheckStackDepth(const ACurrentDepth: Integer);

implementation

uses
  Goccia.Error.Messages,
  Goccia.Values.ErrorHelper;

var
  GMaxStackDepth: Integer;

procedure SetMaxStackDepth(const AMaxDepth: Integer);
begin
  GMaxStackDepth := AMaxDepth;
end;

procedure CheckStackDepth(const ACurrentDepth: Integer);
begin
  if (GMaxStackDepth > 0) and (ACurrentDepth > GMaxStackDepth) then
    ThrowRangeError(SErrorMaxCallStackExceeded);
end;

end.
