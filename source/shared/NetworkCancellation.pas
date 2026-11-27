unit NetworkCancellation;

// A request's cancellation, which the network client observes between its
// steps and in every wait on a socket. Socket-free, so HTTPTypes can carry
// one in a request policy.

{$I Shared.inc}

interface

const
  { The longest a socket wait of a request that can be cancelled lasts
    before it observes the cancellation again. }
  NETWORK_CANCELLATION_POLL_MILLISECONDS = 50;

type
  { Cancel is set once, from any thread, and never cleared. The client never
    waits on a socket for longer than NETWORK_CANCELLATION_POLL_MILLISECONDS
    without looking at it, so a cancelled request ends within one poll,
    however silent its server, and closes its sockets as it ends.

    Counted references: the request's owner holds one, and a connection
    attempt that runs on a thread of its own holds another, since that
    thread can outlive the request by up to one poll. The last Release frees
    it. }
  TNetworkCancellation = class
  private
    FCancelled: Integer;
    FReferenceCount: Integer;
  public
    constructor Create;
    procedure AddReference;
    procedure Release;
    procedure Cancel;
    function Cancelled: Boolean;
  end;

{ True when ACancellation is assigned and cancelled. }
function NetworkCancelled(
  const ACancellation: TNetworkCancellation): Boolean;

{ The time a socket wait of AMilliseconds (-1: no limit) may block before it
  next observes ACancellation: AMilliseconds itself without a cancellation,
  and at most NETWORK_CANCELLATION_POLL_MILLISECONDS with one. }
function CancellableWaitMilliseconds(
  const ACancellation: TNetworkCancellation;
  const AMilliseconds: Int64): Int64;

implementation

uses
  CriticalSections;

constructor TNetworkCancellation.Create;
begin
  inherited Create;
  FReferenceCount := 1;
end;

procedure TNetworkCancellation.AddReference;
begin
  AtomicIncrementInt32(FReferenceCount);
end;

procedure TNetworkCancellation.Release;
begin
  if AtomicDecrementInt32(FReferenceCount) = 0 then
    Free;
end;

procedure TNetworkCancellation.Cancel;
begin
  AtomicExchangeInt32(FCancelled, 1);
end;

function TNetworkCancellation.Cancelled: Boolean;
begin
  Result := AtomicLoadInt32(FCancelled) <> 0;
end;

function NetworkCancelled(
  const ACancellation: TNetworkCancellation): Boolean;
begin
  Result := Assigned(ACancellation) and ACancellation.Cancelled;
end;

function CancellableWaitMilliseconds(
  const ACancellation: TNetworkCancellation;
  const AMilliseconds: Int64): Int64;
begin
  Result := AMilliseconds;
  if Assigned(ACancellation) and
     ((Result < 0) or (Result > NETWORK_CANCELLATION_POLL_MILLISECONDS)) then
    Result := NETWORK_CANCELLATION_POLL_MILLISECONDS;
end;

end.
