unit Goccia.FetchManager;

{$I Goccia.inc}

interface

// Interface dependencies are TYPES ONLY (HTTPTypes carries
// THTTPHeaders); the socket-backed client stays behind the LAKON
// gate in the implementation.

uses
  CriticalSections,
  HTTPTypes,

  Goccia.Values.PromiseValue;

type
  TGocciaFetchManager = class
  public
    class function Instance: TGocciaFetchManager;
    class procedure Initialize;
    class procedure Shutdown;

    procedure StartFetch(const AURL, AMethod: string;
      const AHeaders: THTTPHeaders; const APromise: TGocciaPromiseValue); virtual; abstract;
    function PumpCompletions: Integer; virtual; abstract;
    function HasPending: Boolean; virtual; abstract;
    function WaitForPromise(const APromise: TGocciaPromiseValue): Boolean; virtual; abstract;
    procedure WaitForIdle; virtual; abstract;
    procedure DiscardPending; virtual; abstract;
  end;

procedure DrainMicrotasksAndFetchCompletions;
function WaitForFetchPromise(const APromise: TGocciaPromiseValue): Boolean;
procedure WaitForFetchIdle;
procedure DiscardFetchCompletions;

implementation

// The DEFAULT WORKER BACKEND (TThread + HTTPClient + polling Sleep)
// is gated on platforms with real threads and sockets; the Lakon
// WASM lane compiles only the abstract manager and the drain/wait
// helpers — Initialize leaves Instance nil there, so fetch() is
// unavailable until a WASI backend exists (the host-integration
// slice). Everything the shared helpers need stays outside the gate.

uses
  {$IFNDEF LAKON}
  Classes,
  Generics.Collections,
  SyncObjs,

  HTTPClient,

  Goccia.Values.ErrorHelper,
  Goccia.Values.HeadersValue,
  Goccia.Values.ResponseValue,
  {$ENDIF}
  SysUtils,

  Goccia.Builtins.Atomics,
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue;

const
  FETCH_POLL_INTERVAL_MS = 1;

{$IFNDEF LAKON}
const
  FETCH_WORKER_STACK_SIZE = 8 * 1024 * 1024;
  MAX_FETCH_WORKERS = 16;
  FETCH_WORKER_LIMIT_ERROR = 'fetch worker limit exceeded';

type
  TGocciaFetchCompletion = class
  public
    RequestID: Integer;
    Success: Boolean;
    Response: THTTPResponse;
    ErrorMessage: string;
    constructor Create(const ARequestID: Integer);
  end;

  TGocciaFetchLimiter = class
  private
    FLock: TGocciaCriticalSection;
    FActiveWorkers: Integer;
    FRefCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRef;
    procedure Release;
    function TryAcquireWorker: Boolean;
    procedure ReleaseWorker;
  end;

  TGocciaFetchState = class
  private
    FLock: TGocciaCriticalSection;
    FCompletions: TList<TGocciaFetchCompletion>;
    FAcceptCompletions: Boolean;
    FRefCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRef;
    procedure Release;
    function PostCompletion(
      const ACompletion: TGocciaFetchCompletion): Boolean;
    function PopCompletion(out ACompletion: TGocciaFetchCompletion): Boolean;
    procedure ClearCompletions;
    procedure Abandon;
  end;

  TGocciaPendingFetch = record
    RequestID: Integer;
    Promise: TGocciaPromiseValue;
  end;

  TGocciaFetchWorker = class(TThread)
  private
    FState: TGocciaFetchState;
    FLimiter: TGocciaFetchLimiter;
    FRequestID: Integer;
    FURL: string;
    FMethod: string;
    FHeaders: THTTPHeaders;
  protected
    procedure Execute; override;
  public
    constructor Create(const AState: TGocciaFetchState;
      const ALimiter: TGocciaFetchLimiter; const ARequestID: Integer;
      const AURL, AMethod: string; const AHeaders: THTTPHeaders);
    destructor Destroy; override;
  end;

  TGocciaFetchManagerImpl = class(TGocciaFetchManager)
  private
    FState: TGocciaFetchState;
    FLimiter: TGocciaFetchLimiter;
    FPending: TList<TGocciaPendingFetch>;
    FNextRequestID: Integer;
    function PopCompletion(out ACompletion: TGocciaFetchCompletion): Boolean;
    function FindPendingIndex(const ARequestID: Integer): Integer;
    procedure SettleCompletion(const ACompletion: TGocciaFetchCompletion);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartFetch(const AURL, AMethod: string;
      const AHeaders: THTTPHeaders; const APromise: TGocciaPromiseValue); override;
    function PumpCompletions: Integer; override;
    function HasPending: Boolean; override;
    function WaitForPromise(const APromise: TGocciaPromiseValue): Boolean; override;
    procedure WaitForIdle; override;
    procedure DiscardPending; override;
  end;

{$ENDIF}

threadvar
  FetchManagerThreadInstance: TGocciaFetchManager;

{$IFNDEF LAKON}

{ TGocciaFetchCompletion }

constructor TGocciaFetchCompletion.Create(const ARequestID: Integer);
begin
  inherited Create;
  RequestID := ARequestID;
  Success := False;
  ErrorMessage := '';
end;

{ TGocciaFetchLimiter }

constructor TGocciaFetchLimiter.Create;
begin
  inherited Create;
  CriticalSectionInit(FLock);
  FActiveWorkers := 0;
  FRefCount := 1;
end;

destructor TGocciaFetchLimiter.Destroy;
begin
  CriticalSectionDone(FLock);
  inherited;
end;

procedure TGocciaFetchLimiter.AddRef;
begin
  AtomicIncrementInt32(FRefCount);
end;

procedure TGocciaFetchLimiter.Release;
begin
  if AtomicDecrementInt32(FRefCount) = 0 then
    Free;
end;

function TGocciaFetchLimiter.TryAcquireWorker: Boolean;
begin
  CriticalSectionEnter(FLock);
  try
    Result := FActiveWorkers < MAX_FETCH_WORKERS;
    if Result then
      Inc(FActiveWorkers);
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaFetchLimiter.ReleaseWorker;
begin
  CriticalSectionEnter(FLock);
  try
    if FActiveWorkers > 0 then
      Dec(FActiveWorkers);
  finally
    CriticalSectionLeave(FLock);
  end;
end;

{ TGocciaFetchState }

constructor TGocciaFetchState.Create;
begin
  inherited Create;
  CriticalSectionInit(FLock);
  FCompletions := TList<TGocciaFetchCompletion>.Create;
  FAcceptCompletions := True;
  FRefCount := 1;
end;

destructor TGocciaFetchState.Destroy;
begin
  ClearCompletions;
  FCompletions.Free;
  CriticalSectionDone(FLock);
  inherited;
end;

procedure TGocciaFetchState.AddRef;
begin
  AtomicIncrementInt32(FRefCount);
end;

procedure TGocciaFetchState.Release;
begin
  if AtomicDecrementInt32(FRefCount) = 0 then
    Free;
end;

function TGocciaFetchState.PostCompletion(
  const ACompletion: TGocciaFetchCompletion): Boolean;
begin
  CriticalSectionEnter(FLock);
  try
    Result := FAcceptCompletions;
    if Result then
      FCompletions.Add(ACompletion);
  finally
    CriticalSectionLeave(FLock);
  end;
end;

function TGocciaFetchState.PopCompletion(
  out ACompletion: TGocciaFetchCompletion): Boolean;
begin
  ACompletion := nil;
  CriticalSectionEnter(FLock);
  try
    Result := FCompletions.Count > 0;
    if Result then
    begin
      ACompletion := FCompletions[0];
      FCompletions.Delete(0);
    end;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaFetchState.ClearCompletions;
var
  I: Integer;
begin
  CriticalSectionEnter(FLock);
  try
    for I := 0 to FCompletions.Count - 1 do
      FCompletions[I].Free;
    FCompletions.Clear;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaFetchState.Abandon;
begin
  CriticalSectionEnter(FLock);
  try
    FAcceptCompletions := False;
  finally
    CriticalSectionLeave(FLock);
  end;
  ClearCompletions;
end;

{ TGocciaFetchWorker }

constructor TGocciaFetchWorker.Create(const AState: TGocciaFetchState;
  const ALimiter: TGocciaFetchLimiter; const ARequestID: Integer;
  const AURL, AMethod: string; const AHeaders: THTTPHeaders);
begin
  inherited Create(True, FETCH_WORKER_STACK_SIZE);
  FreeOnTerminate := True;
  FRequestID := ARequestID;
  FURL := AURL;
  FMethod := AMethod;
  FHeaders := AHeaders;
  FState := AState;
  FState.AddRef;
  FLimiter := ALimiter;
  FLimiter.AddRef;
end;

destructor TGocciaFetchWorker.Destroy;
begin
  if Assigned(FLimiter) then
  begin
    FLimiter.ReleaseWorker;
    FLimiter.Release;
  end;
  if Assigned(FState) then
    FState.Release;
  inherited;
end;

procedure TGocciaFetchWorker.Execute;
var
  Completion: TGocciaFetchCompletion;
begin
  Completion := TGocciaFetchCompletion.Create(FRequestID);
  try
    try
      if FMethod = 'HEAD' then
        Completion.Response := HTTPHead(FURL, FHeaders)
      else
        Completion.Response := HTTPGet(FURL, FHeaders);
      Completion.Success := True;
    except
      on E: EHTTPError do
        Completion.ErrorMessage := E.Message;
      on E: Exception do
        Completion.ErrorMessage := 'fetch failed: ' + E.Message;
    end;

    if FState.PostCompletion(Completion) then
      Completion := nil;
  finally
    Completion.Free;
  end;
end;

{$ENDIF}

{ TGocciaFetchManager }

class function TGocciaFetchManager.Instance: TGocciaFetchManager;
begin
  Result := FetchManagerThreadInstance;
end;

class procedure TGocciaFetchManager.Initialize;
begin
  {$IFNDEF LAKON}
  if not Assigned(FetchManagerThreadInstance) then
    FetchManagerThreadInstance := TGocciaFetchManagerImpl.Create;
  {$ENDIF}
end;

class procedure TGocciaFetchManager.Shutdown;
begin
  FreeAndNil(FetchManagerThreadInstance);
end;

{$IFNDEF LAKON}

{ TGocciaFetchManagerImpl }

constructor TGocciaFetchManagerImpl.Create;
begin
  inherited Create;
  FState := TGocciaFetchState.Create;
  FLimiter := TGocciaFetchLimiter.Create;
  FPending := TList<TGocciaPendingFetch>.Create;
  FNextRequestID := 1;
end;

destructor TGocciaFetchManagerImpl.Destroy;
begin
  DiscardPending;
  FState.Abandon;
  FState.Release;
  FLimiter.Release;
  FPending.Free;
  inherited;
end;

procedure TGocciaFetchManagerImpl.StartFetch(const AURL, AMethod: string;
  const AHeaders: THTTPHeaders; const APromise: TGocciaPromiseValue);
var
  Pending: TGocciaPendingFetch;
  Worker: TGocciaFetchWorker;
  Added, LimitAcquired, Rooted: Boolean;
begin
  Worker := nil;
  Added := False;
  LimitAcquired := False;
  Rooted := False;

  if not FLimiter.TryAcquireWorker then
  begin
    APromise.Reject(CreateErrorObject('TypeError', FETCH_WORKER_LIMIT_ERROR));
    Exit;
  end;
  LimitAcquired := True;

  Pending.RequestID := FNextRequestID;
  Inc(FNextRequestID);
  Pending.Promise := APromise;

  try
    Worker := TGocciaFetchWorker.Create(FState, FLimiter,
      Pending.RequestID, AURL, AMethod, AHeaders);
    LimitAcquired := False;

    if (TGarbageCollector.Instance <> nil) then
    begin
      TGarbageCollector.Instance.AddTempRoot(APromise);
      Rooted := True;
    end;

    FPending.Add(Pending);
    Added := True;
    Worker.Start;
    Worker := nil;
  except
    if Added then
      FPending.Delete(FPending.Count - 1);
    if Rooted and (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(APromise);
    Worker.Free;
    if LimitAcquired then
      FLimiter.ReleaseWorker;
    raise;
  end;
end;

function TGocciaFetchManagerImpl.PopCompletion(
  out ACompletion: TGocciaFetchCompletion): Boolean;
begin
  Result := FState.PopCompletion(ACompletion);
end;

function TGocciaFetchManagerImpl.FindPendingIndex(
  const ARequestID: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FPending.Count - 1 do
    if FPending[I].RequestID = ARequestID then
      Exit(I);
  Result := -1;
end;

procedure TGocciaFetchManagerImpl.SettleCompletion(
  const ACompletion: TGocciaFetchCompletion);
var
  Pending: TGocciaPendingFetch;
  PendingIndex, I: Integer;
  RespHeaders: TGocciaHeadersValue;
  RespValue: TGocciaResponseValue;
begin
  PendingIndex := FindPendingIndex(ACompletion.RequestID);
  if PendingIndex < 0 then
    Exit;

  Pending := FPending[PendingIndex];
  FPending.Delete(PendingIndex);

  try
    if ACompletion.Success then
    begin
      RespHeaders := TGocciaHeadersValue.Create;
      RespHeaders.Immutable := True;
      for I := 0 to High(ACompletion.Response.Headers) do
        RespHeaders.AddHeader(ACompletion.Response.Headers[I].Name,
          ACompletion.Response.Headers[I].Value);

      RespValue := TGocciaResponseValue.Create;
      RespValue.InitFromHTTP(
        ACompletion.Response.StatusCode,
        ACompletion.Response.StatusText,
        ACompletion.Response.FinalURL,
        RespHeaders,
        ACompletion.Response.Body,
        ACompletion.Response.Redirected);

      Pending.Promise.Resolve(RespValue);
    end
    else
      Pending.Promise.Reject(CreateErrorObject('TypeError',
        ACompletion.ErrorMessage));

    if (TGocciaMicrotaskQueue.Instance <> nil) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(Pending.Promise);
  end;
end;

function TGocciaFetchManagerImpl.PumpCompletions: Integer;
var
  Completion: TGocciaFetchCompletion;
begin
  Result := 0;
  while PopCompletion(Completion) do
  begin
    try
      SettleCompletion(Completion);
      Inc(Result);
    finally
      Completion.Free;
    end;
  end;
end;

function TGocciaFetchManagerImpl.HasPending: Boolean;
begin
  Result := FPending.Count > 0;
end;

function TGocciaFetchManagerImpl.WaitForPromise(
  const APromise: TGocciaPromiseValue): Boolean;
begin
  Result := Assigned(APromise) and (APromise.State <> gpsPending);
  while Assigned(APromise) and (APromise.State = gpsPending) do
  begin
    DrainMicrotasksAndFetchCompletions;
    if APromise.State <> gpsPending then
      Exit(True);
    if not HasPending then
      Exit(False);

    while HasPending and (PumpCompletions = 0) do
      Sleep(FETCH_POLL_INTERVAL_MS);
  end;
end;

procedure TGocciaFetchManagerImpl.WaitForIdle;
begin
  repeat
    DrainMicrotasksAndFetchCompletions;
    if not HasPending then
      Break;
    while HasPending and (PumpCompletions = 0) do
      Sleep(FETCH_POLL_INTERVAL_MS);
  until False;
  DrainMicrotasksAndFetchCompletions;
end;

procedure TGocciaFetchManagerImpl.DiscardPending;
var
  I: Integer;
  HadPending: Boolean;
  OldState: TGocciaFetchState;
  Pending: TGocciaPendingFetch;
begin
  HadPending := FPending.Count > 0;
  for I := 0 to FPending.Count - 1 do
  begin
    Pending := FPending[I];
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(Pending.Promise);
  end;
  FPending.Clear;

  if not HadPending then
  begin
    FState.ClearCompletions;
    Exit;
  end;

  OldState := FState;
  FState := TGocciaFetchState.Create;
  OldState.Abandon;
  OldState.Release;
end;
{$ENDIF}

procedure DrainMicrotasksAndFetchCompletions;
var
  DidWork: Boolean;
  GC: TGarbageCollector;
  Queue: TGocciaMicrotaskQueue;
  Manager: TGocciaFetchManager;
begin
  GC := TGarbageCollector.Instance;
  try
    if Assigned(GC) then
      GC.ClearKeptObjects;

    repeat
      DidWork := False;

      Queue := TGocciaMicrotaskQueue.Instance;
      if Assigned(Queue) and Queue.HasPending then
      begin
        Queue.DrainQueue;
        DidWork := True;
      end;

      Manager := TGocciaFetchManager.Instance;
      if Assigned(Manager) and Manager.HasPending and
         (Manager.PumpCompletions > 0) then
        DidWork := True;

      if PumpAtomicsWaitAsyncCompletions > 0 then
        DidWork := True;
    until not DidWork;
  finally
    if Assigned(GC) then
      GC.ClearKeptObjects;
  end;
end;

function WaitForFetchPromise(const APromise: TGocciaPromiseValue): Boolean;
var
  Manager: TGocciaFetchManager;
  HasPendingFetch: Boolean;
begin
  if not Assigned(APromise) then
    Exit(False);

  while APromise.State = gpsPending do
  begin
    DrainMicrotasksAndFetchCompletions;
    if APromise.State <> gpsPending then
      Exit(True);

    Manager := TGocciaFetchManager.Instance;
    HasPendingFetch := Assigned(Manager) and Manager.HasPending;
    if not HasPendingFetch and not HasPendingAtomicsWaitAsyncCompletions then
      Exit(False);

    Sleep(FETCH_POLL_INTERVAL_MS);
  end;

  Result := True;
end;

procedure WaitForFetchIdle;
var
  Manager: TGocciaFetchManager;
begin
  Manager := TGocciaFetchManager.Instance;
  if Assigned(Manager) and Manager.HasPending then
    Manager.WaitForIdle
  else
    DrainMicrotasksAndFetchCompletions;
end;

procedure DiscardFetchCompletions;
var
  Manager: TGocciaFetchManager;
begin
  Manager := TGocciaFetchManager.Instance;
  if Assigned(Manager) then
    Manager.DiscardPending;
end;

end.
