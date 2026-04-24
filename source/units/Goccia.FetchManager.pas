unit Goccia.FetchManager;

{$I Goccia.inc}

interface

uses
  HTTPClient,

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

uses
  Classes,
  Generics.Collections,
  SyncObjs,
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HeadersValue,
  Goccia.Values.ResponseValue;

const
  FETCH_WORKER_STACK_SIZE = 8 * 1024 * 1024;
  FETCH_POLL_INTERVAL_MS = 1;

type
  TGocciaFetchWorker = class;
  TGocciaFetchManagerImpl = class;

  TGocciaFetchCompletion = class
  public
    RequestID: Integer;
    Success: Boolean;
    Response: THTTPResponse;
    ErrorMessage: string;
    constructor Create(const ARequestID: Integer);
  end;

  TGocciaPendingFetch = record
    RequestID: Integer;
    Promise: TGocciaPromiseValue;
    Worker: TGocciaFetchWorker;
  end;

  TGocciaFetchWorker = class(TThread)
  private
    FManager: TGocciaFetchManagerImpl;
    FRequestID: Integer;
    FURL: string;
    FMethod: string;
    FHeaders: THTTPHeaders;
  protected
    procedure Execute; override;
  public
    constructor Create(const AManager: TGocciaFetchManagerImpl;
      const ARequestID: Integer; const AURL, AMethod: string;
      const AHeaders: THTTPHeaders);
  end;

  TGocciaFetchManagerImpl = class(TGocciaFetchManager)
  private
    FLock: TRTLCriticalSection;
    FCompletions: TList<TGocciaFetchCompletion>;
    FPending: TList<TGocciaPendingFetch>;
    FNextRequestID: Integer;
    function PopCompletion(out ACompletion: TGocciaFetchCompletion): Boolean;
    function FindPendingIndex(const ARequestID: Integer): Integer;
    procedure PostCompletion(const ACompletion: TGocciaFetchCompletion);
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

threadvar
  FetchManagerThreadInstance: TGocciaFetchManagerImpl;

{ TGocciaFetchCompletion }

constructor TGocciaFetchCompletion.Create(const ARequestID: Integer);
begin
  inherited Create;
  RequestID := ARequestID;
  Success := False;
  ErrorMessage := '';
end;

{ TGocciaFetchWorker }

constructor TGocciaFetchWorker.Create(const AManager: TGocciaFetchManagerImpl;
  const ARequestID: Integer; const AURL, AMethod: string;
  const AHeaders: THTTPHeaders);
begin
  inherited Create(True, FETCH_WORKER_STACK_SIZE);
  FreeOnTerminate := False;
  FManager := AManager;
  FRequestID := ARequestID;
  FURL := AURL;
  FMethod := AMethod;
  FHeaders := AHeaders;
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

    FManager.PostCompletion(Completion);
    Completion := nil;
  finally
    Completion.Free;
  end;
end;

{ TGocciaFetchManager }

class function TGocciaFetchManager.Instance: TGocciaFetchManager;
begin
  Result := FetchManagerThreadInstance;
end;

class procedure TGocciaFetchManager.Initialize;
begin
  if not Assigned(FetchManagerThreadInstance) then
    FetchManagerThreadInstance := TGocciaFetchManagerImpl.Create;
end;

class procedure TGocciaFetchManager.Shutdown;
begin
  FreeAndNil(FetchManagerThreadInstance);
end;

{ TGocciaFetchManagerImpl }

constructor TGocciaFetchManagerImpl.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
  FCompletions := TList<TGocciaFetchCompletion>.Create;
  FPending := TList<TGocciaPendingFetch>.Create;
  FNextRequestID := 1;
end;

destructor TGocciaFetchManagerImpl.Destroy;
begin
  DiscardPending;
  FPending.Free;
  FCompletions.Free;
  DoneCriticalSection(FLock);
  inherited;
end;

procedure TGocciaFetchManagerImpl.StartFetch(const AURL, AMethod: string;
  const AHeaders: THTTPHeaders; const APromise: TGocciaPromiseValue);
var
  Pending: TGocciaPendingFetch;
  Added: Boolean;
begin
  Added := False;
  Pending.RequestID := FNextRequestID;
  Inc(FNextRequestID);
  Pending.Promise := APromise;
  Pending.Worker := TGocciaFetchWorker.Create(Self, Pending.RequestID,
    AURL, AMethod, AHeaders);

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(APromise);

  try
    FPending.Add(Pending);
    Added := True;
    Pending.Worker.Start;
  except
    if Added then
      FPending.Delete(FPending.Count - 1);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(APromise);
    Pending.Worker.Free;
    raise;
  end;
end;

function TGocciaFetchManagerImpl.PopCompletion(
  out ACompletion: TGocciaFetchCompletion): Boolean;
begin
  ACompletion := nil;
  EnterCriticalSection(FLock);
  try
    Result := FCompletions.Count > 0;
    if Result then
    begin
      ACompletion := FCompletions[0];
      FCompletions.Delete(0);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
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

procedure TGocciaFetchManagerImpl.PostCompletion(
  const ACompletion: TGocciaFetchCompletion);
begin
  EnterCriticalSection(FLock);
  try
    FCompletions.Add(ACompletion);
  finally
    LeaveCriticalSection(FLock);
  end;
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
    if Assigned(Pending.Worker) then
      Pending.Worker.WaitFor;

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

    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Pending.Promise);
    Pending.Worker.Free;
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
  Pending: TGocciaPendingFetch;
  Completion: TGocciaFetchCompletion;
begin
  for I := 0 to FPending.Count - 1 do
  begin
    Pending := FPending[I];
    if Assigned(Pending.Worker) then
      Pending.Worker.WaitFor;
  end;

  for I := 0 to FPending.Count - 1 do
  begin
    Pending := FPending[I];
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Pending.Promise);
    Pending.Worker.Free;
  end;
  FPending.Clear;

  EnterCriticalSection(FLock);
  try
    for I := 0 to FCompletions.Count - 1 do
    begin
      Completion := FCompletions[I];
      Completion.Free;
    end;
    FCompletions.Clear;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure DrainMicrotasksAndFetchCompletions;
var
  DidWork: Boolean;
  Queue: TGocciaMicrotaskQueue;
  Manager: TGocciaFetchManager;
begin
  repeat
    DidWork := False;

    Queue := TGocciaMicrotaskQueue.Instance;
    if Assigned(Queue) and Queue.HasPending then
    begin
      Queue.DrainQueue;
      DidWork := True;
    end;

    Manager := TGocciaFetchManager.Instance;
    if Assigned(Manager) and (Manager.PumpCompletions > 0) then
      DidWork := True;
  until not DidWork;
end;

function WaitForFetchPromise(const APromise: TGocciaPromiseValue): Boolean;
var
  Manager: TGocciaFetchManager;
begin
  DrainMicrotasksAndFetchCompletions;
  if not Assigned(APromise) or (APromise.State <> gpsPending) then
    Exit(Assigned(APromise));

  Manager := TGocciaFetchManager.Instance;
  if not Assigned(Manager) then
    Exit(False);
  Result := Manager.WaitForPromise(APromise);
end;

procedure WaitForFetchIdle;
var
  Manager: TGocciaFetchManager;
begin
  Manager := TGocciaFetchManager.Instance;
  if Assigned(Manager) then
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
