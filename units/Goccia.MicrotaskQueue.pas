unit Goccia.MicrotaskQueue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Generics.Collections;

type
  TPromiseReactionType = (prtFulfill, prtReject, prtThenableResolve);

  TGocciaMicrotask = record
    Handler: TGocciaValue;
    ResultPromise: TGocciaValue;
    Value: TGocciaValue;
    ReactionType: TPromiseReactionType;
  end;

  TGocciaMicrotaskQueue = class
  private class var
    FInstance: TGocciaMicrotaskQueue;
  private
    FQueue: TList<TGocciaMicrotask>;
  public
    class function Instance: TGocciaMicrotaskQueue;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(const AMicrotask: TGocciaMicrotask);
    procedure DrainQueue;
    procedure ClearQueue;
    function HasPending: Boolean;
  end;

implementation

uses
  Goccia.Values.FunctionBase, Goccia.Arguments.Collection,
  Goccia.Values.PromiseValue, Goccia.Values.Error,
  Goccia.GarbageCollector, SysUtils;

class function TGocciaMicrotaskQueue.Instance: TGocciaMicrotaskQueue;
begin
  Result := FInstance;
end;

class procedure TGocciaMicrotaskQueue.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaMicrotaskQueue.Create;
end;

class procedure TGocciaMicrotaskQueue.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TGocciaMicrotaskQueue.Create;
begin
  FQueue := TList<TGocciaMicrotask>.Create;
end;

destructor TGocciaMicrotaskQueue.Destroy;
begin
  FQueue.Free;
  inherited;
end;

procedure TGocciaMicrotaskQueue.Enqueue(const AMicrotask: TGocciaMicrotask);
begin
  FQueue.Add(AMicrotask);
end;

procedure TGocciaMicrotaskQueue.DrainQueue;
var
  I: Integer;
  Task: TGocciaMicrotask;
  Promise: TGocciaPromiseValue;
  HandlerResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  I := 0;
  while I < FQueue.Count do
  begin
    Task := FQueue[I];
    Inc(I);

    Promise := TGocciaPromiseValue(Task.ResultPromise);

    if Assigned(TGocciaGC.Instance) then
    begin
      if Assigned(Task.Handler) then
        TGocciaGC.Instance.AddTempRoot(Task.Handler);
      if Assigned(Task.Value) then
        TGocciaGC.Instance.AddTempRoot(Task.Value);
      if Assigned(Promise) then
        TGocciaGC.Instance.AddTempRoot(Promise);
    end;

    try
      if Assigned(Task.Handler) and Task.Handler.IsCallable then
      begin
        CallArgs := TGocciaArgumentsCollection.Create([Task.Value]);
        try
          try
            HandlerResult := TGocciaFunctionBase(Task.Handler).Call(
              CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
            if Assigned(Promise) then
              Promise.Resolve(HandlerResult);
          except
            on E: TGocciaThrowValue do
              if Assigned(Promise) then
                Promise.Reject(E.Value);
              // TODO: Per HTML spec, queueMicrotask callback errors should be
              // "reported" (Node.js: uncaughtException, browsers: global error
              // event). Currently silently discarded because GocciaScript has no
              // process-level error reporting. Add reporting when/if an error
              // event mechanism is implemented.
          end;
        finally
          CallArgs.Free;
        end;
      end
      else
      begin
        if Assigned(Promise) then
        begin
          case Task.ReactionType of
            prtFulfill: Promise.Resolve(Task.Value);
            prtReject: Promise.Reject(Task.Value);
            prtThenableResolve:
              if Task.Value is TGocciaPromiseValue then
                Promise.SubscribeTo(TGocciaPromiseValue(Task.Value));
          end;
        end;
      end;
    finally
      if Assigned(TGocciaGC.Instance) then
      begin
        if Assigned(Task.Handler) then
          TGocciaGC.Instance.RemoveTempRoot(Task.Handler);
        if Assigned(Task.Value) then
          TGocciaGC.Instance.RemoveTempRoot(Task.Value);
        if Assigned(Promise) then
          TGocciaGC.Instance.RemoveTempRoot(Promise);
      end;
    end;
  end;
  if I > 0 then
    FQueue.Clear;

end;

procedure TGocciaMicrotaskQueue.ClearQueue;
var
  I: Integer;
  Task: TGocciaMicrotask;
begin
  if Assigned(TGocciaGC.Instance) then
    for I := 0 to FQueue.Count - 1 do
    begin
      Task := FQueue[I];
      if Assigned(Task.Handler) then
        TGocciaGC.Instance.RemoveTempRoot(Task.Handler);
    end;
  FQueue.Clear;
end;

function TGocciaMicrotaskQueue.HasPending: Boolean;
begin
  Result := FQueue.Count > 0;
end;

end.
