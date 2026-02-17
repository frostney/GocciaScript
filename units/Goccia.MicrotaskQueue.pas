unit Goccia.MicrotaskQueue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Generics.Collections;

type
  TPromiseReactionType = (prtFulfill, prtReject);

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
  Task: TGocciaMicrotask;
  Promise: TGocciaPromiseValue;
  HandlerResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  while FQueue.Count > 0 do
  begin
    Task := FQueue[0];
    FQueue.Delete(0);

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
            begin
              if Assigned(Promise) then
                Promise.Reject(E.Value);
            end;
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
end;

function TGocciaMicrotaskQueue.HasPending: Boolean;
begin
  Result := FQueue.Count > 0;
end;

end.
