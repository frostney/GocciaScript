program Goccia.MicrotaskQueue.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.MicrotaskQueue,
  Goccia.TestSetup,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TTrackingMicrotaskJob = class(TGocciaMicrotaskJob)
  public
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TFailingCaptureMicrotaskJob = class(TTrackingMicrotaskJob)
  public
    procedure CaptureRoots(
      const AContainer: TGocciaObjectValue); override;
  end;

  TMicrotaskQueueTests = class(TTestSuite)
  private
    procedure ExpectOuterScopeJobWaits(const ACleanupJob: Boolean);
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;

    procedure TestFreesJobWhenRootCaptureFails;
    procedure TestOwnsJobAfterSuccessfulEnqueue;
    procedure TestInnerScopeLeavesOuterJobsAlone;
    procedure TestJobForOuterScopeWaitsThere;
    procedure TestCleanupForOuterScopeWaitsThere;
    procedure TestJobForEndedScopeUsesCurrentScope;
    procedure TestLeaveScopeRestoresEntryDepth;
    procedure TestTakesTrackedRejectionsOldestFirst;
    procedure TestHandledRejectionIsNoLongerTracked;
    procedure TestClearQueueForgetsTrackedRejections;
    procedure TestRejectionForOuterScopeWaitsThere;
  end;

var
  GDestroyedJobCount: Integer;
  GExecutedJobCount: Integer;

function EmptyMicrotask: TGocciaMicrotask;
begin
  Result.Handler := nil;
  Result.ResultPromise := nil;
  Result.Value := nil;
  Result.ReactionType := prtFulfill;
  Result.Context := nil;
end;

{ TTrackingMicrotaskJob }

destructor TTrackingMicrotaskJob.Destroy;
begin
  Inc(GDestroyedJobCount);
  inherited;
end;

procedure TTrackingMicrotaskJob.Execute;
begin
  Inc(GExecutedJobCount);
end;

{ TFailingCaptureMicrotaskJob }

procedure TFailingCaptureMicrotaskJob.CaptureRoots(
  const AContainer: TGocciaObjectValue);
begin
  raise Exception.Create('capture failed');
end;

{ TMicrotaskQueueTests }

procedure TMicrotaskQueueTests.SetupTests;
begin
  Test('EnqueueJob frees a job when root capture fails',
    TestFreesJobWhenRootCaptureFails);
  Test('EnqueueJob owns a job after successful enqueue',
    TestOwnsJobAfterSuccessfulEnqueue);
  Test('an inner scope neither runs nor discards the outer scope''s jobs',
    TestInnerScopeLeavesOuterJobsAlone);
  Test('a job for an outer scope waits there while an inner scope is current',
    TestJobForOuterScopeWaitsThere);
  Test('a cleanup job for an outer scope waits there while an inner scope ' +
    'is current',
    TestCleanupForOuterScopeWaitsThere);
  Test('a job for a scope that has ended lands in the current scope',
    TestJobForEndedScopeUsesCurrentScope);
  Test('LeaveScope restores the depth it was entered at',
    TestLeaveScopeRestoresEntryDepth);
  Test('the oldest tracked rejection is taken and the rest forgotten',
    TestTakesTrackedRejectionsOldestFirst);
  Test('a rejection that gets a handler is no longer tracked',
    TestHandledRejectionIsNoLongerTracked);
  Test('ClearQueue forgets the tracked rejections',
    TestClearQueueForgetsTrackedRejections);
  Test('a rejection for an outer scope waits there while an inner scope ' +
    'is current',
    TestRejectionForOuterScopeWaitsThere);
end;

procedure TMicrotaskQueueTests.BeforeEach;
begin
  GDestroyedJobCount := 0;
  GExecutedJobCount := 0;
end;

procedure TMicrotaskQueueTests.TestFreesJobWhenRootCaptureFails;
var
  ErrorMessage: string;
  Queue: TGocciaMicrotaskQueue;
  RaisedExpected: Boolean;
begin
  Queue := TGocciaMicrotaskQueue.Create;
  try
    ErrorMessage := '';
    RaisedExpected := False;
    try
      Queue.EnqueueJob(TFailingCaptureMicrotaskJob.Create);
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        RaisedExpected := True;
      end;
    end;
    Expect<Boolean>(RaisedExpected).ToBe(True);
    Expect<string>(ErrorMessage).ToBe('capture failed');
    Expect<Integer>(GDestroyedJobCount).ToBe(1);
  finally
    Queue.Free;
  end;
  Expect<Integer>(GDestroyedJobCount).ToBe(1);
end;

procedure TMicrotaskQueueTests.TestOwnsJobAfterSuccessfulEnqueue;
var
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Create;
  try
    Queue.EnqueueJob(TTrackingMicrotaskJob.Create);
    Expect<Integer>(GDestroyedJobCount).ToBe(0);
    Queue.ClearQueue;
    Expect<Integer>(GDestroyedJobCount).ToBe(1);
  finally
    Queue.Free;
  end;
  Expect<Integer>(GDestroyedJobCount).ToBe(1);
end;

procedure TMicrotaskQueueTests.TestInnerScopeLeavesOuterJobsAlone;
var
  Queue: TGocciaMicrotaskQueue;
  Token: Integer;
begin
  Queue := TGocciaMicrotaskQueue.Create;
  try
    Queue.EnqueueJob(TTrackingMicrotaskJob.Create);

    Token := Queue.EnterScope;
    Expect<Boolean>(Queue.HasPending).ToBe(False);
    Queue.EnqueueJob(TTrackingMicrotaskJob.Create);
    Queue.DrainQueue;
    Expect<Integer>(GExecutedJobCount).ToBe(1);
    Expect<Integer>(GDestroyedJobCount).ToBe(1);

    Queue.EnqueueJob(TTrackingMicrotaskJob.Create);
    Queue.ClearQueue;
    Expect<Integer>(GDestroyedJobCount).ToBe(2);

    // Left queued, as by a run that failed before its drain.
    Queue.EnqueueJob(TTrackingMicrotaskJob.Create);
    Queue.LeaveScope(Token);
    Expect<Integer>(GExecutedJobCount).ToBe(1);
    Expect<Integer>(GDestroyedJobCount).ToBe(3);

    Expect<Boolean>(Queue.HasPending).ToBe(True);
    Queue.DrainQueue;
    Expect<Integer>(GExecutedJobCount).ToBe(2);
    Expect<Integer>(GDestroyedJobCount).ToBe(4);
  finally
    Queue.Free;
  end;
end;

procedure TMicrotaskQueueTests.ExpectOuterScopeJobWaits(
  const ACleanupJob: Boolean);
var
  OuterScope: TGocciaMicrotaskScopeId;
  Queue: TGocciaMicrotaskQueue;
  Token: Integer;
begin
  Queue := TGocciaMicrotaskQueue.Create;
  try
    OuterScope := Queue.CurrentScope;
    Token := Queue.EnterScope;
    Expect<Boolean>(Queue.CurrentScope <> OuterScope).ToBe(True);

    if ACleanupJob then
      Queue.EnqueueFinalizationCleanup(EmptyMicrotask, OuterScope)
    else
      Queue.EnqueueInScope(EmptyMicrotask, nil, OuterScope);
    Expect<Boolean>(Queue.HasPending).ToBe(False);
    Queue.ClearQueue;

    Queue.LeaveScope(Token);
    Expect<Boolean>(Queue.CurrentScope = OuterScope).ToBe(True);
    Expect<Boolean>(Queue.HasPending).ToBe(True);
    Expect<Boolean>(Queue.DrainOneJob).ToBe(True);
    Expect<Boolean>(Queue.HasPending).ToBe(False);
  finally
    Queue.Free;
  end;
end;

procedure TMicrotaskQueueTests.TestJobForOuterScopeWaitsThere;
begin
  ExpectOuterScopeJobWaits(False);
end;

procedure TMicrotaskQueueTests.TestCleanupForOuterScopeWaitsThere;
begin
  ExpectOuterScopeJobWaits(True);
end;

procedure TMicrotaskQueueTests.TestJobForEndedScopeUsesCurrentScope;
var
  EndedScope: TGocciaMicrotaskScopeId;
  Queue: TGocciaMicrotaskQueue;
  Token: Integer;
begin
  Queue := TGocciaMicrotaskQueue.Create;
  try
    Token := Queue.EnterScope;
    EndedScope := Queue.CurrentScope;
    Queue.LeaveScope(Token);

    Queue.EnqueueInScope(EmptyMicrotask, nil, EndedScope);
    Expect<Boolean>(Queue.HasPending).ToBe(True);
    Queue.ClearQueue;

    // A later scope at the same depth is not the ended one.
    Token := Queue.EnterScope;
    Expect<Boolean>(Queue.CurrentScope <> EndedScope).ToBe(True);
    Queue.EnqueueFinalizationCleanup(EmptyMicrotask, EndedScope);
    Expect<Boolean>(Queue.HasPending).ToBe(True);
    Queue.LeaveScope(Token);
    Expect<Boolean>(Queue.HasPending).ToBe(False);
  finally
    Queue.Free;
  end;
end;

procedure TMicrotaskQueueTests.TestLeaveScopeRestoresEntryDepth;
var
  OuterScope: TGocciaMicrotaskScopeId;
  Queue: TGocciaMicrotaskQueue;
  Token: Integer;
begin
  Queue := TGocciaMicrotaskQueue.Create;
  try
    OuterScope := Queue.CurrentScope;
    Queue.EnqueueJob(TTrackingMicrotaskJob.Create);

    Token := Queue.EnterScope;
    // Never left: an inner run that unwound without its LeaveScope.
    Queue.EnterScope;
    Queue.EnqueueJob(TTrackingMicrotaskJob.Create);

    Queue.LeaveScope(Token);
    Expect<Boolean>(Queue.CurrentScope = OuterScope).ToBe(True);
    Expect<Integer>(GDestroyedJobCount).ToBe(1);
    Queue.DrainQueue;
    Expect<Integer>(GExecutedJobCount).ToBe(1);
  finally
    Queue.Free;
  end;
end;

procedure TMicrotaskQueueTests.TestTakesTrackedRejectionsOldestFirst;
var
  First, Second, Taken: TGocciaValue;
  Queue: TGocciaMicrotaskQueue;
begin
  // The tracker only needs identity, so any value stands in for a promise.
  First := TGocciaObjectValue.Create(nil);
  Second := TGocciaObjectValue.Create(nil);
  Queue := TGocciaMicrotaskQueue.Create;
  try
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(False);

    Queue.TrackRejection(First, Queue.CurrentScope);
    Queue.TrackRejection(Second, Queue.CurrentScope);
    // Tracked rejections are not jobs: there is nothing to drain.
    Expect<Boolean>(Queue.HasPending).ToBe(False);

    // The oldest is the report; the others are forgotten with it.
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(True);
    Expect<Boolean>(Taken = First).ToBe(True);
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(False);

    // Tracking the same promise twice keeps its first place in the order.
    Queue.TrackRejection(Second, Queue.CurrentScope);
    Queue.TrackRejection(First, Queue.CurrentScope);
    Queue.TrackRejection(Second, Queue.CurrentScope);
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(True);
    Expect<Boolean>(Taken = Second).ToBe(True);
  finally
    Queue.Free;
  end;
end;

procedure TMicrotaskQueueTests.TestHandledRejectionIsNoLongerTracked;
var
  First, Second, Taken: TGocciaValue;
  Queue: TGocciaMicrotaskQueue;
begin
  First := TGocciaObjectValue.Create(nil);
  Second := TGocciaObjectValue.Create(nil);
  Queue := TGocciaMicrotaskQueue.Create;
  try
    Queue.TrackRejection(First, Queue.CurrentScope);
    Queue.TrackRejection(Second, Queue.CurrentScope);
    Queue.UntrackRejection(First);
    // Untracking twice, or something never tracked, is harmless.
    Queue.UntrackRejection(First);

    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(True);
    Expect<Boolean>(Taken = Second).ToBe(True);
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(False);
  finally
    Queue.Free;
  end;
end;

procedure TMicrotaskQueueTests.TestClearQueueForgetsTrackedRejections;
var
  Rejected, Taken: TGocciaValue;
  Queue: TGocciaMicrotaskQueue;
begin
  Rejected := TGocciaObjectValue.Create(nil);
  Queue := TGocciaMicrotaskQueue.Create;
  try
    Queue.TrackRejection(Rejected, Queue.CurrentScope);
    Queue.ClearQueue;
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(False);
  finally
    Queue.Free;
  end;
end;

procedure TMicrotaskQueueTests.TestRejectionForOuterScopeWaitsThere;
var
  InnerRejected, OuterRejected, Taken: TGocciaValue;
  OuterScope: TGocciaMicrotaskScopeId;
  Queue: TGocciaMicrotaskQueue;
  Token: Integer;
begin
  InnerRejected := TGocciaObjectValue.Create(nil);
  OuterRejected := TGocciaObjectValue.Create(nil);
  Queue := TGocciaMicrotaskQueue.Create;
  try
    OuterScope := Queue.CurrentScope;
    Token := Queue.EnterScope;

    // A promise the outer engine owns, rejected while the inner one runs.
    Queue.TrackRejection(OuterRejected, OuterScope);
    Queue.TrackRejection(InnerRejected, Queue.CurrentScope);

    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(True);
    Expect<Boolean>(Taken = InnerRejected).ToBe(True);
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(False);

    // The inner scope's own leftovers end with it.
    Queue.TrackRejection(InnerRejected, Queue.CurrentScope);
    Queue.LeaveScope(Token);

    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(True);
    Expect<Boolean>(Taken = OuterRejected).ToBe(True);
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(False);

    // A handler can arrive while the owner's scope is hidden.
    Queue.TrackRejection(OuterRejected, OuterScope);
    Token := Queue.EnterScope;
    Queue.UntrackRejection(OuterRejected);
    Queue.LeaveScope(Token);
    Expect<Boolean>(Queue.TakeUnhandledRejection(Taken)).ToBe(False);
  finally
    Queue.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TMicrotaskQueueTests.Create('MicrotaskQueue'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
