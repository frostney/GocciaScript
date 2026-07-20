program Goccia.MicrotaskQueue.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.MicrotaskQueue,
  Goccia.TestSetup,
  Goccia.Values.ObjectValue;

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
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;

    procedure TestFreesJobWhenRootCaptureFails;
    procedure TestOwnsJobAfterSuccessfulEnqueue;
  end;

var
  GDestroyedJobCount: Integer;

{ TTrackingMicrotaskJob }

destructor TTrackingMicrotaskJob.Destroy;
begin
  Inc(GDestroyedJobCount);
  inherited;
end;

procedure TTrackingMicrotaskJob.Execute;
begin
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
end;

procedure TMicrotaskQueueTests.BeforeEach;
begin
  GDestroyedJobCount := 0;
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

begin
  TestRunnerProgram.AddSuite(
    TMicrotaskQueueTests.Create('MicrotaskQueue'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
