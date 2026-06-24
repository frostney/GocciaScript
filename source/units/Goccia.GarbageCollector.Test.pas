program Goccia.GarbageCollector.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}

  Goccia.GarbageCollector,
  TestingPascalLibrary,

  Goccia.TestSetup;

type
  TChildManaged = class(TGCManagedObject)
  public
    destructor Destroy; override;
  end;

  TParentManaged = class(TGCManagedObject)
  private
    FChild: TChildManaged;
  public
    destructor Destroy; override;
    procedure MarkReferences; override;
    property Child: TChildManaged read FChild write FChild;
  end;

  TTestGarbageCollector = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestCollectYoungTracesOldToYoungReferences;
  end;

var
  GParentDestructorCount: Integer;
  GChildDestructorCount: Integer;

destructor TChildManaged.Destroy;
begin
  Inc(GChildDestructorCount);
  inherited;
end;

destructor TParentManaged.Destroy;
begin
  Inc(GParentDestructorCount);
  inherited;
end;

procedure TParentManaged.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FChild) then
    FChild.MarkReferences;
end;

procedure TTestGarbageCollector.SetupTests;
begin
  Test('CollectYoung traces references from old rooted objects',
    TestCollectYoungTracesOldToYoungReferences);
end;

procedure TTestGarbageCollector.TestCollectYoungTracesOldToYoungReferences;
var
  GC: TGarbageCollector;
  Parent: TParentManaged;
  Child: TChildManaged;
  Watermark: Integer;
begin
  GParentDestructorCount := 0;
  GChildDestructorCount := 0;
  GC := TGarbageCollector.Instance;

  Parent := TParentManaged.Create;
  GC.RegisterObject(Parent);
  GC.AddRootObject(Parent);
  try
    GC.Collect;
    Watermark := GC.Watermark;

    Child := TChildManaged.Create;
    GC.RegisterObject(Child);
    Parent.Child := Child;

    GC.CollectYoung(Watermark);

    Expect<Integer>(GChildDestructorCount).ToBe(0);

    Parent.Child := nil;
    GC.RemoveRootObject(Parent);
    GC.Collect;

    Expect<Integer>(GParentDestructorCount).ToBe(1);
    Expect<Integer>(GChildDestructorCount).ToBe(1);
  finally
    if GParentDestructorCount = 0 then
      GC.RemoveRootObject(Parent);
  end;
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestGarbageCollector.Create('GarbageCollector'));
    TestRunnerProgram.Run;
  finally
    TGarbageCollector.Shutdown;
  end;
  ExitCode := TestResultToExitCode;
end.
