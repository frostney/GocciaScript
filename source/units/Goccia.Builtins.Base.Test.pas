program Goccia.Builtins.Base.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}

  Classes,
  SysUtils,

  Goccia.Builtins.Base,
  Goccia.GarbageCollector,
  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

type
  { Reports its own sweep. Created with a nil prototype: the suite runs with
    no realm, so the builtins under test make their objects without one. }
  TCountedObjectValue = class(TGocciaObjectValue)
  public
    destructor Destroy; override;
  end;

  { Hangs a counted child on its builtin object and collects before the
    constructor returns. That is the shape of a builtin constructor that loads
    a shim before anything else holds FBuiltinObject:
    TGocciaTestAssertions.Create reads the global Date, which loads the Date
    shim as bytecode, which reaches the VM's memory-pressure checkpoint. }
  TCollectingBuiltin = class(TGocciaBuiltin)
  public
    constructor Create;
  end;

  { Replaces FBuiltinObject in a constructor that does not chain to
    TGocciaBuiltin.Create, as TGocciaPerformance does. }
  TReplacingBuiltin = class(TGocciaBuiltin)
  public
    constructor Create;
  end;

  TTestBuiltinBase = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestBuiltinObjectSurvivesCollectionDuringConstruction;
    procedure TestBuiltinObjectSurvivesCollectionAfterConstruction;
    procedure TestReplacedBuiltinObjectIsRooted;
  end;

var
  GCountedValueDestructorCount: Integer;

destructor TCountedObjectValue.Destroy;
begin
  Inc(GCountedValueDestructorCount);
  inherited;
end;

procedure AttachCountedChild(const AObject: TGocciaObjectValue);
begin
  AObject.DefineProperty('child', TGocciaPropertyDescriptorData.Create(
    TCountedObjectValue.Create, [pfWritable, pfConfigurable]));
end;

constructor TCollectingBuiltin.Create;
begin
  inherited Create('Collecting', nil, nil);
  AttachCountedChild(FBuiltinObject);
  TGarbageCollector.Instance.Collect;
end;

constructor TReplacingBuiltin.Create;
begin
  FName := 'Replacing';
  FBuiltinObject := TGocciaObjectValue.Create;
  AttachCountedChild(FBuiltinObject);
end;

procedure TTestBuiltinBase.SetupTests;
begin
  Test('A builtin object survives a collection inside its constructor',
    TestBuiltinObjectSurvivesCollectionDuringConstruction);
  Test('A builtin object stays rooted until the builtin is freed',
    TestBuiltinObjectSurvivesCollectionAfterConstruction);
  Test('A builtin object a constructor replaces is rooted in its place',
    TestReplacedBuiltinObjectIsRooted);
end;

procedure TTestBuiltinBase.TestBuiltinObjectSurvivesCollectionDuringConstruction;
var
  Builtin: TCollectingBuiltin;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;

  Builtin := TCollectingBuiltin.Create;
  try
    Expect<Integer>(GCountedValueDestructorCount).ToBe(0);
  finally
    Builtin.Free;
  end;

  { Freeing the builtin releases its root, so the object is collectable. }
  GC.Collect;
  Expect<Integer>(GCountedValueDestructorCount).ToBe(1);
end;

procedure TTestBuiltinBase.TestBuiltinObjectSurvivesCollectionAfterConstruction;
var
  Builtin: TGocciaBuiltin;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;

  { Not every builtin binds its object in a scope; the engine and runtime
    extensions read it later through BuiltinObject. }
  Builtin := TGocciaBuiltin.Create('Unbound', nil, nil);
  try
    AttachCountedChild(Builtin.BuiltinObject);
    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(0);
  finally
    Builtin.Free;
  end;

  GC.Collect;
  Expect<Integer>(GCountedValueDestructorCount).ToBe(1);
end;

procedure TTestBuiltinBase.TestReplacedBuiltinObjectIsRooted;
var
  Builtin: TReplacingBuiltin;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  GCountedValueDestructorCount := 0;

  Builtin := TReplacingBuiltin.Create;
  try
    GC.Collect;
    Expect<Integer>(GCountedValueDestructorCount).ToBe(0);
  finally
    Builtin.Free;
  end;

  GC.Collect;
  Expect<Integer>(GCountedValueDestructorCount).ToBe(1);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestBuiltinBase.Create('Builtins.Base'));
    RunGocciaTests;
  finally
    TGarbageCollector.Shutdown;
  end;
  ExitCode := TestResultToExitCode;
end.
