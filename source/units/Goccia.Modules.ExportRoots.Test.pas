{ Regression gate for modules rooting the values they hold directly (ADR 0133).

  A value-bound export (TGocciaModule.AddExportValue) has no environment scope
  to root it and no source module to own it. The only other path that marked it
  was the module namespace object, which exists only once something asks for
  it: a host module such as the sandbox's `fs`, imported with
  `import fs from "fs"`, never builds one. The collector then freed the exported
  value while the module still handed it out, and the next import check that
  read it (CanResolveExport) followed a freed object.

  The probe value records its own destruction, so the test does not depend on
  freed memory being reused: the export must survive a collection for as long
  as its module lives, and must be released once the module is gone. }

program Goccia.Modules.ExportRoots.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}

  SysUtils,

  Goccia.GarbageCollector,
  Goccia.Modules,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  { Reports its own sweep. A nil prototype keeps the test free of engine setup. }
  TProbeObjectValue = class(TGocciaObjectValue)
  public
    destructor Destroy; override;
  end;

  TModuleExportRootsTests = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestValueExportSurvivesCollection;
    procedure TestUpdatedValueExportSurvivesCollection;
    procedure TestExportsTableValueSurvivesCollection;
    procedure TestReplacedLocalSnapshotSurvivesCollection;
    procedure TestValueExportReleasedWithModule;
    procedure TestModuleRootsFollowAReplacedCollector;
    procedure TestModuleCreatedBeforeCollectorRootsItsExports;
  end;

var
  GProbeDestroyed: Integer;

destructor TProbeObjectValue.Destroy;
begin
  Inc(GProbeDestroyed);
  inherited;
end;

procedure TModuleExportRootsTests.SetupTests;
begin
  Test('a value-bound export survives a collection while its module lives',
    TestValueExportSurvivesCollection);
  Test('an export value replaced by UpdateExportValue survives a collection',
    TestUpdatedValueExportSurvivesCollection);
  Test('a value a host module writes straight into the export table survives a collection',
    TestExportsTableValueSurvivesCollection);
  Test('a local export''s snapshot stays alive after the binding is reassigned',
    TestReplacedLocalSnapshotSurvivesCollection);
  Test('a value-bound export is released once its module is freed',
    TestValueExportReleasedWithModule);
  // Last two: they replace the thread's collector.
  Test('a module kept across a collector replacement roots values added afterwards',
    TestModuleRootsFollowAReplacedCollector);
  Test('a module created before the thread has a collector still roots its exports',
    TestModuleCreatedBeforeCollectorRootsItsExports);
end;

procedure TModuleExportRootsTests.TestValueExportSurvivesCollection;
var
  Module: TGocciaModule;
  Value: TGocciaValue;
begin
  TGarbageCollector.Instance.Collect;
  GProbeDestroyed := 0;
  Module := TGocciaModule.Create('memory:/host-module');
  try
    Module.AddExportValue('default', TProbeObjectValue.Create(nil));
    TGarbageCollector.Instance.Collect;
    Expect<Integer>(GProbeDestroyed).ToBe(0);
    Expect<Boolean>(Module.CanResolveExport('default')).ToBe(True);
    Expect<Boolean>(Module.TryGetExportValue('default', Value)).ToBe(True);
    Expect<Boolean>(Value is TProbeObjectValue).ToBe(True);
  finally
    Module.Free;
  end;
  TGarbageCollector.Instance.Collect;
end;

procedure TModuleExportRootsTests.TestUpdatedValueExportSurvivesCollection;
var
  Module: TGocciaModule;
  Value: TGocciaValue;
begin
  TGarbageCollector.Instance.Collect;
  GProbeDestroyed := 0;
  Module := TGocciaModule.Create('memory:/updated-module');
  try
    Module.UpdateExportValue('value', TProbeObjectValue.Create(nil));
    TGarbageCollector.Instance.Collect;
    Expect<Integer>(GProbeDestroyed).ToBe(0);
    Expect<Boolean>(Module.TryGetExportValue('value', Value)).ToBe(True);
    Expect<Boolean>(Value is TProbeObjectValue).ToBe(True);
  finally
    Module.Free;
  end;
  TGarbageCollector.Instance.Collect;
end;

procedure TModuleExportRootsTests.TestExportsTableValueSurvivesCollection;
var
  Module: TGocciaModule;
  Value: TGocciaValue;
begin
  // The YAML, TOML, JSON5 and indexed-data modules publish this way, with no
  // export binding behind the entry.
  TGarbageCollector.Instance.Collect;
  GProbeDestroyed := 0;
  Module := TGocciaModule.Create('memory:/data-module');
  try
    Module.ExportsTable.AddOrSetValue('default', TProbeObjectValue.Create(nil));
    TGarbageCollector.Instance.Collect;
    Expect<Integer>(GProbeDestroyed).ToBe(0);
    Expect<Boolean>(Module.TryGetExportValue('default', Value)).ToBe(True);
    Expect<Boolean>(Value is TProbeObjectValue).ToBe(True);
  finally
    Module.Free;
  end;
  TGarbageCollector.Instance.Collect;
end;

procedure TModuleExportRootsTests.TestReplacedLocalSnapshotSurvivesCollection;
var
  Module: TGocciaModule;
  Scope: TGocciaScope;
begin
  // Linking a local export snapshots its value into the export table, and the
  // namespace object built later marks that table. Reassigning the binding
  // does not refresh the snapshot, so the first value must stay alive while
  // the table still names it.
  TGarbageCollector.Instance.Collect;
  GProbeDestroyed := 0;
  Module := TGocciaModule.Create('memory:/local-module');
  try
    Scope := TGocciaScope.Create(nil, skModule, 'ModuleExportRootsTest');
    Module.SetEnvironment(Scope);
    Scope.DefineLexicalBinding('value', TProbeObjectValue.Create(nil), dtLet);
    Module.AddExportBinding('value', 'value', Scope);
    Scope.ForceUpdateBinding('value', TProbeObjectValue.Create(nil));
    TGarbageCollector.Instance.Collect;
    Expect<Integer>(GProbeDestroyed).ToBe(0);
    Module.GetNamespaceObject;
    TGarbageCollector.Instance.Collect;
    Expect<Integer>(GProbeDestroyed).ToBe(0);
  finally
    Module.Free;
  end;
  TGarbageCollector.Instance.Collect;
end;

procedure TModuleExportRootsTests.TestValueExportReleasedWithModule;
var
  Module: TGocciaModule;
begin
  TGarbageCollector.Instance.Collect;
  GProbeDestroyed := 0;
  Module := TGocciaModule.Create('memory:/released-module');
  Module.AddExportValue('default', TProbeObjectValue.Create(nil));
  Module.Free;
  TGarbageCollector.Instance.Collect;
  Expect<Integer>(GProbeDestroyed).ToBe(1);
end;

procedure TModuleExportRootsTests.TestModuleRootsFollowAReplacedCollector;
var
  Module: TGocciaModule;
begin
  // A thread pool that resets its runtime between work items while the host
  // keeps the module.
  Module := TGocciaModule.Create('memory:/kept-module');
  try
    TGarbageCollector.Shutdown;
    TGarbageCollector.Initialize;
    GProbeDestroyed := 0;
    Module.ExportsTable.AddOrSetValue('default', TProbeObjectValue.Create(nil));
    Module.UpdateExportValue('updated', TProbeObjectValue.Create(nil));
    TGarbageCollector.Instance.Collect;
    Expect<Integer>(GProbeDestroyed).ToBe(0);
  finally
    Module.Free;
  end;
  TGarbageCollector.Instance.Collect;
  Expect<Integer>(GProbeDestroyed).ToBe(2);
end;

procedure TModuleExportRootsTests.TestModuleCreatedBeforeCollectorRootsItsExports;
var
  Module: TGocciaModule;
begin
  // An embedder that builds a host module before creating its engine.
  TGarbageCollector.Shutdown;
  Module := TGocciaModule.Create('memory:/early-module');
  try
    Expect<Boolean>(Assigned(TGarbageCollector.Instance)).ToBe(True);
    GProbeDestroyed := 0;
    Module.ExportsTable.AddOrSetValue('default', TProbeObjectValue.Create(nil));
    TGarbageCollector.Instance.Collect;
    Expect<Integer>(GProbeDestroyed).ToBe(0);
  finally
    Module.Free;
  end;
  TGarbageCollector.Instance.Collect;
  Expect<Integer>(GProbeDestroyed).ToBe(1);
end;

begin
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  try
    TestRunnerProgram.AddSuite(
      TModuleExportRootsTests.Create('ModuleExportRoots'));
    RunGocciaTests;
  finally
    TGarbageCollector.Shutdown;
  end;

  ExitCode := TestResultToExitCode;
end.
