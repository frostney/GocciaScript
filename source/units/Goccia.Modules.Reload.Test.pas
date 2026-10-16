program Goccia.Modules.Reload.Test;

{ A module the loader has cached is reloaded when its source changes: the
  next request for it links and evaluates the new version into the module
  record importers already hold. These tests reload modules again and again
  in bytecode mode and collect garbage between reloads, so a reload that
  leaves anything pointing at a freed record, or an environment nothing
  roots, fails here. }

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  OrderedStringMap,
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.TestSetup,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TInMemoryModuleResolver = class(TGocciaModuleResolver)
  public
    function Resolve(const AModulePath, AImportingFilePath: string): string; override;
  end;

  TMemoryModuleEntry = record
    LastModified: TDateTime;
    Text: string;
  end;

  TMemoryModuleContentProvider = class(TGocciaModuleContentProvider)
  private
    FEntries: TOrderedStringMap<TMemoryModuleEntry>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetModule(const APath, AText: string;
      const ALastModified: TDateTime);
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

  { An engine over a module loader whose in-memory modules a test
    rewrites. Each Define stamps its text newer than everything defined
    before it, so the next Load of a module already loaded reloads it. }
  TModuleReloadFixture = class
  private
    FEngine: TGocciaEngine;
    FLoader: TGocciaModuleLoader;
    FProvider: TMemoryModuleContentProvider;
    FResolver: TInMemoryModuleResolver;
    FSource: TStringList;
    FVersion: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Define(const APath, AText: string);
    function Load(const APath: string): TGocciaModule;
    function ExportValue(const AModule: TGocciaModule;
      const AName: string): TGocciaValue;
    function CallExport(const AModule: TGocciaModule;
      const AName: string): TGocciaValue;
  end;

  TModuleReloadTests = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestRepeatedReloadsKeepImportersAndOldClosures;
    procedure TestReloadLinkingAnImporterOfItselfBindsTheCachedModule;
    procedure TestReloadThatFailsToLinkKeepsThePreviousVersion;
    procedure TestRepeatedDataModuleReloadsReplaceTheCachedValues;
    procedure TestRepeatedReloadsDoNotGrowTheManagedHeap;
  end;

const
  RELOAD_ENTRY_PATH = 'memory:/app.mjs';

function TInMemoryModuleResolver.Resolve(const AModulePath,
  AImportingFilePath: string): string;
begin
  Result := AModulePath;
end;

constructor TMemoryModuleContentProvider.Create;
begin
  inherited Create;
  FEntries := TOrderedStringMap<TMemoryModuleEntry>.Create;
end;

destructor TMemoryModuleContentProvider.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TMemoryModuleContentProvider.SetModule(const APath, AText: string;
  const ALastModified: TDateTime);
var
  Entry: TMemoryModuleEntry;
begin
  Entry.Text := AText;
  Entry.LastModified := ALastModified;
  FEntries.AddOrSetValue(APath, Entry);
end;

function TMemoryModuleContentProvider.Exists(const APath: string): Boolean;
var
  Entry: TMemoryModuleEntry;
begin
  Result := FEntries.TryGetValue(APath, Entry);
end;

function TMemoryModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
var
  Entry: TMemoryModuleEntry;
begin
  if not FEntries.TryGetValue(APath, Entry) then
    raise Exception.Create('Module content not found: ' + APath);
  Result := TGocciaModuleContent.Create(Entry.Text, Entry.LastModified);
end;

function TMemoryModuleContentProvider.TryGetLastModified(const APath: string;
  out ALastModified: TDateTime): Boolean;
var
  Entry: TMemoryModuleEntry;
begin
  Result := FEntries.TryGetValue(APath, Entry);
  if Result then
    ALastModified := Entry.LastModified
  else
    ALastModified := 0;
end;

constructor TModuleReloadFixture.Create;
begin
  inherited Create;
  FProvider := TMemoryModuleContentProvider.Create;
  FResolver := TInMemoryModuleResolver.Create;
  FSource := TStringList.Create;
  FSource.Text := '1;';
  FLoader := TGocciaModuleLoader.Create(RELOAD_ENTRY_PATH, FResolver,
    FProvider);
  FEngine := TGocciaEngine.Create(RELOAD_ENTRY_PATH, FSource, FLoader);
  FEngine.Compatibility := [cfFunction];
end;

destructor TModuleReloadFixture.Destroy;
begin
  FEngine.Free;
  FLoader.Free;
  FSource.Free;
  FResolver.Free;
  FProvider.Free;
  inherited;
end;

procedure TModuleReloadFixture.Define(const APath, AText: string);
begin
  Inc(FVersion);
  FProvider.SetModule(APath, AText, EncodeDate(2026, 1, 1) + FVersion);
end;

function TModuleReloadFixture.Load(const APath: string): TGocciaModule;
begin
  Result := FLoader.LoadModule(APath, RELOAD_ENTRY_PATH);
end;

function TModuleReloadFixture.ExportValue(const AModule: TGocciaModule;
  const AName: string): TGocciaValue;
begin
  if not AModule.TryGetExportValue(AName, Result) then
    raise Exception.CreateFmt('Module %s has no export %s',
      [AModule.Path, AName]);
end;

function CallWithThis(const AFunction, AThis: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.Create;
  try
    Result := TGocciaFunctionBase(AFunction).Call(Args, AThis);
  finally
    Args.Free;
  end;
end;

function CallMethod(const AObject: TGocciaValue;
  const AName: string): TGocciaValue;
begin
  Result := CallWithThis(TGocciaObjectValue(AObject).GetProperty(AName),
    AObject);
end;

function TModuleReloadFixture.CallExport(const AModule: TGocciaModule;
  const AName: string): TGocciaValue;
begin
  Result := CallWithThis(ExportValue(AModule, AName),
    TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure CollectGarbage;
begin
  TGarbageCollector.Instance.Collect;
end;

procedure TModuleReloadTests.SetupTests;
begin
  Test('Repeated reloads keep importers live and old closures on their version',
    TestRepeatedReloadsKeepImportersAndOldClosures);
  Test('A reload that links a module importing it back binds the cached record',
    TestReloadLinkingAnImporterOfItselfBindsTheCachedModule);
  Test('A reload that fails to link keeps the previous version',
    TestReloadThatFailsToLinkKeepsThePreviousVersion);
  Test('Repeated JSON module reloads replace the values importers read',
    TestRepeatedDataModuleReloadsReplaceTheCachedValues);
  Test('Two hundred reloads do not grow the managed heap',
    TestRepeatedReloadsDoNotGrowTheManagedHeap);
end;

{ Importers read the newest version through their import bindings. A function
  or class instance the previous version created keeps reading the
  environment it was created in: a reload replaces the module's environment,
  it does not rewrite the old one. }
procedure TModuleReloadTests.TestRepeatedReloadsKeepImportersAndOldClosures;
const
  COUNTER_PATH = 'memory:/counter.js';
  IMPORTER_PATH = 'memory:/importer.js';
var
  Counter: TGocciaModule;
  Fixture: TModuleReloadFixture;
  Importer: TGocciaModule;
  Namespace: TGocciaModuleNamespaceObject;
  OldBox: TGocciaValue;
  OldGet: TGocciaValue;
begin
  Fixture := TModuleReloadFixture.Create;
  try
    Fixture.Define(COUNTER_PATH,
      'export const n = 1;' + sLineBreak +
      'export function get() { return n; }' + sLineBreak +
      'export class Box { value() { return n; } }');
    Fixture.Define(IMPORTER_PATH,
      'import { n, get, Box } from "' + COUNTER_PATH + '";' + sLineBreak +
      'export const readN = () => n;' + sLineBreak +
      'export const callGet = () => get();' + sLineBreak +
      'export const makeBox = () => new Box();');

    Importer := Fixture.Load(IMPORTER_PATH);
    Counter := Fixture.Load(COUNTER_PATH);
    Namespace := TGocciaModuleNamespaceObject(Counter.GetNamespaceObject);
    OldGet := Fixture.ExportValue(Counter, 'get');
    OldBox := Fixture.CallExport(Importer, 'makeBox');
    TGarbageCollector.Instance.AddTempRoot(OldGet);
    TGarbageCollector.Instance.AddTempRoot(OldBox);
    try
      Expect<Double>(Fixture.CallExport(Importer, 'readN')
        .ToNumberLiteral.Value).ToBe(1);
      Expect<Double>(Fixture.CallExport(Importer, 'callGet')
        .ToNumberLiteral.Value).ToBe(1);

      Fixture.Define(COUNTER_PATH,
        'export const n = 2;' + sLineBreak +
        'export function get() { return n * 10; }' + sLineBreak +
        'export class Box { value() { return n * 100; } }');
      Expect<Boolean>(Fixture.Load(COUNTER_PATH) = Counter).ToBe(True);
      CollectGarbage;
      Expect<Double>(Fixture.CallExport(Importer, 'readN')
        .ToNumberLiteral.Value).ToBe(2);
      Expect<Double>(Fixture.CallExport(Importer, 'callGet')
        .ToNumberLiteral.Value).ToBe(20);
      Expect<Double>(CallMethod(Fixture.CallExport(Importer, 'makeBox'),
        'value').ToNumberLiteral.Value).ToBe(200);
      Expect<Double>(CallWithThis(OldGet,
        TGocciaUndefinedLiteralValue.UndefinedValue).ToNumberLiteral.Value)
        .ToBe(1);
      Expect<Double>(CallMethod(OldBox, 'value').ToNumberLiteral.Value)
        .ToBe(1);

      { The third version drops one export and adds another. }
      Fixture.Define(COUNTER_PATH,
        'export const n = 3;' + sLineBreak +
        'export const extra = 4;' + sLineBreak +
        'export class Box { value() { return n * 1000; } }');
      Expect<Boolean>(Fixture.Load(COUNTER_PATH) = Counter).ToBe(True);
      CollectGarbage;
      Expect<Double>(Fixture.CallExport(Importer, 'readN')
        .ToNumberLiteral.Value).ToBe(3);
      Expect<Double>(CallMethod(Fixture.CallExport(Importer, 'makeBox'),
        'value').ToNumberLiteral.Value).ToBe(3000);
      Expect<Boolean>(Namespace.CanResolveExport('get')).ToBe(False);
      Expect<Boolean>(Namespace.CanResolveExport('extra')).ToBe(True);
      Expect<Double>(Namespace.GetProperty('extra').ToNumberLiteral.Value)
        .ToBe(4);
      Expect<Double>(CallWithThis(OldGet,
        TGocciaUndefinedLiteralValue.UndefinedValue).ToNumberLiteral.Value)
        .ToBe(1);
      Expect<Double>(CallMethod(OldBox, 'value').ToNumberLiteral.Value)
        .ToBe(1);

      Fixture.Define(COUNTER_PATH,
        'export const n = 4;' + sLineBreak +
        'export function get() { return n * 10; }' + sLineBreak +
        'export class Box { value() { return n * 100; } }');
      Expect<Boolean>(Fixture.Load(COUNTER_PATH) = Counter).ToBe(True);
      CollectGarbage;
      Expect<Double>(Fixture.CallExport(Importer, 'readN')
        .ToNumberLiteral.Value).ToBe(4);
      Expect<Double>(Fixture.CallExport(Importer, 'callGet')
        .ToNumberLiteral.Value).ToBe(40);
      Expect<Boolean>(Namespace.CanResolveExport('extra')).ToBe(False);
      Expect<Double>(CallMethod(OldBox, 'value').ToNumberLiteral.Value)
        .ToBe(1);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(OldBox);
      TGarbageCollector.Instance.RemoveTempRoot(OldGet);
    end;
  finally
    Fixture.Free;
  end;
end;

{ The second version imports a module nobody loaded yet, which imports the
  reloading module back. Linking that peer binds it to the record the cache
  holds, so it reads every later version too. }
procedure TModuleReloadTests.TestReloadLinkingAnImporterOfItselfBindsTheCachedModule;
const
  PEER_PATH = 'memory:/peer.js';
  SUBJECT_PATH = 'memory:/subject.js';
var
  Fixture: TModuleReloadFixture;
  Subject: TGocciaModule;

  function SubjectVersion(const AName: string): string;
  begin
    Result := 'import { readSubject } from "' + PEER_PATH + '";' +
      sLineBreak +
      'export const name = "' + AName + '";' + sLineBreak +
      'export const viaPeer = () => readSubject();';
  end;

begin
  Fixture := TModuleReloadFixture.Create;
  try
    Fixture.Define(SUBJECT_PATH, 'export const name = "first";');
    Subject := Fixture.Load(SUBJECT_PATH);
    Fixture.Define(PEER_PATH,
      'import { name } from "' + SUBJECT_PATH + '";' + sLineBreak +
      'export const readSubject = () => name;');

    Fixture.Define(SUBJECT_PATH, SubjectVersion('second'));
    Expect<Boolean>(Fixture.Load(SUBJECT_PATH) = Subject).ToBe(True);
    CollectGarbage;
    Expect<string>(Fixture.CallExport(Subject, 'viaPeer')
      .ToStringLiteral.Value).ToBe('second');

    Fixture.Define(SUBJECT_PATH, SubjectVersion('third'));
    Expect<Boolean>(Fixture.Load(SUBJECT_PATH) = Subject).ToBe(True);
    CollectGarbage;
    Expect<string>(Fixture.CallExport(Subject, 'viaPeer')
      .ToStringLiteral.Value).ToBe('third');
    Expect<string>(Fixture.CallExport(Fixture.Load(PEER_PATH), 'readSubject')
      .ToStringLiteral.Value).ToBe('third');
  finally
    Fixture.Free;
  end;
end;

{ A version that cannot link (it imports a module that does not exist)
  fails its request and leaves the version before it in place, still
  marked out of date, so a later request reloads the version after it. }
procedure TModuleReloadTests.TestReloadThatFailsToLinkKeepsThePreviousVersion;
const
  IMPORTER_PATH = 'memory:/importer.js';
  SUBJECT_PATH = 'memory:/subject.js';
var
  Failed: Boolean;
  Fixture: TModuleReloadFixture;
  Importer: TGocciaModule;
  Subject: TGocciaModule;
begin
  Fixture := TModuleReloadFixture.Create;
  try
    Fixture.Define(SUBJECT_PATH,
      'export const value = "first";' + sLineBreak +
      'export function read() { return value; }');
    Fixture.Define(IMPORTER_PATH,
      'import { value } from "' + SUBJECT_PATH + '";' + sLineBreak +
      'export const readValue = () => value;');
    Importer := Fixture.Load(IMPORTER_PATH);
    Subject := Fixture.Load(SUBJECT_PATH);

    Fixture.Define(SUBJECT_PATH,
      'import { missing } from "memory:/absent.js";' + sLineBreak +
      'export const value = "broken";');
    Failed := False;
    try
      Fixture.Load(SUBJECT_PATH);
    except
      on Exception do
        Failed := True;
    end;
    Expect<Boolean>(Failed).ToBe(True);
    CollectGarbage;
    Expect<string>(Fixture.CallExport(Importer, 'readValue')
      .ToStringLiteral.Value).ToBe('first');
    Expect<string>(Fixture.CallExport(Subject, 'read')
      .ToStringLiteral.Value).ToBe('first');

    Fixture.Define(SUBJECT_PATH,
      'export const value = "third";' + sLineBreak +
      'export function read() { return value; }');
    Expect<Boolean>(Fixture.Load(SUBJECT_PATH) = Subject).ToBe(True);
    CollectGarbage;
    Expect<string>(Fixture.CallExport(Importer, 'readValue')
      .ToStringLiteral.Value).ToBe('third');
    Expect<string>(Fixture.CallExport(Subject, 'read')
      .ToStringLiteral.Value).ToBe('third');
  finally
    Fixture.Free;
  end;
end;

{ A data module is parsed again into a new record holding only values,
  which the cached record takes over before the new one is freed. }
procedure TModuleReloadTests.TestRepeatedDataModuleReloadsReplaceTheCachedValues;
const
  DATA_PATH = 'memory:/data.json';
  IMPORTER_PATH = 'memory:/importer.js';
var
  Data: TGocciaModule;
  Fixture: TModuleReloadFixture;
  I: Integer;
  Importer: TGocciaModule;
begin
  Fixture := TModuleReloadFixture.Create;
  try
    Fixture.Define(DATA_PATH, '{ "value": 1 }');
    Fixture.Define(IMPORTER_PATH,
      'import data from "' + DATA_PATH + '";' +
      sLineBreak +
      'import * as fields from "' + DATA_PATH + '";' +
      sLineBreak +
      'export const read = () => data.value * 10 + fields.value;');
    Importer := Fixture.Load(IMPORTER_PATH);
    Data := Fixture.Load(DATA_PATH);
    Expect<Double>(Fixture.CallExport(Importer, 'read')
      .ToNumberLiteral.Value).ToBe(11);

    for I := 2 to 4 do
    begin
      Fixture.Define(DATA_PATH, '{ "value": ' + IntToStr(I) + ' }');
      Expect<Boolean>(Fixture.Load(DATA_PATH) = Data).ToBe(True);
      CollectGarbage;
      Expect<Double>(Fixture.CallExport(Importer, 'read')
        .ToNumberLiteral.Value).ToBe(11 * I);
    end;
  finally
    Fixture.Free;
  end;
end;

{ Each reload replaces the module's environment. Once nothing references a
  replaced environment, the collector frees it with everything created in
  it. Every version here creates PAYLOAD_OBJECTS objects, so a version kept
  alive would add at least that many live objects per reload. What a reload
  does keep is the version's compiled code, which the engine retains for its
  lifetime as it does every compiled program, with the string constants its
  functions have used: a few objects per reload, far fewer than one
  version's payload. }
procedure TModuleReloadTests.TestRepeatedReloadsDoNotGrowTheManagedHeap;
const
  IMPORTER_PATH = 'memory:/importer.js';
  PAYLOAD_OBJECTS = 50;
  RELOADS = 200;
  SETTLED_AFTER = 20;
  SUBJECT_PATH = 'memory:/subject.js';
var
  Fixture: TModuleReloadFixture;
  I: Integer;
  Importer: TGocciaModule;
  SettledObjects: Integer;
begin
  Fixture := TModuleReloadFixture.Create;
  try
    Fixture.Define(IMPORTER_PATH,
      'import { n, get, Box } from "' + SUBJECT_PATH + '";' + sLineBreak +
      'export const read = () => n + get() + new Box().value();');
    Importer := nil;
    SettledObjects := 0;
    for I := 1 to RELOADS do
    begin
      Fixture.Define(SUBJECT_PATH,
        'export const n = ' + IntToStr(I) + ';' + sLineBreak +
        'export const payload = Array.from({ length: ' +
          IntToStr(PAYLOAD_OBJECTS) + ' }, () => ({ n }));' + sLineBreak +
        'export const label = "version " + n;' + sLineBreak +
        'export function get() { return payload[0].n; }' + sLineBreak +
        'export class Box { value() { return payload[1].n; } }');
      if I = 1 then
        Importer := Fixture.Load(IMPORTER_PATH)
      else
        Fixture.Load(SUBJECT_PATH);
      Expect<Double>(Fixture.CallExport(Importer, 'read')
        .ToNumberLiteral.Value).ToBe(3 * I);
      if I = SETTLED_AFTER then
      begin
        CollectGarbage;
        SettledObjects := TGarbageCollector.Instance.ManagedObjectCount;
      end;
    end;
    CollectGarbage;
    Expect<Boolean>(TGarbageCollector.Instance.ManagedObjectCount -
      SettledObjects < (RELOADS - SETTLED_AFTER) * PAYLOAD_OBJECTS div 10)
      .ToBe(True);
  finally
    Fixture.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TModuleReloadTests.Create('Module Reload'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
