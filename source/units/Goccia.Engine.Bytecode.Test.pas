program Goccia.Engine.Bytecode.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.Engine,
  Goccia.Modules.Loader,
  Goccia.Sandbox.Modules,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

const
  COUNTER_SOURCE =
    'import { seed } from "./seed.js";' +
    'export let count = seed;' +
    'export const increment = () => { count += 1; };' +
    'export default "counter";';
  BARREL_SOURCE =
    'export { default, count, increment } from "./counter.js";';
  ENTRY_SOURCE =
    'import label, { count, increment } from "./barrel.js";' +
    'increment(); globalThis.answer = label + ":" + count;';

type
  TEngineBytecodeTests = class(TTestSuite)
  private
    function CompileBuffer(const ASource, ACompileAddress: string;
      const ACompatibility: TGocciaCompatibilityFlags = [];
      const AStrictTypes: Boolean = False;
      const AImportable: Boolean = True): TBytes;
    procedure RegisterAndRelease(const AEngine: TGocciaEngine;
      const AAddress, ASource: string;
      const ACompatibility: TGocciaCompatibilityFlags = []);
    function RunGraph(const APrecompiled, ABufferedEntry: Boolean): string;
    procedure TestRegisteredGraphMatchesSourceAndOwnsBuffers;
    procedure TestBufferedEntryUsesPrecompiledImports;
    procedure TestBufferedURIEntryResolvesRelativeImports;
    procedure TestStandaloneBufferedEntryUsesRuntimeAddress;
    procedure TestSourceEntryImportsExactVirtualAddress;
    procedure TestRelativeAliasesAndRelocatedImportMeta;
    procedure TestCyclicFunctionDeclarations;
    procedure TestAnonymousDefaultFunction;
    procedure TestCyclicLexicalTDZ;
    procedure TestProfileMismatchBeforeEffects;
    procedure TestChangedProfileBeforeLinking;
    procedure TestStrictTypesExecutionParity;
    procedure AssertLegacyRunRejected(const AMatchingProfile: Boolean;
      const AExpectedReason: string);
    procedure TestLegacyRunRejectsLoadedProfileMismatch;
    procedure TestLegacyRunRejectsLinkedArtifact;
    procedure AssertDeferredDiskRetention(const AAccess, AInvalid: Boolean);
    procedure TestDeferredDiskScanReleasesBytecode;
    procedure TestDeferredDiskExecutionRetainsOneBody;
    procedure TestDeferredDiskFailureReleasesBytecode;
    procedure AssertSandboxEntryAddress(const ALinked: Boolean);
    procedure TestLinkedSandboxEntryWithoutSource;
    procedure TestStandaloneSandboxEntryWithoutSource;
  public
    procedure SetupTests; override;
  end;

function RuntimeAddress(const AName: string): string;
begin
  Result := ExpandFileName('aot-runtime' + PathDelim + AName);
end;

procedure TEngineBytecodeTests.SetupTests;
begin
  Test('Registered bytecode matches source with live bindings and source fallback',
    TestRegisteredGraphMatchesSourceAndOwnsBuffers);
  Test('An entry buffer resolves registered precompiled imports',
    TestBufferedEntryUsesPrecompiledImports);
  Test('A URI-addressed entry buffer resolves relative registered imports',
    TestBufferedURIEntryResolvesRelativeImports);
  Test('A standalone entry buffer relocates relative imports to its runtime address',
    TestStandaloneBufferedEntryUsesRuntimeAddress);
  Test('A source entry imports bytecode registered at an exact virtual address',
    TestSourceEntryImportsExactVirtualAddress);
  Test('Relative registration and aliases use the runtime import.meta address',
    TestRelativeAliasesAndRelocatedImportMeta);
  Test('Anonymous default function declarations survive bytecode linking',
    TestAnonymousDefaultFunction);
  Test('Cyclic bytecode modules initialize function declarations before evaluation',
    TestCyclicFunctionDeclarations);
  Test('Cyclic bytecode modules retain lexical temporal dead zones',
    TestCyclicLexicalTDZ);
  Test('Profile mismatches refuse registration and execution before effects',
    TestProfileMismatchBeforeEffects);
  Test('Changing a registered module profile refuses linking before dependencies run',
    TestChangedProfileBeforeLinking);
  Test('Strict type checks agree between source and buffered modules',
    TestStrictTypesExecutionParity);
  Test('Legacy execution APIs reject mismatched loaded profiles before effects',
    TestLegacyRunRejectsLoadedProfileMismatch);
  Test('Legacy execution APIs refuse linked artifacts without a runtime address',
    TestLegacyRunRejectsLinkedArtifact);
  Test('Deferred disk scans release their temporary bytecode',
    TestDeferredDiskScanReleasesBytecode);
  Test('Deferred disk execution retains one library body',
    TestDeferredDiskExecutionRetainsOneBody);
  Test('Failed deferred disk scans release their temporary bytecode',
    TestDeferredDiskFailureReleasesBytecode);
  Test('Linked source-free sandbox entries retain their provider address',
    TestLinkedSandboxEntryWithoutSource);
  Test('Standalone source-free sandbox entries retain their provider address',
    TestStandaloneSandboxEntryWithoutSource);
end;

{ A compilation engine is destroyed before any consumer uses these bytes.
  The consumer receives neither the source nor the compiler's templates. }
function TEngineBytecodeTests.CompileBuffer(const ASource,
  ACompileAddress: string;
  const ACompatibility: TGocciaCompatibilityFlags;
  const AStrictTypes, AImportable: Boolean): TBytes;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Options: TGocciaSourcePipelineOptions;
  Parsed: TGocciaSourcePipelineResult;
  Module: TGocciaBytecodeModule;
begin
  Source := TStringList.Create;
  try
    Source.Text := ASource;
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.Preprocessors := TGocciaEngine.DefaultPreprocessors;
    Options.SourceType := stModule;
    Options.Compatibility := ACompatibility;
    Parsed := TGocciaSourcePipeline.Parse(Source, ACompileAddress, Options);
    try
      Engine := TGocciaEngine.Create(ACompileAddress, Source);
      try
        Engine.SourceType := stModule;
        Engine.Compatibility := ACompatibility;
        Engine.StrictTypes := AStrictTypes;
        if AImportable then
          Module := Engine.CompileImportableModule(Parsed.ProgramNode)
        else
          Module := Engine.CompileModule(Parsed.ProgramNode);
        Result := SaveModuleToBuffer(Module);
      finally
        Engine.Free;
      end;
    finally
      Parsed.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.RegisterAndRelease(const AEngine: TGocciaEngine;
  const AAddress, ASource: string;
  const ACompatibility: TGocciaCompatibilityFlags);
var
  Bytes: TBytes;
begin
  Bytes := CompileBuffer(ASource, 'memory:/compile/library.js', ACompatibility);
  AEngine.RegisterBytecodeModule(AAddress, Bytes);
  { The registry must own its bytes, not share a caller's mutable array. }
  FillChar(Bytes[0], Length(Bytes), 0);
  SetLength(Bytes, 0);
end;

function TEngineBytecodeTests.RunGraph(const APrecompiled,
  ABufferedEntry: Boolean): string;
var
  Engine: TGocciaEngine;
  Source: TStringList;
  EntryBytes: TBytes;
begin
  Source := TStringList.Create;
  try
    if not ABufferedEntry then
      Source.Text := ENTRY_SOURCE;
    Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
    try
      Engine.SourceType := stModule;
      { This dependency has no compiled artifact in either run. }
      Engine.ModuleLoader.InjectModule(RuntimeAddress('seed.js'),
        'export const seed = 40;');
      if APrecompiled then
      begin
        RegisterAndRelease(Engine, RuntimeAddress('counter.js'), COUNTER_SOURCE);
        RegisterAndRelease(Engine, RuntimeAddress('barrel.js'), BARREL_SOURCE);
      end
      else
      begin
        Engine.ModuleLoader.InjectModule(RuntimeAddress('counter.js'),
          COUNTER_SOURCE);
        Engine.ModuleLoader.InjectModule(RuntimeAddress('barrel.js'),
          BARREL_SOURCE);
      end;
      if ABufferedEntry then
      begin
        EntryBytes := CompileBuffer(ENTRY_SOURCE, 'memory:/compile/main.js');
        Engine.RunBytecodeBuffer(EntryBytes, RuntimeAddress('main.js'));
      end
      else
        Engine.Execute;
      Result := TGocciaObjectValue(Engine.Realm.GlobalObject).GetProperty('answer')
        .ToStringLiteral.Value;
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestRegisteredGraphMatchesSourceAndOwnsBuffers;
var
  FromSource: string;
begin
  FromSource := RunGraph(False, False);
  Expect<string>(FromSource).ToBe('counter:41');
  Expect<string>(RunGraph(True, False)).ToBe(FromSource);
end;

procedure TEngineBytecodeTests.TestBufferedEntryUsesPrecompiledImports;
begin
  Expect<string>(RunGraph(True, True)).ToBe('counter:41');
end;

procedure TEngineBytecodeTests.TestBufferedURIEntryResolvesRelativeImports;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Bytes: TBytes;
begin
  Source := TStringList.Create;
  try
    Engine := TGocciaEngine.Create('memory:/runtime/main.js', Source);
    try
      Engine.SourceType := stModule;
      RegisterAndRelease(Engine, 'memory:/runtime/dep.js',
        'export const value = 42;');
      Bytes := CompileBuffer('import { value } from "./dep.js";' +
        'globalThis.answer = value;', 'memory:/compile/main.js');
      Engine.RunBytecodeBuffer(Bytes, 'memory:/runtime/main.js');
      Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
        .GetProperty('answer').ToStringLiteral.Value).ToBe('42');
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestStandaloneBufferedEntryUsesRuntimeAddress;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Bytes: TBytes;
begin
  Source := TStringList.Create;
  try
    Engine := TGocciaEngine.Create('memory:/runtime/main.js', Source);
    try
      Engine.SourceType := stModule;
      RegisterAndRelease(Engine, 'memory:/runtime/dep.js',
        'export const value = 42;');
      Bytes := CompileBuffer('import { value } from "./dep.js";' +
        'globalThis.answer = value;', 'memory:/compile/main.js', [], False, False);
      Engine.RunBytecodeBuffer(Bytes, 'memory:/runtime/main.js');
      Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
        .GetProperty('answer').ToStringLiteral.Value).ToBe('42');
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestSourceEntryImportsExactVirtualAddress;
var
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  try
    Source.Text := 'import { value } from "virtual:library";' +
      'globalThis.answer = value;';
    Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
    try
      Engine.SourceType := stModule;
      RegisterAndRelease(Engine, 'virtual:library', 'export const value = 7;');
      Engine.Execute;
      Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
        .GetProperty('answer').ToStringLiteral.Value).ToBe('7');
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestRelativeAliasesAndRelocatedImportMeta;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Bytes: TBytes;
begin
  Source := TStringList.Create;
  try
    Source.Text := 'import { where } from "library";' +
      'globalThis.answer = where;';
    Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
    try
      Engine.SourceType := stModule;
      Bytes := CompileBuffer('export const where = import.meta.url;',
        'memory:/compile/original.js');
      Engine.RegisterBytecodeModule('./nested/../library.js', Bytes,
        RuntimeAddress('main.js'));
      Engine.Resolver.AddAlias('library', RuntimeAddress('library.js'));
      Engine.Execute;
      Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject).GetProperty('answer')
        .ToStringLiteral.Value).ToBe(RuntimeAddress('library.js'));
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestCyclicFunctionDeclarations;
var
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  try
    Source.Text := 'import { answer } from "./a.js";' +
      'globalThis.answer = answer;';
    Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
    try
      Engine.SourceType := stModule;
      Engine.Compatibility := [cfFunction];
      RegisterAndRelease(Engine, RuntimeAddress('a.js'),
        'import { getB } from "./b.js";' +
        'export function getA() { return "a"; }' +
        'export const answer = getB();', [cfFunction]);
      RegisterAndRelease(Engine, RuntimeAddress('b.js'),
        'import { getA } from "./a.js";' +
        'export function getB() { return getA() + "b"; }', [cfFunction]);
      Engine.Execute;
      Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject).GetProperty('answer')
        .ToStringLiteral.Value).ToBe('ab');
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestAnonymousDefaultFunction;
var
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  try
    Source.Text := 'import fn from "./default.js";' +
      'globalThis.answer = fn.name + ":" + fn();';
    Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
    try
      Engine.SourceType := stModule;
      Engine.Compatibility := [cfFunction];
      RegisterAndRelease(Engine, RuntimeAddress('default.js'),
        'export default function () { return "value"; }', [cfFunction]);
      Engine.Execute;
      Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject).GetProperty('answer')
        .ToStringLiteral.Value).ToBe('default:value');
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestCyclicLexicalTDZ;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Reason: string;
begin
  Source := TStringList.Create;
  try
    Source.Text := 'import "./a.js"; globalThis.executed = true;';
    Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
    try
      Engine.SourceType := stModule;
      RegisterAndRelease(Engine, RuntimeAddress('a.js'),
        'import { b } from "./b.js"; export const a = b;');
      RegisterAndRelease(Engine, RuntimeAddress('b.js'),
        'import { a } from "./a.js"; export const b = a;');
      Reason := '';
      try
        Engine.Execute;
      except
        on E: TGocciaThrowValue do
          Reason := E.Value.GetProperty('name').ToStringLiteral.Value + ':' +
            E.Value.GetProperty('message').ToStringLiteral.Value;
        on E: EGocciaBytecodeThrow do
          Reason := E.ThrownValue.GetProperty('name').ToStringLiteral.Value + ':' +
            E.ThrownValue.GetProperty('message').ToStringLiteral.Value;
        on E: Exception do
          Reason := E.Message;
      end;
      Expect<Boolean>(Pos('ReferenceError', Reason) > 0).ToBe(True);
      Expect<Boolean>(Pos('before initialization', Reason) > 0).ToBe(True);
      Expect<Boolean>(TGocciaObjectValue(Engine.Realm.GlobalObject).GetProperty('executed') =
        TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestProfileMismatchBeforeEffects;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Bytes: TBytes;
  Reason: string;
begin
  Bytes := CompileBuffer('globalThis.executed = true; export const value = 1;',
    'memory:/compile/profile.js', [cfFunction]);
  Source := TStringList.Create;
  try
    Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
    try
      Engine.SourceType := stModule;
      Reason := '';
      try
        Engine.RegisterBytecodeModule(RuntimeAddress('profile.js'), Bytes);
      except
        on E: Exception do
          Reason := E.Message;
      end;
      Expect<Boolean>(Pos('profile', LowerCase(Reason)) > 0).ToBe(True);
      Expect<Boolean>(Engine.ModuleLoader.VirtualModules.Contains(
        RuntimeAddress('profile.js'))).ToBe(False);
      Reason := '';
      try
        Engine.RunBytecodeBuffer(Bytes, RuntimeAddress('main.js'));
      except
        on E: Exception do
          Reason := E.Message;
      end;
      Expect<Boolean>(Pos('profile', LowerCase(Reason)) > 0).ToBe(True);
      Expect<Boolean>(TGocciaObjectValue(Engine.Realm.GlobalObject).GetProperty('executed') =
        TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineBytecodeTests.TestChangedProfileBeforeLinking;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Bytes: TBytes;
  Reason: string;
  ChangeStrictTypes: Boolean;
begin
  Bytes := CompileBuffer('import "./side-effect.js";' +
    'globalThis.executed = true; export const value = 1;',
    'memory:/compile/profile.js');
  for ChangeStrictTypes := False to True do
  begin
    Source := TStringList.Create;
    try
      Source.Text := 'import "./registered.js";';
      Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
      try
        Engine.SourceType := stModule;
        Engine.RegisterBytecodeModule(RuntimeAddress('registered.js'), Bytes);
        Engine.ModuleLoader.InjectModule(RuntimeAddress('side-effect.js'),
          'globalThis.dependencyRan = true;');
        if ChangeStrictTypes then
          Engine.StrictTypes := True
        else
          Engine.Compatibility := [cfFunction];
        Reason := '';
        try
          Engine.Execute;
        except
          on E: Exception do
            Reason := E.Message;
        end;
        Expect<Boolean>(Pos('profile', LowerCase(Reason)) > 0).ToBe(True);
        Expect<Boolean>(TGocciaObjectValue(Engine.Realm.GlobalObject)
          .GetProperty('executed') =
          TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
        Expect<Boolean>(TGocciaObjectValue(Engine.Realm.GlobalObject)
          .GetProperty('dependencyRan') =
          TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
      finally
        Engine.Free;
      end;
    finally
      Source.Free;
    end;
  end;
end;

procedure TEngineBytecodeTests.TestStrictTypesExecutionParity;
const
  TYPED_SOURCE =
    'const checked = (value: number): number => value;' +
    'try { checked("wrong"); globalThis.answer = "accepted"; }' +
    'catch (error) { globalThis.answer = error.name; }';
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Bytes: TBytes;
  UseBuffer: Boolean;
begin
  Bytes := CompileBuffer(TYPED_SOURCE, 'memory:/compile/typed.js', [], True);
  for UseBuffer := False to True do
  begin
    Source := TStringList.Create;
    try
      if not UseBuffer then
        Source.Text := TYPED_SOURCE;
      Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
      try
        Engine.SourceType := stModule;
        Engine.StrictTypes := True;
        if UseBuffer then
          Engine.RunBytecodeBuffer(Bytes, RuntimeAddress('main.js'))
        else
          Engine.Execute;
        Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
          .GetProperty('answer').ToStringLiteral.Value).ToBe('TypeError');
      finally
        Engine.Free;
      end;
    finally
      Source.Free;
    end;
  end;
end;

procedure TEngineBytecodeTests.AssertLegacyRunRejected(
  const AMatchingProfile: Boolean; const AExpectedReason: string);
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Bytes: TBytes;
  Module: TGocciaBytecodeModule;
  Reason: string;
  UseScope: Boolean;
begin
  { Mismatch exercises a standalone artifact, so rejecting only link plans
    cannot satisfy it. The matching-profile case exercises a linked artifact. }
  Bytes := CompileBuffer('globalThis.executed = true; export const value = 1;',
    'memory:/compile/legacy.js', [cfFunction], False, AMatchingProfile);
  for UseScope := False to True do
  begin
    Source := TStringList.Create;
    try
      Engine := TGocciaEngine.Create(RuntimeAddress('main.js'), Source);
      try
        Engine.SourceType := stModule;
        if AMatchingProfile then
          Engine.Compatibility := [cfFunction];
        Module := LoadModuleFromBuffer(Bytes);
        try
          Reason := '';
          try
            if UseScope then
              Engine.RunModuleInScope(Module, Engine.GlobalScope)
            else
              Engine.RunModule(Module);
          except
            on E: Exception do
              Reason := E.Message;
          end;
          Expect<Boolean>(Pos(AExpectedReason, Reason) > 0).ToBe(True);
          Expect<Boolean>(TGocciaObjectValue(Engine.Realm.GlobalObject)
            .GetProperty('executed') =
            TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
        finally
          Module.Free;
        end;
      finally
        Engine.Free;
      end;
    finally
      Source.Free;
    end;
  end;
end;

procedure TEngineBytecodeTests.TestLegacyRunRejectsLoadedProfileMismatch;
begin
  AssertLegacyRunRejected(False, 'profile');
end;

procedure TEngineBytecodeTests.TestLegacyRunRejectsLinkedArtifact;
begin
  AssertLegacyRunRejected(True, 'RunModuleForSourceType');
end;

procedure TEngineBytecodeTests.AssertDeferredDiskRetention(
  const AAccess, AInvalid: Boolean);
var
  Directory, ArtifactPath, LibrarySource: string;
  Bytes: TBytes;
  Source: TStringList;
  Stream: TFileStream;
  Engine: TGocciaEngine;
  Before, ExpectedRetained: Integer;
  Failed: Boolean;
begin
  Directory := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-aot-retention-' + IntToStr(GetProcessID);
  ForceDirectories(Directory);
  ArtifactPath := IncludeTrailingPathDelimiter(Directory) + 'lazy.gbc';
  if AInvalid then
    LibrarySource := 'import { value } from "./missing.mjs"; export { value };'
  else
    LibrarySource := 'globalThis.lazyRuns = (globalThis.lazyRuns ?? 0) + 1;' +
      'export const value = 42;';
  try
    Bytes := CompileBuffer(LibrarySource, '/compile/lazy.mjs');
    Stream := TFileStream.Create(ArtifactPath, fmCreate);
    try
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
    finally
      Stream.Free;
    end;
    Source := TStringList.Create;
    try
      Source.Text := 'import defer * as lazy from "./lazy.mjs";';
      if AAccess then
        Source.Add('globalThis.answer = lazy.value;');
      Engine := TGocciaEngine.Create(
        IncludeTrailingPathDelimiter(Directory) + 'entry.mjs', Source);
      try
        Engine.SourceType := stModule;
        Engine.ModuleLoader.PreferBytecode := True;
        Before := Engine.RetainedModuleCount;
        Failed := False;
        try
          Engine.Execute;
        except
          on Exception do
            Failed := True;
        end;
        Expect<Boolean>(Failed).ToBe(AInvalid);
        { The entry is compiled once. Only actual namespace access should
          retain a library body; declaration scans have no executable owner. }
        ExpectedRetained := Before + 1;
        if AAccess and not AInvalid then
          Inc(ExpectedRetained);
        Expect<Integer>(Engine.RetainedModuleCount).ToBe(ExpectedRetained);
        if not AInvalid then
        begin
          if AAccess then
          begin
            Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
              .GetProperty('answer').ToStringLiteral.Value).ToBe('42');
            Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
              .GetProperty('lazyRuns').ToStringLiteral.Value).ToBe('1');
          end
          else
            Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
              .GetProperty('lazyRuns').ToStringLiteral.Value).ToBe('undefined');
        end;
      finally
        Engine.Free;
      end;
    finally
      Source.Free;
    end;
  finally
    DeleteFile(ArtifactPath);
    RemoveDir(Directory);
  end;
end;

procedure TEngineBytecodeTests.TestDeferredDiskScanReleasesBytecode;
begin
  AssertDeferredDiskRetention(False, False);
end;

procedure TEngineBytecodeTests.TestDeferredDiskExecutionRetainsOneBody;
begin
  AssertDeferredDiskRetention(True, False);
end;

procedure TEngineBytecodeTests.TestDeferredDiskFailureReleasesBytecode;
begin
  AssertDeferredDiskRetention(False, True);
end;

procedure TEngineBytecodeTests.AssertSandboxEntryAddress(const ALinked: Boolean);
const
  ENTRY_SOURCE =
    'import { value } from "./dep.mjs";' +
    'import { extra } from "./source.mjs";' +
    'globalThis.answer = value + extra;' +
    'globalThis.entryURL = import.meta.url;';
var
  FS: TSandboxVirtualFileSystem;
  Resolver: TGocciaSandboxModuleResolver;
  Provider: TGocciaSandboxModuleContentProvider;
  Loader: TGocciaModuleLoader;
  Source: TStringList;
  Engine: TGocciaEngine;
  Module: TGocciaBytecodeModule;
  EntryBytes, DependencyBytes: TBytes;
  EntryFound: Boolean;
  I: Integer;
begin
  EntryBytes := CompileBuffer(ENTRY_SOURCE, '/compile/main.mjs',
    [], False, ALinked);
  DependencyBytes := CompileBuffer('export const value = 40;',
    '/compile/dep.mjs');
  FS := TSandboxVirtualFileSystem.Create;
  Resolver := nil;
  Provider := nil;
  Loader := nil;
  Source := nil;
  Engine := nil;
  Module := nil;
  try
    FS.MakeDirectory('/work');
    FS.WriteAllText('/work/source.mjs', 'export const extra = 2;');
    Resolver := TGocciaSandboxModuleResolver.Create(FS);
    Provider := TGocciaSandboxModuleContentProvider.Create(FS);
    Loader := TGocciaModuleLoader.Create('/work/main.mjs', Resolver, Provider);
    Source := TStringList.Create;
    Engine := TGocciaEngine.Create('/work/main.mjs', Source, Loader);
    Engine.SourceType := stModule;
    Engine.RegisterBytecodeModule('/work/dep.mjs', DependencyBytes);
    if ALinked then
    begin
      Module := LoadModuleFromBuffer(EntryBytes);
      Engine.ValidateBytecodeModule(Module);
      Engine.RetainModule(Module);
      Module := nil;
      Engine.RunModuleForSourceType(
        Engine.RetainedModule(Engine.RetainedModuleCount - 1), '/work/main.mjs');
      Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
        .GetProperty('entryURL').ToStringLiteral.Value).ToBe('/work/main.mjs');
    end
    else
      Engine.RunBytecodeBuffer(EntryBytes, '/work/main.mjs');
    Expect<Boolean>(FS.Exists('/work/main.mjs')).ToBe(False);
    Expect<string>(TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('answer').ToStringLiteral.Value).ToBe('42');
    EntryFound := False;
    for I := 0 to Engine.RetainedModuleCount - 1 do
      EntryFound := EntryFound or
        (Engine.RetainedModule(I).SourcePath = '/work/main.mjs');
    Expect<Boolean>(EntryFound).ToBe(True);
  finally
    Module.Free;
    Engine.Free;
    Source.Free;
    Loader.Free;
    Provider.Free;
    Resolver.Free;
    FS.Free;
  end;
end;

procedure TEngineBytecodeTests.TestLinkedSandboxEntryWithoutSource;
begin
  AssertSandboxEntryAddress(True);
end;

procedure TEngineBytecodeTests.TestStandaloneSandboxEntryWithoutSource;
begin
  AssertSandboxEntryAddress(False);
end;

begin
  TestRunnerProgram.AddSuite(TEngineBytecodeTests.Create('Engine Bytecode'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
