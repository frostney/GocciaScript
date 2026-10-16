program Goccia.Modules.FunctionInstantiation.Test;

{ A module's top-level function declarations are instantiated while the module
  is linked (ES2026 §16.2.1.7.3.1 InitializeEnvironment). They are compiled
  closures of the VM. }

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  OrderedStringMap,
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.TestSetup,
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives;

type
  TInMemoryModuleResolver = class(TGocciaModuleResolver)
  public
    function Resolve(const AModulePath, AImportingFilePath: string): string; override;
  end;

  TMemoryModuleContentProvider = class(TGocciaModuleContentProvider)
  private
    FEntries: TOrderedStringMap<string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddModule(const APath, AText: string);
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

  TModuleFunctionInstantiationTests = class(TTestSuite)
  private
    procedure AssertImportedModuleFunctions;
    procedure AssertEntryModuleFunctionReadsImport;
  public
    procedure SetupTests; override;

    procedure TestBytecodeModuleFunctionsAreCompiled;
    procedure TestEntryModuleFunctionReadsImport;
  end;

const
  // The VM's function values are private to Goccia.VM.
  BYTECODE_FUNCTION_CLASS = 'TGocciaBytecodeFunctionValue';

  DEP_PATH = 'memory:/dep.js';
  ENTRY_PATH = 'memory:/app.js';
  FUNCTIONS_PATH = 'memory:/functions.js';

  DEP_SOURCE =
    'export let label = "dep";' + LineEnding +
    'export function relabel(value) { label = value; }';

  FUNCTIONS_SOURCE =
    'import { label } from "' + DEP_PATH + '";' + LineEnding +
    'export function exported() { return label; }' + LineEnding +
    'function internal() { return "internal:" + label; }' + LineEnding +
    'export const internalRef = () => internal;' + LineEnding +
    'export default function () { return "default"; }' + LineEnding +
    'export async function asyncFn() { return label; }' + LineEnding +
    'export function* generatorFn() { yield label; }';

function TInMemoryModuleResolver.Resolve(const AModulePath,
  AImportingFilePath: string): string;
begin
  Result := AModulePath;
end;

constructor TMemoryModuleContentProvider.Create;
begin
  inherited Create;
  FEntries := TOrderedStringMap<string>.Create;
end;

destructor TMemoryModuleContentProvider.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TMemoryModuleContentProvider.AddModule(const APath, AText: string);
begin
  FEntries.AddOrSetValue(APath, AText);
end;

function TMemoryModuleContentProvider.Exists(const APath: string): Boolean;
var
  Text: string;
begin
  Result := FEntries.TryGetValue(APath, Text);
end;

function TMemoryModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
var
  Text: string;
begin
  if not FEntries.TryGetValue(APath, Text) then
    raise Exception.Create('Module content not found: ' + APath);
  Result := TGocciaModuleContent.Create(Text, 0);
end;

function TMemoryModuleContentProvider.TryGetLastModified(const APath: string;
  out ALastModified: TDateTime): Boolean;
begin
  ALastModified := 0;
  Result := False;
end;

function CallWithoutArguments(const AFunction: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.Create;
  try
    Result := TGocciaFunctionBase(AFunction).Call(Args,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Args.Free;
  end;
end;

procedure TModuleFunctionInstantiationTests.SetupTests;
begin
  Test('Bytecode mode instantiates module function declarations as compiled closures',
    TestBytecodeModuleFunctionsAreCompiled);
  Test('An entry module function declaration reads its imports',
    TestEntryModuleFunctionReadsImport);
end;

procedure TModuleFunctionInstantiationTests.AssertImportedModuleFunctions;
const
  FUNCTION_EXPORTS: array[0..3] of string = ('exported', 'default',
    'asyncFn', 'generatorFn');
var
  Engine: TGocciaEngine;
  ExportName: string;
  ExportValue: TGocciaValue;
  Internal: TGocciaValue;
  Module: TGocciaModule;
  ModuleLoader: TGocciaModuleLoader;
  Provider: TMemoryModuleContentProvider;
  Resolver: TInMemoryModuleResolver;
  Source: TStringList;
begin
  Provider := TMemoryModuleContentProvider.Create;
  Resolver := TInMemoryModuleResolver.Create;
  Source := TStringList.Create;
  try
    Provider.AddModule(DEP_PATH, DEP_SOURCE);
    Provider.AddModule(FUNCTIONS_PATH, FUNCTIONS_SOURCE);
    Source.Text := '1;';

    ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
    try
      Engine := TGocciaEngine.Create(ENTRY_PATH, Source, ModuleLoader);
      try
        Engine.Compatibility := [cfFunction];
        Module := ModuleLoader.LoadModule(FUNCTIONS_PATH, ENTRY_PATH);

        for ExportName in FUNCTION_EXPORTS do
        begin
          Expect<Boolean>(Module.TryGetExportValue(ExportName,
            ExportValue)).ToBe(True);
          Expect<Boolean>(ExportValue.ClassName = BYTECODE_FUNCTION_CLASS)
            .ToBe(True);
        end;

        { A declaration nobody exports is instantiated the same way. }
        Expect<Boolean>(Module.TryGetExportValue('internalRef',
          ExportValue)).ToBe(True);
        Internal := CallWithoutArguments(ExportValue);
        Expect<Boolean>(Internal.ClassName = BYTECODE_FUNCTION_CLASS)
          .ToBe(True);
        Expect<string>(CallWithoutArguments(Internal).ToStringLiteral.Value)
          .ToBe('internal:dep');

        Expect<Boolean>(Module.TryGetExportValue('exported',
          ExportValue)).ToBe(True);
        Expect<string>(CallWithoutArguments(ExportValue).ToStringLiteral.Value)
          .ToBe('dep');
      finally
        Engine.Free;
      end;
    finally
      ModuleLoader.Free;
    end;
  finally
    Source.Free;
    Resolver.Free;
    Provider.Free;
  end;
end;

procedure TModuleFunctionInstantiationTests.TestBytecodeModuleFunctionsAreCompiled;
begin
  AssertImportedModuleFunctions;
end;

{ The entry of TGocciaEngine.Execute links its requests itself. Its function
  declarations exist before its body runs, and read the entry's imports from
  its module environment once the imports' modules are loaded. }
procedure TModuleFunctionInstantiationTests.AssertEntryModuleFunctionReadsImport;
var
  Engine: TGocciaEngine;
  ModuleLoader: TGocciaModuleLoader;
  Provider: TMemoryModuleContentProvider;
  Resolver: TInMemoryModuleResolver;
  ScriptResult: TGocciaScriptResult;
  Source: TStringList;
begin
  Provider := TMemoryModuleContentProvider.Create;
  Resolver := TInMemoryModuleResolver.Create;
  Source := TStringList.Create;
  try
    Provider.AddModule(DEP_PATH, DEP_SOURCE);
    Source.Text :=
      'import { label, relabel } from "' + DEP_PATH + '";' + LineEnding +
      'export function read() { return label; }' + LineEnding +
      'const before = read();' + LineEnding +
      'relabel("relabelled");' + LineEnding +
      'before + "," + read();';

    ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
    try
      Engine := TGocciaEngine.Create(ENTRY_PATH, Source, ModuleLoader);
      try
        Engine.Compatibility := [cfFunction];
        Engine.SourceType := stModule;
        ScriptResult := Engine.Execute;
        Expect<string>(ScriptResult.Result.ToStringLiteral.Value)
          .ToBe('dep,relabelled');
      finally
        Engine.Free;
      end;
    finally
      ModuleLoader.Free;
    end;
  finally
    Source.Free;
    Resolver.Free;
    Provider.Free;
  end;
end;

procedure TModuleFunctionInstantiationTests.TestEntryModuleFunctionReadsImport;
begin
  AssertEntryModuleFunctionReadsImport;
end;

begin
  TestRunnerProgram.AddSuite(TModuleFunctionInstantiationTests.Create(
    'Module Function Instantiation'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
