program Goccia.Modules.ContentProvider.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  OrderedStringMap,
  TestRunner,

  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Interpreter,
  Goccia.Lexer,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Resolver,
  Goccia.Parser,
  Goccia.TestSetup,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  TInMemoryModuleResolver = class(TGocciaModuleResolver)
  public
    function Resolve(const AModulePath, AImportingFilePath: string): string; override;
  end;

  TMemoryModuleEntry = record
    HasLastModified: Boolean;
    LastModified: TDateTime;
    Text: string;
  end;

  TMemoryModuleContentProvider = class(TGocciaModuleContentProvider)
  private
    FEntries: TOrderedStringMap<TMemoryModuleEntry>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddModule(const APath, AText: string);
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

  TModuleContentProviderTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;

    function CreateProgram(const ASource, AFileName: string): TGocciaProgram;
    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteTextFile(const APath, AText: string);

    procedure TestEngineLoadsInMemoryModuleWithCustomProvider;
    procedure TestBytecodeBackendUsesInjectedContentProvider;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

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

procedure TMemoryModuleContentProvider.AddModule(const APath, AText: string);
var
  Entry: TMemoryModuleEntry;
begin
  Entry.Text := AText;
  Entry.HasLastModified := False;
  Entry.LastModified := 0;
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
  Result := FEntries.TryGetValue(APath, Entry) and Entry.HasLastModified;
  if Result then
    ALastModified := Entry.LastModified
  else
    ALastModified := 0;
end;

procedure TModuleContentProviderTests.SetupTests;
begin
  Test('Engine loads in-memory module with custom provider',
    TestEngineLoadsInMemoryModuleWithCustomProvider);
  Test('Bytecode backend uses injected content provider',
    TestBytecodeBackendUsesInjectedContentProvider);
end;

procedure TModuleContentProviderTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FTempDirectories := TStringList.Create;
end;

procedure TModuleContentProviderTests.AfterAll;
var
  I: Integer;
begin
  for I := 0 to FTempDirectories.Count - 1 do
    DeleteDirectoryTree(FTempDirectories[I]);
  FTempDirectories.Free;
  inherited AfterAll;
end;

function TModuleContentProviderTests.CreateProgram(const ASource,
  AFileName: string): TGocciaProgram;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  SourceLines: TStringList;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(ASource, AFileName);
  try
    Tokens := Lexer.ScanTokens;
    SourceLines := TStringList.Create;
    try
      SourceLines.Text := ASource;
      Parser := TGocciaParser.Create(Tokens, AFileName, SourceLines);
      try
        Result := Parser.Parse;
      finally
        Parser.Free;
      end;
    finally
      SourceLines.Free;
    end;
  finally
    Lexer.Free;
  end;
end;

function TModuleContentProviderTests.CreateTempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-content-provider-' + IntToStr(Random(MaxInt));
  ForceDirectories(Result);
  FTempDirectories.Add(Result);
end;

procedure TModuleContentProviderTests.DeleteDirectoryTree(const APath: string);
var
  EntryPath: string;
  SearchRec: TSearchRec;
begin
  if not DirectoryExists(APath) then
    Exit;

  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
    SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;

      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  RemoveDir(APath);
end;

procedure TModuleContentProviderTests.WriteTextFile(const APath, AText: string);
var
  Source: TStringList;
begin
  ForceDirectories(ExtractFileDir(APath));
  Source := TStringList.Create;
  try
    Source.Text := AText;
    Source.SaveToFile(APath);
  finally
    Source.Free;
  end;
end;

procedure TModuleContentProviderTests.TestEngineLoadsInMemoryModuleWithCustomProvider;
const
  ENTRY_PATH = 'memory:/app.js';
  MODULE_PATH = 'memory:/dep.js';
var
  Engine: TGocciaEngine;
  Provider: TMemoryModuleContentProvider;
  Resolver: TInMemoryModuleResolver;
  Source: TStringList;
  ScriptResult: TGocciaScriptResult;
begin
  Provider := TMemoryModuleContentProvider.Create;
  Resolver := TInMemoryModuleResolver.Create;
  Source := TStringList.Create;
  try
    Provider.AddModule(MODULE_PATH, 'export const value = 42;');
    Source.Text := 'import { value } from "' + MODULE_PATH + '";' + LineEnding +
      'value;';

    Engine := TGocciaEngine.Create(ENTRY_PATH, Source,
      TGocciaEngine.DefaultGlobals, Resolver, Provider);
    try
      ScriptResult := Engine.Execute;
    finally
      Engine.Free;
    end;

    Expect<Boolean>(ScriptResult.Result is TGocciaNumberLiteralValue).ToBe(True);
    Expect<Double>(TGocciaNumberLiteralValue(ScriptResult.Result).Value).ToBe(42);
  finally
    Source.Free;
    Resolver.Free;
    Provider.Free;
  end;
end;

procedure TModuleContentProviderTests.TestBytecodeBackendUsesInjectedContentProvider;
var
  Backend: TGocciaBytecodeBackend;
  Module: TGocciaBytecodeModule;
  ProgramNode: TGocciaProgram;
  Provider: TMemoryModuleContentProvider;
  ResultValue: TGocciaValue;
  TempDirectory: string;
  DepPath: string;
  EntryPath: string;
begin
  TempDirectory := CreateTempDirectory;
  EntryPath := IncludeTrailingPathDelimiter(TempDirectory) + 'app.js';
  DepPath := IncludeTrailingPathDelimiter(TempDirectory) + 'dep.js';

  WriteTextFile(DepPath, 'export const value = 7;');

  Provider := TMemoryModuleContentProvider.Create;
  Provider.AddModule(DepPath, 'export const value = 42;');

  Backend := TGocciaBytecodeBackend.Create(EntryPath, Provider);
  try
    Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);
    ProgramNode := CreateProgram(
      'import { value } from "./dep.js";' + LineEnding + 'value;',
      EntryPath);
    try
      Module := Backend.CompileToModule(ProgramNode);
    finally
      ProgramNode.Free;
    end;

    try
      ResultValue := Backend.RunModule(Module);
      Expect<Boolean>(ResultValue is TGocciaNumberLiteralValue).ToBe(True);
      Expect<Double>(TGocciaNumberLiteralValue(ResultValue).Value).ToBe(42);
    finally
      Module.Free;
    end;
  finally
    Backend.Free;
    Provider.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TModuleContentProviderTests.Create('Module Content Provider'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
