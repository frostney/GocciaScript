program Goccia.Modules.ContentProvider.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  OrderedStringMap,
  TestingPascalLibrary,
  TextSemantics,

  Goccia.AST.Node,
  Goccia.Bytecode.Module,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Interpreter,
  Goccia.JSONL,
  Goccia.Lexer,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.Parser,
  Goccia.TestSetup,
  Goccia.Token,
  Goccia.TOML,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.YAML;

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
    procedure SetModule(const APath, AText: string;
      const ALastModified: TDateTime; const AHasLastModified: Boolean = True);
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
    procedure TestEngineReloadsModifiedInMemoryModule;
    procedure TestEngineRetriesModuleAfterFailedLoad;
    procedure TestEngineReportsJSONLModuleLineNumbers;
    procedure TestEngineReportsTOMLModuleSyntaxErrors;
    procedure TestFileSystemContentProviderPreservesUTF8JSONLText;
    procedure TestFileSystemContentProviderPreservesUTF8TOMLText;
    procedure TestFileSystemContentProviderPreservesUTF8YAMLText;
    procedure TestEngineLoadsUTF8TextAssetModule;
    procedure TestEngineNormalizesCRLFTextAssetModulesToLF;
    procedure TestModuleLoaderRejectsRebindingAcrossRuntimes;
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
begin
  SetModule(APath, AText, 0, False);
end;

procedure TMemoryModuleContentProvider.SetModule(const APath, AText: string;
  const ALastModified: TDateTime; const AHasLastModified: Boolean);
var
  Entry: TMemoryModuleEntry;
begin
  Entry.Text := AText;
  Entry.HasLastModified := AHasLastModified;
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
  Test('Engine reloads modified in-memory module',
    TestEngineReloadsModifiedInMemoryModule);
  Test('Engine retries module load after previous failure',
    TestEngineRetriesModuleAfterFailedLoad);
  Test('Engine reports JSONL module parse line numbers',
    TestEngineReportsJSONLModuleLineNumbers);
  Test('Engine reports TOML module syntax errors',
    TestEngineReportsTOMLModuleSyntaxErrors);
  Test('File system content provider preserves UTF-8 JSONL text',
    TestFileSystemContentProviderPreservesUTF8JSONLText);
  Test('File system content provider preserves UTF-8 TOML text',
    TestFileSystemContentProviderPreservesUTF8TOMLText);
  Test('File system content provider preserves UTF-8 YAML text',
    TestFileSystemContentProviderPreservesUTF8YAMLText);
  Test('Engine loads UTF-8 text asset modules',
    TestEngineLoadsUTF8TextAssetModule);
  Test('Engine normalizes CRLF text asset modules to LF',
    TestEngineNormalizesCRLFTextAssetModulesToLF);
  Test('Module loader rejects rebinding across runtimes',
    TestModuleLoaderRejectsRebindingAcrossRuntimes);
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
    SourceLines := CreateUTF8StringList(ASource);
    try
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

procedure WriteUTF8File(const APath: string; const AText: UTF8String);
var
  Stream: TFileStream;
begin
  ForceDirectories(ExtractFileDir(APath));
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(AText) > 0 then
      Stream.WriteBuffer(Pointer(AText)^, Length(AText));
  finally
    Stream.Free;
  end;
end;

procedure TModuleContentProviderTests.TestEngineLoadsInMemoryModuleWithCustomProvider;
const
  ENTRY_PATH = 'memory:/app.js';
  MODULE_PATH = 'memory:/dep.js';
var
  Engine: TGocciaEngine;
  HasLineNumber: Boolean;
  HasParseMessage: Boolean;
  ModuleLoader: TGocciaModuleLoader;
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

    ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
    try
      Engine := TGocciaEngine.Create(ENTRY_PATH, Source,
        [], ModuleLoader);
      try
        ScriptResult := Engine.Execute;
      finally
        Engine.Free;
      end;
    finally
      ModuleLoader.Free;
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
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  ProgramNode: TGocciaProgram;
  Provider: TMemoryModuleContentProvider;
  ResultValue: TGocciaValue;
  Source: TStringList;
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

  Source := TStringList.Create;
  try
    Source.Text := 'import { value } from "./dep.js";' + LineEnding + 'value;';

    Executor := TGocciaBytecodeExecutor.Create;
    try
      Engine := TGocciaEngine.Create(EntryPath, Source, [],
        TGocciaModuleLoader.Create(EntryPath, nil, Provider), Executor);
      try
        ProgramNode := CreateProgram(
          'import { value } from "./dep.js";' + LineEnding + 'value;',
          EntryPath);
        try
          ResultValue := Engine.ExecuteProgram(ProgramNode);
        finally
          ProgramNode.Free;
        end;

        Expect<Boolean>(ResultValue is TGocciaNumberLiteralValue).ToBe(True);
        Expect<Double>(TGocciaNumberLiteralValue(ResultValue).Value).ToBe(42);
      finally
        Engine.Free;
      end;
    finally
      Executor.Free;
    end;
  finally
    Source.Free;
    Provider.Free;
  end;
end;

procedure TModuleContentProviderTests.TestEngineReloadsModifiedInMemoryModule;
const
  ENTRY_PATH = 'memory:/app.js';
  MODULE_PATH = 'memory:/dep.js';
var
  Interpreter: TGocciaInterpreter;
  ModuleLoader: TGocciaModuleLoader;
  ModuleValue: TGocciaModule;
  Provider: TMemoryModuleContentProvider;
  Resolver: TInMemoryModuleResolver;
  Source: TStringList;
begin
  Provider := TMemoryModuleContentProvider.Create;
  Resolver := TInMemoryModuleResolver.Create;
  Source := TStringList.Create;
  try
    Provider.SetModule(MODULE_PATH, 'export const value = 1;', 1);
    Source.Text := 'import { value } from "' + MODULE_PATH + '";' + LineEnding +
      'value;';

    ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
    try
      Interpreter := TGocciaInterpreter.Create(ENTRY_PATH, Source, ModuleLoader);
      try
        ModuleValue := Interpreter.LoadModule(MODULE_PATH, ENTRY_PATH);
        Expect<Boolean>(ModuleValue.ExportsTable['value'] is TGocciaNumberLiteralValue).ToBe(True);
        Expect<Double>(TGocciaNumberLiteralValue(ModuleValue.ExportsTable['value']).Value).ToBe(1);

        Provider.SetModule(MODULE_PATH, 'export const value = 2;', 2);
        ModuleValue := Interpreter.LoadModule(MODULE_PATH, ENTRY_PATH);
        Expect<Boolean>(ModuleValue.ExportsTable['value'] is TGocciaNumberLiteralValue).ToBe(True);
        Expect<Double>(TGocciaNumberLiteralValue(ModuleValue.ExportsTable['value']).Value).ToBe(2);
      finally
        Interpreter.Free;
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

procedure TModuleContentProviderTests.TestEngineRetriesModuleAfterFailedLoad;
const
  ENTRY_PATH = 'memory:/app.js';
  MODULE_PATH = 'memory:/dep.js';
var
  Engine: TGocciaEngine;
  HasLineNumber: Boolean;
  HasParseMessage: Boolean;
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
    Provider.AddModule(MODULE_PATH, 'export const value = ;');
    Source.Text := 'import { value } from "' + MODULE_PATH + '";' + LineEnding +
      'value;';

    ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
    try
      Engine := TGocciaEngine.Create(ENTRY_PATH, Source,
        [], ModuleLoader);
      try
        try
          Engine.Execute;
          Fail('Expected invalid module source to raise an exception.');
        except
          on E: Exception do
          begin
            // Expected parse failure; retry after fixing the module source.
          end;
        end;

        Provider.AddModule(MODULE_PATH, 'export const value = 42;');
        ScriptResult := Engine.Execute;
        Expect<Boolean>(ScriptResult.Result is TGocciaNumberLiteralValue).ToBe(True);
        Expect<Double>(TGocciaNumberLiteralValue(ScriptResult.Result).Value).ToBe(42);
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

procedure TModuleContentProviderTests.TestEngineReportsJSONLModuleLineNumbers;
const
  ENTRY_PATH = 'memory:/app.js';
  MODULE_PATH = 'memory:/events.jsonl';
var
  Engine: TGocciaEngine;
  HasLineNumber: Boolean;
  HasParseMessage: Boolean;
  ModuleLoader: TGocciaModuleLoader;
  Provider: TMemoryModuleContentProvider;
  Resolver: TInMemoryModuleResolver;
  RaisedExpected: Boolean;
  Source: TStringList;
begin
  Provider := TMemoryModuleContentProvider.Create;
  Resolver := TInMemoryModuleResolver.Create;
  Source := TStringList.Create;
  try
    Provider.AddModule(MODULE_PATH,
      '{"id":1}' + LineEnding +
      '{invalid}' + LineEnding +
      '42');
    Source.Text := 'import { "0" as firstRecord } from "' + MODULE_PATH + '";' +
      LineEnding + 'firstRecord;';

    ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
    try
      Engine := TGocciaEngine.Create(ENTRY_PATH, Source,
        [], ModuleLoader);
      try
        RaisedExpected := False;
        try
          Engine.Execute;
          Fail('Expected invalid JSONL module source to raise an exception.');
        except
          on E: Exception do
          begin
            RaisedExpected := True;
            HasParseMessage := Pos('Failed to parse JSONL module',
              E.Message) > 0;
            HasLineNumber := Pos('JSONL line 2', E.Message) > 0;
            if not HasParseMessage then
              Fail('Expected JSONL module parse prefix in error message.');
            if not HasLineNumber then
              Fail('Expected JSONL module error to include line 2.');
          end;
        end;

        Expect<Boolean>(RaisedExpected).ToBe(True);
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

procedure TModuleContentProviderTests.TestEngineReportsTOMLModuleSyntaxErrors;
const
  ENTRY_PATH = 'memory:/app.js';
  MODULE_PATH = 'memory:/config.toml';
var
  Engine: TGocciaEngine;
  HasParseMessage: Boolean;
  HasSyntaxDetail: Boolean;
  ModuleLoader: TGocciaModuleLoader;
  Provider: TMemoryModuleContentProvider;
  Resolver: TInMemoryModuleResolver;
  RaisedExpected: Boolean;
  Source: TStringList;
begin
  Provider := TMemoryModuleContentProvider.Create;
  Resolver := TInMemoryModuleResolver.Create;
  Source := TStringList.Create;
  try
    Provider.AddModule(MODULE_PATH, 'value = 01');
    Source.Text := 'import { value } from "' + MODULE_PATH + '";' + LineEnding +
      'value;';

    ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
    try
      Engine := TGocciaEngine.Create(ENTRY_PATH, Source,
        [], ModuleLoader);
      try
        RaisedExpected := False;
        try
          Engine.Execute;
          Fail('Expected invalid TOML module source to raise an exception.');
        except
          on E: Exception do
          begin
            RaisedExpected := True;
            HasParseMessage := Pos('Failed to parse TOML module',
              E.Message) > 0;
            HasSyntaxDetail := Pos('Invalid TOML value: 01',
              E.Message) > 0;
            if not HasParseMessage then
              Fail('Expected TOML module parse prefix in error message.');
            if not HasSyntaxDetail then
              Fail('Expected TOML module error to include syntax detail.');
          end;
        end;

        Expect<Boolean>(RaisedExpected).ToBe(True);
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

procedure TModuleContentProviderTests.TestFileSystemContentProviderPreservesUTF8JSONLText;
var
  CityValue: UTF8String;
  Content: TGocciaModuleContent;
  ContentProvider: TGocciaFileSystemModuleContentProvider;
  MessageValue: UTF8String;
  NameValue: UTF8String;
  Parser: TGocciaJSONLParser;
  Records: TGocciaArrayValue;
  TempDirectory: string;
  JSONLPath: string;
  JSONLText: UTF8String;
begin
  TempDirectory := CreateTempDirectory;
  JSONLPath := IncludeTrailingPathDelimiter(TempDirectory) + 'unicode.jsonl';
  NameValue := 'Jos' + #$C3#$A9;
  CityValue := 'Z' + #$C3#$BC + 'rich';
  MessageValue := 'Caf' + #$C3#$A9 + ' d' + #$C3#$A9 + 'j' + #$C3#$A0 + ' vu';
  JSONLText :=
    '{"name":"' + NameValue + '","city":"' + CityValue + '"}' + #10 +
    '"' + MessageValue + '"' + #10;
  WriteUTF8File(JSONLPath, JSONLText);

  ContentProvider := TGocciaFileSystemModuleContentProvider.Create;
  try
    Content := ContentProvider.LoadContent(JSONLPath);
    try
      Parser := TGocciaJSONLParser.Create;
      try
        Records := Parser.Parse(Content.Text);
      finally
        Parser.Free;
      end;

      try
        Expect<Integer>(Records.Elements.Count).ToBe(2);
        Expect<string>(TGocciaObjectValue(Records.Elements[0])
          .GetProperty('name').ToStringLiteral.Value).ToBe(NameValue);
        Expect<string>(TGocciaObjectValue(Records.Elements[0])
          .GetProperty('city').ToStringLiteral.Value).ToBe(CityValue);
        Expect<string>(Records.Elements[1].ToStringLiteral.Value)
          .ToBe(MessageValue);
      finally
        Records.Free;
      end;
    finally
      Content.Free;
    end;
  finally
    ContentProvider.Free;
  end;
end;

procedure TModuleContentProviderTests.TestFileSystemContentProviderPreservesUTF8TOMLText;
var
  CityValue: UTF8String;
  Content: TGocciaModuleContent;
  ContentProvider: TGocciaFileSystemModuleContentProvider;
  NameValue: UTF8String;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  Parser: TGocciaTOMLParser;
  QuotedKey: UTF8String;
  TempDirectory: string;
  TomlPath: string;
  TomlText: UTF8String;
begin
  TempDirectory := CreateTempDirectory;
  TomlPath := IncludeTrailingPathDelimiter(TempDirectory) + 'unicode.toml';
  NameValue := 'Jos' + #$C3#$A9;
  QuotedKey := 'd' + #$C3#$A9 + 'j' + #$C3#$A0;
  CityValue := 'Z' + #$C3#$BC + 'rich';
  TomlText :=
    'name = "' + NameValue + '"' + #10 +
    '"' + QuotedKey + '" = "vu"' + #10 +
    '[nested]' + #10 +
    'city = "' + CityValue + '"' + #10;
  WriteUTF8File(TomlPath, TomlText);

  ContentProvider := TGocciaFileSystemModuleContentProvider.Create;
  try
    Content := ContentProvider.LoadContent(TomlPath);
    try
      Parser := TGocciaTOMLParser.Create;
      try
        ParsedValue := Parser.Parse(Content.Text);
      finally
        Parser.Free;
      end;

      Obj := TGocciaObjectValue(ParsedValue);
      Expect<string>(Obj.GetProperty('name').ToStringLiteral.Value)
        .ToBe(NameValue);
      Expect<string>(Obj.GetProperty(QuotedKey)
        .ToStringLiteral.Value).ToBe('vu');
      Expect<string>(TGocciaObjectValue(Obj.GetProperty('nested'))
        .GetProperty('city').ToStringLiteral.Value)
        .ToBe(CityValue);
    finally
      Content.Free;
    end;
  finally
    ContentProvider.Free;
  end;
end;

procedure TModuleContentProviderTests.TestFileSystemContentProviderPreservesUTF8YAMLText;
var
  CityValue: UTF8String;
  Content: TGocciaModuleContent;
  ContentProvider: TGocciaFileSystemModuleContentProvider;
  Documents: TGocciaArrayValue;
  NameValue: UTF8String;
  Obj: TGocciaObjectValue;
  Parser: TGocciaYAMLParser;
  QuotedKey: UTF8String;
  TempDirectory: string;
  YAMLPath: string;
  YAMLText: UTF8String;
begin
  TempDirectory := CreateTempDirectory;
  YAMLPath := IncludeTrailingPathDelimiter(TempDirectory) + 'unicode.yaml';
  NameValue := 'Jos' + #$C3#$A9;
  QuotedKey := 'd' + #$C3#$A9 + 'j' + #$C3#$A0;
  CityValue := 'Z' + #$C3#$BC + 'rich';
  YAMLText :=
    'name: ' + NameValue + #10 +
    '"' + QuotedKey + '": vu' + #10 +
    'nested:' + #10 +
    '  city: ' + CityValue + #10;
  WriteUTF8File(YAMLPath, YAMLText);

  ContentProvider := TGocciaFileSystemModuleContentProvider.Create;
  try
    Content := ContentProvider.LoadContent(YAMLPath);
    try
      Parser := TGocciaYAMLParser.Create;
      try
        Documents := Parser.ParseDocuments(Content.Text);
      finally
        Parser.Free;
      end;

      try
        Expect<Integer>(Documents.Elements.Count).ToBe(1);
        Obj := TGocciaObjectValue(Documents.Elements[0]);
        Expect<string>(Obj.GetProperty('name').ToStringLiteral.Value)
          .ToBe(NameValue);
        Expect<string>(Obj.GetProperty(QuotedKey).ToStringLiteral.Value)
          .ToBe('vu');
        Expect<string>(TGocciaObjectValue(Obj.GetProperty('nested'))
          .GetProperty('city').ToStringLiteral.Value)
          .ToBe(CityValue);
      finally
        Documents.Free;
      end;
    finally
      Content.Free;
    end;
  finally
    ContentProvider.Free;
  end;
end;

procedure TModuleContentProviderTests.TestEngineLoadsUTF8TextAssetModule;
var
  ContentValue: UTF8String;
  Engine: TGocciaEngine;
  MetadataValue: TGocciaObjectValue;
  ResultObject: TGocciaObjectValue;
  ScriptResult: TGocciaScriptResult;
  Source: TStringList;
  TempDirectory: string;
  TextAssetPath: string;
begin
  TempDirectory := CreateTempDirectory;
  TextAssetPath := IncludeTrailingPathDelimiter(TempDirectory) + 'note.txt';
  ContentValue := 'Caf' + #$C3#$A9 + ' asset' + #10 + 'Second line';
  WriteUTF8File(TextAssetPath, ContentValue);

  Source := TStringList.Create;
  try
    Source.Text :=
      'import { content, metadata } from "./note.txt";' + LineEnding +
      '({ content, metadata });';

    Engine := TGocciaEngine.Create(
      IncludeTrailingPathDelimiter(TempDirectory) + 'app.js',
      Source,
      []);
    try
      ScriptResult := Engine.Execute;
    finally
      Engine.Free;
    end;

    Expect<Boolean>(ScriptResult.Result is TGocciaObjectValue).ToBe(True);
    ResultObject := TGocciaObjectValue(ScriptResult.Result);
    Expect<string>(ResultObject.GetProperty(PROP_CONTENT).ToStringLiteral.Value)
      .ToBe(ContentValue);

    Expect<Boolean>(ResultObject.GetProperty(PROP_METADATA) is TGocciaObjectValue)
      .ToBe(True);
    MetadataValue := TGocciaObjectValue(ResultObject.GetProperty(PROP_METADATA));
    Expect<string>(MetadataValue.GetProperty(PROP_KIND).ToStringLiteral.Value)
      .ToBe('text');
    Expect<string>(MetadataValue.GetProperty(PROP_FILE_NAME)
      .ToStringLiteral.Value).ToBe('note.txt');
    Expect<string>(MetadataValue.GetProperty(PROP_EXTENSION)
      .ToStringLiteral.Value).ToBe('.txt');
    Expect<Double>(MetadataValue.GetProperty(PROP_BYTE_LENGTH)
      .ToNumberLiteral.Value).ToBe(Length(ContentValue));
  finally
    Source.Free;
  end;
end;

procedure TModuleContentProviderTests.TestEngineNormalizesCRLFTextAssetModulesToLF;
var
  Engine: TGocciaEngine;
  RawContent: UTF8String;
  ScriptResult: TGocciaScriptResult;
  Source: TStringList;
  TempDirectory: string;
  TextAssetPath: string;
begin
  TempDirectory := CreateTempDirectory;
  TextAssetPath := IncludeTrailingPathDelimiter(TempDirectory) + 'crlf-note.txt';
  RawContent := 'first line' + #13#10 + 'second line' + #13#10;
  WriteUTF8File(TextAssetPath, RawContent);

  Source := TStringList.Create;
  try
    Source.Text :=
      'import { content } from "./crlf-note.txt";' + LineEnding +
      'content;';

    Engine := TGocciaEngine.Create(
      IncludeTrailingPathDelimiter(TempDirectory) + 'app.js',
      Source,
      []);
    try
      ScriptResult := Engine.Execute;
    finally
      Engine.Free;
    end;

    Expect<string>(ScriptResult.Result.ToStringLiteral.Value)
      .ToBe('first line' + #10 + 'second line' + #10);
  finally
    Source.Free;
  end;
end;

procedure TModuleContentProviderTests.TestModuleLoaderRejectsRebindingAcrossRuntimes;
const
  ENTRY_PATH = 'memory:/app.js';
var
  EngineA: TGocciaEngine;
  EngineB: TGocciaEngine;
  RaisedExpected: Boolean;
  HasExpectedMessage: Boolean;
  ModuleLoader: TGocciaModuleLoader;
  Provider: TMemoryModuleContentProvider;
  Resolver: TInMemoryModuleResolver;
  SourceA: TStringList;
  SourceB: TStringList;
begin
  Provider := TMemoryModuleContentProvider.Create;
  Resolver := TInMemoryModuleResolver.Create;
  ModuleLoader := TGocciaModuleLoader.Create(ENTRY_PATH, Resolver, Provider);
  SourceA := TStringList.Create;
  SourceB := TStringList.Create;
  EngineB := nil;
  RaisedExpected := False;
  try
    SourceA.Text := '1;';
    SourceB.Text := '2;';
    EngineA := TGocciaEngine.Create(ENTRY_PATH, SourceA,
      [], ModuleLoader);
    try
      try
        EngineB := TGocciaEngine.Create(ENTRY_PATH, SourceB,
          [], ModuleLoader);
        Fail('Expected module loader rebinding to raise an exception.');
      except
        on E: Exception do
        begin
          RaisedExpected := True;
          HasExpectedMessage := Pos('single-runtime', E.Message) > 0;
          if not HasExpectedMessage then
            Fail('Expected single-runtime error message.');
        end;
      end;
      Expect<Boolean>(RaisedExpected).ToBe(True);
    finally
      EngineA.Free;
      if Assigned(EngineB) then
        EngineB.Free;
    end;
  finally
    SourceA.Free;
    SourceB.Free;
    ModuleLoader.Free;
    Resolver.Free;
    Provider.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TModuleContentProviderTests.Create('Module Content Provider'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
