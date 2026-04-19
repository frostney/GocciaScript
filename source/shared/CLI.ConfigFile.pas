unit CLI.ConfigFile;

{$I Shared.inc}

interface

uses
  CLI.Options;

type
  { A single key-value pair extracted from a configuration file. }
  TConfigEntry = record
    Key: string;
    Value: string;
  end;
  TConfigEntryArray = array of TConfigEntry;

  { Callback that parses file content into flat key-value entries.
    Keys are option long names (e.g. 'mode', 'timeout').
    Values are their string representations (e.g. 'bytecode', '5000').
    Boolean true produces 'true'; false produces 'false'.
    Arrays produce multiple entries with the same key. }
  TConfigParseFunc = function(const AContent: string): TConfigEntryArray;

{ Register a parser for a file extension.
  The extension must include the leading dot and is matched
  case-insensitively (e.g. '.json', '.json5', '.toml'). }
procedure RegisterConfigParser(const AExtension: string;
  AParser: TConfigParseFunc);

{ Apply configuration entries to the given options.
  Each entry whose key matches an option long name is applied.
  Options that are already Present are skipped, so CLI arguments
  parsed before this call naturally take precedence.
  Unknown keys are silently skipped (config files may contain
  keys for other subsystems).
  Flag options are only set when the value is 'true' or empty.
  For repeatable options, values are accumulated even when Present. }
procedure ApplyConfigEntries(const AEntries: TConfigEntryArray;
  const AOptions: TGocciaOptionArray);

{ Parse a configuration file and return its entries without
  applying them.  Handles the "extends" key: if present, the
  referenced base config is loaded first and its entries appear
  before the current file's entries (so the child overrides the
  parent).  The extends path is resolved relative to the config
  file's directory.  Circular extends chains are detected and
  raise an exception. }
function ParseConfigFile(const APath: string): TConfigEntryArray;

{ Read, parse (with extends resolution), and apply a config file
  to the given options.  Equivalent to ApplyConfigEntries +
  ParseConfigFile.  The file format is determined by its
  extension using registered parsers.  JSON (.json) is handled
  by a built-in parser; other formats require prior registration
  via RegisterConfigParser. }
procedure ApplyConfigFile(const APath: string;
  const AOptions: TGocciaOptionArray);

{ Walk up from AStartDirectory looking for a configuration file.
  At each directory level, tries every combination of
  ABaseNames x AExtensions in the order given (extensions are the
  inner loop, so the first extension has highest priority).
  Returns the full path of the first file found, or '' if none. }
function DiscoverConfigFile(const AStartDirectory: string;
  const ABaseNames: array of string;
  const AExtensions: array of string): string;

implementation

uses
  Classes,
  SysUtils,

  JSONParser;

{ ── Parser registry ────────────────────────────────────────── }

type
  TParserRegistryEntry = record
    Extension: string;
    Parser: TConfigParseFunc;
  end;

var
  GParserRegistry: array of TParserRegistryEntry;

procedure RegisterConfigParser(const AExtension: string;
  AParser: TConfigParseFunc);
var
  I, Len: Integer;
  LowerExt: string;
begin
  LowerExt := LowerCase(AExtension);
  for I := 0 to High(GParserRegistry) do
    if GParserRegistry[I].Extension = LowerExt then
    begin
      GParserRegistry[I].Parser := AParser;
      Exit;
    end;
  Len := Length(GParserRegistry);
  SetLength(GParserRegistry, Len + 1);
  GParserRegistry[Len].Extension := LowerExt;
  GParserRegistry[Len].Parser := AParser;
end;

function FindParser(const AExtension: string): TConfigParseFunc;
var
  LowerExt: string;
  I: Integer;
begin
  LowerExt := LowerCase(AExtension);
  for I := 0 to High(GParserRegistry) do
    if GParserRegistry[I].Extension = LowerExt then
      Exit(GParserRegistry[I].Parser);
  Result := nil;
end;

{ ── Built-in JSON config parser (SAX-based) ────────────────── }

type
  { SAX parser that extracts top-level key-value pairs from a
    JSON object.  Only the top-level object's scalar properties
    and flat arrays of scalars are collected; nested objects and
    null values are silently skipped. }
  TConfigJSONParser = class(TAbstractJSONParser)
  private
    FEntries: TConfigEntryArray;
    FCount: Integer;
    FCurrentKey: string;
    FDepth: Integer;
    FInTopArray: Boolean;
    procedure AddEntry(const AValue: string);
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
  public
    function Parse(const AText: string): TConfigEntryArray;
  end;

procedure TConfigJSONParser.AddEntry(const AValue: string);
begin
  if FCount >= Length(FEntries) then
    SetLength(FEntries, Length(FEntries) * 2 + 8);
  FEntries[FCount].Key := FCurrentKey;
  FEntries[FCount].Value := AValue;
  Inc(FCount);
end;

procedure TConfigJSONParser.OnNull;
begin
  { Skip null values. }
end;

procedure TConfigJSONParser.OnBoolean(const AValue: Boolean);
begin
  if FDepth = 1 then
    AddEntry(BoolToStr(AValue, 'true', 'false'));
  if FInTopArray and (FDepth = 2) then
    AddEntry(BoolToStr(AValue, 'true', 'false'));
end;

procedure TConfigJSONParser.OnString(const AValue: string);
begin
  if FDepth = 1 then
    AddEntry(AValue)
  else if FInTopArray and (FDepth = 2) then
    AddEntry(AValue);
end;

procedure TConfigJSONParser.OnInteger(const AValue: Int64);
begin
  if FDepth = 1 then
    AddEntry(IntToStr(AValue))
  else if FInTopArray and (FDepth = 2) then
    AddEntry(IntToStr(AValue));
end;

procedure TConfigJSONParser.OnFloat(const AValue: Double);
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  if FDepth = 1 then
    AddEntry(FloatToStr(AValue, FormatSettings))
  else if FInTopArray and (FDepth = 2) then
    AddEntry(FloatToStr(AValue, FormatSettings));
end;

procedure TConfigJSONParser.OnBeginObject;
begin
  Inc(FDepth);
end;

procedure TConfigJSONParser.OnObjectKey(const AKey: string);
begin
  if FDepth = 1 then
    FCurrentKey := AKey;
end;

procedure TConfigJSONParser.OnEndObject;
begin
  Dec(FDepth);
end;

procedure TConfigJSONParser.OnBeginArray;
begin
  Inc(FDepth);
  if FDepth = 2 then
    FInTopArray := True;
end;

procedure TConfigJSONParser.OnEndArray;
begin
  if FDepth = 2 then
    FInTopArray := False;
  Dec(FDepth);
end;

function TConfigJSONParser.Parse(const AText: string): TConfigEntryArray;
begin
  FCount := 0;
  FDepth := 0;
  FInTopArray := False;
  FCurrentKey := '';
  SetLength(FEntries, 16);

  DoParse(AText);

  SetLength(FEntries, FCount);
  Result := FEntries;
end;

function ParseJSONConfig(const AContent: string): TConfigEntryArray;
var
  Parser: TConfigJSONParser;
begin
  Parser := TConfigJSONParser.Create;
  try
    Result := Parser.Parse(AContent);
  finally
    Parser.Free;
  end;
end;

{ ── Apply entries to options ───────────────────────────────── }

function FindOptionByName(const AOptions: TGocciaOptionArray;
  const AName: string): TGocciaOptionBase;
var
  I: Integer;
begin
  for I := 0 to High(AOptions) do
    if AOptions[I].LongName = AName then
      Exit(AOptions[I]);
  Result := nil;
end;

procedure ApplyConfigEntries(const AEntries: TConfigEntryArray;
  const AOptions: TGocciaOptionArray);
var
  I: Integer;
  Option: TGocciaOptionBase;
begin
  for I := 0 to High(AEntries) do
  begin
    Option := FindOptionByName(AOptions, AEntries[I].Key);
    if Option = nil then
      Continue;

    { Skip options already set by a higher-priority source (CLI).
      Repeatable options are the exception — they accumulate. }
    if Option.Present and not (Option is TGocciaRepeatableOption) then
      Continue;

    if Option is TGocciaFlagOption then
    begin
      if (AEntries[I].Value = 'true') or (AEntries[I].Value = '') then
        Option.Apply('');
    end
    else
      Option.Apply(AEntries[I].Value);
  end;
end;

{ ── Load and parse a config file ───────────────────────────── }

function ReadFileContent(const APath: string): string;
var
  Stream: TStringList;
begin
  Stream := TStringList.Create;
  try
    Stream.LoadFromFile(APath);
    Result := Stream.Text;
  finally
    Stream.Free;
  end;
end;

function ResolveParser(const AExtension: string): TConfigParseFunc;
begin
  if AExtension = '.json' then
    Result := @ParseJSONConfig
  else
  begin
    Result := FindParser(AExtension);
    if not Assigned(Result) then
      raise Exception.CreateFmt(
        'No config file parser registered for extension "%s"', [AExtension]);
  end;
end;

const
  EXTENDS_KEY = 'extends';
  MAX_EXTENDS_DEPTH = 10;

function DoParseConfigFile(const APath: string;
  const ADepth: Integer): TConfigEntryArray;
var
  Extension, Content, ExtendsPath, BaseDir: string;
  Parser: TConfigParseFunc;
  OwnEntries, BaseEntries: TConfigEntryArray;
  I, ExtendsIndex, MergedLen: Integer;
begin
  if ADepth > MAX_EXTENDS_DEPTH then
    raise Exception.Create('Circular or too-deeply-nested extends in config: ' +
      APath);

  Extension := LowerCase(ExtractFileExt(APath));
  Parser := ResolveParser(Extension);
  Content := ReadFileContent(APath);
  OwnEntries := Parser(Content);

  { Look for an "extends" entry. }
  ExtendsIndex := -1;
  for I := 0 to High(OwnEntries) do
    if OwnEntries[I].Key = EXTENDS_KEY then
    begin
      ExtendsIndex := I;
      Break;
    end;

  if ExtendsIndex < 0 then
    Exit(OwnEntries);

  { Resolve the base path relative to this config file's directory. }
  ExtendsPath := OwnEntries[ExtendsIndex].Value;
  BaseDir := ExtractFilePath(ExpandFileName(APath));
  if (ExtractFileDrive(ExtendsPath) = '') and not IsPathDelimiter(ExtendsPath, 1) then
    ExtendsPath := BaseDir + ExtendsPath;
  ExtendsPath := ExpandFileName(ExtendsPath);

  if not FileExists(ExtendsPath) then
    raise Exception.Create('Config extends target not found: ' + ExtendsPath);

  BaseEntries := DoParseConfigFile(ExtendsPath, ADepth + 1);

  { Merge: base entries first, then own entries (minus the extends key).
    When applied, later entries for the same key win because
    ApplyConfigEntries skips already-Present options.  However, since
    we want the child to override the parent, child entries must come
    first so they get applied first. }
  MergedLen := Length(OwnEntries) - 1 + Length(BaseEntries);
  SetLength(Result, MergedLen);

  { Child entries first (skipping the extends key). }
  MergedLen := 0;
  for I := 0 to High(OwnEntries) do
    if I <> ExtendsIndex then
    begin
      Result[MergedLen] := OwnEntries[I];
      Inc(MergedLen);
    end;

  { Then base entries (parent defaults, applied only if not overridden). }
  for I := 0 to High(BaseEntries) do
  begin
    Result[MergedLen] := BaseEntries[I];
    Inc(MergedLen);
  end;

  SetLength(Result, MergedLen);
end;

function ParseConfigFile(const APath: string): TConfigEntryArray;
begin
  Result := DoParseConfigFile(ExpandFileName(APath), 0);
end;

procedure ApplyConfigFile(const APath: string;
  const AOptions: TGocciaOptionArray);
var
  Entries: TConfigEntryArray;
begin
  Entries := ParseConfigFile(APath);
  ApplyConfigEntries(Entries, AOptions);
end;

{ ── Config file discovery ──────────────────────────────────── }

function DiscoverConfigFile(const AStartDirectory: string;
  const ABaseNames: array of string;
  const AExtensions: array of string): string;
var
  CurrentDirectory, ParentDirectory, CandidatePath: string;
  B, E: Integer;
begin
  if AStartDirectory <> '' then
    CurrentDirectory := ExpandFileName(AStartDirectory)
  else
    CurrentDirectory := GetCurrentDir;

  if not DirectoryExists(CurrentDirectory) then
    CurrentDirectory := ExtractFilePath(CurrentDirectory);

  CurrentDirectory := ExcludeTrailingPathDelimiter(CurrentDirectory);
  if CurrentDirectory = '' then
    CurrentDirectory := PathDelim;

  while True do
  begin
    for B := 0 to High(ABaseNames) do
      for E := 0 to High(AExtensions) do
      begin
        CandidatePath := IncludeTrailingPathDelimiter(CurrentDirectory) +
          ABaseNames[B] + AExtensions[E];
        if FileExists(CandidatePath) then
          Exit(CandidatePath);
      end;

    ParentDirectory := ExtractFileDir(CurrentDirectory);
    if (ParentDirectory = '') or (ParentDirectory = CurrentDirectory) then
      Break;

    CurrentDirectory := ParentDirectory;
  end;

  Result := '';
end;

initialization
  SetLength(GParserRegistry, 0);
end.
