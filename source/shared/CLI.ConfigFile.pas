unit CLI.ConfigFile;

{$I Shared.inc}

interface

uses
  CLI.Options;

type
  { What a config value was before it became text. cvkUnsupported marks a
    value with no flat form (null, an object below the first nesting level,
    an array element that is not a scalar): option application and lookups
    skip it, and a consumer that must not ignore it (a permissions block) can
    reject it. }
  TConfigValueKind = (cvkString, cvkNumber, cvkBoolean, cvkEmptyArray,
    cvkUnsupported, cvkObject);

  { A single key-value pair extracted from a configuration file. }
  TConfigEntry = record
    Key: string;
    Value: string;
    { The file that declared the entry. With `extends`, base entries keep
      their own file, so relative values resolve against where they were
      written. }
    SourcePath: string;
    Kind: TConfigValueKind;
    { True for each element of an array value. }
    InArray: Boolean;
    { A number's spelling in the file when it differs from Value (1e8 for
      Value 100000000), for messages; '' otherwise. }
    Written: string;
  end;
  TConfigEntryArray = array of TConfigEntry;

  { Callback that parses file content into flat key-value entries.
    Keys are option long names or config names (e.g. 'mode', 'timeout').
    Values are their string representations (e.g. 'bytecode', '5000').
    Boolean true produces 'true'; false produces 'false'.
    Arrays produce multiple entries with the same key; an empty array
    produces one cvkEmptyArray entry with an empty value.
    Nested objects are flattened one level: an "allow-net" array inside a
    "permissions" object produces `permissions.allow-net` entries.
    null values, deeper objects, and non-scalar array elements produce
    cvkUnsupported entries. Each flattened object is also recorded as one
    cvkObject entry under its own key, before its children, so a key whose
    value must not be an object can be rejected even when the object is
    empty. Lookups and option application skip both kinds except where an
    option accepts an object (TOptionBase.AcceptsObject). }
  TConfigParseFunc = function(const AContent: string): TConfigEntryArray;

{ The value text of a config number, the same for every config format: an
  exact whole number as plain digits (so 1e8 reads as 100000000 and an
  oversized one is reported as too large), anything else in the invariant
  float spelling. }
function ConfigNumberText(const AValue: Double): string;

{ An entry's value as the config wrote it (Written when set), for messages. }
function ConfigEntryText(const AEntry: TConfigEntry): string;

{ Register a parser for a file extension.
  The extension must include the leading dot and is matched
  case-insensitively (e.g. '.json', '.json5', '.toml'). }
procedure RegisterConfigParser(const AExtension: string;
  AParser: TConfigParseFunc);

{ Apply configuration entries to the given options.
  Each entry whose key matches an option long name or config name is applied.
  Options that are already Present are skipped, so CLI options
  parsed before this call naturally take precedence.
  Unknown keys are silently skipped (config files may contain
  keys for other subsystems).
  A removed option's key or a command-line-only key raises TCLIUsageError; a
  flag whose value is not exactly true or false raises TParseError. Options
  marked RequiresTrust are skipped.
  For repeatable options, values are accumulated even when Present. }
procedure ApplyConfigEntries(const AEntries: TConfigEntryArray;
  const AOptions: TOptionArray);

{ The checks ApplyConfigEntries makes, without applying anything, plus each
  option's value check (CheckValue). For per-file configs, which are read
  through FindConfigEntry instead of being applied to the options. Options
  marked ConfigIgnored are skipped entirely. }
procedure ValidateConfigEntries(const AEntries: TConfigEntryArray;
  const AOptions: TOptionArray);

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
  const AOptions: TOptionArray);

{ Walk up from AStartDirectory looking for a configuration file.
  At each directory level, tries every combination of
  ABaseNames x AExtensions in the order given (extensions are the
  inner loop, so the first extension has highest priority).
  Returns the full path of the first file found, or '' if none. }
function DiscoverConfigFile(const AStartDirectory: string;
  const ABaseNames: array of string;
  const AExtensions: array of string): string;

{ Look up a key in a config entry array.  Returns True and sets
  AValue if a matching entry exists, False otherwise.  When
  multiple entries share the same key (e.g. repeatable options),
  the first match wins. }
function FindConfigEntry(const AEntries: TConfigEntryArray;
  const AKey: string; out AValue: string): Boolean;
{ As FindConfigEntry, returning the whole entry. }
function TryFindConfigEntry(const AEntries: TConfigEntryArray;
  const AKey: string; out AEntry: TConfigEntry): Boolean;

{ Resolve an effective boolean for a flag option using the
  standard precedence: CLI flag > per-file config > root config >
  default (False).  AFlag is the parsed option and AFileConfig the
  per-file config entries.  The config key is resolved from the
  option metadata, checking ConfigName before LongName. }
function ResolveFlagOption(const AFlag: TFlagOption;
  const AFileConfig: TConfigEntryArray): Boolean;

implementation

uses
  Classes,
  Math,
  SysUtils,

  FileUtils,
  JSONParser,
  NumericText,
  TextSemantics;

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
    begin
      Result := GParserRegistry[I].Parser;
      Exit;
    end;
  Result := nil;
end;

{ ── Built-in JSON config parser (SAX-based) ────────────────── }

const
  NESTED_KEY_SEPARATOR = '.';

type
  { SAX parser that extracts the top-level key-value pairs of a JSON object:
    scalars, flat arrays of scalars, and one level of nested objects, whose
    keys are flattened to `parent.child`. Deeper objects and null values are
    silently skipped. }
  TConfigJSONParser = class(TAbstractJSONParser)
  private
    FEntries: TConfigEntryArray;
    FCount: Integer;
    FTopKey: string;
    FChildKey: string;
    FDepth: Integer;
    FInNestedObject: Boolean;
    FArrayDepth: Integer;
    FArrayHadElements: Boolean;
    { Depth of an unrepresentable object or array being skipped; 0 when none. }
    FSkipDepth: Integer;
    { The spelling of the number about to be added, when it differs. }
    FPendingWritten: string;
    function CurrentKey: string;
    { True, after recording it, when a container opened at FDepth has no flat
      form: an element of a collected array, or an object as a nested
      object's value. }
    function MarkUnsupportedContainer(const AIsObject: Boolean): Boolean;
    procedure AddEntry(const AValue: string; const AKind: TConfigValueKind;
      const AInArray: Boolean);
    procedure AddScalar(const AValue: string; const AKind: TConfigValueKind);
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

function TConfigJSONParser.CurrentKey: string;
begin
  if FInNestedObject and (FChildKey <> '') then
    Result := FTopKey + NESTED_KEY_SEPARATOR + FChildKey
  else
    Result := FTopKey;
end;

procedure TConfigJSONParser.AddEntry(const AValue: string;
  const AKind: TConfigValueKind; const AInArray: Boolean);
begin
  if FCount >= Length(FEntries) then
    SetLength(FEntries, Length(FEntries) * 2 + 8);
  FEntries[FCount].Key := CurrentKey;
  FEntries[FCount].Value := AValue;
  FEntries[FCount].SourcePath := '';
  FEntries[FCount].Kind := AKind;
  FEntries[FCount].InArray := AInArray;
  FEntries[FCount].Written := FPendingWritten;
  FPendingWritten := '';
  Inc(FCount);
end;

{ A scalar is collected when it is a top-level value, a nested object's
  value, or an element directly inside a collected array. }
procedure TConfigJSONParser.AddScalar(const AValue: string;
  const AKind: TConfigValueKind);
begin
  if FArrayDepth > 0 then
  begin
    if FDepth = FArrayDepth then
    begin
      FArrayHadElements := True;
      AddEntry(AValue, AKind, True);
    end;
  end
  else if (FDepth = 1) or ((FDepth = 2) and FInNestedObject) then
    AddEntry(AValue, AKind, False);
end;

function TConfigJSONParser.MarkUnsupportedContainer(
  const AIsObject: Boolean): Boolean;
begin
  Result := False;
  if (FArrayDepth > 0) and (FDepth = FArrayDepth + 1) then
  begin
    FArrayHadElements := True;
    AddEntry('', cvkUnsupported, True);
    Result := True;
  end
  else if AIsObject and (FArrayDepth = 0) and FInNestedObject and
    (FDepth = 3) then
  begin
    AddEntry('', cvkUnsupported, False);
    Result := True;
  end;
  if Result then
    FSkipDepth := FDepth;
end;

procedure TConfigJSONParser.OnNull;
begin
  if FSkipDepth > 0 then
    Exit;
  AddScalar('', cvkUnsupported);
end;

procedure TConfigJSONParser.OnBoolean(const AValue: Boolean);
begin
  if FSkipDepth > 0 then
    Exit;
  if AValue then
    AddScalar('true', cvkBoolean)
  else
    AddScalar('false', cvkBoolean);
end;

procedure TConfigJSONParser.OnString(const AValue: string);
begin
  if FSkipDepth > 0 then
    Exit;
  AddScalar(AValue, cvkString);
end;

procedure TConfigJSONParser.OnInteger(const AValue: Int64);
begin
  if FSkipDepth > 0 then
    Exit;
  AddScalar(IntToStr(AValue), cvkNumber);
end;

procedure TConfigJSONParser.OnFloat(const AValue: Double);
var
  Text: string;
begin
  if FSkipDepth > 0 then
    Exit;
  Text := ConfigNumberText(AValue);
  if (LastNumberText <> '') and (LastNumberText <> Text) then
    FPendingWritten := LastNumberText;
  AddScalar(Text, cvkNumber);
  FPendingWritten := '';
end;

procedure TConfigJSONParser.OnBeginObject;
begin
  Inc(FDepth);
  if (FSkipDepth > 0) or MarkUnsupportedContainer(True) then
    Exit;
  if (FDepth = 2) and (FArrayDepth = 0) then
  begin
    FInNestedObject := True;
    FChildKey := '';
    AddEntry('', cvkObject, False);
  end;
end;

procedure TConfigJSONParser.OnObjectKey(const AKey: string);
begin
  if FSkipDepth > 0 then
    Exit;
  if FDepth = 1 then
    FTopKey := AKey
  else if (FDepth = 2) and FInNestedObject then
    FChildKey := AKey;
end;

procedure TConfigJSONParser.OnEndObject;
begin
  if FSkipDepth = FDepth then
    FSkipDepth := 0
  else if (FSkipDepth = 0) and (FDepth = 2) and FInNestedObject then
    FInNestedObject := False;
  Dec(FDepth);
end;

procedure TConfigJSONParser.OnBeginArray;
begin
  Inc(FDepth);
  if (FSkipDepth > 0) or MarkUnsupportedContainer(False) then
    Exit;
  if (FArrayDepth = 0) and
     (((FDepth = 2) and not FInNestedObject) or
      ((FDepth = 3) and FInNestedObject)) then
  begin
    FArrayDepth := FDepth;
    FArrayHadElements := False;
  end;
end;

procedure TConfigJSONParser.OnEndArray;
begin
  if FSkipDepth = FDepth then
    FSkipDepth := 0
  else if (FSkipDepth = 0) and (FDepth = FArrayDepth) then
  begin
    FArrayDepth := 0;
    if not FArrayHadElements then
      AddEntry('', cvkEmptyArray, True);
  end;
  Dec(FDepth);
end;

function TConfigJSONParser.Parse(const AText: string): TConfigEntryArray;
begin
  FCount := 0;
  FDepth := 0;
  FInNestedObject := False;
  FArrayDepth := 0;
  FArrayHadElements := False;
  FSkipDepth := 0;
  FTopKey := '';
  FChildKey := '';
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

function ConfigNumberText(const AValue: Double): string;
const
  { Beyond this a whole number has no exact digits worth showing; every
    such value is far past any limit, so digits still read as too large. }
  LARGEST_PRINTED_WHOLE = 1e300;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := CreateInvariantFormatSettings;
  if (not IsNan(AValue)) and (not IsInfinite(AValue)) and
     (Frac(AValue) = 0) and (Abs(AValue) < LARGEST_PRINTED_WHOLE) then
    Result := Format('%.0f', [AValue], FormatSettings)
  else
    Result := FloatToStr(AValue, FormatSettings);
end;

{ ── Apply entries to options ───────────────────────────────── }

{ True for an entry that carries a value: not an unrepresentable value and
  not the marker of an object. }
function IsValueEntry(const AEntry: TConfigEntry): Boolean;
begin
  Result := not (AEntry.Kind in [cvkUnsupported, cvkObject]);
end;

function FindOptionByName(const AOptions: TOptionArray;
  const AName: string): TOptionBase;
var
  I: Integer;
begin
  for I := 0 to High(AOptions) do
    if (AOptions[I].LongName = AName) or
       ((AOptions[I].ConfigName <> '') and
        (AOptions[I].ConfigName = AName)) then
      Exit(AOptions[I]);
  Result := nil;
end;

function FindOptionConfigEntry(const AEntries: TConfigEntryArray;
  const AOption: TOptionBase; out AValue: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AEntries) do
    if IsValueEntry(AEntries[I]) and
       (((AOption.ConfigName <> '') and
        (AEntries[I].Key = AOption.ConfigName)) or
       (AEntries[I].Key = AOption.LongName)) then
    begin
      AValue := AEntries[I].Value;
      Exit(True);
    end;
  Result := False;
end;

function ConfigEntryLocation(const AEntry: TConfigEntry): string;
begin
  if AEntry.SourcePath <> '' then
    Result := AEntry.SourcePath
  else
    Result := 'config';
end;

function DescribeFlagValue(const AEntry: TConfigEntry): string;
begin
  if AEntry.InArray then
    Result := 'an array'
  else if AEntry.Kind = cvkObject then
    Result := 'an object'
  else if AEntry.Kind = cvkUnsupported then
    Result := 'null'
  else if AEntry.Kind = cvkString then
    Result := '"' + AEntry.Value + '"'
  else
    Result := AEntry.Value;
end;

function ConfigEntryText(const AEntry: TConfigEntry): string;
begin
  if AEntry.Written <> '' then
    Result := AEntry.Written
  else
    Result := AEntry.Value;
end;

{ Restates an option's value error in config spelling. }
procedure RaiseConfigValueError(const AEntry: TConfigEntry;
  const AError: TParseError);
begin
  if AError is EOptionValueError then
    raise TParseError.CreateFmt('Invalid value for "%s" in %s: %s (%s)',
      [AEntry.Key, ConfigEntryLocation(AEntry), ConfigEntryText(AEntry),
       EOptionValueError(AError).Reason]);
  raise TParseError.CreateFmt('%s: %s',
    [ConfigEntryLocation(AEntry), AError.Message]);
end;

{ The checks shared by ApplyConfigEntries and ValidateConfigEntries. False
  when the entry must not be applied (a RequiresTrust key). A flag takes
  exactly a boolean; any other option other than a repeatable one takes a
  single scalar. }
function CheckConfigEntry(const AEntry: TConfigEntry;
  const AOption: TOptionBase): Boolean;
begin
  if AOption.ConfigIgnored then
    Exit(False);
  if AOption is TRemovedOption then
    raise TCLIUsageError.Create(TRemovedOption(AOption).RemovedConfigMessage(
      ConfigEntryLocation(AEntry)));
  if AOption.CommandLineOnly then
    raise TCLIUsageError.CreateFmt(
      '%s: "%s" can only be given on the command line%s',
      [ConfigEntryLocation(AEntry), AEntry.Key, AOption.ConfigHint]);
  if (AEntry.Kind = cvkObject) and AOption.AcceptsObject then
    Exit(not AOption.RequiresTrust);
  if AOption is TFlagOption then
  begin
    if (AEntry.Kind <> cvkBoolean) or AEntry.InArray then
      raise TParseError.CreateFmt('%s: "%s" must be true or false, got %s',
        [ConfigEntryLocation(AEntry), AEntry.Key, DescribeFlagValue(AEntry)]);
  end
  else if AOption is TRepeatableOption then
  begin
    if AEntry.Kind in [cvkUnsupported, cvkObject] then
      raise TParseError.CreateFmt(
        '%s: "%s" must be a value or an array of values',
        [ConfigEntryLocation(AEntry), AEntry.Key]);
  end
  else if AEntry.Kind in [cvkUnsupported, cvkObject] then
    raise TParseError.CreateFmt(
      '%s: "%s" must be a single value, not null or an object',
      [ConfigEntryLocation(AEntry), AEntry.Key])
  else if AEntry.InArray then
    raise TParseError.CreateFmt(
      '%s: "%s" must be a single value, not an array',
      [ConfigEntryLocation(AEntry), AEntry.Key]);
  Result := not AOption.RequiresTrust;
end;

procedure ValidateConfigEntries(const AEntries: TConfigEntryArray;
  const AOptions: TOptionArray);
var
  I: Integer;
  Option: TOptionBase;
begin
  for I := 0 to High(AEntries) do
  begin
    Option := FindOptionByName(AOptions, AEntries[I].Key);
    if (Option = nil) or not CheckConfigEntry(AEntries[I], Option) then
      Continue;
    if (Option is TFlagOption) or
       (AEntries[I].Kind in [cvkEmptyArray, cvkObject]) then
      Continue;
    try
      Option.CheckValue(AEntries[I].Value);
    except
      on E: TCLIUsageError do
        raise;
      on E: TParseError do
        RaiseConfigValueError(AEntries[I], E);
    end;
  end;
end;

procedure ApplyConfigEntries(const AEntries: TConfigEntryArray;
  const AOptions: TOptionArray);
var
  I: Integer;
  Option: TOptionBase;
begin
  for I := 0 to High(AEntries) do
  begin
    Option := FindOptionByName(AOptions, AEntries[I].Key);
    if Option = nil then
      Continue;
    if not CheckConfigEntry(AEntries[I], Option) then
      Continue;
    { An accepted object (a modules descriptor map) is read by its owner. }
    if AEntries[I].Kind = cvkObject then
      Continue;

    { Skip options already set by a higher-priority source (CLI or
      a child config's explicit empty array).  Repeatable options
      normally accumulate from config, but not when the CLI set
      them or when the child config explicitly set an empty array
      (Present with zero values). }
    if Option.Present and
       (not (Option is TRepeatableOption) or
        Option.FromCommandLine or
        (TRepeatableOption(Option).Values.Count = 0)) then
      Continue;

    { Empty-array sentinel: mark the option as present so the
      explicit empty override is visible, but do not add a value. }
    if (Option is TRepeatableOption) and
       (AEntries[I].Value = '') then
    begin
      Option.MarkPresent;
      Continue;
    end;

    if Option is TFlagOption then
    begin
      if AEntries[I].Value = 'true' then
        Option.Apply('');
    end
    else
      try
        Option.Apply(AEntries[I].Value);
      except
        on E: TCLIUsageError do
          raise;
        on E: TParseError do
          RaiseConfigValueError(AEntries[I], E);
      end;
  end;
end;

{ ── Load and parse a config file ───────────────────────────── }

function ReadFileContent(const APath: string): string;
begin
  Result := ReadUTF8FileText(APath);
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
  for I := 0 to High(OwnEntries) do
    OwnEntries[I].SourcePath := APath;

  { Look for an "extends" entry. }
  ExtendsIndex := -1;
  for I := 0 to High(OwnEntries) do
    if OwnEntries[I].Key = EXTENDS_KEY then
    begin
      if (OwnEntries[I].Kind <> cvkString) or OwnEntries[I].InArray then
        raise TParseError.CreateFmt('%s: "%s" must be a path',
          [APath, EXTENDS_KEY]);
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
  const AOptions: TOptionArray);
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

{ ── Entry lookup ───────────────────────────────────────────── }

function FindConfigEntry(const AEntries: TConfigEntryArray;
  const AKey: string; out AValue: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AEntries) do
    if (AEntries[I].Key = AKey) and IsValueEntry(AEntries[I]) then
    begin
      AValue := AEntries[I].Value;
      Exit(True);
    end;
  Result := False;
end;

function TryFindConfigEntry(const AEntries: TConfigEntryArray;
  const AKey: string; out AEntry: TConfigEntry): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AEntries) do
    if (AEntries[I].Key = AKey) and IsValueEntry(AEntries[I]) then
    begin
      AEntry := AEntries[I];
      Exit(True);
    end;
  AEntry := Default(TConfigEntry);
  Result := False;
end;

{ ── Flag resolution ────────────────────────────────────────── }

function ResolveFlagOption(const AFlag: TFlagOption;
  const AFileConfig: TConfigEntryArray): Boolean;
var
  ValueStr: string;
begin
  if AFlag.FromCommandLine then
    Result := True
  else if FindOptionConfigEntry(AFileConfig, AFlag, ValueStr) then
    Result := ValueStr = 'true'
  else
    Result := AFlag.Present;
end;

initialization
  SetLength(GParserRegistry, 0);
end.
