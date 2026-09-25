unit Goccia.Modules.Resolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Error,
  Goccia.ModuleResolver;

type
  TGocciaModuleResolver = class(TModuleResolver)
  protected
    function IsAbsoluteImportMapPath(const APath: string): Boolean; virtual;
    function IsRelativeImportMapPath(const APath: string): Boolean; virtual;
    function NormalizeImportMapBaseDirectory(
      const AImportMapDirectory: string): string; virtual;
    function NormalizeImportMapPath(const APath, ABaseDirectory: string): string;
      virtual;
  public
    constructor Create(const ABaseDirectory: string = '');
    class function DiscoverProjectConfig(const AStartDirectory: string): string; static;
    procedure LoadImportMap(const APath: string);
  end;

  EGocciaModuleNotFound = EModuleNotFound;

  { The module loader turns EGocciaModuleNotFound into this runtime error so a
    failed import is catchable from script. Message stays specifier-only —
    ResolvedCandidatePath carries the expanded host address for host-side
    diagnostics and is never copied into a script-visible error (ADR 0108). }
  TGocciaModuleResolutionError = class(TGocciaRuntimeError)
  private
    FResolvedCandidatePath: string;
  public
    constructor CreateResolutionFailure(const AMessage,
      AResolvedCandidatePath, AFileName: string);

    property ResolvedCandidatePath: string read FResolvedCandidatePath;
  end;

{ Renders any engine error for host output: the usual detailed message plus,
  for a module resolution failure, the expanded candidate path the resolver
  tried. Host reporters use this instead of GetDetailedMessage so the candidate
  path kept out of AError.Message still reaches the host. }
function FormatHostErrorDiagnostic(const AError: TGocciaError;
  const AUseColor: Boolean): string;

implementation

uses
  FileUtils,

  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.TextFiles,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  PROJECT_CONFIG_FILE_NAME = 'goccia.json';
  IMPORTS_PROPERTY_NAME = 'imports';
  CURRENT_DIRECTORY_PREFIX = './';
  PARENT_DIRECTORY_PREFIX = '../';
  RESOLVED_CANDIDATE_DIAGNOSTIC_FORMAT = '  Resolved to: %s';

constructor TGocciaModuleResolutionError.CreateResolutionFailure(
  const AMessage, AResolvedCandidatePath, AFileName: string);
begin
  inherited Create(AMessage, 0, 0, AFileName, nil);
  FResolvedCandidatePath := AResolvedCandidatePath;
end;

function FormatHostErrorDiagnostic(const AError: TGocciaError;
  const AUseColor: Boolean): string;
begin
  Result := AError.GetDetailedMessage(AUseColor);
  if (AError is TGocciaModuleResolutionError) and
     (TGocciaModuleResolutionError(AError).ResolvedCandidatePath <> '') then
    Result := Result + Format(RESOLVED_CANDIDATE_DIAGNOSTIC_FORMAT,
      [TGocciaModuleResolutionError(AError).ResolvedCandidatePath]) + sLineBreak;
end;

function TGocciaModuleResolver.IsAbsoluteImportMapPath(
  const APath: string): Boolean;
begin
  if Length(APath) = 0 then
    Exit(False);
  if APath[1] = PathDelim then
    Exit(True);
  if (Length(APath) >= 2) and (APath[2] = ':') then
    Exit(True);
  Result := Copy(APath, 1, 2) = '\\';
end;

function TGocciaModuleResolver.IsRelativeImportMapPath(
  const APath: string): Boolean;
begin
  Result := (Copy(APath, 1, Length(CURRENT_DIRECTORY_PREFIX)) =
      CURRENT_DIRECTORY_PREFIX) or
    (Copy(APath, 1, Length(PARENT_DIRECTORY_PREFIX)) =
      PARENT_DIRECTORY_PREFIX);
end;

function HasImportMapTrailingSlash(const APath: string): Boolean;
begin
  Result := (APath <> '') and (APath[Length(APath)] = '/');
end;

function TGocciaModuleResolver.NormalizeImportMapBaseDirectory(
  const AImportMapDirectory: string): string;
begin
  Result := AImportMapDirectory;
end;

function TGocciaModuleResolver.NormalizeImportMapPath(const APath,
  ABaseDirectory: string): string;
begin
  if IsAbsoluteImportMapPath(APath) then
    Result := ExpandHostFileName(APath)
  else if IsRelativeImportMapPath(APath) then
    Result := ExpandHostFileName(ABaseDirectory + APath)
  else
    Result := APath;

  if HasImportMapTrailingSlash(APath) then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function ReadImportMapText(const APath: string): string;
begin
  Result := ReadUTF8FileText(APath);
end;

constructor TGocciaModuleResolver.Create(const ABaseDirectory: string);
begin
  inherited Create(ABaseDirectory);
  SetExtensions(EngineModuleImportExtensions);
end;

class function TGocciaModuleResolver.DiscoverProjectConfig(
  const AStartDirectory: string): string;
var
  CandidatePath, CurrentDirectory, ParentDirectory: string;
begin
  if AStartDirectory <> '' then
    CurrentDirectory := ExpandHostFileName(AStartDirectory)
  else
    CurrentDirectory := GetCurrentDir;

  if not HostDirectoryExists(CurrentDirectory) then
    CurrentDirectory := ExtractFilePath(CurrentDirectory);

  CurrentDirectory := ExcludeTrailingPathDelimiter(CurrentDirectory);
  if CurrentDirectory = '' then
    CurrentDirectory := PathDelim;

  while True do
  begin
    CandidatePath := IncludeTrailingPathDelimiter(CurrentDirectory) +
      PROJECT_CONFIG_FILE_NAME;
    if HostFileExists(CandidatePath) then
      Exit(CandidatePath);

    ParentDirectory := ExtractFileDir(CurrentDirectory);
    if (ParentDirectory = '') or (ParentDirectory = CurrentDirectory) then
      Break;

    CurrentDirectory := ParentDirectory;
  end;

  Result := '';
end;

procedure TGocciaModuleResolver.LoadImportMap(const APath: string);
var
  ImportMapBaseDirectory, ImportMapDirectory, ImportMapPath, Key: string;
  NormalizedKey, NormalizedValue: string;
  Parser: TGocciaJSONParser;
  ParsedValue, ImportsValue, Value: TGocciaValue;
  ImportsObject, ImportMapObject: TGocciaObjectValue;
begin
  ImportMapPath := ExpandHostFileName(APath);
  if not HostFileExists(ImportMapPath) then
    raise Exception.Create('Import map not found: ' + ImportMapPath);

  Parser := TGocciaJSONParser.Create;
  try
    ParsedValue := Parser.Parse(ReadImportMapText(ImportMapPath));
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise Exception.Create('Import map must be a top-level JSON object.');

  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    ImportMapObject := TGocciaObjectValue(ParsedValue);
    ImportsValue := ImportMapObject.GetProperty(IMPORTS_PROPERTY_NAME);
    if (not Assigned(ImportsValue)) or
       (ImportsValue is TGocciaUndefinedLiteralValue) then
      Exit;
    if not (ImportsValue is TGocciaObjectValue) then
      raise Exception.Create('Import map "imports" field must be a JSON object.');

    ImportsObject := TGocciaObjectValue(ImportsValue);
    ImportMapDirectory := IncludeTrailingPathDelimiter(
      ExtractFilePath(ImportMapPath));
    ImportMapBaseDirectory := NormalizeImportMapBaseDirectory(
      ImportMapDirectory);

    for Key in ImportsObject.GetOwnPropertyKeys do
    begin
      Value := ImportsObject.GetProperty(Key);
      if not (Value is TGocciaStringLiteralValue) then
        raise Exception.CreateFmt(
          'Import map entry "%s" must map to a string address.', [Key]);

      if HasImportMapTrailingSlash(Key) and
         not HasImportMapTrailingSlash(TGocciaStringLiteralValue(Value).Value) then
        raise Exception.CreateFmt(
          'Import map entry "%s" ends with "/" so its address must also end with "/".',
          [Key]);

      if not (IsAbsoluteImportMapPath(TGocciaStringLiteralValue(Value).Value) or
              IsRelativeImportMapPath(TGocciaStringLiteralValue(Value).Value)) then
        raise Exception.CreateFmt(
          'Import map entry "%s" must use an absolute or relative file path address.',
          [Key]);

      NormalizedKey := NormalizeImportMapPath(Key, ImportMapBaseDirectory);
      NormalizedValue := NormalizeImportMapPath(
        TGocciaStringLiteralValue(Value).Value, ImportMapBaseDirectory);
      AddAlias(NormalizedKey, NormalizedValue);
    end;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

end.
