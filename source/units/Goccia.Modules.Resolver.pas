unit Goccia.Modules.Resolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.ModuleResolver;

type
  TGocciaModuleResolver = class(TModuleResolver)
  public
    constructor Create(const ABaseDirectory: string = '');
    class function DiscoverProjectConfig(const AStartDirectory: string): string; static;
    procedure LoadImportMap(const APath: string);
  end;

  EGocciaModuleNotFound = EModuleNotFound;

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

function IsAbsoluteImportMapPath(const APath: string): Boolean;
begin
  if Length(APath) = 0 then
    Exit(False);
  if APath[1] = PathDelim then
    Exit(True);
  if (Length(APath) >= 2) and (APath[2] = ':') then
    Exit(True);
  Result := Copy(APath, 1, 2) = '\\';
end;

function IsRelativeImportMapPath(const APath: string): Boolean;
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

function NormalizeImportMapPath(const APath, ABaseDirectory: string): string;
begin
  if IsAbsoluteImportMapPath(APath) then
    Result := ExpandUTF8FileName(APath)
  else if IsRelativeImportMapPath(APath) then
    Result := ExpandUTF8FileName(ABaseDirectory + APath)
  else
    Result := APath;

  if HasImportMapTrailingSlash(APath) then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function ReadImportMapText(const APath: string): UTF8String;
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
    CurrentDirectory := ExpandUTF8FileName(AStartDirectory)
  else
    CurrentDirectory := GetCurrentDir;

  if not UTF8DirectoryExists(CurrentDirectory) then
    CurrentDirectory := ExtractFilePath(CurrentDirectory);

  CurrentDirectory := ExcludeTrailingPathDelimiter(CurrentDirectory);
  if CurrentDirectory = '' then
    CurrentDirectory := PathDelim;

  while True do
  begin
    CandidatePath := IncludeTrailingPathDelimiter(CurrentDirectory) +
      PROJECT_CONFIG_FILE_NAME;
    if UTF8FileExists(CandidatePath) then
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
  ImportMapDirectory, ImportMapPath, Key, NormalizedKey, NormalizedValue: string;
  Parser: TGocciaJSONParser;
  ParsedValue, ImportsValue, Value: TGocciaValue;
  ImportsObject, ImportMapObject: TGocciaObjectValue;
begin
  ImportMapPath := ExpandUTF8FileName(APath);
  if not UTF8FileExists(ImportMapPath) then
    raise Exception.Create('Import map not found: ' + ImportMapPath);

  Parser := TGocciaJSONParser.Create;
  try
    ParsedValue := Parser.Parse(ReadImportMapText(ImportMapPath));
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise Exception.Create('Import map must be a top-level JSON object.');

  if Assigned(TGarbageCollector.Instance) then
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

      NormalizedKey := NormalizeImportMapPath(Key, ImportMapDirectory);
      NormalizedValue := NormalizeImportMapPath(
        TGocciaStringLiteralValue(Value).Value, ImportMapDirectory);
      AddAlias(NormalizedKey, NormalizedValue);
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

end.
