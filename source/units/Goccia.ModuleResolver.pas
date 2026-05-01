unit Goccia.ModuleResolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap;

type
  TModuleResolverExtensionArray = array of string;

  TModuleResolver = class
  private
    FAliases: TStringStringMap;
    FBaseDirectory: string;
    FExtensions: TModuleResolverExtensionArray;
  protected
    function ApplyAliases(const AModulePath, AImportingFilePath: string): string;
    function TryResolveWithExtensions(const ABasePath: string; out AResolvedPath: string): Boolean;
  public
    constructor Create(const ABaseDirectory: string = '');
    destructor Destroy; override;

    procedure AddAlias(const APattern, AReplacement: string);
    function GetExtensions: TModuleResolverExtensionArray;
    function HasAlias(const AModulePath: string): Boolean;
    procedure SetExtensions(const AExtensions: array of string);
    function Resolve(const AModulePath, AImportingFilePath: string): string; virtual;

    property Aliases: TStringStringMap read FAliases;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
  end;

  EModuleNotFound = class(Exception);

implementation

uses
  FileUtils;

const
  ALIAS_SEGMENT_DELIMITER = '/';
  CURRENT_DIRECTORY_PREFIX = './';
  PARENT_DIRECTORY_PREFIX = '../';

function HasTrailingPathDelimiter(const APath: string): Boolean;
begin
  Result := (APath <> '') and (APath[Length(APath)] = PathDelim);
end;

function EnsureTrailingPathDelimiterIfNeeded(const APath: string;
  const ANeedsTrailingDelimiter: Boolean): string;
begin
  Result := APath;
  if ANeedsTrailingDelimiter and not HasTrailingPathDelimiter(Result) then
    Result := Result + PathDelim;
end;

function IsAbsolutePath(const APath: string): Boolean;
begin
  if Length(APath) = 0 then
    Exit(False);
  if APath[1] = '/' then
    Exit(True);
  if (Length(APath) >= 2) and (APath[2] = ':') then
    Exit(True);
  Result := Copy(APath, 1, 2) = '\\';
end;

function IsRelativeModuleSpecifier(const AModulePath: string): Boolean;
begin
  Result := (Copy(AModulePath, 1, Length(CURRENT_DIRECTORY_PREFIX)) =
      CURRENT_DIRECTORY_PREFIX) or
    (Copy(AModulePath, 1, Length(PARENT_DIRECTORY_PREFIX)) =
      PARENT_DIRECTORY_PREFIX);
end;

function IsURLLikeModuleSpecifier(const AModulePath: string): Boolean;
begin
  Result := IsAbsolutePath(AModulePath) or IsRelativeModuleSpecifier(AModulePath);
end;

function IsPrefixAlias(const AAlias: string): Boolean;
begin
  Result := (AAlias <> '') and (AAlias[Length(AAlias)] = ALIAS_SEGMENT_DELIMITER);
end;

function NormalizeSpecifierForMatching(const AModulePath,
  AImportingFilePath: string): string;
var
  BaseDirectory: string;
begin
  if not IsURLLikeModuleSpecifier(AModulePath) then
    Exit(AModulePath);

  if IsAbsolutePath(AModulePath) then
    Result := ExpandUTF8FileName(AModulePath)
  else
  begin
    BaseDirectory := ExtractFilePath(AImportingFilePath);
    if BaseDirectory = '' then
      BaseDirectory := GetCurrentDir + PathDelim;
    Result := ExpandUTF8FileName(BaseDirectory + AModulePath);
  end;

  Result := EnsureTrailingPathDelimiterIfNeeded(Result, IsPrefixAlias(AModulePath));
end;

function AliasMatchesModulePath(const AAlias, AModulePath,
  AImportingFilePath: string): Boolean;
var
  MatchPath: string;
begin
  if AAlias = '' then
    Exit(False);

  if IsURLLikeModuleSpecifier(AAlias) then
    MatchPath := NormalizeSpecifierForMatching(AModulePath, AImportingFilePath)
  else
    MatchPath := AModulePath;

  if IsPrefixAlias(AAlias) then
    Result := Copy(MatchPath, 1, Length(AAlias)) = AAlias
  else
    Result := MatchPath = AAlias;
end;

constructor TModuleResolver.Create(const ABaseDirectory: string);
begin
  FAliases := TStringStringMap.Create;
  SetLength(FExtensions, 0);
  if ABaseDirectory <> '' then
    FBaseDirectory := IncludeTrailingPathDelimiter(ExpandUTF8FileName(ABaseDirectory))
  else
    FBaseDirectory := IncludeTrailingPathDelimiter(GetCurrentDir);
end;

destructor TModuleResolver.Destroy;
begin
  FAliases.Free;
  inherited;
end;

procedure TModuleResolver.AddAlias(const APattern, AReplacement: string);
begin
  FAliases.AddOrSetValue(APattern, AReplacement);
end;

function TModuleResolver.GetExtensions: TModuleResolverExtensionArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FExtensions));
  for I := 0 to High(FExtensions) do
    Result[I] := FExtensions[I];
end;

function TModuleResolver.HasAlias(const AModulePath: string): Boolean;
var
  Pair: TStringStringMap.TKeyValuePair;
begin
  for Pair in FAliases do
    if AliasMatchesModulePath(Pair.Key, AModulePath, '') then
      Exit(True);
  Result := False;
end;

procedure TModuleResolver.SetExtensions(const AExtensions: array of string);
var
  I: Integer;
begin
  SetLength(FExtensions, Length(AExtensions));
  for I := 0 to High(AExtensions) do
    FExtensions[I] := AExtensions[I];
end;

function TModuleResolver.ApplyAliases(const AModulePath,
  AImportingFilePath: string): string;
var
  Pair: TStringStringMap.TKeyValuePair;
  BestKey, BestValue, MatchPath, Replacement: string;
  Found: Boolean;
begin
  Result := AModulePath;
  Found := False;

  for Pair in FAliases do
  begin
    if AliasMatchesModulePath(Pair.Key, AModulePath, AImportingFilePath) then
    begin
      if (not Found) or (Length(Pair.Key) > Length(BestKey)) then
      begin
        BestKey := Pair.Key;
        BestValue := Pair.Value;
        Found := True;
      end;
    end;
  end;

  if Found then
  begin
    if IsURLLikeModuleSpecifier(BestKey) then
      MatchPath := NormalizeSpecifierForMatching(AModulePath, AImportingFilePath)
    else
      MatchPath := AModulePath;

    if IsPrefixAlias(BestKey) then
      Replacement := BestValue + Copy(MatchPath, Length(BestKey) + 1, MaxInt)
    else
      Replacement := BestValue;

    if not IsAbsolutePath(Replacement) then
      Result := FBaseDirectory + Replacement
    else
      Result := Replacement;
  end;
end;

function TModuleResolver.TryResolveWithExtensions(const ABasePath: string; out AResolvedPath: string): Boolean;
var
  I: Integer;
begin
  if UTF8FileExists(ABasePath) then
  begin
    AResolvedPath := ABasePath;
    Exit(True);
  end;

  for I := 0 to High(FExtensions) do
  begin
    if UTF8FileExists(ABasePath + FExtensions[I]) then
    begin
      AResolvedPath := ABasePath + FExtensions[I];
      Exit(True);
    end;
  end;

  for I := 0 to High(FExtensions) do
  begin
    if UTF8FileExists(ABasePath + PathDelim + 'index' + FExtensions[I]) then
    begin
      AResolvedPath := ABasePath + PathDelim + 'index' + FExtensions[I];
      Exit(True);
    end;
  end;

  Result := False;
end;

function TModuleResolver.Resolve(const AModulePath, AImportingFilePath: string): string;
var
  AliasApplied, BaseDirectory: string;
begin
  AliasApplied := ApplyAliases(AModulePath, AImportingFilePath);

  if AliasApplied <> AModulePath then
  begin
    if TryResolveWithExtensions(ExpandUTF8FileName(AliasApplied), Result) then
      Exit;
    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s" (alias resolved to "%s")', [AModulePath, ExpandUTF8FileName(AliasApplied)]);
  end;

  if IsAbsolutePath(AModulePath) then
  begin
    if TryResolveWithExtensions(ExpandUTF8FileName(AModulePath), Result) then
      Exit;
    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s"', [AModulePath]);
  end;

  if (Copy(AModulePath, 1, 2) = './') or (Copy(AModulePath, 1, 3) = '../') then
  begin
    BaseDirectory := ExtractFilePath(AImportingFilePath);
    if BaseDirectory = '' then
      BaseDirectory := GetCurrentDir + PathDelim;

    if TryResolveWithExtensions(ExpandUTF8FileName(BaseDirectory + AModulePath), Result) then
      Exit;

    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s" (resolved to "%s")', [AModulePath, ExpandUTF8FileName(BaseDirectory + AModulePath)]);
  end;

  raise EModuleNotFound.CreateFmt(
    'Cannot resolve bare module specifier "%s". Imports must start with "./" or "../"', [AModulePath]);
end;

end.
