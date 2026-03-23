unit Modules.Resolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap;

type
  TModuleResolver = class
  private
    FAliases: TStringStringMap;
    FBaseDirectory: string;
    FExtensions: array of string;
  protected
    function ApplyAliases(const AModulePath: string): string;
    function TryResolveWithExtensions(const ABasePath: string; out AResolvedPath: string): Boolean;
  public
    constructor Create(const ABaseDirectory: string = '');
    destructor Destroy; override;

    procedure AddAlias(const APattern, AReplacement: string);
    function HasAlias(const AModulePath: string): Boolean;
    procedure SetExtensions(const AExtensions: array of string);
    function Resolve(const AModulePath, AImportingFilePath: string): string; virtual;

    property Aliases: TStringStringMap read FAliases;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
  end;

  EModuleNotFound = class(Exception);

implementation

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

constructor TModuleResolver.Create(const ABaseDirectory: string);
begin
  FAliases := TStringStringMap.Create;
  SetLength(FExtensions, 0);
  if ABaseDirectory <> '' then
    FBaseDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ABaseDirectory))
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

function TModuleResolver.HasAlias(const AModulePath: string): Boolean;
var
  Pair: TStringStringMap.TKeyValuePair;
begin
  for Pair in FAliases do
    if (Length(AModulePath) >= Length(Pair.Key)) and
       (Copy(AModulePath, 1, Length(Pair.Key)) = Pair.Key) then
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

function TModuleResolver.ApplyAliases(const AModulePath: string): string;
var
  Pair: TStringStringMap.TKeyValuePair;
  BestKey, BestValue, Replacement: string;
  Found: Boolean;
begin
  Result := AModulePath;
  Found := False;

  for Pair in FAliases do
  begin
    if (Length(AModulePath) >= Length(Pair.Key)) and
       (Copy(AModulePath, 1, Length(Pair.Key)) = Pair.Key) then
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
    Replacement := BestValue + Copy(AModulePath, Length(BestKey) + 1, MaxInt);
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
  if FileExists(ABasePath) then
  begin
    AResolvedPath := ABasePath;
    Exit(True);
  end;

  for I := 0 to High(FExtensions) do
  begin
    if FileExists(ABasePath + FExtensions[I]) then
    begin
      AResolvedPath := ABasePath + FExtensions[I];
      Exit(True);
    end;
  end;

  for I := 0 to High(FExtensions) do
  begin
    if FileExists(ABasePath + PathDelim + 'index' + FExtensions[I]) then
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
  AliasApplied := ApplyAliases(AModulePath);

  if AliasApplied <> AModulePath then
  begin
    if TryResolveWithExtensions(ExpandFileName(AliasApplied), Result) then
      Exit;
    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s" (alias resolved to "%s")', [AModulePath, ExpandFileName(AliasApplied)]);
  end;

  if IsAbsolutePath(AModulePath) then
  begin
    if TryResolveWithExtensions(ExpandFileName(AModulePath), Result) then
      Exit;
    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s"', [AModulePath]);
  end;

  if (Copy(AModulePath, 1, 2) = './') or (Copy(AModulePath, 1, 3) = '../') then
  begin
    BaseDirectory := ExtractFilePath(AImportingFilePath);
    if BaseDirectory = '' then
      BaseDirectory := GetCurrentDir + PathDelim;

    if TryResolveWithExtensions(ExpandFileName(BaseDirectory + AModulePath), Result) then
      Exit;

    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s" (resolved to "%s")', [AModulePath, ExpandFileName(BaseDirectory + AModulePath)]);
  end;

  raise EModuleNotFound.CreateFmt(
    'Cannot resolve bare module specifier "%s". Imports must start with "./" or "../"', [AModulePath]);
end;

end.
