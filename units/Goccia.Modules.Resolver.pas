unit Goccia.Modules.Resolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap;

type
  TGocciaModuleResolver = class
  private
    FAliases: TOrderedStringMap<string>;
    FBaseDirectory: string;
  protected
    function ApplyAliases(const AModulePath: string): string;
    function TryResolveWithExtensions(const ABasePath: string; out AResolvedPath: string): Boolean;
  public
    constructor Create(const ABaseDirectory: string = '');
    destructor Destroy; override;

    procedure AddAlias(const APattern, AReplacement: string);
    function HasAlias(const AModulePath: string): Boolean;
    function Resolve(const AModulePath, AImportingFilePath: string): string; virtual;

    property Aliases: TOrderedStringMap<string> read FAliases;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
  end;

  EGocciaModuleNotFound = class(Exception);

implementation

uses
  Goccia.FileExtensions;

constructor TGocciaModuleResolver.Create(const ABaseDirectory: string);
begin
  FAliases := TOrderedStringMap<string>.Create;
  if ABaseDirectory <> '' then
    FBaseDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ABaseDirectory))
  else
    FBaseDirectory := IncludeTrailingPathDelimiter(GetCurrentDir);
end;

destructor TGocciaModuleResolver.Destroy;
begin
  FAliases.Free;
  inherited;
end;

procedure TGocciaModuleResolver.AddAlias(const APattern, AReplacement: string);
begin
  FAliases.AddOrSetValue(APattern, AReplacement);
end;

function TGocciaModuleResolver.HasAlias(const AModulePath: string): Boolean;
var
  Pairs: TOrderedStringMap<string>.TKeyValueArray;
  I: Integer;
begin
  Pairs := FAliases.ToArray;
  for I := 0 to High(Pairs) do
    if (Length(AModulePath) >= Length(Pairs[I].Key)) and
       (Copy(AModulePath, 1, Length(Pairs[I].Key)) = Pairs[I].Key) then
      Exit(True);
  Result := False;
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

function TGocciaModuleResolver.ApplyAliases(const AModulePath: string): string;
var
  Pairs: TOrderedStringMap<string>.TKeyValueArray;
  BestKey, BestValue, Replacement: string;
  I: Integer;
  Found: Boolean;
begin
  Result := AModulePath;
  Found := False;

  Pairs := FAliases.ToArray;
  for I := 0 to High(Pairs) do
  begin
    if (Length(AModulePath) >= Length(Pairs[I].Key)) and
       (Copy(AModulePath, 1, Length(Pairs[I].Key)) = Pairs[I].Key) then
    begin
      if (not Found) or (Length(Pairs[I].Key) > Length(BestKey)) then
      begin
        BestKey := Pairs[I].Key;
        BestValue := Pairs[I].Value;
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

function TGocciaModuleResolver.TryResolveWithExtensions(const ABasePath: string; out AResolvedPath: string): Boolean;
var
  I: Integer;
begin
  if FileExists(ABasePath) then
  begin
    AResolvedPath := ABasePath;
    Exit(True);
  end;

  for I := Low(ScriptExtensions) to High(ScriptExtensions) do
  begin
    if FileExists(ABasePath + ScriptExtensions[I]) then
    begin
      AResolvedPath := ABasePath + ScriptExtensions[I];
      Exit(True);
    end;
  end;

  for I := Low(ScriptExtensions) to High(ScriptExtensions) do
  begin
    if FileExists(ABasePath + PathDelim + 'index' + ScriptExtensions[I]) then
    begin
      AResolvedPath := ABasePath + PathDelim + 'index' + ScriptExtensions[I];
      Exit(True);
    end;
  end;

  Result := False;
end;

function TGocciaModuleResolver.Resolve(const AModulePath, AImportingFilePath: string): string;
var
  AliasApplied, BaseDirectory: string;
begin
  AliasApplied := ApplyAliases(AModulePath);

  if AliasApplied <> AModulePath then
  begin
    if TryResolveWithExtensions(ExpandFileName(AliasApplied), Result) then
      Exit;
    raise EGocciaModuleNotFound.CreateFmt(
      'Module not found: "%s" (alias resolved to "%s")', [AModulePath, ExpandFileName(AliasApplied)]);
  end;

  if IsAbsolutePath(AModulePath) then
  begin
    if TryResolveWithExtensions(ExpandFileName(AModulePath), Result) then
      Exit;
    raise EGocciaModuleNotFound.CreateFmt(
      'Module not found: "%s"', [AModulePath]);
  end;

  if (Copy(AModulePath, 1, 2) = './') or (Copy(AModulePath, 1, 3) = '../') then
  begin
    BaseDirectory := ExtractFilePath(AImportingFilePath);
    if BaseDirectory = '' then
      BaseDirectory := GetCurrentDir + PathDelim;

    if TryResolveWithExtensions(ExpandFileName(BaseDirectory + AModulePath), Result) then
      Exit;

    raise EGocciaModuleNotFound.CreateFmt(
      'Module not found: "%s" (resolved to "%s")', [AModulePath, ExpandFileName(BaseDirectory + AModulePath)]);
  end;

  raise EGocciaModuleNotFound.CreateFmt(
    'Cannot resolve bare module specifier "%s". Imports must start with "./" or "../"', [AModulePath]);
end;

end.
