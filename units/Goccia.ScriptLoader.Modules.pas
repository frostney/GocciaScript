unit Goccia.ScriptLoader.Modules;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Modules.Resolver;

type
  TScriptLoaderAliasPair = record
    Key: string;
    ValueText: string;
  end;

function ParseAliasPair(const AArg: string): TScriptLoaderAliasPair;
function ResolveEntryBaseDirectory(const AFileName: string): string;
procedure ConfigureModuleResolver(const AResolver: TGocciaModuleResolver;
  const AEntryFileName, AExplicitImportMapPath: string;
  const AInlineAliases: TStrings);

implementation

uses
  SysUtils,

  Goccia.ScriptLoader.Input;

function ParseAliasPair(const AArg: string): TScriptLoaderAliasPair;
var
  SeparatorIndex: Integer;
begin
  SeparatorIndex := Pos('=', AArg);
  if SeparatorIndex <= 1 then
    raise Exception.Create('Invalid --alias argument. Use --alias key=value.');

  Result.Key := Copy(AArg, 1, SeparatorIndex - 1);
  Result.ValueText := Copy(AArg, SeparatorIndex + 1, MaxInt);
end;

function ResolveEntryBaseDirectory(const AFileName: string): string;
begin
  if AFileName = STDIN_FILE_NAME then
    Result := GetCurrentDir
  else
    Result := ExtractFilePath(ExpandFileName(AFileName));
end;

procedure ConfigureModuleResolver(const AResolver: TGocciaModuleResolver;
  const AEntryFileName, AExplicitImportMapPath: string;
  const AInlineAliases: TStrings);
var
  AliasPair: TScriptLoaderAliasPair;
  I: Integer;
  ImportMapPath: string;
begin
  if not Assigned(AResolver) then
    Exit;

  if AExplicitImportMapPath <> '' then
    ImportMapPath := ExpandFileName(AExplicitImportMapPath)
  else
    ImportMapPath := TGocciaModuleResolver.DiscoverProjectConfig(
      ResolveEntryBaseDirectory(AEntryFileName));

  if ImportMapPath <> '' then
    AResolver.LoadImportMap(ImportMapPath);

  if not Assigned(AInlineAliases) then
    Exit;

  for I := 0 to AInlineAliases.Count - 1 do
  begin
    AliasPair := ParseAliasPair(AInlineAliases[I]);
    AResolver.AddAlias(AliasPair.Key, AliasPair.ValueText);
  end;
end;

end.
