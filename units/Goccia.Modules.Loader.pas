unit Goccia.Modules.Loader;

{$I Goccia.inc}

interface

uses
  Classes,

  ModuleResolver;

type
  TGocciaModuleSourceKind = (mskScript, mskJson);

  TGocciaLoadedModuleSource = record
    ResolvedPath: string;
    SourceText: string;
    Kind: TGocciaModuleSourceKind;
  end;

function LoadModuleSource(const AResolver: TModuleResolver;
  const AModulePath, AImportingFilePath: string): TGocciaLoadedModuleSource;

implementation

uses
  SysUtils,

  Goccia.FileExtensions;

function LoadModuleSource(const AResolver: TModuleResolver;
  const AModulePath, AImportingFilePath: string): TGocciaLoadedModuleSource;
var
  Source: TStringList;
begin
  if not Assigned(AResolver) then
    raise EModuleNotFound.CreateFmt(
      'No module resolver configured and cannot resolve "%s"', [AModulePath]);

  Result.ResolvedPath := AResolver.Resolve(AModulePath, AImportingFilePath);
  if LowerCase(ExtractFileExt(Result.ResolvedPath)) = EXT_JSON then
    Result.Kind := mskJson
  else
    Result.Kind := mskScript;

  Source := TStringList.Create;
  try
    Source.LoadFromFile(Result.ResolvedPath);
    Result.SourceText := Source.Text;
  finally
    Source.Free;
  end;
end;

end.
