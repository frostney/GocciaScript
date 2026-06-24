unit Goccia.RuntimeExtensions.IndexedDataModule;

{$I Goccia.inc}

interface

uses
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.Values.ArrayValue;

type
  TGocciaIndexedDataModuleRuntimeExtension = class(TGocciaRuntimeExtension)
  protected
    function MatchesModulePath(const AResolvedPath: string): Boolean; virtual; abstract;
    function ParseModuleRecords(const AContent: UTF8String;
      const AResolvedPath: string): TGocciaArrayValue; virtual; abstract;
  public
    function TryLoadModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Modules.ContentProvider;

function TGocciaIndexedDataModuleRuntimeExtension.TryLoadModule(
  const AResolvedPath: string; out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  I: Integer;
  LoadSucceeded: Boolean;
  Records: TGocciaArrayValue;
begin
  AModule := nil;
  Result := MatchesModulePath(AResolvedPath);
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  Records := nil;
  try
    Records := ParseModuleRecords(Content.Text, AResolvedPath);

    LoadSucceeded := False;
    AModule := TGocciaModule.Create(AResolvedPath);
    try
      AModule.LastModified := Content.LastModified;
      for I := 0 to Records.Elements.Count - 1 do
        AModule.ExportsTable.AddOrSetValue(IntToStr(I), Records.Elements[I]);
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    Records.Free;
    Content.Free;
  end;
end;

end.
