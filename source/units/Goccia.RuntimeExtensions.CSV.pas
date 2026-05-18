unit Goccia.RuntimeExtensions.CSV;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.CSV,
  Goccia.Modules,
  Goccia.Runtime;

type
  TGocciaCSVRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinCSV: TGocciaCSVBuiltin;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure AddModuleExtensions(const AExtensions: TStrings); override;
    function TryLoadModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean; override;
  end;

implementation

uses
  SysUtils,

  Goccia.CSV,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.Modules.ContentProvider,
  Goccia.Values.ArrayValue;

procedure TGocciaCSVRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinCSV := TGocciaCSVBuiltin.Create('CSV',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
end;

procedure TGocciaCSVRuntimeExtension.Detach;
begin
  FBuiltinCSV.Free;
  FBuiltinCSV := nil;
  inherited;
end;

procedure TGocciaCSVRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_CSV);
end;

function TGocciaCSVRuntimeExtension.TryLoadModule(const AResolvedPath: string;
  out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  CSVParser: TGocciaCSVParser;
  CSVRecords: TGocciaArrayValue;
  I: Integer;
  LoadSucceeded: Boolean;
begin
  AModule := nil;
  Result := IsCSVExtension(ExtractFileExt(AResolvedPath));
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  CSVRecords := nil;
  try
    CSVParser := TGocciaCSVParser.Create;
    try
      try
        CSVRecords := CSVParser.Parse(Content.Text);
      except
        on E: EGocciaCSVParseError do
          raise TGocciaRuntimeError.Create(
            Format('Failed to parse CSV module "%s": %s',
              [AResolvedPath, E.Message]),
            0, 0, AResolvedPath, nil);
      end;
    finally
      CSVParser.Free;
    end;

    AModule := TGocciaModule.Create(AResolvedPath);
    AModule.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      for I := 0 to CSVRecords.Elements.Count - 1 do
        AModule.ExportsTable.AddOrSetValue(IntToStr(I),
          CSVRecords.Elements[I]);
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    CSVRecords.Free;
    Content.Free;
  end;
end;

end.
