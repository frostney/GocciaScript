unit Goccia.RuntimeExtensions.CSV;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.CSV,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.IndexedDataModule,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.Primitives;

type
  TGocciaCSVRuntimeExtension = class(TGocciaIndexedDataModuleRuntimeExtension)
  private
    FBuiltinCSV: TGocciaCSVBuiltin;
    FCSVModule: TGocciaRuntimeNamespaceModuleRegistration;
    function MaterializeCSV: TGocciaValue;
  protected
    function MatchesModulePath(const AResolvedPath: string): Boolean; override;
    function ParseModuleRecords(const AContent: string;
      const AResolvedPath: string): TGocciaArrayValue; override;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure AddModuleExtensions(const AExtensions: TStrings); override;
  end;

implementation

uses
  SysUtils,

  Goccia.CSV,
  Goccia.Error,
  Goccia.FileExtensions;

procedure TGocciaCSVRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FCSVModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    'goccia:csv',
    MaterializeCSV);
end;

function TGocciaCSVRuntimeExtension.MaterializeCSV: TGocciaValue;
begin
  if not Assigned(FBuiltinCSV) then
    FBuiltinCSV := TGocciaCSVBuiltin.Create('CSV',
      Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError, False);
  Result := FBuiltinCSV.BuiltinObject;
end;

procedure TGocciaCSVRuntimeExtension.Detach;
begin
  FCSVModule.Free;
  FCSVModule := nil;
  FBuiltinCSV.Free;
  FBuiltinCSV := nil;
  inherited;
end;

procedure TGocciaCSVRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_CSV);
end;

function TGocciaCSVRuntimeExtension.MatchesModulePath(
  const AResolvedPath: string): Boolean;
begin
  Result := IsCSVExtension(ExtractFileExt(AResolvedPath));
end;

function TGocciaCSVRuntimeExtension.ParseModuleRecords(
  const AContent: string; const AResolvedPath: string): TGocciaArrayValue;
var
  CSVParser: TGocciaCSVParser;
begin
  CSVParser := TGocciaCSVParser.Create;
  try
    try
      Result := CSVParser.Parse(AContent);
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
end;

end.
