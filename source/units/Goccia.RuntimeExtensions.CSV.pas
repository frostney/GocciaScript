unit Goccia.RuntimeExtensions.CSV;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.CSV,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.IndexedDataModule,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.Primitives;

type
  TGocciaCSVRuntimeExtension = class(TGocciaIndexedDataModuleRuntimeExtension)
  private
    FBuiltinCSV: TGocciaCSVBuiltin;
    function MaterializeCSV: TGocciaValue;
  protected
    function MatchesModulePath(const AResolvedPath: string): Boolean; override;
    function ParseModuleRecords(const AContent: UTF8String;
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
  // The CSV global is a heavyweight, rarely-touched data-format namespace, so
  // defer building it until first reflective access instead of constructing it
  // eagerly at profile-install time.
  Runtime.Engine.RegisterLazyGlobal('CSV', MaterializeCSV, dtLet);
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
  const AContent: UTF8String; const AResolvedPath: string): TGocciaArrayValue;
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
