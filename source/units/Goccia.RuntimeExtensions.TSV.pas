unit Goccia.RuntimeExtensions.TSV;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.TSV,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.IndexedDataModule,
  Goccia.Values.ArrayValue;

type
  TGocciaTSVRuntimeExtension = class(TGocciaIndexedDataModuleRuntimeExtension)
  private
    FBuiltinTSV: TGocciaTSVBuiltin;
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

  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.TSV;

procedure TGocciaTSVRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinTSV := TGocciaTSVBuiltin.Create('TSV',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
end;

procedure TGocciaTSVRuntimeExtension.Detach;
begin
  FBuiltinTSV.Free;
  FBuiltinTSV := nil;
  inherited;
end;

procedure TGocciaTSVRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_TSV);
end;

function TGocciaTSVRuntimeExtension.MatchesModulePath(
  const AResolvedPath: string): Boolean;
begin
  Result := IsTSVExtension(ExtractFileExt(AResolvedPath));
end;

function TGocciaTSVRuntimeExtension.ParseModuleRecords(
  const AContent: UTF8String; const AResolvedPath: string): TGocciaArrayValue;
var
  TSVParser: TGocciaTSVParser;
begin
  TSVParser := TGocciaTSVParser.Create;
  try
    try
      Result := TSVParser.Parse(AContent);
    except
      on E: EGocciaTSVParseError do
        raise TGocciaRuntimeError.Create(
          Format('Failed to parse TSV module "%s": %s',
            [AResolvedPath, E.Message]),
          0, 0, AResolvedPath, nil);
    end;
  finally
    TSVParser.Free;
  end;
end;

end.
