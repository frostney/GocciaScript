unit Goccia.RuntimeExtensions.JSONL;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.JSONL,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.IndexedDataModule,
  Goccia.Values.ArrayValue;

type
  TGocciaJSONLRuntimeExtension = class(TGocciaIndexedDataModuleRuntimeExtension)
  private
    FBuiltinJSONL: TGocciaJSONLBuiltin;
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
  Goccia.JSONL;

procedure TGocciaJSONLRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinJSONL := TGocciaJSONLBuiltin.Create('JSONL',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
end;

procedure TGocciaJSONLRuntimeExtension.Detach;
begin
  FBuiltinJSONL.Free;
  FBuiltinJSONL := nil;
  inherited;
end;

procedure TGocciaJSONLRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_JSONL);
end;

function TGocciaJSONLRuntimeExtension.MatchesModulePath(
  const AResolvedPath: string): Boolean;
begin
  Result := IsJSONLExtension(ExtractFileExt(AResolvedPath));
end;

function TGocciaJSONLRuntimeExtension.ParseModuleRecords(
  const AContent: UTF8String; const AResolvedPath: string): TGocciaArrayValue;
var
  JSONLParser: TGocciaJSONLParser;
begin
  JSONLParser := TGocciaJSONLParser.Create;
  try
    try
      Result := JSONLParser.Parse(AContent);
    except
      on E: EGocciaJSONLParseError do
        raise TGocciaRuntimeError.Create(
          Format('Failed to parse JSONL module "%s": %s',
            [AResolvedPath, E.Message]),
          0, 0, AResolvedPath, nil);
    end;
  finally
    JSONLParser.Free;
  end;
end;

end.
