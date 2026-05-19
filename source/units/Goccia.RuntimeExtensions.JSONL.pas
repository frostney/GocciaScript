unit Goccia.RuntimeExtensions.JSONL;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.JSONL,
  Goccia.Modules,
  Goccia.Runtime;

type
  TGocciaJSONLRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinJSONL: TGocciaJSONLBuiltin;
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

  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.JSONL,
  Goccia.Modules.ContentProvider,
  Goccia.Values.ArrayValue;

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

function TGocciaJSONLRuntimeExtension.TryLoadModule(
  const AResolvedPath: string; out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  I: Integer;
  JSONLParser: TGocciaJSONLParser;
  JSONLRecords: TGocciaArrayValue;
  LoadSucceeded: Boolean;
begin
  AModule := nil;
  Result := IsJSONLExtension(ExtractFileExt(AResolvedPath));
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  JSONLRecords := nil;
  try
    JSONLParser := TGocciaJSONLParser.Create;
    try
      try
        JSONLRecords := JSONLParser.Parse(Content.Text);
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

    AModule := TGocciaModule.Create(AResolvedPath);
    AModule.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      for I := 0 to JSONLRecords.Elements.Count - 1 do
        AModule.ExportsTable.AddOrSetValue(IntToStr(I),
          JSONLRecords.Elements[I]);
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    JSONLRecords.Free;
    Content.Free;
  end;
end;

end.
