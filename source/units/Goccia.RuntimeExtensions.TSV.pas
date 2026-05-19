unit Goccia.RuntimeExtensions.TSV;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.TSV,
  Goccia.Modules,
  Goccia.Runtime;

type
  TGocciaTSVRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinTSV: TGocciaTSVBuiltin;
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
  Goccia.Modules.ContentProvider,
  Goccia.TSV,
  Goccia.Values.ArrayValue;

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

function TGocciaTSVRuntimeExtension.TryLoadModule(const AResolvedPath: string;
  out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  I: Integer;
  LoadSucceeded: Boolean;
  TSVParser: TGocciaTSVParser;
  TSVRecords: TGocciaArrayValue;
begin
  AModule := nil;
  Result := IsTSVExtension(ExtractFileExt(AResolvedPath));
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  TSVRecords := nil;
  try
    TSVParser := TGocciaTSVParser.Create;
    try
      try
        TSVRecords := TSVParser.Parse(Content.Text);
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

    AModule := TGocciaModule.Create(AResolvedPath);
    AModule.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      for I := 0 to TSVRecords.Elements.Count - 1 do
        AModule.ExportsTable.AddOrSetValue(IntToStr(I),
          TSVRecords.Elements[I]);
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    TSVRecords.Free;
    Content.Free;
  end;
end;

end.
