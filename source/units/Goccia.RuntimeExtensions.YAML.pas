unit Goccia.RuntimeExtensions.YAML;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.YAML,
  Goccia.Modules,
  Goccia.Runtime;

type
  TGocciaYAMLRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinYAML: TGocciaYAMLBuiltin;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure AddModuleExtensions(const AExtensions: TStrings); override;
    function TryLoadModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean; override;
    function TryInjectGlobals(const AFormat: string;
      const AContent: UTF8String): Boolean; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.Keywords.Reserved,
  Goccia.Modules.ContentProvider,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.YAML;

procedure TGocciaYAMLRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinYAML := TGocciaYAMLBuiltin.Create('YAML',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
end;

procedure TGocciaYAMLRuntimeExtension.Detach;
begin
  FBuiltinYAML.Free;
  FBuiltinYAML := nil;
  inherited;
end;

procedure TGocciaYAMLRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_YAML);
  AExtensions.Add(EXT_YML);
end;

function TGocciaYAMLRuntimeExtension.TryLoadModule(
  const AResolvedPath: string; out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  DocumentIndex: Integer;
  Documents: TGocciaArrayValue;
  Key: string;
  LoadSucceeded: Boolean;
  Obj: TGocciaObjectValue;
  ParsedDocument: TGocciaValue;
  YAMLParser: TGocciaYAMLParser;
begin
  AModule := nil;
  Result := IsYAMLExtension(ExtractFileExt(AResolvedPath));
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  Documents := nil;
  ParsedDocument := nil;
  try
    YAMLParser := TGocciaYAMLParser.Create;
    try
      try
        Documents := YAMLParser.ParseDocuments(Content.Text);
      except
        on E: EGocciaYAMLParseError do
          raise TGocciaRuntimeError.Create(
            Format('Failed to parse YAML module "%s": %s',
              [AResolvedPath, E.Message]),
            0, 0, AResolvedPath, nil);
      end;
    finally
      YAMLParser.Free;
    end;

    if Documents.Elements.Count = 0 then
      raise TGocciaRuntimeError.Create(
        Format('YAML module "%s" must contain at least one top-level document.',
          [AResolvedPath]),
        0, 0, AResolvedPath, nil);
    if Documents.Elements.Count = 1 then
      ParsedDocument := Documents.Elements[0];

    AModule := TGocciaModule.Create(AResolvedPath);
    AModule.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      if Documents.Elements.Count > 1 then
      begin
        for DocumentIndex := 0 to Documents.Elements.Count - 1 do
          AModule.ExportsTable.AddOrSetValue(IntToStr(DocumentIndex),
            Documents.Elements[DocumentIndex]);
      end
      else if ParsedDocument is TGocciaObjectValue then
      begin
        Obj := TGocciaObjectValue(ParsedDocument);
        for Key in Obj.GetOwnPropertyKeys do
          AModule.ExportsTable.AddOrSetValue(Key, Obj.GetProperty(Key));
      end
      else if Assigned(ParsedDocument) then
        AModule.ExportsTable.AddOrSetValue(KEYWORD_DEFAULT, ParsedDocument);

      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    Documents.Free;
    Content.Free;
  end;
end;

function TGocciaYAMLRuntimeExtension.TryInjectGlobals(
  const AFormat: string; const AContent: UTF8String): Boolean;
var
  Documents: TGocciaArrayValue;
  ParsedDocument: TGocciaValue;
  YAMLParser: TGocciaYAMLParser;
begin
  Result := SameText(AFormat, 'yaml');
  if not Result then
    Exit;

  YAMLParser := TGocciaYAMLParser.Create;
  try
    Documents := YAMLParser.ParseDocuments(AContent);
  finally
    YAMLParser.Free;
  end;

  try
    if Documents.Elements.Count <> 1 then
      Runtime.Engine.ThrowError(
        'Globals YAML must contain exactly one top-level document.', 0, 0);

    ParsedDocument := Documents.Elements[0];
    if not (ParsedDocument is TGocciaObjectValue) then
      Runtime.Engine.ThrowError('Globals YAML must be a top-level object.', 0, 0);

    TGarbageCollector.Instance.AddTempRoot(ParsedDocument);
    try
      Runtime.RegisterGlobalsFromObject(TGocciaObjectValue(ParsedDocument),
        'YAML');
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ParsedDocument);
    end;
  finally
    Documents.Free;
  end;
end;

end.
