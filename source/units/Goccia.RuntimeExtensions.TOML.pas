unit Goccia.RuntimeExtensions.TOML;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.TOML,
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Values.Primitives;

type
  TGocciaTOMLRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinTOML: TGocciaTOMLBuiltin;
    FTOMLModule: TGocciaRuntimeNamespaceModuleRegistration;
    function MaterializeTOML: TGocciaValue;
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
  Goccia.Scope,
  Goccia.TOML,
  Goccia.Values.ObjectValue;

procedure TGocciaTOMLRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FTOMLModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    'goccia:toml',
    MaterializeTOML);
end;

function TGocciaTOMLRuntimeExtension.MaterializeTOML: TGocciaValue;
begin
  if not Assigned(FBuiltinTOML) then
    FBuiltinTOML := TGocciaTOMLBuiltin.Create('TOML',
      Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError, False);
  Result := FBuiltinTOML.BuiltinObject;
end;

procedure TGocciaTOMLRuntimeExtension.Detach;
begin
  FTOMLModule.Free;
  FTOMLModule := nil;
  FBuiltinTOML.Free;
  FBuiltinTOML := nil;
  inherited;
end;

procedure TGocciaTOMLRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_TOML);
end;

function TGocciaTOMLRuntimeExtension.TryLoadModule(
  const AResolvedPath: string; out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  Key: string;
  LoadSucceeded: Boolean;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
  TOMLParser: TGocciaTOMLParser;
begin
  AModule := nil;
  Result := IsTOMLExtension(ExtractFileExt(AResolvedPath));
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  ParsedValue := nil;
  try
    TOMLParser := TGocciaTOMLParser.Create;
    try
      try
        ParsedValue := TOMLParser.Parse(Content.Text);
      except
        on E: EGocciaTOMLParseError do
          raise TGocciaRuntimeError.Create(
            Format('Failed to parse TOML module "%s": %s',
              [AResolvedPath, E.Message]),
            0, 0, AResolvedPath, nil);
      end;
    finally
      TOMLParser.Free;
    end;

    AModule := TGocciaModule.Create(AResolvedPath);
    AModule.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      if ParsedValue is TGocciaObjectValue then
      begin
        Obj := TGocciaObjectValue(ParsedValue);
        for Key in Obj.GetOwnPropertyKeys do
          AModule.ExportsTable.AddOrSetValue(Key, Obj.GetProperty(Key));
      end
      else if Assigned(ParsedValue) then
        AModule.ExportsTable.AddOrSetValue(KEYWORD_DEFAULT, ParsedValue);
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    Content.Free;
  end;
end;

function TGocciaTOMLRuntimeExtension.TryInjectGlobals(
  const AFormat: string; const AContent: UTF8String): Boolean;
var
  ParsedValue: TGocciaValue;
  TOMLParser: TGocciaTOMLParser;
begin
  Result := SameText(AFormat, 'toml');
  if not Result then
    Exit;

  TOMLParser := TGocciaTOMLParser.Create;
  try
    ParsedValue := TOMLParser.Parse(AContent);
  finally
    TOMLParser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    Runtime.Engine.ThrowError('Globals TOML must be a top-level object.', 0, 0);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Runtime.RegisterGlobalsFromObject(TGocciaObjectValue(ParsedValue), 'TOML');
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

end.
