unit Goccia.RuntimeExtensions.JSON5;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Builtins.JSON5,
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Values.Primitives;

type
  TGocciaJSON5RuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinJSON5: TGocciaJSON5Builtin;
    FJSON5Module: TGocciaRuntimeNamespaceModuleRegistration;
    function MaterializeJSON5: TGocciaValue;
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
  Goccia.JSON5,
  Goccia.Keywords.Reserved,
  Goccia.Modules.ContentProvider,
  Goccia.Scope,
  Goccia.Values.ObjectValue;

procedure TGocciaJSON5RuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FJSON5Module := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    'goccia:json5',
    MaterializeJSON5);
end;

function TGocciaJSON5RuntimeExtension.MaterializeJSON5: TGocciaValue;
begin
  if not Assigned(FBuiltinJSON5) then
    FBuiltinJSON5 := TGocciaJSON5Builtin.Create('JSON5',
      Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError, False);
  Result := FBuiltinJSON5.BuiltinObject;
end;

procedure TGocciaJSON5RuntimeExtension.Detach;
begin
  FJSON5Module.Free;
  FJSON5Module := nil;
  FBuiltinJSON5.Free;
  FBuiltinJSON5 := nil;
  inherited;
end;

procedure TGocciaJSON5RuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_JSON5);
  AExtensions.Add(EXT_JSONC);
end;

function TGocciaJSON5RuntimeExtension.TryLoadModule(
  const AResolvedPath: string; out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  JSON5Parser: TGocciaJSON5Parser;
  Key: string;
  LoadSucceeded: Boolean;
  Obj: TGocciaObjectValue;
  ParsedValue: TGocciaValue;
begin
  AModule := nil;
  Result := IsJSON5Extension(ExtractFileExt(AResolvedPath));
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  ParsedValue := nil;
  try
    JSON5Parser := TGocciaJSON5Parser.Create;
    try
      try
        ParsedValue := JSON5Parser.Parse(Content.Text);
      except
        on E: EGocciaJSON5ParseError do
          raise TGocciaRuntimeError.Create(
            Format('Failed to parse JSON5 module "%s": %s',
              [AResolvedPath, E.Message]),
            0, 0, AResolvedPath, nil);
      end;
    finally
      JSON5Parser.Free;
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

function TGocciaJSON5RuntimeExtension.TryInjectGlobals(
  const AFormat: string; const AContent: UTF8String): Boolean;
var
  JSON5Parser: TGocciaJSON5Parser;
  ParsedValue: TGocciaValue;
begin
  Result := SameText(AFormat, 'json5');
  if not Result then
    Exit;

  JSON5Parser := TGocciaJSON5Parser.Create;
  try
    ParsedValue := JSON5Parser.Parse(AContent);
  finally
    JSON5Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    Runtime.Engine.ThrowError('Globals JSON5 must be a top-level object.', 0, 0);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    Runtime.RegisterGlobalsFromObject(TGocciaObjectValue(ParsedValue), 'JSON5');
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

end.
