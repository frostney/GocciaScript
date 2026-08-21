unit Goccia.RuntimeExtensions.NamespaceModule;

{$I Goccia.inc}

interface

uses
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaRuntimeNamespaceFactory = function: TGocciaValue of object;

  TGocciaRuntimeNamespaceModuleRegistration = class
  private
    FModule: TGocciaModule;
    FModuleName: string;
    FFactory: TGocciaRuntimeNamespaceFactory;
    FNamespaceObject: TGocciaObjectValue;
    FRuntime: TGocciaRuntimeCore;
    FExportsDefault: Boolean;
  public
    { AExportsDefault opts a module into exporting the factory object's
      `default` key as its default export. Off by default: a `goccia:` runtime
      module is a namespace of named exports, and exporting one of them as
      `default` as well would be an accident rather than a decision. }
    constructor Create(const ARuntime: TGocciaRuntimeCore;
      const AModuleName: string; const AFactory: TGocciaRuntimeNamespaceFactory;
      const AExportsDefault: Boolean = False);
    destructor Destroy; override;
    function LoadModule: TGocciaModule;
  end;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.Keywords.Reserved;

constructor TGocciaRuntimeNamespaceModuleRegistration.Create(
  const ARuntime: TGocciaRuntimeCore; const AModuleName: string;
  const AFactory: TGocciaRuntimeNamespaceFactory;
  const AExportsDefault: Boolean);
begin
  inherited Create;
  FExportsDefault := AExportsDefault;
  if not Assigned(ARuntime) then
    raise Exception.Create('Runtime namespace module registration needs a runtime.');
  if not Assigned(AFactory) then
    raise Exception.Create('Runtime namespace module registration needs a factory.');
  FRuntime := ARuntime;
  FModuleName := AModuleName;
  FFactory := AFactory;
  FRuntime.Engine.RegisterHostModuleProvider(FModuleName, LoadModule);
end;

destructor TGocciaRuntimeNamespaceModuleRegistration.Destroy;
begin
  if Assigned(FRuntime) and Assigned(FRuntime.Engine) then
    FRuntime.Engine.UnregisterHostModuleProvider(FModuleName);
  if Assigned(FNamespaceObject) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.RemoveRootObject(FNamespaceObject);
  FModule.Free;
  inherited;
end;

function TGocciaRuntimeNamespaceModuleRegistration.LoadModule: TGocciaModule;
var
  ExportName: string;
  Module: TGocciaModule;
  NamespaceValue: TGocciaValue;
begin
  if Assigned(FModule) then
    Exit(FModule);

  NamespaceValue := FFactory();
  if not (NamespaceValue is TGocciaObjectValue) then
    raise Exception.CreateFmt(
      'Runtime module "%s" namespace factory returned a non-object value.',
      [FModuleName]);

  FNamespaceObject := TGocciaObjectValue(NamespaceValue);
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddRootObject(FNamespaceObject);

  Module := TGocciaModule.Create(FModuleName);
  try
    for ExportName in FNamespaceObject.GetOwnPropertyKeys do
      if (ExportName <> KEYWORD_DEFAULT) or FExportsDefault then
        Module.AddExportValue(ExportName,
          FNamespaceObject.GetProperty(ExportName));
    FModule := Module;
    Module := nil;
  finally
    if Assigned(Module) then
    begin
      Module.Free;
      if (TGarbageCollector.Instance <> nil) then
        TGarbageCollector.Instance.RemoveRootObject(FNamespaceObject);
      FNamespaceObject := nil;
    end;
  end;
  Result := FModule;
end;

end.
