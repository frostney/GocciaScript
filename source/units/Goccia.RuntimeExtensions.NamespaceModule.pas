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
  public
    constructor Create(const ARuntime: TGocciaRuntimeCore;
      const AModuleName: string; const AFactory: TGocciaRuntimeNamespaceFactory);
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
  const AFactory: TGocciaRuntimeNamespaceFactory);
begin
  inherited Create;
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
      if ExportName <> KEYWORD_DEFAULT then
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
