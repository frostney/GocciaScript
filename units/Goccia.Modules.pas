unit Goccia.Modules;

{$I Goccia.inc}

interface

uses
  SysUtils,

  GarbageCollector.Generic,
  OrderedStringMap,

  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaModule = class
  private
    FPath: string;
    FExportsTable: TGocciaValueMap;
    FLastModified: TDateTime;
    FNamespaceObject: TGocciaObjectValue;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;
    function GetNamespaceObject: TGocciaObjectValue;
    procedure InvalidateNamespaceObject;
    property Path: string read FPath;
    property ExportsTable: TGocciaValueMap read FExportsTable;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

  TLoadModuleCallback = function(const AModulePath, AImportingFilePath: string): TGocciaModule of object;

implementation

constructor TGocciaModule.Create(const APath: string);
begin
  FPath := APath;
  FExportsTable := TGocciaValueMap.Create;
end;

// ES2026 §16.2.1.13 GetModuleNamespace(module)
function TGocciaModule.GetNamespaceObject: TGocciaObjectValue;
var
  ExportPair: TGocciaValueMap.TKeyValuePair;
begin
  if not Assigned(FNamespaceObject) then
  begin
    FNamespaceObject := TGocciaObjectValue.Create(nil, FExportsTable.Count);
    for ExportPair in FExportsTable do
      FNamespaceObject.DefineProperty(ExportPair.Key,
        TGocciaPropertyDescriptorData.Create(ExportPair.Value, [pfEnumerable]));
    FNamespaceObject.Freeze;
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddRootObject(FNamespaceObject);
  end;
  Result := FNamespaceObject;
end;

procedure TGocciaModule.InvalidateNamespaceObject;
begin
  if Assigned(FNamespaceObject) then
  begin
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveRootObject(FNamespaceObject);
    FNamespaceObject := nil;
  end;
end;

destructor TGocciaModule.Destroy;
begin
  InvalidateNamespaceObject;
  FExportsTable.Free;
  inherited;
end;


end.
