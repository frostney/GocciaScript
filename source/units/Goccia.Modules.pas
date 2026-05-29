unit Goccia.Modules;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  OrderedStringMap,

  Goccia.GarbageCollector,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaImportCallPhase = (icpEvaluation, icpSource, icpDefer);
  TGocciaModule = class;
  TLoadModuleCallback = function(const AModulePath, AImportingFilePath: string): TGocciaModule of object;
  TLoadModuleSourceCallback = function(const AModulePath,
    AImportingFilePath: string): TGocciaValue of object;
  TLoadDeferredModuleCallback = function(const AModulePath,
    AImportingFilePath: string): TGocciaValue of object;

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

  TGocciaModuleSourceValue = class(TGocciaObjectValue)
  private
    FPath: string;
    FSourceText: UTF8String;
  public
    constructor Create(const APath: string; const ASourceText: UTF8String);
    function ToStringTag: string; override;
    property Path: string read FPath;
    property SourceText: UTF8String read FSourceText;
  end;

  TGocciaDeferredModuleNamespaceObject = class(TGocciaObjectValue)
  private
    FModulePath: string;
    FImportingFilePath: string;
    FLoadModule: TLoadModuleCallback;
    FNamespaceObject: TGocciaObjectValue;
    function EnsureNamespaceObject: TGocciaObjectValue;
  public
    constructor Create(const AModulePath, AImportingFilePath: string;
      const ALoadModule: TLoadModuleCallback);
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    function GetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor; override;
    function HasProperty(const AName: string): Boolean; override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function GetEnumerablePropertyNames: TArray<string>; override;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>; override;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetOwnPropertyNames: TArray<string>; override;
    function GetOwnPropertyKeys: TArray<string>; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  Goccia.Constants.PropertyNames;

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

{ TGocciaModuleSourceValue }

constructor TGocciaModuleSourceValue.Create(const APath: string;
  const ASourceText: UTF8String);
begin
  inherited Create(nil);
  FPath := APath;
  FSourceText := ASourceText;
  Freeze;
end;

function TGocciaModuleSourceValue.ToStringTag: string;
begin
  Result := 'ModuleSource';
end;

{ TGocciaDeferredModuleNamespaceObject }

constructor TGocciaDeferredModuleNamespaceObject.Create(
  const AModulePath, AImportingFilePath: string;
  const ALoadModule: TLoadModuleCallback);
begin
  inherited Create(nil);
  FModulePath := AModulePath;
  FImportingFilePath := AImportingFilePath;
  FLoadModule := ALoadModule;
  Freeze;
end;

function TGocciaDeferredModuleNamespaceObject.EnsureNamespaceObject: TGocciaObjectValue;
var
  Module: TGocciaModule;
begin
  if not Assigned(FNamespaceObject) then
  begin
    if not Assigned(FLoadModule) then
      raise Exception.Create('Module loader is not available.');
    Module := FLoadModule(FModulePath, FImportingFilePath);
    FNamespaceObject := Module.GetNamespaceObject;
  end;
  Result := FNamespaceObject;
end;

function TGocciaDeferredModuleNamespaceObject.GetProperty(
  const AName: string): TGocciaValue;
begin
  if (AName = PROP_THEN) and not Assigned(FNamespaceObject) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := EnsureNamespaceObject.GetProperty(AName);
end;

function TGocciaDeferredModuleNamespaceObject.GetPropertyWithContext(
  const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  Result := EnsureNamespaceObject.GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaDeferredModuleNamespaceObject.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
begin
  Result := EnsureNamespaceObject.GetOwnPropertyDescriptor(AName);
end;

function TGocciaDeferredModuleNamespaceObject.HasProperty(
  const AName: string): Boolean;
begin
  Result := EnsureNamespaceObject.HasProperty(AName);
end;

function TGocciaDeferredModuleNamespaceObject.HasOwnProperty(
  const AName: string): Boolean;
begin
  Result := EnsureNamespaceObject.HasOwnProperty(AName);
end;

function TGocciaDeferredModuleNamespaceObject.GetEnumerablePropertyNames:
  TArray<string>;
begin
  Result := EnsureNamespaceObject.GetEnumerablePropertyNames;
end;

function TGocciaDeferredModuleNamespaceObject.GetEnumerablePropertyValues:
  TArray<TGocciaValue>;
begin
  Result := EnsureNamespaceObject.GetEnumerablePropertyValues;
end;

function TGocciaDeferredModuleNamespaceObject.GetEnumerablePropertyEntries:
  TArray<TPair<string, TGocciaValue>>;
begin
  Result := EnsureNamespaceObject.GetEnumerablePropertyEntries;
end;

function TGocciaDeferredModuleNamespaceObject.GetAllPropertyNames:
  TArray<string>;
begin
  Result := EnsureNamespaceObject.GetAllPropertyNames;
end;

function TGocciaDeferredModuleNamespaceObject.GetOwnPropertyNames:
  TArray<string>;
begin
  Result := EnsureNamespaceObject.GetOwnPropertyNames;
end;

function TGocciaDeferredModuleNamespaceObject.GetOwnPropertyKeys:
  TArray<string>;
begin
  Result := EnsureNamespaceObject.GetOwnPropertyKeys;
end;

procedure TGocciaDeferredModuleNamespaceObject.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FNamespaceObject) then
    FNamespaceObject.MarkReferences;
end;

end.
