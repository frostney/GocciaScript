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
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TGocciaImportCallPhase = (icpEvaluation, icpSource, icpDefer);
  EGocciaDeferredModuleNotReady = class(Exception);
  TGocciaModule = class;
  TGocciaModuleList = TList<TGocciaModule>;
  TGocciaModuleExportBinding = class;
  TGocciaModuleExportBindingMap = TOrderedStringMap<TGocciaModuleExportBinding>;
  TLoadModuleCallback = function(const AModulePath, AImportingFilePath: string): TGocciaModule of object;
  TLoadModuleSourceCallback = function(const AModulePath,
    AImportingFilePath: string): TGocciaValue of object;
  TLoadDeferredModuleCallback = function(const AModulePath,
    AImportingFilePath: string): TGocciaValue of object;
  TResolveModuleURLCallback = function(const AModulePath,
    AImportingFilePath: string): string of object;

  TGocciaModuleExportBinding = class
  private
    FEnvironment: TGCManagedObject;
    FLocalName: string;
    FSourceExportName: string;
    FSourceModule: TGocciaModule;
    FValue: TGocciaValue;
  public
    constructor CreateValue(const AValue: TGocciaValue);
    constructor CreateBinding(const AEnvironment: TGCManagedObject;
      const ALocalName: string);
    constructor CreateForwarding(const ASourceModule: TGocciaModule;
      const ASourceExportName: string);
    function GetValue: TGocciaValue;
    procedure UpdateValue(const AValue: TGocciaValue);
    procedure MarkReferences;
  end;

  TGocciaModule = class
  private
    FAmbiguousExports: TOrderedStringMap<Boolean>;
    FPath: string;
    FEnvironment: TGCManagedObject;
    FExportBindings: TGocciaModuleExportBindingMap;
    FExportsTable: TGocciaValueMap;
    FLastModified: TDateTime;
    FNamespaceObject: TGocciaObjectValue;
    FEvaluationPromise: TGocciaValue;
    FAsyncCycleRoot: TGocciaModule;
    FStarExportNames: TOrderedStringMap<Boolean>;
    procedure SetExportBinding(const AExportName: string;
      const ABinding: TGocciaModuleExportBinding);
    function TryResolveExportIdentity(const AExportName: string;
      out ABindingModule: TGocciaModule; out ABindingName: string;
      out AValue: TGocciaValue): Boolean; overload;
    function TryResolveExportIdentity(const AExportName: string;
      out ABindingModule: TGocciaModule; out ABindingName: string;
      out AValue: TGocciaValue;
      const ASeen: TOrderedStringMap<Boolean>): Boolean; overload;
    function TryResolveExportValue(const AExportName: string;
      out AValue: TGocciaValue;
      const ASeen: TOrderedStringMap<Boolean>): Boolean;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;
    procedure AddExportBinding(const AExportName, ALocalName: string;
      const AEnvironment: TGCManagedObject);
    procedure AddExportForwarding(const AExportName: string;
      const ASourceModule: TGocciaModule; const ASourceExportName: string);
    function AddStarExportForwarding(const AExportName: string;
      const ASourceModule: TGocciaModule;
      const ASourceExportName: string): Boolean;
    procedure AddExportValue(const AExportName: string;
      const AValue: TGocciaValue);
    procedure CopyExportsFrom(const ASourceModule: TGocciaModule);
    procedure UpdateExportValue(const AExportName: string;
      const AValue: TGocciaValue);
    procedure ClearExports;
    function CanResolveExport(const AExportName: string): Boolean;
    function GetExportNames: TArray<string>;
    function GetNamespaceObject: TGocciaObjectValue;
    function HasExport(const AExportName: string): Boolean;
    procedure InvalidateNamespaceObject(const ADetachModule: Boolean = False);
    function IsAmbiguousExport(const AExportName: string): Boolean;
    procedure MarkExportReferences;
    procedure SetEnvironment(const AEnvironment: TGCManagedObject);
    function TryGetExportValue(const AExportName: string;
      out AValue: TGocciaValue): Boolean;
    property Path: string read FPath;
    property ExportsTable: TGocciaValueMap read FExportsTable;
    property EvaluationPromise: TGocciaValue read FEvaluationPromise write FEvaluationPromise;
    property AsyncCycleRoot: TGocciaModule read FAsyncCycleRoot
      write FAsyncCycleRoot;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

  TGocciaModuleNamespaceObject = class(TGocciaObjectValue)
  private
    FModule: TGocciaModule;
    procedure DetachModule;
  public
    constructor Create(const AModule: TGocciaModule);
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue;
      const ACanCreate: Boolean = True); override;
    function AssignPropertyWithReceiver(const AName: string;
      const AValue: TGocciaValue; const AReceiver: TGocciaValue): Boolean; override;
    function DeleteProperty(const AName: string): Boolean; override;
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetEnumerablePropertyEntries:
      TArray<TPair<string, TGocciaValue>>; override;
    function GetEnumerablePropertyNames: TArray<string>; override;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>; override;
    function GetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor; override;
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetOwnPropertyNames: TArray<string>; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function HasProperty(const AName: string): Boolean; override;
    procedure MarkReferences; override;
    procedure Freeze; override;
    procedure PreventExtensions; override;
    procedure Seal; override;
    function TestIntegrityFrozen: Boolean; override;
    function TestIntegritySealed: Boolean; override;
    function ToStringTag: string; override;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    property Module: TGocciaModule read FModule;
  end;

  TGocciaModuleSourceValue = class(TGocciaObjectValue)
  private
    FPath: string;
    FSourceText: UTF8String;
  public
    class function SharedPrototype: TGocciaObjectValue; static;
    class function SharedModuleSourcePrototype: TGocciaObjectValue; static;
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
    FEvaluationError: TGocciaValue;
    FHasEvaluationError: Boolean;
    FNamespaceObject: TGocciaObjectValue;
    function EnsureNamespaceObject: TGocciaObjectValue;
  public
    constructor Create(const AModulePath, AImportingFilePath: string;
      const ALoadModule: TLoadModuleCallback);
    function DeleteProperty(const AName: string): Boolean; override;
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;
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
    function GetOwnSymbols: TArray<TGocciaSymbolValue>; override;
    function ToStringTag: string; override;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    function GetSymbolProperty(const ASymbol: TGocciaSymbolValue):
      TGocciaValue; override;
    function GetSymbolPropertyWithReceiver(const ASymbol: TGocciaSymbolValue;
      const AReceiver: TGocciaValue): TGocciaValue; override;
    function GetOwnSymbolPropertyDescriptor(
      const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor; override;
    function HasSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean; override;
    procedure MarkReferences; override;
  end;

function TryReadImportAttributeType(const AOptions: TGocciaValue;
  out AAttributeType: string; out AHasAttributeType: Boolean): Boolean;
function EncodeImportSpecifierAttribute(const AModulePath,
  AAttributeType: string): string;
function DecodeImportSpecifierAttribute(const AEncodedModulePath: string;
  out AModulePath, AAttributeType: string): Boolean;

const
  DEFERRED_EVALUATION_REFERRER_PREFIX = #1'goccia-defer:';
  DEFERRED_MODULE_NOT_READY_MESSAGE =
    'Deferred module cannot be synchronously evaluated while it or one of its dependencies is evaluating';

implementation

uses
  Goccia.Arithmetic,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Keywords.Reserved,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.VM.Exception;

const
  IMPORT_ATTRIBUTE_SEPARATOR = #0;

var
  GModuleSourcePrototypeSlot: TGocciaRealmSlotId;
  GJSModuleSourcePrototypeSlot: TGocciaRealmSlotId;

procedure SortStringArray(var ANames: TArray<string>);
var
  I, J: Integer;
  Current: string;
begin
  for I := 1 to High(ANames) do
  begin
    Current := ANames[I];
    J := I - 1;
    while (J >= 0) and (CompareStr(ANames[J], Current) > 0) do
    begin
      ANames[J + 1] := ANames[J];
      Dec(J);
    end;
    ANames[J + 1] := Current;
  end;
end;

{ TGocciaModuleExportBinding }

constructor TGocciaModuleExportBinding.CreateValue(const AValue: TGocciaValue);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TGocciaModuleExportBinding.CreateBinding(
  const AEnvironment: TGCManagedObject; const ALocalName: string);
begin
  inherited Create;
  FEnvironment := AEnvironment;
  FLocalName := ALocalName;
end;

constructor TGocciaModuleExportBinding.CreateForwarding(
  const ASourceModule: TGocciaModule; const ASourceExportName: string);
begin
  inherited Create;
  FSourceModule := ASourceModule;
  FSourceExportName := ASourceExportName;
end;

function TGocciaModuleExportBinding.GetValue: TGocciaValue;
begin
  if Assigned(FSourceModule) then
  begin
    if FSourceModule.TryGetExportValue(FSourceExportName, Result) then
      Exit;
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if Assigned(FEnvironment) then
    Exit(TGocciaScope(FEnvironment).GetValue(FLocalName));

  if Assigned(FValue) then
    Result := FValue
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaModuleExportBinding.UpdateValue(const AValue: TGocciaValue);
begin
  FEnvironment := nil;
  FLocalName := '';
  FSourceModule := nil;
  FSourceExportName := '';
  FValue := AValue;
end;

procedure TGocciaModuleExportBinding.MarkReferences;
var
  Value: TGocciaValue;
begin
  Value := GetValue;
  if Assigned(Value) then
    Value.MarkReferences;
end;

{ TGocciaModule }

constructor TGocciaModule.Create(const APath: string);
begin
  FAmbiguousExports := TOrderedStringMap<Boolean>.Create;
  FPath := APath;
  FExportBindings := TGocciaModuleExportBindingMap.Create;
  FExportsTable := TGocciaValueMap.Create;
  FStarExportNames := TOrderedStringMap<Boolean>.Create;
end;

procedure TGocciaModule.SetEnvironment(const AEnvironment: TGCManagedObject);
begin
  if FEnvironment = AEnvironment then
    Exit;

  if Assigned(FEnvironment) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveRootObject(FEnvironment);

  FEnvironment := AEnvironment;

  if Assigned(FEnvironment) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddRootObject(FEnvironment);
end;

procedure TGocciaModule.SetExportBinding(const AExportName: string;
  const ABinding: TGocciaModuleExportBinding);
var
  ExistingBinding: TGocciaModuleExportBinding;
  Value: TGocciaValue;
begin
  InvalidateNamespaceObject;

  if FExportBindings.TryGetValue(AExportName, ExistingBinding) then
    ExistingBinding.Free;
  FAmbiguousExports.Remove(AExportName);
  FStarExportNames.Remove(AExportName);
  FExportBindings.AddOrSetValue(AExportName, ABinding);

  try
    Value := ABinding.GetValue;
  except
    on TGocciaReferenceError do
      Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
  FExportsTable.AddOrSetValue(AExportName, Value);
end;

procedure TGocciaModule.AddExportBinding(const AExportName, ALocalName: string;
  const AEnvironment: TGCManagedObject);
begin
  SetExportBinding(AExportName,
    TGocciaModuleExportBinding.CreateBinding(AEnvironment, ALocalName));
end;

procedure TGocciaModule.AddExportForwarding(const AExportName: string;
  const ASourceModule: TGocciaModule; const ASourceExportName: string);
begin
  SetExportBinding(AExportName,
    TGocciaModuleExportBinding.CreateForwarding(ASourceModule,
      ASourceExportName));
end;

function TGocciaModule.AddStarExportForwarding(const AExportName: string;
  const ASourceModule: TGocciaModule;
  const ASourceExportName: string): Boolean;
var
  ExistingBindingModule: TGocciaModule;
  ExistingBindingName: string;
  ExistingValue: TGocciaValue;
  NewBindingModule: TGocciaModule;
  NewBindingName: string;
  NewValue: TGocciaValue;

  function IsSameResolvedExport: Boolean;
  var
    SameBindingModule: Boolean;
  begin
    if TryResolveExportIdentity(AExportName, ExistingBindingModule,
       ExistingBindingName, ExistingValue) and
       ASourceModule.TryResolveExportIdentity(ASourceExportName,
       NewBindingModule, NewBindingName, NewValue) then
    begin
      if Assigned(ExistingBindingModule) or Assigned(NewBindingModule) then
      begin
        SameBindingModule := (ExistingBindingModule = NewBindingModule) or
          (Assigned(ExistingBindingModule) and Assigned(NewBindingModule) and
          (ExistingBindingModule.Path = NewBindingModule.Path));
        Exit(SameBindingModule and (ExistingBindingName = NewBindingName));
      end;
      Exit(Assigned(ExistingValue) and Assigned(NewValue) and
        (IsSameValue(ExistingValue, NewValue)) and
        (((ExistingValue is TGocciaModuleNamespaceObject) and
          (NewValue is TGocciaModuleNamespaceObject)) or
         ((ExistingValue is TGocciaModuleSourceValue) and
          (NewValue is TGocciaModuleSourceValue))));
    end;
    Result := False;
  end;
begin
  Result := False;
  if AExportName = KEYWORD_DEFAULT then
    Exit;

  if FAmbiguousExports.ContainsKey(AExportName) then
    Exit;

  if HasExport(AExportName) then
  begin
    if FStarExportNames.ContainsKey(AExportName) and IsSameResolvedExport then
      Exit;

    if FStarExportNames.ContainsKey(AExportName) then
    begin
      InvalidateNamespaceObject;
      FExportBindings.Remove(AExportName);
      FExportsTable.Remove(AExportName);
      FStarExportNames.Remove(AExportName);
      FAmbiguousExports.AddOrSetValue(AExportName, True);
      Result := True;
    end;
    Exit;
  end;

  if (not Assigned(ASourceModule)) or
     (not ASourceModule.TryResolveExportIdentity(ASourceExportName,
     NewBindingModule, NewBindingName, NewValue)) then
    Exit;

  SetExportBinding(AExportName,
    TGocciaModuleExportBinding.CreateForwarding(ASourceModule,
      ASourceExportName));
  FStarExportNames.AddOrSetValue(AExportName, True);
  Result := True;
end;

procedure TGocciaModule.AddExportValue(const AExportName: string;
  const AValue: TGocciaValue);
begin
  SetExportBinding(AExportName, TGocciaModuleExportBinding.CreateValue(AValue));
end;

procedure TGocciaModule.CopyExportsFrom(const ASourceModule: TGocciaModule);
var
  AmbiguousName: string;
  Binding: TGocciaModuleExportBinding;
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
  StarName: string;
begin
  ClearExports;
  if not Assigned(ASourceModule) then
    Exit;

  if Assigned(ASourceModule.FEnvironment) then
    SetEnvironment(ASourceModule.FEnvironment);

  for BindingPair in ASourceModule.FExportBindings do
  begin
    Binding := BindingPair.Value;
    if Assigned(Binding.FSourceModule) then
      AddExportForwarding(BindingPair.Key, Binding.FSourceModule,
        Binding.FSourceExportName)
    else if Assigned(Binding.FEnvironment) then
      AddExportBinding(BindingPair.Key, Binding.FLocalName,
        Binding.FEnvironment)
    else
      AddExportValue(BindingPair.Key, Binding.FValue);
  end;

  for AmbiguousName in ASourceModule.FAmbiguousExports.Keys do
    FAmbiguousExports.AddOrSetValue(AmbiguousName, True);
  for StarName in ASourceModule.FStarExportNames.Keys do
    FStarExportNames.AddOrSetValue(StarName, True);

  LastModified := ASourceModule.LastModified;
end;

procedure TGocciaModule.UpdateExportValue(const AExportName: string;
  const AValue: TGocciaValue);
var
  Binding: TGocciaModuleExportBinding;
begin
  if FExportBindings.TryGetValue(AExportName, Binding) then
  begin
    if Assigned(Binding.FEnvironment) then
      TGocciaScope(Binding.FEnvironment).ForceUpdateBinding(
        Binding.FLocalName, AValue)
    else if not Assigned(Binding.FSourceModule) then
      Binding.UpdateValue(AValue);
  end
  else
    FExportBindings.AddOrSetValue(AExportName,
      TGocciaModuleExportBinding.CreateValue(AValue));
  FAmbiguousExports.Remove(AExportName);
  FExportsTable.AddOrSetValue(AExportName, AValue);
end;

procedure TGocciaModule.ClearExports;
var
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
begin
  InvalidateNamespaceObject;
  for BindingPair in FExportBindings do
    BindingPair.Value.Free;
  FAmbiguousExports.Clear;
  FExportBindings.Clear;
  FExportsTable.Clear;
  FStarExportNames.Clear;
end;

function ModuleExportResolutionKey(const AModule: TGocciaModule;
  const AExportName: string): string;
begin
  if Assigned(AModule) then
    Result := AModule.Path + #0 + AExportName
  else
    Result := #0 + AExportName;
end;

function TGocciaModule.TryResolveExportIdentity(const AExportName: string;
  out ABindingModule: TGocciaModule; out ABindingName: string;
  out AValue: TGocciaValue): Boolean;
var
  Seen: TOrderedStringMap<Boolean>;
begin
  Seen := TOrderedStringMap<Boolean>.Create;
  try
    Result := TryResolveExportIdentity(AExportName, ABindingModule,
      ABindingName, AValue, Seen);
  finally
    Seen.Free;
  end;
end;

function TGocciaModule.TryResolveExportIdentity(const AExportName: string;
  out ABindingModule: TGocciaModule; out ABindingName: string;
  out AValue: TGocciaValue;
  const ASeen: TOrderedStringMap<Boolean>): Boolean;
var
  Binding: TGocciaModuleExportBinding;
  ResolutionKey: string;
begin
  ABindingModule := nil;
  ABindingName := '';
  AValue := nil;
  if FAmbiguousExports.ContainsKey(AExportName) then
    Exit(False);

  ResolutionKey := ModuleExportResolutionKey(Self, AExportName);
  if ASeen.ContainsKey(ResolutionKey) then
    Exit(False);
  ASeen.AddOrSetValue(ResolutionKey, True);
  try
    if FExportBindings.TryGetValue(AExportName, Binding) then
    begin
      if Assigned(Binding.FSourceModule) then
        Exit(Binding.FSourceModule.TryResolveExportIdentity(
          Binding.FSourceExportName, ABindingModule, ABindingName, AValue,
          ASeen));
      if Assigned(Binding.FEnvironment) then
      begin
        ABindingModule := Self;
        ABindingName := Binding.FLocalName;
        Exit(True);
      end;
      AValue := Binding.FValue;
      Exit(Assigned(AValue));
    end;

    Result := FExportsTable.TryGetValue(AExportName, AValue);
  finally
    ASeen.Remove(ResolutionKey);
  end;
end;

function TGocciaModule.TryResolveExportValue(const AExportName: string;
  out AValue: TGocciaValue;
  const ASeen: TOrderedStringMap<Boolean>): Boolean;
var
  Binding: TGocciaModuleExportBinding;
  ResolutionKey: string;
begin
  AValue := nil;
  if FAmbiguousExports.ContainsKey(AExportName) then
    Exit(False);

  ResolutionKey := ModuleExportResolutionKey(Self, AExportName);
  if ASeen.ContainsKey(ResolutionKey) then
    Exit(False);
  ASeen.AddOrSetValue(ResolutionKey, True);
  try
    if FExportBindings.TryGetValue(AExportName, Binding) then
    begin
      if Assigned(Binding.FSourceModule) then
        Exit(Binding.FSourceModule.TryResolveExportValue(
          Binding.FSourceExportName, AValue, ASeen));
      if Assigned(Binding.FEnvironment) then
      begin
        AValue := TGocciaScope(Binding.FEnvironment).GetValue(
          Binding.FLocalName);
        Exit(True);
      end;
      if Assigned(Binding.FValue) then
        AValue := Binding.FValue
      else
        AValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit(True);
    end;

    Result := FExportsTable.TryGetValue(AExportName, AValue);
  finally
    ASeen.Remove(ResolutionKey);
  end;
end;

function TGocciaModule.CanResolveExport(const AExportName: string): Boolean;
var
  BindingModule: TGocciaModule;
  BindingName: string;
  Value: TGocciaValue;
begin
  Result := TryResolveExportIdentity(AExportName, BindingModule, BindingName,
    Value);
end;

function TGocciaModule.GetExportNames: TArray<string>;
var
  Count: Integer;
  Key: string;
begin
  SetLength(Result, FExportsTable.Count);
  Count := 0;
  for Key in FExportsTable.Keys do
  begin
    Result[Count] := Key;
    Inc(Count);
  end;
  SetLength(Result, Count);
  SortStringArray(Result);
end;

// ES2026 §16.2.1.13 GetModuleNamespace(module)
function TGocciaModule.GetNamespaceObject: TGocciaObjectValue;
begin
  if not Assigned(FNamespaceObject) then
  begin
    FNamespaceObject := TGocciaModuleNamespaceObject.Create(Self);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddRootObject(FNamespaceObject);
  end;
  Result := FNamespaceObject;
end;

function TGocciaModule.HasExport(const AExportName: string): Boolean;
begin
  Result := FExportBindings.ContainsKey(AExportName) or
    FExportsTable.ContainsKey(AExportName);
end;

function TGocciaModule.IsAmbiguousExport(const AExportName: string): Boolean;
begin
  Result := FAmbiguousExports.ContainsKey(AExportName);
end;

procedure TGocciaModule.InvalidateNamespaceObject(const ADetachModule: Boolean);
begin
  if Assigned(FNamespaceObject) then
  begin
    if ADetachModule and (FNamespaceObject is TGocciaModuleNamespaceObject) then
      TGocciaModuleNamespaceObject(FNamespaceObject).DetachModule;
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveRootObject(FNamespaceObject);
    FNamespaceObject := nil;
  end;
end;

procedure TGocciaModule.MarkExportReferences;
var
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
  ExportPair: TGocciaValueMap.TKeyValuePair;
begin
  for BindingPair in FExportBindings do
    BindingPair.Value.MarkReferences;
  for ExportPair in FExportsTable do
    if Assigned(ExportPair.Value) then
      ExportPair.Value.MarkReferences;
  if Assigned(FEvaluationPromise) then
    FEvaluationPromise.MarkReferences;
end;

function TGocciaModule.TryGetExportValue(const AExportName: string;
  out AValue: TGocciaValue): Boolean;
var
  Seen: TOrderedStringMap<Boolean>;
begin
  Seen := TOrderedStringMap<Boolean>.Create;
  try
    Result := TryResolveExportValue(AExportName, AValue, Seen);
  finally
    Seen.Free;
  end;
end;

destructor TGocciaModule.Destroy;
var
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
begin
  InvalidateNamespaceObject(True);
  SetEnvironment(nil);
  for BindingPair in FExportBindings do
    BindingPair.Value.Free;
  FAmbiguousExports.Free;
  FExportBindings.Free;
  FExportsTable.Free;
  FStarExportNames.Free;
  inherited;
end;

{ TGocciaModuleNamespaceObject }

constructor TGocciaModuleNamespaceObject.Create(const AModule: TGocciaModule);
begin
  inherited Create(nil, AModule.ExportsTable.Count);
  FModule := AModule;
  DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create('Module'), []));
  FExtensible := False;
end;

procedure TGocciaModuleNamespaceObject.DetachModule;
begin
  FModule := nil;
end;

procedure TGocciaModuleNamespaceObject.AssignProperty(const AName: string;
  const AValue: TGocciaValue; const ACanCreate: Boolean = True);
begin
  if FModule.HasExport(AName) then
    ThrowTypeError(Format(SErrorReadOnlyPropertyFrozen, [AName]),
      SSuggestCannotDeleteNonConfigurable);
  inherited AssignProperty(AName, AValue, ACanCreate);
end;

function TGocciaModuleNamespaceObject.AssignPropertyWithReceiver(
  const AName: string; const AValue: TGocciaValue;
  const AReceiver: TGocciaValue): Boolean;
begin
  if FModule.HasExport(AName) then
    Exit(False);
  Result := inherited AssignPropertyWithReceiver(AName, AValue, AReceiver);
end;

function TGocciaModuleNamespaceObject.DeleteProperty(
  const AName: string): Boolean;
begin
  if FModule.HasExport(AName) then
    Exit(False);
  Result := inherited DeleteProperty(AName);
end;

procedure TGocciaModuleNamespaceObject.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  if not TryDefineProperty(AName, ADescriptor) then
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
      SSuggestCannotDeleteNonConfigurable);
end;

function TGocciaModuleNamespaceObject.GetAllPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaModuleNamespaceObject.GetEnumerablePropertyEntries:
  TArray<TPair<string, TGocciaValue>>;
var
  I: Integer;
  Names: TArray<string>;
begin
  Names := GetEnumerablePropertyNames;
  SetLength(Result, Length(Names));
  for I := 0 to High(Names) do
  begin
    Result[I].Key := Names[I];
    Result[I].Value := GetProperty(Names[I]);
  end;
end;

function TGocciaModuleNamespaceObject.GetEnumerablePropertyNames:
  TArray<string>;
var
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
begin
  Result := FModule.GetExportNames;
  for I := 0 to High(Result) do
  begin
    Descriptor := GetOwnPropertyDescriptor(Result[I]);
    Descriptor.Free;
  end;
end;

function TGocciaModuleNamespaceObject.GetEnumerablePropertyValues:
  TArray<TGocciaValue>;
var
  I: Integer;
  Names: TArray<string>;
begin
  Names := GetEnumerablePropertyNames;
  SetLength(Result, Length(Names));
  for I := 0 to High(Names) do
    Result[I] := GetProperty(Names[I]);
end;

// ES2026 §10.4.6.5 Module Namespace Exotic Objects [[GetOwnProperty]](P)
function TGocciaModuleNamespaceObject.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
var
  Value: TGocciaValue;
begin
  if FModule.TryGetExportValue(AName, Value) then
    Exit(TGocciaPropertyDescriptorData.Create(Value,
      [pfEnumerable, pfWritable]));
  Result := inherited GetOwnPropertyDescriptor(AName);
end;

// ES2026 §10.4.6.11 Module Namespace Exotic Objects [[OwnPropertyKeys]]()
function TGocciaModuleNamespaceObject.GetOwnPropertyKeys: TArray<string>;
begin
  Result := FModule.GetExportNames;
end;

function TGocciaModuleNamespaceObject.GetOwnPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

// ES2026 §10.4.6.8 Module Namespace Exotic Objects [[Get]](P, Receiver)
function TGocciaModuleNamespaceObject.GetProperty(
  const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaModuleNamespaceObject.GetPropertyWithContext(
  const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if FModule.TryGetExportValue(AName, Result) then
    Exit;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaModuleNamespaceObject.HasOwnProperty(
  const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not FModule.HasExport(AName) then
    Exit(inherited HasOwnProperty(AName));

  Descriptor := GetOwnPropertyDescriptor(AName);
  try
    Result := Assigned(Descriptor);
  finally
    Descriptor.Free;
  end;
end;

// ES2026 §10.4.6.7 Module Namespace Exotic Objects [[HasProperty]](P)
function TGocciaModuleNamespaceObject.HasProperty(const AName: string): Boolean;
begin
  Result := FModule.HasExport(AName) or inherited HasProperty(AName);
end;

procedure TGocciaModuleNamespaceObject.Freeze;
var
  ExportNames: TArray<string>;
begin
  ExportNames := FModule.GetExportNames;
  if Length(ExportNames) > 0 then
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable,
      [ExportNames[0]]), SSuggestCannotDeleteNonConfigurable);
  inherited Freeze;
end;

procedure TGocciaModuleNamespaceObject.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FModule) then
    FModule.MarkExportReferences;
end;

procedure TGocciaModuleNamespaceObject.PreventExtensions;
begin
  FExtensible := False;
end;

procedure TGocciaModuleNamespaceObject.Seal;
begin
  FExtensible := False;
end;

function TGocciaModuleNamespaceObject.TestIntegrityFrozen: Boolean;
begin
  Result := False;
end;

function TGocciaModuleNamespaceObject.TestIntegritySealed: Boolean;
begin
  Result := not FExtensible;
end;

function TGocciaModuleNamespaceObject.ToStringTag: string;
begin
  Result := 'Module';
end;

// ES2026 §10.4.6.6 Module Namespace Exotic Objects [[DefineOwnProperty]](P, Desc)
function TGocciaModuleNamespaceObject.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  CurrentValue: TGocciaValue;
begin
  try
    if not FModule.TryGetExportValue(AName, CurrentValue) then
      Exit(False);
    if ADescriptor.HasConfigurableField and ADescriptor.Configurable then
      Exit(False);
    if ADescriptor.HasEnumerableField and not ADescriptor.Enumerable then
      Exit(False);
    if IsAccessorDescriptor(ADescriptor) then
      Exit(False);
    if ADescriptor.HasWritableField and not ADescriptor.Writable then
      Exit(False);
    if ADescriptor.HasValue then
      Exit(IsSameValue(TGocciaPropertyDescriptorData(ADescriptor).Value,
        CurrentValue));
    Result := True;
  finally
    ADescriptor.Free;
  end;
end;

{ TGocciaModuleSourceValue }

class function TGocciaModuleSourceValue.SharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then
    Exit(nil);

  Result := TGocciaObjectValue(CurrentRealm.GetSlot(GModuleSourcePrototypeSlot));
  if not Assigned(Result) then
  begin
    Result := TGocciaObjectValue.Create(
      TGocciaObjectValue.SharedObjectPrototype);
    CurrentRealm.SetSlot(GModuleSourcePrototypeSlot, Result);
  end;
end;

class function TGocciaModuleSourceValue.SharedModuleSourcePrototype:
  TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then
    Exit(nil);

  Result := TGocciaObjectValue(CurrentRealm.GetSlot(GJSModuleSourcePrototypeSlot));
  if not Assigned(Result) then
  begin
    Result := TGocciaObjectValue.Create(SharedPrototype);
    CurrentRealm.SetSlot(GJSModuleSourcePrototypeSlot, Result);
  end;
end;

constructor TGocciaModuleSourceValue.Create(const APath: string;
  const ASourceText: UTF8String);
begin
  inherited Create(SharedModuleSourcePrototype);
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
  DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create('Deferred Module'), []));
  FExtensible := False;
  FSealed := True;
  FFrozen := True;
end;

function TGocciaDeferredModuleNamespaceObject.EnsureNamespaceObject: TGocciaObjectValue;
var
  Module: TGocciaModule;
begin
  if FHasEvaluationError then
    raise TGocciaThrowValue.Create(FEvaluationError);

  if not Assigned(FNamespaceObject) then
  begin
    if not Assigned(FLoadModule) then
      raise Exception.Create('Module loader is not available.');
    try
      Module := FLoadModule(FModulePath,
        DEFERRED_EVALUATION_REFERRER_PREFIX + FImportingFilePath);
    except
      on E: EGocciaBytecodeThrow do
      begin
        FEvaluationError := E.ThrownValue;
        FHasEvaluationError := True;
        raise TGocciaThrowValue.Create(FEvaluationError);
      end;
      on E: TGocciaThrowValue do
      begin
        FEvaluationError := E.Value;
        FHasEvaluationError := True;
        raise;
      end;
      on E: EGocciaDeferredModuleNotReady do
        raise TGocciaThrowValue.Create(
          CreateErrorObject(TYPE_ERROR_NAME, E.Message));
      on E: TGocciaTypeError do
      begin
        FEvaluationError := CreateErrorObject(TYPE_ERROR_NAME, E.Message);
        FHasEvaluationError := True;
        raise TGocciaThrowValue.Create(FEvaluationError);
      end;
    end;
    if not Assigned(Module) then
      raise Exception.CreateFmt(
        'Module loader returned nil for "%s" imported from "%s".',
        [FModulePath, FImportingFilePath]);
    FNamespaceObject := Module.GetNamespaceObject;
  end;
  Result := FNamespaceObject;
end;

function TGocciaDeferredModuleNamespaceObject.GetProperty(
  const AName: string): TGocciaValue;
begin
  if AName = PROP_THEN then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := EnsureNamespaceObject.GetProperty(AName);
end;

function TGocciaDeferredModuleNamespaceObject.ToStringTag: string;
begin
  Result := 'Deferred Module';
end;

function TGocciaDeferredModuleNamespaceObject.GetPropertyWithContext(
  const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_THEN then
    Exit(inherited GetPropertyWithContext(AName, AThisContext));
  Result := EnsureNamespaceObject.GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaDeferredModuleNamespaceObject.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
begin
  if AName = PROP_THEN then
    Exit(inherited GetOwnPropertyDescriptor(AName));
  Result := EnsureNamespaceObject.GetOwnPropertyDescriptor(AName);
end;

function TGocciaDeferredModuleNamespaceObject.HasProperty(
  const AName: string): Boolean;
begin
  if AName = PROP_THEN then
    Exit(inherited HasProperty(AName));
  Result := EnsureNamespaceObject.HasProperty(AName);
end;

function TGocciaDeferredModuleNamespaceObject.HasOwnProperty(
  const AName: string): Boolean;
begin
  if AName = PROP_THEN then
    Exit(inherited HasOwnProperty(AName));
  Result := EnsureNamespaceObject.HasOwnProperty(AName);
end;

procedure TGocciaDeferredModuleNamespaceObject.DefineProperty(
  const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
begin
  if AName = PROP_THEN then
  begin
    inherited DefineProperty(AName, ADescriptor);
    Exit;
  end;
  EnsureNamespaceObject.DefineProperty(AName, ADescriptor);
end;

function TGocciaDeferredModuleNamespaceObject.TryDefineProperty(
  const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  if AName = PROP_THEN then
    Exit(inherited TryDefineProperty(AName, ADescriptor));
  Result := EnsureNamespaceObject.TryDefineProperty(AName, ADescriptor);
end;

function TGocciaDeferredModuleNamespaceObject.DeleteProperty(
  const AName: string): Boolean;
begin
  if AName = PROP_THEN then
    Exit(inherited DeleteProperty(AName));
  Result := EnsureNamespaceObject.DeleteProperty(AName);
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

function TGocciaDeferredModuleNamespaceObject.GetOwnSymbols:
  TArray<TGocciaSymbolValue>;
begin
  Result := EnsureNamespaceObject.GetOwnSymbols;
end;

procedure TGocciaDeferredModuleNamespaceObject.DefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  inherited DefineSymbolProperty(ASymbol, ADescriptor);
end;

function TGocciaDeferredModuleNamespaceObject.TryDefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  Result := inherited TryDefineSymbolProperty(ASymbol, ADescriptor);
end;

function TGocciaDeferredModuleNamespaceObject.GetSymbolProperty(
  const ASymbol: TGocciaSymbolValue): TGocciaValue;
begin
  Result := inherited GetSymbolProperty(ASymbol);
end;

function TGocciaDeferredModuleNamespaceObject.GetSymbolPropertyWithReceiver(
  const ASymbol: TGocciaSymbolValue; const AReceiver: TGocciaValue):
  TGocciaValue;
begin
  Result := inherited GetSymbolPropertyWithReceiver(ASymbol, AReceiver);
end;

function TGocciaDeferredModuleNamespaceObject.GetOwnSymbolPropertyDescriptor(
  const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
begin
  Result := inherited GetOwnSymbolPropertyDescriptor(ASymbol);
end;

function TGocciaDeferredModuleNamespaceObject.HasSymbolProperty(
  const ASymbol: TGocciaSymbolValue): Boolean;
begin
  Result := inherited HasSymbolProperty(ASymbol);
end;

procedure TGocciaDeferredModuleNamespaceObject.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FNamespaceObject) then
    FNamespaceObject.MarkReferences;
  if Assigned(FEvaluationError) then
    FEvaluationError.MarkReferences;
end;

function TryReadImportAttributeType(const AOptions: TGocciaValue;
  out AAttributeType: string; out AHasAttributeType: Boolean): Boolean;
var
  AttributeKey: string;
  AttributeValue: TGocciaValue;
  I: Integer;
  AttributeNames: TArray<string>;
  OptionsObject: TGocciaObjectValue;
  WithObject: TGocciaObjectValue;
  WithValue: TGocciaValue;
begin
  AAttributeType := '';
  AHasAttributeType := False;
  Result := True;

  if (not Assigned(AOptions)) or (AOptions is TGocciaUndefinedLiteralValue) then
    Exit;

  if not (AOptions is TGocciaObjectValue) then
    raise TGocciaTypeError.Create(
      'Dynamic import options must be an object',
      0, 0, '', nil,
      'Pass an object such as { with: { type: "json" } }.');

  OptionsObject := TGocciaObjectValue(AOptions);
  WithValue := OptionsObject.GetProperty(PROP_WITH);
  if (not Assigned(WithValue)) or (WithValue is TGocciaUndefinedLiteralValue) then
    Exit;

  if not (WithValue is TGocciaObjectValue) then
    raise TGocciaTypeError.Create(
      'Dynamic import options.with must be an object',
      0, 0, '', nil,
      'Pass attributes as strings inside the with object.');

  WithObject := TGocciaObjectValue(WithValue);
  AttributeNames := WithObject.GetEnumerablePropertyNames;
  for I := 0 to High(AttributeNames) do
  begin
    AttributeKey := AttributeNames[I];
    AttributeValue := WithObject.GetProperty(AttributeKey);
    if not (AttributeValue is TGocciaStringLiteralValue) then
      raise TGocciaTypeError.Create(
        'Dynamic import attribute values must be strings',
        0, 0, '', nil,
        'Use string values in import attributes.');
    if AttributeKey <> PROP_TYPE then
      raise TGocciaTypeError.Create(
        Format('Unsupported dynamic import attribute "%s"', [AttributeKey]),
        0, 0, '', nil,
        'GocciaScript currently supports only the "type" import attribute.');
    AAttributeType := TGocciaStringLiteralValue(AttributeValue).Value;
    AHasAttributeType := True;
  end;
end;

function EncodeImportSpecifierAttribute(const AModulePath,
  AAttributeType: string): string;
begin
  if AAttributeType = '' then
    Exit(AModulePath);
  Result := AModulePath + IMPORT_ATTRIBUTE_SEPARATOR + AAttributeType;
end;

function DecodeImportSpecifierAttribute(const AEncodedModulePath: string;
  out AModulePath, AAttributeType: string): Boolean;
var
  SeparatorIndex: SizeInt;
begin
  SeparatorIndex := Pos(IMPORT_ATTRIBUTE_SEPARATOR, AEncodedModulePath);
  Result := SeparatorIndex > 0;
  if not Result then
  begin
    AModulePath := AEncodedModulePath;
    AAttributeType := '';
    Exit;
  end;

  AModulePath := Copy(AEncodedModulePath, 1, SeparatorIndex - 1);
  AAttributeType := Copy(AEncodedModulePath, SeparatorIndex + 1, MaxInt);
end;

initialization
  GModuleSourcePrototypeSlot := RegisterRealmSlot(
    'AbstractModuleSource.prototype');
  GJSModuleSourcePrototypeSlot := RegisterRealmSlot(
    'ModuleSource.prototype');

end.
