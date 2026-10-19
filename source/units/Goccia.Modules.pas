unit Goccia.Modules;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  OrderedStringMap,

  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot.Codec,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TGocciaImportCallPhase = (icpEvaluation, icpSource, icpDefer);
  { What a heap snapshot can make of a module record (ADR 0148): a source
    module the engine compiled, whose state is its environment and exports;
    a data module (JSON, text, bytes), whose exports are values; or a record
    a host or a runtime module loader built, which it refuses. }
  TGocciaModuleSnapshotKind = (mskNone, mskSource, mskData);
  EGocciaDeferredModuleNotReady = class(Exception);
  TGocciaModule = class;
  TGocciaModuleList = TList<TGocciaModule>;
  // One named type, so every unit shares its specialization.
  TGocciaModuleKeyList = TList<string>;
  TGocciaModuleResolutionStamp = record
    Module: TGocciaModule;
    Version: Cardinal;
  end;
  TGocciaModuleResolutionStamps = array of TGocciaModuleResolutionStamp;
  TGocciaModuleExportBinding = class;
  TGocciaModuleImportBinding = class;
  TGocciaModuleExportBindingMap = TOrderedStringMap<TGocciaModuleExportBinding>;
  TGocciaModuleImportBindingMap = TOrderedStringMap<TGocciaModuleImportBinding>;
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
    function IsUninitialized: Boolean;
    procedure UpdateValue(const AValue: TGocciaValue);
    procedure MarkReferences;
  end;

  TGocciaModuleImportBinding = class
  private
    FRequestedExportName: string;
    FRequestedModule: TGocciaModule;
    FResolvedBindingName: string;
    FResolvedModule: TGocciaModule;
    FResolvedValue: TGocciaValue;
    FResolutionStamps: TGocciaModuleResolutionStamps;
    FHasResolution: Boolean;
    FResolvedAsExportValue: Boolean;
    function ResolutionIsCurrent: Boolean;
    function Resolve: Boolean;
  public
    constructor Create(const ATargetModule: TGocciaModule;
      const ATargetExportName: string);
    function CanResolve: Boolean;
    function Matches(const ATargetModule: TGocciaModule;
      const ATargetExportName: string): Boolean;
    procedure MarkReferences;
    function TryGetValue(out AValue: TGocciaValue): Boolean;
    property RequestedModule: TGocciaModule read FRequestedModule;
    property RequestedExportName: string read FRequestedExportName;
  end;

  { Roots the values a module holds directly for as long as the module lives:
    its value-bound exports, the export snapshot table and the evaluation
    promise. A local export is rooted through the module's environment scope
    (SetEnvironment) and a forwarded one belongs to its source module, but a
    value binding has neither. The namespace object marks these values too,
    yet it exists only once something asks for it, so without this a host
    module's exports, or a JSON module's parsed value, were collectable while
    the module still handed them out. See ADR 0133. }
  TGocciaModuleRoots = class(TGCRootSource)
  private
    FModule: TGocciaModule;
  public
    constructor Create(const AModule: TGocciaModule);
    procedure MarkRootReferences; override;
  end;

  { What a module record held before a reload started linking a new version
    of its source into it (TGocciaModule.BeginReload). }
  TGocciaModuleReplacedState = class
  private
    FAmbiguousExports: TOrderedStringMap<Boolean>;
    FAsyncCycleRoot: TGocciaModule;
    FCompiledModule: TObject;
    FEnvironment: TGCManagedObject;
    FEvaluationPromise: TGocciaValue;
    FExportBindings: TGocciaModuleExportBindingMap;
    FExportsTable: TGocciaValueMap;
    FLastModified: TDateTime;
    FStarExportNames: TOrderedStringMap<Boolean>;
  public
    destructor Destroy; override;
  end;

  TGocciaModule = class
  private
    FRoots: TGocciaModuleRoots;
    FReplacedState: TGocciaModuleReplacedState;
    FAmbiguousExports: TOrderedStringMap<Boolean>;
    FPath: string;
    FEnvironment: TGCManagedObject;
    FExportBindings: TGocciaModuleExportBindingMap;
    FExportsTable: TGocciaValueMap;
    FLastModified: TDateTime;
    FIsHostOwned: Boolean;
    FNamespaceObject: TGocciaObjectValue;
    FEvaluationPromise: TGocciaValue;
    FAsyncCycleRoot: TGocciaModule;
    FStarExportNames: TOrderedStringMap<Boolean>;
    FExportResolutionVersion: Cardinal;
    FSnapshotKind: TGocciaModuleSnapshotKind;
    FCompiledModule: TObject;
    procedure InvalidateExportResolutions;
    procedure EnsureRoots;
    function GetExportsTable: TGocciaValueMap;
    procedure MarkHeldValues;
    procedure SetExportBinding(const AExportName: string;
      const ABinding: TGocciaModuleExportBinding);
    function TryResolveExportIdentity(const AExportName: string;
      out ABindingModule: TGocciaModule; out ABindingName: string;
      out AValue: TGocciaValue): Boolean; overload;
    function TryResolveExportIdentity(const AExportName: string;
      out ABindingModule: TGocciaModule; out ABindingName: string;
      out AValue: TGocciaValue;
      out AResolutionStamps: TGocciaModuleResolutionStamps): Boolean; overload;
    function TryResolveExportIdentity(const AExportName: string;
      out ABindingModule: TGocciaModule; out ABindingName: string;
      out AValue: TGocciaValue;
      const ASeen: TOrderedStringMap<Boolean>;
      var AResolutionStamps: TGocciaModuleResolutionStamps): Boolean; overload;
    function TryResolveExportValue(const AExportName: string;
      out AValue: TGocciaValue;
      const ASeen: TOrderedStringMap<Boolean>): Boolean;
    function TryGetDirectExportValue(const AExportName: string;
      out AValue: TGocciaValue): Boolean;
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
    { A reload links and evaluates the new version of a module's source into
      the record importers, namespace objects and the running module body
      already hold, so none of them is left pointing at a record the reload
      replaced. BeginReload sets the record's exports, environment and
      evaluation state aside and leaves it empty for the new version;
      CommitReload releases what was set aside once the new version linked;
      RollbackReload, for a version that failed to link, puts it back. }
    procedure BeginReload;
    procedure CommitReload;
    procedure RollbackReload;
    { Moves every export of ASourceModule, a record built to replace this
      one's contents (a data module read again), into this record, leaving
      ASourceModule empty so freeing it releases nothing this record uses. }
    procedure TakeExportsFrom(const ASourceModule: TGocciaModule);
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
    { A heap snapshot's payload of the record (ADR 0148): its environment,
      exports, namespace object, evaluation state and the records it refers
      to. ReadSnapshot fills an empty record. }
    procedure WriteSnapshot(const AWriter: TGocciaHeapSnapshotWriter);
    procedure ReadSnapshot(const AReader: TGocciaHeapSnapshotReader);
    // Once every record is filled: refuses a forwarded export whose chain of
    // modules is longer than a resolution should follow.
    procedure CheckSnapshotForwardings(
      const AReader: TGocciaHeapSnapshotReader);
    property Path: string read FPath;
    property ExportsTable: TGocciaValueMap read GetExportsTable;
    property EvaluationPromise: TGocciaValue read FEvaluationPromise write FEvaluationPromise;
    property AsyncCycleRoot: TGocciaModule read FAsyncCycleRoot
      write FAsyncCycleRoot;
    property LastModified: TDateTime read FLastModified write FLastModified;
    { Diagnostic ownership stamped at host enrollment and inherited by every
      module this module imports, including later dynamic/deferred imports. }
    property IsHostOwned: Boolean read FIsHostOwned write FIsHostOwned;
    property SnapshotKind: TGocciaModuleSnapshotKind read FSnapshotKind
      write FSnapshotKind;
    { The bytecode the engine compiled a source module to (a
      TGocciaBytecodeModule the engine keeps): a heap snapshot names the
      module by it. }
    property CompiledModule: TObject read FCompiledModule
      write FCompiledModule;
  end;

  TGocciaModuleNamespaceObject = class(TGocciaObjectValue)
  private
    FModule: TGocciaModule;
    FResolvedBindings: TGocciaModuleImportBindingMap;
    procedure ClearResolvedBindings;
    procedure DetachModule;
    function AcceptsDescriptor(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean;
  public
    constructor Create(const AModule: TGocciaModule);
    destructor Destroy; override;
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
    procedure TraceReferences; override;
    procedure Freeze; override;
    procedure PreventExtensions; override;
    procedure Seal; override;
    function TestIntegrityFrozen: Boolean; override;
    function TestIntegritySealed: Boolean; override;
    function ToStringTag: string; override;
    function CanResolveExport(const AName: string): Boolean;
    function TryGetExportValue(const AName: string;
      out AValue: TGocciaValue): Boolean;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    property Module: TGocciaModule read FModule;
  end;

  TGocciaModuleSourceValue = class(TGocciaObjectValue)
  private
    FPath: string;
    FSourceText: string;
  public
    class function SharedPrototype: TGocciaObjectValue; static;
    class function SharedModuleSourcePrototype: TGocciaObjectValue; static;
    class function ToStringTagGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    constructor Create(const APath: string; const ASourceText: string);
    function ToStringTag: string; override;
    property Path: string read FPath;
    property SourceText: string read FSourceText;
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
    procedure TraceReferences; override;
  end;

function TryReadImportAttributeType(const AOptions: TGocciaValue;
  out AAttributeType: string; out AHasAttributeType: Boolean): Boolean;
function EncodeImportSpecifierAttribute(const AModulePath,
  AAttributeType: string): string;
function DecodeImportSpecifierAttribute(const AEncodedModulePath: string;
  out AModulePath, AAttributeType: string): Boolean;

{ Marks a module request whose specifier was computed at run time (a dynamic
  import() whose specifier is not a string literal). Such a request is not
  part of the module graph, so reading the host for it always needs a read
  grant (ADR 0122). A guest that spells the marker itself only makes its own
  request stricter. }
function MarkComputedImportSpecifier(const AModulePath: string): string;
{ A module record a heap snapshot refers to, or nil: one the snapshot holds,
  else the snapshot is refused (ADR 0148). }
procedure WriteSnapshotModuleReference(const AWriter: TGocciaHeapSnapshotWriter;
  const AModule: TGocciaModule);
function ReadSnapshotModuleReference(
  const AReader: TGocciaHeapSnapshotReader): TGocciaModule;
{ Strips the computed marker; True when it was present. }
function StripComputedImportSpecifier(var AModulePath: string): Boolean;

const
  DEFERRED_EVALUATION_REFERRER_PREFIX = #1'goccia-defer:';
  COMPUTED_IMPORT_SPECIFIER_PREFIX = #2'goccia-computed:';
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
  Goccia.Values.NativeFunction,
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

{ Whether GetValue would raise the TDZ ReferenceError: the binding, or the
  forwarding chain behind it, ends at a module-scope binding that is still
  uninitialized. Answered without raising. False only means this check could
  not prove it; GetValue then decides. A plain TGocciaScope is required
  because a subclass may resolve a read before its own lexical bindings. }
function TGocciaModuleExportBinding.IsUninitialized: Boolean;
const
  MAX_FORWARDING_HOPS = 32;
var
  Binding: TGocciaModuleExportBinding;
  Hop: Integer;
  SourceModule: TGocciaModule;
begin
  Binding := Self;
  for Hop := 0 to MAX_FORWARDING_HOPS do
  begin
    SourceModule := Binding.FSourceModule;
    if not Assigned(SourceModule) then
      Exit(Assigned(Binding.FEnvironment) and
        (Binding.FEnvironment.ClassType = TGocciaScope) and
        TGocciaScope(Binding.FEnvironment).IsOwnBindingUninitialized(
          Binding.FLocalName));
    if SourceModule.FAmbiguousExports.ContainsKey(Binding.FSourceExportName) or
       not SourceModule.FExportBindings.TryGetValue(Binding.FSourceExportName,
         Binding) then
      Exit(False);
  end;
  Result := False;
end;

procedure TGocciaModuleExportBinding.UpdateValue(const AValue: TGocciaValue);
begin
  FEnvironment := nil;
  FLocalName := '';
  FSourceModule := nil;
  FSourceExportName := '';
  FValue := AValue;
end;

{ Marks what the binding holds without reading it. A local export is a
  binding in its module environment, which is marked whole: GetValue would
  raise the TDZ ReferenceError inside the collector while the module has not
  yet initialized the binding (a module in an import cycle), and the error
  would reach the guest code that triggered the collection. A forwarded
  export belongs to its source module, which roots its own bindings
  (TGocciaModuleRoots, SetEnvironment). See ADR 0133. }
procedure TGocciaModuleExportBinding.MarkReferences;
begin
  if Assigned(FSourceModule) then
    Exit;
  if Assigned(FEnvironment) then
    FEnvironment.MarkReferences
  else if Assigned(FValue) then
    FValue.MarkReferences;
end;

{ TGocciaModuleImportBinding }

constructor TGocciaModuleImportBinding.Create(
  const ATargetModule: TGocciaModule; const ATargetExportName: string);
begin
  inherited Create;
  FRequestedModule := ATargetModule;
  FRequestedExportName := ATargetExportName;
end;

function TGocciaModuleImportBinding.CanResolve: Boolean;
begin
  Result := (FHasResolution and ResolutionIsCurrent) or Resolve;
end;

function TGocciaModuleImportBinding.Matches(
  const ATargetModule: TGocciaModule;
  const ATargetExportName: string): Boolean;
begin
  Result := (FRequestedModule = ATargetModule) and
    (FRequestedExportName = ATargetExportName);
end;

procedure TGocciaModuleImportBinding.MarkReferences;
var
  Value: TGocciaValue;
begin
  try
    if TryGetValue(Value) and Assigned(Value) then
      Value.MarkReferences;
  except
    on TGocciaReferenceError do
      Exit;
  end;
end;

function TGocciaModuleImportBinding.TryGetValue(
  out AValue: TGocciaValue): Boolean;
begin
  AValue := nil;
  if ((not FHasResolution) or (not ResolutionIsCurrent)) and
     (not Resolve) then
    Exit(False);

  if FResolvedAsExportValue then
  begin
    Result := Assigned(FResolvedModule) and
      FResolvedModule.TryGetDirectExportValue(FResolvedBindingName, AValue);
    if Result then
      Exit;

    // Module hot reload can replace an export record. Re-resolve from the
    // requested module rather than retaining a stale record or scalar value.
    FHasResolution := False;
    if not Resolve then
      Exit(False);
    if FResolvedAsExportValue then
      Exit(FResolvedModule.TryGetDirectExportValue(
        FResolvedBindingName, AValue));
  end;

  if Assigned(FResolvedModule) and
     Assigned(FResolvedModule.FEnvironment) then
  begin
    AValue := TGocciaScope(FResolvedModule.FEnvironment).GetValue(
      FResolvedBindingName);
    Exit(True);
  end;

  AValue := FResolvedValue;
  Result := Assigned(AValue);
end;

function TGocciaModuleImportBinding.ResolutionIsCurrent: Boolean;
var
  I: Integer;
begin
  if Length(FResolutionStamps) = 0 then
    Exit(False);

  for I := 0 to High(FResolutionStamps) do
    if (not Assigned(FResolutionStamps[I].Module)) or
       (FResolutionStamps[I].Module.FExportResolutionVersion <>
       FResolutionStamps[I].Version) then
      Exit(False);

  Result := True;
end;

// ES2026 §9.1.1.5.5 CreateImportBinding stores the target module and binding
// name. Resolve that identity once, but never cache the binding's current
// value: GetBindingValue must remain live across mutation.
function TGocciaModuleImportBinding.Resolve: Boolean;
begin
  FResolvedBindingName := '';
  FResolvedModule := nil;
  FResolvedValue := nil;
  SetLength(FResolutionStamps, 0);
  FResolvedAsExportValue := False;
  FHasResolution := Assigned(FRequestedModule) and
    FRequestedModule.TryResolveExportIdentity(FRequestedExportName,
      FResolvedModule, FResolvedBindingName, FResolvedValue,
      FResolutionStamps);
  if FHasResolution then
    FResolvedAsExportValue := Assigned(FResolvedModule) and
      Assigned(FResolvedValue);
  Result := FHasResolution;
end;

{ TGocciaModuleReplacedState }

destructor TGocciaModuleReplacedState.Destroy;
var
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
begin
  if Assigned(FExportBindings) then
    for BindingPair in FExportBindings do
      BindingPair.Value.Free;
  FAmbiguousExports.Free;
  FExportBindings.Free;
  FExportsTable.Free;
  FStarExportNames.Free;
  inherited;
end;

{ TGocciaModuleRoots }

constructor TGocciaModuleRoots.Create(const AModule: TGocciaModule);
begin
  inherited Create;
  FModule := AModule;
end;

procedure TGocciaModuleRoots.MarkRootReferences;
begin
  FModule.MarkHeldValues;
end;

{ TGocciaModule }

constructor TGocciaModule.Create(const APath: string);
begin
  // An embedder may build a host module before any engine has started the
  // thread's collector; the root source registers only with a live one.
  // Initialize is idempotent.
  if not Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Initialize;
  FAmbiguousExports := TOrderedStringMap<Boolean>.Create;
  FPath := APath;
  FExportBindings := TGocciaModuleExportBindingMap.Create;
  FExportsTable := TGocciaValueMap.Create;
  FStarExportNames := TOrderedStringMap<Boolean>.Create;
  EnsureRoots;
end;

// A host can keep a module across a collector replacement (a thread pool that
// resets its runtime between work items), and a root source marks only for
// the collector it registered with. Re-register before the module takes a
// value the current collector manages, as TGocciaTimerQueue.EnsureRoots does.
procedure TGocciaModule.EnsureRoots;
var
  Collector: TGarbageCollector;
begin
  Collector := TGarbageCollector.Instance;
  if Assigned(FRoots) and Assigned(Collector) and
     (FRoots.RegisteredCollector = Collector) then
    Exit;
  FreeAndNil(FRoots);
  if Assigned(Collector) then
    FRoots := TGocciaModuleRoots.Create(Self);
end;

// Host modules publish by writing this table directly.
function TGocciaModule.GetExportsTable: TGocciaValueMap;
begin
  EnsureRoots;
  Result := FExportsTable;
end;

{ Only value bindings: reading a local binding could raise in its temporal
  dead zone, and its environment scope is a root of its own. The export table
  is marked as well: data modules (YAML, TOML, JSON5) write it without a
  binding, and it keeps the snapshot a local export took when it was linked,
  which a namespace object built later marks. }
procedure MarkHeldExportValues(const AExportBindings: TGocciaModuleExportBindingMap;
  const AExportsTable: TGocciaValueMap);
var
  Binding: TGocciaModuleExportBinding;
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
  ExportPair: TGocciaValueMap.TKeyValuePair;
begin
  for BindingPair in AExportBindings do
  begin
    Binding := BindingPair.Value;
    if Assigned(Binding.FValue) and not Assigned(Binding.FSourceModule) and
       not Assigned(Binding.FEnvironment) then
      Binding.FValue.MarkReferences;
  end;
  for ExportPair in AExportsTable do
    if Assigned(ExportPair.Value) then
      ExportPair.Value.MarkReferences;
end;

procedure TGocciaModule.MarkHeldValues;
begin
  MarkHeldExportValues(FExportBindings, FExportsTable);
  if Assigned(FEvaluationPromise) then
    FEvaluationPromise.MarkReferences;
  // What a reload set aside stays alive until the reload commits or rolls
  // back; its environment stays registered as a root of its own until then.
  if Assigned(FReplacedState) then
  begin
    MarkHeldExportValues(FReplacedState.FExportBindings,
      FReplacedState.FExportsTable);
    if Assigned(FReplacedState.FEvaluationPromise) then
      FReplacedState.FEvaluationPromise.MarkReferences;
  end;
end;

procedure TGocciaModule.InvalidateExportResolutions;
begin
  Inc(FExportResolutionVersion);
end;

procedure TGocciaModule.SetEnvironment(const AEnvironment: TGCManagedObject);
begin
  if FEnvironment = AEnvironment then
    Exit;

  if Assigned(FEnvironment) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.RemoveRootObject(FEnvironment);

  FEnvironment := AEnvironment;

  if Assigned(FEnvironment) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddRootObject(FEnvironment);
end;

procedure TGocciaModule.SetExportBinding(const AExportName: string;
  const ABinding: TGocciaModuleExportBinding);
var
  ExistingBinding: TGocciaModuleExportBinding;
  Value: TGocciaValue;
begin
  EnsureRoots;
  InvalidateNamespaceObject;

  if FExportBindings.TryGetValue(AExportName, ExistingBinding) then
    ExistingBinding.Free;
  FAmbiguousExports.Remove(AExportName);
  FStarExportNames.Remove(AExportName);
  FExportBindings.AddOrSetValue(AExportName, ABinding);

  // An export linked before its module evaluates is still in its temporal
  // dead zone and snapshots as undefined. Settle that common case without
  // raising and catching the ReferenceError a real read of it still throws.
  if ABinding.IsUninitialized then
    Value := TGocciaUndefinedLiteralValue.UndefinedValue
  else
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
  ExistingBinding: TGocciaModuleExportBinding;
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
      if FExportBindings.TryGetValue(AExportName, ExistingBinding) then
        ExistingBinding.Free;
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

procedure TGocciaModule.BeginReload;
begin
  if Assigned(FReplacedState) then
    raise Exception.CreateFmt('Module "%s" is already being reloaded',
      [FPath]);
  EnsureRoots;
  InvalidateNamespaceObject;
  FReplacedState := TGocciaModuleReplacedState.Create;
  FReplacedState.FAmbiguousExports := FAmbiguousExports;
  FReplacedState.FAsyncCycleRoot := FAsyncCycleRoot;
  FReplacedState.FCompiledModule := FCompiledModule;
  // Stays registered as a root until the reload ends, so RollbackReload can
  // put it back whatever the collector ran in between.
  FReplacedState.FEnvironment := FEnvironment;
  FReplacedState.FEvaluationPromise := FEvaluationPromise;
  FReplacedState.FExportBindings := FExportBindings;
  FReplacedState.FExportsTable := FExportsTable;
  FReplacedState.FLastModified := FLastModified;
  FReplacedState.FStarExportNames := FStarExportNames;
  FAmbiguousExports := TOrderedStringMap<Boolean>.Create;
  FAsyncCycleRoot := nil;
  FEnvironment := nil;
  FEvaluationPromise := nil;
  FExportBindings := TGocciaModuleExportBindingMap.Create;
  FExportsTable := TGocciaValueMap.Create;
  FStarExportNames := TOrderedStringMap<Boolean>.Create;
end;

procedure TGocciaModule.CommitReload;
var
  Replaced: TGocciaModuleReplacedState;
begin
  Replaced := FReplacedState;
  if not Assigned(Replaced) then
    Exit;
  FReplacedState := nil;
  // The record no longer roots the previous environment. Functions and
  // instances the previous version created still reference it, and keep it
  // alive for as long as they live.
  if Assigned(Replaced.FEnvironment) and
     (Replaced.FEnvironment <> FEnvironment) and
     (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.RemoveRootObject(Replaced.FEnvironment);
  Replaced.Free;
end;

procedure TGocciaModule.RollbackReload;
var
  Replaced: TGocciaModuleReplacedState;
begin
  Replaced := FReplacedState;
  if not Assigned(Replaced) then
    Exit;
  FReplacedState := nil;
  ClearExports;
  if FEnvironment <> Replaced.FEnvironment then
  begin
    if Assigned(FEnvironment) and (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveRootObject(FEnvironment);
    // Still registered as a root: BeginReload left it registered.
    FEnvironment := Replaced.FEnvironment;
  end;
  FAmbiguousExports.Free;
  FAmbiguousExports := Replaced.FAmbiguousExports;
  Replaced.FAmbiguousExports := nil;
  FExportBindings.Free;
  FExportBindings := Replaced.FExportBindings;
  Replaced.FExportBindings := nil;
  FExportsTable.Free;
  FExportsTable := Replaced.FExportsTable;
  Replaced.FExportsTable := nil;
  FStarExportNames.Free;
  FStarExportNames := Replaced.FStarExportNames;
  Replaced.FStarExportNames := nil;
  FAsyncCycleRoot := Replaced.FAsyncCycleRoot;
  FCompiledModule := Replaced.FCompiledModule;
  FEvaluationPromise := Replaced.FEvaluationPromise;
  FLastModified := Replaced.FLastModified;
  Replaced.Free;
  InvalidateNamespaceObject;
end;

procedure TGocciaModule.TakeExportsFrom(const ASourceModule: TGocciaModule);
var
  SwappedBindings: TGocciaModuleExportBindingMap;
  SwappedNames: TOrderedStringMap<Boolean>;
  SwappedTable: TGocciaValueMap;
begin
  ClearExports;
  if not Assigned(ASourceModule) or (ASourceModule = Self) then
    Exit;
  EnsureRoots;

  SwappedBindings := FExportBindings;
  FExportBindings := ASourceModule.FExportBindings;
  ASourceModule.FExportBindings := SwappedBindings;
  SwappedTable := FExportsTable;
  FExportsTable := ASourceModule.FExportsTable;
  ASourceModule.FExportsTable := SwappedTable;
  SwappedNames := FAmbiguousExports;
  FAmbiguousExports := ASourceModule.FAmbiguousExports;
  ASourceModule.FAmbiguousExports := SwappedNames;
  SwappedNames := FStarExportNames;
  FStarExportNames := ASourceModule.FStarExportNames;
  ASourceModule.FStarExportNames := SwappedNames;

  if Assigned(ASourceModule.FEnvironment) then
  begin
    SetEnvironment(ASourceModule.FEnvironment);
    // The root is this record's now: freeing the source must not retract it.
    ASourceModule.FEnvironment := nil;
  end;

  LastModified := ASourceModule.LastModified;
  ASourceModule.InvalidateNamespaceObject;
  InvalidateNamespaceObject;
end;

procedure TGocciaModule.UpdateExportValue(const AExportName: string;
  const AValue: TGocciaValue);
var
  Binding: TGocciaModuleExportBinding;
begin
  EnsureRoots;
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
  ResolutionStamps: TGocciaModuleResolutionStamps;
  Seen: TOrderedStringMap<Boolean>;
begin
  Seen := TOrderedStringMap<Boolean>.Create;
  try
    Result := TryResolveExportIdentity(AExportName, ABindingModule,
      ABindingName, AValue, Seen, ResolutionStamps);
  finally
    Seen.Free;
  end;
end;

function TGocciaModule.TryResolveExportIdentity(const AExportName: string;
  out ABindingModule: TGocciaModule; out ABindingName: string;
  out AValue: TGocciaValue;
  out AResolutionStamps: TGocciaModuleResolutionStamps): Boolean;
var
  Seen: TOrderedStringMap<Boolean>;
begin
  SetLength(AResolutionStamps, 0);
  Seen := TOrderedStringMap<Boolean>.Create;
  try
    Result := TryResolveExportIdentity(AExportName, ABindingModule,
      ABindingName, AValue, Seen, AResolutionStamps);
  finally
    Seen.Free;
  end;
end;

function TGocciaModule.TryResolveExportIdentity(const AExportName: string;
  out ABindingModule: TGocciaModule; out ABindingName: string;
  out AValue: TGocciaValue;
  const ASeen: TOrderedStringMap<Boolean>;
  var AResolutionStamps: TGocciaModuleResolutionStamps): Boolean;
var
  Binding: TGocciaModuleExportBinding;
  ResolutionStampIndex: Integer;
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
  ResolutionStampIndex := Length(AResolutionStamps);
  SetLength(AResolutionStamps, ResolutionStampIndex + 1);
  AResolutionStamps[ResolutionStampIndex].Module := Self;
  AResolutionStamps[ResolutionStampIndex].Version :=
    FExportResolutionVersion;
  try
    if FExportBindings.TryGetValue(AExportName, Binding) then
    begin
      if Assigned(Binding.FSourceModule) then
        Exit(Binding.FSourceModule.TryResolveExportIdentity(
          Binding.FSourceExportName, ABindingModule, ABindingName, AValue,
          ASeen, AResolutionStamps));
      if Assigned(Binding.FEnvironment) then
      begin
        ABindingModule := Self;
        ABindingName := Binding.FLocalName;
        Exit(True);
      end;
      AValue := Binding.FValue;
      // Namespace and source-phase re-exports are represented as their
      // immutable object value. Their resolved identity is that special
      // binding, not the intermediate module/export pair: two star-export
      // paths to the same object are unambiguous.
      if Assigned(AValue) and
         ((AValue is TGocciaModuleNamespaceObject) or
          (AValue is TGocciaModuleSourceValue)) then
        Exit(True);
      ABindingModule := Self;
      ABindingName := AExportName;
      Exit(Assigned(AValue));
    end;

    Result := FExportsTable.TryGetValue(AExportName, AValue);
    if Result then
    begin
      ABindingModule := Self;
      ABindingName := AExportName;
    end;
  finally
    ASeen.Remove(ResolutionKey);
  end;
end;

function TGocciaModule.TryGetDirectExportValue(const AExportName: string;
  out AValue: TGocciaValue): Boolean;
var
  Binding: TGocciaModuleExportBinding;
begin
  AValue := nil;
  if FAmbiguousExports.ContainsKey(AExportName) then
    Exit(False);

  if FExportBindings.TryGetValue(AExportName, Binding) then
  begin
    if Assigned(Binding.FSourceModule) or Assigned(Binding.FEnvironment) then
      Exit(False);
    AValue := Binding.GetValue;
    Exit(True);
  end;

  Result := FExportsTable.TryGetValue(AExportName, AValue);
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
  Binding: TGocciaModuleExportBinding;
  BindingModule: TGocciaModule;
  BindingName: string;
  Value: TGocciaValue;
begin
  // ES2026 §16.2.1.7.2.2 ResolveExport: a local export resolves to this
  // module's own binding, with no resolveSet to track. Answer that common
  // case without allocating one.
  if (not FAmbiguousExports.ContainsKey(AExportName)) and
     FExportBindings.TryGetValue(AExportName, Binding) and
     (not Assigned(Binding.FSourceModule)) and
     Assigned(Binding.FEnvironment) then
    Exit(True);
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
    if (TGarbageCollector.Instance <> nil) then
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

{ A change to the module's exports drops what the namespace object resolved,
  but keeps the object: ES2026 §16.2.1.13 GetModuleNamespace creates a
  module's [[Namespace]] once, so every namespace import and `export * as`
  of the module is the same object, however late an export is registered.
  Only a module that is going away detaches and releases it. }
procedure TGocciaModule.InvalidateNamespaceObject(const ADetachModule: Boolean);
begin
  InvalidateExportResolutions;
  if Assigned(FNamespaceObject) then
  begin
    if FNamespaceObject is TGocciaModuleNamespaceObject then
    begin
      TGocciaModuleNamespaceObject(FNamespaceObject).ClearResolvedBindings;
      if ADetachModule then
        TGocciaModuleNamespaceObject(FNamespaceObject).DetachModule;
    end;
    if not ADetachModule then
      Exit;
    if (TGarbageCollector.Instance <> nil) then
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
  FreeAndNil(FRoots);
  CommitReload;
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
  FResolvedBindings := TGocciaModuleImportBindingMap.Create;
  DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create('Module'), []));
  FExtensible := False;
end;

procedure TGocciaModuleNamespaceObject.ClearResolvedBindings;
var
  BindingPair: TGocciaModuleImportBindingMap.TKeyValuePair;
begin
  for BindingPair in FResolvedBindings do
    BindingPair.Value.Free;
  FResolvedBindings.Clear;
end;

procedure TGocciaModuleNamespaceObject.DetachModule;
begin
  FModule := nil;
end;

destructor TGocciaModuleNamespaceObject.Destroy;
begin
  ClearResolvedBindings;
  FResolvedBindings.Free;
  inherited;
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

// DefineProperty follows the base contract: it takes ownership of ADescriptor
// only on success, and leaves it to the caller when it throws. Going through
// TryDefineProperty here freed the descriptor before the throw, so callers
// such as Object.defineProperty freed it a second time.
procedure TGocciaModuleNamespaceObject.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  if not AcceptsDescriptor(AName, ADescriptor) then
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
      SSuggestCannotDeleteNonConfigurable);
  ADescriptor.Free;
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
  I: Integer;
begin
  Result := FModule.GetExportNames;
  { [[GetOwnProperty]] of each export: it throws for one still in its TDZ. }
  for I := 0 to High(Result) do
    HasOwnPropertyDescriptor(Result[I]);
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
  if TryGetExportValue(AName, Value) then
    Exit(TGocciaPropertyDescriptorData.Create(Value,
      [pfEnumerable, pfWritable]).AsCallerOwned);
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
  if TryGetExportValue(AName, Result) then
    Exit;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaModuleNamespaceObject.HasOwnProperty(
  const AName: string): Boolean;
begin
  if not FModule.HasExport(AName) then
    Exit(inherited HasOwnProperty(AName));

  Result := HasOwnPropertyDescriptor(AName);
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

procedure TGocciaModuleNamespaceObject.TraceReferences;
begin
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

function TGocciaModuleNamespaceObject.CanResolveExport(
  const AName: string): Boolean;
var
  Binding: TGocciaModuleImportBinding;
begin
  if not Assigned(FModule) then
    Exit(False);

  if FResolvedBindings.TryGetValue(AName, Binding) then
    Exit(Binding.CanResolve);

  Binding := TGocciaModuleImportBinding.Create(FModule, AName);
  if not Binding.CanResolve then
  begin
    Binding.Free;
    Exit(False);
  end;
  FResolvedBindings.AddOrSetValue(AName, Binding);
  Result := True;
end;

function TGocciaModuleNamespaceObject.TryGetExportValue(
  const AName: string; out AValue: TGocciaValue): Boolean;
var
  Binding: TGocciaModuleImportBinding;
begin
  AValue := nil;
  if not Assigned(FModule) then
    Exit(False);

  if not FResolvedBindings.TryGetValue(AName, Binding) then
  begin
    if not CanResolveExport(AName) then
      Exit(False);
    FResolvedBindings.TryGetValue(AName, Binding);
  end;

  Result := Binding.TryGetValue(AValue);
end;

// ES2026 §10.4.6.6 Module Namespace Exotic Objects [[DefineOwnProperty]](P, Desc)
// for a string key: the export must exist and the descriptor must describe it
// unchanged. Owns nothing.
function TGocciaModuleNamespaceObject.AcceptsDescriptor(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  CurrentValue: TGocciaValue;
begin
  if not TryGetExportValue(AName, CurrentValue) then
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
end;

// Takes ownership of ADescriptor in every case, like the base TryDefineProperty.
function TGocciaModuleNamespaceObject.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  try
    Result := AcceptsDescriptor(AName, ADescriptor);
  finally
    ADescriptor.Free;
  end;
end;

{ TGocciaModuleSourceValue }

class function TGocciaModuleSourceValue.SharedPrototype: TGocciaObjectValue;
begin
  if (CurrentRealm = nil) then
    Exit(nil);

  Result := TGocciaObjectValue(CurrentRealm.GetSlot(GModuleSourcePrototypeSlot));
  if not Assigned(Result) then
  begin
    Result := TGocciaObjectValue.Create(
      TGocciaObjectValue.SharedObjectPrototype);
    CurrentRealm.SetSlot(GModuleSourcePrototypeSlot, Result);
    // %AbstractModuleSource%.prototype [ %Symbol.toStringTag% ]: an accessor
    // that reports [[ModuleSourceClassName]], and undefined for anything
    // without that slot.
    Result.DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(ToStringTagGetter,
          'get [Symbol.toStringTag]', 0),
        nil, [pfConfigurable]));
  end;
end;

class function TGocciaModuleSourceValue.ToStringTagGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue is TGocciaModuleSourceValue then
    Result := TGocciaStringLiteralValue.Create(
      TGocciaModuleSourceValue(AThisValue).ToStringTag)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

class function TGocciaModuleSourceValue.SharedModuleSourcePrototype:
  TGocciaObjectValue;
begin
  if (CurrentRealm = nil) then
    Exit(nil);

  Result := TGocciaObjectValue(CurrentRealm.GetSlot(GJSModuleSourcePrototypeSlot));
  if not Assigned(Result) then
  begin
    Result := TGocciaObjectValue.Create(SharedPrototype);
    CurrentRealm.SetSlot(GJSModuleSourcePrototypeSlot, Result);
  end;
end;

constructor TGocciaModuleSourceValue.Create(const APath: string;
  const ASourceText: string);
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
var
  Namespace: TGocciaObjectValue;
begin
  if AName = PROP_THEN then
    Exit(inherited TryDefineProperty(AName, ADescriptor));
  // The call takes ownership of ADescriptor, so it is freed here when the
  // module's evaluation error stops the call before it reaches the namespace.
  try
    Namespace := EnsureNamespaceObject;
  except
    ADescriptor.Free;
    raise;
  end;
  Result := Namespace.TryDefineProperty(AName, ADescriptor);
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

procedure TGocciaDeferredModuleNamespaceObject.TraceReferences;
begin
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

function MarkComputedImportSpecifier(const AModulePath: string): string;
begin
  Result := COMPUTED_IMPORT_SPECIFIER_PREFIX + AModulePath;
end;

function StripComputedImportSpecifier(var AModulePath: string): Boolean;
begin
  Result := Copy(AModulePath, 1, Length(COMPUTED_IMPORT_SPECIFIER_PREFIX)) =
    COMPUTED_IMPORT_SPECIFIER_PREFIX;
  if Result then
    Delete(AModulePath, 1, Length(COMPUTED_IMPORT_SPECIFIER_PREFIX));
end;

function DecodeImportSpecifierAttribute(const AEncodedModulePath: string;
  out AModulePath, AAttributeType: string): Boolean;
var
  SeparatorIndex: NativeInt;
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

{ Heap snapshots (ADR 0148): a module record's payload, and the codec of
  its namespace object. A record is not a managed object; the record codec
  (Goccia.Modules.Snapshot) lists every record of the run and writes each
  one's header, and the record writes and reads its own payload here. }

const
  // An export binding's kind in a record's payload.
  SNAPSHOT_EXPORT_LOCAL = 0;
  SNAPSHOT_EXPORT_FORWARD = 1;
  SNAPSHOT_EXPORT_VALUE = 2;
  // What a restored export binding or name holds, charged before it is made.
  SNAPSHOT_EXPORT_ENTRY_BYTES = 128;

type
  TModuleNamespaceSnapshotCodec = class(TGocciaSnapshotCodec)
  public
    function Tag: string; override;
    function SchemaVersion: Word; override;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
    procedure Finish(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
  end;

procedure WriteSnapshotModuleReference(const AWriter: TGocciaHeapSnapshotWriter;
  const AModule: TGocciaModule);
begin
  if Assigned(AModule) and not AWriter.HoldsModuleRecord(AModule) then
    AWriter.Refuse('a binding of the module ' + AModule.Path + ', which ' +
      'the snapshot cannot hold: a host module, or one that failed or was ' +
      'replaced');
  AWriter.WriteModuleRecord(AModule);
end;

function ReadSnapshotModuleReference(
  const AReader: TGocciaHeapSnapshotReader): TGocciaModule;
var
  Rec: TObject;
begin
  Rec := AReader.ReadModuleRecord;
  if Assigned(Rec) and not (Rec is TGocciaModule) then
    AReader.Refuse('a module record of another kind');
  Result := TGocciaModule(Rec);
end;

procedure WriteSnapshotNames(const AWriter: TGocciaHeapSnapshotWriter;
  const ANames: TOrderedStringMap<Boolean>);
var
  Pair: TOrderedStringMap<Boolean>.TKeyValuePair;
begin
  AWriter.WriteU32(ANames.Count);
  for Pair in ANames do
  begin
    AWriter.Poll;
    AWriter.WriteStr(Pair.Key);
  end;
end;

procedure ReadSnapshotNames(const AReader: TGocciaHeapSnapshotReader;
  const ANames: TOrderedStringMap<Boolean>);
var
  Count, I: Integer;
  Name: string;
begin
  Count := AReader.ReadItemCount(4);
  AReader.ReserveItems(Count, SNAPSHOT_EXPORT_ENTRY_BYTES);
  for I := 0 to Count - 1 do
  begin
    AReader.Poll;
    Name := AReader.ReadStr;
    if ANames.ContainsKey(Name) then
      AReader.Refuse('an export name listed twice');
    ANames.Add(Name, True);
  end;
end;

procedure TGocciaModule.WriteSnapshot(
  const AWriter: TGocciaHeapSnapshotWriter);
var
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
  ExportPair: TGocciaValueMap.TKeyValuePair;
  Binding: TGocciaModuleExportBinding;
begin
  if Assigned(FReplacedState) then
    AWriter.Refuse('the module ' + FPath + ' is being reloaded');
  AWriter.WriteDouble(FLastModified);
  AWriter.WriteRef(FEnvironment);
  AWriter.WriteU32(FExportBindings.Count);
  for BindingPair in FExportBindings do
  begin
    AWriter.Poll;
    Binding := BindingPair.Value;
    AWriter.WriteStr(BindingPair.Key);
    if Assigned(Binding.FSourceModule) then
    begin
      AWriter.WriteU8(SNAPSHOT_EXPORT_FORWARD);
      WriteSnapshotModuleReference(AWriter, Binding.FSourceModule);
      AWriter.WriteStr(Binding.FSourceExportName);
    end
    else if Assigned(Binding.FEnvironment) then
    begin
      AWriter.WriteU8(SNAPSHOT_EXPORT_LOCAL);
      AWriter.WriteRef(Binding.FEnvironment);
      AWriter.WriteStr(Binding.FLocalName);
    end
    else
    begin
      AWriter.WriteU8(SNAPSHOT_EXPORT_VALUE);
      AWriter.WriteValue(Binding.FValue);
    end;
  end;
  AWriter.WriteU32(FExportsTable.Count);
  for ExportPair in FExportsTable do
  begin
    AWriter.Poll;
    AWriter.WriteStr(ExportPair.Key);
    AWriter.WriteValue(ExportPair.Value);
  end;
  WriteSnapshotNames(AWriter, FAmbiguousExports);
  WriteSnapshotNames(AWriter, FStarExportNames);
  AWriter.WriteValue(FNamespaceObject);
  AWriter.WriteValue(FEvaluationPromise);
  WriteSnapshotModuleReference(AWriter, FAsyncCycleRoot);
end;

procedure TGocciaModule.ReadSnapshot(const AReader: TGocciaHeapSnapshotReader);
var
  Count, I: Integer;
  Name, LocalName: string;
  Kind: Byte;
  Environment: TGCManagedObject;
  SourceModule: TGocciaModule;
  Value, Namespace: TGocciaValue;
begin
  if (FExportBindings.Count > 0) or (FExportsTable.Count > 0) or
     Assigned(FEnvironment) or Assigned(FNamespaceObject) then
    AReader.Refuse('a module record filled twice');
  EnsureRoots;
  FLastModified := AReader.ReadDouble;
  Environment := AReader.ReadRef;
  if Assigned(Environment) then
  begin
    if Environment.ClassType <> TGocciaScope then
      AReader.Refuse('a module environment that is not a module scope');
    // Each record roots its own environment, and a root is held once: two
    // records sharing one would leave it unrooted when the first goes.
    if Assigned(AReader.Scratch(Environment)) then
      AReader.Refuse('two modules that share an environment');
    AReader.SetScratch(Environment, TObject.Create);
  end;
  SetEnvironment(Environment);
  Count := AReader.ReadItemCount(6);
  AReader.ReserveItems(Count, SNAPSHOT_EXPORT_ENTRY_BYTES);
  for I := 0 to Count - 1 do
  begin
    AReader.Poll;
    Name := AReader.ReadStr;
    if FExportBindings.ContainsKey(Name) then
      AReader.Refuse('an export bound twice');
    Kind := AReader.ReadU8;
    case Kind of
      SNAPSHOT_EXPORT_LOCAL:
        begin
          // A local export is a binding of the module's own environment,
          // which the module roots, and never one of its import bindings,
          // which read through another export: a forged one could hold a
          // scope nothing roots, or read itself without end.
          Environment := AReader.ReadRef;
          if not Assigned(Environment) or (Environment <> FEnvironment) then
            AReader.Refuse('a local export outside its module''s ' +
              'environment');
          LocalName := AReader.ReadStr;
          if not (TGocciaScope(Environment).ContainsOwnLexicalBinding(
               LocalName) or TGocciaScope(Environment).ContainsOwnVarBinding(
               LocalName)) or
             TGocciaScope(Environment).IsOwnImportBinding(LocalName) then
            AReader.Refuse('a local export of no local binding');
          FExportBindings.Add(Name,
            TGocciaModuleExportBinding.CreateBinding(Environment, LocalName));
        end;
      SNAPSHOT_EXPORT_FORWARD:
        begin
          SourceModule := ReadSnapshotModuleReference(AReader);
          if not Assigned(SourceModule) then
            AReader.Refuse('an export forwarded from no module');
          LocalName := AReader.ReadStr;
          FExportBindings.Add(Name,
            TGocciaModuleExportBinding.CreateForwarding(SourceModule,
              LocalName));
        end;
      SNAPSHOT_EXPORT_VALUE:
        FExportBindings.Add(Name,
          TGocciaModuleExportBinding.CreateValue(AReader.ReadValue));
    else
      AReader.Refuse('an unknown export binding kind');
    end;
  end;
  Count := AReader.ReadItemCount(5);
  AReader.ReserveItems(Count, SNAPSHOT_EXPORT_ENTRY_BYTES);
  for I := 0 to Count - 1 do
  begin
    AReader.Poll;
    Name := AReader.ReadStr;
    if FExportsTable.ContainsKey(Name) then
      AReader.Refuse('an export listed twice');
    Value := AReader.ReadValue;
    FExportsTable.Add(Name, Value);
  end;
  ReadSnapshotNames(AReader, FAmbiguousExports);
  ReadSnapshotNames(AReader, FStarExportNames);
  Namespace := AReader.ReadValue;
  if Assigned(Namespace) then
  begin
    if not (Namespace is TGocciaModuleNamespaceObject) or
       (TGocciaModuleNamespaceObject(Namespace).FModule <> Self) then
      AReader.Refuse('a namespace object of another module');
    FNamespaceObject := TGocciaModuleNamespaceObject(Namespace);
    if TGarbageCollector.Instance <> nil then
      TGarbageCollector.Instance.AddRootObject(FNamespaceObject);
  end;
  FEvaluationPromise := AReader.ReadValue;
  FAsyncCycleRoot := ReadSnapshotModuleReference(AReader);
  InvalidateExportResolutions;
end;

// Resolving an export follows its forwardings one call per module; a
// snapshot's chains are kept far shorter than any native stack.
procedure TGocciaModule.CheckSnapshotForwardings(
  const AReader: TGocciaHeapSnapshotReader);
const
  MAX_FORWARDING_HOPS = 1024;
var
  BindingPair: TGocciaModuleExportBindingMap.TKeyValuePair;
  Binding: TGocciaModuleExportBinding;
  Hops: Integer;
begin
  for BindingPair in FExportBindings do
  begin
    AReader.Poll;
    Binding := BindingPair.Value;
    Hops := 0;
    while Assigned(Binding.FSourceModule) do
    begin
      Inc(Hops);
      if Hops > MAX_FORWARDING_HOPS then
        AReader.Refuse('an export forwarded through more modules than a ' +
          'resolution follows');
      if Hops and 63 = 0 then
        AReader.Poll;
      if not Binding.FSourceModule.FExportBindings.TryGetValue(
         Binding.FSourceExportName, Binding) then
        Break;
    end;
  end;
end;

{ TModuleNamespaceSnapshotCodec }

function TModuleNamespaceSnapshotCodec.Tag: string;
begin
  Result := 'module.namespace';
end;

function TModuleNamespaceSnapshotCodec.SchemaVersion: Word;
begin
  Result := 1;
end;

// A namespace object is its module's: its exports are the module's, and it
// has no state of its own a run can change.
procedure TModuleNamespaceSnapshotCodec.Write(
  const AWriter: TGocciaHeapSnapshotWriter; const AObject: TGCManagedObject);
var
  Namespace: TGocciaModuleNamespaceObject;
begin
  Namespace := TGocciaModuleNamespaceObject(AObject);
  if not Assigned(Namespace.FModule) then
    AWriter.Refuse('a namespace object whose module went away');
  WriteSnapshotModuleReference(AWriter, Namespace.FModule);
end;

function TModuleNamespaceSnapshotCodec.Allocate(
  const AReader: TGocciaHeapSnapshotReader): TGCManagedObject;
var
  Module: TGocciaModule;
begin
  Module := ReadSnapshotModuleReference(AReader);
  if not Assigned(Module) then
    AReader.Refuse('a namespace object of no module');
  Result := TGocciaModuleNamespaceObject.Create(Module);
end;

procedure TModuleNamespaceSnapshotCodec.Fill(
  const AReader: TGocciaHeapSnapshotReader; const AObject: TGCManagedObject);
begin
  if ReadSnapshotModuleReference(AReader) <>
     TGocciaModuleNamespaceObject(AObject).FModule then
    AReader.Refuse('a namespace object whose module changed');
end;

// The records are filled by now: the module must hold this namespace object,
// or it would make another.
procedure TModuleNamespaceSnapshotCodec.Finish(
  const AReader: TGocciaHeapSnapshotReader; const AObject: TGCManagedObject);
var
  Namespace: TGocciaModuleNamespaceObject;
begin
  Namespace := TGocciaModuleNamespaceObject(AObject);
  if Namespace.FModule.FNamespaceObject <> Namespace then
    AReader.Refuse('a namespace object its module does not hold');
end;

initialization
  GModuleSourcePrototypeSlot := RegisterRealmSlot(
    'AbstractModuleSource.prototype');
  GJSModuleSourcePrototypeSlot := RegisterRealmSlot(
    'ModuleSource.prototype');
  RegisterSnapshotCodec(TGocciaModuleNamespaceObject,
    TModuleNamespaceSnapshotCodec.Create);

end.
