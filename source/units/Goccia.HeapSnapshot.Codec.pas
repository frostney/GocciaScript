unit Goccia.HeapSnapshot.Codec;

{ Heap snapshot prototype: the structured object-graph format.

  A snapshot is a numbered table of objects, each written by the codec its
  class registered, plus references into a table of built-ins named by path
  and a few roots. Primitives are written inline. An object whose class has
  no codec refuses the snapshot.

  Restore runs in three passes over the object table: Allocate creates every
  object as an empty shell, Fill reads each payload (a reference resolves to
  a shell), and Finish fixes up state that needs other objects filled first.
  Every shell is rooted until restore ends. }

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  HashMap,

  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.GarbageCollector,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue,
  Goccia.VM.Registers;

const
  HEAP_SNAPSHOT_MAGIC = 'GHS1';
  HEAP_SNAPSHOT_FORMAT_VERSION = 3;
  // Separates the segments of a built-in's path; no property key or symbol
  // description of a built-in contains it.
  HEAP_SNAPSHOT_PATH_SEPARATOR = #31;

  // Value tags.
  HSV_NIL = 0;
  HSV_UNDEFINED = 1;
  HSV_NULL = 2;
  HSV_TRUE = 3;
  HSV_FALSE = 4;
  HSV_HOLE = 5;
  HSV_NUMBER = 6;
  HSV_STRING = 7;
  HSV_BIGINT = 8;
  HSV_OBJECT = 9;
  HSV_BUILTIN = 10;
  HSV_MODULE_SCOPE = 11;
  HSV_REGISTERED_SYMBOL = 12;

type
  EGocciaHeapSnapshotRefused = class(Exception);

  TGocciaHeapSnapshotWriter = class;
  TGocciaHeapSnapshotReader = class;

  { One per serializable class, registered by the unit that owns the class
    (it can reach the private state). }
  TGocciaSnapshotCodec = class
  public
    function Tag: string; virtual; abstract;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); virtual; abstract;
    // Creates the empty object. May read non-reference data from the
    // payload; references do not resolve yet.
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; virtual; abstract;
    // Reads the payload from its start.
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); virtual; abstract;
    procedure Finish(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); virtual;
  end;

  { The built-ins of a fresh engine, numbered by a deterministic walk and
    named by the first path that reached them. }
  TGocciaHeapSnapshotBuiltinTable = class
  private
    FObjects: TList<TGCManagedObject>;
    FPaths: TStringList;
    FIndex: THashMap<TGCManagedObject, Integer>;
    FByPath: TDictionary<string, Integer>;
    FFingerprints: array of UInt64;
    FFingerprintCount: Integer;
    FEmptySlots: TList<Integer>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    // False when AObject is already in the table.
    function Add(const AObject: TGCManagedObject; const APath: string): Boolean;
    function IndexOf(const AObject: TGCManagedObject): Integer;
    function FindPath(const APath: string): TGCManagedObject;
    function ObjectAt(const AIndex: Integer): TGCManagedObject;
    function PathAt(const AIndex: Integer): string;
    function ComputeFingerprint(const AIndex: Integer): UInt64;
    procedure RecordFingerprints;
    // The paths of entries whose own state no longer matches the
    // fingerprint taken when the table was built: built-ins the script
    // changed.
    function DirtyPaths: TStringList;
    property Count: Integer read GetCount;
    // Realm slots that were still empty when the table was built.
    property EmptySlots: TList<Integer> read FEmptySlots;
  end;

  TGocciaHeapSnapshotWriter = class
  private
    FBuiltins: TGocciaHeapSnapshotBuiltinTable;
    FModule: TGocciaBytecodeModule;
    FModuleScope: TGocciaScope;
    FTemplates: THashMap<TGocciaFunctionTemplate, Integer>;
    FIds: THashMap<TGCManagedObject, Integer>;
    FObjects: TList<TGCManagedObject>;
    FParents: TList<Integer>;
    FBuiltinRefs: TList<Integer>;
    FBuiltinRefIndex: THashMap<Integer, Integer>;
    FOut: TStream;
    FCurrentId: Integer;
    FLateSlots: TStringList;
    procedure CollectTemplates(const ATemplate: TGocciaFunctionTemplate);
    function GetObjectCount: Integer;
    function GetBuiltinRefCount: Integer;
    function ReferenceChain(const AId: Integer): string;
  public
    constructor Create(const ABuiltins: TGocciaHeapSnapshotBuiltinTable;
      const AModule: TGocciaBytecodeModule; const AModuleScope: TGocciaScope);
    destructor Destroy; override;

    procedure WriteU8(const AValue: Byte);
    procedure WriteU16(const AValue: Word);
    procedure WriteU32(const AValue: Cardinal);
    procedure WriteI32(const AValue: Integer);
    procedure WriteI64(const AValue: Int64);
    procedure WriteDouble(const AValue: Double);
    procedure WriteBool(const AValue: Boolean);
    procedure WriteStr(const AValue: string);
    procedure WriteValue(const AValue: TGocciaValue);
    // Any managed object: a value, a cell or the module scope.
    procedure WriteRef(const AObject: TGCManagedObject);
    procedure WriteRegister(const ARegister: TGocciaRegister);
    procedure WriteTemplate(const ATemplate: TGocciaFunctionTemplate);
    // Prototype, integrity, own string- and symbol-keyed properties.
    procedure WriteObjectCore(const AObject: TGocciaObjectValue);
    procedure Refuse(const AReason: string);

    // Writes the whole snapshot. ARoots are the pending host-call promises.
    procedure Serialize(const ARoots: array of TGocciaValue;
      const AStream: TStream);
    // Collects, then refuses when a live object is neither in ABaseline
    // (alive before the script ran), a built-in, nor serialized.
    procedure CheckAccounting(const ABaseline: TObject);
    function IsSerialized(const AObject: TGCManagedObject): Boolean;

    property ObjectCount: Integer read GetObjectCount;
    property BuiltinRefCount: Integer read GetBuiltinRefCount;
    // Realm slots created after the built-in table was built, in the order
    // they were added to it; a restore creates them first.
    property LateSlots: TStringList read FLateSlots;
  end;

  // Finds the built-in a path names, adding it to the reader's table; nil
  // when the path names nothing.
  TGocciaBuiltinPathResolve = function(const APath: string): TGCManagedObject
    of object;

  TGocciaHeapSnapshotReader = class
  private
    FBuiltins: TGocciaHeapSnapshotBuiltinTable;
    FModule: TGocciaBytecodeModule;
    FModuleScope: TGocciaScope;
    FTemplates: TList<TGocciaFunctionTemplate>;
    FBuiltinObjects: array of TGCManagedObject;
    FObjects: array of TGCManagedObject;
    FCodecs: array of TGocciaSnapshotCodec;
    FPayloadStart: array of Int64;
    FPayloadEnd: array of Int64;
    FIn: TStream;
    FPayloadLimit: Int64;
    FRoots: TObject;
    FBrandTokens: TDictionary<string, string>;
    FContext: TObject;
    FBuiltinResolve: TGocciaBuiltinPathResolve;
    procedure CollectTemplates(const ATemplate: TGocciaFunctionTemplate);
    function GetObjectCount: Integer;
    procedure Need(const ACount: Int64);
  public
    constructor Create(const ABuiltins: TGocciaHeapSnapshotBuiltinTable;
      const AModule: TGocciaBytecodeModule; const AModuleScope: TGocciaScope);
    destructor Destroy; override;

    function ReadU8: Byte;
    function ReadU16: Word;
    function ReadU32: Cardinal;
    function ReadI32: Integer;
    function ReadI64: Int64;
    function ReadDouble: Double;
    function ReadBool: Boolean;
    function ReadStr: string;
    function ReadValue: TGocciaValue;
    function ReadRef: TGCManagedObject;
    function ReadRegister: TGocciaRegister;
    function ReadTemplate: TGocciaFunctionTemplate;
    procedure ReadObjectCore(const AObject: TGocciaObjectValue);
    procedure Refuse(const AReason: string);
    // A private storage key with a snapshot class's brand token replaced by
    // the restored class's token.
    function MapPrivateKey(const AKey: string): string;
    procedure MapBrandToken(const AOld, ANew: string);

    // Restores the snapshot and returns its pending host-call promises.
    function Restore(const AStream: TStream): TArray<TGocciaValue>;

    property ObjectCount: Integer read GetObjectCount;
    // Engine-side context for codecs that need it (the VM).
    property Context: TObject read FContext write FContext;
    property BuiltinResolve: TGocciaBuiltinPathResolve
      read FBuiltinResolve write FBuiltinResolve;
  end;

procedure RegisterSnapshotCodec(const AClass: TClass;
  const ACodec: TGocciaSnapshotCodec);
function FindSnapshotCodecByClass(const AClass: TClass): TGocciaSnapshotCodec;
function FindSnapshotCodecByTag(const ATag: string): TGocciaSnapshotCodec;

// Builds a plain object's payload codec (exact class TGocciaObjectValue).
function ModuleBytecodeHash(const AModule: TGocciaBytecodeModule): string;
// Whether a snapshot can hold an object of this kind: it writes the value
// inline (undefined, null, booleans, holes, numbers, strings, BigInts) or a
// codec is registered for its class. Built-ins and the entry module's scope
// are named instead, whatever their class.
function SnapshotCanHold(const AObject: TObject): Boolean;

// Native functions whose callback is a method of their captured root are
// restored by naming the method: each unit registers the code addresses of
// the methods it hands out that way.
procedure RegisterSnapshotNativeCallback(const AName: string;
  const ACode: Pointer);

// A realm slot whose intrinsic is created on first use registers how to
// create it, so a restore can create the intrinsics a snapshot refers to.
type
  TGocciaSnapshotSlotMaterializer = procedure;
procedure RegisterSnapshotSlotMaterializer(const ASlotName: string;
  const AMaterializer: TGocciaSnapshotSlotMaterializer);
function FindSnapshotSlotMaterializer(
  const ASlotName: string): TGocciaSnapshotSlotMaterializer;

function SnapshotNativeCallbackName(const ACode: Pointer): string;
function SnapshotNativeCallbackCode(const AName: string): Pointer;

implementation

uses
  Math,

  BigInteger,
  SHA256,

  Goccia.Builtins.GlobalSymbol,
  Goccia.Bytecode.Binary,
  Goccia.HeapSnapshot.Census,
  Goccia.RegExp.Runtime,
  Goccia.Values.BigIntValue,
  Goccia.Values.HoleValue;

type
  TCodecRegistry = class
  public
    ByClass: TDictionary<TClass, TGocciaSnapshotCodec>;
    ByTag: TDictionary<string, TGocciaSnapshotCodec>;
    Owned: TObjectList<TGocciaSnapshotCodec>;
    constructor Create;
    destructor Destroy; override;
  end;

  // Roots every restored object until the restore ends.
  TRestoreRoots = class(TGCRootSource)
  public
    Reader: TGocciaHeapSnapshotReader;
    procedure MarkRootReferences; override;
  end;

  TPlainObjectCodec = class(TGocciaSnapshotCodec)
  public
    function Tag: string; override;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
  end;

  TSymbolCodec = class(TGocciaSnapshotCodec)
  public
    function Tag: string; override;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
  end;

var
  GRegistry: TCodecRegistry;
  GCallbackNames: TDictionary<Pointer, string>;
  GCallbackCodes: TDictionary<string, Pointer>;
  GSlotMaterializers: TDictionary<string, TGocciaSnapshotSlotMaterializer>;

procedure RegisterSnapshotSlotMaterializer(const ASlotName: string;
  const AMaterializer: TGocciaSnapshotSlotMaterializer);
begin
  if not Assigned(GSlotMaterializers) then
    GSlotMaterializers :=
      TDictionary<string, TGocciaSnapshotSlotMaterializer>.Create;
  GSlotMaterializers.AddOrSetValue(ASlotName, AMaterializer);
end;

function FindSnapshotSlotMaterializer(
  const ASlotName: string): TGocciaSnapshotSlotMaterializer;
begin
  Result := nil;
  if Assigned(GSlotMaterializers) then
    GSlotMaterializers.TryGetValue(ASlotName, Result);
end;

procedure RegisterSnapshotNativeCallback(const AName: string;
  const ACode: Pointer);
begin
  if not Assigned(GCallbackNames) then
  begin
    GCallbackNames := TDictionary<Pointer, string>.Create;
    GCallbackCodes := TDictionary<string, Pointer>.Create;
  end;
  GCallbackNames.AddOrSetValue(ACode, AName);
  GCallbackCodes.AddOrSetValue(AName, ACode);
end;

function SnapshotNativeCallbackName(const ACode: Pointer): string;
begin
  Result := '';
  if Assigned(GCallbackNames) then
    GCallbackNames.TryGetValue(ACode, Result);
end;

function SnapshotNativeCallbackCode(const AName: string): Pointer;
begin
  Result := nil;
  if Assigned(GCallbackCodes) then
    GCallbackCodes.TryGetValue(AName, Result);
end;

const
  PRIVATE_PREFIXES: array[0..2] of string =
    ('#slot:', '#brand:', '#initialized:');

{ TCodecRegistry }

constructor TCodecRegistry.Create;
begin
  inherited Create;
  ByClass := TDictionary<TClass, TGocciaSnapshotCodec>.Create;
  ByTag := TDictionary<string, TGocciaSnapshotCodec>.Create;
  Owned := TObjectList<TGocciaSnapshotCodec>.Create(True);
end;

destructor TCodecRegistry.Destroy;
begin
  ByClass.Free;
  ByTag.Free;
  Owned.Free;
  inherited;
end;

procedure RegisterSnapshotCodec(const AClass: TClass;
  const ACodec: TGocciaSnapshotCodec);
begin
  if not Assigned(GRegistry) then
    GRegistry := TCodecRegistry.Create;
  GRegistry.Owned.Add(ACodec);
  GRegistry.ByClass.AddOrSetValue(AClass, ACodec);
  GRegistry.ByTag.AddOrSetValue(ACodec.Tag, ACodec);
end;

function FindSnapshotCodecByClass(const AClass: TClass): TGocciaSnapshotCodec;
begin
  Result := nil;
  if Assigned(GRegistry) then
    GRegistry.ByClass.TryGetValue(AClass, Result);
end;

function FindSnapshotCodecByTag(const ATag: string): TGocciaSnapshotCodec;
begin
  Result := nil;
  if Assigned(GRegistry) then
    GRegistry.ByTag.TryGetValue(ATag, Result);
end;

function SnapshotCanHold(const AObject: TObject): Boolean;
begin
  Result := (AObject is TGocciaUndefinedLiteralValue) or
    (AObject is TGocciaNullLiteralValue) or
    (AObject is TGocciaBooleanLiteralValue) or (AObject is TGocciaHoleValue) or
    (AObject is TGocciaNumberLiteralValue) or
    (AObject is TGocciaStringLiteralValue) or (AObject is TGocciaBigIntValue) or
    Assigned(FindSnapshotCodecByClass(AObject.ClassType));
end;

function ModuleBytecodeHash(const AModule: TGocciaBytecodeModule): string;
var
  Writer: TGocciaBytecodeWriter;
  Stream: TMemoryStream;
  Bytes: TBytes;
begin
  Stream := TMemoryStream.Create;
  Writer := TGocciaBytecodeWriter.Create(Stream);
  try
    Writer.WriteModule(AModule);
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
      Move(Stream.Memory^, Bytes[0], Stream.Size);
    Result := SHA256Hex(Bytes);
  finally
    Writer.Free;
    Stream.Free;
  end;
end;

{ TGocciaSnapshotCodec }

procedure TGocciaSnapshotCodec.Finish(const AReader: TGocciaHeapSnapshotReader;
  const AObject: TGCManagedObject);
begin
end;

{ TRestoreRoots }

procedure TRestoreRoots.MarkRootReferences;
var
  I: Integer;
begin
  if not Assigned(Reader) then
    Exit;
  for I := 0 to High(Reader.FObjects) do
    if Assigned(Reader.FObjects[I]) then
      Reader.FObjects[I].MarkReferences;
end;

{ TGocciaHeapSnapshotBuiltinTable }

constructor TGocciaHeapSnapshotBuiltinTable.Create;
begin
  inherited Create;
  FObjects := TList<TGCManagedObject>.Create;
  FPaths := TStringList.Create;
  FIndex := THashMap<TGCManagedObject, Integer>.Create;
  FByPath := TDictionary<string, Integer>.Create;
  FEmptySlots := TList<Integer>.Create;
end;

destructor TGocciaHeapSnapshotBuiltinTable.Destroy;
begin
  FObjects.Free;
  FPaths.Free;
  FIndex.Free;
  FByPath.Free;
  FEmptySlots.Free;
  inherited;
end;

procedure TGocciaHeapSnapshotBuiltinTable.Clear;
begin
  FObjects.Clear;
  FPaths.Clear;
  FIndex.Clear;
  FByPath.Clear;
  FEmptySlots.Clear;
  SetLength(FFingerprints, 0);
  FFingerprintCount := 0;
end;

function TGocciaHeapSnapshotBuiltinTable.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TGocciaHeapSnapshotBuiltinTable.Add(const AObject: TGCManagedObject;
  const APath: string): Boolean;
begin
  Result := False;
  if not Assigned(AObject) or FIndex.ContainsKey(AObject) then
    Exit;
  FIndex.Add(AObject, FObjects.Count);
  FByPath.AddOrSetValue(APath, FObjects.Count);
  FObjects.Add(AObject);
  FPaths.Add(APath);
  Result := True;
end;

function TGocciaHeapSnapshotBuiltinTable.IndexOf(
  const AObject: TGCManagedObject): Integer;
begin
  if not FIndex.TryGetValue(AObject, Result) then
    Result := -1;
end;

function TGocciaHeapSnapshotBuiltinTable.FindPath(
  const APath: string): TGCManagedObject;
var
  Index: Integer;
begin
  if FByPath.TryGetValue(APath, Index) then
    Result := FObjects[Index]
  else
    Result := nil;
end;

function TGocciaHeapSnapshotBuiltinTable.ObjectAt(
  const AIndex: Integer): TGCManagedObject;
begin
  Result := FObjects[AIndex];
end;

function TGocciaHeapSnapshotBuiltinTable.PathAt(const AIndex: Integer): string;
begin
  Result := FPaths[AIndex];
end;

{$PUSH}{$Q-}{$R-}
function FNV1a(const AHash: UInt64; const AText: string): UInt64;
var
  I: Integer;
begin
  Result := AHash;
  for I := 1 to Length(AText) do
  begin
    Result := Result xor Ord(AText[I]);
    Result := Result * UInt64(1099511628211);
  end;
end;
{$POP}

function TGocciaHeapSnapshotBuiltinTable.ComputeFingerprint(
  const AIndex: Integer): UInt64;

  function Identity(const AValue: TGocciaValue): string;
  var
    Index: Integer;
  begin
    if not Assigned(AValue) then
      Exit('nil');
    Index := IndexOf(AValue);
    // An entry numbered after the fingerprints were taken was materialized
    // since: it hashes as the lazy member it replaced.
    if (Index >= FFingerprintCount) and (FFingerprintCount > 0) then
      Exit('L');
    if Index >= 0 then
      Exit('#' + IntToStr(Index));
    if AValue is TGocciaStringLiteralValue then
      Exit('s' + TGocciaStringLiteralValue(AValue).Value);
    if AValue is TGocciaNumberLiteralValue then
      Exit('n' + FloatToStr(TGocciaNumberLiteralValue(AValue).Value));
    if AValue.IsPrimitive then
      Exit('p' + AValue.ClassName);
    Result := 'X';
  end;

var
  Obj: TGocciaObjectValue;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Symbols: TArray<TGocciaSymbolValue>;
  Descriptor: TGocciaPropertyDescriptor;
  I: Integer;
  Text, Getter, Setter: string;
begin
  Result := UInt64(14695981039346656037);
  if not (FObjects[AIndex] is TGocciaObjectValue) then
    Exit;
  Obj := TGocciaObjectValue(FObjects[AIndex]);
  Text := 'p' + Identity(Obj.Prototype) + BoolToStr(Obj.Extensible, True);
  Result := FNV1a(Result, Text);
  for Pair in Obj.Properties do
  begin
    Descriptor := Pair.Value;
    Text := Pair.Key + '|';
    if Descriptor is TGocciaLazyPropertyDescriptorData then
      Text := Text + 'L'
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Getter := Identity(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
      Setter := Identity(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
      if ((Getter = 'L') or (Getter = 'nil')) and
         ((Setter = 'L') or (Setter = 'nil')) and (Getter + Setter <> 'nilnil') then
        Text := Text + 'L'
      else
        Text := Text + IntToStr(Byte(Descriptor.Flags)) + '|' + Getter + '/' +
          Setter;
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Getter := Identity(TGocciaPropertyDescriptorData(Descriptor).Value);
      if Getter = 'L' then
        Text := Text + 'L'
      else
        Text := Text + IntToStr(Byte(Descriptor.Flags)) + '|' + Getter;
    end;
    Result := FNV1a(Result, Text);
  end;
  Symbols := Obj.GetOwnSymbols;
  for I := 0 to High(Symbols) do
  begin
    Descriptor := Obj.GetOwnSymbolPropertyDescriptor(Symbols[I]);
    Text := Identity(Symbols[I]) + '|';
    if Descriptor is TGocciaPropertyDescriptorAccessor then
      Text := Text + Identity(TGocciaPropertyDescriptorAccessor(
        Descriptor).Getter) + '/' + Identity(
        TGocciaPropertyDescriptorAccessor(Descriptor).Setter)
    else if Descriptor is TGocciaPropertyDescriptorData then
      Text := Text + Identity(TGocciaPropertyDescriptorData(Descriptor).Value);
    Result := FNV1a(Result, Text);
  end;
end;

procedure TGocciaHeapSnapshotBuiltinTable.RecordFingerprints;
var
  I: Integer;
begin
  FFingerprintCount := 0;
  SetLength(FFingerprints, FObjects.Count);
  for I := 0 to FObjects.Count - 1 do
    FFingerprints[I] := ComputeFingerprint(I);
  FFingerprintCount := FObjects.Count;
end;

function TGocciaHeapSnapshotBuiltinTable.DirtyPaths: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Min(FObjects.Count, Length(FFingerprints)) - 1 do
    if ComputeFingerprint(I) <> FFingerprints[I] then
      Result.Add(FPaths[I]);
end;

{ TGocciaHeapSnapshotWriter }

constructor TGocciaHeapSnapshotWriter.Create(
  const ABuiltins: TGocciaHeapSnapshotBuiltinTable;
  const AModule: TGocciaBytecodeModule; const AModuleScope: TGocciaScope);
begin
  inherited Create;
  FBuiltins := ABuiltins;
  FModule := AModule;
  FModuleScope := AModuleScope;
  FTemplates := THashMap<TGocciaFunctionTemplate, Integer>.Create;
  FIds := THashMap<TGCManagedObject, Integer>.Create;
  FObjects := TList<TGCManagedObject>.Create;
  FParents := TList<Integer>.Create;
  FBuiltinRefs := TList<Integer>.Create;
  FBuiltinRefIndex := THashMap<Integer, Integer>.Create;
  FLateSlots := TStringList.Create;
  FCurrentId := -1;
  if Assigned(FModule) then
    CollectTemplates(FModule.TopLevel);
end;

destructor TGocciaHeapSnapshotWriter.Destroy;
begin
  FTemplates.Free;
  FIds.Free;
  FObjects.Free;
  FParents.Free;
  FBuiltinRefs.Free;
  FBuiltinRefIndex.Free;
  FLateSlots.Free;
  inherited;
end;

procedure TGocciaHeapSnapshotWriter.CollectTemplates(
  const ATemplate: TGocciaFunctionTemplate);
var
  I: Integer;
begin
  if not Assigned(ATemplate) then
    Exit;
  FTemplates.Add(ATemplate, FTemplates.Count);
  for I := 0 to ATemplate.FunctionCount - 1 do
    CollectTemplates(ATemplate.GetFunction(I));
end;

function TGocciaHeapSnapshotWriter.GetObjectCount: Integer;
begin
  Result := FObjects.Count;
end;

function TGocciaHeapSnapshotWriter.GetBuiltinRefCount: Integer;
begin
  Result := FBuiltinRefs.Count;
end;

function TGocciaHeapSnapshotWriter.IsSerialized(
  const AObject: TGCManagedObject): Boolean;
begin
  Result := FIds.ContainsKey(AObject);
end;

function TGocciaHeapSnapshotWriter.ReferenceChain(const AId: Integer): string;
var
  Id, Depth: Integer;
begin
  Result := '';
  Id := AId;
  Depth := 0;
  while (Id >= 0) and (Depth < 12) do
  begin
    if Result <> '' then
      Result := Result + ' <- ';
    Result := Result + FObjects[Id].ClassName + '#' + IntToStr(Id);
    Id := FParents[Id];
    Inc(Depth);
  end;
  if Result = '' then
    Result := '(root)';
end;

procedure TGocciaHeapSnapshotWriter.Refuse(const AReason: string);
begin
  if FCurrentId >= 0 then
    raise EGocciaHeapSnapshotRefused.Create(AReason + ' (reached through ' +
      ReferenceChain(FCurrentId) + ')')
  else
    raise EGocciaHeapSnapshotRefused.Create(AReason);
end;

procedure TGocciaHeapSnapshotWriter.WriteU8(const AValue: Byte);
begin
  FOut.WriteBuffer(AValue, 1);
end;

procedure TGocciaHeapSnapshotWriter.WriteU16(const AValue: Word);
var
  V: Word;
begin
  V := NtoLE(AValue);
  FOut.WriteBuffer(V, 2);
end;

procedure TGocciaHeapSnapshotWriter.WriteU32(const AValue: Cardinal);
var
  V: Cardinal;
begin
  V := NtoLE(AValue);
  FOut.WriteBuffer(V, 4);
end;

procedure TGocciaHeapSnapshotWriter.WriteI32(const AValue: Integer);
begin
  WriteU32(Cardinal(AValue));
end;

procedure TGocciaHeapSnapshotWriter.WriteI64(const AValue: Int64);
var
  V: Int64;
begin
  V := NtoLE(AValue);
  FOut.WriteBuffer(V, 8);
end;

procedure TGocciaHeapSnapshotWriter.WriteDouble(const AValue: Double);
var
  Bits: Int64;
begin
  Move(AValue, Bits, 8);
  WriteI64(Bits);
end;

procedure TGocciaHeapSnapshotWriter.WriteBool(const AValue: Boolean);
begin
  WriteU8(Ord(AValue));
end;

procedure TGocciaHeapSnapshotWriter.WriteStr(const AValue: string);
var
  Bytes: UTF8String;
begin
  Bytes := UTF8String(AValue);
  WriteU32(Length(Bytes));
  if Length(Bytes) > 0 then
    FOut.WriteBuffer(Bytes[1], Length(Bytes));
end;

procedure TGocciaHeapSnapshotWriter.WriteRef(const AObject: TGCManagedObject);
var
  Id, BuiltinIndex, RefIndex: Integer;
  Value: TGocciaValue;
begin
  if not Assigned(AObject) then
  begin
    WriteU8(HSV_NIL);
    Exit;
  end;
  if AObject is TGocciaValue then
  begin
    Value := TGocciaValue(AObject);
    if Value is TGocciaUndefinedLiteralValue then
    begin
      WriteU8(HSV_UNDEFINED);
      Exit;
    end;
    if Value is TGocciaNullLiteralValue then
    begin
      WriteU8(HSV_NULL);
      Exit;
    end;
    if Value is TGocciaBooleanLiteralValue then
    begin
      if TGocciaBooleanLiteralValue(Value).Value then
        WriteU8(HSV_TRUE)
      else
        WriteU8(HSV_FALSE);
      Exit;
    end;
    if Value is TGocciaHoleValue then
    begin
      WriteU8(HSV_HOLE);
      Exit;
    end;
    if Value is TGocciaNumberLiteralValue then
    begin
      WriteU8(HSV_NUMBER);
      WriteDouble(TGocciaNumberLiteralValue(Value).Value);
      Exit;
    end;
    if Value is TGocciaStringLiteralValue then
    begin
      WriteU8(HSV_STRING);
      WriteStr(TGocciaStringLiteralValue(Value).Value);
      Exit;
    end;
    if Value is TGocciaBigIntValue then
    begin
      WriteU8(HSV_BIGINT);
      WriteStr(TGocciaBigIntValue(Value).Value.ToString);
      Exit;
    end;
  end;
  BuiltinIndex := FBuiltins.IndexOf(AObject);
  if BuiltinIndex >= 0 then
  begin
    if not FBuiltinRefIndex.TryGetValue(BuiltinIndex, RefIndex) then
    begin
      RefIndex := FBuiltinRefs.Count;
      FBuiltinRefs.Add(BuiltinIndex);
      FBuiltinRefIndex.Add(BuiltinIndex, RefIndex);
    end;
    WriteU8(HSV_BUILTIN);
    WriteU32(RefIndex);
    Exit;
  end;
  if AObject = FModuleScope then
  begin
    WriteU8(HSV_MODULE_SCOPE);
    Exit;
  end;
  if (AObject is TGocciaSymbolValue) and
     TGocciaSymbolValue(AObject).Registered then
  begin
    WriteU8(HSV_REGISTERED_SYMBOL);
    WriteStr(TGocciaSymbolValue(AObject).Description);
    Exit;
  end;
  if not FIds.TryGetValue(AObject, Id) then
  begin
    Id := FObjects.Count;
    FIds.Add(AObject, Id);
    FObjects.Add(AObject);
    FParents.Add(FCurrentId);
  end;
  WriteU8(HSV_OBJECT);
  WriteU32(Id);
end;

procedure TGocciaHeapSnapshotWriter.WriteValue(const AValue: TGocciaValue);
begin
  WriteRef(AValue);
end;

procedure TGocciaHeapSnapshotWriter.WriteRegister(
  const ARegister: TGocciaRegister);
begin
  WriteU8(Ord(ARegister.Kind));
  case ARegister.Kind of
    grkBoolean:
      WriteBool(ARegister.BoolValue);
    grkInt:
      WriteI64(ARegister.IntValue);
    grkFloat:
      WriteDouble(ARegister.FloatValue);
    grkObject:
      WriteValue(ARegister.ObjectValue);
  end;
end;

procedure TGocciaHeapSnapshotWriter.WriteTemplate(
  const ATemplate: TGocciaFunctionTemplate);
var
  Index: Integer;
begin
  if not FTemplates.TryGetValue(ATemplate, Index) then
    Refuse('function template "' + ATemplate.Name +
      '" is not from the entry module');
  WriteU32(Index);
end;

procedure TGocciaHeapSnapshotWriter.WriteObjectCore(
  const AObject: TGocciaObjectValue);
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Count: Integer;
  Descriptor: TGocciaPropertyDescriptor;
  Symbols: TArray<TGocciaSymbolValue>;
  I: Integer;
begin
  // An error keeps its captured stack text; a RegExp keeps its source and
  // flags, and a restore compiles the program again.
  WriteBool(AObject.HasErrorData);
  if AObject.HasErrorData then
    WriteStr(AObject.ErrorStack);
  WriteBool(AObject.HasRegExpData);
  if AObject.HasRegExpData then
  begin
    WriteStr(GetRegExpInternalSource(AObject));
    WriteStr(GetRegExpInternalFlags(AObject));
  end;
  WriteValue(AObject.Prototype);
  WriteBool(AObject.Extensible);
  WriteBool(AObject.Frozen);
  WriteBool(AObject.Sealed);
  WriteBool(AObject.HasEverHadIndexedOwnProperty);
  Count := 0;
  for Pair in AObject.Properties do
    Inc(Count);
  WriteU32(Count);
  for Pair in AObject.Properties do
  begin
    Descriptor := Pair.Value;
    WriteStr(Pair.Key);
    WriteU8(Byte(Descriptor.Flags));
    if Descriptor is TGocciaLazyPropertyDescriptorData then
      Refuse('unmaterialized lazy property "' + Pair.Key + '"')
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      WriteU8(1);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      WriteU8(0);
      WriteValue(TGocciaPropertyDescriptorData(Descriptor).Value);
    end
    else
      Refuse('property descriptor class ' + Descriptor.ClassName);
  end;
  Symbols := AObject.GetOwnSymbols;
  WriteU32(Length(Symbols));
  for I := 0 to High(Symbols) do
  begin
    Descriptor := AObject.GetOwnSymbolPropertyDescriptor(Symbols[I]);
    WriteValue(Symbols[I]);
    WriteU8(Byte(Descriptor.Flags));
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      WriteU8(1);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
    end
    else
    begin
      WriteU8(0);
      WriteValue(TGocciaPropertyDescriptorData(Descriptor).Value);
    end;
  end;
end;

procedure TGocciaHeapSnapshotWriter.Serialize(
  const ARoots: array of TGocciaValue; const AStream: TStream);
var
  Body, Payload, Main: TMemoryStream;
  Names: TGocciaStringArray;
  Binding: TLexicalBinding;
  I, Index: Integer;
  Codec: TGocciaSnapshotCodec;
  Obj: TGCManagedObject;
  Dirty: TStringList;
begin
  Dirty := FBuiltins.DirtyPaths;
  try
    if Dirty.Count > 0 then
      Refuse('the script changed built-ins: ' + Dirty.CommaText);
  finally
    Dirty.Free;
  end;

  Main := TMemoryStream.Create;
  Body := TMemoryStream.Create;
  Payload := TMemoryStream.Create;
  try
    // Roots first: they number the objects they reach.
    FOut := Body;
    Names := FModuleScope.GetOwnBindingNames;
    WriteU32(Length(Names));
    for I := 0 to High(Names) do
    begin
      if not FModuleScope.TryGetOwnLexicalBindingRaw(Names[I], Binding) then
        Refuse('module binding "' + Names[I] + '" vanished');
      WriteStr(Names[I]);
      WriteU8(Ord(Binding.DeclarationType));
      WriteBool(Binding.Initialized);
      if Binding.Initialized then
        WriteValue(Binding.Value)
      else
        WriteValue(nil);
    end;
    if Length(FModuleScope.GetOwnVarBindingNames) > 0 then
      Refuse('module var bindings are not supported yet');
    WriteU32(Length(ARoots));
    for I := 0 to High(ARoots) do
      WriteValue(ARoots[I]);

    // Objects, breadth first: writing one numbers the ones it reaches.
    Index := 0;
    FOut := Payload;
    while Index < FObjects.Count do
    begin
      Obj := FObjects[Index];
      FCurrentId := Index;
      Codec := FindSnapshotCodecByClass(Obj.ClassType);
      if not Assigned(Codec) then
        Refuse('no snapshot codec for ' + Obj.ClassName);
      Payload.Clear;
      Codec.Write(Self, Obj);
      FOut := Main;
      WriteStr(Codec.Tag);
      WriteU32(Payload.Size);
      Main.CopyFrom(Payload, 0);
      FOut := Payload;
      Inc(Index);
    end;
    FCurrentId := -1;

    // Header, built-in names, objects, roots.
    FOut := AStream;
    AStream.WriteBuffer(HEAP_SNAPSHOT_MAGIC[1], 4);
    WriteU16(HEAP_SNAPSHOT_FORMAT_VERSION);
    WriteStr(ModuleBytecodeHash(FModule));
    WriteU32(FLateSlots.Count);
    for I := 0 to FLateSlots.Count - 1 do
      WriteStr(FLateSlots[I]);
    WriteU32(FBuiltinRefs.Count);
    for I := 0 to FBuiltinRefs.Count - 1 do
      WriteStr(FBuiltins.PathAt(FBuiltinRefs[I]));
    WriteU32(FObjects.Count);
    AStream.CopyFrom(Main, 0);
    AStream.CopyFrom(Body, 0);
  finally
    FOut := nil;
    Main.Free;
    Body.Free;
    Payload.Free;
  end;
end;

function DescribeObject(const AObject: TGCManagedObject): string;
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Count: Integer;
begin
  Result := AObject.ClassName;
  if TGarbageCollector.Instance.IsTempRoot(AObject) then
    Result := Result + '(temp-root)';
  if AObject is TGocciaObjectValue then
  begin
    Result := Result + '{';
    Count := 0;
    for Pair in TGocciaObjectValue(AObject).Properties do
    begin
      if Count > 0 then
        Result := Result + ' ';
      Result := Result + Pair.Key;
      Inc(Count);
      if Count >= 6 then
        Break;
    end;
    Result := Result + '}';
  end;
end;

procedure TGocciaHeapSnapshotWriter.CheckAccounting(const ABaseline: TObject);
var
  GC: TGarbageCollector;
  I: Integer;
  Obj: TGCManagedObject;
  Missing: TStringList;
  Index: Integer;
  Text: string;
  Details: TStringList;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  Details := TStringList.Create;
  Missing := TStringList.Create;
  try
    Missing.Sorted := True;
    Missing.Duplicates := dupIgnore;
    for I := 0 to GC.ManagedObjectCount - 1 do
    begin
      Obj := GC.ManagedObjects[I];
      if not Assigned(Obj) then
        Continue;
      if (Obj is TGocciaStringLiteralValue) or
         (Obj is TGocciaNumberLiteralValue) or
         (Obj is TGocciaBooleanLiteralValue) or
         (Obj is TGocciaBigIntValue) then
        Continue;
      if Assigned(ABaseline) and
         TGocciaHeapBaseline(ABaseline).Contains(Obj) then
        Continue;
      if FIds.ContainsKey(Obj) or (FBuiltins.IndexOf(Obj) >= 0) or
         (Obj = FModuleScope) then
        Continue;
      if (Obj is TGocciaSymbolValue) and
         TGocciaSymbolValue(Obj).Registered then
        Continue;
      if Details.Count < 8 then
        Details.Add(DescribeObject(Obj));
      Index := Missing.IndexOf(Obj.ClassName);
      if Index < 0 then
        Missing.AddObject(Obj.ClassName, TObject(PtrInt(1)))
      else
        Missing.Objects[Index] := TObject(PtrInt(Missing.Objects[Index]) + 1);
    end;
    if Missing.Count > 0 then
    begin
      Text := '';
      for I := 0 to Missing.Count - 1 do
      begin
        if Text <> '' then
          Text := Text + ', ';
        Text := Text + Missing[I] + '=' +
          IntToStr(PtrInt(Missing.Objects[I]));
      end;
      raise EGocciaHeapSnapshotRefused.Create(
        'live objects the snapshot does not hold: ' + Text + ' [' +
        Details.CommaText + ']');
    end;
  finally
    Details.Free;
    Missing.Free;
  end;
end;

{ TGocciaHeapSnapshotReader }

constructor TGocciaHeapSnapshotReader.Create(
  const ABuiltins: TGocciaHeapSnapshotBuiltinTable;
  const AModule: TGocciaBytecodeModule; const AModuleScope: TGocciaScope);
begin
  inherited Create;
  FBuiltins := ABuiltins;
  FModule := AModule;
  FModuleScope := AModuleScope;
  FTemplates := TList<TGocciaFunctionTemplate>.Create;
  FBrandTokens := TDictionary<string, string>.Create;
  if Assigned(FModule) then
    CollectTemplates(FModule.TopLevel);
end;

destructor TGocciaHeapSnapshotReader.Destroy;
begin
  FRoots.Free;
  FTemplates.Free;
  FBrandTokens.Free;
  inherited;
end;

procedure TGocciaHeapSnapshotReader.CollectTemplates(
  const ATemplate: TGocciaFunctionTemplate);
var
  I: Integer;
begin
  if not Assigned(ATemplate) then
    Exit;
  FTemplates.Add(ATemplate);
  for I := 0 to ATemplate.FunctionCount - 1 do
    CollectTemplates(ATemplate.GetFunction(I));
end;

function TGocciaHeapSnapshotReader.GetObjectCount: Integer;
begin
  Result := Length(FObjects);
end;

procedure TGocciaHeapSnapshotReader.Refuse(const AReason: string);
begin
  raise EGocciaHeapSnapshotRefused.Create('restore: ' + AReason);
end;

procedure TGocciaHeapSnapshotReader.Need(const ACount: Int64);
begin
  if (ACount < 0) or (FIn.Position + ACount > FPayloadLimit) then
    Refuse('truncated snapshot');
end;

function TGocciaHeapSnapshotReader.ReadU8: Byte;
begin
  Need(1);
  FIn.ReadBuffer(Result, 1);
end;

function TGocciaHeapSnapshotReader.ReadU16: Word;
begin
  Need(2);
  FIn.ReadBuffer(Result, 2);
  Result := LEtoN(Result);
end;

function TGocciaHeapSnapshotReader.ReadU32: Cardinal;
begin
  Need(4);
  FIn.ReadBuffer(Result, 4);
  Result := LEtoN(Result);
end;

function TGocciaHeapSnapshotReader.ReadI32: Integer;
begin
  Result := Integer(ReadU32);
end;

function TGocciaHeapSnapshotReader.ReadI64: Int64;
begin
  Need(8);
  FIn.ReadBuffer(Result, 8);
  Result := LEtoN(Result);
end;

function TGocciaHeapSnapshotReader.ReadDouble: Double;
var
  Bits: Int64;
begin
  Bits := ReadI64;
  Move(Bits, Result, 8);
end;

function TGocciaHeapSnapshotReader.ReadBool: Boolean;
var
  B: Byte;
begin
  B := ReadU8;
  if B > 1 then
    Refuse('bad boolean');
  Result := B = 1;
end;

function TGocciaHeapSnapshotReader.ReadStr: string;
var
  Len: Cardinal;
  Bytes: UTF8String;
begin
  Len := ReadU32;
  Need(Len);
  SetLength(Bytes, Len);
  if Len > 0 then
    FIn.ReadBuffer(Bytes[1], Len);
  Result := string(Bytes);
end;

function TGocciaHeapSnapshotReader.ReadRef: TGCManagedObject;
var
  Tag: Byte;
  Index: Cardinal;
begin
  Tag := ReadU8;
  case Tag of
    HSV_NIL:
      Result := nil;
    HSV_UNDEFINED:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    HSV_NULL:
      Result := TGocciaNullLiteralValue.NullValue;
    HSV_TRUE:
      Result := TGocciaBooleanLiteralValue.TrueValue;
    HSV_FALSE:
      Result := TGocciaBooleanLiteralValue.FalseValue;
    HSV_HOLE:
      Result := TGocciaHoleValue.HoleValue;
    HSV_NUMBER:
      Result := TGocciaNumberLiteralValue.Create(ReadDouble);
    HSV_STRING:
      Result := TGocciaStringLiteralValue.Create(ReadStr);
    HSV_BIGINT:
      Result := TGocciaBigIntValue.Create(TBigInteger.FromDecimalString(ReadStr));
    HSV_OBJECT:
      begin
        Index := ReadU32;
        if Index >= Cardinal(Length(FObjects)) then
          Refuse('object reference out of range');
        Result := FObjects[Index];
      end;
    HSV_BUILTIN:
      begin
        Index := ReadU32;
        if Index >= Cardinal(Length(FBuiltinObjects)) then
          Refuse('built-in reference out of range');
        Result := FBuiltinObjects[Index];
      end;
    HSV_MODULE_SCOPE:
      Result := FModuleScope;
    HSV_REGISTERED_SYMBOL:
      Result := SharedRegisteredSymbol(ReadStr);
  else
    Refuse('bad value tag ' + IntToStr(Tag));
    Result := nil;
  end;
end;

function TGocciaHeapSnapshotReader.ReadValue: TGocciaValue;
var
  Obj: TGCManagedObject;
begin
  Obj := ReadRef;
  if Assigned(Obj) and not (Obj is TGocciaValue) then
    Refuse('expected a value, found ' + Obj.ClassName);
  Result := TGocciaValue(Obj);
end;

function TGocciaHeapSnapshotReader.ReadRegister: TGocciaRegister;
var
  Kind: Byte;
begin
  Kind := ReadU8;
  if Kind > Ord(High(TGocciaRegisterKind)) then
    Refuse('bad register kind');
  Result := Default(TGocciaRegister);
  Result.Kind := TGocciaRegisterKind(Kind);
  case Result.Kind of
    grkBoolean:
      Result.BoolValue := ReadBool;
    grkInt:
      Result.IntValue := ReadI64;
    grkFloat:
      Result.FloatValue := ReadDouble;
    grkObject:
      Result.ObjectValue := ReadValue;
  end;
end;

function TGocciaHeapSnapshotReader.ReadTemplate: TGocciaFunctionTemplate;
var
  Index: Cardinal;
begin
  Index := ReadU32;
  if Index >= Cardinal(FTemplates.Count) then
    Refuse('template index out of range');
  Result := FTemplates[Index];
end;

function TGocciaHeapSnapshotReader.MapPrivateKey(const AKey: string): string;
var
  I, Colon: Integer;
  Prefix, Rest, Token, NewToken: string;
begin
  Result := AKey;
  if (AKey = '') or (AKey[1] <> '#') then
    Exit;
  for I := 0 to High(PRIVATE_PREFIXES) do
  begin
    Prefix := PRIVATE_PREFIXES[I];
    if Copy(AKey, 1, Length(Prefix)) <> Prefix then
      Continue;
    Rest := Copy(AKey, Length(Prefix) + 1, MaxInt);
    Colon := Pos(':', Rest);
    if Colon > 0 then
      Token := Copy(Rest, 1, Colon - 1)
    else
      Token := Rest;
    if FBrandTokens.TryGetValue(Token, NewToken) then
    begin
      if Colon > 0 then
        Result := Prefix + NewToken + ':' +
          MapPrivateKey(Copy(Rest, Colon + 1, MaxInt))
      else
        Result := Prefix + NewToken;
    end;
    Exit;
  end;
end;

procedure TGocciaHeapSnapshotReader.MapBrandToken(const AOld, ANew: string);
begin
  FBrandTokens.AddOrSetValue(AOld, ANew);
end;

procedure TGocciaHeapSnapshotReader.ReadObjectCore(
  const AObject: TGocciaObjectValue);
var
  Proto: TGocciaValue;
  Extensible, Frozen, Sealed, HadIndexed: Boolean;
  Count, I: Cardinal;
  Key: string;
  Flags: TPropertyFlags;
  Kind: Byte;
  Getter, Setter, Value: TGocciaValue;
  Existing: TGocciaPropertyDescriptor;
  Symbol: TGocciaValue;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  RegExpSource, RegExpFlags: string;
begin
  // Drop what the allocating constructor defined; the snapshot has the
  // object's own properties.
  for Pair in AObject.Properties do
    Pair.Value.Free;
  AObject.Properties.Clear;
  if ReadBool then
  begin
    AObject.HasErrorData := True;
    AObject.ErrorStack := ReadStr;
  end;
  if ReadBool then
  begin
    RegExpSource := ReadStr;
    RegExpFlags := ReadStr;
    AObject.HasRegExpData := True;
    try
      ReinitializeRegExpObject(AObject, RegExpSource, RegExpFlags);
    except
      on E: Exception do
        Refuse('a RegExp does not compile: ' + E.Message);
    end;
  end;
  Proto := ReadValue;
  if Assigned(Proto) and not (Proto is TGocciaObjectValue) then
    Refuse('prototype is not an object');
  AObject.Prototype := TGocciaObjectValue(Proto);
  Extensible := ReadBool;
  Frozen := ReadBool;
  Sealed := ReadBool;
  HadIndexed := ReadBool;
  Count := ReadU32;
  for I := 1 to Count do
  begin
    Key := MapPrivateKey(ReadStr);
    Flags := TPropertyFlags(ReadU8);
    Kind := ReadU8;
    if AObject.Properties.TryGetValue(Key, Existing) then
      Existing.Free;
    if Kind = 1 then
    begin
      Getter := ReadValue;
      Setter := ReadValue;
      AObject.Properties.Add(Key,
        TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, Flags));
    end
    else if Kind = 0 then
    begin
      Value := ReadValue;
      AObject.Properties.Add(Key,
        TGocciaPropertyDescriptorData.Create(Value, Flags));
    end
    else
      Refuse('bad descriptor kind');
  end;
  Count := ReadU32;
  for I := 1 to Count do
  begin
    Symbol := ReadValue;
    if not (Symbol is TGocciaSymbolValue) then
      Refuse('symbol key is not a symbol');
    Flags := TPropertyFlags(ReadU8);
    Kind := ReadU8;
    if Kind = 1 then
    begin
      Getter := ReadValue;
      Setter := ReadValue;
      AObject.DefineSymbolPropertyRaw(TGocciaSymbolValue(Symbol),
        TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, Flags));
    end
    else if Kind = 0 then
    begin
      Value := ReadValue;
      AObject.DefineSymbolPropertyRaw(TGocciaSymbolValue(Symbol),
        TGocciaPropertyDescriptorData.Create(Value, Flags));
    end
    else
      Refuse('bad descriptor kind');
  end;
  AObject.SetIntegrityRaw(Extensible, Frozen, Sealed, HadIndexed);
end;

function TGocciaHeapSnapshotReader.Restore(
  const AStream: TStream): TArray<TGocciaValue>;
var
  Magic: array[0..3] of AnsiChar;
  Version: Word;
  Hash: string;
  Count: Cardinal;
  I: Integer;
  Path, Tag: string;
  PayloadLength: Cardinal;
  Roots: TRestoreRoots;
  Name: string;
  DeclType: Byte;
  Initialized: Boolean;
  Value: TGocciaValue;
  Binding: TLexicalBinding;
  Pending: TList<TGocciaValue>;
  BodyStart: Int64;
begin
  FIn := AStream;
  FPayloadLimit := AStream.Size;
  Need(4);
  AStream.ReadBuffer(Magic, 4);
  if Magic <> HEAP_SNAPSHOT_MAGIC then
    Refuse('not a heap snapshot');
  Version := ReadU16;
  if Version <> HEAP_SNAPSHOT_FORMAT_VERSION then
    Refuse('format version ' + IntToStr(Version));
  Hash := ReadStr;
  if Hash <> ModuleBytecodeHash(FModule) then
    Refuse('the recompiled module differs from the snapshot''s');

  Count := ReadU32;
  if Count > 4096 then
    Refuse('too many late intrinsics');
  // Informational: the late realm slots resolve through their paths.
  for I := 1 to Count do
    ReadStr;

  Count := ReadU32;
  if Count > (AStream.Size - AStream.Position) div 4 then
    Refuse('more built-in references than the snapshot holds');
  SetLength(FBuiltinObjects, Count);
  for I := 0 to Integer(Count) - 1 do
  begin
    Path := ReadStr;
    if Assigned(FBuiltinResolve) then
      FBuiltinObjects[I] := FBuiltinResolve(Path)
    else
      FBuiltinObjects[I] := FBuiltins.FindPath(Path);
    if not Assigned(FBuiltinObjects[I]) then
      Refuse('unknown built-in ' + Path);
  end;

  Count := ReadU32;
  if Count > (AStream.Size - AStream.Position) div 5 then
    Refuse('object count exceeds the snapshot size');
  SetLength(FObjects, Count);
  SetLength(FCodecs, Count);
  SetLength(FPayloadStart, Count);
  SetLength(FPayloadEnd, Count);

  Roots := TRestoreRoots.Create;
  Roots.Reader := Self;
  FRoots := Roots;

  // Pass 1: allocate.
  for I := 0 to Integer(Count) - 1 do
  begin
    FPayloadLimit := AStream.Size;
    Tag := ReadStr;
    FCodecs[I] := FindSnapshotCodecByTag(Tag);
    if not Assigned(FCodecs[I]) then
      Refuse('unknown object kind ' + Tag);
    PayloadLength := ReadU32;
    Need(PayloadLength);
    FPayloadStart[I] := AStream.Position;
    FPayloadEnd[I] := AStream.Position + PayloadLength;
    FPayloadLimit := FPayloadEnd[I];
    FObjects[I] := FCodecs[I].Allocate(Self);
    if not Assigned(FObjects[I]) then
      Refuse('codec ' + Tag + ' allocated nothing');
    AStream.Position := FPayloadEnd[I];
  end;
  BodyStart := AStream.Position;

  // Pass 2: fill.
  for I := 0 to Integer(Count) - 1 do
  begin
    AStream.Position := FPayloadStart[I];
    FPayloadLimit := FPayloadEnd[I];
    FCodecs[I].Fill(Self, FObjects[I]);
    if AStream.Position <> FPayloadEnd[I] then
      Refuse('payload of ' + FCodecs[I].Tag + ' not fully read');
  end;

  // Pass 3: finish.
  for I := 0 to Integer(Count) - 1 do
  begin
    AStream.Position := FPayloadStart[I];
    FPayloadLimit := FPayloadEnd[I];
    FCodecs[I].Finish(Self, FObjects[I]);
  end;

  // Roots.
  AStream.Position := BodyStart;
  FPayloadLimit := AStream.Size;
  Count := ReadU32;
  for I := 1 to Count do
  begin
    Name := ReadStr;
    DeclType := ReadU8;
    Initialized := ReadBool;
    Value := ReadValue;
    if not FModuleScope.TryGetOwnLexicalBindingRaw(Name, Binding) then
      Refuse('the recompiled module has no binding "' + Name + '"');
    if DeclType <> Ord(Binding.DeclarationType) then
      Refuse('binding "' + Name + '" changed its declaration kind');
    Binding.Initialized := Initialized;
    Binding.Value := Value;
    FModuleScope.SetOwnLexicalBindingRaw(Name, Binding);
  end;
  Pending := TList<TGocciaValue>.Create;
  try
    Count := ReadU32;
    for I := 1 to Count do
      Pending.Add(ReadValue);
    Result := Pending.ToArray;
  finally
    Pending.Free;
  end;
  if AStream.Position <> AStream.Size then
    Refuse('trailing bytes');
end;

{ TPlainObjectCodec }

function TPlainObjectCodec.Tag: string;
begin
  Result := 'object';
end;

procedure TPlainObjectCodec.Write(const AWriter: TGocciaHeapSnapshotWriter;
  const AObject: TGCManagedObject);
begin
  AWriter.WriteObjectCore(TGocciaObjectValue(AObject));
end;

function TPlainObjectCodec.Allocate(
  const AReader: TGocciaHeapSnapshotReader): TGCManagedObject;
begin
  Result := TGocciaObjectValue.Create;
end;

procedure TPlainObjectCodec.Fill(const AReader: TGocciaHeapSnapshotReader;
  const AObject: TGCManagedObject);
begin
  AReader.ReadObjectCore(TGocciaObjectValue(AObject));
end;

{ TSymbolCodec }

function TSymbolCodec.Tag: string;
begin
  Result := 'symbol';
end;

procedure TSymbolCodec.Write(const AWriter: TGocciaHeapSnapshotWriter;
  const AObject: TGCManagedObject);
begin
  AWriter.WriteStr(TGocciaSymbolValue(AObject).Description);
  AWriter.WriteBool(TGocciaSymbolValue(AObject).HasDescription);
end;

function TSymbolCodec.Allocate(
  const AReader: TGocciaHeapSnapshotReader): TGCManagedObject;
var
  Description: string;
begin
  Description := AReader.ReadStr;
  Result := TGocciaSymbolValue.Create(Description, AReader.ReadBool);
end;

procedure TSymbolCodec.Fill(const AReader: TGocciaHeapSnapshotReader;
  const AObject: TGCManagedObject);
begin
  AReader.ReadStr;
  AReader.ReadBool;
end;

initialization
  RegisterSnapshotCodec(TGocciaObjectValue, TPlainObjectCodec.Create);
  RegisterSnapshotCodec(TGocciaSymbolValue, TSymbolCodec.Create);

finalization
  FreeAndNil(GRegistry);
  FreeAndNil(GCallbackNames);
  FreeAndNil(GCallbackCodes);
  FreeAndNil(GSlotMaterializers);

end.
