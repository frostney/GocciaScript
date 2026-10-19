unit Goccia.HeapSnapshot.Codec;

{ Heap snapshots (ADR 0148): the format, the codec registry, the writer and
  the reader.

  A snapshot is a structured object graph. Its header names the engine build
  and every codec it uses with that codec's schema version; a string table
  follows, then the names of the built-ins it refers to, the module records'
  headers, the compiled modules its functions run, the object table, the
  module records' payloads and the roots. Primitives are written inline, strings by their index in the
  string table, built-ins by name and every other object by its index in the
  object table.

  Each class is written by the codec it registers (RegisterSnapshotCodec), by
  exact class: a subclass is not covered by its parent's codec. A live object
  whose class has no codec, or in a state its codec does not handle, refuses
  the snapshot (EGocciaHeapSnapshotRefused), and the embedder resumes by
  replay.

  The writer numbers objects breadth first: writing one object's payload
  numbers the objects it reaches, and they are written in turn. The reader
  restores in passes over the object table: Allocate creates every object as
  an empty shell, Fill reads each payload (a reference resolves to a shell),
  and Finish fixes up what needs other objects filled first. Neither recurses
  once per object.

  The reader treats its input as hostile (ADR 0103): every count, index and
  length is checked against the input and against TGocciaHeapSnapshotLimits
  with overflow-safe arithmetic, temporary tables are charged to the run's
  memory ceiling before they are allocated, every loop whose length follows
  the input polls the run's execution limits (CheckNativeWork), and anything
  malformed refuses the restore (EGocciaHeapSnapshotInvalid) rather than
  being guessed at. }

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
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue,
  Goccia.VM.Registers;

const
  HEAP_SNAPSHOT_MAGIC: array[0..3] of AnsiChar = ('G', 'H', 'S', 'N');
  HEAP_SNAPSHOT_FORMAT_VERSION = 3;

  // Separates the segments of a built-in's name; no property key or symbol
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
  HSV_REGISTERED_SYMBOL = 11;
  HSV_LAST = HSV_REGISTERED_SYMBOL;

  // Descriptor kinds in an object's property list.
  HSD_DATA = 0;
  HSD_ACCESSOR = 1;

type
  { The snapshot cannot be taken: a class without a codec, a state a codec
    does not handle, a changed built-in. A normal outcome: the embedder
    resumes by replay. }
  EGocciaHeapSnapshotRefused = class(Exception);

  { The snapshot cannot be restored: malformed, over a limit, written by
    another engine build or codec schema, or naming a built-in this engine
    does not have. Also a normal outcome for the embedder. }
  EGocciaHeapSnapshotInvalid = class(EGocciaHeapSnapshotRefused);

  { Bounds on what a reader accepts, checked before anything is allocated
    for it. }
  TGocciaHeapSnapshotLimits = record
    MaxInputBytes: Int64;
    MaxObjects: Integer;
    MaxStrings: Integer;
    MaxStringBytes: Integer;
    MaxBuiltins: Integer;
    MaxCodecs: Integer;
    MaxRoots: Integer;
    class function Defaults: TGocciaHeapSnapshotLimits; static;
  end;

  TGocciaHeapSnapshotWriter = class;
  TGocciaHeapSnapshotReader = class;

  // An object the accounting check is to pass over.
  TGocciaSnapshotObjectFilter = function(
    const AObject: TGCManagedObject): Boolean of object;

  { Objects the accounting check does not count as missed: what was alive
    before the run, and what the embedder keeps itself. One named type, so
    every unit shares its specialization. }
  TGocciaHeapSnapshotBaseline = THashMap<TGCManagedObject, Boolean>;

  { A memory stream whose buffer is charged to the run's memory ceiling
    before it grows, and released when it shrinks or is freed: the writer's
    buffers, which grow with the heap it walks. The charge never collects
    (ADR 0148): one that does not fit refuses the snapshot. }
  TGocciaChargedMemoryStream = class(TMemoryStream)
  private
    FCharged: Int64;
  protected
    function Realloc(var ANewCapacity: PtrInt): Pointer; override;
  public
    destructor Destroy; override;
  end;

  { One per class, registered by the unit that owns the class, which can
    reach its private state. Its schema version changes whenever the fields
    it writes change; a snapshot written under another schema version is
    refused on restore. }
  TGocciaSnapshotCodec = class
  public
    function Tag: string; virtual; abstract;
    function SchemaVersion: Word; virtual; abstract;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); virtual; abstract;
    // Creates the empty object. May read non-reference data from the
    // payload; references do not resolve yet.
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; virtual; abstract;
    // Reads the payload from its start; every byte of it.
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); virtual; abstract;
    // Runs after every object is filled, with the payload readable again.
    procedure Finish(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); virtual;
  end;

  { A run's module records. A module record is not a managed object, so a
    snapshot keeps its records in a table of their own, written and read by
    the record codec the session or the restore supplies, bound to its
    engine. Each record has a header, read before any object is allocated
    (a restore compiles the module a header names there), and a payload,
    read once every object is filled, before any is finished. }
  TGocciaSnapshotModuleRecordCodec = class
  public
    function SchemaVersion: Word; virtual; abstract;
    // Lists every record the snapshot holds with AddModuleRecord, or
    // refuses.
    procedure ListRecords(const AWriter: TGocciaHeapSnapshotWriter);
      virtual; abstract;
    // Strings and plain data only: a header refers to no object.
    procedure WriteHeader(const AWriter: TGocciaHeapSnapshotWriter;
      const ARecord: TObject); virtual; abstract;
    procedure WritePayload(const AWriter: TGocciaHeapSnapshotWriter;
      const ARecord: TObject); virtual; abstract;
    // Reads a header and creates the empty record, which the codec keeps
    // until the restore succeeds or fails.
    function Allocate(const AReader: TGocciaHeapSnapshotReader): TObject;
      virtual; abstract;
    // Reads the payload from its start; every byte of it.
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const ARecord: TObject); virtual; abstract;
    // Runs once every record is filled, before any object is finished, with
    // the payload readable again.
    procedure Finish(const AReader: TGocciaHeapSnapshotReader;
      const ARecord: TObject); virtual;
  end;

  // What a lazy own property of a built-in became when it was built.
  TGocciaMaterializedMember = record
    Accessor: Boolean;
    Value, Getter, Setter: TGocciaValue;
    Flags: TPropertyFlags;
  end;

  TGocciaHeapSnapshotBuiltinTable = class;

  // Keeps alive the values a table recorded lazy members built into: a
  // replaced member's value could otherwise be freed and its address reused
  // by the replacement, which would then compare as the member it replaced.
  TGocciaMaterializedMemberRoots = class(TGCRootSource)
  private
    FTable: TGocciaHeapSnapshotBuiltinTable;
  public
    procedure MarkRootReferences; override;
  end;

  // Records into a table each lazy member built while it is installed.
  TGocciaBuiltinMaterializationObserver = class(
    TGocciaLazyMaterializationObserver)
  private
    FTable: TGocciaHeapSnapshotBuiltinTable;
  public
    procedure Materialized(const AObject: TGocciaObjectValue;
      const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
      override;
  end;

  { The built-ins of an engine, named by the first path that reached them
    from the engine's roots (Goccia.HeapSnapshot builds and resolves it). A
    snapshot refers to built-ins by these names.

    It also records each built-in's own state when RecordFingerprints runs,
    so DirtyPaths can name the built-ins the run changed. A lazy member is
    unbuilt then; one built since counts as unchanged only while it is still
    exactly what building it produced, which the table learns by watching
    the builds while it is installed (Install). }
  TGocciaHeapSnapshotBuiltinTable = class
  private
    FObjects: TList<TGCManagedObject>;
    // TList<string>, not TStringList, whose strings are AnsiString: a key
    // with a lone surrogate would not survive it.
    FPaths: TList<string>;
    FIndex: THashMap<TGCManagedObject, Integer>;
    FByPath: TDictionary<string, Integer>;
    FFingerprints: array of string;
    FFingerprintCount: Integer;
    FMaterialized: TDictionary<string, TGocciaMaterializedMember>;
    FObserver: TGocciaBuiltinMaterializationObserver;
    FMaterializedRoots: TGocciaMaterializedMemberRoots;
    // What its entries and names hold, charged to the run's memory ceiling
    // without collecting as they are added: a run can hang a large graph on
    // a built-in, and the walk names what it reaches before refusing.
    FChargedBytes: Int64;
    FRealm: TObject;
    function GetCount: Integer;
    function MemberKey(const AObject: TGCManagedObject;
      const AName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    // Watches the lazy member builds of ARealm (the engine's realm) on this
    // thread until Uninstall; another engine's, nested on the thread, are
    // its own.
    procedure Install(const ARealm: TObject);
    procedure Uninstall;
    procedure NoteMaterialized(const AObject: TGocciaObjectValue;
      const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
    // False when AObject is already in the table, is nil, or APath already
    // names another object.
    function Add(const AObject: TGCManagedObject; const APath: string): Boolean;
    function IndexOf(const AObject: TGCManagedObject): Integer;
    function FindPath(const APath: string): TGCManagedObject;
    function ObjectAt(const AIndex: Integer): TGCManagedObject;
    function PathAt(const AIndex: Integer): string;
    // The SHA-256 (hex) of a canonical encoding of an entry's own state:
    // prototype, extensibility, integrity, and every own property's key,
    // attributes and value identity, in order. Values are named by their
    // built-in names, so engines with the same built-ins agree.
    function ComputeFingerprint(const AIndex: Integer): string;
    // Records every entry's fingerprint; DirtyPaths compares against it.
    procedure RecordFingerprints;
    // The names of entries whose own state changed since RecordFingerprints:
    // built-ins the run changed. The caller frees the list.
    function DirtyPaths: TStringList;
    property Count: Integer read GetCount;
    // How many entries RecordFingerprints recorded: the ones after them were
    // added since, as the run built them.
    property FingerprintCount: Integer read FFingerprintCount;
  end;

  { The compiled modules whose function templates a snapshot can refer to,
    each named by the SHA-256 of its bytecode, and each template by its
    depth-first index in its module. A restoring engine supplies its own
    compiled modules; a template refers to a module with the same hash, and
    two modules with the same bytecode are interchangeable. A module's hash is
    computed when first asked for, streamed through SHA-256 without a copy of
    the bytecode, and its tables are charged to the run's memory ceiling. }
  TGocciaSnapshotTemplateList = TList<TGocciaFunctionTemplate>;
  TGocciaSnapshotTemplateLists = TObjectList<TGocciaSnapshotTemplateList>;

  TGocciaSnapshotTemplateTable = class
  private
    FModules: TList<TGocciaBytecodeModule>;
    // Each module added, with its index in FModules (-1 for eval code).
    FModuleSet: THashMap<TGocciaBytecodeModule, Integer>;
    FHashes: TList<string>;
    FByHash: TDictionary<string, Integer>;
    FAllHashed: Boolean;
    FTemplates: THashMap<TGocciaFunctionTemplate, Int64>;
    FTemplateLists: TGocciaSnapshotTemplateLists;
    FChargedBytes: Int64;
    procedure Charge(const ABytes: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    // Adds AModule and numbers its templates; a module already in the table
    // is not added again, and neither is global eval code, which the
    // bytecode format cannot write: its functions refuse.
    procedure AddModule(const AModule: TGocciaBytecodeModule);
    // AModule's index and ATemplate's index in it; False for a template of no
    // module in the table.
    function TryIdentify(const ATemplate: TGocciaFunctionTemplate;
      out AModule, AIndex: Integer): Boolean;
    function ModuleHash(const AModule: Integer): string;
    // A module with the bytecode AHash names, or -1.
    function FindModule(const AHash: string): Integer;
    // AModule's index, or -1 for one not in the table (or eval code).
    function IndexOfModule(const AModule: TGocciaBytecodeModule): Integer;
    function ModuleAt(const AModule: Integer): TGocciaBytecodeModule;
    function TemplateAt(const AModule, AIndex: Integer): TGocciaFunctionTemplate;
    function TemplateCount(const AModule: Integer): Integer;
    function ModuleCount: Integer;
  end;

  TGocciaHeapSnapshotWriter = class
  private
    FBuiltins: TGocciaHeapSnapshotBuiltinTable;
    FIds: THashMap<TGCManagedObject, Integer>;
    FObjects: TList<TGCManagedObject>;
    FParents: TList<Integer>;
    FBuiltinRefs: TList<Integer>;
    FBuiltinRefIndex: THashMap<Integer, Integer>;
    FStrings: TList<string>;
    FStringIndex: TDictionary<string, Integer>;
    FCodecs: TList<TGocciaSnapshotCodec>;
    FCodecIndex: THashMap<TGocciaSnapshotCodec, Integer>;
    FOut: TStream;
    FCurrentId: Integer;
    FBuildIdentity: string;
    FReservedBytes: Int64;
    FTemplates: TGocciaSnapshotTemplateTable;
    // The modules the snapshot refers to, in the order first referred to.
    FModuleRefs: TList<Integer>;
    FModuleRefIndex: THashMap<Integer, Integer>;
    // The run's module records, in the order the record codec listed them.
    FRecordCodec: TGocciaSnapshotModuleRecordCodec;
    FRecords: TList<TObject>;
    FRecordIndex: THashMap<TObject, Integer>;
    procedure Reserve(const ABytes: Int64);
    function GetObjectCount: Integer;
    function GetBuiltinRefCount: Integer;
    function ReferenceChain(const AId: Integer): string;
    function StringIndex(const AValue: string): Integer;
    function CodecIndex(const ACodec: TGocciaSnapshotCodec): Integer;
    procedure WriteRaw(const AValue: string);
  public
    constructor Create(const ABuiltins: TGocciaHeapSnapshotBuiltinTable);
    destructor Destroy; override;

    procedure WriteU8(const AValue: Byte);
    procedure WriteU16(const AValue: Word);
    procedure WriteU32(const AValue: Cardinal);
    procedure WriteI32(const AValue: Integer);
    procedure WriteI64(const AValue: Int64);
    procedure WriteDouble(const AValue: Double);
    procedure WriteBool(const AValue: Boolean);
    // A string, by its index in the snapshot's string table.
    procedure WriteStr(const AValue: string);
    // A value or nil.
    procedure WriteValue(const AValue: TGocciaValue);
    // Any managed object or nil: a value, or an object a codec covers.
    procedure WriteRef(const AObject: TGCManagedObject);
    // An ordinary object's own state: prototype, extensibility, integrity,
    // and its own string- and symbol-keyed properties in their order.
    procedure WriteObjectCore(const AObject: TGocciaObjectValue);
    // A function template: its module and its index there. Refuses one of a
    // module the template table does not hold.
    procedure WriteTemplate(const ATemplate: TGocciaFunctionTemplate);
    // A VM register: its kind and its scalar or value.
    procedure WriteRegister(const ARegister: TGocciaRegister);
    // Lists a module record the snapshot holds; the record codec's
    // ListRecords calls it.
    procedure AddModuleRecord(const ARecord: TObject);
    function HoldsModuleRecord(const ARecord: TObject): Boolean;
    // A module record or nil, by its place in the record table. Refuses one
    // the record codec did not list.
    procedure WriteModuleRecord(const ARecord: TObject);
    // A compiled module's identity, the SHA-256 of its bytecode, as a
    // string. Refuses one the template table does not hold.
    procedure WriteModuleIdentity(const AModule: TGocciaBytecodeModule);
    // Polls the run's execution limits; codecs call it once per item in
    // every loop whose length the run controls.
    procedure Poll;
    procedure Refuse(const AReason: string);

    // Writes the whole snapshot of what ARoots reach. Refuses when something
    // reachable has no codec. The caller refuses a changed built-in first
    // (DirtyPaths; the session does).
    procedure Serialize(const ARoots: array of TGCManagedObject;
      const AStream: TStream);
    // Refuses when a live object registered after AFirstOfRun (an object
    // registered when the run began) is neither in ABaseline (the embedder's
    // own), a built-in, nor written. The collector's list keeps objects in
    // the order they registered, so what was alive before the run is what
    // comes before AFirstOfRun. Detects objects a codec missed, not fields.
    // It does not collect: the caller collects before the snapshot, so what
    // is left is live. It also passes over the objects ASkip accepts (the
    // internals of built-ins, which are named through them).
    procedure CheckAccounting(const AFirstOfRun: TGCManagedObject;
      const ABaseline: TGocciaHeapSnapshotBaseline;
      const ASkip: TGocciaSnapshotObjectFilter = nil);
    function IsWritten(const AObject: TGCManagedObject): Boolean;

    property ObjectCount: Integer read GetObjectCount;
    property BuiltinRefCount: Integer read GetBuiltinRefCount;
    // The engine build a snapshot names; a reader refuses another.
    property BuildIdentity: string read FBuildIdentity write FBuildIdentity;
    // The modules whose templates the snapshot can refer to; the caller owns
    // it. Without one, a function refuses.
    property Templates: TGocciaSnapshotTemplateTable read FTemplates
      write FTemplates;
    // Writes the run's module records; the caller owns it. Without one, the
    // snapshot holds no records and refuses any reference to one.
    property ModuleRecords: TGocciaSnapshotModuleRecordCodec
      read FRecordCodec write FRecordCodec;
  end;

  // Finds the built-in a name refers to, adding it to the reader's table;
  // nil when the name refers to nothing in this engine.
  TGocciaBuiltinPathResolve = function(const APath: string): TGCManagedObject
    of object;
  // Builds the named built-ins in this engine and compares each with the
  // fingerprint the writing engine took; the name of the first that differs
  // or is missing, or '' when all match.
  TGocciaBuiltinVerify = function(const APaths: TStringArray;
    const AFingerprints: array of string): string of object;

  TGocciaHeapSnapshotReader = class
  private
    FBuiltins: TGocciaHeapSnapshotBuiltinTable;
    FLimits: TGocciaHeapSnapshotLimits;
    FData: TBytes;
    FPos: Int64;
    FLimit: Int64;
    FStrings: array of string;
    FCodecs: array of TGocciaSnapshotCodec;
    FBuiltinObjects: array of TGCManagedObject;
    FObjects: array of TGCManagedObject;
    // The values ReadRef creates (numbers, strings, BigInts): rooted with
    // the shells until the reader is freed, since growing an object's
    // storage during a fill can collect before a value is stored.
    FCreated: TList<TGCManagedObject>;
    FCreatedCharged: Integer;
    // Registered symbols the thread's registry did not have yet: created
    // unregistered, and registered only once the restore succeeded, so a
    // refused snapshot leaves the registry as it was.
    FNewRegistered: TDictionary<string, TGocciaSymbolValue>;
    FObjectCodecs: array of Integer;
    FPayloadStart: array of Int64;
    FPayloadEnd: array of Int64;
    FRoots: TGCRootSource;
    FReservedBytes: Int64;
    FBuiltinResolve: TGocciaBuiltinPathResolve;
    FBuiltinVerify: TGocciaBuiltinVerify;
    FBuildIdentity: string;
    FContext: TObject;
    FScratch: TObjectDictionary<TObject, TObject>;
    FTemplates: TGocciaSnapshotTemplateTable;
    // The snapshot's modules, as indexes into FTemplates.
    FModuleMap: array of Integer;
    FRecordCodec: TGocciaSnapshotModuleRecordCodec;
    FRecords: array of TObject;
    function GetObjectCount: Integer;
    procedure Need(const ACount: Int64);
    function ReadCount(const ALimit: Int64; const AMinBytesEach: Int64;
      const AWhat: string): Integer;
    procedure Reserve(const ACount, AElementSize: Int64);
    procedure RoomForCreated;
    procedure NoteCreated(const AObject: TGCManagedObject);
    function ReadRawString: string;
    function ReadValueOrHole: TGocciaValue;
    procedure CheckPrototypeChains;
  public
    constructor Create(const ABuiltins: TGocciaHeapSnapshotBuiltinTable;
      const ALimits: TGocciaHeapSnapshotLimits);
    destructor Destroy; override;

    function ReadU8: Byte;
    function ReadU16: Word;
    function ReadU32: Cardinal;
    function ReadI32: Integer;
    function ReadI64: Int64;
    function ReadDouble: Double;
    function ReadBool: Boolean;
    function ReadStr: string;
    // A value or nil; never the array hole, which only ReadElementValue
    // reads.
    function ReadValue: TGocciaValue;
    // A value that must be there: a property's value.
    function ReadPresentValue: TGocciaValue;
    // A dense array element, or a VM register or cell: a value or the hole,
    // which marks a missing element or a binding in its temporal dead zone.
    function ReadElementValue: TGocciaValue;
    // An accessor's getter or setter: absent or a function.
    function ReadAccessorHalf: TGocciaValue;
    function ReadRef: TGCManagedObject;
    // A count of items each at least AMinBytesEach long in what is left of
    // the current payload: refuses one the payload cannot hold.
    function ReadItemCount(const AMinBytesEach: Integer): Integer;
    procedure ReadObjectCore(const AObject: TGocciaObjectValue);
    // A function template the snapshot refers to, in this engine's module of
    // the same bytecode.
    function ReadTemplate: TGocciaFunctionTemplate;
    function ReadRegister: TGocciaRegister;
    // A module record or nil: the record codec's empty record, allocated
    // before any object was.
    function ReadModuleRecord: TObject;
    // Polls the run's execution limits; codecs call it once per item in
    // every loop whose length follows the input.
    procedure Poll;
    // Charges ACount items of AElementSize to the run's memory ceiling
    // before a codec allocates them; the charge ends with the restore unless
    // the codec hands it over to the object that keeps the memory.
    procedure ReserveItems(const ACount, AElementSize: Int64);
    // ABytes of what ReserveItems charged now belong to a restored object,
    // which releases them itself.
    procedure HandOverReservation(const ABytes: Int64);
    // What a codec worked out once for AKey (a template's frame layout, say),
    // kept until the restore ends; the reader owns it.
    function Scratch(const AKey: TObject): TObject;
    procedure SetScratch(const AKey, AValue: TObject);
    procedure Refuse(const AReason: string);

    // Restores the snapshot in AStream and returns its roots, rooted until
    // the reader is freed.
    function Restore(const AStream: TStream): TGCManagedObjectArray;

    property ObjectCount: Integer read GetObjectCount;
    property BuiltinResolve: TGocciaBuiltinPathResolve
      read FBuiltinResolve write FBuiltinResolve;
    // Without it, a snapshot that names built-ins the run built is refused.
    property BuiltinVerify: TGocciaBuiltinVerify
      read FBuiltinVerify write FBuiltinVerify;
    // The engine build this reader accepts.
    property BuildIdentity: string read FBuildIdentity write FBuildIdentity;
    // This engine's compiled modules; the caller owns it. A snapshot naming a
    // module it does not hold is refused. A record codec may add the modules
    // it compiles.
    property Templates: TGocciaSnapshotTemplateTable read FTemplates
      write FTemplates;
    // Restores the snapshot's module records; the caller owns it. Without
    // one, a snapshot holding records is refused.
    property ModuleRecords: TGocciaSnapshotModuleRecordCodec
      read FRecordCodec write FRecordCodec;
    // Engine-side context for codecs that need it.
    property Context: TObject read FContext write FContext;
  end;

procedure RegisterSnapshotCodec(const AClass: TClass;
  const ACodec: TGocciaSnapshotCodec);
function FindSnapshotCodecByClass(const AClass: TClass): TGocciaSnapshotCodec;
function FindSnapshotCodecByTag(const ATag: string): TGocciaSnapshotCodec;
// Whether a snapshot can hold an object of this kind: it writes the value
// inline (undefined, null, booleans, holes, numbers, strings, BigInts) or a
// codec is registered for its exact class. Built-ins are named instead,
// whatever their class.
function SnapshotCanHold(const AObject: TObject): Boolean;
// The engine build a snapshot names by default: version and commit.
function HeapSnapshotBuildIdentity: string;

type
  // Creates the intrinsic of a realm slot that is filled on first use, in the
  // current realm, as using it would.
  TGocciaSnapshotSlotMaterializer = procedure;

// A unit whose realm slot, or owned slot, is filled on first use registers
// how to fill it, under the slot's name, so a restore can build the intrinsic
// a snapshot names (ADR 0148).
procedure RegisterSnapshotSlotMaterializer(const ASlotName: string;
  const AMaterializer: TGocciaSnapshotSlotMaterializer);
function FindSnapshotSlotMaterializer(
  const ASlotName: string): TGocciaSnapshotSlotMaterializer;
// A slot filled when a lazy built-in is first read, instead of by a
// materializer: AGlobalPath names that built-in from the global object, its
// own property names separated by '.' ('Temporal.PlainDate',
// 'Object.prototype.__proto__'), and reading each property's descriptor in
// turn builds what fills the slot without calling anything.
procedure RegisterSnapshotSlotGlobalPath(const ASlotName, AGlobalPath: string);
function FindSnapshotSlotGlobalPath(const ASlotName: string): string;
// Fills the slot ASlotName names in the current realm as using it would, by
// its materializer or its global path; False when it has neither.
function MaterializeSnapshotSlot(const ASlotName: string): Boolean;

implementation

uses
  Math,

  BigInteger,
  SHA256,

  Goccia.Builtins.GlobalSymbol,
  Goccia.Bytecode.Binary,
  Goccia.NativeLimits,
  Goccia.Realm,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.DateData,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Version;

type
  TCodecRegistry = class
  public
    ByClass: TDictionary<TClass, TGocciaSnapshotCodec>;
    ByTag: TDictionary<string, TGocciaSnapshotCodec>;
    Owned: TObjectList<TGocciaSnapshotCodec>;
    constructor Create;
    destructor Destroy; override;
  end;

  // Roots every restored object until the reader is freed.
  TRestoreRoots = class(TGCRootSource)
  public
    Reader: TGocciaHeapSnapshotReader;
    procedure MarkRootReferences; override;
  end;

  // An ordinary object of exactly TGocciaObjectValue.
  TPlainObjectCodec = class(TGocciaSnapshotCodec)
  public
    function Tag: string; override;
    function SchemaVersion: Word; override;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
  end;

  // A symbol the run created (Symbol()); registered and well-known symbols
  // are named instead.
  TSymbolCodec = class(TGocciaSnapshotCodec)
  public
    function Tag: string; override;
    function SchemaVersion: Word; override;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
  end;

var
  GRegistry: TCodecRegistry;
  GSlotMaterializers: TDictionary<string, TGocciaSnapshotSlotMaterializer>;
  GSlotGlobalPaths: TDictionary<string, string>;

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

procedure RegisterSnapshotSlotGlobalPath(const ASlotName, AGlobalPath: string);
begin
  if not Assigned(GSlotGlobalPaths) then
    GSlotGlobalPaths := TDictionary<string, string>.Create;
  GSlotGlobalPaths.AddOrSetValue(ASlotName, AGlobalPath);
end;

function FindSnapshotSlotGlobalPath(const ASlotName: string): string;
begin
  Result := '';
  if Assigned(GSlotGlobalPaths) then
    GSlotGlobalPaths.TryGetValue(ASlotName, Result);
end;

function MaterializeSnapshotSlot(const ASlotName: string): Boolean;
var
  Materializer: TGocciaSnapshotSlotMaterializer;
  Path, Name: string;
  Current: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  Start, I: Integer;
begin
  Materializer := FindSnapshotSlotMaterializer(ASlotName);
  if Assigned(Materializer) then
  begin
    Materializer();
    Exit(True);
  end;
  Path := FindSnapshotSlotGlobalPath(ASlotName);
  Result := Path <> '';
  if not Result or (CurrentRealm = nil) or
     not (CurrentRealm.GlobalObject is TGocciaObjectValue) then
    Exit;
  // Each name is an own property of what the last one held; a descriptor
  // read builds a lazy one and calls no getter.
  Current := TGocciaObjectValue(CurrentRealm.GlobalObject);
  Start := 1;
  for I := 1 to Length(Path) + 1 do
    if (I > Length(Path)) or (Path[I] = '.') then
    begin
      Name := Copy(Path, Start, I - Start);
      Start := I + 1;
      if not Assigned(Current) then
        Exit;
      Descriptor := Current.GetOwnPropertyDescriptor(Name);
      if (Descriptor is TGocciaPropertyDescriptorData) and
         (TGocciaPropertyDescriptorData(Descriptor).Value is
          TGocciaObjectValue) then
        Current := TGocciaObjectValue(
          TGocciaPropertyDescriptorData(Descriptor).Value)
      else
        Current := nil;
      ReleaseDescriptor(Descriptor);
    end;
end;

const
  // The smallest encodings, used to bound counts by the bytes left: a value
  // is at least its tag, a string-table entry at least its length.
  MIN_VALUE_BYTES = 1;
  MIN_STRING_BYTES = 4;
  // A codec entry: tag string index and schema version.
  MIN_CODEC_BYTES = 6;
  // An object entry: codec index and payload length.
  MIN_OBJECT_BYTES = 8;
  // A module record's header holds at least a string and a count.
  MIN_RECORD_HEADER_BYTES = 8;
  // A property entry: key, flags, kind, one value.
  MIN_PROPERTY_BYTES = 7;
  // What the writer's tables hold per object or string, charged up front.
  WRITER_ENTRY_BYTES = 64;
  // What the reader's duplicate-key check and its prototype-chain check hold
  // per entry.
  SEEN_ENTRY_BYTES = 48;
  CHAIN_ENTRY_BYTES = 40;
  // A built-in table entry: its slots in the lists and maps, its fingerprint;
  // its name is held twice (the list and the map key).
  TABLE_ENTRY_BYTES = 160;
  // The most a fingerprint encodes of one built-in; the encoding is a
  // temporary of the walk, bounded rather than charged.
  MAX_FINGERPRINT_BYTES = 4 * 1024 * 1024;
  // TMemoryStream's growth step, which its capacity is rounded to.
  STREAM_BLOCK_BYTES = 4096;

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
  if GRegistry.ByTag.ContainsKey(ACodec.Tag) then
    raise Exception.CreateFmt('heap snapshot codec tag "%s" registered twice',
      [ACodec.Tag]);
  GRegistry.Owned.Add(ACodec);
  GRegistry.ByClass.AddOrSetValue(AClass, ACodec);
  GRegistry.ByTag.Add(ACodec.Tag, ACodec);
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

function IsInlineValue(const AObject: TObject): Boolean; inline;
begin
  Result := (AObject is TGocciaUndefinedLiteralValue) or
    (AObject is TGocciaNullLiteralValue) or
    (AObject is TGocciaBooleanLiteralValue) or (AObject is TGocciaHoleValue) or
    (AObject is TGocciaNumberLiteralValue) or
    (AObject is TGocciaStringLiteralValue) or (AObject is TGocciaBigIntValue);
end;

function SnapshotCanHold(const AObject: TObject): Boolean;
begin
  Result := IsInlineValue(AObject) or
    Assigned(FindSnapshotCodecByClass(AObject.ClassType));
end;

function HeapSnapshotBuildIdentity: string;
begin
  Result := GetVersion + '+' + GetCommit;
end;

// Charges what the writer's own allocations take to the run's memory
// ceiling, without collecting (ADR 0148): a collection after the checkpoint
// could queue work. A charge that does not fit refuses the snapshot.
procedure ChargeWithoutCollecting(const ABytes: Int64);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and not GC.TryChargeExternalBytes(ABytes) then
    raise EGocciaHeapSnapshotRefused.Create(
      'the snapshot does not fit the run''s memory ceiling');
end;

procedure ReleaseCharge(const ABytes: Int64);
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and (ABytes > 0) then
    GC.ReleaseExternalBytes(ABytes);
end;

{ TGocciaChargedMemoryStream }

function TGocciaChargedMemoryStream.Realloc(var ANewCapacity: PtrInt): Pointer;
var
  Want: Int64;
begin
  // TMemoryStream grows by at least a quarter, rounded to its block size:
  // charge that much first, then settle on what it took.
  Want := Int64(ANewCapacity) + Int64(Capacity) div 4 + STREAM_BLOCK_BYTES;
  if Want > FCharged then
  begin
    ChargeWithoutCollecting(Want - FCharged);
    FCharged := Want;
  end;
  Result := inherited Realloc(ANewCapacity);
  if ANewCapacity < FCharged then
  begin
    ReleaseCharge(FCharged - ANewCapacity);
    FCharged := ANewCapacity;
  end;
end;

destructor TGocciaChargedMemoryStream.Destroy;
begin
  inherited;
  ReleaseCharge(FCharged);
  FCharged := 0;
end;

{ TGocciaHeapSnapshotLimits }

class function TGocciaHeapSnapshotLimits.Defaults: TGocciaHeapSnapshotLimits;
begin
  Result.MaxInputBytes := Int64(512) * 1024 * 1024;
  Result.MaxObjects := 16 * 1024 * 1024;
  Result.MaxStrings := 16 * 1024 * 1024;
  Result.MaxStringBytes := 256 * 1024 * 1024;
  Result.MaxBuiltins := 1024 * 1024;
  Result.MaxCodecs := 4096;
  Result.MaxRoots := 1024 * 1024;
end;

{ TGocciaSnapshotCodec }

procedure TGocciaSnapshotCodec.Finish(const AReader: TGocciaHeapSnapshotReader;
  const AObject: TGCManagedObject);
begin
end;

{ TGocciaSnapshotModuleRecordCodec }

procedure TGocciaSnapshotModuleRecordCodec.Finish(
  const AReader: TGocciaHeapSnapshotReader; const ARecord: TObject);
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
  for I := 0 to Reader.FCreated.Count - 1 do
    Reader.FCreated[I].MarkReferences;
end;

{ TGocciaHeapSnapshotBuiltinTable }

constructor TGocciaHeapSnapshotBuiltinTable.Create;
begin
  inherited Create;
  FObjects := TList<TGCManagedObject>.Create;
  FPaths := TList<string>.Create;
  FIndex := THashMap<TGCManagedObject, Integer>.Create;
  FByPath := TDictionary<string, Integer>.Create;
  FMaterialized := TDictionary<string, TGocciaMaterializedMember>.Create;
  FMaterializedRoots := TGocciaMaterializedMemberRoots.Create;
  FMaterializedRoots.FTable := Self;
end;

destructor TGocciaHeapSnapshotBuiltinTable.Destroy;
begin
  Uninstall;
  ReleaseCharge(FChargedBytes);
  FMaterializedRoots.Free;
  FObjects.Free;
  FPaths.Free;
  FIndex.Free;
  FByPath.Free;
  FMaterialized.Free;
  inherited;
end;

procedure TGocciaHeapSnapshotBuiltinTable.Install(const ARealm: TObject);
begin
  if Assigned(FObserver) then
    Exit;
  FRealm := ARealm;
  FObserver := TGocciaBuiltinMaterializationObserver.Create;
  FObserver.FTable := Self;
  AddLazyMaterializationObserver(FObserver);
end;

procedure TGocciaHeapSnapshotBuiltinTable.Uninstall;
begin
  if not Assigned(FObserver) then
    Exit;
  RemoveLazyMaterializationObserver(FObserver);
  FreeAndNil(FObserver);
end;

function TGocciaHeapSnapshotBuiltinTable.MemberKey(
  const AObject: TGCManagedObject; const AName: string): string;
begin
  Result := IntToHex(PtrUInt(AObject), SizeOf(PtrUInt) * 2) + #0 + AName;
end;

procedure TGocciaHeapSnapshotBuiltinTable.NoteMaterialized(
  const AObject: TGocciaObjectValue; const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Member: TGocciaMaterializedMember;
begin
  Member := Default(TGocciaMaterializedMember);
  Member.Flags := ADescriptor.Flags;
  if ADescriptor is TGocciaPropertyDescriptorAccessor then
  begin
    Member.Accessor := True;
    Member.Getter := TGocciaPropertyDescriptorAccessor(ADescriptor).Getter;
    Member.Setter := TGocciaPropertyDescriptorAccessor(ADescriptor).Setter;
  end
  else if ADescriptor is TGocciaPropertyDescriptorData then
    Member.Value := TGocciaPropertyDescriptorData(ADescriptor).Value;
  FMaterialized.AddOrSetValue(MemberKey(AObject, AName), Member);
end;

{ TGocciaMaterializedMemberRoots }

procedure TGocciaMaterializedMemberRoots.MarkRootReferences;
var
  Member: TGocciaMaterializedMember;
  I: Integer;
begin
  // The built-ins the table names stay alive with it: a run can detach one
  // from where it was reached, and the table still fingerprints it.
  for I := 0 to FTable.FObjects.Count - 1 do
    FTable.FObjects[I].MarkReferences;
  for Member in FTable.FMaterialized.Values do
  begin
    if Assigned(Member.Value) then
      Member.Value.MarkReferences;
    if Assigned(Member.Getter) then
      Member.Getter.MarkReferences;
    if Assigned(Member.Setter) then
      Member.Setter.MarkReferences;
  end;
end;

{ TGocciaBuiltinMaterializationObserver }

procedure TGocciaBuiltinMaterializationObserver.Materialized(
  const AObject: TGocciaObjectValue; const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  // A lazy member builds in the realm whose built-in holds it.
  if TObject(CurrentRealm) <> FTable.FRealm then
    Exit;
  FTable.NoteMaterialized(AObject, AName, ADescriptor);
end;

procedure TGocciaHeapSnapshotBuiltinTable.Clear;
begin
  FObjects.Clear;
  FPaths.Clear;
  FIndex.Clear;
  FByPath.Clear;
  FMaterialized.Clear;
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
  // A name names one object: the first to take it keeps it, and another
  // object reached by the same path is not named at all.
  if FByPath.ContainsKey(APath) then
    Exit;
  ChargeWithoutCollecting(TABLE_ENTRY_BYTES + Int64(Length(APath)) * 2 *
    SizeOf(Char));
  Inc(FChargedBytes, TABLE_ENTRY_BYTES + Int64(Length(APath)) * 2 *
    SizeOf(Char));
  FIndex.Add(AObject, FObjects.Count);
  FByPath.Add(APath, FObjects.Count);
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

// A canonical encoding: every field length-prefixed, so no two different
// states encode the same, then hashed with SHA-256.
type
  TFingerprintEncoder = record
    Data: TBytes;
    Length_: Int64;
    // Past MAX_FINGERPRINT_BYTES: no built-in an engine makes is that large,
    // so one is a built-in the run grew, and it fingerprints as changed.
    TooLarge: Boolean;
    // What Data's capacity has charged to the run's memory ceiling; the
    // caller releases it.
    Charged: Int64;
    procedure Grow(const ANeeded: Int64);
    procedure Field(const AText: string);
    procedure Count(const AValue: Integer);
  end;

// Room for ANeeded bytes, never past the cap, charged without collecting.
procedure TFingerprintEncoder.Grow(const ANeeded: Int64);
var
  Capacity: Int64;
begin
  if Length(Data) >= ANeeded then
    Exit;
  Capacity := ANeeded * 2 + 64;
  if Capacity > MAX_FINGERPRINT_BYTES then
    Capacity := MAX_FINGERPRINT_BYTES;
  ChargeWithoutCollecting(Capacity - Length(Data));
  Inc(Charged, Capacity - Length(Data));
  SetLength(Data, Capacity);
end;

procedure TFingerprintEncoder.Count(const AValue: Integer);
begin
  if TooLarge then
    Exit;
  if Length_ + 4 > MAX_FINGERPRINT_BYTES then
  begin
    TooLarge := True;
    Exit;
  end;
  Grow(Length_ + 4);
  Move(AValue, Data[Length_], 4);
  Inc(Length_, 4);
end;

procedure TFingerprintEncoder.Field(const AText: string);
var
  Bytes: Int64;
begin
  Count(Length(AText));
  Bytes := Int64(Length(AText)) * SizeOf(Char);
  if TooLarge or (Length_ + Bytes > MAX_FINGERPRINT_BYTES) then
  begin
    TooLarge := True;
    Exit;
  end;
  Grow(Length_ + Bytes);
  if Bytes > 0 then
    Move(AText[1], Data[Length_], Bytes);
  Inc(Length_, Bytes);
end;

// A BigInt by its sign and limbs, encoded in place: no copy of the value,
// and nothing past the encoder's cap.
procedure EncodeBigInt(var AEncoder: TFingerprintEncoder;
  const AValue: TGocciaBigIntValue);
var
  I: Integer;
begin
  AEncoder.Field('i');
  AEncoder.Count(Ord(AValue.Value.IsNegative));
  AEncoder.Count(AValue.Value.LimbCount);
  for I := AValue.Value.LimbCount - 1 downto 0 do
  begin
    if AEncoder.TooLarge then
      Exit;
    if I and 4095 = 0 then
      CheckNativeWork;
    AEncoder.Count(Integer(AValue.Value.LimbAt(I)));
  end;
end;

function TGocciaHeapSnapshotBuiltinTable.ComputeFingerprint(
  const AIndex: Integer): string;
const
  LAZY = 'L';

  // A value's identity, a tag field and then its value's fields, encoded in
  // place: a built-in by its name, a primitive by its value, anything else
  // (an object the run attached) by its address. Nothing the run made is
  // copied first, so the encoder's cap bounds the work.
  procedure Identity(var AEncoder: TFingerprintEncoder;
    const AValue: TGocciaValue);
  var
    Index: Integer;
  begin
    if not Assigned(AValue) then
    begin
      AEncoder.Field('nil');
      Exit;
    end;
    // A built-in by its name, which is the same in every engine that has it:
    // a fingerprint taken here can be compared with one taken in another.
    Index := IndexOf(AValue);
    if Index >= 0 then
    begin
      AEncoder.Field('#');
      AEncoder.Field(FPaths[Index]);
    end
    else if AValue is TGocciaUndefinedLiteralValue then
      AEncoder.Field('u')
    else if AValue is TGocciaNullLiteralValue then
      AEncoder.Field('n')
    else if AValue is TGocciaBooleanLiteralValue then
    begin
      AEncoder.Field('b');
      AEncoder.Count(Ord(TGocciaBooleanLiteralValue(AValue).Value));
    end
    else if AValue is TGocciaStringLiteralValue then
    begin
      AEncoder.Field('s');
      AEncoder.Field(TGocciaStringLiteralValue(AValue).Value);
    end
    else if AValue is TGocciaNumberLiteralValue then
    begin
      AEncoder.Field('d');
      AEncoder.Field(IntToHex(
        PInt64(@TGocciaNumberLiteralValue(AValue).Value)^, 16));
    end
    else if AValue is TGocciaBigIntValue then
      EncodeBigInt(AEncoder, TGocciaBigIntValue(AValue))
    else if (AValue is TGocciaSymbolValue) and
            TGocciaSymbolValue(AValue).Registered then
    begin
      AEncoder.Field('r');
      AEncoder.Field(TGocciaSymbolValue(AValue).Description);
    end
    else
    begin
      AEncoder.Field('X');
      AEncoder.Field(IntToHex(PtrUInt(AValue), SizeOf(PtrUInt) * 2));
    end;
  end;

  procedure EncodeDescriptor(var AEncoder: TFingerprintEncoder;
    const AObject: TGocciaObjectValue; const AName: string;
    const ADescriptor: TGocciaPropertyDescriptor);
  var
    Member: TGocciaMaterializedMember;
  begin
    if AEncoder.TooLarge then
      Exit;
    if ADescriptor is TGocciaLazyPropertyDescriptorData then
    begin
      AEncoder.Field(LAZY);
      Exit;
    end;
    // Built since the table was recorded, and still exactly what building
    // it produced: the lazy member it was.
    if (AName <> '') and
       FMaterialized.TryGetValue(MemberKey(AObject, AName), Member) and
       (Member.Flags = ADescriptor.Flags) then
    begin
      if Member.Accessor and (ADescriptor is TGocciaPropertyDescriptorAccessor) and
         (TGocciaPropertyDescriptorAccessor(ADescriptor).Getter = Member.Getter) and
         (TGocciaPropertyDescriptorAccessor(ADescriptor).Setter = Member.Setter) then
      begin
        AEncoder.Field(LAZY);
        Exit;
      end;
      if not Member.Accessor and
         (ADescriptor.ClassType = TGocciaPropertyDescriptorData) and
         (TGocciaPropertyDescriptorData(ADescriptor).Value = Member.Value) then
      begin
        AEncoder.Field(LAZY);
        Exit;
      end;
    end;
    if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      AEncoder.Field('a');
      AEncoder.Count(Byte(ADescriptor.Flags));
      Identity(AEncoder, TGocciaPropertyDescriptorAccessor(ADescriptor).Getter);
      Identity(AEncoder, TGocciaPropertyDescriptorAccessor(ADescriptor).Setter);
    end
    else if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      AEncoder.Field('d');
      AEncoder.Count(Byte(ADescriptor.Flags));
      Identity(AEncoder, TGocciaPropertyDescriptorData(ADescriptor).Value);
    end
    else
      AEncoder.Field('?' + ADescriptor.ClassName);
  end;

var
  Obj: TGocciaObjectValue;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Symbol: TGocciaSymbolValue;
  I: Integer;
  Encoder: TFingerprintEncoder;
  Hash: TSHA256Context;
begin
  Result := '';
  if not (FObjects[AIndex] is TGocciaObjectValue) then
    Exit;
  Obj := TGocciaObjectValue(FObjects[AIndex]);
  Encoder := Default(TFingerprintEncoder);
  try
    Encoder.Field('proto');
    Identity(Encoder, Obj.Prototype);
    Encoder.Count(Ord(Obj.Extensible) or (Ord(Obj.Sealed) shl 1) or
      (Ord(Obj.Frozen) shl 2));
    // A function by its kind, whether its code is a shim's, and its
    // synthesized length and name, which are not in the property map: a
    // run's function standing where a built-in's was is not the engine's
    // own, and a deleted length or name is a change.
    if Obj is TGocciaFunctionBase then
    begin
      Encoder.Field(Obj.ClassName);
      Encoder.Count(Ord(TGocciaFunctionBase(Obj).IsShimCode));
      Encoder.Count(Ord(TGocciaFunctionBase(Obj).HasOwnLengthProperty) or
        (Ord(TGocciaFunctionBase(Obj).HasOwnNameProperty) shl 1));
      if TGocciaFunctionBase(Obj).HasOwnLengthProperty then
        Encoder.Count(TGocciaFunctionBase(Obj).IntrinsicLength);
      if TGocciaFunctionBase(Obj).HasOwnNameProperty then
        Encoder.Field(TGocciaFunctionBase(Obj).IntrinsicName);
    end;
    // In order: a deleted and re-added member moves to the end.
    Encoder.Count(Obj.Properties.Count);
    for Pair in Obj.Properties do
    begin
      CheckNativeWork;
      Encoder.Field(Pair.Key);
      // Past the cap nothing more is encoded, and the key is not copied into
      // a lookup either.
      if Encoder.TooLarge then
        Break;
      EncodeDescriptor(Encoder, Obj, Pair.Key, Pair.Value);
    end;
    Encoder.Count(Obj.StoredSymbolCount);
    for I := 0 to Obj.StoredSymbolCount - 1 do
    begin
      if Encoder.TooLarge then
        Break;
      CheckNativeWork;
      Symbol := Obj.StoredSymbolAt(I);
      Identity(Encoder, Symbol);
      EncodeDescriptor(Encoder, Obj, '',
        Obj.GetOwnSymbolPropertyDescriptor(Symbol));
    end;
    // An array built-in (Array.prototype is one) keeps its length and dense
    // elements outside the property map.
    if Obj is TGocciaArrayValue then
    begin
      Encoder.Field('array');
      // Read in place: a descriptor would create the length's number value,
      // and nothing may be created after the snapshot's collection.
      Encoder.Count(Ord(TGocciaArrayValue(Obj).LengthWritable));
      Encoder.Field(IntToStr(TGocciaArrayValue(Obj).StoredLength));
      Encoder.Count(TGocciaArrayValue(Obj).Elements.Count);
      for I := 0 to TGocciaArrayValue(Obj).Elements.Count - 1 do
      begin
        if Encoder.TooLarge then
          Break;
        if I and 1023 = 0 then
          CheckNativeWork;
        Identity(Encoder, TGocciaArrayValue(Obj).Elements[I]);
      end;
    end;
    if Encoder.TooLarge then
      Exit('too large: ' + IntToHex(PtrUInt(Obj), SizeOf(PtrUInt) * 2));
    CheckNativeWork;
    // Hashed in place: no copy of the encoding.
    SHA256Init(Hash);
    if Encoder.Length_ > 0 then
      SHA256Update(Hash, @Encoder.Data[0], Encoder.Length_);
    Result := SHA256FinalHex(Hash);
  finally
    ReleaseCharge(Encoder.Charged);
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
  try
    for I := 0 to FFingerprintCount - 1 do
      if ComputeFingerprint(I) <> FFingerprints[I] then
        Result.Add(FPaths[I]);
  except
    Result.Free;
    raise;
  end;
end;

{ TGocciaSnapshotTemplateTable }

const
  // What one template costs the table: its map entry and its list slot.
  TEMPLATE_ENTRY_BYTES = 48;
  MODULE_ENTRY_BYTES = 160;
  MODULE_SET_ENTRY_BYTES = 32;
  // Bytes hashed between polls of the run's execution limits.
  HASH_POLL_BYTES = 65536;

type
  // A write-only stream that hashes what is written to it: the module's
  // bytecode is never held in memory.
  THashingStream = class(TStream)
  private
    FContext: TSHA256Context;
    FPosition: Int64;
    FSincePoll: Int64;
  public
    constructor Create;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    function HexDigest: string;
  end;

constructor THashingStream.Create;
begin
  inherited Create;
  SHA256Init(FContext);
end;

function THashingStream.Read(var ABuffer; ACount: Longint): Longint;
begin
  raise EStreamError.Create('a hashing stream cannot be read');
end;

function THashingStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  SHA256Update(FContext, @ABuffer, ACount);
  Inc(FPosition, ACount);
  Inc(FSincePoll, ACount);
  if FSincePoll >= HASH_POLL_BYTES then
  begin
    FSincePoll := 0;
    CheckNativeWork;
  end;
  Result := ACount;
end;

function THashingStream.Seek(const AOffset: Int64;
  AOrigin: TSeekOrigin): Int64;
begin
  // Only asking for the position is allowed.
  if (AOffset <> 0) or (AOrigin <> soCurrent) then
    raise EStreamError.Create('a hashing stream cannot seek');
  Result := FPosition;
end;

function THashingStream.HexDigest: string;
begin
  Result := SHA256FinalHex(FContext);
end;

constructor TGocciaSnapshotTemplateTable.Create;
begin
  inherited Create;
  FModules := TList<TGocciaBytecodeModule>.Create;
  FModuleSet := THashMap<TGocciaBytecodeModule, Integer>.Create;
  FHashes := TList<string>.Create;
  FByHash := TDictionary<string, Integer>.Create;
  FTemplates := THashMap<TGocciaFunctionTemplate, Int64>.Create;
  FTemplateLists := TGocciaSnapshotTemplateLists.Create(True);
end;

destructor TGocciaSnapshotTemplateTable.Destroy;
begin
  FModules.Free;
  FModuleSet.Free;
  FHashes.Free;
  FByHash.Free;
  FTemplates.Free;
  FTemplateLists.Free;
  ReleaseCharge(FChargedBytes);
  inherited;
end;

procedure TGocciaSnapshotTemplateTable.Charge(const ABytes: Int64);
begin
  ChargeWithoutCollecting(ABytes);
  Inc(FChargedBytes, ABytes);
end;

procedure TGocciaSnapshotTemplateTable.AddModule(
  const AModule: TGocciaBytecodeModule);
var
  ModuleIndex, I: Integer;
  List: TGocciaSnapshotTemplateList;
  Pending: TGocciaSnapshotTemplateList;
  Template: TGocciaFunctionTemplate;
begin
  if not Assigned(AModule) or FModuleSet.ContainsKey(AModule) then
    Exit;
  // The set entry is charged when it is made, for a skipped eval module
  // too: direct eval retains one module per call.
  Charge(MODULE_SET_ENTRY_BYTES);
  FModuleSet.Add(AModule, -1);
  // Depth first, the top level first and each template's children in
  // order, with an explicit stack: the same numbering in every engine that
  // compiled the same bytecode.
  List := TGocciaSnapshotTemplateList.Create;
  Pending := TGocciaSnapshotTemplateList.Create;
  try
    try
      Pending.Add(AModule.TopLevel);
      while Pending.Count > 0 do
      begin
        CheckNativeWork;
        Template := Pending[Pending.Count - 1];
        Pending.Delete(Pending.Count - 1);
        if not Assigned(Template) then
          Continue;
        // Global eval code has no bytecode file form, so no name another
        // engine shares: the snapshot cannot name its functions.
        if Template.GlobalEvalCode then
        begin
          List.Free;
          Exit;
        end;
        Charge(TEMPLATE_ENTRY_BYTES);
        List.Add(Template);
        for I := Template.FunctionCount - 1 downto 0 do
          Pending.Add(Template.GetFunction(I));
      end;
      Charge(MODULE_ENTRY_BYTES);
    except
      List.Free;
      raise;
    end;
  finally
    Pending.Free;
  end;
  ModuleIndex := FModules.Count;
  FModules.Add(AModule);
  FModuleSet[AModule] := ModuleIndex;
  FHashes.Add('');
  FTemplateLists.Add(List);
  FAllHashed := False;
  for I := 0 to List.Count - 1 do
    if not FTemplates.ContainsKey(List[I]) then
      FTemplates.Add(List[I], (Int64(ModuleIndex) shl 32) or I);
end;

function TGocciaSnapshotTemplateTable.TryIdentify(
  const ATemplate: TGocciaFunctionTemplate; out AModule, AIndex: Integer): Boolean;
var
  Key: Int64;
begin
  Result := FTemplates.TryGetValue(ATemplate, Key);
  if Result then
  begin
    AModule := Integer(Key shr 32);
    AIndex := Integer(Key and $FFFFFFFF);
  end;
end;

// The SHA-256 of a module's bytecode as the bytecode format writes it, its
// source path included.
function TGocciaSnapshotTemplateTable.ModuleHash(const AModule: Integer): string;
var
  Stream: THashingStream;
  Writer: TGocciaBytecodeWriter;
begin
  if FHashes[AModule] = '' then
  begin
    // Once per module as well as per 64 KiB hashed: many small modules
    // hashed in one pass still poll.
    CheckNativeWork;
    Stream := THashingStream.Create;
    Writer := TGocciaBytecodeWriter.Create(Stream);
    try
      Writer.WriteModule(FModules[AModule]);
      FHashes[AModule] := Stream.HexDigest;
    finally
      Writer.Free;
      Stream.Free;
    end;
  end;
  Result := FHashes[AModule];
end;

function TGocciaSnapshotTemplateTable.FindModule(const AHash: string): Integer;
var
  I: Integer;
begin
  if not FAllHashed then
  begin
    for I := 0 to FModules.Count - 1 do
      if not FByHash.ContainsKey(ModuleHash(I)) then
        FByHash.Add(ModuleHash(I), I);
    FAllHashed := True;
  end;
  if not FByHash.TryGetValue(AHash, Result) then
    Result := -1;
end;

function TGocciaSnapshotTemplateTable.IndexOfModule(
  const AModule: TGocciaBytecodeModule): Integer;
begin
  if not FModuleSet.TryGetValue(AModule, Result) then
    Result := -1;
end;

function TGocciaSnapshotTemplateTable.ModuleAt(
  const AModule: Integer): TGocciaBytecodeModule;
begin
  Result := FModules[AModule];
end;

function TGocciaSnapshotTemplateTable.TemplateAt(const AModule,
  AIndex: Integer): TGocciaFunctionTemplate;
begin
  Result := FTemplateLists[AModule][AIndex];
end;

function TGocciaSnapshotTemplateTable.TemplateCount(
  const AModule: Integer): Integer;
begin
  Result := FTemplateLists[AModule].Count;
end;

function TGocciaSnapshotTemplateTable.ModuleCount: Integer;
begin
  Result := FModules.Count;
end;

{ TGocciaHeapSnapshotWriter }

constructor TGocciaHeapSnapshotWriter.Create(
  const ABuiltins: TGocciaHeapSnapshotBuiltinTable);
begin
  inherited Create;
  FBuiltins := ABuiltins;
  FIds := THashMap<TGCManagedObject, Integer>.Create;
  FObjects := TList<TGCManagedObject>.Create;
  FParents := TList<Integer>.Create;
  FBuiltinRefs := TList<Integer>.Create;
  FBuiltinRefIndex := THashMap<Integer, Integer>.Create;
  FStrings := TList<string>.Create;
  FStringIndex := TDictionary<string, Integer>.Create;
  FCodecs := TList<TGocciaSnapshotCodec>.Create;
  FCodecIndex := THashMap<TGocciaSnapshotCodec, Integer>.Create;
  FCurrentId := -1;
  FBuildIdentity := HeapSnapshotBuildIdentity;
  FModuleRefs := TList<Integer>.Create;
  FModuleRefIndex := THashMap<Integer, Integer>.Create;
  FRecords := TList<TObject>.Create;
  FRecordIndex := THashMap<TObject, Integer>.Create;
end;

destructor TGocciaHeapSnapshotWriter.Destroy;
begin
  FModuleRefs.Free;
  FModuleRefIndex.Free;
  FRecords.Free;
  FRecordIndex.Free;
  FIds.Free;
  FObjects.Free;
  FParents.Free;
  FBuiltinRefs.Free;
  FBuiltinRefIndex.Free;
  FStrings.Free;
  FStringIndex.Free;
  FCodecs.Free;
  FCodecIndex.Free;
  ReleaseCharge(FReservedBytes);
  inherited;
end;

// Charges the writer's own tables to the run's memory ceiling as they grow.
procedure TGocciaHeapSnapshotWriter.Reserve(const ABytes: Int64);
begin
  ChargeWithoutCollecting(ABytes);
  Inc(FReservedBytes, ABytes);
end;

function TGocciaHeapSnapshotWriter.GetObjectCount: Integer;
begin
  Result := FObjects.Count;
end;

function TGocciaHeapSnapshotWriter.GetBuiltinRefCount: Integer;
begin
  Result := FBuiltinRefs.Count;
end;

function TGocciaHeapSnapshotWriter.IsWritten(
  const AObject: TGCManagedObject): Boolean;
begin
  Result := FIds.ContainsKey(AObject);
end;

function TGocciaHeapSnapshotWriter.ReferenceChain(const AId: Integer): string;
const
  MAX_CHAIN = 12;
var
  Id, Depth: Integer;
begin
  Result := '';
  Id := AId;
  Depth := 0;
  while (Id >= 0) and (Depth < MAX_CHAIN) do
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
      ReferenceChain(FCurrentId) + ')');
  raise EGocciaHeapSnapshotRefused.Create(AReason);
end;

function TGocciaHeapSnapshotWriter.StringIndex(const AValue: string): Integer;
begin
  if not FStringIndex.TryGetValue(AValue, Result) then
  begin
    Reserve(CheckedNativeByteSize(Length(AValue), SizeOf(Char)) +
      WRITER_ENTRY_BYTES);
    Result := FStrings.Count;
    FStrings.Add(AValue);
    FStringIndex.Add(AValue, Result);
  end;
end;

function TGocciaHeapSnapshotWriter.CodecIndex(
  const ACodec: TGocciaSnapshotCodec): Integer;
begin
  if not FCodecIndex.TryGetValue(ACodec, Result) then
  begin
    Result := FCodecs.Count;
    FCodecs.Add(ACodec);
    FCodecIndex.Add(ACodec, Result);
  end;
end;

procedure TGocciaHeapSnapshotWriter.Poll;
begin
  CheckNativeWork;
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
begin
  WriteU32(StringIndex(AValue));
end;

// A string written in place: the string table's own entries. Its UTF-16 code
// units as they are, little-endian, so a lone surrogate survives (as in the
// bytecode format); copied in chunks with the run's limits polled between.
procedure TGocciaHeapSnapshotWriter.WriteRaw(const AValue: string);
const
  CHUNK_UNITS = 65536;
var
  Units: array of Word;
  Start, Count, I: Integer;
begin
  WriteU32(Length(AValue));
  Start := 1;
  SetLength(Units, Min(Length(AValue), CHUNK_UNITS));
  while Start <= Length(AValue) do
  begin
    Poll;
    Count := Min(CHUNK_UNITS, Length(AValue) - Start + 1);
    for I := 0 to Count - 1 do
      Units[I] := NtoLE(Word(AValue[Start + I]));
    FOut.WriteBuffer(Units[0], Count * 2);
    Inc(Start, Count);
  end;
end;

procedure TGocciaHeapSnapshotWriter.WriteRef(const AObject: TGCManagedObject);
var
  Id, BuiltinIndex, RefIndex, I: Integer;
begin
  if not Assigned(AObject) then
  begin
    WriteU8(HSV_NIL);
    Exit;
  end;
  if AObject is TGocciaUndefinedLiteralValue then
    WriteU8(HSV_UNDEFINED)
  else if AObject is TGocciaNullLiteralValue then
    WriteU8(HSV_NULL)
  else if AObject is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(AObject).Value then
      WriteU8(HSV_TRUE)
    else
      WriteU8(HSV_FALSE);
  end
  else if AObject is TGocciaHoleValue then
    WriteU8(HSV_HOLE)
  else if AObject is TGocciaNumberLiteralValue then
  begin
    WriteU8(HSV_NUMBER);
    WriteDouble(TGocciaNumberLiteralValue(AObject).Value);
  end
  else if AObject is TGocciaStringLiteralValue then
  begin
    WriteU8(HSV_STRING);
    WriteStr(TGocciaStringLiteralValue(AObject).Value);
  end
  else if AObject is TGocciaBigIntValue then
  begin
    // Its limbs, not its decimal text: reading decimal back is quadratic.
    WriteU8(HSV_BIGINT);
    WriteBool(TGocciaBigIntValue(AObject).Value.IsNegative);
    WriteU32(TGocciaBigIntValue(AObject).Value.LimbCount);
    for I := 0 to TGocciaBigIntValue(AObject).Value.LimbCount - 1 do
    begin
      if I and 4095 = 0 then
        Poll;
      WriteU32(TGocciaBigIntValue(AObject).Value.LimbAt(I));
    end;
  end
  else
  begin
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
    if (AObject is TGocciaSymbolValue) and
       TGocciaSymbolValue(AObject).Registered then
    begin
      WriteU8(HSV_REGISTERED_SYMBOL);
      WriteStr(TGocciaSymbolValue(AObject).Description);
      Exit;
    end;
    if not FIds.TryGetValue(AObject, Id) then
    begin
      Reserve(WRITER_ENTRY_BYTES);
      Id := FObjects.Count;
      FIds.Add(AObject, Id);
      FObjects.Add(AObject);
      FParents.Add(FCurrentId);
    end;
    WriteU8(HSV_OBJECT);
    WriteU32(Id);
  end;
end;

procedure TGocciaHeapSnapshotWriter.WriteValue(const AValue: TGocciaValue);
begin
  WriteRef(AValue);
end;

procedure TGocciaHeapSnapshotWriter.WriteObjectCore(
  const AObject: TGocciaObjectValue);
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  Descriptor: TGocciaPropertyDescriptor;
  Symbol: TGocciaSymbolValue;
  I: Integer;
begin
  // Error and RegExp internals belong to their own codecs.
  if AObject.HasErrorData then
    Refuse('no snapshot codec for an object''s error data');
  if AObject.HasRegExpData then
    Refuse('no snapshot codec for an object''s RegExp data');
  // A Date's time value is not the object's own: the Date shim keeps it in
  // a built-in WeakMap, which is named, not written.
  if HasDateValue(AObject) then
    Refuse('no snapshot codec for a Date''s time value');
  WriteValue(AObject.Prototype);
  WriteBool(AObject.Extensible);
  WriteBool(AObject.Frozen);
  WriteBool(AObject.Sealed);
  WriteBool(AObject.HasEverHadIndexedOwnProperty);
  WriteU32(AObject.Properties.Count);
  for Pair in AObject.Properties do
  begin
    Poll;
    Descriptor := Pair.Value;
    if Descriptor is TGocciaLazyPropertyDescriptorData then
      Refuse('an unmaterialized lazy property "' + Pair.Key + '"');
    WriteStr(Pair.Key);
    WriteU8(Byte(Descriptor.Flags));
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      WriteU8(HSD_ACCESSOR);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      WriteU8(HSD_DATA);
      WriteValue(TGocciaPropertyDescriptorData(Descriptor).Value);
    end
    else
      Refuse('no snapshot codec for a ' + Descriptor.ClassName);
  end;
  WriteU32(AObject.StoredSymbolCount);
  for I := 0 to AObject.StoredSymbolCount - 1 do
  begin
    Poll;
    Symbol := AObject.StoredSymbolAt(I);
    Descriptor := AObject.GetOwnSymbolPropertyDescriptor(Symbol);
    WriteValue(Symbol);
    WriteU8(Byte(Descriptor.Flags));
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      WriteU8(HSD_ACCESSOR);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);
      WriteValue(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
    begin
      WriteU8(HSD_DATA);
      WriteValue(TGocciaPropertyDescriptorData(Descriptor).Value);
    end
    else
      Refuse('no snapshot codec for a ' + Descriptor.ClassName);
  end;
end;

procedure TGocciaHeapSnapshotWriter.WriteTemplate(
  const ATemplate: TGocciaFunctionTemplate);
var
  Module, Index, RefIndex: Integer;
begin
  if not Assigned(FTemplates) or
     not FTemplates.TryIdentify(ATemplate, Module, Index) then
    Refuse('a function whose code is in no module the snapshot can name');
  if not FModuleRefIndex.TryGetValue(Module, RefIndex) then
  begin
    // The list entry, the map entry and the hash index Serialize keeps.
    Reserve(WRITER_ENTRY_BYTES + SizeOf(Integer));
    RefIndex := FModuleRefs.Count;
    FModuleRefs.Add(Module);
    FModuleRefIndex.Add(Module, RefIndex);
  end;
  WriteU32(RefIndex);
  WriteU32(Index);
end;

procedure TGocciaHeapSnapshotWriter.AddModuleRecord(const ARecord: TObject);
begin
  if not Assigned(ARecord) or FRecordIndex.ContainsKey(ARecord) then
    Exit;
  Reserve(WRITER_ENTRY_BYTES);
  FRecordIndex.Add(ARecord, FRecords.Count);
  FRecords.Add(ARecord);
end;

function TGocciaHeapSnapshotWriter.HoldsModuleRecord(
  const ARecord: TObject): Boolean;
begin
  Result := FRecordIndex.ContainsKey(ARecord);
end;

// 0 for nil, else the record's place plus one.
procedure TGocciaHeapSnapshotWriter.WriteModuleRecord(const ARecord: TObject);
var
  Index: Integer;
begin
  if not Assigned(ARecord) then
  begin
    WriteU32(0);
    Exit;
  end;
  if not FRecordIndex.TryGetValue(ARecord, Index) then
    Refuse('a module record the snapshot does not hold');
  WriteU32(Cardinal(Index) + 1);
end;

procedure TGocciaHeapSnapshotWriter.WriteModuleIdentity(
  const AModule: TGocciaBytecodeModule);
var
  Index: Integer;
begin
  Index := -1;
  if Assigned(FTemplates) then
    Index := FTemplates.IndexOfModule(AModule);
  if Index < 0 then
    Refuse('a module whose bytecode the snapshot cannot name');
  WriteStr(FTemplates.ModuleHash(Index));
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

procedure TGocciaHeapSnapshotWriter.Serialize(
  const ARoots: array of TGCManagedObject; const AStream: TStream);
var
  RootsOut, Payload, ObjectsOut, RecordHeadersOut, RecordsOut: TMemoryStream;
  I, Index, RecordIndex: Integer;
  Codec: TGocciaSnapshotCodec;
  Obj: TGCManagedObject;
  PathIndexes, LatePathIndexes, LateFingerprintIndexes,
    ModuleHashIndexes: array of Integer;
  LateFingerprints: array of string;
begin
  RootsOut := TGocciaChargedMemoryStream.Create;
  Payload := TGocciaChargedMemoryStream.Create;
  ObjectsOut := TGocciaChargedMemoryStream.Create;
  RecordHeadersOut := TGocciaChargedMemoryStream.Create;
  RecordsOut := TGocciaChargedMemoryStream.Create;
  try
    // The module records, every one the run has, and their headers, which
    // name strings only.
    if Assigned(FRecordCodec) then
    begin
      FRecordCodec.ListRecords(Self);
      FOut := RecordHeadersOut;
      for I := 0 to FRecords.Count - 1 do
      begin
        Poll;
        FRecordCodec.WriteHeader(Self, FRecords[I]);
      end;
    end;

    // The roots first: writing them numbers the objects they reach.
    FOut := RootsOut;
    WriteU32(Length(ARoots));
    for I := 0 to High(ARoots) do
      WriteRef(ARoots[I]);

    // Objects, breadth first: writing one numbers the ones it reaches. A
    // record's payload is written once the objects so far are, and can
    // number more.
    Index := 0;
    RecordIndex := 0;
    while (Index < FObjects.Count) or (RecordIndex < FRecords.Count) do
    begin
      CheckNativeWork;
      if Index >= FObjects.Count then
      begin
        Payload.Clear;
        FOut := Payload;
        FRecordCodec.WritePayload(Self, FRecords[RecordIndex]);
        if Payload.Size > High(Cardinal) then
          Refuse('a module record''s payload exceeds 4 GiB');
        FOut := RecordsOut;
        WriteU32(Payload.Size);
        RecordsOut.CopyFrom(Payload, 0);
        Inc(RecordIndex);
        Continue;
      end;
      Obj := FObjects[Index];
      FCurrentId := Index;
      Codec := FindSnapshotCodecByClass(Obj.ClassType);
      if not Assigned(Codec) then
        Refuse('no snapshot codec for ' + Obj.ClassName);
      Payload.Clear;
      FOut := Payload;
      Codec.Write(Self, Obj);
      if Payload.Size > High(Cardinal) then
        Refuse('an object''s payload exceeds 4 GiB');
      FOut := ObjectsOut;
      WriteU16(CodecIndex(Codec));
      WriteU32(Payload.Size);
      ObjectsOut.CopyFrom(Payload, 0);
      Inc(Index);
    end;
    FCurrentId := -1;

    // Built-in names go into the string table before it is written.
    // The index tables below are charged as the writer's other tables are.
    Reserve(Int64(FBuiltinRefs.Count) * SizeOf(Integer));
    SetLength(PathIndexes, FBuiltinRefs.Count);
    for I := 0 to FBuiltinRefs.Count - 1 do
      PathIndexes[I] := StringIndex(FBuiltins.PathAt(FBuiltinRefs[I]));
    // The built-ins the run built since the table was recorded: no pristine
    // state of theirs was seen here, so each goes with its fingerprint, and
    // the restoring engine builds the same ones and compares.
    // Two indexes and a fingerprint (64 hex digits) for each.
    Reserve(Int64(FBuiltins.Count - FBuiltins.FingerprintCount) *
      (2 * SizeOf(Integer) + SizeOf(string) + 160));
    SetLength(LatePathIndexes, FBuiltins.Count - FBuiltins.FingerprintCount);
    SetLength(LateFingerprints, Length(LatePathIndexes));
    SetLength(LateFingerprintIndexes, Length(LatePathIndexes));
    for I := 0 to High(LatePathIndexes) do
    begin
      Poll;
      LatePathIndexes[I] := StringIndex(FBuiltins.PathAt(
        FBuiltins.FingerprintCount + I));
      LateFingerprints[I] := FBuiltins.ComputeFingerprint(
        FBuiltins.FingerprintCount + I);
      LateFingerprintIndexes[I] := StringIndex(LateFingerprints[I]);
    end;
    for I := 0 to FCodecs.Count - 1 do
      StringIndex(FCodecs[I].Tag);
    SetLength(ModuleHashIndexes, FModuleRefs.Count);
    for I := 0 to FModuleRefs.Count - 1 do
      ModuleHashIndexes[I] := StringIndex(FTemplates.ModuleHash(FModuleRefs[I]));

    // Header, codecs, strings, built-in names, module records, modules,
    // objects, record payloads, roots.
    FOut := AStream;
    AStream.WriteBuffer(HEAP_SNAPSHOT_MAGIC[0], 4);
    WriteU16(HEAP_SNAPSHOT_FORMAT_VERSION);
    WriteRaw(FBuildIdentity);
    WriteU32(FStrings.Count);
    for I := 0 to FStrings.Count - 1 do
      WriteRaw(FStrings[I]);
    WriteU32(FCodecs.Count);
    for I := 0 to FCodecs.Count - 1 do
    begin
      WriteU32(StringIndex(FCodecs[I].Tag));
      WriteU16(FCodecs[I].SchemaVersion);
    end;
    WriteU32(FBuiltinRefs.Count);
    for I := 0 to FBuiltinRefs.Count - 1 do
      WriteU32(PathIndexes[I]);
    WriteU32(Length(LatePathIndexes));
    for I := 0 to High(LatePathIndexes) do
    begin
      WriteU32(LatePathIndexes[I]);
      WriteU32(LateFingerprintIndexes[I]);
    end;
    WriteU32(FRecords.Count);
    if FRecords.Count > 0 then
    begin
      WriteU16(FRecordCodec.SchemaVersion);
      AStream.CopyFrom(RecordHeadersOut, 0);
    end;
    WriteU32(Length(ModuleHashIndexes));
    for I := 0 to High(ModuleHashIndexes) do
      WriteU32(ModuleHashIndexes[I]);
    WriteU32(FObjects.Count);
    AStream.CopyFrom(ObjectsOut, 0);
    AStream.CopyFrom(RecordsOut, 0);
    AStream.CopyFrom(RootsOut, 0);
  finally
    FOut := nil;
    FCurrentId := -1;
    RootsOut.Free;
    Payload.Free;
    ObjectsOut.Free;
    RecordHeadersOut.Free;
    RecordsOut.Free;
  end;
end;

procedure TGocciaHeapSnapshotWriter.CheckAccounting(
  const AFirstOfRun: TGCManagedObject;
  const ABaseline: TGocciaHeapSnapshotBaseline;
  const ASkip: TGocciaSnapshotObjectFilter);
const
  MAX_DETAILS = 8;
var
  GC: TGarbageCollector;
  I, Index: Integer;
  Obj: TGCManagedObject;
  Missing: TStringList;
  Text: string;
begin
  GC := TGarbageCollector.Instance;
  Missing := TStringList.Create;
  try
    Missing.Sorted := True;
    Missing.Duplicates := dupIgnore;
    for I := AFirstOfRun.GCIndex + 1 to GC.ManagedObjectCount - 1 do
    begin
      Poll;
      Obj := GC.ManagedObjects[I];
      if not Assigned(Obj) or IsInlineValue(Obj) then
        Continue;
      if Assigned(ABaseline) and ABaseline.ContainsKey(Obj) then
        Continue;
      if FIds.ContainsKey(Obj) or (FBuiltins.IndexOf(Obj) >= 0) then
        Continue;
      if Assigned(ASkip) and ASkip(Obj) then
        Continue;
      if (Obj is TGocciaSymbolValue) and
         TGocciaSymbolValue(Obj).Registered then
        Continue;
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
        if I = MAX_DETAILS then
        begin
          Text := Text + ', ...';
          Break;
        end;
        if Text <> '' then
          Text := Text + ', ';
        Text := Text + Missing[I] + '=' + IntToStr(PtrInt(Missing.Objects[I]));
      end;
      raise EGocciaHeapSnapshotRefused.Create(
        'live objects the snapshot does not hold: ' + Text);
    end;
  finally
    Missing.Free;
  end;
end;

{ TGocciaHeapSnapshotReader }

constructor TGocciaHeapSnapshotReader.Create(
  const ABuiltins: TGocciaHeapSnapshotBuiltinTable;
  const ALimits: TGocciaHeapSnapshotLimits);
begin
  inherited Create;
  FBuiltins := ABuiltins;
  FLimits := ALimits;
  FBuildIdentity := HeapSnapshotBuildIdentity;
  FCreated := TList<TGCManagedObject>.Create;
  FNewRegistered := TDictionary<string, TGocciaSymbolValue>.Create;
  FScratch := TObjectDictionary<TObject, TObject>.Create([doOwnsValues]);
end;

destructor TGocciaHeapSnapshotReader.Destroy;
begin
  FRoots.Free;
  FCreated.Free;
  FNewRegistered.Free;
  FScratch.Free;
  ReleaseNativeBytes(FReservedBytes);
  inherited;
end;

procedure TGocciaHeapSnapshotReader.ReserveItems(const ACount,
  AElementSize: Int64);
begin
  Reserve(ACount, AElementSize);
end;

procedure TGocciaHeapSnapshotReader.HandOverReservation(const ABytes: Int64);
begin
  if (ABytes < 0) or (ABytes > FReservedBytes) then
    Refuse('a hand-over of more than the reader reserved');
  Dec(FReservedBytes, ABytes);
end;

function TGocciaHeapSnapshotReader.Scratch(const AKey: TObject): TObject;
begin
  if not FScratch.TryGetValue(AKey, Result) then
    Result := nil;
end;

procedure TGocciaHeapSnapshotReader.SetScratch(const AKey, AValue: TObject);
begin
  FScratch.AddOrSetValue(AKey, AValue);
end;

function TGocciaHeapSnapshotReader.GetObjectCount: Integer;
begin
  Result := Length(FObjects);
end;

procedure TGocciaHeapSnapshotReader.Refuse(const AReason: string);
begin
  raise EGocciaHeapSnapshotInvalid.Create('restore: ' + AReason);
end;

procedure TGocciaHeapSnapshotReader.Poll;
begin
  CheckNativeWork;
end;

procedure TGocciaHeapSnapshotReader.Need(const ACount: Int64);
begin
  if (ACount < 0) or (ACount > FLimit - FPos) then
    Refuse('truncated snapshot');
end;

// Charges ACount elements of AElementSize bytes to the run's memory ceiling
// before the reader allocates them; released when the reader is freed. These
// are the reader's own copies: its input, its tables, the string table's
// strings and a BigInt's limbs while it reads them. What the restored graph
// keeps is charged as the engine charges it anywhere else: each value and
// object when it registers with the collector, a string's backing store when
// the string value is created (TGocciaStringLiteralValue reserves it), and an
// array's elements through the element store's gate.
procedure TGocciaHeapSnapshotReader.Reserve(const ACount, AElementSize: Int64);
var
  Bytes: Int64;
begin
  Bytes := CheckedNativeByteSize(ACount, AElementSize);
  ReserveNativeBytes(Bytes);
  Inc(FReservedBytes, Bytes);
end;

// Makes room in the list of created values before a value is created: the
// charge can collect, and nothing roots a value until it is in the list.
procedure TGocciaHeapSnapshotReader.RoomForCreated;
const
  CHARGE_STEP = 4096;
begin
  if FCreated.Count >= FCreatedCharged then
  begin
    Reserve(CHARGE_STEP, SizeOf(Pointer));
    Inc(FCreatedCharged, CHARGE_STEP);
    FCreated.Capacity := FCreatedCharged;
  end;
end;

// Roots a value just created, in the room RoomForCreated made: no
// allocation, so no collection, between its creation and this.
procedure TGocciaHeapSnapshotReader.NoteCreated(const AObject: TGCManagedObject);
begin
  FCreated.Add(AObject);
end;

function TGocciaHeapSnapshotReader.ReadCount(const ALimit: Int64;
  const AMinBytesEach: Int64; const AWhat: string): Integer;
var
  Count: Cardinal;
begin
  Count := ReadU32;
  if Count > ALimit then
    Refuse('too many ' + AWhat);
  // Each item takes at least AMinBytesEach of what is left.
  if (AMinBytesEach > 0) and (Int64(Count) > (FLimit - FPos) div AMinBytesEach) then
    Refuse('more ' + AWhat + ' than the snapshot holds');
  Result := Integer(Count);
end;

function TGocciaHeapSnapshotReader.ReadItemCount(
  const AMinBytesEach: Integer): Integer;
begin
  Result := ReadCount(High(Integer), AMinBytesEach, 'items');
end;

function TGocciaHeapSnapshotReader.ReadU8: Byte;
begin
  Need(1);
  Result := FData[FPos];
  Inc(FPos);
end;

function TGocciaHeapSnapshotReader.ReadU16: Word;
begin
  Need(2);
  Move(FData[FPos], Result, 2);
  Inc(FPos, 2);
  Result := LEtoN(Result);
end;

function TGocciaHeapSnapshotReader.ReadU32: Cardinal;
begin
  Need(4);
  Move(FData[FPos], Result, 4);
  Inc(FPos, 4);
  Result := LEtoN(Result);
end;

function TGocciaHeapSnapshotReader.ReadI32: Integer;
begin
  Result := Integer(ReadU32);
end;

function TGocciaHeapSnapshotReader.ReadI64: Int64;
begin
  Need(8);
  Move(FData[FPos], Result, 8);
  Inc(FPos, 8);
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
    Refuse('a boolean that is neither 0 nor 1');
  Result := B = 1;
end;

// A string's UTF-16 code units, little-endian, decoded in chunks with the
// run's limits polled between.
function TGocciaHeapSnapshotReader.ReadRawString: string;
const
  CHUNK_UNITS = 65536;
var
  Len: Cardinal;
  I, Start, Count: Int64;
  Unit_: Word;
begin
  Len := ReadU32;
  if Int64(Len) * 2 > FLimits.MaxStringBytes then
    Refuse('a string longer than the limit');
  Need(Int64(Len) * 2);
  Reserve(Len, SizeOf(Char));
  SetLength(Result, Len);
  Start := 0;
  while Start < Len do
  begin
    Poll;
    Count := Min(Int64(CHUNK_UNITS), Int64(Len) - Start);
    for I := 0 to Count - 1 do
    begin
      Move(FData[FPos + (Start + I) * 2], Unit_, 2);
      Result[Start + I + 1] := Char(LEtoN(Unit_));
    end;
    Inc(Start, Count);
  end;
  Inc(FPos, Int64(Len) * 2);
end;

function TGocciaHeapSnapshotReader.ReadStr: string;
var
  Index: Cardinal;
begin
  Index := ReadU32;
  if Index >= Cardinal(Length(FStrings)) then
    Refuse('a string index out of range');
  Result := FStrings[Index];
end;

function TGocciaHeapSnapshotReader.ReadRef: TGCManagedObject;
var
  Tag: Byte;
  Index: Cardinal;
  Negative: Boolean;
  Count, I: Integer;
  Limbs: TLimbArray;
  Text: string;
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
      begin
        RoomForCreated;
        Result := TGocciaNumberLiteralValue.Create(ReadDouble);
        NoteCreated(Result);
      end;
    HSV_STRING:
      begin
        Text := ReadStr;
        RoomForCreated;
        Result := TGocciaStringLiteralValue.Create(Text);
        NoteCreated(Result);
      end;
    HSV_BIGINT:
      begin
        Negative := ReadBool;
        Count := ReadItemCount(4);
        Reserve(Count, 4);
        SetLength(Limbs, Count);
        for I := 0 to Count - 1 do
        begin
          if I and 4095 = 0 then
            Poll;
          Limbs[I] := ReadU32;
        end;
        // Canonical only, as the writer writes it: no leading zero limb, and
        // zero is not negative. The value then takes the limbs over as read.
        if (Count > 0) and (Limbs[Count - 1] = 0) then
          Refuse('a BigInt with a leading zero limb');
        if (Count = 0) and Negative then
          Refuse('a negative zero BigInt');
        RoomForCreated;
        Result := TGocciaBigIntValue.Create(
          TBigInteger.FromLimbs(Limbs, Negative));
        NoteCreated(Result);
        // The limbs live as long as the value: the reservation goes with
        // them rather than ending with the restore.
        TGocciaBigIntValue(Result).AdoptCharge(Int64(Count) * 4);
        Dec(FReservedBytes, Int64(Count) * 4);
      end;
    HSV_OBJECT:
      begin
        Index := ReadU32;
        if Index >= Cardinal(Length(FObjects)) then
          Refuse('an object reference out of range');
        Result := FObjects[Index];
      end;
    HSV_BUILTIN:
      begin
        Index := ReadU32;
        if Index >= Cardinal(Length(FBuiltinObjects)) then
          Refuse('a built-in reference out of range');
        Result := FBuiltinObjects[Index];
      end;
    HSV_REGISTERED_SYMBOL:
      begin
        Text := ReadStr;
        Result := FindRegisteredSymbol(Text);
        if not Assigned(Result) and
           not FNewRegistered.TryGetValue(Text, TGocciaSymbolValue(Result)) then
        begin
          Reserve(1, SEEN_ENTRY_BYTES);
          RoomForCreated;
          Result := TGocciaSymbolValue.Create(Text);
          NoteCreated(Result);
          FNewRegistered.Add(Text, TGocciaSymbolValue(Result));
        end;
      end;
  else
    Refuse('an unknown value tag ' + IntToStr(Tag));
    Result := nil;
  end;
end;

// A property's value: present (nil is no JavaScript value).
function TGocciaHeapSnapshotReader.ReadPresentValue: TGocciaValue;
begin
  Result := ReadValue;
  if not Assigned(Result) then
    Refuse('a value that is missing');
end;

function TGocciaHeapSnapshotReader.ReadElementValue: TGocciaValue;
begin
  Result := ReadValueOrHole;
  if not Assigned(Result) then
    Refuse('a value that is missing');
end;

// One half of an accessor: absent (nil or undefined) or a function.
function TGocciaHeapSnapshotReader.ReadAccessorHalf: TGocciaValue;
begin
  Result := ReadValue;
  if Assigned(Result) and not (Result is TGocciaUndefinedLiteralValue) and
     not Result.IsCallable then
    Refuse('an accessor half that is not a function');
end;

function TGocciaHeapSnapshotReader.ReadTemplate: TGocciaFunctionTemplate;
var
  Module, Index: Cardinal;
begin
  Module := ReadU32;
  Index := ReadU32;
  if Module >= Cardinal(Length(FModuleMap)) then
    Refuse('a module reference out of range');
  if Index >= Cardinal(FTemplates.TemplateCount(FModuleMap[Module])) then
    Refuse('a function template out of range');
  Result := FTemplates.TemplateAt(FModuleMap[Module], Index);
end;

function TGocciaHeapSnapshotReader.ReadRegister: TGocciaRegister;
var
  Kind: Byte;
begin
  Kind := ReadU8;
  if Kind > Ord(High(TGocciaRegisterKind)) then
    Refuse('an unknown register kind');
  Result := Default(TGocciaRegister);
  Result.Kind := TGocciaRegisterKind(Kind);
  case Result.Kind of
    grkBoolean:
      Result.BoolValue := ReadBool;
    grkInt:
      Result.IntValue := ReadI64;
    grkFloat:
      Result.FloatValue := ReadDouble;
    // A register may hold the hole: it marks a binding still in its
    // temporal dead zone.
    grkObject:
      Result.ObjectValue := ReadElementValue;
  end;
end;

function TGocciaHeapSnapshotReader.ReadModuleRecord: TObject;
var
  Index: Cardinal;
begin
  Index := ReadU32;
  if Index = 0 then
    Exit(nil);
  if Index > Cardinal(Length(FRecords)) then
    Refuse('a module record index out of range');
  Result := FRecords[Index - 1];
end;

function TGocciaHeapSnapshotReader.ReadValue: TGocciaValue;
begin
  Result := ReadValueOrHole;
  // The hole is the element store's marker, never a value of its own.
  if Result = TGocciaHoleValue.HoleValue then
    Refuse('an array hole outside an array''s elements');
end;

function TGocciaHeapSnapshotReader.ReadValueOrHole: TGocciaValue;
var
  Obj: TGCManagedObject;
begin
  Obj := ReadRef;
  if Assigned(Obj) and not (Obj is TGocciaValue) then
    Refuse('a value that is a ' + Obj.ClassName);
  Result := TGocciaValue(Obj);
end;

procedure TGocciaHeapSnapshotReader.ReadObjectCore(
  const AObject: TGocciaObjectValue);
const
  KNOWN_FLAGS = [pfEnumerable, pfConfigurable, pfWritable];
var
  Proto: TGocciaValue;
  Extensible, Frozen, Sealed, HadIndexed: Boolean;
  Count, I: Integer;
  Key: string;
  Pair: TGocciaPropertyMap.TKeyValuePair;
  SymbolDescriptor: TGocciaPropertyDescriptor;
  FlagBits: Byte;
  Flags: TPropertyFlags;
  Kind: Byte;
  Getter, Setter, Value: TGocciaValue;
  Symbol: TGocciaValue;
  Seen: TDictionary<string, Boolean>;
  Descriptor: TGocciaPropertyDescriptor;
begin
  Proto := ReadValue;
  if Assigned(Proto) and not (Proto is TGocciaObjectValue) then
    Refuse('a prototype that is not an object');
  Extensible := ReadBool;
  Frozen := ReadBool;
  Sealed := ReadBool;
  HadIndexed := ReadBool;
  // The state each flag implies must hold: frozen is sealed, sealed is not
  // extensible.
  if (Frozen and not Sealed) or (Sealed and Extensible) then
    Refuse('integrity flags that contradict each other');
  AObject.ClearOwnPropertiesForRestore;
  AObject.Prototype := TGocciaObjectValue(Proto);
  Count := ReadItemCount(MIN_PROPERTY_BYTES);
  // The duplicate check's table, charged for the most it can hold.
  Reserve(Count, SEEN_ENTRY_BYTES);
  Seen := TDictionary<string, Boolean>.Create;
  try
    for I := 1 to Count do
    begin
      Poll;
      Key := ReadStr;
      if Seen.ContainsKey(Key) then
        Refuse('a property listed twice');
      Seen.Add(Key, True);
      FlagBits := ReadU8;
      if FlagBits > Byte(KNOWN_FLAGS) then
        Refuse('unknown property attributes');
      Flags := TPropertyFlags(FlagBits);
      Kind := ReadU8;
      case Kind of
        HSD_ACCESSOR:
          begin
            if pfWritable in Flags then
              Refuse('an accessor marked writable');
            Getter := ReadAccessorHalf;
            Setter := ReadAccessorHalf;
            Descriptor := TGocciaPropertyDescriptorAccessor.Create(Getter,
              Setter, Flags);
          end;
        HSD_DATA:
          Descriptor := TGocciaPropertyDescriptorData.Create(ReadPresentValue,
            Flags);
      else
        Refuse('an unknown property kind');
        Descriptor := nil;
      end;
      // Owned by the map once added; freed here if adding it raised.
      try
        AObject.Properties.Add(Key, Descriptor);
      except
        Descriptor.Free;
        raise;
      end;
    end;
  finally
    Seen.Free;
  end;
  Count := ReadItemCount(MIN_PROPERTY_BYTES);
  for I := 1 to Count do
  begin
    Poll;
    Symbol := ReadValue;
    if not (Symbol is TGocciaSymbolValue) then
      Refuse('a symbol key that is not a symbol');
    if AObject.HasOwnSymbolPropertyDescriptor(TGocciaSymbolValue(Symbol)) then
      Refuse('a symbol property listed twice');
    FlagBits := ReadU8;
    if FlagBits > Byte(KNOWN_FLAGS) then
      Refuse('unknown property attributes');
    Flags := TPropertyFlags(FlagBits);
    Kind := ReadU8;
    case Kind of
      HSD_ACCESSOR:
        begin
          if pfWritable in Flags then
            Refuse('an accessor marked writable');
          Getter := ReadAccessorHalf;
          Setter := ReadAccessorHalf;
          AObject.DefineSymbolPropertyForRestore(TGocciaSymbolValue(Symbol),
            TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, Flags));
        end;
      HSD_DATA:
        begin
          Value := ReadPresentValue;
          AObject.DefineSymbolPropertyForRestore(TGocciaSymbolValue(Symbol),
            TGocciaPropertyDescriptorData.Create(Value, Flags));
        end;
    else
      Refuse('an unknown property kind');
    end;
  end;
  // Sealed and frozen are what the properties say they are: an object the
  // engine sealed has no configurable property, one it froze no writable
  // one either.
  if Sealed or Frozen then
  begin
    for Pair in AObject.Properties do
      if pfConfigurable in Pair.Value.Flags then
        Refuse('a sealed object with a configurable property');
    if Frozen then
      for Pair in AObject.Properties do
        if (Pair.Value is TGocciaPropertyDescriptorData) and
           (pfWritable in Pair.Value.Flags) then
          Refuse('a frozen object with a writable property');
    for I := 0 to AObject.StoredSymbolCount - 1 do
    begin
      Poll;
      SymbolDescriptor := AObject.GetOwnSymbolPropertyDescriptor(
        AObject.StoredSymbolAt(I));
      if pfConfigurable in SymbolDescriptor.Flags then
        Refuse('a sealed object with a configurable property');
      if Frozen and (SymbolDescriptor is TGocciaPropertyDescriptorData) and
         (pfWritable in SymbolDescriptor.Flags) then
        Refuse('a frozen object with a writable property');
    end;
  end;
  AObject.SetIntegrityForRestore(Extensible, Frozen, Sealed, HadIndexed);
end;

function TGocciaHeapSnapshotReader.Restore(
  const AStream: TStream): TGCManagedObjectArray;
var
  Size: Int64;
  RecordStart, RecordEnd: array of Int64;
  RecordLength: Cardinal;
  Magic: array[0..3] of AnsiChar;
  Version: Word;
  Identity, Path, Tag: string;
  Count, I, J: Integer;
  CodecIndex: Word;
  Schema: Word;
  PayloadLength: Cardinal;
  Roots: TRestoreRoots;
  RootStart: Int64;
  Seen: TDictionary<string, Boolean>;
  LatePaths: TStringArray;
  LateFingerprints: array of string;
  Pair: TPair<string, TGocciaSymbolValue>;
begin
  if Assigned(FRoots) then
    raise EInvalidOperation.Create('a heap snapshot reader restores once');
  Size := AStream.Size - AStream.Position;
  if Size > FLimits.MaxInputBytes then
    Refuse('the snapshot is larger than the limit');
  Reserve(Size, 1);
  SetLength(FData, Size);
  if Size > 0 then
    AStream.ReadBuffer(FData[0], Size);
  FPos := 0;
  FLimit := Size;

  Need(4);
  Move(FData[0], Magic, 4);
  FPos := 4;
  if Magic <> HEAP_SNAPSHOT_MAGIC then
    Refuse('not a heap snapshot');
  Version := ReadU16;
  if Version <> HEAP_SNAPSHOT_FORMAT_VERSION then
    Refuse('format version ' + IntToStr(Version) + ', this engine reads ' +
      IntToStr(HEAP_SNAPSHOT_FORMAT_VERSION));
  Identity := ReadRawString;
  if Identity <> FBuildIdentity then
    Refuse('written by engine build ' + Identity + ', this is ' +
      FBuildIdentity);

  // Strings.
  Count := ReadCount(FLimits.MaxStrings, MIN_STRING_BYTES, 'strings');
  Reserve(Count, SizeOf(string));
  SetLength(FStrings, Count);
  for I := 0 to Count - 1 do
  begin
    Poll;
    FStrings[I] := ReadRawString;
  end;

  // Codecs, each with the schema version it wrote.
  Count := ReadCount(FLimits.MaxCodecs, MIN_CODEC_BYTES, 'codecs');
  Reserve(Count, SizeOf(Pointer) + SEEN_ENTRY_BYTES);
  SetLength(FCodecs, Count);
  Seen := TDictionary<string, Boolean>.Create;
  try
    for I := 0 to Count - 1 do
    begin
      Poll;
      Tag := ReadStr;
      Schema := ReadU16;
      if Seen.ContainsKey(Tag) then
        Refuse('codec ' + Tag + ' listed twice');
      Seen.Add(Tag, True);
      FCodecs[I] := FindSnapshotCodecByTag(Tag);
      if not Assigned(FCodecs[I]) then
        Refuse('no codec ' + Tag + ' in this engine');
      if FCodecs[I].SchemaVersion <> Schema then
        Refuse('codec ' + Tag + ' schema ' + IntToStr(Schema) +
          ', this engine has ' + IntToStr(FCodecs[I].SchemaVersion));
    end;
  finally
    Seen.Free;
  end;

  // Built-ins, resolved by name.
  Count := ReadCount(FLimits.MaxBuiltins, 4, 'built-in references');
  Reserve(Count, SizeOf(Pointer));
  SetLength(FBuiltinObjects, Count);
  for I := 0 to Count - 1 do
  begin
    Poll;
    Path := ReadStr;
    if Assigned(FBuiltinResolve) then
      FBuiltinObjects[I] := FBuiltinResolve(Path)
    else
      FBuiltinObjects[I] := FBuiltins.FindPath(Path);
    if not Assigned(FBuiltinObjects[I]) then
      Refuse('no built-in ' + StringReplace(Path,
        HEAP_SNAPSHOT_PATH_SEPARATOR, '.', [rfReplaceAll]) + ' in this engine');
  end;

  // The built-ins the writing run built, each with its fingerprint there.
  Count := ReadCount(FLimits.MaxBuiltins, 8, 'built-ins the run built');
  if Count > 0 then
  begin
    Reserve(Count, 2 * SizeOf(Pointer));
    SetLength(LatePaths, Count);
    SetLength(LateFingerprints, Count);
    for I := 0 to Count - 1 do
    begin
      Poll;
      LatePaths[I] := ReadStr;
      LateFingerprints[I] := ReadStr;
    end;
    if not Assigned(FBuiltinVerify) then
      Refuse('built-ins the run built, and no way to check them');
    Path := FBuiltinVerify(LatePaths, LateFingerprints);
    if Path <> '' then
      Refuse('the run changed the built-in ' + StringReplace(Path,
        HEAP_SNAPSHOT_PATH_SEPARATOR, '.', [rfReplaceAll]) +
        ', or this engine does not build it the same way');
  end;

  // The run's module records, each allocated from its header: a record
  // codec can compile the module a header names, and add it to the
  // templates, before the modules below are looked up.
  Count := ReadCount(FLimits.MaxCodecs, MIN_RECORD_HEADER_BYTES,
    'module records');
  if Count > 0 then
  begin
    Schema := ReadU16;
    if not Assigned(FRecordCodec) then
      Refuse('module records, and no way to restore them');
    if FRecordCodec.SchemaVersion <> Schema then
      Refuse('module records schema ' + IntToStr(Schema) +
        ', this engine has ' + IntToStr(FRecordCodec.SchemaVersion));
    Reserve(Count, SizeOf(Pointer) + 2 * SizeOf(Int64));
    SetLength(FRecords, Count);
    SetLength(RecordStart, Count);
    SetLength(RecordEnd, Count);
    for I := 0 to Count - 1 do
    begin
      Poll;
      FRecords[I] := FRecordCodec.Allocate(Self);
      if not Assigned(FRecords[I]) then
        Refuse('the record codec allocated no module record');
    end;
  end;

  // The modules whose templates the snapshot's functions run.
  Count := ReadCount(FLimits.MaxCodecs, 4, 'modules');
  Reserve(Count, SizeOf(Integer));
  SetLength(FModuleMap, Count);
  for I := 0 to Count - 1 do
  begin
    Poll;
    Path := ReadStr;
    if Assigned(FTemplates) then
      FModuleMap[I] := FTemplates.FindModule(Path)
    else
      FModuleMap[I] := -1;
    if FModuleMap[I] < 0 then
      Refuse('a function whose module this engine has not compiled');
  end;

  // The object table.
  Count := ReadCount(FLimits.MaxObjects, MIN_OBJECT_BYTES, 'objects');
  Reserve(Count, SizeOf(Pointer) + SizeOf(Integer) + 2 * SizeOf(Int64));
  SetLength(FObjects, Count);
  SetLength(FObjectCodecs, Count);
  SetLength(FPayloadStart, Count);
  SetLength(FPayloadEnd, Count);

  Roots := TRestoreRoots.Create;
  Roots.Reader := Self;
  FRoots := Roots;

  // Pass 1: every payload's place, and an empty shell for each object.
  for I := 0 to Count - 1 do
  begin
    Poll;
    FLimit := Length(FData);
    CodecIndex := ReadU16;
    if CodecIndex >= Length(FCodecs) then
      Refuse('a codec index out of range');
    FObjectCodecs[I] := CodecIndex;
    PayloadLength := ReadU32;
    Need(PayloadLength);
    FPayloadStart[I] := FPos;
    FPayloadEnd[I] := FPos + PayloadLength;
    FLimit := FPayloadEnd[I];
    FObjects[I] := FCodecs[CodecIndex].Allocate(Self);
    if not Assigned(FObjects[I]) then
      Refuse('codec ' + FCodecs[CodecIndex].Tag + ' allocated nothing');
    FPos := FPayloadEnd[I];
  end;
  FLimit := Length(FData);
  // Where each record's payload is.
  for I := 0 to High(FRecords) do
  begin
    Poll;
    RecordLength := ReadU32;
    Need(RecordLength);
    RecordStart[I] := FPos;
    RecordEnd[I] := FPos + RecordLength;
    FPos := RecordEnd[I];
  end;
  RootStart := FPos;

  // Pass 2: fill.
  for I := 0 to Count - 1 do
  begin
    Poll;
    FPos := FPayloadStart[I];
    FLimit := FPayloadEnd[I];
    FCodecs[FObjectCodecs[I]].Fill(Self, FObjects[I]);
    if FPos <> FPayloadEnd[I] then
      Refuse('the payload of a ' + FCodecs[FObjectCodecs[I]].Tag +
        ' was not read to its end');
  end;

  // The module records, with every object filled.
  for I := 0 to High(FRecords) do
  begin
    Poll;
    FPos := RecordStart[I];
    FLimit := RecordEnd[I];
    FRecordCodec.Fill(Self, FRecords[I]);
    if FPos <> RecordEnd[I] then
      Refuse('the payload of a module record was not read to its end');
  end;

  // Pass 3: finish, the records first.
  for I := 0 to High(FRecords) do
  begin
    Poll;
    FPos := RecordStart[I];
    FLimit := RecordEnd[I];
    FRecordCodec.Finish(Self, FRecords[I]);
  end;
  for I := 0 to Count - 1 do
  begin
    Poll;
    FPos := FPayloadStart[I];
    FLimit := FPayloadEnd[I];
    FCodecs[FObjectCodecs[I]].Finish(Self, FObjects[I]);
  end;

  CheckPrototypeChains;

  // The roots.
  FPos := RootStart;
  FLimit := Length(FData);
  Count := ReadCount(FLimits.MaxRoots, MIN_VALUE_BYTES, 'roots');
  Reserve(Count, SizeOf(Pointer));
  SetLength(Result, Count);
  for J := 0 to Count - 1 do
  begin
    Poll;
    Result[J] := ReadRef;
  end;
  if FPos <> FLimit then
    Refuse('bytes after the roots');
  // The restore succeeded: the registered symbols it made are registered,
  // polling as any loop the input sizes does. A stop between two leaves
  // some registered and the rest not, which is harmless: a registered
  // symbol is only its key, and each is what Symbol.for(key) would have
  // made and pinned, under the same limits.
  for Pair in FNewRegistered do
  begin
    Poll;
    RegisterSymbol(Pair.Value);
  end;
  FNewRegistered.Clear;
  // The input is not needed any more; its reservation is released with the
  // reader.
  SetLength(FData, 0);
end;

// A snapshot sets prototypes without [[SetPrototypeOf]]'s cycle check, so a
// hostile one could make a chain that loops and hang every lookup through
// it. Each restored object's chain is followed once, iteratively, until it
// leaves the restored objects or meets one already checked; meeting one on
// the chain being followed is a cycle.
procedure TGocciaHeapSnapshotReader.CheckPrototypeChains;
const
  ON_PATH = 1;
  CHECKED = 2;
var
  State: THashMap<TGCManagedObject, Byte>;
  Path: TList<TGCManagedObject>;
  I, J: Integer;
  Current: TGCManagedObject;
  Mark: Byte;
begin
  // The state of each restored object and the chain being followed, charged
  // for the most they can hold.
  Reserve(Length(FObjects), CHAIN_ENTRY_BYTES);
  State := THashMap<TGCManagedObject, Byte>.Create;
  Path := TList<TGCManagedObject>.Create;
  try
    // Restored objects start unvisited; anything else ends a chain.
    for I := 0 to High(FObjects) do
    begin
      if I and 1023 = 0 then
        Poll;
      if FObjects[I] is TGocciaObjectValue then
        State.AddOrSetValue(FObjects[I], 0);
    end;
    for I := 0 to High(FObjects) do
    begin
      Poll;
      Current := FObjects[I];
      Path.Clear;
      while Assigned(Current) and State.TryGetValue(Current, Mark) and
            (Mark <> CHECKED) do
      begin
        if Path.Count and 1023 = 1023 then
          Poll;
        if Mark = ON_PATH then
          Refuse('a prototype chain that loops');
        State[Current] := ON_PATH;
        Path.Add(Current);
        Current := TGocciaObjectValue(Current).Prototype;
      end;
      for J := 0 to Path.Count - 1 do
        State[Path[J]] := CHECKED;
    end;
  finally
    Path.Free;
    State.Free;
  end;
end;

{ TPlainObjectCodec }

function TPlainObjectCodec.Tag: string;
begin
  Result := 'object';
end;

function TPlainObjectCodec.SchemaVersion: Word;
begin
  Result := 1;
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

function TSymbolCodec.SchemaVersion: Word;
begin
  Result := 1;
end;

procedure TSymbolCodec.Write(const AWriter: TGocciaHeapSnapshotWriter;
  const AObject: TGCManagedObject);
begin
  AWriter.WriteBool(TGocciaSymbolValue(AObject).HasDescription);
  AWriter.WriteStr(TGocciaSymbolValue(AObject).Description);
end;

// A symbol's description is read here, before any object is filled.
function TSymbolCodec.Allocate(
  const AReader: TGocciaHeapSnapshotReader): TGCManagedObject;
var
  HasDescription: Boolean;
begin
  HasDescription := AReader.ReadBool;
  Result := TGocciaSymbolValue.Create(AReader.ReadStr, HasDescription);
end;

procedure TSymbolCodec.Fill(const AReader: TGocciaHeapSnapshotReader;
  const AObject: TGCManagedObject);
begin
  AReader.ReadBool;
  AReader.ReadStr;
end;

initialization
  RegisterSnapshotCodec(TGocciaObjectValue, TPlainObjectCodec.Create);
  RegisterSnapshotCodec(TGocciaSymbolValue, TSymbolCodec.Create);

finalization
  FreeAndNil(GRegistry);
  FreeAndNil(GSlotMaterializers);
  FreeAndNil(GSlotGlobalPaths);

end.
