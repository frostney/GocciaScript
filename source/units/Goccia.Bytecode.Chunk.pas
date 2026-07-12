unit Goccia.Bytecode.Chunk;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  OrderedStringMap,

  Goccia.Bytecode.Debug;

type
  TGocciaBytecodeConstantKind = (
    bckNil,
    bckTrue,
    bckFalse,
    bckInteger,
    bckFloat,
    bckString,
    // ES2026 §13.2.8.3: lazily-built frozen template object, cached per call site.
    // Replaces the per-call OP_NEW_ARRAY + OP_FREEZE build sequence with a single
    // OP_LOAD_CONST.  CookedStrings/RawStrings are serialised; CachedValue is
    // runtime-only (starts nil, populated by the VM on first execution).
    // IntValue stores the cache slot index in TGocciaFunctionTemplate.
    bckTemplateObject,
    // ES2026 §13.2.7.1: regexp literal payload. Each evaluation creates a
    // fresh RegExp object, while IntValue identifies a runtime-only compiled
    // program cache slot on the owning TGocciaFunctionTemplate.
    bckRegExpLiteral,
    bckBigInt
  );

  TGocciaBytecodeStringArray = array of string;
  // TC39 Template Literal Revision: per-segment validity flags for cooked strings.
  TGocciaBytecodeTemplateCookedValid = array of Boolean;

  TGocciaBytecodeConstant = record
    Kind: TGocciaBytecodeConstantKind;
    IntValue: Int64;
    FloatValue: Double;
    StringValue: string;
    RegExpFlags: string;                                  // for bckRegExpLiteral
    CookedStrings: TGocciaBytecodeStringArray;          // for bckTemplateObject
    RawStrings: TGocciaBytecodeStringArray;             // for bckTemplateObject
    CookedValid: TGocciaBytecodeTemplateCookedValid;    // for bckTemplateObject
  end;

  TGocciaUpvalueDescriptor = record
    Name: string;
    IsLocal: Boolean;
    Index: UInt16;
  end;

  TGocciaDirectEvalBindingKind = (
    debLocal,
    debUpvalue,
    debGlobal,
    debWithLocal,
    debWithUpvalue
  );

  TGocciaDirectEvalBindingInfo = record
    Name: string;
    Kind: TGocciaDirectEvalBindingKind;
    Index: UInt16;
    IsConst: Boolean;
    IsVarEnvironmentBinding: Boolean;
    IsEvalSyntheticArguments: Boolean;
  end;

  TGocciaDirectEvalBindingArray = array of TGocciaDirectEvalBindingInfo;

  TGocciaDirectEvalEnvironment = record
    PC: UInt32;
    RejectArgumentsReference: Boolean;
    Bindings: TGocciaDirectEvalBindingArray;
  end;

  TGocciaExceptionHandler = record
    TryStart: UInt32;
    TryEnd: UInt32;
    CatchTarget: UInt32;
    FinallyTarget: UInt32;
    CatchRegister: UInt16;
  end;

  TGocciaLocalType = (
    sltUntyped,
    sltInteger,
    sltFloat,
    sltBoolean,
    sltString,
    sltReference
  );

  // Runtime-only inline cache for OP_GET_GLOBAL sites, indexed by the
  // instruction's name-constant index.  Scope is a weak pointer compared for
  // identity only (never dereferenced); Version/EntryIndex validate against
  // the scope's lexical binding map.  Not serialised to .gbc.
  TGocciaGlobalReadCacheEntry = record
    Scope: Pointer;
    Version: Cardinal;
    EntryIndex: Integer;
  end;
  PGocciaGlobalReadCacheEntry = ^TGocciaGlobalReadCacheEntry;

  // Runtime-only inline cache for OP_GET_PROP_CONST sites, indexed by the
  // instruction's name-constant index.  Shape is a weak pointer to an
  // interned per-realm shape (Goccia.Values.Shape), compared for identity
  // only and never dereferenced.  No version stamp is needed: same shape
  // implies same key at the same entry index (shaped maps are append-only;
  // delete/clear leave shaped mode), shapes are never freed within an
  // engine's lifetime, and function templates never outlive their engine —
  // so a cached pointer can never validate against a recycled address.
  // Fills never store nil or the dictionary sentinel.  MissStreak counts
  // consecutive misses (different-shape refills and fill declines); every
  // validated hit resets it, so transient warm-up polymorphism cannot
  // permanently disable a site.  Once it saturates the site is treated as
  // megamorphic and reads go through the uncached own-data fast path
  // instead of thrashing the cache.  Not serialised to .gbc.
  TGocciaPropertyReadCacheEntry = record
    Shape: Pointer;
    EntryIndex: Integer;
    MissStreak: Byte;
  end;
  PGocciaPropertyReadCacheEntry = ^TGocciaPropertyReadCacheEntry;

  // Runtime-only inline cache for OP_GET_PROP_CONST sites that resolve on
  // the receiver's prototype chain (methods on class prototype objects are
  // the dominant case). Shapes[0] is the receiver's own shape — proving
  // continued ABSENCE of the name on the receiver — and Shapes[1..Holder-
  // Level] are the chain shapes, proving absence below the holder and
  // presence at the holder. A shape is the key set, so an unchanged fresh
  // shape proves absence/presence regardless of object identity, which
  // keeps the cache valid across setPrototypeOf to a same-shaped
  // prototype; the hit path walks the live chain, so link changes are
  // followed inherently. The holder's descriptor is re-read by entry index
  // on every hit. HolderLevel = 0 means the slot is empty. All pointers
  // are weak and compared for identity only. Not serialised to .gbc.
  TGocciaProtoReadCacheEntry = record
    Shapes: array [0 .. 2] of Pointer;
    EntryIndex: Integer;
    HolderLevel: Byte;
    MissStreak: Byte;
  end;
  PGocciaProtoReadCacheEntry = ^TGocciaProtoReadCacheEntry;

  TGocciaFunctionTemplate = class
  private
    FName: string;
    FCode: array of UInt32;
    FCodeCount: Integer;
    FConstants: array of TGocciaBytecodeConstant;
    FConstantCount: Integer;
    FFunctions: TObjectList<TGocciaFunctionTemplate>;
    FUpvalueDescriptors: array of TGocciaUpvalueDescriptor;
    FDirectEvalEnvironments: array of TGocciaDirectEvalEnvironment;
    FDirectEvalEnvironmentCount: Integer;
    FExceptionHandlers: array of TGocciaExceptionHandler;
    FExceptionHandlerCount: Integer;
    FMaxRegisters: UInt16;
    FParameterCount: UInt16;
    FFormalParameterCount: UInt16;
    FUpvalueCount: UInt16;
    FDebugInfo: TGocciaDebugInfo;
    FLocalTypes: array of TGocciaLocalType;
    FLocalTypeCount: UInt16;
    FLocalStrictFlags: array of Boolean;
    FLocalStrictCount: UInt16;
    FIsAsync: Boolean;
    FIsGenerator: Boolean;
    FIsArrow: Boolean;
    FHasOwnPrototype: Boolean;
    FStrictThis: Boolean;
    FStrictCode: Boolean;
    FParameterPreambleSize: UInt16;
    FTypeCheckPreambleSize: UInt16;
    FDirectEvalSyntheticArgumentsSlot: Integer;
    FDerivedThisInitializedSlot: Integer;
    FRejectArgumentsInDirectEval: Boolean;
    FProfileIndex: Integer;
    FSourceText: string;
    FTemplateSiteId: UInt64;
    FStringConstantIndex: TOrderedStringMap<UInt16>;
    // Runtime-only cache for bckTemplateObject constants.  Indexed by the slot
    // number stored in the constant's IntValue field.  Not serialised to .gbc.
    FTemplateObjectCaches: array of TObject;  // TGocciaValue — typed as TObject to keep GC import out of interface
    FTemplateObjectCacheCount: Integer;
    FRegExpProgramCaches: array of TObject;
    FRegExpProgramCacheCount: Integer;
    FGlobalReadCaches: array of TGocciaGlobalReadCacheEntry;
    // Property/proto read caches use DENSE slots: OP_GET_PROP_CONST name
    // constants are a small subset of the constant pool, so a per-constant
    // UInt16 map (0 = unassigned, else dense slot + 1) assigns slots on
    // first use and the entry arrays grow only to the number of distinct
    // property-name constants actually read. Both tiers share one slot id
    // (they are keyed by the same instruction operand). The instruction's
    // OP_WIDE allows every constant-pool entry to name a cached property read.
    FPropertyReadSlotMap: array of UInt32;
    FPropertyReadCaches: array of TGocciaPropertyReadCacheEntry;
    FProtoReadCaches: array of TGocciaProtoReadCacheEntry;
    FPropertyReadSlotCount: Integer;
    function PropertyReadSlot(const AConstIndex: Integer): Integer;
    function GetFunctionCount: Integer;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    function EmitInstruction(const AInstruction: UInt64;
      const AForceWide: Boolean = False): Integer;
    procedure PatchInstruction(const AIndex: Integer; const AInstruction: UInt64);
    function AddConstantNil: UInt16;
    function AddConstantBoolean(const AValue: Boolean): UInt16;
    function AddConstantInteger(const AValue: Int64): UInt16;
    function AddConstantFloat(const AValue: Double): UInt16;
    function AddConstantString(const AValue: string): UInt16;
    function AddConstantBigInt(const AValue: string): UInt16;
    // ES2026 §13.2.8.3: add a bckTemplateObject constant that the VM will lazily
    // build and cache on first execution.  Each call site gets its own entry;
    // no deduplication is performed.
    function AddConstantTemplateObject(const ACookedStrings, ARawStrings: array of string;
      const ACookedValid: array of Boolean): UInt16;
    function AddConstantRegExpLiteral(const APattern, AFlags: string): UInt16;
    function GetTemplateObjectCache(const ASlot: Integer): TObject;
    procedure SetTemplateObjectCache(const ASlot: Integer; const AValue: TObject);
    function GetRegExpProgramCache(const ASlot: Integer): TObject;
    procedure SetRegExpProgramCache(const ASlot: Integer; const AValue: TObject);
    function GlobalReadCacheSlot(
      const AConstIndex: Integer): PGocciaGlobalReadCacheEntry; inline;
    function PropertyReadCacheSlot(
      const AConstIndex: Integer): PGocciaPropertyReadCacheEntry; inline;
    function ProtoReadCacheSlot(
      const AConstIndex: Integer): PGocciaProtoReadCacheEntry; inline;
    function AddFunction(const AFunction: TGocciaFunctionTemplate): UInt16;
    procedure AddUpvalueDescriptor(const AIsLocal: Boolean; const AIndex: UInt16;
      const AName: string = '');
    procedure AddDirectEvalEnvironment(const APC: UInt32;
      const ARejectArgumentsReference: Boolean;
      const ABindings: TGocciaDirectEvalBindingArray);
    procedure AddExceptionHandler(const ATryStart, ATryEnd, ACatchTarget,
      AFinallyTarget: UInt32; const ACatchRegister: UInt16);

    function GetInstruction(const AIndex: Integer): UInt32; inline;
    function GetConstant(const AIndex: Integer): TGocciaBytecodeConstant; inline;
    function GetFunction(const AIndex: Integer): TGocciaFunctionTemplate;
    function GetInstructionUnchecked(const AIndex: Integer): UInt32; inline;
    function GetConstantUnchecked(const AIndex: Integer): TGocciaBytecodeConstant; inline;
    function GetFunctionUnchecked(const AIndex: Integer): TGocciaFunctionTemplate; inline;
    function GetUpvalueDescriptor(const AIndex: Integer): TGocciaUpvalueDescriptor;
    function GetDirectEvalEnvironment(
      const AIndex: Integer): TGocciaDirectEvalEnvironment;
    function FindDirectEvalEnvironment(const APC: UInt32;
      out AIndex: Integer): Boolean;
    function GetExceptionHandler(const AIndex: Integer): TGocciaExceptionHandler;

    property Name: string read FName write FName;
    property CodeCount: Integer read FCodeCount;
    property ConstantCount: Integer read FConstantCount;
    property FunctionCount: Integer read GetFunctionCount;
    property ExceptionHandlerCount: Integer read FExceptionHandlerCount;
    property MaxRegisters: UInt16 read FMaxRegisters write FMaxRegisters;
    property ParameterCount: UInt16 read FParameterCount write FParameterCount;
    property FormalParameterCount: UInt16 read FFormalParameterCount write FFormalParameterCount;
    property UpvalueCount: UInt16 read FUpvalueCount;
    property DirectEvalEnvironmentCount: Integer read FDirectEvalEnvironmentCount;
    property DebugInfo: TGocciaDebugInfo read FDebugInfo write FDebugInfo;

    procedure SetLocalType(const ASlot: UInt16; const AKind: TGocciaLocalType);
    function GetLocalType(const ASlot: UInt16): TGocciaLocalType;
    property LocalTypeCount: UInt16 read FLocalTypeCount;

    procedure SetLocalStrictFlag(const ASlot: UInt16; const AStrict: Boolean);
    function GetLocalStrictFlag(const ASlot: UInt16): Boolean;
    property LocalStrictCount: UInt16 read FLocalStrictCount;

    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property IsGenerator: Boolean read FIsGenerator write FIsGenerator;
    property IsArrow: Boolean read FIsArrow write FIsArrow;
    property StrictThis: Boolean read FStrictThis write FStrictThis;
    property StrictCode: Boolean read FStrictCode write FStrictCode;
    // True when this template represents a `function`/`function*` declaration or
    // expression (or async generator).  Per ES2026 §10.2.5 MakeConstructor, such
    // functions get an own `prototype` property installed at closure-creation
    // time pointing at a fresh ordinary object whose `constructor` is the
    // function itself.
    property HasOwnPrototype: Boolean read FHasOwnPrototype write FHasOwnPrototype;
    property ParameterPreambleSize: UInt16 read FParameterPreambleSize write FParameterPreambleSize;
    property TypeCheckPreambleSize: UInt16 read FTypeCheckPreambleSize write FTypeCheckPreambleSize;
    property DirectEvalSyntheticArgumentsSlot: Integer read FDirectEvalSyntheticArgumentsSlot write FDirectEvalSyntheticArgumentsSlot;
    property DerivedThisInitializedSlot: Integer read FDerivedThisInitializedSlot write FDerivedThisInitializedSlot;
    property RejectArgumentsInDirectEval: Boolean read FRejectArgumentsInDirectEval write FRejectArgumentsInDirectEval;
    property ProfileIndex: Integer read FProfileIndex write FProfileIndex;
    property SourceText: string read FSourceText write FSourceText;
    property TemplateSiteId: UInt64 read FTemplateSiteId;
  end;

implementation

uses
  SysUtils,

  Goccia.Bytecode,
  Goccia.GarbageCollector;

var
  GTemplateSiteIdLock: TRTLCriticalSection;
  GNextTemplateSiteId: UInt64;

function AllocateTemplateSiteId: UInt64;
begin
  EnterCriticalSection(GTemplateSiteIdLock);
  try
    Inc(GNextTemplateSiteId);
    Result := GNextTemplateSiteId;
  finally
    LeaveCriticalSection(GTemplateSiteIdLock);
  end;
end;

function FloatBitsAreNaN(const AValue: Double): Boolean; inline;
var
  Bits: UInt64;
begin
  Move(AValue, Bits, SizeOf(Double));
  Result := ((Bits and $7FF0000000000000) = $7FF0000000000000) and
            ((Bits and $000FFFFFFFFFFFFF) <> 0);
end;

constructor TGocciaFunctionTemplate.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FCodeCount := 0;
  FConstantCount := 0;
  FFunctions := TObjectList<TGocciaFunctionTemplate>.Create(True);
  FStringConstantIndex := TOrderedStringMap<UInt16>.Create;
  FDirectEvalEnvironmentCount := 0;
  FExceptionHandlerCount := 0;
  FMaxRegisters := 0;
  FParameterCount := 0;
  FFormalParameterCount := 0;
  FUpvalueCount := 0;
  FDebugInfo := nil;
  FLocalTypeCount := 0;
  FLocalStrictCount := 0;
  FIsArrow := False;
  FHasOwnPrototype := False;
  FStrictThis := True;
  FStrictCode := True;
  FParameterPreambleSize := 0;
  FTypeCheckPreambleSize := 0;
  FDirectEvalSyntheticArgumentsSlot := -1;
  FDerivedThisInitializedSlot := -1;
  FRejectArgumentsInDirectEval := False;
  FProfileIndex := -1;
  FTemplateSiteId := AllocateTemplateSiteId;
  FRegExpProgramCacheCount := 0;
end;

destructor TGocciaFunctionTemplate.Destroy;
var
  I: Integer;
begin
  // Unpin any template objects that were built and cached during VM execution.
  // Guard against the GC already having been shut down.
  if Assigned(TGarbageCollector.Instance) then
    for I := 0 to FTemplateObjectCacheCount - 1 do
      if Assigned(FTemplateObjectCaches[I]) then
        TGarbageCollector.Instance.UnpinObject(
          TGCManagedObject(FTemplateObjectCaches[I]));
  for I := 0 to FRegExpProgramCacheCount - 1 do
    FRegExpProgramCaches[I].Free;
  FStringConstantIndex.Free;
  FFunctions.Free;
  FDebugInfo.Free;
  inherited;
end;

function TGocciaFunctionTemplate.EmitInstruction(
  const AInstruction: UInt64; const AForceWide: Boolean): Integer;
var
  HighOperands: UInt32;
begin
  HighOperands := UInt32(AInstruction shr 32);
  if AForceWide or (HighOperands <> 0) then
  begin
    if FCodeCount >= Length(FCode) then
      SetLength(FCode, FCodeCount * 2 + 16);
    FCode[FCodeCount] := UInt32(Ord(OP_WIDE)) or
      ((HighOperands and $FF) shl 8) or
      (((HighOperands shr 8) and $FF) shl 16) or
      (((HighOperands shr 16) and $FF) shl 24);
    Inc(FCodeCount);
  end;
  if FCodeCount >= Length(FCode) then
    SetLength(FCode, FCodeCount * 2 + 16);
  FCode[FCodeCount] := UInt32(AInstruction and $FFFFFFFF);
  Result := FCodeCount;
  Inc(FCodeCount);
end;

procedure TGocciaFunctionTemplate.PatchInstruction(const AIndex: Integer;
  const AInstruction: UInt64);
var
  HighOperands: UInt32;
begin
  if (AIndex < 0) or (AIndex >= FCodeCount) then
    raise ERangeError.CreateFmt('PatchInstruction: index %d out of range 0..%d',
      [AIndex, FCodeCount - 1]);
  HighOperands := UInt32(AInstruction shr 32);
  if HighOperands <> 0 then
  begin
    if (AIndex = 0) or (DecodeOp(FCode[AIndex - 1]) <> Ord(OP_WIDE)) then
      raise ERangeError.Create(
        'PatchInstruction: wide instruction has no prefix slot');
    FCode[AIndex - 1] := UInt32(Ord(OP_WIDE)) or
      ((HighOperands and $FF) shl 8) or
      (((HighOperands shr 8) and $FF) shl 16) or
      (((HighOperands shr 16) and $FF) shl 24);
  end;
  FCode[AIndex] := UInt32(AInstruction and $FFFFFFFF);
end;

function TGocciaFunctionTemplate.AddConstantNil: UInt16;
var
  I: Integer;
begin
  for I := 0 to FConstantCount - 1 do
    if FConstants[I].Kind = bckNil then
      Exit(UInt16(I));

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckNil;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TGocciaFunctionTemplate.AddConstantBoolean(
  const AValue: Boolean): UInt16;
var
  I: Integer;
  Target: TGocciaBytecodeConstantKind;
begin
  if AValue then
    Target := bckTrue
  else
    Target := bckFalse;

  for I := 0 to FConstantCount - 1 do
    if FConstants[I].Kind = Target then
      Exit(UInt16(I));

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := Target;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TGocciaFunctionTemplate.AddConstantInteger(
  const AValue: Int64): UInt16;
var
  I: Integer;
begin
  for I := 0 to FConstantCount - 1 do
    if (FConstants[I].Kind = bckInteger) and (FConstants[I].IntValue = AValue) then
      Exit(UInt16(I));

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckInteger;
  FConstants[FConstantCount].IntValue := AValue;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TGocciaFunctionTemplate.AddConstantFloat(
  const AValue: Double): UInt16;
var
  I: Integer;
  ValueIsNaN: Boolean;
begin
  ValueIsNaN := FloatBitsAreNaN(AValue);
  for I := 0 to FConstantCount - 1 do
    if (FConstants[I].Kind = bckFloat) and
       (FloatBitsAreNaN(FConstants[I].FloatValue) = ValueIsNaN) then
    begin
      if ValueIsNaN or (FConstants[I].FloatValue = AValue) then
        Exit(UInt16(I));
    end;

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckFloat;
  FConstants[FConstantCount].FloatValue := AValue;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TGocciaFunctionTemplate.AddConstantString(
  const AValue: string): UInt16;
begin
  if FStringConstantIndex.TryGetValue(AValue, Result) then
    Exit;

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckString;
  FConstants[FConstantCount].StringValue := AValue;
  Result := UInt16(FConstantCount);
  FStringConstantIndex.Add(AValue, Result);
  Inc(FConstantCount);
end;

function TGocciaFunctionTemplate.AddConstantBigInt(
  const AValue: string): UInt16;
begin
  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckBigInt;
  FConstants[FConstantCount].StringValue := AValue;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

// ES2026 §13.2.8.3 GetTemplateObject(templateLiteral)
function TGocciaFunctionTemplate.AddConstantTemplateObject(
  const ACookedStrings, ARawStrings: array of string;
  const ACookedValid: array of Boolean): UInt16;
var
  K: Integer;
begin
  if Length(ACookedStrings) <> Length(ARawStrings) then
    raise Exception.Create(
      'Template payload length mismatch: cooked vs raw');
  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckTemplateObject;
  // IntValue holds the runtime cache slot index in FTemplateObjectCaches
  FConstants[FConstantCount].IntValue := FTemplateObjectCacheCount;
  SetLength(FConstants[FConstantCount].CookedStrings, Length(ACookedStrings));
  for K := 0 to High(ACookedStrings) do
    FConstants[FConstantCount].CookedStrings[K] := ACookedStrings[K];
  SetLength(FConstants[FConstantCount].RawStrings, Length(ARawStrings));
  for K := 0 to High(ARawStrings) do
    FConstants[FConstantCount].RawStrings[K] := ARawStrings[K];
  // TC39 Template Literal Revision: per-segment cooked validity flags
  SetLength(FConstants[FConstantCount].CookedValid, Length(ACookedValid));
  for K := 0 to High(ACookedValid) do
    FConstants[FConstantCount].CookedValid[K] := ACookedValid[K];
  // Grow the runtime cache; the new slot starts nil (built on first VM execution)
  if FTemplateObjectCacheCount >= Length(FTemplateObjectCaches) then
    SetLength(FTemplateObjectCaches, FTemplateObjectCacheCount * 2 + 4);
  Inc(FTemplateObjectCacheCount);
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

// ES2026 §13.2.7.1 Runtime Semantics: Evaluation
function TGocciaFunctionTemplate.AddConstantRegExpLiteral(
  const APattern, AFlags: string): UInt16;
begin
  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckRegExpLiteral;
  FConstants[FConstantCount].StringValue := APattern;
  FConstants[FConstantCount].RegExpFlags := AFlags;
  FConstants[FConstantCount].IntValue := FRegExpProgramCacheCount;

  if FRegExpProgramCacheCount >= Length(FRegExpProgramCaches) then
    SetLength(FRegExpProgramCaches, FRegExpProgramCacheCount * 2 + 4);
  Inc(FRegExpProgramCacheCount);

  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TGocciaFunctionTemplate.GetTemplateObjectCache(const ASlot: Integer): TObject;
begin
  if (ASlot >= 0) and (ASlot < FTemplateObjectCacheCount) then
    Result := FTemplateObjectCaches[ASlot]
  else
    Result := nil;
end;

procedure TGocciaFunctionTemplate.SetTemplateObjectCache(const ASlot: Integer;
  const AValue: TObject);
begin
  if (ASlot >= 0) and (ASlot < FTemplateObjectCacheCount) then
    FTemplateObjectCaches[ASlot] := AValue;
end;

function TGocciaFunctionTemplate.GetRegExpProgramCache(
  const ASlot: Integer): TObject;
begin
  if (ASlot >= 0) and (ASlot < FRegExpProgramCacheCount) then
    Result := FRegExpProgramCaches[ASlot]
  else
    Result := nil;
end;

procedure TGocciaFunctionTemplate.SetRegExpProgramCache(const ASlot: Integer;
  const AValue: TObject);
begin
  if (ASlot >= 0) and (ASlot < FRegExpProgramCacheCount) then
    FRegExpProgramCaches[ASlot] := AValue;
end;

function TGocciaFunctionTemplate.GlobalReadCacheSlot(
  const AConstIndex: Integer): PGocciaGlobalReadCacheEntry;
begin
  // Out-of-range constant indices (possible only with corrupt or hostile
  // .gbc input) must not yield a pointer past the array end — the VM writes
  // through this pointer, so that would be a wild heap write in production
  // builds.  nil tells the caller to run uncached.
  if (AConstIndex < 0) or (AConstIndex >= FConstantCount) then
    Exit(nil);
  // Lazily sized to the constant pool on first use; constants are fixed by
  // the time the VM executes, so the array never needs to grow again.
  // SetLength zero-fills, so fresh slots have Scope = nil and always miss.
  if AConstIndex >= Length(FGlobalReadCaches) then
    SetLength(FGlobalReadCaches, FConstantCount);
  Result := @FGlobalReadCaches[AConstIndex];
end;

function TGocciaFunctionTemplate.PropertyReadSlot(
  const AConstIndex: Integer): Integer;
var
  NewCapacity: Integer;
begin
  // nil-slot contract as GlobalReadCacheSlot: out-of-range constant
  // indices (corrupt or hostile .gbc input) run uncached rather than
  // risking a wild write.
  if (AConstIndex < 0) or (AConstIndex >= FConstantCount) then
    Exit(-1);
  if AConstIndex >= Length(FPropertyReadSlotMap) then
    SetLength(FPropertyReadSlotMap, FConstantCount);
  if FPropertyReadSlotMap[AConstIndex] = 0 then
  begin
    // Grow BOTH dense arrays together so a held own-tier pointer stays
    // valid while the proto-tier slot for the same instruction is fetched.
    // Re-entrant slot assignment (user code re-entering this template) can
    // still reallocate them, so slot pointers must never be used after a
    // call that may run user code.
    if FPropertyReadSlotCount >= Length(FPropertyReadCaches) then
    begin
      NewCapacity := Length(FPropertyReadCaches) * 2;
      if NewCapacity < 4 then
        NewCapacity := 4;
      SetLength(FPropertyReadCaches, NewCapacity);
      SetLength(FProtoReadCaches, NewCapacity);
    end;
    Inc(FPropertyReadSlotCount);
    FPropertyReadSlotMap[AConstIndex] := UInt32(FPropertyReadSlotCount);
  end;
  Result := Integer(FPropertyReadSlotMap[AConstIndex]) - 1;
end;

function TGocciaFunctionTemplate.PropertyReadCacheSlot(
  const AConstIndex: Integer): PGocciaPropertyReadCacheEntry;
var
  Slot: Integer;
begin
  Slot := PropertyReadSlot(AConstIndex);
  if Slot < 0 then
    Exit(nil);
  Result := @FPropertyReadCaches[Slot];
end;

function TGocciaFunctionTemplate.ProtoReadCacheSlot(
  const AConstIndex: Integer): PGocciaProtoReadCacheEntry;
var
  Slot: Integer;
begin
  Slot := PropertyReadSlot(AConstIndex);
  if Slot < 0 then
    Exit(nil);
  Result := @FProtoReadCaches[Slot];
end;

function TGocciaFunctionTemplate.AddFunction(
  const AFunction: TGocciaFunctionTemplate): UInt16;
begin
  if FFunctions.Count > High(UInt16) then
    raise Exception.Create('Function pool overflow: exceeds 65535 entries');
  Result := UInt16(FFunctions.Count);
  FFunctions.Add(AFunction);
end;

procedure TGocciaFunctionTemplate.AddUpvalueDescriptor(
  const AIsLocal: Boolean; const AIndex: UInt16; const AName: string);
begin
  if FUpvalueCount >= High(UInt16) then
    raise Exception.Create('Upvalue descriptor overflow: exceeds 65535 entries');
  if FUpvalueCount >= Length(FUpvalueDescriptors) then
    SetLength(FUpvalueDescriptors, FUpvalueCount * 2 + 4);
  FUpvalueDescriptors[FUpvalueCount].Name := AName;
  FUpvalueDescriptors[FUpvalueCount].IsLocal := AIsLocal;
  FUpvalueDescriptors[FUpvalueCount].Index := AIndex;
  Inc(FUpvalueCount);
end;

procedure TGocciaFunctionTemplate.AddDirectEvalEnvironment(const APC: UInt32;
  const ARejectArgumentsReference: Boolean;
  const ABindings: TGocciaDirectEvalBindingArray);
var
  I: Integer;
begin
  if FDirectEvalEnvironmentCount >= High(UInt16) then
    raise Exception.Create('Direct eval environment overflow: exceeds 65535 entries');
  if FDirectEvalEnvironmentCount >= Length(FDirectEvalEnvironments) then
    SetLength(FDirectEvalEnvironments, FDirectEvalEnvironmentCount * 2 + 4);
  FDirectEvalEnvironments[FDirectEvalEnvironmentCount].PC := APC;
  FDirectEvalEnvironments[FDirectEvalEnvironmentCount].RejectArgumentsReference :=
    ARejectArgumentsReference;
  SetLength(FDirectEvalEnvironments[FDirectEvalEnvironmentCount].Bindings,
    Length(ABindings));
  for I := 0 to High(ABindings) do
    FDirectEvalEnvironments[FDirectEvalEnvironmentCount].Bindings[I] :=
      ABindings[I];
  Inc(FDirectEvalEnvironmentCount);
end;

procedure TGocciaFunctionTemplate.AddExceptionHandler(
  const ATryStart, ATryEnd, ACatchTarget, AFinallyTarget: UInt32;
  const ACatchRegister: UInt16);
begin
  if FExceptionHandlerCount >= Length(FExceptionHandlers) then
    SetLength(FExceptionHandlers, FExceptionHandlerCount * 2 + 4);
  FExceptionHandlers[FExceptionHandlerCount].TryStart := ATryStart;
  FExceptionHandlers[FExceptionHandlerCount].TryEnd := ATryEnd;
  FExceptionHandlers[FExceptionHandlerCount].CatchTarget := ACatchTarget;
  FExceptionHandlers[FExceptionHandlerCount].FinallyTarget := AFinallyTarget;
  FExceptionHandlers[FExceptionHandlerCount].CatchRegister := ACatchRegister;
  Inc(FExceptionHandlerCount);
end;

function TGocciaFunctionTemplate.GetInstruction(const AIndex: Integer): UInt32;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FCodeCount) then
    raise ERangeError.CreateFmt('GetInstruction: index %d out of range 0..%d',
      [AIndex, FCodeCount - 1]);
  {$ENDIF}
  Result := FCode[AIndex];
end;

function TGocciaFunctionTemplate.GetConstant(
  const AIndex: Integer): TGocciaBytecodeConstant;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FConstantCount) then
    raise ERangeError.CreateFmt('GetConstant: index %d out of range 0..%d',
      [AIndex, FConstantCount - 1]);
  {$ENDIF}
  Result := FConstants[AIndex];
end;

function TGocciaFunctionTemplate.GetConstantUnchecked(
  const AIndex: Integer): TGocciaBytecodeConstant;
begin
  Result := FConstants[AIndex];
end;

function TGocciaFunctionTemplate.GetFunction(
  const AIndex: Integer): TGocciaFunctionTemplate;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FFunctions.Count) then
    raise ERangeError.CreateFmt('GetFunction: index %d out of range 0..%d',
      [AIndex, FFunctions.Count - 1]);
  {$ENDIF}
  Result := FFunctions[AIndex];
end;

function TGocciaFunctionTemplate.GetFunctionUnchecked(
  const AIndex: Integer): TGocciaFunctionTemplate;
begin
  Result := FFunctions[AIndex];
end;

function TGocciaFunctionTemplate.GetInstructionUnchecked(
  const AIndex: Integer): UInt32;
begin
  Result := FCode[AIndex];
end;

function TGocciaFunctionTemplate.GetUpvalueDescriptor(
  const AIndex: Integer): TGocciaUpvalueDescriptor;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FUpvalueCount) then
    raise ERangeError.CreateFmt('GetUpvalueDescriptor: index %d out of range 0..%d',
      [AIndex, FUpvalueCount - 1]);
  {$ENDIF}
  Result := FUpvalueDescriptors[AIndex];
end;

function TGocciaFunctionTemplate.GetDirectEvalEnvironment(
  const AIndex: Integer): TGocciaDirectEvalEnvironment;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FDirectEvalEnvironmentCount) then
    raise ERangeError.CreateFmt(
      'GetDirectEvalEnvironment: index %d out of range 0..%d',
      [AIndex, FDirectEvalEnvironmentCount - 1]);
  {$ENDIF}
  Result := FDirectEvalEnvironments[AIndex];
end;

function TGocciaFunctionTemplate.FindDirectEvalEnvironment(const APC: UInt32;
  out AIndex: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to FDirectEvalEnvironmentCount - 1 do
    if FDirectEvalEnvironments[I].PC = APC then
    begin
      AIndex := I;
      Exit(True);
    end;
  AIndex := -1;
  Result := False;
end;

function TGocciaFunctionTemplate.GetFunctionCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TGocciaFunctionTemplate.GetExceptionHandler(
  const AIndex: Integer): TGocciaExceptionHandler;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FExceptionHandlerCount) then
    raise ERangeError.CreateFmt('GetExceptionHandler: index %d out of range 0..%d',
      [AIndex, FExceptionHandlerCount - 1]);
  {$ENDIF}
  Result := FExceptionHandlers[AIndex];
end;

procedure TGocciaFunctionTemplate.SetLocalType(const ASlot: UInt16;
  const AKind: TGocciaLocalType);
begin
  if ASlot >= Length(FLocalTypes) then
    SetLength(FLocalTypes, ASlot + 1);
  FLocalTypes[ASlot] := AKind;
  if ASlot >= FLocalTypeCount then
    FLocalTypeCount := ASlot + 1;
end;

function TGocciaFunctionTemplate.GetLocalType(
  const ASlot: UInt16): TGocciaLocalType;
begin
  if ASlot < FLocalTypeCount then
    Result := FLocalTypes[ASlot]
  else
    Result := sltUntyped;
end;

procedure TGocciaFunctionTemplate.SetLocalStrictFlag(const ASlot: UInt16;
  const AStrict: Boolean);
begin
  if ASlot >= Length(FLocalStrictFlags) then
    SetLength(FLocalStrictFlags, ASlot + 1);
  FLocalStrictFlags[ASlot] := AStrict;
  if ASlot >= FLocalStrictCount then
    FLocalStrictCount := ASlot + 1;
end;

function TGocciaFunctionTemplate.GetLocalStrictFlag(
  const ASlot: UInt16): Boolean;
begin
  if ASlot < FLocalStrictCount then
    Result := FLocalStrictFlags[ASlot]
  else
    Result := False;
end;

initialization
  InitCriticalSection(GTemplateSiteIdLock);
  GNextTemplateSiteId := 0;

finalization
  DoneCriticalSection(GTemplateSiteIdLock);

end.
