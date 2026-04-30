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
    CookedStrings: TGocciaBytecodeStringArray;          // for bckTemplateObject
    RawStrings: TGocciaBytecodeStringArray;             // for bckTemplateObject
    CookedValid: TGocciaBytecodeTemplateCookedValid;    // for bckTemplateObject
  end;

  TGocciaUpvalueDescriptor = record
    IsLocal: Boolean;
    Index: UInt8;
  end;

  TGocciaExceptionHandler = record
    TryStart: UInt32;
    TryEnd: UInt32;
    CatchTarget: UInt32;
    FinallyTarget: UInt32;
    CatchRegister: UInt8;
  end;

  TGocciaLocalType = (
    sltUntyped,
    sltInteger,
    sltFloat,
    sltBoolean,
    sltString,
    sltReference
  );

  TGocciaFunctionTemplate = class
  private
    FName: string;
    FCode: array of UInt32;
    FCodeCount: Integer;
    FConstants: array of TGocciaBytecodeConstant;
    FConstantCount: Integer;
    FFunctions: TObjectList<TGocciaFunctionTemplate>;
    FUpvalueDescriptors: array of TGocciaUpvalueDescriptor;
    FExceptionHandlers: array of TGocciaExceptionHandler;
    FExceptionHandlerCount: Integer;
    FMaxRegisters: UInt8;
    FParameterCount: UInt8;
    FFormalParameterCount: UInt8;
    FUpvalueCount: UInt8;
    FDebugInfo: TGocciaDebugInfo;
    FLocalTypes: array of TGocciaLocalType;
    FLocalTypeCount: UInt8;
    FLocalStrictFlags: array of Boolean;
    FLocalStrictCount: UInt8;
    FIsAsync: Boolean;
    FIsGenerator: Boolean;
    FIsArrow: Boolean;
    FHasOwnPrototype: Boolean;
    FTypeCheckPreambleSize: UInt8;
    FProfileIndex: Integer;
    FSourceText: string;
    FStringConstantIndex: TOrderedStringMap<UInt16>;
    // Runtime-only cache for bckTemplateObject constants.  Indexed by the slot
    // number stored in the constant's IntValue field.  Not serialised to .gbc.
    FTemplateObjectCaches: array of TObject;  // TGocciaValue — typed as TObject to keep GC import out of interface
    FTemplateObjectCacheCount: Integer;
    function GetFunctionCount: Integer;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    function EmitInstruction(const AInstruction: UInt32): Integer;
    procedure PatchInstruction(const AIndex: Integer; const AInstruction: UInt32);
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
    function GetTemplateObjectCache(const ASlot: Integer): TObject;
    procedure SetTemplateObjectCache(const ASlot: Integer; const AValue: TObject);
    function AddFunction(const AFunction: TGocciaFunctionTemplate): UInt16;
    procedure AddUpvalueDescriptor(const AIsLocal: Boolean; const AIndex: UInt8);
    procedure AddExceptionHandler(const ATryStart, ATryEnd, ACatchTarget,
      AFinallyTarget: UInt32; const ACatchRegister: UInt8);

    function GetInstruction(const AIndex: Integer): UInt32; inline;
    function GetConstant(const AIndex: Integer): TGocciaBytecodeConstant; inline;
    function GetFunction(const AIndex: Integer): TGocciaFunctionTemplate;
    function GetInstructionUnchecked(const AIndex: Integer): UInt32; inline;
    function GetConstantUnchecked(const AIndex: Integer): TGocciaBytecodeConstant; inline;
    function GetFunctionUnchecked(const AIndex: Integer): TGocciaFunctionTemplate; inline;
    function GetUpvalueDescriptor(const AIndex: Integer): TGocciaUpvalueDescriptor;
    function GetExceptionHandler(const AIndex: Integer): TGocciaExceptionHandler;

    property Name: string read FName write FName;
    property CodeCount: Integer read FCodeCount;
    property ConstantCount: Integer read FConstantCount;
    property FunctionCount: Integer read GetFunctionCount;
    property ExceptionHandlerCount: Integer read FExceptionHandlerCount;
    property MaxRegisters: UInt8 read FMaxRegisters write FMaxRegisters;
    property ParameterCount: UInt8 read FParameterCount write FParameterCount;
    property FormalParameterCount: UInt8 read FFormalParameterCount write FFormalParameterCount;
    property UpvalueCount: UInt8 read FUpvalueCount;
    property DebugInfo: TGocciaDebugInfo read FDebugInfo write FDebugInfo;

    procedure SetLocalType(const ASlot: UInt8; const AKind: TGocciaLocalType);
    function GetLocalType(const ASlot: UInt8): TGocciaLocalType;
    property LocalTypeCount: UInt8 read FLocalTypeCount;

    procedure SetLocalStrictFlag(const ASlot: UInt8; const AStrict: Boolean);
    function GetLocalStrictFlag(const ASlot: UInt8): Boolean;
    property LocalStrictCount: UInt8 read FLocalStrictCount;

    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property IsGenerator: Boolean read FIsGenerator write FIsGenerator;
    property IsArrow: Boolean read FIsArrow write FIsArrow;
    // True when this template represents a `function`/`function*` declaration or
    // expression (or async generator).  Per ES2026 §10.2.5 MakeConstructor, such
    // functions get an own `prototype` property installed at closure-creation
    // time pointing at a fresh ordinary object whose `constructor` is the
    // function itself.
    property HasOwnPrototype: Boolean read FHasOwnPrototype write FHasOwnPrototype;
    property TypeCheckPreambleSize: UInt8 read FTypeCheckPreambleSize write FTypeCheckPreambleSize;
    property ProfileIndex: Integer read FProfileIndex write FProfileIndex;
    property SourceText: string read FSourceText write FSourceText;
  end;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector;

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
  FProfileIndex := -1;
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
  FStringConstantIndex.Free;
  FFunctions.Free;
  FDebugInfo.Free;
  inherited;
end;

function TGocciaFunctionTemplate.EmitInstruction(
  const AInstruction: UInt32): Integer;
begin
  if FCodeCount >= Length(FCode) then
    SetLength(FCode, FCodeCount * 2 + 16);
  FCode[FCodeCount] := AInstruction;
  Result := FCodeCount;
  Inc(FCodeCount);
end;

procedure TGocciaFunctionTemplate.PatchInstruction(const AIndex: Integer;
  const AInstruction: UInt32);
begin
  if (AIndex < 0) or (AIndex >= FCodeCount) then
    raise ERangeError.CreateFmt('PatchInstruction: index %d out of range 0..%d',
      [AIndex, FCodeCount - 1]);
  FCode[AIndex] := AInstruction;
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
begin
  for I := 0 to FConstantCount - 1 do
    if (FConstants[I].Kind = bckFloat) and
       ((FConstants[I].FloatValue = AValue) or
        (FloatBitsAreNaN(FConstants[I].FloatValue) and FloatBitsAreNaN(AValue))) then
      Exit(UInt16(I));

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

function TGocciaFunctionTemplate.AddFunction(
  const AFunction: TGocciaFunctionTemplate): UInt16;
begin
  if FFunctions.Count > High(UInt16) then
    raise Exception.Create('Function pool overflow: exceeds 65535 entries');
  Result := UInt16(FFunctions.Count);
  FFunctions.Add(AFunction);
end;

procedure TGocciaFunctionTemplate.AddUpvalueDescriptor(
  const AIsLocal: Boolean; const AIndex: UInt8);
begin
  if FUpvalueCount >= High(UInt8) then
    raise Exception.Create('Upvalue descriptor overflow: exceeds 255 entries');
  if FUpvalueCount >= Length(FUpvalueDescriptors) then
    SetLength(FUpvalueDescriptors, FUpvalueCount * 2 + 4);
  FUpvalueDescriptors[FUpvalueCount].IsLocal := AIsLocal;
  FUpvalueDescriptors[FUpvalueCount].Index := AIndex;
  Inc(FUpvalueCount);
end;

procedure TGocciaFunctionTemplate.AddExceptionHandler(
  const ATryStart, ATryEnd, ACatchTarget, AFinallyTarget: UInt32;
  const ACatchRegister: UInt8);
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

procedure TGocciaFunctionTemplate.SetLocalType(const ASlot: UInt8;
  const AKind: TGocciaLocalType);
begin
  if ASlot >= Length(FLocalTypes) then
    SetLength(FLocalTypes, ASlot + 1);
  FLocalTypes[ASlot] := AKind;
  if ASlot >= FLocalTypeCount then
    FLocalTypeCount := ASlot + 1;
end;

function TGocciaFunctionTemplate.GetLocalType(
  const ASlot: UInt8): TGocciaLocalType;
begin
  if ASlot < FLocalTypeCount then
    Result := FLocalTypes[ASlot]
  else
    Result := sltUntyped;
end;

procedure TGocciaFunctionTemplate.SetLocalStrictFlag(const ASlot: UInt8;
  const AStrict: Boolean);
begin
  if ASlot >= Length(FLocalStrictFlags) then
    SetLength(FLocalStrictFlags, ASlot + 1);
  FLocalStrictFlags[ASlot] := AStrict;
  if ASlot >= FLocalStrictCount then
    FLocalStrictCount := ASlot + 1;
end;

function TGocciaFunctionTemplate.GetLocalStrictFlag(
  const ASlot: UInt8): Boolean;
begin
  if ASlot < FLocalStrictCount then
    Result := FLocalStrictFlags[ASlot]
  else
    Result := False;
end;

end.
