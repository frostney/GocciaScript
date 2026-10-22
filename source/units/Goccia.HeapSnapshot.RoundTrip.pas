unit Goccia.HeapSnapshot.RoundTrip;

{ Test-runner coverage of values observed by expect. Observations are rooted
  until the file ends: snapshots describe their final state at the idle
  checkpoint, since an executing assertion is not a snapshot checkpoint. }

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedStringMap,

  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.Values.Primitives;

type
  TGocciaHeapSnapshotEngineFactory = function(
    const ASourceEngine: TGocciaEngine): TGocciaEngine of object;

  TGocciaHeapSnapshotRoundTrip = class(TGCRootSource)
  private
    FEngine: TGocciaEngine;
    FSession: TGocciaHeapSnapshotSession;
    FValues: TList<TGocciaValue>;
    FSeen: TDictionary<TGocciaValue, Boolean>;
    FObservations: Integer;
    FPassed: Integer;
    FRefused: Integer;
    FFailed: Integer;
    FVerified: Boolean;
    FRefusalReasons: TOrderedStringMap<Integer>;
    FFailureDetails: TStringList;
    function GetObjects: Integer;
    procedure RecordRefusal(const AReason: string);
    procedure VerifyValue(const AIndex: Integer;
      const ADestinationEngine: TGocciaEngine;
      const ADestinationSession: TGocciaHeapSnapshotSession;
      const AFactory: TGocciaHeapSnapshotEngineFactory);
    procedure VerifyAll(const ADestinationEngine: TGocciaEngine;
      const AFactory: TGocciaHeapSnapshotEngineFactory);
  public
    constructor Create(const ASourceEngine: TGocciaEngine);
    destructor Destroy; override;
    procedure MarkRootReferences; override;
    procedure Observe(const AValue: TGocciaValue);
    // Destination has the same runtime profile and source, with code images
    // only for templates not restored through module records (ADR 0150).
    // It must be idle and outlive this call. The test source is not replayed.
    procedure Verify(const ADestinationEngine: TGocciaEngine); overload;
    // Fresh destination per accepted root: repeated restores must not inherit
    // lazy built-ins materialized by an earlier restore. Owns factory results.
    procedure Verify(const AFactory: TGocciaHeapSnapshotEngineFactory); overload;
    property Observations: Integer read FObservations;
    property Objects: Integer read GetObjects;
    property Passed: Integer read FPassed;
    property Refused: Integer read FRefused;
    property Failed: Integer read FFailed;
    property RefusalReasons: TOrderedStringMap<Integer> read FRefusalReasons;
    property FailureDetails: TStringList read FFailureDetails;
  end;

// An independent native description of graph identity, primitive values,
// ordinary descriptors, prototypes, integrity and array/function state.
// Built-ins are leaves identified by their stable names. No getter, proxy
// trap, toJSON or custom test serializer is called. Private VM state is
// additionally checked by the runner's re-snapshot byte comparison.
function CanonicalDescription(const AValue: TGocciaValue;
  const ABuiltins: TGocciaHeapSnapshotBuiltinTable = nil): string;

implementation

uses
  SysUtils,

  Goccia.EngineFault,
  Goccia.ExecutionContext,
  Goccia.NativeLimits,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

const
  MAX_DESCRIPTION_BYTES = 64 * 1024 * 1024;
  MAX_DESCRIPTION_OBJECTS = 1000000;
  MAX_OBSERVED_OBJECTS = 1000000;
  MAX_FAILURE_DETAILS = 20;

type
  TStoredSymbolDescriptor = function(const ASymbol: TGocciaSymbolValue):
    TGocciaPropertyDescriptor of object;

  TCanonicalGraph = class
  private
    FTable: TGocciaHeapSnapshotBuiltinTable;
    FIds: TDictionary<TGocciaValue, Integer>;
    FPending: TList<TGocciaValue>;
    FOutput: TMemoryStream;
    procedure Token(const AText: string);
    procedure Value(const AValue: TGocciaValue);
    procedure Descriptor(const ADescriptor: TGocciaPropertyDescriptor);
    procedure ObjectState(const AObject: TGocciaObjectValue);
  public
    constructor Create(const ATable: TGocciaHeapSnapshotBuiltinTable);
    destructor Destroy; override;
    function Describe(const AValue: TGocciaValue): string;
  end;

function StoredSymbolDescriptor(const AObject: TGocciaObjectValue;
  const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
var
  Getter: TStoredSymbolDescriptor;
begin
  // Bind the ordinary implementation directly: a virtual call could invoke
  // an exotic object's guest trap, and a derived-class cast is not valid.
  TMethod(Getter).Code := @TGocciaObjectValue.GetOwnSymbolPropertyDescriptor;
  TMethod(Getter).Data := AObject;
  Result := Getter(ASymbol);
end;

constructor TCanonicalGraph.Create(
  const ATable: TGocciaHeapSnapshotBuiltinTable);
begin
  inherited Create;
  FTable := ATable;
  FIds := TDictionary<TGocciaValue, Integer>.Create;
  FPending := TList<TGocciaValue>.Create;
  FOutput := TMemoryStream.Create;
end;

destructor TCanonicalGraph.Destroy;
begin
  FOutput.Free;
  FPending.Free;
  FIds.Free;
  inherited;
end;

procedure TCanonicalGraph.Token(const AText: string);
var
  Prefix: string;
  Bytes, PrefixBytes: Int64;
begin
  Bytes := CheckedNativeByteSize(Length(AText), SizeOf(Char));
  Prefix := IntToStr(Length(AText)) + ':';
  PrefixBytes := CheckedNativeByteSize(Length(Prefix), SizeOf(Char));
  if (Bytes > MAX_DESCRIPTION_BYTES) or
     (FOutput.Size + PrefixBytes + Bytes > MAX_DESCRIPTION_BYTES) or
     (FOutput.Size + PrefixBytes + Bytes > MAX_NATIVE_ALLOCATION_BYTES) then
    raise Exception.Create('canonical description exceeds its byte limit');
  FOutput.WriteBuffer(Prefix[1], SizeInt(PrefixBytes));
  if Bytes > 0 then
    FOutput.WriteBuffer(AText[1], SizeInt(Bytes));
end;

procedure TCanonicalGraph.Value(const AValue: TGocciaValue);
var
  Index, I: Integer;
  Bits: QWord;
  Number: Double;
  Symbol: TGocciaSymbolValue;
begin
  CheckNativeWork;
  if not Assigned(AValue) then
    Token('nil')
  else if AValue is TGocciaUndefinedLiteralValue then
    Token('undefined')
  else if AValue is TGocciaNullLiteralValue then
    Token('null')
  else if AValue is TGocciaHoleValue then
    Token('hole')
  else if AValue is TGocciaBooleanLiteralValue then
    Token('boolean:' + IntToStr(Ord(TGocciaBooleanLiteralValue(AValue).Value)))
  else if AValue is TGocciaNumberLiteralValue then
  begin
    Number := TGocciaNumberLiteralValue(AValue).Value;
    Move(Number, Bits, SizeOf(Bits));
    Token('number:' + IntToHex(Bits, 16));
  end
  else if AValue is TGocciaStringLiteralValue then
  begin
    Token('string');
    Token(TGocciaStringLiteralValue(AValue).Value);
  end
  else if AValue is TGocciaBigIntValue then
  begin
    Token('bigint-sign:' + IntToStr(Ord(TGocciaBigIntValue(AValue).Value.IsNegative)));
    Token('limbs:' + IntToStr(TGocciaBigIntValue(AValue).Value.LimbCount));
    for I := 0 to TGocciaBigIntValue(AValue).Value.LimbCount - 1 do
    begin
      CheckNativeWork;
      Token(IntToHex(TGocciaBigIntValue(AValue).Value.LimbAt(I), 8));
    end;
  end
  else
  begin
    if Assigned(FTable) then
    begin
      Index := FTable.IndexOf(AValue);
      if Index >= 0 then
      begin
        Token('builtin');
        Token(FTable.PathAt(Index));
        Exit;
      end;
    end;
    if (AValue is TGocciaSymbolValue) and
       TGocciaSymbolValue(AValue).Registered then
    begin
      Symbol := TGocciaSymbolValue(AValue);
      Token('registered-symbol');
      Token(Symbol.Description);
      Exit;
    end;
    if not FIds.TryGetValue(AValue, Index) then
    begin
      if FPending.Count >= MAX_DESCRIPTION_OBJECTS then
        raise Exception.Create('canonical description exceeds its object limit');
      CheckedNativeByteSize(Int64(FPending.Count) + 1,
        SizeOf(Pointer) * 8);
      Index := FPending.Count;
      FIds.Add(AValue, Index);
      FPending.Add(AValue);
    end;
    Token('reference:' + IntToStr(Index));
  end;
end;

procedure TCanonicalGraph.Descriptor(
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  if not Assigned(ADescriptor) then
  begin
    Token('absent');
    Exit;
  end;
  Token('flags:' + IntToStr(Byte(ADescriptor.Flags)));
  if ADescriptor is TGocciaLazyPropertyDescriptorData then
    raise Exception.Create('canonical description encountered a lazy property')
  else if ADescriptor is TGocciaPropertyDescriptorAccessor then
  begin
    Token('accessor');
    Value(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter);
    Value(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter);
  end
  else if ADescriptor is TGocciaPropertyDescriptorData then
  begin
    Token('data');
    Value(TGocciaPropertyDescriptorData(ADescriptor).Value);
  end
  else
    raise Exception.Create('canonical description encountered an unknown descriptor');
end;

procedure TCanonicalGraph.ObjectState(const AObject: TGocciaObjectValue);
var
  Pair: TGocciaPropertyMap.TKeyValuePair;
  I: Integer;
  Symbol: TGocciaSymbolValue;
  PropertyDescriptor: TGocciaPropertyDescriptor;
  ArrayValue: TGocciaArrayValue;
  FunctionValue: TGocciaFunctionBase;
begin
  Value(AObject.Prototype);
  Token('integrity:' + IntToStr(Ord(AObject.Extensible)) +
    IntToStr(Ord(AObject.Frozen)) + IntToStr(Ord(AObject.Sealed)));
  Token('properties:' + IntToStr(AObject.Properties.Count));
  for Pair in AObject.Properties do
  begin
    CheckNativeWork;
    Token(Pair.Key);
    Descriptor(Pair.Value);
  end;
  Token('symbols:' + IntToStr(AObject.StoredSymbolCount));
  for I := 0 to AObject.StoredSymbolCount - 1 do
  begin
    CheckNativeWork;
    Symbol := AObject.StoredSymbolAt(I);
    Value(Symbol);
    // Invoke the ordinary implementation, never an exotic override/trap.
    PropertyDescriptor := StoredSymbolDescriptor(AObject, Symbol);
    try
      Descriptor(PropertyDescriptor);
    finally
      ReleaseDescriptor(PropertyDescriptor);
    end;
  end;
  if AObject is TGocciaArrayValue then
  begin
    ArrayValue := TGocciaArrayValue(AObject);
    Token('array-length:' + IntToStr(ArrayValue.StoredLength));
    Token('array-writable:' + IntToStr(Ord(ArrayValue.LengthWritable)));
    Token('elements:' + IntToStr(ArrayValue.Elements.Count));
    for I := 0 to ArrayValue.Elements.Count - 1 do
      Value(ArrayValue.Elements[I]);
  end;
  if AObject is TGocciaFunctionBase then
  begin
    FunctionValue := TGocciaFunctionBase(AObject);
    Token('function-length:' + IntToStr(FunctionValue.IntrinsicLength));
    Token(FunctionValue.IntrinsicName);
    Token('function-flags:' + IntToStr(Ord(FunctionValue.HasOwnLengthProperty)) +
      IntToStr(Ord(FunctionValue.HasOwnNameProperty)) +
      IntToStr(Ord(FunctionValue.StrictThis)) +
      IntToStr(Ord(FunctionValue.StrictCode)));
  end;
end;

function TCanonicalGraph.Describe(const AValue: TGocciaValue): string;
var
  Index: Integer;
  Current: TGocciaValue;
  Symbol: TGocciaSymbolValue;
begin
  Value(AValue);
  Index := 0;
  while Index < FPending.Count do
  begin
    CheckNativeWork;
    Current := FPending[Index];
    Token(Current.ClassName);
    if Current is TGocciaSymbolValue then
    begin
      Symbol := TGocciaSymbolValue(Current);
      Token('symbol-description:' + IntToStr(Ord(Symbol.HasDescription)));
      Token(Symbol.Description);
    end
    else if Current is TGocciaObjectValue then
      ObjectState(TGocciaObjectValue(Current))
    else
      raise Exception.Create('canonical description encountered an unknown value');
    Inc(Index);
  end;
  CheckedNativeByteSize(FOutput.Size div SizeOf(Char), SizeOf(Char));
  SetLength(Result, SizeInt(FOutput.Size div SizeOf(Char)));
  if FOutput.Size > 0 then
    Move(FOutput.Memory^, Result[1], SizeInt(FOutput.Size));
end;

function CanonicalDescription(const AValue: TGocciaValue;
  const ABuiltins: TGocciaHeapSnapshotBuiltinTable): string;
var
  Graph: TCanonicalGraph;
begin
  Graph := TCanonicalGraph.Create(ABuiltins);
  try
    Result := Graph.Describe(AValue);
  finally
    Graph.Free;
  end;
end;

constructor TGocciaHeapSnapshotRoundTrip.Create(
  const ASourceEngine: TGocciaEngine);
var
  Context: TGocciaExecutionContextScope;
begin
  inherited Create;
  FEngine := ASourceEngine;
  FValues := TList<TGocciaValue>.Create;
  FSeen := TDictionary<TGocciaValue, Boolean>.Create;
  FRefusalReasons := TOrderedStringMap<Integer>.Create;
  FFailureDetails := TStringList.Create;
  Context := FEngine.ActivateRealmExecutionContext;
  try
    // The VM slot resolver builds this entire intrinsic family together.
    // Give both sessions that same native baseline before recording it, so
    // restore does not introduce unrelated late-built-in header entries.
    MaterializeSnapshotSlot('GeneratorFunction.prototype');
    FSession := TGocciaHeapSnapshotSession.Create(FEngine);
    FSession.CheckAccounting := False;
  finally
    Context.Free;
  end;
end;

destructor TGocciaHeapSnapshotRoundTrip.Destroy;
begin
  FSession.Free;
  FFailureDetails.Free;
  FRefusalReasons.Free;
  FSeen.Free;
  FValues.Free;
  inherited;
end;

procedure TGocciaHeapSnapshotRoundTrip.MarkRootReferences;
var
  Value: TGocciaValue;
begin
  // Temp roots are a set: a matcher removing its own temporary root must
  // not release an observation that this collector still owns.
  for Value in FValues do
    Value.MarkReferences;
end;

procedure TGocciaHeapSnapshotRoundTrip.Observe(const AValue: TGocciaValue);
begin
  if not (AValue is TGocciaObjectValue) then
    Exit;
  if FVerified then
    raise Exception.Create('cannot observe after heap round-trip verification');
  if FObservations = High(Integer) then
    raise Exception.Create('heap round-trip observation count overflow');
  Inc(FObservations);
  if FSeen.ContainsKey(AValue) then
    Exit;
  if FValues.Count >= MAX_OBSERVED_OBJECTS then
    raise Exception.Create('heap round-trip exceeds its observed object limit');
  CheckedNativeByteSize(Int64(FValues.Count) + 1, SizeOf(Pointer) * 8);
  FSeen.Add(AValue, True);
  FValues.Add(AValue);
end;

function TGocciaHeapSnapshotRoundTrip.GetObjects: Integer;
begin
  Result := FValues.Count;
end;

procedure TGocciaHeapSnapshotRoundTrip.RecordRefusal(const AReason: string);
var
  Reason: string;
  Count, Chain: Integer;
begin
  Inc(FRefused);
  Reason := AReason;
  Chain := Pos(' (reached through ', Reason);
  if Chain > 0 then
    SetLength(Reason, Chain - 1);
  if Pos('the run changed built-ins:', Reason) = 1 then
    Reason := 'the run changed built-ins';
  if FRefusalReasons.TryGetValue(Reason, Count) then
    FRefusalReasons[Reason] := Count + 1
  else
  begin
    CheckedNativeByteSize(Int64(FRefusalReasons.Count) + 1,
      SizeOf(TOrderedStringMap<Integer>.TEntry) * 4);
    FRefusalReasons.Add(Reason, 1);
  end;
end;

procedure TGocciaHeapSnapshotRoundTrip.VerifyValue(const AIndex: Integer;
  const ADestinationEngine: TGocciaEngine;
  const ADestinationSession: TGocciaHeapSnapshotSession;
  const AFactory: TGocciaHeapSnapshotEngineFactory);
var
  Original, Rewritten: TMemoryStream;
  Restored: TGocciaHeapSnapshotRestore;
  Context: TGocciaExecutionContextScope;
  BeforeDescription, AfterDescription: string;
  DestinationEngine: TGocciaEngine;
  DestinationSession: TGocciaHeapSnapshotSession;
begin
  Original := TMemoryStream.Create;
  Rewritten := TMemoryStream.Create;
  Restored := nil;
  DestinationEngine := ADestinationEngine;
  DestinationSession := ADestinationSession;
  try
    Context := FEngine.ActivateRealmExecutionContext;
    try
      try
        FSession.Snapshot([FValues[AIndex]], Original);
      except
        on E: EGocciaHeapSnapshotRefused do
        begin
          RecordRefusal(E.Message);
          Exit;
        end;
      end;
      BeforeDescription := CanonicalDescription(FValues[AIndex], FSession.Table);
    finally
      Context.Free;
    end;
    if Assigned(AFactory) then
    begin
      DestinationEngine := AFactory(FEngine);
      if not Assigned(DestinationEngine) then
        raise Exception.Create('heap round-trip factory returned no engine');
      Context := DestinationEngine.ActivateRealmExecutionContext;
      try
        MaterializeSnapshotSlot('GeneratorFunction.prototype');
        DestinationSession := TGocciaHeapSnapshotSession.Create(DestinationEngine);
        DestinationSession.CheckAccounting := False;
      finally
        Context.Free;
      end;
    end;
    Context := DestinationEngine.ActivateRealmExecutionContext;
    try
      Original.Position := 0;
      Restored := TGocciaHeapSnapshotRestore.Create(DestinationEngine,
        Original, TGocciaHeapSnapshotLimits.Defaults);
      if (Length(Restored.Roots) <> 1) or
         not (Restored.Roots[0] is TGocciaObjectValue) then
        raise Exception.Create('restored root is not the observed object');
      DestinationSession.Snapshot([Restored.Roots[0]], Rewritten);
      AfterDescription := CanonicalDescription(TGocciaValue(Restored.Roots[0]),
        DestinationSession.Table);
      if BeforeDescription <> AfterDescription then
        raise Exception.Create('native canonical descriptions differ');
      if (Original.Size <> Rewritten.Size) or
         ((Original.Size > 0) and not CompareMem(Original.Memory,
          Rewritten.Memory, SizeInt(Original.Size))) then
        raise Exception.Create('re-snapshot bytes differ');
    finally
      Context.Free;
    end;
  finally
    Restored.Free;
    if Assigned(AFactory) then
    begin
      DestinationSession.Free;
      DestinationEngine.Free;
    end;
    Rewritten.Free;
    Original.Free;
  end;
  Inc(FPassed);
end;

procedure TGocciaHeapSnapshotRoundTrip.VerifyAll(
  const ADestinationEngine: TGocciaEngine;
  const AFactory: TGocciaHeapSnapshotEngineFactory);
var
  DestinationSession: TGocciaHeapSnapshotSession;
  Context: TGocciaExecutionContextScope;
  I: Integer;
begin
  if FVerified then
    raise Exception.Create('heap round-trip verification already ran');
  FVerified := True;
  DestinationSession := nil;
  if Assigned(ADestinationEngine) then
  begin
    Context := ADestinationEngine.ActivateRealmExecutionContext;
    try
      MaterializeSnapshotSlot('GeneratorFunction.prototype');
      DestinationSession := TGocciaHeapSnapshotSession.Create(ADestinationEngine);
      DestinationSession.CheckAccounting := False;
    finally
      Context.Free;
    end;
  end;
  try
    for I := 0 to FValues.Count - 1 do
      try
        VerifyValue(I, ADestinationEngine, DestinationSession, AFactory);
      except
        on E: Exception do
        begin
          if IsEngineIntegrityFault(E) then
            raise;
          Inc(FFailed);
          if FFailureDetails.Count < MAX_FAILURE_DETAILS then
            FFailureDetails.Add('object ' + IntToStr(I) + ' (' +
              FValues[I].ClassName + '): ' + E.ClassName + ': ' + E.Message);
        end;
      end;
  finally
    DestinationSession.Free;
  end;
end;

procedure TGocciaHeapSnapshotRoundTrip.Verify(
  const ADestinationEngine: TGocciaEngine);
begin
  VerifyAll(ADestinationEngine, nil);
end;

procedure TGocciaHeapSnapshotRoundTrip.Verify(
  const AFactory: TGocciaHeapSnapshotEngineFactory);
begin
  if not Assigned(AFactory) then
    raise Exception.Create('heap round-trip needs a destination factory');
  VerifyAll(nil, AFactory);
end;

end.
