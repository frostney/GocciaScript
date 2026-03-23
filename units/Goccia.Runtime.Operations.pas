unit Goccia.Runtime.Operations;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  HashMap,
  OrderedStringMap,
  Souffle.Bytecode.Chunk,
  Souffle.Compound,
  Souffle.Heap,
  Souffle.Iterator,
  Souffle.Value,
  Souffle.VM,
  Souffle.VM.Closure,
  Souffle.VM.RuntimeOperations,

  Goccia.AST.Statements,
  Goccia.Evaluator.Decorators,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

const
  GOCCIA_HEAP_WRAPPED = 128;

{$IFDEF BRIDGE_METRICS}
type
  TBridgeMetrics = record
    UnwrapCount: Int64;
    WrapCount: Int64;
    WrapGocciaCount: Int64;

    InvokeGocciaCount: Int64;
    InvokeNativeCount: Int64;
    InvokeClosureDirectCount: Int64;
    InvokeNativeDirectCount: Int64;
    ConstructGocciaCount: Int64;
    ConstructClassValueCount: Int64;
    ConstructNativeFnCount: Int64;
    ConstructNativeCount: Int64;

    GetPropertyBridgeCount: Int64;
    GetPropertyNativeCount: Int64;
    SetPropertyBridgeCount: Int64;
    SetPropertyNativeCount: Int64;

    CoercionBridgeCount: Int64;
    IsInstanceBridgeCount: Int64;
    TypeOfBridgeCount: Int64;

    GetIteratorCount: Int64;
    IteratorNextCount: Int64;
    AwaitValueCount: Int64;
    ImportModuleCount: Int64;
    ArrayCacheHit: Int64;
    ArrayCacheMiss: Int64;
    ClosureCacheHit: Int64;
    ClosureCacheMiss: Int64;
    RecordCacheHit: Int64;
    RecordCacheMiss: Int64;

    procedure Reset;
    procedure Dump;
    class procedure DumpGlobal; static;
  end;

var
  GBridgeMetrics: TBridgeMetrics;
{$ENDIF}

type
  TGocciaRuntimeOperations = class;

  TGocciaDecoratorSession = class
  public
    MetadataObject: TGocciaObjectValue;
    MethodCollector: TGocciaInitializerCollector;
    FieldCollector: TGocciaInitializerCollector;
    StaticFieldCollector: TGocciaInitializerCollector;
    ClassCollector: TGocciaInitializerCollector;
    ClassValue: TGocciaValue;
    constructor Create(const AMetadataObject: TGocciaObjectValue);
    destructor Destroy; override;
  end;


  TGocciaWrappedValue = class(TSouffleHeapObject)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    procedure MarkReferences; override;
    function DebugString: string; override;
    property Value: TGocciaValue read FValue;
  end;

  TGocciaBridgedFunction = class(TSouffleHeapObject)
  private
    FGocciaFn: TGocciaNativeFunctionValue;
    FRuntime: TGocciaRuntimeOperations;
    FName: string;
    FArity: Integer;
  public
    constructor Create(const AGocciaFn: TGocciaNativeFunctionValue;
      const ARuntime: TGocciaRuntimeOperations);
    function Invoke(const AReceiver: TSouffleValue;
      const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
    procedure MarkReferences; override;
    function DebugString: string; override;
    property Name: string read FName;
    property Arity: Integer read FArity;
  end;

  TGocciaSuperCallHelper = class(TSouffleHeapObject)
  private
    FSuperClass: TObject;
    FRuntime: TGocciaRuntimeOperations;
  public
    constructor Create(const ASuperClass: TObject;
      const ARuntime: TGocciaRuntimeOperations);
    function Invoke(const AReceiver: TSouffleValue;
      const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

  TGocciaSouffleArrayIterator = TSouffleArrayIterator;
  TGocciaSouffleStringIterator = TSouffleStringIterator;

  TGocciaSouffleProxy = class(TGocciaObjectValue)
  private
    FTarget: TSouffleHeapObject;
    FRuntime: TGocciaRuntimeOperations;
    FPrototypeResolved: Boolean;
  public
    constructor Create(const ATarget: TSouffleHeapObject;
      const ARuntime: TGocciaRuntimeOperations);

    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string;
      const AValue: TGocciaValue); override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function IsPrimitive: Boolean; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    procedure MarkReferences; override;

    function HasOwnProperty(const AName: string): Boolean; override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    function DeleteProperty(const AName: string): Boolean; override;
    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); override;
    function GetEnumerablePropertyNames: TArray<string>; override;
    function GetEnumerablePropertyValues: TArray<TGocciaValue>; override;
    function GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>; override;
    function GetOwnPropertyNames: TArray<string>; override;
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetSymbolProperty(const ASymbol: TGocciaSymbolValue): TGocciaValue; override;
    function HasSymbolProperty(const ASymbol: TGocciaSymbolValue): Boolean; override;
    procedure Freeze; override;
    function IsFrozen: Boolean; override;
    procedure Seal; override;
    function IsSealed: Boolean; override;
    procedure PreventExtensions; override;
    function IsExtensible: Boolean; override;

    property Target: TSouffleHeapObject read FTarget;
  end;

  TGocciaRuntimeOperations = class(TSouffleRuntimeOperations)
  private
    FGlobals: TOrderedStringMap<TSouffleValue>;
    FConstGlobals: TOrderedStringMap<Boolean>;
    FExports: TOrderedStringMap<TSouffleValue>;
    FClosureBridgeCache: THashMap<TSouffleClosure, TObject>;
    FArrayBridgeCache: THashMap<TObject, TObject>;
    FArrayBridgeReverse: THashMap<TObject, TObject>;
    FRecordBridgeCache: THashMap<TObject, TObject>;
    FBlueprintBridgeCache: THashMap<TObject, TObject>;
    FBlueprintSuperValues: THashMap<TObject, TSouffleValue>;
    FFormalParameterCounts: THashMap<TSouffleFunctionTemplate, Integer>;
    FClassDefinitionScopes: THashMap<TObject, TObject>;
    FWrappedValues: TList;
    FBridgeCallDepth: Integer;
    FVM: TSouffleVM;
    FEngine: TObject;
    FSourcePath: string;
    FStringDelegate: TSouffleRecord;
    FNumberDelegate: TSouffleRecord;
    FMapDelegate: TSouffleRecord;
    FSetDelegate: TSouffleRecord;
    FPromiseDelegate: TSouffleRecord;
    FPromiseStaticDelegate: TSouffleRecord;
    FPromiseConstructor: TSouffleHeapObject;
    FDescribeDelegate: TSouffleRecord;
    FTestDelegate: TSouffleRecord;
    FActiveDecoratorSession: TGocciaDecoratorSession;
    FArrayBridgeDirty: Boolean;
    function WrapGocciaValue(const AValue: TGocciaValue): TSouffleValue;
    function CoerceKeyToString(const AKey: TSouffleValue): string;
    function CoerceToNumber(const A: TSouffleValue): Double;
    function CoerceToString(const A: TSouffleValue): string;
    procedure RethrowAsVM(const E: TGocciaThrowValue);
    procedure MarkWrappedGocciaValues;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const A, B: TSouffleValue): TSouffleValue; override;
    function Subtract(const A, B: TSouffleValue): TSouffleValue; override;
    function Multiply(const A, B: TSouffleValue): TSouffleValue; override;
    function Divide(const A, B: TSouffleValue): TSouffleValue; override;
    function Modulo(const A, B: TSouffleValue): TSouffleValue; override;
    function Power(const A, B: TSouffleValue): TSouffleValue; override;
    function Negate(const A: TSouffleValue): TSouffleValue; override;

    function BitwiseAnd(const A, B: TSouffleValue): TSouffleValue; override;
    function BitwiseOr(const A, B: TSouffleValue): TSouffleValue; override;
    function BitwiseXor(const A, B: TSouffleValue): TSouffleValue; override;
    function ShiftLeft(const A, B: TSouffleValue): TSouffleValue; override;
    function ShiftRight(const A, B: TSouffleValue): TSouffleValue; override;
    function UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue; override;
    function BitwiseNot(const A: TSouffleValue): TSouffleValue; override;

    function Equal(const A, B: TSouffleValue): TSouffleValue; override;
    function NotEqual(const A, B: TSouffleValue): TSouffleValue; override;
    function LessThan(const A, B: TSouffleValue): TSouffleValue; override;
    function GreaterThan(const A, B: TSouffleValue): TSouffleValue; override;
    function LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue; override;
    function GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue; override;

    function LogicalNot(const A: TSouffleValue): TSouffleValue; override;
    function TypeOf(const A: TSouffleValue): TSouffleValue; override;
    function IsInstance(const A, B: TSouffleValue): TSouffleValue; override;
    function HasProperty(const AObject, AKey: TSouffleValue): TSouffleValue; override;
    function ToBoolean(const A: TSouffleValue): TSouffleValue; override;
    function ToPrimitive(const A: TSouffleValue): TSouffleValue; override;

    function GetProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; override;
    procedure SetProperty(const AObject: TSouffleValue; const AKey: string;
      const AValue: TSouffleValue); override;
    function GetIndex(const AObject, AKey: TSouffleValue): TSouffleValue; override;
    procedure SetIndex(const AObject: TSouffleValue;
      const AKey, AValue: TSouffleValue); override;
    function DeleteProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; override;
    function DeleteIndex(const AObject, AKey: TSouffleValue): TSouffleValue; override;

    function Invoke(const ACallee: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer; const AReceiver: TSouffleValue): TSouffleValue; override;
    function Construct(const AConstructor: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; override;

    function GetIterator(const AIterable: TSouffleValue;
      const ATryAsync: Boolean = False): TSouffleValue; override;
    function IteratorNext(const AIterator: TSouffleValue;
      out ADone: Boolean): TSouffleValue; override;
    procedure SpreadInto(const ATarget, ASource: TSouffleValue);
    procedure SpreadObjectInto(const ATarget, ASource: TSouffleValue);

    function ObjectRest(const ASource,
      AExclusionKeys: TSouffleValue): TSouffleValue;
    procedure RequireObjectCoercible(const AValue: TSouffleValue);
    function RequireIterable(const AValue: TSouffleValue): TSouffleValue;
    function CoerceValueToString(const A: TSouffleValue): TSouffleValue; override;

    function ImportModule(const APath: string): TSouffleValue; override;
    procedure ExportBinding(const AValue: TSouffleValue;
      const AName: string); override;

    function AwaitValue(const AValue: TSouffleValue): TSouffleValue; override;

    function SuperMethodGet(const ASuperBlueprint: TSouffleValue;
      const AMethodName: string): TSouffleValue;
    function WrapInPromise(const AValue: TSouffleValue;
      const AIsRejected: Boolean): TSouffleValue; override;

    function GetGlobal(const AName: string): TSouffleValue; override;
    procedure SetGlobal(const AName: string;
      const AValue: TSouffleValue); override;
    function HasGlobal(const AName: string): Boolean; override;

    procedure DefineGetter(const AObject: TSouffleValue; const AKey: string;
      const AGetter: TSouffleValue);
    procedure DefineSetter(const AObject: TSouffleValue; const AKey: string;
      const ASetter: TSouffleValue);
    procedure DefineStaticGetter(const AObject: TSouffleValue; const AKey: string;
      const AGetter: TSouffleValue);
    procedure DefineStaticSetter(const AObject: TSouffleValue; const AKey: string;
      const ASetter: TSouffleValue);

    function ComputedKeyAsString(const AKey: TSouffleValue): string;
    procedure DefineComputedGetter(const AObject, AKey, AGetter: TSouffleValue);
    procedure DefineComputedSetter(const AObject, AKey, ASetter: TSouffleValue);
    procedure DefineComputedStaticGetter(const AObject, AKey, AGetter: TSouffleValue);
    procedure DefineComputedStaticSetter(const AObject, AKey, ASetter: TSouffleValue);

    function GetSymbolOnNativeObject(const AObject: TSouffleValue;
      const ASymKey: TGocciaSymbolValue;
      out AFound: Boolean): TSouffleValue;
    function SetSymbolOnNativeObject(const AObject: TSouffleValue;
      const ASymKey: TGocciaSymbolValue;
      const AValue: TSouffleValue): Boolean;

    procedure PropertyWriteViolation(const AObject: TSouffleValue;
      const AKey: string);
    procedure PropertyDeleteViolation(const AObject: TSouffleValue;
      const AKey: string);
    procedure ThrowTypeErrorMessage(
      const AMessage: string);

    function FinalizeEnum(const ARecord: TSouffleValue;
      const AName: string): TSouffleValue;

    procedure SetupAutoAccessor(const ABlueprint: TSouffleValue;
      const AName: string; const AInitClosure: TSouffleValue);
    procedure BeginDecorators(const ABlueprint, ASuper: TSouffleValue);
    procedure ApplyElementDecorator(const ADecoratorFn: TSouffleValue;
      const ADescriptor: string);
    procedure ApplyClassDecorator(const ADecoratorFn: TSouffleValue);
    procedure FinishDecorators(var ADest: TSouffleValue);

    procedure ExtendedOperation(const ASubOp: UInt8;
      var ADest: TSouffleValue; const AOperand, AExtra: TSouffleValue;
      const ATemplate: TSouffleFunctionTemplate;
      const AOperandIndex: UInt8); override;
    procedure MarkExternalRoots; override;
    procedure CheckLocalType(const AValue: TSouffleValue;
      const AExpectedType: TSouffleLocalType); override;


    function TryInvokeGetter(const AGetterVal, AReceiver: TSouffleValue;
      out AResult: TSouffleValue): Boolean;
    function UnwrapToGocciaValue(const AValue: TSouffleValue): TGocciaValue;
    function ToSouffleValue(const AValue: TGocciaValue): TSouffleValue;

    function ResolveProxyGet(const ATarget: TSouffleHeapObject;
      const AName: string): TGocciaValue;
    procedure ResolveProxySet(const ATarget: TSouffleHeapObject;
      const AName: string; const AValue: TGocciaValue);

    procedure ClearTransientCaches;
    procedure RegisterDelegates;
    procedure RegisterTestNatives;
    procedure RegisterNativeBuiltins;
    procedure RegisterGlobal(const AName: string; const AValue: TSouffleValue);
    procedure RegisterConstGlobal(const AName: string;
      const AValue: TSouffleValue);
    procedure PatchGocciaScriptStrictTypes;
    procedure RegisterFormalParameterCount(
      const ATemplate: TSouffleFunctionTemplate; const ACount: Integer);
    function GetFormalParameterCount(
      const ATemplate: TSouffleFunctionTemplate): Integer;
    property ModuleExports: TOrderedStringMap<TSouffleValue> read FExports;
    property VM: TSouffleVM read FVM write FVM;
    property Engine: TObject read FEngine write FEngine;
    property SourcePath: string read FSourcePath write FSourcePath;
  end;

implementation

uses
  Math,
  StrUtils,
  SysUtils,

  GarbageCollector.Generic,
  Souffle.Bytecode.Debug,
  Souffle.VM.CallFrame,
  Souffle.VM.Exception,
  Souffle.VM.NativeFunction,

  Goccia.Arguments.Collection,
  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Engine,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.Evaluator.TypeOperations,
  Goccia.Interpreter,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Runtime.Collections,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ArrayValue,
  Goccia.Values.AutoAccessor,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.EnumValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.MapValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SetValue,
  Goccia.Values.ToPrimitive;

const
  GOCCIA_NIL_UNDEFINED = 0;
  GOCCIA_NIL_NULL      = 1;
  GOCCIA_NIL_HOLE      = 2;

type
  TGocciaSouffleNativeFunctionBridge = class(TGocciaFunctionBase)
  private
    FNativeFunction: TSouffleNativeFunction;
    FRuntime: TGocciaRuntimeOperations;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const ANativeFunction: TSouffleNativeFunction;
      const ARuntime: TGocciaRuntimeOperations);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
    property NativeFunction: TSouffleNativeFunction read FNativeFunction;
  end;

  TGocciaSouffleClosureBridge = class(TGocciaFunctionBase)
  private
    FClosure: TSouffleClosure;
    FRuntime: TGocciaRuntimeOperations;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AClosure: TSouffleClosure;
      const ARuntime: TGocciaRuntimeOperations);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
    property Closure: TSouffleClosure read FClosure;
  end;

  TGocciaSouffleMethodBridge = class(TGocciaMethodValue)
  private
    FSouffleClosure: TSouffleClosure;
    FBridgeRuntime: TGocciaRuntimeOperations;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AClosure: TSouffleClosure;
      const ARuntime: TGocciaRuntimeOperations);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

procedure RebuildArrayBridgeCache(
  const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope); forward;
procedure SyncCachedGocciaToSouffle(
  const ARuntime: TGocciaRuntimeOperations); forward;
function SouffleIsHole(const AValue: TSouffleValue): Boolean;
begin
  Result := (AValue.Kind = svkNil) and (AValue.Flags = GOCCIA_NIL_HOLE);
end;

{ TGocciaSouffleNativeFunctionBridge }

constructor TGocciaSouffleNativeFunctionBridge.Create(
  const ANativeFunction: TSouffleNativeFunction;
  const ARuntime: TGocciaRuntimeOperations);
begin
  inherited Create;
  FNativeFunction := ANativeFunction;
  FRuntime := ARuntime;
end;

function TGocciaSouffleNativeFunctionBridge.GetFunctionLength: Integer;
begin
  Result := FNativeFunction.Arity;
end;

function TGocciaSouffleNativeFunctionBridge.GetFunctionName: string;
begin
  Result := FNativeFunction.Name;
end;

function TGocciaSouffleNativeFunctionBridge.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: array of TSouffleValue;
  I: Integer;
  Receiver, SouffleResult: TSouffleValue;
begin
  SetLength(Args, AArguments.Length);
  for I := 0 to AArguments.Length - 1 do
    Args[I] := FRuntime.ToSouffleValue(AArguments.GetElement(I));
  Receiver := FRuntime.ToSouffleValue(AThisValue);
  if AArguments.Length > 0 then
    SouffleResult := FNativeFunction.Invoke(Receiver, @Args[0], AArguments.Length)
  else
    SouffleResult := FNativeFunction.Invoke(Receiver, nil, 0);
  Result := FRuntime.UnwrapToGocciaValue(SouffleResult);
end;

procedure TGocciaSouffleNativeFunctionBridge.MarkReferences;
begin
  inherited;
  if Assigned(FNativeFunction) and not FNativeFunction.GCMarked then
    FNativeFunction.MarkReferences;
end;

{ TGocciaSouffleClosureBridge }

constructor TGocciaSouffleClosureBridge.Create(
  const AClosure: TSouffleClosure;
  const ARuntime: TGocciaRuntimeOperations);
begin
  inherited Create;
  FClosure := AClosure;
  FRuntime := ARuntime;
end;

function TGocciaSouffleClosureBridge.GetFunctionLength: Integer;
begin
  Result := FRuntime.GetFormalParameterCount(FClosure.Template);
  if Result < 0 then
    Result := FClosure.Template.ParameterCount;
end;

function TGocciaSouffleClosureBridge.GetFunctionName: string;
begin
  Result := FClosure.Template.Name;
end;

function TGocciaSouffleClosureBridge.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: array of TSouffleValue;
  I: Integer;
  SouffleResult: TSouffleValue;
begin
  SetLength(Args, AArguments.Length + 1);
  Args[0] := FRuntime.ToSouffleValue(AThisValue);
  for I := 0 to AArguments.Length - 1 do
    Args[1 + I] := FRuntime.ToSouffleValue(AArguments.GetElement(I));

  SyncCachedGocciaToSouffle(FRuntime);
  Inc(FRuntime.FBridgeCallDepth);
  try
    SouffleResult := FRuntime.VM.ExecuteFunction(FClosure, Args);
  except
    on E: ESouffleThrow do
    begin
      Dec(FRuntime.FBridgeCallDepth);
      if FRuntime.FBridgeCallDepth = 0 then
      begin
        FRuntime.FArrayBridgeCache.Clear;
        FRuntime.FArrayBridgeDirty := False;
        FRuntime.FRecordBridgeCache.Clear;
      end;
      raise TGocciaThrowValue.Create(
        FRuntime.UnwrapToGocciaValue(E.ThrownValue));
    end;
  end;
  Dec(FRuntime.FBridgeCallDepth);
  if FRuntime.FBridgeCallDepth = 0 then
  begin
    FRuntime.FArrayBridgeCache.Clear;
    FRuntime.FArrayBridgeDirty := False;
    FRuntime.FRecordBridgeCache.Clear;
  end;
  Result := FRuntime.UnwrapToGocciaValue(SouffleResult);
end;

procedure TGocciaSouffleClosureBridge.MarkReferences;
begin
  inherited;
  if Assigned(FClosure) and not FClosure.GCMarked then
    FClosure.MarkReferences;
end;

{ TGocciaSouffleMethodBridge }

constructor TGocciaSouffleMethodBridge.Create(const AClosure: TSouffleClosure;
  const ARuntime: TGocciaRuntimeOperations);
begin
  inherited Create(nil, nil, nil, AClosure.Template.Name);
  FSouffleClosure := AClosure;
  FBridgeRuntime := ARuntime;
end;

function TGocciaSouffleMethodBridge.GetFunctionLength: Integer;
begin
  Result := FSouffleClosure.Template.ParameterCount - 1;
end;

function TGocciaSouffleMethodBridge.GetFunctionName: string;
begin
  Result := FSouffleClosure.Template.Name;
end;

function TGocciaSouffleMethodBridge.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: array of TSouffleValue;
  I: Integer;
  SouffleResult: TSouffleValue;
begin
  SetLength(Args, AArguments.Length + 1);
  Args[0] := FBridgeRuntime.ToSouffleValue(AThisValue);
  for I := 0 to AArguments.Length - 1 do
    Args[1 + I] := FBridgeRuntime.ToSouffleValue(AArguments.GetElement(I));

  SyncCachedGocciaToSouffle(FBridgeRuntime);
  Inc(FBridgeRuntime.FBridgeCallDepth);
  try
    SouffleResult := FBridgeRuntime.VM.ExecuteFunction(FSouffleClosure, Args);
  except
    on E: ESouffleThrow do
    begin
      Dec(FBridgeRuntime.FBridgeCallDepth);
      if FBridgeRuntime.FBridgeCallDepth = 0 then
      begin
        FBridgeRuntime.FArrayBridgeCache.Clear;
        FBridgeRuntime.FArrayBridgeDirty := False;
        FBridgeRuntime.FRecordBridgeCache.Clear;
      end;
      raise TGocciaThrowValue.Create(
        FBridgeRuntime.UnwrapToGocciaValue(E.ThrownValue));
    end;
  end;
  Dec(FBridgeRuntime.FBridgeCallDepth);
  if FBridgeRuntime.FBridgeCallDepth = 0 then
  begin
    FBridgeRuntime.FArrayBridgeCache.Clear;
    FBridgeRuntime.FArrayBridgeDirty := False;
    FBridgeRuntime.FRecordBridgeCache.Clear;
  end;
  Result := FBridgeRuntime.UnwrapToGocciaValue(SouffleResult);
end;

procedure TGocciaSouffleMethodBridge.MarkReferences;
begin
  inherited;
  if Assigned(FSouffleClosure) and not FSouffleClosure.GCMarked then
    FSouffleClosure.MarkReferences;
end;

{ TGocciaWrappedValue }

constructor TGocciaWrappedValue.Create(const AValue: TGocciaValue);
begin
  inherited Create(GOCCIA_HEAP_WRAPPED);
  FValue := AValue;
end;

procedure TGocciaWrappedValue.MarkReferences;
begin
  inherited;
  if Assigned(FValue) then
    FValue.MarkReferences;
end;

function TGocciaWrappedValue.DebugString: string;
begin
  if Assigned(FValue) then
    Result := '[Wrapped: ' + FValue.ToStringLiteral.Value + ']'
  else
    Result := '[Wrapped: nil]';
end;

{ TGocciaBridgedFunction }

constructor TGocciaBridgedFunction.Create(
  const AGocciaFn: TGocciaNativeFunctionValue;
  const ARuntime: TGocciaRuntimeOperations);
begin
  inherited Create(SOUFFLE_HEAP_NATIVE_FUNCTION);
  FGocciaFn := AGocciaFn;
  FRuntime := ARuntime;
  FName := AGocciaFn.Name;
  FArity := AGocciaFn.Arity;
end;

function TGocciaBridgedFunction.Invoke(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Args: TGocciaArgumentsCollection;
  GocciaReceiver, GocciaResult: TGocciaValue;
  I: Integer;
begin
  Args := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to AArgCount - 1 do
      Args.Add(FRuntime.UnwrapToGocciaValue(
        PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
    if SouffleIsNil(AReceiver) then
      GocciaReceiver := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      GocciaReceiver := FRuntime.UnwrapToGocciaValue(AReceiver);
    GocciaResult := FGocciaFn.Call(Args, GocciaReceiver);
    if Assigned(GocciaResult) then
      Result := FRuntime.ToSouffleValue(GocciaResult)
    else
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  finally
    Args.Free;
  end;
end;

procedure TGocciaBridgedFunction.MarkReferences;
begin
  inherited;
  if Assigned(FGocciaFn) and not FGocciaFn.GCMarked then
    FGocciaFn.MarkReferences;
end;

function TGocciaBridgedFunction.DebugString: string;
begin
  Result := '[BridgedFn: ' + FName + ']';
end;

{ TGocciaSuperCallHelper }

constructor TGocciaSuperCallHelper.Create(
  const ASuperClass: TObject;
  const ARuntime: TGocciaRuntimeOperations);
begin
  inherited Create(SOUFFLE_HEAP_NATIVE_FUNCTION);
  FSuperClass := ASuperClass;
  FRuntime := ARuntime;
end;

function TGocciaSuperCallHelper.Invoke(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Instance: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  I: Integer;
  SuperClass: TGocciaClassValue;
begin
  Instance := FRuntime.UnwrapToGocciaValue(AReceiver);
  SuperClass := TGocciaClassValue(FSuperClass);
  Args := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to AArgCount - 1 do
      Args.Add(FRuntime.UnwrapToGocciaValue(
        PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
    if Assigned(SuperClass.ConstructorMethod) then
      SuperClass.ConstructorMethod.Call(Args, Instance)
    else if (Instance is TGocciaInstanceValue) then
      TGocciaInstanceValue(Instance).InitializeNativeFromArguments(Args);
  finally
    Args.Free;
  end;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

procedure TGocciaSuperCallHelper.MarkReferences;
begin
  inherited;
  if (FSuperClass is TGocciaValue) and
     not TGocciaValue(FSuperClass).GCMarked then
    TGocciaValue(FSuperClass).MarkReferences;
end;

function TGocciaSuperCallHelper.DebugString: string;
begin
  Result := '[SuperCallHelper]';
end;

{ TGocciaSouffleProxy }

constructor TGocciaSouffleProxy.Create(const ATarget: TSouffleHeapObject;
  const ARuntime: TGocciaRuntimeOperations);
begin
  inherited Create;
  FTarget := ATarget;
  FRuntime := ARuntime;
end;

function TGocciaSouffleProxy.GetProperty(const AName: string): TGocciaValue;
begin
  Result := FRuntime.ResolveProxyGet(FTarget, AName);
  if not Assigned(Result) then
    Result := inherited GetProperty(AName);
end;

procedure TGocciaSouffleProxy.SetProperty(const AName: string;
  const AValue: TGocciaValue);
begin
  FRuntime.ResolveProxySet(FTarget, AName, AValue);
end;

function TGocciaSouffleProxy.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create(
    FRuntime.CoerceToString(SouffleReference(FTarget)));
end;

function TGocciaSouffleProxy.TypeName: string;
begin
  Result := 'object';
end;

function TGocciaSouffleProxy.TypeOf: string;
begin
  Result := 'object';
end;

function TGocciaSouffleProxy.IsPrimitive: Boolean;
begin
  Result := False;
end;

function TGocciaSouffleProxy.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaSouffleProxy.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

procedure TGocciaSouffleProxy.MarkReferences;
begin
  inherited MarkReferences;
  if Assigned(FTarget) and not FTarget.GCMarked then
    FTarget.MarkReferences;
end;

function TGocciaSouffleProxy.HasOwnProperty(const AName: string): Boolean;
begin
  if FTarget is TSouffleRecord then
    Result := TSouffleRecord(FTarget).Has(AName)
  else
    Result := False;
end;

function TGocciaSouffleProxy.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
var
  Rec: TSouffleRecord;
  Val, GetterVal, SetterVal: TSouffleValue;
  Flags: Byte;
  PFlags: TPropertyFlags;
  HasGetter, HasSetter: Boolean;
  GocciaGetter, GocciaSetter: TGocciaValue;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    HasGetter := Rec.HasGetters and Rec.Getters.Get(AName, GetterVal);
    HasSetter := Rec.HasSetters and Rec.Setters.Get(AName, SetterVal);
    if HasGetter or HasSetter then
    begin
      GocciaGetter := nil;
      GocciaSetter := nil;
      if HasGetter then
        GocciaGetter := FRuntime.UnwrapToGocciaValue(GetterVal);
      if HasSetter then
        GocciaSetter := FRuntime.UnwrapToGocciaValue(SetterVal);
      PFlags := [];
      if HasGetter then
        Flags := Rec.Getters.GetEntryFlags(AName)
      else
        Flags := Rec.Setters.GetEntryFlags(AName);
      if Flags and SOUFFLE_PROP_CONFIGURABLE <> 0 then
        Include(PFlags, pfConfigurable);
      if Flags and SOUFFLE_PROP_ENUMERABLE <> 0 then
        Include(PFlags, pfEnumerable);
      Result := TGocciaPropertyDescriptorAccessor.Create(
        GocciaGetter, GocciaSetter, PFlags);
    end
    else if Rec.Get(AName, Val) then
    begin
      Flags := Rec.GetEntryFlags(AName);
      PFlags := [];
      if Flags and SOUFFLE_PROP_WRITABLE <> 0 then
        Include(PFlags, pfWritable);
      if Flags and SOUFFLE_PROP_CONFIGURABLE <> 0 then
        Include(PFlags, pfConfigurable);
      if Flags and SOUFFLE_PROP_ENUMERABLE <> 0 then
        Include(PFlags, pfEnumerable);
      Result := TGocciaPropertyDescriptorData.Create(
        FRuntime.UnwrapToGocciaValue(Val), PFlags);
    end
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TGocciaSouffleProxy.DeleteProperty(const AName: string): Boolean;
var
  Flags: Byte;
begin
  if FTarget is TSouffleRecord then
  begin
    if TSouffleRecord(FTarget).Has(AName) then
    begin
      Flags := TSouffleRecord(FTarget).GetEntryFlags(AName);
      if Flags and SOUFFLE_PROP_CONFIGURABLE = 0 then
        ThrowTypeError('Cannot delete property ''' + AName +
          ''' of a non-configurable property');
    end;
    Result := TSouffleRecord(FTarget).Delete(AName)
  end
  else
    Result := False;
end;

procedure TGocciaSouffleProxy.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Rec: TSouffleRecord;
  SFlags, ExistingFlags: Byte;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    if Rec.Has(AName) then
    begin
      ExistingFlags := Rec.GetEntryFlags(AName);
      if ExistingFlags and SOUFFLE_PROP_CONFIGURABLE = 0 then
        ThrowTypeError('Cannot redefine property: ' + AName);
    end;
    SFlags := 0;
    if pfWritable in ADescriptor.Flags then
      SFlags := SFlags or SOUFFLE_PROP_WRITABLE;
    if pfConfigurable in ADescriptor.Flags then
      SFlags := SFlags or SOUFFLE_PROP_CONFIGURABLE;
    if pfEnumerable in ADescriptor.Flags then
      SFlags := SFlags or SOUFFLE_PROP_ENUMERABLE;
    if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      if Rec.HasGetters then Rec.Getters.Delete(AName);
      if Rec.HasSetters then Rec.Setters.Delete(AName);
      Rec.PutWithFlags(AName,
        FRuntime.ToSouffleValue(TGocciaPropertyDescriptorData(ADescriptor).Value),
        SFlags);
    end
    else if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if Rec.Has(AName) then Rec.Delete(AName);
      if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
        Rec.Getters.PutWithFlags(AName,
          FRuntime.ToSouffleValue(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter),
          SFlags);
      if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
        Rec.Setters.PutWithFlags(AName,
          FRuntime.ToSouffleValue(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter),
          SFlags);
    end;
  end;
end;

function TGocciaSouffleProxy.GetEnumerablePropertyNames: TArray<string>;
var
  Rec: TSouffleRecord;
  I, Count: Integer;
  Flags: Byte;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    SetLength(Result, Rec.Count);
    Count := 0;
    for I := 0 to Rec.Count - 1 do
    begin
      Flags := Rec.GetEntryFlags(Rec.GetOrderedKey(I));
      if Flags and SOUFFLE_PROP_ENUMERABLE <> 0 then
      begin
        Result[Count] := Rec.GetOrderedKey(I);
        Inc(Count);
      end;
    end;
    SetLength(Result, Count);
  end
  else
    Result := nil;
end;

function TGocciaSouffleProxy.GetEnumerablePropertyValues: TArray<TGocciaValue>;
var
  Rec: TSouffleRecord;
  I, Count: Integer;
  Flags: Byte;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    SetLength(Result, Rec.Count);
    Count := 0;
    for I := 0 to Rec.Count - 1 do
    begin
      Flags := Rec.GetEntryFlags(Rec.GetOrderedKey(I));
      if Flags and SOUFFLE_PROP_ENUMERABLE <> 0 then
      begin
        Result[Count] := FRuntime.UnwrapToGocciaValue(Rec.GetOrderedValue(I));
        Inc(Count);
      end;
    end;
    SetLength(Result, Count);
  end
  else
    Result := nil;
end;

function TGocciaSouffleProxy.GetEnumerablePropertyEntries: TArray<TPair<string, TGocciaValue>>;
var
  Rec: TSouffleRecord;
  I, Count: Integer;
  Flags: Byte;
  Key: string;
  Seen: TOrderedStringMap<Boolean>;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    Seen := TOrderedStringMap<Boolean>.Create;
    try
      SetLength(Result, Rec.Count + 16);
      Count := 0;
      for I := 0 to Rec.Count - 1 do
      begin
        Key := Rec.GetOrderedKey(I);
        Flags := Rec.GetEntryFlags(Key);
        if Flags and SOUFFLE_PROP_ENUMERABLE <> 0 then
        begin
          Seen.AddOrSetValue(Key, True);
          if Count >= Length(Result) then
            SetLength(Result, Count + 16);
          Result[Count].Key := Key;
          Result[Count].Value := FRuntime.UnwrapToGocciaValue(Rec.GetOrderedValue(I));
          Inc(Count);
        end;
      end;
      if Rec.HasGetters then
        for I := 0 to Rec.Getters.Count - 1 do
        begin
          Key := Rec.Getters.GetOrderedKey(I);
          if not Seen.ContainsKey(Key) then
          begin
            Flags := Rec.Getters.GetEntryFlags(Key);
            if Flags and SOUFFLE_PROP_ENUMERABLE <> 0 then
            begin
              Seen.AddOrSetValue(Key, True);
              if Count >= Length(Result) then
                SetLength(Result, Count + 16);
              Result[Count].Key := Key;
              Result[Count].Value := GetProperty(Key);
              Inc(Count);
            end;
          end;
        end;
      SetLength(Result, Count);
    finally
      Seen.Free;
    end;
  end
  else
    Result := nil;
end;

function TGocciaSouffleProxy.GetOwnPropertyNames: TArray<string>;
var
  Rec: TSouffleRecord;
  I, Count: Integer;
  Key: string;
  Seen: TOrderedStringMap<Boolean>;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    Seen := TOrderedStringMap<Boolean>.Create;
    try
      SetLength(Result, Rec.Count + 16);
      Count := 0;
      for I := 0 to Rec.Count - 1 do
      begin
        Key := Rec.GetOrderedKey(I);
        if not Seen.ContainsKey(Key) then
        begin
          Seen.Add(Key, True);
          if Count >= Length(Result) then
            SetLength(Result, Count + 16);
          Result[Count] := Key;
          Inc(Count);
        end;
      end;
      if Rec.HasGetters then
        for I := 0 to Rec.Getters.Count - 1 do
        begin
          Key := Rec.Getters.GetOrderedKey(I);
          if not Seen.ContainsKey(Key) then
          begin
            Seen.Add(Key, True);
            if Count >= Length(Result) then
              SetLength(Result, Count + 16);
            Result[Count] := Key;
            Inc(Count);
          end;
        end;
      if Rec.HasSetters then
        for I := 0 to Rec.Setters.Count - 1 do
        begin
          Key := Rec.Setters.GetOrderedKey(I);
          if not Seen.ContainsKey(Key) then
          begin
            Seen.Add(Key, True);
            if Count >= Length(Result) then
              SetLength(Result, Count + 16);
            Result[Count] := Key;
            Inc(Count);
          end;
        end;
      SetLength(Result, Count);
    finally
      Seen.Free;
    end;
  end
  else
    Result := nil;
end;

function TGocciaSouffleProxy.GetOwnPropertyKeys: TArray<string>;
begin
  Result := GetOwnPropertyNames;
end;

function TGocciaSouffleProxy.GetAllPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyNames;
end;

function TGocciaSouffleProxy.GetSymbolProperty(
  const ASymbol: TGocciaSymbolValue): TGocciaValue;
var
  SymPropKey: string;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  GetterVal, InvokeResult, DirectVal: TSouffleValue;
begin
  Result := inherited GetSymbolProperty(ASymbol);
  if Assigned(Result) and not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  if FTarget is TSouffleRecord then
  begin
    SymPropKey := '@@sym:' + IntToStr(ASymbol.Id);
    Rec := TSouffleRecord(FTarget);
    if Rec.HasGetters and Rec.Getters.Get(SymPropKey, GetterVal) then
      if FRuntime.TryInvokeGetter(GetterVal, SouffleReference(FTarget),
           InvokeResult) then
        Exit(FRuntime.UnwrapToGocciaValue(InvokeResult));
    if Rec.Get(SymPropKey, DirectVal) then
      Exit(FRuntime.UnwrapToGocciaValue(DirectVal));
    if Assigned(Rec.Blueprint) then
    begin
      Bp := Rec.Blueprint;
      while Assigned(Bp) do
      begin
        if Bp.HasGetters and Bp.Getters.Get(SymPropKey, GetterVal) then
          if FRuntime.TryInvokeGetter(GetterVal, SouffleReference(FTarget),
               InvokeResult) then
            Exit(FRuntime.UnwrapToGocciaValue(InvokeResult));
        Bp := Bp.SuperBlueprint;
      end;
    end;
  end;
end;

function TGocciaSouffleProxy.HasSymbolProperty(
  const ASymbol: TGocciaSymbolValue): Boolean;
begin
  Result := inherited HasSymbolProperty(ASymbol);
end;

procedure TGocciaSouffleProxy.Freeze;
begin
  if FTarget is TSouffleRecord then
    TSouffleRecord(FTarget).Freeze;
end;

function TGocciaSouffleProxy.IsFrozen: Boolean;
begin
  if FTarget is TSouffleRecord then
    Result := not TSouffleRecord(FTarget).Extensible
  else
    Result := False;
end;

procedure TGocciaSouffleProxy.Seal;
begin
  if FTarget is TSouffleRecord then
    TSouffleRecord(FTarget).PreventExtensions;
end;

function TGocciaSouffleProxy.IsSealed: Boolean;
begin
  if FTarget is TSouffleRecord then
    Result := not TSouffleRecord(FTarget).Extensible
  else
    Result := False;
end;

procedure TGocciaSouffleProxy.PreventExtensions;
begin
  if FTarget is TSouffleRecord then
    TSouffleRecord(FTarget).PreventExtensions;
end;

function TGocciaSouffleProxy.IsExtensible: Boolean;
begin
  if FTarget is TSouffleRecord then
    Result := TSouffleRecord(FTarget).Extensible
  else
    Result := False;
end;

{$IFDEF BRIDGE_METRICS}
procedure TBridgeMetrics.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

class procedure TBridgeMetrics.DumpGlobal;
begin
  if GBridgeMetrics.UnwrapCount > 0 then
    GBridgeMetrics.Dump;
end;

procedure TBridgeMetrics.Dump;
  procedure W(const ALabel: string; const AValue: Int64);
  begin
    if AValue > 0 then
      WriteLn(StdErr, '  ', ALabel:40, AValue:12);
  end;
begin
  WriteLn(StdErr, '--- Bridge Metrics ---');
  WriteLn(StdErr, 'Conversions:');
  W('UnwrapToGocciaValue', UnwrapCount);
  W('ToSouffleValue', WrapCount);
  W('WrapGocciaValue', WrapGocciaCount);
  WriteLn(StdErr, 'Invocation:');
  W('Invoke (bridge to interpreter)', InvokeGocciaCount);
  W('Invoke (Souffle closures/native)', InvokeNativeCount);
  W('Invoke (closure direct)', InvokeClosureDirectCount);
  W('Invoke (native direct)', InvokeNativeDirectCount);
  W('Construct (bridge to interpreter)', ConstructGocciaCount);
  W('  -> TGocciaClassValue', ConstructClassValueCount);
  W('  -> TGocciaNativeFunctionValue', ConstructNativeFnCount);
  W('Construct (Souffle blueprints)', ConstructNativeCount);
  WriteLn(StdErr, 'Property access:');
  W('GetProperty (bridge)', GetPropertyBridgeCount);
  W('GetProperty (Souffle types)', GetPropertyNativeCount);
  W('SetProperty (bridge)', SetPropertyBridgeCount);
  W('SetProperty (Souffle types)', SetPropertyNativeCount);
  WriteLn(StdErr, 'Type operations:');
  W('Coercion (bridge)', CoercionBridgeCount);
  W('IsInstance (bridge)', IsInstanceBridgeCount);
  W('TypeOf (bridge)', TypeOfBridgeCount);
  WriteLn(StdErr, 'Interpreter delegation:');
  W('GetIterator', GetIteratorCount);
  W('IteratorNext', IteratorNextCount);
  W('AwaitValue', AwaitValueCount);
  W('ImportModule', ImportModuleCount);
  WriteLn(StdErr, 'Bridge caches:');
  W('Array cache hit', ArrayCacheHit);
  W('Array cache miss', ArrayCacheMiss);
  W('Closure cache hit', ClosureCacheHit);
  W('Closure cache miss', ClosureCacheMiss);
  W('Record cache hit', RecordCacheHit);
  W('Record cache miss', RecordCacheMiss);
  WriteLn(StdErr, '----------------------');
end;
{$ENDIF}

{ TGocciaRuntimeOperations }

constructor TGocciaRuntimeOperations.Create;
begin
  inherited Create;
  FGlobals := TOrderedStringMap<TSouffleValue>.Create;
  FConstGlobals := TOrderedStringMap<Boolean>.Create;
  FExports := TOrderedStringMap<TSouffleValue>.Create;
  FClosureBridgeCache := THashMap<TSouffleClosure, TObject>.Create;
  FArrayBridgeCache := THashMap<TObject, TObject>.Create;
  FArrayBridgeReverse := THashMap<TObject, TObject>.Create;
  FRecordBridgeCache := THashMap<TObject, TObject>.Create;
  FBlueprintBridgeCache := THashMap<TObject, TObject>.Create;
  FBlueprintSuperValues := THashMap<TObject, TSouffleValue>.Create;
  FClassDefinitionScopes := THashMap<TObject, TObject>.Create;
  FWrappedValues := TList.Create;
  FFormalParameterCounts := THashMap<TSouffleFunctionTemplate, Integer>.Create;
  FBridgeCallDepth := 0;
  FVM := nil;
  TGarbageCollector.Initialize;
  TGarbageCollector.Instance.AddExternalRootMarker(MarkWrappedGocciaValues);
end;

destructor TGocciaRuntimeOperations.Destroy;
begin
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RemoveExternalRootMarker(MarkWrappedGocciaValues);
  FGlobals.Free;
  FConstGlobals.Free;
  FExports.Free;
  FClosureBridgeCache.Free;
  FArrayBridgeCache.Free;
  FArrayBridgeReverse.Free;
  FRecordBridgeCache.Free;
  FBlueprintBridgeCache.Free;
  FBlueprintSuperValues.Free;
  FClassDefinitionScopes.Free;
  FWrappedValues.Free;
  FFormalParameterCounts.Free;
  FActiveDecoratorSession.Free;
  inherited;
end;

function TGocciaRuntimeOperations.WrapGocciaValue(
  const AValue: TGocciaValue): TSouffleValue;
var
  Wrapped: TGocciaWrappedValue;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.WrapGocciaCount);
  {$ENDIF}
  Wrapped := TGocciaWrappedValue.Create(AValue);
  FWrappedValues.Add(Wrapped);
  Result := SouffleReference(Wrapped);
end;

procedure TGocciaRuntimeOperations.RethrowAsVM(const E: TGocciaThrowValue);
begin
  raise ESouffleThrow.Create(WrapGocciaValue(E.Value));
end;

procedure PushVMFramesToGoccia(const AVM: TSouffleVM);
var
  I: Integer;
  Frame: PSouffleVMCallFrame;
  FuncName, FilePath: string;
  Line: Integer;
  Column: Integer;
begin
  if not Assigned(AVM) or not Assigned(TGocciaCallStack.Instance) then Exit;
  if AVM.CallStack.Count = 0 then Exit;
  for I := 0 to AVM.CallStack.Count - 1 do
  begin
    Frame := AVM.CallStack.GetFrame(I);
    FuncName := Frame^.Template.Name;
    if Assigned(Frame^.Template.DebugInfo) then
    begin
      FilePath := Frame^.Template.DebugInfo.SourceFile;
      Line := Frame^.Template.DebugInfo.GetLineForPC(Frame^.IP);
      Column := Frame^.Template.DebugInfo.GetColumnForPC(Frame^.IP);
    end
    else
    begin
      FilePath := '';
      Line := 0;
      Column := 0;
    end;
    TGocciaCallStack.Instance.Push(FuncName, FilePath, Line, Column);
  end;
end;

procedure PopVMFramesFromGoccia(const AVM: TSouffleVM);
var
  I: Integer;
begin
  if not Assigned(AVM) or not Assigned(TGocciaCallStack.Instance) then Exit;
  for I := 0 to AVM.CallStack.Count - 1 do
    TGocciaCallStack.Instance.Pop;
end;

function CoerceToPrimitiveSouffle(const ARuntime: TGocciaRuntimeOperations;
  const A: TSouffleValue): TSouffleValue; forward;

function StringToNumber(const S: string): Double;
var
  Trimmed: string;
  Code: Integer;
  HexVal: Int64;
begin
  Trimmed := Trim(S);
  if Trimmed = '' then
    Exit(0.0);
  if Trimmed = 'Infinity' then
    Exit(Infinity);
  if Trimmed = '-Infinity' then
    Exit(NegInfinity);
  Val(Trimmed, Result, Code);
  if Code <> 0 then
  begin
    if (Length(Trimmed) > 2) and (Trimmed[1] = '0') and
       ((Trimmed[2] = 'x') or (Trimmed[2] = 'X')) then
    begin
      Val(Trimmed, HexVal, Code);
      if Code = 0 then
        Result := HexVal
      else
        Result := NaN;
    end
    else
      Result := NaN;
  end;
end;

function TGocciaRuntimeOperations.CoerceToNumber(
  const A: TSouffleValue): Double;
var
  Prim: TSouffleValue;
begin
  case A.Kind of
    svkInteger:
      Result := A.AsInteger;
    svkFloat:
      Result := A.AsFloat;
    svkBoolean:
      if A.AsBoolean then
        Result := 1.0
      else
        Result := 0.0;
    svkNil:
      if A.Flags = GOCCIA_NIL_NULL then
        Result := 0.0
      else
        Result := NaN;
    svkString:
      Result := StringToNumber(SouffleGetString(A));
    svkReference:
      if A.AsReference is TSouffleHeapString then
        Result := StringToNumber(TSouffleHeapString(A.AsReference).Value)
      else if (A.AsReference is TGocciaWrappedValue) and
              (TGocciaWrappedValue(A.AsReference).Value is TGocciaSymbolValue) then
      begin
        ThrowTypeError('Cannot convert a Symbol value to a number');
        Result := NaN;
      end
      else
      begin
        Prim := CoerceToPrimitiveSouffle(Self, A);
        if (Prim.Kind <> svkReference) or SouffleIsStringValue(Prim) then
          Result := CoerceToNumber(Prim)
        else
          Result := NaN;
      end;
  else
    Result := NaN;
  end;
end;

function JoinArrayElements(const AArr: TSouffleArray;
  const ASep: string; const ARuntime: TGocciaRuntimeOperations): string; forward;
function JoinElementToString(const AElem: TSouffleValue;
  const ARuntime: TGocciaRuntimeOperations): string; forward;
function CreateBridgedContext(
  const ARuntime: TGocciaRuntimeOperations): TGocciaEvaluationContext; forward;

procedure SyncArraysBack(const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope); forward;

procedure SyncScopeToGlobals(const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope); forward;


function ConvertBlueprintToClassValue(const ABp: TSouffleBlueprint;
  const ARuntime: TGocciaRuntimeOperations): TGocciaClassValue; forward;

function SouffleArrayToString(const ARuntime: TGocciaRuntimeOperations;
  const AArr: TSouffleArray): string;
begin
  Result := JoinArrayElements(AArr, ',', ARuntime);
end;

function FloatToECMAString(const AValue: Double): string;
begin
  if IsNaN(AValue) then
    Result := 'NaN'
  else if IsInfinite(AValue) then
  begin
    if AValue > 0 then
      Result := 'Infinity'
    else
      Result := '-Infinity';
  end
  else if (Frac(AValue) = 0.0) and (Abs(AValue) < 1e20) then
    Result := FloatToStrF(AValue, ffFixed, 20, 0)
  else
    Result := FloatToStr(AValue);
end;

function InvokeMethodForPrimitive(const ARuntime: TGocciaRuntimeOperations;
  const AMethodVal, ASelf: TSouffleValue;
  out APrimitive: TSouffleValue): Boolean;
var
  VMArgs: array of TSouffleValue;
begin
  Result := False;
  if not SouffleIsReference(AMethodVal) or not Assigned(AMethodVal.AsReference) then
    Exit;
  SetLength(VMArgs, 1);
  VMArgs[0] := ASelf;
  if AMethodVal.AsReference is TSouffleClosure then
    APrimitive := ARuntime.VM.ExecuteFunction(
      TSouffleClosure(AMethodVal.AsReference), VMArgs)
  else if AMethodVal.AsReference is TSouffleNativeFunction then
    APrimitive := TSouffleNativeFunction(AMethodVal.AsReference)
      .Invoke(ASelf, nil, 0)
  else
    Exit;
  Result := (APrimitive.Kind <> svkReference) or SouffleIsStringValue(APrimitive);
end;

function FindRecordMethod(const ARec: TSouffleRecord;
  const AName: string; out AMethod: TSouffleValue): Boolean;
var
  Bp: TSouffleBlueprint;
  Walk: TSouffleRecord;
begin
  if ARec.Get(AName, AMethod) then
    Exit(True);
  if Assigned(ARec.Blueprint) then
  begin
    Bp := ARec.Blueprint;
    while Assigned(Bp) do
    begin
      if Bp.Methods.Get(AName, AMethod) then
        Exit(True);
      Bp := Bp.SuperBlueprint;
    end;
  end;
  Walk := ARec;
  while Assigned(Walk.Delegate) and (Walk.Delegate is TSouffleRecord) do
  begin
    Walk := TSouffleRecord(Walk.Delegate);
    if Walk.Get(AName, AMethod) then
      Exit(True);
  end;
  Result := False;
end;

function RecordToString(const ARuntime: TGocciaRuntimeOperations;
  const ARec: TSouffleRecord; const ASelf: TSouffleValue): string;
var
  MethodVal, PrimResult: TSouffleValue;
  TriedMethod: Boolean;
begin
  TriedMethod := False;
  if FindRecordMethod(ARec, PROP_TO_STRING, MethodVal) then
  begin
    TriedMethod := True;
    if InvokeMethodForPrimitive(ARuntime, MethodVal, ASelf, PrimResult) then
    begin
      Result := ARuntime.CoerceToString(PrimResult);
      Exit;
    end;
  end;
  if FindRecordMethod(ARec, PROP_VALUE_OF, MethodVal) then
  begin
    TriedMethod := True;
    if InvokeMethodForPrimitive(ARuntime, MethodVal, ASelf, PrimResult) then
    begin
      Result := ARuntime.CoerceToString(PrimResult);
      Exit;
    end;
  end;
  if TriedMethod then
    ThrowTypeError('Cannot convert object to primitive value');
  Result := '[object Object]';
end;

function TGocciaRuntimeOperations.CoerceToString(
  const A: TSouffleValue): string;
begin
  case A.Kind of
    svkNil:
      if A.Flags = GOCCIA_NIL_NULL then
        Result := 'null'
      else
        Result := 'undefined';
    svkBoolean:
      if A.AsBoolean then
        Result := 'true'
      else
        Result := 'false';
    svkInteger:
      Result := IntToStr(A.AsInteger);
    svkFloat:
      Result := FloatToECMAString(A.AsFloat);
    svkString:
      Result := SouffleGetString(A);
    svkReference:
      if A.AsReference is TSouffleHeapString then
        Result := TSouffleHeapString(A.AsReference).Value
      else if A.AsReference is TSouffleArray then
        Result := SouffleArrayToString(Self, TSouffleArray(A.AsReference))
      else if A.AsReference is TSouffleRecord then
        Result := RecordToString(Self, TSouffleRecord(A.AsReference), A)
      else if A.AsReference is TSouffleClosure then
      begin
        if TSouffleClosure(A.AsReference).Template.Name <> '' then
          Result := 'function ' + TSouffleClosure(A.AsReference).Template.Name +
            '() { [native code] }'
        else
          Result := 'function () { [native code] }';
      end
      else if A.AsReference is TSouffleNativeFunction then
        Result := 'function ' + TSouffleNativeFunction(A.AsReference).Name +
          '() { [native code] }'
      else if A.AsReference is TSouffleBlueprint then
        Result := 'function ' + TSouffleBlueprint(A.AsReference).Name +
          '() { [native code] }'
      else if A.AsReference is TGocciaWrappedValue then
      begin
        if TGocciaWrappedValue(A.AsReference).Value is TGocciaSymbolValue then
          ThrowTypeError('Cannot convert a Symbol value to a string');
        Result := TGocciaWrappedValue(A.AsReference).Value.ToStringLiteral.Value;
      end
      else
        Result := '[object Object]';
  else
    Result := 'undefined';
  end;
end;

procedure ConvertSouffleArrayInto(
  const ARuntime: TGocciaRuntimeOperations;
  const AArr: TSouffleArray; const ADest: TGocciaArrayValue);
var
  I: Integer;
begin
  for I := 0 to AArr.Count - 1 do
    ADest.Elements.Add(ARuntime.UnwrapToGocciaValue(AArr.Get(I)));
end;

function ConvertSouffleArrayToGocciaArray(
  const ARuntime: TGocciaRuntimeOperations;
  const AArr: TSouffleArray): TGocciaArrayValue;
begin
  Result := TGocciaArrayValue.Create;
  ConvertSouffleArrayInto(ARuntime, AArr, Result);
end;

function TGocciaRuntimeOperations.TryInvokeGetter(
  const AGetterVal, AReceiver: TSouffleValue;
  out AResult: TSouffleValue): Boolean;
var
  Ref: TSouffleHeapObject;
begin
  Result := False;
  if not SouffleIsReference(AGetterVal) then
    Exit;
  Ref := AGetterVal.AsReference;
  if Ref is TSouffleClosure then
  begin
    AResult := FVM.ExecuteFunction(TSouffleClosure(Ref), [AReceiver]);
    Result := True;
  end
  else if Ref is TSouffleNativeFunction then
  begin
    AResult := TSouffleNativeFunction(Ref).Invoke(AReceiver, nil, 0);
    Result := True;
  end
  else if Ref is TGocciaBridgedFunction then
  begin
    AResult := TGocciaBridgedFunction(Ref).Invoke(AReceiver, nil, 0);
    Result := True;
  end;
end;

function TGocciaRuntimeOperations.UnwrapToGocciaValue(
  const AValue: TSouffleValue): TGocciaValue;
var
  CachedBridge: TObject;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.UnwrapCount);
  {$ENDIF}
  case AValue.Kind of
    svkNil:
      if AValue.Flags = GOCCIA_NIL_NULL then
        Result := TGocciaNullLiteralValue.Create
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    svkBoolean:
      if AValue.AsBoolean then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    svkInteger:
      Result := TGocciaNumberLiteralValue.Create(AValue.AsInteger);
    svkFloat:
      Result := TGocciaNumberLiteralValue.Create(AValue.AsFloat);
    svkString:
      Result := TGocciaStringLiteralValue.Create(SouffleGetString(AValue));
    svkReference:
      if AValue.AsReference is TSouffleHeapString then
        Result := TGocciaStringLiteralValue.Create(
          TSouffleHeapString(AValue.AsReference).Value)
      else if AValue.AsReference is TGocciaWrappedValue then
        Result := TGocciaWrappedValue(AValue.AsReference).Value
      else if AValue.AsReference is TGocciaBridgedFunction then
        Result := TGocciaBridgedFunction(AValue.AsReference).FGocciaFn
      else if AValue.AsReference is TSouffleArray then
      begin
        if not FArrayBridgeCache.TryGetValue(AValue.AsReference, CachedBridge) then
        begin
          {$IFDEF BRIDGE_METRICS}
          Inc(GBridgeMetrics.ArrayCacheMiss);
          {$ENDIF}
          CachedBridge := TGocciaArrayValue.Create;
          FArrayBridgeCache.Add(AValue.AsReference, CachedBridge);
          FArrayBridgeReverse.AddOrSetValue(CachedBridge, AValue.AsReference);
          ConvertSouffleArrayInto(Self,
            TSouffleArray(AValue.AsReference),
            TGocciaArrayValue(CachedBridge));
          FArrayBridgeDirty := True;
        end
        {$IFDEF BRIDGE_METRICS}
        else
          Inc(GBridgeMetrics.ArrayCacheHit)
        {$ENDIF}
        ;
        Result := TGocciaValue(CachedBridge);
      end
      else if AValue.AsReference is TSouffleRecord then
      begin
        if not FRecordBridgeCache.TryGetValue(AValue.AsReference, CachedBridge) then
        begin
          {$IFDEF BRIDGE_METRICS}
          Inc(GBridgeMetrics.RecordCacheMiss);
          {$ENDIF}
          CachedBridge := TGocciaSouffleProxy.Create(AValue.AsReference, Self);
          FRecordBridgeCache.Add(AValue.AsReference, CachedBridge);
        end
        {$IFDEF BRIDGE_METRICS}
        else
          Inc(GBridgeMetrics.RecordCacheHit)
        {$ENDIF}
        ;
        if not TGocciaSouffleProxy(CachedBridge).FPrototypeResolved and
           Assigned(TSouffleRecord(AValue.AsReference).Delegate) and
           (TSouffleRecord(AValue.AsReference).Delegate is TSouffleRecord) then
        begin
          TGocciaSouffleProxy(CachedBridge).FPrototypeResolved := True;
          if Assigned(FVM) and
             (TSouffleRecord(AValue.AsReference).Delegate = FVM.RecordDelegate) and
             Assigned(TGocciaObjectValue.SharedObjectPrototype) then
            TGocciaSouffleProxy(CachedBridge).Prototype :=
              TGocciaObjectValue.SharedObjectPrototype
          else
            TGocciaSouffleProxy(CachedBridge).Prototype :=
              TGocciaSouffleProxy(UnwrapToGocciaValue(
                SouffleReference(TSouffleRecord(AValue.AsReference).Delegate)));
        end;
        Result := TGocciaValue(CachedBridge);
      end
      else if (AValue.AsReference is TSouffleClosure) and Assigned(FVM) then
      begin
        if not FClosureBridgeCache.TryGetValue(
            TSouffleClosure(AValue.AsReference), CachedBridge) then
        begin
          {$IFDEF BRIDGE_METRICS}
          Inc(GBridgeMetrics.ClosureCacheMiss);
          {$ENDIF}
          CachedBridge := TGocciaSouffleClosureBridge.Create(
            TSouffleClosure(AValue.AsReference), Self);
          FClosureBridgeCache.Add(TSouffleClosure(AValue.AsReference),
            CachedBridge);
        end
        {$IFDEF BRIDGE_METRICS}
        else
          Inc(GBridgeMetrics.ClosureCacheHit)
        {$ENDIF}
        ;
        Result := TGocciaValue(CachedBridge);
      end
      else if AValue.AsReference is TSouffleNativeFunction then
        Result := TGocciaSouffleNativeFunctionBridge.Create(
          TSouffleNativeFunction(AValue.AsReference), Self)
      else if AValue.AsReference is TSouffleBlueprint then
      begin
        if not FBlueprintBridgeCache.TryGetValue(AValue.AsReference, CachedBridge) then
        begin
          CachedBridge := ConvertBlueprintToClassValue(
            TSouffleBlueprint(AValue.AsReference), Self);
          FBlueprintBridgeCache.Add(AValue.AsReference, CachedBridge);
        end;
        Result := TGocciaValue(CachedBridge);
      end
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function ConvertGocciaMapToSouffle(const AMap: TGocciaMapValue;
  const ARuntime: TGocciaRuntimeOperations;
  const ADelegate: TSouffleRecord): TGocciaSouffleMap; forward;
function ConvertGocciaSetToSouffle(const ASet: TGocciaSetValue;
  const ARuntime: TGocciaRuntimeOperations;
  const ADelegate: TSouffleRecord): TGocciaSouffleSet; forward;

function TGocciaRuntimeOperations.ToSouffleValue(
  const AValue: TGocciaValue): TSouffleValue;
var
  NumVal: TGocciaNumberLiteralValue;
  CachedReverse: TObject;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.WrapCount);
  {$ENDIF}
  if not Assigned(AValue) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));

  if AValue is TGocciaSouffleProxy then
    Exit(SouffleReference(TGocciaSouffleProxy(AValue).Target));

  if AValue is TGocciaSouffleClosureBridge then
    Exit(SouffleReference(TGocciaSouffleClosureBridge(AValue).Closure));

  if AValue is TGocciaSouffleNativeFunctionBridge then
    Exit(SouffleReference(TGocciaSouffleNativeFunctionBridge(AValue).NativeFunction));

  if AValue is TGocciaNullLiteralValue then
    Result := SouffleNilWithFlags(GOCCIA_NIL_NULL)
  else if AValue is TGocciaUndefinedLiteralValue then
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)
  else if AValue is TGocciaBooleanLiteralValue then
    Result := SouffleBoolean(TGocciaBooleanLiteralValue(AValue).Value)
  else if AValue is TGocciaNumberLiteralValue then
  begin
    NumVal := TGocciaNumberLiteralValue(AValue);
    if NumVal.IsNaN then
      Result := SouffleFloat(NaN)
    else if NumVal.IsInfinite then
    begin
      if NumVal.IsNegativeInfinity then
        Result := SouffleFloat(NegInfinity)
      else
        Result := SouffleFloat(Infinity);
    end
    else if NumVal.IsNegativeZero then
      Result := SouffleFloat(-0.0)
    else if (Frac(NumVal.Value) = 0.0)
       and (NumVal.Value >= -2147483648.0)
       and (NumVal.Value <= 2147483647.0) then
      Result := SouffleInteger(Trunc(NumVal.Value))
    else
      Result := SouffleFloat(NumVal.Value);
  end
  else if AValue is TGocciaStringLiteralValue then
    Result := SouffleString(TGocciaStringLiteralValue(AValue).Value)
  else if (AValue is TGocciaArrayValue) and
     FArrayBridgeReverse.TryGetValue(AValue, CachedReverse) then
    Result := SouffleReference(TSouffleHeapObject(CachedReverse))
  else if AValue is TGocciaNativeFunctionValue then
  begin
    Result := SouffleReference(TGocciaBridgedFunction.Create(
      TGocciaNativeFunctionValue(AValue), Self));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
  end
  else
    Result := WrapGocciaValue(AValue);
end;

{ Arithmetic }

function InvokePrimitiveMethod(const ARuntime: TGocciaRuntimeOperations;
  const AMethodVal, ASelf: TSouffleValue;
  out AResult: TSouffleValue): Boolean;
var
  VMArgs: array of TSouffleValue;
begin
  Result := False;
  if not SouffleIsReference(AMethodVal) or not Assigned(AMethodVal.AsReference) then
    Exit;
  SetLength(VMArgs, 1);
  VMArgs[0] := ASelf;
  if AMethodVal.AsReference is TSouffleClosure then
    AResult := ARuntime.VM.ExecuteFunction(
      TSouffleClosure(AMethodVal.AsReference), VMArgs)
  else if AMethodVal.AsReference is TSouffleNativeFunction then
    AResult := TSouffleNativeFunction(AMethodVal.AsReference)
      .Invoke(ASelf, nil, 0)
  else
    Exit;
  Result := not SouffleIsReference(AResult) or SouffleIsStringValue(AResult);
end;

function CoerceToPrimitiveSouffle(const ARuntime: TGocciaRuntimeOperations;
  const A: TSouffleValue): TSouffleValue;
var
  GocciaVal, Prim: TGocciaValue;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  MethodVal, MethodResult: TSouffleValue;
begin
  if not SouffleIsReference(A) or not Assigned(A.AsReference) then
    Exit(A);
  if SouffleIsStringValue(A) then
    Exit(A);
  if A.AsReference is TSouffleArray then
    Exit(SouffleString(
      SouffleArrayToString(ARuntime, TSouffleArray(A.AsReference))));
  if A.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(A.AsReference);

    if Rec.Get(PROP_VALUE_OF, MethodVal) then
      if InvokePrimitiveMethod(ARuntime, MethodVal, A, MethodResult) then
        Exit(MethodResult);
    if Rec.Get(PROP_TO_STRING, MethodVal) then
      if InvokePrimitiveMethod(ARuntime, MethodVal, A, MethodResult) then
        Exit(MethodResult);

    if Assigned(Rec.Blueprint) then
    begin
      Bp := Rec.Blueprint;
      while Assigned(Bp) do
      begin
        if Bp.Methods.Get(PROP_VALUE_OF, MethodVal) then
          if InvokePrimitiveMethod(ARuntime, MethodVal, A, MethodResult) then
            Exit(MethodResult);
        if Bp.Methods.Get(PROP_TO_STRING, MethodVal) then
          if InvokePrimitiveMethod(ARuntime, MethodVal, A, MethodResult) then
            Exit(MethodResult);
        Bp := Bp.SuperBlueprint;
      end;
    end;
    while Assigned(Rec.Delegate) and (Rec.Delegate is TSouffleRecord) do
    begin
      Rec := TSouffleRecord(Rec.Delegate);
      if Rec.Get(PROP_TO_STRING, MethodVal) then
        if InvokePrimitiveMethod(ARuntime, MethodVal, A, MethodResult) then
          Exit(MethodResult);
    end;
    Exit(SouffleString('[object Object]'));
  end;
  if A.AsReference is TGocciaWrappedValue then
  begin
    {$IFDEF BRIDGE_METRICS}
    Inc(GBridgeMetrics.CoercionBridgeCount);
    {$ENDIF}
    GocciaVal := TGocciaWrappedValue(A.AsReference).Value;
    if GocciaVal.IsPrimitive then
      Exit(ARuntime.ToSouffleValue(GocciaVal));
    Prim := Goccia.Values.ToPrimitive.ToPrimitive(GocciaVal);
    Exit(ARuntime.ToSouffleValue(Prim));
  end;
  Result := SouffleString('[object Object]');
end;

function IsWrappedSymbol(const A: TSouffleValue): Boolean; inline;
begin
  Result := SouffleIsReference(A) and Assigned(A.AsReference) and
    (A.AsReference is TGocciaWrappedValue) and
    (TGocciaWrappedValue(A.AsReference).Value is TGocciaSymbolValue);
end;

function TGocciaRuntimeOperations.Add(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    if SouffleIsInteger(A) and SouffleIsInteger(B) then
      Exit(SouffleInteger(A.AsInteger + B.AsInteger));
    if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
      Exit(SouffleFloat(SouffleAsNumber(A) + SouffleAsNumber(B)));

    if IsWrappedSymbol(A) or IsWrappedSymbol(B) then
      ThrowTypeError('Cannot convert a Symbol value to a string');

    if SouffleIsStringValue(A) then
      Result := SouffleString(SouffleGetString(A) + CoerceToString(B))
    else if SouffleIsStringValue(B) then
      Result := SouffleString(CoerceToString(A) + SouffleGetString(B))
    else
      Result := SouffleFloat(CoerceToNumber(A) + CoerceToNumber(B));
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.Subtract(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    if SouffleIsInteger(A) and SouffleIsInteger(B) then
      Result := SouffleInteger(A.AsInteger - B.AsInteger)
    else
      Result := SouffleFloat(CoerceToNumber(A) - CoerceToNumber(B));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.Multiply(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    if SouffleIsInteger(A) and SouffleIsInteger(B) then
      Result := SouffleInteger(A.AsInteger * B.AsInteger)
    else
      Result := SouffleFloat(CoerceToNumber(A) * CoerceToNumber(B));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

// ES2026 §13.7 Multiplicative Operators — IEEE 754 handles all edge cases
// (NaN propagation, division by ±0, Infinity arithmetic, negative zero sign)
function TGocciaRuntimeOperations.Divide(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    Result := SouffleFloat(CoerceToNumber(A) / CoerceToNumber(B));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.Modulo(const A, B: TSouffleValue): TSouffleValue;
var
  Dividend, Divisor: Double;
begin
  try
    if SouffleIsInteger(A) and SouffleIsInteger(B) and (B.AsInteger <> 0) then
      Result := SouffleInteger(A.AsInteger mod B.AsInteger)
    else
    begin
      Dividend := CoerceToNumber(A);
      Divisor := CoerceToNumber(B);
      if IsNaN(Dividend) or IsNaN(Divisor) then
        Result := SouffleFloat(NaN)
      else if Divisor = 0.0 then
        Result := SouffleFloat(NaN)
      else
        Result := SouffleFloat(FMod(Dividend, Divisor));
    end;
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.Power(const A, B: TSouffleValue): TSouffleValue;
var
  Base, Exp: Double;
begin
  try
    Base := CoerceToNumber(A);
    Exp := CoerceToNumber(B);
    if Exp = 0.0 then
      Exit(SouffleFloat(1.0));
    if IsNaN(Exp) or IsNaN(Base) then
      Exit(SouffleFloat(NaN));
    if IsInfinite(Base) then
    begin
      if Exp > 0 then
      begin
        if (Base < 0) and (Frac(Exp) = 0) and (Frac(Exp / 2) <> 0) then
          Result := SouffleFloat(NegInfinity)
        else
          Result := SouffleFloat(Infinity);
      end
      else
      begin
        if (Base < 0) and (Frac(Exp) = 0) and (Frac(Exp / 2) <> 0) then
          Result := SouffleFloat(-0.0)
        else
          Result := SouffleFloat(0.0);
      end;
      Exit;
    end;
    Result := SouffleFloat(Math.Power(Abs(Base), Exp));
    if (Base < 0) and (Frac(Exp) = 0) and (Frac(Exp / 2) <> 0) then
      Result := SouffleFloat(-SouffleAsNumber(Result));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.Negate(const A: TSouffleValue): TSouffleValue;
begin
  try
    if SouffleIsInteger(A) then
    begin
      if A.AsInteger = 0 then
        Result := SouffleFloat(-0.0)
      else
        Result := SouffleInteger(-A.AsInteger);
    end
    else
      Result := SouffleFloat(-CoerceToNumber(A));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

{ Bitwise }

function ToInt32(const AValue: Double): Int32;
begin
  if IsNaN(AValue) or IsInfinite(AValue) or (AValue = 0.0) then
    Result := 0
  else
    Result := Int32(Trunc(AValue));
end;

function TGocciaRuntimeOperations.BitwiseAnd(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    Result := SouffleInteger(ToInt32(CoerceToNumber(A)) and
      ToInt32(CoerceToNumber(B)));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.BitwiseOr(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    Result := SouffleInteger(ToInt32(CoerceToNumber(A)) or
      ToInt32(CoerceToNumber(B)));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.BitwiseXor(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    Result := SouffleInteger(ToInt32(CoerceToNumber(A)) xor
      ToInt32(CoerceToNumber(B)));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.ShiftLeft(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    Result := SouffleInteger(ToInt32(CoerceToNumber(A)) shl
      (ToInt32(CoerceToNumber(B)) and $1F));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.ShiftRight(const A, B: TSouffleValue): TSouffleValue;
begin
  try
    Result := SouffleInteger(SarLongint(ToInt32(CoerceToNumber(A)),
      ToInt32(CoerceToNumber(B)) and $1F));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue;
var
  LVal: Cardinal;
  Shift: Integer;
  FloatResult: Double;
begin
  try
    LVal := Cardinal(Trunc(CoerceToNumber(A)));
    Shift := Trunc(CoerceToNumber(B)) and 31;
    LVal := LVal shr Shift;
    FloatResult := LVal;
    Result := SouffleFloat(FloatResult);
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.BitwiseNot(const A: TSouffleValue): TSouffleValue;
begin
  try
    Result := SouffleInteger(not ToInt32(CoerceToNumber(A)));
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

{ Comparison }

function WrappedValuesEqual(const A, B: TSouffleValue): Boolean;
begin
  if SouffleIsReference(A) and SouffleIsReference(B) and
     Assigned(A.AsReference) and Assigned(B.AsReference) and
     (A.AsReference is TGocciaWrappedValue) and
     (B.AsReference is TGocciaWrappedValue) then
    Exit(TGocciaWrappedValue(A.AsReference).Value =
         TGocciaWrappedValue(B.AsReference).Value);
  Result := SouffleValuesEqual(A, B);
end;

function TGocciaRuntimeOperations.Equal(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) = SouffleAsNumber(B))
  else
    Result := SouffleBoolean(WrappedValuesEqual(A, B));
end;

function TGocciaRuntimeOperations.NotEqual(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) <> SouffleAsNumber(B))
  else
    Result := SouffleBoolean(not WrappedValuesEqual(A, B));
end;

function AbstractRelationalComparison(
  const ARuntime: TGocciaRuntimeOperations;
  const A, B: TSouffleValue): Double;
var
  NumA, NumB: Double;
begin
  if SouffleIsStringValue(A) and SouffleIsStringValue(B) then
  begin
    if SouffleGetString(A) < SouffleGetString(B) then
      Result := -1
    else if SouffleGetString(A) > SouffleGetString(B) then
      Result := 1
    else
      Result := 0;
    Exit;
  end;
  NumA := ARuntime.CoerceToNumber(A);
  NumB := ARuntime.CoerceToNumber(B);
  if IsNaN(NumA) or IsNaN(NumB) then
    Result := NaN
  else
    Result := NumA - NumB;
end;

function TGocciaRuntimeOperations.LessThan(const A, B: TSouffleValue): TSouffleValue;
var
  Cmp: Double;
begin
  try
    Cmp := AbstractRelationalComparison(Self, A, B);
    if IsNaN(Cmp) then
      Result := SouffleBoolean(False)
    else
      Result := SouffleBoolean(Cmp < 0);
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.GreaterThan(const A, B: TSouffleValue): TSouffleValue;
var
  Cmp: Double;
begin
  try
    Cmp := AbstractRelationalComparison(Self, A, B);
    if IsNaN(Cmp) then
      Result := SouffleBoolean(False)
    else
      Result := SouffleBoolean(Cmp > 0);
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue;
var
  Cmp: Double;
begin
  try
    Cmp := AbstractRelationalComparison(Self, A, B);
    if IsNaN(Cmp) then
      Result := SouffleBoolean(False)
    else
      Result := SouffleBoolean(Cmp <= 0);
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue;
var
  Cmp: Double;
begin
  try
    Cmp := AbstractRelationalComparison(Self, A, B);
    if IsNaN(Cmp) then
      Result := SouffleBoolean(False)
    else
      Result := SouffleBoolean(Cmp >= 0);
  except
    on E: TGocciaThrowValue do RethrowAsVM(E);
  end;
end;

{ Logical / Type }

function TGocciaRuntimeOperations.LogicalNot(const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleBoolean(not SouffleIsTrue(A));
end;

function TGocciaRuntimeOperations.TypeOf(const A: TSouffleValue): TSouffleValue;
begin
  case A.Kind of
    svkNil:
      if A.Flags = GOCCIA_NIL_NULL then
        Result := SouffleString('object')
      else
        Result := SouffleString('undefined');
    svkBoolean:
      Result := SouffleString('boolean');
    svkInteger, svkFloat:
      Result := SouffleString('number');
    svkString:
      Result := SouffleString('string');
    svkReference:
    begin
      if not Assigned(A.AsReference) then
        Exit(SouffleString('undefined'));
      if (A.AsReference is TSouffleClosure) or
         (A.AsReference is TSouffleNativeFunction) or
         (A.AsReference is TSouffleBlueprint) or
         (A.AsReference is TGocciaBridgedFunction) then
        Result := SouffleString('function')
      else if A.AsReference is TSouffleHeapString then
        Result := SouffleString('string')
      else if A.AsReference is TGocciaWrappedValue then
      begin
        {$IFDEF BRIDGE_METRICS}
        Inc(GBridgeMetrics.TypeOfBridgeCount);
        {$ENDIF}
        Result := SouffleString(
          TGocciaWrappedValue(A.AsReference).Value.TypeOf);
      end
      else
        Result := SouffleString('object');
    end;
  else
    Result := SouffleString('undefined');
  end;
end;

function TGocciaRuntimeOperations.IsInstance(const A, B: TSouffleValue): TSouffleValue;

  function ResolveClassName: string;
  var
    GV: TGocciaValue;
  begin
    if SouffleIsReference(B) and Assigned(B.AsReference) then
    begin
      if B.AsReference is TSouffleBlueprint then
        Exit(TSouffleBlueprint(B.AsReference).Name);
      GV := UnwrapToGocciaValue(B);
      if GV is TGocciaClassValue then
        Exit(TGocciaClassValue(GV).Name);
    end;
    Result := '';
  end;

var
  GocciaObj, GocciaClass: TGocciaValue;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  ClassName: string;
  R: TGocciaValue;
begin
  if not SouffleIsReference(A) or not Assigned(A.AsReference) then
    Exit(SouffleBoolean(False));

  if A.AsReference is TSouffleClosure then
  begin
    ClassName := ResolveClassName;
    Exit(SouffleBoolean(ClassName = 'Function'));
  end;

  if A.AsReference is TSouffleBlueprint then
  begin
    ClassName := ResolveClassName;
    Exit(SouffleBoolean(ClassName = 'Function'));
  end;

  if A.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(A.AsReference);

    if SouffleIsReference(B) and Assigned(B.AsReference) and
       (B.AsReference is TSouffleBlueprint) then
    begin
      if Assigned(Rec.Blueprint) then
      begin
        Bp := Rec.Blueprint;
        while Assigned(Bp) do
        begin
          if Bp = TSouffleBlueprint(B.AsReference) then
            Exit(SouffleBoolean(True));
          Bp := Bp.SuperBlueprint;
        end;
      end;
      Exit(SouffleBoolean(False));
    end;

    GocciaClass := UnwrapToGocciaValue(B);
    if GocciaClass is TGocciaClassValue then
    begin
      ClassName := TGocciaClassValue(GocciaClass).Name;
      if ClassName = CONSTRUCTOR_OBJECT then
        Exit(SouffleBoolean(True));
      if Assigned(Rec.Blueprint) then
      begin
        Bp := Rec.Blueprint;
        while Assigned(Bp) do
        begin
          if Bp.Name = ClassName then
            Exit(SouffleBoolean(True));
          Bp := Bp.SuperBlueprint;
        end;
      end;
      Exit(SouffleBoolean(False));
    end;
  end;

  if SouffleIsReference(A) and Assigned(A.AsReference) and
     (A.AsReference is TGocciaWrappedValue) then
  begin
    GocciaObj := TGocciaWrappedValue(A.AsReference).Value;
    if SouffleIsReference(B) and Assigned(B.AsReference) and
       (B.AsReference is TGocciaBridgedFunction) then
      GocciaClass := TGocciaBridgedFunction(B.AsReference).FGocciaFn
    else if SouffleIsReference(B) and Assigned(B.AsReference) and
       (B.AsReference is TGocciaWrappedValue) then
      GocciaClass := TGocciaWrappedValue(B.AsReference).Value
    else
      GocciaClass := UnwrapToGocciaValue(B);

    R := EvaluateInstanceof(GocciaObj, GocciaClass, IsObjectInstanceOfClass);
    Result := SouffleBoolean(R = TGocciaBooleanLiteralValue.TrueValue);
    Exit;
  end;

  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.IsInstanceBridgeCount);
  {$ENDIF}
  GocciaObj := UnwrapToGocciaValue(A);
  GocciaClass := UnwrapToGocciaValue(B);

  R := EvaluateInstanceof(GocciaObj, GocciaClass, IsObjectInstanceOfClass);
  Result := SouffleBoolean(R = TGocciaBooleanLiteralValue.TrueValue);
end;

function TGocciaRuntimeOperations.HasProperty(
  const AObject, AKey: TSouffleValue): TSouffleValue;
var
  KeyStr: string;
  Prop: TGocciaValue;
  Idx: Int64;
  GocciaVal: TGocciaValue;
  Rec: TSouffleRecord;
begin
 try
  if SouffleIsStringValue(AObject) or
     (not SouffleIsReference(AObject) or not Assigned(AObject.AsReference)) then
  begin
    if SouffleIsNil(AObject) or
       SouffleIsBoolean(AObject) or SouffleIsInteger(AObject) or
       SouffleIsFloat(AObject) or SouffleIsStringValue(AObject) then
      ThrowTypeError('Cannot use ''in'' operator to search for ''' +
        SouffleValueToString(AKey) + ''' in ' + SouffleValueToString(AObject));
    Exit(SouffleBoolean(False));
  end;

  if SouffleIsReference(AKey) and Assigned(AKey.AsReference) and
     (AKey.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AKey.AsReference).Value is TGocciaSymbolValue) then
  begin
    GocciaVal := UnwrapToGocciaValue(AObject);
    if GocciaVal is TGocciaObjectValue then
      Exit(SouffleBoolean(TGocciaObjectValue(GocciaVal).HasSymbolProperty(
        TGocciaSymbolValue(TGocciaWrappedValue(AKey.AsReference).Value))));
    Exit(SouffleBoolean(False));
  end;

  KeyStr := SouffleValueToString(AKey);

  if AObject.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AObject.AsReference);
    while Assigned(Rec) do
    begin
      if Rec.Has(KeyStr) or
         (Rec.HasGetters and Rec.Getters.Has(KeyStr)) or
         (Rec.HasSetters and Rec.Setters.Has(KeyStr)) then
        Exit(SouffleBoolean(True));
      if Assigned(Rec.Blueprint) and Rec.Blueprint.Methods.Has(KeyStr) then
        Exit(SouffleBoolean(True));
      if Assigned(Rec.Delegate) and (Rec.Delegate is TSouffleRecord) then
        Rec := TSouffleRecord(Rec.Delegate)
      else
        Break;
    end;
    Exit(SouffleBoolean(False));
  end;

  if AObject.AsReference is TSouffleArray then
  begin
    if SouffleIsInteger(AKey) then
    begin
      Idx := AKey.AsInteger;
      if (Idx >= 0) and (Idx < TSouffleArray(AObject.AsReference).Count) then
        Exit(SouffleBoolean(not SouffleIsNil(
          TSouffleArray(AObject.AsReference).Get(Integer(Idx)))))
      else
        Exit(SouffleBoolean(False));
    end;
    if KeyStr = PROP_LENGTH then
      Exit(SouffleBoolean(True));
    if TryStrToInt64(KeyStr, Idx) then
    begin
      if (Idx >= 0) and (Idx < TSouffleArray(AObject.AsReference).Count) then
        Exit(SouffleBoolean(not SouffleIsNil(
          TSouffleArray(AObject.AsReference).Get(Integer(Idx)))))
      else
        Exit(SouffleBoolean(False));
    end;
    Prop := UnwrapToGocciaValue(AObject).GetProperty(KeyStr);
    Exit(SouffleBoolean(Assigned(Prop) and
      not (Prop is TGocciaUndefinedLiteralValue)));
  end;

  if AObject.AsReference is TGocciaWrappedValue then
  begin
    GocciaVal := TGocciaWrappedValue(AObject.AsReference).Value;
    if GocciaVal is TGocciaObjectValue then
      Exit(SouffleBoolean(TGocciaObjectValue(GocciaVal).HasProperty(KeyStr)));
    if GocciaVal is TGocciaArrayValue then
    begin
      if TryStrToInt64(KeyStr, Idx) then
        Exit(SouffleBoolean((Idx >= 0) and (Idx < TGocciaArrayValue(GocciaVal).Elements.Count)))
      else
      begin
        Prop := GocciaVal.GetProperty(KeyStr);
        Exit(SouffleBoolean(Assigned(Prop) and not (Prop is TGocciaUndefinedLiteralValue)));
      end;
    end;
    Prop := GocciaVal.GetProperty(KeyStr);
    Exit(SouffleBoolean(Assigned(Prop) and not (Prop is TGocciaUndefinedLiteralValue)));
  end;

  Result := SouffleBoolean(False);
 except
   on E: TGocciaThrowValue do
     RethrowAsVM(E);
 end;
end;

function TGocciaRuntimeOperations.ToBoolean(const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleBoolean(SouffleIsTrue(A));
end;

function TGocciaRuntimeOperations.ToPrimitive(const A: TSouffleValue): TSouffleValue;
begin
  Result := CoerceToPrimitiveSouffle(Self, A);
end;

{ Property access }

function TGocciaRuntimeOperations.GetProperty(const AObject: TSouffleValue;
  const AKey: string): TSouffleValue;
var
  GocciaObj, Prop: TGocciaValue;
  Boxed: TGocciaObjectValue;
  Rec: TSouffleRecord;
  Arr: TSouffleArray;
  Bp: TSouffleBlueprint;
  GetterVal: TSouffleValue;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.GetPropertyNativeCount);
  {$ENDIF}
  try
    if AObject.Kind = svkNil then
    begin
      if AObject.Flags = GOCCIA_NIL_NULL then
        ThrowTypeError('Cannot read properties of null (reading ''' + AKey + ''')')
      else
        ThrowTypeError('Cannot read properties of undefined (reading ''' + AKey + ''')');
    end;

    if SouffleIsStringValue(AObject) then
    begin
      if AKey = PROP_LENGTH then
        Exit(SouffleInteger(Length(SouffleGetString(AObject))));
      if Assigned(FStringDelegate) and FStringDelegate.Get(AKey, Result) then
        Exit;
    end;

    if SouffleIsNumeric(AObject) then
    begin
      if Assigned(FNumberDelegate) and FNumberDelegate.Get(AKey, Result) then
        Exit;
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
    begin
      if AObject.AsReference is TSouffleRecord then
      begin
        Rec := TSouffleRecord(AObject.AsReference);
        if Rec.HasGetters and Rec.Getters.Get(AKey, GetterVal) then
        begin
          if TryInvokeGetter(GetterVal, AObject, Result) then
            Exit;
          Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
        end;
        if Rec.Get(AKey, Result) then
          Exit;
        if Assigned(Rec.Blueprint) then
        begin
          Bp := Rec.Blueprint;
          while Assigned(Bp) do
          begin
            if Bp.HasGetters and Bp.Getters.Get(AKey, GetterVal) then
            begin
              if TryInvokeGetter(GetterVal, AObject, Result) then
                Exit;
              Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
            end;
            if Bp.Methods.Get(AKey, Result) then
              Exit;
            Bp := Bp.SuperBlueprint;
          end;
        end;
        while Assigned(Rec.Delegate) and (Rec.Delegate is TSouffleRecord) do
        begin
          Rec := TSouffleRecord(Rec.Delegate);
          if Rec.HasGetters and Rec.Getters.Get(AKey, GetterVal) then
          begin
            if TryInvokeGetter(GetterVal, AObject, Result) then
              Exit;
            Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
          end;
          if Rec.Get(AKey, Result) then
            Exit;
        end;
      end
      else if AObject.AsReference is TSouffleBlueprint then
      begin
        Bp := TSouffleBlueprint(AObject.AsReference);
        if AKey = PROP_PROTOTYPE then
          Exit(SouffleReference(Bp.Prototype))
        else if AKey = PROP_NAME then
          Exit(SouffleString(Bp.Name));

        if Bp.HasStaticGetters and Bp.StaticGetters.Get(AKey, GetterVal) then
        begin
          if TryInvokeGetter(GetterVal, AObject, Result) then
            Exit;
          Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
        end;

        if Bp.HasStaticFields and Bp.StaticFields.Get(AKey, Result) then
          Exit;

        if Bp.Methods.Get(AKey, Result) then
          Exit;

        // Walk super blueprint chain for inherited static getters, fields, methods
        Bp := Bp.SuperBlueprint;
        while Assigned(Bp) do
        begin
          if Bp.HasStaticGetters and Bp.StaticGetters.Get(AKey, GetterVal) then
          begin
            if TryInvokeGetter(GetterVal, AObject, Result) then
              Exit;
            Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
          end;
          if Bp.HasStaticFields and Bp.StaticFields.Get(AKey, Result) then
            Exit;
          Bp := Bp.SuperBlueprint;
        end;

        Bp := TSouffleBlueprint(AObject.AsReference);
        if FBlueprintSuperValues.ContainsKey(Bp) then
        begin
          GocciaObj := UnwrapToGocciaValue(FBlueprintSuperValues[Bp]);
          if Assigned(GocciaObj) then
          begin
            Prop := GocciaObj.GetProperty(AKey);
            if Assigned(Prop) and not (Prop is TGocciaUndefinedLiteralValue) then
              Exit(ToSouffleValue(Prop));
          end;
        end;

        Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
      end
      else if AObject.AsReference is TSouffleArray then
      begin
        Arr := TSouffleArray(AObject.AsReference);
        if AKey = PROP_LENGTH then
          Exit(SouffleInteger(Arr.Count));
        if Assigned(Arr.Delegate) and (Arr.Delegate is TSouffleRecord) and
           TSouffleRecord(Arr.Delegate).Get(AKey, Result) then
          Exit;
      end
      else if AObject.AsReference is TSouffleClosure then
      begin
        if AKey = PROP_NAME then
          Exit(SouffleString(TSouffleClosure(AObject.AsReference).Template.Name));
        if AKey = PROP_LENGTH then
          Exit(SouffleInteger(
            GetFormalParameterCount(TSouffleClosure(AObject.AsReference).Template)));
      end
      else if AObject.AsReference is TSouffleNativeFunction then
      begin
        if AKey = PROP_NAME then
          Exit(SouffleString(
            TSouffleNativeFunction(AObject.AsReference).Name));
        if AKey = PROP_LENGTH then
          Exit(SouffleInteger(
            TSouffleNativeFunction(AObject.AsReference).Arity));
        if Assigned(FDescribeDelegate) and
           (TSouffleNativeFunction(AObject.AsReference).Name = 'describe') and
           FDescribeDelegate.Get(AKey, Result) then
          Exit;
        if Assigned(FTestDelegate) and
           (TSouffleNativeFunction(AObject.AsReference).Name = 'test') and
           FTestDelegate.Get(AKey, Result) then
          Exit;
      end
      else if AObject.AsReference is TGocciaBridgedFunction then
      begin
        if AKey = PROP_NAME then
          Exit(SouffleString(
            TGocciaBridgedFunction(AObject.AsReference).Name));
        if AKey = PROP_LENGTH then
          Exit(SouffleInteger(
            TGocciaBridgedFunction(AObject.AsReference).Arity));
        if (AObject.AsReference = FPromiseConstructor) and
           Assigned(FPromiseStaticDelegate) and
           FPromiseStaticDelegate.Get(AKey, Result) then
          Exit;
        Prop := TGocciaBridgedFunction(AObject.AsReference).FGocciaFn
          .GetProperty(AKey);
        if Assigned(Prop) then
          Exit(ToSouffleValue(Prop));
      end;
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TGocciaSouffleMap) then
    begin
      if AKey = PROP_SIZE then
        Exit(SouffleInteger(TGocciaSouffleMap(AObject.AsReference).Count));
      if Assigned(FMapDelegate) and FMapDelegate.Get(AKey, Result) then
        Exit;
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TGocciaSouffleSet) then
    begin
      if AKey = PROP_SIZE then
        Exit(SouffleInteger(TGocciaSouffleSet(AObject.AsReference).Count));
      if Assigned(FSetDelegate) and FSetDelegate.Get(AKey, Result) then
        Exit;
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TGocciaWrappedValue) then
    begin
      GocciaObj := TGocciaWrappedValue(AObject.AsReference).Value;

      if GocciaObj is TGocciaMapValue then
      begin
        if AKey = PROP_SIZE then
          Exit(SouffleInteger(TGocciaMapValue(GocciaObj).Entries.Count));
        if Assigned(FMapDelegate) and FMapDelegate.Get(AKey, Result) then
          Exit;
      end
      else if GocciaObj is TGocciaSetValue then
      begin
        if AKey = PROP_SIZE then
          Exit(SouffleInteger(TGocciaSetValue(GocciaObj).Items.Count));
        if Assigned(FSetDelegate) and FSetDelegate.Get(AKey, Result) then
          Exit;
      end
      else if GocciaObj is TGocciaPromiseValue then
      begin
        if Assigned(FPromiseDelegate) and FPromiseDelegate.Get(AKey, Result) then
          Exit;
      end;

      Prop := GocciaObj.GetProperty(AKey);
      if not Assigned(Prop) then
      begin
        Boxed := GocciaObj.Box;
        if Assigned(Boxed) then
          Prop := Boxed.GetProperty(AKey);
      end;
      if Assigned(Prop) then
        Result := ToSouffleValue(Prop)
      else
        Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      Exit;
    end;

    {$IFDEF BRIDGE_METRICS}
    Inc(GBridgeMetrics.GetPropertyBridgeCount);
    {$ENDIF}
    GocciaObj := UnwrapToGocciaValue(AObject);
    Prop := GocciaObj.GetProperty(AKey);

    if not Assigned(Prop) then
    begin
      Boxed := GocciaObj.Box;
      if Assigned(Boxed) then
        Prop := Boxed.GetProperty(AKey);
    end;

    if Assigned(Prop) then
      Result := ToSouffleValue(Prop)
    else
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.SetProperty(const AObject: TSouffleValue;
  const AKey: string; const AValue: TSouffleValue);
var
  GocciaObj, Val: TGocciaValue;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  SetterVal: TSouffleValue;
begin
  try
    if AObject.Kind = svkNil then
    begin
      if AObject.Flags = GOCCIA_NIL_NULL then
        ThrowTypeError('Cannot set properties of null (setting ''' + AKey + ''')')
      else
        ThrowTypeError('Cannot set properties of undefined (setting ''' + AKey + ''')');
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
    begin
      if AObject.AsReference is TSouffleRecord then
      begin
        Rec := TSouffleRecord(AObject.AsReference);
        if Rec.HasSetters and Rec.Setters.Get(AKey, SetterVal) then
        begin
          if SouffleIsReference(SetterVal) and
             (SetterVal.AsReference is TSouffleClosure) then
            FVM.ExecuteFunction(
              TSouffleClosure(SetterVal.AsReference), [AObject, AValue]);
          Exit;
        end;
        if Assigned(Rec.Blueprint) then
        begin
          Bp := Rec.Blueprint;
          while Assigned(Bp) do
          begin
            if Bp.HasSetters and Bp.Setters.Get(AKey, SetterVal) then
            begin
              if SouffleIsReference(SetterVal) and
                 (SetterVal.AsReference is TSouffleClosure) then
                FVM.ExecuteFunction(
                  TSouffleClosure(SetterVal.AsReference), [AObject, AValue]);
              Exit;
            end;
            Bp := Bp.SuperBlueprint;
          end;
        end;
        if (AKey <> '') and (AKey[1] = '#') then
        begin
          Rec.PutWithFlags(AKey, AValue,
            SOUFFLE_PROP_WRITABLE or SOUFFLE_PROP_CONFIGURABLE);
        end
        else if not Rec.PutChecked(AKey, AValue) then
          ThrowTypeError('Cannot assign to read only property ''' + AKey + '''');
        Exit;
      end;
      if AObject.AsReference is TSouffleBlueprint then
      begin
        Bp := TSouffleBlueprint(AObject.AsReference);
        while Assigned(Bp) do
        begin
          if Bp.HasStaticSetters and Bp.StaticSetters.Get(AKey, SetterVal) then
          begin
            if SouffleIsReference(SetterVal) and
               (SetterVal.AsReference is TSouffleClosure) then
              FVM.ExecuteFunction(
                TSouffleClosure(SetterVal.AsReference), [AObject, AValue]);
            Exit;
          end;
          Bp := Bp.SuperBlueprint;
        end;
        TSouffleBlueprint(AObject.AsReference).StaticFields.Put(AKey, AValue);
        Exit;
      end;
      if AObject.AsReference is TGocciaWrappedValue then
      begin
        {$IFDEF BRIDGE_METRICS}
        Inc(GBridgeMetrics.SetPropertyBridgeCount);
        {$ENDIF}
        GocciaObj := TGocciaWrappedValue(AObject.AsReference).Value;
        Val := UnwrapToGocciaValue(AValue);
        GocciaObj.SetProperty(AKey, Val);
      end;
    end;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.CoerceKeyToString(
  const AKey: TSouffleValue): string;
begin
  Result := CoerceToString(AKey);
end;

function TGocciaRuntimeOperations.ComputedKeyAsString(
  const AKey: TSouffleValue): string;
begin
  if SouffleIsReference(AKey) and Assigned(AKey.AsReference) and
     (AKey.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AKey.AsReference).Value is TGocciaSymbolValue) then
    Result := '@@sym:' + IntToStr(
      TGocciaSymbolValue(TGocciaWrappedValue(AKey.AsReference).Value).Id)
  else
    Result := CoerceToString(AKey);
end;

function TGocciaRuntimeOperations.GetIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
var
  GocciaObj: TGocciaValue;
  SymKey: TGocciaSymbolValue;
  PropVal: TGocciaValue;
  I: Integer;
  Found: Boolean;
begin
  try
    if AObject.Kind = svkNil then
    begin
      if AObject.Flags = GOCCIA_NIL_NULL then
        ThrowTypeError('Cannot read properties of null (reading ''' + CoerceKeyToString(AKey) + ''')')
      else
        ThrowTypeError('Cannot read properties of undefined (reading ''' + CoerceKeyToString(AKey) + ''')');
    end;

    if SouffleIsStringValue(AObject) and (AKey.Kind = svkInteger) then
    begin
      I := Integer(AKey.AsInteger);
      if (I >= 0) and (I < Length(SouffleGetString(AObject))) then
        Exit(SouffleString(SouffleGetString(AObject)[I + 1]))
      else
        Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    end
    else if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TSouffleArray) and (AKey.Kind = svkInteger) then
    begin
      Result := TSouffleArray(AObject.AsReference).Get(Integer(AKey.AsInteger));
      if SouffleIsHole(Result) then
        Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
    end
    else if SouffleIsReference(AKey) and Assigned(AKey.AsReference) and
       (AKey.AsReference is TGocciaWrappedValue) and
       (TGocciaWrappedValue(AKey.AsReference).Value is TGocciaSymbolValue) then
    begin
      SymKey := TGocciaSymbolValue(TGocciaWrappedValue(AKey.AsReference).Value);

      Result := GetSymbolOnNativeObject(AObject, SymKey, Found);
      if Found then
        Exit;

      GocciaObj := UnwrapToGocciaValue(AObject);
      if GocciaObj is TGocciaClassValue then
      begin
        PropVal := TGocciaClassValue(GocciaObj).GetSymbolProperty(SymKey);
        if Assigned(PropVal) and
           not (PropVal is TGocciaUndefinedLiteralValue) then
          Exit(ToSouffleValue(PropVal));
      end
      else if GocciaObj is TGocciaObjectValue then
      begin
        PropVal := TGocciaObjectValue(GocciaObj).GetSymbolProperty(SymKey);
        if Assigned(PropVal) then
          Exit(ToSouffleValue(PropVal));
      end
      else if GocciaObj.IsPrimitive then
      begin
        PropVal := nil;
        if Assigned(GocciaObj.Box) then
          PropVal := GocciaObj.Box.GetSymbolProperty(SymKey);
        if Assigned(PropVal) then
          Exit(ToSouffleValue(PropVal));
      end;
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
    end
    else
      Result := GetProperty(AObject, CoerceKeyToString(AKey));
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.SetIndex(const AObject: TSouffleValue;
  const AKey, AValue: TSouffleValue);
var
  GocciaObj: TGocciaValue;
  SymKey: TGocciaSymbolValue;
  ScopeObj: TObject;
  ClassKey: TObject;
begin
  try
    if AObject.Kind = svkNil then
    begin
      if AObject.Flags = GOCCIA_NIL_NULL then
        ThrowTypeError('Cannot set properties of null (setting ''' + CoerceKeyToString(AKey) + ''')')
      else
        ThrowTypeError('Cannot set properties of undefined (setting ''' + CoerceKeyToString(AKey) + ''')');
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TSouffleArray) and (AKey.Kind = svkInteger) then
      TSouffleArray(AObject.AsReference).Put(Integer(AKey.AsInteger), AValue)
    else if SouffleIsReference(AKey) and Assigned(AKey.AsReference) and
       (AKey.AsReference is TGocciaWrappedValue) and
       (TGocciaWrappedValue(AKey.AsReference).Value is TGocciaSymbolValue) then
    begin
      SymKey := TGocciaSymbolValue(TGocciaWrappedValue(AKey.AsReference).Value);

      if SetSymbolOnNativeObject(AObject, SymKey, AValue) then
        Exit;

      GocciaObj := UnwrapToGocciaValue(AObject);
      ClassKey := nil;
      if GocciaObj is TGocciaClassValue then
      begin
        TGocciaClassValue(GocciaObj).AssignSymbolProperty(
          SymKey, UnwrapToGocciaValue(AValue));
        ClassKey := GocciaObj;
      end
      else if GocciaObj is TGocciaObjectValue then
      begin
        TGocciaObjectValue(GocciaObj).AssignSymbolProperty(
          SymKey, UnwrapToGocciaValue(AValue));
        if GocciaObj is TGocciaInstanceValue then
          ClassKey := TGocciaInstanceValue(GocciaObj).ClassValue;
      end;
      if Assigned(ClassKey) and
         FClassDefinitionScopes.TryGetValue(ClassKey, ScopeObj) then
        SyncScopeToGlobals(Self, TGocciaScope(ScopeObj));
    end
    else
      SetProperty(AObject, CoerceKeyToString(AKey), AValue);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.DeleteProperty(const AObject: TSouffleValue;
  const AKey: string): TSouffleValue;
var
  GocciaObj: TGocciaValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
  begin
    if AObject.AsReference is TSouffleRecord then
    begin
      if TSouffleRecord(AObject.AsReference).Has(AKey) and
         (TSouffleRecord(AObject.AsReference).GetEntryFlags(AKey)
           and SOUFFLE_PROP_CONFIGURABLE = 0) then
      begin
        try
          ThrowTypeError('Cannot delete property ''' + AKey + '''');
        except
          on E: TGocciaThrowValue do
            RethrowAsVM(E);
        end;
        Exit(SouffleBoolean(False));
      end;
      Result := SouffleBoolean(TSouffleRecord(AObject.AsReference).Delete(AKey));
      Exit;
    end;
    if AObject.AsReference is TGocciaWrappedValue then
    begin
      GocciaObj := TGocciaWrappedValue(AObject.AsReference).Value;
      if GocciaObj is TGocciaObjectValue then
      begin
        TGocciaObjectValue(GocciaObj).DeleteProperty(AKey);
        Result := SouffleBoolean(True);
        Exit;
      end;
    end;
  end;
  Result := SouffleBoolean(False);
end;

function TGocciaRuntimeOperations.DeleteIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
var
  Idx: Int64;
begin
  if SouffleIsReference(AObject) and (AObject.AsReference is TSouffleArray) then
  begin
    Idx := Trunc(SouffleAsNumber(AKey));
    if (Idx >= 0) and (Idx < TSouffleArray(AObject.AsReference).Count) then
      TSouffleArray(AObject.AsReference).Put(Integer(Idx),
        SouffleNilWithFlags(GOCCIA_NIL_HOLE));
    Exit(SouffleBoolean(True));
  end;
  Result := DeleteProperty(AObject, CoerceKeyToString(AKey));
end;

{ Invocation }

function TGocciaRuntimeOperations.Invoke(const ACallee: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AReceiver: TSouffleValue): TSouffleValue;
var
  GocciaCallee, GocciaReceiver, GocciaResult: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  DirectArgs: array of TSouffleValue;
  I: Integer;
begin
  if SouffleIsReference(ACallee) and Assigned(ACallee.AsReference) then
  begin
    if ACallee.AsReference is TSouffleClosure then
    begin
      {$IFDEF BRIDGE_METRICS}
      Inc(GBridgeMetrics.InvokeClosureDirectCount);
      {$ENDIF}
      SetLength(DirectArgs, AArgCount + 1);
      DirectArgs[0] := AReceiver;
      if AArgCount > 0 then
        Move(AArgs^, DirectArgs[1], AArgCount * SizeOf(TSouffleValue));
      Result := FVM.ExecuteFunction(
        TSouffleClosure(ACallee.AsReference), DirectArgs);
      Exit;
    end;

    if ACallee.AsReference is TSouffleNativeFunction then
    begin
      {$IFDEF BRIDGE_METRICS}
      Inc(GBridgeMetrics.InvokeNativeDirectCount);
      {$ENDIF}
      try
        Result := TSouffleNativeFunction(ACallee.AsReference).Invoke(
          AReceiver, AArgs, AArgCount);
      except
        on E: TGocciaThrowValue do
          RethrowAsVM(E);
      end;
      Exit;
    end;

    if ACallee.AsReference is TGocciaSuperCallHelper then
    begin
      {$IFDEF BRIDGE_METRICS}
      Inc(GBridgeMetrics.InvokeNativeDirectCount);
      {$ENDIF}
      try
        Result := TGocciaSuperCallHelper(ACallee.AsReference).Invoke(
          AReceiver, AArgs, AArgCount);
      except
        on E: TGocciaThrowValue do
          RethrowAsVM(E);
      end;
      Exit;
    end;

    if ACallee.AsReference is TGocciaBridgedFunction then
    begin
      {$IFDEF BRIDGE_METRICS}
      Inc(GBridgeMetrics.InvokeNativeDirectCount);
      {$ENDIF}
      try
        Result := TGocciaBridgedFunction(ACallee.AsReference).Invoke(
          AReceiver, AArgs, AArgCount);
      except
        on E: TGocciaThrowValue do
          RethrowAsVM(E);
      end;
      Exit;
    end;
  end;

  if SouffleIsReference(ACallee) and Assigned(ACallee.AsReference) and
     (ACallee.AsReference is TGocciaWrappedValue) then
  begin
    GocciaCallee := TGocciaWrappedValue(ACallee.AsReference).Value;
    if GocciaCallee is TGocciaFunctionBase then
    begin
      {$IFDEF BRIDGE_METRICS}
      Inc(GBridgeMetrics.InvokeNativeDirectCount);
      {$ENDIF}
      try
        Args := TGocciaArgumentsCollection.Create;
        try
          for I := 0 to AArgCount - 1 do
            Args.Add(UnwrapToGocciaValue(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
          GocciaReceiver := UnwrapToGocciaValue(AReceiver);
          GocciaResult := TGocciaFunctionBase(GocciaCallee).Call(Args, GocciaReceiver);
          if Assigned(GocciaResult) then
            Result := ToSouffleValue(GocciaResult)
          else
            Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
        finally
          Args.Free;
        end;
      except
        on E: TGocciaThrowValue do
          RethrowAsVM(E);
      end;
      Exit;
    end;
  end;

  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.InvokeGocciaCount);
  {$ENDIF}
  try
    if SouffleIsReference(ACallee) and Assigned(ACallee.AsReference) and
       (ACallee.AsReference is TSouffleBlueprint) then
    begin
      ThrowTypeError('Class constructor ' +
        TSouffleBlueprint(ACallee.AsReference).Name +
        ' cannot be invoked without ''new''');
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      Exit;
    end;
    GocciaCallee := UnwrapToGocciaValue(ACallee);
    if not GocciaCallee.IsCallable then
    begin
      ThrowTypeError(GocciaCallee.ToStringLiteral.Value + ' is not a function');
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      Exit;
    end;

    Args := TGocciaArgumentsCollection.Create;
    try
      for I := 0 to AArgCount - 1 do
        Args.Add(UnwrapToGocciaValue(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));

      GocciaReceiver := UnwrapToGocciaValue(AReceiver);
      if GocciaCallee is TGocciaFunctionBase then
        GocciaResult := TGocciaFunctionBase(GocciaCallee).Call(Args, GocciaReceiver)
      else if GocciaCallee is TGocciaClassValue then
        GocciaResult := TGocciaClassValue(GocciaCallee).Call(Args, GocciaReceiver)
      else
        GocciaResult := nil;

      if Assigned(GocciaResult) then
        Result := ToSouffleValue(GocciaResult)
      else
        Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
    finally
      Args.Free;
    end;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.Construct(const AConstructor: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
{$IFDEF BRIDGE_METRICS}
  procedure MetricNative; begin Inc(AGBridgeMetrics.ConstructNativeCount); end;
  procedure MetricBridge; begin Inc(AGBridgeMetrics.ConstructGocciaCount); end;
{$ENDIF}
var
  GocciaConstructor, GocciaResult: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  Context: TGocciaEvaluationContext;
  CachedScope, CachedBridge: TObject;
  I: Integer;
  Bp, WalkBp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  CtorMethod: TSouffleValue;
  VMArgs: array of TSouffleValue;
  FieldInits: array of TSouffleClosure;
  FieldInitCount: Integer;
begin
  if SouffleIsReference(AConstructor) and
     Assigned(AConstructor.AsReference) and
     (AConstructor.AsReference is TSouffleBlueprint) then
  begin
    Bp := TSouffleBlueprint(AConstructor.AsReference);

    WalkBp := Bp;
    while Assigned(WalkBp.SuperBlueprint) do
      WalkBp := WalkBp.SuperBlueprint;
    if not FBlueprintSuperValues.ContainsKey(WalkBp) then
    begin
      Rec := TSouffleRecord.CreateFromBlueprint(Bp);
      Rec.Delegate := Bp.Prototype;

      if not Assigned(Bp.Prototype.Delegate) then
      begin
        WalkBp := Bp;
        while Assigned(WalkBp) do
        begin
          if not Assigned(WalkBp.Prototype.Delegate) then
            WalkBp.Prototype.Delegate := WalkBp.Methods;
          if not Assigned(WalkBp.Methods.Delegate) then
          begin
            if Assigned(WalkBp.SuperBlueprint) then
              WalkBp.Methods.Delegate := WalkBp.SuperBlueprint.Prototype
            else if Assigned(FVM) then
              WalkBp.Methods.Delegate := FVM.RecordDelegate;
          end;
          if not WalkBp.Prototype.Get(PROP_CONSTRUCTOR, CtorMethod) then
            WalkBp.Prototype.Put(PROP_CONSTRUCTOR, SouffleReference(WalkBp));
          WalkBp := WalkBp.SuperBlueprint;
        end;
      end;

      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(Rec);

      FieldInitCount := 0;
      SetLength(FieldInits, 4);
      WalkBp := Bp;
      while Assigned(WalkBp) do
      begin
        if WalkBp.Methods.Get('__fields__', CtorMethod) and
           SouffleIsReference(CtorMethod) and
           (CtorMethod.AsReference is TSouffleClosure) then
        begin
          if FieldInitCount >= Length(FieldInits) then
            SetLength(FieldInits, Length(FieldInits) * 2);
          FieldInits[FieldInitCount] := TSouffleClosure(CtorMethod.AsReference);
          Inc(FieldInitCount);
        end;
        WalkBp := WalkBp.SuperBlueprint;
      end;
      if FieldInitCount > 0 then
      begin
        SetLength(VMArgs, 1);
        VMArgs[0] := SouffleReference(Rec);
        for I := FieldInitCount - 1 downto 0 do
          FVM.ExecuteFunction(FieldInits[I], VMArgs);
      end;

      WalkBp := Bp;
      CtorMethod := SouffleNil;
      while Assigned(WalkBp) do
      begin
        if WalkBp.Methods.Get('constructor', CtorMethod) then
          Break;
        WalkBp := WalkBp.SuperBlueprint;
      end;

      if SouffleIsReference(CtorMethod) and
         (CtorMethod.AsReference is TSouffleClosure) then
      begin
        SetLength(VMArgs, AArgCount + 1);
        VMArgs[0] := SouffleReference(Rec);
        for I := 0 to AArgCount - 1 do
          VMArgs[1 + I] := PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^;
        FVM.ExecuteFunction(TSouffleClosure(CtorMethod.AsReference), VMArgs);
      end;

      Result := SouffleReference(Rec);
      {$IFDEF BRIDGE_METRICS}
      MetricNative;
      {$ENDIF}
      Exit;
    end;
  end;

  PushVMFramesToGoccia(FVM);
  try try
    if SouffleIsReference(AConstructor) and
       Assigned(AConstructor.AsReference) and
       (AConstructor.AsReference is TSouffleBlueprint) then
    begin
      Bp := TSouffleBlueprint(AConstructor.AsReference);

      if not FBlueprintBridgeCache.TryGetValue(Bp, CachedBridge) then
      begin
        CachedBridge := ConvertBlueprintToClassValue(Bp, Self);
        FBlueprintBridgeCache.Add(Bp, CachedBridge);
      end;
      GocciaConstructor := TGocciaValue(CachedBridge);
      {$IFDEF BRIDGE_METRICS}
      MetricBridge;
      Inc(GBridgeMetrics.ConstructClassValueCount);
      {$ENDIF}
      Args := TGocciaArgumentsCollection.Create;
      try
        for I := 0 to AArgCount - 1 do
          Args.Add(UnwrapToGocciaValue(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));

        if FClassDefinitionScopes.TryGetValue(GocciaConstructor, CachedScope) then
        begin
          Context := TGocciaEngine(FEngine).Interpreter.CreateEvaluationContext;
          Context.Scope := TGocciaScope(CachedScope);
          RebuildArrayBridgeCache(Self, TGocciaScope(CachedScope));
        end
        else
          Context := CreateBridgedContext(Self);

        GocciaResult := InstantiateClass(
          TGocciaClassValue(GocciaConstructor), Args, Context);

        SyncArraysBack(Self, Context.Scope);
        FArrayBridgeCache.Clear;
        FArrayBridgeDirty := False;

        if Assigned(GocciaResult) then
          Result := ToSouffleValue(GocciaResult)
        else
          Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      finally
        Args.Free;
      end;
      Exit;
    end;

    if SouffleIsReference(AConstructor) and
       Assigned(AConstructor.AsReference) and
       (AConstructor.AsReference is TGocciaBridgedFunction) then
    begin
      {$IFDEF BRIDGE_METRICS}
      MetricNative;
      {$ENDIF}
      Args := TGocciaArgumentsCollection.Create;
      try
        for I := 0 to AArgCount - 1 do
          Args.Add(UnwrapToGocciaValue(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));

        if Assigned(TGocciaCallStack.Instance) then
          TGocciaCallStack.Instance.Push(
            TGocciaBridgedFunction(AConstructor.AsReference).Name, '', 0, 0);
        try
          GocciaResult := TGocciaBridgedFunction(AConstructor.AsReference).FGocciaFn.Call(
            Args, TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          if Assigned(TGocciaCallStack.Instance) then
            TGocciaCallStack.Instance.Pop;
        end;

        if Assigned(GocciaResult) then
          Result := ToSouffleValue(GocciaResult)
        else
          Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      finally
        Args.Free;
      end;
      Exit;
    end;

    {$IFDEF BRIDGE_METRICS}
    MetricBridge;
    {$ENDIF}
    GocciaConstructor := UnwrapToGocciaValue(AConstructor);

    if (GocciaConstructor is TGocciaClassValue) then
    begin
      {$IFDEF BRIDGE_METRICS}
      Inc(GBridgeMetrics.ConstructClassValueCount);
      {$ENDIF}
      Args := TGocciaArgumentsCollection.Create;
      try
        for I := 0 to AArgCount - 1 do
          Args.Add(UnwrapToGocciaValue(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));

        if FClassDefinitionScopes.TryGetValue(GocciaConstructor, CachedScope) then
        begin
          Context := TGocciaEngine(FEngine).Interpreter.CreateEvaluationContext;
          Context.Scope := TGocciaScope(CachedScope);
          RebuildArrayBridgeCache(Self, TGocciaScope(CachedScope));
        end
        else
          Context := CreateBridgedContext(Self);

        GocciaResult := InstantiateClass(
          TGocciaClassValue(GocciaConstructor), Args, Context);

        SyncArraysBack(Self, Context.Scope);
        FArrayBridgeCache.Clear;
        FArrayBridgeDirty := False;

        if Assigned(GocciaResult) then
          Result := ToSouffleValue(GocciaResult)
        else
          Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      finally
        Args.Free;
      end;
      Exit;
    end;

    if GocciaConstructor is TGocciaNativeFunctionValue then
    begin
      {$IFDEF BRIDGE_METRICS}
      Inc(GBridgeMetrics.ConstructNativeFnCount);
      {$ENDIF}
      Args := TGocciaArgumentsCollection.Create;
      try
        for I := 0 to AArgCount - 1 do
          Args.Add(UnwrapToGocciaValue(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));

        if Assigned(TGocciaCallStack.Instance) then
          TGocciaCallStack.Instance.Push(
            TGocciaNativeFunctionValue(GocciaConstructor).Name, '', 0, 0);
        try
          GocciaResult := TGocciaNativeFunctionValue(GocciaConstructor).Call(
            Args, TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          if Assigned(TGocciaCallStack.Instance) then
            TGocciaCallStack.Instance.Pop;
        end;

        if Assigned(GocciaResult) then
          Result := ToSouffleValue(GocciaResult)
        else
          Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      finally
        Args.Free;
      end;
      Exit;
    end;

    ThrowTypeError(GocciaConstructor.TypeName + ' is not a constructor');
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
  finally
    PopVMFramesFromGoccia(FVM);
  end;
end;

{ Iteration }

function TGocciaRuntimeOperations.GetIterator(
  const AIterable: TSouffleValue;
  const ATryAsync: Boolean): TSouffleValue;
var
  GocciaVal: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.GetIteratorCount);
  {$ENDIF}
  try
    if (not ATryAsync) and SouffleIsReference(AIterable) and
       Assigned(AIterable.AsReference) then
    begin
      if AIterable.AsReference is TSouffleArray then
      begin
        Result := SouffleReference(
          TGocciaSouffleArrayIterator.Create(
            TSouffleArray(AIterable.AsReference)));
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(
            TGocciaSouffleArrayIterator(Result.AsReference));
        Exit;
      end;
      if AIterable.AsReference is TSouffleHeapString then
      begin
        Result := SouffleReference(
          TGocciaSouffleStringIterator.Create(
            TSouffleHeapString(AIterable.AsReference).Value));
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(
            TGocciaSouffleStringIterator(Result.AsReference));
        Exit;
      end;
      if AIterable.AsReference is TGocciaWrappedValue then
      begin
        GocciaVal := TGocciaWrappedValue(AIterable.AsReference).Value;
        if GocciaVal is TGocciaMapValue then
        begin
          Iterator := TGocciaMapIteratorValue.Create(GocciaVal, mkEntries);
          Result := WrapGocciaValue(Iterator);
          Exit;
        end;
        if GocciaVal is TGocciaSetValue then
        begin
          Iterator := TGocciaSetIteratorValue.Create(GocciaVal, skValues);
          Result := WrapGocciaValue(Iterator);
          Exit;
        end;
      end;
    end;

    GocciaVal := UnwrapToGocciaValue(AIterable);

    Iterator := nil;

    if ATryAsync and (GocciaVal is TGocciaObjectValue) then
    begin
      IteratorMethod := TGocciaObjectValue(GocciaVal).GetSymbolProperty(
        TGocciaSymbolValue.WellKnownAsyncIterator);
      if Assigned(IteratorMethod) and
         not (IteratorMethod is TGocciaUndefinedLiteralValue) and
         IteratorMethod.IsCallable then
      begin
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(
            CallArgs, GocciaVal);
        finally
          CallArgs.Free;
        end;
        if Assigned(IteratorObj) then
        begin
          Result := WrapGocciaValue(IteratorObj);
          Exit;
        end;
      end;
    end;

    if Iterator = nil then
    begin
      if GocciaVal is TGocciaIteratorValue then
        Iterator := TGocciaIteratorValue(GocciaVal)
      else if GocciaVal is TGocciaStringLiteralValue then
        Iterator := TGocciaStringIteratorValue.Create(GocciaVal)
      else if GocciaVal is TGocciaObjectValue then
      begin
        IteratorMethod := TGocciaObjectValue(GocciaVal).GetSymbolProperty(
          TGocciaSymbolValue.WellKnownIterator);
        if Assigned(IteratorMethod) and
           not (IteratorMethod is TGocciaUndefinedLiteralValue) and
           IteratorMethod.IsCallable then
        begin
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(
              CallArgs, GocciaVal);
          finally
            CallArgs.Free;
          end;
          if IteratorObj is TGocciaIteratorValue then
            Iterator := TGocciaIteratorValue(IteratorObj)
          else if IteratorObj is TGocciaObjectValue then
          begin
            NextMethod := IteratorObj.GetProperty(PROP_NEXT);
            if Assigned(NextMethod) and
               not (NextMethod is TGocciaUndefinedLiteralValue) and
               NextMethod.IsCallable then
              Iterator := TGocciaGenericIteratorValue.Create(IteratorObj);
          end;
        end;
      end;
    end;

    if Iterator = nil then
    begin
      ThrowTypeError(GocciaVal.TypeName + ' is not iterable');
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      Exit;
    end;

    Result := WrapGocciaValue(Iterator);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.IteratorNext(const AIterator: TSouffleValue;
  out ADone: Boolean): TSouffleValue;
var
  GocciaVal: TGocciaValue;
  Value, NextMethod, NextResult, Resolved, DoneValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  NewArr: TSouffleArray;
  I: Integer;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.IteratorNextCount);
  {$ENDIF}
  ADone := True;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  try
    if SouffleIsReference(AIterator) and Assigned(AIterator.AsReference) then
    begin
      if AIterator.AsReference is TGocciaSouffleArrayIterator then
      begin
        Result := TGocciaSouffleArrayIterator(AIterator.AsReference).Next(ADone);
        Exit;
      end;
      if AIterator.AsReference is TGocciaSouffleStringIterator then
      begin
        Result := TGocciaSouffleStringIterator(AIterator.AsReference).Next(ADone);
        Exit;
      end;
    end;

    GocciaVal := UnwrapToGocciaValue(AIterator);

    if GocciaVal is TGocciaIteratorValue then
    begin
      Value := TGocciaIteratorValue(GocciaVal).DirectNext(ADone);
      if not ADone then
      begin
        if (Value is TGocciaArrayValue) and
           not FArrayBridgeReverse.ContainsKey(Value) then
        begin
          NewArr := TSouffleArray.Create(TGocciaArrayValue(Value).Elements.Count);
          for I := 0 to TGocciaArrayValue(Value).Elements.Count - 1 do
            NewArr.Push(ToSouffleValue(TGocciaArrayValue(Value).Elements[I]));
          if Assigned(TGarbageCollector.Instance) then
            TGarbageCollector.Instance.AllocateObject(NewArr);
          Result := SouffleReference(NewArr);
        end
        else
          Result := ToSouffleValue(Value);
      end;
    end
    else if GocciaVal is TGocciaObjectValue then
    begin
      NextMethod := GocciaVal.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or not NextMethod.IsCallable then
        Exit;

      CallArgs := TGocciaArgumentsCollection.Create;
      try
        NextResult := TGocciaFunctionBase(NextMethod).Call(CallArgs, GocciaVal);
      finally
        CallArgs.Free;
      end;

      Resolved := Goccia.Evaluator.AwaitValue(NextResult);
      if Resolved.IsPrimitive then
      begin
        ThrowTypeError('Iterator result ' +
          Resolved.ToStringLiteral.Value + ' is not an object');
        Exit;
      end;

      DoneValue := Resolved.GetProperty(PROP_DONE);
      ADone := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      if not ADone then
      begin
        Value := Resolved.GetProperty(PROP_VALUE);
        if not Assigned(Value) then
          Value := TGocciaUndefinedLiteralValue.UndefinedValue;
        Result := ToSouffleValue(Value);
      end;
    end;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.SpreadInto(
  const ATarget, ASource: TSouffleValue);
var
  TgtArr: TSouffleArray;
  SrcArr: TSouffleArray;
  I: Integer;
  StrVal: string;
  IterVal: TSouffleValue;
  ElemVal: TSouffleValue;
  Done: Boolean;
begin
  try
    if not (SouffleIsReference(ATarget) and (ATarget.AsReference is TSouffleArray)) then
      Exit;

    TgtArr := TSouffleArray(ATarget.AsReference);

    if SouffleIsNil(ASource) then
    begin
      ThrowTypeError('Cannot spread a non-iterable value');
      Exit;
    end;

    if SouffleIsStringValue(ASource) then
    begin
      StrVal := SouffleGetString(ASource);
      for I := 1 to Length(StrVal) do
        TgtArr.Push(SouffleString(StrVal[I]));
      Exit;
    end;

    if not SouffleIsReference(ASource) then
    begin
      ThrowTypeError('Cannot spread a non-iterable value');
      Exit;
    end;

    if ASource.AsReference is TSouffleArray then
    begin
      SrcArr := TSouffleArray(ASource.AsReference);
      for I := 0 to SrcArr.Count - 1 do
        TgtArr.Push(SrcArr.Get(I));
      Exit;
    end;

    IterVal := GetIterator(ASource);
    repeat
      ElemVal := IteratorNext(IterVal, Done);
      if not Done then
        TgtArr.Push(ElemVal);
    until Done;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.SpreadObjectInto(
  const ATarget, ASource: TSouffleValue);
var
  SrcRec: TSouffleRecord;
  SrcGoccia, TgtGoccia: TGocciaValue;
  SrcObj, TgtObj: TGocciaObjectValue;
  Key: string;
  I: Integer;
  SymPair: TPair<TGocciaSymbolValue, TGocciaValue>;
begin
  if not SouffleIsReference(ATarget) or not Assigned(ATarget.AsReference) then
    Exit;
  if SouffleIsNil(ASource) then
    Exit;

  if ATarget.AsReference is TSouffleRecord then
  begin
    if SouffleIsReference(ASource) and (ASource.AsReference is TSouffleRecord) then
    begin
      SrcRec := TSouffleRecord(ASource.AsReference);
      for I := 0 to SrcRec.Count - 1 do
      begin
        Key := SrcRec.GetOrderedKey(I);
        if SrcRec.GetEntryFlags(Key) and SOUFFLE_PROP_ENUMERABLE <> 0 then
          TSouffleRecord(ATarget.AsReference).Put(Key, SrcRec.GetOrderedValue(I));
      end;
      if SrcRec.HasGetters then
        for I := 0 to SrcRec.Getters.Count - 1 do
        begin
          Key := SrcRec.Getters.GetOrderedKey(I);
          TSouffleRecord(ATarget.AsReference).Put(Key,
            GetProperty(ASource, Key));
        end;
      SrcGoccia := UnwrapToGocciaValue(ASource);
      TgtGoccia := UnwrapToGocciaValue(ATarget);
      if (SrcGoccia is TGocciaObjectValue) and (TgtGoccia is TGocciaObjectValue) then
        for SymPair in TGocciaObjectValue(SrcGoccia).GetEnumerableSymbolProperties do
          TGocciaObjectValue(TgtGoccia).AssignSymbolProperty(SymPair.Key, SymPair.Value);
      Exit;
    end;
    if SouffleIsStringValue(ASource) then
    begin
      Key := SouffleGetString(ASource);
      for I := 1 to Length(Key) do
        TSouffleRecord(ATarget.AsReference).Put(IntToStr(I - 1),
          SouffleString(Key[I]));
      Exit;
    end;
    if SouffleIsReference(ASource) and Assigned(ASource.AsReference) and
       (ASource.AsReference is TGocciaWrappedValue) then
    begin
      SrcGoccia := TGocciaWrappedValue(ASource.AsReference).Value;
      if SrcGoccia is TGocciaStringLiteralValue then
      begin
        Key := TGocciaStringLiteralValue(SrcGoccia).Value;
        for I := 1 to Length(Key) do
          TSouffleRecord(ATarget.AsReference).Put(IntToStr(I - 1),
            SouffleString(Key[I]));
      end
      else if SrcGoccia is TGocciaObjectValue then
      begin
        for Key in TGocciaObjectValue(SrcGoccia).GetEnumerablePropertyNames do
          TSouffleRecord(ATarget.AsReference).Put(Key,
            ToSouffleValue(TGocciaObjectValue(SrcGoccia).GetProperty(Key)));
        TgtGoccia := UnwrapToGocciaValue(ATarget);
        if TgtGoccia is TGocciaObjectValue then
          for SymPair in TGocciaObjectValue(SrcGoccia).GetEnumerableSymbolProperties do
            TGocciaObjectValue(TgtGoccia).AssignSymbolProperty(SymPair.Key, SymPair.Value);
      end
      else if SrcGoccia is TGocciaArrayValue then
      begin
        for I := 0 to TGocciaArrayValue(SrcGoccia).Elements.Count - 1 do
          if TGocciaArrayValue(SrcGoccia).Elements[I] <> nil then
            TSouffleRecord(ATarget.AsReference).Put(IntToStr(I),
              ToSouffleValue(TGocciaArrayValue(SrcGoccia).Elements[I]));
      end;
      Exit;
    end;
  end;

  if ATarget.AsReference is TGocciaWrappedValue then
    TgtGoccia := TGocciaWrappedValue(ATarget.AsReference).Value
  else
    TgtGoccia := nil;

  if not (TgtGoccia is TGocciaObjectValue) then
    Exit;
  TgtObj := TGocciaObjectValue(TgtGoccia);

  if SouffleIsReference(ASource) and Assigned(ASource.AsReference) then
  begin
    if ASource.AsReference is TSouffleRecord then
    begin
      SrcRec := TSouffleRecord(ASource.AsReference);
      for I := 0 to SrcRec.Count - 1 do
      begin
        Key := SrcRec.GetOrderedKey(I);
        if SrcRec.GetEntryFlags(Key) and SOUFFLE_PROP_ENUMERABLE <> 0 then
          TgtObj.SetProperty(Key, UnwrapToGocciaValue(SrcRec.GetOrderedValue(I)));
      end;
      if SrcRec.HasGetters then
        for I := 0 to SrcRec.Getters.Count - 1 do
        begin
          Key := SrcRec.Getters.GetOrderedKey(I);
          TgtObj.SetProperty(Key,
            UnwrapToGocciaValue(GetProperty(ASource, Key)));
        end;
    end
    else if ASource.AsReference is TGocciaWrappedValue then
    begin
      SrcGoccia := TGocciaWrappedValue(ASource.AsReference).Value;
      if SrcGoccia is TGocciaObjectValue then
      begin
        SrcObj := TGocciaObjectValue(SrcGoccia);
        for Key in SrcObj.GetEnumerablePropertyNames do
          TgtObj.SetProperty(Key, SrcObj.GetProperty(Key));
        for SymPair in SrcObj.GetEnumerableSymbolProperties do
          TgtObj.AssignSymbolProperty(SymPair.Key, SymPair.Value);
      end
      else if SrcGoccia is TGocciaArrayValue then
      begin
        for I := 0 to TGocciaArrayValue(SrcGoccia).Elements.Count - 1 do
          if TGocciaArrayValue(SrcGoccia).Elements[I] <> nil then
            TgtObj.SetProperty(IntToStr(I), TGocciaArrayValue(SrcGoccia).Elements[I]);
      end
      else if SrcGoccia is TGocciaStringLiteralValue then
      begin
        for I := 1 to Length(TGocciaStringLiteralValue(SrcGoccia).Value) do
          TgtObj.SetProperty(IntToStr(I - 1),
            TGocciaStringLiteralValue.Create(TGocciaStringLiteralValue(SrcGoccia).Value[I]));
      end;
    end;
  end
  else if SouffleIsStringValue(ASource) then
  begin
    for I := 1 to Length(SouffleGetString(ASource)) do
      TgtObj.SetProperty(IntToStr(I - 1),
        TGocciaStringLiteralValue.Create(SouffleGetString(ASource)[I]));
  end;
end;

{ Rest Operations }

function TGocciaRuntimeOperations.ObjectRest(
  const ASource, AExclusionKeys: TSouffleValue): TSouffleValue;
var
  GocciaVal: TGocciaValue;
  SrcObj: TGocciaObjectValue;
  RestObj: TGocciaObjectValue;
  SrcRec: TSouffleRecord;
  ExclArr: TSouffleArray;
  I, J: Integer;
  Key: string;
  Excluded: Boolean;
  PropNames: TArray<string>;
begin
  if not SouffleIsReference(ASource) or not Assigned(ASource.AsReference) then
    Exit(SouffleNil);

  if SouffleIsReference(AExclusionKeys) and
     (AExclusionKeys.AsReference is TSouffleArray) then
    ExclArr := TSouffleArray(AExclusionKeys.AsReference)
  else
    ExclArr := nil;

  if ASource.AsReference is TSouffleRecord then
  begin
    SrcRec := TSouffleRecord(ASource.AsReference);
    RestObj := TGocciaObjectValue.Create;
    for I := 0 to SrcRec.Count - 1 do
    begin
      Key := SrcRec.GetOrderedKey(I);
      Excluded := False;
      if Assigned(ExclArr) then
        for J := 0 to ExclArr.Count - 1 do
          if SouffleValueToString(ExclArr.Get(J)) = Key then
          begin
            Excluded := True;
            Break;
          end;
      if not Excluded then
        RestObj.SetProperty(Key, UnwrapToGocciaValue(SrcRec.GetOrderedValue(I)));
    end;
    Exit(ToSouffleValue(RestObj));
  end;

  if ASource.AsReference is TGocciaWrappedValue then
  begin
    GocciaVal := TGocciaWrappedValue(ASource.AsReference).Value;
    if GocciaVal is TGocciaObjectValue then
    begin
      SrcObj := TGocciaObjectValue(GocciaVal);
      RestObj := TGocciaObjectValue.Create;
      PropNames := SrcObj.GetOwnPropertyNames;
      for I := 0 to High(PropNames) do
      begin
        Key := PropNames[I];
        Excluded := False;
        if Assigned(ExclArr) then
          for J := 0 to ExclArr.Count - 1 do
            if SouffleValueToString(ExclArr.Get(J)) = Key then
            begin
              Excluded := True;
              Break;
            end;
        if not Excluded then
          RestObj.SetProperty(Key, SrcObj.GetProperty(Key));
      end;
      Exit(ToSouffleValue(RestObj));
    end;
  end;

  Result := SouffleNil;
end;

{ Modules }

function TGocciaRuntimeOperations.ImportModule(const APath: string): TSouffleValue;
var
  EngineObj: TGocciaEngine;
  Module: TGocciaModule;
  Pair: TGocciaValueMap.TKeyValuePair;
  Rec: TSouffleRecord;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.ImportModuleCount);
  {$ENDIF}
  if not Assigned(FEngine) then
    Exit(SouffleNil);

  EngineObj := TGocciaEngine(FEngine);
  Module := EngineObj.Interpreter.LoadModule(APath, FSourcePath);
  if not Assigned(Module) then
    Exit(SouffleNil);

  Rec := TSouffleRecord.Create(Module.ExportsTable.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);

  for Pair in Module.ExportsTable do
    Rec.Put(Pair.Key, ToSouffleValue(Pair.Value));

  Result := SouffleReference(Rec);
end;

procedure TGocciaRuntimeOperations.ExportBinding(const AValue: TSouffleValue;
  const AName: string);
begin
  FExports.AddOrSetValue(AName, AValue);
end;

{ Async }

function TGocciaRuntimeOperations.AwaitValue(const AValue: TSouffleValue): TSouffleValue;
var
  GocciaVal, Resolved: TGocciaValue;
begin
  {$IFDEF BRIDGE_METRICS}
  Inc(GBridgeMetrics.AwaitValueCount);
  {$ENDIF}
  case AValue.Kind of
    svkNil, svkBoolean, svkInteger, svkFloat, svkString:
      Exit(AValue);
  end;
  if SouffleIsReference(AValue) and Assigned(AValue.AsReference) then
  begin
    if AValue.AsReference is TSouffleHeapString then
      Exit(AValue);
    if AValue.AsReference is TGocciaWrappedValue then
    begin
      GocciaVal := TGocciaWrappedValue(AValue.AsReference).Value;
      if GocciaVal is TGocciaPromiseValue then
      begin
        if TGocciaPromiseValue(GocciaVal).State = gpsFulfilled then
          Exit(ToSouffleValue(TGocciaPromiseValue(GocciaVal).PromiseResult));
        if TGocciaPromiseValue(GocciaVal).State = gpsRejected then
        begin
          RethrowAsVM(TGocciaThrowValue.Create(
            TGocciaPromiseValue(GocciaVal).PromiseResult));
          Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
        end;
      end
      else if not (GocciaVal is TGocciaObjectValue) then
        Exit(ToSouffleValue(GocciaVal));
    end;
  end;
  try
    GocciaVal := UnwrapToGocciaValue(AValue);
    Resolved := Goccia.Evaluator.AwaitValue(GocciaVal);
    Result := ToSouffleValue(Resolved);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.WrapInPromise(const AValue: TSouffleValue;
  const AIsRejected: Boolean): TSouffleValue;
var
  Promise: TGocciaPromiseValue;
  GocciaVal: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;
  GocciaVal := UnwrapToGocciaValue(AValue);
  if AIsRejected then
    Promise.Reject(GocciaVal)
  else
    Promise.Resolve(GocciaVal);
  if Assigned(TGocciaMicrotaskQueue.Instance) and
     TGocciaMicrotaskQueue.Instance.HasPending then
    TGocciaMicrotaskQueue.Instance.DrainQueue;
  Result := WrapGocciaValue(Promise);
end;

{ Globals }

function TGocciaRuntimeOperations.GetGlobal(const AName: string): TSouffleValue;
var
  Value: TSouffleValue;
begin
  if FGlobals.TryGetValue(AName, Value) then
    Result := Value
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

procedure TGocciaRuntimeOperations.SetGlobal(const AName: string;
  const AValue: TSouffleValue);
begin
  if FConstGlobals.ContainsKey(AName) then
  begin
    try
      ThrowTypeError('Assignment to constant variable ''' + AName + '''');
    except
      on E: TGocciaThrowValue do
        RethrowAsVM(E);
    end;
    Exit;
  end;
  if not FGlobals.ContainsKey(AName) then
  begin
    try
      ThrowReferenceError('Undefined variable: ' + AName);
    except
      on E: TGocciaThrowValue do
        RethrowAsVM(E);
    end;
    Exit;
  end;
  FGlobals.AddOrSetValue(AName, AValue);
end;

function TGocciaRuntimeOperations.HasGlobal(const AName: string): Boolean;
begin
  Result := FGlobals.ContainsKey(AName);
end;

procedure TGocciaRuntimeOperations.DefineGetter(const AObject: TSouffleValue;
  const AKey: string; const AGetter: TSouffleValue);
var
  Bp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  Obj: TGocciaObjectValue;
  GocciaGetter: TGocciaValue;
  Existing: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
  begin
    if AObject.AsReference is TSouffleBlueprint then
    begin
      Bp := TSouffleBlueprint(AObject.AsReference);
      Bp.Getters.Put(AKey, AGetter);
      Exit;
    end;
    if AObject.AsReference is TSouffleRecord then
    begin
      Rec := TSouffleRecord(AObject.AsReference);
      Rec.Getters.Put(AKey, AGetter);
      Exit;
    end;
  end;

  Obj := UnwrapToGocciaValue(AObject) as TGocciaObjectValue;
  GocciaGetter := UnwrapToGocciaValue(AGetter);

  ExistingSetter := nil;
  Existing := Obj.GetOwnPropertyDescriptor(AKey);
  if (Existing <> nil) and (Existing is TGocciaPropertyDescriptorAccessor) then
    ExistingSetter := TGocciaPropertyDescriptorAccessor(Existing).Setter;

  Obj.DefineProperty(AKey, TGocciaPropertyDescriptorAccessor.Create(
    GocciaGetter, ExistingSetter, [pfEnumerable, pfConfigurable]));
end;

procedure TGocciaRuntimeOperations.DefineSetter(const AObject: TSouffleValue;
  const AKey: string; const ASetter: TSouffleValue);
var
  Bp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  Obj: TGocciaObjectValue;
  GocciaSetter: TGocciaValue;
  Existing: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
  begin
    if AObject.AsReference is TSouffleBlueprint then
    begin
      Bp := TSouffleBlueprint(AObject.AsReference);
      Bp.Setters.Put(AKey, ASetter);
      Exit;
    end;
    if AObject.AsReference is TSouffleRecord then
    begin
      Rec := TSouffleRecord(AObject.AsReference);
      Rec.Setters.Put(AKey, ASetter);
      Exit;
    end;
  end;

  Obj := UnwrapToGocciaValue(AObject) as TGocciaObjectValue;
  GocciaSetter := UnwrapToGocciaValue(ASetter);

  ExistingGetter := nil;
  Existing := Obj.GetOwnPropertyDescriptor(AKey);
  if (Existing <> nil) and (Existing is TGocciaPropertyDescriptorAccessor) then
    ExistingGetter := TGocciaPropertyDescriptorAccessor(Existing).Getter;

  Obj.DefineProperty(AKey, TGocciaPropertyDescriptorAccessor.Create(
    ExistingGetter, GocciaSetter, [pfEnumerable, pfConfigurable]));
end;

procedure TGocciaRuntimeOperations.DefineStaticGetter(
  const AObject: TSouffleValue; const AKey: string;
  const AGetter: TSouffleValue);
var
  Bp: TSouffleBlueprint;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
     (AObject.AsReference is TSouffleBlueprint) then
  begin
    Bp := TSouffleBlueprint(AObject.AsReference);
    Bp.StaticGetters.Put(AKey, AGetter);
  end;
end;

procedure TGocciaRuntimeOperations.DefineStaticSetter(
  const AObject: TSouffleValue; const AKey: string;
  const ASetter: TSouffleValue);
var
  Bp: TSouffleBlueprint;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
     (AObject.AsReference is TSouffleBlueprint) then
  begin
    Bp := TSouffleBlueprint(AObject.AsReference);
    Bp.StaticSetters.Put(AKey, ASetter);
  end;
end;

procedure TGocciaRuntimeOperations.DefineComputedGetter(
  const AObject, AKey, AGetter: TSouffleValue);
begin
  DefineGetter(AObject, ComputedKeyAsString(AKey), AGetter);
end;

procedure TGocciaRuntimeOperations.DefineComputedSetter(
  const AObject, AKey, ASetter: TSouffleValue);
begin
  DefineSetter(AObject, ComputedKeyAsString(AKey), ASetter);
end;

procedure TGocciaRuntimeOperations.DefineComputedStaticGetter(
  const AObject, AKey, AGetter: TSouffleValue);
begin
  DefineStaticGetter(AObject, ComputedKeyAsString(AKey), AGetter);
end;

procedure TGocciaRuntimeOperations.DefineComputedStaticSetter(
  const AObject, AKey, ASetter: TSouffleValue);
begin
  DefineStaticSetter(AObject, ComputedKeyAsString(AKey), ASetter);
end;

function TGocciaRuntimeOperations.GetSymbolOnNativeObject(
  const AObject: TSouffleValue;
  const ASymKey: TGocciaSymbolValue;
  out AFound: Boolean): TSouffleValue;
var
  SymPropKey: string;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  GetterVal, DirectVal: TSouffleValue;
begin
  AFound := False;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AObject) or not Assigned(AObject.AsReference) then
    Exit;

  SymPropKey := '@@sym:' + IntToStr(ASymKey.Id);

  if AObject.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AObject.AsReference);
    if Rec.HasGetters and Rec.Getters.Get(SymPropKey, GetterVal) then
    begin
      AFound := True;
      if TryInvokeGetter(GetterVal, AObject, Result) then
        Exit;
      Exit;
    end;
    if Rec.Get(SymPropKey, DirectVal) then
    begin
      AFound := True;
      Result := DirectVal;
      Exit;
    end;
    if Assigned(Rec.Blueprint) then
    begin
      Bp := Rec.Blueprint;
      while Assigned(Bp) do
      begin
        if Bp.HasGetters and Bp.Getters.Get(SymPropKey, GetterVal) then
        begin
          AFound := True;
          TryInvokeGetter(GetterVal, AObject, Result);
          Exit;
        end;
        Bp := Bp.SuperBlueprint;
      end;
    end;
  end
  else if AObject.AsReference is TSouffleBlueprint then
  begin
    Bp := TSouffleBlueprint(AObject.AsReference);
    if Bp.HasStaticGetters and Bp.StaticGetters.Get(SymPropKey, GetterVal) then
    begin
      AFound := True;
      TryInvokeGetter(GetterVal, AObject, Result);
      Exit;
    end;
    Bp := Bp.SuperBlueprint;
    while Assigned(Bp) do
    begin
      if Bp.HasStaticGetters and Bp.StaticGetters.Get(SymPropKey, GetterVal) then
      begin
        AFound := True;
        TryInvokeGetter(GetterVal, AObject, Result);
        Exit;
      end;
      Bp := Bp.SuperBlueprint;
    end;
  end;
end;

function TGocciaRuntimeOperations.SetSymbolOnNativeObject(
  const AObject: TSouffleValue;
  const ASymKey: TGocciaSymbolValue;
  const AValue: TSouffleValue): Boolean;
var
  SymPropKey: string;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  SetterVal, GetterVal: TSouffleValue;
begin
  Result := False;
  if not SouffleIsReference(AObject) or not Assigned(AObject.AsReference) then
    Exit;

  SymPropKey := '@@sym:' + IntToStr(ASymKey.Id);

  if AObject.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AObject.AsReference);
    if Rec.HasSetters and Rec.Setters.Get(SymPropKey, SetterVal) then
    begin
      if SouffleIsReference(SetterVal) and
         (SetterVal.AsReference is TSouffleClosure) then
      begin
        FVM.ExecuteFunction(
          TSouffleClosure(SetterVal.AsReference), [AObject, AValue]);
        Exit(True);
      end;
    end;
    if Rec.HasGetters and Rec.Getters.Get(SymPropKey, GetterVal) then
    begin
      ThrowTypeError('Cannot set property Symbol(' +
        ASymKey.Description + ') which has only a getter');
      Exit(True);
    end;
    if Assigned(Rec.Blueprint) then
    begin
      Bp := Rec.Blueprint;
      while Assigned(Bp) do
      begin
        if Bp.HasSetters and Bp.Setters.Get(SymPropKey, SetterVal) then
        begin
          if SouffleIsReference(SetterVal) and
             (SetterVal.AsReference is TSouffleClosure) then
          begin
            FVM.ExecuteFunction(
              TSouffleClosure(SetterVal.AsReference), [AObject, AValue]);
            Exit(True);
          end;
        end;
        if Bp.HasGetters and Bp.Getters.Get(SymPropKey, GetterVal) then
        begin
          ThrowTypeError('Cannot set property Symbol(' +
            ASymKey.Description + ') which has only a getter');
          Exit(True);
        end;
        Bp := Bp.SuperBlueprint;
      end;
    end;
  end
  else if AObject.AsReference is TSouffleBlueprint then
  begin
    Bp := TSouffleBlueprint(AObject.AsReference);
    while Assigned(Bp) do
    begin
      if Bp.HasStaticSetters and Bp.StaticSetters.Get(SymPropKey, SetterVal) then
      begin
        if SouffleIsReference(SetterVal) and
           (SetterVal.AsReference is TSouffleClosure) then
        begin
          FVM.ExecuteFunction(
            TSouffleClosure(SetterVal.AsReference), [AObject, AValue]);
          Exit(True);
        end;
      end;
      if Bp.HasStaticGetters and Bp.StaticGetters.Get(SymPropKey, GetterVal) then
      begin
        ThrowTypeError('Cannot set property Symbol(' +
          ASymKey.Description + ') which has only a getter');
        Exit(True);
      end;
      Bp := Bp.SuperBlueprint;
    end;
  end;
end;

{ Property violations }

procedure TGocciaRuntimeOperations.PropertyWriteViolation(
  const AObject: TSouffleValue; const AKey: string);
begin
  try
    ThrowTypeError(
      'Cannot assign to read only property ''' + AKey + '''');
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.ThrowTypeErrorMessage(
  const AMessage: string);
begin
  try
    ThrowTypeError(AMessage);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function LocalTypeDisplayName(const AType: TSouffleLocalType): string;
begin
  case AType of
    sltInteger:   Result := NUMBER_TYPE_NAME;
    sltFloat:     Result := NUMBER_TYPE_NAME;
    sltBoolean:   Result := BOOLEAN_TYPE_NAME;
    sltString:    Result := STRING_TYPE_NAME;
    sltReference: Result := OBJECT_TYPE_NAME;
  else
    Result := 'unknown';
  end;
end;

function SouffleValueTypeName(const AValue: TSouffleValue): string;
begin
  if SouffleIsNil(AValue) then
  begin
    if AValue.Flags = GOCCIA_NIL_NULL then
      Result := NULL_TYPE_NAME
    else
      Result := UNDEFINED_TYPE_NAME;
  end
  else if SouffleIsBoolean(AValue) then
    Result := BOOLEAN_TYPE_NAME
  else if SouffleIsInteger(AValue) or SouffleIsFloat(AValue) then
    Result := NUMBER_TYPE_NAME
  else if SouffleIsStringValue(AValue) then
    Result := STRING_TYPE_NAME
  else if SouffleIsReference(AValue) then
    Result := OBJECT_TYPE_NAME
  else
    Result := 'unknown';
end;

procedure TGocciaRuntimeOperations.CheckLocalType(
  const AValue: TSouffleValue; const AExpectedType: TSouffleLocalType);
begin
  ThrowTypeErrorMessage('Type ''' + SouffleValueTypeName(AValue) +
    ''' is not assignable to type ''' + LocalTypeDisplayName(AExpectedType) + '''');
end;

procedure TGocciaRuntimeOperations.PropertyDeleteViolation(
  const AObject: TSouffleValue; const AKey: string);
begin
  try
    ThrowTypeError(
      'Cannot delete property ''' + AKey + '''');
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

{ Proxy resolution }

function TGocciaRuntimeOperations.ResolveProxyGet(
  const ATarget: TSouffleHeapObject; const AName: string): TGocciaValue;
var
  Arr: TSouffleArray;
  Rec: TSouffleRecord;
  Index: Integer;
  Val: TSouffleValue;
  TempArr: TGocciaArrayValue;
begin
  if ATarget is TSouffleArray then
  begin
    Arr := TSouffleArray(ATarget);
    if AName = PROP_LENGTH then
      Exit(TGocciaNumberLiteralValue.Create(Arr.Count * 1.0));
    if TryStrToInt(AName, Index) and (Index >= 0) and (Index < Arr.Count) then
      Exit(UnwrapToGocciaValue(Arr.Get(Index)));
    TempArr := ConvertSouffleArrayToGocciaArray(Self, Arr);
    Result := TempArr.GetProperty(AName);
  end
  else if ATarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(ATarget);
    if Rec.HasGetters and Rec.Getters.Get(AName, Val) then
      Exit(UnwrapToGocciaValue(
        GetProperty(SouffleReference(ATarget), AName)));
    if Rec.Get(AName, Val) then
      Exit(UnwrapToGocciaValue(Val));
    if Assigned(Rec.Blueprint) and Rec.Blueprint.Methods.Get(AName, Val) then
      Exit(UnwrapToGocciaValue(Val));
    while Assigned(Rec.Delegate) do
    begin
      if Rec.Delegate is TSouffleRecord then
      begin
        Rec := TSouffleRecord(Rec.Delegate);
        if Rec.HasGetters and Rec.Getters.Get(AName, Val) then
          Exit(UnwrapToGocciaValue(
            GetProperty(SouffleReference(ATarget), AName)));
        if Rec.Get(AName, Val) then
          Exit(UnwrapToGocciaValue(Val));
        if Assigned(Rec.Blueprint) and Rec.Blueprint.Methods.Get(AName, Val) then
          Exit(UnwrapToGocciaValue(Val));
      end
      else
        Break;
    end;
    Result := nil;
  end
  else
    Result := nil;
end;

procedure TGocciaRuntimeOperations.ResolveProxySet(
  const ATarget: TSouffleHeapObject; const AName: string;
  const AValue: TGocciaValue);
var
  Arr: TSouffleArray;
  Rec: TSouffleRecord;
  Index: Integer;
begin
  if ATarget is TSouffleArray then
  begin
    Arr := TSouffleArray(ATarget);
    if TryStrToInt(AName, Index) and (Index >= 0) then
      Arr.Put(Index, ToSouffleValue(AValue));
  end
  else if ATarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(ATarget);
    Rec.Put(AName, ToSouffleValue(AValue));
  end;
end;

{ Native delegate callbacks }

procedure SyncSouffleArrayToCache(const AArr: TSouffleArray); forward;

function NativeArrayPush(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  I: Integer;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray) then
  begin
    Arr := TSouffleArray(AReceiver.AsReference);
    for I := 0 to AArgCount - 1 do
      Arr.Push(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^);
    SyncSouffleArrayToCache(Arr);
    Result := SouffleInteger(Arr.Count);
  end
  else
    Result := SouffleNil;
end;

function NativeArrayPop(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray) then
  begin
    Arr := TSouffleArray(AReceiver.AsReference);
    Result := Arr.Pop;
    if SouffleIsHole(Result) then
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
    SyncSouffleArrayToCache(Arr);
  end
  else
    Result := SouffleNil;
end;

function NativeArrayLength(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray) then
    Result := SouffleInteger(TSouffleArray(AReceiver.AsReference).Count)
  else
    Result := SouffleInteger(0);
end;

function JoinElementToString(const AElem: TSouffleValue;
  const ARuntime: TGocciaRuntimeOperations): string;
var
  StrResult: TSouffleValue;
begin
  if SouffleIsNil(AElem) then
    Exit('');
  if SouffleIsStringValue(AElem) then
    Exit(SouffleGetString(AElem));
  if SouffleIsReference(AElem) and Assigned(AElem.AsReference) then
  begin
    if AElem.AsReference is TSouffleArray then
      Exit(JoinArrayElements(TSouffleArray(AElem.AsReference), ',', ARuntime));
    if AElem.AsReference is TSouffleRecord then
    begin
      StrResult := ARuntime.CoerceValueToString(AElem);
      if SouffleIsStringValue(StrResult) then
        Exit(SouffleGetString(StrResult));
      Exit('[object Object]');
    end;
  end;
  Result := ARuntime.CoerceToString(AElem);
end;

function JoinArrayElements(const AArr: TSouffleArray;
  const ASep: string; const ARuntime: TGocciaRuntimeOperations): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AArr.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ASep;
    Result := Result + JoinElementToString(AArr.Get(I), ARuntime);
  end;
end;

var
  GNativeArrayJoinRuntime: TGocciaRuntimeOperations;
  GPromiseAllFn: TGocciaValue;
  GPromiseAllSettledFn: TGocciaValue;
  GPromiseRaceFn: TGocciaValue;
  GPromiseAnyFn: TGocciaValue;
  GPromiseWithResolversFn: TGocciaValue;
  GPromiseTryFn: TGocciaValue;

{ Bridged test framework globals }
var
  GBridgedDescribe: TGocciaNativeFunctionValue;
  GBridgedDescribeSkip: TGocciaNativeFunctionValue;
  GBridgedDescribeSkipIf: TGocciaNativeFunctionValue;
  GBridgedDescribeRunIf: TGocciaNativeFunctionValue;
  GBridgedTest: TGocciaNativeFunctionValue;
  GBridgedTestSkip: TGocciaNativeFunctionValue;
  GBridgedTestSkipIf: TGocciaNativeFunctionValue;
  GBridgedTestRunIf: TGocciaNativeFunctionValue;
  GBridgedExpect: TGocciaNativeFunctionValue;
  GBridgedIt: TGocciaNativeFunctionValue;
  GBridgedBeforeEach: TGocciaNativeFunctionValue;
  GBridgedAfterEach: TGocciaNativeFunctionValue;
  GBridgedRunTests: TGocciaNativeFunctionValue;

function InvokeBridgedFn(const AGocciaFn: TGocciaNativeFunctionValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Args: TGocciaArgumentsCollection;
  GocciaResult: TGocciaValue;
  I: Integer;
begin
  Args := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to AArgCount - 1 do
      Args.Add(GNativeArrayJoinRuntime.UnwrapToGocciaValue(
        PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
    GocciaResult := AGocciaFn.Call(Args,
      TGocciaUndefinedLiteralValue.UndefinedValue);
    if Assigned(GocciaResult) then
      Result := GNativeArrayJoinRuntime.ToSouffleValue(GocciaResult)
    else
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  finally
    Args.Free;
  end;
end;

function NativeBridgedDescribe(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedDescribe, AArgs, AArgCount);
end;

function NativeBridgedTest(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedTest, AArgs, AArgCount);
end;

function NativeBridgedIt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedIt, AArgs, AArgCount);
end;

function NativeBridgedExpect(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedExpect, AArgs, AArgCount);
end;

function NativeBridgedBeforeEach(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedBeforeEach, AArgs, AArgCount);
end;

function NativeBridgedAfterEach(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedAfterEach, AArgs, AArgCount);
end;

function NativeBridgedRunTests(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedRunTests, AArgs, AArgCount);
end;

function NativeBridgedDescribeSkip(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedDescribeSkip, AArgs, AArgCount);
end;

function NativeBridgedDescribeSkipIf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedDescribeSkipIf, AArgs, AArgCount);
end;

function NativeBridgedDescribeRunIf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedDescribeRunIf, AArgs, AArgCount);
end;

function NativeBridgedTestSkip(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedTestSkip, AArgs, AArgCount);
end;

function NativeBridgedTestSkipIf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedTestSkipIf, AArgs, AArgCount);
end;

function NativeBridgedTestRunIf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeBridgedFn(GBridgedTestRunIf, AArgs, AArgCount);
end;

procedure SyncSouffleArrayToCache(const AArr: TSouffleArray);
var
  CachedBridge: TObject;
  GArr: TGocciaArrayValue;
  J: Integer;
begin
  if not Assigned(GNativeArrayJoinRuntime) then Exit;
  if not GNativeArrayJoinRuntime.FArrayBridgeCache.TryGetValue(
      AArr, CachedBridge) then
    Exit;
  GArr := TGocciaArrayValue(CachedBridge);
  GArr.Elements.Clear;
  for J := 0 to AArr.Count - 1 do
    GArr.Elements.Add(
      GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArr.Get(J)));
end;

function NativeArrayJoin(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Sep: string;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray) then
  begin
    Arr := TSouffleArray(AReceiver.AsReference);
    Sep := ',';
    if (AArgCount > 0) and not SouffleIsNil(AArgs^) then
      Sep := GNativeArrayJoinRuntime.CoerceToString(AArgs^);
    Result := SouffleString(
      JoinArrayElements(Arr, Sep, GNativeArrayJoinRuntime));
  end
  else
    Result := SouffleString('');
end;

function NativeArrayToString(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := NativeArrayJoin(AReceiver, nil, 0);
end;

function NativeArrayIndexOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  I, StartFrom, Len: Integer;
  SearchVal, Arg2: TSouffleValue;
begin
  Result := SouffleInteger(-1);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if AArgCount < 1 then Exit;

  Arr := TSouffleArray(AReceiver.AsReference);
  Len := Arr.Count;
  SearchVal := AArgs^;
  StartFrom := 0;
  if AArgCount > 1 then
  begin
    Arg2 := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    if Arg2.Kind = svkInteger then
      StartFrom := Arg2.AsInteger
    else if Arg2.Kind = svkFloat then
      StartFrom := Trunc(Arg2.AsFloat);
    if StartFrom < 0 then
    begin
      StartFrom := Len + StartFrom;
      if StartFrom < 0 then StartFrom := 0;
    end;
  end;
  if StartFrom >= Len then Exit;

  for I := StartFrom to Len - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    if SouffleValuesEqual(Arr.Get(I), SearchVal) then
      Exit(SouffleInteger(I));
  end;
end;

function SouffleSameValueZero(const A, B: TSouffleValue): Boolean;
var
  F: Double;
begin
  if (A.Kind = svkFloat) and (B.Kind = svkFloat) then
  begin
    if IsNaN(A.AsFloat) and IsNaN(B.AsFloat) then
      Exit(True);
    Exit(A.AsFloat = B.AsFloat);
  end;
  if (A.Kind = svkFloat) and (B.Kind = svkInteger) then
  begin
    F := B.AsInteger;
    Exit(A.AsFloat = F);
  end;
  if (A.Kind = svkInteger) and (B.Kind = svkFloat) then
  begin
    F := A.AsInteger;
    Exit(F = B.AsFloat);
  end;
  Result := SouffleValuesEqual(A, B);
end;

function NativeArrayIncludes(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  I, StartFrom, Len: Integer;
  SearchVal, Arg2: TSouffleValue;
begin
  Result := SouffleBoolean(False);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if AArgCount < 1 then Exit;

  Arr := TSouffleArray(AReceiver.AsReference);
  Len := Arr.Count;
  SearchVal := AArgs^;
  StartFrom := 0;
  if AArgCount > 1 then
  begin
    Arg2 := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    if Arg2.Kind = svkInteger then
      StartFrom := Arg2.AsInteger
    else if Arg2.Kind = svkFloat then
      StartFrom := Trunc(Arg2.AsFloat);
    if StartFrom < 0 then
    begin
      StartFrom := Len + StartFrom;
      if StartFrom < 0 then StartFrom := 0;
    end;
  end;
  if StartFrom >= Len then Exit;

  for I := StartFrom to Len - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then
    begin
      if SouffleIsNil(SearchVal) and (SearchVal.Flags = GOCCIA_NIL_UNDEFINED) then
        Exit(SouffleBoolean(True));
      Continue;
    end;
    if SouffleSameValueZero(Arr.Get(I), SearchVal) then
      Exit(SouffleBoolean(True));
  end;
end;

function NativeArraySlice(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr, NewArr: TSouffleArray;
  Start, Stop, I, Len: Integer;
  Arg: TSouffleValue;
begin
  Result := SouffleNil;
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;

  Arr := TSouffleArray(AReceiver.AsReference);
  Len := Arr.Count;
  Start := 0;
  Stop := Len;

  if AArgCount > 0 then
  begin
    Arg := AArgs^;
    if Arg.Kind = svkInteger then Start := Arg.AsInteger
    else if Arg.Kind = svkFloat then Start := Trunc(Arg.AsFloat);
    if Start < 0 then Start := Len + Start;
    if Start < 0 then Start := 0;
  end;
  if AArgCount > 1 then
  begin
    Arg := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    if Arg.Kind = svkInteger then Stop := Arg.AsInteger
    else if Arg.Kind = svkFloat then Stop := Trunc(Arg.AsFloat);
    if Stop < 0 then Stop := Len + Stop;
    if Stop > Len then Stop := Len;
  end;

  NewArr := TSouffleArray.Create(Stop - Start);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewArr);
  for I := Start to Stop - 1 do
    NewArr.Push(Arr.Get(I));
  Result := SouffleReference(NewArr);
end;

function NativeArrayReverse(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  I, Half: Integer;
  Tmp: TSouffleValue;
begin
  Result := AReceiver;
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Half := Arr.Count div 2;
  for I := 0 to Half - 1 do
  begin
    Tmp := Arr.Get(I);
    Arr.Put(I, Arr.Get(Arr.Count - 1 - I));
    Arr.Put(Arr.Count - 1 - I, Tmp);
  end;
  SyncSouffleArrayToCache(Arr);
end;

function NativeArrayConcat(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr, SrcArr, NewArr: TSouffleArray;
  I, J: Integer;
  Arg: TSouffleValue;
begin
  Result := SouffleNil;
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;

  Arr := TSouffleArray(AReceiver.AsReference);
  NewArr := TSouffleArray.Create(Arr.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewArr);

  for I := 0 to Arr.Count - 1 do
    NewArr.Push(Arr.Get(I));

  for J := 0 to AArgCount - 1 do
  begin
    Arg := PSouffleValue(PByte(AArgs) + J * SizeOf(TSouffleValue))^;
    if SouffleIsReference(Arg) and Assigned(Arg.AsReference) and
       (Arg.AsReference is TSouffleArray) then
    begin
      SrcArr := TSouffleArray(Arg.AsReference);
      for I := 0 to SrcArr.Count - 1 do
        NewArr.Push(SrcArr.Get(I));
    end
    else
      NewArr.Push(Arg);
  end;
  Result := SouffleReference(NewArr);
end;

function NativeArrayShift(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  I: Integer;
begin
  Result := SouffleNil;
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  if Arr.Count = 0 then Exit;
  Result := Arr.Get(0);
  for I := 1 to Arr.Count - 1 do
    Arr.Put(I - 1, Arr.Get(I));
  Arr.Pop;
  SyncSouffleArrayToCache(Arr);
end;

function NativeArrayUnshift(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  I, J, OldCount: Integer;
begin
  Result := SouffleInteger(0);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  if AArgCount < 1 then
    Exit(SouffleInteger(Arr.Count));
  OldCount := Arr.Count;
  for I := 0 to AArgCount - 1 do
    Arr.Push(SouffleNil);
  for I := OldCount - 1 downto 0 do
    Arr.Put(I + AArgCount, Arr.Get(I));
  for J := 0 to AArgCount - 1 do
    Arr.Put(J, PSouffleValue(PByte(AArgs) + J * SizeOf(TSouffleValue))^);
  SyncSouffleArrayToCache(Arr);
  Result := SouffleInteger(Arr.Count);
end;

function NativeArrayFill(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  FillVal: TSouffleValue;
  I, Start, Stop, Len: Integer;
  Arg: TSouffleValue;
begin
  Result := AReceiver;
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Len := Arr.Count;
  if AArgCount < 1 then
    FillVal := SouffleNil
  else
    FillVal := AArgs^;
  Start := 0;
  Stop := Len;
  if AArgCount > 1 then
  begin
    Arg := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    if Arg.Kind = svkInteger then Start := Arg.AsInteger
    else if Arg.Kind = svkFloat then Start := Trunc(Arg.AsFloat);
    if Start < 0 then Start := Len + Start;
    if Start < 0 then Start := 0;
  end;
  if AArgCount > 2 then
  begin
    Arg := PSouffleValue(PByte(AArgs) + 2 * SizeOf(TSouffleValue))^;
    if Arg.Kind = svkInteger then Stop := Arg.AsInteger
    else if Arg.Kind = svkFloat then Stop := Trunc(Arg.AsFloat);
    if Stop < 0 then Stop := Len + Stop;
    if Stop > Len then Stop := Len;
  end;
  for I := Start to Stop - 1 do
    Arr.Put(I, FillVal);
  SyncSouffleArrayToCache(Arr);
end;

function NativeArrayAt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Idx: Integer;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if AArgCount < 1 then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  if AArgs^.Kind = svkInteger then
    Idx := AArgs^.AsInteger
  else if AArgs^.Kind = svkFloat then
    Idx := Trunc(AArgs^.AsFloat)
  else
    Exit;
  if Idx < 0 then Idx := Arr.Count + Idx;
  if (Idx >= 0) and (Idx < Arr.Count) then
    Result := Arr.Get(Idx);
end;

function NativeArrayForEach(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  for I := 0 to Arr.Count - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Arr.Get(I), SouffleInteger(I), AReceiver]);
  end;
end;

function NativeArrayMap(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr, NewArr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  NewArr := TSouffleArray.Create(Arr.Count);
  NewArr.Delegate := GNativeArrayJoinRuntime.VM.ArrayDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewArr);
  for I := 0 to Arr.Count - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then
    begin
      NewArr.Push(SouffleNilWithFlags(GOCCIA_NIL_HOLE));
      Continue;
    end;
    NewArr.Push(GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Arr.Get(I), SouffleInteger(I), AReceiver]));
  end;
  Result := SouffleReference(NewArr);
end;

function NativeArrayFilter(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr, NewArr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
  Elem, TestResult: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  NewArr := TSouffleArray.Create(Arr.Count);
  NewArr.Delegate := GNativeArrayJoinRuntime.VM.ArrayDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewArr);
  for I := 0 to Arr.Count - 1 do
  begin
    Elem := Arr.Get(I);
    if SouffleIsHole(Elem) then Continue;
    TestResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Elem, SouffleInteger(I), AReceiver]);
    if SouffleIsTrue(TestResult) then
      NewArr.Push(Elem);
  end;
  Result := SouffleReference(NewArr);
end;

function NativeArrayFind(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
  Elem, TestResult: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  for I := 0 to Arr.Count - 1 do
  begin
    Elem := Arr.Get(I);
    if SouffleIsHole(Elem) then Continue;
    TestResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Elem, SouffleInteger(I), AReceiver]);
    if SouffleIsTrue(TestResult) then
      Exit(Elem);
  end;
end;

function NativeArrayFindIndex(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
  TestResult: TSouffleValue;
begin
  Result := SouffleInteger(-1);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  for I := 0 to Arr.Count - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    TestResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Arr.Get(I), SouffleInteger(I), AReceiver]);
    if SouffleIsTrue(TestResult) then
      Exit(SouffleInteger(I));
  end;
end;

function NativeArrayEvery(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
  TestResult: TSouffleValue;
begin
  Result := SouffleBoolean(True);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  for I := 0 to Arr.Count - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    TestResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Arr.Get(I), SouffleInteger(I), AReceiver]);
    if not SouffleIsTrue(TestResult) then
      Exit(SouffleBoolean(False));
  end;
end;

function NativeArraySome(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
  TestResult: TSouffleValue;
begin
  Result := SouffleBoolean(False);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  for I := 0 to Arr.Count - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    TestResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Arr.Get(I), SouffleInteger(I), AReceiver]);
    if SouffleIsTrue(TestResult) then
      Exit(SouffleBoolean(True));
  end;
end;

function NativeArrayReduce(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I, StartIdx: Integer;
  Accumulator: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  if AArgCount > 1 then
  begin
    Accumulator := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    StartIdx := 0;
  end
  else
  begin
    if Arr.Count = 0 then
    begin
      try
        ThrowTypeError('Reduce of empty array with no initial value');
      except
        on E: TGocciaThrowValue do
          GNativeArrayJoinRuntime.RethrowAsVM(E);
      end;
      Exit;
    end;
    StartIdx := 0;
    while (StartIdx < Arr.Count) and SouffleIsHole(Arr.Get(StartIdx)) do
      Inc(StartIdx);
    if StartIdx >= Arr.Count then
    begin
      try
        ThrowTypeError('Reduce of empty array with no initial value');
      except
        on E: TGocciaThrowValue do
          GNativeArrayJoinRuntime.RethrowAsVM(E);
      end;
      Exit;
    end;
    Accumulator := Arr.Get(StartIdx);
    Inc(StartIdx);
  end;
  for I := StartIdx to Arr.Count - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    Accumulator := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Accumulator, Arr.Get(I), SouffleInteger(I), AReceiver]);
  end;
  Result := Accumulator;
end;

function NativeArrayReduceRight(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I, StartIdx: Integer;
  Accumulator: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  if AArgCount > 1 then
  begin
    Accumulator := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    StartIdx := Arr.Count - 1;
  end
  else
  begin
    if Arr.Count = 0 then Exit;
    StartIdx := Arr.Count - 1;
    while (StartIdx >= 0) and SouffleIsHole(Arr.Get(StartIdx)) do
      Dec(StartIdx);
    if StartIdx < 0 then Exit;
    Accumulator := Arr.Get(StartIdx);
    Dec(StartIdx);
  end;
  for I := StartIdx downto 0 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    Accumulator := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Accumulator, Arr.Get(I), SouffleInteger(I), AReceiver]);
  end;
  Result := Accumulator;
end;

function NativeArraySort(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  HasComparator: Boolean;
  Comparator: TSouffleClosure;
  I, J, Len: Integer;
  Tmp, CmpResult: TSouffleValue;
  CmpVal: Double;
  StrA, StrB: string;
begin
  Result := AReceiver;
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Len := Arr.Count;
  if Len <= 1 then Exit;
  HasComparator := (AArgCount > 0) and SouffleIsReference(AArgs^) and
                   (AArgs^.AsReference is TSouffleClosure);
  if HasComparator then
    Comparator := TSouffleClosure(AArgs^.AsReference)
  else
    Comparator := nil;
  for I := 0 to Len - 2 do
    for J := 0 to Len - 2 - I do
    begin
      if HasComparator then
      begin
        CmpResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Comparator,
          [SouffleNil, Arr.Get(J), Arr.Get(J + 1)]);
        if CmpResult.Kind = svkInteger then
          CmpVal := CmpResult.AsInteger
        else if CmpResult.Kind = svkFloat then
          CmpVal := CmpResult.AsFloat
        else
          CmpVal := 0;
      end
      else
      begin
        StrA := GNativeArrayJoinRuntime.CoerceToString(Arr.Get(J));
        StrB := GNativeArrayJoinRuntime.CoerceToString(Arr.Get(J + 1));
        if StrA > StrB then CmpVal := 1
        else if StrA < StrB then CmpVal := -1
        else CmpVal := 0;
      end;
      if CmpVal > 0 then
      begin
        Tmp := Arr.Get(J);
        Arr.Put(J, Arr.Get(J + 1));
        Arr.Put(J + 1, Tmp);
      end;
    end;
  SyncSouffleArrayToCache(Arr);
end;

function NativeArrayFindLast(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
  Elem, TestResult: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  for I := Arr.Count - 1 downto 0 do
  begin
    Elem := Arr.Get(I);
    TestResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Elem, SouffleInteger(I), AReceiver]);
    if SouffleIsTrue(TestResult) then
      Exit(Elem);
  end;
end;

function NativeArrayFindLastIndex(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Callback: TSouffleClosure;
  I: Integer;
  TestResult: TSouffleValue;
begin
  Result := SouffleInteger(-1);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  for I := Arr.Count - 1 downto 0 do
  begin
    TestResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Arr.Get(I), SouffleInteger(I), AReceiver]);
    if SouffleIsTrue(TestResult) then
      Exit(SouffleInteger(I));
  end;
end;

function NativeArrayFlat(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;

  procedure FlattenInto(const ASrc: TSouffleArray; const ADest: TSouffleArray;
    const ADepth: Integer);
  var
    I: Integer;
    Elem: TSouffleValue;
    Inner: TSouffleArray;
  begin
    for I := 0 to ASrc.Count - 1 do
    begin
      Elem := ASrc.Get(I);
      if SouffleIsHole(Elem) then Continue;
      if (ADepth > 0) and SouffleIsReference(Elem) and
         Assigned(Elem.AsReference) and (Elem.AsReference is TSouffleArray) then
      begin
        Inner := TSouffleArray(Elem.AsReference);
        FlattenInto(Inner, ADest, ADepth - 1);
      end
      else
        ADest.Push(Elem);
    end;
  end;

var
  Arr, NewArr: TSouffleArray;
  Depth: Integer;
  Arg: TSouffleValue;
  DepthF: Double;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Depth := 1;
  if AArgCount > 0 then
  begin
    Arg := AArgs^;
    if Arg.Kind = svkInteger then
      Depth := Arg.AsInteger
    else if Arg.Kind = svkFloat then
    begin
      DepthF := Arg.AsFloat;
      if IsInfinite(DepthF) and (DepthF > 0) then
        Depth := MaxInt
      else if IsNaN(DepthF) or (DepthF < 0) then
        Depth := 0
      else
        Depth := Trunc(DepthF);
    end;
  end;
  NewArr := TSouffleArray.Create(Arr.Count);
  NewArr.Delegate := GNativeArrayJoinRuntime.VM.ArrayDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewArr);
  FlattenInto(Arr, NewArr, Depth);
  Result := SouffleReference(NewArr);
end;

function NativeArrayFlatMap(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr, NewArr: TSouffleArray;
  Callback: TSouffleClosure;
  I, J: Integer;
  Mapped: TSouffleValue;
  MappedArr: TSouffleArray;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not (AArgs^.AsReference is TSouffleClosure) then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Callback := TSouffleClosure(AArgs^.AsReference);
  NewArr := TSouffleArray.Create(Arr.Count);
  NewArr.Delegate := GNativeArrayJoinRuntime.VM.ArrayDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewArr);
  for I := 0 to Arr.Count - 1 do
  begin
    if SouffleIsHole(Arr.Get(I)) then Continue;
    Mapped := GNativeArrayJoinRuntime.VM.ExecuteFunction(Callback,
      [SouffleNil, Arr.Get(I), SouffleInteger(I), AReceiver]);
    if SouffleIsReference(Mapped) and Assigned(Mapped.AsReference) and
       (Mapped.AsReference is TSouffleArray) then
    begin
      MappedArr := TSouffleArray(Mapped.AsReference);
      for J := 0 to MappedArr.Count - 1 do
      begin
        if not SouffleIsHole(MappedArr.Get(J)) then
          NewArr.Push(MappedArr.Get(J));
      end;
    end
    else
      NewArr.Push(Mapped);
  end;
  Result := SouffleReference(NewArr);
end;

function NativeArraySplice(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr, Removed, TmpArr: TSouffleArray;
  Start, DeleteCount, Len, I, InsertCount: Integer;
  Arg: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray)) then Exit;
  if AArgCount < 1 then Exit;
  Arr := TSouffleArray(AReceiver.AsReference);
  Len := Arr.Count;

  Arg := AArgs^;
  if Arg.Kind = svkInteger then Start := Arg.AsInteger
  else if Arg.Kind = svkFloat then Start := Trunc(Arg.AsFloat)
  else Start := 0;
  if Start < 0 then Start := Len + Start;
  if Start < 0 then Start := 0;
  if Start > Len then Start := Len;

  if AArgCount > 1 then
  begin
    Arg := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    if Arg.Kind = svkInteger then DeleteCount := Arg.AsInteger
    else if Arg.Kind = svkFloat then DeleteCount := Trunc(Arg.AsFloat)
    else DeleteCount := 0;
    if DeleteCount < 0 then DeleteCount := 0;
    if DeleteCount > Len - Start then DeleteCount := Len - Start;
  end
  else
    DeleteCount := Len - Start;

  InsertCount := AArgCount - 2;
  if InsertCount < 0 then InsertCount := 0;

  Removed := TSouffleArray.Create(DeleteCount);
  Removed.Delegate := GNativeArrayJoinRuntime.VM.ArrayDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Removed);
  for I := 0 to DeleteCount - 1 do
    Removed.Push(Arr.Get(Start + I));

  TmpArr := TSouffleArray.Create(Len - DeleteCount + InsertCount);
  for I := 0 to Start - 1 do
    TmpArr.Push(Arr.Get(I));
  for I := 0 to InsertCount - 1 do
    TmpArr.Push(PSouffleValue(PByte(AArgs) + (I + 2) * SizeOf(TSouffleValue))^);
  for I := Start + DeleteCount to Len - 1 do
    TmpArr.Push(Arr.Get(I));

  Arr.Clear;
  for I := 0 to TmpArr.Count - 1 do
    Arr.Push(TmpArr.Get(I));
  SyncSouffleArrayToCache(Arr);

  Result := SouffleReference(Removed);
end;

function ConvertGocciaMapToSouffle(const AMap: TGocciaMapValue;
  const ARuntime: TGocciaRuntimeOperations;
  const ADelegate: TSouffleRecord): TGocciaSouffleMap;
var
  I: Integer;
  Entry: TGocciaMapEntry;
begin
  Result := TGocciaSouffleMap.Create(AMap.Entries.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Result);
  Result.Delegate := ADelegate;
  for I := 0 to AMap.Entries.Count - 1 do
  begin
    Entry := AMap.Entries[I];
    Result.SetEntry(ARuntime.ToSouffleValue(Entry.Key),
      ARuntime.ToSouffleValue(Entry.Value));
  end;
end;

function ConvertGocciaSetToSouffle(const ASet: TGocciaSetValue;
  const ARuntime: TGocciaRuntimeOperations;
  const ADelegate: TSouffleRecord): TGocciaSouffleSet;
var
  I: Integer;
begin
  Result := TGocciaSouffleSet.Create(ASet.Items.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Result);
  Result.Delegate := ADelegate;
  for I := 0 to ASet.Items.Count - 1 do
    Result.Add(ARuntime.ToSouffleValue(ASet.Items[I]));
end;

{ Native Map delegate methods }

function GetSouffleMap(const AReceiver: TSouffleValue): TGocciaSouffleMap; inline;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TGocciaSouffleMap) then
    Result := TGocciaSouffleMap(AReceiver.AsReference)
  else
    Result := nil;
end;

function GetSouffleSet(const AReceiver: TSouffleValue): TGocciaSouffleSet; inline;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TGocciaSouffleSet) then
    Result := TGocciaSouffleSet(AReceiver.AsReference)
  else
    Result := nil;
end;

function UnwrapMapFromReceiver(const AReceiver: TSouffleValue): TGocciaMapValue; inline;
begin
  Result := nil;
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AReceiver.AsReference).Value is TGocciaMapValue) then
    Result := TGocciaMapValue(TGocciaWrappedValue(AReceiver.AsReference).Value);
end;

function UnwrapSetFromReceiver(const AReceiver: TSouffleValue): TGocciaSetValue; inline;
begin
  Result := nil;
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AReceiver.AsReference).Value is TGocciaSetValue) then
    Result := TGocciaSetValue(TGocciaWrappedValue(AReceiver.AsReference).Value);
end;

function InvokeGocciaMethod(const AReceiver: TSouffleValue;
  const AMethodName: string; const AArgs: PSouffleValue;
  const AArgCount: Integer): TSouffleValue;
var
  GocciaObj: TGocciaValue;
  MethodFn, GocciaResult: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  I: Integer;
begin
  GocciaObj := TGocciaWrappedValue(AReceiver.AsReference).Value;
  MethodFn := GocciaObj.GetProperty(AMethodName);
  if not Assigned(MethodFn) or not (MethodFn is TGocciaFunctionBase) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Args := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to AArgCount - 1 do
      Args.Add(GNativeArrayJoinRuntime.UnwrapToGocciaValue(
        PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
    GocciaResult := TGocciaFunctionBase(MethodFn).Call(Args, GocciaObj);
    if Assigned(GocciaResult) then
      Result := GNativeArrayJoinRuntime.ToSouffleValue(GocciaResult)
    else
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  finally
    Args.Free;
  end;
end;

function NativeMapGet(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
  M: TGocciaMapValue;
  Key: TGocciaValue;
  Index: Integer;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    if AArgCount < 1 then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    Result := SM.GetEntry(AArgs^);
    Exit;
  end;
  M := UnwrapMapFromReceiver(AReceiver);
  if not Assigned(M) or (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Key := GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArgs^);
  Index := M.FindEntry(Key);
  if Index >= 0 then
    Result := GNativeArrayJoinRuntime.ToSouffleValue(M.Entries[Index].Value)
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeMapSet(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
  M: TGocciaMapValue;
  Key, Val: TGocciaValue;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    if AArgCount >= 2 then
      SM.SetEntry(AArgs^, PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^)
    else if AArgCount = 1 then
      SM.SetEntry(AArgs^, SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    Exit(AReceiver);
  end;
  M := UnwrapMapFromReceiver(AReceiver);
  if not Assigned(M) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if AArgCount >= 2 then
  begin
    Key := GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArgs^);
    Val := GNativeArrayJoinRuntime.UnwrapToGocciaValue(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);
    M.SetEntry(Key, Val);
  end
  else if AArgCount = 1 then
  begin
    Key := GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArgs^);
    M.SetEntry(Key, TGocciaUndefinedLiteralValue.UndefinedValue);
  end;
  Result := AReceiver;
end;

function NativeMapHas(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
  M: TGocciaMapValue;
  Key: TGocciaValue;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    if AArgCount < 1 then Exit(SouffleBoolean(False));
    Exit(SouffleBoolean(SM.HasKey(AArgs^)));
  end;
  M := UnwrapMapFromReceiver(AReceiver);
  if not Assigned(M) or (AArgCount < 1) then
    Exit(SouffleBoolean(False));
  Key := GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArgs^);
  Result := SouffleBoolean(M.FindEntry(Key) >= 0);
end;

function NativeMapDelete(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    if AArgCount < 1 then Exit(SouffleBoolean(False));
    Exit(SouffleBoolean(SM.DeleteEntry(AArgs^)));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'delete', AArgs, AArgCount);
end;

function NativeMapClear(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    SM.Clear;
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'clear', AArgs, AArgCount);
end;

function NativeMapForEach(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) and (AArgCount >= 1) and SouffleIsReference(AArgs^) then
  begin
    for I := 0 to SM.Count - 1 do
    begin
      CallArgs[0] := SM.GetValueAt(I);
      CallArgs[1] := SM.GetKeyAt(I);
      CallArgs[2] := AReceiver;
      GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, SouffleNil);
    end;
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'forEach', AArgs, AArgCount);
end;

function NativeMapKeys(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeGocciaMethod(AReceiver, 'keys', AArgs, AArgCount);
end;

function NativeMapValues(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeGocciaMethod(AReceiver, 'values', AArgs, AArgCount);
end;

function NativeMapEntries(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeGocciaMethod(AReceiver, 'entries', AArgs, AArgCount);
end;

{ Native Set delegate methods }

function NativeSetHas(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SS: TGocciaSouffleSet;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) then
  begin
    if AArgCount < 1 then Exit(SouffleBoolean(False));
    Exit(SouffleBoolean(SS.Contains(AArgs^)));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'has', AArgs, AArgCount);
end;

function NativeSetAdd(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SS: TGocciaSouffleSet;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) then
  begin
    if AArgCount >= 1 then
      SS.Add(AArgs^);
    Exit(AReceiver);
  end;
  Result := InvokeGocciaMethod(AReceiver, 'add', AArgs, AArgCount);
end;

function NativeSetDelete(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SS: TGocciaSouffleSet;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) then
  begin
    if AArgCount < 1 then Exit(SouffleBoolean(False));
    Exit(SouffleBoolean(SS.Delete(AArgs^)));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'delete', AArgs, AArgCount);
end;

function NativeSetClear(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SS: TGocciaSouffleSet;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) then
  begin
    SS.Clear;
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'clear', AArgs, AArgCount);
end;

function NativeSetForEach(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SS: TGocciaSouffleSet;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) and (AArgCount >= 1) and SouffleIsReference(AArgs^) then
  begin
    for I := 0 to SS.Count - 1 do
    begin
      CallArgs[0] := SS.GetItemAt(I);
      CallArgs[1] := SS.GetItemAt(I);
      CallArgs[2] := AReceiver;
      GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, SouffleNil);
    end;
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'forEach', AArgs, AArgCount);
end;

function NativeSetValues(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeGocciaMethod(AReceiver, 'values', AArgs, AArgCount);
end;

function NativeSetEntries(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := InvokeGocciaMethod(AReceiver, 'entries', AArgs, AArgCount);
end;

{ Native Promise delegate helpers }

function UnwrapPromiseFromReceiver(
  const AReceiver: TSouffleValue): TGocciaPromiseValue; inline;
begin
  Result := nil;
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AReceiver.AsReference).Value is TGocciaPromiseValue) then
    Result := TGocciaPromiseValue(
      TGocciaWrappedValue(AReceiver.AsReference).Value);
end;

function SouffleToCallable(const AArg: TSouffleValue): TGocciaValue;
var
  V: TGocciaValue;
begin
  Result := nil;
  if not SouffleIsReference(AArg) or not Assigned(AArg.AsReference) then Exit;
  V := GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArg);
  if Assigned(V) and V.IsCallable then
    Result := V;
end;

function BridgePromiseStatic(const AGocciaFn: TGocciaValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Args: TGocciaArgumentsCollection;
  I: Integer;
  GocciaArgs: array of TGocciaValue;
  GocciaResult: TGocciaValue;
begin
  SetLength(GocciaArgs, AArgCount);
  for I := 0 to AArgCount - 1 do
    GocciaArgs[I] := GNativeArrayJoinRuntime.UnwrapToGocciaValue(
      PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^);
  Args := TGocciaArgumentsCollection.Create(GocciaArgs);
  try
    try
      GocciaResult := TGocciaFunctionBase(AGocciaFn).Call(Args,
        TGocciaUndefinedLiteralValue.UndefinedValue);
      if Assigned(GocciaResult) then
        Result := GNativeArrayJoinRuntime.ToSouffleValue(GocciaResult)
      else
        Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
    except
      on E: TGocciaThrowValue do
      begin
        GNativeArrayJoinRuntime.RethrowAsVM(E);
        Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      end;
    end;
  finally
    Args.Free;
  end;
end;

{ Native Promise prototype delegate callbacks }

function NativePromiseThen(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  P: TGocciaPromiseValue;
  OnFulfilled, OnRejected: TGocciaValue;
begin
  P := UnwrapPromiseFromReceiver(AReceiver);
  if not Assigned(P) then
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(
      'Promise.prototype.then called on non-Promise');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  OnFulfilled := nil;
  OnRejected := nil;
  if AArgCount > 0 then
    OnFulfilled := SouffleToCallable(AArgs^);
  if AArgCount > 1 then
    OnRejected := SouffleToCallable(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);

  Result := GNativeArrayJoinRuntime.WrapGocciaValue(
    P.InvokeThen(OnFulfilled, OnRejected));
end;

function NativePromiseCatch(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  P: TGocciaPromiseValue;
  OnRejected: TGocciaValue;
begin
  P := UnwrapPromiseFromReceiver(AReceiver);
  if not Assigned(P) then
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(
      'Promise.prototype.catch called on non-Promise');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  OnRejected := nil;
  if AArgCount > 0 then
    OnRejected := SouffleToCallable(AArgs^);

  Result := GNativeArrayJoinRuntime.WrapGocciaValue(
    P.InvokeThen(nil, OnRejected));
end;

function NativePromiseFinally(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  P: TGocciaPromiseValue;
  OnFinally: TGocciaValue;
begin
  P := UnwrapPromiseFromReceiver(AReceiver);
  if not Assigned(P) then
  begin
    Goccia.Values.ErrorHelper.ThrowTypeError(
      'Promise.prototype.finally called on non-Promise');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  OnFinally := nil;
  if AArgCount > 0 then
    OnFinally := SouffleToCallable(AArgs^);

  Result := GNativeArrayJoinRuntime.WrapGocciaValue(
    P.InvokeFinally(OnFinally));
end;

{ Native Promise static delegate callbacks }

function NativePromiseStaticResolve(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Value: TGocciaValue;
  P: TGocciaPromiseValue;
begin
  if AArgCount > 0 then
    Value := GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArgs^)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;

  if Value is TGocciaPromiseValue then
    Exit(GNativeArrayJoinRuntime.WrapGocciaValue(Value));

  P := TGocciaPromiseValue.Create;
  P.Resolve(Value);
  Result := GNativeArrayJoinRuntime.WrapGocciaValue(P);
end;

function NativePromiseStaticReject(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  P: TGocciaPromiseValue;
begin
  P := TGocciaPromiseValue.Create;
  if AArgCount > 0 then
    P.Reject(GNativeArrayJoinRuntime.UnwrapToGocciaValue(AArgs^))
  else
    P.Reject(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := GNativeArrayJoinRuntime.WrapGocciaValue(P);
end;

function NativePromiseStaticAll(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := BridgePromiseStatic(GPromiseAllFn, AArgs, AArgCount);
end;

function NativePromiseStaticAllSettled(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := BridgePromiseStatic(GPromiseAllSettledFn, AArgs, AArgCount);
end;

function NativePromiseStaticRace(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := BridgePromiseStatic(GPromiseRaceFn, AArgs, AArgCount);
end;

function NativePromiseStaticAny(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := BridgePromiseStatic(GPromiseAnyFn, AArgs, AArgCount);
end;

function NativePromiseStaticWithResolvers(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := BridgePromiseStatic(GPromiseWithResolversFn, AArgs, AArgCount);
end;

function NativePromiseStaticTry(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := BridgePromiseStatic(GPromiseTryFn, AArgs, AArgCount);
end;

function NativeRecordHasOwnProperty(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Key: string;
  GocciaObj: TGocciaValue;
  IsSymbol: Boolean;
  SymVal: TGocciaSymbolValue;
begin
  Result := SouffleBoolean(False);
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then
    Exit;
  IsSymbol := False;
  SymVal := nil;
  if AArgCount < 1 then
    Key := 'undefined'
  else if SouffleIsReference(AArgs^) and Assigned(AArgs^.AsReference) and
     (AArgs^.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AArgs^.AsReference).Value is TGocciaSymbolValue) then
  begin
    SymVal := TGocciaSymbolValue(TGocciaWrappedValue(AArgs^.AsReference).Value);
    Key := '@@sym:' + IntToStr(SymVal.Id);
    IsSymbol := True;
  end
  else
    Key := GNativeArrayJoinRuntime.CoerceToString(AArgs^);
  if AReceiver.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Rec.Has(Key) or
       (Rec.HasGetters and Rec.Getters.Has(Key)) or
       (Rec.HasSetters and Rec.Setters.Has(Key)) then
      Exit(SouffleBoolean(True));
    if IsSymbol then
    begin
      GocciaObj := GNativeArrayJoinRuntime.UnwrapToGocciaValue(AReceiver);
      if GocciaObj is TGocciaObjectValue then
        Exit(SouffleBoolean(
          TGocciaObjectValue(GocciaObj).HasSymbolProperty(SymVal)));
    end;
  end
  else if AReceiver.AsReference is TGocciaWrappedValue then
  begin
    GocciaObj := TGocciaWrappedValue(AReceiver.AsReference).Value;
    if GocciaObj is TGocciaObjectValue then
      Result := SouffleBoolean(TGocciaObjectValue(GocciaObj).HasOwnProperty(Key));
  end;
end;

function NativeRecordToString(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TagValue: TSouffleValue;
  Found: Boolean;
begin
  TagValue := GNativeArrayJoinRuntime.GetSymbolOnNativeObject(
    AReceiver, TGocciaSymbolValue.WellKnownToStringTag, Found);
  if Found and SouffleIsStringValue(TagValue) then
    Result := SouffleString('[object ' + SouffleGetString(TagValue) + ']')
  else
    Result := SouffleString('[object Object]');
end;

function NativeRecordValueOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := AReceiver;
end;

function NativeRecordIsPrototypeOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Target: TSouffleValue;
  Walk: TSouffleHeapObject;
  ReceiverObj: TSouffleHeapObject;
  ReceiverGoccia: TGocciaValue;
  TargetGoccia: TGocciaValue;
  GocciaProto: TGocciaObjectValue;
  Proxy: TGocciaSouffleProxy;
begin
  Result := SouffleBoolean(False);
  if AArgCount < 1 then Exit;
  Target := AArgs^;
  if not SouffleIsReference(Target) or not Assigned(Target.AsReference) then Exit;
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then Exit;
  ReceiverObj := AReceiver.AsReference;

  Walk := Target.AsReference.Delegate;
  while Assigned(Walk) do
  begin
    if Walk = ReceiverObj then
      Exit(SouffleBoolean(True));
    Walk := Walk.Delegate;
  end;

  ReceiverGoccia := nil;
  if ReceiverObj is TGocciaWrappedValue then
    ReceiverGoccia := TGocciaWrappedValue(ReceiverObj).Value;

  TargetGoccia := nil;
  if Target.AsReference is TGocciaWrappedValue then
    TargetGoccia := TGocciaWrappedValue(Target.AsReference).Value;

  if Assigned(TargetGoccia) and (TargetGoccia is TGocciaObjectValue) then
  begin
    GocciaProto := TGocciaObjectValue(TargetGoccia).Prototype;
    while Assigned(GocciaProto) do
    begin
      if Assigned(ReceiverGoccia) and (GocciaProto = ReceiverGoccia) then
        Exit(SouffleBoolean(True));
      if GocciaProto is TGocciaSouffleProxy then
      begin
        Proxy := TGocciaSouffleProxy(GocciaProto);
        if Proxy.Target = ReceiverObj then
          Exit(SouffleBoolean(True));
      end;
      GocciaProto := GocciaProto.Prototype;
    end;
  end;
end;

function NativeRecordPropertyIsEnumerable(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Key: string;
  Flags: Byte;
begin
  Result := SouffleBoolean(False);
  if not (SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleRecord)) then Exit;
  Rec := TSouffleRecord(AReceiver.AsReference);
  if AArgCount < 1 then
    Key := 'undefined'
  else
    Key := SouffleValueToString(AArgs^);
  if not Rec.Has(Key) then Exit;
  Flags := Rec.GetEntryFlags(Key);
  Result := SouffleBoolean(Flags and SOUFFLE_PROP_ENUMERABLE <> 0);
end;

function NativeRecordToLocaleString(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  ToStr: TSouffleValue;
begin
  ToStr := GNativeArrayJoinRuntime.GetProperty(AReceiver, 'toString');
  if SouffleIsReference(ToStr) and Assigned(ToStr.AsReference) and
     (ToStr.AsReference is TSouffleClosure) then
    Result := GNativeArrayJoinRuntime.VM.ExecuteFunction(
      TSouffleClosure(ToStr.AsReference), [AReceiver])
  else if SouffleIsReference(ToStr) and Assigned(ToStr.AsReference) and
     (ToStr.AsReference is TSouffleNativeFunction) then
    Result := TSouffleNativeFunction(ToStr.AsReference).Invoke(AReceiver, nil, 0)
  else
    Result := SouffleString('[object Object]');
end;

{ Native String Prototype Methods }

function NativeStrArgAsInt(const AArgs: PSouffleValue;
  const AArgCount, AIndex: Integer; const ADefault: Int64 = 0): Int64;
var
  V: TSouffleValue;
  D: Double;
begin
  if AIndex >= AArgCount then
    Exit(ADefault);
  V := PSouffleValue(PByte(AArgs) + AIndex * SizeOf(TSouffleValue))^;
  case V.Kind of
    svkInteger: Result := V.AsInteger;
    svkFloat:
    begin
      D := V.AsFloat;
      if IsNaN(D) then Result := 0
      else if IsInfinite(D) then
      begin
        if D > 0 then Result := High(Int64) else Result := Low(Int64);
      end
      else Result := Trunc(D);
    end;
    svkBoolean: if V.AsBoolean then Result := 1 else Result := 0;
    svkNil: Result := 0;
  else
    if SouffleIsReference(V) and Assigned(V.AsReference) and
       (V.AsReference is TGocciaWrappedValue) and
       (TGocciaWrappedValue(V.AsReference).Value is TGocciaSymbolValue) then
      Goccia.Values.ErrorHelper.ThrowTypeError(
        'Cannot convert a Symbol value to a number')
    else
      Result := ADefault;
  end;
end;

function NativeStrArgAsString(const AArgs: PSouffleValue;
  const AArgCount, AIndex: Integer;
  const ADefault: string = 'undefined'): string;
var
  V: TSouffleValue;
begin
  if AIndex >= AArgCount then Exit(ADefault);
  V := PSouffleValue(PByte(AArgs) + AIndex * SizeOf(TSouffleValue))^;
  case V.Kind of
    svkNil:
      if V.Flags = GOCCIA_NIL_NULL then Result := 'null'
      else Result := 'undefined';
    svkBoolean:
      if V.AsBoolean then Result := 'true' else Result := 'false';
    svkInteger:
      Result := IntToStr(V.AsInteger);
    svkFloat:
      Result := GNativeArrayJoinRuntime.CoerceToString(V);
  else
    if SouffleIsStringValue(V) then
      Result := SouffleGetString(V)
    else
      Result := GNativeArrayJoinRuntime.CoerceToString(V);
  end;
end;

function NativeStrArgIsUndefined(const AArgs: PSouffleValue;
  const AArgCount, AIndex: Integer): Boolean;
var
  V: TSouffleValue;
begin
  if AIndex >= AArgCount then Exit(True);
  V := PSouffleValue(PByte(AArgs) + AIndex * SizeOf(TSouffleValue))^;
  Result := (V.Kind = svkNil) and (V.Flags <> GOCCIA_NIL_NULL);
end;

function NativeStrNormalizeIndex(const ARelative, ALength: Integer): Integer;
  inline;
begin
  if ARelative < 0 then
    Result := Max(ALength + ARelative, 0)
  else
    Result := Min(ARelative, ALength);
end;

// ES2026 §22.1.3.1 String.prototype.charAt(pos)
function NativeStringCharAt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Idx: Int64;
begin
  S := SouffleGetString(AReceiver);
  Idx := NativeStrArgAsInt(AArgs, AArgCount, 0);
  if (Idx >= 0) and (Idx < Length(S)) then
    Result := SouffleString(S[Idx + 1])
  else
    Result := SouffleString('');
end;

// ES2026 §22.1.3.2 String.prototype.charCodeAt(pos)
function NativeStringCharCodeAt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Idx: Int64;
begin
  S := SouffleGetString(AReceiver);
  Idx := NativeStrArgAsInt(AArgs, AArgCount, 0);
  if (Idx >= 0) and (Idx < Length(S)) then
    Result := SouffleInteger(Ord(S[Idx + 1]))
  else
    Result := SouffleFloat(NaN);
end;

// ES2026 §22.1.3.3 String.prototype.codePointAt(pos)
function NativeStringCodePointAt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Idx: Int64;
  B: Byte;
  CP: Cardinal;
begin
  S := SouffleGetString(AReceiver);
  Idx := NativeStrArgAsInt(AArgs, AArgCount, 0);
  if (Idx < 0) or (Idx >= Length(S)) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  B := Ord(S[Idx + 1]);
  if B < $80 then
    CP := B
  else if (B and $E0) = $C0 then
  begin
    if Idx + 2 > Length(S) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    CP := (Cardinal(B and $1F) shl 6) or Cardinal(Ord(S[Idx + 2]) and $3F);
  end
  else if (B and $F0) = $E0 then
  begin
    if Idx + 3 > Length(S) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    CP := (Cardinal(B and $0F) shl 12) or
      (Cardinal(Ord(S[Idx + 2]) and $3F) shl 6) or
      Cardinal(Ord(S[Idx + 3]) and $3F);
  end
  else if (B and $F8) = $F0 then
  begin
    if Idx + 4 > Length(S) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    CP := (Cardinal(B and $07) shl 18) or
      (Cardinal(Ord(S[Idx + 2]) and $3F) shl 12) or
      (Cardinal(Ord(S[Idx + 3]) and $3F) shl 6) or
      Cardinal(Ord(S[Idx + 4]) and $3F);
  end
  else
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Result := SouffleFloat(CP * 1.0);
end;

// ES2026 §22.1.3.1 String.prototype.at(index)
function NativeStringAt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Idx: Int64;
begin
  S := SouffleGetString(AReceiver);
  Idx := NativeStrArgAsInt(AArgs, AArgCount, 0);
  if Idx < 0 then Idx := Length(S) + Idx;
  if (Idx < 0) or (Idx >= Length(S)) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Result := SouffleString(S[Idx + 1]);
end;

// ES2026 §22.1.3.9 String.prototype.indexOf(searchString [, position])
function NativeStringIndexOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, SearchStr: string;
  StartPos, Found: Integer;
begin
  S := SouffleGetString(AReceiver);
  SearchStr := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  StartPos := Integer(Min(Max(NativeStrArgAsInt(AArgs, AArgCount, 1, 0),
    0), Length(S)));
  if SearchStr = '' then
    Exit(SouffleInteger(Min(StartPos, Length(S))));
  if StartPos < Length(S) then
  begin
    Found := Pos(SearchStr, Copy(S, StartPos + 1, Length(S)));
    if Found > 0 then
      Exit(SouffleInteger(Found + StartPos - 1));
  end;
  Result := SouffleInteger(-1);
end;

// ES2026 §22.1.3.10 String.prototype.lastIndexOf(searchString [, position])
function NativeStringLastIndexOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, SearchStr: string;
  StartPos, I, Found: Integer;
begin
  S := SouffleGetString(AReceiver);
  SearchStr := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  StartPos := Integer(Min(NativeStrArgAsInt(AArgs, AArgCount, 1, Length(S)),
    Int64(Length(S))));
  if SearchStr = '' then
    Exit(SouffleInteger(StartPos));
  Found := -1;
  for I := Min(StartPos, Length(S) - Length(SearchStr)) downto 0 do
    if Copy(S, I + 1, Length(SearchStr)) = SearchStr then
    begin
      Found := I;
      Break;
    end;
  Result := SouffleInteger(Found);
end;

// ES2026 §22.1.3.7 String.prototype.includes(searchString [, position])
function NativeStringIncludes(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, SearchStr: string;
  StartPos: Integer;
begin
  S := SouffleGetString(AReceiver);
  SearchStr := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  StartPos := Integer(Min(Max(NativeStrArgAsInt(AArgs, AArgCount, 1, 0),
    0), Int64(Length(S))));
  if SearchStr = '' then Exit(SouffleBoolean(True));
  if StartPos >= Length(S) then Exit(SouffleBoolean(False));
  Result := SouffleBoolean(
    Pos(SearchStr, Copy(S, StartPos + 1, Length(S))) > 0);
end;

// ES2026 §22.1.3.24 String.prototype.startsWith(searchString [, position])
function NativeStringStartsWith(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, SearchStr: string;
  StartPos: Integer;
begin
  S := SouffleGetString(AReceiver);
  SearchStr := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  StartPos := Integer(Min(Max(NativeStrArgAsInt(AArgs, AArgCount, 1, 0),
    0), Int64(Length(S))));
  if StartPos + Length(SearchStr) > Length(S) then
    Exit(SouffleBoolean(False));
  Result := SouffleBoolean(
    Copy(S, StartPos + 1, Length(SearchStr)) = SearchStr);
end;

// ES2026 §22.1.3.6 String.prototype.endsWith(searchString [, endPosition])
function NativeStringEndsWith(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, SearchStr: string;
  EndPos: Integer;
begin
  S := SouffleGetString(AReceiver);
  SearchStr := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  if NativeStrArgIsUndefined(AArgs, AArgCount, 1) then
    EndPos := Length(S)
  else
    EndPos := Integer(Min(Max(NativeStrArgAsInt(AArgs, AArgCount, 1), 0),
      Int64(Length(S))));
  if EndPos < Length(SearchStr) then Exit(SouffleBoolean(False));
  Result := SouffleBoolean(
    Copy(S, EndPos - Length(SearchStr) + 1, Length(SearchStr)) = SearchStr);
end;

// ES2026 §22.1.3.22 String.prototype.slice(start, end)
function NativeStringSlice(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Len, StartIdx, EndIdx: Integer;
  RawStart, RawEnd: Int64;
begin
  S := SouffleGetString(AReceiver);
  Len := Length(S);
  RawStart := NativeStrArgAsInt(AArgs, AArgCount, 0);
  StartIdx := NativeStrNormalizeIndex(
    Integer(Min(Max(RawStart, Int64(-Len)), Int64(Len))), Len);
  if NativeStrArgIsUndefined(AArgs, AArgCount, 1) then
    EndIdx := Len
  else
  begin
    RawEnd := NativeStrArgAsInt(AArgs, AArgCount, 1);
    EndIdx := NativeStrNormalizeIndex(
      Integer(Min(Max(RawEnd, Int64(-Len)), Int64(Len))), Len);
  end;
  if StartIdx > EndIdx then StartIdx := EndIdx;
  if (StartIdx >= 0) and (StartIdx < Len) and (EndIdx > StartIdx) then
    Result := SouffleString(Copy(S, StartIdx + 1, EndIdx - StartIdx))
  else
    Result := SouffleString('');
end;

// ES2026 §22.1.3.25 String.prototype.substring(start, end)
function NativeStringSubstring(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Len, StartIdx, EndIdx, Temp: Integer;
begin
  S := SouffleGetString(AReceiver);
  Len := Length(S);
  StartIdx := Integer(Min(Max(NativeStrArgAsInt(AArgs, AArgCount, 0), 0),
    Int64(Len)));
  if NativeStrArgIsUndefined(AArgs, AArgCount, 1) then
    EndIdx := Len
  else
    EndIdx := Integer(Min(Max(NativeStrArgAsInt(AArgs, AArgCount, 1), 0),
      Int64(Len)));
  if StartIdx > EndIdx then
  begin
    Temp := StartIdx; StartIdx := EndIdx; EndIdx := Temp;
  end;
  if EndIdx > StartIdx then
    Result := SouffleString(Copy(S, StartIdx + 1, EndIdx - StartIdx))
  else
    Result := SouffleString('');
end;

// ES2026 §22.1.3.28 String.prototype.toUpperCase()
function NativeStringToUpperCase(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString(UpperCase(SouffleGetString(AReceiver)));
end;

// ES2026 §22.1.3.26 String.prototype.toLowerCase()
function NativeStringToLowerCase(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString(LowerCase(SouffleGetString(AReceiver)));
end;

// ES2026 §22.1.3.29 String.prototype.trim()
function NativeStringTrim(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString(Trim(SouffleGetString(AReceiver)));
end;

// ES2026 §22.1.3.30 String.prototype.trimStart()
function NativeStringTrimStart(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString(TrimLeft(SouffleGetString(AReceiver)));
end;

// ES2026 §22.1.3.31 String.prototype.trimEnd()
function NativeStringTrimEnd(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString(TrimRight(SouffleGetString(AReceiver)));
end;

// ES2026 §22.1.3.18 String.prototype.repeat(count)
function NativeStringRepeat(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Count: Int64;
begin
  S := SouffleGetString(AReceiver);
  Count := NativeStrArgAsInt(AArgs, AArgCount, 0, 0);
  if Count < 0 then
    Goccia.Values.ErrorHelper.ThrowRangeError(
      'Invalid count value: must be non-negative');
  if Count > MaxInt then
    Goccia.Values.ErrorHelper.ThrowRangeError(
      'Invalid count value: too large');
  Result := SouffleString(DupeString(S, Integer(Count)));
end;

// ES2026 §22.1.3.15 String.prototype.padStart(maxLength [, fillString])
function NativeStringPadStart(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, PadStr, Padding: string;
  TargetLen, PadNeeded: Integer;
begin
  S := SouffleGetString(AReceiver);
  TargetLen := Integer(Min(NativeStrArgAsInt(AArgs, AArgCount, 0),
    Int64(MaxInt)));
  if Length(S) >= TargetLen then Exit(SouffleString(S));
  if (AArgCount > 1) and not NativeStrArgIsUndefined(AArgs, AArgCount, 1) then
    PadStr := NativeStrArgAsString(AArgs, AArgCount, 1, ' ')
  else
    PadStr := ' ';
  if PadStr = '' then Exit(SouffleString(S));
  PadNeeded := TargetLen - Length(S);
  Padding := '';
  while Length(Padding) < PadNeeded do
    Padding := Padding + PadStr;
  Padding := Copy(Padding, 1, PadNeeded);
  Result := SouffleString(Padding + S);
end;

// ES2026 §22.1.3.14 String.prototype.padEnd(maxLength [, fillString])
function NativeStringPadEnd(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, PadStr, Padding: string;
  TargetLen, PadNeeded: Integer;
begin
  S := SouffleGetString(AReceiver);
  TargetLen := Integer(Min(NativeStrArgAsInt(AArgs, AArgCount, 0),
    Int64(MaxInt)));
  if Length(S) >= TargetLen then Exit(SouffleString(S));
  if (AArgCount > 1) and not NativeStrArgIsUndefined(AArgs, AArgCount, 1) then
    PadStr := NativeStrArgAsString(AArgs, AArgCount, 1, ' ')
  else
    PadStr := ' ';
  if PadStr = '' then Exit(SouffleString(S));
  PadNeeded := TargetLen - Length(S);
  Padding := '';
  while Length(Padding) < PadNeeded do
    Padding := Padding + PadStr;
  Padding := Copy(Padding, 1, PadNeeded);
  Result := SouffleString(S + Padding);
end;

// ES2026 §22.1.3.19 String.prototype.replace(searchValue, replaceValue)
function NativeStringReplace(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, SearchStr, ReplaceStr: string;
  ReplacerArg, CallResult: TSouffleValue;
  FoundPos: Integer;
begin
  S := SouffleGetString(AReceiver);
  SearchStr := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  if AArgCount >= 2 then
    ReplacerArg := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^
  else
    ReplacerArg := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if SouffleIsReference(ReplacerArg) and
     Assigned(ReplacerArg.AsReference) and
     (ReplacerArg.AsReference is TSouffleClosure) then
  begin
    FoundPos := Pos(SearchStr, S);
    if FoundPos = 0 then Exit(SouffleString(S));
    CallResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(
      TSouffleClosure(ReplacerArg.AsReference),
      [SouffleNil, SouffleString(SearchStr),
       SouffleInteger(FoundPos - 1), AReceiver]);
    ReplaceStr := GNativeArrayJoinRuntime.CoerceToString(CallResult);
    Result := SouffleString(
      Copy(S, 1, FoundPos - 1) + ReplaceStr +
      Copy(S, FoundPos + Length(SearchStr), Length(S)));
  end
  else
  begin
    ReplaceStr := NativeStrArgAsString(AArgs, AArgCount, 1, 'undefined');
    if SearchStr = '' then
      Result := SouffleString(ReplaceStr + S)
    else
      Result := SouffleString(StringReplace(S, SearchStr, ReplaceStr, []));
  end;
end;

// ES2026 §22.1.3.20 String.prototype.replaceAll(searchValue, replaceValue)
function NativeStringReplaceAll(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, SearchStr, ReplaceStr, Remaining, Built: string;
  ReplacerArg, CallResult: TSouffleValue;
  FoundPos: Integer;
begin
  S := SouffleGetString(AReceiver);
  SearchStr := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  if AArgCount >= 2 then
    ReplacerArg := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^
  else
    ReplacerArg := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if SouffleIsReference(ReplacerArg) and
     Assigned(ReplacerArg.AsReference) and
     (ReplacerArg.AsReference is TSouffleClosure) then
  begin
    Built := '';
    Remaining := S;
    if SearchStr = '' then
    begin
      FoundPos := 1;
      while FoundPos <= Length(Remaining) do
      begin
        CallResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(
          TSouffleClosure(ReplacerArg.AsReference),
          [SouffleNil, SouffleString(''),
           SouffleInteger(FoundPos - 1), AReceiver]);
        Built := Built +
          GNativeArrayJoinRuntime.CoerceToString(CallResult) +
          Remaining[FoundPos];
        Inc(FoundPos);
      end;
      CallResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(
        TSouffleClosure(ReplacerArg.AsReference),
        [SouffleNil, SouffleString(''),
         SouffleInteger(Length(S)), AReceiver]);
      Built := Built +
        GNativeArrayJoinRuntime.CoerceToString(CallResult);
      Result := SouffleString(Built);
    end
    else
    begin
      while True do
      begin
        FoundPos := Pos(SearchStr, Remaining);
        if FoundPos = 0 then
        begin
          Built := Built + Remaining;
          Break;
        end;
        Built := Built + Copy(Remaining, 1, FoundPos - 1);
        CallResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(
          TSouffleClosure(ReplacerArg.AsReference),
          [SouffleNil, SouffleString(SearchStr),
           SouffleInteger(Length(S) - Length(Remaining) + FoundPos - 1),
           AReceiver]);
        Built := Built +
          GNativeArrayJoinRuntime.CoerceToString(CallResult);
        Remaining := Copy(Remaining,
          FoundPos + Length(SearchStr), Length(Remaining));
      end;
      Result := SouffleString(Built);
    end;
  end
  else
  begin
    ReplaceStr := NativeStrArgAsString(AArgs, AArgCount, 1, 'undefined');
    Result := SouffleString(
      StringReplace(S, SearchStr, ReplaceStr, [rfReplaceAll]));
  end;
end;

// ES2026 §22.1.3.23 String.prototype.split(separator, limit)
function NativeStringSplit(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, Sep, Remaining, Segment: string;
  Arr: TSouffleArray;
  GC: TGarbageCollector;
  SepPos, Limit, I: Integer;
  HasLimit: Boolean;
begin
  S := SouffleGetString(AReceiver);
  Sep := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  HasLimit := (AArgCount > 1) and
    not NativeStrArgIsUndefined(AArgs, AArgCount, 1);
  if HasLimit then
  begin
    Limit := Integer(NativeStrArgAsInt(AArgs, AArgCount, 1));
    if Limit = 0 then
    begin
      Arr := TSouffleArray.Create(0);
      GC := TGarbageCollector.Instance;
      if Assigned(GC) then GC.AllocateObject(Arr);
      if Assigned(GNativeArrayJoinRuntime) then
        Arr.Delegate := GNativeArrayJoinRuntime.VM.ArrayDelegate;
      Exit(SouffleReference(Arr));
    end;
    if Limit < 0 then HasLimit := False;
  end;

  Arr := TSouffleArray.Create(4);
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then GC.AllocateObject(Arr);
  if Assigned(GNativeArrayJoinRuntime) then
    Arr.Delegate := GNativeArrayJoinRuntime.VM.ArrayDelegate;

  if S = '' then
  begin
    if Sep <> '' then Arr.Push(SouffleString(''));
    Exit(SouffleReference(Arr));
  end;
  if Sep = '' then
  begin
    for I := 1 to Length(S) do
    begin
      Arr.Push(SouffleString(S[I]));
      if HasLimit and (Arr.Count >= Limit) then Break;
    end;
    Exit(SouffleReference(Arr));
  end;
  if Pos(Sep, S) = 0 then
  begin
    Arr.Push(SouffleString(S));
    Exit(SouffleReference(Arr));
  end;

  Remaining := S;
  while True do
  begin
    SepPos := Pos(Sep, Remaining);
    if SepPos = 0 then
    begin
      Arr.Push(SouffleString(Remaining));
      Break;
    end;
    Segment := Copy(Remaining, 1, SepPos - 1);
    Arr.Push(SouffleString(Segment));
    if HasLimit and (Arr.Count >= Limit) then Break;
    Remaining := Copy(Remaining, SepPos + Length(Sep), Length(Remaining));
  end;
  Result := SouffleReference(Arr);
end;

// ES2026 §22.1.3.4 String.prototype.concat(...args)
function NativeStringConcat(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  I: Integer;
begin
  S := SouffleGetString(AReceiver);
  for I := 0 to AArgCount - 1 do
    S := S + NativeStrArgAsString(AArgs, AArgCount, I);
  Result := SouffleString(S);
end;

// ES2026 §22.1.3.11 String.prototype.localeCompare(that)
function NativeStringLocaleCompare(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, That: string;
begin
  S := SouffleGetString(AReceiver);
  That := NativeStrArgAsString(AArgs, AArgCount, 0, 'undefined');
  if S < That then Result := SouffleInteger(-1)
  else if S > That then Result := SouffleInteger(1)
  else Result := SouffleInteger(0);
end;

// ES2026 §22.1.3.13 String.prototype.normalize([form])
function NativeStringNormalize(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Form: string;
begin
  if (AArgCount > 0) and not NativeStrArgIsUndefined(AArgs, AArgCount, 0) then
  begin
    Form := NativeStrArgAsString(AArgs, AArgCount, 0, 'NFC');
    if (Form <> 'NFC') and (Form <> 'NFD') and
       (Form <> 'NFKC') and (Form <> 'NFKD') then
      Goccia.Values.ErrorHelper.ThrowRangeError(
        'The normalization form should be one of NFC, NFD, NFKC, NFKD');
  end;
  Result := SouffleString(SouffleGetString(AReceiver));
end;

// ES2026 §22.1.3.8 String.prototype.isWellFormed()
function NativeStringIsWellFormed(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  I: Integer;
  B: Byte;
begin
  S := SouffleGetString(AReceiver);
  I := 1;
  while I <= Length(S) do
  begin
    B := Ord(S[I]);
    if B < $80 then Inc(I)
    else if (B and $E0) = $C0 then
    begin
      if (I + 1 > Length(S)) or ((Ord(S[I + 1]) and $C0) <> $80) then
        Exit(SouffleBoolean(False));
      Inc(I, 2);
    end
    else if (B and $F0) = $E0 then
    begin
      if (I + 2 > Length(S)) or ((Ord(S[I + 1]) and $C0) <> $80) or
         ((Ord(S[I + 2]) and $C0) <> $80) then
        Exit(SouffleBoolean(False));
      if (B = $ED) and (Ord(S[I + 1]) >= $A0) then
        Exit(SouffleBoolean(False));
      Inc(I, 3);
    end
    else if (B and $F8) = $F0 then
    begin
      if (I + 3 > Length(S)) or ((Ord(S[I + 1]) and $C0) <> $80) or
         ((Ord(S[I + 2]) and $C0) <> $80) or
         ((Ord(S[I + 3]) and $C0) <> $80) then
        Exit(SouffleBoolean(False));
      Inc(I, 4);
    end
    else
      Exit(SouffleBoolean(False));
  end;
  Result := SouffleBoolean(True);
end;

// ES2026 §22.1.3.33 String.prototype.toWellFormed()
function NativeStringToWellFormed(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S, R: string;
  I: Integer;
  B: Byte;
begin
  S := SouffleGetString(AReceiver);
  R := '';
  I := 1;
  while I <= Length(S) do
  begin
    B := Ord(S[I]);
    if B < $80 then begin R := R + S[I]; Inc(I); end
    else if (B and $E0) = $C0 then
    begin
      if (I + 1 > Length(S)) or ((Ord(S[I + 1]) and $C0) <> $80) then
      begin R := R + #$EF#$BF#$BD; Inc(I); end
      else begin R := R + Copy(S, I, 2); Inc(I, 2); end;
    end
    else if (B and $F0) = $E0 then
    begin
      if (I + 2 > Length(S)) or ((Ord(S[I + 1]) and $C0) <> $80) or
         ((Ord(S[I + 2]) and $C0) <> $80) then
      begin R := R + #$EF#$BF#$BD; Inc(I); end
      else if (B = $ED) and (Ord(S[I + 1]) >= $A0) then
      begin R := R + #$EF#$BF#$BD; Inc(I, 3); end
      else begin R := R + Copy(S, I, 3); Inc(I, 3); end;
    end
    else if (B and $F8) = $F0 then
    begin
      if (I + 3 > Length(S)) or ((Ord(S[I + 1]) and $C0) <> $80) or
         ((Ord(S[I + 2]) and $C0) <> $80) or
         ((Ord(S[I + 3]) and $C0) <> $80) then
      begin R := R + #$EF#$BF#$BD; Inc(I); end
      else begin R := R + Copy(S, I, 4); Inc(I, 4); end;
    end
    else begin R := R + #$EF#$BF#$BD; Inc(I); end;
  end;
  Result := SouffleString(R);
end;

// ES2026 §22.1.3.32 String.prototype.valueOf()
function NativeStringValueOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString(SouffleGetString(AReceiver));
end;

// ES2026 §22.1.3.27 String.prototype.toString()
function NativeStringToString(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString(SouffleGetString(AReceiver));
end;

{ Native Number Prototype Methods }

function NativeNumExtract(const AReceiver: TSouffleValue): Double; inline;
begin
  case AReceiver.Kind of
    svkInteger: Result := AReceiver.AsInteger;
    svkFloat:   Result := AReceiver.AsFloat;
  else
    Result := NaN;
  end;
end;

// ES2026 §21.1.3.3 Number.prototype.toFixed(fractionDigits)
function NativeNumberToFixed(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Num: Double;
  Digits: Integer;
begin
  Num := NativeNumExtract(AReceiver);
  if IsNaN(Num) then Exit(SouffleString('NaN'));
  if IsInfinite(Num) then
  begin
    if Num > 0 then Exit(SouffleString('Infinity'))
    else Exit(SouffleString('-Infinity'));
  end;
  Digits := Integer(NativeStrArgAsInt(AArgs, AArgCount, 0, 0));
  if (Digits < 0) or (Digits > 100) then
    Goccia.Values.ErrorHelper.ThrowRangeError(
      'toFixed() digits argument must be between 0 and 100');
  Result := SouffleString(
    FormatFloat('0.' + StringOfChar('0', Digits), Num));
end;

// ES2026 §21.1.3.6 Number.prototype.toString([radix])
function NativeNumberToString(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Num: Double;
  Radix: Integer;
begin
  Num := NativeNumExtract(AReceiver);
  if IsNaN(Num) then Exit(SouffleString('NaN'));
  if IsInfinite(Num) then
  begin
    if Num > 0 then Exit(SouffleString('Infinity'))
    else Exit(SouffleString('-Infinity'));
  end;
  if NativeStrArgIsUndefined(AArgs, AArgCount, 0) then
    Exit(SouffleString(
      GNativeArrayJoinRuntime.CoerceToString(AReceiver)));
  Radix := Integer(NativeStrArgAsInt(AArgs, AArgCount, 0, 10));
  if (Radix < 2) or (Radix > 36) then
    Goccia.Values.ErrorHelper.ThrowRangeError(
      'toString() radix must be between 2 and 36');
  if Radix = 16 then
    Result := SouffleString(LowerCase(IntToHex(Trunc(Num), 1)))
  else
    Result := SouffleString(
      GNativeArrayJoinRuntime.CoerceToString(AReceiver));
end;

// ES2026 §21.1.3.7 Number.prototype.valueOf()
function NativeNumberValueOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := AReceiver;
end;

// ES2026 §21.1.3.5 Number.prototype.toPrecision(precision)
function NativeNumberToPrecision(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Num: Double;
  Precision: Integer;
begin
  Num := NativeNumExtract(AReceiver);
  if IsNaN(Num) or IsInfinite(Num) then
    Exit(SouffleString(
      GNativeArrayJoinRuntime.CoerceToString(AReceiver)));
  if NativeStrArgIsUndefined(AArgs, AArgCount, 0) then
    Exit(SouffleString(
      GNativeArrayJoinRuntime.CoerceToString(AReceiver)));
  Precision := Integer(NativeStrArgAsInt(AArgs, AArgCount, 0, 1));
  if (Precision < 1) or (Precision > 100) then
    Goccia.Values.ErrorHelper.ThrowRangeError(
      'toPrecision() argument must be between 1 and 100');
  Result := SouffleString(FloatToStrF(Num, ffGeneral, Precision, 0));
end;

// ES2026 §21.1.3.2 Number.prototype.toExponential(fractionDigits)
function NativeNumberToExponential(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Num, Mantissa: Double;
  FractionDigits, Exp: Integer;
  HasExplicit: Boolean;
  Sign, MantissaStr, ExpSign: string;
begin
  Num := NativeNumExtract(AReceiver);
  if IsNaN(Num) then Exit(SouffleString('NaN'));
  if IsInfinite(Num) then
  begin
    if Num > 0 then Exit(SouffleString('Infinity'))
    else Exit(SouffleString('-Infinity'));
  end;
  HasExplicit := (AArgCount > 0) and
    not NativeStrArgIsUndefined(AArgs, AArgCount, 0);
  if HasExplicit then
  begin
    FractionDigits := Integer(NativeStrArgAsInt(AArgs, AArgCount, 0));
    if (FractionDigits < 0) or (FractionDigits > 100) then
      Goccia.Values.ErrorHelper.ThrowRangeError(
        'toExponential() argument must be between 0 and 100');
  end
  else
    FractionDigits := -1;
  Sign := '';
  if Num < 0 then begin Sign := '-'; Num := -Num; end;
  if Num = 0 then
  begin
    if FractionDigits < 0 then FractionDigits := 0;
    if FractionDigits = 0 then MantissaStr := '0'
    else MantissaStr := '0.' + StringOfChar('0', FractionDigits);
    Exit(SouffleString(Sign + MantissaStr + 'e+0'));
  end;
  Exp := Trunc(Math.Floor(Math.Log10(Num)));
  Mantissa := Num / Math.Power(10, Exp);
  if FractionDigits < 0 then
    MantissaStr := FloatToStrF(Mantissa, ffGeneral, 15, 0)
  else
  begin
    MantissaStr := FormatFloat(
      '0.' + StringOfChar('0', FractionDigits), Mantissa);
    if (Length(MantissaStr) > 1) and (MantissaStr[1] <> '0') and
       (Pos('.', MantissaStr) > 0) and (MantissaStr[1] >= '2') then
    begin
      Mantissa := Mantissa / 10; Inc(Exp);
      MantissaStr := FormatFloat(
        '0.' + StringOfChar('0', FractionDigits), Mantissa);
    end;
  end;
  if Exp >= 0 then ExpSign := '+' else ExpSign := '';
  Result := SouffleString(
    Sign + MantissaStr + 'e' + ExpSign + IntToStr(Exp));
end;

const
  STRING_PROTOTYPE_METHODS: array[0..27] of TSouffleMethodEntry = (
    (Name: 'charAt';         Arity: 1; Callback: @NativeStringCharAt),
    (Name: 'charCodeAt';     Arity: 1; Callback: @NativeStringCharCodeAt),
    (Name: 'codePointAt';    Arity: 1; Callback: @NativeStringCodePointAt),
    (Name: 'at';             Arity: 1; Callback: @NativeStringAt),
    (Name: 'indexOf';        Arity: 1; Callback: @NativeStringIndexOf),
    (Name: 'lastIndexOf';    Arity: 1; Callback: @NativeStringLastIndexOf),
    (Name: 'includes';       Arity: 1; Callback: @NativeStringIncludes),
    (Name: 'startsWith';     Arity: 1; Callback: @NativeStringStartsWith),
    (Name: 'endsWith';       Arity: 1; Callback: @NativeStringEndsWith),
    (Name: 'slice';          Arity: 2; Callback: @NativeStringSlice),
    (Name: 'substring';      Arity: 2; Callback: @NativeStringSubstring),
    (Name: 'toUpperCase';    Arity: 0; Callback: @NativeStringToUpperCase),
    (Name: 'toLowerCase';    Arity: 0; Callback: @NativeStringToLowerCase),
    (Name: 'trim';           Arity: 0; Callback: @NativeStringTrim),
    (Name: 'trimStart';      Arity: 0; Callback: @NativeStringTrimStart),
    (Name: 'trimEnd';        Arity: 0; Callback: @NativeStringTrimEnd),
    (Name: 'repeat';         Arity: 1; Callback: @NativeStringRepeat),
    (Name: 'padStart';       Arity: 1; Callback: @NativeStringPadStart),
    (Name: 'padEnd';         Arity: 1; Callback: @NativeStringPadEnd),
    (Name: 'replace';        Arity: 2; Callback: @NativeStringReplace),
    (Name: 'replaceAll';     Arity: 2; Callback: @NativeStringReplaceAll),
    (Name: 'split';          Arity: 1; Callback: @NativeStringSplit),
    (Name: 'concat';         Arity: 1; Callback: @NativeStringConcat),
    (Name: 'localeCompare';  Arity: 1; Callback: @NativeStringLocaleCompare),
    (Name: 'normalize';      Arity: 0; Callback: @NativeStringNormalize),
    (Name: 'isWellFormed';   Arity: 0; Callback: @NativeStringIsWellFormed),
    (Name: 'toWellFormed';   Arity: 0; Callback: @NativeStringToWellFormed),
    (Name: 'toString';       Arity: 0; Callback: @NativeStringToString)
  );

  NUMBER_PROTOTYPE_METHODS: array[0..4] of TSouffleMethodEntry = (
    (Name: 'toFixed';       Arity: 1; Callback: @NativeNumberToFixed),
    (Name: 'toString';      Arity: 1; Callback: @NativeNumberToString),
    (Name: 'valueOf';       Arity: 0; Callback: @NativeNumberValueOf),
    (Name: 'toPrecision';   Arity: 1; Callback: @NativeNumberToPrecision),
    (Name: 'toExponential'; Arity: 1; Callback: @NativeNumberToExponential)
  );

  ARRAY_PROTOTYPE_METHODS: array[0..27] of TSouffleMethodEntry = (
    (Name: 'toString';      Arity: 0; Callback: @NativeArrayToString),
    (Name: 'push';          Arity: 1; Callback: @NativeArrayPush),
    (Name: 'pop';           Arity: 0; Callback: @NativeArrayPop),
    (Name: 'join';          Arity: 1; Callback: @NativeArrayJoin),
    (Name: 'indexOf';       Arity: 1; Callback: @NativeArrayIndexOf),
    (Name: 'includes';      Arity: 1; Callback: @NativeArrayIncludes),
    (Name: 'slice';         Arity: 2; Callback: @NativeArraySlice),
    (Name: 'reverse';       Arity: 0; Callback: @NativeArrayReverse),
    (Name: 'concat';        Arity: 1; Callback: @NativeArrayConcat),
    (Name: 'shift';         Arity: 0; Callback: @NativeArrayShift),
    (Name: 'unshift';       Arity: 1; Callback: @NativeArrayUnshift),
    (Name: 'fill';          Arity: 1; Callback: @NativeArrayFill),
    (Name: 'at';            Arity: 1; Callback: @NativeArrayAt),
    (Name: 'forEach';       Arity: 1; Callback: @NativeArrayForEach),
    (Name: 'map';           Arity: 1; Callback: @NativeArrayMap),
    (Name: 'filter';        Arity: 1; Callback: @NativeArrayFilter),
    (Name: 'find';          Arity: 1; Callback: @NativeArrayFind),
    (Name: 'findIndex';     Arity: 1; Callback: @NativeArrayFindIndex),
    (Name: 'every';         Arity: 1; Callback: @NativeArrayEvery),
    (Name: 'some';          Arity: 1; Callback: @NativeArraySome),
    (Name: 'reduce';        Arity: 1; Callback: @NativeArrayReduce),
    (Name: 'reduceRight';   Arity: 1; Callback: @NativeArrayReduceRight),
    (Name: 'sort';          Arity: 0; Callback: @NativeArraySort),
    (Name: 'findLast';      Arity: 1; Callback: @NativeArrayFindLast),
    (Name: 'findLastIndex';  Arity: 1; Callback: @NativeArrayFindLastIndex),
    (Name: 'flat';          Arity: 0; Callback: @NativeArrayFlat),
    (Name: 'flatMap';       Arity: 1; Callback: @NativeArrayFlatMap),
    (Name: 'splice';        Arity: 2; Callback: @NativeArraySplice)
  );

  RECORD_PROTOTYPE_METHODS: array[0..5] of TSouffleMethodEntry = (
    (Name: 'hasOwnProperty';       Arity: 1; Callback: @NativeRecordHasOwnProperty),
    (Name: 'isPrototypeOf';        Arity: 1; Callback: @NativeRecordIsPrototypeOf),
    (Name: 'propertyIsEnumerable'; Arity: 1; Callback: @NativeRecordPropertyIsEnumerable),
    (Name: 'toString';             Arity: 0; Callback: @NativeRecordToString),
    (Name: 'valueOf';              Arity: 0; Callback: @NativeRecordValueOf),
    (Name: 'toLocaleString';       Arity: 0; Callback: @NativeRecordToLocaleString)
  );

  MAP_PROTOTYPE_METHODS: array[0..8] of TSouffleMethodEntry = (
    (Name: 'get';      Arity: 1; Callback: @NativeMapGet),
    (Name: 'set';      Arity: 2; Callback: @NativeMapSet),
    (Name: 'has';      Arity: 1; Callback: @NativeMapHas),
    (Name: 'delete';   Arity: 1; Callback: @NativeMapDelete),
    (Name: 'clear';    Arity: 0; Callback: @NativeMapClear),
    (Name: 'forEach';  Arity: 1; Callback: @NativeMapForEach),
    (Name: 'keys';     Arity: 0; Callback: @NativeMapKeys),
    (Name: 'values';   Arity: 0; Callback: @NativeMapValues),
    (Name: 'entries';  Arity: 0; Callback: @NativeMapEntries)
  );

  SET_PROTOTYPE_METHODS: array[0..6] of TSouffleMethodEntry = (
    (Name: 'has';      Arity: 1; Callback: @NativeSetHas),
    (Name: 'add';      Arity: 1; Callback: @NativeSetAdd),
    (Name: 'delete';   Arity: 1; Callback: @NativeSetDelete),
    (Name: 'clear';    Arity: 0; Callback: @NativeSetClear),
    (Name: 'forEach';  Arity: 1; Callback: @NativeSetForEach),
    (Name: 'values';   Arity: 0; Callback: @NativeSetValues),
    (Name: 'entries';  Arity: 0; Callback: @NativeSetEntries)
  );

  PROMISE_PROTOTYPE_METHODS: array[0..2] of TSouffleMethodEntry = (
    (Name: 'then';    Arity: 2; Callback: @NativePromiseThen),
    (Name: 'catch';   Arity: 1; Callback: @NativePromiseCatch),
    (Name: 'finally'; Arity: 1; Callback: @NativePromiseFinally)
  );

  PROMISE_STATIC_METHODS: array[0..7] of TSouffleMethodEntry = (
    (Name: 'resolve';       Arity: 1; Callback: @NativePromiseStaticResolve),
    (Name: 'reject';        Arity: 1; Callback: @NativePromiseStaticReject),
    (Name: 'all';           Arity: 1; Callback: @NativePromiseStaticAll),
    (Name: 'allSettled';    Arity: 1; Callback: @NativePromiseStaticAllSettled),
    (Name: 'race';          Arity: 1; Callback: @NativePromiseStaticRace),
    (Name: 'any';           Arity: 1; Callback: @NativePromiseStaticAny),
    (Name: 'withResolvers'; Arity: 0; Callback: @NativePromiseStaticWithResolvers),
    (Name: 'try';           Arity: 1; Callback: @NativePromiseStaticTry)
  );

procedure TGocciaRuntimeOperations.ClearTransientCaches;
begin
  FClosureBridgeCache.Clear;
  FArrayBridgeCache.Clear;
  FArrayBridgeDirty := False;
  FArrayBridgeReverse.Clear;
  FRecordBridgeCache.Clear;
end;

procedure TGocciaRuntimeOperations.RegisterDelegates;
begin
  if not Assigned(FVM) then Exit;

  RegisterConstGlobal('undefined', SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  RegisterConstGlobal('NaN', SouffleFloat(NaN));
  RegisterConstGlobal('Infinity', SouffleFloat(Infinity));

  GNativeArrayJoinRuntime := Self;

  FStringDelegate := TSouffleRecord(
    BuildDelegate(STRING_PROTOTYPE_METHODS));
  FNumberDelegate := TSouffleRecord(
    BuildDelegate(NUMBER_PROTOTYPE_METHODS));

  FMapDelegate := TSouffleRecord(
    BuildDelegate(MAP_PROTOTYPE_METHODS));
  FSetDelegate := TSouffleRecord(
    BuildDelegate(SET_PROTOTYPE_METHODS));
  FPromiseDelegate := TSouffleRecord(
    BuildDelegate(PROMISE_PROTOTYPE_METHODS));
  FPromiseStaticDelegate := TSouffleRecord(
    BuildDelegate(PROMISE_STATIC_METHODS));

  FVM.ArrayDelegate := TSouffleRecord(
    BuildDelegate(ARRAY_PROTOTYPE_METHODS));
  FVM.RecordDelegate := TSouffleRecord(
    BuildDelegate(RECORD_PROTOTYPE_METHODS));
end;

function ExtractNativeFn(const AGlobals: TOrderedStringMap<TSouffleValue>;
  const AName: string): TGocciaNativeFunctionValue;
var
  Val: TSouffleValue;
begin
  Result := nil;
  if AGlobals.TryGetValue(AName, Val) and SouffleIsReference(Val) and
     Assigned(Val.AsReference) and (Val.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(Val.AsReference).Value is TGocciaNativeFunctionValue) then
    Result := TGocciaNativeFunctionValue(TGocciaWrappedValue(Val.AsReference).Value);
end;

function ExtractSubMethod(const AFn: TGocciaNativeFunctionValue;
  const AName: string): TGocciaNativeFunctionValue;
var
  Sub: TGocciaValue;
begin
  Result := nil;
  if not Assigned(AFn) then Exit;
  Sub := AFn.GetProperty(AName);
  if Assigned(Sub) and (Sub is TGocciaNativeFunctionValue) then
    Result := TGocciaNativeFunctionValue(Sub);
end;

procedure ReplaceGlobalWithNative(
  const AGlobals: TOrderedStringMap<TSouffleValue>;
  const AConstGlobals: TOrderedStringMap<Boolean>;
  const AName: string; const AArity: Integer;
  const ACallback: TSouffleNativeCallback);
var
  Fn: TSouffleNativeFunction;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  Fn := TSouffleNativeFunction.Create(AName, AArity, ACallback);
  if Assigned(GC) then GC.AllocateObject(Fn);
  AGlobals.AddOrSetValue(AName, SouffleReference(Fn));
  AConstGlobals.AddOrSetValue(AName, True);
end;

function BuildSubMethodDelegate(
  const AEntries: array of TSouffleMethodEntry): TSouffleRecord;
var
  Fn: TSouffleNativeFunction;
  GC: TGarbageCollector;
  I: Integer;
begin
  GC := TGarbageCollector.Instance;
  Result := TSouffleRecord.Create(Length(AEntries));
  if Assigned(GC) then GC.AllocateObject(Result);
  for I := 0 to High(AEntries) do
  begin
    Fn := TSouffleNativeFunction.Create(
      AEntries[I].Name, AEntries[I].Arity, AEntries[I].Callback);
    if Assigned(GC) then GC.AllocateObject(Fn);
    Result.Put(AEntries[I].Name, SouffleReference(Fn));
  end;
end;

function NativePerformanceToStringTag(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := SouffleString('Performance');
end;

function BuildPerformanceRecord(const APerformance: TGocciaObjectValue;
  const ARuntime: TGocciaRuntimeOperations): TSouffleRecord;
var
  GC: TGarbageCollector;
  FnVal, GetterVal: TGocciaValue;
  NativeFn: TGocciaNativeFunctionValue;
  Descriptor: TGocciaPropertyDescriptor;
  GetterFn: TSouffleNativeFunction;

  procedure AddMethod(const AName: string);
  var
    BridgedFn: TGocciaBridgedFunction;
  begin
    FnVal := APerformance.GetProperty(AName);
    if not (FnVal is TGocciaNativeFunctionValue) then
      Exit;
    BridgedFn := TGocciaBridgedFunction.Create(
      TGocciaNativeFunctionValue(FnVal), ARuntime);
    if Assigned(GC) then
      GC.AllocateObject(BridgedFn);
    Result.PutWithFlags(AName, SouffleReference(BridgedFn),
      SOUFFLE_PROP_WRITABLE or SOUFFLE_PROP_CONFIGURABLE);
  end;

begin
  GC := TGarbageCollector.Instance;
  Result := TSouffleRecord.Create(2);
  if Assigned(ARuntime.VM) then
    Result.Delegate := ARuntime.VM.RecordDelegate;
  if Assigned(GC) then
    GC.AllocateObject(Result);

  AddMethod(PROP_NOW);
  AddMethod(PROP_TO_JSON);

  Descriptor := APerformance.GetOwnPropertyDescriptor(PROP_TIME_ORIGIN);
  if (Descriptor is TGocciaPropertyDescriptorAccessor) then
  begin
    GetterVal := TGocciaPropertyDescriptorAccessor(Descriptor).Getter;
    if GetterVal is TGocciaNativeFunctionValue then
    begin
      NativeFn := TGocciaNativeFunctionValue(GetterVal);
      Result.Getters.PutWithFlags(PROP_TIME_ORIGIN,
        ARuntime.ToSouffleValue(NativeFn), SOUFFLE_PROP_CONFIGURABLE);
    end;
  end;

  GetterFn := TSouffleNativeFunction.Create(
    'get [Symbol.toStringTag]', 0, @NativePerformanceToStringTag);
  if Assigned(GC) then
    GC.AllocateObject(GetterFn);
  Result.Getters.PutWithFlags(
    '@@sym:' + IntToStr(TGocciaSymbolValue.WellKnownToStringTag.Id),
    SouffleReference(GetterFn), SOUFFLE_PROP_CONFIGURABLE);
end;

procedure TGocciaRuntimeOperations.RegisterTestNatives;
const
  DESCRIBE_SUB_METHODS: array[0..2] of TSouffleMethodEntry = (
    (Name: 'skip';   Arity: 2; Callback: @NativeBridgedDescribeSkip),
    (Name: 'skipIf'; Arity: 1; Callback: @NativeBridgedDescribeSkipIf),
    (Name: 'runIf';  Arity: 1; Callback: @NativeBridgedDescribeRunIf)
  );
  TEST_SUB_METHODS: array[0..2] of TSouffleMethodEntry = (
    (Name: 'skip';   Arity: 2; Callback: @NativeBridgedTestSkip),
    (Name: 'skipIf'; Arity: 1; Callback: @NativeBridgedTestSkipIf),
    (Name: 'runIf';  Arity: 1; Callback: @NativeBridgedTestRunIf)
  );
var
  DescribeFn, TestFn: TGocciaNativeFunctionValue;
begin
  if not Assigned(FVM) then Exit;
  if not FGlobals.ContainsKey('describe') then Exit;

  DescribeFn := ExtractNativeFn(FGlobals, 'describe');
  if not Assigned(DescribeFn) then Exit;

  GBridgedDescribe := DescribeFn;
  GBridgedDescribeSkip := ExtractSubMethod(DescribeFn, 'skip');
  GBridgedDescribeSkipIf := ExtractSubMethod(DescribeFn, 'skipIf');
  GBridgedDescribeRunIf := ExtractSubMethod(DescribeFn, 'runIf');
  ReplaceGlobalWithNative(FGlobals, FConstGlobals, 'describe', 2, @NativeBridgedDescribe);
  FDescribeDelegate := BuildSubMethodDelegate(DESCRIBE_SUB_METHODS);

  TestFn := ExtractNativeFn(FGlobals, 'test');
  if Assigned(TestFn) then
  begin
    GBridgedTest := TestFn;
    GBridgedTestSkip := ExtractSubMethod(TestFn, 'skip');
    GBridgedTestSkipIf := ExtractSubMethod(TestFn, 'skipIf');
    GBridgedTestRunIf := ExtractSubMethod(TestFn, 'runIf');
    ReplaceGlobalWithNative(FGlobals, FConstGlobals, 'test', 2, @NativeBridgedTest);
    FTestDelegate := BuildSubMethodDelegate(TEST_SUB_METHODS);
  end;

  GBridgedExpect := ExtractNativeFn(FGlobals, 'expect');
  if Assigned(GBridgedExpect) then
    ReplaceGlobalWithNative(FGlobals, FConstGlobals, 'expect', 1, @NativeBridgedExpect);

  GBridgedIt := ExtractNativeFn(FGlobals, 'it');
  if Assigned(GBridgedIt) then
    ReplaceGlobalWithNative(FGlobals, FConstGlobals, 'it', 2, @NativeBridgedIt);

  GBridgedBeforeEach := ExtractNativeFn(FGlobals, 'beforeEach');
  if Assigned(GBridgedBeforeEach) then
    ReplaceGlobalWithNative(FGlobals, FConstGlobals, 'beforeEach', 1, @NativeBridgedBeforeEach);

  GBridgedAfterEach := ExtractNativeFn(FGlobals, 'afterEach');
  if Assigned(GBridgedAfterEach) then
    ReplaceGlobalWithNative(FGlobals, FConstGlobals, 'afterEach', 1, @NativeBridgedAfterEach);

  GBridgedRunTests := ExtractNativeFn(FGlobals, 'runTests');
  if Assigned(GBridgedRunTests) then
    ReplaceGlobalWithNative(FGlobals, FConstGlobals, 'runTests', 0, @NativeBridgedRunTests);
end;

function ConvertObjectToNativeRecord(
  const AObj: TGocciaObjectValue;
  const ARuntime: TGocciaRuntimeOperations): TSouffleRecord;
var
  Names: TArray<string>;
  I: Integer;
  PropVal, TagVal: TGocciaValue;
  BridgedFn: TGocciaBridgedFunction;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  Names := AObj.GetOwnPropertyNames;
  Result := TSouffleRecord.Create(Length(Names));
  if Assigned(ARuntime.VM) then
    Result.Delegate := ARuntime.VM.RecordDelegate;
  if Assigned(GC) then GC.AllocateObject(Result);
  for I := 0 to High(Names) do
  begin
    PropVal := AObj.GetProperty(Names[I]);
    if not Assigned(PropVal) then Continue;
    if PropVal is TGocciaNativeFunctionValue then
    begin
      BridgedFn := TGocciaBridgedFunction.Create(
        TGocciaNativeFunctionValue(PropVal), ARuntime);
      if Assigned(GC) then GC.AllocateObject(BridgedFn);
      Result.Put(Names[I], SouffleReference(BridgedFn));
    end
    else
      Result.Put(Names[I], ARuntime.ToSouffleValue(PropVal));
  end;

  TagVal := AObj.GetSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag);
  if Assigned(TagVal) and (TagVal is TGocciaStringLiteralValue) then
    Result.Put('@@sym:' + IntToStr(TGocciaSymbolValue.WellKnownToStringTag.Id),
      SouffleString(TGocciaStringLiteralValue(TagVal).Value));
end;

procedure TGocciaRuntimeOperations.RegisterNativeBuiltins;
var
  Keys: TOrderedStringMap<TSouffleValue>.TKeyArray;
  KeyIdx: Integer;
  Key: string;
  Val, GlobalThisVal: TSouffleValue;
  Wrapped: TGocciaWrappedValue;
  GocciaVal: TGocciaValue;
  ObjVal: TGocciaObjectValue;
  NativeFnVal: TGocciaNativeFunctionValue;
  BridgedFn: TGocciaBridgedFunction;
  Rec, GlobalThisRec: TSouffleRecord;
  GC: TGarbageCollector;
  GlobalPair: TOrderedStringMap<TSouffleValue>.TKeyValuePair;
begin
  if not Assigned(FVM) then Exit;
  GC := TGarbageCollector.Instance;
  Keys := FGlobals.Keys;
  for KeyIdx := 0 to Length(Keys) - 1 do
  begin
    Key := Keys[KeyIdx];
    if Key = 'globalThis' then Continue;
    Val := FGlobals[Key];
    if not SouffleIsReference(Val) or not Assigned(Val.AsReference) then Continue;
    if not (Val.AsReference is TGocciaWrappedValue) then Continue;

    Wrapped := TGocciaWrappedValue(Val.AsReference);
    GocciaVal := Wrapped.Value;

    if Key = 'performance' then
    begin
      if GocciaVal is TGocciaObjectValue then
      begin
        Rec := BuildPerformanceRecord(TGocciaObjectValue(GocciaVal), Self);
        FGlobals.AddOrSetValue(Key, SouffleReference(Rec));
      end;
      Continue;
    end;

    if GocciaVal is TGocciaNativeFunctionValue then
    begin
      NativeFnVal := TGocciaNativeFunctionValue(GocciaVal);
      BridgedFn := TGocciaBridgedFunction.Create(NativeFnVal, Self);
      if Assigned(GC) then GC.AllocateObject(BridgedFn);
      FGlobals.AddOrSetValue(Key, SouffleReference(BridgedFn));
      if Key = CONSTRUCTOR_PROMISE then
      begin
        FPromiseConstructor := BridgedFn;
        GPromiseAllFn := NativeFnVal.GetProperty('all');
        GPromiseAllSettledFn := NativeFnVal.GetProperty('allSettled');
        GPromiseRaceFn := NativeFnVal.GetProperty('race');
        GPromiseAnyFn := NativeFnVal.GetProperty('any');
        GPromiseWithResolversFn := NativeFnVal.GetProperty('withResolvers');
        GPromiseTryFn := NativeFnVal.GetProperty('try');
      end;
    end
    else if (GocciaVal is TGocciaObjectValue)
      and not (GocciaVal is TGocciaClassValue)
      and not (GocciaVal is TGocciaFunctionBase) then
    begin
      ObjVal := TGocciaObjectValue(GocciaVal);
      Rec := ConvertObjectToNativeRecord(ObjVal, Self);
      FGlobals.AddOrSetValue(Key, SouffleReference(Rec));
    end;
  end;

  if FGlobals.TryGetValue('globalThis', GlobalThisVal) and
     SouffleIsReference(GlobalThisVal) and
     Assigned(GlobalThisVal.AsReference) then
  begin
    GlobalThisRec := TSouffleRecord.Create(FGlobals.Count);
    if Assigned(FVM) then
      GlobalThisRec.Delegate := FVM.RecordDelegate;
    if Assigned(GC) then GC.AllocateObject(GlobalThisRec);
    for GlobalPair in FGlobals do
      GlobalThisRec.Put(GlobalPair.Key, GlobalPair.Value);
    GlobalThisRec.Put('globalThis', SouffleReference(GlobalThisRec));
    FGlobals.AddOrSetValue('globalThis', SouffleReference(GlobalThisRec));
  end;
end;

procedure TGocciaRuntimeOperations.RegisterFormalParameterCount(
  const ATemplate: TSouffleFunctionTemplate; const ACount: Integer);
begin
  FFormalParameterCounts.AddOrSetValue(ATemplate, ACount);
end;

function TGocciaRuntimeOperations.GetFormalParameterCount(
  const ATemplate: TSouffleFunctionTemplate): Integer;
begin
  if not FFormalParameterCounts.TryGetValue(ATemplate, Result) then
    Result := -1;
end;

procedure TGocciaRuntimeOperations.RegisterGlobal(const AName: string;
  const AValue: TSouffleValue);
begin
  FGlobals.AddOrSetValue(AName, AValue);
end;

procedure TGocciaRuntimeOperations.RegisterConstGlobal(const AName: string;
  const AValue: TSouffleValue);
begin
  FGlobals.AddOrSetValue(AName, AValue);
  FConstGlobals.AddOrSetValue(AName, True);
end;

procedure TGocciaRuntimeOperations.PatchGocciaScriptStrictTypes;
var
  Val: TSouffleValue;
  Obj: TGocciaObjectValue;
begin
  if not FGlobals.TryGetValue('GocciaScript', Val) then
    Exit;
  if not SouffleIsReference(Val) then
    Exit;
  if Val.AsReference is TGocciaWrappedValue then
  begin
    if TGocciaWrappedValue(Val.AsReference).Value is TGocciaObjectValue then
    begin
      Obj := TGocciaObjectValue(TGocciaWrappedValue(Val.AsReference).Value);
      Obj.AssignProperty(PROP_STRICT_TYPES, TGocciaBooleanLiteralValue.TrueValue);
    end;
  end
  else if Val.AsReference is TSouffleRecord then
    TSouffleRecord(Val.AsReference).Put(PROP_STRICT_TYPES, SouffleBoolean(True));
end;

function TGocciaRuntimeOperations.RequireIterable(
  const AValue: TSouffleValue): TSouffleValue;
var
  Arr: TSouffleArray;
  IterVal, ElemVal: TSouffleValue;
  Done: Boolean;
  StrVal: string;
  I: Integer;
begin
  try
    if SouffleIsReference(AValue) and Assigned(AValue.AsReference) and
       (AValue.AsReference is TSouffleArray) then
      Exit(AValue);

    if SouffleIsNil(AValue) then
    begin
      if AValue.Flags = GOCCIA_NIL_NULL then
        ThrowTypeError('Cannot destructure null as it is not iterable')
      else
        ThrowTypeError('Cannot destructure undefined as it is not iterable');
    end;

    Arr := TSouffleArray.Create(4);

    if SouffleIsStringValue(AValue) then
    begin
      StrVal := SouffleGetString(AValue);
      for I := 1 to Length(StrVal) do
        Arr.Push(SouffleString(StrVal[I]));
      Exit(SouffleReference(Arr));
    end;

    IterVal := GetIterator(AValue);
    repeat
      ElemVal := IteratorNext(IterVal, Done);
      if not Done then
        Arr.Push(ElemVal);
    until Done;
    Result := SouffleReference(Arr);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.RequireObjectCoercible(
  const AValue: TSouffleValue);
begin
  try
    if SouffleIsNil(AValue) and (AValue.Flags = GOCCIA_NIL_UNDEFINED) then
      ThrowTypeError('Cannot destructure undefined as it is not an object')
    else if SouffleIsNil(AValue) then
      ThrowTypeError('Cannot destructure null as it is not an object');
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function TGocciaRuntimeOperations.CoerceValueToString(
  const A: TSouffleValue): TSouffleValue;
begin
  try
    if SouffleIsStringValue(A) then
      Exit(A);
    Result := SouffleString(CoerceToString(A));
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
    on E: ESouffleThrow do
      raise;
  end;
end;

function TGocciaRuntimeOperations.SuperMethodGet(
  const ASuperBlueprint: TSouffleValue;
  const AMethodName: string): TSouffleValue;
var
  Bp: TSouffleBlueprint;
  Method: TSouffleValue;
  GocciaVal, PropVal: TGocciaValue;
  Helper: TGocciaSuperCallHelper;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(ASuperBlueprint) then Exit;
  if not Assigned(ASuperBlueprint.AsReference) then Exit;

  if ASuperBlueprint.AsReference is TSouffleBlueprint then
  begin
    Bp := TSouffleBlueprint(ASuperBlueprint.AsReference);
    while Assigned(Bp) do
    begin
      if Bp.Methods.Get(AMethodName, Method) then
        Exit(Method);
      if Bp.HasStaticFields and Bp.StaticFields.Get(AMethodName, Method) then
        Exit(Method);
      Bp := Bp.SuperBlueprint;
    end;
    Exit;
  end;

  GocciaVal := UnwrapToGocciaValue(ASuperBlueprint);
  if not Assigned(GocciaVal) then Exit;

  if (AMethodName = PROP_CONSTRUCTOR) and (GocciaVal is TGocciaClassValue) then
  begin
    Helper := TGocciaSuperCallHelper.Create(
      TGocciaClassValue(GocciaVal), Self);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Helper);
    Result := SouffleReference(Helper);
  end
  else
  begin
    PropVal := GocciaVal.GetProperty(AMethodName);
    if Assigned(PropVal) then
      Result := ToSouffleValue(PropVal);
  end;
end;

function CreateBridgedContext(
  const ARuntime: TGocciaRuntimeOperations): TGocciaEvaluationContext;
var
  BridgeScope: TGocciaScope;
  GocciaVal: TGocciaValue;
  GlobalPair: TOrderedStringMap<TSouffleValue>.TKeyValuePair;
begin
  Result := TGocciaEngine(ARuntime.Engine).Interpreter.CreateEvaluationContext;
  if ARuntime.FGlobals.Count = 0 then
    Exit;

  BridgeScope := Result.Scope.CreateChild(skBlock);
  for GlobalPair in ARuntime.FGlobals do
    if not BridgeScope.Contains(GlobalPair.Key) then
    begin
      GocciaVal := ARuntime.UnwrapToGocciaValue(GlobalPair.Value);
      BridgeScope.DefineLexicalBinding(GlobalPair.Key, GocciaVal, dtLet);
    end;
  Result.Scope := BridgeScope;
end;

procedure SyncArraysBack(const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope);
var
  GlobalPair: TOrderedStringMap<TSouffleValue>.TKeyValuePair;
  GocciaVal: TGocciaValue;
  SArr: TSouffleArray;
  GArr: TGocciaArrayValue;
  I: Integer;
begin
  for GlobalPair in ARuntime.FGlobals do
  begin
    if not SouffleIsReference(GlobalPair.Value) then
      Continue;
    if not (GlobalPair.Value.AsReference is TSouffleArray) then
      Continue;
    if not AScope.Contains(GlobalPair.Key) then
      Continue;
    GocciaVal := AScope.GetValue(GlobalPair.Key);
    if not (GocciaVal is TGocciaArrayValue) then
      Continue;
    SArr := TSouffleArray(GlobalPair.Value.AsReference);
    GArr := TGocciaArrayValue(GocciaVal);
    SArr.Clear;
    for I := 0 to GArr.Elements.Count - 1 do
      SArr.Push(ARuntime.ToSouffleValue(GArr.Elements[I]));
  end;
end;

procedure RebuildArrayBridgeCache(
  const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope);
var
  GlobalPair: TOrderedStringMap<TSouffleValue>.TKeyValuePair;
  GocciaVal: TGocciaValue;
begin
  for GlobalPair in ARuntime.FGlobals do
  begin
    if not SouffleIsReference(GlobalPair.Value) then
      Continue;
    if not (GlobalPair.Value.AsReference is TSouffleArray) then
      Continue;
    if not AScope.Contains(GlobalPair.Key) then
      Continue;
    GocciaVal := AScope.GetValue(GlobalPair.Key);
    if not (GocciaVal is TGocciaArrayValue) then
      Continue;
    ARuntime.FArrayBridgeCache.AddOrSetValue(
      GlobalPair.Value.AsReference, GocciaVal);
    ARuntime.FArrayBridgeDirty := True;
  end;
end;

procedure SyncCachedGocciaToSouffle(
  const ARuntime: TGocciaRuntimeOperations);
var
  CachePair: THashMap<TObject, TObject>.TKeyValuePair;
  SArr: TSouffleArray;
  GArr: TGocciaArrayValue;
  J: Integer;
begin
  if not ARuntime.FArrayBridgeDirty then
    Exit;
  for CachePair in ARuntime.FArrayBridgeCache do
  begin
    SArr := TSouffleArray(CachePair.Key);
    GArr := TGocciaArrayValue(CachePair.Value);
    SArr.Clear;
    for J := 0 to GArr.Elements.Count - 1 do
      SArr.Push(ARuntime.ToSouffleValue(GArr.Elements[J]));
  end;
  ARuntime.FArrayBridgeDirty := False;
end;

procedure SyncScopeToGlobals(const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope);
var
  GlobalPair: TOrderedStringMap<TSouffleValue>.TKeyValuePair;
  GocciaVal: TGocciaValue;
begin
  for GlobalPair in ARuntime.FGlobals do
  begin
    if not AScope.Contains(GlobalPair.Key) then
      Continue;
    GocciaVal := AScope.GetValue(GlobalPair.Key);
    ARuntime.FGlobals.AddOrSetValue(
      GlobalPair.Key, ARuntime.ToSouffleValue(GocciaVal));
  end;
end;

function TGocciaRuntimeOperations.FinalizeEnum(const ARecord: TSouffleValue;
  const AName: string): TSouffleValue;
var
  Rec: TSouffleRecord;
  EnumObj: TGocciaEnumValue;
  Entries: TGocciaArrayValue;
  I: Integer;
  Key: string;
  Val: TSouffleValue;
  GocciaVal: TGocciaValue;
  PairArr: TGocciaArrayValue;
begin
  try
    if not SouffleIsReference(ARecord) or
       not (ARecord.AsReference is TSouffleRecord) then
      Exit(ARecord);

    Rec := TSouffleRecord(ARecord.AsReference);
    EnumObj := TGocciaEnumValue.Create(AName);
    Entries := TGocciaArrayValue.Create;
    EnumObj.Entries := Entries;

    for I := 0 to Rec.Count - 1 do
    begin
      Key := Rec.GetOrderedKey(I);
      Val := Rec.GetOrderedValue(I);
      GocciaVal := UnwrapToGocciaValue(Val);

      if not (GocciaVal is TGocciaNumberLiteralValue) and
         not (GocciaVal is TGocciaStringLiteralValue) and
         not (GocciaVal is TGocciaSymbolValue) then
        ThrowTypeError('Enum member ''' + Key +
          ''' must be a number, string, or symbol');

      EnumObj.DefineProperty(Key,
        TGocciaPropertyDescriptorData.Create(GocciaVal, [pfEnumerable]));

      PairArr := TGocciaArrayValue.Create;
      PairArr.Elements.Add(TGocciaStringLiteralValue.Create(Key));
      PairArr.Elements.Add(GocciaVal);
      Entries.Elements.Add(PairArr);
    end;

    EnumObj.PreventExtensions;
    InitializeEnumSymbols(EnumObj);

    Result := WrapGocciaValue(EnumObj);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

{ TGocciaDecoratorSession }

constructor TGocciaDecoratorSession.Create(const AMetadataObject: TGocciaObjectValue);
begin
  inherited Create;
  MetadataObject := AMetadataObject;
  MethodCollector := TGocciaInitializerCollector.Create;
  FieldCollector := TGocciaInitializerCollector.Create;
  StaticFieldCollector := TGocciaInitializerCollector.Create;
  ClassCollector := TGocciaInitializerCollector.Create;
  ClassValue := nil;
end;

destructor TGocciaDecoratorSession.Destroy;
begin
  MethodCollector.Free;
  FieldCollector.Free;
  StaticFieldCollector.Free;
  ClassCollector.Free;
  inherited;
end;

procedure ParseElementDescriptor(const ADescriptor: string;
  out AKind: Char; out AName: string; out AFlags: Integer);
var
  P1, P2: Integer;
begin
  P1 := Pos(':', ADescriptor);
  AKind := ADescriptor[1];
  P2 := Pos(':', ADescriptor, P1 + 1);
  AName := Copy(ADescriptor, P1 + 1, P2 - P1 - 1);
  AFlags := StrToIntDef(Copy(ADescriptor, P2 + 1, Length(ADescriptor) - P2), 0);
end;

procedure TGocciaRuntimeOperations.SetupAutoAccessor(
  const ABlueprint: TSouffleValue; const AName: string;
  const AInitClosure: TSouffleValue);
var
  ClassVal: TGocciaClassValue;
  BackingName: string;
begin
  try
    if not Assigned(FActiveDecoratorSession) then
      Exit;
    if not (FActiveDecoratorSession.ClassValue is TGocciaClassValue) then
      Exit;
    ClassVal := TGocciaClassValue(FActiveDecoratorSession.ClassValue);
    BackingName := '__accessor_' + AName;
    ClassVal.AddAutoAccessor(AName, BackingName, False);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

function ResolveSymbolFromKey(const AKey: string): TGocciaSymbolValue;
var
  SymIdStr: string;
  SymId: Integer;
begin
  Result := nil;
  if (Length(AKey) > 6) and (Copy(AKey, 1, 6) = '@@sym:') then
  begin
    SymIdStr := Copy(AKey, 7, Length(AKey) - 6);
    if TryStrToInt(SymIdStr, SymId) then
      Result := TGocciaSymbolValue.ById(SymId);
  end;
end;

function ConvertBlueprintToClassValue(const ABp: TSouffleBlueprint;
  const ARuntime: TGocciaRuntimeOperations): TGocciaClassValue;
var
  SuperClass: TGocciaClassValue;
  I: Integer;
  Key: string;
  Val, WrappedSuperVal: TSouffleValue;
  GocciaSuperVal: TGocciaValue;
  MethodBridge: TGocciaSouffleMethodBridge;
  Sym: TGocciaSymbolValue;
begin
  SuperClass := nil;
  if Assigned(ABp.SuperBlueprint) then
    SuperClass := ConvertBlueprintToClassValue(ABp.SuperBlueprint, ARuntime)
  else if ARuntime.FBlueprintSuperValues.TryGetValue(ABp, WrappedSuperVal) then
  begin
    GocciaSuperVal := ARuntime.UnwrapToGocciaValue(WrappedSuperVal);
    if GocciaSuperVal is TGocciaClassValue then
      SuperClass := TGocciaClassValue(GocciaSuperVal);
  end;

  Result := TGocciaClassValue.Create(ABp.Name, SuperClass);
  Result.Prototype.AssignProperty(PROP_CONSTRUCTOR, Result);

  for I := 0 to ABp.Methods.Count - 1 do
  begin
    Key := ABp.Methods.GetOrderedKey(I);
    Val := ABp.Methods.GetOrderedValue(I);
    if Key = '__fields__' then Continue;
    if not SouffleIsReference(Val) then Continue;
    if not (Val.AsReference is TSouffleClosure) then Continue;
    MethodBridge := TGocciaSouffleMethodBridge.Create(
      TSouffleClosure(Val.AsReference), ARuntime);
    MethodBridge.OwningClass := Result;
    if (Length(Key) > 1) and (Key[1] = '#') then
    begin
      Result.AddPrivateMethod(Copy(Key, 2, Length(Key) - 1), MethodBridge);
      Result.Prototype.AssignProperty(Key, MethodBridge);
    end
    else
      Result.AddMethod(Key, MethodBridge);
  end;

  if ABp.HasGetters then
    for I := 0 to ABp.Getters.Count - 1 do
    begin
      Key := ABp.Getters.GetOrderedKey(I);
      Val := ABp.Getters.GetOrderedValue(I);
      if SouffleIsReference(Val) and (Val.AsReference is TSouffleClosure) then
      begin
        Sym := ResolveSymbolFromKey(Key);
        if Assigned(Sym) then
          Result.Prototype.DefineSymbolProperty(Sym,
            TGocciaPropertyDescriptorAccessor.Create(
              TGocciaSouffleMethodBridge.Create(
                TSouffleClosure(Val.AsReference), ARuntime),
              nil, [pfConfigurable]))
        else
          Result.AddGetter(Key, TGocciaSouffleMethodBridge.Create(
            TSouffleClosure(Val.AsReference), ARuntime));
      end;
    end;

  if ABp.HasSetters then
    for I := 0 to ABp.Setters.Count - 1 do
    begin
      Key := ABp.Setters.GetOrderedKey(I);
      Val := ABp.Setters.GetOrderedValue(I);
      if SouffleIsReference(Val) and (Val.AsReference is TSouffleClosure) then
      begin
        Sym := ResolveSymbolFromKey(Key);
        if Assigned(Sym) then
          Result.Prototype.DefineSymbolProperty(Sym,
            TGocciaPropertyDescriptorAccessor.Create(nil,
              TGocciaSouffleMethodBridge.Create(
                TSouffleClosure(Val.AsReference), ARuntime),
              [pfConfigurable]))
        else
          Result.AddSetter(Key, TGocciaSouffleMethodBridge.Create(
            TSouffleClosure(Val.AsReference), ARuntime));
      end;
    end;

  if ABp.HasStaticGetters then
    for I := 0 to ABp.StaticGetters.Count - 1 do
    begin
      Key := ABp.StaticGetters.GetOrderedKey(I);
      Val := ABp.StaticGetters.GetOrderedValue(I);
      if SouffleIsReference(Val) and (Val.AsReference is TSouffleClosure) then
      begin
        Sym := ResolveSymbolFromKey(Key);
        if Assigned(Sym) then
          Result.DefineSymbolProperty(Sym,
            TGocciaPropertyDescriptorAccessor.Create(
              TGocciaSouffleMethodBridge.Create(
                TSouffleClosure(Val.AsReference), ARuntime),
              nil, [pfConfigurable]))
        else
          Result.AddStaticGetter(Key, TGocciaSouffleMethodBridge.Create(
            TSouffleClosure(Val.AsReference), ARuntime));
      end;
    end;

  if ABp.HasStaticSetters then
    for I := 0 to ABp.StaticSetters.Count - 1 do
    begin
      Key := ABp.StaticSetters.GetOrderedKey(I);
      Val := ABp.StaticSetters.GetOrderedValue(I);
      if SouffleIsReference(Val) and (Val.AsReference is TSouffleClosure) then
      begin
        Sym := ResolveSymbolFromKey(Key);
        if Assigned(Sym) then
          Result.DefineSymbolProperty(Sym,
            TGocciaPropertyDescriptorAccessor.Create(nil,
              TGocciaSouffleMethodBridge.Create(
                TSouffleClosure(Val.AsReference), ARuntime),
              [pfConfigurable]))
        else
          Result.AddStaticSetter(Key, TGocciaSouffleMethodBridge.Create(
            TSouffleClosure(Val.AsReference), ARuntime));
      end;
    end;

  if ABp.HasStaticFields then
    for I := 0 to ABp.StaticFields.Count - 1 do
    begin
      Key := ABp.StaticFields.GetOrderedKey(I);
      Val := ABp.StaticFields.GetOrderedValue(I);
      Result.SetProperty(Key, ARuntime.UnwrapToGocciaValue(Val));
    end;

  if ABp.Methods.Get('__fields__', Val) and
     SouffleIsReference(Val) and (Val.AsReference is TSouffleClosure) then
    Result.SetMethodInitializers([TGocciaSouffleMethodBridge.Create(
      TSouffleClosure(Val.AsReference), ARuntime)]);
end;

procedure TGocciaRuntimeOperations.BeginDecorators(
  const ABlueprint, ASuper: TSouffleValue);
var
  Bp: TSouffleBlueprint;
  ClassVal: TGocciaClassValue;
  SuperClassVal: TGocciaClassValue;
  SuperMetadata: TGocciaValue;
  Meta: TGocciaObjectValue;
begin
  try
    if not (SouffleIsReference(ABlueprint) and
            Assigned(ABlueprint.AsReference) and
            (ABlueprint.AsReference is TSouffleBlueprint)) then
      Exit;

    Bp := TSouffleBlueprint(ABlueprint.AsReference);
    ClassVal := ConvertBlueprintToClassValue(Bp, Self);

    if not Assigned(ClassVal.SuperClass) and not SouffleIsNil(ASuper) then
    begin
      SuperClassVal := nil;
      if SouffleIsReference(ASuper) and Assigned(ASuper.AsReference) then
      begin
        if ASuper.AsReference is TSouffleBlueprint then
          SuperClassVal := ConvertBlueprintToClassValue(
            TSouffleBlueprint(ASuper.AsReference), Self)
        else
        begin
          SuperMetadata := UnwrapToGocciaValue(ASuper);
          if SuperMetadata is TGocciaClassValue then
            SuperClassVal := TGocciaClassValue(SuperMetadata);
        end;
      end;
      if Assigned(SuperClassVal) then
        ClassVal.SuperClass := SuperClassVal;
    end;

    SuperMetadata := nil;
    if Assigned(ClassVal.SuperClass) then
      SuperMetadata := ClassVal.SuperClass.GetSymbolProperty(
        TGocciaSymbolValue.WellKnownMetadata);

    if (SuperMetadata <> nil) and (SuperMetadata is TGocciaObjectValue) then
      Meta := TGocciaObjectValue.Create(TGocciaObjectValue(SuperMetadata))
    else
      Meta := TGocciaObjectValue.Create;
    TGarbageCollector.Instance.AddTempRoot(Meta);

    FreeAndNil(FActiveDecoratorSession);
    FActiveDecoratorSession := TGocciaDecoratorSession.Create(Meta);
    FActiveDecoratorSession.ClassValue := ClassVal;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.ApplyElementDecorator(
  const ADecoratorFn: TSouffleValue; const ADescriptor: string);
var
  Kind: Char;
  Name: string;
  Flags: Integer;
  IsStatic, IsPrivate: Boolean;
  ClassVal: TGocciaClassValue;
  DecoratorFn, ElementValue, DecoratorResult: TGocciaValue;
  ContextObject, AccessObject, AutoAccessorValue, DecResultObj: TGocciaObjectValue;
  AccessGetterHelper: TGocciaAccessGetter;
  AccessSetterHelper: TGocciaAccessSetter;
  Collector: TGocciaInitializerCollector;
  DecoratorArgs: TGocciaArgumentsCollection;
  KindStr: string;
  NewGetter, NewSetter, NewInit: TGocciaValue;
begin
  try
    if not Assigned(FActiveDecoratorSession) then
      Exit;
    ClassVal := nil;
    if FActiveDecoratorSession.ClassValue is TGocciaClassValue then
      ClassVal := TGocciaClassValue(FActiveDecoratorSession.ClassValue);
    if not Assigned(ClassVal) then
      Exit;

    DecoratorFn := UnwrapToGocciaValue(ADecoratorFn);
    if not DecoratorFn.IsCallable then
      ThrowTypeError('Decorator must be a function');

    ParseElementDescriptor(ADescriptor, Kind, Name, Flags);
    IsStatic := (Flags and 1) <> 0;
    IsPrivate := (Flags and 2) <> 0;

    case Kind of
      'm': KindStr := 'method';
      'g': KindStr := 'getter';
      's': KindStr := 'setter';
      'f': KindStr := 'field';
      'a': KindStr := 'accessor';
    else
      KindStr := 'method';
    end;

    ContextObject := TGocciaObjectValue.Create;
    ContextObject.AssignProperty(PROP_KIND,
      TGocciaStringLiteralValue.Create(KindStr));
    if IsPrivate then
      ContextObject.AssignProperty(PROP_NAME,
        TGocciaStringLiteralValue.Create('#' + Name))
    else
      ContextObject.AssignProperty(PROP_NAME,
        TGocciaStringLiteralValue.Create(Name));
    if IsStatic then
      ContextObject.AssignProperty(PROP_STATIC,
        TGocciaBooleanLiteralValue.TrueValue)
    else
      ContextObject.AssignProperty(PROP_STATIC,
        TGocciaBooleanLiteralValue.FalseValue);
    if IsPrivate then
      ContextObject.AssignProperty(PROP_PRIVATE,
        TGocciaBooleanLiteralValue.TrueValue)
    else
      ContextObject.AssignProperty(PROP_PRIVATE,
        TGocciaBooleanLiteralValue.FalseValue);
    ContextObject.AssignProperty(PROP_METADATA,
      FActiveDecoratorSession.MetadataObject);

    AccessObject := TGocciaObjectValue.Create;
    case Kind of
      'm':
      begin
        if IsPrivate then
        begin
          ElementValue := ClassVal.GetPrivateMethod(Name);
          if not Assigned(ElementValue) then
            ElementValue := ClassVal.Prototype.GetProperty('#' + Name);
        end
        else if IsStatic then
          ElementValue := ClassVal.GetProperty(Name)
        else
          ElementValue := ClassVal.Prototype.GetProperty(Name);
        AccessGetterHelper := TGocciaAccessGetter.Create(ElementValue, Name);
        AccessObject.AssignProperty(PROP_GET,
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            AccessGetterHelper.Get, PROP_GET, 0));
        ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
      end;
      'f', 'a':
      begin
        AccessGetterHelper := TGocciaAccessGetter.Create(nil, Name);
        AccessSetterHelper := TGocciaAccessSetter.Create(Name);
        AccessObject.AssignProperty(PROP_GET,
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            AccessGetterHelper.Get, PROP_GET, 0));
        AccessObject.AssignProperty(PROP_SET,
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            AccessSetterHelper.SetValue, PROP_SET, 1));
        ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
      end;
    end;

    if IsStatic then
      Collector := FActiveDecoratorSession.StaticFieldCollector
    else if Kind in ['f', 'a'] then
      Collector := FActiveDecoratorSession.FieldCollector
    else
      Collector := FActiveDecoratorSession.MethodCollector;
    ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        Collector.AddInitializer, PROP_ADD_INITIALIZER, 1));

    case Kind of
      'm':
        ; // ElementValue already set above
      'g':
      begin
        if IsPrivate then
          ElementValue := ClassVal.PrivatePropertyGetter[Name]
        else if IsStatic then
          ElementValue := ClassVal.StaticPropertyGetter[Name]
        else
          ElementValue := ClassVal.PropertyGetter[Name];
      end;
      's':
      begin
        if IsPrivate then
          ElementValue := ClassVal.PrivatePropertySetter[Name]
        else if IsStatic then
          ElementValue := ClassVal.StaticPropertySetter[Name]
        else
          ElementValue := ClassVal.PropertySetter[Name];
      end;
      'f':
        ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      'a':
      begin
        AutoAccessorValue := TGocciaObjectValue.Create;
        AutoAccessorValue.AssignProperty(PROP_GET,
          ClassVal.PropertyGetter[Name]);
        AutoAccessorValue.AssignProperty(PROP_SET,
          ClassVal.PropertySetter[Name]);
        ElementValue := AutoAccessorValue;
      end;
    end;

    DecoratorArgs := TGocciaArgumentsCollection.Create([ElementValue, ContextObject]);
    try
      DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(
        DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      DecoratorArgs.Free;
    end;

    if (DecoratorResult = nil) or (DecoratorResult is TGocciaUndefinedLiteralValue) then
      Exit;

    case Kind of
      'm':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Method decorator must return a function or undefined');
        if IsPrivate then
        begin
          if DecoratorResult is TGocciaMethodValue then
            ClassVal.AddPrivateMethod(Name, TGocciaMethodValue(DecoratorResult));
          ClassVal.Prototype.AssignProperty('#' + Name, DecoratorResult);
        end
        else if IsStatic then
          ClassVal.SetProperty(Name, DecoratorResult)
        else
          ClassVal.Prototype.AssignProperty(Name, DecoratorResult);
      end;
      'g':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Getter decorator must return a function or undefined');
        if IsPrivate then
          ClassVal.AddPrivateGetter(Name, TGocciaFunctionValue(DecoratorResult))
        else if IsStatic then
          ClassVal.AddStaticGetter(Name, TGocciaFunctionValue(DecoratorResult))
        else
          ClassVal.AddGetter(Name, TGocciaFunctionValue(DecoratorResult));
      end;
      's':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Setter decorator must return a function or undefined');
        if IsPrivate then
          ClassVal.AddPrivateSetter(Name, TGocciaFunctionValue(DecoratorResult))
        else if IsStatic then
          ClassVal.AddStaticSetter(Name, TGocciaFunctionValue(DecoratorResult))
        else
          ClassVal.AddSetter(Name, TGocciaFunctionValue(DecoratorResult));
      end;
      'f':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Field decorator must return a function or undefined');
        ClassVal.AddFieldInitializer(Name, DecoratorResult, IsPrivate, IsStatic);
      end;
      'a':
      begin
        if not (DecoratorResult is TGocciaObjectValue) then
          ThrowTypeError('Accessor decorator must return an object or undefined');
        DecResultObj := TGocciaObjectValue(DecoratorResult);
        NewGetter := DecResultObj.GetProperty(PROP_GET);
        NewSetter := DecResultObj.GetProperty(PROP_SET);
        NewInit := DecResultObj.GetProperty(PROP_INIT);
        if Assigned(NewGetter) and not (NewGetter is TGocciaUndefinedLiteralValue) then
          ClassVal.AddGetter(Name, TGocciaFunctionValue(NewGetter));
        if Assigned(NewSetter) and not (NewSetter is TGocciaUndefinedLiteralValue) then
          ClassVal.AddSetter(Name, TGocciaFunctionValue(NewSetter));
        if Assigned(NewInit) and not (NewInit is TGocciaUndefinedLiteralValue) and
           NewInit.IsCallable then
          ClassVal.AddFieldInitializer(Name, NewInit, IsPrivate, IsStatic);
      end;
    end;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.ApplyClassDecorator(
  const ADecoratorFn: TSouffleValue);
var
  ClassVal: TGocciaClassValue;
  DecoratorFn, DecoratorResult: TGocciaValue;
  ContextObject: TGocciaObjectValue;
  DecoratorArgs: TGocciaArgumentsCollection;
begin
  try
    if not Assigned(FActiveDecoratorSession) then
      Exit;
    ClassVal := nil;
    if FActiveDecoratorSession.ClassValue is TGocciaClassValue then
      ClassVal := TGocciaClassValue(FActiveDecoratorSession.ClassValue);
    if not Assigned(ClassVal) then
      Exit;

    DecoratorFn := UnwrapToGocciaValue(ADecoratorFn);
    if not DecoratorFn.IsCallable then
      ThrowTypeError('Decorator must be a function');

    ContextObject := TGocciaObjectValue.Create;
    ContextObject.AssignProperty(PROP_KIND,
      TGocciaStringLiteralValue.Create('class'));
    ContextObject.AssignProperty(PROP_NAME,
      TGocciaStringLiteralValue.Create(ClassVal.Name));
    ContextObject.AssignProperty(PROP_METADATA,
      FActiveDecoratorSession.MetadataObject);
    ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        FActiveDecoratorSession.ClassCollector.AddInitializer,
        PROP_ADD_INITIALIZER, 1));

    DecoratorArgs := TGocciaArgumentsCollection.Create([ClassVal, ContextObject]);
    try
      DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(
        DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      DecoratorArgs.Free;
    end;

    if (DecoratorResult <> nil) and
       not (DecoratorResult is TGocciaUndefinedLiteralValue) then
    begin
      if not (DecoratorResult is TGocciaClassValue) then
        ThrowTypeError('Class decorator must return a class or undefined');
      FActiveDecoratorSession.ClassValue := DecoratorResult;
    end;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;
end;

procedure TGocciaRuntimeOperations.FinishDecorators(
  var ADest: TSouffleValue);
var
  ClassVal: TGocciaClassValue;
  InitializerResults: TArray<TGocciaValue>;
  I: Integer;
  InitArgs: TGocciaArgumentsCollection;
begin
  try
    if not Assigned(FActiveDecoratorSession) then
      Exit;

    ClassVal := nil;
    if FActiveDecoratorSession.ClassValue is TGocciaClassValue then
      ClassVal := TGocciaClassValue(FActiveDecoratorSession.ClassValue);
    if not Assigned(ClassVal) then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(
        FActiveDecoratorSession.MetadataObject);
      FreeAndNil(FActiveDecoratorSession);
      Exit;
    end;

    FClassDefinitionScopes.AddOrSetValue(ClassVal, nil);

    ClassVal.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownMetadata,
      TGocciaPropertyDescriptorData.Create(
        FActiveDecoratorSession.MetadataObject, [pfConfigurable]));

    InitializerResults := FActiveDecoratorSession.MethodCollector.GetInitializers;
    ClassVal.AppendMethodInitializers(InitializerResults);
    InitializerResults := FActiveDecoratorSession.FieldCollector.GetInitializers;
    ClassVal.AppendFieldInitializers(InitializerResults);

    InitializerResults := FActiveDecoratorSession.ClassCollector.GetInitializers;
    for I := 0 to High(InitializerResults) do
    begin
      InitArgs := TGocciaArgumentsCollection.Create;
      try
        TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassVal);
      finally
        InitArgs.Free;
      end;
    end;

    InitializerResults := FActiveDecoratorSession.StaticFieldCollector.GetInitializers;
    for I := 0 to High(InitializerResults) do
    begin
      InitArgs := TGocciaArgumentsCollection.Create;
      try
        TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassVal);
      finally
        InitArgs.Free;
      end;
    end;

    ADest := WrapGocciaValue(ClassVal);

    TGarbageCollector.Instance.RemoveTempRoot(
      FActiveDecoratorSession.MetadataObject);
    FreeAndNil(FActiveDecoratorSession);
  except
    on E: TGocciaThrowValue do
    begin
      if Assigned(FActiveDecoratorSession) then
      begin
        TGarbageCollector.Instance.RemoveTempRoot(
          FActiveDecoratorSession.MetadataObject);
        FreeAndNil(FActiveDecoratorSession);
      end;
      RethrowAsVM(E);
    end;
  end;
end;

procedure TGocciaRuntimeOperations.ExtendedOperation(const ASubOp: UInt8;
  var ADest: TSouffleValue; const AOperand, AExtra: TSouffleValue;
  const ATemplate: TSouffleFunctionTemplate;
  const AOperandIndex: UInt8);
begin
  case ASubOp of
    1: // GOCCIA_EXT_SPREAD_OBJ
      SpreadObjectInto(ADest, AOperand);
    2: // GOCCIA_EXT_OBJ_REST
      ADest := ObjectRest(AOperand, AExtra);
    3: // GOCCIA_EXT_FINALIZE_ENUM
      ADest := FinalizeEnum(ADest, ATemplate.GetConstant(AOperandIndex).StringValue);
    4: // GOCCIA_EXT_DEF_GETTER
      DefineGetter(ADest, ATemplate.GetConstant(AOperandIndex).StringValue, AExtra);
    5: // GOCCIA_EXT_DEF_SETTER
      DefineSetter(ADest, ATemplate.GetConstant(AOperandIndex).StringValue, AExtra);
    6: // GOCCIA_EXT_DEF_STATIC_GETTER
      DefineStaticGetter(ADest, ATemplate.GetConstant(AOperandIndex).StringValue, AExtra);
    7: // GOCCIA_EXT_DEF_STATIC_SETTER
      DefineStaticSetter(ADest, ATemplate.GetConstant(AOperandIndex).StringValue, AExtra);
    8: // GOCCIA_EXT_REQUIRE_OBJECT
      RequireObjectCoercible(ADest);
    10: // GOCCIA_EXT_THROW_TYPE_ERROR
      ThrowTypeErrorMessage(ATemplate.GetConstant(AOperandIndex).StringValue);
    11: // GOCCIA_EXT_SUPER_GET
      ADest := SuperMethodGet(AExtra, ATemplate.GetConstant(AOperandIndex).StringValue);
    12: // GOCCIA_EXT_SPREAD
      SpreadInto(ADest, AOperand);
    13: // GOCCIA_EXT_REQUIRE_ITERABLE
      ADest := RequireIterable(ADest);
    14: // GOCCIA_EXT_DEFINE_GLOBAL
      FGlobals.AddOrSetValue(
        ATemplate.GetConstant(AOperandIndex).StringValue, ADest);
    15: // GOCCIA_EXT_SETUP_AUTO_ACCESSOR
      SetupAutoAccessor(ADest,
        ATemplate.GetConstant(AOperandIndex).StringValue, AExtra);
    16: // GOCCIA_EXT_BEGIN_DECORATORS
      BeginDecorators(ADest, AExtra);
    17: // GOCCIA_EXT_APPLY_ELEMENT_DECORATOR
      ApplyElementDecorator(ADest,
        ATemplate.GetConstant(AOperandIndex).StringValue);
    18: // GOCCIA_EXT_APPLY_CLASS_DECORATOR
      ApplyClassDecorator(ADest);
    19: // GOCCIA_EXT_FINISH_DECORATORS
      FinishDecorators(ADest);
    20: // GOCCIA_EXT_DEF_COMPUTED_GETTER
      DefineComputedGetter(ADest, AOperand, AExtra);
    21: // GOCCIA_EXT_DEF_COMPUTED_SETTER
      DefineComputedSetter(ADest, AOperand, AExtra);
    22: // GOCCIA_EXT_DEF_COMPUTED_STATIC_GETTER
      DefineComputedStaticGetter(ADest, AOperand, AExtra);
    23: // GOCCIA_EXT_DEF_COMPUTED_STATIC_SETTER
      DefineComputedStaticSetter(ADest, AOperand, AExtra);
    24: // GOCCIA_EXT_SET_WRAPPED_SUPER
    begin
      if SouffleIsReference(ADest) and
         (ADest.AsReference is TSouffleBlueprint) and
         not Assigned(TSouffleBlueprint(ADest.AsReference).SuperBlueprint) and
         SouffleIsReference(AOperand) and Assigned(AOperand.AsReference) then
        FBlueprintSuperValues.AddOrSetValue(ADest.AsReference, AOperand);
    end;
  end;
end;

procedure TGocciaRuntimeOperations.MarkExternalRoots;
var
  GlobalPair: TOrderedStringMap<TSouffleValue>.TKeyValuePair;
  ClosurePair: THashMap<TSouffleClosure, TObject>.TKeyValuePair;
  CachePair: THashMap<TObject, TObject>.TKeyValuePair;
  SuperPair: THashMap<TObject, TSouffleValue>.TKeyValuePair;
begin
  for GlobalPair in FGlobals do
    if SouffleIsReference(GlobalPair.Value) and Assigned(GlobalPair.Value.AsReference)
      and not GlobalPair.Value.AsReference.GCMarked then
      GlobalPair.Value.AsReference.MarkReferences;

  for GlobalPair in FExports do
    if SouffleIsReference(GlobalPair.Value) and Assigned(GlobalPair.Value.AsReference)
      and not GlobalPair.Value.AsReference.GCMarked then
      GlobalPair.Value.AsReference.MarkReferences;

  for ClosurePair in FClosureBridgeCache do
    if not ClosurePair.Key.GCMarked then
      ClosurePair.Key.MarkReferences;

  for CachePair in FArrayBridgeCache do
    if (CachePair.Key is TSouffleHeapObject) and not TSouffleHeapObject(CachePair.Key).GCMarked then
      TSouffleHeapObject(CachePair.Key).MarkReferences;

  for CachePair in FArrayBridgeReverse do
    if (CachePair.Value is TSouffleHeapObject) and not TSouffleHeapObject(CachePair.Value).GCMarked then
      TSouffleHeapObject(CachePair.Value).MarkReferences;

  for CachePair in FRecordBridgeCache do
    if (CachePair.Key is TSouffleHeapObject) and not TSouffleHeapObject(CachePair.Key).GCMarked then
      TSouffleHeapObject(CachePair.Key).MarkReferences;

  for CachePair in FBlueprintBridgeCache do
    if (CachePair.Key is TSouffleHeapObject) and not TSouffleHeapObject(CachePair.Key).GCMarked then
      TSouffleHeapObject(CachePair.Key).MarkReferences;

  for SuperPair in FBlueprintSuperValues do
    if SouffleIsReference(SuperPair.Value) and Assigned(SuperPair.Value.AsReference)
      and not SuperPair.Value.AsReference.GCMarked then
      SuperPair.Value.AsReference.MarkReferences;

  if Assigned(FStringDelegate) and not FStringDelegate.GCMarked then
    FStringDelegate.MarkReferences;
  if Assigned(FNumberDelegate) and not FNumberDelegate.GCMarked then
    FNumberDelegate.MarkReferences;
  if Assigned(FMapDelegate) and not FMapDelegate.GCMarked then
    FMapDelegate.MarkReferences;
  if Assigned(FSetDelegate) and not FSetDelegate.GCMarked then
    FSetDelegate.MarkReferences;
  if Assigned(FPromiseDelegate) and not FPromiseDelegate.GCMarked then
    FPromiseDelegate.MarkReferences;
  if Assigned(FPromiseStaticDelegate) and not FPromiseStaticDelegate.GCMarked then
    FPromiseStaticDelegate.MarkReferences;
  if Assigned(FDescribeDelegate) and not FDescribeDelegate.GCMarked then
    FDescribeDelegate.MarkReferences;
  if Assigned(FTestDelegate) and not FTestDelegate.GCMarked then
    FTestDelegate.MarkReferences;
end;

procedure TGocciaRuntimeOperations.MarkWrappedGocciaValues;
var
  I: Integer;
  Wrapped: TGocciaWrappedValue;
  ClosurePair: THashMap<TSouffleClosure, TObject>.TKeyValuePair;
  CachePair: THashMap<TObject, TObject>.TKeyValuePair;
begin
  for I := 0 to FWrappedValues.Count - 1 do
  begin
    Wrapped := TGocciaWrappedValue(FWrappedValues[I]);
    if Assigned(Wrapped.Value) and not Wrapped.Value.GCMarked then
      Wrapped.Value.MarkReferences;
  end;

  for ClosurePair in FClosureBridgeCache do
    if (ClosurePair.Value is TGocciaValue) and not TGocciaValue(ClosurePair.Value).GCMarked then
      TGocciaValue(ClosurePair.Value).MarkReferences;

  for CachePair in FArrayBridgeCache do
    if (CachePair.Value is TGocciaValue) and not TGocciaValue(CachePair.Value).GCMarked then
      TGocciaValue(CachePair.Value).MarkReferences;

  for CachePair in FArrayBridgeReverse do
    if (CachePair.Key is TGocciaValue) and not TGocciaValue(CachePair.Key).GCMarked then
      TGocciaValue(CachePair.Key).MarkReferences;

  for CachePair in FRecordBridgeCache do
    if (CachePair.Value is TGocciaValue) and not TGocciaValue(CachePair.Value).GCMarked then
      TGocciaValue(CachePair.Value).MarkReferences;

  for CachePair in FBlueprintBridgeCache do
    if (CachePair.Value is TGocciaValue) and not TGocciaValue(CachePair.Value).GCMarked then
      TGocciaValue(CachePair.Value).MarkReferences;

  if Assigned(FActiveDecoratorSession) then
  begin
    if Assigned(FActiveDecoratorSession.MetadataObject) and
       not FActiveDecoratorSession.MetadataObject.GCMarked then
      FActiveDecoratorSession.MetadataObject.MarkReferences;
    if Assigned(FActiveDecoratorSession.ClassValue) and
       not FActiveDecoratorSession.ClassValue.GCMarked then
      FActiveDecoratorSession.ClassValue.MarkReferences;
  end;
end;

{$IFDEF BRIDGE_METRICS}
initialization
  GBridgeMetrics.Reset;

finalization
  TBridgeMetrics.DumpGlobal;
{$ENDIF}

end.
