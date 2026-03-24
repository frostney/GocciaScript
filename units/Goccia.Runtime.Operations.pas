unit Goccia.Runtime.Operations;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  HashMap,
  ModuleResolver,
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
  Goccia.Runtime.Collections,
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
    function ToStringTag: string; override;
    function GetOwnSymbolPropertyDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor; override;
    function NativeKind: string; override;
    function CloneNative: TGocciaObjectValue; override;

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
    FMapIteratorDelegate: TSouffleRecord;
    FSetIteratorDelegate: TSouffleRecord;
    FMapBlueprint: TSouffleBlueprint;
    FSetBlueprint: TSouffleBlueprint;
    FPromiseBlueprint: TSouffleBlueprint;
    FPromiseDelegate: TSouffleRecord;
    FArrayBufferBlueprint: TSouffleBlueprint;
    FSharedArrayBufferBlueprint: TSouffleBlueprint;
    FArrayBufferDelegate: TSouffleRecord;
    FSharedArrayBufferDelegate: TSouffleRecord;
    FTypedArrayDelegate: TSouffleRecord;
    FTypedArrayBlueprints: array[TSouffleTypedArrayKind] of TSouffleBlueprint;
    FArrayBlueprint: TSouffleBlueprint;
    FObjectBlueprint: TSouffleBlueprint;
    FStringBlueprint: TSouffleBlueprint;
    FNumberBlueprint: TSouffleBlueprint;
    FBooleanBlueprint: TSouffleBlueprint;
    FSymbolBlueprint: TSouffleBlueprint;
    FFunctionBlueprint: TSouffleBlueprint;
    FErrorBlueprint: TSouffleBlueprint;
    FTypeErrorBlueprint: TSouffleBlueprint;
    FRangeErrorBlueprint: TSouffleBlueprint;
    FReferenceErrorBlueprint: TSouffleBlueprint;
    FSyntaxErrorBlueprint: TSouffleBlueprint;
    FURIErrorBlueprint: TSouffleBlueprint;
    FAggregateErrorBlueprint: TSouffleBlueprint;
    FDOMExceptionBlueprint: TSouffleBlueprint;
    FErrorDelegate: TSouffleRecord;
    FDescribeDelegate: TSouffleRecord;
    FTestDelegate: TSouffleRecord;
    FActiveDecoratorSession: TGocciaDecoratorSession;
    FArrayBridgeDirty: Boolean;
    FModuleResolver: TModuleResolver;
    FModuleCache: TOrderedStringMap<TSouffleValue>;
    FCompiledModules: TList;
    function LoadModuleNative(const APath, AImportingPath: string): TSouffleValue;
    function LoadJsonModuleNative(const AResolvedPath: string): TSouffleValue;
    function WrapGocciaValue(const AValue: TGocciaValue): TSouffleValue;
    function CoerceKeyToString(const AKey: TSouffleValue): string;
    function CoerceToNumber(const A: TSouffleValue): Double;
    function CoerceToString(const A: TSouffleValue): string;
    procedure RethrowAsVM(const E: TGocciaThrowValue); overload;
    procedure RethrowAsVM(const AValue: TSouffleValue); overload;
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
    function CreateBuiltinBlueprint(const AName: string; const ASlotCount: Integer;
      const AMethodDelegate: TSouffleRecord; const AIteratorMethod: string): TSouffleBlueprint;
    function ConstructNativeMap(const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue;
    function ConstructNativeSet(const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue;
    function ConstructNativePromise(const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue;
    function WrapSoufflePromise(const APromise: TSoufflePromise): TSouffleValue;
    function ConstructNativeArrayBuffer(const AArgs: PSouffleValue;
      const AArgCount: Integer; const ABlueprint: TSouffleBlueprint): TSouffleValue;
    function ConstructNativeTypedArray(const AArgs: PSouffleValue;
      const AArgCount: Integer; const AKind: TSouffleTypedArrayKind): TSouffleValue;
    procedure InvokeSouffleMicrotask(const AHandler, AValue: TSouffleValue;
      out AResult: TSouffleValue; out AHadError: Boolean);
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
      const AKey: string); override;
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
    function ConvertBlueprintMapToGoccia(const ARec: TSouffleRecord): TGocciaValue;
    function ConvertBlueprintSetToGoccia(const ARec: TSouffleRecord): TGocciaValue;
    procedure PopulateMapFromIterable(const AMap: TSouffleHeapObject; const AIterable: TSouffleValue);
    procedure PopulateSetFromIterable(const ASet: TSouffleHeapObject; const AIterable: TSouffleValue);

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
    property ModuleResolver: TModuleResolver read FModuleResolver write FModuleResolver;
  end;

implementation

uses
  Math,
  StrUtils,
  SysUtils,

  GarbageCollector.Generic,
  Souffle.Bytecode.Debug,
  Souffle.Bytecode.Module,
  Souffle.JSON,
  Souffle.VM.CallFrame,
  Souffle.VM.Exception,
  Souffle.VM.NativeFunction,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.CallStack,
  Goccia.Compiler,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Engine,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.Evaluator.TypeOperations,
  Goccia.Interpreter,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Parser,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Token,
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
  TGocciaSouffleIteratorBridge = class(TGocciaIteratorValue)
  private
    FIteratorRef: TSouffleValue;
    FRuntime: TGocciaRuntimeOperations;
  public
    constructor Create(const AIteratorRef: TSouffleValue;
      const ARuntime: TGocciaRuntimeOperations);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
  end;

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
  Rec: TSouffleRecord;
  Slot0: TSouffleValue;
begin
  { Fast path: blueprint-based Map/Set super() — populate from iterable }
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TSouffleRecord) then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Assigned(Rec.Blueprint) and (Rec.Blueprint.SlotCount > 0) then
    begin
      Slot0 := Rec.GetSlot(0);
      if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) then
      begin
        if Slot0.AsReference is TGocciaSouffleMap then
        begin
          if AArgCount >= 1 then
            FRuntime.PopulateMapFromIterable(
              TGocciaSouffleMap(Slot0.AsReference), AArgs^);
          Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
        end;
        if Slot0.AsReference is TGocciaSouffleSet then
        begin
          if AArgCount >= 1 then
            FRuntime.PopulateSetFromIterable(
              TGocciaSouffleSet(Slot0.AsReference), AArgs^);
          Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
        end;
      end;
    end;
  end;

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

{ TGocciaSouffleIteratorBridge }

constructor TGocciaSouffleIteratorBridge.Create(
  const AIteratorRef: TSouffleValue;
  const ARuntime: TGocciaRuntimeOperations);
begin
  inherited Create;
  FIteratorRef := AIteratorRef;
  FRuntime := ARuntime;
end;

function TGocciaSouffleIteratorBridge.AdvanceNext: TGocciaObjectValue;
var
  Done: Boolean;
  Val: TSouffleValue;
  GocciaVal: TGocciaValue;
begin
  Val := FRuntime.IteratorNext(FIteratorRef, Done);
  if Done then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
  end
  else
  begin
    GocciaVal := FRuntime.UnwrapToGocciaValue(Val);
    Result := CreateIteratorResult(GocciaVal, False);
  end;
end;

function TGocciaSouffleIteratorBridge.DirectNext(
  out ADone: Boolean): TGocciaValue;
var
  Val: TSouffleValue;
begin
  Val := FRuntime.IteratorNext(FIteratorRef, ADone);
  if ADone then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    Result := FRuntime.UnwrapToGocciaValue(Val);
end;

function TGocciaSouffleProxy.ToStringTag: string;
var
  SymTagKey: string;
  TagVal: TSouffleValue;
  Rec: TSouffleRecord;
  WalkRec: TSouffleRecord;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    SymTagKey := '@@sym:' + IntToStr(TGocciaSymbolValue.WellKnownToStringTag.Id);
    { Check record itself }
    if Rec.Get(SymTagKey, TagVal) and SouffleIsStringValue(TagVal) then
      Exit(SouffleGetString(TagVal));
    { Walk delegate chain for Symbol.toStringTag }
    WalkRec := Rec;
    while Assigned(WalkRec.Delegate) and (WalkRec.Delegate is TSouffleRecord) do
    begin
      WalkRec := TSouffleRecord(WalkRec.Delegate);
      if WalkRec.Get(SymTagKey, TagVal) and SouffleIsStringValue(TagVal) then
        Exit(SouffleGetString(TagVal));
    end;
  end;
  Result := inherited ToStringTag;
end;

function TGocciaSouffleProxy.GetOwnSymbolPropertyDescriptor(
  const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
var
  SymPropKey: string;
  Rec: TSouffleRecord;
  GetterVal, SetterVal: TSouffleValue;
  GocciaGetter, GocciaSetter: TGocciaValue;
  HasGetter, HasSetter: Boolean;
begin
  Result := inherited GetOwnSymbolPropertyDescriptor(ASymbol);
  if Assigned(Result) then Exit;

  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    SymPropKey := '@@sym:' + IntToStr(ASymbol.Id);
    HasGetter := Rec.HasGetters and Rec.Getters.Get(SymPropKey, GetterVal);
    HasSetter := Rec.HasSetters and Rec.Setters.Get(SymPropKey, SetterVal);
    if HasGetter or HasSetter then
    begin
      if HasGetter then
        GocciaGetter := FRuntime.UnwrapToGocciaValue(GetterVal)
      else
        GocciaGetter := nil;
      if HasSetter then
        GocciaSetter := FRuntime.UnwrapToGocciaValue(SetterVal)
      else
        GocciaSetter := nil;
      Result := TGocciaPropertyDescriptorAccessor.Create(
        GocciaGetter, GocciaSetter, [pfConfigurable]);
      Exit;
    end;
  end;
end;

function TGocciaSouffleProxy.NativeKind: string;
begin
  if (FTarget is TSouffleRecord) and
     Assigned(TSouffleRecord(FTarget).Blueprint) then
    Result := TSouffleRecord(FTarget).Blueprint.Name
  else
    Result := '';
end;

function TGocciaSouffleProxy.CloneNative: TGocciaObjectValue;
var
  SrcRec, NewRec: TSouffleRecord;
  SrcMap: TGocciaSouffleMap;
  SrcSet: TGocciaSouffleSet;
  NewMap: TGocciaSouffleMap;
  NewSet: TGocciaSouffleSet;
  SrcAB, NewAB: TSouffleArrayBuffer;
  Slot0: TSouffleValue;
  I: Integer;
  GC: TGarbageCollector;
begin
  Result := nil;
  if not (FTarget is TSouffleRecord) then Exit;
  SrcRec := TSouffleRecord(FTarget);
  if not Assigned(SrcRec.Blueprint) or (SrcRec.Blueprint.SlotCount < 1) then Exit;

  GC := TGarbageCollector.Instance;
  Slot0 := SrcRec.GetSlot(0);
  if not SouffleIsReference(Slot0) or not Assigned(Slot0.AsReference) then Exit;

  if Slot0.AsReference is TGocciaSouffleMap then
  begin
    SrcMap := TGocciaSouffleMap(Slot0.AsReference);
    NewMap := TGocciaSouffleMap.Create(SrcMap.Count);
    if Assigned(GC) then GC.AllocateObject(NewMap);
    for I := 0 to SrcMap.Count - 1 do
      NewMap.SetEntry(SrcMap.GetKeyAt(I), SrcMap.GetValueAt(I));
    NewRec := TSouffleRecord.CreateFromBlueprint(SrcRec.Blueprint);
    NewRec.Delegate := SrcRec.Blueprint.Prototype;
    NewRec.SetSlot(0, SouffleReference(NewMap));
    if Assigned(GC) then GC.AllocateObject(NewRec);
    Result := TGocciaSouffleProxy.Create(NewRec, FRuntime);
  end
  else if Slot0.AsReference is TGocciaSouffleSet then
  begin
    SrcSet := TGocciaSouffleSet(Slot0.AsReference);
    NewSet := TGocciaSouffleSet.Create(SrcSet.Count);
    if Assigned(GC) then GC.AllocateObject(NewSet);
    for I := 0 to SrcSet.Count - 1 do
      NewSet.Add(SrcSet.GetItemAt(I));
    NewRec := TSouffleRecord.CreateFromBlueprint(SrcRec.Blueprint);
    NewRec.Delegate := SrcRec.Blueprint.Prototype;
    NewRec.SetSlot(0, SouffleReference(NewSet));
    if Assigned(GC) then GC.AllocateObject(NewRec);
    Result := TGocciaSouffleProxy.Create(NewRec, FRuntime);
  end
  else if Slot0.AsReference is TSouffleArrayBuffer then
  begin
    SrcAB := TSouffleArrayBuffer(Slot0.AsReference);
    NewAB := TSouffleArrayBuffer.Create(SrcAB.ByteLength);
    if Assigned(GC) then GC.AllocateObject(NewAB);
    if SrcAB.ByteLength > 0 then
      Move(SrcAB.Data[0], NewAB.Data[0], SrcAB.ByteLength);
    NewRec := TSouffleRecord.CreateFromBlueprint(SrcRec.Blueprint);
    NewRec.Delegate := SrcRec.Blueprint.Prototype;
    NewRec.SetSlot(0, SouffleReference(NewAB));
    if Assigned(GC) then GC.AllocateObject(NewRec);
    Result := TGocciaSouffleProxy.Create(NewRec, FRuntime);
  end;
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
  Rec, WalkRec: TSouffleRecord;
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
    { Walk delegate chain for symbol properties (prototype, method delegate, etc.) }
    if Assigned(Rec.Delegate) and (Rec.Delegate is TSouffleRecord) then
    begin
      WalkRec := TSouffleRecord(Rec.Delegate);
      while Assigned(WalkRec) do
      begin
        if WalkRec.Get(SymPropKey, DirectVal) then
          Exit(FRuntime.UnwrapToGocciaValue(DirectVal));
        if not Assigned(WalkRec.Delegate) or
           not (WalkRec.Delegate is TSouffleRecord) then Break;
        WalkRec := TSouffleRecord(WalkRec.Delegate);
      end;
    end;
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
  if Result then Exit;
  { Check if GetSymbolProperty can find it on the record/delegate chain }
  Result := Assigned(GetSymbolProperty(ASymbol)) and
    not (GetSymbolProperty(ASymbol) is TGocciaUndefinedLiteralValue);
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
  FModuleCache := TOrderedStringMap<TSouffleValue>.Create;
  FCompiledModules := TList.Create;
  FModuleResolver := nil;
  FBridgeCallDepth := 0;
  FVM := nil;
  TGarbageCollector.Initialize;
  TGarbageCollector.Instance.AddExternalRootMarker(MarkWrappedGocciaValues);
end;

destructor TGocciaRuntimeOperations.Destroy;
var
  I: Integer;
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
  FModuleCache.Free;
  for I := 0 to FCompiledModules.Count - 1 do
    TObject(FCompiledModules[I]).Free;
  FCompiledModules.Free;
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
var
  Bp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  ErrObj: TGocciaObjectValue;
  NameStr, MsgStr, StackStr: string;
  NameVal, MsgVal, StackVal, CauseVal: TGocciaValue;
begin
  { Convert Goccia error objects to native blueprint records }
  if Assigned(FErrorBlueprint) and (E.Value is TGocciaObjectValue) and
     TGocciaObjectValue(E.Value).HasErrorData then
  begin
    ErrObj := TGocciaObjectValue(E.Value);
    NameVal := ErrObj.GetProperty('name');
    if Assigned(NameVal) then
      NameStr := NameVal.ToStringLiteral.Value
    else
      NameStr := 'Error';

    { Find matching blueprint }
    Bp := FErrorBlueprint;
    if NameStr = 'TypeError' then Bp := FTypeErrorBlueprint
    else if NameStr = 'RangeError' then Bp := FRangeErrorBlueprint
    else if NameStr = 'ReferenceError' then Bp := FReferenceErrorBlueprint
    else if NameStr = 'SyntaxError' then Bp := FSyntaxErrorBlueprint
    else if NameStr = 'URIError' then Bp := FURIErrorBlueprint
    else if NameStr = 'AggregateError' then Bp := FAggregateErrorBlueprint;

    Rec := TSouffleRecord.CreateFromBlueprint(Bp);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Rec);
    Rec.Delegate := Bp.Prototype;
    Rec.Put('name', SouffleString(NameStr));

    MsgVal := ErrObj.GetProperty('message');
    if Assigned(MsgVal) and not (MsgVal is TGocciaUndefinedLiteralValue) then
      MsgStr := MsgVal.ToStringLiteral.Value
    else
      MsgStr := '';
    Rec.Put('message', SouffleString(MsgStr));

    StackVal := ErrObj.GetProperty('stack');
    if Assigned(StackVal) and not (StackVal is TGocciaUndefinedLiteralValue) then
      StackStr := StackVal.ToStringLiteral.Value
    else
      StackStr := NameStr + ': ' + MsgStr;
    Rec.Put('stack', SouffleString(StackStr));

    CauseVal := ErrObj.GetProperty('cause');
    if Assigned(CauseVal) and not (CauseVal is TGocciaUndefinedLiteralValue) then
      Rec.Put('cause', ToSouffleValue(CauseVal));

    raise ESouffleThrow.Create(SouffleReference(Rec));
  end;

  { Convert DOMException (HasErrorData=False but has name/message/code) }
  if Assigned(FDOMExceptionBlueprint) and (E.Value is TGocciaObjectValue) and
     not TGocciaObjectValue(E.Value).HasErrorData then
  begin
    ErrObj := TGocciaObjectValue(E.Value);
    NameVal := ErrObj.GetProperty('name');
    if Assigned(NameVal) and Assigned(ErrObj.GetProperty('code')) then
    begin
      NameStr := NameVal.ToStringLiteral.Value;
      MsgVal := ErrObj.GetProperty('message');
      if Assigned(MsgVal) and not (MsgVal is TGocciaUndefinedLiteralValue) then
        MsgStr := MsgVal.ToStringLiteral.Value
      else
        MsgStr := '';

      Rec := TSouffleRecord.CreateFromBlueprint(FDOMExceptionBlueprint);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(Rec);
      Rec.Delegate := FDOMExceptionBlueprint.Prototype;
      Rec.Put('name', SouffleString(NameStr));
      Rec.Put('message', SouffleString(MsgStr));
      Rec.Put('code', ToSouffleValue(ErrObj.GetProperty('code')));

      StackVal := ErrObj.GetProperty('stack');
      if Assigned(StackVal) and not (StackVal is TGocciaUndefinedLiteralValue) then
        Rec.Put('stack', SouffleString(StackVal.ToStringLiteral.Value));

      raise ESouffleThrow.Create(SouffleReference(Rec));
    end;
  end;

  raise ESouffleThrow.Create(WrapGocciaValue(E.Value));
end;

procedure TGocciaRuntimeOperations.RethrowAsVM(const AValue: TSouffleValue);
begin
  raise ESouffleThrow.Create(AValue);
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
  Bp: TSouffleBlueprint;
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
        { All records (including Map/Set blueprint records) → cached TGocciaSouffleProxy }
        if not FRecordBridgeCache.TryGetValue(AValue.AsReference, CachedBridge) then
        begin
          {$IFDEF BRIDGE_METRICS}
          Inc(GBridgeMetrics.RecordCacheMiss);
          {$ENDIF}
          CachedBridge := TGocciaSouffleProxy.Create(AValue.AsReference, Self);
          { Mark Error blueprint records with HasErrorData (not DOMException) }
          if Assigned(TSouffleRecord(AValue.AsReference).Blueprint) and
             Assigned(FErrorBlueprint) and
             (TSouffleRecord(AValue.AsReference).Blueprint <> FDOMExceptionBlueprint) then
          begin
            Bp := TSouffleRecord(AValue.AsReference).Blueprint;
            while Assigned(Bp) do
            begin
              if Bp = FErrorBlueprint then
              begin
                TGocciaSouffleProxy(CachedBridge).HasErrorData := True;
                Break;
              end;
              Bp := Bp.SuperBlueprint;
            end;
          end;
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
      else if (AValue.AsReference is TGocciaSouffleMapIterator) or
              (AValue.AsReference is TGocciaSouffleSetIterator) or
              (AValue.AsReference is TSouffleTypedArrayIterator) then
        Result := TGocciaSouffleIteratorBridge.Create(AValue, Self)
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
function GetSouffleMap(const AReceiver: TSouffleValue): TGocciaSouffleMap; forward;
function GetSouffleSet(const AReceiver: TSouffleValue): TGocciaSouffleSet; forward;
function GetSoufflePromise(const AReceiver: TSouffleValue): TSoufflePromise; forward;
function GetSouffleArrayBuffer(const AReceiver: TSouffleValue): TSouffleArrayBuffer; forward;
function GetSouffleTypedArray(const AReceiver: TSouffleValue): TSouffleTypedArray; forward;
function ConstructNativeError(const ARuntime: TGocciaRuntimeOperations;
  const ABp: TSouffleBlueprint; const AErrorName: string;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue; forward;
function NativeAggregateErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue; forward;
function ConstructNativeDOMException(const AArgs: PSouffleValue;
  const AArgCount: Integer): TSouffleValue; forward;
function NativeTypedArrayBuffer(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue; forward;

function CreateBlueprintMapFromGoccia(const AMap: TGocciaMapValue): TSouffleValue; forward;
function CreateBlueprintSetFromGoccia(const ASet: TGocciaSetValue): TSouffleValue; forward;

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

  if AValue is TGocciaSouffleIteratorBridge then
    Exit(TGocciaSouffleIteratorBridge(AValue).FIteratorRef);

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
  else if (AValue is TGocciaMapValue) and Assigned(FMapBlueprint) then
    Result := CreateBlueprintMapFromGoccia(TGocciaMapValue(AValue))
  else if (AValue is TGocciaSetValue) and Assigned(FSetBlueprint) then
    Result := CreateBlueprintSetFromGoccia(TGocciaSetValue(AValue))
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
  SM: TGocciaSouffleMap;
  SS: TGocciaSouffleSet;
  TA: TSouffleTypedArray;
  AB: TSouffleArrayBuffer;
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
        Prop := TGocciaBridgedFunction(AObject.AsReference).FGocciaFn
          .GetProperty(AKey);
        if Assigned(Prop) then
          Exit(ToSouffleValue(Prop));
      end;
    end;

    { Blueprint-based Map/Set size getter }
    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TSouffleRecord) and
       Assigned(TSouffleRecord(AObject.AsReference).Blueprint) and
       (AKey = PROP_SIZE) then
    begin
      Bp := TSouffleRecord(AObject.AsReference).Blueprint;
      while Assigned(Bp) do
      begin
        if Bp = FMapBlueprint then
        begin
          SM := GetSouffleMap(AObject);
          if Assigned(SM) then
            Exit(SouffleInteger(SM.Count));
          Break;
        end;
        if Bp = FSetBlueprint then
        begin
          SS := GetSouffleSet(AObject);
          if Assigned(SS) then
            Exit(SouffleInteger(SS.Count));
          Break;
        end;
        Bp := Bp.SuperBlueprint;
      end;
    end;

    { TypedArray accessor properties }
    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TSouffleRecord) then
    begin
      TA := GetSouffleTypedArray(AObject);
      if Assigned(TA) then
      begin
        if AKey = 'length' then Exit(SouffleInteger(TA.ElementLength));
        if AKey = 'byteLength' then Exit(SouffleInteger(TA.ElementLength * BytesPerElement(TA.Kind)));
        if AKey = 'byteOffset' then Exit(SouffleInteger(TA.ByteOffset));
        if AKey = 'BYTES_PER_ELEMENT' then Exit(SouffleInteger(BytesPerElement(TA.Kind)));
        if AKey = 'buffer' then
        begin
          Result := NativeTypedArrayBuffer(AObject, nil, 0);
          Exit;
        end;
      end;
      AB := GetSouffleArrayBuffer(AObject);
      if Assigned(AB) then
      begin
        if AKey = 'byteLength' then Exit(SouffleInteger(AB.ByteLength));
      end;
    end;

    { Legacy direct TGocciaSouffleMap/Set (for backward compat during migration) }
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
       (AObject.AsReference is TGocciaSouffleMapIterator) then
    begin
      if Assigned(FMapIteratorDelegate) and FMapIteratorDelegate.Get(AKey, Result) then
        Exit;
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TGocciaSouffleSetIterator) then
    begin
      if Assigned(FSetIteratorDelegate) and FSetIteratorDelegate.Get(AKey, Result) then
        Exit;
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
       (AObject.AsReference is TSouffleTypedArrayIterator) then
    begin
      if Assigned(FMapIteratorDelegate) and FMapIteratorDelegate.Get(AKey, Result) then
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
      ;
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
  TA: TSouffleTypedArray;
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
    { TypedArray numeric index access }
    else if (AKey.Kind = svkInteger) and SouffleIsReference(AObject) and
       Assigned(AObject.AsReference) then
    begin
      TA := GetSouffleTypedArray(AObject);
      if Assigned(TA) then
      begin
        I := Integer(AKey.AsInteger);
        if (I >= 0) and (I < TA.ElementLength) then
          Exit(SouffleFloat(TA.ReadElement(I)))
        else
          Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
      end;
      Result := GetProperty(AObject, CoerceKeyToString(AKey));
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
  TA: TSouffleTypedArray;
  I: Integer;
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
    else if (AKey.Kind = svkInteger) and SouffleIsReference(AObject) and
       Assigned(AObject.AsReference) then
    begin
      TA := GetSouffleTypedArray(AObject);
      if Assigned(TA) then
      begin
        I := Integer(AKey.AsInteger);
        if (I >= 0) and (I < TA.ElementLength) then
          TA.WriteElement(I, CoerceToNumber(AValue));
        Exit;
      end;
      SetProperty(AObject, CoerceKeyToString(AKey), AValue);
    end
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
  SM: TGocciaSouffleMap;
  SS: TGocciaSouffleSet;
  CtorMethod: TSouffleValue;
  VMArgs: array of TSouffleValue;
  FieldInits: array of TSouffleClosure;
  FieldInitCount: Integer;
  TAKind: TSouffleTypedArrayKind;
begin
  if SouffleIsReference(AConstructor) and
     Assigned(AConstructor.AsReference) and
     (AConstructor.AsReference is TSouffleBlueprint) then
  begin
    Bp := TSouffleBlueprint(AConstructor.AsReference);

    { Built-in type construction — no bridge crossing }
    if Bp = FMapBlueprint then
    begin
      {$IFDEF BRIDGE_METRICS}
      MetricNative;
      {$ENDIF}
      Result := ConstructNativeMap(AArgs, AArgCount);
      Exit;
    end;
    if Bp = FPromiseBlueprint then
    begin
      {$IFDEF BRIDGE_METRICS}
      MetricNative;
      {$ENDIF}
      Result := ConstructNativePromise(AArgs, AArgCount);
      Exit;
    end;
    if Bp = FSetBlueprint then
    begin
      {$IFDEF BRIDGE_METRICS}
      MetricNative;
      {$ENDIF}
      Result := ConstructNativeSet(AArgs, AArgCount);
      Exit;
    end;
    if Bp = FArrayBufferBlueprint then
    begin
      {$IFDEF BRIDGE_METRICS}
      MetricNative;
      {$ENDIF}
      Result := ConstructNativeArrayBuffer(AArgs, AArgCount, FArrayBufferBlueprint);
      Exit;
    end;
    if Bp = FSharedArrayBufferBlueprint then
    begin
      {$IFDEF BRIDGE_METRICS}
      MetricNative;
      {$ENDIF}
      Result := ConstructNativeArrayBuffer(AArgs, AArgCount, FSharedArrayBufferBlueprint);
      Exit;
    end;
    { Error construction }
    if Bp = FErrorBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := ConstructNativeError(Self, FErrorBlueprint, 'Error', AArgs, AArgCount); Exit; end;
    if Bp = FTypeErrorBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := ConstructNativeError(Self, FTypeErrorBlueprint, 'TypeError', AArgs, AArgCount); Exit; end;
    if Bp = FRangeErrorBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := ConstructNativeError(Self, FRangeErrorBlueprint, 'RangeError', AArgs, AArgCount); Exit; end;
    if Bp = FReferenceErrorBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := ConstructNativeError(Self, FReferenceErrorBlueprint, 'ReferenceError', AArgs, AArgCount); Exit; end;
    if Bp = FSyntaxErrorBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := ConstructNativeError(Self, FSyntaxErrorBlueprint, 'SyntaxError', AArgs, AArgCount); Exit; end;
    if Bp = FURIErrorBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := ConstructNativeError(Self, FURIErrorBlueprint, 'URIError', AArgs, AArgCount); Exit; end;
    if Bp = FAggregateErrorBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := NativeAggregateErrorConstructor(SouffleNil, AArgs, AArgCount); Exit; end;
    if Bp = FDOMExceptionBlueprint then begin {$IFDEF BRIDGE_METRICS} MetricNative; {$ENDIF} Result := ConstructNativeDOMException(AArgs, AArgCount); Exit; end;
    { TypedArray construction }
    for TAKind := Low(TSouffleTypedArrayKind) to High(TSouffleTypedArrayKind) do
      if Bp = FTypedArrayBlueprints[TAKind] then
      begin
        {$IFDEF BRIDGE_METRICS}
        MetricNative;
        {$ENDIF}
        Result := ConstructNativeTypedArray(AArgs, AArgCount, TAKind);
        Exit;
      end;

    WalkBp := Bp;
    while Assigned(WalkBp.SuperBlueprint) do
      WalkBp := WalkBp.SuperBlueprint;
    if not FBlueprintSuperValues.ContainsKey(WalkBp) then
    begin
      Rec := TSouffleRecord.CreateFromBlueprint(Bp);
      Rec.Delegate := Bp.Prototype;

      { Initialize slot 0 for Map/Set subclasses — empty, super() populates }
      if (WalkBp = FMapBlueprint) and (Bp <> FMapBlueprint) then
      begin
        SM := TGocciaSouffleMap.Create(4);
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(SM);
        Rec.SetSlot(0, SouffleReference(SM));
      end
      else if (WalkBp = FSetBlueprint) and (Bp <> FSetBlueprint) then
      begin
        SS := TGocciaSouffleSet.Create(4);
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(SS);
        Rec.SetSlot(0, SouffleReference(SS));
      end;

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
      end
      else if SouffleIsReference(CtorMethod) and
         (CtorMethod.AsReference is TSouffleNativeFunction) then
      begin
        TSouffleNativeFunction(CtorMethod.AsReference).Invoke(
          SouffleReference(Rec), AArgs, AArgCount);
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
    { Native iterator self-return — works for both sync and async contexts }
    if SouffleIsReference(AIterable) and Assigned(AIterable.AsReference) then
    begin
      if AIterable.AsReference is TGocciaSouffleMapIterator then
        Exit(AIterable);
      if AIterable.AsReference is TGocciaSouffleSetIterator then
        Exit(AIterable);
      if AIterable.AsReference is TSouffleTypedArrayIterator then
        Exit(AIterable);
    end;

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
      { Blueprint record Map/Set }
      if AIterable.AsReference is TSouffleRecord then
      begin
        if Assigned(TSouffleRecord(AIterable.AsReference).Blueprint) then
        begin
          if TSouffleRecord(AIterable.AsReference).Blueprint = FMapBlueprint then
          begin
            Result := SouffleReference(
              TGocciaSouffleMapIterator.Create(
                GetSouffleMap(AIterable), mikEntries));
            if Assigned(TGarbageCollector.Instance) then
              TGarbageCollector.Instance.AllocateObject(Result.AsReference);
            Exit;
          end;
          if TSouffleRecord(AIterable.AsReference).Blueprint = FSetBlueprint then
          begin
            Result := SouffleReference(
              TGocciaSouffleSetIterator.Create(
                GetSouffleSet(AIterable), sikValues));
            if Assigned(TGarbageCollector.Instance) then
              TGarbageCollector.Instance.AllocateObject(Result.AsReference);
            Exit;
          end;
        end;
      end;
      if AIterable.AsReference is TGocciaSouffleMap then
      begin
        Result := SouffleReference(
          TGocciaSouffleMapIterator.Create(
            TGocciaSouffleMap(AIterable.AsReference), mikEntries));
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(Result.AsReference);
        Exit;
      end;
      if AIterable.AsReference is TGocciaSouffleSet then
      begin
        Result := SouffleReference(
          TGocciaSouffleSetIterator.Create(
            TGocciaSouffleSet(AIterable.AsReference), sikValues));
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(Result.AsReference);
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
      if AIterator.AsReference is TGocciaSouffleMapIterator then
      begin
        Result := TGocciaSouffleMapIterator(AIterator.AsReference).Next(ADone);
        Exit;
      end;
      if AIterator.AsReference is TGocciaSouffleSetIterator then
      begin
        Result := TGocciaSouffleSetIterator(AIterator.AsReference).Next(ADone);
        Exit;
      end;
      if AIterator.AsReference is TSouffleTypedArrayIterator then
      begin
        Result := TSouffleTypedArrayIterator(AIterator.AsReference).Next(ADone);
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
begin
  if Assigned(FModuleResolver) then
    Result := LoadModuleNative(APath, FSourcePath)
  else
    Result := SouffleNil;
end;

function TGocciaRuntimeOperations.LoadModuleNative(
  const APath, AImportingPath: string): TSouffleValue;
var
  ResolvedPath: string;
  Source: TStringList;
  SourceText: string;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  Module: TSouffleBytecodeModule;
  Template: TSouffleFunctionTemplate;
  Rec: TSouffleRecord;
  ExportPair: TOrderedStringMap<TSouffleValue>.TKeyValuePair;
  SavedSourcePath: string;
  SavedExports: TOrderedStringMap<TSouffleValue>;
begin
  try
    ResolvedPath := FModuleResolver.Resolve(APath, AImportingPath);
  except
    on E: EModuleNotFound do
    begin
      ThrowTypeErrorMessage(E.Message);
      Exit(SouffleNil);
    end;
  end;

  if FModuleCache.TryGetValue(ResolvedPath, Result) then
    Exit;

  if LowerCase(ExtractFileExt(ResolvedPath)) = '.json' then
  begin
    try
      Result := LoadJsonModuleNative(ResolvedPath);
    except
      on E: Exception do
      begin
        ThrowTypeErrorMessage(
          Format('Failed to parse JSON module "%s": %s', [ResolvedPath, E.Message]));
        Exit(SouffleNil);
      end;
    end;
    FModuleCache.AddOrSetValue(ResolvedPath, Result);
    Exit;
  end;

  Rec := TSouffleRecord.Create(0);
  if Assigned(FVM) then
    Rec.Delegate := FVM.RecordDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Result := SouffleReference(Rec);
  FModuleCache.AddOrSetValue(ResolvedPath, Result);

  try
    Source := TStringList.Create;
    try
      Source.LoadFromFile(ResolvedPath);
      SourceText := Source.Text;
    finally
      Source.Free;
    end;
  except
    on E: Exception do
    begin
      ThrowTypeErrorMessage(
        Format('Cannot load module "%s": %s', [APath, E.Message]));
      Exit;
    end;
  end;

  SavedSourcePath := FSourcePath;
  SavedExports := FExports;
  FExports := TOrderedStringMap<TSouffleValue>.Create;
  FSourcePath := ResolvedPath;
  try
    try
      Lexer := TGocciaLexer.Create(SourceText, ResolvedPath);
      try
        Tokens := Lexer.ScanTokens;
        Parser := TGocciaParser.Create(Tokens, ResolvedPath, Lexer.SourceLines);
        try
          ProgramNode := Parser.Parse;
          try
            Compiler := TGocciaCompiler.Create(ResolvedPath);
            try
              Module := Compiler.Compile(ProgramNode);
              FCompiledModules.Add(Module);

              for Template in Compiler.FormalParameterCounts.Keys do
                RegisterFormalParameterCount(
                  Template, Compiler.FormalParameterCounts[Template]);

              FVM.Execute(Module);

              if Assigned(TGocciaMicrotaskQueue.Instance) then
                TGocciaMicrotaskQueue.Instance.DrainQueue;

              for ExportPair in FExports do
                Rec.Put(ExportPair.Key, ExportPair.Value);
            finally
              Compiler.Free;
            end;
          finally
            ProgramNode.Free;
          end;
        finally
          Parser.Free;
        end;
      finally
        Lexer.Free;
      end;
    except
      on E: ESouffleThrow do
        raise;
      on E: TGocciaThrowValue do
        RethrowAsVM(E);
      on E: Exception do
        ThrowTypeErrorMessage(
          Format('Error loading module "%s": %s', [APath, E.Message]));
    end;
  finally
    FExports.Free;
    FExports := SavedExports;
    FSourcePath := SavedSourcePath;
  end;
end;

function TGocciaRuntimeOperations.LoadJsonModuleNative(
  const AResolvedPath: string): TSouffleValue;
var
  Source: TStringList;
  Parser: TSouffleJSONParser;
  Rec: TSouffleRecord;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AResolvedPath);
    Parser := TSouffleJSONParser.Create(FVM);
    try
      Result := Parser.Parse(Source.Text);
    finally
      Parser.Free;
    end;
  finally
    Source.Free;
  end;

  if SouffleIsReference(Result) and (Result.AsReference is TSouffleRecord) then
    Exit;

  Rec := TSouffleRecord.Create(1);
  if Assigned(FVM) then
    Rec.Delegate := FVM.RecordDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Put('default', Result);
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
  GocciaVal, ThenMethod: TGocciaValue;
  ThenVal: TSouffleValue;
  Promise: TGocciaPromiseValue;
  SP: TSoufflePromise;
  ThenArgs: TGocciaArgumentsCollection;
  Queue: TGocciaMicrotaskQueue;
  PromiseRooted: Boolean;
begin
  case AValue.Kind of
    svkNil, svkBoolean, svkInteger, svkFloat, svkString:
      Exit(AValue);
  end;
  if not SouffleIsReference(AValue) or not Assigned(AValue.AsReference) then
    Exit(AValue);
  if AValue.AsReference is TSouffleHeapString then
    Exit(AValue);

  { Fast path: TSoufflePromise in blueprint record — no bridge crossing }
  SP := GetSoufflePromise(AValue);
  if Assigned(SP) then
  begin
    if SP.State = spssFulfilled then
      Exit(SP.PromiseResult);
    if SP.State = spssRejected then
    begin
      RethrowAsVM(SP.PromiseResult);
      Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    end;
    { Pending: drain microtask queue }
    Queue := TGocciaMicrotaskQueue.Instance;
    if Assigned(Queue) then
      Queue.DrainQueue;
    if SP.State = spssFulfilled then
      Exit(SP.PromiseResult);
    if SP.State = spssRejected then
    begin
      RethrowAsVM(SP.PromiseResult);
      Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    end;
    ThrowTypeErrorMessage('await: Promise did not settle after microtask drain');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  Promise := nil;
  PromiseRooted := False;

  if AValue.AsReference is TGocciaWrappedValue then
  begin
    GocciaVal := TGocciaWrappedValue(AValue.AsReference).Value;

    if GocciaVal is TGocciaPromiseValue then
      Promise := TGocciaPromiseValue(GocciaVal)
    else if GocciaVal is TGocciaObjectValue then
    begin
      ThenMethod := GocciaVal.GetProperty(PROP_THEN);
      if Assigned(ThenMethod) and
         not (ThenMethod is TGocciaUndefinedLiteralValue) and
         ThenMethod.IsCallable then
      begin
        Promise := TGocciaPromiseValue.Create;
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AddTempRoot(Promise);
        PromiseRooted := True;
        ThenArgs := TGocciaArgumentsCollection.Create([
          TGocciaNativeFunctionValue.Create(Promise.DoResolve, 'resolve', 1),
          TGocciaNativeFunctionValue.Create(Promise.DoReject, 'reject', 1)
        ]);
        try
          try
            TGocciaFunctionBase(ThenMethod).Call(ThenArgs, GocciaVal);
          except
            on E: TGocciaThrowValue do
              Promise.Reject(E.Value);
          end;
        finally
          ThenArgs.Free;
        end;
      end
      else
        Exit(AValue);
    end
    else
      Exit(ToSouffleValue(GocciaVal));
  end
  else if AValue.AsReference is TSouffleRecord then
  begin
    ThenVal := GetProperty(AValue, PROP_THEN);
    GocciaVal := UnwrapToGocciaValue(ThenVal);
    if Assigned(GocciaVal) and
       not (GocciaVal is TGocciaUndefinedLiteralValue) and
       (GocciaVal is TGocciaFunctionBase) then
    begin
      Promise := TGocciaPromiseValue.Create;
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(Promise);
      PromiseRooted := True;
      ThenArgs := TGocciaArgumentsCollection.Create([
        TGocciaNativeFunctionValue.Create(Promise.DoResolve, 'resolve', 1),
        TGocciaNativeFunctionValue.Create(Promise.DoReject, 'reject', 1)
      ]);
      try
        try
          TGocciaFunctionBase(GocciaVal).Call(
            ThenArgs, UnwrapToGocciaValue(AValue));
        except
          on E: TGocciaThrowValue do
            Promise.Reject(E.Value);
        end;
      finally
        ThenArgs.Free;
      end;
    end
    else
      Exit(AValue);
  end
  else
    Exit(AValue);

  if not Assigned(Promise) then
    Exit(AValue);

  if not PromiseRooted and Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.AddTempRoot(Promise);
    PromiseRooted := True;
  end;
  try
    if Promise.State = gpsFulfilled then
      Exit(ToSouffleValue(Promise.PromiseResult));
    if Promise.State = gpsRejected then
    begin
      RethrowAsVM(TGocciaThrowValue.Create(Promise.PromiseResult));
      Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    end;

    Queue := TGocciaMicrotaskQueue.Instance;
    if Assigned(Queue) then
      Queue.DrainQueue;

    if Promise.State = gpsFulfilled then
      Result := ToSouffleValue(Promise.PromiseResult)
    else if Promise.State = gpsRejected then
    begin
      RethrowAsVM(TGocciaThrowValue.Create(Promise.PromiseResult));
      Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
    end
    else
      ThrowTypeErrorMessage(
        'await: Promise did not settle after microtask drain');
  finally
    if PromiseRooted and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
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
  Bp, WalkBp: TSouffleBlueprint;
  GetterVal, DirectVal: TSouffleValue;
begin
  AFound := False;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AObject) or not Assigned(AObject.AsReference) then
    Exit;

  SymPropKey := '@@sym:' + IntToStr(ASymKey.Id);

  { Blueprint symbol property access (e.g., Map[Symbol.species]) }
  if AObject.AsReference is TSouffleBlueprint then
  begin
    Bp := TSouffleBlueprint(AObject.AsReference);
    { Symbol.species returns the constructor itself for built-in types and subclasses }
    if ASymKey = TGocciaSymbolValue.WellKnownSpecies then
    begin
      WalkBp := Bp;
      while Assigned(WalkBp) do
      begin
        if (WalkBp = FMapBlueprint) or (WalkBp = FSetBlueprint) then
        begin
          AFound := True;
          Result := AObject;
          Exit;
        end;
        WalkBp := WalkBp.SuperBlueprint;
      end;
    end;
    if Bp.HasStaticFields and Bp.StaticFields.Get(SymPropKey, DirectVal) then
    begin
      AFound := True;
      Result := DirectVal;
      Exit;
    end;
    if Bp.HasStaticGetters and Bp.StaticGetters.Get(SymPropKey, GetterVal) then
    begin
      AFound := True;
      if TryInvokeGetter(GetterVal, AObject, Result) then
        Exit;
      Exit;
    end;
    Exit;
  end;

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
    { Walk delegate chain for symbol properties (e.g., Symbol.toStringTag on prototype) }
    if Assigned(Rec.Delegate) and (Rec.Delegate is TSouffleRecord) then
    begin
      if TSouffleRecord(Rec.Delegate).Get(SymPropKey, DirectVal) then
      begin
        AFound := True;
        Result := DirectVal;
        Exit;
      end;
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
var
  MsgVal: TSouffleValue;
begin
  if Assigned(FTypeErrorBlueprint) then
  begin
    MsgVal := SouffleString(AMessage);
    raise ESouffleThrow.Create(ConstructNativeError(Self,
      FTypeErrorBlueprint, 'TypeError', @MsgVal, 1));
  end
  else
  begin
    try ThrowTypeError(AMessage);
    except on E: TGocciaThrowValue do RethrowAsVM(E); end;
  end;
end;

var
  GThrowRangeErrorRuntime: TGocciaRuntimeOperations;

procedure ThrowRangeErrorNative(const AMessage: string);
var
  MsgVal: TSouffleValue;
  Wrapped: TGocciaWrappedValue;
begin
  { Try native blueprint path first }
  if Assigned(GThrowRangeErrorRuntime) and
     Assigned(GThrowRangeErrorRuntime.FRangeErrorBlueprint) then
  begin
    MsgVal := SouffleString(AMessage);
    raise ESouffleThrow.Create(ConstructNativeError(GThrowRangeErrorRuntime,
      GThrowRangeErrorRuntime.FRangeErrorBlueprint, 'RangeError', @MsgVal, 1));
  end;
  { Fallback: Goccia error wrapped }
  Wrapped := TGocciaWrappedValue.Create(
    Goccia.Values.ErrorHelper.CreateErrorObject('RangeError', AMessage));
  raise ESouffleThrow.Create(SouffleReference(Wrapped));
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

function CreateBlueprintMapFromGoccia(const AMap: TGocciaMapValue): TSouffleValue;
var
  SM: TGocciaSouffleMap;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  I: Integer;
  Entry: TGocciaMapEntry;
begin
  Bp := GNativeArrayJoinRuntime.FMapBlueprint;
  SM := TGocciaSouffleMap.Create(AMap.Entries.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(SM);
  for I := 0 to AMap.Entries.Count - 1 do
  begin
    Entry := AMap.Entries[I];
    SM.SetEntry(GNativeArrayJoinRuntime.ToSouffleValue(Entry.Key),
      GNativeArrayJoinRuntime.ToSouffleValue(Entry.Value));
  end;
  Rec := TSouffleRecord.CreateFromBlueprint(Bp);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Delegate := Bp.Prototype;
  Rec.SetSlot(0, SouffleReference(SM));
  Result := SouffleReference(Rec);
end;

function CreateBlueprintSetFromGoccia(const ASet: TGocciaSetValue): TSouffleValue;
var
  SS: TGocciaSouffleSet;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  I: Integer;
begin
  Bp := GNativeArrayJoinRuntime.FSetBlueprint;
  SS := TGocciaSouffleSet.Create(ASet.Items.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(SS);
  for I := 0 to ASet.Items.Count - 1 do
    SS.Add(GNativeArrayJoinRuntime.ToSouffleValue(ASet.Items[I]));
  Rec := TSouffleRecord.CreateFromBlueprint(Bp);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Delegate := Bp.Prototype;
  Rec.SetSlot(0, SouffleReference(SS));
  Result := SouffleReference(Rec);
end;

function TGocciaRuntimeOperations.ConvertBlueprintMapToGoccia(
  const ARec: TSouffleRecord): TGocciaValue;
var
  SM: TGocciaSouffleMap;
  Slot0: TSouffleValue;
  MapVal: TGocciaMapValue;
  I: Integer;
begin
  MapVal := TGocciaMapValue.Create;
  Slot0 := ARec.GetSlot(0);
  if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) and
     (Slot0.AsReference is TGocciaSouffleMap) then
  begin
    SM := TGocciaSouffleMap(Slot0.AsReference);
    for I := 0 to SM.Count - 1 do
      MapVal.SetEntry(
        UnwrapToGocciaValue(SM.GetKeyAt(I)),
        UnwrapToGocciaValue(SM.GetValueAt(I)));
  end;
  Result := MapVal;
end;

function TGocciaRuntimeOperations.ConvertBlueprintSetToGoccia(
  const ARec: TSouffleRecord): TGocciaValue;
var
  SS: TGocciaSouffleSet;
  Slot0: TSouffleValue;
  SetVal: TGocciaSetValue;
  I: Integer;
begin
  SetVal := TGocciaSetValue.Create;
  Slot0 := ARec.GetSlot(0);
  if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) and
     (Slot0.AsReference is TGocciaSouffleSet) then
  begin
    SS := TGocciaSouffleSet(Slot0.AsReference);
    for I := 0 to SS.Count - 1 do
      SetVal.AddItem(UnwrapToGocciaValue(SS.GetItemAt(I)));
  end;
  Result := SetVal;
end;

procedure TGocciaRuntimeOperations.PopulateMapFromIterable(
  const AMap: TSouffleHeapObject; const AIterable: TSouffleValue);
var
  SM: TGocciaSouffleMap;
  IterObj, EntryVal: TSouffleValue;
  EntryArr: TSouffleArray;
  Done: Boolean;
begin
  SM := TGocciaSouffleMap(AMap);
  if SouffleIsNil(AIterable) or not SouffleIsReference(AIterable) then
    Exit;
  IterObj := GetIterator(AIterable, False);
  if not SouffleIsReference(IterObj) or not Assigned(IterObj.AsReference) then
    Exit;
  repeat
    EntryVal := IteratorNext(IterObj, Done);
    if not Done then
    begin
      if SouffleIsReference(EntryVal) and Assigned(EntryVal.AsReference) and
         (EntryVal.AsReference is TSouffleArray) then
      begin
        EntryArr := TSouffleArray(EntryVal.AsReference);
        if EntryArr.Count >= 2 then
          SM.SetEntry(EntryArr.Get(0), EntryArr.Get(1));
      end;
    end;
  until Done;
end;

procedure TGocciaRuntimeOperations.PopulateSetFromIterable(
  const ASet: TSouffleHeapObject; const AIterable: TSouffleValue);
var
  SS: TGocciaSouffleSet;
  IterObj, ItemVal: TSouffleValue;
  Done: Boolean;
begin
  SS := TGocciaSouffleSet(ASet);
  if SouffleIsNil(AIterable) or not SouffleIsReference(AIterable) then
    Exit;
  IterObj := GetIterator(AIterable, False);
  if not SouffleIsReference(IterObj) or not Assigned(IterObj.AsReference) then
    Exit;
  repeat
    ItemVal := IteratorNext(IterObj, Done);
    if not Done then
      SS.Add(ItemVal);
  until Done;
end;

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

{ Promise blueprint helpers }

function TGocciaRuntimeOperations.WrapSoufflePromise(
  const APromise: TSoufflePromise): TSouffleValue;
var
  Rec: TSouffleRecord;
begin
  Rec := TSouffleRecord.CreateFromBlueprint(FPromiseBlueprint);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Delegate := FPromiseBlueprint.Prototype;
  Rec.SetSlot(0, SouffleReference(APromise));
  Result := SouffleReference(Rec);
end;

function NativePromiseResolveCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  P: TSoufflePromise;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if SouffleIsReference(AContext) and Assigned(AContext.AsReference) and
     (AContext.AsReference is TSoufflePromise) then
  begin
    P := TSoufflePromise(AContext.AsReference);
    if P.AlreadyResolved then Exit;
    P.AlreadyResolved := True;
    if AArgCount > 0 then
      P.Resolve(AArgs^)
    else
      P.Resolve(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
end;

function NativePromiseRejectCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  P: TSoufflePromise;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if SouffleIsReference(AContext) and Assigned(AContext.AsReference) and
     (AContext.AsReference is TSoufflePromise) then
  begin
    P := TSoufflePromise(AContext.AsReference);
    if P.AlreadyResolved then Exit;
    P.AlreadyResolved := True;
    if AArgCount > 0 then
      P.Reject(AArgs^)
    else
      P.Reject(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
end;

function TGocciaRuntimeOperations.ConstructNativePromise(
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  P: TSoufflePromise;
  Executor: TSouffleValue;
  ResolveFn, RejectFn: TSouffleNativeClosure;
  VMArgs: array[0..1] of TSouffleValue;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) then
  begin
    ThrowTypeErrorMessage('Promise resolver is not a function');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  Executor := AArgs^;
  if not ((Executor.AsReference is TSouffleClosure) or
          (Executor.AsReference is TSouffleNativeFunction) or
          (Executor.AsReference is TSouffleNativeClosure)) then
  begin
    ThrowTypeErrorMessage('Promise resolver is not a function');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  P := TSoufflePromise.Create;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(P);

  ResolveFn := TSouffleNativeClosure.Create('resolve', 1,
    @NativePromiseResolveCallback, SouffleReference(P));
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(ResolveFn);

  RejectFn := TSouffleNativeClosure.Create('reject', 1,
    @NativePromiseRejectCallback, SouffleReference(P));
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(RejectFn);

  VMArgs[0] := SouffleReference(ResolveFn);
  VMArgs[1] := SouffleReference(RejectFn);

  try
    if Executor.AsReference is TSouffleClosure then
      FVM.ExecuteFunction(TSouffleClosure(Executor.AsReference),
        [SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED),
         SouffleReference(ResolveFn), SouffleReference(RejectFn)])
    else if Executor.AsReference is TSouffleNativeFunction then
      TSouffleNativeFunction(Executor.AsReference).Invoke(
        SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED), @VMArgs[0], 2)
    else if Executor.AsReference is TSouffleNativeClosure then
      TSouffleNativeClosure(Executor.AsReference).Invoke(
        SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED), @VMArgs[0], 2);
  except
    on E: ESouffleThrow do
    begin
      if not P.AlreadyResolved then
      begin
        P.AlreadyResolved := True;
        P.Reject(E.ThrownValue);
      end;
    end;
  end;

  Result := WrapSoufflePromise(P);
end;

procedure TGocciaRuntimeOperations.InvokeSouffleMicrotask(
  const AHandler, AValue: TSouffleValue;
  out AResult: TSouffleValue; out AHadError: Boolean);
var
  VMArgs: array[0..0] of TSouffleValue;
begin
  AHadError := False;
  AResult := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  try
    VMArgs[0] := AValue;
    if AHandler.AsReference is TSouffleClosure then
      AResult := FVM.ExecuteFunction(
        TSouffleClosure(AHandler.AsReference),
        [SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED), AValue])
    else if AHandler.AsReference is TSouffleNativeFunction then
      AResult := TSouffleNativeFunction(AHandler.AsReference).Invoke(
        SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED), @VMArgs[0], 1)
    else if AHandler.AsReference is TSouffleNativeClosure then
      AResult := TSouffleNativeClosure(AHandler.AsReference).Invoke(
        SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED), @VMArgs[0], 1);
  except
    on E: ESouffleThrow do
    begin
      AHadError := True;
      AResult := E.ThrownValue;
    end;
  end;
end;

{ Native ArrayBuffer/TypedArray blueprint construction }

function TGocciaRuntimeOperations.ConstructNativeArrayBuffer(
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const ABlueprint: TSouffleBlueprint): TSouffleValue;
var
  AB: TSouffleArrayBuffer;
  Rec: TSouffleRecord;
  ByteLen: Integer;
  RawLen: Double;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  ByteLen := 0;
  if AArgCount > 0 then
  begin
    RawLen := CoerceToNumber(AArgs^);
    if IsNan(RawLen) then
      ByteLen := 0
    else if IsInfinite(RawLen) then
    begin
      ThrowRangeErrorNative('Invalid array buffer length');
      Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    end
    else
    begin
      ByteLen := Trunc(RawLen);
      if ByteLen < 0 then
      begin
        ThrowRangeErrorNative('Invalid array buffer length');
        Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
      end;
    end;
  end;

  AB := TSouffleArrayBuffer.Create(ByteLen, ABlueprint = FSharedArrayBufferBlueprint);
  if Assigned(GC) then GC.AllocateObject(AB);

  Rec := TSouffleRecord.CreateFromBlueprint(ABlueprint);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := ABlueprint.Prototype;
  Rec.SetSlot(0, SouffleReference(AB));
  Result := SouffleReference(Rec);
end;

function TGocciaRuntimeOperations.ConstructNativeTypedArray(
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AKind: TSouffleTypedArrayKind): TSouffleValue;
var
  AB: TSouffleArrayBuffer;
  TA, SrcTA: TSouffleTypedArray;
  SrcAB: TSouffleArrayBuffer;
  Rec: TSouffleRecord;
  Len, ByteLen, I, ByteOff: Integer;
  GC: TGarbageCollector;
  Arg: TSouffleValue;
  Arr: TSouffleArray;
begin
  GC := TGarbageCollector.Instance;

  if AArgCount < 1 then
  begin
    AB := TSouffleArrayBuffer.Create(0);
    if Assigned(GC) then GC.AllocateObject(AB);
    TA := TSouffleTypedArray.Create(AB, 0, 0, AKind);
    if Assigned(GC) then GC.AllocateObject(TA);
  end
  else
  begin
    Arg := AArgs^;

    { Case 1: new TypedArray(length) }
    if (Arg.Kind = svkInteger) or (Arg.Kind = svkFloat) then
    begin
      if IsNan(CoerceToNumber(Arg)) then
        Len := 0
      else
      begin
        Len := Trunc(CoerceToNumber(Arg));
        if Len < 0 then
          ThrowRangeErrorNative('Invalid typed array length');
      end;
      ByteLen := Len * BytesPerElement(AKind);
      AB := TSouffleArrayBuffer.Create(ByteLen);
      if Assigned(GC) then GC.AllocateObject(AB);
      TA := TSouffleTypedArray.Create(AB, 0, Len, AKind);
      if Assigned(GC) then GC.AllocateObject(TA);
    end
    { Case 2: new TypedArray(typedArray) }
    else if SouffleIsReference(Arg) and Assigned(Arg.AsReference) then
    begin
      SrcTA := GetSouffleTypedArray(Arg);
      if Assigned(SrcTA) then
      begin
        Len := SrcTA.ElementLength;
        ByteLen := Len * BytesPerElement(AKind);
        AB := TSouffleArrayBuffer.Create(ByteLen);
        if Assigned(GC) then GC.AllocateObject(AB);
        TA := TSouffleTypedArray.Create(AB, 0, Len, AKind);
        if Assigned(GC) then GC.AllocateObject(TA);
        for I := 0 to Len - 1 do
          TA.WriteElement(I, SrcTA.ReadElement(I));
      end
      { Case 3: new TypedArray(arrayBuffer[, byteOffset[, length]]) }
      else
      begin
        SrcAB := GetSouffleArrayBuffer(Arg);
        if Assigned(SrcAB) then
        begin
          ByteOff := 0;
          if AArgCount > 1 then
            ByteOff := Trunc(CoerceToNumber(
              PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^));
          { Alignment check }
          if (BytesPerElement(AKind) > 1) and (ByteOff mod BytesPerElement(AKind) <> 0) then
            ThrowRangeErrorNative('Start offset of typed array should be a multiple of ' + IntToStr(BytesPerElement(AKind)));
          if ByteOff > SrcAB.ByteLength then
            ThrowRangeErrorNative('Start offset is outside the bounds of the buffer');
          if AArgCount > 2 then
          begin
            Len := Trunc(CoerceToNumber(
              PSouffleValue(PByte(AArgs) + 2 * SizeOf(TSouffleValue))^));
            if ByteOff + Len * BytesPerElement(AKind) > SrcAB.ByteLength then
              ThrowRangeErrorNative('Invalid typed array length');
          end
          else
          begin
            if (SrcAB.ByteLength - ByteOff) mod BytesPerElement(AKind) <> 0 then
              ThrowRangeErrorNative('Byte length of typed array should be a multiple of ' + IntToStr(BytesPerElement(AKind)));
            Len := (SrcAB.ByteLength - ByteOff) div BytesPerElement(AKind);
          end;
          if Len < 0 then Len := 0;
          TA := TSouffleTypedArray.Create(SrcAB, ByteOff, Len, AKind);
          TA.BufferRecord := Arg; { preserve original buffer record for identity }
          if Assigned(GC) then GC.AllocateObject(TA);
          AB := SrcAB;
        end
        { Case 4: new TypedArray(array) }
        else if Arg.AsReference is TSouffleArray then
        begin
          Arr := TSouffleArray(Arg.AsReference);
          Len := Arr.Count;
          ByteLen := Len * BytesPerElement(AKind);
          AB := TSouffleArrayBuffer.Create(ByteLen);
          if Assigned(GC) then GC.AllocateObject(AB);
          TA := TSouffleTypedArray.Create(AB, 0, Len, AKind);
          if Assigned(GC) then GC.AllocateObject(TA);
          for I := 0 to Len - 1 do
            TA.WriteElement(I, CoerceToNumber(Arr.Get(I)));
        end
        else
        begin
          { Fallback: treat as length 0 }
          AB := TSouffleArrayBuffer.Create(0);
          if Assigned(GC) then GC.AllocateObject(AB);
          TA := TSouffleTypedArray.Create(AB, 0, 0, AKind);
          if Assigned(GC) then GC.AllocateObject(TA);
        end;
      end;
    end
    else
    begin
      AB := TSouffleArrayBuffer.Create(0);
      if Assigned(GC) then GC.AllocateObject(AB);
      TA := TSouffleTypedArray.Create(AB, 0, 0, AKind);
      if Assigned(GC) then GC.AllocateObject(TA);
    end;
  end;

  Rec := TSouffleRecord.CreateFromBlueprint(FTypedArrayBlueprints[AKind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := FTypedArrayBlueprints[AKind].Prototype;
  Rec.SetSlot(0, SouffleReference(TA));
  Result := SouffleReference(Rec);
end;

{ Native Map/Set blueprint construction }

function TGocciaRuntimeOperations.ConstructNativeMap(const AArgs: PSouffleValue;
  const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  SM: TGocciaSouffleMap;
  Iterable, EntryVal: TSouffleValue;
  Arr, EntryArr: TSouffleArray;
  Done: Boolean;
  IterObj: TSouffleValue;
  SrcMap: TGocciaSouffleMap;
  I: Integer;
begin
  SM := TGocciaSouffleMap.Create(4);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(SM);

  Rec := TSouffleRecord.CreateFromBlueprint(FMapBlueprint);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Delegate := FMapBlueprint.Prototype;
  Rec.SetSlot(0, SouffleReference(SM));
  Result := SouffleReference(Rec);

  if AArgCount >= 1 then
  begin
    Iterable := AArgs^;
    if not SouffleIsReference(Iterable) or not Assigned(Iterable.AsReference) then
      Exit;
    if SouffleIsNil(Iterable) then
      Exit;

    { Fast path: copy from another blueprint Map }
    if (Iterable.AsReference is TSouffleRecord) and
       Assigned(TSouffleRecord(Iterable.AsReference).Blueprint) and
       (TSouffleRecord(Iterable.AsReference).Blueprint = FMapBlueprint) then
    begin
      SrcMap := TGocciaSouffleMap(
        TSouffleRecord(Iterable.AsReference).GetSlot(0).AsReference);
      for I := 0 to SrcMap.Count - 1 do
        SM.SetEntry(SrcMap.GetKeyAt(I), SrcMap.GetValueAt(I));
      Exit;
    end;

    { Fast path: copy from wrapped Goccia Map }
    if (Iterable.AsReference is TGocciaWrappedValue) and
       (TGocciaWrappedValue(Iterable.AsReference).Value is TGocciaMapValue) then
    begin
      SrcMap := ConvertGocciaMapToSouffle(
        TGocciaMapValue(TGocciaWrappedValue(Iterable.AsReference).Value),
        Self, nil);
      for I := 0 to SrcMap.Count - 1 do
        SM.SetEntry(SrcMap.GetKeyAt(I), SrcMap.GetValueAt(I));
      Exit;
    end;

    { Fast path: Array of [key, value] pairs }
    if Iterable.AsReference is TSouffleArray then
    begin
      Arr := TSouffleArray(Iterable.AsReference);
      for I := 0 to Arr.Count - 1 do
      begin
        EntryVal := Arr.Get(I);
        if SouffleIsReference(EntryVal) and Assigned(EntryVal.AsReference) and
           (EntryVal.AsReference is TSouffleArray) then
        begin
          EntryArr := TSouffleArray(EntryVal.AsReference);
          if EntryArr.Count >= 2 then
            SM.SetEntry(EntryArr.Get(0), EntryArr.Get(1))
          else if EntryArr.Count = 1 then
            SM.SetEntry(EntryArr.Get(0), SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
        end;
      end;
      Exit;
    end;

    { General iterable path }
    IterObj := GetIterator(Iterable, False);
    if SouffleIsReference(IterObj) and Assigned(IterObj.AsReference) then
    begin
      repeat
        EntryVal := IteratorNext(IterObj, Done);
        if not Done then
        begin
          if SouffleIsReference(EntryVal) and Assigned(EntryVal.AsReference) and
             (EntryVal.AsReference is TSouffleArray) then
          begin
            EntryArr := TSouffleArray(EntryVal.AsReference);
            if EntryArr.Count >= 2 then
              SM.SetEntry(EntryArr.Get(0), EntryArr.Get(1));
          end;
        end;
      until Done;
    end;
  end;
end;

function TGocciaRuntimeOperations.ConstructNativeSet(const AArgs: PSouffleValue;
  const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  SS: TGocciaSouffleSet;
  Iterable, ItemVal: TSouffleValue;
  Arr: TSouffleArray;
  Done: Boolean;
  IterObj: TSouffleValue;
  SrcSet: TGocciaSouffleSet;
  I: Integer;
begin
  SS := TGocciaSouffleSet.Create(4);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(SS);

  Rec := TSouffleRecord.CreateFromBlueprint(FSetBlueprint);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Delegate := FSetBlueprint.Prototype;
  Rec.SetSlot(0, SouffleReference(SS));
  Result := SouffleReference(Rec);

  if AArgCount >= 1 then
  begin
    Iterable := AArgs^;
    if not SouffleIsReference(Iterable) or not Assigned(Iterable.AsReference) then
      Exit;
    if SouffleIsNil(Iterable) then
      Exit;

    { Fast path: copy from another blueprint Set }
    if (Iterable.AsReference is TSouffleRecord) and
       Assigned(TSouffleRecord(Iterable.AsReference).Blueprint) and
       (TSouffleRecord(Iterable.AsReference).Blueprint = FSetBlueprint) then
    begin
      SrcSet := TGocciaSouffleSet(
        TSouffleRecord(Iterable.AsReference).GetSlot(0).AsReference);
      for I := 0 to SrcSet.Count - 1 do
        SS.Add(SrcSet.GetItemAt(I));
      Exit;
    end;

    { Fast path: copy from wrapped Goccia Set }
    if (Iterable.AsReference is TGocciaWrappedValue) and
       (TGocciaWrappedValue(Iterable.AsReference).Value is TGocciaSetValue) then
    begin
      SrcSet := ConvertGocciaSetToSouffle(
        TGocciaSetValue(TGocciaWrappedValue(Iterable.AsReference).Value),
        Self, nil);
      for I := 0 to SrcSet.Count - 1 do
        SS.Add(SrcSet.GetItemAt(I));
      Exit;
    end;

    { Fast path: Array }
    if Iterable.AsReference is TSouffleArray then
    begin
      Arr := TSouffleArray(Iterable.AsReference);
      for I := 0 to Arr.Count - 1 do
        SS.Add(Arr.Get(I));
      Exit;
    end;

    { General iterable path }
    IterObj := GetIterator(Iterable, False);
    if SouffleIsReference(IterObj) and Assigned(IterObj.AsReference) then
    begin
      repeat
        ItemVal := IteratorNext(IterObj, Done);
        if not Done then
          SS.Add(ItemVal);
      until Done;
    end;
  end;
end;

{ Native Map delegate methods }

function GetSouffleMap(const AReceiver: TSouffleValue): TGocciaSouffleMap;
var
  Rec: TSouffleRecord;
  Slot0: TSouffleValue;
begin
  Result := nil;
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then
    Exit;
  { Blueprint record path: slot 0 holds TGocciaSouffleMap }
  if AReceiver.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Assigned(Rec.Blueprint) and (Rec.Blueprint.SlotCount > 0) then
    begin
      Slot0 := Rec.GetSlot(0);
      if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) and
         (Slot0.AsReference is TGocciaSouffleMap) then
        Result := TGocciaSouffleMap(Slot0.AsReference);
    end;
  end
  { Legacy direct reference path }
  else if AReceiver.AsReference is TGocciaSouffleMap then
    Result := TGocciaSouffleMap(AReceiver.AsReference);
end;

function GetSouffleSet(const AReceiver: TSouffleValue): TGocciaSouffleSet;
var
  Rec: TSouffleRecord;
  Slot0: TSouffleValue;
begin
  Result := nil;
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then
    Exit;
  { Blueprint record path: slot 0 holds TGocciaSouffleSet }
  if AReceiver.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Assigned(Rec.Blueprint) and (Rec.Blueprint.SlotCount > 0) then
    begin
      Slot0 := Rec.GetSlot(0);
      if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) and
         (Slot0.AsReference is TGocciaSouffleSet) then
        Result := TGocciaSouffleSet(Slot0.AsReference);
    end;
  end
  { Legacy direct reference path }
  else if AReceiver.AsReference is TGocciaSouffleSet then
    Result := TGocciaSouffleSet(AReceiver.AsReference);
end;

function GetSoufflePromise(const AReceiver: TSouffleValue): TSoufflePromise;
var
  Rec: TSouffleRecord;
  Slot0: TSouffleValue;
begin
  Result := nil;
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then
    Exit;
  if AReceiver.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Assigned(Rec.Blueprint) and (Rec.Blueprint.SlotCount > 0) then
    begin
      Slot0 := Rec.GetSlot(0);
      if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) and
         (Slot0.AsReference is TSoufflePromise) then
        Result := TSoufflePromise(Slot0.AsReference);
    end;
  end
  else if AReceiver.AsReference is TSoufflePromise then
    Result := TSoufflePromise(AReceiver.AsReference);
end;

function GetSouffleArrayBuffer(const AReceiver: TSouffleValue): TSouffleArrayBuffer;
var
  Rec: TSouffleRecord;
  Slot0: TSouffleValue;
begin
  Result := nil;
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then Exit;
  if AReceiver.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Assigned(Rec.Blueprint) and (Rec.Blueprint.SlotCount > 0) then
    begin
      Slot0 := Rec.GetSlot(0);
      if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) and
         (Slot0.AsReference is TSouffleArrayBuffer) then
        Result := TSouffleArrayBuffer(Slot0.AsReference);
    end;
  end;
end;

function GetSouffleTypedArray(const AReceiver: TSouffleValue): TSouffleTypedArray;
var
  Rec: TSouffleRecord;
  Slot0: TSouffleValue;
begin
  Result := nil;
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then Exit;
  if AReceiver.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Assigned(Rec.Blueprint) and (Rec.Blueprint.SlotCount > 0) then
    begin
      Slot0 := Rec.GetSlot(0);
      if SouffleIsReference(Slot0) and Assigned(Slot0.AsReference) and
         (Slot0.AsReference is TSouffleTypedArray) then
        Result := TSouffleTypedArray(Slot0.AsReference);
    end;
  end;
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
var
  SM: TGocciaSouffleMap;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    Result := SouffleReference(TGocciaSouffleMapIterator.Create(SM, mikKeys));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
    Exit;
  end;
  Result := InvokeGocciaMethod(AReceiver, 'keys', AArgs, AArgCount);
end;

function NativeMapValues(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    Result := SouffleReference(TGocciaSouffleMapIterator.Create(SM, mikValues));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
    Exit;
  end;
  Result := InvokeGocciaMethod(AReceiver, 'values', AArgs, AArgCount);
end;

function NativeMapEntries(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) then
  begin
    Result := SouffleReference(TGocciaSouffleMapIterator.Create(SM, mikEntries));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
    Exit;
  end;
  Result := InvokeGocciaMethod(AReceiver, 'entries', AArgs, AArgCount);
end;

{ Native Map/Set iterator delegate methods }

function NativeMapIteratorNext(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Done: Boolean;
  Value: TSouffleValue;
  Rec: TSouffleRecord;
begin
  Done := True;
  Value := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) then
  begin
    if AReceiver.AsReference is TGocciaSouffleMapIterator then
      Value := TGocciaSouffleMapIterator(AReceiver.AsReference).Next(Done)
    else if AReceiver.AsReference is TSouffleTypedArrayIterator then
      Value := TSouffleTypedArrayIterator(AReceiver.AsReference).Next(Done)
    else
      Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    Rec := TSouffleRecord.Create(2);
    if Assigned(GNativeArrayJoinRuntime) and Assigned(GNativeArrayJoinRuntime.VM) then
      Rec.Delegate := GNativeArrayJoinRuntime.VM.RecordDelegate;
    Rec.Put(PROP_VALUE, Value);
    Rec.Put(PROP_DONE, SouffleBoolean(Done));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Rec);
    Result := SouffleReference(Rec);
  end
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeSetIteratorNext(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Iter: TGocciaSouffleSetIterator;
  Done: Boolean;
  Value: TSouffleValue;
  Rec: TSouffleRecord;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TGocciaSouffleSetIterator) then
  begin
    Iter := TGocciaSouffleSetIterator(AReceiver.AsReference);
    Value := Iter.Next(Done);
    Rec := TSouffleRecord.Create(2);
    if Assigned(GNativeArrayJoinRuntime) and Assigned(GNativeArrayJoinRuntime.VM) then
      Rec.Delegate := GNativeArrayJoinRuntime.VM.RecordDelegate;
    Rec.Put(PROP_VALUE, Value);
    Rec.Put(PROP_DONE, SouffleBoolean(Done));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Rec);
    Result := SouffleReference(Rec);
  end
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeMapConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
begin
  SM := GetSouffleMap(AReceiver);
  if Assigned(SM) and (AArgCount >= 1) then
    GNativeArrayJoinRuntime.PopulateMapFromIterable(SM, AArgs^);
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeSetConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SS: TGocciaSouffleSet;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) and (AArgCount >= 1) then
    GNativeArrayJoinRuntime.PopulateSetFromIterable(SS, AArgs^);
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeMapGroupBy(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Iterable, Callback, Item, Key: TSouffleValue;
  SM: TGocciaSouffleMap;
  GroupArr: TSouffleArray;
  IterObj: TSouffleValue;
  Done: Boolean;
  CallArgs: array[0..1] of TSouffleValue;
  Idx, Index: Integer;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
begin
  if AArgCount < 2 then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Iterable := AArgs^;
  Callback := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;

  Bp := GNativeArrayJoinRuntime.FMapBlueprint;
  SM := TGocciaSouffleMap.Create(4);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(SM);

  Rec := TSouffleRecord.CreateFromBlueprint(Bp);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Delegate := Bp.Prototype;
  Rec.SetSlot(0, SouffleReference(SM));
  Result := SouffleReference(Rec);

  IterObj := GNativeArrayJoinRuntime.GetIterator(Iterable, False);
  Index := 0;
  repeat
    Item := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
    if Done then Break;
    CallArgs[0] := Item;
    CallArgs[1] := SouffleInteger(Index);
    Key := GNativeArrayJoinRuntime.Invoke(Callback, @CallArgs[0], 2, SouffleNil);
    Idx := SM.FindEntry(Key);
    if Idx >= 0 then
    begin
      GroupArr := TSouffleArray(SM.GetValueAt(Idx).AsReference);
      GroupArr.Push(Item);
    end
    else
    begin
      GroupArr := TSouffleArray.Create(4);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(GroupArr);
      GroupArr.Push(Item);
      SM.SetEntry(Key, SouffleReference(GroupArr));
    end;
    Inc(Index);
  until False;
end;

function NativeMapGetOrInsert(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
  Key, DefaultVal: TSouffleValue;
  Idx: Integer;
begin
  SM := GetSouffleMap(AReceiver);
  if not Assigned(SM) or (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Key := AArgs^;
  Idx := SM.FindEntry(Key);
  if Idx >= 0 then
    Exit(SM.GetValueAt(Idx));
  if AArgCount >= 2 then
    DefaultVal := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^
  else
    DefaultVal := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  SM.SetEntry(Key, DefaultVal);
  Result := DefaultVal;
end;

function NativeMapGetOrInsertComputed(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SM: TGocciaSouffleMap;
  Key, Callback, ComputedVal: TSouffleValue;
  Idx: Integer;
  CallArgs: array[0..0] of TSouffleValue;
begin
  SM := GetSouffleMap(AReceiver);
  if not Assigned(SM) or (AArgCount < 2) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Key := AArgs^;
  Callback := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
  Idx := SM.FindEntry(Key);
  if Idx >= 0 then
    Exit(SM.GetValueAt(Idx));
  CallArgs[0] := Key;
  ComputedVal := GNativeArrayJoinRuntime.Invoke(Callback, @CallArgs[0], 1, SouffleNil);
  SM.SetEntry(Key, ComputedVal);
  Result := ComputedVal;
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
var
  SS: TGocciaSouffleSet;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) then
  begin
    Result := SouffleReference(TGocciaSouffleSetIterator.Create(SS, sikValues));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
    Exit;
  end;
  Result := InvokeGocciaMethod(AReceiver, 'values', AArgs, AArgCount);
end;

function NativeSetEntries(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SS: TGocciaSouffleSet;
begin
  SS := GetSouffleSet(AReceiver);
  if Assigned(SS) then
  begin
    Result := SouffleReference(TGocciaSouffleSetIterator.Create(SS, sikEntries));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
    Exit;
  end;
  Result := InvokeGocciaMethod(AReceiver, 'entries', AArgs, AArgCount);
end;

function CreateBlueprintSet(const AItems: TGocciaSouffleSet = nil): TSouffleValue;
var
  SS: TGocciaSouffleSet;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
begin
  Bp := GNativeArrayJoinRuntime.FSetBlueprint;
  if AItems <> nil then
    SS := AItems
  else
  begin
    SS := TGocciaSouffleSet.Create(4);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(SS);
  end;
  Rec := TSouffleRecord.CreateFromBlueprint(Bp);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  Rec.Delegate := Bp.Prototype;
  Rec.SetSlot(0, SouffleReference(SS));
  Result := SouffleReference(Rec);
end;

function NativeSetKeys(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := NativeSetValues(AReceiver, AArgs, AArgCount);
end;

function NativeSetUnion(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SelfSet, OtherSet: TGocciaSouffleSet;
  NewSet: TGocciaSouffleSet;
  I: Integer;
begin
  SelfSet := GetSouffleSet(AReceiver);
  if not Assigned(SelfSet) or (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  OtherSet := GetSouffleSet(AArgs^);
  if not Assigned(OtherSet) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  NewSet := TGocciaSouffleSet.Create(SelfSet.Count + OtherSet.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewSet);
  for I := 0 to SelfSet.Count - 1 do
    NewSet.Add(SelfSet.GetItemAt(I));
  for I := 0 to OtherSet.Count - 1 do
    NewSet.Add(OtherSet.GetItemAt(I));
  Result := CreateBlueprintSet(NewSet);
end;

function NativeSetIntersection(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SelfSet, OtherSet: TGocciaSouffleSet;
  NewSet: TGocciaSouffleSet;
  I: Integer;
begin
  SelfSet := GetSouffleSet(AReceiver);
  if not Assigned(SelfSet) or (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  OtherSet := GetSouffleSet(AArgs^);
  if not Assigned(OtherSet) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  NewSet := TGocciaSouffleSet.Create(SelfSet.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewSet);
  for I := 0 to SelfSet.Count - 1 do
    if OtherSet.Contains(SelfSet.GetItemAt(I)) then
      NewSet.Add(SelfSet.GetItemAt(I));
  Result := CreateBlueprintSet(NewSet);
end;

function NativeSetDifference(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SelfSet, OtherSet: TGocciaSouffleSet;
  NewSet: TGocciaSouffleSet;
  I: Integer;
begin
  SelfSet := GetSouffleSet(AReceiver);
  if not Assigned(SelfSet) or (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  OtherSet := GetSouffleSet(AArgs^);
  if not Assigned(OtherSet) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  NewSet := TGocciaSouffleSet.Create(SelfSet.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewSet);
  for I := 0 to SelfSet.Count - 1 do
    if not OtherSet.Contains(SelfSet.GetItemAt(I)) then
      NewSet.Add(SelfSet.GetItemAt(I));
  Result := CreateBlueprintSet(NewSet);
end;

function NativeSetSymmetricDifference(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SelfSet, OtherSet: TGocciaSouffleSet;
  NewSet: TGocciaSouffleSet;
  I: Integer;
begin
  SelfSet := GetSouffleSet(AReceiver);
  if not Assigned(SelfSet) or (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  OtherSet := GetSouffleSet(AArgs^);
  if not Assigned(OtherSet) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  NewSet := TGocciaSouffleSet.Create(SelfSet.Count + OtherSet.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NewSet);
  for I := 0 to SelfSet.Count - 1 do
    if not OtherSet.Contains(SelfSet.GetItemAt(I)) then
      NewSet.Add(SelfSet.GetItemAt(I));
  for I := 0 to OtherSet.Count - 1 do
    if not SelfSet.Contains(OtherSet.GetItemAt(I)) then
      NewSet.Add(OtherSet.GetItemAt(I));
  Result := CreateBlueprintSet(NewSet);
end;

function NativeSetIsSubsetOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SelfSet, OtherSet: TGocciaSouffleSet;
  I: Integer;
begin
  SelfSet := GetSouffleSet(AReceiver);
  if not Assigned(SelfSet) or (AArgCount < 1) then
    Exit(SouffleBoolean(False));
  OtherSet := GetSouffleSet(AArgs^);
  if not Assigned(OtherSet) then
    Exit(SouffleBoolean(False));
  for I := 0 to SelfSet.Count - 1 do
    if not OtherSet.Contains(SelfSet.GetItemAt(I)) then
      Exit(SouffleBoolean(False));
  Result := SouffleBoolean(True);
end;

function NativeSetIsSupersetOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SelfSet, OtherSet: TGocciaSouffleSet;
  I: Integer;
begin
  SelfSet := GetSouffleSet(AReceiver);
  if not Assigned(SelfSet) or (AArgCount < 1) then
    Exit(SouffleBoolean(False));
  OtherSet := GetSouffleSet(AArgs^);
  if not Assigned(OtherSet) then
    Exit(SouffleBoolean(False));
  for I := 0 to OtherSet.Count - 1 do
    if not SelfSet.Contains(OtherSet.GetItemAt(I)) then
      Exit(SouffleBoolean(False));
  Result := SouffleBoolean(True);
end;

function NativeSetIsDisjointFrom(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SelfSet, OtherSet: TGocciaSouffleSet;
  I: Integer;
begin
  SelfSet := GetSouffleSet(AReceiver);
  if not Assigned(SelfSet) or (AArgCount < 1) then
    Exit(SouffleBoolean(True));
  OtherSet := GetSouffleSet(AArgs^);
  if not Assigned(OtherSet) then
    Exit(SouffleBoolean(True));
  for I := 0 to SelfSet.Count - 1 do
    if OtherSet.Contains(SelfSet.GetItemAt(I)) then
      Exit(SouffleBoolean(False));
  Result := SouffleBoolean(True);
end;

{ Native Promise delegate helpers }

{ Native Promise prototype delegate callbacks }

function NativePromiseThen(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SP: TSoufflePromise;
  OnFulfilled, OnRejected: TSouffleValue;
  ChildPromise: TSoufflePromise;
begin
  SP := GetSoufflePromise(AReceiver);
  if Assigned(SP) then
  begin
    OnFulfilled := SouffleNil;
    OnRejected := SouffleNil;
    if AArgCount > 0 then OnFulfilled := AArgs^;
    if AArgCount > 1 then
      OnRejected := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
    { Only pass callable values }
    if not (SouffleIsReference(OnFulfilled) and Assigned(OnFulfilled.AsReference)) then
      OnFulfilled := SouffleNil;
    if not (SouffleIsReference(OnRejected) and Assigned(OnRejected.AsReference)) then
      OnRejected := SouffleNil;
    ChildPromise := SP.InvokeThen(OnFulfilled, OnRejected);
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ChildPromise);
    Exit;
  end;
  { Fallback for TGocciaPromiseValue wrapped values }
  Result := InvokeGocciaMethod(AReceiver, 'then', AArgs, AArgCount);
end;

function NativePromiseCatch(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SP: TSoufflePromise;
  OnRejected: TSouffleValue;
  ChildPromise: TSoufflePromise;
begin
  SP := GetSoufflePromise(AReceiver);
  if Assigned(SP) then
  begin
    OnRejected := SouffleNil;
    if AArgCount > 0 then OnRejected := AArgs^;
    if not (SouffleIsReference(OnRejected) and Assigned(OnRejected.AsReference)) then
      OnRejected := SouffleNil;
    ChildPromise := SP.InvokeThen(SouffleNil, OnRejected);
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ChildPromise);
    Exit;
  end;
  Result := InvokeGocciaMethod(AReceiver, 'catch', AArgs, AArgCount);
end;

function NativePromiseFinally(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SP: TSoufflePromise;
  OnFinally: TSouffleValue;
  ChildPromise: TSoufflePromise;
begin
  SP := GetSoufflePromise(AReceiver);
  if Assigned(SP) then
  begin
    OnFinally := SouffleNil;
    if AArgCount > 0 then OnFinally := AArgs^;
    if not (SouffleIsReference(OnFinally) and Assigned(OnFinally.AsReference)) then
    begin
      { No callable — pass through }
      ChildPromise := SP.InvokeThen(SouffleNil, SouffleNil);
      Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ChildPromise);
      Exit;
    end;
    { TODO: Full finally semantics with passthrough wrappers }
    { For now, delegate to InvokeThen with the callback for both paths }
    ChildPromise := SP.InvokeThen(OnFinally, OnFinally);
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ChildPromise);
    Exit;
  end;
  Result := InvokeGocciaMethod(AReceiver, 'finally', AArgs, AArgCount);
end;

{ Native Promise static delegate callbacks }

function NativePromiseStaticResolve(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arg: TSouffleValue;
  SP: TSoufflePromise;
begin
  if AArgCount > 0 then
    Arg := AArgs^
  else
    Arg := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);

  { If already a promise blueprint record, return as-is }
  SP := GetSoufflePromise(Arg);
  if Assigned(SP) then
    Exit(Arg);

  SP := TSoufflePromise.Create;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(SP);
  SP.Resolve(Arg);
  Result := GNativeArrayJoinRuntime.WrapSoufflePromise(SP);
end;

function NativePromiseStaticReject(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  SP: TSoufflePromise;
begin
  SP := TSoufflePromise.Create;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(SP);
  if AArgCount > 0 then
    SP.Reject(AArgs^)
  else
    SP.Reject(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Result := GNativeArrayJoinRuntime.WrapSoufflePromise(SP);
end;

{ Promise.all per-element resolve callback }
function PromiseAllResolveCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  CtxRec: TSouffleRecord;
  State: TSoufflePromiseAllState;
  StateVal, IdxVal: TSouffleValue;
  Idx: Integer;
  Val: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AContext) or not (AContext.AsReference is TSouffleRecord) then Exit;
  CtxRec := TSouffleRecord(AContext.AsReference);
  if not CtxRec.Get('state', StateVal) then Exit;
  if not CtxRec.Get('index', IdxVal) then Exit;
  if not SouffleIsReference(StateVal) or not (StateVal.AsReference is TSoufflePromiseAllState) then Exit;
  State := TSoufflePromiseAllState(StateVal.AsReference);
  Idx := Integer(IdxVal.AsInteger);
  if State.Settled then Exit;

  if AArgCount > 0 then Val := AArgs^ else Val := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  State.Results.Put(Idx, Val);
  Dec(State.Remaining);
  if State.Remaining = 0 then
  begin
    State.Settled := True;
    State.ResultPromise.Resolve(SouffleReference(State.Results));
  end;
end;

{ Promise.all/race reject callback — rejects result promise }
function PromiseAllRejectCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  State: TSoufflePromiseAllState;
  Val: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AContext) or not (AContext.AsReference is TSoufflePromiseAllState) then Exit;
  State := TSoufflePromiseAllState(AContext.AsReference);
  if State.Settled then Exit;
  State.Settled := True;
  if AArgCount > 0 then Val := AArgs^ else Val := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  State.ResultPromise.Reject(Val);
end;

{ Promise.race resolve callback — resolves result promise with first value }
function PromiseRaceResolveCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  State: TSoufflePromiseAllState;
  Val: TSouffleValue;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AContext) or not (AContext.AsReference is TSoufflePromiseAllState) then Exit;
  State := TSoufflePromiseAllState(AContext.AsReference);
  if State.Settled then Exit;
  State.Settled := True;
  if AArgCount > 0 then Val := AArgs^ else Val := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  State.ResultPromise.Resolve(Val);
end;

{ Helper: wrap value as promise (resolve non-promise, return promise as-is) }
function WrapAsNativePromise(const AValue: TSouffleValue): TSoufflePromise;
var
  GC: TGarbageCollector;
begin
  Result := GetSoufflePromise(AValue);
  if Assigned(Result) then Exit;
  GC := TGarbageCollector.Instance;
  Result := TSoufflePromise.Create;
  if Assigned(GC) then GC.AllocateObject(Result);
  Result.Resolve(AValue);
end;

function NativePromiseStaticAll(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Iterable, ItemVal: TSouffleValue;
  IterObj: TSouffleValue;
  Items: array of TSouffleValue;
  ItemCount, I: Integer;
  Done: Boolean;
  ResultP: TSoufflePromise;
  State: TSoufflePromiseAllState;
  WrappedP: TSoufflePromise;
  ResolveCtx: TSouffleRecord;
  ResolveFn, RejectFn: TSouffleNativeClosure;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  ResultP := TSoufflePromise.Create;
  if Assigned(GC) then GC.AllocateObject(ResultP);

  if AArgCount < 1 then
  begin
    ResultP.Reject(SouffleNil);
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
    Exit;
  end;

  Iterable := AArgs^;
  { Collect items from iterable }
  ItemCount := 0;
  SetLength(Items, 8);
  try
    IterObj := GNativeArrayJoinRuntime.GetIterator(Iterable, False);
    repeat
      ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
      if not Done then
      begin
        if ItemCount >= Length(Items) then
          SetLength(Items, ItemCount * 2 + 4);
        Items[ItemCount] := ItemVal;
        Inc(ItemCount);
      end;
    until Done;
  except
    on E: ESouffleThrow do
    begin
      ResultP.Reject(E.ThrownValue);
      Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
      Exit;
    end;
  end;

  if ItemCount = 0 then
  begin
    ResultP.Resolve(SouffleReference(TSouffleArray.Create(0)));
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
    Exit;
  end;

  State := TSoufflePromiseAllState.Create(0);
  if Assigned(GC) then GC.AllocateObject(State);
  State.Results := TSouffleArray.Create(ItemCount);
  if Assigned(GC) then GC.AllocateObject(State.Results);
  { Pre-fill with undefined }
  for I := 0 to ItemCount - 1 do
    State.Results.Push(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  State.Remaining := ItemCount;
  State.ResultPromise := ResultP;
  State.Settled := False;

  for I := 0 to ItemCount - 1 do
  begin
    WrappedP := WrapAsNativePromise(Items[I]);

    { Create context record with state ref and index }
    ResolveCtx := TSouffleRecord.Create(2);
    if Assigned(GC) then GC.AllocateObject(ResolveCtx);
    ResolveCtx.Put('state', SouffleReference(State));
    ResolveCtx.Put('index', SouffleInteger(I));

    ResolveFn := TSouffleNativeClosure.Create('all-resolve', 1,
      @PromiseAllResolveCallback, SouffleReference(ResolveCtx));
    if Assigned(GC) then GC.AllocateObject(ResolveFn);

    RejectFn := TSouffleNativeClosure.Create('all-reject', 1,
      @PromiseAllRejectCallback, SouffleReference(State));
    if Assigned(GC) then GC.AllocateObject(RejectFn);

    WrappedP.InvokeThen(SouffleReference(ResolveFn), SouffleReference(RejectFn));
  end;

  Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
end;

{ Promise.allSettled per-element fulfill callback }
function PromiseAllSettledFulfillCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  CtxRec: TSouffleRecord;
  State: TSoufflePromiseAllState;
  StateVal, IdxVal: TSouffleValue;
  Idx: Integer;
  Entry: TSouffleRecord;
  GC: TGarbageCollector;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AContext) or not (AContext.AsReference is TSouffleRecord) then Exit;
  CtxRec := TSouffleRecord(AContext.AsReference);
  if not CtxRec.Get('state', StateVal) then Exit;
  if not CtxRec.Get('index', IdxVal) then Exit;
  if not SouffleIsReference(StateVal) or not (StateVal.AsReference is TSoufflePromiseAllState) then Exit;
  State := TSoufflePromiseAllState(StateVal.AsReference);
  Idx := Integer(IdxVal.AsInteger);
  if State.Settled then Exit;
  GC := TGarbageCollector.Instance;

  Entry := TSouffleRecord.Create(2);
  if Assigned(GC) then GC.AllocateObject(Entry);
  Entry.Put('status', SouffleString('fulfilled'));
  if AArgCount > 0 then Entry.Put('value', AArgs^)
  else Entry.Put('value', SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));

  State.Results.Put(Idx, SouffleReference(Entry));
  Dec(State.Remaining);
  if State.Remaining = 0 then
  begin
    State.Settled := True;
    State.ResultPromise.Resolve(SouffleReference(State.Results));
  end;
end;

{ Promise.allSettled per-element reject callback }
function PromiseAllSettledRejectCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  CtxRec: TSouffleRecord;
  State: TSoufflePromiseAllState;
  StateVal, IdxVal: TSouffleValue;
  Idx: Integer;
  Entry: TSouffleRecord;
  GC: TGarbageCollector;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AContext) or not (AContext.AsReference is TSouffleRecord) then Exit;
  CtxRec := TSouffleRecord(AContext.AsReference);
  if not CtxRec.Get('state', StateVal) then Exit;
  if not CtxRec.Get('index', IdxVal) then Exit;
  if not SouffleIsReference(StateVal) or not (StateVal.AsReference is TSoufflePromiseAllState) then Exit;
  State := TSoufflePromiseAllState(StateVal.AsReference);
  Idx := Integer(IdxVal.AsInteger);
  if State.Settled then Exit;
  GC := TGarbageCollector.Instance;

  Entry := TSouffleRecord.Create(2);
  if Assigned(GC) then GC.AllocateObject(Entry);
  Entry.Put('status', SouffleString('rejected'));
  if AArgCount > 0 then Entry.Put('reason', AArgs^)
  else Entry.Put('reason', SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));

  State.Results.Put(Idx, SouffleReference(Entry));
  Dec(State.Remaining);
  if State.Remaining = 0 then
  begin
    State.Settled := True;
    State.ResultPromise.Resolve(SouffleReference(State.Results));
  end;
end;

function NativePromiseStaticAllSettled(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Iterable, ItemVal: TSouffleValue;
  IterObj: TSouffleValue;
  Items: array of TSouffleValue;
  ItemCount, I: Integer;
  Done: Boolean;
  ResultP: TSoufflePromise;
  State: TSoufflePromiseAllState;
  WrappedP: TSoufflePromise;
  Ctx: TSouffleRecord;
  FulfillFn, RejectFn: TSouffleNativeClosure;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  ResultP := TSoufflePromise.Create;
  if Assigned(GC) then GC.AllocateObject(ResultP);

  if AArgCount < 1 then
  begin
    ResultP.Resolve(SouffleReference(TSouffleArray.Create(0)));
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
    Exit;
  end;

  Iterable := AArgs^;
  ItemCount := 0;
  SetLength(Items, 8);
  try
    IterObj := GNativeArrayJoinRuntime.GetIterator(Iterable, False);
    repeat
      ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
      if not Done then
      begin
        if ItemCount >= Length(Items) then SetLength(Items, ItemCount * 2 + 4);
        Items[ItemCount] := ItemVal;
        Inc(ItemCount);
      end;
    until Done;
  except
    on E: ESouffleThrow do
    begin
      ResultP.Reject(E.ThrownValue);
      Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
      Exit;
    end;
  end;

  if ItemCount = 0 then
  begin
    ResultP.Resolve(SouffleReference(TSouffleArray.Create(0)));
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
    Exit;
  end;

  State := TSoufflePromiseAllState.Create(0);
  if Assigned(GC) then GC.AllocateObject(State);
  State.Results := TSouffleArray.Create(ItemCount);
  if Assigned(GC) then GC.AllocateObject(State.Results);
  for I := 0 to ItemCount - 1 do
    State.Results.Push(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  State.Remaining := ItemCount;
  State.ResultPromise := ResultP;
  State.Settled := False;

  for I := 0 to ItemCount - 1 do
  begin
    WrappedP := WrapAsNativePromise(Items[I]);

    Ctx := TSouffleRecord.Create(2);
    if Assigned(GC) then GC.AllocateObject(Ctx);
    Ctx.Put('state', SouffleReference(State));
    Ctx.Put('index', SouffleInteger(I));

    FulfillFn := TSouffleNativeClosure.Create('allSettled-fulfill', 1,
      @PromiseAllSettledFulfillCallback, SouffleReference(Ctx));
    if Assigned(GC) then GC.AllocateObject(FulfillFn);

    RejectFn := TSouffleNativeClosure.Create('allSettled-reject', 1,
      @PromiseAllSettledRejectCallback, SouffleReference(Ctx));
    if Assigned(GC) then GC.AllocateObject(RejectFn);

    WrappedP.InvokeThen(SouffleReference(FulfillFn), SouffleReference(RejectFn));
  end;

  Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
end;

function NativePromiseStaticRace(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Iterable, ItemVal: TSouffleValue;
  IterObj: TSouffleValue;
  Done: Boolean;
  ResultP: TSoufflePromise;
  State: TSoufflePromiseAllState;
  WrappedP: TSoufflePromise;
  ResolveFn, RejectFn: TSouffleNativeClosure;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  ResultP := TSoufflePromise.Create;
  if Assigned(GC) then GC.AllocateObject(ResultP);

  if AArgCount < 1 then
  begin
    ResultP.Reject(SouffleNil);
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
    Exit;
  end;

  State := TSoufflePromiseAllState.Create(0);
  if Assigned(GC) then GC.AllocateObject(State);
  State.ResultPromise := ResultP;
  State.Settled := False;

  Iterable := AArgs^;
  try
    IterObj := GNativeArrayJoinRuntime.GetIterator(Iterable, False);
    repeat
      ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
      if not Done then
      begin
        WrappedP := WrapAsNativePromise(ItemVal);
        ResolveFn := TSouffleNativeClosure.Create('race-resolve', 1,
          @PromiseRaceResolveCallback, SouffleReference(State));
        if Assigned(GC) then GC.AllocateObject(ResolveFn);
        RejectFn := TSouffleNativeClosure.Create('race-reject', 1,
          @PromiseAllRejectCallback, SouffleReference(State));
        if Assigned(GC) then GC.AllocateObject(RejectFn);
        WrappedP.InvokeThen(SouffleReference(ResolveFn), SouffleReference(RejectFn));
      end;
    until Done;
  except
    on E: ESouffleThrow do
    begin
      ResultP.Reject(E.ThrownValue);
      Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
      Exit;
    end;
  end;

  Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
end;

{ Promise.any per-element reject callback — collects errors }
function PromiseAnyRejectCallback(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer;
  const AContext: TSouffleValue): TSouffleValue;
var
  CtxRec: TSouffleRecord;
  State: TSoufflePromiseAllState;
  StateVal, IdxVal: TSouffleValue;
  Idx: Integer;
  ErrorRec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not SouffleIsReference(AContext) or not (AContext.AsReference is TSouffleRecord) then Exit;
  CtxRec := TSouffleRecord(AContext.AsReference);
  if not CtxRec.Get('state', StateVal) then Exit;
  if not CtxRec.Get('index', IdxVal) then Exit;
  if not SouffleIsReference(StateVal) or not (StateVal.AsReference is TSoufflePromiseAllState) then Exit;
  State := TSoufflePromiseAllState(StateVal.AsReference);
  Idx := Integer(IdxVal.AsInteger);
  if State.Settled then Exit;
  GC := TGarbageCollector.Instance;

  if AArgCount > 0 then
    State.Results.Put(Idx, AArgs^)
  else
    State.Results.Put(Idx, SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Dec(State.Remaining);

  if State.Remaining = 0 then
  begin
    State.Settled := True;
    ErrorRec := TSouffleRecord.Create(2);
    if Assigned(GC) then GC.AllocateObject(ErrorRec);
    ErrorRec.Put('message', SouffleString('All promises were rejected'));
    ErrorRec.Put('errors', SouffleReference(State.Results));
    ErrorRec.Put('name', SouffleString('AggregateError'));
    State.ResultPromise.Reject(SouffleReference(ErrorRec));
  end;
end;

function NativePromiseStaticAny(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Iterable, ItemVal: TSouffleValue;
  IterObj: TSouffleValue;
  Items: array of TSouffleValue;
  ItemCount, I: Integer;
  Done: Boolean;
  ResultP: TSoufflePromise;
  State: TSoufflePromiseAllState;
  WrappedP: TSoufflePromise;
  Ctx: TSouffleRecord;
  FulfillFn, RejectFn: TSouffleNativeClosure;
  ErrorRec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  ResultP := TSoufflePromise.Create;
  if Assigned(GC) then GC.AllocateObject(ResultP);

  if AArgCount < 1 then
  begin
    ErrorRec := TSouffleRecord.Create(2);
    if Assigned(GC) then GC.AllocateObject(ErrorRec);
    ErrorRec.Put('message', SouffleString('All promises were rejected'));
    ErrorRec.Put('errors', SouffleReference(TSouffleArray.Create(0)));
    ErrorRec.Put('name', SouffleString('AggregateError'));
    ResultP.Reject(SouffleReference(ErrorRec));
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
    Exit;
  end;

  Iterable := AArgs^;
  ItemCount := 0;
  SetLength(Items, 8);
  try
    IterObj := GNativeArrayJoinRuntime.GetIterator(Iterable, False);
    repeat
      ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
      if not Done then
      begin
        if ItemCount >= Length(Items) then SetLength(Items, ItemCount * 2 + 4);
        Items[ItemCount] := ItemVal;
        Inc(ItemCount);
      end;
    until Done;
  except
    on E: ESouffleThrow do
    begin
      ResultP.Reject(E.ThrownValue);
      Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
      Exit;
    end;
  end;

  if ItemCount = 0 then
  begin
    ErrorRec := TSouffleRecord.Create(2);
    if Assigned(GC) then GC.AllocateObject(ErrorRec);
    ErrorRec.Put('message', SouffleString('All promises were rejected'));
    ErrorRec.Put('errors', SouffleReference(TSouffleArray.Create(0)));
    ErrorRec.Put('name', SouffleString('AggregateError'));
    ResultP.Reject(SouffleReference(ErrorRec));
    Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
    Exit;
  end;

  { State: Results holds errors, first fulfill resolves }
  State := TSoufflePromiseAllState.Create(0);
  if Assigned(GC) then GC.AllocateObject(State);
  State.Results := TSouffleArray.Create(ItemCount);
  if Assigned(GC) then GC.AllocateObject(State.Results);
  for I := 0 to ItemCount - 1 do
    State.Results.Push(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  State.Remaining := ItemCount;
  State.ResultPromise := ResultP;
  State.Settled := False;

  for I := 0 to ItemCount - 1 do
  begin
    WrappedP := WrapAsNativePromise(Items[I]);

    { Fulfill: first one wins (same as race resolve) }
    FulfillFn := TSouffleNativeClosure.Create('any-fulfill', 1,
      @PromiseRaceResolveCallback, SouffleReference(State));
    if Assigned(GC) then GC.AllocateObject(FulfillFn);

    { Reject: collect errors }
    Ctx := TSouffleRecord.Create(2);
    if Assigned(GC) then GC.AllocateObject(Ctx);
    Ctx.Put('state', SouffleReference(State));
    Ctx.Put('index', SouffleInteger(I));

    RejectFn := TSouffleNativeClosure.Create('any-reject', 1,
      @PromiseAnyRejectCallback, SouffleReference(Ctx));
    if Assigned(GC) then GC.AllocateObject(RejectFn);

    WrappedP.InvokeThen(SouffleReference(FulfillFn), SouffleReference(RejectFn));
  end;

  Result := GNativeArrayJoinRuntime.WrapSoufflePromise(ResultP);
end;

function NativePromiseStaticWithResolvers(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  P: TSoufflePromise;
  ResolveFn, RejectFn: TSouffleNativeClosure;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  P := TSoufflePromise.Create;
  if Assigned(GC) then GC.AllocateObject(P);

  ResolveFn := TSouffleNativeClosure.Create('resolve', 1,
    @NativePromiseResolveCallback, SouffleReference(P));
  if Assigned(GC) then GC.AllocateObject(ResolveFn);
  RejectFn := TSouffleNativeClosure.Create('reject', 1,
    @NativePromiseRejectCallback, SouffleReference(P));
  if Assigned(GC) then GC.AllocateObject(RejectFn);

  Rec := TSouffleRecord.Create(3);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Put('promise', GNativeArrayJoinRuntime.WrapSoufflePromise(P));
  Rec.Put('resolve', SouffleReference(ResolveFn));
  Rec.Put('reject', SouffleReference(RejectFn));
  Result := SouffleReference(Rec);
end;

function NativePromiseStaticTry(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  P: TSoufflePromise;
  CallbackResult: TSouffleValue;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  P := TSoufflePromise.Create;
  if Assigned(GC) then GC.AllocateObject(P);

  if (AArgCount >= 1) and SouffleIsReference(AArgs^) and Assigned(AArgs^.AsReference) then
  begin
    try
      if AArgs^.AsReference is TSouffleClosure then
        CallbackResult := GNativeArrayJoinRuntime.VM.ExecuteFunction(
          TSouffleClosure(AArgs^.AsReference),
          [SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)])
      else if AArgs^.AsReference is TSouffleNativeFunction then
        CallbackResult := TSouffleNativeFunction(AArgs^.AsReference).Invoke(
          SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED), nil, 0)
      else if AArgs^.AsReference is TSouffleNativeClosure then
        CallbackResult := TSouffleNativeClosure(AArgs^.AsReference).Invoke(
          SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED), nil, 0)
      else
        CallbackResult := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
      P.Resolve(CallbackResult);
    except
      on E: ESouffleThrow do
        P.Reject(E.ThrownValue);
    end;
  end
  else
    P.Resolve(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));

  Result := GNativeArrayJoinRuntime.WrapSoufflePromise(P);
end;

{ Native ArrayBuffer delegate callbacks }

function NativeArrayBufferByteLength(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  AB: TSouffleArrayBuffer;
begin
  AB := GetSouffleArrayBuffer(AReceiver);
  if Assigned(AB) then
    Result := SouffleInteger(AB.ByteLength)
  else
    Result := SouffleInteger(0);
end;

function NativeArrayBufferSlice(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  AB, NewAB: TSouffleArrayBuffer;
  StartIdx, EndIdx, NewLen: Integer;
  RawStart, RawEnd: Double;
  Bp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  AB := GetSouffleArrayBuffer(AReceiver);
  if not Assigned(AB) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));

  { SharedArrayBuffer with zero length: throw TypeError }
  if AB.IsShared and (AB.ByteLength = 0) then
  begin
    GNativeArrayJoinRuntime.ThrowTypeErrorMessage(
      'Cannot slice a zero-length SharedArrayBuffer');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  GC := TGarbageCollector.Instance;
  StartIdx := 0;
  EndIdx := AB.ByteLength;

  if AArgCount > 0 then
  begin
    RawStart := GNativeArrayJoinRuntime.CoerceToNumber(AArgs^);
    if IsNan(RawStart) then
      StartIdx := 0
    else
    begin
      StartIdx := Trunc(RawStart);
      if StartIdx < 0 then StartIdx := AB.ByteLength + StartIdx;
      if StartIdx < 0 then StartIdx := 0;
      if StartIdx > AB.ByteLength then StartIdx := AB.ByteLength;
    end;
  end;
  if AArgCount > 1 then
  begin
    { Check if arg is undefined (svkNil with undefined flag) }
    if PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^.Kind = svkNil then
      { undefined end → keep default (byteLength) }
    else
    begin
      RawEnd := GNativeArrayJoinRuntime.CoerceToNumber(
        PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);
      if IsNan(RawEnd) then
        EndIdx := 0
      else
      begin
        EndIdx := Trunc(RawEnd);
        if EndIdx < 0 then EndIdx := AB.ByteLength + EndIdx;
        if EndIdx < 0 then EndIdx := 0;
        if EndIdx > AB.ByteLength then EndIdx := AB.ByteLength;
      end;
    end;
  end;

  NewLen := EndIdx - StartIdx;
  if NewLen < 0 then NewLen := 0;

  NewAB := TSouffleArrayBuffer.Create(NewLen, AB.IsShared);
  if Assigned(GC) then GC.AllocateObject(NewAB);
  if NewLen > 0 then
    Move(AB.Data[StartIdx], NewAB.Data[0], NewLen);

  { Wrap in blueprint record — preserve ArrayBuffer vs SharedArrayBuffer }
  if AB.IsShared then
    Bp := GNativeArrayJoinRuntime.FSharedArrayBufferBlueprint
  else
    Bp := GNativeArrayJoinRuntime.FArrayBufferBlueprint;
  Rec := TSouffleRecord.CreateFromBlueprint(Bp);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := Bp.Prototype;
  Rec.SetSlot(0, SouffleReference(NewAB));
  Result := SouffleReference(Rec);
end;

function NativeArrayBufferIsView(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  if AArgCount < 1 then Exit(SouffleBoolean(False));
  TA := GetSouffleTypedArray(AArgs^);
  Result := SouffleBoolean(Assigned(TA));
end;

{ Native TypedArray delegate callbacks }

function NativeTypedArrayLength(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
    Result := SouffleInteger(TA.ElementLength)
  else
    Result := SouffleInteger(0);
end;

function NativeTypedArrayByteLength(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
    Result := SouffleInteger(TA.ElementLength * BytesPerElement(TA.Kind))
  else
    Result := SouffleInteger(0);
end;

function NativeTypedArrayByteOffset(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
    Result := SouffleInteger(TA.ByteOffset)
  else
    Result := SouffleInteger(0);
end;

function NativeTypedArrayBuffer(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  Bp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) and Assigned(TA.Buffer) then
  begin
    { Return cached buffer record (preserves identity across all views) }
    if SouffleIsReference(TA.BufferRecord) and Assigned(TA.BufferRecord.AsReference) then
      Exit(TA.BufferRecord);
    if SouffleIsReference(TA.Buffer.CachedRecord) and Assigned(TA.Buffer.CachedRecord.AsReference) then
      Exit(TA.Buffer.CachedRecord);
    GC := TGarbageCollector.Instance;
    if TA.Buffer.IsShared then
      Bp := GNativeArrayJoinRuntime.FSharedArrayBufferBlueprint
    else
      Bp := GNativeArrayJoinRuntime.FArrayBufferBlueprint;
    Rec := TSouffleRecord.CreateFromBlueprint(Bp);
    if Assigned(GC) then GC.AllocateObject(Rec);
    Rec.Delegate := Bp.Prototype;
    Rec.SetSlot(0, SouffleReference(TA.Buffer));
    Result := SouffleReference(Rec);
    TA.BufferRecord := Result;
    TA.Buffer.CachedRecord := Result; { cache on buffer for all views }
  end
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayAt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  Idx: Integer;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) or (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Idx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(AArgs^));
  if Idx < 0 then Idx := TA.ElementLength + Idx;
  if (Idx < 0) or (Idx >= TA.ElementLength) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Result := SouffleFloat(TA.ReadElement(Idx));
end;

function NativeTypedArrayFill(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  Val: Double;
  StartIdx, EndIdx, I: Integer;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(AReceiver);
  Val := 0;
  if AArgCount > 0 then Val := GNativeArrayJoinRuntime.CoerceToNumber(AArgs^);
  StartIdx := 0;
  EndIdx := TA.ElementLength;
  if AArgCount > 1 then
  begin
    StartIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^));
    if StartIdx < 0 then StartIdx := TA.ElementLength + StartIdx;
    if StartIdx < 0 then StartIdx := 0;
  end;
  if AArgCount > 2 then
  begin
    EndIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + 2 * SizeOf(TSouffleValue))^));
    if EndIdx < 0 then EndIdx := TA.ElementLength + EndIdx;
    if EndIdx < 0 then EndIdx := 0;
  end;
  if EndIdx > TA.ElementLength then EndIdx := TA.ElementLength;
  for I := StartIdx to EndIdx - 1 do
    TA.WriteElement(I, Val);
  Result := AReceiver;
end;

function NativeTypedArraySlice(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, NewTA: TSouffleTypedArray;
  NewAB: TSouffleArrayBuffer;
  StartIdx, EndIdx, NewLen, I: Integer;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  GC := TGarbageCollector.Instance;
  StartIdx := 0;
  EndIdx := TA.ElementLength;
  if AArgCount > 0 then
  begin
    StartIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(AArgs^));
    if StartIdx < 0 then StartIdx := TA.ElementLength + StartIdx;
    if StartIdx < 0 then StartIdx := 0;
    if StartIdx > TA.ElementLength then StartIdx := TA.ElementLength;
  end;
  if AArgCount > 1 then
  begin
    EndIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^));
    if EndIdx < 0 then EndIdx := TA.ElementLength + EndIdx;
    if EndIdx < 0 then EndIdx := 0;
    if EndIdx > TA.ElementLength then EndIdx := TA.ElementLength;
  end;
  NewLen := EndIdx - StartIdx;
  if NewLen < 0 then NewLen := 0;

  NewAB := TSouffleArrayBuffer.Create(NewLen * BytesPerElement(TA.Kind));
  if Assigned(GC) then GC.AllocateObject(NewAB);
  NewTA := TSouffleTypedArray.Create(NewAB, 0, NewLen, TA.Kind);
  if Assigned(GC) then GC.AllocateObject(NewTA);
  for I := 0 to NewLen - 1 do
    NewTA.WriteElement(I, TA.ReadElement(StartIdx + I));

  { Find the right blueprint for this kind }
  Rec := TSouffleRecord.CreateFromBlueprint(
    GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(NewTA));
  Result := SouffleReference(Rec);
end;

function NativeTypedArrayReverse(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  I, J: Integer;
  Tmp: Double;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
  begin
    I := 0;
    J := TA.ElementLength - 1;
    while I < J do
    begin
      Tmp := TA.ReadElement(I);
      TA.WriteElement(I, TA.ReadElement(J));
      TA.WriteElement(J, Tmp);
      Inc(I);
      Dec(J);
    end;
  end;
  Result := AReceiver;
end;

function NativeTypedArrayIndexOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  SearchVal: Double;
  I, StartIdx: Integer;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then
  begin
    if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) then
      GNativeArrayJoinRuntime.ThrowTypeErrorMessage('Method called on incompatible receiver');
    Exit(SouffleInteger(-1));
  end;
  if AArgCount < 1 then Exit(SouffleInteger(-1));
  SearchVal := GNativeArrayJoinRuntime.CoerceToNumber(AArgs^);
  StartIdx := 0;
  if AArgCount > 1 then
  begin
    StartIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^));
    if StartIdx < 0 then StartIdx := TA.ElementLength + StartIdx;
    if StartIdx < 0 then StartIdx := 0;
  end;
  for I := StartIdx to TA.ElementLength - 1 do
    if TA.ReadElement(I) = SearchVal then
      Exit(SouffleInteger(I));
  Result := SouffleInteger(-1);
end;

function NativeTypedArrayIncludes(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  SearchVal, Elem: Double;
  I, StartIdx: Integer;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) or (AArgCount < 1) then Exit(SouffleBoolean(False));
  SearchVal := GNativeArrayJoinRuntime.CoerceToNumber(AArgs^);
  StartIdx := 0;
  if AArgCount > 1 then
  begin
    StartIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^));
    if StartIdx < 0 then StartIdx := TA.ElementLength + StartIdx;
    if StartIdx < 0 then StartIdx := 0;
  end;
  for I := StartIdx to TA.ElementLength - 1 do
  begin
    Elem := TA.ReadElement(I);
    if (Elem = SearchVal) or (IsNan(Elem) and IsNan(SearchVal)) then
      Exit(SouffleBoolean(True));
  end;
  Result := SouffleBoolean(False);
end;

function NativeTypedArrayJoin(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  Sep: string;
  I: Integer;
  S: string;
  V: Double;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleString(''));
  Sep := ',';
  if AArgCount > 0 then
    Sep := GNativeArrayJoinRuntime.CoerceToString(AArgs^);
  S := '';
  for I := 0 to TA.ElementLength - 1 do
  begin
    if I > 0 then S := S + Sep;
    V := TA.ReadElement(I);
    if (Frac(V) = 0) and (V >= -2147483648) and (V <= 2147483647) then
      S := S + IntToStr(Trunc(V))
    else
      S := S + FloatToStr(V);
  end;
  Result := SouffleString(S);
end;

function NativeTypedArrayToString(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := NativeTypedArrayJoin(AReceiver, nil, 0);
end;

function NativeTypedArrayForEach(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..2] of TSouffleValue;
  ThisArg: TSouffleValue;
  I: Integer;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if (AArgCount < 1) then
  begin
    GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
  if AArgCount > 1 then ThisArg := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^
  else ThisArg := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  for I := 0 to TA.ElementLength - 1 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ThisArg);
  end;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayToStringTagGetter(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
const
  KIND_NAMES: array[TSouffleTypedArrayKind] of string = (
    'Int8Array', 'Uint8Array', 'Uint8ClampedArray',
    'Int16Array', 'Uint16Array',
    'Int32Array', 'Uint32Array',
    'Float32Array', 'Float64Array'
  );
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
    Result := SouffleString(KIND_NAMES[TA.Kind])
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function ExtractThisArg(const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue; inline;
begin
  if AArgCount > 1 then
    Result := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayCopyWithin(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  Target, Start, EndIdx, Len, I: Integer;
  Tmp: Double;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(AReceiver);
  Len := TA.ElementLength;
  Target := 0; Start := 0; EndIdx := Len;
  if AArgCount > 0 then begin Target := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(AArgs^)); if Target < 0 then Target := Len + Target; if Target < 0 then Target := 0; end;
  if AArgCount > 1 then begin Start := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^)); if Start < 0 then Start := Len + Start; if Start < 0 then Start := 0; end;
  if AArgCount > 2 then begin EndIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(PSouffleValue(PByte(AArgs) + 2*SizeOf(TSouffleValue))^)); if EndIdx < 0 then EndIdx := Len + EndIdx; if EndIdx < 0 then EndIdx := 0; end;
  if EndIdx > Len then EndIdx := Len;
  if Target < Start then
    for I := 0 to EndIdx - Start - 1 do
    begin if Target + I < Len then TA.WriteElement(Target + I, TA.ReadElement(Start + I)); end
  else
    for I := EndIdx - Start - 1 downto 0 do
    begin if Target + I < Len then TA.WriteElement(Target + I, TA.ReadElement(Start + I)); end;
  Result := AReceiver;
end;

function NativeTypedArraySubarray(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, NewTA: TSouffleTypedArray;
  StartIdx, EndIdx, NewLen: Integer;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  GC := TGarbageCollector.Instance;
  StartIdx := 0; EndIdx := TA.ElementLength;
  if AArgCount > 0 then begin StartIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(AArgs^)); if StartIdx < 0 then StartIdx := TA.ElementLength + StartIdx; if StartIdx < 0 then StartIdx := 0; if StartIdx > TA.ElementLength then StartIdx := TA.ElementLength; end;
  if AArgCount > 1 then begin EndIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^)); if EndIdx < 0 then EndIdx := TA.ElementLength + EndIdx; if EndIdx < 0 then EndIdx := 0; if EndIdx > TA.ElementLength then EndIdx := TA.ElementLength; end;
  NewLen := EndIdx - StartIdx;
  if NewLen < 0 then NewLen := 0;
  NewTA := TSouffleTypedArray.Create(TA.Buffer, TA.ByteOffset + StartIdx * BytesPerElement(TA.Kind), NewLen, TA.Kind);
  NewTA.BufferRecord := TA.BufferRecord; { share buffer identity }
  if Assigned(GC) then GC.AllocateObject(NewTA);
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(NewTA));
  Result := SouffleReference(Rec);
end;

function NativeTypedArraySet(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, SrcTA: TSouffleTypedArray;
  Offset, I, SrcLen: Integer;
  SrcArr: TSouffleArray;
  Src: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) or (AArgCount < 1) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  Src := AArgs^;
  Offset := 0;
  if AArgCount > 1 then Offset := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^));
  if Offset < 0 then ThrowRangeErrorNative('Offset is out of bounds');
  SrcTA := GetSouffleTypedArray(Src);
  if Assigned(SrcTA) then
  begin
    if Offset + SrcTA.ElementLength > TA.ElementLength then ThrowRangeErrorNative('Source is too large');
    for I := 0 to SrcTA.ElementLength - 1 do
      TA.WriteElement(Offset + I, SrcTA.ReadElement(I));
  end
  else if SouffleIsReference(Src) and Assigned(Src.AsReference) and (Src.AsReference is TSouffleArray) then
  begin
    SrcArr := TSouffleArray(Src.AsReference);
    if Offset + SrcArr.Count > TA.ElementLength then ThrowRangeErrorNative('Source is too large');
    for I := 0 to SrcArr.Count - 1 do
      TA.WriteElement(Offset + I, GNativeArrayJoinRuntime.CoerceToNumber(SrcArr.Get(I)));
  end;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArraySort(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  Vals: array of Double;
  I, J: Integer;
  Tmp: Double;
  HasCompare: Boolean;
  CompareArgs: array[0..1] of TSouffleValue;
  CmpResult: TSouffleValue;
  CmpVal: Double;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) or (TA.ElementLength <= 1) then Exit(AReceiver);
  HasCompare := (AArgCount > 0) and SouffleIsReference(AArgs^) and Assigned(AArgs^.AsReference);
  SetLength(Vals, TA.ElementLength);
  for I := 0 to TA.ElementLength - 1 do Vals[I] := TA.ReadElement(I);
  { Simple insertion sort }
  for I := 1 to High(Vals) do
  begin
    Tmp := Vals[I]; J := I - 1;
    if HasCompare then
    begin
      while J >= 0 do
      begin
        CompareArgs[0] := SouffleFloat(Vals[J]);
        CompareArgs[1] := SouffleFloat(Tmp);
        CmpResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CompareArgs[0], 2, SouffleNil);
        CmpVal := GNativeArrayJoinRuntime.CoerceToNumber(CmpResult);
        if CmpVal <= 0 then Break;
        Vals[J + 1] := Vals[J]; Dec(J);
      end;
    end
    else
      while (J >= 0) and (Vals[J] > Tmp) do begin Vals[J + 1] := Vals[J]; Dec(J); end;
    Vals[J + 1] := Tmp;
  end;
  for I := 0 to TA.ElementLength - 1 do TA.WriteElement(I, Vals[I]);
  Result := AReceiver;
end;

function NativeTypedArrayLastIndexOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  SearchVal: Double;
  I, FromIdx: Integer;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) or (AArgCount < 1) then Exit(SouffleInteger(-1));
  SearchVal := GNativeArrayJoinRuntime.CoerceToNumber(AArgs^);
  FromIdx := TA.ElementLength - 1;
  if AArgCount > 1 then begin FromIdx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^)); if FromIdx < 0 then FromIdx := TA.ElementLength + FromIdx; end;
  if FromIdx >= TA.ElementLength then FromIdx := TA.ElementLength - 1;
  for I := FromIdx downto 0 do
    if TA.ReadElement(I) = SearchVal then Exit(SouffleInteger(I));
  Result := SouffleInteger(-1);
end;

function NativeTypedArrayFind(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
  CbResult: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  for I := 0 to TA.ElementLength - 1 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    if GNativeArrayJoinRuntime.CoerceToNumber(GNativeArrayJoinRuntime.ToBoolean(CbResult)) <> 0 then
      Exit(SouffleFloat(TA.ReadElement(I)));
  end;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayFindIndex(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
  CbResult: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleInteger(-1));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleInteger(-1)); end;
  for I := 0 to TA.ElementLength - 1 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    if GNativeArrayJoinRuntime.CoerceToNumber(GNativeArrayJoinRuntime.ToBoolean(CbResult)) <> 0 then
      Exit(SouffleInteger(I));
  end;
  Result := SouffleInteger(-1);
end;

function NativeTypedArrayFindLast(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
  CbResult: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  for I := TA.ElementLength - 1 downto 0 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    if GNativeArrayJoinRuntime.CoerceToNumber(GNativeArrayJoinRuntime.ToBoolean(CbResult)) <> 0 then
      Exit(SouffleFloat(TA.ReadElement(I)));
  end;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayFindLastIndex(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
  CbResult: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleInteger(-1));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleInteger(-1)); end;
  for I := TA.ElementLength - 1 downto 0 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    if GNativeArrayJoinRuntime.CoerceToNumber(GNativeArrayJoinRuntime.ToBoolean(CbResult)) <> 0 then
      Exit(SouffleInteger(I));
  end;
  Result := SouffleInteger(-1);
end;

function NativeTypedArrayEvery(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
  CbResult: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleBoolean(True));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleBoolean(True)); end;
  for I := 0 to TA.ElementLength - 1 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    if GNativeArrayJoinRuntime.CoerceToNumber(GNativeArrayJoinRuntime.ToBoolean(CbResult)) = 0 then
      Exit(SouffleBoolean(False));
  end;
  Result := SouffleBoolean(True);
end;

function NativeTypedArraySome(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
  CbResult: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleBoolean(False));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleBoolean(False)); end;
  for I := 0 to TA.ElementLength - 1 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    if GNativeArrayJoinRuntime.CoerceToNumber(GNativeArrayJoinRuntime.ToBoolean(CbResult)) <> 0 then
      Exit(SouffleBoolean(True));
  end;
  Result := SouffleBoolean(False);
end;

function NativeTypedArrayMap(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, NewTA: TSouffleTypedArray;
  NewAB: TSouffleArrayBuffer;
  CallArgs: array[0..2] of TSouffleValue;
  I: Integer;
  CbResult: TSouffleValue;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(AReceiver);
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(AReceiver); end;
  GC := TGarbageCollector.Instance;
  NewAB := TSouffleArrayBuffer.Create(TA.ElementLength * BytesPerElement(TA.Kind));
  if Assigned(GC) then GC.AllocateObject(NewAB);
  NewTA := TSouffleTypedArray.Create(NewAB, 0, TA.ElementLength, TA.Kind);
  if Assigned(GC) then GC.AllocateObject(NewTA);
  for I := 0 to TA.ElementLength - 1 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    NewTA.WriteElement(I, GNativeArrayJoinRuntime.CoerceToNumber(CbResult));
  end;
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(NewTA));
  Result := SouffleReference(Rec);
end;

function NativeTypedArrayFilter(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, NewTA: TSouffleTypedArray;
  NewAB: TSouffleArrayBuffer;
  CallArgs: array[0..2] of TSouffleValue;
  I, Count: Integer;
  CbResult: TSouffleValue;
  Kept: array of Double;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(AReceiver);
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(AReceiver); end;
  GC := TGarbageCollector.Instance;
  SetLength(Kept, TA.ElementLength);
  Count := 0;
  for I := 0 to TA.ElementLength - 1 do
  begin
    CallArgs[0] := SouffleFloat(TA.ReadElement(I));
    CallArgs[1] := SouffleInteger(I);
    CallArgs[2] := AReceiver;
    CbResult := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 3, ExtractThisArg(AArgs, AArgCount));
    if GNativeArrayJoinRuntime.CoerceToNumber(GNativeArrayJoinRuntime.ToBoolean(CbResult)) <> 0 then
    begin Kept[Count] := TA.ReadElement(I); Inc(Count); end;
  end;
  NewAB := TSouffleArrayBuffer.Create(Count * BytesPerElement(TA.Kind));
  if Assigned(GC) then GC.AllocateObject(NewAB);
  NewTA := TSouffleTypedArray.Create(NewAB, 0, Count, TA.Kind);
  if Assigned(GC) then GC.AllocateObject(NewTA);
  for I := 0 to Count - 1 do NewTA.WriteElement(I, Kept[I]);
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(NewTA));
  Result := SouffleReference(Rec);
end;

function NativeTypedArrayReduce(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..3] of TSouffleValue;
  I, StartIdx: Integer;
  Acc: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  if AArgCount > 1 then
  begin Acc := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^; StartIdx := 0; end
  else if TA.ElementLength > 0 then
  begin Acc := SouffleFloat(TA.ReadElement(0)); StartIdx := 1; end
  else begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('Reduce of empty array with no initial value'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  for I := StartIdx to TA.ElementLength - 1 do
  begin
    CallArgs[0] := Acc;
    CallArgs[1] := SouffleFloat(TA.ReadElement(I));
    CallArgs[2] := SouffleInteger(I);
    CallArgs[3] := AReceiver;
    Acc := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 4, SouffleNil);
  end;
  Result := Acc;
end;

function NativeTypedArrayReduceRight(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
  CallArgs: array[0..3] of TSouffleValue;
  I, StartIdx: Integer;
  Acc: TSouffleValue;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if (AArgCount < 1) then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('callback is not a function'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  if AArgCount > 1 then
  begin Acc := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^; StartIdx := TA.ElementLength - 1; end
  else if TA.ElementLength > 0 then
  begin Acc := SouffleFloat(TA.ReadElement(TA.ElementLength - 1)); StartIdx := TA.ElementLength - 2; end
  else begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('Reduce of empty array with no initial value'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  for I := StartIdx downto 0 do
  begin
    CallArgs[0] := Acc;
    CallArgs[1] := SouffleFloat(TA.ReadElement(I));
    CallArgs[2] := SouffleInteger(I);
    CallArgs[3] := AReceiver;
    Acc := GNativeArrayJoinRuntime.Invoke(AArgs^, @CallArgs[0], 4, SouffleNil);
  end;
  Result := Acc;
end;

function NativeTypedArrayToReversed(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, NewTA: TSouffleTypedArray;
  NewAB: TSouffleArrayBuffer;
  I: Integer;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then
  begin
    GNativeArrayJoinRuntime.ThrowTypeErrorMessage('Method called on incompatible receiver');
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;
  GC := TGarbageCollector.Instance;
  NewAB := TSouffleArrayBuffer.Create(TA.ElementLength * BytesPerElement(TA.Kind));
  if Assigned(GC) then GC.AllocateObject(NewAB);
  NewTA := TSouffleTypedArray.Create(NewAB, 0, TA.ElementLength, TA.Kind);
  if Assigned(GC) then GC.AllocateObject(NewTA);
  for I := 0 to TA.ElementLength - 1 do
    NewTA.WriteElement(I, TA.ReadElement(TA.ElementLength - 1 - I));
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(NewTA));
  Result := SouffleReference(Rec);
end;

function NativeTypedArrayToSorted(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, NewTA: TSouffleTypedArray;
  NewAB: TSouffleArrayBuffer;
  I: Integer;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  GC := TGarbageCollector.Instance;
  NewAB := TSouffleArrayBuffer.Create(TA.ElementLength * BytesPerElement(TA.Kind));
  if Assigned(GC) then GC.AllocateObject(NewAB);
  NewTA := TSouffleTypedArray.Create(NewAB, 0, TA.ElementLength, TA.Kind);
  if Assigned(GC) then GC.AllocateObject(NewTA);
  for I := 0 to TA.ElementLength - 1 do
    NewTA.WriteElement(I, TA.ReadElement(I));
  { Create temp record, sort it, return }
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(NewTA));
  Result := NativeTypedArraySort(SouffleReference(Rec), AArgs, AArgCount);
end;

function NativeTypedArrayWith(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA, NewTA: TSouffleTypedArray;
  NewAB: TSouffleArrayBuffer;
  Idx, I: Integer;
  Val: Double;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if not Assigned(TA) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  GC := TGarbageCollector.Instance;
  Idx := 0;
  if AArgCount > 0 then Idx := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(AArgs^));
  if Idx < 0 then Idx := TA.ElementLength + Idx;
  if (Idx < 0) or (Idx >= TA.ElementLength) then
  begin ThrowRangeErrorNative('Invalid index'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  if AArgCount > 1 then Val := GNativeArrayJoinRuntime.CoerceToNumber(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^) else Val := NaN;
  NewAB := TSouffleArrayBuffer.Create(TA.ElementLength * BytesPerElement(TA.Kind));
  if Assigned(GC) then GC.AllocateObject(NewAB);
  NewTA := TSouffleTypedArray.Create(NewAB, 0, TA.ElementLength, TA.Kind);
  if Assigned(GC) then GC.AllocateObject(NewTA);
  for I := 0 to TA.ElementLength - 1 do
    if I = Idx then NewTA.WriteElement(I, Val)
    else NewTA.WriteElement(I, TA.ReadElement(I));
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[TA.Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(NewTA));
  Result := SouffleReference(Rec);
end;

function NativeTypedArrayValues(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
  begin
    Result := SouffleReference(TSouffleTypedArrayIterator.Create(TA, taikValues));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
  end
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayKeys(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
  begin
    Result := SouffleReference(TSouffleTypedArrayIterator.Create(TA, taikKeys));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
  end
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayEntries(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
  begin
    Result := SouffleReference(TSouffleTypedArrayIterator.Create(TA, taikEntries));
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Result.AsReference);
  end
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayBytesPerElement(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  TA: TSouffleTypedArray;
begin
  TA := GetSouffleTypedArray(AReceiver);
  if Assigned(TA) then
    Result := SouffleInteger(BytesPerElement(TA.Kind))
  else
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

function NativeTypedArrayStaticFrom(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Src, ItemVal, MapFn: TSouffleValue;
  IterObj: TSouffleValue;
  Items: array of Double;
  ItemCount, I: Integer;
  Done: Boolean;
  HasMap: Boolean;
  MapArgs: array[0..1] of TSouffleValue;
  MappedVal: TSouffleValue;
  Kind: TSouffleTypedArrayKind;
  Bp: TSouffleBlueprint;
  AB: TSouffleArrayBuffer;
  TA: TSouffleTypedArray;
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
begin
  if AArgCount < 1 then
  begin GNativeArrayJoinRuntime.ThrowTypeErrorMessage('Cannot convert undefined or null to object'); Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)); end;
  GC := TGarbageCollector.Instance;
  Src := AArgs^;
  HasMap := (AArgCount > 1) and SouffleIsReference(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);
  if HasMap then MapFn := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;

  { Determine kind from receiver blueprint }
  Kind := stakFloat64; { default }
  if SouffleIsReference(AReceiver) and (AReceiver.AsReference is TSouffleBlueprint) then
  begin
    Bp := TSouffleBlueprint(AReceiver.AsReference);
    for Kind := Low(TSouffleTypedArrayKind) to High(TSouffleTypedArrayKind) do
      if Bp = GNativeArrayJoinRuntime.FTypedArrayBlueprints[Kind] then Break;
  end;

  { Collect items }
  ItemCount := 0;
  SetLength(Items, 8);
  IterObj := GNativeArrayJoinRuntime.GetIterator(Src, False);
  repeat
    ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
    if not Done then
    begin
      if ItemCount >= Length(Items) then SetLength(Items, ItemCount * 2 + 4);
      if HasMap then
      begin
        MapArgs[0] := ItemVal;
        MapArgs[1] := SouffleInteger(ItemCount);
        MappedVal := GNativeArrayJoinRuntime.Invoke(MapFn, @MapArgs[0], 2, SouffleNil);
        Items[ItemCount] := GNativeArrayJoinRuntime.CoerceToNumber(MappedVal);
      end
      else
        Items[ItemCount] := GNativeArrayJoinRuntime.CoerceToNumber(ItemVal);
      Inc(ItemCount);
    end;
  until Done;

  AB := TSouffleArrayBuffer.Create(ItemCount * BytesPerElement(Kind));
  if Assigned(GC) then GC.AllocateObject(AB);
  TA := TSouffleTypedArray.Create(AB, 0, ItemCount, Kind);
  if Assigned(GC) then GC.AllocateObject(TA);
  for I := 0 to ItemCount - 1 do TA.WriteElement(I, Items[I]);
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(TA));
  Result := SouffleReference(Rec);
end;

function NativeTypedArrayStaticOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Kind: TSouffleTypedArrayKind;
  Bp: TSouffleBlueprint;
  AB: TSouffleArrayBuffer;
  TA: TSouffleTypedArray;
  Rec: TSouffleRecord;
  I: Integer;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  Kind := stakFloat64;
  if SouffleIsReference(AReceiver) and (AReceiver.AsReference is TSouffleBlueprint) then
  begin
    Bp := TSouffleBlueprint(AReceiver.AsReference);
    for Kind := Low(TSouffleTypedArrayKind) to High(TSouffleTypedArrayKind) do
      if Bp = GNativeArrayJoinRuntime.FTypedArrayBlueprints[Kind] then Break;
  end;

  AB := TSouffleArrayBuffer.Create(AArgCount * BytesPerElement(Kind));
  if Assigned(GC) then GC.AllocateObject(AB);
  TA := TSouffleTypedArray.Create(AB, 0, AArgCount, Kind);
  if Assigned(GC) then GC.AllocateObject(TA);
  for I := 0 to AArgCount - 1 do
    TA.WriteElement(I, GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
  Rec := TSouffleRecord.CreateFromBlueprint(GNativeArrayJoinRuntime.FTypedArrayBlueprints[Kind]);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FTypedArrayBlueprints[Kind].Prototype;
  Rec.SetSlot(0, SouffleReference(TA));
  Result := SouffleReference(Rec);
end;

{ Native Error construction }

function ConstructNativeError(const ARuntime: TGocciaRuntimeOperations;
  const ABp: TSouffleBlueprint; const AErrorName: string;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Msg, Stack: string;
  GC: TGarbageCollector;
  OptionsRec: TSouffleRecord;
  CauseVal: TSouffleValue;
begin
  GC := TGarbageCollector.Instance;
  Rec := TSouffleRecord.CreateFromBlueprint(ABp);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := ABp.Prototype;

  { Set name }
  Rec.Put('name', SouffleString(AErrorName));

  { Set message }
  if (AArgCount > 0) and (AArgs^.Kind <> svkNil) then
    Msg := ARuntime.CoerceToString(AArgs^)
  else
    Msg := '';
  Rec.Put('message', SouffleString(Msg));

  { Set stack — push VM frames so CaptureStackTrace can see them }
  if Assigned(ARuntime.FVM) then
    PushVMFramesToGoccia(ARuntime.FVM);
  try
    if Assigned(TGocciaCallStack.Instance) then
      Stack := TGocciaCallStack.Instance.CaptureStackTrace(AErrorName, Msg, 0)
    else
      Stack := AErrorName + ': ' + Msg;
  finally
    if Assigned(ARuntime.FVM) then
      PopVMFramesFromGoccia(ARuntime.FVM);
  end;
  Rec.Put('stack', SouffleString(Stack));

  { Handle options.cause (ES2022) }
  if AArgCount > 1 then
  begin
    if SouffleIsReference(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^) and
       Assigned(PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^.AsReference) and
       (PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^.AsReference is TSouffleRecord) then
    begin
      OptionsRec := TSouffleRecord(
        PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^.AsReference);
      if OptionsRec.Get('cause', CauseVal) then
        Rec.Put('cause', CauseVal);
    end;
  end;

  Result := SouffleReference(Rec);
end;

function NativeErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := ConstructNativeError(GNativeArrayJoinRuntime,
    GNativeArrayJoinRuntime.FErrorBlueprint, 'Error', AArgs, AArgCount);
end;

function NativeTypeErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := ConstructNativeError(GNativeArrayJoinRuntime,
    GNativeArrayJoinRuntime.FTypeErrorBlueprint, 'TypeError', AArgs, AArgCount);
end;

function NativeRangeErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := ConstructNativeError(GNativeArrayJoinRuntime,
    GNativeArrayJoinRuntime.FRangeErrorBlueprint, 'RangeError', AArgs, AArgCount);
end;

function NativeReferenceErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := ConstructNativeError(GNativeArrayJoinRuntime,
    GNativeArrayJoinRuntime.FReferenceErrorBlueprint, 'ReferenceError', AArgs, AArgCount);
end;

function NativeSyntaxErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := ConstructNativeError(GNativeArrayJoinRuntime,
    GNativeArrayJoinRuntime.FSyntaxErrorBlueprint, 'SyntaxError', AArgs, AArgCount);
end;

function NativeURIErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := ConstructNativeError(GNativeArrayJoinRuntime,
    GNativeArrayJoinRuntime.FURIErrorBlueprint, 'URIError', AArgs, AArgCount);
end;

function NativeAggregateErrorConstructor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Msg: string;
  GC: TGarbageCollector;
  ErrorsIterable, ItemVal, IterObj: TSouffleValue;
  ErrorsArr: TSouffleArray;
  Done: Boolean;
  I: Integer;
begin
  GC := TGarbageCollector.Instance;
  Rec := TSouffleRecord.CreateFromBlueprint(
    GNativeArrayJoinRuntime.FAggregateErrorBlueprint);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FAggregateErrorBlueprint.Prototype;

  Rec.Put('name', SouffleString('AggregateError'));

  { First arg: errors iterable }
  ErrorsArr := TSouffleArray.Create(4);
  if Assigned(GC) then GC.AllocateObject(ErrorsArr);
  if AArgCount > 0 then
  begin
    ErrorsIterable := AArgs^;
    try
      IterObj := GNativeArrayJoinRuntime.GetIterator(ErrorsIterable, False);
      repeat
        ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
        if not Done then ErrorsArr.Push(ItemVal);
      until Done;
    except
      on E: ESouffleThrow do; { silently ignore iteration errors }
    end;
  end;
  Rec.Put('errors', SouffleReference(ErrorsArr));

  { Second arg: message }
  if (AArgCount > 1) and (PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^.Kind <> svkNil) then
    Msg := GNativeArrayJoinRuntime.CoerceToString(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^)
  else
    Msg := '';
  Rec.Put('message', SouffleString(Msg));

  if Assigned(TGocciaCallStack.Instance) then
    Rec.Put('stack', SouffleString(
      TGocciaCallStack.Instance.CaptureStackTrace('AggregateError', Msg, 1)))
  else
    Rec.Put('stack', SouffleString('AggregateError: ' + Msg));

  Result := SouffleReference(Rec);
end;

{ Error.isError static method }
function NativeErrorIsError(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
begin
  if AArgCount < 1 then Exit(SouffleBoolean(False));
  if not SouffleIsReference(AArgs^) or not Assigned(AArgs^.AsReference) then
    Exit(SouffleBoolean(False));
  { Check if it's a blueprint record with Error in the blueprint ancestry }
  if AArgs^.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AArgs^.AsReference);
    if Assigned(Rec.Blueprint) then
    begin
      Bp := Rec.Blueprint;
      { DOMException inherits from Error but isError returns false }
      if Bp = GNativeArrayJoinRuntime.FDOMExceptionBlueprint then
        Exit(SouffleBoolean(False));
      while Assigned(Bp) do
      begin
        if Bp = GNativeArrayJoinRuntime.FErrorBlueprint then
          Exit(SouffleBoolean(True));
        Bp := Bp.SuperBlueprint;
      end;
    end;
  end;
  { Check wrapped Goccia error objects }
  if AArgs^.AsReference is TGocciaWrappedValue then
  begin
    if TGocciaWrappedValue(AArgs^.AsReference).Value is TGocciaObjectValue then
      Exit(SouffleBoolean(
        TGocciaObjectValue(TGocciaWrappedValue(AArgs^.AsReference).Value).HasErrorData));
  end;
  Result := SouffleBoolean(False);
end;

function ConstructNativeDOMException(const AArgs: PSouffleValue;
  const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  GC: TGarbageCollector;
  Msg, Name: string;
  Code: Integer;
begin
  GC := TGarbageCollector.Instance;
  Rec := TSouffleRecord.CreateFromBlueprint(
    GNativeArrayJoinRuntime.FDOMExceptionBlueprint);
  if Assigned(GC) then GC.AllocateObject(Rec);
  Rec.Delegate := GNativeArrayJoinRuntime.FDOMExceptionBlueprint.Prototype;

  { DOMException(message, name) }
  if (AArgCount > 0) and (AArgs^.Kind <> svkNil) then
    Msg := GNativeArrayJoinRuntime.CoerceToString(AArgs^)
  else
    Msg := '';

  if (AArgCount > 1) and (PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^.Kind <> svkNil) then
    Name := GNativeArrayJoinRuntime.CoerceToString(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^)
  else
    Name := 'Error';

  Rec.Put('name', SouffleString(Name));
  Rec.Put('message', SouffleString(Msg));

  { Legacy code property }
  Code := 0;
  if Name = 'IndexSizeError' then Code := 1
  else if Name = 'HierarchyRequestError' then Code := 3
  else if Name = 'WrongDocumentError' then Code := 4
  else if Name = 'InvalidCharacterError' then Code := 5
  else if Name = 'NoModificationAllowedError' then Code := 7
  else if Name = 'NotFoundError' then Code := 8
  else if Name = 'NotSupportedError' then Code := 9
  else if Name = 'InvalidStateError' then Code := 11
  else if Name = 'SyntaxError' then Code := 12
  else if Name = 'InvalidModificationError' then Code := 13
  else if Name = 'NamespaceError' then Code := 14
  else if Name = 'InvalidAccessError' then Code := 15
  else if Name = 'TypeMismatchError' then Code := 17
  else if Name = 'SecurityError' then Code := 18
  else if Name = 'NetworkError' then Code := 19
  else if Name = 'AbortError' then Code := 20
  else if Name = 'URLMismatchError' then Code := 21
  else if Name = 'QuotaExceededError' then Code := 22
  else if Name = 'TimeoutError' then Code := 23
  else if Name = 'InvalidNodeTypeError' then Code := 24
  else if Name = 'DataCloneError' then Code := 25;
  Rec.Put('code', SouffleInteger(Code));

  if Assigned(TGocciaCallStack.Instance) then
    Rec.Put('stack', SouffleString(
      TGocciaCallStack.Instance.CaptureStackTrace(Name, Msg, 0)));

  Result := SouffleReference(Rec);
end;

{ Native Error delegate callbacks }

function NativeErrorToString(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  NameVal, MsgVal: TSouffleValue;
  Name, Msg: string;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference) and
     (AReceiver.AsReference is TSouffleRecord) then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    if Rec.Get('name', NameVal) then
      Name := GNativeArrayJoinRuntime.CoerceToString(NameVal)
    else
      Name := 'Error';
    if Rec.Get('message', MsgVal) then
      Msg := GNativeArrayJoinRuntime.CoerceToString(MsgVal)
    else
      Msg := '';
    if Msg = '' then
      Result := SouffleString(Name)
    else
      Result := SouffleString(Name + ': ' + Msg);
  end
  else
    Result := SouffleString('Error');
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

  MAP_PROTOTYPE_METHODS: array[0..10] of TSouffleMethodEntry = (
    (Name: 'get';                  Arity: 1; Callback: @NativeMapGet),
    (Name: 'set';                  Arity: 2; Callback: @NativeMapSet),
    (Name: 'has';                  Arity: 1; Callback: @NativeMapHas),
    (Name: 'delete';               Arity: 1; Callback: @NativeMapDelete),
    (Name: 'clear';                Arity: 0; Callback: @NativeMapClear),
    (Name: 'forEach';              Arity: 1; Callback: @NativeMapForEach),
    (Name: 'keys';                 Arity: 0; Callback: @NativeMapKeys),
    (Name: 'values';               Arity: 0; Callback: @NativeMapValues),
    (Name: 'entries';              Arity: 0; Callback: @NativeMapEntries),
    (Name: 'getOrInsert';          Arity: 2; Callback: @NativeMapGetOrInsert),
    (Name: 'getOrInsertComputed';  Arity: 2; Callback: @NativeMapGetOrInsertComputed)
  );

  MAP_ITERATOR_METHODS: array[0..0] of TSouffleMethodEntry = (
    (Name: 'next'; Arity: 0; Callback: @NativeMapIteratorNext)
  );

  SET_ITERATOR_METHODS: array[0..0] of TSouffleMethodEntry = (
    (Name: 'next'; Arity: 0; Callback: @NativeSetIteratorNext)
  );

  SET_PROTOTYPE_METHODS: array[0..14] of TSouffleMethodEntry = (
    (Name: 'has';                 Arity: 1; Callback: @NativeSetHas),
    (Name: 'add';                 Arity: 1; Callback: @NativeSetAdd),
    (Name: 'delete';              Arity: 1; Callback: @NativeSetDelete),
    (Name: 'clear';               Arity: 0; Callback: @NativeSetClear),
    (Name: 'forEach';             Arity: 1; Callback: @NativeSetForEach),
    (Name: 'values';              Arity: 0; Callback: @NativeSetValues),
    (Name: 'keys';                Arity: 0; Callback: @NativeSetKeys),
    (Name: 'entries';             Arity: 0; Callback: @NativeSetEntries),
    (Name: 'union';               Arity: 1; Callback: @NativeSetUnion),
    (Name: 'intersection';        Arity: 1; Callback: @NativeSetIntersection),
    (Name: 'difference';          Arity: 1; Callback: @NativeSetDifference),
    (Name: 'symmetricDifference'; Arity: 1; Callback: @NativeSetSymmetricDifference),
    (Name: 'isSubsetOf';          Arity: 1; Callback: @NativeSetIsSubsetOf),
    (Name: 'isSupersetOf';        Arity: 1; Callback: @NativeSetIsSupersetOf),
    (Name: 'isDisjointFrom';      Arity: 1; Callback: @NativeSetIsDisjointFrom)
  );

  PROMISE_PROTOTYPE_METHODS: array[0..2] of TSouffleMethodEntry = (
    (Name: 'then';    Arity: 2; Callback: @NativePromiseThen),
    (Name: 'catch';   Arity: 1; Callback: @NativePromiseCatch),
    (Name: 'finally'; Arity: 1; Callback: @NativePromiseFinally)
  );

  ARRAYBUFFER_PROTOTYPE_METHODS: array[0..0] of TSouffleMethodEntry = (
    (Name: 'slice';      Arity: 2; Callback: @NativeArrayBufferSlice)
  );

  ERROR_PROTOTYPE_METHODS: array[0..0] of TSouffleMethodEntry = (
    (Name: 'toString'; Arity: 0; Callback: @NativeErrorToString)
  );

  TYPEDARRAY_PROTOTYPE_METHODS: array[0..29] of TSouffleMethodEntry = (
    (Name: 'at';              Arity: 1; Callback: @NativeTypedArrayAt),
    (Name: 'fill';            Arity: 3; Callback: @NativeTypedArrayFill),
    (Name: 'copyWithin';      Arity: 3; Callback: @NativeTypedArrayCopyWithin),
    (Name: 'slice';           Arity: 2; Callback: @NativeTypedArraySlice),
    (Name: 'subarray';        Arity: 2; Callback: @NativeTypedArraySubarray),
    (Name: 'set';             Arity: 2; Callback: @NativeTypedArraySet),
    (Name: 'reverse';         Arity: 0; Callback: @NativeTypedArrayReverse),
    (Name: 'sort';            Arity: 1; Callback: @NativeTypedArraySort),
    (Name: 'indexOf';         Arity: 1; Callback: @NativeTypedArrayIndexOf),
    (Name: 'lastIndexOf';     Arity: 1; Callback: @NativeTypedArrayLastIndexOf),
    (Name: 'includes';        Arity: 1; Callback: @NativeTypedArrayIncludes),
    (Name: 'find';            Arity: 1; Callback: @NativeTypedArrayFind),
    (Name: 'findIndex';       Arity: 1; Callback: @NativeTypedArrayFindIndex),
    (Name: 'findLast';        Arity: 1; Callback: @NativeTypedArrayFindLast),
    (Name: 'findLastIndex';   Arity: 1; Callback: @NativeTypedArrayFindLastIndex),
    (Name: 'every';           Arity: 1; Callback: @NativeTypedArrayEvery),
    (Name: 'some';            Arity: 1; Callback: @NativeTypedArraySome),
    (Name: 'forEach';         Arity: 1; Callback: @NativeTypedArrayForEach),
    (Name: 'map';             Arity: 1; Callback: @NativeTypedArrayMap),
    (Name: 'filter';          Arity: 1; Callback: @NativeTypedArrayFilter),
    (Name: 'reduce';          Arity: 2; Callback: @NativeTypedArrayReduce),
    (Name: 'reduceRight';     Arity: 2; Callback: @NativeTypedArrayReduceRight),
    (Name: 'join';            Arity: 1; Callback: @NativeTypedArrayJoin),
    (Name: 'toString';        Arity: 0; Callback: @NativeTypedArrayToString),
    (Name: 'toReversed';      Arity: 0; Callback: @NativeTypedArrayToReversed),
    (Name: 'toSorted';        Arity: 1; Callback: @NativeTypedArrayToSorted),
    (Name: 'with';            Arity: 2; Callback: @NativeTypedArrayWith),
    (Name: 'values';          Arity: 0; Callback: @NativeTypedArrayValues),
    (Name: 'keys';            Arity: 0; Callback: @NativeTypedArrayKeys),
    (Name: 'entries';         Arity: 0; Callback: @NativeTypedArrayEntries)
  );


procedure TGocciaRuntimeOperations.ClearTransientCaches;
begin
  FClosureBridgeCache.Clear;
  FArrayBridgeCache.Clear;
  FArrayBridgeDirty := False;
  FArrayBridgeReverse.Clear;
  FRecordBridgeCache.Clear;
end;

function TGocciaRuntimeOperations.CreateBuiltinBlueprint(const AName: string;
  const ASlotCount: Integer; const AMethodDelegate: TSouffleRecord;
  const AIteratorMethod: string): TSouffleBlueprint;
var
  GC: TGarbageCollector;
  SymTagKey, SymIterKey: string;
  IterVal: TSouffleValue;
begin
  GC := TGarbageCollector.Instance;
  Result := TSouffleBlueprint.Create(AName, ASlotCount);
  if Assigned(GC) then
    GC.AllocateObject(Result);

  { Wire prototype delegate chain: prototype → method delegate → record delegate }
  Result.Prototype.Delegate := AMethodDelegate;
  if Assigned(FVM) then
    AMethodDelegate.Delegate := FVM.RecordDelegate;

  { Symbol.toStringTag on prototype }
  SymTagKey := '@@sym:' + IntToStr(TGocciaSymbolValue.WellKnownToStringTag.Id);
  Result.Prototype.Put(SymTagKey, SouffleString(AName));

  { Symbol.iterator on prototype — points to the named method on the delegate }
  if (AIteratorMethod <> '') and AMethodDelegate.Get(AIteratorMethod, IterVal) then
  begin
    SymIterKey := '@@sym:' + IntToStr(TGocciaSymbolValue.WellKnownIterator.Id);
    Result.Prototype.Put(SymIterKey, IterVal);
  end;

  { Constructor property on prototype }
  Result.Prototype.Put(PROP_CONSTRUCTOR, SouffleReference(Result));
end;

procedure AddMethodToBlueprint(const ABp: TSouffleBlueprint;
  const AName: string; const AArity: Integer;
  const ACallback: TSouffleNativeCallback);
var
  NF: TSouffleNativeFunction;
begin
  NF := TSouffleNativeFunction.Create(AName, AArity, ACallback);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NF);
  ABp.Methods.Put(AName, SouffleReference(NF));
end;

procedure AddStaticMethod(const ABp: TSouffleBlueprint;
  const AName: string; const AArity: Integer;
  const ACallback: TSouffleNativeCallback);
var
  NF: TSouffleNativeFunction;
begin
  NF := TSouffleNativeFunction.Create(AName, AArity, ACallback);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(NF);
  ABp.StaticFields.Put(AName, SouffleReference(NF));
end;

{ === Native Number static callbacks === }

function NativeNumberIsNaN(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if AArgCount < 1 then Exit(SouffleBoolean(False));
  if AArgs^.Kind = svkFloat then
    Result := SouffleBoolean(IsNan(AArgs^.AsFloat))
  else
    Result := SouffleBoolean(False);
end;

function NativeNumberIsFinite(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if AArgCount < 1 then Exit(SouffleBoolean(False));
  case AArgs^.Kind of
    svkInteger: Result := SouffleBoolean(True);
    svkFloat: Result := SouffleBoolean(not IsNan(AArgs^.AsFloat) and not IsInfinite(AArgs^.AsFloat));
  else
    Result := SouffleBoolean(False);
  end;
end;

function NativeNumberIsInteger(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if AArgCount < 1 then Exit(SouffleBoolean(False));
  case AArgs^.Kind of
    svkInteger: Result := SouffleBoolean(True);
    svkFloat: Result := SouffleBoolean(
      not IsNan(AArgs^.AsFloat) and not IsInfinite(AArgs^.AsFloat) and
      (Frac(AArgs^.AsFloat) = 0.0));
  else
    Result := SouffleBoolean(False);
  end;
end;

function NativeNumberIsSafeInteger(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
const
  MAX_SAFE = 9007199254740991.0;
begin
  if AArgCount < 1 then Exit(SouffleBoolean(False));
  case AArgs^.Kind of
    svkInteger: Result := SouffleBoolean(True);
    svkFloat: Result := SouffleBoolean(
      not IsNan(AArgs^.AsFloat) and not IsInfinite(AArgs^.AsFloat) and
      (Frac(AArgs^.AsFloat) = 0.0) and
      (Abs(AArgs^.AsFloat) <= MAX_SAFE));
  else
    Result := SouffleBoolean(False);
  end;
end;

function NativeNumberParseInt(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  Radix, I, Digit: Integer;
  Negative: Boolean;
  IntResult: Int64;
begin
  if AArgCount < 1 then Exit(SouffleFloat(NaN));
  S := Trim(GNativeArrayJoinRuntime.CoerceToString(AArgs^));
  if S = '' then Exit(SouffleFloat(NaN));

  Radix := 10;
  if AArgCount > 1 then
  begin
    Radix := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^));
    if (Radix < 2) or (Radix > 36) then
      Exit(SouffleFloat(NaN));
  end;

  Negative := False;
  I := 1;
  if (I <= Length(S)) and ((S[I] = '+') or (S[I] = '-')) then
  begin
    Negative := S[I] = '-';
    Inc(I);
  end;

  if (Radix = 16) and (I + 1 <= Length(S)) and (S[I] = '0') and
     ((S[I + 1] = 'x') or (S[I + 1] = 'X')) then
    Inc(I, 2)
  else if (AArgCount < 2) and (I + 1 <= Length(S)) and (S[I] = '0') and
     ((S[I + 1] = 'x') or (S[I + 1] = 'X')) then
  begin
    Radix := 16;
    Inc(I, 2);
  end;

  IntResult := 0;
  if I > Length(S) then Exit(SouffleFloat(NaN));
  while I <= Length(S) do
  begin
    if (S[I] >= '0') and (S[I] <= '9') then
      Digit := Ord(S[I]) - Ord('0')
    else if (S[I] >= 'a') and (S[I] <= 'z') then
      Digit := Ord(S[I]) - Ord('a') + 10
    else if (S[I] >= 'A') and (S[I] <= 'Z') then
      Digit := Ord(S[I]) - Ord('A') + 10
    else
      Break;
    if Digit >= Radix then Break;
    IntResult := IntResult * Radix + Digit;
    Inc(I);
  end;

  if Negative then IntResult := -IntResult;
  if (IntResult >= -2147483648) and (IntResult <= 2147483647) then
    Result := SouffleInteger(IntResult)
  else
    Result := SouffleFloat(IntResult);
end;

function NativeNumberParseFloat(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  F: Double;
  Code: Integer;
  FloatFmt: TFormatSettings;
begin
  if AArgCount < 1 then Exit(SouffleFloat(NaN));
  S := Trim(GNativeArrayJoinRuntime.CoerceToString(AArgs^));
  if S = '' then Exit(SouffleFloat(NaN));
  if S = 'Infinity' then Exit(SouffleFloat(Infinity));
  if S = '-Infinity' then Exit(SouffleFloat(NegInfinity));
  FloatFmt := FormatSettings;
  FloatFmt.DecimalSeparator := '.';
  Val(S, F, Code);
  if Code = 0 then
    Result := SouffleFloat(F)
  else
    Result := SouffleFloat(NaN);
end;

{ === Native Array static callbacks === }

function NativeArrayIsArray(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if AArgCount < 1 then Exit(SouffleBoolean(False));
  Result := SouffleBoolean(
    SouffleIsReference(AArgs^) and Assigned(AArgs^.AsReference) and
    (AArgs^.AsReference is TSouffleArray));
end;

function NativeArrayFrom(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Iterable, ItemVal, MapFn, ThisArg: TSouffleValue;
  Arr: TSouffleArray;
  SrcArr: TSouffleArray;
  IterObj: TSouffleValue;
  Done: Boolean;
  I, Len: Integer;
  HasMap: Boolean;
  MapArgs: array[0..1] of TSouffleValue;
  LenVal: TSouffleValue;
begin
  if AArgCount < 1 then
  begin
    Arr := TSouffleArray.Create(0);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Arr);
    Exit(SouffleReference(Arr));
  end;

  Iterable := AArgs^;
  HasMap := (AArgCount > 1) and SouffleIsReference(
    PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);
  if HasMap then
    MapFn := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^
  else
    MapFn := SouffleNil;
  if AArgCount > 2 then
    ThisArg := PSouffleValue(PByte(AArgs) + 2 * SizeOf(TSouffleValue))^
  else
    ThisArg := SouffleNil;

  { Fast path: source is array }
  if SouffleIsReference(Iterable) and Assigned(Iterable.AsReference) and
     (Iterable.AsReference is TSouffleArray) then
  begin
    SrcArr := TSouffleArray(Iterable.AsReference);
    Arr := TSouffleArray.Create(SrcArr.Count);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(Arr);
    for I := 0 to SrcArr.Count - 1 do
    begin
      ItemVal := SrcArr.Get(I);
      if HasMap then
      begin
        MapArgs[0] := ItemVal;
        MapArgs[1] := SouffleInteger(I);
        ItemVal := GNativeArrayJoinRuntime.Invoke(MapFn, @MapArgs[0], 2, ThisArg);
      end;
      Arr.Push(ItemVal);
    end;
    Exit(SouffleReference(Arr));
  end;

  { Iterable path }
  Arr := TSouffleArray.Create(4);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Arr);
  try
    IterObj := GNativeArrayJoinRuntime.GetIterator(Iterable, False);
    I := 0;
    repeat
      ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
      if not Done then
      begin
        if HasMap then
        begin
          MapArgs[0] := ItemVal;
          MapArgs[1] := SouffleInteger(I);
          ItemVal := GNativeArrayJoinRuntime.Invoke(MapFn, @MapArgs[0], 2, ThisArg);
        end;
        Arr.Push(ItemVal);
        Inc(I);
      end;
    until Done;
  except
    on E: ESouffleThrow do
    begin
      { Array-like fallback }
      LenVal := GNativeArrayJoinRuntime.GetProperty(Iterable, 'length');
      if LenVal.Kind = svkInteger then
        Len := Integer(LenVal.AsInteger)
      else if LenVal.Kind = svkFloat then
        Len := Trunc(LenVal.AsFloat)
      else
        Len := 0;
      Arr := TSouffleArray.Create(Len);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(Arr);
      for I := 0 to Len - 1 do
      begin
        ItemVal := GNativeArrayJoinRuntime.GetIndex(Iterable, SouffleInteger(I));
        if HasMap then
        begin
          MapArgs[0] := ItemVal;
          MapArgs[1] := SouffleInteger(I);
          ItemVal := GNativeArrayJoinRuntime.Invoke(MapFn, @MapArgs[0], 2, ThisArg);
        end;
        Arr.Push(ItemVal);
      end;
    end;
  end;
  Result := SouffleReference(Arr);
end;

function NativeArrayOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  I: Integer;
begin
  Arr := TSouffleArray.Create(AArgCount);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Arr);
  for I := 0 to AArgCount - 1 do
    Arr.Push(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^);
  Result := SouffleReference(Arr);
end;

{ === Native Object static callbacks === }

function NativeObjectKeys(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Arr: TSouffleArray;
  I: Integer;
  Key: string;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    { Fallback: delegate to Goccia for non-record objects }
    Result := InvokeGocciaMethod(AReceiver, 'keys', AArgs, AArgCount);
    Exit;
  end;
  Rec := TSouffleRecord(AArgs^.AsReference);
  Arr := TSouffleArray.Create(Rec.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Arr);
  for I := 0 to Rec.Count - 1 do
  begin
    Key := Rec.GetOrderedKey(I);
    if (Length(Key) > 0) and (Key[1] <> '#') and
       (Copy(Key, 1, 5) <> '@@sym') then
      Arr.Push(SouffleString(Key));
  end;
  Result := SouffleReference(Arr);
end;

function NativeObjectValues(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Arr: TSouffleArray;
  I: Integer;
  Key: string;
  Val: TSouffleValue;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    Result := InvokeGocciaMethod(AReceiver, 'values', AArgs, AArgCount);
    Exit;
  end;
  Rec := TSouffleRecord(AArgs^.AsReference);
  Arr := TSouffleArray.Create(Rec.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Arr);
  for I := 0 to Rec.Count - 1 do
  begin
    Key := Rec.GetOrderedKey(I);
    if (Length(Key) > 0) and (Key[1] <> '#') and
       (Copy(Key, 1, 5) <> '@@sym') then
    begin
      if Rec.Get(Key, Val) then
        Arr.Push(Val);
    end;
  end;
  Result := SouffleReference(Arr);
end;

function NativeObjectEntries(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Arr, EntryArr: TSouffleArray;
  I: Integer;
  Key: string;
  Val: TSouffleValue;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    Result := InvokeGocciaMethod(AReceiver, 'entries', AArgs, AArgCount);
    Exit;
  end;
  Rec := TSouffleRecord(AArgs^.AsReference);
  Arr := TSouffleArray.Create(Rec.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Arr);
  for I := 0 to Rec.Count - 1 do
  begin
    Key := Rec.GetOrderedKey(I);
    if (Length(Key) > 0) and (Key[1] <> '#') and
       (Copy(Key, 1, 5) <> '@@sym') then
    begin
      if Rec.Get(Key, Val) then
      begin
        EntryArr := TSouffleArray.Create(2);
        EntryArr.Push(SouffleString(Key));
        EntryArr.Push(Val);
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(EntryArr);
        Arr.Push(SouffleReference(EntryArr));
      end;
    end;
  end;
  Result := SouffleReference(Arr);
end;

function NativeObjectAssign(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Target, Source: TSouffleRecord;
  I, J: Integer;
  Key: string;
  Val: TSouffleValue;
  SrcVal: TSouffleValue;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    Result := InvokeGocciaMethod(AReceiver, 'assign', AArgs, AArgCount);
    Exit;
  end;
  Target := TSouffleRecord(AArgs^.AsReference);
  for I := 1 to AArgCount - 1 do
  begin
    SrcVal := PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^;
    if SouffleIsReference(SrcVal) and Assigned(SrcVal.AsReference) and
       (SrcVal.AsReference is TSouffleRecord) then
    begin
      Source := TSouffleRecord(SrcVal.AsReference);
      for J := 0 to Source.Count - 1 do
      begin
        Key := Source.GetOrderedKey(J);
        if Source.Get(Key, Val) then
          Target.Put(Key, Val);
      end;
    end;
  end;
  Result := AArgs^;
end;

function NativeObjectIs(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  A, B: TSouffleValue;
begin
  if AArgCount < 1 then A := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)
  else A := AArgs^;
  if AArgCount < 2 then B := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED)
  else B := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;

  { SameValue algorithm }
  if A.Kind <> B.Kind then
    Exit(SouffleBoolean(False));
  case A.Kind of
    svkNil: Result := SouffleBoolean(A.Flags = B.Flags);
    svkBoolean: Result := SouffleBoolean(A.AsBoolean = B.AsBoolean);
    svkInteger: Result := SouffleBoolean(A.AsInteger = B.AsInteger);
    svkFloat:
    begin
      if IsNan(A.AsFloat) and IsNan(B.AsFloat) then
        Result := SouffleBoolean(True)
      else if (A.AsFloat = 0.0) and (B.AsFloat = 0.0) then
        { Distinguish +0 and -0 }
        Result := SouffleBoolean((1.0 / A.AsFloat) = (1.0 / B.AsFloat))
      else
        Result := SouffleBoolean(A.AsFloat = B.AsFloat);
    end;
    svkString: Result := SouffleBoolean(SouffleGetString(A) = SouffleGetString(B));
    svkReference: Result := SouffleBoolean(A.AsReference = B.AsReference);
  else
    Result := SouffleBoolean(False);
  end;
end;

function NativeObjectHasOwn(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Key: string;
  Val: TSouffleValue;
begin
  if (AArgCount < 2) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    Result := InvokeGocciaMethod(AReceiver, 'hasOwn', AArgs, AArgCount);
    Exit;
  end;
  Rec := TSouffleRecord(AArgs^.AsReference);
  Key := GNativeArrayJoinRuntime.CoerceToString(
    PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);
  Result := SouffleBoolean(Rec.Get(Key, Val));
end;

function NativeObjectFreeze(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if (AArgCount < 1) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if not SouffleIsReference(AArgs^) or not Assigned(AArgs^.AsReference) then
    Exit(AArgs^);
  if AArgs^.AsReference is TSouffleRecord then
    TSouffleRecord(AArgs^.AsReference).Freeze;
  Result := AArgs^;
end;

function NativeObjectIsFrozen(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  I: Integer;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) then
    Exit(SouffleBoolean(True));
  if not (AArgs^.AsReference is TSouffleRecord) then
    Exit(SouffleBoolean(True));
  Rec := TSouffleRecord(AArgs^.AsReference);
  if Rec.Extensible then Exit(SouffleBoolean(False));
  for I := 0 to Rec.Count - 1 do
    if (Rec.GetEntryFlags(Rec.GetOrderedKey(I)) and
       (SOUFFLE_PROP_WRITABLE or SOUFFLE_PROP_CONFIGURABLE)) <> 0 then
      Exit(SouffleBoolean(False));
  Result := SouffleBoolean(True);
end;

function NativeObjectSeal(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  I: Integer;
  Key: string;
  Flags: Byte;
begin
  if (AArgCount < 1) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if not SouffleIsReference(AArgs^) or not Assigned(AArgs^.AsReference) then
    Exit(AArgs^);
  if AArgs^.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AArgs^.AsReference);
    for I := 0 to Rec.Count - 1 do
    begin
      Key := Rec.GetOrderedKey(I);
      Flags := Rec.GetEntryFlags(Key);
      Rec.SetEntryFlags(Key, Flags and (not SOUFFLE_PROP_CONFIGURABLE));
    end;
    Rec.PreventExtensions;
  end;
  Result := AArgs^;
end;

function NativeObjectIsSealed(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  I: Integer;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) then
    Exit(SouffleBoolean(True));
  if not (AArgs^.AsReference is TSouffleRecord) then
    Exit(SouffleBoolean(True));
  Rec := TSouffleRecord(AArgs^.AsReference);
  if Rec.Extensible then Exit(SouffleBoolean(False));
  for I := 0 to Rec.Count - 1 do
    if (Rec.GetEntryFlags(Rec.GetOrderedKey(I)) and SOUFFLE_PROP_CONFIGURABLE) <> 0 then
      Exit(SouffleBoolean(False));
  Result := SouffleBoolean(True);
end;

function NativeObjectPreventExtensions(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if (AArgCount < 1) then Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  if SouffleIsReference(AArgs^) and Assigned(AArgs^.AsReference) and
     (AArgs^.AsReference is TSouffleRecord) then
    TSouffleRecord(AArgs^.AsReference).PreventExtensions;
  Result := AArgs^;
end;

function NativeObjectIsExtensible(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) then
    Exit(SouffleBoolean(False));
  if AArgs^.AsReference is TSouffleRecord then
    Result := SouffleBoolean(TSouffleRecord(AArgs^.AsReference).Extensible)
  else
    Result := SouffleBoolean(False);
end;

function NativeObjectCreate(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
begin
  Rec := TSouffleRecord.Create(4);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  if (AArgCount > 0) and SouffleIsReference(AArgs^) and
     Assigned(AArgs^.AsReference) and
     (AArgs^.AsReference is TSouffleRecord) then
    Rec.Delegate := TSouffleRecord(AArgs^.AsReference)
  else if (AArgCount > 0) and (AArgs^.Kind = svkNil) and
     (AArgs^.Flags = GOCCIA_NIL_NULL) then
    { Object.create(null) — no prototype }
  else if Assigned(GNativeArrayJoinRuntime) and Assigned(GNativeArrayJoinRuntime.FVM) then
    Rec.Delegate := GNativeArrayJoinRuntime.FVM.RecordDelegate;
  Result := SouffleReference(Rec);
end;

function NativeObjectSetPrototypeOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  ProtoVal: TSouffleValue;
begin
  if (AArgCount < 2) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
    Exit(AArgs^);
  Rec := TSouffleRecord(AArgs^.AsReference);
  ProtoVal := PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^;
  if SouffleIsReference(ProtoVal) and Assigned(ProtoVal.AsReference) and
     (ProtoVal.AsReference is TSouffleRecord) then
    Rec.Delegate := TSouffleRecord(ProtoVal.AsReference)
  else if (ProtoVal.Kind = svkNil) and (ProtoVal.Flags = GOCCIA_NIL_NULL) then
    Rec.Delegate := nil;
  Result := AArgs^;
end;

function NativeObjectGetOwnPropertyNames(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Arr: TSouffleArray;
  I: Integer;
  Key: string;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    Result := InvokeGocciaMethod(AReceiver, 'getOwnPropertyNames', AArgs, AArgCount);
    Exit;
  end;
  Rec := TSouffleRecord(AArgs^.AsReference);
  Arr := TSouffleArray.Create(Rec.Count);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Arr);
  for I := 0 to Rec.Count - 1 do
  begin
    Key := Rec.GetOrderedKey(I);
    if (Copy(Key, 1, 5) <> '@@sym') and
       ((Length(Key) = 0) or (Key[1] <> '#')) then
      Arr.Push(SouffleString(Key));
  end;
  Result := SouffleReference(Arr);
end;

function NativeObjectGetOwnPropertyDescriptor(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec, DescRec: TSouffleRecord;
  Key: string;
  Val, GetterVal, SetterVal: TSouffleValue;
  Flags: Byte;
begin
  if (AArgCount < 2) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    Result := InvokeGocciaMethod(AReceiver, 'getOwnPropertyDescriptor', AArgs, AArgCount);
    Exit;
  end;
  Rec := TSouffleRecord(AArgs^.AsReference);
  Key := GNativeArrayJoinRuntime.CoerceToString(
    PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);
  if not Rec.Get(Key, Val) then
  begin
    { Check for accessor descriptors }
    if Rec.HasGetters and Rec.Getters.Get(Key, GetterVal) then
    begin
      DescRec := TSouffleRecord.Create(4);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(DescRec);
      DescRec.Put('get', GetterVal);
      if Rec.HasSetters and Rec.Setters.Get(Key, SetterVal) then
        DescRec.Put('set', SetterVal)
      else
        DescRec.Put('set', SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
      DescRec.Put('enumerable', SouffleBoolean(True));
      DescRec.Put('configurable', SouffleBoolean(True));
      Exit(SouffleReference(DescRec));
    end;
    Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  end;

  Flags := Rec.GetEntryFlags(Key);
  DescRec := TSouffleRecord.Create(4);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(DescRec);
  DescRec.Put('value', Val);
  DescRec.Put('writable', SouffleBoolean((Flags and SOUFFLE_PROP_WRITABLE) <> 0));
  DescRec.Put('enumerable', SouffleBoolean((Flags and SOUFFLE_PROP_ENUMERABLE) <> 0));
  DescRec.Put('configurable', SouffleBoolean((Flags and SOUFFLE_PROP_CONFIGURABLE) <> 0));
  Result := SouffleReference(DescRec);
end;

function NativeObjectDefineProperty(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec, DescRec: TSouffleRecord;
  Key: string;
  Val, FlagVal, GetterVal, SetterVal: TSouffleValue;
  Flags: Byte;
  HasValue, HasGetter, HasSetter: Boolean;
begin
  if (AArgCount < 3) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) or
     not (AArgs^.AsReference is TSouffleRecord) then
  begin
    Result := InvokeGocciaMethod(AReceiver, 'defineProperty', AArgs, AArgCount);
    Exit;
  end;
  Rec := TSouffleRecord(AArgs^.AsReference);
  Key := GNativeArrayJoinRuntime.CoerceToString(
    PSouffleValue(PByte(AArgs) + SizeOf(TSouffleValue))^);

  Val := PSouffleValue(PByte(AArgs) + 2 * SizeOf(TSouffleValue))^;
  if not SouffleIsReference(Val) or not Assigned(Val.AsReference) or
     not (Val.AsReference is TSouffleRecord) then
    Exit(AArgs^);

  DescRec := TSouffleRecord(Val.AsReference);

  { Check for accessor descriptor }
  HasGetter := DescRec.Get('get', GetterVal);
  HasSetter := DescRec.Get('set', SetterVal);

  if HasGetter or HasSetter then
  begin
    if HasGetter then
      Rec.Getters.Put(Key, GetterVal);
    if HasSetter then
      Rec.Setters.Put(Key, SetterVal);
    Exit(AArgs^);
  end;

  { Data descriptor }
  Flags := SOUFFLE_PROP_DEFAULT;
  HasValue := DescRec.Get('value', Val);

  if DescRec.Get('writable', FlagVal) then
  begin
    if (FlagVal.Kind = svkBoolean) and not FlagVal.AsBoolean then
      Flags := Flags and (not SOUFFLE_PROP_WRITABLE);
  end;
  if DescRec.Get('enumerable', FlagVal) then
  begin
    if (FlagVal.Kind = svkBoolean) and not FlagVal.AsBoolean then
      Flags := Flags and (not SOUFFLE_PROP_ENUMERABLE);
  end;
  if DescRec.Get('configurable', FlagVal) then
  begin
    if (FlagVal.Kind = svkBoolean) and not FlagVal.AsBoolean then
      Flags := Flags and (not SOUFFLE_PROP_CONFIGURABLE);
  end;

  if HasValue then
    Rec.PutWithFlags(Key, Val, Flags)
  else if Rec.Has(Key) then
    Rec.SetEntryFlags(Key, Flags);

  Result := AArgs^;
end;

function NativeObjectGroupBy(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  { Delegate to Goccia — complex iterator + Map creation }
  Result := InvokeGocciaMethod(AReceiver, 'groupBy', AArgs, AArgCount);
end;

function NativeObjectGetPrototypeOf(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
begin
  if (AArgCount < 1) or not SouffleIsReference(AArgs^) or
     not Assigned(AArgs^.AsReference) then
    Exit(SouffleNilWithFlags(GOCCIA_NIL_NULL));
  if AArgs^.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AArgs^.AsReference);
    if Assigned(Rec.Delegate) then
      Exit(SouffleReference(Rec.Delegate))
    else
      Exit(SouffleNilWithFlags(GOCCIA_NIL_NULL));
  end;
  Result := InvokeGocciaMethod(AReceiver, 'getPrototypeOf', AArgs, AArgCount);
end;

function NativeObjectFromEntries(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  IterObj, ItemVal: TSouffleValue;
  Done: Boolean;
  EntryArr: TSouffleArray;
  Key: string;
begin
  Rec := TSouffleRecord.Create(4);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  if Assigned(GNativeArrayJoinRuntime.FVM) then
    Rec.Delegate := GNativeArrayJoinRuntime.FVM.RecordDelegate;
  if AArgCount < 1 then
    Exit(SouffleReference(Rec));

  IterObj := GNativeArrayJoinRuntime.GetIterator(AArgs^, False);
  repeat
    ItemVal := GNativeArrayJoinRuntime.IteratorNext(IterObj, Done);
    if not Done then
    begin
      if SouffleIsReference(ItemVal) and Assigned(ItemVal.AsReference) and
         (ItemVal.AsReference is TSouffleArray) then
      begin
        EntryArr := TSouffleArray(ItemVal.AsReference);
        if EntryArr.Count >= 2 then
        begin
          Key := GNativeArrayJoinRuntime.CoerceToString(EntryArr.Get(0));
          Rec.Put(Key, EntryArr.Get(1));
        end;
      end;
    end;
  until Done;
  Result := SouffleReference(Rec);
end;

{ === Native String static callbacks === }

function NativeStringFromCharCode(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  I, Code: Integer;
begin
  S := '';
  for I := 0 to AArgCount - 1 do
  begin
    Code := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
    S := S + Chr(Code and $FFFF);
  end;
  Result := SouffleString(S);
end;

function NativeStringFromCodePoint(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  S: string;
  I, CP: Integer;
begin
  S := '';
  for I := 0 to AArgCount - 1 do
  begin
    CP := Trunc(GNativeArrayJoinRuntime.CoerceToNumber(
      PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));
    if (CP < 0) or (CP > $10FFFF) then
    begin
      ThrowRangeErrorNative('Invalid code point ' + IntToStr(CP));
      Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
    end;
    if CP <= $FFFF then
      S := S + Chr(CP)
    else
    begin
      Dec(CP, $10000);
      S := S + Chr($D800 + (CP shr 10)) + Chr($DC00 + (CP and $3FF));
    end;
  end;
  Result := SouffleString(S);
end;

function CreateBlueprintFromConstructor(
  const ARuntime: TGocciaRuntimeOperations;
  const AName: string;
  const ADelegate: TSouffleRecord;
  const AGocciaFn: TGocciaNativeFunctionValue): TSouffleBlueprint;
var
  PropNames: TArray<string>;
  I: Integer;
  PropVal: TGocciaValue;
begin
  Result := ARuntime.CreateBuiltinBlueprint(AName, 0, ADelegate, '');

  { Extract non-callable properties (constants) from Goccia constructor }
  PropNames := AGocciaFn.GetOwnPropertyNames;
  for I := 0 to Length(PropNames) - 1 do
  begin
    if (PropNames[I] = 'length') or (PropNames[I] = 'name') or
       (PropNames[I] = 'prototype') or (PropNames[I] = 'caller') or
       (PropNames[I] = 'arguments') then
      Continue;
    PropVal := AGocciaFn.GetProperty(PropNames[I]);
    if Assigned(PropVal) and not PropVal.IsCallable then
      Result.StaticFields.Put(PropNames[I], ARuntime.ToSouffleValue(PropVal));
  end;
  { Callable methods are added as native callbacks by the caller }
end;

procedure TGocciaRuntimeOperations.RegisterDelegates;
var
  SymSpeciesKey, SymTagKey: string;
  TAKind: TSouffleTypedArrayKind;
  SharedTAProto: TSouffleRecord;
  TagGetterFn: TSouffleNativeFunction;
begin
  if not Assigned(FVM) then Exit;

  RegisterConstGlobal('undefined', SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  RegisterConstGlobal('NaN', SouffleFloat(NaN));
  RegisterConstGlobal('Infinity', SouffleFloat(Infinity));

  GNativeArrayJoinRuntime := Self;
  GThrowRangeErrorRuntime := Self;

  FStringDelegate := TSouffleRecord(
    BuildDelegate(STRING_PROTOTYPE_METHODS));
  FNumberDelegate := TSouffleRecord(
    BuildDelegate(NUMBER_PROTOTYPE_METHODS));

  FMapDelegate := TSouffleRecord(
    BuildDelegate(MAP_PROTOTYPE_METHODS));
  FSetDelegate := TSouffleRecord(
    BuildDelegate(SET_PROTOTYPE_METHODS));
  FMapIteratorDelegate := TSouffleRecord(
    BuildDelegate(MAP_ITERATOR_METHODS));
  FSetIteratorDelegate := TSouffleRecord(
    BuildDelegate(SET_ITERATOR_METHODS));
  FPromiseDelegate := TSouffleRecord(
    BuildDelegate(PROMISE_PROTOTYPE_METHODS));
  FArrayBufferDelegate := TSouffleRecord(
    BuildDelegate(ARRAYBUFFER_PROTOTYPE_METHODS));
  FSharedArrayBufferDelegate := TSouffleRecord(
    BuildDelegate(ARRAYBUFFER_PROTOTYPE_METHODS));
  FTypedArrayDelegate := TSouffleRecord(
    BuildDelegate(TYPEDARRAY_PROTOTYPE_METHODS));
  FErrorDelegate := TSouffleRecord(
    BuildDelegate(ERROR_PROTOTYPE_METHODS));

  FVM.ArrayDelegate := TSouffleRecord(
    BuildDelegate(ARRAY_PROTOTYPE_METHODS));
  FVM.RecordDelegate := TSouffleRecord(
    BuildDelegate(RECORD_PROTOTYPE_METHODS));

  { Create blueprints for built-in types }
  FMapBlueprint := CreateBuiltinBlueprint('Map', 1, FMapDelegate, 'entries');
  FSetBlueprint := CreateBuiltinBlueprint('Set', 1, FSetDelegate, 'values');
  FPromiseBlueprint := CreateBuiltinBlueprint('Promise', 1, FPromiseDelegate, '');
  FArrayBufferBlueprint := CreateBuiltinBlueprint('ArrayBuffer', 1, FArrayBufferDelegate, '');
  FSharedArrayBufferBlueprint := CreateBuiltinBlueprint('SharedArrayBuffer', 1, FSharedArrayBufferDelegate, '');
  FTypedArrayBlueprints[stakInt8] := CreateBuiltinBlueprint('Int8Array', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakUint8] := CreateBuiltinBlueprint('Uint8Array', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakUint8Clamped] := CreateBuiltinBlueprint('Uint8ClampedArray', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakInt16] := CreateBuiltinBlueprint('Int16Array', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakUint16] := CreateBuiltinBlueprint('Uint16Array', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakInt32] := CreateBuiltinBlueprint('Int32Array', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakUint32] := CreateBuiltinBlueprint('Uint32Array', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakFloat32] := CreateBuiltinBlueprint('Float32Array', 1, FTypedArrayDelegate, 'values');
  FTypedArrayBlueprints[stakFloat64] := CreateBuiltinBlueprint('Float64Array', 1, FTypedArrayDelegate, 'values');

  { ArrayBuffer/SharedArrayBuffer byteLength as getter-only }
  begin
    TagGetterFn := TSouffleNativeFunction.Create('get byteLength', 0,
      @NativeArrayBufferByteLength);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(TagGetterFn);
    FArrayBufferBlueprint.Prototype.Getters.Put('byteLength',
      SouffleReference(TagGetterFn));
    FSharedArrayBufferBlueprint.Prototype.Getters.Put('byteLength',
      SouffleReference(TagGetterFn));
  end;

  { Error blueprints }
  FErrorBlueprint := CreateBuiltinBlueprint('Error', 0, FErrorDelegate, '');
  FErrorBlueprint.Prototype.Put('name', SouffleString('Error'));
  FErrorBlueprint.Prototype.Put('message', SouffleString(''));

  FTypeErrorBlueprint := CreateBuiltinBlueprint('TypeError', 0, FErrorDelegate, '');
  FTypeErrorBlueprint.SuperBlueprint := FErrorBlueprint;
  FTypeErrorBlueprint.Prototype.Delegate := FErrorBlueprint.Prototype;
  FTypeErrorBlueprint.Prototype.Put('name', SouffleString('TypeError'));

  FRangeErrorBlueprint := CreateBuiltinBlueprint('RangeError', 0, FErrorDelegate, '');
  FRangeErrorBlueprint.SuperBlueprint := FErrorBlueprint;
  FRangeErrorBlueprint.Prototype.Delegate := FErrorBlueprint.Prototype;
  FRangeErrorBlueprint.Prototype.Put('name', SouffleString('RangeError'));

  FReferenceErrorBlueprint := CreateBuiltinBlueprint('ReferenceError', 0, FErrorDelegate, '');
  FReferenceErrorBlueprint.SuperBlueprint := FErrorBlueprint;
  FReferenceErrorBlueprint.Prototype.Delegate := FErrorBlueprint.Prototype;
  FReferenceErrorBlueprint.Prototype.Put('name', SouffleString('ReferenceError'));

  FSyntaxErrorBlueprint := CreateBuiltinBlueprint('SyntaxError', 0, FErrorDelegate, '');
  FSyntaxErrorBlueprint.SuperBlueprint := FErrorBlueprint;
  FSyntaxErrorBlueprint.Prototype.Delegate := FErrorBlueprint.Prototype;
  FSyntaxErrorBlueprint.Prototype.Put('name', SouffleString('SyntaxError'));

  FURIErrorBlueprint := CreateBuiltinBlueprint('URIError', 0, FErrorDelegate, '');
  FURIErrorBlueprint.SuperBlueprint := FErrorBlueprint;
  FURIErrorBlueprint.Prototype.Delegate := FErrorBlueprint.Prototype;
  FURIErrorBlueprint.Prototype.Put('name', SouffleString('URIError'));

  FAggregateErrorBlueprint := CreateBuiltinBlueprint('AggregateError', 0, FErrorDelegate, '');
  FAggregateErrorBlueprint.SuperBlueprint := FErrorBlueprint;
  FAggregateErrorBlueprint.Prototype.Delegate := FErrorBlueprint.Prototype;
  FAggregateErrorBlueprint.Prototype.Put('name', SouffleString('AggregateError'));

  FDOMExceptionBlueprint := CreateBuiltinBlueprint('DOMException', 0, FErrorDelegate, '');
  FDOMExceptionBlueprint.SuperBlueprint := FErrorBlueprint;
  FDOMExceptionBlueprint.Prototype.Delegate := FErrorBlueprint.Prototype;
  FDOMExceptionBlueprint.Prototype.Put('name', SouffleString('Error'));

  { Error static methods }
  AddStaticMethod(FErrorBlueprint, 'isError', 1, @NativeErrorIsError);

  { ArrayBuffer static methods }
  AddStaticMethod(FArrayBufferBlueprint, 'isView', 1, @NativeArrayBufferIsView);

  { Symbol.toStringTag getter on shared TypedArray method delegate }
  begin
    SymTagKey := '@@sym:' + IntToStr(TGocciaSymbolValue.WellKnownToStringTag.Id);
    TagGetterFn := TSouffleNativeFunction.Create('get [Symbol.toStringTag]', 0,
      @NativeTypedArrayToStringTagGetter);
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AllocateObject(TagGetterFn);
    FTypedArrayDelegate.Getters.Put(SymTagKey, SouffleReference(TagGetterFn));
  end;

  { TypedArray static fields and methods }
  for TAKind := Low(TSouffleTypedArrayKind) to High(TSouffleTypedArrayKind) do
  begin
    FTypedArrayBlueprints[TAKind].StaticFields.Put('BYTES_PER_ELEMENT',
      SouffleInteger(BytesPerElement(TAKind)));
    AddStaticMethod(FTypedArrayBlueprints[TAKind], 'from', 1, @NativeTypedArrayStaticFrom);
    AddStaticMethod(FTypedArrayBlueprints[TAKind], 'of', -1, @NativeTypedArrayStaticOf);
  end;

  { Wire Souffle microtask invoker }
  if Assigned(TGocciaMicrotaskQueue.Instance) then
    TGocciaMicrotaskQueue.Instance.SouffleInvoker := InvokeSouffleMicrotask;

  { Map/Set constructor methods (for super() calls in subclasses) }
  AddMethodToBlueprint(FMapBlueprint, 'constructor', 1, @NativeMapConstructor);
  AddMethodToBlueprint(FSetBlueprint, 'constructor', 1, @NativeSetConstructor);

  { Map static methods }
  AddStaticMethod(FMapBlueprint, 'groupBy', 2, @NativeMapGroupBy);

  { Promise static methods }
  AddStaticMethod(FPromiseBlueprint, 'resolve', 1, @NativePromiseStaticResolve);
  AddStaticMethod(FPromiseBlueprint, 'reject', 1, @NativePromiseStaticReject);
  AddStaticMethod(FPromiseBlueprint, 'all', 1, @NativePromiseStaticAll);
  AddStaticMethod(FPromiseBlueprint, 'allSettled', 1, @NativePromiseStaticAllSettled);
  AddStaticMethod(FPromiseBlueprint, 'race', 1, @NativePromiseStaticRace);
  AddStaticMethod(FPromiseBlueprint, 'any', 1, @NativePromiseStaticAny);
  AddStaticMethod(FPromiseBlueprint, 'withResolvers', 0, @NativePromiseStaticWithResolvers);
  AddStaticMethod(FPromiseBlueprint, 'try', 1, @NativePromiseStaticTry);

  { Symbol.species — handled in GetSymbolOnNativeObject for blueprints }
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
    { Replace Error globals with blueprints BEFORE TGocciaWrappedValue check }
    if (Key = 'Error') and Assigned(FErrorBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FErrorBlueprint)); Continue; end;
    if (Key = 'TypeError') and Assigned(FTypeErrorBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypeErrorBlueprint)); Continue; end;
    if (Key = 'RangeError') and Assigned(FRangeErrorBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FRangeErrorBlueprint)); Continue; end;
    if (Key = 'ReferenceError') and Assigned(FReferenceErrorBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FReferenceErrorBlueprint)); Continue; end;
    if (Key = 'SyntaxError') and Assigned(FSyntaxErrorBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FSyntaxErrorBlueprint)); Continue; end;
    if (Key = 'URIError') and Assigned(FURIErrorBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FURIErrorBlueprint)); Continue; end;
    if (Key = 'AggregateError') and Assigned(FAggregateErrorBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FAggregateErrorBlueprint)); Continue; end;
    if (Key = 'DOMException') and Assigned(FDOMExceptionBlueprint) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FDOMExceptionBlueprint)); Continue; end;
    if not (Val.AsReference is TGocciaWrappedValue) then Continue;

    Wrapped := TGocciaWrappedValue(Val.AsReference);
    GocciaVal := Wrapped.Value;

    { Replace Map/Set constructors with blueprints }
    if (Key = 'Map') and Assigned(FMapBlueprint) then
    begin
      FGlobals.AddOrSetValue(Key, SouffleReference(FMapBlueprint));
      Continue;
    end;
    if (Key = 'Set') and Assigned(FSetBlueprint) then
    begin
      FGlobals.AddOrSetValue(Key, SouffleReference(FSetBlueprint));
      Continue;
    end;
    if (Key = CONSTRUCTOR_PROMISE) and Assigned(FPromiseBlueprint) then
    begin
      FGlobals.AddOrSetValue(Key, SouffleReference(FPromiseBlueprint));
      Continue;
    end;
    if (Key = 'ArrayBuffer') and Assigned(FArrayBufferBlueprint) then
    begin
      FGlobals.AddOrSetValue(Key, SouffleReference(FArrayBufferBlueprint));
      Continue;
    end;
    if (Key = 'SharedArrayBuffer') and Assigned(FSharedArrayBufferBlueprint) then
    begin
      FGlobals.AddOrSetValue(Key, SouffleReference(FSharedArrayBufferBlueprint));
      Continue;
    end;
    { Old duplicates removed — Error replacements are now before TGocciaWrappedValue check }
    if (Key = 'Int8Array') and Assigned(FTypedArrayBlueprints[stakInt8]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakInt8])); Continue; end;
    if (Key = 'Uint8Array') and Assigned(FTypedArrayBlueprints[stakUint8]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakUint8])); Continue; end;
    if (Key = 'Uint8ClampedArray') and Assigned(FTypedArrayBlueprints[stakUint8Clamped]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakUint8Clamped])); Continue; end;
    if (Key = 'Int16Array') and Assigned(FTypedArrayBlueprints[stakInt16]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakInt16])); Continue; end;
    if (Key = 'Uint16Array') and Assigned(FTypedArrayBlueprints[stakUint16]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakUint16])); Continue; end;
    if (Key = 'Int32Array') and Assigned(FTypedArrayBlueprints[stakInt32]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakInt32])); Continue; end;
    if (Key = 'Uint32Array') and Assigned(FTypedArrayBlueprints[stakUint32]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakUint32])); Continue; end;
    if (Key = 'Float32Array') and Assigned(FTypedArrayBlueprints[stakFloat32]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakFloat32])); Continue; end;
    if (Key = 'Float64Array') and Assigned(FTypedArrayBlueprints[stakFloat64]) then begin FGlobals.AddOrSetValue(Key, SouffleReference(FTypedArrayBlueprints[stakFloat64])); Continue; end;

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

      { Convert core constructors to blueprints with static fields }
      if Key = 'Array' then begin
        FArrayBlueprint := CreateBlueprintFromConstructor(Self, 'Array', TSouffleRecord(FVM.ArrayDelegate), NativeFnVal);
        AddStaticMethod(FArrayBlueprint, 'isArray', 1, @NativeArrayIsArray);
        AddStaticMethod(FArrayBlueprint, 'from', 1, @NativeArrayFrom);
        AddStaticMethod(FArrayBlueprint, 'of', -1, @NativeArrayOf);
        FGlobals.AddOrSetValue(Key, SouffleReference(FArrayBlueprint)); end
      else if Key = 'Object' then begin
        FObjectBlueprint := CreateBlueprintFromConstructor(Self, 'Object', TSouffleRecord(FVM.RecordDelegate), NativeFnVal);
        AddStaticMethod(FObjectBlueprint, 'keys', 1, @NativeObjectKeys);
        AddStaticMethod(FObjectBlueprint, 'values', 1, @NativeObjectValues);
        AddStaticMethod(FObjectBlueprint, 'entries', 1, @NativeObjectEntries);
        AddStaticMethod(FObjectBlueprint, 'assign', -1, @NativeObjectAssign);
        AddStaticMethod(FObjectBlueprint, 'is', 2, @NativeObjectIs);
        AddStaticMethod(FObjectBlueprint, 'hasOwn', 2, @NativeObjectHasOwn);
        AddStaticMethod(FObjectBlueprint, 'freeze', 1, @NativeObjectFreeze);
        AddStaticMethod(FObjectBlueprint, 'isFrozen', 1, @NativeObjectIsFrozen);
        AddStaticMethod(FObjectBlueprint, 'seal', 1, @NativeObjectSeal);
        AddStaticMethod(FObjectBlueprint, 'isSealed', 1, @NativeObjectIsSealed);
        AddStaticMethod(FObjectBlueprint, 'preventExtensions', 1, @NativeObjectPreventExtensions);
        AddStaticMethod(FObjectBlueprint, 'isExtensible', 1, @NativeObjectIsExtensible);
        AddStaticMethod(FObjectBlueprint, 'create', 1, @NativeObjectCreate);
        AddStaticMethod(FObjectBlueprint, 'setPrototypeOf', 2, @NativeObjectSetPrototypeOf);
        AddStaticMethod(FObjectBlueprint, 'getPrototypeOf', 1, @NativeObjectGetPrototypeOf);
        AddStaticMethod(FObjectBlueprint, 'getOwnPropertyNames', 1, @NativeObjectGetOwnPropertyNames);
        AddStaticMethod(FObjectBlueprint, 'getOwnPropertyDescriptor', 2, @NativeObjectGetOwnPropertyDescriptor);
        AddStaticMethod(FObjectBlueprint, 'defineProperty', 3, @NativeObjectDefineProperty);
        AddStaticMethod(FObjectBlueprint, 'fromEntries', 1, @NativeObjectFromEntries);
        AddStaticMethod(FObjectBlueprint, 'groupBy', 2, @NativeObjectGroupBy);
        FGlobals.AddOrSetValue(Key, SouffleReference(FObjectBlueprint)); end
      else if Key = 'String' then begin
        FStringBlueprint := CreateBlueprintFromConstructor(Self, 'String', FStringDelegate, NativeFnVal);
        AddStaticMethod(FStringBlueprint, 'fromCharCode', 1, @NativeStringFromCharCode);
        AddStaticMethod(FStringBlueprint, 'fromCodePoint', 1, @NativeStringFromCodePoint);
        FGlobals.AddOrSetValue(Key, SouffleReference(FStringBlueprint)); end
      else if Key = 'Number' then begin
        FNumberBlueprint := CreateBlueprintFromConstructor(Self, 'Number', FNumberDelegate, NativeFnVal);
        AddStaticMethod(FNumberBlueprint, 'isNaN', 0, @NativeNumberIsNaN);
        AddStaticMethod(FNumberBlueprint, 'isFinite', 0, @NativeNumberIsFinite);
        AddStaticMethod(FNumberBlueprint, 'isInteger', 0, @NativeNumberIsInteger);
        AddStaticMethod(FNumberBlueprint, 'isSafeInteger', 0, @NativeNumberIsSafeInteger);
        AddStaticMethod(FNumberBlueprint, 'parseInt', 1, @NativeNumberParseInt);
        AddStaticMethod(FNumberBlueprint, 'parseFloat', 1, @NativeNumberParseFloat);
        FGlobals.AddOrSetValue(Key, SouffleReference(FNumberBlueprint)); end
      else if Key = 'Boolean' then begin
        FBooleanBlueprint := CreateBlueprintFromConstructor(Self, 'Boolean', nil, NativeFnVal);
        FGlobals.AddOrSetValue(Key, SouffleReference(FBooleanBlueprint)); end
      else if Key = 'Symbol' then begin
        FSymbolBlueprint := CreateBlueprintFromConstructor(Self, 'Symbol', nil, NativeFnVal);
        FGlobals.AddOrSetValue(Key, SouffleReference(FSymbolBlueprint)); end
      else if Key = 'Function' then begin
        FFunctionBlueprint := CreateBlueprintFromConstructor(Self, 'Function', nil, NativeFnVal);
        FGlobals.AddOrSetValue(Key, SouffleReference(FFunctionBlueprint)); end
      else
      begin
        BridgedFn := TGocciaBridgedFunction.Create(NativeFnVal, Self);
        if Assigned(GC) then GC.AllocateObject(BridgedFn);
        FGlobals.AddOrSetValue(Key, SouffleReference(BridgedFn));
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
  TAKind: TSouffleTypedArrayKind;
begin
  for GlobalPair in FGlobals do
    if SouffleIsReference(GlobalPair.Value) and Assigned(GlobalPair.Value.AsReference)
      and not GlobalPair.Value.AsReference.GCMarked then
      GlobalPair.Value.AsReference.MarkReferences;

  for GlobalPair in FExports do
    if SouffleIsReference(GlobalPair.Value) and Assigned(GlobalPair.Value.AsReference)
      and not GlobalPair.Value.AsReference.GCMarked then
      GlobalPair.Value.AsReference.MarkReferences;

  for GlobalPair in FModuleCache do
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
  if Assigned(FMapIteratorDelegate) and not FMapIteratorDelegate.GCMarked then
    FMapIteratorDelegate.MarkReferences;
  if Assigned(FSetIteratorDelegate) and not FSetIteratorDelegate.GCMarked then
    FSetIteratorDelegate.MarkReferences;
  if Assigned(FMapBlueprint) and not FMapBlueprint.GCMarked then
    FMapBlueprint.MarkReferences;
  if Assigned(FSetBlueprint) and not FSetBlueprint.GCMarked then
    FSetBlueprint.MarkReferences;
  if Assigned(FPromiseDelegate) and not FPromiseDelegate.GCMarked then
    FPromiseDelegate.MarkReferences;
  if Assigned(FPromiseBlueprint) and not FPromiseBlueprint.GCMarked then
    FPromiseBlueprint.MarkReferences;
  if Assigned(FArrayBufferBlueprint) and not FArrayBufferBlueprint.GCMarked then
    FArrayBufferBlueprint.MarkReferences;
  if Assigned(FSharedArrayBufferBlueprint) and not FSharedArrayBufferBlueprint.GCMarked then
    FSharedArrayBufferBlueprint.MarkReferences;
  if Assigned(FArrayBufferDelegate) and not FArrayBufferDelegate.GCMarked then
    FArrayBufferDelegate.MarkReferences;
  if Assigned(FSharedArrayBufferDelegate) and not FSharedArrayBufferDelegate.GCMarked then
    FSharedArrayBufferDelegate.MarkReferences;
  if Assigned(FTypedArrayDelegate) and not FTypedArrayDelegate.GCMarked then
    FTypedArrayDelegate.MarkReferences;
  for TAKind := Low(TSouffleTypedArrayKind) to High(TSouffleTypedArrayKind) do
    if Assigned(FTypedArrayBlueprints[TAKind]) and not FTypedArrayBlueprints[TAKind].GCMarked then
      FTypedArrayBlueprints[TAKind].MarkReferences;
  if Assigned(FArrayBlueprint) and not FArrayBlueprint.GCMarked then
    FArrayBlueprint.MarkReferences;
  if Assigned(FObjectBlueprint) and not FObjectBlueprint.GCMarked then
    FObjectBlueprint.MarkReferences;
  if Assigned(FStringBlueprint) and not FStringBlueprint.GCMarked then
    FStringBlueprint.MarkReferences;
  if Assigned(FNumberBlueprint) and not FNumberBlueprint.GCMarked then
    FNumberBlueprint.MarkReferences;
  if Assigned(FBooleanBlueprint) and not FBooleanBlueprint.GCMarked then
    FBooleanBlueprint.MarkReferences;
  if Assigned(FSymbolBlueprint) and not FSymbolBlueprint.GCMarked then
    FSymbolBlueprint.MarkReferences;
  if Assigned(FFunctionBlueprint) and not FFunctionBlueprint.GCMarked then
    FFunctionBlueprint.MarkReferences;
  if Assigned(FErrorBlueprint) and not FErrorBlueprint.GCMarked then
    FErrorBlueprint.MarkReferences;
  if Assigned(FTypeErrorBlueprint) and not FTypeErrorBlueprint.GCMarked then
    FTypeErrorBlueprint.MarkReferences;
  if Assigned(FRangeErrorBlueprint) and not FRangeErrorBlueprint.GCMarked then
    FRangeErrorBlueprint.MarkReferences;
  if Assigned(FReferenceErrorBlueprint) and not FReferenceErrorBlueprint.GCMarked then
    FReferenceErrorBlueprint.MarkReferences;
  if Assigned(FSyntaxErrorBlueprint) and not FSyntaxErrorBlueprint.GCMarked then
    FSyntaxErrorBlueprint.MarkReferences;
  if Assigned(FURIErrorBlueprint) and not FURIErrorBlueprint.GCMarked then
    FURIErrorBlueprint.MarkReferences;
  if Assigned(FAggregateErrorBlueprint) and not FAggregateErrorBlueprint.GCMarked then
    FAggregateErrorBlueprint.MarkReferences;
  if Assigned(FDOMExceptionBlueprint) and not FDOMExceptionBlueprint.GCMarked then
    FDOMExceptionBlueprint.MarkReferences;
  if Assigned(FErrorDelegate) and not FErrorDelegate.GCMarked then
    FErrorDelegate.MarkReferences;
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
