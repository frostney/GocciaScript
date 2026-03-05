unit Goccia.Runtime.Operations;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Souffle.Bytecode.Chunk,
  Souffle.Heap,
  Souffle.Value,
  Souffle.VM,
  Souffle.VM.Closure,
  Souffle.VM.RuntimeOperations,

  Goccia.AST.Statements,
  Goccia.Values.Error,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

const
  GOCCIA_HEAP_WRAPPED = 128;

type
  TGocciaRuntimeOperations = class;

  TGocciaPendingClassEntry = record
    ClassDefinition: TGocciaClassDefinition;
    Line: Integer;
    Column: Integer;
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
    FGlobals: TDictionary<string, TSouffleValue>;
    FConstGlobals: TDictionary<string, Boolean>;
    FExports: TDictionary<string, TSouffleValue>;
    FClosureBridgeCache: TDictionary<TSouffleClosure, TObject>;
    FArrayBridgeCache: TDictionary<TObject, TObject>;
    FArrayBridgeReverse: TDictionary<TObject, TObject>;
    FRecordBridgeCache: TDictionary<TObject, TObject>;
    FFormalParameterCounts: TDictionary<TSouffleFunctionTemplate, Integer>;
    FPendingClasses: array of TGocciaPendingClassEntry;
    FPendingClassCount: Integer;
    FClassDefinitionScopes: TDictionary<TObject, TObject>;
    FWrappedValues: TList;
    FBridgeCallDepth: Integer;
    FVM: TSouffleVM;
    FEngine: TObject;
    FSourcePath: string;

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

    procedure PropertyWriteViolation(const AObject: TSouffleValue;
      const AKey: string);
    procedure PropertyDeleteViolation(const AObject: TSouffleValue;
      const AKey: string);
    procedure ThrowTypeErrorMessage(
      const AMessage: string);

    function EvaluateClassByIndex(
      const AIndex: Integer): TSouffleValue;
    function FinalizeEnum(const ARecord: TSouffleValue;
      const AName: string): TSouffleValue;

    procedure ExtendedOperation(const ASubOp: UInt8;
      var ADest: TSouffleValue; const AOperand, AExtra: TSouffleValue;
      const ATemplate: TSouffleFunctionTemplate;
      const AOperandIndex: UInt8); override;
    procedure MarkExternalRoots; override;

    function AddPendingClassDef(
      const AClassDef: TGocciaClassDefinition;
      const ALine, AColumn: Integer): Integer;

    function UnwrapToGocciaValue(const AValue: TSouffleValue): TGocciaValue;
    function ToSouffleValue(const AValue: TGocciaValue): TSouffleValue;

    function ResolveProxyGet(const ATarget: TSouffleHeapObject;
      const AName: string): TGocciaValue;
    procedure ResolveProxySet(const ATarget: TSouffleHeapObject;
      const AName: string; const AValue: TGocciaValue);

    procedure RegisterDelegates;
    procedure RegisterGlobal(const AName: string; const AValue: TSouffleValue);
    procedure RegisterConstGlobal(const AName: string;
      const AValue: TSouffleValue);
    procedure RegisterFormalParameterCount(
      const ATemplate: TSouffleFunctionTemplate; const ACount: Integer);
    function GetFormalParameterCount(
      const ATemplate: TSouffleFunctionTemplate): Integer;
    property ModuleExports: TDictionary<string, TSouffleValue> read FExports;
    property VM: TSouffleVM read FVM write FVM;
    property Engine: TObject read FEngine write FEngine;
    property SourcePath: string read FSourcePath write FSourcePath;
  end;

implementation

uses
  Math,
  SysUtils,

  Souffle.Bytecode.Debug,
  Souffle.Compound,
  Souffle.GarbageCollector,
  Souffle.VM.CallFrame,
  Souffle.VM.Exception,
  Souffle.VM.NativeFunction,

  Goccia.Arguments.Collection,
  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Evaluator,
  Goccia.Evaluator.TypeOperations,
  Goccia.GarbageCollector,
  Goccia.Interpreter,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ArrayValue,
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
  Goccia.Values.NativeFunction,
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
    FRuntime.FRecordBridgeCache.Clear;
  end;
  Result := FRuntime.UnwrapToGocciaValue(SouffleResult);
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
    FBridgeRuntime.FRecordBridgeCache.Clear;
  end;
  Result := FBridgeRuntime.UnwrapToGocciaValue(SouffleResult);
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
  Seen: TDictionary<string, Boolean>;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    Seen := TDictionary<string, Boolean>.Create;
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
  Seen: TDictionary<string, Boolean>;
begin
  if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    Seen := TDictionary<string, Boolean>.Create;
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
begin
  Result := inherited GetSymbolProperty(ASymbol);
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

{ TGocciaRuntimeOperations }

constructor TGocciaRuntimeOperations.Create;
begin
  inherited Create;
  FGlobals := TDictionary<string, TSouffleValue>.Create;
  FConstGlobals := TDictionary<string, Boolean>.Create;
  FExports := TDictionary<string, TSouffleValue>.Create;
  FClosureBridgeCache := TDictionary<TSouffleClosure, TObject>.Create;
  FArrayBridgeCache := TDictionary<TObject, TObject>.Create;
  FArrayBridgeReverse := TDictionary<TObject, TObject>.Create;
  FRecordBridgeCache := TDictionary<TObject, TObject>.Create;
  FClassDefinitionScopes := TDictionary<TObject, TObject>.Create;
  FWrappedValues := TList.Create;
  FFormalParameterCounts := TDictionary<TSouffleFunctionTemplate, Integer>.Create;
  FBridgeCallDepth := 0;
  FVM := nil;
  TGocciaGarbageCollector.Initialize;
  TGocciaGarbageCollector.Instance.SetExternalRootMarker(MarkWrappedGocciaValues);
end;

destructor TGocciaRuntimeOperations.Destroy;
begin
  if Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.SetExternalRootMarker(nil);
  FGlobals.Free;
  FConstGlobals.Free;
  FExports.Free;
  FClosureBridgeCache.Free;
  FArrayBridgeCache.Free;
  FArrayBridgeReverse.Free;
  FRecordBridgeCache.Free;
  FClassDefinitionScopes.Free;
  FWrappedValues.Free;
  FFormalParameterCounts.Free;
  inherited;
end;

function TGocciaRuntimeOperations.WrapGocciaValue(
  const AValue: TGocciaValue): TSouffleValue;
var
  Wrapped: TGocciaWrappedValue;
begin
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

procedure SyncDefinitionScopesBack(
  const ARuntime: TGocciaRuntimeOperations); forward;

function BlueprintToClassValue(const ABlueprint: TSouffleBlueprint;
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
  Result := APrimitive.Kind <> svkReference;
end;

function FindRecordMethod(const ARec: TSouffleRecord;
  const AName: string; out AMethod: TSouffleValue): Boolean;
var
  Bp: TSouffleBlueprint;
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

function TGocciaRuntimeOperations.UnwrapToGocciaValue(
  const AValue: TSouffleValue): TGocciaValue;
var
  CachedBridge: TObject;
begin
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
      else if AValue.AsReference is TSouffleArray then
      begin
        if not FArrayBridgeCache.TryGetValue(AValue.AsReference, CachedBridge) then
        begin
          CachedBridge := TGocciaArrayValue.Create;
          FArrayBridgeCache.Add(AValue.AsReference, CachedBridge);
          FArrayBridgeReverse.AddOrSetValue(CachedBridge, AValue.AsReference);
          ConvertSouffleArrayInto(Self,
            TSouffleArray(AValue.AsReference),
            TGocciaArrayValue(CachedBridge));
        end;
        Result := TGocciaValue(CachedBridge);
      end
      else if AValue.AsReference is TSouffleRecord then
      begin
        if not FRecordBridgeCache.TryGetValue(AValue.AsReference, CachedBridge) then
        begin
          CachedBridge := TGocciaSouffleProxy.Create(AValue.AsReference, Self);
          FRecordBridgeCache.Add(AValue.AsReference, CachedBridge);
        end;
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
          CachedBridge := TGocciaSouffleClosureBridge.Create(
            TSouffleClosure(AValue.AsReference), Self);
          FClosureBridgeCache.Add(TSouffleClosure(AValue.AsReference),
            CachedBridge);
        end;
        Result := TGocciaValue(CachedBridge);
      end
      else if AValue.AsReference is TSouffleNativeFunction then
        Result := TGocciaSouffleNativeFunctionBridge.Create(
          TSouffleNativeFunction(AValue.AsReference), Self)
      else if AValue.AsReference is TSouffleBlueprint then
        Result := BlueprintToClassValue(
          TSouffleBlueprint(AValue.AsReference), Self)
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaRuntimeOperations.ToSouffleValue(
  const AValue: TGocciaValue): TSouffleValue;
var
  NumVal: TGocciaNumberLiteralValue;
begin
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
  else if FArrayBridgeReverse.ContainsKey(AValue) then
    Result := SouffleReference(TSouffleHeapObject(FArrayBridgeReverse[AValue]))
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
    Exit(SouffleString('[object Object]'));
  end;
  if A.AsReference is TGocciaWrappedValue then
  begin
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
         (A.AsReference is TSouffleBlueprint) then
        Result := SouffleString('function')
      else if A.AsReference is TSouffleHeapString then
        Result := SouffleString('string')
      else if A.AsReference is TGocciaWrappedValue then
        Result := SouffleString(
          TGocciaWrappedValue(A.AsReference).Value.TypeOf)
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
      if Rec.Has(KeyStr) then
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
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
    begin
      if AObject.AsReference is TSouffleRecord then
      begin
        Rec := TSouffleRecord(AObject.AsReference);
        if Rec.HasGetters and Rec.Getters.Get(AKey, GetterVal) then
        begin
          if SouffleIsReference(GetterVal) and
             (GetterVal.AsReference is TSouffleClosure) then
            Exit(FVM.ExecuteFunction(
              TSouffleClosure(GetterVal.AsReference), [AObject]));
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
              if SouffleIsReference(GetterVal) and
                 (GetterVal.AsReference is TSouffleClosure) then
                Exit(FVM.ExecuteFunction(
                  TSouffleClosure(GetterVal.AsReference), [AObject]));
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
            if SouffleIsReference(GetterVal) and
               (GetterVal.AsReference is TSouffleClosure) then
              Exit(FVM.ExecuteFunction(
                TSouffleClosure(GetterVal.AsReference), [AObject]));
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
          if SouffleIsReference(GetterVal) and
             (GetterVal.AsReference is TSouffleClosure) then
            Exit(FVM.ExecuteFunction(
              TSouffleClosure(GetterVal.AsReference), [AObject]));
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
            if SouffleIsReference(GetterVal) and
               (GetterVal.AsReference is TSouffleClosure) then
              Exit(FVM.ExecuteFunction(
                TSouffleClosure(GetterVal.AsReference), [AObject]));
            Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
          end;
          if Bp.HasStaticFields and Bp.StaticFields.Get(AKey, Result) then
            Exit;
          Bp := Bp.SuperBlueprint;
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
      end;
    end;

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
        if not Rec.PutChecked(AKey, AValue) then
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

function TGocciaRuntimeOperations.GetIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
var
  GocciaObj: TGocciaValue;
  SymKey: TGocciaSymbolValue;
  PropVal: TGocciaValue;
begin
  try
    if AObject.Kind = svkNil then
    begin
      if AObject.Flags = GOCCIA_NIL_NULL then
        ThrowTypeError('Cannot read properties of null (reading ''' + CoerceKeyToString(AKey) + ''')')
      else
        ThrowTypeError('Cannot read properties of undefined (reading ''' + CoerceKeyToString(AKey) + ''')');
    end;

    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
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
  I: Integer;
begin
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
var
  GocciaConstructor, GocciaResult: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  Context: TGocciaEvaluationContext;
  CachedScope: TObject;
  I: Integer;
  Bp, WalkBp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  CtorMethod: TSouffleValue;
  VMArgs: array of TSouffleValue;
begin
  PushVMFramesToGoccia(FVM);
  try try
    if SouffleIsReference(AConstructor) and
       Assigned(AConstructor.AsReference) and
       (AConstructor.AsReference is TSouffleBlueprint) then
    begin
      Bp := TSouffleBlueprint(AConstructor.AsReference);
      Rec := TSouffleRecord.CreateFromBlueprint(Bp);
      Rec.Delegate := Bp.Prototype;
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
        WalkBp := WalkBp.SuperBlueprint;
      end;
      if Assigned(TSouffleGarbageCollector.Instance) then
        TSouffleGarbageCollector.Instance.AllocateObject(Rec);

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
      Exit;
    end;

    GocciaConstructor := UnwrapToGocciaValue(AConstructor);

    if (GocciaConstructor is TGocciaClassValue) then
    begin
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
  try
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
          if Assigned(TGocciaGarbageCollector.Instance) then
            TGocciaGarbageCollector.Instance.AddTempRoot(IteratorObj);
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

    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
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
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  Value, NextMethod, NextResult, Resolved, DoneValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  ADone := True;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  try
    GocciaVal := UnwrapToGocciaValue(AIterator);

    if GocciaVal is TGocciaIteratorValue then
    begin
      Iterator := TGocciaIteratorValue(GocciaVal);
      IterResult := Iterator.AdvanceNext;

      ADone := IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value;
      if not ADone then
      begin
        Value := IterResult.GetProperty(PROP_VALUE);
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
  Rec: TSouffleRecord;
  Pair: TPair<string, TGocciaValue>;
begin
  if not Assigned(FEngine) then
    Exit(SouffleNil);

  EngineObj := TGocciaEngine(FEngine);
  Module := EngineObj.Interpreter.LoadModule(APath, FSourcePath);
  if not Assigned(Module) then
    Exit(SouffleNil);

  Rec := TSouffleRecord.Create(Module.ExportsTable.Count);
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(Rec);

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
  if Assigned(TGocciaMicrotaskQueue.Instance) then
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
  GocciaVal: TGocciaValue;
  StrVal: TGocciaStringLiteralValue;
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

  GocciaVal := ARuntime.UnwrapToGocciaValue(AElem);
  if (GocciaVal is TGocciaUndefinedLiteralValue) or
     (GocciaVal is TGocciaNullLiteralValue) then
    Exit('');
  StrVal := Goccia.Values.ToPrimitive.ToECMAString(GocciaVal);
  Result := StrVal.Value;
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
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(NewArr);
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
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(NewArr);

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
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(NewArr);
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
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(NewArr);
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
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(NewArr);
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
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(NewArr);
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
  if Assigned(TSouffleGarbageCollector.Instance) then
    TSouffleGarbageCollector.Instance.AllocateObject(Removed);
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

function NativeRecordHasOwnProperty(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Rec: TSouffleRecord;
  Key: string;
  GocciaObj: TGocciaValue;
begin
  Result := SouffleBoolean(False);
  if not SouffleIsReference(AReceiver) or not Assigned(AReceiver.AsReference) then
    Exit;
  if AArgCount < 1 then
    Key := 'undefined'
  else
    Key := GNativeArrayJoinRuntime.CoerceToString(AArgs^);
  if AReceiver.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(AReceiver.AsReference);
    Result := SouffleBoolean(Rec.Has(Key));
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
begin
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

const
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

procedure TGocciaRuntimeOperations.RegisterDelegates;
begin
  if not Assigned(FVM) then Exit;

  RegisterConstGlobal('undefined', SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
  RegisterConstGlobal('NaN', SouffleFloat(NaN));
  RegisterConstGlobal('Infinity', SouffleFloat(Infinity));

  GNativeArrayJoinRuntime := Self;

  FVM.ArrayDelegate := TSouffleRecord(
    BuildDelegate(ARRAY_PROTOTYPE_METHODS));
  FVM.RecordDelegate := TSouffleRecord(
    BuildDelegate(RECORD_PROTOTYPE_METHODS));
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
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if not (SouffleIsReference(ASuperBlueprint) and
     Assigned(ASuperBlueprint.AsReference) and
     (ASuperBlueprint.AsReference is TSouffleBlueprint)) then
    Exit;

  Bp := TSouffleBlueprint(ASuperBlueprint.AsReference);
  while Assigned(Bp) do
  begin
    if Bp.Methods.Get(AMethodName, Method) then
      Exit(Method);
    if Bp.HasStaticFields and Bp.StaticFields.Get(AMethodName, Method) then
      Exit(Method);
    Bp := Bp.SuperBlueprint;
  end;
end;

function TGocciaRuntimeOperations.AddPendingClassDef(
  const AClassDef: TGocciaClassDefinition;
  const ALine, AColumn: Integer): Integer;
begin
  Result := FPendingClassCount;
  if FPendingClassCount >= Length(FPendingClasses) then
    SetLength(FPendingClasses, FPendingClassCount * 2 + 4);
  FPendingClasses[FPendingClassCount].ClassDefinition := AClassDef;
  FPendingClasses[FPendingClassCount].Line := ALine;
  FPendingClasses[FPendingClassCount].Column := AColumn;
  Inc(FPendingClassCount);
end;

function BlueprintToClassValue(const ABlueprint: TSouffleBlueprint;
  const ARuntime: TGocciaRuntimeOperations): TGocciaClassValue;
var
  SuperClass: TGocciaClassValue;
  I: Integer;
  Key: string;
  MethodVal: TSouffleValue;
  Bridge: TGocciaSouffleMethodBridge;
begin
  SuperClass := nil;
  if Assigned(ABlueprint.SuperBlueprint) then
    SuperClass := BlueprintToClassValue(ABlueprint.SuperBlueprint, ARuntime);
  Result := TGocciaClassValue.Create(ABlueprint.Name, SuperClass);
  for I := 0 to ABlueprint.Methods.Count - 1 do
  begin
    Key := ABlueprint.Methods.GetOrderedKey(I);
    MethodVal := ABlueprint.Methods.GetOrderedValue(I);
    if SouffleIsReference(MethodVal) and
       (MethodVal.AsReference is TSouffleClosure) then
    begin
      Bridge := TGocciaSouffleMethodBridge.Create(
        TSouffleClosure(MethodVal.AsReference), ARuntime);
      Result.AddMethod(Key, Bridge);
    end;
  end;
end;

function CreateBridgedContext(
  const ARuntime: TGocciaRuntimeOperations): TGocciaEvaluationContext;
var
  BridgeScope: TGocciaScope;
  GocciaVal: TGocciaValue;
  Pair: TPair<string, TSouffleValue>;
begin
  Result := TGocciaEngine(ARuntime.Engine).Interpreter.CreateEvaluationContext;
  if ARuntime.FGlobals.Count = 0 then
    Exit;

  BridgeScope := Result.Scope.CreateChild(skBlock);
  for Pair in ARuntime.FGlobals do
    if not BridgeScope.Contains(Pair.Key) then
    begin
      GocciaVal := ARuntime.UnwrapToGocciaValue(Pair.Value);
      BridgeScope.DefineLexicalBinding(Pair.Key, GocciaVal, dtLet);
    end;
  Result.Scope := BridgeScope;
end;

procedure SyncArraysBack(const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope);
var
  Pair: TPair<string, TSouffleValue>;
  GocciaVal: TGocciaValue;
  SArr: TSouffleArray;
  GArr: TGocciaArrayValue;
  I: Integer;
begin
  for Pair in ARuntime.FGlobals do
  begin
    if not SouffleIsReference(Pair.Value) then
      Continue;
    if not (Pair.Value.AsReference is TSouffleArray) then
      Continue;
    if not AScope.Contains(Pair.Key) then
      Continue;
    GocciaVal := AScope.GetValue(Pair.Key);
    if not (GocciaVal is TGocciaArrayValue) then
      Continue;
    SArr := TSouffleArray(Pair.Value.AsReference);
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
  Pair: TPair<string, TSouffleValue>;
  GocciaVal: TGocciaValue;
begin
  for Pair in ARuntime.FGlobals do
  begin
    if not SouffleIsReference(Pair.Value) then
      Continue;
    if not (Pair.Value.AsReference is TSouffleArray) then
      Continue;
    if not AScope.Contains(Pair.Key) then
      Continue;
    GocciaVal := AScope.GetValue(Pair.Key);
    if not (GocciaVal is TGocciaArrayValue) then
      Continue;
    ARuntime.FArrayBridgeCache.AddOrSetValue(
      Pair.Value.AsReference, GocciaVal);
  end;
end;

procedure SyncCachedGocciaToSouffle(
  const ARuntime: TGocciaRuntimeOperations);
var
  Pair: TPair<TObject, TObject>;
  SArr: TSouffleArray;
  GArr: TGocciaArrayValue;
  I: Integer;
begin
  for Pair in ARuntime.FArrayBridgeCache do
  begin
    SArr := TSouffleArray(Pair.Key);
    GArr := TGocciaArrayValue(Pair.Value);
    SArr.Clear;
    for I := 0 to GArr.Elements.Count - 1 do
      SArr.Push(ARuntime.ToSouffleValue(GArr.Elements[I]));
  end;
end;

procedure SyncScopeToGlobals(const ARuntime: TGocciaRuntimeOperations;
  const AScope: TGocciaScope);
var
  GlobalPair: TPair<string, TSouffleValue>;
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

procedure SyncDefinitionScopesBack(
  const ARuntime: TGocciaRuntimeOperations);
var
  ScopePair: TPair<TObject, TObject>;
begin
  for ScopePair in ARuntime.FClassDefinitionScopes do
    SyncScopeToGlobals(ARuntime, TGocciaScope(ScopePair.Value));
end;

function TGocciaRuntimeOperations.EvaluateClassByIndex(
  const AIndex: Integer): TSouffleValue;
var
  Entry: TGocciaPendingClassEntry;
  Context: TGocciaEvaluationContext;
  EvalScope: TGocciaScope;
  ClassValue: TGocciaClassValue;
  GocciaValue: TGocciaValue;
  Pair: TPair<string, TSouffleValue>;
begin
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  if (AIndex < 0) or (AIndex >= FPendingClassCount) then
    Exit;

  Entry := FPendingClasses[AIndex];
  Context := CreateBridgedContext(Self);
  EvalScope := Context.Scope;

  try
    ClassValue := EvaluateClassDefinition(
      Entry.ClassDefinition, Context, Entry.Line, Entry.Column);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
  end;

  SyncArraysBack(Self, EvalScope);
  FArrayBridgeCache.Clear;

  if Assigned(ClassValue) then
  begin
    FClassDefinitionScopes.AddOrSetValue(ClassValue, EvalScope);
    TGocciaGarbageCollector.Instance.AddTempRoot(ClassValue);
    try
      Result := ToSouffleValue(ClassValue);
      FGlobals.AddOrSetValue(Entry.ClassDefinition.Name, Result);
      if EvalScope.ContainsOwnLexicalBinding(Entry.ClassDefinition.Name) then
        EvalScope.AssignLexicalBinding(Entry.ClassDefinition.Name, ClassValue)
      else
        EvalScope.DefineLexicalBinding(
          Entry.ClassDefinition.Name, ClassValue, dtLet);
    finally
      TGocciaGarbageCollector.Instance.RemoveTempRoot(ClassValue);
    end;
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

    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.AddTempRoot(EnumObj);

    Result := WrapGocciaValue(EnumObj);
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
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
    9: // GOCCIA_EXT_EVAL_CLASS
      ADest := EvaluateClassByIndex(AOperandIndex);
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
  end;
end;

procedure TGocciaRuntimeOperations.MarkExternalRoots;
var
  GlobalVal: TSouffleValue;
  ClosureKey: TSouffleClosure;
  BridgeKey: TObject;
begin
  for GlobalVal in FGlobals.Values do
    if SouffleIsReference(GlobalVal) and Assigned(GlobalVal.AsReference)
      and not GlobalVal.AsReference.GCMarked then
      GlobalVal.AsReference.MarkReferences;

  for GlobalVal in FExports.Values do
    if SouffleIsReference(GlobalVal) and Assigned(GlobalVal.AsReference)
      and not GlobalVal.AsReference.GCMarked then
      GlobalVal.AsReference.MarkReferences;

  for ClosureKey in FClosureBridgeCache.Keys do
    if not ClosureKey.GCMarked then
      ClosureKey.MarkReferences;

  for BridgeKey in FArrayBridgeCache.Keys do
    if (BridgeKey is TSouffleHeapObject) and not TSouffleHeapObject(BridgeKey).GCMarked then
      TSouffleHeapObject(BridgeKey).MarkReferences;

  for BridgeKey in FRecordBridgeCache.Keys do
    if (BridgeKey is TSouffleHeapObject) and not TSouffleHeapObject(BridgeKey).GCMarked then
      TSouffleHeapObject(BridgeKey).MarkReferences;
end;

procedure TGocciaRuntimeOperations.MarkWrappedGocciaValues;
var
  I: Integer;
  Wrapped: TGocciaWrappedValue;
  BridgeVal: TObject;
begin
  for I := 0 to FWrappedValues.Count - 1 do
  begin
    Wrapped := TGocciaWrappedValue(FWrappedValues[I]);
    if Assigned(Wrapped.Value) and not Wrapped.Value.GCMarked then
      Wrapped.Value.MarkReferences;
  end;

  for BridgeVal in FClosureBridgeCache.Values do
    if (BridgeVal is TGocciaValue) and not TGocciaValue(BridgeVal).GCMarked then
      TGocciaValue(BridgeVal).MarkReferences;

  for BridgeVal in FArrayBridgeCache.Values do
    if (BridgeVal is TGocciaValue) and not TGocciaValue(BridgeVal).GCMarked then
      TGocciaValue(BridgeVal).MarkReferences;

  for BridgeVal in FRecordBridgeCache.Values do
    if (BridgeVal is TGocciaValue) and not TGocciaValue(BridgeVal).GCMarked then
      TGocciaValue(BridgeVal).MarkReferences;
end;

end.
