unit Goccia.Runtime.Operations;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Souffle.Bytecode.Chunk,
  Souffle.Heap,
  Souffle.Value,
  Souffle.VM,
  Souffle.VM.Closure,
  Souffle.VM.RuntimeOperations,

  Goccia.Values.Error,
  Goccia.Values.Primitives;

const
  GOCCIA_HEAP_WRAPPED = 128;

type
  TGocciaRuntimeOperations = class;

  TGocciaWrappedValue = class(TSouffleHeapObject)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    procedure MarkReferences; override;
    function DebugString: string; override;
    property Value: TGocciaValue read FValue;
  end;

  TGocciaSouffleProxy = class(TGocciaValue)
  private
    FTarget: TSouffleHeapObject;
    FRuntime: TGocciaRuntimeOperations;
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

    property Target: TSouffleHeapObject read FTarget;
  end;

  TGocciaRuntimeOperations = class(TSouffleRuntimeOperations)
  private
    FGlobals: TDictionary<string, TSouffleValue>;
    FExports: TDictionary<string, TSouffleValue>;
    FClosureBridgeCache: TDictionary<TSouffleClosure, TObject>;
    FArrayBridgeCache: TDictionary<TObject, TObject>;
    FFormalParameterCounts: TDictionary<TSouffleFunctionTemplate, Integer>;
    FVM: TSouffleVM;
    FEngine: TObject;

    function WrapGocciaValue(const AValue: TGocciaValue): TSouffleValue;
    function CoerceKeyToString(const AKey: TSouffleValue): string;
    function CoerceToNumber(const A: TSouffleValue): Double;
    function CoerceToString(const A: TSouffleValue): string;
    procedure RethrowAsVM(const E: TGocciaThrowValue);
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

    function GetIterator(const AIterable: TSouffleValue): TSouffleValue; override;
    function IteratorNext(const AIterator: TSouffleValue;
      out ADone: Boolean): TSouffleValue; override;
    procedure SpreadInto(const ATarget, ASource: TSouffleValue); override;
    procedure SpreadObjectInto(const ATarget, ASource: TSouffleValue); override;

    function ArrayRest(const ASource: TSouffleValue;
      const AStartIndex: Integer): TSouffleValue; override;
    function ObjectRest(const ASource,
      AExclusionKeys: TSouffleValue): TSouffleValue; override;
    procedure RequireObjectCoercible(const AValue: TSouffleValue); override;
    procedure RequireIterable(const AValue: TSouffleValue); override;
    function CoerceValueToString(const A: TSouffleValue): TSouffleValue; override;

    function ImportModule(const APath: string): TSouffleValue; override;
    procedure ExportBinding(const AValue: TSouffleValue;
      const AName: string); override;

    function AwaitValue(const AValue: TSouffleValue): TSouffleValue; override;

    function GetGlobal(const AName: string): TSouffleValue; override;
    procedure SetGlobal(const AName: string;
      const AValue: TSouffleValue); override;
    function HasGlobal(const AName: string): Boolean; override;

    procedure DefineGetter(const AObject: TSouffleValue; const AKey: string;
      const AGetter: TSouffleValue); override;
    procedure DefineSetter(const AObject: TSouffleValue; const AKey: string;
      const ASetter: TSouffleValue); override;

    function UnwrapToGocciaValue(const AValue: TSouffleValue): TGocciaValue;
    function ToSouffleValue(const AValue: TGocciaValue): TSouffleValue;

    function ResolveProxyGet(const ATarget: TSouffleHeapObject;
      const AName: string): TGocciaValue;
    procedure ResolveProxySet(const ATarget: TSouffleHeapObject;
      const AName: string; const AValue: TGocciaValue);

    procedure RegisterDelegates;
    procedure RegisterGlobal(const AName: string; const AValue: TSouffleValue);
    procedure RegisterFormalParameterCount(
      const ATemplate: TSouffleFunctionTemplate; const ACount: Integer);
    function GetFormalParameterCount(
      const ATemplate: TSouffleFunctionTemplate): Integer;
    property ModuleExports: TDictionary<string, TSouffleValue> read FExports;
    property VM: TSouffleVM read FVM write FVM;
    property Engine: TObject read FEngine write FEngine;
  end;

implementation

uses
  Math,
  SysUtils,

  Souffle.Compound,
  Souffle.GarbageCollector,
  Souffle.VM.Exception,
  Souffle.VM.NativeFunction,

  Goccia.Arguments.Collection,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Evaluator,
  Goccia.Evaluator.TypeOperations,
  Goccia.Interpreter,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

const
  GOCCIA_NIL_UNDEFINED = 0;
  GOCCIA_NIL_NULL      = 1;
  GOCCIA_NIL_HOLE      = 2;

type
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

  try
    SouffleResult := FRuntime.VM.ExecuteFunction(FClosure, Args);
  except
    on E: ESouffleThrow do
      raise TGocciaThrowValue.Create(
        FRuntime.UnwrapToGocciaValue(E.ThrownValue));
  end;
  Result := FRuntime.UnwrapToGocciaValue(SouffleResult);
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
end;

procedure TGocciaSouffleProxy.SetProperty(const AName: string;
  const AValue: TGocciaValue);
begin
  FRuntime.ResolveProxySet(FTarget, AName, AValue);
end;

function TGocciaSouffleProxy.ToStringLiteral: TGocciaStringLiteralValue;
var
  I: Integer;
  Arr: TSouffleArray;
  Rec: TSouffleRecord;
  S: string;
begin
  if FTarget is TSouffleArray then
  begin
    Arr := TSouffleArray(FTarget);
    S := '';
    for I := 0 to Arr.Count - 1 do
    begin
      if I > 0 then
        S := S + ',';
      S := S + SouffleValueToString(Arr.Get(I));
    end;
    Result := TGocciaStringLiteralValue.Create(S);
  end
  else if FTarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(FTarget);
    S := '{';
    for I := 0 to Rec.Count - 1 do
    begin
      if I > 0 then
        S := S + ', ';
      S := S + Rec.GetOrderedKey(I) + ': '
        + SouffleValueToString(Rec.GetOrderedValue(I));
    end;
    S := S + '}';
    Result := TGocciaStringLiteralValue.Create(S);
  end
  else
    Result := TGocciaStringLiteralValue.Create('[object]');
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

{ TGocciaRuntimeOperations }

constructor TGocciaRuntimeOperations.Create;
begin
  inherited Create;
  FGlobals := TDictionary<string, TSouffleValue>.Create;
  FExports := TDictionary<string, TSouffleValue>.Create;
  FClosureBridgeCache := TDictionary<TSouffleClosure, TObject>.Create;
  FArrayBridgeCache := TDictionary<TObject, TObject>.Create;
  FFormalParameterCounts := TDictionary<TSouffleFunctionTemplate, Integer>.Create;
  FVM := nil;
end;

destructor TGocciaRuntimeOperations.Destroy;
begin
  FGlobals.Free;
  FExports.Free;
  FClosureBridgeCache.Free;
  FArrayBridgeCache.Free;
  FFormalParameterCounts.Free;
  inherited;
end;

function TGocciaRuntimeOperations.WrapGocciaValue(
  const AValue: TGocciaValue): TSouffleValue;
begin
  Result := SouffleReference(TGocciaWrappedValue.Create(AValue));
end;

procedure TGocciaRuntimeOperations.RethrowAsVM(const E: TGocciaThrowValue);
begin
  raise ESouffleThrow.Create(WrapGocciaValue(E.Value));
end;

function TGocciaRuntimeOperations.CoerceToNumber(
  const A: TSouffleValue): Double;
var
  GocciaVal: TGocciaValue;
  NumVal: TGocciaNumberLiteralValue;
  S: string;
  Code: Integer;
  HexVal: Int64;
begin
  case A.Kind of
    svkInteger:
      Result := A.AsInteger * 1.0;
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
    begin
      S := Trim(SouffleGetString(A));
      if S = '' then
        Result := 0.0
      else if S = 'Infinity' then
        Result := Infinity
      else if S = '-Infinity' then
        Result := NegInfinity
      else
      begin
        Val(S, Result, Code);
        if Code <> 0 then
        begin
          if (Length(S) > 2) and (S[1] = '0') and
             ((S[2] = 'x') or (S[2] = 'X')) then
          begin
            Val(S, HexVal, Code);
            if Code = 0 then
              Result := HexVal * 1.0
            else
              Result := NaN;
          end
          else
            Result := NaN;
        end;
      end;
    end;
    svkReference:
      if A.AsReference is TSouffleHeapString then
      begin
        S := Trim(TSouffleHeapString(A.AsReference).Value);
        if S = '' then
          Result := 0.0
        else
        begin
          Val(S, Result, Code);
          if Code <> 0 then
            Result := NaN;
        end;
      end
      else if A.AsReference is TSouffleArray then
      begin
        S := Trim(CoerceToString(A));
        if S = '' then
          Result := 0.0
        else
        begin
          Val(S, Result, Code);
          if Code <> 0 then
            Result := NaN;
        end;
      end
      else if A.AsReference is TGocciaWrappedValue then
      begin
        GocciaVal := TGocciaWrappedValue(A.AsReference).Value;
        NumVal := GocciaVal.ToNumberLiteral;
        if NumVal.IsNaN then
          Result := NaN
        else
          Result := NumVal.Value;
      end
      else
        Result := NaN;
  else
    Result := NaN;
  end;
end;

function JoinArrayElements(const AArr: TSouffleArray;
  const ASep: string; const ARuntime: TGocciaRuntimeOperations): string; forward;
function JoinElementToString(const AElem: TSouffleValue;
  const ARuntime: TGocciaRuntimeOperations): string; forward;

function SouffleArrayToString(const ARuntime: TGocciaRuntimeOperations;
  const AArr: TSouffleArray): string;
begin
  Result := JoinArrayElements(AArr, ',', ARuntime);
end;

function TGocciaRuntimeOperations.CoerceToString(
  const A: TSouffleValue): string;
var
  GocciaVal: TGocciaValue;
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
      if IsNaN(A.AsFloat) then
        Result := 'NaN'
      else if IsInfinite(A.AsFloat) then
      begin
        if A.AsFloat > 0 then
          Result := 'Infinity'
        else
          Result := '-Infinity';
      end
      else
        Result := FloatToStr(A.AsFloat);
    svkString:
      Result := SouffleGetString(A);
    svkReference:
      if A.AsReference is TSouffleHeapString then
        Result := TSouffleHeapString(A.AsReference).Value
      else if A.AsReference is TGocciaWrappedValue then
      begin
        GocciaVal := TGocciaWrappedValue(A.AsReference).Value;
        Result := GocciaVal.ToStringLiteral.Value;
      end
      else if A.AsReference is TSouffleArray then
        Result := SouffleArrayToString(Self, TSouffleArray(A.AsReference))
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
      Result := TGocciaNumberLiteralValue.Create(AValue.AsInteger * 1.0);
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
          ConvertSouffleArrayInto(Self,
            TSouffleArray(AValue.AsReference),
            TGocciaArrayValue(CachedBridge));
        end;
        Result := TGocciaValue(CachedBridge);
      end
      else if AValue.AsReference is TSouffleRecord then
        Result := TGocciaSouffleProxy.Create(AValue.AsReference, Self)
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
  if AValue is TGocciaSouffleProxy then
    Exit(SouffleReference(TGocciaSouffleProxy(AValue).Target));

  if AValue is TGocciaSouffleClosureBridge then
    Exit(SouffleReference(TGocciaSouffleClosureBridge(AValue).Closure));

  if AValue is TGocciaNullLiteralValue then
    Result := SouffleNil
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
    else if (Frac(NumVal.Value) = 0.0)
       and (NumVal.Value >= Low(Int64) * 1.0)
       and (NumVal.Value <= High(Int64) * 1.0) then
      Result := SouffleInteger(Trunc(NumVal.Value))
    else
      Result := SouffleFloat(NumVal.Value);
  end
  else if AValue is TGocciaStringLiteralValue then
    Result := SouffleString(TGocciaStringLiteralValue(AValue).Value)
  else
    Result := WrapGocciaValue(AValue);
end;

{ Arithmetic }

function CoerceToPrimitiveSouffle(const ARuntime: TGocciaRuntimeOperations;
  const A: TSouffleValue): TSouffleValue;
var
  GocciaVal, Prim: TGocciaValue;
  Rec: TSouffleRecord;
  Bp: TSouffleBlueprint;
  MethodVal, MethodResult: TSouffleValue;
  VMArgs: array of TSouffleValue;
begin
  if SouffleIsStringValue(A) then
    Exit(A);
  if not SouffleIsReference(A) or not Assigned(A.AsReference) then
    Exit(A);
  if A.AsReference is TSouffleArray then
    Exit(SouffleString(
      SouffleArrayToString(ARuntime, TSouffleArray(A.AsReference))));
  if A.AsReference is TSouffleRecord then
  begin
    Rec := TSouffleRecord(A.AsReference);
    if Assigned(Rec.Blueprint) then
    begin
      Bp := Rec.Blueprint;
      while Assigned(Bp) do
      begin
        if Bp.Methods.Get(PROP_VALUE_OF, MethodVal) then
        begin
          SetLength(VMArgs, 1);
          VMArgs[0] := A;
          MethodResult := ARuntime.VM.ExecuteFunction(
            TSouffleClosure(MethodVal.AsReference), VMArgs);
          if not SouffleIsReference(MethodResult) or SouffleIsStringValue(MethodResult) then
            Exit(MethodResult);
        end;
        if Bp.Methods.Get(PROP_TO_STRING, MethodVal) then
        begin
          SetLength(VMArgs, 1);
          VMArgs[0] := A;
          MethodResult := ARuntime.VM.ExecuteFunction(
            TSouffleClosure(MethodVal.AsReference), VMArgs);
          if not SouffleIsReference(MethodResult) or SouffleIsStringValue(MethodResult) then
            Exit(MethodResult);
        end;
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
  Result := A;
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
  if SouffleIsInteger(A) and SouffleIsInteger(B) then
    Result := SouffleInteger(A.AsInteger - B.AsInteger)
  else
    Result := SouffleFloat(CoerceToNumber(A) - CoerceToNumber(B));
end;

function TGocciaRuntimeOperations.Multiply(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsInteger(A) and SouffleIsInteger(B) then
    Result := SouffleInteger(A.AsInteger * B.AsInteger)
  else
    Result := SouffleFloat(CoerceToNumber(A) * CoerceToNumber(B));
end;

// ES2026 §13.7 Multiplicative Operators — IEEE 754 handles all edge cases
// (NaN propagation, division by ±0, Infinity arithmetic, negative zero sign)
function TGocciaRuntimeOperations.Divide(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleFloat(CoerceToNumber(A) / CoerceToNumber(B));
end;

function TGocciaRuntimeOperations.Modulo(const A, B: TSouffleValue): TSouffleValue;
var
  Dividend, Divisor: Double;
begin
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
end;

function TGocciaRuntimeOperations.Power(const A, B: TSouffleValue): TSouffleValue;
var
  Base, Exp: Double;
begin
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
end;

function TGocciaRuntimeOperations.Negate(const A: TSouffleValue): TSouffleValue;
begin
  if SouffleIsInteger(A) then
    Result := SouffleInteger(-A.AsInteger)
  else
    Result := SouffleFloat(-CoerceToNumber(A));
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
  Result := SouffleInteger(ToInt32(CoerceToNumber(A)) and
    ToInt32(CoerceToNumber(B)));
end;

function TGocciaRuntimeOperations.BitwiseOr(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(ToInt32(CoerceToNumber(A)) or
    ToInt32(CoerceToNumber(B)));
end;

function TGocciaRuntimeOperations.BitwiseXor(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(ToInt32(CoerceToNumber(A)) xor
    ToInt32(CoerceToNumber(B)));
end;

function TGocciaRuntimeOperations.ShiftLeft(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(ToInt32(CoerceToNumber(A)) shl
    (ToInt32(CoerceToNumber(B)) and $1F));
end;

function TGocciaRuntimeOperations.ShiftRight(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(SarLongint(ToInt32(CoerceToNumber(A)),
    ToInt32(CoerceToNumber(B)) and $1F));
end;

function TGocciaRuntimeOperations.UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue;
var
  LVal: Cardinal;
  Shift: Integer;
  FloatResult: Double;
begin
  LVal := Cardinal(Trunc(CoerceToNumber(A)));
  Shift := Trunc(CoerceToNumber(B)) and 31;
  LVal := LVal shr Shift;
  FloatResult := LVal;
  Result := SouffleFloat(FloatResult);
end;

function TGocciaRuntimeOperations.BitwiseNot(const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(not ToInt32(CoerceToNumber(A)));
end;

{ Comparison }

function TGocciaRuntimeOperations.Equal(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) = SouffleAsNumber(B))
  else
    Result := SouffleBoolean(SouffleValuesEqual(A, B));
end;

function TGocciaRuntimeOperations.NotEqual(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) <> SouffleAsNumber(B))
  else
    Result := SouffleBoolean(not SouffleValuesEqual(A, B));
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
  Cmp := AbstractRelationalComparison(Self, A, B);
  if IsNaN(Cmp) then
    Result := SouffleBoolean(False)
  else
    Result := SouffleBoolean(Cmp < 0);
end;

function TGocciaRuntimeOperations.GreaterThan(const A, B: TSouffleValue): TSouffleValue;
var
  Cmp: Double;
begin
  Cmp := AbstractRelationalComparison(Self, A, B);
  if IsNaN(Cmp) then
    Result := SouffleBoolean(False)
  else
    Result := SouffleBoolean(Cmp > 0);
end;

function TGocciaRuntimeOperations.LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue;
var
  Cmp: Double;
begin
  Cmp := AbstractRelationalComparison(Self, A, B);
  if IsNaN(Cmp) then
    Result := SouffleBoolean(False)
  else
    Result := SouffleBoolean(Cmp <= 0);
end;

function TGocciaRuntimeOperations.GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue;
var
  Cmp: Double;
begin
  Cmp := AbstractRelationalComparison(Self, A, B);
  if IsNaN(Cmp) then
    Result := SouffleBoolean(False)
  else
    Result := SouffleBoolean(Cmp >= 0);
end;

{ Logical / Type }

function TGocciaRuntimeOperations.LogicalNot(const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleBoolean(not SouffleIsTrue(A));
end;

function TGocciaRuntimeOperations.TypeOf(const A: TSouffleValue): TSouffleValue;
var
  GocciaVal: TGocciaValue;
  TypeStr: string;
  Rec: TSouffleRecord;
begin
  if SouffleIsStringValue(A) then
    Exit(SouffleString('string'));
  if SouffleIsReference(A) and Assigned(A.AsReference) then
  begin
    if A.AsReference is TSouffleArray then
      Exit(SouffleString('object'));
    if A.AsReference is TSouffleRecord then
      Exit(SouffleString('object'));
    if A.AsReference is TSouffleBlueprint then
      Exit(SouffleString('function'));
  end;
  GocciaVal := UnwrapToGocciaValue(A);
  TypeStr := GocciaVal.TypeOf;
  Result := SouffleString(TypeStr);
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
    Exit(SouffleBoolean(TSouffleRecord(AObject.AsReference).Has(KeyStr)));

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
    if KeyStr = 'length' then
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
begin
  try
    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
    begin
      if AObject.AsReference is TSouffleRecord then
      begin
        Rec := TSouffleRecord(AObject.AsReference);
        if Rec.Get(AKey, Result) then
          Exit;
        if Assigned(Rec.Blueprint) then
        begin
          Bp := Rec.Blueprint;
          while Assigned(Bp) do
          begin
            if Bp.Methods.Get(AKey, Result) then
              Exit;
            Bp := Bp.SuperBlueprint;
          end;
        end;
        if Assigned(Rec.Blueprint) then
          Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
      end
      else if AObject.AsReference is TSouffleBlueprint then
      begin
        Bp := TSouffleBlueprint(AObject.AsReference);
        if AKey = PROP_PROTOTYPE then
          Exit(SouffleReference(Bp.Prototype))
        else if AKey = PROP_NAME then
          Exit(SouffleString(Bp.Name))
        else if Bp.Methods.Get(AKey, Result) then
          Exit;
        Exit(SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));
      end
      else if AObject.AsReference is TSouffleArray then
      begin
        Arr := TSouffleArray(AObject.AsReference);
        if AKey = 'length' then
          Exit(SouffleInteger(Arr.Count));
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
begin
  try
    if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
    begin
      if AObject.AsReference is TSouffleRecord then
      begin
        TSouffleRecord(AObject.AsReference).Put(AKey, AValue);
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
  if AKey.Kind = svkNil then
    Exit('null');
  if SouffleIsReference(AKey) and Assigned(AKey.AsReference) then
  begin
    if AKey.AsReference is TGocciaWrappedValue then
      Exit(TGocciaWrappedValue(AKey.AsReference).Value.ToStringLiteral.Value);
  end;
  Result := SouffleValueToString(AKey);
end;

function TGocciaRuntimeOperations.GetIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
var
  GocciaObj: TGocciaValue;
  SymKey: TGocciaSymbolValue;
  PropVal: TGocciaValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
     (AObject.AsReference is TSouffleArray) and (AKey.Kind = svkInteger) then
    Result := TSouffleArray(AObject.AsReference).Get(Integer(AKey.AsInteger))
  else if SouffleIsReference(AKey) and Assigned(AKey.AsReference) and
     (AKey.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AKey.AsReference).Value is TGocciaSymbolValue) then
  begin
    SymKey := TGocciaSymbolValue(TGocciaWrappedValue(AKey.AsReference).Value);
    GocciaObj := UnwrapToGocciaValue(AObject);
    if GocciaObj is TGocciaObjectValue then
    begin
      PropVal := TGocciaObjectValue(GocciaObj).GetSymbolProperty(SymKey);
      if Assigned(PropVal) then
        Exit(ToSouffleValue(PropVal));
    end;
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
  end
  else
    Result := GetProperty(AObject, CoerceKeyToString(AKey));
end;

procedure TGocciaRuntimeOperations.SetIndex(const AObject: TSouffleValue;
  const AKey, AValue: TSouffleValue);
var
  GocciaObj: TGocciaValue;
  SymKey: TGocciaSymbolValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
     (AObject.AsReference is TSouffleArray) and (AKey.Kind = svkInteger) then
    TSouffleArray(AObject.AsReference).Put(Integer(AKey.AsInteger), AValue)
  else if SouffleIsReference(AKey) and Assigned(AKey.AsReference) and
     (AKey.AsReference is TGocciaWrappedValue) and
     (TGocciaWrappedValue(AKey.AsReference).Value is TGocciaSymbolValue) then
  begin
    SymKey := TGocciaSymbolValue(TGocciaWrappedValue(AKey.AsReference).Value);
    GocciaObj := UnwrapToGocciaValue(AObject);
    if GocciaObj is TGocciaObjectValue then
      TGocciaObjectValue(GocciaObj).AssignSymbolProperty(SymKey, UnwrapToGocciaValue(AValue));
  end
  else
    SetProperty(AObject, CoerceKeyToString(AKey), AValue);
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
      TSouffleArray(AObject.AsReference).Put(Integer(Idx), SouffleNil);
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
  GocciaCallee := UnwrapToGocciaValue(ACallee);
  if not GocciaCallee.IsCallable then
  begin
    Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
    Exit;
  end;

  Args := TGocciaArgumentsCollection.Create;
  try
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
    except
      on E: TGocciaThrowValue do
        RethrowAsVM(E);
    end;
  finally
    Args.Free;
  end;
end;

function TGocciaRuntimeOperations.Construct(const AConstructor: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  GocciaConstructor, GocciaResult: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  Context: TGocciaEvaluationContext;
  I: Integer;
  Bp, WalkBp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  CtorMethod: TSouffleValue;
  VMArgs: array of TSouffleValue;
begin
  try
    if SouffleIsReference(AConstructor) and
       Assigned(AConstructor.AsReference) and
       (AConstructor.AsReference is TSouffleBlueprint) then
    begin
      Bp := TSouffleBlueprint(AConstructor.AsReference);
      Rec := TSouffleRecord.CreateFromBlueprint(Bp);
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

        Context := TGocciaEngine(FEngine).Interpreter.CreateEvaluationContext;
        GocciaResult := InstantiateClass(
          TGocciaClassValue(GocciaConstructor), Args, Context);

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

        GocciaResult := TGocciaNativeFunctionValue(GocciaConstructor).Call(
          Args, TGocciaUndefinedLiteralValue.UndefinedValue);

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
end;

{ Iteration }

function TGocciaRuntimeOperations.GetIterator(
  const AIterable: TSouffleValue): TSouffleValue;
begin
  Result := AIterable;
end;

function TGocciaRuntimeOperations.IteratorNext(const AIterator: TSouffleValue;
  out ADone: Boolean): TSouffleValue;
begin
  ADone := True;
  Result := SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED);
end;

procedure TGocciaRuntimeOperations.SpreadInto(
  const ATarget, ASource: TSouffleValue);
var
  SrcArr: TSouffleArray;
  TgtArr: TSouffleArray;
  PairArr: TSouffleArray;
  I: Integer;
  GocciaVal: TGocciaValue;
  ArrVal: TGocciaArrayValue;
  MapVal: TGocciaMapValue;
  SetVal: TGocciaSetValue;
  StrVal: string;
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
    end
    else if not SouffleIsReference(ASource) then
    begin
      ThrowTypeError('Cannot spread a non-iterable value');
      Exit;
    end
    else if ASource.AsReference is TSouffleArray then
    begin
      SrcArr := TSouffleArray(ASource.AsReference);
      for I := 0 to SrcArr.Count - 1 do
        TgtArr.Push(SrcArr.Get(I));
    end
    else if ASource.AsReference is TGocciaWrappedValue then
    begin
      GocciaVal := TGocciaWrappedValue(ASource.AsReference).Value;
      if GocciaVal is TGocciaArrayValue then
      begin
        ArrVal := TGocciaArrayValue(GocciaVal);
        for I := 0 to ArrVal.Elements.Count - 1 do
          TgtArr.Push(ToSouffleValue(ArrVal.Elements[I]));
      end
      else if GocciaVal is TGocciaStringLiteralValue then
      begin
        StrVal := TGocciaStringLiteralValue(GocciaVal).Value;
        for I := 1 to Length(StrVal) do
          TgtArr.Push(SouffleString(StrVal[I]));
      end
      else if GocciaVal is TGocciaMapValue then
      begin
        MapVal := TGocciaMapValue(GocciaVal);
        for I := 0 to MapVal.Entries.Count - 1 do
        begin
          PairArr := TSouffleArray.Create(2);
          PairArr.Push(ToSouffleValue(MapVal.Entries[I].Key));
          PairArr.Push(ToSouffleValue(MapVal.Entries[I].Value));
          TgtArr.Push(SouffleReference(PairArr));
        end;
      end
      else if GocciaVal is TGocciaSetValue then
      begin
        SetVal := TGocciaSetValue(GocciaVal);
        for I := 0 to SetVal.Items.Count - 1 do
          TgtArr.Push(ToSouffleValue(SetVal.Items[I]));
      end
      else
        ThrowTypeError('Cannot spread a non-iterable value');
    end
    else
      ThrowTypeError('Cannot spread a non-iterable value');
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
        TSouffleRecord(ATarget.AsReference).Put(SrcRec.GetOrderedKey(I), SrcRec.GetOrderedValue(I));
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
        TgtObj.SetProperty(SrcRec.GetOrderedKey(I), UnwrapToGocciaValue(SrcRec.GetOrderedValue(I)));
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

function TGocciaRuntimeOperations.ArrayRest(const ASource: TSouffleValue;
  const AStartIndex: Integer): TSouffleValue;
var
  GocciaVal: TGocciaValue;
  Arr: TGocciaArrayValue;
  SrcArr: TSouffleArray;
  RestArr: TSouffleArray;
  StrVal: string;
  I: Integer;
begin
  if SouffleIsStringValue(ASource) then
  begin
    StrVal := SouffleGetString(ASource);
    RestArr := TSouffleArray.Create(Length(StrVal) - AStartIndex);
    for I := AStartIndex + 1 to Length(StrVal) do
      RestArr.Push(SouffleString(StrVal[I]));
    Exit(SouffleReference(RestArr));
  end;
  if SouffleIsReference(ASource) and Assigned(ASource.AsReference) then
  begin
    if ASource.AsReference is TSouffleArray then
    begin
      SrcArr := TSouffleArray(ASource.AsReference);
      RestArr := TSouffleArray.Create(SrcArr.Count - AStartIndex);
      for I := AStartIndex to SrcArr.Count - 1 do
        RestArr.Push(SrcArr.Get(I));
      Exit(SouffleReference(RestArr));
    end;
    if ASource.AsReference is TGocciaWrappedValue then
    begin
      GocciaVal := TGocciaWrappedValue(ASource.AsReference).Value;
      if GocciaVal is TGocciaArrayValue then
      begin
        Arr := TGocciaArrayValue(GocciaVal);
        RestArr := TSouffleArray.Create(Arr.Elements.Count - AStartIndex);
        for I := AStartIndex to Arr.Elements.Count - 1 do
          RestArr.Push(ToSouffleValue(Arr.Elements[I]));
        Exit(SouffleReference(RestArr));
      end;
      if GocciaVal is TGocciaStringLiteralValue then
      begin
        StrVal := TGocciaStringLiteralValue(GocciaVal).Value;
        RestArr := TSouffleArray.Create(Length(StrVal) - AStartIndex);
        for I := AStartIndex + 1 to Length(StrVal) do
          RestArr.Push(SouffleString(StrVal[I]));
        Exit(SouffleReference(RestArr));
      end;
    end;
  end;
  Result := SouffleNil;
end;

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
  Result := SouffleNil;
end;

procedure TGocciaRuntimeOperations.ExportBinding(const AValue: TSouffleValue;
  const AName: string);
begin
  FExports.AddOrSetValue(AName, AValue);
end;

{ Async }

function TGocciaRuntimeOperations.AwaitValue(const AValue: TSouffleValue): TSouffleValue;
begin
  Result := AValue;
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
  FGlobals.AddOrSetValue(AName, AValue);
end;

function TGocciaRuntimeOperations.HasGlobal(const AName: string): Boolean;
begin
  Result := FGlobals.ContainsKey(AName);
end;

procedure TGocciaRuntimeOperations.DefineGetter(const AObject: TSouffleValue;
  const AKey: string; const AGetter: TSouffleValue);
var
  Obj: TGocciaObjectValue;
  GocciaGetter: TGocciaValue;
  Existing: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
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
  Obj: TGocciaObjectValue;
  GocciaSetter: TGocciaValue;
  Existing: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  Obj := UnwrapToGocciaValue(AObject) as TGocciaObjectValue;
  GocciaSetter := UnwrapToGocciaValue(ASetter);

  ExistingGetter := nil;
  Existing := Obj.GetOwnPropertyDescriptor(AKey);
  if (Existing <> nil) and (Existing is TGocciaPropertyDescriptorAccessor) then
    ExistingGetter := TGocciaPropertyDescriptorAccessor(Existing).Getter;

  Obj.DefineProperty(AKey, TGocciaPropertyDescriptorAccessor.Create(
    ExistingGetter, GocciaSetter, [pfEnumerable, pfConfigurable]));
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
    if AName = 'length' then
      Exit(TGocciaNumberLiteralValue.Create(Arr.Count * 1.0));
    if TryStrToInt(AName, Index) and (Index >= 0) and (Index < Arr.Count) then
      Exit(UnwrapToGocciaValue(Arr.Get(Index)));
    TempArr := ConvertSouffleArrayToGocciaArray(Self, Arr);
    Result := TempArr.GetProperty(AName);
  end
  else if ATarget is TSouffleRecord then
  begin
    Rec := TSouffleRecord(ATarget);
    if Rec.Get(AName, Val) then
      Exit(UnwrapToGocciaValue(Val));
    if Assigned(Rec.Blueprint) then
    begin
      if Rec.Blueprint.Methods.Get(AName, Val) then
        Exit(UnwrapToGocciaValue(Val));
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
    Result := SouffleInteger(Arr.Count);
  end
  else
    Result := SouffleNil;
end;

function NativeArrayPop(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray) then
    Result := TSouffleArray(AReceiver.AsReference).Pop
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
    if (AArgCount > 0) and SouffleIsStringValue(AArgs^) then
      Sep := SouffleGetString(AArgs^);
    Result := SouffleString(
      JoinArrayElements(Arr, Sep, GNativeArrayJoinRuntime));
  end
  else
    Result := SouffleString('');
end;

procedure TGocciaRuntimeOperations.RegisterDelegates;
var
  ArrayMeta: TSouffleRecord;
  GC: TSouffleGarbageCollector;
  PushFn, PopFn, JoinFn: TSouffleNativeFunction;
begin
  if not Assigned(FVM) then Exit;

  GC := TSouffleGarbageCollector.Instance;

  RegisterGlobal('undefined', SouffleNilWithFlags(GOCCIA_NIL_UNDEFINED));

  ArrayMeta := TSouffleRecord.Create(8);
  if Assigned(GC) then
    GC.AllocateObject(ArrayMeta);

  PushFn := TSouffleNativeFunction.Create('push', 1, @NativeArrayPush);
  if Assigned(GC) then
    GC.AllocateObject(PushFn);
  ArrayMeta.Put('push', SouffleReference(PushFn));

  PopFn := TSouffleNativeFunction.Create('pop', 0, @NativeArrayPop);
  if Assigned(GC) then
    GC.AllocateObject(PopFn);
  ArrayMeta.Put('pop', SouffleReference(PopFn));

  GNativeArrayJoinRuntime := Self;
  JoinFn := TSouffleNativeFunction.Create('join', 1, @NativeArrayJoin);
  if Assigned(GC) then
    GC.AllocateObject(JoinFn);
  ArrayMeta.Put('join', SouffleReference(JoinFn));

  FVM.ArrayDelegate := ArrayMeta;
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

procedure TGocciaRuntimeOperations.RequireIterable(
  const AValue: TSouffleValue);
var
  GocciaVal: TGocciaValue;
begin
  try
    if SouffleIsStringValue(AValue) then
      Exit;
    if SouffleIsReference(AValue) and Assigned(AValue.AsReference) then
    begin
      if AValue.AsReference is TSouffleArray then
        Exit;
      if AValue.AsReference is TGocciaWrappedValue then
      begin
        GocciaVal := TGocciaWrappedValue(AValue.AsReference).Value;
        if (GocciaVal is TGocciaArrayValue) or
           (GocciaVal is TGocciaStringLiteralValue) then
          Exit;
      end;
    end;
    ThrowTypeError(CoerceToString(AValue) + ' is not iterable');
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
var
  GocciaVal: TGocciaValue;
  StrVal: TGocciaStringLiteralValue;
  MethodVal, MethodResult: TSouffleValue;
  Bp: TSouffleBlueprint;
  Rec: TSouffleRecord;
  VMArgs: array of TSouffleValue;
begin
  try
    case A.Kind of
      svkNil:
        if A.Flags = GOCCIA_NIL_NULL then
          Result := SouffleString('null')
        else
          Result := SouffleString('undefined');
      svkBoolean:
        if A.AsBoolean then
          Result := SouffleString('true')
        else
          Result := SouffleString('false');
      svkInteger:
        Result := SouffleString(IntToStr(A.AsInteger));
      svkFloat:
        Result := SouffleString(CoerceToString(A));
      svkString:
        Result := A;
    else
      if SouffleIsReference(A) and Assigned(A.AsReference) then
      begin
        if A.AsReference is TSouffleHeapString then
        begin
          Result := A;
          Exit;
        end;
        if A.AsReference is TSouffleRecord then
        begin
          Rec := TSouffleRecord(A.AsReference);
          if Assigned(Rec.Blueprint) then
          begin
            Bp := Rec.Blueprint;
            while Assigned(Bp) do
            begin
              if Bp.Methods.Get(PROP_TO_STRING, MethodVal) then
              begin
                SetLength(VMArgs, 1);
                VMArgs[0] := A;
                MethodResult := VM.ExecuteFunction(
                  TSouffleClosure(MethodVal.AsReference), VMArgs);
                Exit(CoerceValueToString(MethodResult));
              end;
              Bp := Bp.SuperBlueprint;
            end;
          end;
          Result := SouffleString('[object Object]');
          Exit;
        end;

        GocciaVal := UnwrapToGocciaValue(A);
        StrVal := Goccia.Values.ToPrimitive.ToECMAString(GocciaVal);
        Result := SouffleString(StrVal.Value);
      end
      else
        Result := SouffleString(CoerceToString(A));
    end;
  except
    on E: TGocciaThrowValue do
      RethrowAsVM(E);
    on E: ESouffleThrow do
      raise;
  end;
end;

end.
