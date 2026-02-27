unit Goccia.Runtime.Operations;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Souffle.Heap,
  Souffle.Value,
  Souffle.VM,
  Souffle.VM.Closure,
  Souffle.VM.RuntimeOperations,

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
    FVM: TSouffleVM;
    FEngine: TObject;

    function WrapGocciaValue(const AValue: TGocciaValue): TSouffleValue;
    function CoerceKeyToString(const AKey: TSouffleValue): string;
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

    function GetProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; override;
    procedure SetProperty(const AObject: TSouffleValue; const AKey: string;
      const AValue: TSouffleValue); override;
    function GetIndex(const AObject, AKey: TSouffleValue): TSouffleValue; override;
    procedure SetIndex(const AObject: TSouffleValue;
      const AKey, AValue: TSouffleValue); override;
    function DeleteProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; override;

    function Invoke(const ACallee: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer; const AReceiver: TSouffleValue): TSouffleValue; override;
    function Construct(const AConstructor: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; override;

    function GetIterator(const AIterable: TSouffleValue): TSouffleValue; override;
    function IteratorNext(const AIterator: TSouffleValue;
      out ADone: Boolean): TSouffleValue; override;
    procedure SpreadInto(const ATarget, ASource: TSouffleValue); override;

    function ImportModule(const APath: string): TSouffleValue; override;
    procedure ExportBinding(const AValue: TSouffleValue;
      const AName: string); override;

    function AwaitValue(const AValue: TSouffleValue): TSouffleValue; override;

    function GetGlobal(const AName: string): TSouffleValue; override;
    procedure SetGlobal(const AName: string;
      const AValue: TSouffleValue); override;
    function HasGlobal(const AName: string): Boolean; override;

    function UnwrapToGocciaValue(const AValue: TSouffleValue): TGocciaValue;
    function ToSouffleValue(const AValue: TGocciaValue): TSouffleValue;

    function ResolveProxyGet(const ATarget: TSouffleHeapObject;
      const AName: string): TGocciaValue;
    procedure ResolveProxySet(const ATarget: TSouffleHeapObject;
      const AName: string; const AValue: TGocciaValue);

    procedure RegisterMetatables;
    procedure RegisterGlobal(const AName: string; const AValue: TSouffleValue);
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
  Goccia.Engine,
  Goccia.Evaluator,
  Goccia.Interpreter,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue;

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
  SetLength(Args, AArguments.Length);
  for I := 0 to AArguments.Length - 1 do
    Args[I] := FRuntime.ToSouffleValue(AArguments.GetElement(I));

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
  Tbl: TSouffleTable;
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
  else if FTarget is TSouffleTable then
  begin
    Tbl := TSouffleTable(FTarget);
    S := '{';
    for I := 0 to Tbl.Count - 1 do
    begin
      if I > 0 then
        S := S + ', ';
      S := S + Tbl.GetOrderedKey(I) + ': '
        + SouffleValueToString(Tbl.GetOrderedValue(I));
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
  FVM := nil;
  FGlobals.AddOrSetValue('null',
    WrapGocciaValue(TGocciaNullLiteralValue.Create));
end;

destructor TGocciaRuntimeOperations.Destroy;
begin
  FGlobals.Free;
  FExports.Free;
  inherited;
end;

function TGocciaRuntimeOperations.WrapGocciaValue(
  const AValue: TGocciaValue): TSouffleValue;
begin
  Result := SouffleReference(TGocciaWrappedValue.Create(AValue));
end;

function ConvertSouffleArrayToGocciaArray(
  const ARuntime: TGocciaRuntimeOperations;
  const AArr: TSouffleArray): TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to AArr.Count - 1 do
    Result.Elements.Add(ARuntime.UnwrapToGocciaValue(AArr.Get(I)));
end;

function TGocciaRuntimeOperations.UnwrapToGocciaValue(
  const AValue: TSouffleValue): TGocciaValue;
begin
  case AValue.Kind of
    svkNil:
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
    svkReference:
      if AValue.AsReference is TGocciaWrappedValue then
        Result := TGocciaWrappedValue(AValue.AsReference).Value
      else if AValue.AsReference is TSouffleString then
        Result := TGocciaStringLiteralValue.Create(
          TSouffleString(AValue.AsReference).Value)
      else if AValue.AsReference is TSouffleArray then
        Result := ConvertSouffleArrayToGocciaArray(Self,
          TSouffleArray(AValue.AsReference))
      else if AValue.AsReference is TSouffleTable then
        Result := TGocciaSouffleProxy.Create(AValue.AsReference, Self)
      else if (AValue.AsReference is TSouffleClosure) and Assigned(FVM) then
        Result := TGocciaSouffleClosureBridge.Create(
          TSouffleClosure(AValue.AsReference), Self)
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

  if AValue is TGocciaUndefinedLiteralValue then
    Result := SouffleNil
  else if AValue is TGocciaNullLiteralValue then
    Result := SouffleNil
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
    Result := SouffleReference(
      TSouffleString.Create(TGocciaStringLiteralValue(AValue).Value))
  else
    Result := WrapGocciaValue(AValue);
end;

{ Arithmetic }

function TGocciaRuntimeOperations.Add(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsInteger(A) and SouffleIsInteger(B) then
    Result := SouffleInteger(A.AsInteger + B.AsInteger)
  else if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleFloat(SouffleAsNumber(A) + SouffleAsNumber(B))
  else if (A.Kind = svkReference) and (A.AsReference is TSouffleString) then
    Result := SouffleReference(TSouffleString.Create(
      TSouffleString(A.AsReference).Value + SouffleValueToString(B)))
  else if (B.Kind = svkReference) and (B.AsReference is TSouffleString) then
    Result := SouffleReference(TSouffleString.Create(
      SouffleValueToString(A) + TSouffleString(B.AsReference).Value))
  else
    Result := SouffleFloat(SouffleAsNumber(A) + SouffleAsNumber(B));
end;

function TGocciaRuntimeOperations.Subtract(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsInteger(A) and SouffleIsInteger(B) then
    Result := SouffleInteger(A.AsInteger - B.AsInteger)
  else
    Result := SouffleFloat(SouffleAsNumber(A) - SouffleAsNumber(B));
end;

function TGocciaRuntimeOperations.Multiply(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsInteger(A) and SouffleIsInteger(B) then
    Result := SouffleInteger(A.AsInteger * B.AsInteger)
  else
    Result := SouffleFloat(SouffleAsNumber(A) * SouffleAsNumber(B));
end;

function TGocciaRuntimeOperations.Divide(const A, B: TSouffleValue): TSouffleValue;
var
  Divisor: Double;
begin
  Divisor := SouffleAsNumber(B);
  if Divisor = 0.0 then
  begin
    if SouffleAsNumber(A) = 0.0 then
      Result := SouffleFloat(NaN)
    else if SouffleAsNumber(A) > 0 then
      Result := SouffleFloat(Infinity)
    else
      Result := SouffleFloat(NegInfinity);
  end
  else
    Result := SouffleFloat(SouffleAsNumber(A) / Divisor);
end;

function TGocciaRuntimeOperations.Modulo(const A, B: TSouffleValue): TSouffleValue;
var
  Divisor: Double;
begin
  if SouffleIsInteger(A) and SouffleIsInteger(B) and (B.AsInteger <> 0) then
    Result := SouffleInteger(A.AsInteger mod B.AsInteger)
  else
  begin
    Divisor := SouffleAsNumber(B);
    if Divisor = 0.0 then
      Result := SouffleFloat(NaN)
    else
      Result := SouffleFloat(FMod(SouffleAsNumber(A), Divisor));
  end;
end;

function TGocciaRuntimeOperations.Power(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleFloat(Math.Power(SouffleAsNumber(A), SouffleAsNumber(B)));
end;

function TGocciaRuntimeOperations.Negate(const A: TSouffleValue): TSouffleValue;
begin
  if SouffleIsInteger(A) then
    Result := SouffleInteger(-A.AsInteger)
  else
    Result := SouffleFloat(-SouffleAsNumber(A));
end;

{ Bitwise }

function TGocciaRuntimeOperations.BitwiseAnd(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(Int32(Trunc(SouffleAsNumber(A))) and
    Int32(Trunc(SouffleAsNumber(B))));
end;

function TGocciaRuntimeOperations.BitwiseOr(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(Int32(Trunc(SouffleAsNumber(A))) or
    Int32(Trunc(SouffleAsNumber(B))));
end;

function TGocciaRuntimeOperations.BitwiseXor(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(Int32(Trunc(SouffleAsNumber(A))) xor
    Int32(Trunc(SouffleAsNumber(B))));
end;

function TGocciaRuntimeOperations.ShiftLeft(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(Int32(Trunc(SouffleAsNumber(A))) shl
    (Int32(Trunc(SouffleAsNumber(B))) and $1F));
end;

function TGocciaRuntimeOperations.ShiftRight(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(Int32(Trunc(SouffleAsNumber(A))) shr
    (Int32(Trunc(SouffleAsNumber(B))) and $1F));
end;

function TGocciaRuntimeOperations.UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(Int64(UInt32(Trunc(SouffleAsNumber(A))) shr
    (Int32(Trunc(SouffleAsNumber(B))) and $1F)));
end;

function TGocciaRuntimeOperations.BitwiseNot(const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleInteger(not Int32(Trunc(SouffleAsNumber(A))));
end;

{ Comparison }

function TGocciaRuntimeOperations.Equal(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleBoolean(SouffleValuesEqual(A, B));
end;

function TGocciaRuntimeOperations.NotEqual(const A, B: TSouffleValue): TSouffleValue;
begin
  Result := SouffleBoolean(not SouffleValuesEqual(A, B));
end;

function TGocciaRuntimeOperations.LessThan(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) < SouffleAsNumber(B))
  else
    Result := SouffleBoolean(SouffleValueToString(A) < SouffleValueToString(B));
end;

function TGocciaRuntimeOperations.GreaterThan(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) > SouffleAsNumber(B))
  else
    Result := SouffleBoolean(SouffleValueToString(A) > SouffleValueToString(B));
end;

function TGocciaRuntimeOperations.LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) <= SouffleAsNumber(B))
  else
    Result := SouffleBoolean(SouffleValueToString(A) <= SouffleValueToString(B));
end;

function TGocciaRuntimeOperations.GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue;
begin
  if SouffleIsNumeric(A) and SouffleIsNumeric(B) then
    Result := SouffleBoolean(SouffleAsNumber(A) >= SouffleAsNumber(B))
  else
    Result := SouffleBoolean(SouffleValueToString(A) >= SouffleValueToString(B));
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
begin
  if SouffleIsReference(A) and Assigned(A.AsReference) then
  begin
    if (A.AsReference is TSouffleArray) or (A.AsReference is TSouffleTable) then
      Exit(SouffleReference(TSouffleString.Create('object')));
  end;
  GocciaVal := UnwrapToGocciaValue(A);
  TypeStr := GocciaVal.TypeOf;
  Result := SouffleReference(TSouffleString.Create(TypeStr));
end;

function TGocciaRuntimeOperations.IsInstance(const A, B: TSouffleValue): TSouffleValue;
var
  GocciaObj, GocciaClass: TGocciaValue;
begin
  GocciaObj := UnwrapToGocciaValue(A);
  GocciaClass := UnwrapToGocciaValue(B);
  if (GocciaObj is TGocciaInstanceValue) and (GocciaClass is TGocciaClassValue) then
    Result := SouffleBoolean(
      TGocciaInstanceValue(GocciaObj).IsInstanceOf(TGocciaClassValue(GocciaClass)))
  else
    Result := SouffleBoolean(False);
end;

function TGocciaRuntimeOperations.HasProperty(
  const AObject, AKey: TSouffleValue): TSouffleValue;
var
  KeyStr: string;
  Prop: TGocciaValue;
begin
  KeyStr := SouffleValueToString(AKey);
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
  begin
    if AObject.AsReference is TSouffleTable then
      Exit(SouffleBoolean(TSouffleTable(AObject.AsReference).Has(KeyStr)));
    if AObject.AsReference is TGocciaWrappedValue then
    begin
      Prop := TGocciaWrappedValue(AObject.AsReference).Value.GetProperty(KeyStr);
      Result := SouffleBoolean(Assigned(Prop) and
        not (Prop is TGocciaUndefinedLiteralValue));
      Exit;
    end;
  end;
  Result := SouffleBoolean(False);
end;

function TGocciaRuntimeOperations.ToBoolean(const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleBoolean(SouffleIsTrue(A));
end;

{ Property access }

function TGocciaRuntimeOperations.GetProperty(const AObject: TSouffleValue;
  const AKey: string): TSouffleValue;
var
  GocciaObj, Prop: TGocciaValue;
  Boxed: TGocciaObjectValue;
  Tbl: TSouffleTable;
  Arr: TSouffleArray;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
  begin
    if AObject.AsReference is TSouffleTable then
    begin
      Tbl := TSouffleTable(AObject.AsReference);
      if Tbl.Get(AKey, Result) then
        Exit;
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
    Result := SouffleNil;
end;

procedure TGocciaRuntimeOperations.SetProperty(const AObject: TSouffleValue;
  const AKey: string; const AValue: TSouffleValue);
var
  GocciaObj, Val: TGocciaValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) then
  begin
    if AObject.AsReference is TSouffleTable then
    begin
      TSouffleTable(AObject.AsReference).Put(AKey, AValue);
      Exit;
    end;
    if AObject.AsReference is TGocciaWrappedValue then
    begin
      GocciaObj := TGocciaWrappedValue(AObject.AsReference).Value;
      Val := UnwrapToGocciaValue(AValue);
      GocciaObj.SetProperty(AKey, Val);
    end;
  end;
end;

function TGocciaRuntimeOperations.CoerceKeyToString(
  const AKey: TSouffleValue): string;
begin
  if AKey.Kind = svkNil then
    Exit('undefined');
  if SouffleIsReference(AKey) and Assigned(AKey.AsReference) then
  begin
    if AKey.AsReference is TGocciaWrappedValue then
      Exit(TGocciaWrappedValue(AKey.AsReference).Value.ToStringLiteral.Value);
  end;
  Result := SouffleValueToString(AKey);
end;

function TGocciaRuntimeOperations.GetIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
     (AObject.AsReference is TSouffleArray) and (AKey.Kind = svkInteger) then
    Result := TSouffleArray(AObject.AsReference).Get(Integer(AKey.AsInteger))
  else
    Result := GetProperty(AObject, CoerceKeyToString(AKey));
end;

procedure TGocciaRuntimeOperations.SetIndex(const AObject: TSouffleValue;
  const AKey, AValue: TSouffleValue);
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
     (AObject.AsReference is TSouffleArray) and (AKey.Kind = svkInteger) then
    TSouffleArray(AObject.AsReference).Put(Integer(AKey.AsInteger), AValue)
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
    if AObject.AsReference is TSouffleTable then
    begin
      Result := SouffleBoolean(TSouffleTable(AObject.AsReference).Delete(AKey));
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

{ Invocation -- delegates to GocciaScript's existing call mechanism }

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
    Result := SouffleNil;
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
      Result := SouffleNil;
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
begin
  GocciaConstructor := UnwrapToGocciaValue(AConstructor);

  if not GocciaConstructor.IsCallable then
  begin
    Result := SouffleNil;
    Exit;
  end;

  Args := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to AArgCount - 1 do
      Args.Add(UnwrapToGocciaValue(PSouffleValue(PByte(AArgs) + I * SizeOf(TSouffleValue))^));

    if (GocciaConstructor is TGocciaClassValue) and Assigned(FEngine) then
    begin
      Context := TGocciaEngine(FEngine).Interpreter.CreateEvaluationContext;
      GocciaResult := InstantiateClass(
        TGocciaClassValue(GocciaConstructor), Args, Context);
    end
    else if GocciaConstructor is TGocciaFunctionBase then
      GocciaResult := TGocciaFunctionBase(GocciaConstructor).Call(Args, nil)
    else
      GocciaResult := nil;

    if Assigned(GocciaResult) then
      Result := ToSouffleValue(GocciaResult)
    else
      Result := SouffleNil;
  finally
    Args.Free;
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
  Result := SouffleNil;
end;

procedure TGocciaRuntimeOperations.SpreadInto(
  const ATarget, ASource: TSouffleValue);
var
  SrcArr: TSouffleArray;
  TgtArr: TSouffleArray;
  I: Integer;
begin
  if SouffleIsReference(ATarget) and (ATarget.AsReference is TSouffleArray) and
     SouffleIsReference(ASource) and (ASource.AsReference is TSouffleArray) then
  begin
    TgtArr := TSouffleArray(ATarget.AsReference);
    SrcArr := TSouffleArray(ASource.AsReference);
    for I := 0 to SrcArr.Count - 1 do
      TgtArr.Push(SrcArr.Get(I));
  end;
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
    Result := SouffleNil;
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

{ Proxy resolution — single dispatch point for all Souffle compound types }

function TGocciaRuntimeOperations.ResolveProxyGet(
  const ATarget: TSouffleHeapObject; const AName: string): TGocciaValue;
var
  Arr: TSouffleArray;
  Tbl: TSouffleTable;
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
  else if ATarget is TSouffleTable then
  begin
    Tbl := TSouffleTable(ATarget);
    if Tbl.Get(AName, Val) then
      Exit(UnwrapToGocciaValue(Val));
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
  Tbl: TSouffleTable;
  Index: Integer;
begin
  if ATarget is TSouffleArray then
  begin
    Arr := TSouffleArray(ATarget);
    if TryStrToInt(AName, Index) and (Index >= 0) then
      Arr.Put(Index, ToSouffleValue(AValue));
  end
  else if ATarget is TSouffleTable then
  begin
    Tbl := TSouffleTable(ATarget);
    Tbl.Put(AName, ToSouffleValue(AValue));
  end;
end;

{ Native metatable callbacks — operate on TSouffleValue directly, no wrapping }

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

function NativeArrayJoin(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
var
  Arr: TSouffleArray;
  Sep, S: string;
  I: Integer;
  SepStr: TSouffleString;
begin
  if SouffleIsReference(AReceiver) and Assigned(AReceiver.AsReference)
     and (AReceiver.AsReference is TSouffleArray) then
  begin
    Arr := TSouffleArray(AReceiver.AsReference);
    Sep := ',';
    if (AArgCount > 0) and SouffleIsReference(AArgs^)
       and (AArgs^.AsReference is TSouffleString) then
    begin
      SepStr := TSouffleString(AArgs^.AsReference);
      Sep := SepStr.Value;
    end;
    S := '';
    for I := 0 to Arr.Count - 1 do
    begin
      if I > 0 then
        S := S + Sep;
      S := S + SouffleValueToString(Arr.Get(I));
    end;
    Result := SouffleReference(TSouffleString.Create(S));
    if Assigned(TSouffleGarbageCollector.Instance) then
      TSouffleGarbageCollector.Instance.AllocateObject(Result.AsReference);
  end
  else
    Result := SouffleReference(TSouffleString.Create(''));
end;

procedure TGocciaRuntimeOperations.RegisterMetatables;
var
  ArrayMeta: TSouffleTable;
  GC: TSouffleGarbageCollector;
  PushFn, PopFn, JoinFn: TSouffleNativeFunction;
begin
  if not Assigned(FVM) then Exit;

  GC := TSouffleGarbageCollector.Instance;

  ArrayMeta := TSouffleTable.Create(8);
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

  JoinFn := TSouffleNativeFunction.Create('join', 1, @NativeArrayJoin);
  if Assigned(GC) then
    GC.AllocateObject(JoinFn);
  ArrayMeta.Put('join', SouffleReference(JoinFn));

  FVM.ArrayMetatable := ArrayMeta;
end;

procedure TGocciaRuntimeOperations.RegisterGlobal(const AName: string;
  const AValue: TSouffleValue);
begin
  FGlobals.AddOrSetValue(AName, AValue);
end;

end.
