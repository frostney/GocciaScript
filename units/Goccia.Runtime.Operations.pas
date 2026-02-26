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

  TGocciaRuntimeOperations = class(TSouffleRuntimeOperations)
  private
    FGlobals: TDictionary<string, TSouffleValue>;
    FExports: TDictionary<string, TSouffleValue>;
    FVM: TSouffleVM;
    FEngine: TObject;

    function WrapGocciaValue(const AValue: TGocciaValue): TSouffleValue;
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

    function CreateCompound(const ATypeTag: UInt8): TSouffleValue; override;
    procedure InitField(const ACompound: TSouffleValue; const AKey: string;
      const AValue: TSouffleValue); override;
    procedure InitIndex(const ACompound: TSouffleValue; const AIndex: TSouffleValue;
      const AValue: TSouffleValue); override;

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

    function UnwrapToGocciaValue(const AValue: TSouffleValue): TGocciaValue;
    function ToSouffleValue(const AValue: TGocciaValue): TSouffleValue;

    procedure RegisterGlobal(const AName: string; const AValue: TSouffleValue);
    property ModuleExports: TDictionary<string, TSouffleValue> read FExports;
    property VM: TSouffleVM read FVM write FVM;
    property Engine: TObject read FEngine write FEngine;
  end;

implementation

uses
  Math,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Evaluator,
  Goccia.Interpreter,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
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
  Result := FClosure.Prototype.ParameterCount;
end;

function TGocciaSouffleClosureBridge.GetFunctionName: string;
begin
  Result := FClosure.Prototype.Name;
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

  SouffleResult := FRuntime.VM.ExecuteFunction(FClosure, Args);
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
  if SouffleIsReference(AObject) and
     (AObject.AsReference is TGocciaWrappedValue) then
  begin
    Prop := TGocciaWrappedValue(AObject.AsReference).Value.GetProperty(KeyStr);
    Result := SouffleBoolean(Assigned(Prop) and
      not (Prop is TGocciaUndefinedLiteralValue));
  end
  else
    Result := SouffleBoolean(False);
end;

function TGocciaRuntimeOperations.ToBoolean(const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleBoolean(SouffleIsTrue(A));
end;

{ Compound creation }

function TGocciaRuntimeOperations.CreateCompound(const ATypeTag: UInt8): TSouffleValue;
var
  GocciaVal: TGocciaValue;
begin
  case ATypeTag of
    1: GocciaVal := TGocciaArrayValue.Create;
  else
    GocciaVal := TGocciaObjectValue.Create;
  end;
  Result := WrapGocciaValue(GocciaVal);
end;

procedure TGocciaRuntimeOperations.InitField(const ACompound: TSouffleValue;
  const AKey: string; const AValue: TSouffleValue);
var
  Wrapped: TGocciaValue;
  Val: TGocciaValue;
begin
  if SouffleIsReference(ACompound) and
     (ACompound.AsReference is TGocciaWrappedValue) then
  begin
    Wrapped := TGocciaWrappedValue(ACompound.AsReference).Value;
    Val := UnwrapToGocciaValue(AValue);
    Wrapped.SetProperty(AKey, Val);
  end;
end;

procedure TGocciaRuntimeOperations.InitIndex(const ACompound: TSouffleValue;
  const AIndex: TSouffleValue; const AValue: TSouffleValue);
var
  Wrapped: TGocciaValue;
  Val: TGocciaValue;
begin
  if SouffleIsReference(ACompound) and
     (ACompound.AsReference is TGocciaWrappedValue) then
  begin
    Wrapped := TGocciaWrappedValue(ACompound.AsReference).Value;
    Val := UnwrapToGocciaValue(AValue);
    if Wrapped is TGocciaArrayValue then
      TGocciaArrayValue(Wrapped).Elements.Add(Val)
    else
      Wrapped.SetProperty(SouffleValueToString(AIndex), Val);
  end;
end;

{ Property access }

function TGocciaRuntimeOperations.GetProperty(const AObject: TSouffleValue;
  const AKey: string): TSouffleValue;
var
  GocciaObj, Prop: TGocciaValue;
  Boxed: TGocciaObjectValue;
begin
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
  if SouffleIsReference(AObject) and
     (AObject.AsReference is TGocciaWrappedValue) then
  begin
    GocciaObj := TGocciaWrappedValue(AObject.AsReference).Value;
    Val := UnwrapToGocciaValue(AValue);
    GocciaObj.SetProperty(AKey, Val);
  end;
end;

function TGocciaRuntimeOperations.GetIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
begin
  Result := GetProperty(AObject, SouffleValueToString(AKey));
end;

procedure TGocciaRuntimeOperations.SetIndex(const AObject: TSouffleValue;
  const AKey, AValue: TSouffleValue);
begin
  SetProperty(AObject, SouffleValueToString(AKey), AValue);
end;

function TGocciaRuntimeOperations.DeleteProperty(const AObject: TSouffleValue;
  const AKey: string): TSouffleValue;
var
  GocciaObj: TGocciaValue;
begin
  if SouffleIsReference(AObject) and Assigned(AObject.AsReference) and
     (AObject.AsReference is TGocciaWrappedValue) then
  begin
    GocciaObj := TGocciaWrappedValue(AObject.AsReference).Value;
    if GocciaObj is TGocciaObjectValue then
    begin
      TGocciaObjectValue(GocciaObj).DeleteProperty(AKey);
      Result := SouffleBoolean(True);
      Exit;
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
begin
  // Placeholder for spread implementation
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

procedure TGocciaRuntimeOperations.RegisterGlobal(const AName: string;
  const AValue: TSouffleValue);
begin
  FGlobals.AddOrSetValue(AName, AValue);
end;

end.
