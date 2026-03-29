unit Goccia.VM;

{$I Goccia.inc}

interface

uses
  Math,

  Goccia.Arguments.Collection,
  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.Evaluator.Context,
  Goccia.Interpreter,
  Goccia.Modules,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.CallFrame,
  Goccia.VM.Closure,
  Goccia.VM.Exception,
  Goccia.VM.Upvalue;

type
  TGocciaBytecodeCellArray = array of TGocciaBytecodeCell;
  TGocciaArgumentsPoolArray = array of TGocciaArgumentsCollection;

  TGocciaVM = class
  private
    FRegisters: TGocciaRegisterArray;
    FLocalCells: TGocciaBytecodeCellArray;
    FArgCount: Integer;
    FGlobalScope: TGocciaScope;
    FInterpreter: TGocciaInterpreter;
    FCurrentClosure: TGocciaBytecodeClosure;
    FHandlerStack: TGocciaBytecodeHandlerStack;
    FFrameDepth: Integer;
    FCurrentModuleSourcePath: string;
    FCurrentModuleExports: TGocciaValueMap;
    FActiveDecoratorSession: TObject;
    FPreviousExceptionMask: TFPUExceptionMask;
    FArgumentPool: TGocciaArgumentsPoolArray;
    FArgumentPoolCount: Integer;
    function ConstantToValue(const AConstant: TGocciaBytecodeConstant): TGocciaValue;
    function AcquireArguments(const ACapacity: Integer = 0): TGocciaArgumentsCollection;
    procedure ReleaseArguments(const AArguments: TGocciaArgumentsCollection);
    function AcquireRegisters(const ACount: Integer): TGocciaRegisterArray;
    procedure ReleaseRegisters(var ARegisters: TGocciaRegisterArray);
    function AcquireLocalCells(const ACount: Integer): TGocciaBytecodeCellArray;
    procedure ReleaseLocalCells(var ALocalCells: TGocciaBytecodeCellArray);
    procedure EnsureRegisterCapacity(const ACount: Integer);
    procedure EnsureLocalCapacity(const ACount: Integer);
    function GetLocalCell(const AIndex: Integer): TGocciaBytecodeCell;
    function GetRegister(const AIndex: Integer): TGocciaValue; inline;
    function GetRegisterFast(const AIndex: Integer): TGocciaValue; inline;
    procedure SetRegister(const AIndex: Integer; const AValue: TGocciaValue); inline;
    procedure SetRegisterFast(const AIndex: Integer; const AValue: TGocciaValue); inline;
    function GetLocal(const AIndex: Integer): TGocciaValue; inline;
    function GetLocalFast(const AIndex: Integer): TGocciaValue; inline;
    procedure SetLocal(const AIndex: Integer; const AValue: TGocciaValue); inline;
    procedure SetLocalFast(const AIndex: Integer; const AValue: TGocciaValue); inline;
    function MatchesNullishKind(const AValue: TGocciaValue; const AKind: UInt8): Boolean;
    function TryGetArrayIndex(const AKey: TGocciaValue; out AIndex: Integer): Boolean;
    function KeyToPropertyName(const AKey: TGocciaValue): string;
    function IterableToArray(const AIterable: TGocciaValue;
      const ATryAsync: Boolean = False): TGocciaArrayValue;
    procedure SpreadObjectIntoValue(const ATarget: TGocciaObjectValue;
      const ASource: TGocciaValue);
    function ObjectRestValue(const ASource: TGocciaValue;
      const AExclusionKeys: TGocciaArrayValue): TGocciaObjectValue;
    function GetIteratorValue(const AIterable: TGocciaValue;
      const ATryAsync: Boolean): TGocciaValue;
    function ConstructValue(const AConstructor: TGocciaValue;
      const AArguments: TGocciaArgumentsCollection): TGocciaValue;
    function ImportModuleValue(const APath: string): TGocciaValue;
    procedure ExportBindingValue(const AName: string; const AValue: TGocciaValue);
    procedure DefineGetterProperty(const ATarget: TGocciaValue; const AName: string;
      const AGetter: TGocciaValue);
    procedure DefineSetterProperty(const ATarget: TGocciaValue; const AName: string;
      const ASetter: TGocciaValue);
    procedure DefineStaticGetterProperty(const ATarget: TGocciaValue; const AName: string;
      const AGetter: TGocciaValue);
    procedure DefineStaticSetterProperty(const ATarget: TGocciaValue; const AName: string;
      const ASetter: TGocciaValue);
    procedure DefineGetterPropertyByKey(const ATarget, AKey, AGetter: TGocciaValue);
    procedure DefineSetterPropertyByKey(const ATarget, AKey, ASetter: TGocciaValue);
    procedure DefineStaticGetterPropertyByKey(const ATarget, AKey, AGetter: TGocciaValue);
    procedure DefineStaticSetterPropertyByKey(const ATarget, AKey, ASetter: TGocciaValue);
    procedure DefineGlobalValue(const AName: string; const AValue: TGocciaValue);
    function FinalizeEnumValue(const AValue: TGocciaValue; const AName: string): TGocciaValue;
    procedure SetupAutoAccessorValue(const AName: string);
    procedure RunClassInitializers(const AClassValue: TGocciaClassValue;
      const AInstance: TGocciaValue);
    procedure InvokeImplicitSuperInitialization(const AClassValue: TGocciaClassValue;
      const AInstance: TGocciaValue; const AArguments: TGocciaArgumentsCollection);
    procedure BeginDecorators(const AClassValue, ASuperValue: TGocciaValue);
    procedure ApplyElementDecorator(const ADecoratorFn: TGocciaValue;
      const ADescriptor: string);
    procedure ApplyClassDecorator(const ADecoratorFn: TGocciaValue);
    function FinishDecorators(const ACurrentValue: TGocciaValue): TGocciaValue;
    function GetSuperPropertyValue(const ASuperValue, AThisValue: TGocciaValue;
      const AName: string): TGocciaValue;
    function GetPropertyValue(const AObject: TGocciaValue; const AKey: string): TGocciaValue;
    procedure SetPropertyValue(const AObject: TGocciaValue; const AKey: string;
      const AValue: TGocciaValue);
    function TryGetRawPrivateValue(const AObject: TGocciaValue; const AKey: string;
      out AValue: TGocciaValue): Boolean;
    procedure SetRawPrivateValue(const AObject: TGocciaValue; const AKey: string;
      const AValue: TGocciaValue);
    function HasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
    function InvokeFunctionValue(const ACallee: TGocciaValue;
      const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExecuteClosure(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection): TGocciaValue;
  public
    constructor Create;
    destructor Destroy; override;
    function ExecuteFunction(const ATemplate: TGocciaFunctionTemplate): TGocciaValue;
    function ExecuteModule(const AModule: TGocciaBytecodeModule): TGocciaValue;
    property GlobalScope: TGocciaScope read FGlobalScope write FGlobalScope;
    property Interpreter: TGocciaInterpreter read FInterpreter write FInterpreter;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,

  GarbageCollector.Generic,

  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Decorators,
  Goccia.Scope.BindingMap,
  Goccia.Values.ClassHelper,
  Goccia.Values.EnumValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

function VMIsObjectInstanceOfClass(const AObj: TGocciaObjectValue;
  const AClassValue: TGocciaClassValue): Boolean;
var
  CurrentPrototype: TGocciaObjectValue;
  TargetPrototype: TGocciaObjectValue;
begin
  Result := False;
  TargetPrototype := AClassValue.Prototype;
  if not Assigned(TargetPrototype) then
    Exit;

  CurrentPrototype := AObj.Prototype;
  while Assigned(CurrentPrototype) do
  begin
    if CurrentPrototype = TargetPrototype then
      Exit(True);
    CurrentPrototype := CurrentPrototype.Prototype;
  end;
end;

function VMIsPrototypeInChain(const AObj: TGocciaObjectValue;
  const ATargetProto: TGocciaObjectValue): Boolean; inline;
var
  CurrentProto: TGocciaObjectValue;
begin
  Result := False;
  CurrentProto := AObj.Prototype;
  while Assigned(CurrentProto) do
  begin
    if CurrentProto = ATargetProto then
      Exit(True);
    CurrentProto := CurrentProto.Prototype;
  end;
end;

function VMNumberValue(const AValue: Double): TGocciaNumberLiteralValue; inline;
var
  Bits: Int64 absolute AValue;
begin
  if Math.IsNaN(AValue) then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  if Math.IsInfinite(AValue) then
  begin
    if AValue > 0 then
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
  end;
  if AValue = 0.0 then
  begin
    if Bits < 0 then
      Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
    Exit(TGocciaNumberLiteralValue.ZeroValue);
  end;
  if (AValue >= 0.0) and (AValue <= 255.0) and (Frac(AValue) = 0.0) then
    Exit(TGocciaNumberLiteralValue.SmallInt(Trunc(AValue)));
  if AValue = 1.0 then
    Exit(TGocciaNumberLiteralValue.OneValue);
  Result := TGocciaNumberLiteralValue.Create(AValue);
end;

function VMInfinityWithSign(const APositive: Boolean): TGocciaNumberLiteralValue; inline;
begin
  if APositive then
    Result := TGocciaNumberLiteralValue.InfinityValue
  else
    Result := TGocciaNumberLiteralValue.NegativeInfinityValue;
end;

function VMToNumericPair(const ALeft, ARight: TGocciaValue;
  out ALeftNum, ARightNum: TGocciaNumberLiteralValue): Boolean; inline;
begin
  if (ALeft is TGocciaSymbolValue) or (ARight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a number');
  ALeftNum := ALeft.ToNumberLiteral;
  ARightNum := ARight.ToNumberLiteral;
  Result := not (ALeftNum.IsNaN or ARightNum.IsNaN);
end;

function VMIsActualZero(const ANum: TGocciaNumberLiteralValue): Boolean; inline;
begin
  Result := (ANum.Value = 0) and not ANum.IsNaN and not ANum.IsInfinite;
end;

function VMCompareNumbers(const ALeftNum, ARightNum: TGocciaNumberLiteralValue;
  const AIsGreater: Boolean): Boolean; inline;
begin
  if ALeftNum.IsNaN or ARightNum.IsNaN then
    Exit(False);

  if AIsGreater then
  begin
    if ALeftNum.IsInfinity then
      Exit(not ARightNum.IsInfinity);
    if ALeftNum.IsNegativeInfinity then
      Exit(False);
    if ARightNum.IsInfinity then
      Exit(False);
    if ARightNum.IsNegativeInfinity then
      Exit(True);
    Exit(ALeftNum.Value > ARightNum.Value);
  end;

  if ALeftNum.IsInfinity then
    Exit(False);
  if ALeftNum.IsNegativeInfinity then
    Exit(not ARightNum.IsNegativeInfinity);
  if ARightNum.IsInfinity then
    Exit(True);
  if ARightNum.IsNegativeInfinity then
    Exit(False);
  Result := ALeftNum.Value < ARightNum.Value;
end;

function VMLessThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue(ALeft).Value <
      TGocciaStringLiteralValue(ARight).Value);

  Result := VMCompareNumbers(ALeft.ToNumberLiteral, ARight.ToNumberLiteral, False);
end;

function VMGreaterThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue(ALeft).Value >
      TGocciaStringLiteralValue(ARight).Value);

  Result := VMCompareNumbers(ALeft.ToNumberLiteral, ARight.ToNumberLiteral, True);
end;

function VMLessThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue(ALeft).Value <=
      TGocciaStringLiteralValue(ARight).Value);

  LeftNum := ALeft.ToNumberLiteral;
  RightNum := ARight.ToNumberLiteral;
  if LeftNum.IsNaN or RightNum.IsNaN then
    Exit(False);
  Result := not VMGreaterThan(ALeft, ARight);
end;

function VMGreaterThanOrEqual(const ALeft, ARight: TGocciaValue): Boolean; inline;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue(ALeft).Value >=
      TGocciaStringLiteralValue(ARight).Value);

  LeftNum := ALeft.ToNumberLiteral;
  RightNum := ARight.ToNumberLiteral;
  if LeftNum.IsNaN or RightNum.IsNaN then
    Exit(False);
  Result := not VMLessThan(ALeft, ARight);
end;

function VMToECMAStringFast(const AValue: TGocciaValue): TGocciaStringLiteralValue; inline;
var
  PrimitiveValue: TGocciaValue;
begin
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue(AValue));

  if AValue is TGocciaSymbolValue then
    ThrowTypeError('Cannot convert a Symbol value to a string');

  if AValue.IsPrimitive then
    Exit(AValue.ToStringLiteral);

  PrimitiveValue := ToPrimitive(AValue, tphString);
  if PrimitiveValue is TGocciaSymbolValue then
    ThrowTypeError('Cannot convert a Symbol value to a string');
  Result := PrimitiveValue.ToStringLiteral;
end;

function VMBitwiseAndValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
begin
  Result := VMNumberValue(Trunc(ALeft.ToNumberLiteral.Value) and
    Trunc(ARight.ToNumberLiteral.Value));
end;

function VMBitwiseOrValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
begin
  Result := VMNumberValue(Trunc(ALeft.ToNumberLiteral.Value) or
    Trunc(ARight.ToNumberLiteral.Value));
end;

function VMBitwiseXorValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
begin
  Result := VMNumberValue(Trunc(ALeft.ToNumberLiteral.Value) xor
    Trunc(ARight.ToNumberLiteral.Value));
end;

function VMLeftShiftValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
begin
  Result := VMNumberValue(Trunc(ALeft.ToNumberLiteral.Value) shl
    (Trunc(ARight.ToNumberLiteral.Value) and 31));
end;

function VMRightShiftValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
begin
  Result := VMNumberValue(SarLongint(Int32(Trunc(ALeft.ToNumberLiteral.Value)),
    Trunc(ARight.ToNumberLiteral.Value) and 31));
end;

function VMUnsignedRightShiftValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
begin
  Result := VMNumberValue(Cardinal(Trunc(ALeft.ToNumberLiteral.Value)) shr
    (Trunc(ARight.ToNumberLiteral.Value) and 31));
end;

function VMBitwiseNotValue(const AOperand: TGocciaValue): TGocciaValue; inline;
begin
  Result := VMNumberValue(not Trunc(AOperand.ToNumberLiteral.Value));
end;

function VMInstanceOfValue(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  ConstructorProto: TGocciaValue;
begin
  if not (ARight is TGocciaClassValue) then
  begin
    if (ARight is TGocciaFunctionBase) and (ALeft is TGocciaObjectValue) then
    begin
      ConstructorProto := TGocciaFunctionBase(ARight).GetProperty(PROP_PROTOTYPE);
      if (ConstructorProto is TGocciaObjectValue) and
         VMIsPrototypeInChain(TGocciaObjectValue(ALeft),
           TGocciaObjectValue(ConstructorProto)) then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
    end;
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  if ALeft is TGocciaInstanceValue then
  begin
    if TGocciaClassValue(ARight).Name = CONSTRUCTOR_OBJECT then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    if TGocciaInstanceValue(ALeft).IsInstanceOf(TGocciaClassValue(ARight)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    if VMIsObjectInstanceOfClass(TGocciaObjectValue(ALeft), TGocciaClassValue(ARight)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  if (ALeft is TGocciaFunctionValue) and
     (TGocciaClassValue(ARight).Name = 'Function') then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaNativeFunctionValue) and
     (TGocciaClassValue(ARight).Name = 'Function') then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaClassValue) and
     (TGocciaClassValue(ARight).Name = 'Function') then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaArrayValue) and
     (TGocciaClassValue(ARight).Name = CONSTRUCTOR_ARRAY) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaArrayValue) and
     (TGocciaClassValue(ARight).Name = CONSTRUCTOR_OBJECT) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaObjectValue) and
     (TGocciaClassValue(ARight).Name = CONSTRUCTOR_OBJECT) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaObjectValue) and
     VMIsObjectInstanceOfClass(TGocciaObjectValue(ALeft), TGocciaClassValue(ARight)) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);

  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function VMAddValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue.Create(
      TGocciaStringLiteralValue(ALeft).Value + TGocciaStringLiteralValue(ARight).Value));

  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    LeftNum := TGocciaNumberLiteralValue(ALeft);
    RightNum := TGocciaNumberLiteralValue(ARight);
    if LeftNum.IsNaN or RightNum.IsNaN then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    if LeftNum.IsInfinite or RightNum.IsInfinite then
    begin
      if LeftNum.IsInfinite and RightNum.IsInfinite then
      begin
        if LeftNum.IsInfinity = RightNum.IsInfinity then
          Exit(VMInfinityWithSign(LeftNum.IsInfinity));
        Exit(TGocciaNumberLiteralValue.NaNValue);
      end;
      if LeftNum.IsInfinite then
        Exit(VMInfinityWithSign(LeftNum.IsInfinity));
      Exit(VMInfinityWithSign(RightNum.IsInfinity));
    end;
    Exit(VMNumberValue(LeftNum.Value + RightNum.Value));
  end;

  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaSymbolValue) or (PrimRight is TGocciaSymbolValue) then
    ThrowTypeError('Cannot convert a Symbol value to a string');

  if (PrimLeft is TGocciaStringLiteralValue) or (PrimRight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue.Create(
      PrimLeft.ToStringLiteral.Value + PrimRight.ToStringLiteral.Value));

  LeftNum := PrimLeft.ToNumberLiteral;
  RightNum := PrimRight.ToNumberLiteral;

  if LeftNum.IsNaN or RightNum.IsNaN then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite and RightNum.IsInfinite then
    begin
      if LeftNum.IsInfinity = RightNum.IsInfinity then
        Exit(VMInfinityWithSign(LeftNum.IsInfinity));
      Exit(TGocciaNumberLiteralValue.NaNValue);
    end;
    if LeftNum.IsInfinite then
      Exit(VMInfinityWithSign(LeftNum.IsInfinity));
    Exit(VMInfinityWithSign(RightNum.IsInfinity));
  end;

  Result := VMNumberValue(LeftNum.Value + RightNum.Value);
end;

function VMSubtractValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if not VMToNumericPair(ALeft, ARight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite and RightNum.IsInfinite then
    begin
      if LeftNum.IsInfinity <> RightNum.IsInfinity then
        Exit(VMInfinityWithSign(LeftNum.IsInfinity));
      Exit(TGocciaNumberLiteralValue.NaNValue);
    end;
    if LeftNum.IsInfinite then
      Exit(VMInfinityWithSign(LeftNum.IsInfinity));
    Exit(VMInfinityWithSign(not RightNum.IsInfinity));
  end;

  Result := VMNumberValue(LeftNum.Value - RightNum.Value);
end;

function VMMultiplyValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  LeftZero, RightZero: Boolean;
  SameSign: Boolean;
begin
  if not VMToNumericPair(ALeft, ARight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite or RightNum.IsInfinite then
  begin
    LeftZero := (not LeftNum.IsInfinite) and (LeftNum.Value = 0);
    RightZero := (not RightNum.IsInfinite) and (RightNum.Value = 0);
    if LeftZero or RightZero then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    SameSign := LeftNum.IsInfinity = RightNum.IsInfinity;
    if not LeftNum.IsInfinite then
      SameSign := (LeftNum.Value > 0) = RightNum.IsInfinity
    else if not RightNum.IsInfinite then
      SameSign := LeftNum.IsInfinity = (RightNum.Value > 0);
    Exit(VMInfinityWithSign(SameSign));
  end;

  Result := VMNumberValue(LeftNum.Value * RightNum.Value);
end;

function VMDivideValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  SameSign: Boolean;
begin
  if not VMToNumericPair(ALeft, ARight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite then
  begin
    if RightNum.IsInfinite then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    SameSign := LeftNum.IsInfinity =
      ((RightNum.Value > 0) or ((RightNum.Value = 0) and not RightNum.IsNegativeZero));
    Exit(VMInfinityWithSign(SameSign));
  end;

  if RightNum.IsInfinite then
  begin
    SameSign := (LeftNum.Value > 0) or
      ((LeftNum.Value = 0) and not LeftNum.IsNegativeZero);
    if SameSign = RightNum.IsInfinity then
      Exit(TGocciaNumberLiteralValue.ZeroValue);
    Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
  end;

  if RightNum.Value = 0 then
  begin
    if LeftNum.Value = 0 then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    if LeftNum.Value > 0 then
    begin
      if RightNum.IsNegativeZero then
        Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    end;
    if RightNum.IsNegativeZero then
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
  end;

  Result := VMNumberValue(LeftNum.Value / RightNum.Value);
end;

function VMModuloValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if not VMToNumericPair(ALeft, ARight, LeftNum, RightNum) then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if LeftNum.IsInfinite then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  if RightNum.IsInfinite then
    Exit(VMNumberValue(LeftNum.Value));

  if RightNum.Value = 0 then
    Exit(TGocciaNumberLiteralValue.NaNValue);

  Result := VMNumberValue(
    LeftNum.Value - RightNum.Value * Trunc(LeftNum.Value / RightNum.Value));
end;

function VMPowerValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  if not VMToNumericPair(ALeft, ARight, LeftNum, RightNum) then
  begin
    if VMIsActualZero(RightNum) then
      Exit(TGocciaNumberLiteralValue.OneValue);
    Exit(TGocciaNumberLiteralValue.NaNValue);
  end;

  if VMIsActualZero(RightNum) then
    Exit(TGocciaNumberLiteralValue.OneValue);

  if RightNum.IsInfinite then
  begin
    if LeftNum.IsInfinite or (Abs(LeftNum.Value) > 1) then
    begin
      if RightNum.IsInfinity then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      Exit(TGocciaNumberLiteralValue.ZeroValue);
    end;
    if Abs(LeftNum.Value) = 1 then
      Exit(TGocciaNumberLiteralValue.NaNValue);
    if RightNum.IsInfinity then
      Exit(TGocciaNumberLiteralValue.ZeroValue);
    Exit(TGocciaNumberLiteralValue.InfinityValue);
  end;

  if LeftNum.IsInfinite then
  begin
    if RightNum.Value > 0 then
    begin
      if LeftNum.IsInfinity then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      if Frac(RightNum.Value) <> 0 then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      if Frac(RightNum.Value / 2) = 0 then
        Exit(TGocciaNumberLiteralValue.InfinityValue);
      Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
    end;
    if LeftNum.IsNegativeInfinity and (Frac(RightNum.Value) = 0) and
       (Frac(RightNum.Value / 2) <> 0) then
      Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
    Exit(TGocciaNumberLiteralValue.ZeroValue);
  end;

  Result := VMNumberValue(Power(LeftNum.Value, RightNum.Value));
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
  AFlags := StrToIntDef(Copy(ADescriptor, P2 + 1,
    Length(ADescriptor) - P2), 0);
end;

type
  TGocciaVMDecoratorSession = class
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

  TGocciaBytecodeFunctionValue = class(TGocciaFunctionBase)
  private
    FClosure: TGocciaBytecodeClosure;
    FVM: TGocciaVM;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure);
    destructor Destroy; override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMSuperConstructorValue = class(TGocciaFunctionBase)
  private
    FSuperClass: TGocciaClassValue;
  protected
    function GetFunctionName: string; override;
  public
    constructor Create(const ASuperClass: TGocciaClassValue);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMClassValue = class(TGocciaClassValue)
  private
    FVM: TGocciaVM;
    FConstructorValue: TGocciaValue;
  public
    constructor Create(const AVM: TGocciaVM; const AName: string;
      const ASuperClass: TGocciaClassValue);
    function Instantiate(const AArguments: TGocciaArgumentsCollection): TGocciaValue; override;
    procedure SetVMConstructor(const AValue: TGocciaValue);
    procedure MarkReferences; override;
  end;

constructor TGocciaBytecodeFunctionValue.Create(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure);
begin
  inherited Create;
  FVM := AVM;
  FClosure := AClosure;
end;

constructor TGocciaVMClassValue.Create(const AVM: TGocciaVM; const AName: string;
  const ASuperClass: TGocciaClassValue);
begin
  inherited Create(AName, ASuperClass);
  FVM := AVM;
  FConstructorValue := nil;
end;

constructor TGocciaVMSuperConstructorValue.Create(
  const ASuperClass: TGocciaClassValue);
begin
  inherited Create;
  FSuperClass := ASuperClass;
end;

function TGocciaBytecodeFunctionValue.GetFunctionLength: Integer;
begin
  Result := FClosure.Template.FormalParameterCount;
end;

constructor TGocciaVMDecoratorSession.Create(
  const AMetadataObject: TGocciaObjectValue);
begin
  inherited Create;
  MetadataObject := AMetadataObject;
  MethodCollector := TGocciaInitializerCollector.Create;
  FieldCollector := TGocciaInitializerCollector.Create;
  StaticFieldCollector := TGocciaInitializerCollector.Create;
  ClassCollector := TGocciaInitializerCollector.Create;
  ClassValue := nil;
end;

destructor TGocciaVMDecoratorSession.Destroy;
begin
  MethodCollector.Free;
  FieldCollector.Free;
  StaticFieldCollector.Free;
  ClassCollector.Free;
  inherited;
end;

function TGocciaBytecodeFunctionValue.GetFunctionName: string;
begin
  Result := FClosure.Template.Name;
end;

function TGocciaVMSuperConstructorValue.GetFunctionName: string;
begin
  Result := 'super';
end;

function TGocciaBytecodeFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    try
      try
        Promise.Resolve(FVM.ExecuteClosure(FClosure, AThisValue, AArguments));
      except
        on E: EGocciaBytecodeThrow do
          Promise.Reject(E.ThrownValue);
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    except
      Promise.Free;
      raise;
    end;
    Exit(Promise);
  end;

  Result := FVM.ExecuteClosure(FClosure, AThisValue, AArguments);
end;

function TGocciaVMSuperConstructorValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if (FSuperClass is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(FSuperClass).FConstructorValue) then
  begin
    TGocciaVMClassValue(FSuperClass).FVM.RunClassInitializers(
      FSuperClass, AThisValue);
    Exit(TGocciaVMClassValue(FSuperClass).FVM.InvokeFunctionValue(
      TGocciaVMClassValue(FSuperClass).FConstructorValue,
      AArguments, AThisValue));
  end;

  if Assigned(FSuperClass) and Assigned(FSuperClass.ConstructorMethod) then
  begin
    if FSuperClass is TGocciaVMClassValue then
      TGocciaVMClassValue(FSuperClass).FVM.RunClassInitializers(
        FSuperClass, AThisValue);
    Exit(FSuperClass.ConstructorMethod.Call(AArguments, AThisValue));
  end;

  if Assigned(FSuperClass) and Assigned(FSuperClass.SuperClass) and
     Assigned(FSuperClass.SuperClass.ConstructorMethod) then
  begin
    if FSuperClass is TGocciaVMClassValue then
      TGocciaVMClassValue(FSuperClass).FVM.InvokeImplicitSuperInitialization(
        FSuperClass.SuperClass, AThisValue, AArguments);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if AThisValue is TGocciaInstanceValue then
  begin
    if (FSuperClass is TGocciaVMClassValue) and Assigned(FSuperClass) then
      TGocciaVMClassValue(FSuperClass).FVM.InvokeImplicitSuperInitialization(
        FSuperClass, AThisValue, AArguments);
    TGocciaInstanceValue(AThisValue).InitializeNativeFromArguments(AArguments);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

constructor TGocciaVM.Create;
begin
  inherited Create;
  FPreviousExceptionMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  FHandlerStack := TGocciaBytecodeHandlerStack.Create;
  FArgumentPoolCount := 0;
  FActiveDecoratorSession := nil;
end;

destructor TGocciaVM.Destroy;
var
  I: Integer;
begin
  if Assigned(FActiveDecoratorSession) then
  begin
    TGarbageCollector.Instance.RemoveTempRoot(
      TGocciaVMDecoratorSession(FActiveDecoratorSession).MetadataObject);
    FActiveDecoratorSession.Free;
  end;
  for I := 0 to FArgumentPoolCount - 1 do
    FArgumentPool[I].Free;
  SetLength(FArgumentPool, 0);
  FHandlerStack.Free;
  SetExceptionMask(FPreviousExceptionMask);
  inherited;
end;

destructor TGocciaBytecodeFunctionValue.Destroy;
begin
  FClosure.Free;
  inherited;
end;

function TGocciaVMClassValue.Instantiate(
  const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  ConstructorToCall: TGocciaMethodValue;
begin
  NativeInstance := nil;
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    NativeInstance := WalkClass.CreateNativeInstance(AArguments);
    if Assigned(NativeInstance) then
      Break;
    WalkClass := WalkClass.SuperClass;
  end;

  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    Instance.Prototype := Prototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := Self;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(Self);
    Instance.Prototype := Prototype;
  end;

  if Assigned(FConstructorValue) then
  begin
    TGarbageCollector.Instance.AddTempRoot(Instance);
    try
      FVM.RunClassInitializers(Self, Instance);
      FVM.InvokeFunctionValue(FConstructorValue, AArguments, Instance);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Instance);
    end;
  end
  else
  begin
    TGarbageCollector.Instance.AddTempRoot(Instance);
    try
      FVM.RunClassInitializers(Self, Instance);

      ConstructorToCall := nil;
      if (SuperClass is TGocciaVMClassValue) and
         Assigned(TGocciaVMClassValue(SuperClass).FConstructorValue) then
      begin
        TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
          SuperClass, Instance);
        TGocciaVMClassValue(SuperClass).FVM.InvokeFunctionValue(
          TGocciaVMClassValue(SuperClass).FConstructorValue,
          AArguments, Instance);
      end
      else
      begin
        if Assigned(SuperClass) then
          ConstructorToCall := SuperClass.ConstructorMethod;

        if Assigned(ConstructorToCall) then
        begin
          FVM.RunClassInitializers(SuperClass, Instance);
          ConstructorToCall.Call(AArguments, Instance);
        end
        else if Assigned(SuperClass) then
          FVM.InvokeImplicitSuperInitialization(SuperClass, Instance, AArguments)
        else if Instance is TGocciaInstanceValue then
          TGocciaInstanceValue(Instance).InitializeNativeFromArguments(AArguments);
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Instance);
    end;
  end;

  Result := Instance;
end;

procedure TGocciaVMClassValue.SetVMConstructor(const AValue: TGocciaValue);
begin
  FConstructorValue := AValue;
end;

procedure TGocciaVMClassValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FConstructorValue) then
    FConstructorValue.MarkReferences;
end;

procedure TGocciaVMSuperConstructorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;
end;

procedure TGocciaBytecodeFunctionValue.MarkReferences;
var
  I: Integer;
  Upvalue: TGocciaBytecodeUpvalue;
begin
  if GCMarked then Exit;
  inherited;

  if not Assigned(FClosure) then
    Exit;

  for I := 0 to FClosure.UpvalueCount - 1 do
  begin
    Upvalue := FClosure.GetUpvalue(I);
    if Assigned(Upvalue) and Assigned(Upvalue.Cell) and
       Assigned(Upvalue.Cell.Value) then
      Upvalue.Cell.Value.MarkReferences;
  end;
end;

function TGocciaVM.ConstantToValue(const AConstant: TGocciaBytecodeConstant): TGocciaValue;
begin
  case AConstant.Kind of
    bckNil:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    bckTrue:
      Result := TGocciaBooleanLiteralValue.TrueValue;
    bckFalse:
      Result := TGocciaBooleanLiteralValue.FalseValue;
    bckInteger:
      if (AConstant.IntValue >= 0) and (AConstant.IntValue <= 255) then
        Result := TGocciaNumberLiteralValue.SmallInt(AConstant.IntValue)
      else
        Result := TGocciaNumberLiteralValue.Create(AConstant.IntValue);
    bckFloat:
      Result := TGocciaNumberLiteralValue.Create(AConstant.FloatValue);
    bckString:
      Result := TGocciaStringLiteralValue.Create(AConstant.StringValue);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaVM.AcquireArguments(
  const ACapacity: Integer): TGocciaArgumentsCollection;
begin
  if FArgumentPoolCount > 0 then
  begin
    Dec(FArgumentPoolCount);
    Result := FArgumentPool[FArgumentPoolCount];
    FArgumentPool[FArgumentPoolCount] := nil;
    Result.Clear;
    Result.EnsureCapacity(ACapacity);
    Exit;
  end;

  if ACapacity > 0 then
    Result := TGocciaArgumentsCollection.CreateWithCapacity(ACapacity)
  else
    Result := TGocciaArgumentsCollection.Create;
end;

procedure TGocciaVM.ReleaseArguments(
  const AArguments: TGocciaArgumentsCollection);
begin
  if not Assigned(AArguments) then
    Exit;
  AArguments.Clear;
  if FArgumentPoolCount < 32 then
  begin
    if Length(FArgumentPool) <= FArgumentPoolCount then
      SetLength(FArgumentPool, FArgumentPoolCount + 8);
    FArgumentPool[FArgumentPoolCount] := AArguments;
    Inc(FArgumentPoolCount);
  end
  else
    AArguments.Free;
end;

function TGocciaVM.AcquireRegisters(const ACount: Integer): TGocciaRegisterArray;
begin
  SetLength(Result, ACount);
end;

procedure TGocciaVM.ReleaseRegisters(var ARegisters: TGocciaRegisterArray);
begin
  ARegisters := nil;
end;

function TGocciaVM.AcquireLocalCells(
  const ACount: Integer): TGocciaBytecodeCellArray;
begin
  SetLength(Result, ACount);
end;

procedure TGocciaVM.ReleaseLocalCells(var ALocalCells: TGocciaBytecodeCellArray);
begin
  ALocalCells := nil;
end;

procedure TGocciaVM.EnsureRegisterCapacity(const ACount: Integer);
begin
  if Length(FRegisters) < ACount then
    SetLength(FRegisters, ACount);
end;

procedure TGocciaVM.EnsureLocalCapacity(const ACount: Integer);
begin
  if Length(FLocalCells) < ACount then
    SetLength(FLocalCells, ACount);
end;

function TGocciaVM.GetLocalCell(const AIndex: Integer): TGocciaBytecodeCell;
begin
  EnsureLocalCapacity(AIndex + 1);
  if not Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex] := TGocciaBytecodeCell.Create(GetRegister(AIndex));
  Result := FLocalCells[AIndex];
end;

function TGocciaVM.GetRegister(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < Length(FRegisters)) and Assigned(FRegisters[AIndex]) then
    Result := FRegisters[AIndex]
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.GetRegisterFast(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex < 0) or (AIndex >= Length(FRegisters)) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := FRegisters[AIndex];
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaVM.SetRegister(const AIndex: Integer; const AValue: TGocciaValue);
begin
  EnsureRegisterCapacity(AIndex + 1);
  FRegisters[AIndex] := AValue;
  if (AIndex >= 0) and (AIndex < Length(FLocalCells)) and
     Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := AValue;
end;

procedure TGocciaVM.SetRegisterFast(const AIndex: Integer;
  const AValue: TGocciaValue);
begin
  if (AIndex < 0) or (AIndex >= Length(FRegisters)) then
  begin
    SetRegister(AIndex, AValue);
    Exit;
  end;
  FRegisters[AIndex] := AValue;
  if (AIndex < Length(FLocalCells)) and Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := AValue;
end;

function TGocciaVM.GetLocal(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < Length(FLocalCells)) and Assigned(FLocalCells[AIndex]) then
    Exit(FLocalCells[AIndex].Value);
  Result := GetRegister(AIndex);
end;

function TGocciaVM.GetLocalFast(const AIndex: Integer): TGocciaValue;
begin
  if AIndex < 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if (AIndex < Length(FLocalCells)) and Assigned(FLocalCells[AIndex]) then
    Exit(FLocalCells[AIndex].Value);
  Result := GetRegisterFast(AIndex);
end;

procedure TGocciaVM.SetLocal(const AIndex: Integer; const AValue: TGocciaValue);
begin
  SetRegister(AIndex, AValue);
end;

procedure TGocciaVM.SetLocalFast(const AIndex: Integer; const AValue: TGocciaValue);
begin
  if AIndex < 0 then
    Exit;
  SetRegisterFast(AIndex, AValue);
end;

function TGocciaVM.MatchesNullishKind(const AValue: TGocciaValue;
  const AKind: UInt8): Boolean;
begin
  case AKind of
    GOCCIA_NULLISH_MATCH_ANY:
      Result := (AValue is TGocciaUndefinedLiteralValue) or
                (AValue is TGocciaNullLiteralValue) or
                (AValue = TGocciaHoleValue.HoleValue);
    GOCCIA_NULLISH_MATCH_UNDEFINED:
      Result := AValue is TGocciaUndefinedLiteralValue;
    GOCCIA_NULLISH_MATCH_NULL:
      Result := AValue is TGocciaNullLiteralValue;
    GOCCIA_NULLISH_MATCH_HOLE:
      Result := AValue = TGocciaHoleValue.HoleValue;
  else
    Result := False;
  end;
end;

function TGocciaVM.TryGetArrayIndex(const AKey: TGocciaValue; out AIndex: Integer): Boolean;
var
  NumberValue: TGocciaNumberLiteralValue;
begin
  Result := False;
  AIndex := -1;
  if not Assigned(AKey) or not (AKey is TGocciaNumberLiteralValue) then
    Exit;

  NumberValue := AKey.ToNumberLiteral;
  if Frac(NumberValue.Value) <> 0.0 then
    Exit;

  AIndex := Trunc(NumberValue.Value);
  Result := AIndex >= 0;
end;

function TGocciaVM.KeyToPropertyName(const AKey: TGocciaValue): string;
begin
  if not Assigned(AKey) then
    Exit('');
  Result := AKey.ToStringLiteral.Value;
end;

function TGocciaVM.IterableToArray(const AIterable: TGocciaValue;
  const ATryAsync: Boolean): TGocciaArrayValue;
var
  IteratorValue: TGocciaValue;
  DoneFlag: Boolean;
  NextMethod: TGocciaValue;
  NextResult: TGocciaValue;
  DoneValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  Result := TGocciaArrayValue.Create;
  IteratorValue := GetIteratorValue(AIterable, ATryAsync);
  if IteratorValue is TGocciaIteratorValue then
  begin
    repeat
      NextResult := TGocciaIteratorValue(IteratorValue).DirectNext(DoneFlag);
      if not DoneFlag then
        Result.Elements.Add(NextResult);
    until DoneFlag;
    Exit;
  end;

  if IteratorValue is TGocciaObjectValue then
  begin
    repeat
      NextMethod := IteratorValue.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or
         (NextMethod is TGocciaUndefinedLiteralValue) or
         not NextMethod.IsCallable then
        Break;
      CallArgs := AcquireArguments;
      try
        NextResult := TGocciaFunctionBase(NextMethod).Call(CallArgs, IteratorValue);
      finally
        ReleaseArguments(CallArgs);
      end;
      NextResult := AwaitValue(NextResult);
      if NextResult.IsPrimitive then
        ThrowTypeError('Iterator result ' + NextResult.ToStringLiteral.Value +
          ' is not an object');
      DoneValue := NextResult.GetProperty(PROP_DONE);
      DoneFlag := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      if not DoneFlag then
        Result.Elements.Add(NextResult.GetProperty(PROP_VALUE));
    until DoneFlag;
    Exit;
  end;
end;

procedure TGocciaVM.SpreadObjectIntoValue(const ATarget: TGocciaObjectValue;
  const ASource: TGocciaValue);
var
  SourceObject: TGocciaObjectValue;
  Key: string;
  I: Integer;
  SymbolPair: TPair<TGocciaSymbolValue, TGocciaValue>;
begin
  if not Assigned(ATarget) then
    Exit;
  if (ASource is TGocciaNullLiteralValue) or
     (ASource is TGocciaUndefinedLiteralValue) then
    Exit;

  if ASource is TGocciaObjectValue then
  begin
    SourceObject := TGocciaObjectValue(ASource);
    for Key in SourceObject.GetEnumerablePropertyNames do
      ATarget.SetProperty(Key, SourceObject.GetProperty(Key));
    for SymbolPair in SourceObject.GetEnumerableSymbolProperties do
      ATarget.AssignSymbolProperty(SymbolPair.Key, SymbolPair.Value);
    Exit;
  end;

  if ASource is TGocciaArrayValue then
  begin
    for I := 0 to TGocciaArrayValue(ASource).Elements.Count - 1 do
      if TGocciaArrayValue(ASource).Elements[I] <> TGocciaHoleValue.HoleValue then
        ATarget.SetProperty(IntToStr(I), TGocciaArrayValue(ASource).Elements[I]);
    Exit;
  end;

  if ASource is TGocciaStringLiteralValue then
  begin
    for I := 1 to Length(TGocciaStringLiteralValue(ASource).Value) do
      ATarget.SetProperty(IntToStr(I - 1),
        TGocciaStringLiteralValue.Create(TGocciaStringLiteralValue(ASource).Value[I]));
  end;
end;

function TGocciaVM.ObjectRestValue(const ASource: TGocciaValue;
  const AExclusionKeys: TGocciaArrayValue): TGocciaObjectValue;
var
  SourceObject: TGocciaObjectValue;
  Key: string;
  I, J: Integer;
  Excluded: Boolean;
begin
  Result := TGocciaObjectValue.Create;
  if not (ASource is TGocciaObjectValue) then
    Exit;

  SourceObject := TGocciaObjectValue(ASource);
  for Key in SourceObject.GetOwnPropertyNames do
  begin
    Excluded := False;
    if Assigned(AExclusionKeys) then
      for J := 0 to AExclusionKeys.Elements.Count - 1 do
        if AExclusionKeys.GetElement(J).ToStringLiteral.Value = Key then
        begin
          Excluded := True;
          Break;
        end;
    if not Excluded then
      Result.SetProperty(Key, SourceObject.GetProperty(Key));
  end;

  for I := 0 to Length(SourceObject.GetOwnSymbols) - 1 do
  begin
    Excluded := False;
    if Assigned(AExclusionKeys) then
      for J := 0 to AExclusionKeys.Elements.Count - 1 do
        if (AExclusionKeys.GetElement(J) is TGocciaSymbolValue) and
           (AExclusionKeys.GetElement(J) = SourceObject.GetOwnSymbols[I]) then
        begin
          Excluded := True;
          Break;
        end;
    if not Excluded then
      Result.AssignSymbolProperty(SourceObject.GetOwnSymbols[I],
        SourceObject.GetSymbolProperty(SourceObject.GetOwnSymbols[I]));
  end;
end;

function TGocciaVM.GetIteratorValue(const AIterable: TGocciaValue;
  const ATryAsync: Boolean): TGocciaValue;
var
  IteratorMethod: TGocciaValue;
  IteratorObject: TGocciaValue;
  NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if ATryAsync and (AIterable is TGocciaObjectValue) then
  begin
    IteratorMethod := TGocciaObjectValue(AIterable).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownAsyncIterator);
    if Assigned(IteratorMethod) and
       not (IteratorMethod is TGocciaUndefinedLiteralValue) and
       IteratorMethod.IsCallable then
    begin
      CallArgs := AcquireArguments;
      try
        Result := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AIterable);
      finally
        ReleaseArguments(CallArgs);
      end;
      if Assigned(Result) then
        Exit;
    end;
  end;

  if AIterable is TGocciaIteratorValue then
    Exit(AIterable);

  if AIterable is TGocciaArrayValue then
    Exit(TGocciaArrayIteratorValue.Create(AIterable, akValues));

  if AIterable is TGocciaStringLiteralValue then
    Exit(TGocciaStringIteratorValue.Create(AIterable));

  if AIterable is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(AIterable).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and
       not (IteratorMethod is TGocciaUndefinedLiteralValue) and
       IteratorMethod.IsCallable then
    begin
      CallArgs := AcquireArguments;
      try
        IteratorObject := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AIterable);
      finally
        ReleaseArguments(CallArgs);
      end;

      if IteratorObject is TGocciaIteratorValue then
        Exit(IteratorObject);

      if IteratorObject is TGocciaObjectValue then
      begin
        NextMethod := IteratorObject.GetProperty(PROP_NEXT);
        if Assigned(NextMethod) and
           not (NextMethod is TGocciaUndefinedLiteralValue) and
           NextMethod.IsCallable then
          Exit(IteratorObject);
      end;
    end;
  end;

  ThrowTypeError(AIterable.TypeName + ' is not iterable');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.ConstructValue(const AConstructor: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  Context: TGocciaEvaluationContext;
  ConstructorName: string;
begin
  if AConstructor is TGocciaVMClassValue then
  begin
    Result := TGocciaVMClassValue(AConstructor).Instantiate(AArguments);
    Exit;
  end;

  if AConstructor is TGocciaClassValue then
  begin
    FillChar(Context, SizeOf(Context), 0);
    Context.Scope := FGlobalScope;
    Result := InstantiateClass(TGocciaClassValue(AConstructor), AArguments, Context);
    Exit;
  end;

  if AConstructor is TGocciaNativeFunctionValue then
  begin
    ConstructorName := TGocciaNativeFunctionValue(AConstructor).Name;
    if Assigned(TGocciaCallStack.Instance) then
      TGocciaCallStack.Instance.Push(ConstructorName, '', 0, 0);
    try
      Result := TGocciaNativeFunctionValue(AConstructor).Call(
        AArguments, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
    Exit;
  end;

  if AConstructor is TGocciaBytecodeFunctionValue then
  begin
    if TGocciaBytecodeFunctionValue(AConstructor).FClosure.Template.IsArrow then
      ThrowTypeError(TGocciaBytecodeFunctionValue(AConstructor).GetProperty(PROP_NAME)
        .ToStringLiteral.Value + ' is not a constructor');
  end;

  if AConstructor is TGocciaFunctionBase then
  begin
    ConstructorName := TGocciaFunctionBase(AConstructor).GetProperty(PROP_NAME)
      .ToStringLiteral.Value;
    if Assigned(TGocciaCallStack.Instance) then
      TGocciaCallStack.Instance.Push(ConstructorName, '', 0, 0);
    try
      Result := TGocciaFunctionBase(AConstructor).Call(
        AArguments, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
    Exit;
  end;

  ThrowTypeError(AConstructor.TypeName + ' is not a constructor');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.ImportModuleValue(const APath: string): TGocciaValue;
var
  Module: TGocciaModule;
  ExportPair: TGocciaValueMap.TKeyValuePair;
  NamespaceObject: TGocciaObjectValue;
begin
  if not Assigned(FInterpreter) then
    ThrowTypeError('Module loading is not available in TGocciaVM');

  Module := FInterpreter.LoadModule(APath, FCurrentModuleSourcePath);
  if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    TGocciaObjectValue.InitializeSharedPrototype;
  NamespaceObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  for ExportPair in Module.ExportsTable do
    NamespaceObject.SetProperty(ExportPair.Key, ExportPair.Value);
  Result := NamespaceObject;
end;

procedure TGocciaVM.ExportBindingValue(const AName: string; const AValue: TGocciaValue);
begin
  if not Assigned(FCurrentModuleExports) then
    FCurrentModuleExports := TGocciaValueMap.Create;
  FCurrentModuleExports.AddOrSetValue(AName, AValue);
end;

procedure TGocciaVM.DefineGetterProperty(const ATarget: TGocciaValue;
  const AName: string; const AGetter: TGocciaValue);
var
  TargetObject: TGocciaObjectValue;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  if ATarget is TGocciaVMClassValue then
    TargetObject := TGocciaVMClassValue(ATarget).Prototype
  else if ATarget is TGocciaObjectValue then
    TargetObject := TGocciaObjectValue(ATarget)
  else
    Exit;

  ExistingDescriptor := TargetObject.GetOwnPropertyDescriptor(AName);
  ExistingSetter := nil;
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
    ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
  TargetObject.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(
    AGetter, ExistingSetter, [pfEnumerable, pfConfigurable, pfWritable]));
end;

procedure TGocciaVM.DefineSetterProperty(const ATarget: TGocciaValue;
  const AName: string; const ASetter: TGocciaValue);
var
  TargetObject: TGocciaObjectValue;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  if ATarget is TGocciaVMClassValue then
    TargetObject := TGocciaVMClassValue(ATarget).Prototype
  else if ATarget is TGocciaObjectValue then
    TargetObject := TGocciaObjectValue(ATarget)
  else
    Exit;

  ExistingDescriptor := TargetObject.GetOwnPropertyDescriptor(AName);
  ExistingGetter := nil;
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
    ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
  TargetObject.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(
    ExistingGetter, ASetter, [pfEnumerable, pfConfigurable, pfWritable]));
end;

procedure TGocciaVM.DefineStaticGetterProperty(const ATarget: TGocciaValue;
  const AName: string; const AGetter: TGocciaValue);
begin
  if ATarget is TGocciaClassValue then
    TGocciaClassValue(ATarget).AddStaticGetter(AName, TGocciaFunctionBase(AGetter));
end;

procedure TGocciaVM.DefineStaticSetterProperty(const ATarget: TGocciaValue;
  const AName: string; const ASetter: TGocciaValue);
begin
  if ATarget is TGocciaClassValue then
    TGocciaClassValue(ATarget).AddStaticSetter(AName, TGocciaFunctionBase(ASetter));
end;

procedure TGocciaVM.DefineGetterPropertyByKey(const ATarget, AKey,
  AGetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  if AKey is TGocciaSymbolValue then
  begin
    if ATarget is TGocciaVMClassValue then
    begin
      ExistingDescriptor := TGocciaVMClassValue(ATarget).Prototype
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingSetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
        ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
      TGocciaVMClassValue(ATarget).Prototype.DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ExistingSetter, [pfEnumerable, pfConfigurable, pfWritable]));
    end
    else if ATarget is TGocciaObjectValue then
    begin
      ExistingDescriptor := TGocciaObjectValue(ATarget)
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingSetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
        ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
      TGocciaObjectValue(ATarget).DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ExistingSetter, [pfEnumerable, pfConfigurable, pfWritable]));
    end;
    Exit;
  end;

  DefineGetterProperty(ATarget, AKey.ToStringLiteral.Value, AGetter);
end;

procedure TGocciaVM.DefineSetterPropertyByKey(const ATarget, AKey,
  ASetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  if AKey is TGocciaSymbolValue then
  begin
    if ATarget is TGocciaVMClassValue then
    begin
      ExistingDescriptor := TGocciaVMClassValue(ATarget).Prototype
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingGetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
        ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
      TGocciaVMClassValue(ATarget).Prototype.DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          ExistingGetter, ASetter, [pfEnumerable, pfConfigurable, pfWritable]));
    end
    else if ATarget is TGocciaObjectValue then
    begin
      ExistingDescriptor := TGocciaObjectValue(ATarget)
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingGetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
        ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
      TGocciaObjectValue(ATarget).DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          ExistingGetter, ASetter, [pfEnumerable, pfConfigurable, pfWritable]));
    end;
    Exit;
  end;

  DefineSetterProperty(ATarget, AKey.ToStringLiteral.Value, ASetter);
end;

procedure TGocciaVM.DefineStaticGetterPropertyByKey(const ATarget, AKey,
  AGetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  if (ATarget is TGocciaClassValue) and (AKey is TGocciaSymbolValue) then
  begin
    ExistingDescriptor := TGocciaClassValue(ATarget)
      .GetOwnStaticSymbolDescriptor(TGocciaSymbolValue(AKey));
    ExistingSetter := nil;
    if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
      ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
    TGocciaClassValue(ATarget).DefineSymbolProperty(
      TGocciaSymbolValue(AKey),
      TGocciaPropertyDescriptorAccessor.Create(
        AGetter, ExistingSetter, [pfEnumerable, pfConfigurable, pfWritable]));
    Exit;
  end;

  DefineStaticGetterProperty(ATarget, AKey.ToStringLiteral.Value, AGetter);
end;

procedure TGocciaVM.DefineStaticSetterPropertyByKey(const ATarget, AKey,
  ASetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  if (ATarget is TGocciaClassValue) and (AKey is TGocciaSymbolValue) then
  begin
    ExistingDescriptor := TGocciaClassValue(ATarget)
      .GetOwnStaticSymbolDescriptor(TGocciaSymbolValue(AKey));
    ExistingGetter := nil;
    if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
      ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
    TGocciaClassValue(ATarget).DefineSymbolProperty(
      TGocciaSymbolValue(AKey),
      TGocciaPropertyDescriptorAccessor.Create(
        ExistingGetter, ASetter, [pfEnumerable, pfConfigurable, pfWritable]));
    Exit;
  end;

  DefineStaticSetterProperty(ATarget, AKey.ToStringLiteral.Value, ASetter);
end;

procedure TGocciaVM.DefineGlobalValue(const AName: string; const AValue: TGocciaValue);
begin
  if not Assigned(FGlobalScope) then
    Exit;

  if FGlobalScope.Contains(AName) then
    FGlobalScope.AssignLexicalBinding(AName, AValue)
  else
    FGlobalScope.DefineLexicalBinding(AName, AValue, dtLet);
end;

function TGocciaVM.FinalizeEnumValue(const AValue: TGocciaValue;
  const AName: string): TGocciaValue;
var
  EnumObj: TGocciaEnumValue;
  Entries: TGocciaArrayValue;
  PairArr: TGocciaArrayValue;
  Names: TArray<string>;
  I: Integer;
  Key: string;
  MemberValue: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
    Exit(AValue);

  EnumObj := TGocciaEnumValue.Create(AName);
  Entries := TGocciaArrayValue.Create;
  EnumObj.Entries := Entries;

  Names := TGocciaObjectValue(AValue).GetOwnPropertyNames;
  for I := 0 to High(Names) do
  begin
    Key := Names[I];
    MemberValue := TGocciaObjectValue(AValue).GetProperty(Key);

    if not (MemberValue is TGocciaNumberLiteralValue) and
       not (MemberValue is TGocciaStringLiteralValue) and
       not (MemberValue is TGocciaSymbolValue) then
      ThrowTypeError('Enum member ''' + Key +
        ''' must be a number, string, or symbol');

    EnumObj.DefineProperty(Key,
      TGocciaPropertyDescriptorData.Create(MemberValue, [pfEnumerable]));

    PairArr := TGocciaArrayValue.Create;
    PairArr.Elements.Add(TGocciaStringLiteralValue.Create(Key));
    PairArr.Elements.Add(MemberValue);
    Entries.Elements.Add(PairArr);
  end;

  EnumObj.PreventExtensions;
  InitializeEnumSymbols(EnumObj);
  Result := EnumObj;
end;

procedure TGocciaVM.RunClassInitializers(const AClassValue: TGocciaClassValue;
  const AInstance: TGocciaValue);
begin
  AClassValue.RunMethodInitializers(AInstance);
  AClassValue.RunFieldInitializers(AInstance);
  AClassValue.RunDecoratorFieldInitializers(AInstance);
end;

procedure TGocciaVM.InvokeImplicitSuperInitialization(
  const AClassValue: TGocciaClassValue; const AInstance: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection);
begin
  if not Assigned(AClassValue) then
    Exit;

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).FConstructorValue) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    TGocciaVMClassValue(AClassValue).FVM.InvokeFunctionValue(
      TGocciaVMClassValue(AClassValue).FConstructorValue,
      AArguments, AInstance);
    Exit;
  end;

  if Assigned(AClassValue.ConstructorMethod) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    AClassValue.ConstructorMethod.Call(AArguments, AInstance);
    Exit;
  end;

  InvokeImplicitSuperInitialization(AClassValue.SuperClass, AInstance, AArguments);
  if not (AClassValue is TGocciaVMClassValue) and
     (AInstance is TGocciaInstanceValue) then
    TGocciaInstanceValue(AInstance).InitializeNativeFromArguments(AArguments);
  RunClassInitializers(AClassValue, AInstance);
end;

procedure TGocciaVM.SetupAutoAccessorValue(const AName: string);
var
  ClassVal: TGocciaClassValue;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit;
  if not (TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue is TGocciaClassValue) then
    Exit;

  ClassVal := TGocciaClassValue(
    TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue);
  ClassVal.AddAutoAccessor(AName, '__accessor_' + AName, False);
end;

procedure TGocciaVM.BeginDecorators(const AClassValue, ASuperValue: TGocciaValue);
var
  ClassVal: TGocciaClassValue;
  SuperMetadata: TGocciaValue;
  Meta: TGocciaObjectValue;
begin
  if not (AClassValue is TGocciaClassValue) then
    Exit;

  ClassVal := TGocciaClassValue(AClassValue);
  SuperMetadata := nil;

  if Assigned(ClassVal.SuperClass) then
    SuperMetadata := ClassVal.SuperClass.GetSymbolProperty(
      TGocciaSymbolValue.WellKnownMetadata)
  else if ASuperValue is TGocciaClassValue then
    SuperMetadata := TGocciaClassValue(ASuperValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownMetadata);

  if (SuperMetadata <> nil) and (SuperMetadata is TGocciaObjectValue) then
    Meta := TGocciaObjectValue.Create(TGocciaObjectValue(SuperMetadata))
  else
    Meta := TGocciaObjectValue.Create;

  TGarbageCollector.Instance.AddTempRoot(Meta);

  if Assigned(FActiveDecoratorSession) then
  begin
    TGarbageCollector.Instance.RemoveTempRoot(
      TGocciaVMDecoratorSession(FActiveDecoratorSession).MetadataObject);
    FActiveDecoratorSession.Free;
  end;

  FActiveDecoratorSession := TGocciaVMDecoratorSession.Create(Meta);
  TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue := ClassVal;
end;

procedure TGocciaVM.ApplyClassDecorator(const ADecoratorFn: TGocciaValue);
var
  Session: TGocciaVMDecoratorSession;
  ClassVal, DecoratorResult: TGocciaValue;
  ContextObject: TGocciaObjectValue;
  DecoratorArgs: TGocciaArgumentsCollection;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit;

  Session := TGocciaVMDecoratorSession(FActiveDecoratorSession);
  ClassVal := Session.ClassValue;
  if not (ClassVal is TGocciaClassValue) then
    Exit;
  if not ADecoratorFn.IsCallable then
    ThrowTypeError('Decorator must be a function');

  ContextObject := TGocciaObjectValue.Create;
  ContextObject.AssignProperty(PROP_KIND,
    TGocciaStringLiteralValue.Create('class'));
  ContextObject.AssignProperty(PROP_NAME,
    TGocciaStringLiteralValue.Create(TGocciaClassValue(ClassVal).Name));
  ContextObject.AssignProperty(PROP_METADATA, Session.MetadataObject);
  ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Session.ClassCollector.AddInitializer, PROP_ADD_INITIALIZER, 1));

  DecoratorArgs := AcquireArguments(2);
  DecoratorArgs.Add(ClassVal);
  DecoratorArgs.Add(ContextObject);
  try
    DecoratorResult := TGocciaFunctionBase(ADecoratorFn).Call(
      DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    ReleaseArguments(DecoratorArgs);
  end;

  if (DecoratorResult <> nil) and
     not (DecoratorResult is TGocciaUndefinedLiteralValue) then
  begin
    if not (DecoratorResult is TGocciaClassValue) then
      ThrowTypeError('Class decorator must return a class or undefined');
    Session.ClassValue := DecoratorResult;
  end;
end;

procedure TGocciaVM.ApplyElementDecorator(const ADecoratorFn: TGocciaValue;
  const ADescriptor: string);
var
  Session: TGocciaVMDecoratorSession;
  Kind: Char;
  Name: string;
  Flags: Integer;
  IsStatic, IsPrivate: Boolean;
  ClassVal, DecoratorResult, ElementValue: TGocciaValue;
  ContextObject, AccessObject, AutoAccessorValue, DecResultObj: TGocciaObjectValue;
  AccessGetterHelper: TGocciaAccessGetter;
  AccessSetterHelper: TGocciaAccessSetter;
  Collector: TGocciaInitializerCollector;
  DecoratorArgs: TGocciaArgumentsCollection;
  KindStr: string;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  GetterValue, SetterValue, NewGetter, NewSetter, NewInit: TGocciaValue;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit;

  Session := TGocciaVMDecoratorSession(FActiveDecoratorSession);
  ClassVal := Session.ClassValue;
  if not (ClassVal is TGocciaClassValue) then
    Exit;
  if not ADecoratorFn.IsCallable then
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
  ContextObject.AssignProperty(PROP_METADATA, Session.MetadataObject);

  AccessObject := TGocciaObjectValue.Create;
  case Kind of
    'm':
      begin
        if IsPrivate then
          ElementValue := TGocciaClassValue(ClassVal).GetPrivateMethod(Name)
        else if IsStatic then
          ElementValue := TGocciaClassValue(ClassVal).GetProperty(Name)
        else
          ElementValue := TGocciaClassValue(ClassVal).Prototype.GetProperty(Name);
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
    Collector := Session.StaticFieldCollector
  else if Kind in ['f', 'a'] then
    Collector := Session.FieldCollector
  else
    Collector := Session.MethodCollector;
  ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Collector.AddInitializer, PROP_ADD_INITIALIZER, 1));

  case Kind of
    'm':
      ; // already set above
    'g':
      begin
        if IsPrivate then
          ElementValue := TGocciaClassValue(ClassVal).PrivatePropertyGetter[Name]
        else if IsStatic then
          ElementValue := TGocciaClassValue(ClassVal).StaticPropertyGetter[Name]
        else
          ElementValue := TGocciaClassValue(ClassVal).PropertyGetter[Name];
      end;
    's':
      begin
        if IsPrivate then
          ElementValue := TGocciaClassValue(ClassVal).PrivatePropertySetter[Name]
        else if IsStatic then
          ElementValue := TGocciaClassValue(ClassVal).StaticPropertySetter[Name]
        else
          ElementValue := TGocciaClassValue(ClassVal).PropertySetter[Name];
      end;
    'f':
      ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    'a':
      begin
        AutoAccessorValue := TGocciaObjectValue.Create;
        if IsStatic then
        begin
          AutoAccessorValue.AssignProperty(PROP_GET,
            TGocciaClassValue(ClassVal).StaticPropertyGetter[Name]);
          AutoAccessorValue.AssignProperty(PROP_SET,
            TGocciaClassValue(ClassVal).StaticPropertySetter[Name]);
        end
        else
        begin
          AutoAccessorValue.AssignProperty(PROP_GET,
            TGocciaClassValue(ClassVal).PropertyGetter[Name]);
          AutoAccessorValue.AssignProperty(PROP_SET,
            TGocciaClassValue(ClassVal).PropertySetter[Name]);
        end;
        ElementValue := AutoAccessorValue;
      end;
  end;

  DecoratorArgs := AcquireArguments(2);
  DecoratorArgs.Add(ElementValue);
  DecoratorArgs.Add(ContextObject);
  try
    DecoratorResult := TGocciaFunctionBase(ADecoratorFn).Call(
      DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    ReleaseArguments(DecoratorArgs);
  end;

  if (DecoratorResult = nil) or
     (DecoratorResult is TGocciaUndefinedLiteralValue) then
    Exit;

  case Kind of
    'm':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Method decorator must return a function or undefined');
        if IsPrivate then
          TGocciaClassValue(ClassVal).Prototype.AssignProperty(
            '#' + Name, DecoratorResult)
        else if IsStatic then
          TGocciaClassValue(ClassVal).SetProperty(Name, DecoratorResult)
        else
          TGocciaClassValue(ClassVal).Prototype.AssignProperty(
            Name, DecoratorResult);
      end;
    'g':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Getter decorator must return a function or undefined');
        if IsStatic then
          TGocciaClassValue(ClassVal).AddStaticGetter(
            Name, TGocciaFunctionValue(DecoratorResult))
        else
        begin
          ExistingDescriptor := TGocciaClassValue(ClassVal).Prototype
            .GetOwnPropertyDescriptor(Name);
          SetterValue := nil;
          if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
             Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
            SetterValue := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
          TGocciaClassValue(ClassVal).Prototype.DefineProperty(Name,
            TGocciaPropertyDescriptorAccessor.Create(
              DecoratorResult, SetterValue, [pfEnumerable, pfConfigurable, pfWritable]));
        end;
      end;
    's':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Setter decorator must return a function or undefined');
        if IsStatic then
          TGocciaClassValue(ClassVal).AddStaticSetter(
            Name, TGocciaFunctionValue(DecoratorResult))
        else
        begin
          ExistingDescriptor := TGocciaClassValue(ClassVal).Prototype
            .GetOwnPropertyDescriptor(Name);
          GetterValue := nil;
          if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
             Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
            GetterValue := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
          TGocciaClassValue(ClassVal).Prototype.DefineProperty(Name,
            TGocciaPropertyDescriptorAccessor.Create(
              GetterValue, DecoratorResult, [pfEnumerable, pfConfigurable, pfWritable]));
        end;
      end;
    'f':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError('Field decorator must return a function or undefined');
        TGocciaClassValue(ClassVal).AddFieldInitializer(
          Name, DecoratorResult, IsPrivate, IsStatic);
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
        begin
          if IsStatic then
            TGocciaClassValue(ClassVal).AddStaticGetter(
              Name, TGocciaFunctionBase(NewGetter))
          else
            TGocciaClassValue(ClassVal).AddGetter(
              Name, TGocciaFunctionBase(NewGetter));
        end;

        if Assigned(NewSetter) and not (NewSetter is TGocciaUndefinedLiteralValue) then
        begin
          if IsStatic then
            TGocciaClassValue(ClassVal).AddStaticSetter(
              Name, TGocciaFunctionBase(NewSetter))
          else
            TGocciaClassValue(ClassVal).AddSetter(
              Name, TGocciaFunctionBase(NewSetter));
        end;

        if Assigned(NewInit) and not (NewInit is TGocciaUndefinedLiteralValue) and
           NewInit.IsCallable then
          TGocciaClassValue(ClassVal).AddFieldInitializer(
            Name, NewInit, IsPrivate, IsStatic);
      end;
  end;
end;

function TGocciaVM.FinishDecorators(const ACurrentValue: TGocciaValue): TGocciaValue;
var
  Session: TGocciaVMDecoratorSession;
  ClassVal: TGocciaClassValue;
  InitializerResults: TArray<TGocciaValue>;
  I: Integer;
  InitArgs: TGocciaArgumentsCollection;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit(ACurrentValue);

  Session := TGocciaVMDecoratorSession(FActiveDecoratorSession);
  if not (Session.ClassValue is TGocciaClassValue) then
  begin
    TGarbageCollector.Instance.RemoveTempRoot(Session.MetadataObject);
    FActiveDecoratorSession.Free;
    FActiveDecoratorSession := nil;
    Exit(ACurrentValue);
  end;

  ClassVal := TGocciaClassValue(Session.ClassValue);
  ClassVal.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownMetadata,
    TGocciaPropertyDescriptorData.Create(
      Session.MetadataObject, [pfConfigurable]));

  InitializerResults := Session.MethodCollector.GetInitializers;
  ClassVal.AppendMethodInitializers(InitializerResults);
  InitializerResults := Session.FieldCollector.GetInitializers;
  ClassVal.AppendFieldInitializers(InitializerResults);

  InitializerResults := Session.ClassCollector.GetInitializers;
  for I := 0 to High(InitializerResults) do
  begin
    InitArgs := AcquireArguments;
    try
      TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassVal);
    finally
      ReleaseArguments(InitArgs);
    end;
  end;

  InitializerResults := Session.StaticFieldCollector.GetInitializers;
  for I := 0 to High(InitializerResults) do
  begin
    InitArgs := AcquireArguments;
    try
      TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassVal);
    finally
      ReleaseArguments(InitArgs);
    end;
  end;

  Result := ClassVal;
  TGarbageCollector.Instance.RemoveTempRoot(Session.MetadataObject);
  FActiveDecoratorSession.Free;
  FActiveDecoratorSession := nil;
end;

function TGocciaVM.GetSuperPropertyValue(const ASuperValue, AThisValue: TGocciaValue;
  const AName: string): TGocciaValue;
var
  SuperClass: TGocciaClassValue;
begin
  if not (ASuperValue is TGocciaClassValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  SuperClass := TGocciaClassValue(ASuperValue);
  if AName = PROP_CONSTRUCTOR then
    Exit(TGocciaVMSuperConstructorValue.Create(SuperClass));

  if AThisValue is TGocciaClassValue then
    Exit(SuperClass.GetProperty(AName));

  if Assigned(SuperClass.Prototype) then
    Exit(SuperClass.Prototype.GetPropertyWithContext(AName, AThisValue));

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.GetPropertyValue(const AObject: TGocciaValue;
  const AKey: string): TGocciaValue;
var
  Boxed: TGocciaObjectValue;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError('Cannot read properties of null (reading ''' + AKey + ''')');
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError('Cannot read properties of undefined (reading ''' + AKey + ''')');

  if TryGetRawPrivateValue(AObject, AKey, Result) then
    Exit;

  Result := AObject.GetProperty(AKey);
  if Assigned(Result) then
    Exit;

  Boxed := AObject.Box;
  if Assigned(Boxed) then
    Result := Boxed.GetProperty(AKey)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaVM.SetPropertyValue(const AObject: TGocciaValue;
  const AKey: string; const AValue: TGocciaValue);
var
  Current: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError('Cannot set properties of null (setting ''' + AKey + ''')');
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError('Cannot set properties of undefined (setting ''' + AKey + ''')');
  if (AKey <> '') and (AKey[1] = '#') then
  begin
    if AObject is TGocciaObjectValue then
    begin
      Current := TGocciaObjectValue(AObject);
      while Assigned(Current) do
      begin
        Descriptor := Current.GetOwnPropertyDescriptor(AKey);
        if (Descriptor is TGocciaPropertyDescriptorAccessor) and
           Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
        begin
          AObject.SetProperty(AKey, AValue);
          Exit;
        end;
        Current := Current.Prototype;
      end;
    end;
    SetRawPrivateValue(AObject, AKey, AValue);
    Exit;
  end;

  AObject.SetProperty(AKey, AValue);
end;

function TGocciaVM.TryGetRawPrivateValue(const AObject: TGocciaValue;
  const AKey: string; out AValue: TGocciaValue): Boolean;
begin
  Result := False;
  AValue := nil;
  if (AKey = '') or (AKey[1] <> '#') then
    Exit;

  if (AObject is TGocciaInstanceValue) and
     TGocciaInstanceValue(AObject).TryGetRawPrivateProperty(AKey, AValue) then
  begin
    Result := True;
    Exit;
  end;

  if (AObject is TGocciaClassValue) and
     TGocciaClassValue(AObject).PrivateStaticProperties.TryGetValue(AKey, AValue) then
  begin
    Result := True;
    Exit;
  end;
end;

procedure TGocciaVM.SetRawPrivateValue(const AObject: TGocciaValue;
  const AKey: string; const AValue: TGocciaValue);
begin
  if AObject is TGocciaInstanceValue then
  begin
    TGocciaInstanceValue(AObject).SetRawPrivateProperty(AKey, AValue);
    Exit;
  end;

  if AObject is TGocciaClassValue then
  begin
    TGocciaClassValue(AObject).AddPrivateStaticProperty(AKey, AValue);
    Exit;
  end;

  AObject.SetProperty(AKey, AValue);
end;

function TGocciaVM.HasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
var
  KeyStr: string;
  Index: Integer;
  Prop: TGocciaValue;
begin
  if (AObject is TGocciaNullLiteralValue) or
     (AObject is TGocciaUndefinedLiteralValue) or
     (AObject is TGocciaBooleanLiteralValue) or
     (AObject is TGocciaNumberLiteralValue) or
     (AObject is TGocciaStringLiteralValue) then
    ThrowTypeError('Cannot use ''in'' operator to search for ''' +
      AKey.ToStringLiteral.Value + ''' in ' + AObject.ToStringLiteral.Value);

  if (AKey is TGocciaSymbolValue) and (AObject is TGocciaObjectValue) then
  begin
    if TGocciaObjectValue(AObject).HasSymbolProperty(TGocciaSymbolValue(AKey)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  KeyStr := KeyToPropertyName(AKey);
  if AObject is TGocciaArrayValue then
  begin
    if TryGetArrayIndex(AKey, Index) then
    begin
      if (Index >= 0) and (Index < TGocciaArrayValue(AObject).Elements.Count) and
         (TGocciaArrayValue(AObject).Elements[Index] <> TGocciaHoleValue.HoleValue) then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    end;

    Prop := TGocciaArrayValue(AObject).GetProperty(KeyStr);
    if Assigned(Prop) and not (Prop is TGocciaUndefinedLiteralValue) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  if AObject is TGocciaObjectValue then
  begin
    if TGocciaObjectValue(AObject).HasProperty(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  Prop := AObject.GetProperty(KeyStr);
  if Assigned(Prop) and not (Prop is TGocciaUndefinedLiteralValue) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaVM.InvokeFunctionValue(const ACallee: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CalleeDesc: string;
begin
  if ACallee is TGocciaBytecodeFunctionValue then
    Exit(TGocciaBytecodeFunctionValue(ACallee).Call(AArguments, AThisValue));
  if ACallee is TGocciaFunctionBase then
    Exit(TGocciaFunctionBase(ACallee).Call(AArguments, AThisValue));
  if ACallee is TGocciaClassValue then
    Exit(TGocciaClassValue(ACallee).Call(AArguments, AThisValue));
  if Assigned(ACallee) then
    CalleeDesc := ACallee.TypeName
  else
    CalleeDesc := 'undefined';
  ThrowTypeError(CalleeDesc + ' is not a function');
end;

function TGocciaVM.ExecuteClosure(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  Frame: TGocciaVMCallFrame;
  SavedRegisters: TGocciaRegisterArray;
  SavedLocalCells: TGocciaBytecodeCellArray;
  SavedArgCount: Integer;
  SavedClosure: TGocciaBytecodeClosure;
  SavedHandlerCount: Integer;
  Instruction: UInt32;
  Op: UInt8;
  A, B, C: UInt8;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  KeyIndex: Integer;
  ArgsArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  GlobalName: string;
  Upvalue: TGocciaBytecodeUpvalue;
  ChildClosure: TGocciaBytecodeClosure;
  Desc: TGocciaUpvalueDescriptor;
  Handler: TGocciaBytecodeHandlerEntry;
  DoneValue: TGocciaValue;
  IterResult: TGocciaValue;
  NextMethod: TGocciaValue;
  DoneFlag: Boolean;
  Running: Boolean;
  Template: TGocciaFunctionTemplate;
  ChildTemplate: TGocciaFunctionTemplate;
  LeftValue, RightValue: TGocciaValue;
begin
  SavedRegisters := FRegisters;
  SavedLocalCells := FLocalCells;
  SavedArgCount := FArgCount;
  SavedClosure := FCurrentClosure;
  SavedHandlerCount := FHandlerStack.Count;
  try
    FRegisters := AcquireRegisters(Max(AClosure.Template.MaxRegisters, 1));
    FLocalCells := AcquireLocalCells(Max(AClosure.Template.MaxRegisters, 1));
    FArgCount := AArguments.Length;
    FCurrentClosure := AClosure;
    Inc(FFrameDepth);
    if Assigned(TGocciaCallStack.Instance) then
      TGocciaCallStack.Instance.Push(
        AClosure.Template.Name,
        FCurrentModuleSourcePath,
        0, 0);

    FillChar(Frame, SizeOf(Frame), 0);
    Template := AClosure.Template;
    Frame.Template := Template;

    SetLocal(0, AThisValue);
    for I := 0 to AArguments.Length - 1 do
      SetLocal(I + 1, AArguments.GetElement(I));

    Running := True;
    while Running and (Frame.IP < Template.CodeCount) do
    begin
      try
        Instruction := Template.GetInstructionUnchecked(Frame.IP);
        Inc(Frame.IP);

        Op := DecodeOp(Instruction);
        A := DecodeA(Instruction);
        B := DecodeB(Instruction);
        C := DecodeC(Instruction);
        case TGocciaOpCode(Op) of
      OP_LOAD_CONST:
        SetRegisterFast(A, ConstantToValue(Template.GetConstantUnchecked(DecodeBx(Instruction))));

      OP_LOAD_UNDEFINED:
        SetRegisterFast(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_LOAD_TRUE:
        SetRegisterFast(A, TGocciaBooleanLiteralValue.TrueValue);

      OP_LOAD_FALSE:
        SetRegisterFast(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_LOAD_NULL:
        SetRegisterFast(A, TGocciaNullLiteralValue.NullValue);

      OP_LOAD_HOLE:
        SetRegisterFast(A, TGocciaHoleValue.HoleValue);

      OP_CHECK_TYPE:
        case TGocciaLocalType(B) of
          sltInteger:
            begin
              if not (GetRegister(A) is TGocciaNumberLiteralValue) or
                 GetRegister(A).ToNumberLiteral.IsNaN or
                 GetRegister(A).ToNumberLiteral.IsInfinite or
                 (Frac(GetRegister(A).ToNumberLiteral.Value) <> 0.0) then
                ThrowTypeError('Type ''' + GetRegister(A).TypeName +
                  ''' is not assignable to type ''integer''');
            end;
          sltFloat:
            begin
              if not (GetRegister(A) is TGocciaNumberLiteralValue) then
                ThrowTypeError('Type ''' + GetRegister(A).TypeName +
                  ''' is not assignable to type ''number''');
            end;
          sltBoolean:
            begin
              if not (GetRegister(A) is TGocciaBooleanLiteralValue) then
                ThrowTypeError('Type ''' + GetRegister(A).TypeName +
                  ''' is not assignable to type ''boolean''');
            end;
          sltString:
            begin
              if not (GetRegister(A) is TGocciaStringLiteralValue) then
                ThrowTypeError('Type ''' + GetRegister(A).TypeName +
                  ''' is not assignable to type ''string''');
            end;
          sltReference:
            begin
              if GetRegister(A).IsPrimitive then
                ThrowTypeError('Type ''' + GetRegister(A).TypeName +
                  ''' is not assignable to type ''object''');
            end;
        end;

      OP_TO_PRIMITIVE:
        SetRegisterFast(A, ToPrimitive(GetRegisterFast(B)));

      OP_LOAD_INT:
        if (DecodesBx(Instruction) >= 0) and (DecodesBx(Instruction) <= 255) then
          SetRegisterFast(A, TGocciaNumberLiteralValue.SmallInt(DecodesBx(Instruction)))
        else
          SetRegisterFast(A, TGocciaNumberLiteralValue.Create(DecodesBx(Instruction)));

      OP_MOVE:
        SetRegisterFast(A, GetRegisterFast(B));

      OP_GET_LOCAL:
        SetRegisterFast(A, GetLocal(DecodeBx(Instruction)));

      OP_SET_LOCAL:
        SetLocal(DecodeBx(Instruction), GetRegister(A));

      OP_GET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
            SetRegister(A, Upvalue.Cell.Value)
          else
            SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
        end
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;

      OP_SET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
            Upvalue.Cell.Value := GetRegister(A);
        end;
      end;

      OP_CLOSE_UPVALUE:
        if (A >= 0) and (A < Length(FLocalCells)) then
          FLocalCells[A] := nil;

      OP_ARG_COUNT:
        SetRegisterFast(A, VMNumberValue(FArgCount));

      OP_PACK_ARGS:
      begin
        ArgsArray := TGocciaArrayValue.Create;
        for I := B to FArgCount - 1 do
          ArgsArray.Elements.Add(GetLocal(I + 1));
        SetRegisterFast(A, ArgsArray);
      end;

      OP_JUMP:
        Inc(Frame.IP, DecodeAx(Instruction));

      OP_JUMP_IF_TRUE:
        if GetRegister(A).ToBooleanLiteral.Value then
          Inc(Frame.IP, DecodesBx(Instruction));

      OP_JUMP_IF_FALSE:
        if not GetRegister(A).ToBooleanLiteral.Value then
          Inc(Frame.IP, DecodesBx(Instruction));

      OP_JUMP_IF_NULLISH:
        if MatchesNullishKind(GetRegister(A), B) then
          Inc(Frame.IP, C);

      OP_JUMP_IF_NOT_NULLISH:
        if not MatchesNullishKind(GetRegister(A), B) then
          Inc(Frame.IP, C);

      OP_PUSH_HANDLER:
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth);

      OP_POP_HANDLER:
        if not FHandlerStack.IsEmpty then
          FHandlerStack.Pop;

      OP_ADD_INT, OP_ADD_FLOAT:
      begin
        LeftNum := GetRegister(B).ToNumberLiteral;
        RightNum := GetRegister(C).ToNumberLiteral;
        SetRegisterFast(A, VMNumberValue(LeftNum.Value + RightNum.Value));
      end;

      OP_SUB_INT, OP_SUB_FLOAT:
      begin
        LeftNum := GetRegister(B).ToNumberLiteral;
        RightNum := GetRegister(C).ToNumberLiteral;
        SetRegisterFast(A, VMNumberValue(LeftNum.Value - RightNum.Value));
      end;

      OP_MUL_INT, OP_MUL_FLOAT:
      begin
        LeftNum := GetRegister(B).ToNumberLiteral;
        RightNum := GetRegister(C).ToNumberLiteral;
        SetRegisterFast(A, VMNumberValue(LeftNum.Value * RightNum.Value));
      end;

      OP_DIV_INT, OP_DIV_FLOAT:
      begin
        LeftNum := GetRegister(B).ToNumberLiteral;
        RightNum := GetRegister(C).ToNumberLiteral;
        SetRegisterFast(A, VMNumberValue(LeftNum.Value / RightNum.Value));
      end;

      OP_MOD_INT, OP_MOD_FLOAT:
      begin
        LeftNum := GetRegister(B).ToNumberLiteral;
        RightNum := GetRegister(C).ToNumberLiteral;
        SetRegisterFast(A, VMNumberValue(FMod(LeftNum.Value, RightNum.Value)));
      end;

      OP_EQ_INT, OP_EQ_FLOAT:
        if GetRegister(B).ToNumberLiteral.Value = GetRegister(C).ToNumberLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_NEQ_INT, OP_NEQ_FLOAT:
        if GetRegister(B).ToNumberLiteral.Value <> GetRegister(C).ToNumberLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_LT_INT, OP_LT_FLOAT:
        if GetRegister(B).ToNumberLiteral.Value < GetRegister(C).ToNumberLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_GT_INT, OP_GT_FLOAT:
        if GetRegister(B).ToNumberLiteral.Value > GetRegister(C).ToNumberLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_LTE_INT, OP_LTE_FLOAT:
        if GetRegister(B).ToNumberLiteral.Value <= GetRegister(C).ToNumberLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_GTE_INT, OP_GTE_FLOAT:
        if GetRegister(B).ToNumberLiteral.Value >= GetRegister(C).ToNumberLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_NEG_INT, OP_NEG_FLOAT:
        SetRegisterFast(A, VMNumberValue(-GetRegisterFast(B).ToNumberLiteral.Value));

      OP_CONCAT:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaStringLiteralValue) and
           (RightValue is TGocciaStringLiteralValue) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            TGocciaStringLiteralValue(LeftValue).Value +
            TGocciaStringLiteralValue(RightValue).Value))
        else
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            VMToECMAStringFast(LeftValue).Value +
            VMToECMAStringFast(RightValue).Value));
      end;

      OP_NEW_ARRAY:
        SetRegister(A, TGocciaArrayValue.Create);

      OP_ARRAY_POP:
      begin
        if GetRegister(B) is TGocciaArrayValue then
        begin
          if TGocciaArrayValue(GetRegister(B)).Elements.Count = 0 then
            SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue)
          else
          begin
            SetRegister(A, TGocciaArrayValue(GetRegister(B)).Elements[
              TGocciaArrayValue(GetRegister(B)).Elements.Count - 1]);
            TGocciaArrayValue(GetRegister(B)).Elements.Delete(
              TGocciaArrayValue(GetRegister(B)).Elements.Count - 1);
            if GetRegister(A) = TGocciaHoleValue.HoleValue then
              SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
          end;
        end
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;

      OP_ARRAY_PUSH:
        TGocciaArrayValue(GetRegister(A)).Elements.Add(GetRegister(B));

      OP_ARRAY_GET:
      begin
        if GetRegister(B) is TGocciaArrayValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, TGocciaArrayValue(GetRegister(B)).GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else if TryGetArrayIndex(GetRegister(C), KeyIndex) then
          begin
            if (KeyIndex >= 0) and
               (KeyIndex < TGocciaArrayValue(GetRegister(B)).Elements.Count) then
            begin
              if TGocciaArrayValue(GetRegister(B)).Elements[KeyIndex] = TGocciaHoleValue.HoleValue then
                SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue)
              else
                SetRegister(A, TGocciaArrayValue(GetRegister(B)).Elements[KeyIndex]);
            end
            else
              SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
          end
          else
            SetRegister(A, TGocciaArrayValue(GetRegister(B)).GetProperty(
              KeyToPropertyName(GetRegister(C))));
        end
        else if GetRegister(B) is TGocciaObjectValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, TGocciaObjectValue(GetRegister(B)).GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else
            SetRegister(A, TGocciaObjectValue(GetRegister(B)).GetProperty(
              KeyToPropertyName(GetRegister(C))));
        end
        else if GetRegister(B) is TGocciaClassValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, TGocciaClassValue(GetRegister(B)).GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else
            SetRegister(A, TGocciaClassValue(GetRegister(B)).GetProperty(
              KeyToPropertyName(GetRegister(C))));
        end
        else if GetRegister(B) is TGocciaStringLiteralValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, GetRegister(B).Box.GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else if TryGetArrayIndex(GetRegister(C), KeyIndex) and
             (KeyIndex >= 0) and
             (KeyIndex < Length(TGocciaStringLiteralValue(GetRegister(B)).Value)) then
            SetRegister(A, TGocciaStringLiteralValue.Create(
              TGocciaStringLiteralValue(GetRegister(B)).Value[KeyIndex + 1]))
          else
            SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
        end
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;

      OP_ARRAY_SET:
      begin
        if GetRegister(A) is TGocciaArrayValue then
        begin
          if GetRegister(B) is TGocciaSymbolValue then
            TGocciaArrayValue(GetRegister(A)).AssignSymbolProperty(
              TGocciaSymbolValue(GetRegister(B)), GetRegister(C))
          else if TryGetArrayIndex(GetRegister(B), KeyIndex) then
            TGocciaArrayValue(GetRegister(A)).SetElement(KeyIndex, GetRegister(C))
          else
            TGocciaArrayValue(GetRegister(A)).SetProperty(
              KeyToPropertyName(GetRegister(B)),
              GetRegister(C));
        end
        else if GetRegister(A) is TGocciaObjectValue then
        begin
          if GetRegister(B) is TGocciaSymbolValue then
            TGocciaObjectValue(GetRegister(A)).AssignSymbolProperty(
              TGocciaSymbolValue(GetRegister(B)), GetRegister(C))
          else
            TGocciaObjectValue(GetRegister(A)).SetProperty(
              KeyToPropertyName(GetRegister(B)),
              GetRegister(C));
        end
        else if GetRegister(A) is TGocciaClassValue then
        begin
          if GetRegister(B) is TGocciaSymbolValue then
            TGocciaClassValue(GetRegister(A)).AssignSymbolProperty(
              TGocciaSymbolValue(GetRegister(B)), GetRegister(C))
          else
            TGocciaClassValue(GetRegister(A)).SetProperty(
              KeyToPropertyName(GetRegister(B)),
              GetRegister(C));
        end;
      end;

      OP_GET_LENGTH:
      begin
        if GetRegister(B) is TGocciaArrayValue then
          SetRegister(A, VMNumberValue(TGocciaArrayValue(GetRegister(B)).GetLength))
        else
          SetRegister(A, TGocciaNumberLiteralValue.ZeroValue);
      end;

      OP_NEW_OBJECT:
      begin
        if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
          TGocciaObjectValue.InitializeSharedPrototype;
        SetRegister(A, TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype));
      end;

      OP_NEW_CLASS:
      begin
        SetRegister(A, TGocciaVMClassValue.Create(Self,
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue, nil));
        TGocciaVMClassValue(GetRegister(A)).Prototype.AssignProperty(
          PROP_CONSTRUCTOR, GetRegister(A));
      end;

      OP_CLASS_SET_SUPER:
      begin
        if (GetRegister(A) is TGocciaVMClassValue) and
           (GetRegister(B) is TGocciaClassValue) then
        begin
          TGocciaVMClassValue(GetRegister(A)).SuperClass :=
            TGocciaClassValue(GetRegister(B));
          TGocciaVMClassValue(GetRegister(A)).Prototype.Prototype :=
            TGocciaClassValue(GetRegister(B)).Prototype;
        end;
      end;

      OP_CLASS_ADD_METHOD_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        if GetRegister(A) is TGocciaVMClassValue then
        begin
          if GlobalName = PROP_CONSTRUCTOR then
          begin
            TGocciaVMClassValue(GetRegister(A)).SetVMConstructor(GetRegister(C));
            TGocciaVMClassValue(GetRegister(A)).Prototype.AssignProperty(
              PROP_CONSTRUCTOR, GetRegister(A));
          end
          else
            TGocciaVMClassValue(GetRegister(A)).Prototype.AssignProperty(
              GlobalName, GetRegister(C));
        end
        else
          SetPropertyValue(GetRegister(A), GlobalName, GetRegister(C));
      end;

      OP_CLASS_SET_FIELD_INITIALIZER:
      begin
        if GetRegister(A) is TGocciaVMClassValue then
          TGocciaVMClassValue(GetRegister(A)).SetMethodInitializers(
            [GetRegister(B)]);
      end;

      OP_GET_PROP_CONST:
        SetRegister(A, GetPropertyValue(GetRegister(B),
          Template.GetConstantUnchecked(C).StringValue));

      OP_SET_PROP_CONST:
        SetPropertyValue(GetRegister(A),
          Template.GetConstantUnchecked(B).StringValue,
          GetRegister(C));

      OP_DELETE_PROP_CONST:
      begin
        if GetRegister(A) is TGocciaObjectValue then
        begin
          if TGocciaObjectValue(GetRegister(A)).DeleteProperty(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue) then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            ThrowTypeError('Cannot delete property ''' +
              Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue +
              ''' of [object Object]');
        end
        else
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue);
      end;

      OP_UNPACK:
      begin
        if GetRegister(B) is TGocciaArrayValue then
        begin
          ArgsArray := TGocciaArrayValue.Create;
          for I := C to TGocciaArrayValue(GetRegister(B)).Elements.Count - 1 do
            ArgsArray.Elements.Add(TGocciaArrayValue(GetRegister(B)).GetElement(I));
          SetRegister(A, ArgsArray);
        end
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;

      OP_GET_INDEX:
      begin
        if GetRegister(B) is TGocciaArrayValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, TGocciaArrayValue(GetRegister(B)).GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else if TryGetArrayIndex(GetRegister(C), KeyIndex) then
          begin
            if (KeyIndex >= 0) and
               (KeyIndex < TGocciaArrayValue(GetRegister(B)).Elements.Count) then
            begin
              if TGocciaArrayValue(GetRegister(B)).Elements[KeyIndex] = TGocciaHoleValue.HoleValue then
                SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue)
              else
                SetRegister(A, TGocciaArrayValue(GetRegister(B)).Elements[KeyIndex]);
            end
            else
              SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
          end
          else
            SetRegister(A, TGocciaArrayValue(GetRegister(B)).GetProperty(
              KeyToPropertyName(GetRegister(C))));
        end
        else if GetRegister(B) is TGocciaObjectValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, TGocciaObjectValue(GetRegister(B)).GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else
            SetRegister(A, TGocciaObjectValue(GetRegister(B)).GetProperty(
              KeyToPropertyName(GetRegister(C))));
        end
        else if GetRegister(B) is TGocciaStringLiteralValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, GetRegister(B).Box.GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else if TryGetArrayIndex(GetRegister(C), KeyIndex) and
             (KeyIndex >= 0) and
             (KeyIndex < Length(TGocciaStringLiteralValue(GetRegister(B)).Value)) then
            SetRegister(A, TGocciaStringLiteralValue.Create(
              TGocciaStringLiteralValue(GetRegister(B)).Value[KeyIndex + 1]))
          else
            SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
        end
        else if GetRegister(B) is TGocciaClassValue then
        begin
          if GetRegister(C) is TGocciaSymbolValue then
            SetRegister(A, TGocciaClassValue(GetRegister(B)).GetSymbolProperty(
              TGocciaSymbolValue(GetRegister(C))))
          else
            SetRegister(A, TGocciaClassValue(GetRegister(B)).GetProperty(
              KeyToPropertyName(GetRegister(C))));
        end
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;

      OP_SET_INDEX:
      begin
        if GetRegister(A) is TGocciaArrayValue then
        begin
          if GetRegister(B) is TGocciaSymbolValue then
            TGocciaArrayValue(GetRegister(A)).AssignSymbolProperty(
              TGocciaSymbolValue(GetRegister(B)), GetRegister(C))
          else if TryGetArrayIndex(GetRegister(B), KeyIndex) then
            TGocciaArrayValue(GetRegister(A)).SetElement(KeyIndex, GetRegister(C))
          else
            TGocciaArrayValue(GetRegister(A)).SetProperty(
              KeyToPropertyName(GetRegister(B)),
              GetRegister(C));
        end
        else if GetRegister(A) is TGocciaObjectValue then
        begin
          if GetRegister(B) is TGocciaSymbolValue then
            TGocciaObjectValue(GetRegister(A)).AssignSymbolProperty(
              TGocciaSymbolValue(GetRegister(B)), GetRegister(C))
          else
            TGocciaObjectValue(GetRegister(A)).SetProperty(
              KeyToPropertyName(GetRegister(B)),
              GetRegister(C));
        end
        else if GetRegister(A) is TGocciaClassValue then
        begin
          if GetRegister(B) is TGocciaSymbolValue then
            TGocciaClassValue(GetRegister(A)).AssignSymbolProperty(
              TGocciaSymbolValue(GetRegister(B)), GetRegister(C))
          else
            TGocciaClassValue(GetRegister(A)).SetProperty(
              KeyToPropertyName(GetRegister(B)),
              GetRegister(C));
        end;
      end;

      OP_ADD:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaNumberLiteralValue) and
           (RightValue is TGocciaNumberLiteralValue) then
          SetRegisterFast(A, VMAddValues(LeftValue, RightValue))
        else if (LeftValue is TGocciaStringLiteralValue) and
                (RightValue is TGocciaStringLiteralValue) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            TGocciaStringLiteralValue(LeftValue).Value +
            TGocciaStringLiteralValue(RightValue).Value))
        else if LeftValue.IsPrimitive and RightValue.IsPrimitive then
        begin
          if (LeftValue is TGocciaStringLiteralValue) or
             (RightValue is TGocciaStringLiteralValue) then
            SetRegisterFast(A, TGocciaStringLiteralValue.Create(
              LeftValue.ToStringLiteral.Value + RightValue.ToStringLiteral.Value))
          else
            SetRegisterFast(A, VMAddValues(LeftValue, RightValue));
        end
        else
          SetRegister(A, VMAddValues(LeftValue, RightValue));
      end;

      OP_SUB:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaNumberLiteralValue) and
           (RightValue is TGocciaNumberLiteralValue) then
          SetRegisterFast(A, VMSubtractValues(LeftValue, RightValue))
        else
          SetRegister(A, VMSubtractValues(LeftValue, RightValue));
      end;

      OP_MUL:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaNumberLiteralValue) and
           (RightValue is TGocciaNumberLiteralValue) then
          SetRegisterFast(A, VMMultiplyValues(LeftValue, RightValue))
        else
          SetRegister(A, VMMultiplyValues(LeftValue, RightValue));
      end;

      OP_DIV:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaNumberLiteralValue) and
           (RightValue is TGocciaNumberLiteralValue) then
          SetRegisterFast(A, VMDivideValues(LeftValue, RightValue))
        else
          SetRegister(A, VMDivideValues(LeftValue, RightValue));
      end;

      OP_MOD:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaNumberLiteralValue) and
           (RightValue is TGocciaNumberLiteralValue) then
          SetRegisterFast(A, VMModuloValues(LeftValue, RightValue))
        else
          SetRegister(A, VMModuloValues(LeftValue, RightValue));
      end;

      OP_POW:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaNumberLiteralValue) and
           (RightValue is TGocciaNumberLiteralValue) then
          SetRegisterFast(A, VMPowerValues(LeftValue, RightValue))
        else
          SetRegister(A, VMPowerValues(LeftValue, RightValue));
      end;

      OP_NEG:
        SetRegister(A, VMNumberValue(-GetRegister(B).ToNumberLiteral.Value));

      OP_BAND:
        SetRegister(A, VMBitwiseAndValues(GetRegister(B), GetRegister(C)));

      OP_BOR:
        SetRegister(A, VMBitwiseOrValues(GetRegister(B), GetRegister(C)));

      OP_BXOR:
        SetRegister(A, VMBitwiseXorValues(GetRegister(B), GetRegister(C)));

      OP_SHL:
        SetRegister(A, VMLeftShiftValues(GetRegister(B), GetRegister(C)));

      OP_SHR:
        SetRegister(A, VMRightShiftValues(GetRegister(B), GetRegister(C)));

      OP_USHR:
        SetRegister(A, VMUnsignedRightShiftValues(GetRegister(B), GetRegister(C)));

      OP_BNOT:
        SetRegister(A, VMBitwiseNotValue(GetRegister(B)));

      OP_EQ:
        SetRegister(A, GetRegister(B).IsEqual(GetRegister(C)));

      OP_NEQ:
        SetRegister(A, GetRegister(B).IsNotEqual(GetRegister(C)));

      OP_LT:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaStringLiteralValue) and
           (RightValue is TGocciaStringLiteralValue) then
        begin
          if TGocciaStringLiteralValue(LeftValue).Value <
             TGocciaStringLiteralValue(RightValue).Value then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if (LeftValue is TGocciaNumberLiteralValue) and
                (RightValue is TGocciaNumberLiteralValue) then
        begin
          if VMCompareNumbers(TGocciaNumberLiteralValue(LeftValue),
             TGocciaNumberLiteralValue(RightValue), False) then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if VMLessThan(LeftValue, RightValue) then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
      end;

      OP_GT:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaStringLiteralValue) and
           (RightValue is TGocciaStringLiteralValue) then
        begin
          if TGocciaStringLiteralValue(LeftValue).Value >
             TGocciaStringLiteralValue(RightValue).Value then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if (LeftValue is TGocciaNumberLiteralValue) and
                (RightValue is TGocciaNumberLiteralValue) then
        begin
          if VMCompareNumbers(TGocciaNumberLiteralValue(LeftValue),
             TGocciaNumberLiteralValue(RightValue), True) then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if VMGreaterThan(LeftValue, RightValue) then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
      end;

      OP_LTE:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaStringLiteralValue) and
           (RightValue is TGocciaStringLiteralValue) then
        begin
          if TGocciaStringLiteralValue(LeftValue).Value <=
             TGocciaStringLiteralValue(RightValue).Value then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if (LeftValue is TGocciaNumberLiteralValue) and
                (RightValue is TGocciaNumberLiteralValue) then
        begin
          if (not TGocciaNumberLiteralValue(LeftValue).IsNaN) and
             (not TGocciaNumberLiteralValue(RightValue).IsNaN) and
             (not VMCompareNumbers(TGocciaNumberLiteralValue(LeftValue),
               TGocciaNumberLiteralValue(RightValue), True)) then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if VMLessThanOrEqual(LeftValue, RightValue) then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
      end;

      OP_GTE:
      begin
        LeftValue := GetRegisterFast(B);
        RightValue := GetRegisterFast(C);
        if (LeftValue is TGocciaStringLiteralValue) and
           (RightValue is TGocciaStringLiteralValue) then
        begin
          if TGocciaStringLiteralValue(LeftValue).Value >=
             TGocciaStringLiteralValue(RightValue).Value then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if (LeftValue is TGocciaNumberLiteralValue) and
                (RightValue is TGocciaNumberLiteralValue) then
        begin
          if (not TGocciaNumberLiteralValue(LeftValue).IsNaN) and
             (not TGocciaNumberLiteralValue(RightValue).IsNaN) and
             (not VMCompareNumbers(TGocciaNumberLiteralValue(LeftValue),
               TGocciaNumberLiteralValue(RightValue), False)) then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if VMGreaterThanOrEqual(LeftValue, RightValue) then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
      end;

      OP_TYPEOF:
        SetRegister(A, TGocciaStringLiteralValue.Create(GetRegister(B).TypeOf));

      OP_IS_INSTANCE:
        SetRegister(A, VMInstanceOfValue(GetRegister(B), GetRegister(C)));

      OP_HAS_PROPERTY:
        SetRegister(A, HasPropertyValue(GetRegister(B), GetRegister(C)));

      OP_TO_NUMBER:
        SetRegister(A, GetRegister(B).ToNumberLiteral);

      OP_TO_STRING:
        SetRegisterFast(A, VMToECMAStringFast(GetRegisterFast(B)));

      OP_DEL_INDEX:
      begin
        if GetRegister(B) is TGocciaArrayValue then
        begin
          if TryGetArrayIndex(GetRegister(C), KeyIndex) and
             (KeyIndex >= 0) and
             (KeyIndex < TGocciaArrayValue(GetRegister(B)).Elements.Count) then
          begin
            TGocciaArrayValue(GetRegister(B)).SetElement(KeyIndex, TGocciaHoleValue.HoleValue);
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue);
          end
          else
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue);
        end
        else if GetRegister(B) is TGocciaObjectValue then
        begin
          if TGocciaObjectValue(GetRegister(B)).DeleteProperty(
            KeyToPropertyName(GetRegister(C))) then
            SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
        end
        else
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue);
      end;

      OP_CLOSURE:
      begin
        ChildTemplate := Template.GetFunctionUnchecked(DecodeBx(Instruction));
        ChildClosure := TGocciaBytecodeClosure.Create(
          ChildTemplate, ChildTemplate.UpvalueCount);
        for I := 0 to ChildTemplate.UpvalueCount - 1 do
        begin
          Desc := ChildTemplate.GetUpvalueDescriptor(I);
          if Desc.IsLocal then
            ChildClosure.SetUpvalue(I, TGocciaBytecodeUpvalue.Create(
              GetLocalCell(Desc.Index)))
          else if Assigned(FCurrentClosure) then
            ChildClosure.SetUpvalue(I, FCurrentClosure.GetUpvalue(Desc.Index));
        end;
        SetRegister(A, TGocciaBytecodeFunctionValue.Create(Self, ChildClosure));
      end;

      OP_CALL:
      begin
        if (C and 1) = 1 then
          CallArgs := AcquireArguments
        else
          CallArgs := AcquireArguments(B);
        try
          if (C and 1) = 1 then
          begin
            if GetRegister(B) is TGocciaArrayValue then
              for I := 0 to TGocciaArrayValue(GetRegister(B)).Elements.Count - 1 do
                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetElement(I));
          end
          else
            for I := 0 to B - 1 do
              CallArgs.Add(GetRegister(A + 1 + I));
          SetRegister(A, InvokeFunctionValue(
            GetRegister(A), CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_CALL_METHOD:
      begin
        if (C and 1) = 1 then
          CallArgs := AcquireArguments
        else
          CallArgs := AcquireArguments(B);
        try
          if (C and 1) = 1 then
          begin
            if GetRegister(B) is TGocciaArrayValue then
              for I := 0 to TGocciaArrayValue(GetRegister(B)).Elements.Count - 1 do
                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetElement(I));
          end
          else
            for I := 0 to B - 1 do
              CallArgs.Add(GetRegister(A + 1 + I));
          SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs, GetRegister(A - 1)));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_CONSTRUCT:
      begin
        CallArgs := AcquireArguments(C);
        try
          for I := 0 to C - 1 do
            CallArgs.Add(GetRegister(B + 1 + I));
          SetRegister(A, ConstructValue(GetRegister(B), CallArgs));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_GET_ITER:
        SetRegister(A, GetIteratorValue(GetRegister(B), C <> 0));

      OP_ITER_NEXT:
      begin
        IterResult := GetRegister(C);
        if IterResult is TGocciaIteratorValue then
        begin
          SetRegister(A, TGocciaIteratorValue(IterResult).DirectNext(DoneFlag));
          if DoneFlag then
            SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
          if DoneFlag then
            SetRegister(B, TGocciaBooleanLiteralValue.TrueValue)
          else
            SetRegister(B, TGocciaBooleanLiteralValue.FalseValue);
        end
        else if IterResult is TGocciaObjectValue then
        begin
          NextMethod := IterResult.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or
             (NextMethod is TGocciaUndefinedLiteralValue) or
             not NextMethod.IsCallable then
          begin
            SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
            SetRegister(B, TGocciaBooleanLiteralValue.TrueValue);
          end
          else
          begin
            CallArgs := AcquireArguments;
            try
              IterResult := TGocciaFunctionBase(NextMethod).Call(CallArgs, IterResult);
            finally
              ReleaseArguments(CallArgs);
            end;

            IterResult := AwaitValue(IterResult);
            if IterResult.IsPrimitive then
              ThrowTypeError('Iterator result ' +
                IterResult.ToStringLiteral.Value + ' is not an object');

            DoneValue := IterResult.GetProperty(PROP_DONE);
            if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
            begin
              SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
              SetRegister(B, TGocciaBooleanLiteralValue.TrueValue);
            end
            else
            begin
              SetRegister(A, IterResult.GetProperty(PROP_VALUE));
              SetRegister(B, TGocciaBooleanLiteralValue.FalseValue);
            end;
          end;
        end
        else
        begin
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
          SetRegister(B, TGocciaBooleanLiteralValue.TrueValue);
        end;
      end;

      OP_AWAIT:
        SetRegister(A, AwaitValue(GetRegister(B)));

      OP_SETUP_AUTO_ACCESSOR_CONST:
        SetupAutoAccessorValue(Template.GetConstantUnchecked(C).StringValue);

      OP_BEGIN_DECORATORS:
        BeginDecorators(GetRegister(A), GetRegister(A + 1));

      OP_APPLY_ELEMENT_DECORATOR_CONST:
        ApplyElementDecorator(GetRegister(A),
          Template.GetConstantUnchecked(C).StringValue);

      OP_APPLY_CLASS_DECORATOR:
        ApplyClassDecorator(GetRegister(A));

      OP_FINISH_DECORATORS:
        SetRegister(A, FinishDecorators(GetRegister(A)));

      OP_GET_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) and FGlobalScope.Contains(GlobalName) then
          SetRegister(A, FGlobalScope.GetValue(GlobalName))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;

      OP_SET_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
        begin
          if FGlobalScope.Contains(GlobalName) then
            FGlobalScope.AssignLexicalBinding(GlobalName, GetRegister(A))
          else
            ThrowReferenceError(GlobalName + ' is not defined');
        end;
      end;

      OP_HAS_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) and FGlobalScope.Contains(GlobalName) then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);
      end;

      OP_IMPORT:
        SetRegister(A, ImportModuleValue(
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue));

      OP_EXPORT:
        ExportBindingValue(
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
          GetRegister(A));

      OP_THROW:
        raise EGocciaBytecodeThrow.Create(GetRegister(A));

      OP_NOT:
        if GetRegister(B).ToBooleanLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue);

      OP_TO_BOOL:
        if GetRegister(B).ToBooleanLiteral.Value then
          SetRegister(A, TGocciaBooleanLiteralValue.TrueValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FalseValue);

      OP_DEFINE_ACCESSOR_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(C).StringValue;
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterProperty(GetRegister(A), GlobalName, GetRegister(A + 1))
          else
            DefineStaticGetterProperty(GetRegister(A), GlobalName, GetRegister(A + 1));
        end
        else
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterProperty(GetRegister(A), GlobalName, GetRegister(A + 1))
          else
            DefineGetterProperty(GetRegister(A), GlobalName, GetRegister(A + 1));
        end;
      end;

      OP_DEFINE_ACCESSOR_DYNAMIC:
      begin
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterPropertyByKey(GetRegister(A), GetRegister(C), GetRegister(A + 1))
          else
            DefineStaticGetterPropertyByKey(GetRegister(A), GetRegister(C), GetRegister(A + 1));
        end
        else
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterPropertyByKey(GetRegister(A), GetRegister(C), GetRegister(A + 1))
          else
            DefineGetterPropertyByKey(GetRegister(A), GetRegister(C), GetRegister(A + 1));
        end;
      end;

      OP_COLLECTION_OP:
      begin
        case B of
          COLLECTION_OP_SPREAD_OBJECT:
            if GetRegister(A) is TGocciaObjectValue then
              SpreadObjectIntoValue(TGocciaObjectValue(GetRegister(A)), GetRegister(C));

          COLLECTION_OP_OBJECT_REST:
            begin
              if (A + 1 < Length(FRegisters)) and
                 (GetRegister(A + 1) is TGocciaArrayValue) then
                SetRegister(A, ObjectRestValue(GetRegister(C),
                  TGocciaArrayValue(GetRegister(A + 1))))
              else
                SetRegister(A, ObjectRestValue(GetRegister(C), nil));
            end;

          COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY:
            begin
              DoneValue := IterableToArray(GetRegister(C));
              if (GetRegister(A) is TGocciaArrayValue) and (DoneValue is TGocciaArrayValue) then
                for I := 0 to TGocciaArrayValue(DoneValue).Elements.Count - 1 do
                  TGocciaArrayValue(GetRegister(A)).Elements.Add(
                    TGocciaArrayValue(DoneValue).GetElement(I));
            end;
        else
          raise Exception.CreateFmt('Unsupported collection helper mode: %d', [B]);
        end;
      end;

      OP_VALIDATE_VALUE:
      begin
        case B of
          VALIDATE_OP_REQUIRE_OBJECT:
            begin
              if (GetRegister(A) is TGocciaNullLiteralValue) or
                 (GetRegister(A) is TGocciaUndefinedLiteralValue) then
                ThrowTypeError('Cannot destructure ' +
                  GetRegister(A).ToStringLiteral.Value +
                  ' as it is not an object');
            end;

          VALIDATE_OP_REQUIRE_ITERABLE:
            SetRegister(A, IterableToArray(GetRegister(A)));
        else
          raise Exception.CreateFmt('Unsupported validation mode: %d', [B]);
        end;
      end;

      OP_THROW_TYPE_ERROR_CONST:
        ThrowTypeError(Template.GetConstantUnchecked(C).StringValue);

      OP_DEFINE_GLOBAL_CONST:
        DefineGlobalValue(Template.GetConstantUnchecked(C).StringValue, GetRegister(A));

      OP_FINALIZE_ENUM:
        SetRegister(A, FinalizeEnumValue(GetRegister(A),
          Template.GetConstantUnchecked(C).StringValue));

      OP_SUPER_GET_CONST:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValue(GetRegister(A + 1),
            GetRegister(A - 1), Template.GetConstantUnchecked(C).StringValue))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_RETURN:
        Exit(GetRegister(A));
        else
          raise Exception.CreateFmt('Unsupported Goccia VM opcode in minimal executor: %d', [Op]);
        end;
      except
        on E: EGocciaBytecodeThrow do
        begin
          if (not FHandlerStack.IsEmpty) and
             (FHandlerStack.Peek.FrameDepth = FFrameDepth) then
          begin
            Handler := FHandlerStack.Peek;
            FHandlerStack.Pop;
            Frame.IP := Handler.CatchIP;
            SetRegister(Handler.CatchRegister, E.ThrownValue);
          end
          else
            raise;
        end;
        on E: TGocciaThrowValue do
        begin
          if (not FHandlerStack.IsEmpty) and
             (FHandlerStack.Peek.FrameDepth = FFrameDepth) then
          begin
            Handler := FHandlerStack.Peek;
            FHandlerStack.Pop;
            Frame.IP := Handler.CatchIP;
            SetRegister(Handler.CatchRegister, E.Value);
          end
          else
            raise EGocciaBytecodeThrow.Create(E.Value);
        end;
        on E: TGocciaTypeError do
        begin
          if (not FHandlerStack.IsEmpty) and
             (FHandlerStack.Peek.FrameDepth = FFrameDepth) then
          begin
            Handler := FHandlerStack.Peek;
            FHandlerStack.Pop;
            Frame.IP := Handler.CatchIP;
            SetRegister(Handler.CatchRegister,
              CreateErrorObject(TYPE_ERROR_NAME, E.Message));
          end
          else
            raise EGocciaBytecodeThrow.Create(
              CreateErrorObject(TYPE_ERROR_NAME, E.Message));
        end;
        on E: TGocciaReferenceError do
        begin
          if (not FHandlerStack.IsEmpty) and
             (FHandlerStack.Peek.FrameDepth = FFrameDepth) then
          begin
            Handler := FHandlerStack.Peek;
            FHandlerStack.Pop;
            Frame.IP := Handler.CatchIP;
            SetRegister(Handler.CatchRegister,
              CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
          end
          else
            raise EGocciaBytecodeThrow.Create(
              CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
        end;
        on E: TGocciaRuntimeError do
        begin
          if (not FHandlerStack.IsEmpty) and
             (FHandlerStack.Peek.FrameDepth = FFrameDepth) then
          begin
            Handler := FHandlerStack.Peek;
            FHandlerStack.Pop;
            Frame.IP := Handler.CatchIP;
            SetRegister(Handler.CatchRegister,
              CreateErrorObject(ERROR_NAME, E.Message));
          end
          else
            raise EGocciaBytecodeThrow.Create(
              CreateErrorObject(ERROR_NAME, E.Message));
        end;
      end;
    end;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  finally
    if Assigned(TGocciaCallStack.Instance) then
      TGocciaCallStack.Instance.Pop;
    while FHandlerStack.Count > SavedHandlerCount do
      FHandlerStack.Pop;
    Dec(FFrameDepth);
    FCurrentClosure := SavedClosure;
    FArgCount := SavedArgCount;
    ReleaseLocalCells(FLocalCells);
    ReleaseRegisters(FRegisters);
    FLocalCells := SavedLocalCells;
    FRegisters := SavedRegisters;
  end;
end;

function TGocciaVM.ExecuteModule(const AModule: TGocciaBytecodeModule): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
  TopClosure: TGocciaBytecodeClosure;
  SavedModuleSourcePath: string;
  SavedModuleExports: TGocciaValueMap;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create;
  TopClosure := TGocciaBytecodeClosure.Create(AModule.TopLevel);
  SavedModuleSourcePath := FCurrentModuleSourcePath;
  SavedModuleExports := FCurrentModuleExports;
  FCurrentModuleSourcePath := AModule.SourcePath;
  FCurrentModuleExports := TGocciaValueMap.Create;
  try
    try
      Result := ExecuteClosure(TopClosure,
        TGocciaUndefinedLiteralValue.UndefinedValue, EmptyArgs);
    finally
      FCurrentModuleExports.Free;
      FCurrentModuleExports := SavedModuleExports;
      FCurrentModuleSourcePath := SavedModuleSourcePath;
    end;
  finally
    TopClosure.Free;
    EmptyArgs.Free;
  end;
end;

function TGocciaVM.ExecuteFunction(const ATemplate: TGocciaFunctionTemplate): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
  TopClosure: TGocciaBytecodeClosure;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create;
  TopClosure := TGocciaBytecodeClosure.Create(ATemplate);
  try
    Result := ExecuteClosure(TopClosure,
      TGocciaUndefinedLiteralValue.UndefinedValue, EmptyArgs);
  finally
    TopClosure.Free;
    EmptyArgs.Free;
  end;
end;

end.
