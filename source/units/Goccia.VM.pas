unit Goccia.VM;

{$I Goccia.inc}
{$POINTERMATH ON}

interface

uses
  Math,

  Goccia.Arguments.Collection,
  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.Evaluator.Context,
  Goccia.Modules,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.CallFrame,
  Goccia.VM.Closure,
  Goccia.VM.Exception,
  Goccia.VM.Registers,
  Goccia.VM.Upvalue;

type
  TGocciaVMLiteralObjectValue = class(TGocciaObjectValue)
  private
    FFastLiteralMode: Boolean;
  public
    constructor Create(const APrototype: TGocciaObjectValue = nil;
      const APropertyCapacity: Integer = 0);
    function TryGetOwnDataPropertyFastRegister(const AName: string;
      out AValue: TGocciaRegister): Boolean;
    function TryGetOwnDataPropertyFast(const AName: string;
      out AValue: TGocciaValue): Boolean;
    function TrySetLiteralDataPropertyFast(const AName: string;
      const AValue: TGocciaValue): Boolean;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    procedure Freeze; override;
    procedure Seal; override;
    procedure PreventExtensions; override;
  end;

  TGocciaBytecodeCellArray = array of TGocciaBytecodeCell;
  PGocciaBytecodeCell = ^TGocciaBytecodeCell;
  TGocciaArgumentsPoolArray = array of TGocciaArgumentsCollection;
  TGocciaBytecodeGeneratorResumeKind = (bgrkNext, bgrkReturn, bgrkThrow);
  TGocciaBytecodeGeneratorState = (bgsSuspendedStart, bgsSuspendedYield,
    bgsExecuting, bgsCompleted);

  TGocciaVM = class
  private
    FRegisterStack: TGocciaRegisterArray;
    FRegisterBase: Integer;
    FRegisters: PGocciaRegister;
    FRegisterCount: Integer;
    FLocalCellStack: TGocciaBytecodeCellArray;
    FLocalCellBase: Integer;
    FLocalCells: PGocciaBytecodeCell;
    FLocalCellCount: Integer;
    FArgCount: Integer;
    FGlobalScope: TGocciaScope;
    FLoadModule: TLoadModuleCallback;
    FCurrentClosure: TGocciaBytecodeClosure;
    FHandlerStack: TGocciaBytecodeHandlerStack;
    FFrameDepth: Integer;
    FFrameStack: array of TGocciaVMCallFrame;
    FFrameStackCount: Integer;
    FCurrentModuleSourcePath: string;
    FCurrentModuleExports: TGocciaValueMap;
    FActiveDecoratorSession: TObject;
    FCoverageEnabled: Boolean;
    FProfilingOpcodes: Boolean;
    FProfilingFunctions: Boolean;
    FPreviousExceptionMask: TFPUExceptionMask;
    FArgumentPool: TGocciaArgumentsPoolArray;
    FArgumentPoolCount: Integer;
    function ConstantToValue(const AConstant: TGocciaBytecodeConstant): TGocciaValue;
    // ES2026 §13.2.8.3 GetTemplateObject — lazy-build helper for bckTemplateObject constants.
    function BuildTemplateObjectConstant(const ATemplate: TGocciaFunctionTemplate;
      const AConstantIndex: Integer): TGocciaValue;
    function AcquireArguments(const ACapacity: Integer = 0): TGocciaArgumentsCollection;
    procedure ReleaseArguments(const AArguments: TGocciaArgumentsCollection);
    procedure AcquireRegisters(const ACount: Integer);
    procedure AcquireLocalCells(const ACount: Integer);
    procedure EnsureRegisterCapacity(const ACount: Integer);
    procedure EnsureLocalCapacity(const ACount: Integer);
    function GetLocalCell(const AIndex: Integer): TGocciaBytecodeCell;
    function GetLocalRegister(const AIndex: Integer): TGocciaRegister; inline;
    function GetRegister(const AIndex: Integer): TGocciaValue; inline;
    function GetRegisterFast(const AIndex: Integer): TGocciaValue; inline;
    procedure SetRegister(const AIndex: Integer; const AValue: TGocciaValue); inline;
    procedure SetRegisterFast(const AIndex: Integer; const AValue: TGocciaValue); inline;
    procedure SetRegisterRaw(const AIndex: Integer; const AValue: TGocciaRegister); inline;
    procedure InstallFunctionPrototype(const AFunction: TGocciaObjectValue;
      const AIsGenerator: Boolean);
    function GetLocal(const AIndex: Integer): TGocciaValue; inline;
    function GetLocalFast(const AIndex: Integer): TGocciaValue; inline;
    procedure SetLocal(const AIndex: Integer; const AValue: TGocciaValue); inline;
    procedure SetLocalFast(const AIndex: Integer; const AValue: TGocciaValue); inline;
    procedure SetLocalRaw(const AIndex: Integer; const AValue: TGocciaRegister); inline;
    function MatchesNullishKind(const AValue: TGocciaValue; const AKind: UInt8): Boolean;
    function TryGetArrayIndex(const AKey: TGocciaValue; out AIndex: Integer): Boolean;
    function TryGetArrayIndexRegister(const AKey: TGocciaRegister;
      out AIndex: Integer): Boolean;
    function KeyToPropertyName(const AKey: TGocciaValue): string;
    function KeyToPropertyNameRegister(const AKey: TGocciaRegister): string;
    // ALimit semantics:
    //   ALimit < 0 → unbounded (drain until iterator returns done:true);
    //   ALimit = 0 → consume zero elements (used for `const [] = iter`);
    //   ALimit > 0 → consume exactly ALimit elements then close.
    // Default is unbounded so existing spread / Iterator.from / rest
    // call sites preserve their historical behaviour without explicit
    // arguments.
    function IterableToArray(const AIterable: TGocciaValue;
      const ATryAsync: Boolean = False;
      const ALimit: Integer = -1): TGocciaArrayValue;
    function TryIterableToArray(const AIterable: TGocciaValue;
      out AArray: TGocciaArrayValue): Boolean;
    procedure SpreadObjectIntoValue(const ATarget: TGocciaObjectValue;
      const ASource: TGocciaValue);
    function ObjectRestValue(const ASource: TGocciaValue;
      const AExclusionKeys: TGocciaArrayValue): TGocciaObjectValue;
    function GetIteratorValue(const AIterable: TGocciaValue;
      const ATryAsync: Boolean): TGocciaValue;
    function ConstructValue(const AConstructor: TGocciaValue;
      const AArguments: TGocciaArgumentsCollection): TGocciaValue;
    function ImportModuleValue(const APath: string): TGocciaValue; overload;
    function ImportModuleValue(const APath, AReferrer: string): TGocciaValue; overload;
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
    procedure DefineGlobalBinding(const AName: string;
      const AValue: TGocciaValue;
      const ADeclarationType: TGocciaDeclarationType);
    function FinalizeEnumValue(const AValue: TGocciaValue; const AName: string): TGocciaValue;
    procedure SetupAutoAccessorValue(const AName: string);
    procedure RunClassInitializers(const AClassValue: TGocciaClassValue;
      const AInstance: TGocciaValue);
    function MaterializeArguments(
      const AArguments: TGocciaRegisterArray): TGocciaArgumentsCollection;
    procedure InvokeImplicitSuperInitialization(const AClassValue: TGocciaClassValue;
      const AInstance: TGocciaValue; const AArguments: TGocciaArgumentsCollection);
    procedure InvokeImplicitSuperInitializationRegisters(
      const AClassValue: TGocciaClassValue; const AInstance: TGocciaValue;
      const AArguments: TGocciaRegisterArray);
    procedure BeginDecorators(const AClassValue, ASuperValue: TGocciaValue);
    procedure ApplyElementDecorator(const ADecoratorFn: TGocciaValue;
      const ADescriptor: string);
    procedure ApplyClassDecorator(const ADecoratorFn: TGocciaValue);
    function FinishDecorators(const ACurrentValue: TGocciaValue): TGocciaValue;
    function GetSuperPropertyValue(const ASuperValue, AThisValue: TGocciaValue;
      const AName: string): TGocciaValue;
    function GetSuperPropertyValueByKey(const ASuperValue, AThisValue,
      AKey: TGocciaValue): TGocciaValue;
    function GetPropertyValue(const AObject: TGocciaValue; const AKey: string): TGocciaValue;
    procedure SetPropertyValue(const AObject: TGocciaValue; const AKey: string;
      const AValue: TGocciaValue);
    function TryGetRawPrivateValue(const AObject: TGocciaValue; const AKey: string;
      out AValue: TGocciaValue): Boolean;
    procedure SetRawPrivateValue(const AObject: TGocciaValue; const AKey: string;
      const AValue: TGocciaValue);
    function HasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
    function MatchHasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
    function MatchExtractorValue(const ASubject, AMatcher: TGocciaValue): TGocciaValue;
    function InvokeFunctionValue(const ACallee: TGocciaValue;
      const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure PushFrame(const AResultRegister, AFrameIP: Integer;
      const ATemplate: TGocciaFunctionTemplate;
      const APrevCovLine: UInt32; const AProfileTimestamp: Int64);
    function PopFrame(var AFrame: TGocciaVMCallFrame;
      out ATemplate: TGocciaFunctionTemplate;
      out APrevCovLine: UInt32; out AProfileTimestamp: Int64): Integer;
    procedure TeardownCurrentFrame(const ATemplate: TGocciaFunctionTemplate;
      const AProfileTimestamp: Int64; const ATargetHandlerCount: Integer);
    procedure SetupNewFrame(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray;
      const AArgCount: Integer; const AArg0, AArg1, AArg2: TGocciaRegister;
      const AUseFixedArgs: Boolean;
      var AFrame: TGocciaVMCallFrame; out ATemplate: TGocciaFunctionTemplate;
      out APrevCovLine: UInt32; out AProfileTimestamp: Int64);
    procedure HandleExceptionUnwind(const AErrorValue: TGocciaValue;
      const AInitialFrameStackCount, ASavedHandlerCount: Integer;
      var AFrame: TGocciaVMCallFrame; var ATemplate: TGocciaFunctionTemplate;
      var APrevCovLine: UInt32; var AProfileTimestamp: Int64);
    function ExecuteClosureRegistersInternal(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray;
      const AArgCount: Integer; const AArg0, AArg1, AArg2: TGocciaRegister;
      const AUseFixedArgs: Boolean): TGocciaRegister;
    function ExecuteClosureRegisters0(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister): TGocciaRegister;
    function ExecuteClosureRegisters1(const AClosure: TGocciaBytecodeClosure;
      const AThisValue, AArg0: TGocciaRegister): TGocciaRegister;
    function ExecuteClosureRegisters2(const AClosure: TGocciaBytecodeClosure;
      const AThisValue, AArg0, AArg1: TGocciaRegister): TGocciaRegister;
    function ExecuteClosureRegisters3(const AClosure: TGocciaBytecodeClosure;
      const AThisValue, AArg0, AArg1, AArg2: TGocciaRegister): TGocciaRegister;
    function ExecuteClosureRegisters(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray): TGocciaRegister;
    function ExecuteClosure(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection): TGocciaValue;
  public
    constructor Create;
    destructor Destroy; override;
    function ExecuteFunction(const ATemplate: TGocciaFunctionTemplate): TGocciaValue;
    function ExecuteModule(const AModule: TGocciaBytecodeModule): TGocciaValue;
    property GlobalScope: TGocciaScope read FGlobalScope write FGlobalScope;
    property LoadModule: TLoadModuleCallback read FLoadModule write FLoadModule;
    property CoverageEnabled: Boolean read FCoverageEnabled write FCoverageEnabled;
    property ProfilingOpcodes: Boolean read FProfilingOpcodes write FProfilingOpcodes;
    property ProfilingFunctions: Boolean read FProfilingFunctions write FProfilingFunctions;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,

  BigInteger,
  TimingUtils,

  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Coverage,
  Goccia.DisposalTracker,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator,
  Goccia.Evaluator.Decorators,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.InstructionLimit,
  Goccia.PatternMatching,
  Goccia.Profiler,
  Goccia.StackLimit,
  Goccia.Timeout,
  Goccia.Types.Enforcement,
  Goccia.Utils,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.EnumValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.PromiseValue,
  Goccia.Values.ProxyValue,
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

function VMHasSymbolPropertyInChain(const AObject: TGocciaObjectValue;
  const ASymbol: TGocciaSymbolValue): Boolean; inline;
var
  Current: TGocciaObjectValue;
begin
  Current := AObject;
  while Assigned(Current) do
  begin
    if Current.HasSymbolProperty(ASymbol) then
      Exit(True);
    Current := Current.Prototype;
  end;
  Result := False;
end;

function VMGetOwnDataDescriptorValue(const AObject: TGocciaObjectValue;
  const AName: string; out AValue: TGocciaValue): Boolean; inline;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := Assigned(AObject) and
    AObject.Properties.TryGetValue(AName, Descriptor) and
    (Descriptor is TGocciaPropertyDescriptorData);
  if Result then
    AValue := TGocciaPropertyDescriptorData(Descriptor).Value
  else
    AValue := nil;
end;

function VMValueToRegisterFast(const AValue: TGocciaValue): TGocciaRegister; inline;
var
  NumberValue: Double;
  Bits: Int64 absolute NumberValue;
begin
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(RegisterUndefined);
  if AValue is TGocciaNullLiteralValue then
    Exit(RegisterNull);
  if AValue = TGocciaHoleValue.HoleValue then
    Exit(RegisterHole);
  if AValue = TGocciaBooleanLiteralValue.TrueValue then
    Exit(RegisterBoolean(True));
  if AValue = TGocciaBooleanLiteralValue.FalseValue then
    Exit(RegisterBoolean(False));
  if AValue is TGocciaNumberLiteralValue then
  begin
    NumberValue := TGocciaNumberLiteralValue(AValue).Value;
    if NumberValue = 0.0 then
    begin
      if Bits < 0 then
        Exit(RegisterObject(AValue));
      Exit(RegisterInt(0));
    end;
    if NumberValue = 1.0 then
      Exit(RegisterInt(1));
    if (not TGocciaNumberLiteralValue(AValue).IsNaN) and
       (not TGocciaNumberLiteralValue(AValue).IsInfinite) and
       (Frac(NumberValue) = 0.0) and
       (NumberValue >= Low(LongInt)) and
       (NumberValue <= High(LongInt)) then
      Exit(RegisterInt(Trunc(NumberValue)));
    Exit(RegisterFloat(NumberValue));
  end;
  Result := RegisterObject(AValue);
end;

function VMGetOwnDataDescriptorRegister(const AObject: TGocciaObjectValue;
  const AName: string; out AValue: TGocciaRegister): Boolean; inline;
var
  Value: TGocciaValue;
begin
  Result := VMGetOwnDataDescriptorValue(AObject, AName, Value);
  if not Result then
  begin
    AValue := RegisterUndefined;
    Exit;
  end;

  AValue := VMValueToRegisterFast(Value);
end;

function VMSetOwnWritableDataDescriptorValue(const AObject: TGocciaObjectValue;
  const AName: string; const AValue: TGocciaValue): Boolean; inline;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := Assigned(AObject) and
    AObject.Properties.TryGetValue(AName, Descriptor) and
    (Descriptor is TGocciaPropertyDescriptorData);
  if not Result then
    Exit;
  if not TGocciaPropertyDescriptorData(Descriptor).Writable then
    ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]),
      SSuggestReadOnlyProperty);
  TGocciaPropertyDescriptorData(Descriptor).Value := AValue;
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
  if AValue = 1.0 then
    Exit(TGocciaNumberLiteralValue.OneValue);
  Result := TGocciaNumberLiteralValue.Create(AValue);
end;

function VMNumberRegister(const AValue: Double): TGocciaRegister; inline;
var
  Bits: Int64 absolute AValue;
begin
  if Math.IsNaN(AValue) or Math.IsInfinite(AValue) then
    Exit(RegisterObject(VMNumberValue(AValue)));
  if AValue = 0.0 then
  begin
    if Bits < 0 then
      Exit(RegisterObject(TGocciaNumberLiteralValue.NegativeZeroValue));
    Exit(RegisterInt(0));
  end;
  if (AValue >= Low(LongInt)) and (AValue <= High(LongInt)) and
     (Frac(AValue) = 0.0) then
    Exit(RegisterInt(Trunc(AValue)));
  Result := RegisterFloat(AValue);
end;


// ES2026 Types-as-comments: runtime guard for OP_CHECK_TYPE (compiler emits Ord(TGocciaLocalType) in operand B).
// Delegates to Goccia.Types.Enforcement.EnforceStrictType so the interpreter
// and bytecode VM share a single enforcement implementation.
procedure VMStrictTypeCheckRegisterValue(const AValue: TGocciaValue;
  const AExpected: TGocciaLocalType); inline;
begin
  EnforceStrictType(AValue, AExpected);
end;

// Integer-only result: skips IsNaN/IsInfinite/Frac checks that VMNumberRegister
// performs, since integer arithmetic on LongInt-range inputs cannot produce
// NaN, Infinity, negative zero, or fractional results.
// Uses implicit Double assignment (not Int64 * 1.0) to avoid AArch64 FPC 3.2.2
// codegen bug where Int64 * 1.0 produces wrong results near LongInt boundaries.
function VMIntResult(const AValue: Int64): TGocciaRegister; inline;
var
  FloatValue: Double;
begin
  if (AValue >= Low(LongInt)) and (AValue <= High(LongInt)) then
    Result := RegisterInt(AValue)
  else
  begin
    FloatValue := AValue;
    Result := RegisterFloat(FloatValue);
  end;
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
    ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
  if (ALeft is TGocciaBigIntValue) xor (ARight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntMixedTypes, SSuggestBigIntNoMixedArithmetic);
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

// ES2026 §7.2.14 — cross-type BigInt/Number comparison (mathematical-value)
function VMCompareBigIntAndNumber(const ABigInt: TGocciaBigIntValue;
  const ANumber: TGocciaNumberLiteralValue): Integer; inline;
var
  NumVal, FloorVal: Double;
  NumAsBigInt: TBigInteger;
begin
  if ANumber.IsNaN then
    Exit(2); // unordered
  if ANumber.IsInfinity then
    Exit(-1);
  if ANumber.IsNegativeInfinity then
    Exit(1);

  NumVal := ANumber.Value;

  // Fractional Number: compare BigInt against floor/ceil
  if Frac(NumVal) <> 0 then
  begin
    FloorVal := System.Int(NumVal);
    if NumVal > 0 then
    begin
      NumAsBigInt := TBigInteger.FromDouble(FloorVal);
      Result := ABigInt.Value.Compare(NumAsBigInt);
      if Result <= 0 then
        Result := -1
      else
        Result := 1;
    end
    else
    begin
      NumAsBigInt := TBigInteger.FromDouble(FloorVal);
      Result := ABigInt.Value.Compare(NumAsBigInt);
      if Result >= 0 then
        Result := 1
      else
        Result := -1;
    end;
    Exit;
  end;

  // Exact integer Number: convert to BigInt for precise comparison
  NumAsBigInt := TBigInteger.FromDouble(NumVal);
  Result := ABigInt.Value.Compare(NumAsBigInt);
end;

function VMLessThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
var
  Cmp: Integer;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue(ALeft).Value <
      TGocciaStringLiteralValue(ARight).Value);

  // ES2026 §7.2.14 — BigInt comparisons
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue(ALeft).Value.Compare(TGocciaBigIntValue(ARight).Value) < 0);
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    Cmp := VMCompareBigIntAndNumber(TGocciaBigIntValue(ALeft), TGocciaNumberLiteralValue(ARight));
    Exit(Cmp = -1);
  end;
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaBigIntValue) then
  begin
    Cmp := VMCompareBigIntAndNumber(TGocciaBigIntValue(ARight), TGocciaNumberLiteralValue(ALeft));
    Exit(Cmp = 1);
  end;

  Result := VMCompareNumbers(ALeft.ToNumberLiteral, ARight.ToNumberLiteral, False);
end;

function VMGreaterThan(const ALeft, ARight: TGocciaValue): Boolean; inline;
var
  Cmp: Integer;
begin
  if (ALeft is TGocciaUndefinedLiteralValue) or
     (ARight is TGocciaUndefinedLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaNullLiteralValue) and (ARight is TGocciaNullLiteralValue) then
    Exit(False);

  if (ALeft is TGocciaStringLiteralValue) and (ARight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue(ALeft).Value >
      TGocciaStringLiteralValue(ARight).Value);

  // ES2026 §7.2.14 — BigInt comparisons
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue(ALeft).Value.Compare(TGocciaBigIntValue(ARight).Value) > 0);
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaNumberLiteralValue) then
  begin
    Cmp := VMCompareBigIntAndNumber(TGocciaBigIntValue(ALeft), TGocciaNumberLiteralValue(ARight));
    Exit(Cmp = 1);
  end;
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaBigIntValue) then
  begin
    Cmp := VMCompareBigIntAndNumber(TGocciaBigIntValue(ARight), TGocciaNumberLiteralValue(ALeft));
    Exit(Cmp = -1);
  end;

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

  // NaN guard: cross-type BigInt/Number must check the Number operand
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaNumberLiteralValue) and
     TGocciaNumberLiteralValue(ARight).IsNaN then
    Exit(False);
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaBigIntValue) and
     TGocciaNumberLiteralValue(ALeft).IsNaN then
    Exit(False);
  if not ((ALeft is TGocciaBigIntValue) or (ARight is TGocciaBigIntValue)) then
  begin
    LeftNum := ALeft.ToNumberLiteral;
    RightNum := ARight.ToNumberLiteral;
    if LeftNum.IsNaN or RightNum.IsNaN then
      Exit(False);
  end;
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

  // NaN guard: cross-type BigInt/Number must check the Number operand
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaNumberLiteralValue) and
     TGocciaNumberLiteralValue(ARight).IsNaN then
    Exit(False);
  if (ALeft is TGocciaNumberLiteralValue) and (ARight is TGocciaBigIntValue) and
     TGocciaNumberLiteralValue(ALeft).IsNaN then
    Exit(False);
  if not ((ALeft is TGocciaBigIntValue) or (ARight is TGocciaBigIntValue)) then
  begin
    LeftNum := ALeft.ToNumberLiteral;
    RightNum := ARight.ToNumberLiteral;
    if LeftNum.IsNaN or RightNum.IsNaN then
      Exit(False);
  end;
  Result := not VMLessThan(ALeft, ARight);
end;

function VMToECMAStringFast(const AValue: TGocciaValue): TGocciaStringLiteralValue; inline;
var
  PrimitiveValue: TGocciaValue;
begin
  if AValue is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue(AValue));

  if AValue is TGocciaSymbolValue then
    ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);

  if AValue.IsPrimitive then
    Exit(AValue.ToStringLiteral);

  PrimitiveValue := ToPrimitive(AValue, tphString);
  if PrimitiveValue is TGocciaSymbolValue then
    ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
  Result := PrimitiveValue.ToStringLiteral;
end;

function VMRegisterToECMAStringFast(
  const AValue: TGocciaRegister): TGocciaStringLiteralValue; inline;
begin
  case AValue.Kind of
    grkUndefined:
      Exit(TGocciaStringLiteralValue.Create('undefined'));
    grkNull:
      Exit(TGocciaStringLiteralValue.Create('null'));
    grkHole:
      Exit(TGocciaStringLiteralValue.Create('undefined'));
    grkBoolean:
      if AValue.BoolValue then
        Exit(TGocciaStringLiteralValue.Create('true'))
      else
        Exit(TGocciaStringLiteralValue.Create('false'));
    grkInt:
      Exit(TGocciaStringLiteralValue.Create(IntToStr(AValue.IntValue)));
    grkFloat:
      Exit(RegisterToValue(AValue).ToStringLiteral);
    grkObject:
      begin
        if AValue.ObjectValue is TGocciaStringLiteralValue then
          Exit(TGocciaStringLiteralValue(AValue.ObjectValue));
        if AValue.ObjectValue is TGocciaSymbolValue then
          ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
        if Assigned(AValue.ObjectValue) then
          Exit(VMToECMAStringFast(AValue.ObjectValue));
      end;
  end;
  Result := TGocciaStringLiteralValue.Create('');
end;

procedure VMCheckBigIntMixed(const ALeft, ARight: TGocciaValue); inline;
begin
  if (ALeft is TGocciaBigIntValue) xor (ARight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntMixedTypes, SSuggestBigIntNoMixedArithmetic);
end;

// All Number-side bitwise helpers route operands through ToInt32Value /
// ToUint32Value (Goccia.Utils) — the spec-compliant ToInt32 / ToUint32
// implementations.  Using bare Trunc() yields divergent results across
// architectures because FPC's Trunc(NaN) returns Int64.MinValue on
// x86_64 (cvttsd2si "indefinite") but 0 on aarch64; that single-instruction
// difference accounted for ~22 test262 failures on Linux x86_64 CI that
// passed on macOS arm64 / Linux aarch64.
//
// Operands also go through ToPrimitive first (returns the value
// unchanged for primitives — fast path is one virtual IsPrimitive
// call) so boxed BigInts (e.g. `Object(1n)`) unbox to their primitive
// BigInt and take the BigInt branch instead of being silently coerced
// to Number 0/-1 via the boxed object's ToNumberLiteral path.  Per
// ES2026 §13.15.3 / §6.1.6.2 the IsBigInt? check applies to the
// post-ToPrimitive value.

function VMBitwiseAndValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseAnd(TGocciaBigIntValue(PrimRight).Value)));
  VMCheckBigIntMixed(PrimLeft, PrimRight);
  Result := VMNumberValue(ToInt32Value(PrimLeft) and ToInt32Value(PrimRight));
end;

function VMBitwiseOrValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseOr(TGocciaBigIntValue(PrimRight).Value)));
  VMCheckBigIntMixed(PrimLeft, PrimRight);
  Result := VMNumberValue(ToInt32Value(PrimLeft) or ToInt32Value(PrimRight));
end;

function VMBitwiseXorValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.BitwiseXor(TGocciaBigIntValue(PrimRight).Value)));
  VMCheckBigIntMixed(PrimLeft, PrimRight);
  Result := VMNumberValue(ToInt32Value(PrimLeft) xor ToInt32Value(PrimRight));
end;

function VMLeftShiftValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.ShiftLeft(
        TGocciaBigIntValue(PrimRight).Value.ToInt64)));
  VMCheckBigIntMixed(PrimLeft, PrimRight);
  Result := VMNumberValue(
    ToInt32Value(PrimLeft) shl (ToUint32Value(PrimRight) and 31));
end;

function VMRightShiftValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.ShiftRight(
        TGocciaBigIntValue(PrimRight).Value.ToInt64)));
  VMCheckBigIntMixed(PrimLeft, PrimRight);
  Result := VMNumberValue(SarLongint(
    ToInt32Value(PrimLeft), ToUint32Value(PrimRight) and 31));
end;

// ES2026 §6.1.6.2.11 BigInt::unsignedRightShift — always throws
function VMUnsignedRightShiftValues(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
var
  PrimLeft, PrimRight: TGocciaValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);
  if (PrimLeft is TGocciaBigIntValue) or (PrimRight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntUnsignedRightShift, SSuggestBigIntNoMixedArithmetic);
  Result := VMNumberValue(
    ToUint32Value(PrimLeft) shr (ToUint32Value(PrimRight) and 31));
end;

function VMBitwiseNotValue(const AOperand: TGocciaValue): TGocciaValue; inline;
var
  PrimOperand: TGocciaValue;
begin
  PrimOperand := ToPrimitive(AOperand);
  if PrimOperand is TGocciaBigIntValue then
    Exit(TGocciaBigIntValue.Create(TGocciaBigIntValue(PrimOperand).Value.BitwiseNot));
  Result := VMNumberValue(not ToInt32Value(PrimOperand));
end;

function VMGlobalConstructor(const AScope: TGocciaScope;
  const AName: string): TGocciaValue; inline;
var
  RootScope: TGocciaScope;
begin
  RootScope := AScope;
  while Assigned(RootScope) and Assigned(RootScope.Parent) do
    RootScope := RootScope.Parent;

  if Assigned(RootScope) and RootScope.ContainsOwnLexicalBinding(AName) then
    Result := RootScope.GetValue(AName)
  else
    Result := nil;
end;

function VMGlobalObjectConstructor(const AScope: TGocciaScope): TGocciaValue; inline;
begin
  Result := VMGlobalConstructor(AScope, CONSTRUCTOR_OBJECT);
end;

function VMGlobalFunctionConstructor(const AScope: TGocciaScope): TGocciaValue; inline;
begin
  Result := VMGlobalConstructor(AScope, CONSTRUCTOR_FUNCTION);
end;

function VMBuiltinConstructorMatchValue(const AMatcher, ASubject: TGocciaValue;
  const AScope: TGocciaScope; out AMatches: Boolean): Boolean; inline;
var
  ObjectConstructor, ArrayConstructor, StringConstructor, NumberConstructor,
    BooleanConstructor, FunctionConstructor, BigIntConstructor,
    SymbolConstructor: TGocciaValue;
begin
  Result := False;
  AMatches := False;

  ObjectConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_OBJECT);
  if Assigned(ObjectConstructor) and (AMatcher = ObjectConstructor) then
  begin
    AMatches := ASubject is TGocciaObjectValue;
    Exit(True);
  end;

  ArrayConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_ARRAY);
  if Assigned(ArrayConstructor) and (AMatcher = ArrayConstructor) then
  begin
    AMatches := ASubject is TGocciaArrayValue;
    Exit(True);
  end;

  StringConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_STRING);
  if Assigned(StringConstructor) and (AMatcher = StringConstructor) then
  begin
    AMatches := ASubject is TGocciaStringLiteralValue;
    Exit(True);
  end;

  NumberConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_NUMBER);
  if Assigned(NumberConstructor) and (AMatcher = NumberConstructor) then
  begin
    AMatches := ASubject is TGocciaNumberLiteralValue;
    Exit(True);
  end;

  BooleanConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_BOOLEAN);
  if Assigned(BooleanConstructor) and (AMatcher = BooleanConstructor) then
  begin
    AMatches := ASubject is TGocciaBooleanLiteralValue;
    Exit(True);
  end;

  FunctionConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_FUNCTION);
  if Assigned(FunctionConstructor) and (AMatcher = FunctionConstructor) then
  begin
    AMatches := ASubject.IsCallable;
    Exit(True);
  end;

  BigIntConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_BIGINT);
  if Assigned(BigIntConstructor) and (AMatcher = BigIntConstructor) then
  begin
    AMatches := ASubject is TGocciaBigIntValue;
    Exit(True);
  end;

  SymbolConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_SYMBOL);
  if Assigned(SymbolConstructor) and (AMatcher = SymbolConstructor) then
  begin
    AMatches := ASubject is TGocciaSymbolValue;
    Exit(True);
  end;
end;

function VMInstanceOfValue(const ALeft, ARight,
  AObjectConstructor, AFunctionConstructor: TGocciaValue): TGocciaValue; inline;
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
    if Assigned(AObjectConstructor) and (ARight = AObjectConstructor) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    if TGocciaInstanceValue(ALeft).IsInstanceOf(TGocciaClassValue(ARight)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    if VMIsObjectInstanceOfClass(TGocciaObjectValue(ALeft), TGocciaClassValue(ARight)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  if (ALeft is TGocciaFunctionValue) and
     Assigned(AFunctionConstructor) and (ARight = AFunctionConstructor) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaNativeFunctionValue) and
     Assigned(AFunctionConstructor) and (ARight = AFunctionConstructor) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaClassValue) and
     Assigned(AFunctionConstructor) and (ARight = AFunctionConstructor) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaArrayValue) and (ARight is TGocciaArrayClassValue) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaArrayValue) and Assigned(AObjectConstructor) and
     (ARight = AObjectConstructor) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  if (ALeft is TGocciaObjectValue) and Assigned(AObjectConstructor) and
     (ARight = AObjectConstructor) then
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

  // ES2026 §6.1.6.2.1 BigInt::add
  if (ALeft is TGocciaBigIntValue) and (ARight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(ALeft).Value.Add(TGocciaBigIntValue(ARight).Value)));

  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaSymbolValue) or (PrimRight is TGocciaSymbolValue) then
    ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);

  if (PrimLeft is TGocciaStringLiteralValue) or (PrimRight is TGocciaStringLiteralValue) then
    Exit(TGocciaStringLiteralValue.Create(
      PrimLeft.ToStringLiteral.Value + PrimRight.ToStringLiteral.Value));

  // Check for BigInt mixed-type after string check (string + bigint = string concat)
  if (PrimLeft is TGocciaBigIntValue) xor (PrimRight is TGocciaBigIntValue) then
    ThrowTypeError(SErrorBigIntMixedTypes, SSuggestBigIntNoMixedArithmetic);

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
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Subtract(TGocciaBigIntValue(PrimRight).Value)));

  if not VMToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
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
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  LeftZero, RightZero: Boolean;
  SameSign: Boolean;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Multiply(TGocciaBigIntValue(PrimRight).Value)));

  if not VMToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
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
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  SameSign: Boolean;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    if TGocciaBigIntValue(PrimRight).Value.IsZero then
      ThrowRangeError(SErrorBigIntDivisionByZero);
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Divide(TGocciaBigIntValue(PrimRight).Value)));
  end;

  if not VMToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
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
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    if TGocciaBigIntValue(PrimRight).Value.IsZero then
      ThrowRangeError(SErrorBigIntDivisionByZero);
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Modulo(TGocciaBigIntValue(PrimRight).Value)));
  end;

  if not VMToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
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
  PrimLeft, PrimRight: TGocciaValue;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
begin
  PrimLeft := ToPrimitive(ALeft);
  PrimRight := ToPrimitive(ARight);

  if (PrimLeft is TGocciaBigIntValue) and (PrimRight is TGocciaBigIntValue) then
  begin
    if TGocciaBigIntValue(PrimRight).Value.IsNegative then
      ThrowRangeError(SErrorBigIntNegativeExponent);
    Exit(TGocciaBigIntValue.Create(
      TGocciaBigIntValue(PrimLeft).Value.Power(TGocciaBigIntValue(PrimRight).Value)));
  end;

  if not VMToNumericPair(PrimLeft, PrimRight, LeftNum, RightNum) then
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
    function GetSourceText: string; override;
  public
    constructor Create(const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure);
    destructor Destroy; override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallPreparedArgs(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallOneArg(const AArg0, AThisValue: TGocciaValue): TGocciaValue; override;
    function CallTwoArgs(const AArg0, AArg1, AThisValue: TGocciaValue): TGocciaValue; override;
    function CallThreeArgs(const AArg0, AArg1, AArg2, AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  EGocciaBytecodeYield = class(Exception)
  private
    FValue: TGocciaRegister;
    FYieldIndex: Integer;
  public
    constructor Create(const AValue: TGocciaRegister; const AYieldIndex: Integer);
    property Value: TGocciaRegister read FValue;
    property YieldIndex: Integer read FYieldIndex;
  end;

  EGocciaBytecodeGeneratorReturn = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

  TGocciaBytecodeGeneratorObjectValue = class(TGocciaIteratorValue)
  private
    FVM: TGocciaVM;
    FClosure: TGocciaBytecodeClosure;
    FThisValue: TGocciaRegister;
    FArguments: TGocciaRegisterArray;
    FState: TGocciaBytecodeGeneratorState;
    FResumeKind: TGocciaBytecodeGeneratorResumeKind;
    FResumeValue: TGocciaRegister;
    FResumeRegister: UInt8;
    FReturnSentinel: TGocciaValue;
    FReturnValue: TGocciaValue;
    FHasContinuation: Boolean;
    FContinuationIP: Integer;
    FContinuationRegisters: TGocciaRegisterArray;
    FContinuationLocalCells: TGocciaBytecodeCellArray;
    FContinuationHandlers: TGocciaBytecodeHandlerEntryArray;
    FContinuationPrevCovLine: UInt32;
    FDelegateActive: Boolean;
    FDelegateIteratorValue: TGocciaValue;
    FDelegateIterator: TGocciaIteratorValue;
    FDelegateNextMethod: TGocciaValue;
    function ResumeRaw(const AKind: TGocciaBytecodeGeneratorResumeKind;
      const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
    procedure CaptureContinuation(const AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; const APrevCovLine: UInt32;
      const AResumeRegister: UInt8; const AContinuationIP: Integer);
    function RestoreContinuation(var AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; out APrevCovLine: UInt32): Boolean;
    procedure ClearDelegateState;
  public
    constructor Create(const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection);
    constructor CreateRegisters(const AVM: TGocciaVM;
      const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
      const AArguments: TGocciaRegisterArray);
    destructor Destroy; override;
    function AdvanceNext: TGocciaObjectValue; override;
    function AdvanceNextValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function DirectNextValue(const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue; override;
    function ReturnValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function ThrowValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    procedure Close; override;
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure HandleYield(const AValue: TGocciaRegister;
      const AResumeRegister: UInt8; const AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; const APrevCovLine: UInt32;
      const AContinuationIP: Integer);
    procedure HandleYieldDelegate(const AIterable: TGocciaRegister;
      const AResumeRegister: UInt8; const AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; const APrevCovLine: UInt32;
      const AContinuationIP: Integer);
    procedure MarkReferences; override;
    function ToStringTag: string; override;
  end;

  TGocciaBytecodeAsyncGeneratorObjectValue = class(TGocciaObjectValue)
  private
    FInner: TGocciaBytecodeGeneratorObjectValue;
    function ResumeAsPromise(const AKind: TGocciaBytecodeGeneratorResumeKind;
      const AValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection);
    constructor CreateRegisters(const AVM: TGocciaVM;
      const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
      const AArguments: TGocciaRegisterArray);
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncIteratorSelf(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
    function ToStringTag: string; override;
  end;

  TGocciaVMSuperConstructorValue = class(TGocciaFunctionBase)
  private
    FSuperClass: TGocciaValue;
  protected
    function GetFunctionName: string; override;
  public
    constructor Create(const ASuperClass: TGocciaValue);
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
    function CreateNativeInstance(
      const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function GetClassLength: Integer; override;
    function Instantiate(const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaClassValue = nil): TGocciaValue; override;
    function InstantiateRegisters(
      const AArguments: TGocciaRegisterArray): TGocciaRegister;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
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

function TGocciaVMLiteralObjectValue.TryGetOwnDataPropertyFast(
  const AName: string; out AValue: TGocciaValue): Boolean;
begin
  Result := VMGetOwnDataDescriptorValue(Self, AName, AValue);
end;

function TGocciaVMLiteralObjectValue.TryGetOwnDataPropertyFastRegister(
  const AName: string; out AValue: TGocciaRegister): Boolean;
begin
  Result := VMGetOwnDataDescriptorRegister(Self, AName, AValue);
end;

constructor TGocciaVMLiteralObjectValue.Create(const APrototype: TGocciaObjectValue;
  const APropertyCapacity: Integer);
begin
  inherited Create(APrototype, APropertyCapacity);
  FFastLiteralMode := True;
end;

function TGocciaVMLiteralObjectValue.TrySetLiteralDataPropertyFast(
  const AName: string; const AValue: TGocciaValue): Boolean;
begin
  if not FFastLiteralMode then
    Exit(False);

  if VMSetOwnWritableDataDescriptorValue(Self, AName, AValue) then
    Exit(True);

  inherited DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
  Result := True;
end;

function TGocciaVMLiteralObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  if not TryGetOwnDataPropertyFast(AName, Result) then
    Result := inherited GetProperty(AName);
end;

procedure TGocciaVMLiteralObjectValue.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  FFastLiteralMode := False;
  inherited DefineProperty(AName, ADescriptor);
end;

procedure TGocciaVMLiteralObjectValue.Freeze;
begin
  FFastLiteralMode := False;
  inherited Freeze;
end;

procedure TGocciaVMLiteralObjectValue.Seal;
begin
  FFastLiteralMode := False;
  inherited Seal;
end;

procedure TGocciaVMLiteralObjectValue.PreventExtensions;
begin
  FFastLiteralMode := False;
  inherited PreventExtensions;
end;

constructor TGocciaVMClassValue.Create(const AVM: TGocciaVM; const AName: string;
  const ASuperClass: TGocciaClassValue);
begin
  inherited Create(AName, ASuperClass);
  FVM := AVM;
  FConstructorValue := nil;
end;

function TGocciaVMClassValue.GetClassLength: Integer;
var
  Bytecode: TGocciaBytecodeFunctionValue;
begin
  // VM-compiled classes hold their constructor as a bytecode function value
  // rather than the interpreter's TGocciaMethodValue, so the inherited
  // implementation (which only looks at FConstructorMethod) would always
  // return 0. Bridge to the bytecode function's own length.
  if FConstructorValue is TGocciaBytecodeFunctionValue then
  begin
    Bytecode := TGocciaBytecodeFunctionValue(FConstructorValue);
    if Assigned(Bytecode.FClosure) and Assigned(Bytecode.FClosure.Template) then
      Exit(Bytecode.FClosure.Template.FormalParameterCount);
  end;
  Result := inherited GetClassLength;
end;

function TGocciaVMClassValue.CreateNativeInstance(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  ConstructedValue: TGocciaValue;
begin
  Result := inherited CreateNativeInstance(AArguments);
  if Assigned(Result) or not Assigned(NativeSuperConstructor) then
    Exit;

  ConstructedValue := FVM.ConstructValue(NativeSuperConstructor, AArguments);
  if ConstructedValue is TGocciaObjectValue then
    Result := TGocciaObjectValue(ConstructedValue)
  else
  begin
    ThrowTypeError('Superclass constructor did not return an object',
      SSuggestNotConstructorType);
    Result := nil;
  end;
end;

constructor TGocciaVMSuperConstructorValue.Create(
  const ASuperClass: TGocciaValue);
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

threadvar
  GActiveBytecodeGenerator: TGocciaBytecodeGeneratorObjectValue;

type
  TGocciaVMAsyncFromSyncIteratorValue = class(TGocciaObjectValue)
  private
    FIteratorValue: TGocciaValue;
    FNextMethod: TGocciaValue;
    function PromiseResolve(const AValue: TGocciaValue): TGocciaValue;
    function PromiseReject(const AValue: TGocciaValue): TGocciaValue;
    procedure ClearIteratorState;
    procedure CloseIteratorAfterRejectedValue;
    function AwaitIteratorValue(const AValue: TGocciaValue;
      const ADone, ACloseOnRejection: Boolean): TGocciaValue;
    function Next(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReturnValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ThrowValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AIteratorValue, ANextMethod: TGocciaValue);
    procedure MarkReferences; override;
  end;

constructor TGocciaVMAsyncFromSyncIteratorValue.Create(
  const AIteratorValue, ANextMethod: TGocciaValue);
begin
  inherited Create;
  FIteratorValue := AIteratorValue;
  FNextMethod := ANextMethod;
  AssignProperty(PROP_NEXT, TGocciaNativeFunctionValue.Create(Next, PROP_NEXT, 1));
  AssignProperty(PROP_RETURN, TGocciaNativeFunctionValue.Create(ReturnValue, PROP_RETURN, 1));
  AssignProperty(PROP_THROW, TGocciaNativeFunctionValue.Create(ThrowValue, PROP_THROW, 1));
end;

procedure TGocciaVMAsyncFromSyncIteratorValue.ClearIteratorState;
begin
  FIteratorValue := nil;
  FNextMethod := nil;
end;

procedure TGocciaVMAsyncFromSyncIteratorValue.CloseIteratorAfterRejectedValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ReturnMethod: TGocciaValue;
begin
  if not Assigned(FIteratorValue) then
    Exit;
  try
    if FIteratorValue is TGocciaIteratorValue then
      TGocciaIteratorValue(FIteratorValue).Close
    else if FIteratorValue is TGocciaObjectValue then
    begin
      ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
      if Assigned(ReturnMethod) and
         not (ReturnMethod is TGocciaUndefinedLiteralValue) and
         not (ReturnMethod is TGocciaNullLiteralValue) and
         ReturnMethod.IsCallable then
      begin
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          TGocciaFunctionBase(ReturnMethod).Call(CallArgs, FIteratorValue);
        finally
          CallArgs.Free;
        end;
      end;
    end;
  except
    // AsyncFromSyncIteratorContinuation preserves the rejected value when
    // closing after a rejected wrapped value also fails.
  end;
  ClearIteratorState;
end;

function TGocciaVMAsyncFromSyncIteratorValue.AwaitIteratorValue(
  const AValue: TGocciaValue; const ADone, ACloseOnRejection: Boolean): TGocciaValue;
begin
  try
    Result := AwaitValue(AValue);
  except
    if ACloseOnRejection and not ADone then
      CloseIteratorAfterRejectedValue;
    raise;
  end;
end;

function TGocciaVMAsyncFromSyncIteratorValue.PromiseResolve(
  const AValue: TGocciaValue): TGocciaValue;
var
  IsRooted: Boolean;
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  IsRooted := Assigned(TGarbageCollector.Instance);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    Promise.Resolve(AValue);
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
      IsRooted := False;
    end;
    Promise.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Promise);
  Result := Promise;
end;

function TGocciaVMAsyncFromSyncIteratorValue.PromiseReject(
  const AValue: TGocciaValue): TGocciaValue;
var
  IsRooted: Boolean;
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  IsRooted := Assigned(TGarbageCollector.Instance);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    Promise.Reject(AValue);
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
      IsRooted := False;
    end;
    Promise.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Promise);
  Result := Promise;
end;

function TGocciaVMAsyncFromSyncIteratorValue.Next(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaValue;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  try
    if not Assigned(FIteratorValue) then
      Exit(PromiseResolve(CreateIteratorResult(
        TGocciaUndefinedLiteralValue.UndefinedValue, True)));

    if FIteratorValue is TGocciaIteratorValue then
    begin
      if AArgs.Length > 0 then
        Value := TGocciaIteratorValue(FIteratorValue).DirectNextValue(
          AArgs.GetElement(0), Done)
      else
        Value := TGocciaIteratorValue(FIteratorValue).DirectNext(Done);
      if Done then
        ClearIteratorState;
      UnwrappedValue := AwaitIteratorValue(Value, Done, True);
      IteratorResult := CreateIteratorResult(UnwrappedValue, Done);
      Exit(PromiseResolve(IteratorResult));
    end;

    if AArgs.Length > 0 then
      CallArgs := TGocciaArgumentsCollection.Create([AArgs.GetElement(0)])
    else
      CallArgs := TGocciaArgumentsCollection.Create;
    try
      IteratorResult := TGocciaFunctionBase(FNextMethod).Call(CallArgs, FIteratorValue);
    finally
      CallArgs.Free;
    end;

    if not (IteratorResult is TGocciaObjectValue) then
      Exit(PromiseReject(CreateErrorObject(TYPE_ERROR_NAME,
        Format(SErrorIteratorResultNotObject, [IteratorResult.TypeName]))));

    DoneValue := IteratorResult.GetProperty(PROP_DONE);
    Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
    Value := IteratorResult.GetProperty(PROP_VALUE);
    if not Assigned(Value) then
      Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    if Done then
      ClearIteratorState;
    UnwrappedValue := AwaitIteratorValue(Value, Done, True);
    Result := PromiseResolve(CreateIteratorResult(UnwrappedValue, Done));
  except
    on E: EGocciaBytecodeThrow do
      Result := PromiseReject(E.ThrownValue);
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: TGocciaRuntimeError do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
end;

function TGocciaVMAsyncFromSyncIteratorValue.ReturnValue(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaValue;
  ReturnMethod: TGocciaValue;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  try
    if not Assigned(FIteratorValue) then
    begin
      UnwrappedValue := AwaitIteratorValue(Value, True, False);
      Exit(PromiseResolve(CreateIteratorResult(UnwrappedValue, True)));
    end;

    if FIteratorValue is TGocciaIteratorValue then
    begin
      IteratorResult := TGocciaIteratorValue(FIteratorValue).ReturnValue(Value);
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      Value := IteratorResult.GetProperty(PROP_VALUE);
      if not Assigned(Value) then
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Done then
        ClearIteratorState;
      UnwrappedValue := AwaitIteratorValue(Value, Done, False);
      Exit(PromiseResolve(CreateIteratorResult(UnwrappedValue, Done)));
    end;

    ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
    if not Assigned(ReturnMethod) or
       (ReturnMethod is TGocciaUndefinedLiteralValue) or
       (ReturnMethod is TGocciaNullLiteralValue) then
    begin
      ClearIteratorState;
      UnwrappedValue := AwaitIteratorValue(Value, True, False);
      Exit(PromiseResolve(CreateIteratorResult(UnwrappedValue, True)));
    end;
    if not ReturnMethod.IsCallable then
      ThrowTypeError('Iterator return is not callable');

    CallArgs := TGocciaArgumentsCollection.Create([Value]);
    try
      IteratorResult := TGocciaFunctionBase(ReturnMethod).Call(CallArgs, FIteratorValue);
    finally
      CallArgs.Free;
    end;
    if not (IteratorResult is TGocciaObjectValue) then
      ThrowTypeError('Iterator return result is not an object');
    DoneValue := IteratorResult.GetProperty(PROP_DONE);
    Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
    Value := IteratorResult.GetProperty(PROP_VALUE);
    if not Assigned(Value) then
      Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    if Done then
      ClearIteratorState;
    UnwrappedValue := AwaitIteratorValue(Value, Done, False);
    Result := PromiseResolve(CreateIteratorResult(UnwrappedValue, Done));
  except
    on E: EGocciaBytecodeThrow do
      Result := PromiseReject(E.ThrownValue);
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: TGocciaRuntimeError do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
end;

function TGocciaVMAsyncFromSyncIteratorValue.ThrowValue(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaValue;
  ReturnMethod: TGocciaValue;
  ThrowMethod: TGocciaValue;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  try
    if not Assigned(FIteratorValue) then
      raise TGocciaThrowValue.Create(Value);

    if FIteratorValue is TGocciaIteratorValue then
    begin
      IteratorResult := TGocciaIteratorValue(FIteratorValue).ThrowValue(Value);
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      Value := IteratorResult.GetProperty(PROP_VALUE);
      if not Assigned(Value) then
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Done then
        ClearIteratorState;
      UnwrappedValue := AwaitIteratorValue(Value, Done, True);
      Exit(PromiseResolve(CreateIteratorResult(UnwrappedValue, Done)));
    end;

    ThrowMethod := FIteratorValue.GetProperty(PROP_THROW);
    if Assigned(ThrowMethod) and
       not (ThrowMethod is TGocciaUndefinedLiteralValue) and
       not (ThrowMethod is TGocciaNullLiteralValue) then
    begin
      if not ThrowMethod.IsCallable then
        ThrowTypeError('Iterator throw is not callable');
      CallArgs := TGocciaArgumentsCollection.Create([Value]);
      try
        IteratorResult := TGocciaFunctionBase(ThrowMethod).Call(CallArgs, FIteratorValue);
      finally
        CallArgs.Free;
      end;
      if not (IteratorResult is TGocciaObjectValue) then
        ThrowTypeError('Iterator throw result is not an object');
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      Value := IteratorResult.GetProperty(PROP_VALUE);
      if not Assigned(Value) then
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Done then
        ClearIteratorState;
      UnwrappedValue := AwaitIteratorValue(Value, Done, True);
      Exit(PromiseResolve(CreateIteratorResult(UnwrappedValue, Done)));
    end;

    ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
    if Assigned(ReturnMethod) and
       not (ReturnMethod is TGocciaUndefinedLiteralValue) and
       not (ReturnMethod is TGocciaNullLiteralValue) then
    begin
      if not ReturnMethod.IsCallable then
        ThrowTypeError('Iterator return is not callable');
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        IteratorResult := TGocciaFunctionBase(ReturnMethod).Call(CallArgs, FIteratorValue);
      finally
        CallArgs.Free;
      end;
      if not (IteratorResult is TGocciaObjectValue) then
        ThrowTypeError('Iterator return result is not an object');
    end;
    ClearIteratorState;
    ThrowTypeError('Iterator throw is not callable');
  except
    on E: EGocciaBytecodeThrow do
      Result := PromiseReject(E.ThrownValue);
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: TGocciaRuntimeError do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
end;

procedure TGocciaVMAsyncFromSyncIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FIteratorValue) then
    FIteratorValue.MarkReferences;
  if Assigned(FNextMethod) then
    FNextMethod.MarkReferences;
end;

function VMArgumentOrUndefined(const AArgs: TGocciaArgumentsCollection): TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Result := AArgs.GetElement(0)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function VMRejectedTypeErrorPromise(const AMessage: string): TGocciaPromiseValue;
var
  IsRooted: Boolean;
begin
  Result := TGocciaPromiseValue.Create;
  IsRooted := Assigned(TGarbageCollector.Instance);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Result);
  try
    Result.Reject(CreateErrorObject(TYPE_ERROR_NAME, AMessage));
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Result);
      IsRooted := False;
    end;
    Result.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Result);
end;

function VMGeneratorTypeError: TGocciaValue;
begin
  Result := CreateErrorObject(TYPE_ERROR_NAME,
    'Generator method called on incompatible receiver');
end;

function VMGeneratorExecutingError: TGocciaValue;
begin
  Result := CreateErrorObject(TYPE_ERROR_NAME, 'Generator is already executing');
end;

constructor EGocciaBytecodeYield.Create(const AValue: TGocciaRegister;
  const AYieldIndex: Integer);
begin
  inherited Create('');
  FValue := AValue;
  FYieldIndex := AYieldIndex;
end;

constructor EGocciaBytecodeGeneratorReturn.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

constructor TGocciaBytecodeGeneratorObjectValue.Create(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection);
var
  I: Integer;
begin
  inherited Create;
  FVM := AVM;
  FClosure := AClosure.Clone;
  FThisValue := VMValueToRegisterFast(AThisValue);
  if Assigned(AArguments) then
  begin
    SetLength(FArguments, AArguments.Length);
    for I := 0 to AArguments.Length - 1 do
      FArguments[I] := VMValueToRegisterFast(AArguments.GetElement(I));
  end;
  FState := bgsSuspendedStart;
  FResumeValue := RegisterUndefined;
  DefineProperty(PROP_NEXT, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(GeneratorNext, PROP_NEXT, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_RETURN, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(GeneratorReturn, PROP_RETURN, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_THROW, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(GeneratorThrow, PROP_THROW, 1),
    [pfConfigurable, pfWritable]));
end;

destructor TGocciaBytecodeGeneratorObjectValue.Destroy;
begin
  FClosure.Free;
  inherited;
end;

constructor TGocciaBytecodeGeneratorObjectValue.CreateRegisters(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
  const AArguments: TGocciaRegisterArray);
var
  I: Integer;
begin
  inherited Create;
  FVM := AVM;
  FClosure := AClosure.Clone;
  FThisValue := AThisValue;
  SetLength(FArguments, Length(AArguments));
  for I := 0 to High(AArguments) do
    FArguments[I] := AArguments[I];
  FState := bgsSuspendedStart;
  FResumeValue := RegisterUndefined;
  DefineProperty(PROP_NEXT, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(GeneratorNext, PROP_NEXT, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_RETURN, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(GeneratorReturn, PROP_RETURN, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_THROW, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(GeneratorThrow, PROP_THROW, 1),
    [pfConfigurable, pfWritable]));
end;

procedure TGocciaBytecodeGeneratorObjectValue.CaptureContinuation(
  const AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  const APrevCovLine: UInt32; const AResumeRegister: UInt8;
  const AContinuationIP: Integer);
var
  I: Integer;
  LiveLocalCellCount: Integer;
  LiveRegisterCount: Integer;
begin
  FHasContinuation := True;
  FContinuationIP := AContinuationIP;
  FContinuationPrevCovLine := APrevCovLine;
  FResumeRegister := AResumeRegister;

  LiveLocalCellCount := FVM.FLocalCellCount;
  while (LiveLocalCellCount > 0) and
        not Assigned(FVM.FLocalCells[LiveLocalCellCount - 1]) do
    Dec(LiveLocalCellCount);

  LiveRegisterCount := FVM.FRegisterCount;
  while (LiveRegisterCount > 0) and
        (FVM.FRegisters[LiveRegisterCount - 1].Kind = grkUndefined) and
        (LiveRegisterCount > LiveLocalCellCount) do
    Dec(LiveRegisterCount);

  SetLength(FContinuationRegisters, LiveRegisterCount);
  for I := 0 to High(FContinuationRegisters) do
    FContinuationRegisters[I] := FVM.FRegisters[I];

  SetLength(FContinuationLocalCells, LiveLocalCellCount);
  for I := 0 to High(FContinuationLocalCells) do
    FContinuationLocalCells[I] := FVM.FLocalCells[I];

  FVM.FHandlerStack.CopyFrom(AHandlerBaseCount, FContinuationHandlers);
end;

function TGocciaBytecodeGeneratorObjectValue.RestoreContinuation(
  var AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  out APrevCovLine: UInt32): Boolean;
var
  I: Integer;
begin
  Result := FHasContinuation;
  if not Result then
    Exit;

  FHasContinuation := False;
  FVM.EnsureRegisterCapacity(Length(FContinuationRegisters));
  for I := 0 to High(FContinuationRegisters) do
    FVM.FRegisters[I] := FContinuationRegisters[I];
  FVM.EnsureLocalCapacity(Length(FContinuationLocalCells));
  for I := 0 to High(FContinuationLocalCells) do
    FVM.FLocalCells[I] := FContinuationLocalCells[I];

  while FVM.FHandlerStack.Count > AHandlerBaseCount do
    FVM.FHandlerStack.Pop;
  FVM.FHandlerStack.RestoreFrom(FContinuationHandlers);

  AFrame.IP := FContinuationIP;
  APrevCovLine := FContinuationPrevCovLine;
  SetLength(FContinuationRegisters, 0);
  SetLength(FContinuationLocalCells, 0);
  SetLength(FContinuationHandlers, 0);
end;

procedure TGocciaBytecodeGeneratorObjectValue.ClearDelegateState;
begin
  FDelegateActive := False;
  FDelegateIteratorValue := nil;
  FDelegateIterator := nil;
  FDelegateNextMethod := nil;
end;

procedure TGocciaBytecodeGeneratorObjectValue.HandleYield(
  const AValue: TGocciaRegister; const AResumeRegister: UInt8;
  const AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  const APrevCovLine: UInt32; const AContinuationIP: Integer);
begin
  ClearDelegateState;
  CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
    AResumeRegister, AContinuationIP);
  raise EGocciaBytecodeYield.Create(AValue, 0);
end;

procedure TGocciaBytecodeGeneratorObjectValue.HandleYieldDelegate(
  const AIterable: TGocciaRegister; const AResumeRegister: UInt8;
  const AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  const APrevCovLine: UInt32; const AContinuationIP: Integer);
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorValue: TGocciaValue;
  NextResult: TGocciaValue;
  ReturnMethod: TGocciaValue;
  YieldedValue: TGocciaValue;
  HadDelegateContinuation: Boolean;

  procedure ClearDelegateAndThrowTypeError(const AMessage: string);
  begin
    ClearDelegateState;
    ThrowTypeError(AMessage);
  end;

  procedure ClearDelegateAndThrowTypeErrorWithSuggestion(
    const AMessage, ASuggestion: string);
  begin
    ClearDelegateState;
    ThrowTypeError(AMessage, ASuggestion);
  end;

  function AwaitDelegateResult(const AResult: TGocciaValue): TGocciaValue;
  begin
    Result := AResult;
    if Assigned(FClosure) and Assigned(FClosure.Template) and
       FClosure.Template.IsAsync then
      Result := AwaitValue(Result);
  end;

  function CallDelegateMethod(const AMethod, AThisValue: TGocciaValue;
    const AArgs: TGocciaArgumentsCollection): TGocciaValue;
  begin
    try
      Result := AwaitDelegateResult(TGocciaFunctionBase(AMethod).Call(
        AArgs, AThisValue));
    except
      ClearDelegateState;
      raise;
    end;
  end;

  procedure CloseDelegateForMissingThrow;
  begin
    ReturnMethod := nil;
    if Assigned(FDelegateIteratorValue) then
      ReturnMethod := FDelegateIteratorValue.GetProperty(PROP_RETURN);
    if Assigned(ReturnMethod) and
       not (ReturnMethod is TGocciaUndefinedLiteralValue) and
       not (ReturnMethod is TGocciaNullLiteralValue) then
    begin
      if not ReturnMethod.IsCallable then
      begin
        ClearDelegateState;
        ThrowTypeError('Iterator return is not callable');
      end;
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        ReturnMethod := CallDelegateMethod(ReturnMethod,
          FDelegateIteratorValue, CallArgs);
      finally
        CallArgs.Free;
      end;
      if not (ReturnMethod is TGocciaObjectValue) then
      begin
        ClearDelegateState;
        ThrowTypeError('Iterator return result is not an object');
      end;
    end;
    ClearDelegateState;
    ThrowTypeError('Iterator throw is not callable');
  end;
begin
  try
    HadDelegateContinuation := FDelegateActive;
    if not FDelegateActive then
    begin
      IteratorValue := FVM.GetIteratorValue(RegisterToValue(AIterable),
        Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync);
      if not Assigned(IteratorValue) then
        Exit;

      FDelegateIteratorValue := IteratorValue;
      FDelegateIterator := nil;
      FDelegateNextMethod := nil;
      if IteratorValue is TGocciaIteratorValue then
        FDelegateIterator := TGocciaIteratorValue(IteratorValue)
      else if IteratorValue is TGocciaObjectValue then
        FDelegateNextMethod := IteratorValue.GetProperty(PROP_NEXT);
      FDelegateActive := True;
    end;

    if FResumeKind = bgrkReturn then
    begin
      NextResult := nil;
      FReturnValue := nil;
      if Assigned(FDelegateIterator) then
        NextResult := FDelegateIterator.ReturnValue(RegisterToValue(FResumeValue))
      else if Assigned(FDelegateIteratorValue) then
      begin
        NextResult := FDelegateIteratorValue.GetProperty(PROP_RETURN);
        if Assigned(NextResult) and
           not (NextResult is TGocciaUndefinedLiteralValue) and
           not (NextResult is TGocciaNullLiteralValue) then
        begin
          if not NextResult.IsCallable then
            ClearDelegateAndThrowTypeError('Iterator return is not callable');
          CallArgs := TGocciaArgumentsCollection.Create([RegisterToValue(FResumeValue)]);
          try
            NextResult := CallDelegateMethod(NextResult,
              FDelegateIteratorValue, CallArgs);
          finally
            CallArgs.Free;
          end;
        end
        else
          NextResult := nil;
      end;
      if Assigned(NextResult) then
      begin
        if not (NextResult is TGocciaObjectValue) then
          ClearDelegateAndThrowTypeError('Iterator return result is not an object');
        DoneValue := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
        YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
        if not Assigned(YieldedValue) then
          YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if not Assigned(DoneValue) or not DoneValue.ToBooleanLiteral.Value then
        begin
          CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
            AResumeRegister, AContinuationIP);
          raise EGocciaBytecodeYield.Create(
            VMValueToRegisterFast(YieldedValue), 0);
        end;
        FReturnValue := YieldedValue;
      end;
      ClearDelegateState;
      if not Assigned(FReturnValue) then
        FReturnValue := RegisterToValue(FResumeValue);
      if not Assigned(FReturnSentinel) then
        FReturnSentinel := TGocciaObjectValue.Create;
      raise EGocciaBytecodeThrow.Create(FReturnSentinel);
    end;

    if FResumeKind = bgrkThrow then
    begin
      NextResult := nil;
      if Assigned(FDelegateIterator) then
        NextResult := FDelegateIterator.ThrowValue(RegisterToValue(FResumeValue))
      else if Assigned(FDelegateIteratorValue) then
      begin
        NextResult := FDelegateIteratorValue.GetProperty(PROP_THROW);
        if Assigned(NextResult) and
           not (NextResult is TGocciaUndefinedLiteralValue) and
           not (NextResult is TGocciaNullLiteralValue) then
        begin
          if not NextResult.IsCallable then
            CloseDelegateForMissingThrow;
          CallArgs := TGocciaArgumentsCollection.Create([RegisterToValue(FResumeValue)]);
          try
            NextResult := CallDelegateMethod(NextResult,
              FDelegateIteratorValue, CallArgs);
          finally
            CallArgs.Free;
          end;
        end
        else
          CloseDelegateForMissingThrow;
      end;
      if Assigned(NextResult) then
      begin
        if not (NextResult is TGocciaObjectValue) then
          ClearDelegateAndThrowTypeError('Iterator throw result is not an object');
        DoneValue := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
        if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
        begin
          YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
          if not Assigned(YieldedValue) then
            YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          FVM.FRegisters[AResumeRegister] := VMValueToRegisterFast(YieldedValue);
          ClearDelegateState;
          Exit;
        end;
        YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
        if not Assigned(YieldedValue) then
          YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
          AResumeRegister, AContinuationIP);
        raise EGocciaBytecodeYield.Create(VMValueToRegisterFast(YieldedValue), 0);
      end;
      ClearDelegateState;
      raise EGocciaBytecodeThrow.Create(RegisterToValue(FResumeValue));
    end;

    while True do
    begin
      if Assigned(FDelegateIterator) then
      begin
        if HadDelegateContinuation and (FResumeKind = bgrkNext) then
          YieldedValue := FDelegateIterator.DirectNextValue(
            RegisterToValue(FResumeValue), Done)
        else
          YieldedValue := FDelegateIterator.DirectNext(Done);
      end
      else
      begin
        if not Assigned(FDelegateNextMethod) or not FDelegateNextMethod.IsCallable then
          ClearDelegateAndThrowTypeError('Iterator.next is not a function');
        if HadDelegateContinuation and (FResumeKind = bgrkNext) then
          CallArgs := TGocciaArgumentsCollection.Create([RegisterToValue(FResumeValue)])
        else
          CallArgs := TGocciaArgumentsCollection.Create;
        try
          NextResult := CallDelegateMethod(FDelegateNextMethod,
            FDelegateIteratorValue, CallArgs);
        finally
          CallArgs.Free;
        end;
        if not (NextResult is TGocciaObjectValue) then
          ClearDelegateAndThrowTypeErrorWithSuggestion(
            Format(SErrorIteratorResultNotObject,
            [NextResult.ToStringLiteral.Value]), SSuggestIteratorResultObject);
        DoneValue := NextResult.GetProperty(PROP_DONE);
        Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
        YieldedValue := NextResult.GetProperty(PROP_VALUE);
        if not Assigned(YieldedValue) then
          YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      end;

      if Done then
      begin
        FVM.FRegisters[AResumeRegister] := VMValueToRegisterFast(YieldedValue);
        ClearDelegateState;
        Exit;
      end;

      CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
        AResumeRegister, AContinuationIP);
      raise EGocciaBytecodeYield.Create(VMValueToRegisterFast(YieldedValue), 0);
    end;
  except
    on E: EGocciaBytecodeYield do
      raise;
    else
    begin
      ClearDelegateState;
      raise;
    end;
  end;
end;

function TGocciaBytecodeGeneratorObjectValue.ResumeRaw(
  const AKind: TGocciaBytecodeGeneratorResumeKind; const AValue: TGocciaValue;
  out ADone: Boolean): TGocciaValue;
var
  PreviousGenerator: TGocciaBytecodeGeneratorObjectValue;
  ReturnRegister: TGocciaRegister;
begin
  if FState = bgsCompleted then
  begin
    ADone := True;
    if AKind = bgrkThrow then
      raise TGocciaThrowValue.Create(AValue);
    if AKind = bgrkReturn then
      Exit(AValue);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if FState = bgsExecuting then
    raise TGocciaThrowValue.Create(VMGeneratorExecutingError);

  if (FState = bgsSuspendedStart) and (AKind = bgrkReturn) then
  begin
    FState := bgsCompleted;
    ADone := True;
    Exit(AValue);
  end;

  if (FState = bgsSuspendedStart) and (AKind = bgrkThrow) then
  begin
    FState := bgsCompleted;
    raise TGocciaThrowValue.Create(AValue);
  end;

  FResumeKind := AKind;
  FResumeValue := VMValueToRegisterFast(AValue);
  FState := bgsExecuting;

  PreviousGenerator := GActiveBytecodeGenerator;
  GActiveBytecodeGenerator := Self;
  try
    try
      ReturnRegister := FVM.ExecuteClosureRegisters(FClosure, FThisValue, FArguments);
      FState := bgsCompleted;
      ClearDelegateState;
      ADone := True;
      Result := RegisterToValue(ReturnRegister);
    except
      on E: EGocciaBytecodeYield do
      begin
        FState := bgsSuspendedYield;
        ADone := False;
        Result := RegisterToValue(E.Value);
      end;
      on E: EGocciaBytecodeGeneratorReturn do
      begin
        FState := bgsCompleted;
        ClearDelegateState;
        ADone := True;
        Result := E.Value;
      end;
      on E: EGocciaBytecodeThrow do
      begin
        if Assigned(FReturnSentinel) and (E.ThrownValue = FReturnSentinel) then
        begin
          FState := bgsCompleted;
          ClearDelegateState;
          ADone := True;
          Result := FReturnValue;
        end
        else
        begin
          FState := bgsCompleted;
          ClearDelegateState;
          ADone := True;
          raise;
        end;
      end;
    end;
  finally
    GActiveBytecodeGenerator := PreviousGenerator;
  end;
end;

function TGocciaBytecodeGeneratorObjectValue.AdvanceNext: TGocciaObjectValue;
begin
  Result := AdvanceNextValue(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaBytecodeGeneratorObjectValue.AdvanceNextValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  ResultValue := ResumeRaw(bgrkNext, AValue, Done);
  Result := CreateIteratorResult(ResultValue, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.DirectNext(out ADone: Boolean): TGocciaValue;
begin
  Result := DirectNextValue(TGocciaUndefinedLiteralValue.UndefinedValue, ADone);
end;

function TGocciaBytecodeGeneratorObjectValue.DirectNextValue(
  const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
begin
  Result := ResumeRaw(bgrkNext, AValue, ADone);
end;

function TGocciaBytecodeGeneratorObjectValue.ReturnValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  ResultValue := ResumeRaw(bgrkReturn, AValue, Done);
  Result := CreateIteratorResult(ResultValue, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.ThrowValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  ResultValue := ResumeRaw(bgrkThrow, AValue, Done);
  Result := CreateIteratorResult(ResultValue, Done);
end;

procedure TGocciaBytecodeGeneratorObjectValue.Close;
begin
  if FState = bgsCompleted then
    Exit;
  ReturnValue(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaBytecodeGeneratorObjectValue.GeneratorNext(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(VMGeneratorTypeError);
  Value := TGocciaBytecodeGeneratorObjectValue(AThisValue).ResumeRaw(
    bgrkNext, VMArgumentOrUndefined(AArgs), Done);
  Result := CreateIteratorResult(Value, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.GeneratorReturn(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(VMGeneratorTypeError);
  Value := TGocciaBytecodeGeneratorObjectValue(AThisValue).ResumeRaw(
    bgrkReturn, VMArgumentOrUndefined(AArgs), Done);
  Result := CreateIteratorResult(Value, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.GeneratorThrow(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(VMGeneratorTypeError);
  Value := TGocciaBytecodeGeneratorObjectValue(AThisValue).ResumeRaw(
    bgrkThrow, VMArgumentOrUndefined(AArgs), Done);
  Result := CreateIteratorResult(Value, Done);
end;

procedure TGocciaBytecodeGeneratorObjectValue.MarkReferences;
var
  I: Integer;
  Upvalue: TGocciaBytecodeUpvalue;
begin
  inherited;
  if Assigned(FClosure) then
  begin
    if Assigned(FClosure.HomeObject) then
      FClosure.HomeObject.MarkReferences;
    for I := 0 to FClosure.UpvalueCount - 1 do
    begin
      Upvalue := FClosure.GetUpvalue(I);
      if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
        MarkRegisterReferences(Upvalue.Cell.Value);
    end;
  end;
  MarkRegisterReferences(FThisValue);
  MarkRegisterReferences(FResumeValue);
  for I := 0 to High(FArguments) do
    MarkRegisterReferences(FArguments[I]);
  for I := 0 to High(FContinuationRegisters) do
    MarkRegisterReferences(FContinuationRegisters[I]);
  for I := 0 to High(FContinuationLocalCells) do
    if Assigned(FContinuationLocalCells[I]) then
      MarkRegisterReferences(FContinuationLocalCells[I].Value);
  if Assigned(FDelegateIteratorValue) then
    FDelegateIteratorValue.MarkReferences;
  if Assigned(FDelegateIterator) then
    FDelegateIterator.MarkReferences;
  if Assigned(FDelegateNextMethod) then
    FDelegateNextMethod.MarkReferences;
  if Assigned(FReturnSentinel) then
    FReturnSentinel.MarkReferences;
  if Assigned(FReturnValue) then
    FReturnValue.MarkReferences;
end;

function TGocciaBytecodeGeneratorObjectValue.ToStringTag: string;
begin
  Result := 'Generator';
end;

constructor TGocciaBytecodeAsyncGeneratorObjectValue.Create(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection);
begin
  inherited Create;
  FInner := TGocciaBytecodeGeneratorObjectValue.Create(AVM, AClosure,
    AThisValue, AArguments);
  DefineProperty(PROP_NEXT, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(AsyncGeneratorNext, PROP_NEXT, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_RETURN, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(AsyncGeneratorReturn, PROP_RETURN, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_THROW, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(AsyncGeneratorThrow, PROP_THROW, 1),
    [pfConfigurable, pfWritable]));
  DefineSymbolProperty(TGocciaSymbolValue.WellKnownAsyncIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(AsyncIteratorSelf, '[Symbol.asyncIterator]', 0),
      [pfConfigurable, pfWritable]));
end;

constructor TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(
  const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray);
begin
  inherited Create;
  FInner := TGocciaBytecodeGeneratorObjectValue.CreateRegisters(AVM, AClosure,
    AThisValue, AArguments);
  DefineProperty(PROP_NEXT, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(AsyncGeneratorNext, PROP_NEXT, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_RETURN, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(AsyncGeneratorReturn, PROP_RETURN, 1),
    [pfConfigurable, pfWritable]));
  DefineProperty(PROP_THROW, TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(AsyncGeneratorThrow, PROP_THROW, 1),
    [pfConfigurable, pfWritable]));
  DefineSymbolProperty(TGocciaSymbolValue.WellKnownAsyncIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(AsyncIteratorSelf, '[Symbol.asyncIterator]', 0),
      [pfConfigurable, pfWritable]));
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.ResumeAsPromise(
  const AKind: TGocciaBytecodeGeneratorResumeKind; const AValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  Done: Boolean;
  IsRooted: Boolean;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  Promise := TGocciaPromiseValue.Create;
  IsRooted := Assigned(TGarbageCollector.Instance);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    try
      try
        Value := FInner.ResumeRaw(AKind, AValue, Done);
        UnwrappedValue := AwaitValue(Value);
        Promise.Resolve(CreateIteratorResult(UnwrappedValue, Done));
      except
        on E: EGocciaBytecodeThrow do
          Promise.Reject(E.ThrownValue);
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    except
      if IsRooted then
      begin
        TGarbageCollector.Instance.RemoveTempRoot(Promise);
        IsRooted := False;
      end;
      Promise.Free;
      raise;
    end;
  finally
    if IsRooted then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
  end;
  Result := Promise;
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncGeneratorNext(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeAsyncGeneratorObjectValue) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  if not Assigned(TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).FInner) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    bgrkNext, VMArgumentOrUndefined(AArgs));
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncGeneratorReturn(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeAsyncGeneratorObjectValue) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  if not Assigned(TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).FInner) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    bgrkReturn, VMArgumentOrUndefined(AArgs));
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncGeneratorThrow(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeAsyncGeneratorObjectValue) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  if not Assigned(TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).FInner) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    bgrkThrow, VMArgumentOrUndefined(AArgs));
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncIteratorSelf(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.MarkReferences;
begin
  inherited;
  if Assigned(FInner) then
    FInner.MarkReferences;
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.ToStringTag: string;
begin
  Result := 'AsyncGenerator';
end;

function TGocciaBytecodeFunctionValue.GetFunctionName: string;
begin
  Result := FClosure.Template.Name;
end;

function TGocciaBytecodeFunctionValue.GetSourceText: string;
begin
  Result := FClosure.Template.SourceText;
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
  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(TGocciaBytecodeAsyncGeneratorObjectValue.Create(FVM, FClosure,
        AThisValue, AArguments));
    Exit(TGocciaBytecodeGeneratorObjectValue.Create(FVM, FClosure,
      AThisValue, AArguments));
  end;

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

function TGocciaBytecodeFunctionValue.CallPreparedArgs(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  case AArguments.Length of
    0: Result := CallNoArgs(AThisValue);
    1: Result := CallOneArg(AArguments.GetElement(0), AThisValue);
    2: Result := CallTwoArgs(AArguments.GetElement(0), AArguments.GetElement(1),
      AThisValue);
    3: Result := CallThreeArgs(AArguments.GetElement(0), AArguments.GetElement(1),
      AArguments.GetElement(2), AThisValue);
  else
    Result := Call(AArguments, AThisValue);
  end;
end;

function TGocciaBytecodeFunctionValue.CallNoArgs(
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(AThisValue), TGocciaRegisterArray(nil)));
    Exit(TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
      VMValueToRegisterFast(AThisValue), TGocciaRegisterArray(nil)));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    try
      try
        Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters0(FClosure,
          VMValueToRegisterFast(AThisValue))));
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

  Result := RegisterToValue(FVM.ExecuteClosureRegisters0(FClosure,
    VMValueToRegisterFast(AThisValue)));
end;

function TGocciaBytecodeFunctionValue.CallOneArg(const AArg0,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(AThisValue),
        TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0))));
    Exit(TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
      VMValueToRegisterFast(AThisValue),
      TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0))));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    try
      try
        Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters1(FClosure,
          VMValueToRegisterFast(AThisValue), VMValueToRegisterFast(AArg0))));
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

  Result := RegisterToValue(FVM.ExecuteClosureRegisters1(FClosure,
    VMValueToRegisterFast(AThisValue), VMValueToRegisterFast(AArg0)));
end;

function TGocciaBytecodeFunctionValue.CallTwoArgs(const AArg0, AArg1,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(AThisValue),
        TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
          VMValueToRegisterFast(AArg1))));
    Exit(TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
      VMValueToRegisterFast(AThisValue),
      TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
        VMValueToRegisterFast(AArg1))));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    try
      try
        Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters2(FClosure,
          VMValueToRegisterFast(AThisValue), VMValueToRegisterFast(AArg0),
          VMValueToRegisterFast(AArg1))));
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

  Result := RegisterToValue(FVM.ExecuteClosureRegisters2(FClosure,
    VMValueToRegisterFast(AThisValue), VMValueToRegisterFast(AArg0),
    VMValueToRegisterFast(AArg1)));
end;

function TGocciaBytecodeFunctionValue.CallThreeArgs(const AArg0, AArg1, AArg2,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(AThisValue),
        TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
          VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2))));
    Exit(TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
      VMValueToRegisterFast(AThisValue),
      TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
        VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2))));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    try
      try
        Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters3(FClosure,
          VMValueToRegisterFast(AThisValue), VMValueToRegisterFast(AArg0),
          VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2))));
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

  Result := RegisterToValue(FVM.ExecuteClosureRegisters3(FClosure,
    VMValueToRegisterFast(AThisValue), VMValueToRegisterFast(AArg0),
    VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2)));
end;

function TGocciaVMSuperConstructorValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  SuperClass: TGocciaClassValue;
  SuperResult: TGocciaValue;
begin
  if (FSuperClass is TGocciaObjectValue) and
     (not (FSuperClass is TGocciaClassValue)) and
     FSuperClass.IsCallable then
  begin
    if (AThisValue is TGocciaObjectValue) and
       not (AThisValue is TGocciaInstanceValue) then
      Exit(AThisValue);

    if FSuperClass is TGocciaProxyValue then
      Exit(TGocciaProxyValue(FSuperClass).ConstructTrap(AArguments));
    if FSuperClass is TGocciaNativeFunctionValue then
    begin
      if TGocciaNativeFunctionValue(FSuperClass).NotConstructable then
        ThrowTypeError(
          Format(SErrorNotConstructor,
            [TGocciaNativeFunctionValue(FSuperClass).Name]),
          Format('''%s'' is not a constructor',
            [TGocciaNativeFunctionValue(FSuperClass).Name]));
      Exit(TGocciaNativeFunctionValue(FSuperClass).Call(
        AArguments, TGocciaHoleValue.HoleValue));
    end;
    if FSuperClass is TGocciaFunctionBase then
      Exit(TGocciaFunctionBase(FSuperClass).Call(
        AArguments, TGocciaUndefinedLiteralValue.UndefinedValue));
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if not (FSuperClass is TGocciaClassValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  SuperClass := TGocciaClassValue(FSuperClass);

  if (SuperClass is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(SuperClass).FConstructorValue) then
  begin
    TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
      SuperClass, AThisValue);
    SuperResult := TGocciaVMClassValue(SuperClass).FVM.InvokeFunctionValue(
      TGocciaVMClassValue(SuperClass).FConstructorValue,
      AArguments, AThisValue);
    if SuperResult is TGocciaObjectValue then
      Exit(SuperResult);
    Exit(AThisValue);
  end;

  if Assigned(SuperClass.ConstructorMethod) then
  begin
    if SuperClass is TGocciaVMClassValue then
      TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
        SuperClass, AThisValue);
    SuperResult := SuperClass.ConstructorMethod.Call(AArguments, AThisValue);
    if SuperResult is TGocciaObjectValue then
      Exit(SuperResult);
    Exit(AThisValue);
  end;

  if Assigned(SuperClass.SuperClass) and
     Assigned(SuperClass.SuperClass.ConstructorMethod) then
  begin
    if SuperClass is TGocciaVMClassValue then
      TGocciaVMClassValue(SuperClass).FVM.InvokeImplicitSuperInitialization(
        SuperClass.SuperClass, AThisValue, AArguments);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if AThisValue is TGocciaInstanceValue then
  begin
    if SuperClass is TGocciaVMClassValue then
      TGocciaVMClassValue(SuperClass).FVM.InvokeImplicitSuperInitialization(
        SuperClass, AThisValue, AArguments);
    TGocciaInstanceValue(AThisValue).InitializeNativeFromArguments(AArguments);
    Exit(AThisValue);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

constructor TGocciaVM.Create;
const
  INITIAL_STACK_SIZE = 4096;
begin
  inherited Create;
  FPreviousExceptionMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  FHandlerStack := TGocciaBytecodeHandlerStack.Create;
  FArgumentPoolCount := 0;
  FActiveDecoratorSession := nil;
  SetLength(FRegisterStack, INITIAL_STACK_SIZE);
  FRegisterBase := 0;
  FRegisters := nil;
  FRegisterCount := 0;
  SetLength(FLocalCellStack, INITIAL_STACK_SIZE);
  FLocalCellBase := 0;
  FLocalCells := nil;
  FLocalCellCount := 0;
  SetLength(FFrameStack, 64);
  FFrameStackCount := 0;
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

// ES2026 §10.2.2 [[Construct]](argumentsList, newTarget)
function TGocciaVMClassValue.Instantiate(
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaClassValue): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  ConstructorToCall: TGocciaMethodValue;
  InstancePrototype: TGocciaObjectValue;
begin
  // ES2026 §10.2.2 step 5: Let proto be ? GetPrototypeFromConstructor(newTarget)
  if Assigned(ANewTarget) then
    InstancePrototype := ANewTarget.Prototype
  else
    InstancePrototype := Prototype;

  NativeInstance := nil;
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    NativeInstance := WalkClass.CreateNativeInstance(AArguments);
    if Assigned(NativeInstance) then
      Break;
    WalkClass := WalkClass.SuperClass;
  end;

  // ES2026 §10.2.2 step 6: Set proto on the instance before constructor runs
  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    Instance.Prototype := InstancePrototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := Self;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(Self);
    Instance.Prototype := InstancePrototype;
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

function TGocciaVMClassValue.InstantiateRegisters(
  const AArguments: TGocciaRegisterArray): TGocciaRegister;
var
  Instance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  ConstructorToCall: TGocciaMethodValue;
  BoxedArgs: TGocciaArgumentsCollection;
  BytecodeConstructor: TGocciaBytecodeFunctionValue;
  BytecodeSuperConstructor: TGocciaBytecodeFunctionValue;
  procedure EnsureBoxedArgs;
  begin
    if not Assigned(BoxedArgs) then
      BoxedArgs := FVM.MaterializeArguments(AArguments);
  end;
begin
  CheckExecutionTimeout;
  CheckInstructionLimit;
  BoxedArgs := nil;
  try
    NativeInstance := nil;
    WalkClass := Self;
    while Assigned(WalkClass) do
    begin
      if not (WalkClass is TGocciaVMClassValue) then
      begin
        EnsureBoxedArgs;
        NativeInstance := WalkClass.CreateNativeInstance(BoxedArgs);
      end
      else if Assigned(TGocciaVMClassValue(WalkClass).NativeSuperConstructor) then
      begin
        EnsureBoxedArgs;
        NativeInstance := WalkClass.CreateNativeInstance(BoxedArgs);
      end;
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

    TGarbageCollector.Instance.AddTempRoot(Instance);
    try
      if Assigned(FConstructorValue) then
      begin
        FVM.RunClassInitializers(Self, Instance);
        if FConstructorValue is TGocciaBytecodeFunctionValue then
        begin
          BytecodeConstructor := TGocciaBytecodeFunctionValue(FConstructorValue);
          if Assigned(BytecodeConstructor.FClosure) and
             Assigned(BytecodeConstructor.FClosure.Template) and
             (not BytecodeConstructor.FClosure.Template.IsAsync) then
            FVM.ExecuteClosureRegisters(BytecodeConstructor.FClosure,
              RegisterObject(Instance), AArguments)
          else
          begin
            EnsureBoxedArgs;
            FVM.InvokeFunctionValue(FConstructorValue, BoxedArgs, Instance);
          end;
        end
        else
        begin
          EnsureBoxedArgs;
          FVM.InvokeFunctionValue(FConstructorValue, BoxedArgs, Instance);
        end;
      end
      else
      begin
        FVM.RunClassInitializers(Self, Instance);
        ConstructorToCall := nil;

        if (SuperClass is TGocciaVMClassValue) and
           Assigned(TGocciaVMClassValue(SuperClass).FConstructorValue) then
        begin
          TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
            SuperClass, Instance);
          if TGocciaVMClassValue(SuperClass).FConstructorValue is TGocciaBytecodeFunctionValue then
          begin
            BytecodeSuperConstructor := TGocciaBytecodeFunctionValue(
              TGocciaVMClassValue(SuperClass).FConstructorValue);
            if Assigned(BytecodeSuperConstructor.FClosure) and
               Assigned(BytecodeSuperConstructor.FClosure.Template) and
               (not BytecodeSuperConstructor.FClosure.Template.IsAsync) then
              TGocciaVMClassValue(SuperClass).FVM.ExecuteClosureRegisters(
                BytecodeSuperConstructor.FClosure,
                RegisterObject(Instance), AArguments)
            else
            begin
              EnsureBoxedArgs;
              TGocciaVMClassValue(SuperClass).FVM.InvokeFunctionValue(
                TGocciaVMClassValue(SuperClass).FConstructorValue,
                BoxedArgs, Instance);
            end;
          end
          else
          begin
            EnsureBoxedArgs;
            TGocciaVMClassValue(SuperClass).FVM.InvokeFunctionValue(
              TGocciaVMClassValue(SuperClass).FConstructorValue,
              BoxedArgs, Instance);
          end;
        end
        else
        begin
          if Assigned(SuperClass) then
            ConstructorToCall := SuperClass.ConstructorMethod;

          if Assigned(ConstructorToCall) then
          begin
            EnsureBoxedArgs;
            FVM.RunClassInitializers(SuperClass, Instance);
            ConstructorToCall.Call(BoxedArgs, Instance);
          end
          else if Assigned(SuperClass) then
            FVM.InvokeImplicitSuperInitializationRegisters(SuperClass, Instance, AArguments)
          else
          begin
            EnsureBoxedArgs;
            if Instance is TGocciaInstanceValue then
              TGocciaInstanceValue(Instance).InitializeNativeFromArguments(BoxedArgs);
          end;
        end;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Instance);
    end;

    Result := RegisterObject(Instance);
  finally
    if Assigned(BoxedArgs) then
      FVM.ReleaseArguments(BoxedArgs);
  end;
end;

function TGocciaVMClassValue.GetProperty(const AName: string): TGocciaValue;
var
  Getter: TGocciaFunctionBase;
  Current: TGocciaClassValue;
  BytecodeGetter: TGocciaBytecodeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  Getter := StaticPropertyGetter[AName];
  if Assigned(Getter) then
  begin
    if (Getter is TGocciaBytecodeFunctionValue) then
    begin
      BytecodeGetter := TGocciaBytecodeFunctionValue(Getter);
      if Assigned(BytecodeGetter.FClosure) and
         Assigned(BytecodeGetter.FClosure.Template) and
         (not BytecodeGetter.FClosure.Template.IsAsync) then
        Exit(RegisterToValue(FVM.ExecuteClosureRegisters(
          BytecodeGetter.FClosure, RegisterObject(Self), [])));
    end;
    Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
    try
      Exit(Getter.Call(Args, Self));
    finally
      Args.Free;
    end;
  end;

  Result := inherited GetProperty(AName);
end;

procedure TGocciaVMClassValue.SetProperty(const AName: string;
  const AValue: TGocciaValue);
var
  Setter: TGocciaFunctionBase;
  Current: TGocciaClassValue;
  BytecodeSetter: TGocciaBytecodeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  Current := Self;
  repeat
    Setter := Current.StaticPropertySetter[AName];
    if Assigned(Setter) then
    begin
      if Setter is TGocciaBytecodeFunctionValue then
      begin
        BytecodeSetter := TGocciaBytecodeFunctionValue(Setter);
        if Assigned(BytecodeSetter.FClosure) and
           Assigned(BytecodeSetter.FClosure.Template) and
           (not BytecodeSetter.FClosure.Template.IsAsync) then
        begin
          FVM.ExecuteClosureRegisters(BytecodeSetter.FClosure, RegisterObject(Self),
            [ValueToRegister(AValue)]);
          Exit;
        end;
      end;

      Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
      try
        Args.Add(AValue);
        Setter.Call(Args, Self);
      finally
        Args.Free;
      end;
      Exit;
    end;
    Current := Current.SuperClass;
  until not Assigned(Current);

  inherited SetProperty(AName, AValue);
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

  if Assigned(FClosure.HomeObject) then
    FClosure.HomeObject.MarkReferences;

  for I := 0 to FClosure.UpvalueCount - 1 do
  begin
    Upvalue := FClosure.GetUpvalue(I);
    if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
      MarkRegisterReferences(Upvalue.Cell.Value);
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
      if AConstant.IntValue = 0 then
        Result := TGocciaNumberLiteralValue.ZeroValue
      else if AConstant.IntValue = 1 then
        Result := TGocciaNumberLiteralValue.OneValue
      else
        Result := TGocciaNumberLiteralValue.Create(AConstant.IntValue);
    bckFloat:
      Result := TGocciaNumberLiteralValue.Create(AConstant.FloatValue);
    bckString:
      Result := TGocciaStringLiteralValue.Create(AConstant.StringValue);
    bckBigInt:
      Result := TGocciaBigIntValue.Create(
        TBigInteger.FromDecimalString(AConstant.StringValue));
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §13.2.8.3 GetTemplateObject — return the cached frozen template object
// for the call site identified by AConstantIndex, building and pinning it on
// the first execution of this instruction for the given function template.
// Every subsequent execution of the same OP_LOAD_CONST instruction within the
// same template returns the identical object reference, satisfying the spec's
// per-Parse-Node identity requirement without any new opcodes.
function TGocciaVM.BuildTemplateObjectConstant(const ATemplate: TGocciaFunctionTemplate;
  const AConstantIndex: Integer): TGocciaValue;
var
  Constant: TGocciaBytecodeConstant;
  Slot: Integer;
  CookedArray, RawArray: TGocciaArrayValue;
  I: Integer;
begin
  Constant := ATemplate.GetConstantUnchecked(AConstantIndex);
  Slot := Integer(Constant.IntValue);
  Result := TGocciaValue(ATemplate.GetTemplateObjectCache(Slot));
  if Assigned(Result) then
    Exit;

  // First execution: build, freeze, and pin the template object
  RawArray := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(RawArray);
  try
    for I := 0 to Length(Constant.RawStrings) - 1 do
      RawArray.Elements.Add(TGocciaStringLiteralValue.Create(Constant.RawStrings[I]));
    RawArray.Freeze;

    CookedArray := TGocciaArrayValue.Create;
    TGarbageCollector.Instance.AddTempRoot(CookedArray);
    try
      // TC39 Template Literal Revision: segments with malformed escapes get
      // cooked=undefined; valid segments get the resolved string value.
      for I := 0 to Length(Constant.CookedStrings) - 1 do
      begin
        if (I < Length(Constant.CookedValid)) and not Constant.CookedValid[I] then
          CookedArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
        else
          CookedArray.Elements.Add(TGocciaStringLiteralValue.Create(Constant.CookedStrings[I]));
      end;
      // ES2026 §13.2.8.3 step 8: raw is non-enumerable, non-writable, non-configurable
      CookedArray.DefineProperty(PROP_RAW,
        TGocciaPropertyDescriptorData.Create(RawArray, []));
      // ES2026 §13.2.8.3 step 12: freeze the template object
      CookedArray.Freeze;
      // ES2026 §13.2.8.3 step 13: cache keyed by this Parse Node (template slot)
      TGarbageCollector.Instance.PinObject(CookedArray);
      ATemplate.SetTemplateObjectCache(Slot, CookedArray);
      Result := CookedArray;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(CookedArray);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(RawArray);
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

procedure TGocciaVM.AcquireRegisters(const ACount: Integer);
var
  NewBase, Required: Integer;
begin
  NewBase := FRegisterBase + FRegisterCount;
  Required := NewBase + ACount;
  if Required > Length(FRegisterStack) then
    SetLength(FRegisterStack, Required * 2);
  FRegisterBase := NewBase;
  FRegisterCount := ACount;
  FRegisters := @FRegisterStack[FRegisterBase];
  FillChar(FRegisters^, ACount * SizeOf(TGocciaRegister), 0);
end;

procedure TGocciaVM.AcquireLocalCells(const ACount: Integer);
var
  NewBase, Required: Integer;
begin
  NewBase := FLocalCellBase + FLocalCellCount;
  Required := NewBase + ACount;
  if Required > Length(FLocalCellStack) then
    SetLength(FLocalCellStack, Required * 2);
  FLocalCellBase := NewBase;
  FLocalCellCount := ACount;
  FLocalCells := @FLocalCellStack[FLocalCellBase];
  FillChar(FLocalCells^, ACount * SizeOf(TGocciaBytecodeCell), 0);
end;

procedure TGocciaVM.EnsureRegisterCapacity(const ACount: Integer);
var
  Growth, Required: Integer;
begin
  if ACount > FRegisterCount then
  begin
    Growth := ACount - FRegisterCount;
    Required := FRegisterBase + ACount;
    if Required > Length(FRegisterStack) then
      SetLength(FRegisterStack, Required * 2);
    FillChar(FRegisterStack[FRegisterBase + FRegisterCount],
      Growth * SizeOf(TGocciaRegister), 0);
    FRegisterCount := ACount;
    FRegisters := @FRegisterStack[FRegisterBase];
  end;
end;

procedure TGocciaVM.EnsureLocalCapacity(const ACount: Integer);
var
  Growth, Required: Integer;
begin
  if ACount > FLocalCellCount then
  begin
    Growth := ACount - FLocalCellCount;
    Required := FLocalCellBase + ACount;
    if Required > Length(FLocalCellStack) then
      SetLength(FLocalCellStack, Required * 2);
    FillChar(FLocalCellStack[FLocalCellBase + FLocalCellCount],
      Growth * SizeOf(TGocciaBytecodeCell), 0);
    FLocalCellCount := ACount;
    FLocalCells := @FLocalCellStack[FLocalCellBase];
  end;
end;

function TGocciaVM.GetLocalCell(const AIndex: Integer): TGocciaBytecodeCell;
begin
  EnsureLocalCapacity(AIndex + 1);
  if not Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex] := TGocciaBytecodeCell.Create(FRegisters[AIndex]);
  Result := FLocalCells[AIndex];
end;

function TGocciaVM.GetLocalRegister(const AIndex: Integer): TGocciaRegister;
begin
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    Exit(FLocalCells[AIndex].Value);
  if (AIndex >= 0) and (AIndex < FRegisterCount) then
    Exit(FRegisters[AIndex]);
  Result := RegisterUndefined;
end;

function TGocciaVM.GetRegister(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < FRegisterCount) then
    Result := RegisterToValue(FRegisters[AIndex])
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.GetRegisterFast(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex < 0) or (AIndex >= FRegisterCount) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := RegisterToValue(FRegisters[AIndex]);
end;

procedure TGocciaVM.SetRegister(const AIndex: Integer; const AValue: TGocciaValue);
begin
  EnsureRegisterCapacity(AIndex + 1);
  FRegisters[AIndex] := VMValueToRegisterFast(AValue);
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and
     Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := FRegisters[AIndex];
end;

procedure TGocciaVM.SetRegisterFast(const AIndex: Integer;
  const AValue: TGocciaValue);
begin
  if (AIndex < 0) or (AIndex >= FRegisterCount) then
  begin
    SetRegister(AIndex, AValue);
    Exit;
  end;
  FRegisters[AIndex] := VMValueToRegisterFast(AValue);
  if (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := FRegisters[AIndex];
end;

procedure TGocciaVM.SetRegisterRaw(const AIndex: Integer;
  const AValue: TGocciaRegister);
begin
  EnsureRegisterCapacity(AIndex + 1);
  FRegisters[AIndex] := AValue;
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and
     Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := AValue;
end;

procedure TGocciaVM.InstallFunctionPrototype(
  const AFunction: TGocciaObjectValue;
  const AIsGenerator: Boolean);
var
  PrototypeObj: TGocciaObjectValue;
  PrototypeFlags: TPropertyFlags;
begin
  // ES2026 §10.2.5 MakeConstructor — adds a `prototype` data property to a
  // function whose value is a fresh ordinary object.  Shape differs by kind:
  //   - Ordinary function (§15.2): prototype is { writable, !enumerable,
  //     !configurable } with an own `constructor` pointing at the function.
  //   - (Async) generator (§15.5 / §15.6): prototype is { !writable,
  //     !enumerable, !configurable } with NO own `constructor` — per spec it
  //     inherits `constructor` from %GeneratorFunction.prototype.prototype%
  //     (which points at %GeneratorFunction.prototype%, not the specific
  //     generator function), so an own back-reference would be wrong.
  // The prototype object's [[Prototype]] is %Object.prototype% per ES2026
  // §10.2.5.1 OrdinaryFunctionCreate.  (For generators it should be %Generator%,
  // but GocciaScript does not yet expose that intrinsic; falling back to
  // Object.prototype keeps the chain non-null and lets generic object methods
  // like hasOwnProperty resolve.)
  //
  // Match the lazy-init guard used by OP_NEW_OBJECT — the bytecode VM can be
  // exercised outside the normal engine bootstrap (e.g. Goccia.VM.Test.pas),
  // so the realm slot may not be primed yet on the first OP_CLOSURE.
  if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    TGocciaObjectValue.InitializeSharedPrototype;
  PrototypeObj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if AIsGenerator then
  begin
    PrototypeFlags := [];
  end
  else
  begin
    PrototypeFlags := [pfWritable];
    PrototypeObj.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(AFunction, [pfWritable, pfConfigurable]));
  end;
  AFunction.DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(PrototypeObj, PrototypeFlags));
end;

procedure SetBytecodeHomeObject(const AFunctionValue: TGocciaValue;
  const AHomeObject: TGocciaObjectValue);
begin
  if (AFunctionValue is TGocciaBytecodeFunctionValue) and
     Assigned(TGocciaBytecodeFunctionValue(AFunctionValue).FClosure) then
    TGocciaBytecodeFunctionValue(AFunctionValue).FClosure.HomeObject := AHomeObject;
end;

function TGocciaVM.GetLocal(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    Exit(RegisterToValue(FLocalCells[AIndex].Value));
  Result := GetRegister(AIndex);
end;

function TGocciaVM.GetLocalFast(const AIndex: Integer): TGocciaValue;
begin
  if AIndex < 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    Exit(RegisterToValue(FLocalCells[AIndex].Value));
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

procedure TGocciaVM.SetLocalRaw(const AIndex: Integer;
  const AValue: TGocciaRegister);
begin
  if AIndex < 0 then
    Exit;
  SetRegisterRaw(AIndex, AValue);
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

function CreateModuleNamespaceObject(const AModule: TGocciaModule): TGocciaObjectValue;
begin
  Result := AModule.GetNamespaceObject;
end;

function RegisterMatchesNullishKind(const AValue: TGocciaRegister;
  const AKind: UInt8): Boolean; inline;
begin
  case AKind of
    GOCCIA_NULLISH_MATCH_ANY:
      Result := AValue.Kind in [grkUndefined, grkNull, grkHole];
    GOCCIA_NULLISH_MATCH_UNDEFINED:
      Result := AValue.Kind = grkUndefined;
    GOCCIA_NULLISH_MATCH_NULL:
      Result := AValue.Kind = grkNull;
    GOCCIA_NULLISH_MATCH_HOLE:
      Result := AValue.Kind = grkHole;
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

function TGocciaVM.TryGetArrayIndexRegister(const AKey: TGocciaRegister;
  out AIndex: Integer): Boolean;
begin
  Result := False;
  AIndex := -1;
  case AKey.Kind of
    grkInt:
      begin
        AIndex := AKey.IntValue;
        Result := AIndex >= 0;
      end;
    grkFloat:
      begin
        if Frac(AKey.FloatValue) <> 0.0 then
          Exit;
        AIndex := Trunc(AKey.FloatValue);
        Result := AIndex >= 0;
      end;
  else
    Result := TryGetArrayIndex(RegisterToValue(AKey), AIndex);
  end;
end;

function TGocciaVM.KeyToPropertyName(const AKey: TGocciaValue): string;
begin
  if not Assigned(AKey) then
    Exit('');
  Result := AKey.ToStringLiteral.Value;
end;

function TGocciaVM.KeyToPropertyNameRegister(const AKey: TGocciaRegister): string;
begin
  case AKey.Kind of
    grkUndefined:
      Result := 'undefined';
    grkNull:
      Result := 'null';
    grkBoolean:
      if AKey.BoolValue then
        Result := 'true'
      else
        Result := 'false';
    grkInt:
      Result := IntToStr(AKey.IntValue);
    grkFloat:
      Result := VMRegisterToECMAStringFast(AKey).Value;
    grkObject:
      if Assigned(AKey.ObjectValue) then
        Result := VMRegisterToECMAStringFast(AKey).Value
      else
        Result := '';
  else
    Result := '';
  end;
end;

// IteratorClose for the raw-object form returned by GetIteratorValue
// when the source supplies its own iterator (i.e. an object with next()
// rather than a TGocciaIteratorValue).  Normal-completion variant per
// ES2024 §7.4.10 IteratorClose:
//   - step 3.b: if `return` is missing/undefined/null, return the
//     completion as-is (only these three values skip silently —
//     non-callable returns are an error);
//   - step 3.c: call return; errors propagate;
//   - step 3.d: validate the result is an Object on normal completion;
//     a primitive result is a TypeError.
procedure CloseRawIterator(const AIteratorObject: TGocciaValue);
var
  ReturnMethod, ReturnResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not Assigned(AIteratorObject) then
    Exit;
  if AIteratorObject is TGocciaIteratorValue then
  begin
    CloseIterator(TGocciaIteratorValue(AIteratorObject));
    Exit;
  end;
  if not (AIteratorObject is TGocciaObjectValue) then
    Exit;
  ReturnMethod := AIteratorObject.GetProperty(PROP_RETURN);
  // §7.4.10 step 3.b: only missing/undefined/null skip.
  if not Assigned(ReturnMethod) or
     (ReturnMethod is TGocciaUndefinedLiteralValue) or
     (ReturnMethod is TGocciaNullLiteralValue) then
    Exit;
  // §7.4.10 step 3.c (implicit): a present-but-non-callable `return`
  // is a TypeError, not a silent no-op.
  if not ReturnMethod.IsCallable then
    ThrowTypeError(SErrorIteratorReturnMustBeCallable,
      SSuggestIteratorProtocol);
  CallArgs := TGocciaArgumentsCollection.Create;
  try
    ReturnResult := TGocciaFunctionBase(ReturnMethod).Call(
      CallArgs, AIteratorObject);
  finally
    CallArgs.Free;
  end;
  // §7.4.10 step 3.d: on normal completion, the IteratorResult must be
  // an Object.  Primitive results (including undefined, numbers, etc.)
  // are a TypeError.
  if (ReturnResult is TGocciaUndefinedLiteralValue)
      or (ReturnResult is TGocciaNullLiteralValue)
      or ReturnResult.IsPrimitive then
    ThrowTypeError(SErrorIteratorReturnObject,
      SSuggestIteratorResultObject);
end;

// Abrupt-completion variant of CloseRawIterator: per ES2024 §7.4.10
// step 5, when an iteration body completes abruptly the close must
// not let iter.return()'s own errors replace the original exception.
// Mirrors CloseIteratorPreservingError in Goccia.Values.IteratorSupport
// for the TGocciaIteratorValue case.
procedure CloseRawIteratorPreservingError(const AIteratorObject: TGocciaValue);
begin
  if not Assigned(AIteratorObject) then
    Exit;
  try
    CloseRawIterator(AIteratorObject);
  except
    // Swallow: the original abrupt completion is the one that must
    // surface to the caller.
  end;
end;

function TGocciaVM.IterableToArray(const AIterable: TGocciaValue;
  const ATryAsync: Boolean; const ALimit: Integer): TGocciaArrayValue;
var
  IteratorValue: TGocciaValue;
  DoneFlag: Boolean;
  NextMethod: TGocciaValue;
  NextResult: TGocciaValue;
  DoneValue: TGocciaValue;
  Value: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
  ArrayRooted, IteratorRooted: Boolean;
begin
  Result := TGocciaArrayValue.Create;
  // The materialised array and any newly-synthesised iterator wrapper
  // must outlive the consume/close re-entry into JS land: DirectNext
  // calls user next(), AwaitValue may pump microtasks, CloseIterator /
  // CloseRawIterator invoke user return().  Each of those can trigger a
  // GC sweep, and if `Result` or a fresh `IteratorValue` aren't on the
  // root set, they can be reclaimed mid-flight.  Mirrors the rooting
  // already done in TryIterableToArray below.
  GC := TGarbageCollector.Instance;
  ArrayRooted := False;
  IteratorRooted := False;
  IteratorValue := nil;
  if Assigned(GC) then
  begin
    GC.AddTempRoot(Result);
    ArrayRooted := True;
  end;
  try
    // GetIteratorValue can throw (e.g. for a non-iterable source, or
    // when [Symbol.iterator] returns a non-object).  Calling it inside
    // the try/finally is what guarantees Result is unrooted on the
    // throw path — otherwise it stays permanently temp-rooted and
    // accumulates across calls.
    IteratorValue := GetIteratorValue(AIterable, ATryAsync);
    if Assigned(GC) and Assigned(IteratorValue)
        and (IteratorValue <> AIterable)
        and (IteratorValue is TGocciaObjectValue) then
    begin
      GC.AddTempRoot(IteratorValue);
      IteratorRooted := True;
    end;

    if IteratorValue is TGocciaIteratorValue then
    begin
      // ES2024 §8.5.3 IteratorBindingInitialization: when an array
      // binding pattern has no rest element, the iterator must be
      // consumed for exactly N elements and then closed via
      // IteratorClose.  Without this bound, an iterator that returns
      // done:false indefinitely allocates unboundedly during
      // destructuring (test262 ary-init-iter-close cluster).
      //
      // ALimit semantics: <0 unbounded, 0 consume zero, >0 exact.  The
      // ALimit = 0 case must close the iterator before the first
      // next() call (per the spec, step 4 fires after zero
      // BindingElementList iterations for an empty pattern).
      if ALimit = 0 then
      begin
        CloseIterator(TGocciaIteratorValue(IteratorValue));
        Exit;
      end;
      try
        repeat
          CheckExecutionTimeout;
          CheckInstructionLimit;
          NextResult := TGocciaIteratorValue(IteratorValue).DirectNext(DoneFlag);
          if not DoneFlag then
            Result.Elements.Add(NextResult);
          if (ALimit > 0) and (Result.Elements.Count >= ALimit) then
          begin
            // ES2024 §7.4.10 step 5: normal-completion IteratorClose lets
            // errors from iter.return() propagate.  Use CloseIterator (not
            // PreservingError) so test262 tests like Iterator.from →
            // Iterator.prototype.return-throws are reported correctly.
            if not DoneFlag then
              CloseIterator(TGocciaIteratorValue(IteratorValue));
            Exit;
          end;
        until DoneFlag;
      except
        // §7.4.10 step 5 abrupt-completion path: DirectNext (and any
        // user code it calls) can throw.  Close the iterator, swallow
        // any error from iter.return(), and re-raise the original.
        CloseIteratorPreservingError(TGocciaIteratorValue(IteratorValue));
        raise;
      end;
      Exit;
    end;

    if IteratorValue is TGocciaObjectValue then
    begin
      // Same ALimit = 0 short-circuit as above.
      if ALimit = 0 then
      begin
        CloseRawIterator(IteratorValue);
        Exit;
      end;
      // ES2024 §7.4.2 GetIteratorDirect: NextMethod is captured ONCE
      // at iteratorRecord creation, not re-resolved per IteratorStep.
      // Hoisting the GetProperty + IsCallable validation out of the
      // loop matches the spec semantic (post-acquisition mutations of
      // `iterator.next` are ignored — §7.4.5 IteratorStep calls the
      // captured iteratorRecord.[[NextMethod]]) and avoids a redundant
      // hash lookup per iteration.  A missing/non-callable next is a
      // TypeError, not silent termination — see the try/except arm
      // below for the abrupt-completion close.
      NextMethod := IteratorValue.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or
         (NextMethod is TGocciaUndefinedLiteralValue) or
         not NextMethod.IsCallable then
        ThrowTypeError(SErrorIteratorNextMustBeCallable,
          SSuggestIteratorProtocol);
      try
        repeat
          CheckExecutionTimeout;
          CheckInstructionLimit;
          CallArgs := AcquireArguments;
          try
            NextResult := TGocciaFunctionBase(NextMethod).Call(CallArgs, IteratorValue);
          finally
            ReleaseArguments(CallArgs);
          end;
          // Only async iterators yield a Promise from next().  For sync
          // iteration (the default — destructuring, spread, for-of), the
          // result is the IteratorResult object directly; calling
          // AwaitValue on it would needlessly pump microtasks and open a
          // re-entrancy window where time-of-check / time-of-use bugs can
          // surface (e.g. user code mutating the result between unwrap and
          // PROP_DONE / PROP_VALUE reads).  Mirror the spec's split between
          // §7.4.7 IteratorStep (sync) and §7.4.13 AsyncIteratorStep.
          if ATryAsync then
            NextResult := AwaitValue(NextResult);
          if NextResult.IsPrimitive then
            ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.ToStringLiteral.Value]),
              SSuggestIteratorResultObject);
          DoneValue := NextResult.GetProperty(PROP_DONE);
          DoneFlag := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
          if not DoneFlag then
          begin
            // Normalize missing IteratorResult.value to undefined, matching
            // the well-defined behaviour in TryIterableToArray.  Without
            // this, GetProperty(PROP_VALUE) returning nil would store nil
            // in the array, causing later .Elements[I] reads to crash or
            // misclassify a missing result as "absent" in destructuring.
            Value := NextResult.GetProperty(PROP_VALUE);
            if not Assigned(Value) then
              Value := TGocciaUndefinedLiteralValue.UndefinedValue;
            Result.Elements.Add(Value);
          end;
          if (ALimit > 0) and (Result.Elements.Count >= ALimit) then
          begin
            // ES2024 §7.4.10 step 5 (normal completion): errors from
            // iter.return() propagate as the new completion.
            if not DoneFlag then
              CloseRawIterator(IteratorValue);
            Exit;
          end;
        until DoneFlag;
      except
        // §7.4.10 step 5 abrupt-completion path covering the
        // SErrorIteratorNextMustBeCallable throw above, the
        // SErrorIteratorResultNotObject throw, AwaitValue rejections,
        // and any error raised by user-supplied next().  iter.return()
        // is best-effort and must not replace the original error.
        CloseRawIteratorPreservingError(IteratorValue);
        raise;
      end;
      Exit;
    end;
  finally
    if IteratorRooted then
      GC.RemoveTempRoot(IteratorValue);
    if ArrayRooted then
      GC.RemoveTempRoot(Result);
  end;
end;

function TGocciaVM.TryIterableToArray(const AIterable: TGocciaValue;
  out AArray: TGocciaArrayValue): Boolean;
var
  IteratorValue, IteratorMethod, IteratorObject, NextMethod, NextResult,
    DoneValue, Value: TGocciaValue;
  IteratorSource: TGocciaObjectValue;
  DoneFlag: Boolean;
  ArrayRooted, IteratorRooted: Boolean;
  CallArgs: TGocciaArgumentsCollection;
begin
  AArray := nil;
  ArrayRooted := False;
  IteratorRooted := False;
  // NextMethod is only consumed by the OBJECT loop further down; the
  // TGocciaIteratorValue path uses DirectNext and exits early.  Init
  // here so FPC's uninitialized-local analysis is unambiguous.
  NextMethod := nil;

  if AIterable is TGocciaIteratorValue then
    IteratorValue := AIterable
  else
  begin
    if AIterable is TGocciaObjectValue then
      IteratorSource := TGocciaObjectValue(AIterable)
    else
      IteratorSource := AIterable.Box;

    if not Assigned(IteratorSource) then
      Exit(False);

    IteratorMethod := IteratorSource.GetSymbolProperty(
      TGocciaSymbolValue.WellKnownIterator);
    if not Assigned(IteratorMethod) or
       (IteratorMethod is TGocciaUndefinedLiteralValue) then
      Exit(False);
    if not IteratorMethod.IsCallable then
      ThrowTypeError('Object [Symbol.iterator] must be callable');

    CallArgs := AcquireArguments;
    try
      IteratorObject := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AIterable);
    finally
      ReleaseArguments(CallArgs);
    end;

    if IteratorObject is TGocciaIteratorValue then
      IteratorValue := IteratorObject
    else if IteratorObject is TGocciaObjectValue then
    begin
      NextMethod := IteratorObject.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or
         (NextMethod is TGocciaUndefinedLiteralValue) or
         not NextMethod.IsCallable then
        ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
      IteratorValue := IteratorObject;
    end
    else
      ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
  end;

  AArray := TGocciaArrayValue.Create;
  ArrayRooted := Assigned(TGarbageCollector.Instance);
  if ArrayRooted then
    TGarbageCollector.Instance.AddTempRoot(AArray);
  IteratorRooted := Assigned(TGarbageCollector.Instance) and (IteratorValue <> AIterable);
  if IteratorRooted then
    TGarbageCollector.Instance.AddTempRoot(IteratorValue);
  try
    if IteratorValue is TGocciaIteratorValue then
    begin
      try
        repeat
          CheckExecutionTimeout;
          CheckInstructionLimit;
          NextResult := TGocciaIteratorValue(IteratorValue).DirectNext(DoneFlag);
          if not DoneFlag then
            AArray.Elements.Add(NextResult);
        until DoneFlag;
      except
        // §7.4.10 step 5 abrupt-completion path: DirectNext can throw
        // (user next() / wrapped iterator may execute arbitrary code).
        // Close the iterator while preserving the original error per
        // ES2024 IteratorClose semantics.
        CloseIteratorPreservingError(TGocciaIteratorValue(IteratorValue));
        raise;
      end;
      Exit(True);
    end;

    // ES2024 §7.4.2 GetIteratorDirect captures NextMethod ONCE at
    // iteratorRecord creation; §7.4.5 IteratorStep then calls that
    // captured reference.  NextMethod was already resolved and
    // validated at the acquisition site above (the `if IteratorObject
    // is TGocciaObjectValue` arm), so the loop just reuses it instead
    // of re-running GetProperty(PROP_NEXT) per iteration.
    try
      repeat
        CheckExecutionTimeout;
        CheckInstructionLimit;
        CallArgs := AcquireArguments;
        try
          NextResult := TGocciaFunctionBase(NextMethod).Call(CallArgs, IteratorValue);
        finally
          ReleaseArguments(CallArgs);
        end;
        if NextResult.IsPrimitive then
          ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.ToStringLiteral.Value]),
            SSuggestIteratorResultObject);
        DoneValue := NextResult.GetProperty(PROP_DONE);
        DoneFlag := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
        if not DoneFlag then
        begin
          Value := NextResult.GetProperty(PROP_VALUE);
          if not Assigned(Value) then
            Value := TGocciaUndefinedLiteralValue.UndefinedValue;
          AArray.Elements.Add(Value);
        end;
      until DoneFlag;
    except
      // §7.4.10 step 5 abrupt-completion path: covers user next()
      // throws and our SErrorIteratorResultNotObject TypeError.  Call
      // iter.return() best-effort and surface the original exception.
      CloseRawIteratorPreservingError(IteratorValue);
      raise;
    end;
    Result := True;
  finally
    if IteratorRooted then
      TGarbageCollector.Instance.RemoveTempRoot(IteratorValue);
    if ArrayRooted then
      TGarbageCollector.Instance.RemoveTempRoot(AArray);
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
  Entry: TPair<string, TGocciaValue>;
  SymbolEntry: TPair<TGocciaSymbolValue, TGocciaValue>;
  ExclusionKey: TGocciaValue;
  J: Integer;
  Excluded: Boolean;
begin
  Result := TGocciaObjectValue.Create;
  if ASource is TGocciaObjectValue then
    SourceObject := TGocciaObjectValue(ASource)
  else
    SourceObject := ASource.Box;
  if not Assigned(SourceObject) then
    Exit;

  for Entry in SourceObject.GetEnumerablePropertyEntries do
  begin
    Excluded := False;
    if Assigned(AExclusionKeys) then
      for J := 0 to AExclusionKeys.Elements.Count - 1 do
      begin
        ExclusionKey := AExclusionKeys.GetElement(J);
        if (ExclusionKey is TGocciaSymbolValue) then
          Continue;
        if ExclusionKey.ToStringLiteral.Value = Entry.Key then
        begin
          Excluded := True;
          Break;
        end;
      end;
    if not Excluded then
      Result.SetProperty(Entry.Key, Entry.Value);
  end;

  for SymbolEntry in SourceObject.GetEnumerableSymbolProperties do
  begin
    Excluded := False;
    if Assigned(AExclusionKeys) then
      for J := 0 to AExclusionKeys.Elements.Count - 1 do
      begin
        ExclusionKey := AExclusionKeys.GetElement(J);
        if (ExclusionKey is TGocciaSymbolValue) and
           (ExclusionKey = SymbolEntry.Key) then
        begin
          Excluded := True;
          Break;
        end;
      end;
    if not Excluded then
      Result.AssignSymbolProperty(SymbolEntry.Key, SymbolEntry.Value);
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
      if not (Result is TGocciaObjectValue) then
        ThrowTypeError(Format(SErrorIteratorResultNotObject,
          [Result.ToStringLiteral.Value]), SSuggestIteratorResultObject);
      NextMethod := Result.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or
         (NextMethod is TGocciaUndefinedLiteralValue) or
         not NextMethod.IsCallable then
        ThrowTypeError(SErrorAsyncIteratorNextNotCallable,
          SSuggestAsyncIteratorProtocol);
      // Note: the captured NextMethod is not propagated downstream
      // here.  TGocciaVMAsyncFromSyncIteratorValue is sync->async
      // wrapping only — for a TRUE async iterator (whose next()
      // already returns Promise<IteratorResult>) routing through
      // that wrapper would call GetProperty(PROP_DONE) on the
      // unresolved Promise and produce an infinite for-await-of
      // loop.  Capture-once for the async branch would require a
      // dedicated TGocciaGenericAsyncIteratorValue class; until
      // that exists, downstream consumers (for-await-of dispatch)
      // re-resolve PROP_NEXT per iteration.  The §7.4.2 validation
      // above still ensures `next` is callable at acquisition time.
      Exit;
    end
    else if Assigned(IteratorMethod) and
            not (IteratorMethod is TGocciaUndefinedLiteralValue) and
            not (IteratorMethod is TGocciaNullLiteralValue) then
      ThrowTypeError('Async iterator method is not callable');
  end;

  if AIterable is TGocciaIteratorValue then
  begin
    if ATryAsync then
      Exit(TGocciaVMAsyncFromSyncIteratorValue.Create(AIterable,
        AIterable.GetProperty(PROP_NEXT)));
    Exit(AIterable);
  end;

  if AIterable is TGocciaArrayValue then
  begin
    Result := TGocciaArrayIteratorValue.Create(AIterable, akValues);
    if ATryAsync then
      Exit(TGocciaVMAsyncFromSyncIteratorValue.Create(Result,
        Result.GetProperty(PROP_NEXT)));
    Exit;
  end;

  if AIterable is TGocciaStringLiteralValue then
  begin
    Result := TGocciaStringIteratorValue.Create(AIterable);
    if ATryAsync then
      Exit(TGocciaVMAsyncFromSyncIteratorValue.Create(Result,
        Result.GetProperty(PROP_NEXT)));
    Exit;
  end;

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
      begin
        if ATryAsync then
          Exit(TGocciaVMAsyncFromSyncIteratorValue.Create(IteratorObject,
            IteratorObject.GetProperty(PROP_NEXT)));
        Exit(IteratorObject);
      end;

      if IteratorObject is TGocciaObjectValue then
      begin
        NextMethod := IteratorObject.GetProperty(PROP_NEXT);
        if Assigned(NextMethod) and
           not (NextMethod is TGocciaUndefinedLiteralValue) and
           NextMethod.IsCallable then
        begin
          if ATryAsync then
            Exit(TGocciaVMAsyncFromSyncIteratorValue.Create(IteratorObject,
              NextMethod));
          // Wrap the raw iterator object in a TGocciaGenericIteratorValue
          // so callers (OP_ITER_NEXT, IterableToArray, etc.) get a
          // TGocciaIteratorValue with NextMethod captured ONCE per
          // ES2024 §7.4.2 GetIteratorDirect.  Pass the already-
          // resolved NextMethod via the two-arg constructor — the
          // single-arg overload would re-run GetProperty(PROP_NEXT),
          // re-opening the post-acquisition mutation window we just
          // closed by validating above.
          Exit(TGocciaGenericIteratorValue.Create(IteratorObject, NextMethod));
        end;
        // ES2024 §7.4.2 GetIteratorDirect step 2: a missing or
        // non-callable [[NextMethod]] is a TypeError specific to the
        // iterator protocol — the source IS an iterable (its
        // [@@iterator] returned an object), but that object doesn't
        // satisfy the iterator interface.  Falling through to
        // SErrorNotIterable would mis-attribute the failure to the
        // outer iterable; throw the protocol-specific message instead.
        if ATryAsync then
          ThrowTypeError(SErrorAsyncIteratorNextNotCallable,
            SSuggestAsyncIteratorProtocol);
        ThrowTypeError(SErrorIteratorNextMustBeCallable,
          SSuggestIteratorProtocol);
      end;
    end;
  end;

  ThrowTypeError(Format(SErrorNotIterable, [AIterable.TypeName]),
    SSuggestNotIterable);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.ConstructValue(const AConstructor: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  BytecodeFunction: TGocciaBytecodeFunctionValue;
  Context: TGocciaEvaluationContext;
  ConstructorName: string;
begin
  // ES2026 §28.1.1 [[Construct]](argumentsList, newTarget)
  if AConstructor is TGocciaProxyValue then
  begin
    Result := TGocciaProxyValue(AConstructor).ConstructTrap(AArguments);
    Exit;
  end;

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
    if TGocciaNativeFunctionValue(AConstructor).NotConstructable then
      ThrowTypeError(
        Format(SErrorNotConstructor,
          [TGocciaNativeFunctionValue(AConstructor).Name]),
        Format('''%s'' is not a constructor',
          [TGocciaNativeFunctionValue(AConstructor).Name]));
    ConstructorName := TGocciaNativeFunctionValue(AConstructor).Name;
    if Assigned(TGocciaCallStack.Instance) then
      TGocciaCallStack.Instance.Push(ConstructorName, '', 0, 0);
    try
      Result := TGocciaNativeFunctionValue(AConstructor).Call(
        AArguments, TGocciaHoleValue.HoleValue);
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
    Exit;
  end;

  if AConstructor is TGocciaBytecodeFunctionValue then
  begin
    BytecodeFunction := TGocciaBytecodeFunctionValue(AConstructor);
    if Assigned(BytecodeFunction.FClosure) and
       Assigned(BytecodeFunction.FClosure.Template) and
       (BytecodeFunction.FClosure.Template.IsGenerator or
        BytecodeFunction.FClosure.Template.IsAsync or
        BytecodeFunction.FClosure.Template.IsArrow) then
      ThrowTypeError(Format(SErrorNotConstructor,
        [BytecodeFunction.GetProperty(PROP_NAME).ToStringLiteral.Value]),
        SSuggestNotConstructorType);
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

  ThrowTypeError(Format(SErrorValueNotConstructor, [AConstructor.TypeName]),
    SSuggestNotConstructorType);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.ImportModuleValue(const APath: string): TGocciaValue;
var
  Module: TGocciaModule;
begin
  if not Assigned(FLoadModule) then
    ThrowTypeError(SErrorModuleNotAvailableInVM);

  Module := FLoadModule(APath, FCurrentModuleSourcePath);
  Result := CreateModuleNamespaceObject(Module);
end;

function TGocciaVM.ImportModuleValue(const APath, AReferrer: string): TGocciaValue;
var
  Module: TGocciaModule;
begin
  if not Assigned(FLoadModule) then
    ThrowTypeError(SErrorModuleNotAvailableInVM);

  Module := FLoadModule(APath, AReferrer);
  Result := CreateModuleNamespaceObject(Module);
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

  SetBytecodeHomeObject(AGetter, TargetObject);
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

  SetBytecodeHomeObject(ASetter, TargetObject);
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
  begin
    SetBytecodeHomeObject(AGetter, TGocciaObjectValue(ATarget));
    if (AName <> '') and (AName[1] = '#') then
      TGocciaClassValue(ATarget).AddPrivateGetter(
        Copy(AName, 2, MaxInt), TGocciaFunctionBase(AGetter))
    else
      TGocciaClassValue(ATarget).AddStaticGetter(AName, TGocciaFunctionBase(AGetter));
  end;
end;

procedure TGocciaVM.DefineStaticSetterProperty(const ATarget: TGocciaValue;
  const AName: string; const ASetter: TGocciaValue);
begin
  if ATarget is TGocciaClassValue then
  begin
    SetBytecodeHomeObject(ASetter, TGocciaObjectValue(ATarget));
    if (AName <> '') and (AName[1] = '#') then
      TGocciaClassValue(ATarget).AddPrivateSetter(
        Copy(AName, 2, MaxInt), TGocciaFunctionBase(ASetter))
    else
      TGocciaClassValue(ATarget).AddStaticSetter(AName, TGocciaFunctionBase(ASetter));
  end;
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
      SetBytecodeHomeObject(AGetter, TGocciaVMClassValue(ATarget).Prototype);
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
      SetBytecodeHomeObject(AGetter, TGocciaObjectValue(ATarget));
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
      SetBytecodeHomeObject(ASetter, TGocciaVMClassValue(ATarget).Prototype);
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
      SetBytecodeHomeObject(ASetter, TGocciaObjectValue(ATarget));
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
    SetBytecodeHomeObject(AGetter, TGocciaObjectValue(ATarget));
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
    SetBytecodeHomeObject(ASetter, TGocciaObjectValue(ATarget));
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

procedure TGocciaVM.DefineGlobalBinding(const AName: string;
  const AValue: TGocciaValue;
  const ADeclarationType: TGocciaDeclarationType);
begin
  if Assigned(FGlobalScope) then
    FGlobalScope.DefineLexicalBinding(AName, AValue, ADeclarationType);
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
      ThrowTypeError(Format(SErrorEnumMemberType, [Key]),
        SSuggestEnumValueType);

    EnumObj.DefineProperty(Key,
      TGocciaPropertyDescriptorData.Create(MemberValue, [pfEnumerable]));

    PairArr := TGocciaArrayValue.Create;
    PairArr.Elements.Add(TGocciaStringLiteralValue.Create(Key));
    PairArr.Elements.Add(MemberValue);
    Entries.Elements.Add(PairArr);
  end;

  InitializeEnumSymbols(EnumObj);
  EnumObj.PreventExtensions;
  Result := EnumObj;
end;

procedure TGocciaVM.RunClassInitializers(const AClassValue: TGocciaClassValue;
  const AInstance: TGocciaValue);
begin
  AClassValue.RunMethodInitializers(AInstance);
  AClassValue.RunFieldInitializers(AInstance);
  AClassValue.RunDecoratorFieldInitializers(AInstance);
end;

function TGocciaVM.MaterializeArguments(
  const AArguments: TGocciaRegisterArray): TGocciaArgumentsCollection;
var
  I: Integer;
begin
  Result := AcquireArguments(Length(AArguments));
  for I := 0 to High(AArguments) do
    Result.Add(RegisterToValue(AArguments[I]));
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

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).NativeSuperConstructor) then
  begin
    RunClassInitializers(AClassValue, AInstance);
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

procedure TGocciaVM.InvokeImplicitSuperInitializationRegisters(
  const AClassValue: TGocciaClassValue; const AInstance: TGocciaValue;
  const AArguments: TGocciaRegisterArray);
var
  BoxedArgs: TGocciaArgumentsCollection;
  BytecodeConstructor: TGocciaBytecodeFunctionValue;
begin
  if not Assigned(AClassValue) then
    Exit;

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).FConstructorValue) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    if TGocciaVMClassValue(AClassValue).FConstructorValue is TGocciaBytecodeFunctionValue then
    begin
      BytecodeConstructor := TGocciaBytecodeFunctionValue(
        TGocciaVMClassValue(AClassValue).FConstructorValue);
      if Assigned(BytecodeConstructor.FClosure) and
         Assigned(BytecodeConstructor.FClosure.Template) and
         (not BytecodeConstructor.FClosure.Template.IsAsync) then
      begin
        TGocciaVMClassValue(AClassValue).FVM.ExecuteClosureRegisters(
          BytecodeConstructor.FClosure, RegisterObject(AInstance), AArguments);
        Exit;
      end;
    end;

    BoxedArgs := MaterializeArguments(AArguments);
    try
      TGocciaVMClassValue(AClassValue).FVM.InvokeFunctionValue(
        TGocciaVMClassValue(AClassValue).FConstructorValue,
        BoxedArgs, AInstance);
    finally
      ReleaseArguments(BoxedArgs);
    end;
    Exit;
  end;

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).NativeSuperConstructor) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    Exit;
  end;

  if Assigned(AClassValue.ConstructorMethod) then
  begin
    BoxedArgs := MaterializeArguments(AArguments);
    try
      RunClassInitializers(AClassValue, AInstance);
      AClassValue.ConstructorMethod.Call(BoxedArgs, AInstance);
    finally
      ReleaseArguments(BoxedArgs);
    end;
    Exit;
  end;

  InvokeImplicitSuperInitializationRegisters(AClassValue.SuperClass, AInstance, AArguments);
  if not (AClassValue is TGocciaVMClassValue) and
     (AInstance is TGocciaInstanceValue) then
  begin
    BoxedArgs := MaterializeArguments(AArguments);
    try
      TGocciaInstanceValue(AInstance).InitializeNativeFromArguments(BoxedArgs);
    finally
      ReleaseArguments(BoxedArgs);
    end;
  end;
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
    ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);

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
      ThrowTypeError(SErrorClassDecoratorReturn, SSuggestDecoratorFunction);
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
    ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);

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
          ThrowTypeError(SErrorMethodDecoratorReturn, SSuggestDecoratorFunction);
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
          ThrowTypeError(SErrorGetterDecoratorReturn, SSuggestDecoratorFunction);
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
          ThrowTypeError(SErrorSetterDecoratorReturn, SSuggestDecoratorFunction);
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
          ThrowTypeError(SErrorFieldDecoratorReturn, SSuggestDecoratorFunction);
        TGocciaClassValue(ClassVal).AddFieldInitializer(
          Name, DecoratorResult, IsPrivate, IsStatic);
      end;
    'a':
      begin
        if not (DecoratorResult is TGocciaObjectValue) then
          ThrowTypeError(SErrorAccessorDecoratorReturn, SSuggestDecoratorFunction);
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
  SuperObject: TGocciaObjectValue;
  HomeObject: TGocciaObjectValue;
  SuperPrototype: TGocciaValue;
begin
  HomeObject := nil;
  if Assigned(FCurrentClosure) then
    HomeObject := FCurrentClosure.HomeObject;

  if (ASuperValue is TGocciaObjectValue) and
     (not (ASuperValue is TGocciaClassValue)) and
     ASuperValue.IsCallable then
  begin
    SuperObject := TGocciaObjectValue(ASuperValue);
    if AName = PROP_CONSTRUCTOR then
      Exit(TGocciaVMSuperConstructorValue.Create(SuperObject));

    if AThisValue is TGocciaClassValue then
      Exit(SuperObject.GetPropertyWithContext(AName, AThisValue));

    if Assigned(HomeObject) then
    begin
      SuperPrototype := HomeObject.Prototype;
      if SuperPrototype is TGocciaObjectValue then
        Exit(TGocciaObjectValue(SuperPrototype).GetPropertyWithContext(
          AName, AThisValue));
      Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
    end;

    SuperPrototype := SuperObject.GetProperty(PROP_PROTOTYPE);
    if SuperPrototype is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SuperPrototype).GetPropertyWithContext(
        AName, AThisValue));

    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if not (ASuperValue is TGocciaClassValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  SuperClass := TGocciaClassValue(ASuperValue);
  if AName = PROP_CONSTRUCTOR then
    Exit(TGocciaVMSuperConstructorValue.Create(SuperClass));

  if AThisValue is TGocciaClassValue then
    Exit(SuperClass.GetProperty(AName));

  if Assigned(HomeObject) then
  begin
    SuperPrototype := HomeObject.Prototype;
    if SuperPrototype is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SuperPrototype).GetPropertyWithContext(
        AName, AThisValue));
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if Assigned(SuperClass.Prototype) then
    Exit(SuperClass.Prototype.GetPropertyWithContext(AName, AThisValue));

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.GetSuperPropertyValueByKey(const ASuperValue, AThisValue,
  AKey: TGocciaValue): TGocciaValue;
var
  SuperClass: TGocciaClassValue;
  SuperObject: TGocciaObjectValue;
  HomeObject: TGocciaObjectValue;
  SuperPrototype: TGocciaValue;
begin
  if not (AKey is TGocciaSymbolValue) then
    Exit(GetSuperPropertyValue(ASuperValue, AThisValue,
      KeyToPropertyName(AKey)));

  HomeObject := nil;
  if Assigned(FCurrentClosure) then
    HomeObject := FCurrentClosure.HomeObject;

  if (ASuperValue is TGocciaObjectValue) and
     (not (ASuperValue is TGocciaClassValue)) and
     ASuperValue.IsCallable then
  begin
    SuperObject := TGocciaObjectValue(ASuperValue);
    if AThisValue is TGocciaClassValue then
      Exit(SuperObject.GetSymbolPropertyWithReceiver(
        TGocciaSymbolValue(AKey), AThisValue));

    if Assigned(HomeObject) then
    begin
      SuperPrototype := HomeObject.Prototype;
      if SuperPrototype is TGocciaObjectValue then
        Exit(TGocciaObjectValue(SuperPrototype).GetSymbolPropertyWithReceiver(
          TGocciaSymbolValue(AKey), AThisValue));
      Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
    end;

    SuperPrototype := SuperObject.GetProperty(PROP_PROTOTYPE);
    if SuperPrototype is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SuperPrototype).GetSymbolPropertyWithReceiver(
        TGocciaSymbolValue(AKey), AThisValue));

    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if not (ASuperValue is TGocciaClassValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  SuperClass := TGocciaClassValue(ASuperValue);
  if AThisValue is TGocciaClassValue then
    Exit(SuperClass.GetSymbolPropertyWithReceiver(
      TGocciaSymbolValue(AKey), AThisValue));

  if Assigned(HomeObject) then
  begin
    SuperPrototype := HomeObject.Prototype;
    if SuperPrototype is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SuperPrototype).GetSymbolPropertyWithReceiver(
        TGocciaSymbolValue(AKey), AThisValue));
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if Assigned(SuperClass.Prototype) then
    Exit(SuperClass.Prototype.GetSymbolPropertyWithReceiver(
      TGocciaSymbolValue(AKey), AThisValue));

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.GetPropertyValue(const AObject: TGocciaValue;
  const AKey: string): TGocciaValue;
var
  Boxed: TGocciaObjectValue;
  Current: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PrivateName: string;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull, [AKey]),
      SSuggestCheckNullBeforeAccess);
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined, [AKey]),
      SSuggestCheckNullBeforeAccess);

  if (AKey <> '') and (AKey[1] = '#') then
  begin
    PrivateName := Copy(AKey, 2, MaxInt);
    if AObject is TGocciaClassValue then
    begin
      if TGocciaClassValue(AObject).HasOwnPrivateGetter(PrivateName) then
      begin
        EmptyArgs := TGocciaArgumentsCollection.Create;
        try
          Exit(TGocciaClassValue(AObject).GetOwnPrivatePropertyGetter(
            PrivateName).Call(
            EmptyArgs, AObject));
        finally
          EmptyArgs.Free;
        end;
      end;
      if TGocciaClassValue(AObject).HasOwnPrivateSetter(PrivateName) then
        ThrowTypeError(Format(SErrorPrivateAccessorNoGetter, [AKey]),
          SSuggestPrivateFieldAccess);
      if TryGetRawPrivateValue(AObject, AKey, Result) then
        Exit;
      ThrowTypeError(Format(SErrorPrivateFieldNotAccessible, [AKey]),
        SSuggestPrivateFieldAccess);
    end;

    if AObject is TGocciaObjectValue then
    begin
      Current := TGocciaObjectValue(AObject);
      while Assigned(Current) do
      begin
        Descriptor := Current.GetOwnPropertyDescriptor(AKey);
        if Descriptor is TGocciaPropertyDescriptorAccessor then
        begin
          if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
            Exit(AObject.GetProperty(AKey));
          ThrowTypeError(Format(SErrorPrivateAccessorNoGetter, [AKey]),
            SSuggestPrivateFieldAccess);
        end;
        Current := Current.Prototype;
      end;
    end;

    if TryGetRawPrivateValue(AObject, AKey, Result) then
      Exit;
  end
  else if TryGetRawPrivateValue(AObject, AKey, Result) then
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
  SetterArgs: TGocciaArgumentsCollection;
  PrivateName: string;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, [AKey]),
      SSuggestCheckNullBeforeAccess);
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfUndefined, [AKey]),
      SSuggestCheckNullBeforeAccess);
  if (AKey <> '') and (AKey[1] = '#') then
  begin
    PrivateName := Copy(AKey, 2, MaxInt);
    if AObject is TGocciaClassValue then
    begin
      if TGocciaClassValue(AObject).HasOwnPrivateSetter(PrivateName) then
      begin
        SetterArgs := TGocciaArgumentsCollection.Create([AValue]);
        try
          TGocciaClassValue(AObject).GetOwnPrivatePropertySetter(
            PrivateName).Call(
            SetterArgs, AObject);
        finally
          SetterArgs.Free;
        end;
        Exit;
      end;
      if TGocciaClassValue(AObject).HasOwnPrivateGetter(PrivateName) then
        ThrowTypeError(Format(SErrorPrivateAccessorNoSetter, [AKey]),
          SSuggestPrivateFieldAccess);
      if TGocciaClassValue(AObject).HasOwnPrivateStaticProperty(AKey) then
      begin
        TGocciaClassValue(AObject).AddPrivateStaticProperty(AKey, AValue);
        Exit;
      end;
      ThrowTypeError(Format(SErrorPrivateFieldNotAccessible, [AKey]),
        SSuggestPrivateFieldAccess);
    end;

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
        if Descriptor is TGocciaPropertyDescriptorAccessor then
          ThrowTypeError(Format(SErrorPrivateAccessorNoSetter, [AKey]),
            SSuggestPrivateFieldAccess);
        Current := Current.Prototype;
      end;
    end;
    SetRawPrivateValue(AObject, AKey, AValue);
    Exit;
  end;

  if not (AObject is TGocciaObjectValue) then
    ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
      SSuggestCheckNullBeforeAccess);

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
  begin
    if AKey is TGocciaSymbolValue then
      ThrowTypeError(Format(SErrorCannotUseInOperator, [TGocciaSymbolValue(AKey).ToDisplayString.Value, AObject.ToStringLiteral.Value]),
        SSuggestCheckNullBeforeAccess)
    else
      ThrowTypeError(Format(SErrorCannotUseInOperator, [AKey.ToStringLiteral.Value, AObject.ToStringLiteral.Value]),
        SSuggestCheckNullBeforeAccess);
  end;

  if (AKey is TGocciaSymbolValue) and (AObject is TGocciaObjectValue) then
  begin
    // ES2026 §28.1.1 [[HasProperty]](P) — symbol key
    if AObject is TGocciaProxyValue then
    begin
      if TGocciaProxyValue(AObject).HasSymbolTrap(TGocciaSymbolValue(AKey)) then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    end;
    if TGocciaObjectValue(AObject).HasSymbolProperty(TGocciaSymbolValue(AKey)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  KeyStr := KeyToPropertyName(AKey);

  // ES2026 §28.1.1 [[HasProperty]](P) — string key
  if AObject is TGocciaProxyValue then
  begin
    if TGocciaProxyValue(AObject).HasTrap(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

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

function TGocciaVM.MatchHasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
var
  KeyStr: string;
  Boxed: TGocciaObjectValue;
  Prop: TGocciaValue;
begin
  if (AObject is TGocciaNullLiteralValue) or
     (AObject is TGocciaUndefinedLiteralValue) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  if AKey is TGocciaSymbolValue then
  begin
    if AObject is TGocciaObjectValue then
    begin
      if AObject is TGocciaProxyValue then
      begin
        if TGocciaProxyValue(AObject).HasSymbolTrap(TGocciaSymbolValue(AKey)) then
          Exit(TGocciaBooleanLiteralValue.TrueValue);
        Exit(TGocciaBooleanLiteralValue.FalseValue);
      end;
      if VMHasSymbolPropertyInChain(TGocciaObjectValue(AObject),
        TGocciaSymbolValue(AKey)) then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    end;

    Boxed := AObject.Box;
    if Assigned(Boxed) and VMHasSymbolPropertyInChain(Boxed,
      TGocciaSymbolValue(AKey)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  KeyStr := KeyToPropertyName(AKey);
  if AObject is TGocciaProxyValue then
  begin
    if TGocciaProxyValue(AObject).HasTrap(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  if AObject is TGocciaObjectValue then
  begin
    if TGocciaObjectValue(AObject).HasProperty(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  Boxed := AObject.Box;
  if Assigned(Boxed) then
  begin
    if Boxed.HasProperty(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  Prop := AObject.GetProperty(KeyStr);
  if Assigned(Prop) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaVM.MatchExtractorValue(const ASubject,
  AMatcher: TGocciaValue): TGocciaValue;
var
  CustomMatcher, Extracted: TGocciaValue;
  MatchHintObject: TGocciaObjectValue;
  ExtractedArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  ObjectConstructorValue, FunctionConstructorValue: TGocciaValue;
begin
  CustomMatcher := GetCustomMatcher(AMatcher);
  if not Assigned(CustomMatcher) then
  begin
    if AMatcher is TGocciaClassValue then
    begin
      ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
      FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
      if VMInstanceOfValue(ASubject, AMatcher, ObjectConstructorValue,
         FunctionConstructorValue).ToBooleanLiteral.Value then
        Exit(TGocciaArrayValue.Create);
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    end;

    ThrowTypeError('Extractor pattern requires a custom matcher');
  end;

  if not CustomMatcher.IsCallable then
    ThrowTypeError('Symbol.customMatcher must be callable');

  CallArgs := AcquireArguments(2);
  try
    MatchHintObject := TGocciaObjectValue.Create;
    MatchHintObject.AssignProperty(PROP_MATCH_TYPE,
      TGocciaStringLiteralValue.Create('extractor'));
    CallArgs.Add(ASubject);
    CallArgs.Add(MatchHintObject);
    Extracted := InvokeFunctionValue(CustomMatcher, CallArgs, AMatcher);
  finally
    ReleaseArguments(CallArgs);
  end;

  if Extracted is TGocciaBooleanLiteralValue then
  begin
    if not TGocciaBooleanLiteralValue(Extracted).Value then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    Exit(TGocciaArrayValue.Create);
  end;

  if not TryIterableToArray(Extracted, ExtractedArray) then
    ThrowTypeError('Extractor pattern result must be true, false, or iterable');
  Result := ExtractedArray;
end;

function TGocciaVM.InvokeFunctionValue(const ACallee: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CalleeDesc: string;
begin
  // ES2026 §28.1.1 [[Call]](thisArgument, argumentsList)
  if ACallee is TGocciaProxyValue then
    Exit(TGocciaProxyValue(ACallee).ApplyTrap(AArguments, AThisValue));
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
  ThrowTypeError(Format(SErrorValueNotFunction, [CalleeDesc]),
    SSuggestNotFunctionType);
end;

function TGocciaVM.ExecuteClosureRegisters(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray): TGocciaRegister;
begin
  CheckExecutionTimeout;
  CheckInstructionLimit;
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue, AArguments,
    Length(AArguments), RegisterUndefined, RegisterUndefined, RegisterUndefined,
    False);
end;

function TGocciaVM.ExecuteClosureRegisters0(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 0, RegisterUndefined, RegisterUndefined,
    RegisterUndefined, True);
end;

function TGocciaVM.ExecuteClosureRegisters1(const AClosure: TGocciaBytecodeClosure;
  const AThisValue, AArg0: TGocciaRegister): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 1, AArg0, RegisterUndefined, RegisterUndefined,
    True);
end;

function TGocciaVM.ExecuteClosureRegisters2(const AClosure: TGocciaBytecodeClosure;
  const AThisValue, AArg0, AArg1: TGocciaRegister): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 2, AArg0, AArg1, RegisterUndefined, True);
end;

function TGocciaVM.ExecuteClosureRegisters3(const AClosure: TGocciaBytecodeClosure;
  const AThisValue, AArg0, AArg1, AArg2: TGocciaRegister): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 3, AArg0, AArg1, AArg2, True);
end;

procedure TGocciaVM.PushFrame(const AResultRegister, AFrameIP: Integer;
  const ATemplate: TGocciaFunctionTemplate;
  const APrevCovLine: UInt32; const AProfileTimestamp: Int64);
begin
  CheckStackDepth(FFrameDepth + 1);
  if FFrameStackCount >= Length(FFrameStack) then
    SetLength(FFrameStack, FFrameStackCount * 2 + 8);
  FFrameStack[FFrameStackCount].Template := ATemplate;
  FFrameStack[FFrameStackCount].IP := AFrameIP;
  FFrameStack[FFrameStackCount].ReturnRegister := AResultRegister;
  FFrameStack[FFrameStackCount].RegisterBase := FRegisterBase;
  FFrameStack[FFrameStackCount].RegisterCount := FRegisterCount;
  FFrameStack[FFrameStackCount].LocalCellBase := FLocalCellBase;
  FFrameStack[FFrameStackCount].LocalCellCount := FLocalCellCount;
  FFrameStack[FFrameStackCount].ArgCount := FArgCount;
  FFrameStack[FFrameStackCount].Closure := FCurrentClosure;
  FFrameStack[FFrameStackCount].HandlerCount := FHandlerStack.Count;
  FFrameStack[FFrameStackCount].PrevCovLine := APrevCovLine;
  FFrameStack[FFrameStackCount].ProfileEntryTimestamp := AProfileTimestamp;
  Inc(FFrameStackCount);
end;

function TGocciaVM.PopFrame(var AFrame: TGocciaVMCallFrame;
  out ATemplate: TGocciaFunctionTemplate;
  out APrevCovLine: UInt32; out AProfileTimestamp: Int64): Integer;
begin
  Dec(FFrameStackCount);
  ATemplate := FFrameStack[FFrameStackCount].Template;
  AFrame.IP := FFrameStack[FFrameStackCount].IP;
  AFrame.Template := ATemplate;
  FRegisterBase := FFrameStack[FFrameStackCount].RegisterBase;
  FRegisterCount := FFrameStack[FFrameStackCount].RegisterCount;
  FRegisters := @FRegisterStack[FRegisterBase];
  FLocalCellBase := FFrameStack[FFrameStackCount].LocalCellBase;
  FLocalCellCount := FFrameStack[FFrameStackCount].LocalCellCount;
  FLocalCells := @FLocalCellStack[FLocalCellBase];
  FArgCount := FFrameStack[FFrameStackCount].ArgCount;
  FCurrentClosure := FFrameStack[FFrameStackCount].Closure;
  APrevCovLine := FFrameStack[FFrameStackCount].PrevCovLine;
  AProfileTimestamp := FFrameStack[FFrameStackCount].ProfileEntryTimestamp;
  Result := FFrameStack[FFrameStackCount].ReturnRegister;
end;

procedure TGocciaVM.TeardownCurrentFrame(const ATemplate: TGocciaFunctionTemplate;
  const AProfileTimestamp: Int64; const ATargetHandlerCount: Integer);
begin
  if FProfilingFunctions and Assigned(ATemplate) and
     (ATemplate.ProfileIndex >= 0) then
    TGocciaProfiler.Instance.PopFunction(ATemplate.ProfileIndex, GetNanoseconds);
  if Assigned(TGocciaCallStack.Instance) then
    TGocciaCallStack.Instance.Pop;
  while FHandlerStack.Count > ATargetHandlerCount do
    FHandlerStack.Pop;
  Dec(FFrameDepth);
end;

procedure TGocciaVM.SetupNewFrame(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray;
  const AArgCount: Integer; const AArg0, AArg1, AArg2: TGocciaRegister;
  const AUseFixedArgs: Boolean;
  var AFrame: TGocciaVMCallFrame; out ATemplate: TGocciaFunctionTemplate;
  out APrevCovLine: UInt32; out AProfileTimestamp: Int64);
var
  I: Integer;
begin
  AProfileTimestamp := 0;
  AcquireRegisters(Max(AClosure.Template.MaxRegisters, 1));
  AcquireLocalCells(Max(AClosure.Template.MaxRegisters, 1));
  FArgCount := AArgCount;
  FCurrentClosure := AClosure;
  Inc(FFrameDepth);
  if Assigned(TGocciaCallStack.Instance) then
    TGocciaCallStack.Instance.Push(
      AClosure.Template.Name,
      FCurrentModuleSourcePath,
      0, 0);

  FillChar(AFrame, SizeOf(AFrame), 0);
  ATemplate := AClosure.Template;
  AFrame.Template := ATemplate;

  if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and
     Assigned(ATemplate.DebugInfo) and (ATemplate.DebugInfo.LineMapCount > 0) then
    TGocciaCoverageTracker.Instance.RecordLineHit(
      ATemplate.DebugInfo.SourceFile,
      ATemplate.DebugInfo.GetLineMapEntry(0).Line);

  if FProfilingFunctions and Assigned(TGocciaProfiler.Instance) then
  begin
    if ATemplate.ProfileIndex < 0 then
    begin
      if Assigned(ATemplate.DebugInfo) and (ATemplate.DebugInfo.LineMapCount > 0) then
        ATemplate.ProfileIndex := TGocciaProfiler.Instance.RegisterTemplate(
          ATemplate.Name, ATemplate.DebugInfo.SourceFile,
          ATemplate.DebugInfo.GetLineMapEntry(0).Line)
      else
        ATemplate.ProfileIndex := TGocciaProfiler.Instance.RegisterTemplate(
          ATemplate.Name, '', 0);
    end;
    AProfileTimestamp := GetNanoseconds;
    TGocciaProfiler.Instance.PushFunction(
      ATemplate.ProfileIndex, AProfileTimestamp);
  end;

  SetLocalRaw(0, AThisValue);
  if AUseFixedArgs then
    case AArgCount of
      1:
        SetLocalRaw(1, AArg0);
      2:
        begin
          SetLocalRaw(1, AArg0);
          SetLocalRaw(2, AArg1);
        end;
      3:
        begin
          SetLocalRaw(1, AArg0);
          SetLocalRaw(2, AArg1);
          SetLocalRaw(3, AArg2);
        end;
    end
  else
    for I := 0 to High(AArguments) do
      SetLocalRaw(I + 1, AArguments[I]);

  if FCoverageEnabled and Assigned(ATemplate.DebugInfo) and
     (ATemplate.DebugInfo.LineMapCount > 0) then
    APrevCovLine := ATemplate.DebugInfo.GetLineMapEntry(0).Line
  else
    APrevCovLine := 0;
end;

procedure TGocciaVM.HandleExceptionUnwind(const AErrorValue: TGocciaValue;
  const AInitialFrameStackCount, ASavedHandlerCount: Integer;
  var AFrame: TGocciaVMCallFrame; var ATemplate: TGocciaFunctionTemplate;
  var APrevCovLine: UInt32; var AProfileTimestamp: Int64);
var
  Handler: TGocciaBytecodeHandlerEntry;
  TargetHandlerCount: Integer;
begin
  while True do
  begin
    if (not FHandlerStack.IsEmpty) and
       (FHandlerStack.Peek.FrameDepth = FFrameDepth) then
    begin
      Handler := FHandlerStack.Peek;
      FHandlerStack.Pop;
      AFrame.IP := Handler.CatchIP;
      SetRegister(Handler.CatchRegister, AErrorValue);
      Exit;
    end;
    // Outermost frame: let the finally block handle teardown
    if FFrameStackCount <= AInitialFrameStackCount then
      raise EGocciaBytecodeThrow.Create(AErrorValue);
    // Intermediate trampoline frame: tear down and pop to parent
    TeardownCurrentFrame(ATemplate, AProfileTimestamp,
      FFrameStack[FFrameStackCount - 1].HandlerCount);
    PopFrame(AFrame, ATemplate, APrevCovLine, AProfileTimestamp);
  end;
end;

function TGocciaVM.ExecuteClosureRegistersInternal(
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
  const AArguments: TGocciaRegisterArray; const AArgCount: Integer;
  const AArg0, AArg1, AArg2: TGocciaRegister; const AUseFixedArgs: Boolean): TGocciaRegister;
var
  Frame: TGocciaVMCallFrame;
  SavedRegisterBase: Integer;
  SavedRegisterCount: Integer;
  SavedLocalCellBase: Integer;
  SavedLocalCellCount: Integer;
  SavedArgCount: Integer;
  SavedClosure: TGocciaBytecodeClosure;
  SavedHandlerCount: Integer;
  InitialFrameStackCount: Integer;
  ReturnValue: TGocciaRegister;
  ResultReg: Integer;
  TargetHandlerCount: Integer;
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
  FunctionConstructorValue, ObjectConstructorValue: TGocciaValue;
  CustomMatcherValue, MatchResultValue: TGocciaValue;
  MatchHintObject: TGocciaObjectValue;
  BuiltinConstructorMatch: Boolean;
  RegisterArgs: TGocciaRegisterArray;
  BytecodeFunction: TGocciaBytecodeFunctionValue;
  BoundFunction: TGocciaBoundFunctionValue;
  JumpOffset: Integer;
  PrevCovLine, CovLine: UInt32;
  ProfileEntryTimestamp: Int64;
  DynImportPromise: TGocciaPromiseValue;
  SpreadArray: TGocciaArrayValue;
  RestoredContinuation: Boolean;
begin
  SavedRegisterBase := FRegisterBase;
  SavedRegisterCount := FRegisterCount;
  SavedLocalCellBase := FLocalCellBase;
  SavedLocalCellCount := FLocalCellCount;
  SavedArgCount := FArgCount;
  SavedClosure := FCurrentClosure;
  SavedHandlerCount := FHandlerStack.Count;
  InitialFrameStackCount := FFrameStackCount;
  try
    SetupNewFrame(AClosure, AThisValue, AArguments, AArgCount,
      AArg0, AArg1, AArg2, AUseFixedArgs,
      Frame, Template, PrevCovLine, ProfileEntryTimestamp);
    RestoredContinuation := Assigned(GActiveBytecodeGenerator) and
      GActiveBytecodeGenerator.RestoreContinuation(
        Frame, SavedHandlerCount, PrevCovLine);
    if RestoredContinuation then
    begin
      if GActiveBytecodeGenerator.FDelegateActive then
      begin
        if GActiveBytecodeGenerator.FResumeKind = bgrkNext then
          SetRegisterRaw(GActiveBytecodeGenerator.FResumeRegister,
            GActiveBytecodeGenerator.FResumeValue);
      end
      else
      begin
        case GActiveBytecodeGenerator.FResumeKind of
          bgrkNext:
            SetRegisterRaw(GActiveBytecodeGenerator.FResumeRegister,
              GActiveBytecodeGenerator.FResumeValue);
          bgrkThrow:
            HandleExceptionUnwind(
              RegisterToValue(GActiveBytecodeGenerator.FResumeValue),
              InitialFrameStackCount, SavedHandlerCount,
              Frame, Template, PrevCovLine, ProfileEntryTimestamp);
          bgrkReturn:
            begin
              GActiveBytecodeGenerator.FReturnValue :=
                RegisterToValue(GActiveBytecodeGenerator.FResumeValue);
              if not Assigned(GActiveBytecodeGenerator.FReturnSentinel) then
                GActiveBytecodeGenerator.FReturnSentinel := TGocciaObjectValue.Create;
              HandleExceptionUnwind(GActiveBytecodeGenerator.FReturnSentinel,
                InitialFrameStackCount, SavedHandlerCount,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
            end;
        end;
      end;
    end;
    Running := True;
    while Running and (Frame.IP < Template.CodeCount) do
    begin
      try
        CheckInstructionLimit;
        Instruction := Template.GetInstructionUnchecked(Frame.IP);
        Inc(Frame.IP);
        IncrementInstructionCounter;

        if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and
           Assigned(Template.DebugInfo) then
        begin
          CovLine := Template.DebugInfo.GetLineForPC(Frame.IP - 1);
          if (CovLine <> 0) and (CovLine <> PrevCovLine) then
          begin
            TGocciaCoverageTracker.Instance.RecordLineHit(
              Template.DebugInfo.SourceFile, CovLine);
            PrevCovLine := CovLine;
          end;
        end;

        Op := DecodeOp(Instruction);
        if FProfilingOpcodes then
          TGocciaProfiler.Instance.RecordOpcode(Op);
        A := DecodeA(Instruction);
        B := DecodeB(Instruction);
        C := DecodeC(Instruction);
        case TGocciaOpCode(Op) of
      OP_LOAD_CONST:
        // ES2026 §13.2.8.3: bckTemplateObject constants are lazily built and cached
        if Template.GetConstantUnchecked(DecodeBx(Instruction)).Kind = bckTemplateObject then
          FRegisters[A] := ValueToRegister(BuildTemplateObjectConstant(Template, DecodeBx(Instruction)))
        else
          FRegisters[A] := ValueToRegister(
            ConstantToValue(Template.GetConstantUnchecked(DecodeBx(Instruction))));

      OP_LOAD_UNDEFINED:
        FRegisters[A] := RegisterUndefined;

      OP_GET_THIS_BINDING:
        // ES2026 §9.4.3 ResolveThisBinding falls through to GetThisBinding
        // on the surrounding environment record.  At Script top level the
        // global env's [[GlobalThisValue]] is the global object; at Module
        // top level the module env's binding resolves to undefined.  The
        // active FGlobalScope already encodes that distinction (the
        // module loader rewires FGlobalScope to the module scope while
        // executing module bodies), so reading ThisValue here is correct
        // for both kinds without needing a compile-time flag.
        if Assigned(FGlobalScope) then
          FRegisters[A] := VMValueToRegisterFast(FGlobalScope.ThisValue)
        else
          FRegisters[A] := RegisterUndefined;

      OP_LOAD_TRUE:
        FRegisters[A] := RegisterBoolean(True);

      OP_LOAD_FALSE:
        FRegisters[A] := RegisterBoolean(False);

      OP_LOAD_NULL:
        FRegisters[A] := RegisterNull;

      OP_LOAD_HOLE:
        FRegisters[A] := RegisterHole;

      OP_CHECK_TYPE:
        VMStrictTypeCheckRegisterValue(GetRegister(A), TGocciaLocalType(B));

      OP_TO_PRIMITIVE:
        if FRegisters[B].Kind <> grkObject then
          FRegisters[A] := FRegisters[B]
        else
          SetRegisterFast(A, ToPrimitive(GetRegisterFast(B)));

      OP_LOAD_INT:
        FRegisters[A] := RegisterInt(DecodesBx(Instruction));

      OP_MOVE:
        SetRegisterRaw(A, FRegisters[B]);

      OP_GET_LOCAL:
        SetRegisterRaw(A, GetLocalRegister(DecodeBx(Instruction)));

      OP_SET_LOCAL:
        SetLocalRaw(DecodeBx(Instruction), FRegisters[A]);

      OP_GET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
            SetRegisterRaw(A, Upvalue.Cell.Value)
          else
            FRegisters[A] := RegisterUndefined;
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_SET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
            Upvalue.Cell.Value := FRegisters[A];
        end;
      end;

      OP_CLOSE_UPVALUE:
        if (A >= 0) and (A < FLocalCellCount) then
          FLocalCells[A] := nil;

      OP_ARG_COUNT:
        FRegisters[A] := RegisterInt(FArgCount);

      OP_PACK_ARGS:
      begin
        ArgsArray := TGocciaArrayValue.Create;
        for I := B to FArgCount - 1 do
          ArgsArray.Elements.Add(RegisterToValue(GetLocalRegister(I + 1)));
        FRegisters[A] := RegisterObject(ArgsArray);
      end;

      OP_JUMP:
      begin
        JumpOffset := DecodeAx(Instruction);
        Inc(Frame.IP, JumpOffset);
        if JumpOffset < 0 then
          CheckExecutionTimeout;
      end;

      OP_JUMP_IF_TRUE:
        if RegisterToBoolean(FRegisters[A]) then
        begin
          if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(Frame.IP - 1),
              Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 0);
          JumpOffset := DecodesBx(Instruction);
          Inc(Frame.IP, JumpOffset);
          if JumpOffset < 0 then
            CheckExecutionTimeout;
        end
        else if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(Frame.IP - 1),
            Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 1);

      OP_JUMP_IF_FALSE:
        if not RegisterToBoolean(FRegisters[A]) then
        begin
          if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(Frame.IP - 1),
              Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 0);
          JumpOffset := DecodesBx(Instruction);
          Inc(Frame.IP, JumpOffset);
          if JumpOffset < 0 then
            CheckExecutionTimeout;
        end
        else if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(Frame.IP - 1),
            Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 1);

      OP_JUMP_IF_NULLISH:
        if RegisterMatchesNullishKind(FRegisters[A], B) then
        begin
          if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(Frame.IP - 1),
              Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 0);
          Inc(Frame.IP, C);
        end
        else if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(Frame.IP - 1),
            Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 1);

      OP_JUMP_IF_NOT_NULLISH:
        if not RegisterMatchesNullishKind(FRegisters[A], B) then
        begin
          if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(Frame.IP - 1),
              Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 0);
          Inc(Frame.IP, C);
        end
        else if FCoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(Frame.IP - 1),
            Template.DebugInfo.GetColumnForPC(Frame.IP - 1), 1);

      OP_PUSH_HANDLER:
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth);

      OP_POP_HANDLER:
        if not FHandlerStack.IsEmpty then
          FHandlerStack.Pop;

      OP_ADD_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue +
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
            RegisterToDouble(FRegisters[C]));

      OP_ADD_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
          RegisterToDouble(FRegisters[C]));

      OP_SUB_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
            RegisterToDouble(FRegisters[C]));

      OP_SUB_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
          RegisterToDouble(FRegisters[C]));

      OP_MUL_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue *
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
            RegisterToDouble(FRegisters[C]));

      OP_MUL_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
          RegisterToDouble(FRegisters[C]));

      OP_DIV_INT, OP_DIV_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) /
          RegisterToDouble(FRegisters[C]));

      OP_MOD_INT, OP_MOD_FLOAT:
        FRegisters[A] := VMNumberRegister(FMod(RegisterToDouble(FRegisters[B]),
          RegisterToDouble(FRegisters[C])));

      OP_EQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) = RegisterToDouble(FRegisters[C]));

      OP_EQ_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) = RegisterToDouble(FRegisters[C]));

      OP_NEQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) <> RegisterToDouble(FRegisters[C]));

      OP_NEQ_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) <> RegisterToDouble(FRegisters[C]));

      OP_LT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue < FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) < RegisterToDouble(FRegisters[C]));

      OP_LT_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) < RegisterToDouble(FRegisters[C]));

      OP_GT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue > FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) > RegisterToDouble(FRegisters[C]));

      OP_GT_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) > RegisterToDouble(FRegisters[C]));

      OP_LTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <= FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) <= RegisterToDouble(FRegisters[C]));

      OP_LTE_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) <= RegisterToDouble(FRegisters[C]));

      OP_GTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue >= FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) >= RegisterToDouble(FRegisters[C]));

      OP_GTE_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) >= RegisterToDouble(FRegisters[C]));

      OP_NEG_INT, OP_NEG_FLOAT:
        FRegisters[A] := VMNumberRegister(-RegisterToDouble(FRegisters[B]));

      OP_CONCAT:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value +
            TGocciaStringLiteralValue(FRegisters[C].ObjectValue).Value))
        else
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            VMRegisterToECMAStringFast(FRegisters[B]).Value +
            VMRegisterToECMAStringFast(FRegisters[C]).Value));
      end;

      OP_NEW_ARRAY:
        SetRegister(A, TGocciaArrayValue.Create(nil, B));

      OP_ARRAY_POP:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          if TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count = 0 then
            FRegisters[A] := RegisterUndefined
          else
          begin
            FRegisters[A] := VMValueToRegisterFast(TGocciaArrayValue(
              FRegisters[B].ObjectValue).Elements[
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1]);
            TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Delete(
              TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1);
            if FRegisters[A].Kind = grkHole then
              FRegisters[A] := RegisterUndefined;
          end;
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_ARRAY_PUSH:
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaArrayValue) then
          TGocciaArrayValue(FRegisters[A].ObjectValue).Elements.Add(
            RegisterToValue(FRegisters[B]));

      OP_ARRAY_GET:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, TGocciaArrayValue(FRegisters[B].ObjectValue).GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else if TryGetArrayIndexRegister(FRegisters[C], KeyIndex) then
          begin
            if (KeyIndex >= 0) and
               (KeyIndex < TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count) and
               (TGocciaArrayValue(FRegisters[B].ObjectValue).Elements[KeyIndex] <>
                TGocciaHoleValue.HoleValue) then
              FRegisters[A] := VMValueToRegisterFast(
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements[KeyIndex])
            else
              // Hole, out-of-range, or accessor-shadowed slot: take the slow
              // path so accessor descriptors and prototype lookups run.
              SetRegister(A, TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(
                IntToStr(KeyIndex)));
          end
          else
            SetRegister(A, TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(
              KeyToPropertyNameRegister(FRegisters[C])));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaClassValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, TGocciaClassValue(FRegisters[B].ObjectValue).GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else
            SetRegister(A, TGocciaClassValue(FRegisters[B].ObjectValue).GetProperty(
              KeyToPropertyNameRegister(FRegisters[C])));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaObjectValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, TGocciaObjectValue(FRegisters[B].ObjectValue).GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else
            SetRegister(A, TGocciaObjectValue(FRegisters[B].ObjectValue).GetProperty(
              KeyToPropertyNameRegister(FRegisters[C])));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, FRegisters[B].ObjectValue.Box.GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else if TryGetArrayIndexRegister(FRegisters[C], KeyIndex) and
             (KeyIndex >= 0) and
             (KeyIndex < Length(TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value)) then
            SetRegister(A, TGocciaStringLiteralValue.Create(
              TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value[KeyIndex + 1]))
          else
            FRegisters[A] := RegisterUndefined;
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) and
             Assigned(TGocciaSymbolValue.SharedPrototype) then
            SetRegister(A, TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype)
              .GetSymbolPropertyWithReceiver(
                TGocciaSymbolValue(FRegisters[C].ObjectValue),
                FRegisters[B].ObjectValue))
          else
            SetRegister(A, FRegisters[B].ObjectValue.GetProperty(
              KeyToPropertyNameRegister(FRegisters[C])));
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_ARRAY_SET:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaArrayValue) then
        begin
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
            TGocciaArrayValue(FRegisters[A].ObjectValue).AssignSymbolProperty(
              TGocciaSymbolValue(FRegisters[B].ObjectValue), RegisterToValue(FRegisters[C]))
          else if TryGetArrayIndexRegister(FRegisters[B], KeyIndex) then
            TGocciaArrayValue(FRegisters[A].ObjectValue).SetElement(
              KeyIndex, RegisterToValue(FRegisters[C]))
          else
            TGocciaArrayValue(FRegisters[A].ObjectValue).SetProperty(
              KeyToPropertyNameRegister(FRegisters[B]),
              RegisterToValue(FRegisters[C]));
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaClassValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[C]),
            TGocciaObjectValue(FRegisters[A].ObjectValue));
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
            TGocciaClassValue(FRegisters[A].ObjectValue).AssignSymbolProperty(
              TGocciaSymbolValue(FRegisters[B].ObjectValue), RegisterToValue(FRegisters[C]))
          else
            TGocciaClassValue(FRegisters[A].ObjectValue).SetProperty(
              KeyToPropertyNameRegister(FRegisters[B]),
              RegisterToValue(FRegisters[C]));
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
            TGocciaObjectValue(FRegisters[A].ObjectValue).AssignSymbolProperty(
              TGocciaSymbolValue(FRegisters[B].ObjectValue), RegisterToValue(FRegisters[C]))
          else
            TGocciaObjectValue(FRegisters[A].ObjectValue).SetProperty(
              KeyToPropertyNameRegister(FRegisters[B]),
              RegisterToValue(FRegisters[C]));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
          ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
            SSuggestCheckNullBeforeAccess)
        else
          SetPropertyValue(GetRegister(A),
            KeyToPropertyNameRegister(FRegisters[B]),
            RegisterToValue(FRegisters[C]));
      end;

      OP_GET_LENGTH:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
          FRegisters[A] := VMNumberRegister(
            TGocciaArrayValue(FRegisters[B].ObjectValue).GetLength)
        else
          FRegisters[A] := RegisterInt(0);
      end;

      OP_NEW_OBJECT:
      begin
        if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
          TGocciaObjectValue.InitializeSharedPrototype;
        FRegisters[A] := RegisterObject(TGocciaVMLiteralObjectValue.Create(
          TGocciaObjectValue.SharedObjectPrototype,
          DecodeBx(Instruction)));
      end;

      OP_NEW_CLASS:
      begin
        FRegisters[A] := RegisterObject(TGocciaVMClassValue.Create(Self,
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue, nil));
        TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.DefineProperty(
          PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(
            FRegisters[A].ObjectValue, [pfConfigurable, pfWritable]));
      end;

      OP_CLASS_SET_SUPER:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
           (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaClassValue) then
        begin
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SuperClass :=
            TGocciaClassValue(FRegisters[B].ObjectValue);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).NativeSuperConstructor :=
            nil;
          // Set [[Prototype]] of derived class constructor to superclass
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetConstructorPrototype(
            TGocciaObjectValue(FRegisters[B].ObjectValue));
          // Set .prototype chain: DerivedClass.prototype.[[Prototype]] = SuperClass.prototype
          TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype :=
            TGocciaClassValue(FRegisters[B].ObjectValue).Prototype;
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
                (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaObjectValue) and
                FRegisters[B].ObjectValue.IsCallable then
        begin
          // Native constructor superclass: preserve static and prototype
          // inheritance links, and remember the constructor for instantiation.
          TGocciaVMClassValue(FRegisters[A].ObjectValue).LinkNativeSuperConstructor(
            TGocciaObjectValue(FRegisters[B].ObjectValue));
          RightValue := FRegisters[B].ObjectValue.GetProperty(PROP_PROTOTYPE);
          if RightValue is TGocciaNullLiteralValue then
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype := nil
          else if RightValue is TGocciaObjectValue then
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype :=
              TGocciaObjectValue(RightValue)
          else
            ThrowTypeError(
              'Superclass prototype must be an object or null',
              'set the superclass prototype property to an object or null');
        end;
      end;

      OP_CLASS_ADD_METHOD_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[C]),
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype);
          if GlobalName = PROP_CONSTRUCTOR then
          begin
            TGocciaVMClassValue(FRegisters[A].ObjectValue).SetVMConstructor(
              RegisterToValue(FRegisters[C]));
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.DefineProperty(
              PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(
                FRegisters[A].ObjectValue, [pfConfigurable, pfWritable]));
          end
          else
            // ES §14.3.7: class prototype methods are non-enumerable
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.DefineProperty(
              GlobalName, TGocciaPropertyDescriptorData.Create(
                RegisterToValue(FRegisters[C]), [pfConfigurable, pfWritable]));
        end
        else if (FRegisters[A].Kind = grkObject) and Assigned(FRegisters[A].ObjectValue) then
          SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RegisterToValue(FRegisters[C]))
        else
          SetPropertyValue(GetRegister(A), GlobalName, GetRegister(C));
      end;

      OP_CLASS_SET_FIELD_INITIALIZER:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetMethodInitializers(
            [RegisterToValue(FRegisters[B])]);
      end;

      OP_CLASS_DECLARE_PRIVATE_STATIC_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
          TGocciaVMClassValue(FRegisters[A].ObjectValue).AddPrivateStaticProperty(
            GlobalName, TGocciaUndefinedLiteralValue.UndefinedValue);
      end;

      // ES2022 §15.7.14: execute static block closure with this = class
      OP_CLASS_EXEC_STATIC_BLOCK:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          PushFrame(B, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
          SetupNewFrame(
            TGocciaBytecodeFunctionValue(FRegisters[B].ObjectValue).FClosure,
            FRegisters[A], TGocciaRegisterArray(nil), 0,
            RegisterUndefined, RegisterUndefined, RegisterUndefined, True,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
          Continue;
        end
        else if (FRegisters[B].Kind = grkObject) and
                Assigned(FRegisters[B].ObjectValue) and
                FRegisters[B].ObjectValue.IsCallable then
        begin
          CallArgs := AcquireArguments(0);
          try
            InvokeFunctionValue(RegisterToValue(FRegisters[B]),
              CallArgs, RegisterToValue(FRegisters[A]));
          finally
            ReleaseArguments(CallArgs);
          end;
        end;
      end;

      OP_GET_PROP_CONST:
        if (FRegisters[B].Kind = grkObject) and Assigned(FRegisters[B].ObjectValue) then
        begin
          GlobalName := Template.GetConstantUnchecked(C).StringValue;
          if (FRegisters[B].ObjectValue is TGocciaVMLiteralObjectValue) and
             TGocciaVMLiteralObjectValue(FRegisters[B].ObjectValue).TryGetOwnDataPropertyFastRegister(
               GlobalName, FRegisters[A]) then
            { fast path already assigned }
          else
            SetRegister(A, GetPropertyValue(FRegisters[B].ObjectValue, GlobalName));
        end
        else
          SetRegister(A, GetPropertyValue(GetRegister(B),
            Template.GetConstantUnchecked(C).StringValue));

      OP_SET_PROP_CONST:
        if (FRegisters[A].Kind = grkObject) and Assigned(FRegisters[A].ObjectValue) then
        begin
          GlobalName := Template.GetConstantUnchecked(B).StringValue;
          RightValue := RegisterToValue(FRegisters[C]);
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue,
              TGocciaObjectValue(FRegisters[A].ObjectValue));
          if FRegisters[A].ObjectValue is TGocciaVMLiteralObjectValue then
          begin
            if not TGocciaVMLiteralObjectValue(FRegisters[A].ObjectValue)
              .TrySetLiteralDataPropertyFast(GlobalName, RightValue) then
              SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RightValue);
          end
          else
            SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RightValue);
        end
        else
          SetPropertyValue(GetRegister(A),
            Template.GetConstantUnchecked(B).StringValue,
            GetRegister(C));

      OP_DELETE_PROP_CONST:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if TGocciaObjectValue(FRegisters[A].ObjectValue).DeleteProperty(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue) then
            FRegisters[A] := RegisterBoolean(True)
          else
            ThrowTypeError(Format(SErrorCannotDeletePropertyOf, [Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue, '[object Object]']),
              SSuggestCannotDeleteNonConfigurable);
        end
        else
          FRegisters[A] := RegisterBoolean(True);
      end;

      OP_UNPACK:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          ArgsArray := TGocciaArrayValue.Create;
          for I := C to TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1 do
            ArgsArray.Elements.Add(
              TGocciaArrayValue(FRegisters[B].ObjectValue).GetElement(I));
          FRegisters[A] := RegisterObject(ArgsArray);
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_GET_INDEX:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, TGocciaArrayValue(FRegisters[B].ObjectValue).GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else if TryGetArrayIndexRegister(FRegisters[C], KeyIndex) then
          begin
            if (KeyIndex >= 0) and
               (KeyIndex < TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count) and
               (TGocciaArrayValue(FRegisters[B].ObjectValue).Elements[KeyIndex] <>
                TGocciaHoleValue.HoleValue) then
              FRegisters[A] := VMValueToRegisterFast(
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements[KeyIndex])
            else
              // Hole, out-of-range, or accessor-shadowed slot: take the slow
              // path so accessor descriptors and prototype lookups run.
              SetRegister(A, TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(
                IntToStr(KeyIndex)));
          end
          else
            SetRegister(A, TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(
              KeyToPropertyNameRegister(FRegisters[C])));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaClassValue) then
        begin
          // Classes must be matched before TGocciaObjectValue so that static
          // symbol-keyed properties (FStaticSymbolDescriptors) and the
          // superclass walk are reached. TGocciaClassValue.GetSymbolProperty
          // is not virtual on the base, so a TGocciaObjectValue cast would
          // bypass them. Mirrors OP_ARRAY_GET ordering.
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, TGocciaClassValue(FRegisters[B].ObjectValue).GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else
            SetRegister(A, TGocciaClassValue(FRegisters[B].ObjectValue).GetProperty(
              KeyToPropertyNameRegister(FRegisters[C])));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaObjectValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, TGocciaObjectValue(FRegisters[B].ObjectValue).GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else if (FRegisters[B].ObjectValue is TGocciaVMLiteralObjectValue) and
                  TGocciaVMLiteralObjectValue(FRegisters[B].ObjectValue).TryGetOwnDataPropertyFastRegister(
                    KeyToPropertyNameRegister(FRegisters[C]), FRegisters[A]) then
            { fast path already assigned }
          else
            SetRegister(A, TGocciaObjectValue(FRegisters[B].ObjectValue).GetProperty(
              KeyToPropertyNameRegister(FRegisters[C])));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) then
        begin
          if (FRegisters[C].Kind = grkObject) and
             (FRegisters[C].ObjectValue is TGocciaSymbolValue) then
            SetRegister(A, FRegisters[B].ObjectValue.Box.GetSymbolProperty(
              TGocciaSymbolValue(FRegisters[C].ObjectValue)))
          else if TryGetArrayIndexRegister(FRegisters[C], KeyIndex) and
             (KeyIndex >= 0) and
             (KeyIndex < Length(TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value)) then
            SetRegister(A, TGocciaStringLiteralValue.Create(
              TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value[KeyIndex + 1]))
          else
            SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_SET_INDEX:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaArrayValue) then
        begin
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
            TGocciaArrayValue(FRegisters[A].ObjectValue).AssignSymbolProperty(
              TGocciaSymbolValue(FRegisters[B].ObjectValue), RegisterToValue(FRegisters[C]))
          else if TryGetArrayIndexRegister(FRegisters[B], KeyIndex) then
            TGocciaArrayValue(FRegisters[A].ObjectValue).SetElement(
              KeyIndex, RegisterToValue(FRegisters[C]))
          else
            TGocciaArrayValue(FRegisters[A].ObjectValue).SetProperty(
              KeyToPropertyNameRegister(FRegisters[B]),
              RegisterToValue(FRegisters[C]));
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaClassValue) then
        begin
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
            TGocciaClassValue(FRegisters[A].ObjectValue).AssignSymbolProperty(
              TGocciaSymbolValue(FRegisters[B].ObjectValue), RegisterToValue(FRegisters[C]))
          else
            TGocciaClassValue(FRegisters[A].ObjectValue).SetProperty(
              KeyToPropertyNameRegister(FRegisters[B]),
              RegisterToValue(FRegisters[C]));
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
            TGocciaObjectValue(FRegisters[A].ObjectValue).AssignSymbolProperty(
              TGocciaSymbolValue(FRegisters[B].ObjectValue), RegisterToValue(FRegisters[C]))
          else
            TGocciaObjectValue(FRegisters[A].ObjectValue).SetProperty(
              KeyToPropertyNameRegister(FRegisters[B]),
              RegisterToValue(FRegisters[C]));
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaSymbolValue) then
          ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
            SSuggestCheckNullBeforeAccess)
        else
          SetPropertyValue(GetRegister(A),
            KeyToPropertyNameRegister(FRegisters[B]),
            RegisterToValue(FRegisters[C]));
      end;

      OP_ADD:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue +
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
            RegisterToDouble(FRegisters[C]));
        end
        else begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarMiss;
          if (((FRegisters[B].Kind = grkObject) and
                  (FRegisters[B].ObjectValue is TGocciaStringLiteralValue)) or
                 ((FRegisters[C].Kind = grkObject) and
                  (FRegisters[C].ObjectValue is TGocciaStringLiteralValue))) and
                (not ((FRegisters[B].Kind = grkObject) and
                      Assigned(FRegisters[B].ObjectValue) and
                      (not FRegisters[B].ObjectValue.IsPrimitive))) and
                (not ((FRegisters[C].Kind = grkObject) and
                      Assigned(FRegisters[C].ObjectValue) and
                      (not FRegisters[C].ObjectValue.IsPrimitive))) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            VMRegisterToECMAStringFast(FRegisters[B]).Value +
            VMRegisterToECMAStringFast(FRegisters[C]).Value))
        else
        begin
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
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
        end;
      end;

      OP_SUB:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMSubtractValues(GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_MUL:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue *
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMMultiplyValues(GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_DIV:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) /
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMDivideValues(GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_MOD:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(FMod(RegisterToDouble(FRegisters[B]),
            RegisterToDouble(FRegisters[C])));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMModuloValues(GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_POW:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(Power(RegisterToDouble(FRegisters[B]),
            RegisterToDouble(FRegisters[C])));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMPowerValues(GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_NEG:
        if RegisterIsNumericScalar(FRegisters[B]) then
          FRegisters[A] := VMNumberRegister(-RegisterToDouble(FRegisters[B]))
        else
        begin
          // ES2026 §13.5.5 UnaryMinus invokes ToNumeric (= ToPrimitive
          // then a BigInt? branch).  Apply ToPrimitive so boxed BigInts
          // (Object(1n)) unbox to their primitive and take the
          // BigInt::unaryMinus path; without it the box's
          // ToNumberLiteral coerces to NaN and we lose the BigInt.
          LeftValue := ToPrimitive(GetRegisterFast(B));
          if LeftValue is TGocciaBigIntValue then
            SetRegister(A, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Negate))
          else
            SetRegister(A, VMNumberValue(-LeftValue.ToNumberLiteral.Value));
        end;

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
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else
          SetRegister(A, GetRegister(B).IsEqual(GetRegister(C)));

      OP_NEQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else
          SetRegister(A, GetRegister(B).IsNotEqual(GetRegister(C)));

      OP_LT:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue <
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) <
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              TGocciaStringLiteralValue(LeftValue).Value <
              TGocciaStringLiteralValue(RightValue).Value)
          else
            FRegisters[A] := RegisterBoolean(VMLessThan(LeftValue, RightValue));
        end;
      end;

      OP_GT:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue >
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) >
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              TGocciaStringLiteralValue(LeftValue).Value >
              TGocciaStringLiteralValue(RightValue).Value)
          else
            FRegisters[A] := RegisterBoolean(VMGreaterThan(LeftValue, RightValue));
        end;
      end;

      OP_LTE:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue <=
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) <=
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              TGocciaStringLiteralValue(LeftValue).Value <=
              TGocciaStringLiteralValue(RightValue).Value)
          else
            FRegisters[A] := RegisterBoolean(VMLessThanOrEqual(LeftValue, RightValue));
        end;
      end;

      OP_GTE:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue >=
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) >=
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              TGocciaStringLiteralValue(LeftValue).Value >=
              TGocciaStringLiteralValue(RightValue).Value)
          else
            FRegisters[A] := RegisterBoolean(VMGreaterThanOrEqual(LeftValue, RightValue));
        end;
      end;

      OP_TYPEOF:
        case FRegisters[B].Kind of
          grkUndefined:
            SetRegister(A, TGocciaStringLiteralValue.Create('undefined'));
          grkNull, grkHole:
            SetRegister(A, TGocciaStringLiteralValue.Create('object'));
          grkBoolean:
            SetRegister(A, TGocciaStringLiteralValue.Create('boolean'));
          grkInt, grkFloat:
            SetRegister(A, TGocciaStringLiteralValue.Create('number'));
        else
          SetRegister(A, TGocciaStringLiteralValue.Create(GetRegister(B).TypeOf));
        end;

      OP_IS_INSTANCE:
      begin
        ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
        FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
        SetRegister(A, VMInstanceOfValue(GetRegister(B), GetRegister(C),
          ObjectConstructorValue, FunctionConstructorValue));
      end;

      OP_HAS_PROPERTY:
        SetRegister(A, HasPropertyValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_HAS_PROPERTY:
        SetRegister(A, MatchHasPropertyValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_EXTRACTOR:
        SetRegister(A, MatchExtractorValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_VALUE:
      begin
        LeftValue := GetRegister(B);
        RightValue := GetRegister(C);
        CustomMatcherValue := GetCustomMatcher(RightValue);
        if Assigned(CustomMatcherValue) then
        begin
          if not CustomMatcherValue.IsCallable then
            ThrowTypeError('Symbol.customMatcher must be callable');
          CallArgs := AcquireArguments(2);
          try
            MatchHintObject := TGocciaObjectValue.Create;
            MatchHintObject.AssignProperty(PROP_MATCH_TYPE,
              TGocciaStringLiteralValue.Create('boolean'));
            CallArgs.Add(LeftValue);
            CallArgs.Add(MatchHintObject);
            MatchResultValue := InvokeFunctionValue(CustomMatcherValue,
              CallArgs, RightValue);
            SetRegister(A, MatchResultValue.ToBooleanLiteral);
          finally
            ReleaseArguments(CallArgs);
          end;
        end
        else if RightValue is TGocciaClassValue then
        begin
          ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
          FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
          if VMBuiltinConstructorMatchValue(RightValue, LeftValue,
            FGlobalScope, BuiltinConstructorMatch) then
            SetRegister(A, TGocciaBooleanLiteralValue.Create(BuiltinConstructorMatch))
          else
            SetRegister(A, VMInstanceOfValue(LeftValue, RightValue,
              ObjectConstructorValue, FunctionConstructorValue));
        end
        else if VMBuiltinConstructorMatchValue(RightValue, LeftValue,
          FGlobalScope, BuiltinConstructorMatch) then
          SetRegister(A, TGocciaBooleanLiteralValue.Create(BuiltinConstructorMatch))
        else
          SetRegister(A, TGocciaBooleanLiteralValue.Create(
            MatchValueEquals(LeftValue, RightValue)));
      end;

      OP_TO_NUMBER:
        case FRegisters[B].Kind of
          grkInt, grkFloat:
            FRegisters[A] := FRegisters[B];
          grkBoolean:
            if FRegisters[B].BoolValue then
              FRegisters[A] := RegisterInt(1)
            else
              FRegisters[A] := RegisterInt(0);
          grkNull:
            FRegisters[A] := RegisterInt(0);
          grkUndefined, grkHole:
            FRegisters[A] := RegisterObject(TGocciaNumberLiteralValue.NaNValue);
        else
          SetRegister(A, GetRegister(B).ToNumberLiteral);
        end;

      OP_TO_STRING:
        SetRegisterFast(A, VMRegisterToECMAStringFast(FRegisters[B]));

      OP_DEL_INDEX:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          if TryGetArrayIndexRegister(FRegisters[C], KeyIndex) and
             (KeyIndex >= 0) and
             (KeyIndex < TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count) then
          begin
            TGocciaArrayValue(FRegisters[B].ObjectValue).SetElement(
              KeyIndex, TGocciaHoleValue.HoleValue);
            FRegisters[A] := RegisterBoolean(True);
          end
          else
            FRegisters[A] := RegisterBoolean(True);
        end
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaObjectValue) then
        begin
          if TGocciaObjectValue(FRegisters[B].ObjectValue).DeleteProperty(
            KeyToPropertyNameRegister(FRegisters[C])) then
            FRegisters[A] := RegisterBoolean(True)
          else
            FRegisters[A] := RegisterBoolean(False);
        end
        else
          FRegisters[A] := RegisterBoolean(True);
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
        BytecodeFunction := TGocciaBytecodeFunctionValue.Create(Self, ChildClosure);
        // ES2026 §10.2.5 MakeConstructor: install own `prototype` data property
        // for `function`/`function*` declarations and expressions (including
        // async generators).  The prototype is a fresh ordinary object whose
        // `constructor` data property back-references the function.
        if ChildTemplate.HasOwnPrototype then
          InstallFunctionPrototype(BytecodeFunction, ChildTemplate.IsGenerator);
        SetRegister(A, BytecodeFunction);
      end;

      OP_CALL:
      begin
        CheckExecutionTimeout;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBoundFunctionValue) then
        begin
          BoundFunction := TGocciaBoundFunctionValue(FRegisters[A].ObjectValue);
          if BoundFunction.OriginalFunction is TGocciaBytecodeFunctionValue then
          begin
            BytecodeFunction := TGocciaBytecodeFunctionValue(BoundFunction.OriginalFunction);
            if Assigned(BytecodeFunction.FClosure) and
               Assigned(BytecodeFunction.FClosure.Template) and
               (not BytecodeFunction.FClosure.Template.IsAsync) and
               (not BytecodeFunction.FClosure.Template.IsGenerator) then
            begin
              if (C and 1) = 0 then
              begin
                SetLength(RegisterArgs, BoundFunction.BoundArgCount + B);
                for I := 0 to BoundFunction.BoundArgCount - 1 do
                  RegisterArgs[I] := ValueToRegister(BoundFunction.GetBoundArg(I));
                for I := 0 to B - 1 do
                  RegisterArgs[BoundFunction.BoundArgCount + I] := FRegisters[A + 1 + I];
                PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  ValueToRegister(BoundFunction.BoundThis), RegisterArgs,
                  Length(RegisterArgs), RegisterUndefined, RegisterUndefined,
                  RegisterUndefined, False,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                Continue;
              end
              else if (FRegisters[B].Kind = grkObject) and
                      (FRegisters[B].ObjectValue is TGocciaArrayValue) then
              begin
                SetLength(RegisterArgs,
                  BoundFunction.BoundArgCount +
                  TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
                for I := 0 to BoundFunction.BoundArgCount - 1 do
                  RegisterArgs[I] := VMValueToRegisterFast(BoundFunction.GetBoundArg(I));
                for I := 0 to TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1 do
                  RegisterArgs[BoundFunction.BoundArgCount + I] := VMValueToRegisterFast(
                    TGocciaArrayValue(FRegisters[B].ObjectValue).Elements[I]);
                PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  ValueToRegister(BoundFunction.BoundThis), RegisterArgs,
                  Length(RegisterArgs), RegisterUndefined, RegisterUndefined,
                  RegisterUndefined, False,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                Continue;
              end;
            end;
          end;
        end;

        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A].ObjectValue);
          if Assigned(BytecodeFunction.FClosure) and
             Assigned(BytecodeFunction.FClosure.Template) and
             (not BytecodeFunction.FClosure.Template.IsAsync) and
             (not BytecodeFunction.FClosure.Template.IsGenerator) then
          begin
            if (C and 1) = 0 then
            begin
              SetLength(RegisterArgs, B);
              for I := 0 to B - 1 do
                RegisterArgs[I] := FRegisters[A + 1 + I];
              PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                RegisterUndefined, RegisterArgs, B,
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
              for I := 0 to High(RegisterArgs) do
                RegisterArgs[I] := ValueToRegister(
                  TGocciaArrayValue(FRegisters[B].ObjectValue).Elements[I]);
              PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                RegisterUndefined, RegisterArgs, Length(RegisterArgs),
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end;
          end;
        end;

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
        CheckExecutionTimeout;
        if (C and 1) = 0 then
        begin
          if (FRegisters[A - 1].Kind = grkObject) and
             (FRegisters[A].Kind = grkObject) and
             (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) then
          begin
            GlobalName := TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).Name;
            if (GlobalName = 'bind') and
               (FRegisters[A - 1].ObjectValue is TGocciaFunctionBase) then
            begin
              case B of
                0:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      TGocciaUndefinedLiteralValue.UndefinedValue));
                1:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1])));
                2:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithSingleArg(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1]),
                      RegisterToValue(FRegisters[A + 2])));
              else
                BytecodeFunction := nil;
              end;
              if B <= 2 then
                Continue;
            end;

            if FRegisters[A - 1].ObjectValue is TGocciaBytecodeFunctionValue then
            begin
              BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A - 1].ObjectValue);
              if Assigned(BytecodeFunction.FClosure) and
                 Assigned(BytecodeFunction.FClosure.Template) and
                 (not BytecodeFunction.FClosure.Template.IsAsync) and
                 (not BytecodeFunction.FClosure.Template.IsGenerator) then
              begin
                if GlobalName = 'call' then
                begin
                  PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                  case B of
                    0:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        RegisterUndefined, TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    1:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    2:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 1,
                        FRegisters[A + 2], RegisterUndefined, RegisterUndefined,
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    3:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 2,
                        FRegisters[A + 2], FRegisters[A + 3], RegisterUndefined,
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    4:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 3,
                        FRegisters[A + 2], FRegisters[A + 3], FRegisters[A + 4],
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  else
                    begin
                      SetLength(RegisterArgs, B - 1);
                      for I := 1 to B - 1 do
                        RegisterArgs[I - 1] := FRegisters[A + 1 + I];
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], RegisterArgs, Length(RegisterArgs),
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        False, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    end;
                  end;
                  Continue;
                end
                else if (GlobalName = 'apply') and (B >= 2) and
                        (FRegisters[A + 2].Kind = grkObject) and
                        (FRegisters[A + 2].ObjectValue is TGocciaArrayValue) then
                begin
                  ArgsArray := TGocciaArrayValue(FRegisters[A + 2].ObjectValue);
                  PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                  case ArgsArray.Elements.Count of
                    0:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    1:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 1,
                        VMValueToRegisterFast(ArgsArray.Elements[0]),
                        RegisterUndefined, RegisterUndefined,
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    2:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 2,
                        VMValueToRegisterFast(ArgsArray.Elements[0]),
                        VMValueToRegisterFast(ArgsArray.Elements[1]),
                        RegisterUndefined,
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    3:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], TGocciaRegisterArray(nil), 3,
                        VMValueToRegisterFast(ArgsArray.Elements[0]),
                        VMValueToRegisterFast(ArgsArray.Elements[1]),
                        VMValueToRegisterFast(ArgsArray.Elements[2]),
                        True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  else
                    begin
                      SetLength(RegisterArgs, ArgsArray.Elements.Count);
                      for I := 0 to High(RegisterArgs) do
                        RegisterArgs[I] := VMValueToRegisterFast(ArgsArray.Elements[I]);
                      SetupNewFrame(BytecodeFunction.FClosure,
                        FRegisters[A + 1], RegisterArgs, Length(RegisterArgs),
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        False, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    end;
                  end;
                  Continue;
                end;
              end;
            end;
          end;
        end;

        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A].ObjectValue);
          if Assigned(BytecodeFunction.FClosure) and
             Assigned(BytecodeFunction.FClosure.Template) and
             (not BytecodeFunction.FClosure.Template.IsAsync) and
             (not BytecodeFunction.FClosure.Template.IsGenerator) then
          begin
            if (C and 1) = 0 then
            begin
              SetLength(RegisterArgs, B);
              for I := 0 to B - 1 do
                RegisterArgs[I] := FRegisters[A + 1 + I];
              PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                FRegisters[A - 1], RegisterArgs, B,
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
              for I := 0 to High(RegisterArgs) do
                RegisterArgs[I] := VMValueToRegisterFast(
                  TGocciaArrayValue(FRegisters[B].ObjectValue).Elements[I]);
              PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                FRegisters[A - 1], RegisterArgs, Length(RegisterArgs),
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end;
          end;
        end;

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
        // C high bit clear: normal — C = arg count, args in B+1..B+C
        // C high bit set:   spread — (C and $7F) = register holding args array
        if (C and $80) = 0 then
        begin
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
          begin
            SetLength(RegisterArgs, C);
            for I := 0 to C - 1 do
              RegisterArgs[I] := FRegisters[B + 1 + I];
            FRegisters[A] := TGocciaVMClassValue(FRegisters[B].ObjectValue)
              .InstantiateRegisters(RegisterArgs);
          end
          else
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
        end
        else
        begin
          // Spread path: args array register = C and $7F
          SpreadArray := TGocciaArrayValue(FRegisters[C and $7F].ObjectValue);
          if (FRegisters[B].Kind = grkObject) and
             (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
          begin
            SetLength(RegisterArgs, SpreadArray.Elements.Count);
            for I := 0 to SpreadArray.Elements.Count - 1 do
              RegisterArgs[I] := VMValueToRegisterFast(SpreadArray.GetElement(I));
            FRegisters[A] := TGocciaVMClassValue(FRegisters[B].ObjectValue)
              .InstantiateRegisters(RegisterArgs);
          end
          else
          begin
            CallArgs := AcquireArguments(SpreadArray.Elements.Count);
            try
              for I := 0 to SpreadArray.Elements.Count - 1 do
                CallArgs.Add(SpreadArray.GetElement(I));
              SetRegister(A, ConstructValue(GetRegister(B), CallArgs));
            finally
              ReleaseArguments(CallArgs);
            end;
          end;
        end;
      end;

      OP_GET_ITER:
        SetRegister(A, GetIteratorValue(GetRegister(B), C <> 0));

      OP_ITER_NEXT:
      begin
        if (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaIteratorValue) then
        begin
          IterResult := TGocciaIteratorValue(FRegisters[C].ObjectValue).DirectNext(DoneFlag);
          if DoneFlag then
            FRegisters[A] := RegisterUndefined
          else
            FRegisters[A] := VMValueToRegisterFast(IterResult);
          if DoneFlag then
            FRegisters[B] := RegisterBoolean(True)
          else
            FRegisters[B] := RegisterBoolean(False);
        end
        else if (FRegisters[C].Kind = grkObject) and
                (FRegisters[C].ObjectValue is TGocciaObjectValue) then
        begin
          IterResult := FRegisters[C].ObjectValue;
          NextMethod := IterResult.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or
             (NextMethod is TGocciaUndefinedLiteralValue) or
             not NextMethod.IsCallable then
          begin
            FRegisters[A] := RegisterUndefined;
            FRegisters[B] := RegisterBoolean(True);
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
              ThrowTypeError(Format(SErrorIteratorResultNotObject, [IterResult.ToStringLiteral.Value]),
                SSuggestIteratorResultObject);

            DoneValue := IterResult.GetProperty(PROP_DONE);
            if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
            begin
              FRegisters[A] := RegisterUndefined;
              FRegisters[B] := RegisterBoolean(True);
            end
            else
            begin
              FRegisters[A] := VMValueToRegisterFast(IterResult.GetProperty(PROP_VALUE));
              FRegisters[B] := RegisterBoolean(False);
            end;
          end;
        end
        else
        begin
          FRegisters[A] := RegisterUndefined;
          FRegisters[B] := RegisterBoolean(True);
        end;
      end;

      OP_ITER_CLOSE:
        if FRegisters[A].Kind = grkObject then
          CloseRawIteratorPreservingError(FRegisters[A].ObjectValue);

      OP_AWAIT:
        SetRegister(A, AwaitValue(GetRegister(B)));

      OP_YIELD:
      begin
        if Assigned(GActiveBytecodeGenerator) then
        begin
          if (C and 1) <> 0 then
            GActiveBytecodeGenerator.HandleYieldDelegate(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              Frame.IP - 1)
          else
            GActiveBytecodeGenerator.HandleYield(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              Frame.IP);
        end
        else if A <> B then
          FRegisters[B] := FRegisters[A];
      end;

      OP_SETUP_AUTO_ACCESSOR_CONST:
        SetupAutoAccessorValue(Template.GetConstantUnchecked(C).StringValue);

      OP_BEGIN_DECORATORS:
        BeginDecorators(RegisterToValue(FRegisters[A]), RegisterToValue(FRegisters[A + 1]));

      OP_APPLY_ELEMENT_DECORATOR_CONST:
        ApplyElementDecorator(RegisterToValue(FRegisters[A]),
          Template.GetConstantUnchecked(C).StringValue);

      OP_APPLY_CLASS_DECORATOR:
        ApplyClassDecorator(RegisterToValue(FRegisters[A]));

      OP_FINISH_DECORATORS:
        SetRegister(A, FinishDecorators(RegisterToValue(FRegisters[A])));

      OP_GET_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) and FGlobalScope.Contains(GlobalName) then
          FRegisters[A] := VMValueToRegisterFast(FGlobalScope.GetValue(GlobalName))
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_SET_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
        begin
          if FGlobalScope.Contains(GlobalName) then
            FGlobalScope.AssignBinding(GlobalName, RegisterToValue(FRegisters[A]))
          else
            ThrowReferenceError(GlobalName + ' is not defined');
        end;
      end;

      OP_HAS_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) and FGlobalScope.Contains(GlobalName) then
          FRegisters[A] := RegisterBoolean(True)
        else
          FRegisters[A] := RegisterBoolean(False);
      end;

      OP_IMPORT:
        SetRegister(A, ImportModuleValue(
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue));

      OP_EXPORT:
        ExportBindingValue(
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
          GetRegister(A));

      // ES2026 §13.3.12.1 — import.meta binds lexically to the defining module
      OP_IMPORT_META:
        if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
          SetRegister(A, GetOrCreateImportMeta(Template.DebugInfo.SourceFile))
        else
          SetRegister(A, GetOrCreateImportMeta(FCurrentModuleSourcePath));

      // ES2026 §13.3.10.1 ImportCall — import(specifier)
      OP_DYNAMIC_IMPORT:
      begin
        DynImportPromise := TGocciaPromiseValue.Create;
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AddTempRoot(DynImportPromise);
        try
          try
            if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
              DynImportPromise.Resolve(ImportModuleValue(
                VMRegisterToECMAStringFast(FRegisters[B]).Value,
                Template.DebugInfo.SourceFile))
            else
              DynImportPromise.Resolve(ImportModuleValue(
                VMRegisterToECMAStringFast(FRegisters[B]).Value,
                FCurrentModuleSourcePath));
          except
            on E: EGocciaBytecodeThrow do
              DynImportPromise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              DynImportPromise.Reject(E.Value);
            on E: TGocciaSyntaxError do
              DynImportPromise.Reject(
                CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
            on E: TGocciaTypeError do
              DynImportPromise.Reject(
                CreateErrorObject(TYPE_ERROR_NAME, E.Message));
            on E: TGocciaReferenceError do
              DynImportPromise.Reject(
                CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
            on E: Exception do
              DynImportPromise.Reject(
                CreateErrorObject(ERROR_NAME, E.Message));
          end;
          SetRegister(A, DynImportPromise);
        finally
          if Assigned(TGarbageCollector.Instance) then
            TGarbageCollector.Instance.RemoveTempRoot(DynImportPromise);
        end;
      end;

      // TC39 Explicit Resource Management: OP_USING_INIT
      // A=dest (dispose method), B=value, C=flags (0=sync, 1=async)
      // Validates value has [Symbol.dispose]/[Symbol.asyncDispose], stores method in A.
      // For null/undefined, stores null. Throws TypeError if not disposable.
      OP_USING_INIT:
      begin
        LeftValue := RegisterToValue(FRegisters[B]);
        if (LeftValue is TGocciaUndefinedLiteralValue) or
           (LeftValue is TGocciaNullLiteralValue) then
          FRegisters[A] := RegisterNull
        else
        begin
          if C = 1 then
            RightValue := GetDisposeMethod(LeftValue, dhAsyncDispose)
          else
            RightValue := GetDisposeMethod(LeftValue, dhSyncDispose);
          if not Assigned(RightValue) then
          begin
            if C = 1 then
              raise EGocciaBytecodeThrow.Create(
                CreateErrorObject(TYPE_ERROR_NAME,
                  'Value is not disposable (missing [Symbol.asyncDispose] and [Symbol.dispose])'))
            else
              raise EGocciaBytecodeThrow.Create(
                CreateErrorObject(TYPE_ERROR_NAME,
                  'Value is not disposable (missing [Symbol.dispose])'));
          end;
          SetRegister(A, RightValue);
        end;
      end;

      // TC39 Explicit Resource Management: OP_USING_DISPOSE
      // A=errorAccum, B=disposeMethod, C=resource
      // Calls disposeMethod.call(resource). On error, wraps with SuppressedError
      // if errorAccum already holds an error.
      // TC39 Explicit Resource Management: OP_USING_DISPOSE
      // A=errorAccum, B=disposeMethod (overwritten with call result), C=resource
      // Calls disposeMethod.call(resource). Stores result in B for OP_AWAIT.
      // On error, wraps with SuppressedError in A.
      OP_USING_DISPOSE:
      begin
        LeftValue := RegisterToValue(FRegisters[B]); // dispose method
        if Assigned(LeftValue) and not (LeftValue is TGocciaNullLiteralValue) and
           not (LeftValue is TGocciaUndefinedLiteralValue) and
           LeftValue.IsCallable then
        begin
          try
            // Clear B before the call so that if it throws, the follow-up
            // OP_AWAIT sees null instead of the stale dispose function.
            FRegisters[B] := RegisterNull;
            RightValue := TGocciaFunctionBase(LeftValue).CallNoArgs(
              RegisterToValue(FRegisters[C]));
            // Store result in B so a follow-up OP_AWAIT can await it
            if Assigned(RightValue) then
              SetRegister(B, RightValue);
          except
            on E: EGocciaBytecodeThrow do
            begin
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 not (RightValue is TGocciaNullLiteralValue) and
                 not (RightValue is TGocciaUndefinedLiteralValue) then
                SetRegister(A, CreateSuppressedErrorObject(E.ThrownValue, RightValue))
              else
                SetRegister(A, E.ThrownValue);
            end;
            on E: TGocciaThrowValue do
            begin
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 not (RightValue is TGocciaNullLiteralValue) and
                 not (RightValue is TGocciaUndefinedLiteralValue) then
                SetRegister(A, CreateSuppressedErrorObject(E.Value, RightValue))
              else
                SetRegister(A, E.Value);
            end;
            on E: Exception do
            begin
              // Preserve typed error names for native Goccia exceptions
              if E is TGocciaTypeError then
                LeftValue := CreateErrorObject(TYPE_ERROR_NAME, E.Message)
              else if E is TGocciaReferenceError then
                LeftValue := CreateErrorObject(REFERENCE_ERROR_NAME, E.Message)
              else if E is TGocciaSyntaxError then
                LeftValue := CreateErrorObject(SYNTAX_ERROR_NAME, E.Message)
              else
                LeftValue := CreateErrorObject(ERROR_NAME, E.Message);
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 not (RightValue is TGocciaNullLiteralValue) and
                 not (RightValue is TGocciaUndefinedLiteralValue) then
                SetRegister(A, CreateSuppressedErrorObject(LeftValue, RightValue))
              else
                SetRegister(A, LeftValue);
            end;
          end;
        end;
      end;

      OP_THROW:
        raise EGocciaBytecodeThrow.Create(GetRegister(A));

      OP_NOT:
        FRegisters[A] := RegisterBoolean(not RegisterToBoolean(FRegisters[B]));

      OP_TO_BOOL:
        FRegisters[A] := RegisterBoolean(RegisterToBoolean(FRegisters[B]));

      OP_DEFINE_ACCESSOR_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(C).StringValue;
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]))
          else
            DefineStaticGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]));
        end
        else
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]))
          else
            DefineGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]));
        end;
      end;

      OP_DEFINE_ACCESSOR_DYNAMIC:
      begin
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]))
          else
            DefineStaticGetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]));
        end
        else
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]))
          else
            DefineGetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]));
        end;
      end;

      OP_COLLECTION_OP:
      begin
        case B of
          COLLECTION_OP_SPREAD_OBJECT:
            if (FRegisters[A].Kind = grkObject) and
               (FRegisters[A].ObjectValue is TGocciaObjectValue) then
              SpreadObjectIntoValue(TGocciaObjectValue(FRegisters[A].ObjectValue),
                RegisterToValue(FRegisters[C]));

          COLLECTION_OP_OBJECT_REST:
            begin
              if (A + 1 < FRegisterCount) and
                 (FRegisters[A + 1].Kind = grkObject) and
                 (FRegisters[A + 1].ObjectValue is TGocciaArrayValue) then
                SetRegister(A, ObjectRestValue(RegisterToValue(FRegisters[C]),
                  TGocciaArrayValue(FRegisters[A + 1].ObjectValue)))
              else
                SetRegister(A, ObjectRestValue(RegisterToValue(FRegisters[C]), nil));
            end;

          COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY:
            begin
              DoneValue := IterableToArray(RegisterToValue(FRegisters[C]));
              if (FRegisters[A].Kind = grkObject) and
                 (FRegisters[A].ObjectValue is TGocciaArrayValue) and
                 (DoneValue is TGocciaArrayValue) then
                for I := 0 to TGocciaArrayValue(DoneValue).Elements.Count - 1 do
                  TGocciaArrayValue(FRegisters[A].ObjectValue).Elements.Add(
                    TGocciaArrayValue(DoneValue).GetElement(I));
            end;

          COLLECTION_OP_TRY_ITERABLE_TO_ARRAY:
            begin
              if TryIterableToArray(RegisterToValue(FRegisters[C]), SpreadArray) then
                SetRegister(A, SpreadArray)
              else
                FRegisters[A] := RegisterUndefined;
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
              if FRegisters[A].Kind in [grkNull, grkUndefined] then
                ThrowTypeError(Format(SErrorCannotDestructureNotObject, [RegisterToValue(FRegisters[A]).ToStringLiteral.Value]),
                  SSuggestDestructureRequiresObject);
            end;

          VALIDATE_OP_REQUIRE_ITERABLE:
            // Operand C is the iteration bound emitted by the compiler
            // for array destructuring (see ITERABLE_LIMIT_UNBOUNDED in
            // Goccia.Bytecode):
            //   0..254  = exact element count to consume; 0 means
            //             "consume zero elements" for `const [] = iter`
            //             then close;
            //   255     = unbounded (rest pattern present or pattern
            //             length exceeds the encoding range).
            // IterableToArray's ALimit uses -1 = unbounded, 0+ = exact
            // count, so translate the sentinel here.
            if C = ITERABLE_LIMIT_UNBOUNDED then
              SetRegister(A, IterableToArray(RegisterToValue(FRegisters[A]),
                False, -1))
            else
              SetRegister(A, IterableToArray(RegisterToValue(FRegisters[A]),
                False, C));
        else
          raise Exception.CreateFmt('Unsupported validation mode: %d', [B]);
        end;
      end;

      OP_THROW_TYPE_ERROR_CONST:
        ThrowTypeError(Template.GetConstantUnchecked(C).StringValue);

      OP_DEFINE_GLOBAL_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(C).StringValue;
        if B = 2 then
          DefineGlobalBinding(GlobalName, GetRegister(A), dtConst)
        else
          DefineGlobalBinding(GlobalName, GetRegister(A), dtLet);
      end;

      OP_FINALIZE_ENUM:
        SetRegister(A, FinalizeEnumValue(GetRegister(A),
          Template.GetConstantUnchecked(C).StringValue));

      OP_SUPER_GET_CONST:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValue(GetRegister(A + 1),
            GetRegister(A - 1), Template.GetConstantUnchecked(C).StringValue))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_SUPER_GET:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValueByKey(GetRegister(A + 1),
            GetRegister(A - 1), GetRegister(C)))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_RETURN:
      begin
        ReturnValue := FRegisters[A];
        // Outermost frame: let the finally block handle teardown
        if FFrameStackCount <= InitialFrameStackCount then
          Exit(ReturnValue);
        // Intermediate trampoline frame: tear down and pop to parent
        TeardownCurrentFrame(Template, ProfileEntryTimestamp,
          FFrameStack[FFrameStackCount - 1].HandlerCount);
        ResultReg := PopFrame(Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        SetRegisterRaw(ResultReg, ReturnValue);
        Continue;
      end;
        else
          raise Exception.CreateFmt('Unsupported Goccia VM opcode in minimal executor: %d', [Op]);
        end;
      except
        on E: EGocciaBytecodeThrow do
          HandleExceptionUnwind(E.ThrownValue,
            InitialFrameStackCount, SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaThrowValue do
          HandleExceptionUnwind(E.Value,
            InitialFrameStackCount, SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaTypeError do
          HandleExceptionUnwind(
            CreateErrorObject(TYPE_ERROR_NAME, E.Message),
            InitialFrameStackCount, SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaReferenceError do
          HandleExceptionUnwind(
            CreateErrorObject(REFERENCE_ERROR_NAME, E.Message),
            InitialFrameStackCount, SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaSyntaxError do
          HandleExceptionUnwind(
            CreateErrorObject(SYNTAX_ERROR_NAME, E.Message),
            InitialFrameStackCount, SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaRuntimeError do
          HandleExceptionUnwind(
            CreateErrorObject(ERROR_NAME, E.Message),
            InitialFrameStackCount, SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
      end;
    end;
    Result := RegisterUndefined;
  finally
    // Unwind any remaining trampoline frames (exception escape path)
    while FFrameStackCount > InitialFrameStackCount do
    begin
      TeardownCurrentFrame(Template, ProfileEntryTimestamp,
        FFrameStack[FFrameStackCount - 1].HandlerCount);
      PopFrame(Frame, Template, PrevCovLine, ProfileEntryTimestamp);
    end;
    // Teardown the outermost frame
    TeardownCurrentFrame(Template, ProfileEntryTimestamp, SavedHandlerCount);
    // Restore the caller's state
    FCurrentClosure := SavedClosure;
    FArgCount := SavedArgCount;
    FRegisterBase := SavedRegisterBase;
    FRegisterCount := SavedRegisterCount;
    FRegisters := @FRegisterStack[FRegisterBase];
    FLocalCellBase := SavedLocalCellBase;
    FLocalCellCount := SavedLocalCellCount;
    FLocalCells := @FLocalCellStack[FLocalCellBase];
  end;
end;

function TGocciaVM.ExecuteClosure(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  RegisterArgs: TGocciaRegisterArray;
  I: Integer;
begin
  SetLength(RegisterArgs, AArguments.Length);
  for I := 0 to AArguments.Length - 1 do
    RegisterArgs[I] := VMValueToRegisterFast(AArguments.GetElement(I));
  Result := RegisterToValue(ExecuteClosureRegisters(AClosure,
    VMValueToRegisterFast(AThisValue), RegisterArgs));
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
