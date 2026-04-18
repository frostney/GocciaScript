unit Goccia.Values.FunctionBase;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // Forward declarations
  TGocciaBoundFunctionValue = class;

  TGocciaFunctionSharedPrototype = class(TGocciaObjectValue)
  public
    constructor Create;

    // Function prototype methods that are available on all functions
  public
    function FunctionCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionBind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  // Base class for all callable functions
  TGocciaFunctionBase = class(TGocciaObjectValue)
  protected
    // Subclasses should override these to provide name/length
    function GetFunctionLength: Integer; virtual;
    function GetFunctionName: string; virtual;
  public
    constructor Create;
    class procedure SetSharedPrototypeParent(const AParent: TGocciaObjectValue);

    // Override GetProperty to provide call, apply, bind methods and length/name
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;

    // Abstract method that subclasses must implement
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallPreparedArgs(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallOneArg(const AArg0, AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallTwoArgs(const AArg0, AArg1, AThisValue: TGocciaValue): TGocciaValue; virtual;
    function CallThreeArgs(const AArg0, AArg1, AArg2, AThisValue: TGocciaValue): TGocciaValue; virtual;

    // VMT-based type discrimination
    function IsCallable: Boolean; override;

    // Override TypeName and TypeOf for all functions
    function TypeName: string; override;
    function TypeOf: string; override;

  end;

  // Helper class for bound functions
  TGocciaBoundFunctionValue = class(TGocciaFunctionBase)
  private
    FOriginalFunction: TGocciaValue; // The function being bound
    FBoundThis: TGocciaValue; // The bound 'this' value
    FBoundArgs: TGocciaValueList; // Pre-filled arguments when count > 1
    FSingleBoundArg: TGocciaValue;
    FBoundArgCount: Integer;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor CreateWithoutArgs(const AOriginalFunction: TGocciaValue;
      const ABoundThis: TGocciaValue);
    constructor CreateWithSingleArg(const AOriginalFunction: TGocciaValue;
      const ABoundThis, ABoundArg: TGocciaValue);
    constructor Create(const AOriginalFunction: TGocciaValue; const ABoundThis: TGocciaValue; const ABoundArgs: TGocciaValueList);
    destructor Destroy; override;
    function GetBoundArg(const AIndex: Integer): TGocciaValue;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallOneArg(const AArg0, AThisValue: TGocciaValue): TGocciaValue; override;
    function CallTwoArgs(const AArg0, AArg1, AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
    property OriginalFunction: TGocciaValue read FOriginalFunction;
    property BoundThis: TGocciaValue read FBoundThis;
    property BoundArgCount: Integer read FBoundArgCount;
  end;

implementation

uses
  Math,
  SysUtils,

  Goccia.Arguments.ArrayLike,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.ObjectModel,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

threadvar
  FSharedPrototype: TGocciaFunctionSharedPrototype;

{ TGocciaFunctionBase }

constructor TGocciaFunctionBase.Create;
begin
  inherited Create;

  if not Assigned(FSharedPrototype) then
  begin
    FSharedPrototype := TGocciaFunctionSharedPrototype.Create;
    // Pin the shared prototype so the GC never collects it
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FSharedPrototype);
  end;

  Self.Prototype := FSharedPrototype;
end;

function TGocciaFunctionBase.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaFunctionBase.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  // ECMAScript: Function.length and Function.name
  if AName = PROP_LENGTH then
  begin
    Result := TGocciaNumberLiteralValue.Create(GetFunctionLength);
    Exit;
  end;
  if AName = PROP_NAME then
  begin
    Result := TGocciaStringLiteralValue.Create(GetFunctionName);
    Exit;
  end;
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaFunctionBase.GetFunctionLength: Integer;
begin
  Result := 0;
end;

function TGocciaFunctionBase.GetFunctionName: string;
begin
  Result := '';
end;

function TGocciaFunctionBase.IsCallable: Boolean;
begin
  Result := True;
end;

function TGocciaFunctionBase.TypeName: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaFunctionBase.TypeOf: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaFunctionBase.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFunctionBase.CallPreparedArgs(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := Call(AArguments, AThisValue);
end;

function TGocciaFunctionBase.CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
  try
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

function TGocciaFunctionBase.CallOneArg(const AArg0,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
  try
    Args.Add(AArg0);
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

function TGocciaFunctionBase.CallTwoArgs(const AArg0, AArg1,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
  try
    Args.Add(AArg0);
    Args.Add(AArg1);
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

function TGocciaFunctionBase.CallThreeArgs(const AArg0, AArg1, AArg2,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(3);
  try
    Args.Add(AArg0);
    Args.Add(AArg1);
    Args.Add(AArg2);
    Result := Call(Args, AThisValue);
  finally
    Args.Free;
  end;
end;

class procedure TGocciaFunctionBase.SetSharedPrototypeParent(
  const AParent: TGocciaObjectValue);
begin
  if not Assigned(FSharedPrototype) then
  begin
    FSharedPrototype := TGocciaFunctionSharedPrototype.Create;
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FSharedPrototype);
  end;
  FSharedPrototype.Prototype := AParent;
end;

constructor TGocciaFunctionSharedPrototype.Create;
var
  Members: array[0..2] of TGocciaMemberDefinition;
begin
  inherited Create;

  // Set the threadvar early to prevent infinite recursion when creating
  // TGocciaNativeFunctionValue instances (which inherit from TGocciaFunctionBase)
  FSharedPrototype := Self;
  try
    Members[0] := DefineNamedMethod('call', FunctionCall, 1);
    Members[1] := DefineNamedMethod('apply', FunctionApply, 2);
    Members[2] := DefineNamedMethod('bind', FunctionBind, 1);
    RegisterMemberDefinitions(Self, Members);
  except
    if FSharedPrototype = Self then
      FSharedPrototype := nil;
    raise;
  end;
end;

function TGocciaFunctionSharedPrototype.FunctionCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NewThisValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  // Function.prototype.call(thisArg, ...args)
  // AThisValue is the function being called

  if not AThisValue.IsCallable then
    raise TGocciaError.Create('Function.prototype.call called on non-function', 0, 0, '', nil);

  // First argument is the 'this' value for the call
  if AArgs.Length > 0 then
    NewThisValue := AArgs.GetElement(0)
  else
    NewThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  case AArgs.Length of
    0, 1:
      Exit(TGocciaFunctionBase(AThisValue).CallNoArgs(NewThisValue));
    2:
      Exit(TGocciaFunctionBase(AThisValue).CallOneArg(AArgs.GetElement(1), NewThisValue));
    3:
      Exit(TGocciaFunctionBase(AThisValue).CallTwoArgs(AArgs.GetElement(1),
        AArgs.GetElement(2), NewThisValue));
    4:
      Exit(TGocciaFunctionBase(AThisValue).CallThreeArgs(AArgs.GetElement(1),
        AArgs.GetElement(2), AArgs.GetElement(3), NewThisValue));
  end;

  CallArgs := TGocciaArgumentsCollection.CreateWithCapacity(Max(0, AArgs.Length - 1));
  try
    for I := 1 to AArgs.Length - 1 do
      CallArgs.Add(AArgs.GetElement(I));

    Result := TGocciaFunctionBase(AThisValue).Call(CallArgs, NewThisValue);
  finally
    CallArgs.Free;
  end;
end;

// ES2026 §20.2.3.1 Function.prototype.apply(thisArg, argArray)
function TGocciaFunctionSharedPrototype.FunctionApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  NewThisValue: TGocciaValue;
  ArgArray: TGocciaValue;
  ArrVal: TGocciaArrayValue;
begin
  // Step 1: Perform ? RequireObjectCoercible(this)
  if not AThisValue.IsCallable then
    ThrowTypeError(SErrorFunctionApplyNonFunction, SSuggestFunctionApply);

  // thisArg
  if AArgs.Length > 0 then
    NewThisValue := AArgs.GetElement(0)
  else
    NewThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 2: If argArray is undefined or null, return F.[[Call]](thisArg, <<>>)
  if AArgs.Length <= 1 then
    Exit(TGocciaFunctionBase(AThisValue).CallNoArgs(NewThisValue));

  ArgArray := AArgs.GetElement(1);
  if (ArgArray is TGocciaUndefinedLiteralValue) or
     (ArgArray is TGocciaNullLiteralValue) then
    Exit(TGocciaFunctionBase(AThisValue).CallNoArgs(NewThisValue));

  // Fast path: small arrays use specialized call methods
  if ArgArray is TGocciaArrayValue then
  begin
    ArrVal := TGocciaArrayValue(ArgArray);
    case ArrVal.Elements.Count of
      0:
        Exit(TGocciaFunctionBase(AThisValue).CallNoArgs(NewThisValue));
      1:
        Exit(TGocciaFunctionBase(AThisValue).CallOneArg(ArrVal.Elements[0], NewThisValue));
      2:
        Exit(TGocciaFunctionBase(AThisValue).CallTwoArgs(ArrVal.Elements[0],
          ArrVal.Elements[1], NewThisValue));
      3:
        Exit(TGocciaFunctionBase(AThisValue).CallThreeArgs(ArrVal.Elements[0],
          ArrVal.Elements[1], ArrVal.Elements[2], NewThisValue));
    end;
  end;

  // Step 3: Let argList be ? CreateListFromArrayLike(argArray)
  CallArgs := CreateListFromArrayLike(ArgArray, 'Function.prototype.apply');
  try
    // Step 4: Return ? Call(func, thisArg, argList)
    Result := TGocciaFunctionBase(AThisValue).Call(CallArgs, NewThisValue);
  finally
    CallArgs.Free;
  end;
end;

function TGocciaFunctionSharedPrototype.FunctionBind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  BoundThis: TGocciaValue;
  BoundArgs: TGocciaValueList;
  I: Integer;
begin
  // Function.prototype.bind(thisArg, ...args)
  // AThisValue is the function being bound

  if not AThisValue.IsCallable then
    raise TGocciaError.Create('Function.prototype.bind called on non-function', 0, 0, '', nil);

  // First argument is the 'this' value to bind
  if AArgs.Length > 0 then
    BoundThis := AArgs.GetElement(0)
  else
    BoundThis := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Remaining arguments are pre-filled arguments
  case AArgs.Length of
    0, 1:
      Exit(TGocciaBoundFunctionValue.CreateWithoutArgs(AThisValue, BoundThis));
    2:
      Exit(TGocciaBoundFunctionValue.CreateWithSingleArg(AThisValue,
        BoundThis, AArgs.GetElement(1)));
  end;

  BoundArgs := TGocciaValueList.Create(False);
  try
    BoundArgs.Capacity := AArgs.Length - 1;
    for I := 1 to AArgs.Length - 1 do
      BoundArgs.Add(AArgs.GetElement(I));

    Result := TGocciaBoundFunctionValue.Create(AThisValue, BoundThis, BoundArgs);
    BoundArgs := nil;
  finally
    if Assigned(BoundArgs) then
      BoundArgs.Free;
  end;
end;

{ TGocciaBoundFunctionValue }

constructor TGocciaBoundFunctionValue.CreateWithoutArgs(
  const AOriginalFunction, ABoundThis: TGocciaValue);
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := nil;
  FSingleBoundArg := nil;
  FBoundArgCount := 0;
end;

constructor TGocciaBoundFunctionValue.CreateWithSingleArg(
  const AOriginalFunction, ABoundThis, ABoundArg: TGocciaValue);
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := nil;
  FSingleBoundArg := ABoundArg;
  FBoundArgCount := 1;
end;

constructor TGocciaBoundFunctionValue.Create(const AOriginalFunction: TGocciaValue; const ABoundThis: TGocciaValue; const ABoundArgs: TGocciaValueList);
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := nil;
  FSingleBoundArg := nil;
  FBoundArgCount := ABoundArgs.Count;
  case ABoundArgs.Count of
    0:
      ;
    1:
      FSingleBoundArg := ABoundArgs[0];
  else
    FBoundArgs := ABoundArgs;
    Exit;
  end;
  ABoundArgs.Free;
end;

destructor TGocciaBoundFunctionValue.Destroy;
begin
  FBoundArgs.Free;
  inherited;
end;

function TGocciaBoundFunctionValue.GetBoundArg(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex < 0) or (AIndex >= FBoundArgCount) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if FBoundArgCount = 1 then
    Exit(FSingleBoundArg);

  if Assigned(FBoundArgs) then
    Exit(FBoundArgs[AIndex]);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaBoundFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CombinedArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  // Combine bound arguments with call arguments
  CombinedArgs := TGocciaArgumentsCollection.CreateWithCapacity(
    FBoundArgCount + AArguments.Length);
  try
    // Add bound arguments first
    if FBoundArgCount = 1 then
      CombinedArgs.Add(FSingleBoundArg)
    else if Assigned(FBoundArgs) then
      for I := 0 to FBoundArgs.Count - 1 do
        CombinedArgs.Add(FBoundArgs[I]);

    // Add call arguments
    for I := 0 to AArguments.Length - 1 do
      CombinedArgs.Add(AArguments.GetElement(I));

    // Call the original function with the bound 'this' and combined arguments
    if FOriginalFunction is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(FOriginalFunction).Call(CombinedArgs, FBoundThis)
    else
      raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);
  finally
    CombinedArgs.Free;
  end;
end;

function TGocciaBoundFunctionValue.CallNoArgs(
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (FOriginalFunction is TGocciaFunctionBase) then
    raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);

  case FBoundArgCount of
    0:
      Result := TGocciaFunctionBase(FOriginalFunction).CallNoArgs(FBoundThis);
    1:
      Result := TGocciaFunctionBase(FOriginalFunction).CallOneArg(FSingleBoundArg, FBoundThis);
  else
    Result := inherited CallNoArgs(AThisValue);
  end;
end;

function TGocciaBoundFunctionValue.CallOneArg(const AArg0,
  AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (FOriginalFunction is TGocciaFunctionBase) then
    raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);

  case FBoundArgCount of
    0:
      Result := TGocciaFunctionBase(FOriginalFunction).CallOneArg(AArg0, FBoundThis);
    1:
      Result := TGocciaFunctionBase(FOriginalFunction).CallTwoArgs(FSingleBoundArg, AArg0, FBoundThis);
  else
    Result := inherited CallOneArg(AArg0, AThisValue);
  end;
end;

function TGocciaBoundFunctionValue.CallTwoArgs(const AArg0, AArg1,
  AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (FOriginalFunction is TGocciaFunctionBase) then
    raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);

  case FBoundArgCount of
    0:
      Result := TGocciaFunctionBase(FOriginalFunction).CallTwoArgs(AArg0, AArg1, FBoundThis);
    1:
      Result := TGocciaFunctionBase(FOriginalFunction).CallThreeArgs(FSingleBoundArg, AArg0, AArg1, FBoundThis);
  else
    Result := inherited CallTwoArgs(AArg0, AArg1, AThisValue);
  end;
end;

function TGocciaBoundFunctionValue.GetFunctionLength: Integer;
var
  OrigLength: Integer;
begin
  // ECMAScript: bound function length = max(0, originalLength - boundArgs.length)
  if FOriginalFunction is TGocciaFunctionBase then
    OrigLength := TGocciaFunctionBase(FOriginalFunction).GetFunctionLength
  else
    OrigLength := 0;
  Result := OrigLength - FBoundArgCount;
  if Result < 0 then
    Result := 0;
end;

function TGocciaBoundFunctionValue.GetFunctionName: string;
begin
  // ECMAScript: bound function name = "bound " + originalName
  if FOriginalFunction is TGocciaFunctionBase then
    Result := 'bound ' + TGocciaFunctionBase(FOriginalFunction).GetFunctionName
  else
    Result := 'bound ';
end;

procedure TGocciaBoundFunctionValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark the original function and bound this
  if Assigned(FOriginalFunction) then
    FOriginalFunction.MarkReferences;
  if Assigned(FBoundThis) then
    FBoundThis.MarkReferences;

  // Mark bound arguments
  if FBoundArgCount = 1 then
  begin
    if Assigned(FSingleBoundArg) then
      FSingleBoundArg.MarkReferences;
  end
  else if Assigned(FBoundArgs) then
    for I := 0 to FBoundArgs.Count - 1 do
      if Assigned(FBoundArgs[I]) then
        FBoundArgs[I].MarkReferences;
end;

end.
