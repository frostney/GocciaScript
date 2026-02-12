unit Goccia.Values.FunctionBase;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue, Goccia.Interfaces,
  Goccia.Arguments.Collection, Generics.Collections, Goccia.Values.Primitives,
  Goccia.Error, SysUtils;

type
  // Forward declarations
  TGocciaBoundFunctionValue = class;

  TGocciaFunctionSharedPrototype = class(TGocciaObjectValue)
  public
    constructor Create;

    // Function prototype methods that are available on all functions
    function FunctionCall(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function FunctionApply(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function FunctionBind(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  end;

  // Base class for all callable functions
  TGocciaFunctionBase = class(TGocciaObjectValue)
  private
    class var FSharedPrototype: TGocciaFunctionSharedPrototype;
  public
    constructor Create;

    // Override GetProperty to provide call, apply, bind methods
    function GetProperty(const AName: string): TGocciaValue;

    // Abstract method that subclasses must implement
    function Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue; virtual;

    // Override TypeName and TypeOf for all functions
    function TypeName: string; override;
    function TypeOf: string; override;
  end;

  // Helper class for bound functions
  TGocciaBoundFunctionValue = class(TGocciaFunctionBase)
  private
    FOriginalFunction: TGocciaValue; // The function being bound
    FBoundThis: TGocciaValue; // The bound 'this' value
    FBoundArgs: TObjectList<TGocciaValue>; // Pre-filled arguments
  public
    constructor Create(AOriginalFunction: TGocciaValue; ABoundThis: TGocciaValue; ABoundArgs: TObjectList<TGocciaValue>);
    destructor Destroy; override;
    function Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue; override;
  end;

implementation

uses
  Goccia.Values.NativeFunction;

{ TGocciaFunctionBase }

constructor TGocciaFunctionBase.Create;
begin
  inherited Create;

  if not Assigned(FSharedPrototype) then
    FSharedPrototype := TGocciaFunctionSharedPrototype.Create;

  Self.Prototype := FSharedPrototype;
end;

function TGocciaFunctionBase.GetProperty(const AName: string): TGocciaValue;
begin
  Result := inherited GetProperty(AName);
end;

function TGocciaFunctionBase.TypeName: string;
begin
  Result := 'function';
end;

function TGocciaFunctionBase.TypeOf: string;
begin
  Result := 'function';
end;

function TGocciaFunctionBase.Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

constructor TGocciaFunctionSharedPrototype.Create;
begin
  inherited Create;

  // Set the class var early to prevent infinite recursion when creating
  // TGocciaNativeFunctionValue instances (which inherit from TGocciaFunctionBase)
  TGocciaFunctionBase.FSharedPrototype := Self;

  // Register call, apply, bind as native methods on the function prototype
  RegisterNativeMethod(TGocciaNativeFunctionValue.Create(FunctionCall, 'call', 1));
  RegisterNativeMethod(TGocciaNativeFunctionValue.Create(FunctionApply, 'apply', 2));
  RegisterNativeMethod(TGocciaNativeFunctionValue.Create(FunctionBind, 'bind', 1));
end;

function TGocciaFunctionSharedPrototype.FunctionCall(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TObjectList<TGocciaValue>;
  NewThisValue: TGocciaValue;
  I: Integer;
  SlicedArgs: TGocciaArgumentsCollection;
begin
  // Function.prototype.call(thisArg, ...args)
  // ThisValue is the function being called

  if not (ThisValue is TGocciaFunctionBase) then
    raise TGocciaError.Create('Function.prototype.call called on non-function', 0, 0, '', nil);

  // First argument is the 'this' value for the call
  if Args.Length > 0 then
    NewThisValue := Args.GetElement(0)
  else
    NewThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Remaining arguments are passed to the function (skip the first one which is thisArg)
  SlicedArgs := Args.Slice(1);
  try
    // Call the function directly since we know it's a TGocciaFunctionBase
    Result := TGocciaFunctionBase(ThisValue).Call(SlicedArgs, NewThisValue);
  finally
    SlicedArgs.Free;
  end;
end;

function TGocciaFunctionSharedPrototype.FunctionApply(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  NewThisValue: TGocciaValue;
  I: Integer;
  ArrayObj: TGocciaObjectValue;
  LengthProp: TGocciaValue;
  ArrayLength: Integer;
begin
  // Function.prototype.apply(thisArg, argsArray)
  // ThisValue is the function being called

  if not (ThisValue is TGocciaFunctionBase) then
    raise TGocciaError.Create('Function.prototype.apply called on non-function', 0, 0, '', nil);

  // First argument is the 'this' value for the call
  if Args.Length > 0 then
    NewThisValue := Args.GetElement(0)
  else
    NewThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    // Second argument should be an array of arguments
    if Args.Length > 1 then
    begin
      // Check if it's array-like (has length property and numeric indices)
      if Args.GetElement(1) is TGocciaObjectValue then
      begin
        ArrayObj := TGocciaObjectValue(Args.GetElement(1));
        LengthProp := ArrayObj.GetProperty('length');
        if not (LengthProp is TGocciaUndefinedLiteralValue) then
        begin
          ArrayLength := Trunc((LengthProp as TGocciaNumberLiteralValue).Value);
          for I := 0 to ArrayLength - 1 do
          begin
            CallArgs.Add(ArrayObj.GetProperty(IntToStr(I)));
          end;
        end;
      end
      else if not (Args.GetElement(1) is TGocciaUndefinedLiteralValue) and not (Args.GetElement(1) is TGocciaNullLiteralValue) then
      begin
        raise TGocciaError.Create('Function.prototype.apply: second argument must be an array', 0, 0, '', nil);
      end;
      // null and undefined are treated as no arguments
    end;

    // Call the function directly since we know it's a TGocciaFunctionBase
    Result := TGocciaFunctionBase(ThisValue).Call(CallArgs, NewThisValue);
  finally
    CallArgs.Free;
  end;
end;

function TGocciaFunctionSharedPrototype.FunctionBind(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  BoundThis: TGocciaValue;
  BoundArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  // Function.prototype.bind(thisArg, ...args)
  // ThisValue is the function being bound

  if not (ThisValue is TGocciaFunctionBase) then
    raise TGocciaError.Create('Function.prototype.bind called on non-function', 0, 0, '', nil);

  // First argument is the 'this' value to bind
  if Args.Length > 0 then
    BoundThis := Args.GetElement(0)
  else
    BoundThis := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Remaining arguments are pre-filled arguments
  BoundArgs := TObjectList<TGocciaValue>.Create(False);
  for I := 1 to Args.Length - 1 do
    BoundArgs.Add(Args.GetElement(I));

  // Create and return a bound function
  Result := TGocciaBoundFunctionValue.Create(ThisValue, BoundThis, BoundArgs);
end;

{ TGocciaBoundFunctionValue }

constructor TGocciaBoundFunctionValue.Create(AOriginalFunction: TGocciaValue; ABoundThis: TGocciaValue; ABoundArgs: TObjectList<TGocciaValue>);
var
  I: Integer;
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := TObjectList<TGocciaValue>.Create(False);

  // Copy the bound arguments
  for I := 0 to ABoundArgs.Count - 1 do
    FBoundArgs.Add(ABoundArgs[I]);
end;

destructor TGocciaBoundFunctionValue.Destroy;
begin
  FBoundArgs.Free;
  inherited;
end;

function TGocciaBoundFunctionValue.Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  CombinedArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  // Combine bound arguments with call arguments
  CombinedArgs := TGocciaArgumentsCollection.Create;
  try
    // Add bound arguments first
    for I := 0 to FBoundArgs.Count - 1 do
      CombinedArgs.Add(FBoundArgs[I]);

    // Add call arguments
    for I := 0 to Arguments.Length - 1 do
      CombinedArgs.Add(Arguments.GetElement(I));

    // Call the original function with the bound 'this' and combined arguments
    if FOriginalFunction is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(FOriginalFunction).Call(CombinedArgs, FBoundThis)
    else
      raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);
  finally
    CombinedArgs.Free;
  end;
end;

end.
