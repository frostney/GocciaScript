unit Goccia.Values.FunctionBase;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Values.ObjectValue, Goccia.Interfaces,
  Generics.Collections, Goccia.Values.Primitives,
  Goccia.Error, SysUtils;

type
  // Forward declarations
  TGocciaBoundFunctionValue = class;

  TGocciaFunctionSharedPrototype = class(TGocciaObjectValue)
  public
    constructor Create;

    // Function prototype methods that are available on all functions
    function FunctionCall(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function FunctionApply(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function FunctionBind(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
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
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue; virtual;

    // Override TypeName for all functions
    function TypeName: string; override;
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
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue; override;
    function ToString: string; override;
  end;

implementation

{ TGocciaFunctionBase }

constructor TGocciaFunctionBase.Create;
begin
  inherited Create;
  // No need to register methods here - GetProperty handles it dynamically

  if not Assigned(FSharedPrototype) then
    FSharedPrototype := TGocciaFunctionSharedPrototype.Create;

  Self.Prototype := FSharedPrototype;
end;

function TGocciaFunctionBase.GetProperty(const AName: string): TGocciaValue;
begin
  // TODO: This feels too manual and too hacky
  // if AName = 'call' then
  //   Result := TGocciaFunctionBase.Create('call', Self)
  // else if AName = 'apply' then
  //   Result := TGocciaFunctionBase.Create('apply', Self)
  // else if AName = 'bind' then
  //   Result := TGocciaFunctionBase.Create('bind', Self)
  // else
    Result := inherited GetProperty(AName);
end;

function TGocciaFunctionBase.TypeName: string;
begin
  Result := 'function';
end;

function TGocciaFunctionBase.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.Create;
end;

constructor TGocciaFunctionSharedPrototype.Create;
begin
  inherited Create;
end;

function TGocciaFunctionSharedPrototype.FunctionCall(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TObjectList<TGocciaValue>;
  NewThisValue: TGocciaValue;
  I: Integer;
begin
  // Function.prototype.call(thisArg, ...args)
  // ThisValue is the function being called

  if not (ThisValue is TGocciaFunctionBase) then
    raise TGocciaError.Create('Function.prototype.call called on non-function', 0, 0, '', nil);

  // First argument is the 'this' value for the call
  if Args.Count > 0 then
    NewThisValue := Args[0]
  else
    NewThisValue := TGocciaUndefinedLiteralValue.Create;

  // Remaining arguments are passed to the function
  CallArgs := TObjectList<TGocciaValue>.Create(False);
  try
    for I := 1 to Args.Count - 1 do
      CallArgs.Add(Args[I]);

    // Call the function directly since we know it's a TGocciaFunctionBase
    Result := TGocciaFunctionBase(ThisValue).Call(CallArgs, NewThisValue);
  finally
    CallArgs.Free;
  end;
end;

function TGocciaFunctionSharedPrototype.FunctionApply(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TObjectList<TGocciaValue>;
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
  if Args.Count > 0 then
    NewThisValue := Args[0]
  else
    NewThisValue := TGocciaUndefinedLiteralValue.Create;

  CallArgs := TObjectList<TGocciaValue>.Create(False);
  try
    // Second argument should be an array of arguments
    if Args.Count > 1 then
    begin
      // Check if it's array-like (has length property and numeric indices)
      if Args[1] is TGocciaObjectValue then
      begin
        ArrayObj := TGocciaObjectValue(Args[1]);
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
      else if not (Args[1] is TGocciaUndefinedLiteralValue) and not (Args[1] is TGocciaNullLiteralValue) then
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

function TGocciaFunctionSharedPrototype.FunctionBind(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
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
  if Args.Count > 0 then
    BoundThis := Args[0]
  else
    BoundThis := TGocciaUndefinedLiteralValue.Create;

  // Remaining arguments are pre-filled arguments
  BoundArgs := TObjectList<TGocciaValue>.Create(False);
  for I := 1 to Args.Count - 1 do
    BoundArgs.Add(Args[I]);

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

function TGocciaBoundFunctionValue.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  CombinedArgs: TObjectList<TGocciaValue>;
  I: Integer;
begin
  // Combine bound arguments with call arguments
  CombinedArgs := TObjectList<TGocciaValue>.Create(False);
  try
    // Add bound arguments first
    for I := 0 to FBoundArgs.Count - 1 do
      CombinedArgs.Add(FBoundArgs[I]);

    // Add call arguments
    for I := 0 to Arguments.Count - 1 do
      CombinedArgs.Add(Arguments[I]);

    // Call the original function with the bound 'this' and combined arguments
    if FOriginalFunction is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(FOriginalFunction).Call(CombinedArgs, FBoundThis)
    else
      raise TGocciaError.Create('BoundFunction.Call: Original function is not callable', 0, 0, '', nil);
  finally
    CombinedArgs.Free;
  end;
end;

function TGocciaBoundFunctionValue.ToString: string;
begin
  Result := '[BoundFunction]';
end;

end.
