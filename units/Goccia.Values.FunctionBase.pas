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
    function FunctionCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FunctionBind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  // Base class for all callable functions
  TGocciaFunctionBase = class(TGocciaObjectValue)
  private class var
    FSharedPrototype: TGocciaFunctionSharedPrototype;
  protected
    // Subclasses should override these to provide name/length
    function GetFunctionLength: Integer; virtual;
    function GetFunctionName: string; virtual;
  public
    constructor Create;

    // Override GetProperty to provide call, apply, bind methods and length/name
    function GetProperty(const AName: string): TGocciaValue; override;

    // Abstract method that subclasses must implement
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; virtual;

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
    FBoundArgs: TGocciaValueList; // Pre-filled arguments
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AOriginalFunction: TGocciaValue; const ABoundThis: TGocciaValue; const ABoundArgs: TGocciaValueList);
    destructor Destroy; override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction;

{ TGocciaFunctionBase }

constructor TGocciaFunctionBase.Create;
begin
  inherited Create;

  if not Assigned(FSharedPrototype) then
  begin
    FSharedPrototype := TGocciaFunctionSharedPrototype.Create;
    // Pin the shared prototype so the GC never collects it
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.PinValue(FSharedPrototype);
  end;

  Self.Prototype := FSharedPrototype;
end;

function TGocciaFunctionBase.GetProperty(const AName: string): TGocciaValue;
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
  Result := inherited GetProperty(AName);
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

function TGocciaFunctionSharedPrototype.FunctionCall(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaValueList;
  NewThisValue: TGocciaValue;
  I: Integer;
  SlicedArgs: TGocciaArgumentsCollection;
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

  // Remaining arguments are passed to the function (skip the first one which is thisArg)
  SlicedArgs := AArgs.Slice(1);
  try
    // Call the function directly since we know it's a TGocciaFunctionBase
    Result := TGocciaFunctionBase(AThisValue).Call(SlicedArgs, NewThisValue);
  finally
    SlicedArgs.Free;
  end;
end;

function TGocciaFunctionSharedPrototype.FunctionApply(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  NewThisValue: TGocciaValue;
  I: Integer;
  ArrVal: TGocciaArrayValue;
  ArrayObj: TGocciaObjectValue;
  LengthProp: TGocciaValue;
  ArrayLength: Integer;
begin
  if not AThisValue.IsCallable then
    raise TGocciaError.Create('Function.prototype.apply called on non-function', 0, 0, '', nil);

  if AArgs.Length > 0 then
    NewThisValue := AArgs.GetElement(0)
  else
    NewThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    if AArgs.Length > 1 then
    begin
      // Fast path: direct element access for TGocciaArrayValue
      if AArgs.GetElement(1) is TGocciaArrayValue then
      begin
        ArrVal := TGocciaArrayValue(AArgs.GetElement(1));
        for I := 0 to ArrVal.Elements.Count - 1 do
          CallArgs.Add(ArrVal.Elements[I]);
      end
      // Generic path: array-like objects with length + numeric indices
      else if AArgs.GetElement(1) is TGocciaObjectValue then
      begin
        ArrayObj := TGocciaObjectValue(AArgs.GetElement(1));
        LengthProp := ArrayObj.GetProperty(PROP_LENGTH);
        if not (LengthProp is TGocciaUndefinedLiteralValue) then
        begin
          ArrayLength := Trunc((LengthProp as TGocciaNumberLiteralValue).Value);
          for I := 0 to ArrayLength - 1 do
            CallArgs.Add(ArrayObj.GetProperty(IntToStr(I)));
        end;
      end
      else if not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) and not (AArgs.GetElement(1) is TGocciaNullLiteralValue) then
      begin
        raise TGocciaError.Create('Function.prototype.apply: second argument must be an array', 0, 0, '', nil);
      end;
    end;

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
  BoundArgs := TGocciaValueList.Create(False);
  for I := 1 to AArgs.Length - 1 do
    BoundArgs.Add(AArgs.GetElement(I));

  // Create and return a bound function
  Result := TGocciaBoundFunctionValue.Create(AThisValue, BoundThis, BoundArgs);
end;

{ TGocciaBoundFunctionValue }

constructor TGocciaBoundFunctionValue.Create(const AOriginalFunction: TGocciaValue; const ABoundThis: TGocciaValue; const ABoundArgs: TGocciaValueList);
var
  I: Integer;
begin
  inherited Create;
  FOriginalFunction := AOriginalFunction;
  FBoundThis := ABoundThis;
  FBoundArgs := TGocciaValueList.Create(False);

  // Copy the bound arguments
  for I := 0 to ABoundArgs.Count - 1 do
    FBoundArgs.Add(ABoundArgs[I]);
end;

destructor TGocciaBoundFunctionValue.Destroy;
begin
  FBoundArgs.Free;
  inherited;
end;

function TGocciaBoundFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
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

function TGocciaBoundFunctionValue.GetFunctionLength: Integer;
var
  OrigLength: Integer;
begin
  // ECMAScript: bound function length = max(0, originalLength - boundArgs.length)
  if FOriginalFunction is TGocciaFunctionBase then
    OrigLength := TGocciaFunctionBase(FOriginalFunction).GetFunctionLength
  else
    OrigLength := 0;
  Result := OrigLength - FBoundArgs.Count;
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
  for I := 0 to FBoundArgs.Count - 1 do
    if Assigned(FBoundArgs[I]) then
      FBoundArgs[I].MarkReferences;
end;

end.
