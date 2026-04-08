unit Goccia.Values.MockFunction;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaMockFunctionValue = class(TGocciaFunctionBase)
  private
    FMockName: string;
    FCalls: TGocciaValueList;
    FResults: TGocciaValueList;
    FInstances: TGocciaValueList;
    FImplementation: TGocciaValue;
    FImplementationOnce: TGocciaValueList;
    FDefaultReturnValue: TGocciaValue;
    FHasDefaultReturnValue: Boolean;
    FReturnValuesOnce: TGocciaValueList;
    FSpyTarget: TGocciaObjectValue;
    FSpyMethodName: string;
    FSpyOriginalValue: TGocciaValue;
    FMockObject: TGocciaObjectValue;
    procedure SetupMethods;
    procedure InvalidateMockObject;
    function BuildMockObject: TGocciaObjectValue;
    function BuildCallsArray: TGocciaArrayValue;
    function BuildResultsArray: TGocciaArrayValue;
    function BuildInstancesArray: TGocciaArrayValue;
    function CreateResultEntry(const AResultType: string;
      const AValue: TGocciaValue): TGocciaObjectValue;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AImplementation: TGocciaValue);
    constructor CreateSpy(const ATarget: TGocciaObjectValue;
      const AMethodName: string);
    destructor Destroy; override;

    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;

    // Methods exposed as native function properties on the mock
    function DoMockImplementation(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoMockImplementationOnce(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoMockReturnValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoMockReturnValueOnce(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoMockReset(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoMockClear(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoMockRestore(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoMockName(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoGetMockName(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DoGetMockProperty(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // Internal accessors for matchers
    property MockCalls: TGocciaValueList read FCalls;
    property MockResults: TGocciaValueList read FResults;
    property MockInstances: TGocciaValueList read FInstances;
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,

  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor;

const
  MOCK_DEFAULT_NAME = 'mock';
  RESULT_TYPE_RETURN = 'return';
  RESULT_TYPE_THROW = 'throw';
  PROP_TYPE = 'type';
  PROP_VALUE = 'value';
  PROP_CALLS = 'calls';
  PROP_RESULTS = 'results';
  PROP_INSTANCES = 'instances';
  PROP_LAST_CALL = 'lastCall';

{ TGocciaMockFunctionValue }

constructor TGocciaMockFunctionValue.Create(const AImplementation: TGocciaValue);
begin
  inherited Create;

  FMockName := MOCK_DEFAULT_NAME;
  FCalls := TGocciaValueList.Create(False);
  FResults := TGocciaValueList.Create(False);
  FInstances := TGocciaValueList.Create(False);
  FImplementationOnce := TGocciaValueList.Create(False);
  FReturnValuesOnce := TGocciaValueList.Create(False);
  FImplementation := AImplementation;
  FHasDefaultReturnValue := False;
  FDefaultReturnValue := nil;
  FSpyTarget := nil;
  FSpyOriginalValue := nil;
  FMockObject := nil;

  SetupMethods;
end;

constructor TGocciaMockFunctionValue.CreateSpy(const ATarget: TGocciaObjectValue;
  const AMethodName: string);
var
  OriginalValue: TGocciaValue;
begin
  OriginalValue := ATarget.GetProperty(AMethodName);

  Create(nil);

  FSpyTarget := ATarget;
  FSpyMethodName := AMethodName;
  FSpyOriginalValue := OriginalValue;

  // Spy passes through to original by default
  if Assigned(OriginalValue) and (OriginalValue is TGocciaFunctionBase) then
    FImplementation := OriginalValue;

  // Replace the method on the target object with this spy
  ATarget.DefineProperty(AMethodName,
    TGocciaPropertyDescriptorData.Create(Self, [pfEnumerable, pfConfigurable, pfWritable]));
end;

destructor TGocciaMockFunctionValue.Destroy;
begin
  FCalls.Free;
  FResults.Free;
  FInstances.Free;
  FImplementationOnce.Free;
  FReturnValuesOnce.Free;
  inherited;
end;

procedure TGocciaMockFunctionValue.SetupMethods;
begin
  DefineProperty('mockImplementation', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockImplementation, 'mockImplementation', 1),
    [pfConfigurable, pfWritable]));
  DefineProperty('mockImplementationOnce', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockImplementationOnce, 'mockImplementationOnce', 1),
    [pfConfigurable, pfWritable]));
  DefineProperty('mockReturnValue', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockReturnValue, 'mockReturnValue', 1),
    [pfConfigurable, pfWritable]));
  DefineProperty('mockReturnValueOnce', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockReturnValueOnce, 'mockReturnValueOnce', 1),
    [pfConfigurable, pfWritable]));
  DefineProperty('mockReset', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockReset, 'mockReset', 0),
    [pfConfigurable, pfWritable]));
  DefineProperty('mockClear', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockClear, 'mockClear', 0),
    [pfConfigurable, pfWritable]));
  DefineProperty('mockRestore', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockRestore, 'mockRestore', 0),
    [pfConfigurable, pfWritable]));
  DefineProperty('mockName', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoMockName, 'mockName', 1),
    [pfConfigurable, pfWritable]));
  DefineProperty('getMockName', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(DoGetMockName, 'getMockName', 0),
    [pfConfigurable, pfWritable]));

  // .mock is an accessor getter that builds the mock info object
  DefineProperty('mock', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.Create(DoGetMockProperty, 'mock', 0),
    nil, [pfConfigurable]));
end;

procedure TGocciaMockFunctionValue.InvalidateMockObject;
begin
  FMockObject := nil;
end;

function TGocciaMockFunctionValue.CreateResultEntry(const AResultType: string;
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty(PROP_TYPE, TGocciaStringLiteralValue.Create(AResultType));
  Result.AssignProperty(PROP_VALUE, AValue);
end;

function TGocciaMockFunctionValue.BuildCallsArray: TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FCalls.Count - 1 do
    Result.Elements.Add(FCalls[I]);
end;

function TGocciaMockFunctionValue.BuildResultsArray: TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FResults.Count - 1 do
    Result.Elements.Add(FResults[I]);
end;

function TGocciaMockFunctionValue.BuildInstancesArray: TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FInstances.Count - 1 do
    Result.Elements.Add(FInstances[I]);
end;

function TGocciaMockFunctionValue.BuildMockObject: TGocciaObjectValue;
var
  LastCall: TGocciaValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty(PROP_CALLS, BuildCallsArray);
  Result.AssignProperty(PROP_RESULTS, BuildResultsArray);
  Result.AssignProperty(PROP_INSTANCES, BuildInstancesArray);

  if FCalls.Count > 0 then
    LastCall := FCalls[FCalls.Count - 1]
  else
    LastCall := TGocciaUndefinedLiteralValue.UndefinedValue;
  Result.AssignProperty(PROP_LAST_CALL, LastCall);
end;

function TGocciaMockFunctionValue.GetFunctionLength: Integer;
begin
  Result := 0;
end;

function TGocciaMockFunctionValue.GetFunctionName: string;
begin
  Result := FMockName;
end;

function TGocciaMockFunctionValue.Call(const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ArgsArray: TGocciaArrayValue;
  I: Integer;
  ImplFn: TGocciaValue;
begin
  // Record call arguments as an array
  ArgsArray := TGocciaArrayValue.Create;
  for I := 0 to AArguments.Length - 1 do
    ArgsArray.Elements.Add(AArguments.GetElement(I));
  FCalls.Add(ArgsArray);

  // Record this value
  FInstances.Add(AThisValue);

  // Invalidate cached mock object
  InvalidateMockObject;

  // Determine what to execute:
  // 1. One-shot implementation (FIFO)
  if FImplementationOnce.Count > 0 then
  begin
    ImplFn := FImplementationOnce[0];
    FImplementationOnce.Delete(0);
    try
      Result := TGocciaFunctionBase(ImplFn).Call(AArguments, AThisValue);
      FResults.Add(CreateResultEntry(RESULT_TYPE_RETURN, Result));
    except
      on E: TGocciaThrowValue do
      begin
        FResults.Add(CreateResultEntry(RESULT_TYPE_THROW, E.Value));
        raise;
      end;
    end;
    Exit;
  end;

  // 2. One-shot return value (FIFO)
  if FReturnValuesOnce.Count > 0 then
  begin
    Result := FReturnValuesOnce[0];
    FReturnValuesOnce.Delete(0);
    FResults.Add(CreateResultEntry(RESULT_TYPE_RETURN, Result));
    Exit;
  end;

  // 3. Permanent implementation
  if Assigned(FImplementation) and (FImplementation is TGocciaFunctionBase) then
  begin
    try
      Result := TGocciaFunctionBase(FImplementation).Call(AArguments, AThisValue);
      FResults.Add(CreateResultEntry(RESULT_TYPE_RETURN, Result));
    except
      on E: TGocciaThrowValue do
      begin
        FResults.Add(CreateResultEntry(RESULT_TYPE_THROW, E.Value));
        raise;
      end;
    end;
    Exit;
  end;

  // 4. Default return value
  if FHasDefaultReturnValue then
  begin
    Result := FDefaultReturnValue;
    FResults.Add(CreateResultEntry(RESULT_TYPE_RETURN, Result));
    Exit;
  end;

  // 5. Return undefined
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  FResults.Add(CreateResultEntry(RESULT_TYPE_RETURN, Result));
end;

procedure TGocciaMockFunctionValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  for I := 0 to FCalls.Count - 1 do
    if Assigned(FCalls[I]) then
      FCalls[I].MarkReferences;

  for I := 0 to FResults.Count - 1 do
    if Assigned(FResults[I]) then
      FResults[I].MarkReferences;

  for I := 0 to FInstances.Count - 1 do
    if Assigned(FInstances[I]) then
      FInstances[I].MarkReferences;

  for I := 0 to FImplementationOnce.Count - 1 do
    if Assigned(FImplementationOnce[I]) then
      FImplementationOnce[I].MarkReferences;

  for I := 0 to FReturnValuesOnce.Count - 1 do
    if Assigned(FReturnValuesOnce[I]) then
      FReturnValuesOnce[I].MarkReferences;

  if Assigned(FImplementation) then
    FImplementation.MarkReferences;

  if Assigned(FDefaultReturnValue) then
    FDefaultReturnValue.MarkReferences;

  if Assigned(FSpyTarget) then
    FSpyTarget.MarkReferences;

  if Assigned(FSpyOriginalValue) then
    FSpyOriginalValue.MarkReferences;

  if Assigned(FMockObject) then
    FMockObject.MarkReferences;
end;

// mockImplementation(fn) — sets the implementation for all subsequent calls
function TGocciaMockFunctionValue.DoMockImplementation(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if (AArgs.Length > 0) and (AArgs.GetElement(0) is TGocciaFunctionBase) then
    FImplementation := AArgs.GetElement(0)
  else
    FImplementation := nil;

  Result := Self;
end;

// mockImplementationOnce(fn) — uses implementation for the next call only
function TGocciaMockFunctionValue.DoMockImplementationOnce(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if (AArgs.Length > 0) and (AArgs.GetElement(0) is TGocciaFunctionBase) then
    FImplementationOnce.Add(AArgs.GetElement(0));

  Result := Self;
end;

// mockReturnValue(val) — sets a permanent return value, overrides any implementation
function TGocciaMockFunctionValue.DoMockReturnValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length > 0 then
    FDefaultReturnValue := AArgs.GetElement(0)
  else
    FDefaultReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  FHasDefaultReturnValue := True;
  // Clear permanent implementation so the return value takes effect
  FImplementation := nil;
  Result := Self;
end;

// mockReturnValueOnce(val) — returns value for next call only
function TGocciaMockFunctionValue.DoMockReturnValueOnce(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length > 0 then
    FReturnValuesOnce.Add(AArgs.GetElement(0))
  else
    FReturnValuesOnce.Add(TGocciaUndefinedLiteralValue.UndefinedValue);

  Result := Self;
end;

// mockClear() — clears tracking data, preserves implementation
function TGocciaMockFunctionValue.DoMockClear(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FCalls.Clear;
  FResults.Clear;
  FInstances.Clear;
  InvalidateMockObject;
  Result := Self;
end;

// mockReset() — clears everything including implementation
function TGocciaMockFunctionValue.DoMockReset(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FCalls.Clear;
  FResults.Clear;
  FInstances.Clear;
  FImplementationOnce.Clear;
  FReturnValuesOnce.Clear;
  FImplementation := nil;
  FDefaultReturnValue := nil;
  FHasDefaultReturnValue := False;
  InvalidateMockObject;
  Result := Self;
end;

// mockRestore() — restores original method for spies
function TGocciaMockFunctionValue.DoMockRestore(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(FSpyTarget) and Assigned(FSpyOriginalValue) then
  begin
    FSpyTarget.DefineProperty(FSpyMethodName,
      TGocciaPropertyDescriptorData.Create(FSpyOriginalValue,
        [pfEnumerable, pfConfigurable, pfWritable]));
  end;

  // Also reset tracking
  DoMockReset(AArgs, AThisValue);
  Result := Self;
end;

// mockName(name) — sets the mock name
function TGocciaMockFunctionValue.DoMockName(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if (AArgs.Length > 0) and (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    FMockName := TGocciaStringLiteralValue(AArgs.GetElement(0)).Value;

  Result := Self;
end;

// getMockName() — returns the mock name
function TGocciaMockFunctionValue.DoGetMockName(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(FMockName);
end;

// Accessor getter for .mock property
function TGocciaMockFunctionValue.DoGetMockProperty(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not Assigned(FMockObject) then
    FMockObject := BuildMockObject;

  Result := FMockObject;
end;

end.
