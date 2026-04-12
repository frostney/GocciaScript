unit Goccia.DisposalTracker;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaDisposalHint = (dhSyncDispose, dhAsyncDispose);

  TGocciaDisposableResource = record
    ResourceValue: TGocciaValue;
    DisposeMethod: TGocciaValue;
    Hint: TGocciaDisposalHint;
  end;

  { Tracks disposable resources for a block scope. Resources are disposed in
    reverse declaration order at block exit. This class only stores the
    resource records; the actual disposal logic lives in the evaluator to
    avoid circular dependencies with AwaitValue. }
  TGocciaDisposalTracker = class
  private
    FResources: array of TGocciaDisposableResource;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { TC39 Explicit Resource Management §3.5 AddDisposableResource }
    procedure AddResource(const AValue, ADisposeMethod: TGocciaValue;
      const AHint: TGocciaDisposalHint);

    function GetResource(const AIndex: Integer): TGocciaDisposableResource;
    property Count: Integer read FCount;
  end;

{ TC39 Explicit Resource Management §3.3 GetDisposeMethod(V, hint)
  Gets the dispose method for a value based on the disposal hint.
  Returns nil if no method is found. Throws TypeError if the method
  exists but is not callable. }
function GetDisposeMethod(const AValue: TGocciaValue;
  const AHint: TGocciaDisposalHint): TGocciaValue;

{ Creates a SuppressedError object with the given error and suppressed values.
  TC39 Explicit Resource Management §6.1 SuppressedError(error, suppressed [, message]) }
function CreateSuppressedErrorObject(const AError, ASuppressed: TGocciaValue;
  const AMessage: string = ''): TGocciaValue;

implementation

uses
  SysUtils,

  Goccia.Builtins.Globals,
  Goccia.CallStack,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

{ TGocciaDisposalTracker }

constructor TGocciaDisposalTracker.Create;
begin
  inherited Create;
  FCount := 0;
  SetLength(FResources, 0);
end;

destructor TGocciaDisposalTracker.Destroy;
var
  I: Integer;
  GC: TGarbageCollector;
begin
  // Unroot all tracked values
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    for I := 0 to FCount - 1 do
    begin
      if Assigned(FResources[I].ResourceValue) then
        GC.RemoveTempRoot(FResources[I].ResourceValue);
      if Assigned(FResources[I].DisposeMethod) then
        GC.RemoveTempRoot(FResources[I].DisposeMethod);
    end;
  end;
  inherited;
end;

// TC39 Explicit Resource Management §3.5 AddDisposableResource(disposeCapability, V, hint [, method])
procedure TGocciaDisposalTracker.AddResource(const AValue, ADisposeMethod: TGocciaValue;
  const AHint: TGocciaDisposalHint);
var
  GC: TGarbageCollector;
begin
  SetLength(FResources, FCount + 1);
  FResources[FCount].ResourceValue := AValue;
  FResources[FCount].DisposeMethod := ADisposeMethod;
  FResources[FCount].Hint := AHint;
  Inc(FCount);

  // Root tracked values so the GC cannot collect them before disposal
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    if Assigned(AValue) then
      GC.AddTempRoot(AValue);
    if Assigned(ADisposeMethod) then
      GC.AddTempRoot(ADisposeMethod);
  end;
end;

function TGocciaDisposalTracker.GetResource(const AIndex: Integer): TGocciaDisposableResource;
begin
  Result := FResources[AIndex];
end;

// TC39 Explicit Resource Management §6.1 SuppressedError(error, suppressed [, message])
function CreateSuppressedErrorObject(const AError, ASuppressed: TGocciaValue;
  const AMessage: string): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
begin
  if Assigned(GSuppressedErrorProto) then
    ErrorObj := TGocciaObjectValue.Create(GSuppressedErrorProto)
  else
    ErrorObj := TGocciaObjectValue.Create(GErrorProto);
  ErrorObj.HasErrorData := True;
  ErrorObj.DefineProperty(PROP_NAME,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(SUPPRESSED_ERROR_NAME),
      [pfConfigurable, pfWritable]));
  if AMessage <> '' then
    ErrorObj.DefineProperty(PROP_MESSAGE,
      TGocciaPropertyDescriptorData.Create(
        TGocciaStringLiteralValue.Create(AMessage),
        [pfConfigurable, pfWritable]))
  else
    ErrorObj.DefineProperty(PROP_MESSAGE,
      TGocciaPropertyDescriptorData.Create(
        TGocciaStringLiteralValue.Create(''),
        [pfConfigurable, pfWritable]));
  // TC39 Explicit Resource Management §6.1 step 4: error property (non-enumerable)
  ErrorObj.DefineProperty(PROP_ERROR,
    TGocciaPropertyDescriptorData.Create(AError, [pfConfigurable, pfWritable]));
  // TC39 Explicit Resource Management §6.1 step 5: suppressed property (non-enumerable)
  ErrorObj.DefineProperty(PROP_SUPPRESSED,
    TGocciaPropertyDescriptorData.Create(ASuppressed, [pfConfigurable, pfWritable]));

  if Assigned(TGocciaCallStack.Instance) then
    ErrorObj.AssignProperty(PROP_STACK,
      TGocciaStringLiteralValue.Create(
        TGocciaCallStack.Instance.CaptureStackTrace(SUPPRESSED_ERROR_NAME, AMessage)));

  Result := ErrorObj;
end;

// TC39 Explicit Resource Management §3.3 GetDisposeMethod(V, hint)
function GetDisposeMethod(const AValue: TGocciaValue;
  const AHint: TGocciaDisposalHint): TGocciaValue;
var
  Method: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
  begin
    Result := nil;
    Exit;
  end;

  if AHint = dhAsyncDispose then
  begin
    // Step 1: Try @@asyncDispose first
    Method := TGocciaObjectValue(AValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownAsyncDispose);
    if Assigned(Method) and not (Method is TGocciaUndefinedLiteralValue) then
    begin
      if not Method.IsCallable then
        ThrowTypeError('Property [Symbol.asyncDispose] is not a function');
      Result := Method;
      Exit;
    end;

    // Step 2: Fall back to @@dispose for async-dispose hint
    Method := TGocciaObjectValue(AValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownDispose);
    if Assigned(Method) and not (Method is TGocciaUndefinedLiteralValue) then
    begin
      if not Method.IsCallable then
        ThrowTypeError('Property [Symbol.dispose] is not a function');
      Result := Method;
      Exit;
    end;

    Result := nil;
  end
  else
  begin
    // Sync dispose: just check @@dispose
    Method := TGocciaObjectValue(AValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownDispose);
    if Assigned(Method) and not (Method is TGocciaUndefinedLiteralValue) then
    begin
      if not Method.IsCallable then
        ThrowTypeError('Property [Symbol.dispose] is not a function');
      Result := Method;
      Exit;
    end;

    Result := nil;
  end;
end;

end.
