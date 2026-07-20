unit Goccia.Builtins.GlobalArray;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalArray = class(TGocciaBuiltin)
  private
  protected
  published
    function IsArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFromAsync(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.ThreadCleanupRegistry,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Utils.Arrays,
  Goccia.Values.ArrayValue,
  Goccia.Values.Await,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.PromiseValue,
  Goccia.Values.ProxyValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.VM.Exception;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

type
  TArrayFromAsyncSyncIteratorJob = class(TGocciaObjectValue)
  private
    FPromise: TGocciaPromiseValue;
    FResultObj: TGocciaObjectValue;
    FIterator: TGocciaIteratorValue;
    FMapCallback: TGocciaValue;
    FThisArg: TGocciaValue;
    FNextResult: TGocciaObjectValue;
    FMapArgs: TGocciaArgumentsCollection;
    FK: Integer;
    FMapping: Boolean;
    FSettled: Boolean;
    procedure RejectWithException(const AException: Exception);
    procedure Schedule;
  public
    constructor Create(const APromise: TGocciaPromiseValue;
      const AResultObj: TGocciaObjectValue; const AIterator: TGocciaIteratorValue;
      const AMapping: Boolean; const AMapCallback, AThisArg: TGocciaValue;
      const AFirstResult: TGocciaObjectValue);
    destructor Destroy; override;
    function ContinueJob(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

procedure ClearThreadvarMembers;
begin
  SetLength(FStaticMembers, 0);
end;

constructor TArrayFromAsyncSyncIteratorJob.Create(
  const APromise: TGocciaPromiseValue; const AResultObj: TGocciaObjectValue;
  const AIterator: TGocciaIteratorValue; const AMapping: Boolean;
  const AMapCallback, AThisArg: TGocciaValue;
  const AFirstResult: TGocciaObjectValue);
begin
  inherited Create(nil);
  FPromise := APromise;
  FResultObj := AResultObj;
  FIterator := AIterator;
  FMapping := AMapping;
  FMapCallback := AMapCallback;
  FThisArg := AThisArg;
  FNextResult := AFirstResult;
  FK := 0;
  FSettled := False;
  if FMapping then
    FMapArgs := TGocciaArgumentsCollection.Create([
      TGocciaUndefinedLiteralValue.UndefinedValue,
      TGocciaNumberLiteralValue.ZeroValue])
  else
    FMapArgs := nil;
end;

destructor TArrayFromAsyncSyncIteratorJob.Destroy;
begin
  FMapArgs.Free;
  inherited;
end;

procedure TArrayFromAsyncSyncIteratorJob.RejectWithException(
  const AException: Exception);
begin
  if FSettled then
    Exit;
  FSettled := True;

  if AException is EGocciaBytecodeThrow then
    FPromise.Reject(EGocciaBytecodeThrow(AException).ThrownValue)
  else if AException is TGocciaThrowValue then
    FPromise.Reject(TGocciaThrowValue(AException).Value)
  else if AException is TGocciaTypeError then
    FPromise.Reject(CreateErrorObject(TYPE_ERROR_NAME, AException.Message))
  else if AException is TGocciaReferenceError then
    FPromise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, AException.Message))
  else if AException is TGocciaSyntaxError then
    FPromise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, AException.Message))
  else
    FPromise.Reject(CreateErrorObject(ERROR_NAME, AException.Message));
end;

procedure TArrayFromAsyncSyncIteratorJob.Schedule;
var
  ContinueFunction: TGocciaNativeFunctionValue;
  Task: TGocciaMicrotask;
  Queue: TGocciaMicrotaskQueue;
  Args: TGocciaArgumentsCollection;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  ContinueFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ContinueJob, 'array-fromAsync-sync-continue', 1);
  ContinueFunction.CapturedRoot := Self;

  if Assigned(Queue) then
  begin
    Task.Handler := ContinueFunction;
    Task.Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    Task.ResultPromise := nil;
    Task.ReactionType := prtFulfill;
    Queue.Enqueue(Task);
  end
  else
  begin
    Task.Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    Args := TGocciaArgumentsCollection.Create([Task.Value]);
    try
      ContinueJob(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      Args.Free;
    end;
  end;
end;

function TArrayFromAsyncSyncIteratorJob.ContinueJob(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  DoneValue, KValue: TGocciaValue;
  Done: Boolean;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if FSettled then
    Exit;

  try
    DoneValue := FNextResult.GetProperty(PROP_DONE);
    Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
    if Done then
    begin
      FResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(FK));
      FPromise.Resolve(FResultObj);
      FSettled := True;
      Exit;
    end;

    KValue := FNextResult.GetProperty(PROP_VALUE);
    if not Assigned(KValue) then
      KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    KValue := AwaitValue(KValue);

    if FMapping then
    begin
      FMapArgs.SetElement(0, KValue);
      FMapArgs.SetElement(1, TGocciaNumberLiteralValue.Create(FK));
      KValue := InvokeCallable(FMapCallback, FMapArgs, FThisArg);
      KValue := AwaitValue(KValue);
    end;

    FResultObj.CreateDataPropertyOrThrow(IntToStr(FK), KValue);
    Inc(FK);
    FNextResult := FIterator.AdvanceNext;
    Schedule;
  except
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: Exception do
    begin
      if Assigned(FIterator) then
      begin
        try
          FIterator.Close;
        except
          on CloseError: Exception do
          begin
            RejectWithException(CloseError);
            Exit;
          end;
        end;
      end;
      RejectWithException(E);
    end;
  end;
end;

procedure TArrayFromAsyncSyncIteratorJob.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
  if Assigned(FResultObj) then
    FResultObj.MarkReferences;
  if Assigned(FIterator) then
    FIterator.MarkReferences;
  if Assigned(FMapCallback) then
    FMapCallback.MarkReferences;
  if Assigned(FThisArg) then
    FThisArg.MarkReferences;
  if Assigned(FNextResult) then
    FNextResult.MarkReferences;
end;

constructor TGocciaGlobalArray.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(IsArray, 1, gmkStaticMethod);
    Members.AddMethod(ArrayFrom, 1, gmkStaticMethod);
    Members.AddMethod(ArrayFromAsync, 1, gmkStaticMethod);
    Members.AddMethod(ArrayOf, -1, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
end;

// ES2026 §23.1.2.2 Array.isArray(arg)
function TGocciaGlobalArray.IsArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // Step 1: Return IsArray(arg)
  if AArgs.Length < 1 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
  begin
    Value := AArgs.GetElement(0);
    while Value is TGocciaProxyValue do
    begin
      if TGocciaProxyValue(Value).Revoked then
        ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
      Value := TGocciaProxyValue(Value).Target;
    end;
    if Value is TGocciaArrayValue then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

function ConstructArrayFromThis(const AThisValue: TGocciaValue;
  const ALen: Integer; const APassLength: Boolean): TGocciaObjectValue;
var
  ConstructorArgs: TGocciaArgumentsCollection;
  Constructed: TGocciaValue;
begin
  if Assigned(AThisValue) and AThisValue.IsConstructable then
  begin
    if APassLength then
      ConstructorArgs := TGocciaArgumentsCollection.Create(
        [TGocciaNumberLiteralValue.Create(ALen)])
    else
      ConstructorArgs := TGocciaArgumentsCollection.Create;
    try
      Constructed := ConstructValue(AThisValue, ConstructorArgs, AThisValue);
    finally
      ConstructorArgs.Free;
    end;
    if not (Constructed is TGocciaObjectValue) then
      ThrowTypeError(Format(SErrorValueNotConstructor, [AThisValue.TypeName]),
        SSuggestNotConstructorType);
    Result := TGocciaObjectValue(Constructed);
  end
  else
    Result := TGocciaArrayValue.Create;
end;

// ES2026 §23.1.2.1 Array.from(items [, mapfn [, thisArg]])
function TGocciaGlobalArray.ArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultObj: TGocciaObjectValue;
  ArrayLike: TGocciaObjectValue;
  Source, MapCallback, ThisArg, KValue: TGocciaValue;
  IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  LengthVal: TGocciaValue;
  CallArgs, MapArgs: TGocciaArgumentsCollection;
  Mapping: Boolean;
  K, Len: Integer;
  SourceRoot, ResultRoot, MapCallbackRoot, ThisArgRoot, ValueRoot,
  MapValueRoot, MapIndexRoot, IteratorObjRoot, IteratorRoot,
  ArrayLikeRoot: TGocciaTempRoot;

  // ES2026 §7.3.5 CreateDataPropertyOrThrow(A, P, V)
  procedure CreateDataProperty(const AIndex: Integer; const AValue: TGocciaValue);
  begin
    ResultObj.CreateDataPropertyOrThrow(IntToStr(AIndex), AValue);
  end;

  procedure CreateDataPropertyRooted(const AIndex: Integer; const AValue: TGocciaValue);
  begin
    AddTempRootIfNeeded(ValueRoot, AValue);
    try
      CreateDataProperty(AIndex, AValue);
    finally
      RemoveTempRootIfNeeded(ValueRoot);
    end;
  end;

  function InvokeMapCallbackRooted(
    const AValue: TGocciaValue; const AIndex: Integer): TGocciaValue;
  var
    IndexValue: TGocciaValue;
  begin
    AddTempRootIfNeeded(MapValueRoot, AValue);
    IndexValue := TGocciaNumberLiteralValue.Create(AIndex);
    AddTempRootIfNeeded(MapIndexRoot, IndexValue);
    try
      MapArgs.SetElement(0, AValue);
      MapArgs.SetElement(1, IndexValue);
      Result := InvokeCallable(MapCallback, MapArgs, ThisArg);
    finally
      RemoveTempRootIfNeeded(MapIndexRoot);
      RemoveTempRootIfNeeded(MapValueRoot);
    end;
  end;

  // Steps 5a-b / 9-10: If IsConstructor(C), Construct(C, ...); else ArrayCreate(len)
  function ConstructOrCreate(const ALen: Integer;
    const APassLength: Boolean): TGocciaObjectValue;
  begin
    Result := ConstructArrayFromThis(AThisValue, ALen, APassLength);
  end;

begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);

  Source := AArgs.GetElement(0);

  // Steps 2-3: Handle mapfn argument
  Mapping := False;
  MapCallback := nil;
  ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length > 1 then
  begin
    MapCallback := AArgs.GetElement(1);
    if not (MapCallback is TGocciaUndefinedLiteralValue) then
    begin
      // Step 3a: If IsCallable(mapfn) is false, throw TypeError
      if not MapCallback.IsCallable then
        ThrowTypeError(SErrorArrayFromMapFn,
          SSuggestArrayFromMapFn);
      // Step 3b: Let mapping be true
      Mapping := True;
      if AArgs.Length > 2 then
        ThisArg := AArgs.GetElement(2);
    end;
  end;

  MapArgs := nil;
  if Mapping then
    MapArgs := TGocciaArgumentsCollection.Create([TGocciaUndefinedLiteralValue.UndefinedValue, TGocciaNumberLiteralValue.ZeroValue]);

  InitializeTempRoot(SourceRoot);
  InitializeTempRoot(ResultRoot);
  InitializeTempRoot(MapCallbackRoot);
  InitializeTempRoot(ThisArgRoot);
  InitializeTempRoot(ValueRoot);
  InitializeTempRoot(MapValueRoot);
  InitializeTempRoot(MapIndexRoot);
  InitializeTempRoot(IteratorObjRoot);
  InitializeTempRoot(IteratorRoot);
  InitializeTempRoot(ArrayLikeRoot);
  AddTempRootIfNeeded(SourceRoot, Source);
  if Mapping then
  begin
    AddTempRootIfNeeded(MapCallbackRoot, MapCallback);
    AddTempRootIfNeeded(ThisArgRoot, ThisArg);
  end;
  try
    // Step 4: Let usingIterator = GetMethod(items, @@iterator)
    Iterator := nil;
    IteratorMethod := nil;
    if Source is TGocciaObjectValue then
      IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator)
    else if not (Source is TGocciaNullLiteralValue) and
            not (Source is TGocciaUndefinedLiteralValue) then
      Iterator := GetIteratorFromValue(Source);

    if Assigned(IteratorMethod) and
       not (IteratorMethod is TGocciaUndefinedLiteralValue) and
       not (IteratorMethod is TGocciaNullLiteralValue) and
       not IteratorMethod.IsCallable then
      ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']),
        SSuggestIteratorProtocol);

    // Step 5: If usingIterator is not undefined
    if Assigned(Iterator) or
       (Assigned(IteratorMethod) and
        not (IteratorMethod is TGocciaUndefinedLiteralValue) and
        IteratorMethod.IsCallable) then
    begin
      // Step 5a-b: If IsConstructor(C), Construct(C, « »); else ArrayCreate(0)
      ResultObj := ConstructOrCreate(0, False);
      AddTempRootIfNeeded(ResultRoot, ResultObj);
      try
        if not Assigned(Iterator) then
        begin
          // Step 5c: Let iteratorRecord = GetIteratorFromMethod(items, usingIterator)
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            IteratorObj := InvokeCallable(IteratorMethod, CallArgs, Source);
          finally
            CallArgs.Free;
          end;

          if IteratorObj is TGocciaIteratorValue then
            Iterator := TGocciaIteratorValue(IteratorObj)
          else if IteratorObj is TGocciaObjectValue then
          begin
            AddTempRootIfNeeded(IteratorObjRoot, IteratorObj);
            try
              NextMethod := IteratorObj.GetProperty(PROP_NEXT);
              if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
                // Capture-once per ES2024 §7.4.2 GetIteratorDirect.
                Iterator := CreateRootedGenericIterator(IteratorObj, NextMethod)
              else
                Iterator := nil;
            finally
              RemoveTempRootIfNeeded(IteratorObjRoot);
            end;
          end;
        end;

        if not Assigned(Iterator) then
          ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);

        AddTempRootIfNeeded(IteratorRoot, Iterator);
        try
          // Step 5d-e: Iterate
          K := 0;
          IterResult := Iterator.AdvanceNext;
          while not IteratorResultDone(IterResult) do
          begin
            KValue := IteratorResultValue(IterResult);
            try
              if Mapping then
                KValue := InvokeMapCallbackRooted(KValue, K);
              // Step 5e-v: CreateDataPropertyOrThrow(A, ToString(k), mappedValue)
              CreateDataPropertyRooted(K, KValue);
            except
              PreserveCurrentExceptionAcrossNestedHandler;
              CloseIteratorPreservingError(Iterator);
              raise;
            end;
            Inc(K);
            IterResult := Iterator.AdvanceNext;
          end;
        finally
          RemoveTempRootIfNeeded(IteratorRoot);
        end;
        // Step 5e-iii: Set A.[[length]] to k
        ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(K));
        Result := ResultObj;
      finally
        RemoveTempRootIfNeeded(ResultRoot);
      end;
    end
    else
    begin
      // Steps 7-8: Array-like path — Let arrayLike = ToObject(items), len = LengthOfArrayLike
      ArrayLike := ToObject(Source);
      AddTempRootIfNeeded(ArrayLikeRoot, ArrayLike);
      try
        LengthVal := ArrayLike.GetProperty(PROP_LENGTH);
        if Assigned(LengthVal) and not (LengthVal is TGocciaUndefinedLiteralValue) then
        begin
          Len := ToLengthValue(LengthVal);
          // ArrayCreate rejects lengths beyond what Integer-indexed storage
          // can hold (spec caps at 2^32-1; ToLengthValue saturates at MaxInt).
          if Len = MaxInt then
            ThrowRangeError(SErrorInvalidArrayLength, SSuggestArrayLengthRange);
          // Step 9-10: If IsConstructor(C), Construct(C, « len »); else ArrayCreate(len)
          ResultObj := ConstructOrCreate(Len, True);
          AddTempRootIfNeeded(ResultRoot, ResultObj);
          try
            // Step 12: Iterate array-like
            for K := 0 to Len - 1 do
            begin
              // Step 12b: Let kValue = Get(arrayLike, Pk)
              KValue := ArrayLike.GetProperty(IntToStr(K));
              if not Assigned(KValue) then
                KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
              // Step 12c-d: If mapping, Call(mapfn, thisArg, « kValue, k »)
              if Mapping then
                KValue := InvokeMapCallbackRooted(KValue, K);
              // Step 12e: CreateDataPropertyOrThrow(A, Pk, mappedValue)
              CreateDataPropertyRooted(K, KValue);
            end;
            // Step 13: Set A.[[length]] = len
            ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(Len));
            Result := ResultObj;
          finally
            RemoveTempRootIfNeeded(ResultRoot);
          end;
        end
        else
        begin
          ResultObj := ConstructOrCreate(0, True);
          ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.ZeroValue);
          Result := ResultObj;
        end;
      finally
        RemoveTempRootIfNeeded(ArrayLikeRoot);
      end;
    end;
  finally
    RemoveTempRootIfNeeded(ThisArgRoot);
    RemoveTempRootIfNeeded(MapCallbackRoot);
    RemoveTempRootIfNeeded(SourceRoot);
    MapArgs.Free;
  end;
end;

// TC39 Array.fromAsync §2.1.1.1 Array.fromAsync(asyncItems [, mapfn [, thisArg]])
function TGocciaGlobalArray.ArrayFromAsync(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  ResultObj: TGocciaObjectValue;
  ArrayLike: TGocciaObjectValue;
  Source, MapCallback, ThisArg, KValue: TGocciaValue;
  IteratorMethod, IteratorObj, NextMethod, NextResult, DoneValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  EmptyArgs, MapArgs: TGocciaArgumentsCollection;
  Mapping: Boolean;
  PromisePendingAsync: Boolean;
  SyncIteratorJob: TArrayFromAsyncSyncIteratorJob;
  K, Len: Integer;
  LengthValue: TGocciaValue;
  GC: TGarbageCollector;
  SourceRoot: TGocciaTempRoot;

  procedure CreateResultObject(const ALen: Integer; const APassLength: Boolean);
  begin
    ResultObj := ConstructArrayFromThis(AThisValue, ALen, APassLength);
    if Assigned(GC) then
      GC.AddTempRoot(ResultObj);
  end;

  procedure AddElement(const AIndex: Integer; const AValue: TGocciaValue);
  begin
    ResultObj.CreateDataPropertyOrThrow(IntToStr(AIndex), AValue);
  end;

  procedure CloseAsyncIterator(const AIterator: TGocciaValue);
  var
    ReturnMethod: TGocciaValue;
    CloseArgs: TGocciaArgumentsCollection;
    CloseResult: TGocciaValue;
  begin
    if not (AIterator is TGocciaObjectValue) then Exit;

    ReturnMethod := TGocciaObjectValue(AIterator).GetProperty(PROP_RETURN);
    if not Assigned(ReturnMethod) or (ReturnMethod is TGocciaUndefinedLiteralValue) or not ReturnMethod.IsCallable then
      Exit;

    CloseArgs := TGocciaArgumentsCollection.Create;
    try
      CloseResult := InvokeCallable(ReturnMethod, CloseArgs, AIterator);
      CloseResult := AwaitValue(CloseResult);
      if not (CloseResult is TGocciaObjectValue) then
        ThrowTypeError(SErrorIteratorReturnObject, SSuggestIteratorProtocol);
    finally
      CloseArgs.Free;
    end;
  end;

  procedure CloseSyncIterator(const AIterator: TGocciaIteratorValue);
  begin
    if Assigned(AIterator) then
      AIterator.Close;
  end;

begin
  GC := TGarbageCollector.Instance;
  InitializeTempRoot(SourceRoot);
  Promise := TGocciaPromiseValue.Create;

  if Assigned(GC) then
    GC.AddTempRoot(Promise);
  try
    Source := AArgs.GetElement(0);

    // TC39 Array.fromAsync §2.1.1.1 step 3.c: GetMethod(asyncItems, @@asyncIterator) — ToObject throws on null/undefined
    if (Source is TGocciaNullLiteralValue) or (Source is TGocciaUndefinedLiteralValue) then
    begin
      Promise.Reject(CreateErrorObject(TYPE_ERROR_NAME, 'Cannot convert ' + Source.ToStringLiteral.Value + ' to object'));
      Result := Promise;
      Exit;
    end;
    AddTempRootIfNeeded(SourceRoot, Source);

    Mapping := False;
    MapCallback := nil;
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
    if AArgs.Length > 1 then
    begin
      MapCallback := AArgs.GetElement(1);
      if not (MapCallback is TGocciaUndefinedLiteralValue) then
      begin
        if not MapCallback.IsCallable then
        begin
          Promise.Reject(CreateErrorObject(TYPE_ERROR_NAME, 'Array.fromAsync mapfn is not a function'));
          Result := Promise;
          Exit;
        end;
        Mapping := True;
        if AArgs.Length > 2 then
          ThisArg := AArgs.GetElement(2);
      end;
    end;

    MapArgs := nil;
    if Mapping then
      MapArgs := TGocciaArgumentsCollection.Create([TGocciaUndefinedLiteralValue.UndefinedValue, TGocciaNumberLiteralValue.ZeroValue]);

    ResultObj := nil;
    PromisePendingAsync := False;
    try
      try
        // TC39 Array.fromAsync §2.1.1.1 step 3.c: GetMethod(asyncItems, @@asyncIterator)
        IteratorMethod := nil;
        if Source is TGocciaObjectValue then
          IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownAsyncIterator);

        if Assigned(IteratorMethod) and
           not (IteratorMethod is TGocciaUndefinedLiteralValue) and
           not (IteratorMethod is TGocciaNullLiteralValue) and
           not IteratorMethod.IsCallable then
          ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.asyncIterator]']),
            SSuggestIteratorProtocol);

        if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
        begin
          CreateResultObject(0, False);
          // TC39 Array.fromAsync §2.1.1.1 step 4.a: GetIterator(asyncItems, async)
          EmptyArgs := TGocciaArgumentsCollection.Create;
          try
            IteratorObj := InvokeCallable(IteratorMethod, EmptyArgs, Source);
          finally
            EmptyArgs.Free;
          end;

          if Assigned(GC) then
            GC.AddTempRoot(IteratorObj);
          try
            NextMethod := IteratorObj.GetProperty(PROP_NEXT);
            if not Assigned(NextMethod) or not NextMethod.IsCallable then
              ThrowTypeError(SErrorAsyncIteratorNextNotCallable, SSuggestIteratorProtocol);

            K := 0;
            EmptyArgs := TGocciaArgumentsCollection.Create;
            try
              try
                // TC39 Array.fromAsync §2.1.1.1 step 4.b: Repeat
                while True do
                begin
                  // TC39 Array.fromAsync §2.1.1.1 step 4.b.ii: Await(IteratorNext(iteratorRecord))
                  NextResult := InvokeCallable(NextMethod, EmptyArgs, IteratorObj);
                  NextResult := AwaitValue(NextResult);

                  // ES2026 §7.4.2 step 5: If nextResult is not an Object, throw a TypeError
                  if NextResult.IsPrimitive then
                    ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.ToStringLiteral.Value]),
                      SSuggestIteratorResultObject);

                  // TC39 Array.fromAsync §2.1.1.1 step 4.b.iii: IteratorComplete(next)
                  DoneValue := NextResult.GetProperty(PROP_DONE);
                  if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
                    Break;

                  // TC39 Array.fromAsync §2.1.1.1 step 4.b.v: Await(IteratorValue(next))
                  KValue := NextResult.GetProperty(PROP_VALUE);
                  if not Assigned(KValue) then
                    KValue := TGocciaUndefinedLiteralValue.UndefinedValue;

                  if Mapping then
                  begin
                    // TC39 Array.fromAsync §2.1.1.1 step 4.b.vii: Await(Call(mapfn, thisArg, « kValue, k »))
                    MapArgs.SetElement(0, KValue);
                    MapArgs.SetElement(1, TGocciaNumberLiteralValue.Create(K));
                    KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
                    KValue := AwaitValue(KValue);
                  end;

                  // TC39 Array.fromAsync §2.1.1.1 step 4.b.ix: CreateDataPropertyOrThrow(A, Pk, mappedValue)
                  AddElement(K, KValue);
                  Inc(K);
                end;
              except
                CloseAsyncIterator(IteratorObj);
                raise;
              end;
            finally
              EmptyArgs.Free;
            end;

            // TC39 Array.fromAsync §2.1.1.1 step 4.b.iv: Set A.[[length]] to k
            ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(K));
          finally
            if Assigned(GC) then
              GC.RemoveTempRoot(IteratorObj);
          end;
        end
        else
        begin
          // TC39 Array.fromAsync §2.1.1.1 step 3.d: GetMethod(asyncItems, @@iterator)
          Iterator := nil;
          if Source is TGocciaObjectValue then
          begin
            IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
            // Per ES2024 GetMethod: a present-but-non-callable
            // @@iterator is a TypeError, not silent fall-through to
            // the array-like path.  Falling through silently would
            // mis-handle iterator-protocol violations as array-like
            // sources and produce subtly wrong results.
            if Assigned(IteratorMethod) and
               not (IteratorMethod is TGocciaUndefinedLiteralValue) and
               not (IteratorMethod is TGocciaNullLiteralValue) and
               not IteratorMethod.IsCallable then
              ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']),
                SSuggestIteratorProtocol);
            if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
            begin
              // TC39 Array.fromAsync §2.1.1.1 step 4.a: GetIterator(asyncItems, sync)
              EmptyArgs := TGocciaArgumentsCollection.Create;
              try
                IteratorObj := InvokeCallable(IteratorMethod, EmptyArgs, Source);
              finally
                EmptyArgs.Free;
              end;

              if IteratorObj is TGocciaIteratorValue then
                Iterator := TGocciaIteratorValue(IteratorObj)
              else if IteratorObj is TGocciaObjectValue then
              begin
                NextMethod := IteratorObj.GetProperty(PROP_NEXT);
                if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
                  // Capture-once per ES2024 §7.4.2 GetIteratorDirect.
                  Iterator := CreateRootedGenericIterator(IteratorObj, NextMethod)
                else
                  // Iterator's next is missing / non-callable: protocol
                  // violation per §7.4.2 GetIteratorDirect step 2.
                  ThrowTypeError(SErrorIteratorNextMustBeCallable,
                    SSuggestIteratorProtocol);
              end
              else
                // @@iterator returned a non-Object: protocol violation
                // per §7.4.3 GetIterator step 4.
                ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
            end;
          end
          else if Source is TGocciaStringLiteralValue then
            Iterator := TGocciaStringIteratorValue.Create(Source);

          if Assigned(Iterator) then
          begin
            CreateResultObject(0, False);
            if Assigned(GC) then
              GC.AddTempRoot(Iterator);
            try
              // The first next() call is immediate, but its processing and
              // all subsequent next() calls run as promise jobs.  This
              // matches CreateAsyncFromSyncIterator + Await(nextResult):
              // callers can mutate an array after Array.fromAsync(items)
              // returns, and the iterator observes those later changes.
              IterResult := Iterator.AdvanceNext;
              SyncIteratorJob := TArrayFromAsyncSyncIteratorJob.Create(
                Promise, ResultObj, Iterator, Mapping, MapCallback, ThisArg,
                IterResult);
              PromisePendingAsync := True;
              SyncIteratorJob.Schedule;
            finally
              if Assigned(GC) then
                GC.RemoveTempRoot(Iterator);
            end;
          end
          else
          begin
            // TC39 Array.fromAsync §2.1.1.1 step 5: Array-like path — Let len be LengthOfArrayLike(arrayLike)
            ArrayLike := ToObject(Source);
            LengthValue := ArrayLike.GetProperty(PROP_LENGTH);
            Len := 0;
            if Assigned(LengthValue) and not (LengthValue is TGocciaUndefinedLiteralValue) then
            begin
              Len := ToLengthValue(LengthValue);
              if Len = MaxInt then
                ThrowRangeError(SErrorInvalidArrayLength, SSuggestArrayLengthRange);
            end;
            CreateResultObject(Len, True);
            K := 0;
            // TC39 Array.fromAsync §2.1.1.1 step 5.e: Repeat, while k < len
            while K < Len do
            begin
              KValue := ArrayLike.GetProperty(IntToStr(K));
              if not Assigned(KValue) then
                KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
              // TC39 Array.fromAsync §2.1.1.1 step 5.e.iii: Await(kValue)
              KValue := AwaitValue(KValue);

              if Mapping then
              begin
                MapArgs.SetElement(0, KValue);
                MapArgs.SetElement(1, TGocciaNumberLiteralValue.Create(K));
                KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
                KValue := AwaitValue(KValue);
              end;

              AddElement(K, KValue);
              Inc(K);
            end;

            // TC39 Array.fromAsync §2.1.1.1 step 5.f: Set A.[[length]] to len
            ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(K));
          end;
        end;

        if not PromisePendingAsync then
          Promise.Resolve(ResultObj);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
        on E: EGocciaBytecodeThrow do
          Promise.Reject(E.ThrownValue);
      end;
    finally
      MapArgs.Free;
      if Assigned(GC) and Assigned(ResultObj) then
        GC.RemoveTempRoot(ResultObj);
    end;
  finally
    RemoveTempRootIfNeeded(SourceRoot);
    if Assigned(GC) then
      GC.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

// ES2026 §23.1.2.3 Array.of(...items)
function TGocciaGlobalArray.ArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultObj: TGocciaObjectValue;
  Len, K: Integer;
begin
  // Step 1: Let len be the number of elements in items
  Len := AArgs.Length;

  // Steps 3-5: Let C be the this value; if IsConstructor(C), Construct(C, « len »); else ArrayCreate(len)
  ResultObj := ConstructArrayFromThis(AThisValue, Len, True);

  // Steps 6-7: CreateDataPropertyOrThrow(A, ToString(k), items[k])
  for K := 0 to Len - 1 do
    ResultObj.CreateDataPropertyOrThrow(IntToStr(K), AArgs.GetElement(K));

  // Step 8: Set A.[[length]] = len
  ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(Len));
  Result := ResultObj;
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.
