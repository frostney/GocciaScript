unit Goccia.Builtins.GlobalArray;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalArray = class(TGocciaBuiltin)
  protected
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
  Goccia.GarbageCollector,
  Goccia.MicrotaskQueue,
  Goccia.Utils,
  Goccia.Utils.Arrays,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue;

constructor TGocciaGlobalArray.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(IsArray, 'isArray', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFrom, 'from', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFromAsync, 'fromAsync', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayOf, 'of', -1));
end;

// ES2026 §23.1.2.2 Array.isArray(arg)
function TGocciaGlobalArray.IsArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return IsArray(arg)
  if AArgs.Length < 1 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    if AArgs.GetElement(0) is TGocciaArrayValue then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.1.2.1 Array.from(items [, mapfn [, thisArg]])
function TGocciaGlobalArray.ArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultObj: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  SourceArray: TGocciaArrayValue;
  UseConstructor: Boolean;
  ConstructorArgs: TGocciaArgumentsCollection;
  Source, MapCallback, ThisArg, KValue: TGocciaValue;
  IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  LengthVal: TGocciaValue;
  CallArgs, MapArgs: TGocciaArgumentsCollection;
  SourceStr: string;
  Mapping: Boolean;
  K, Len: Integer;

  // ES2026 §7.3.5 CreateDataPropertyOrThrow(A, P, V)
  procedure CreateDataProperty(const AIndex: Integer; const AValue: TGocciaValue);
  begin
    if UseConstructor and not (ResultObj is TGocciaArrayValue) then
      ResultObj.SetProperty(IntToStr(AIndex), AValue)
    else
      TGocciaArrayValue(ResultObj).Elements.Add(AValue);
  end;

  // Steps 5a-b / 9-10: If IsConstructor(C), Construct(C, « len »); else ArrayCreate(len)
  function ConstructOrCreate(const ALen: Integer): TGocciaValue;
  begin
    if (AThisValue is TGocciaClassValue) and (AThisValue <> nil) then
    begin
      UseConstructor := True;
      ConstructorArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.SmallInt(ALen)]);
      try
        Result := TGocciaClassValue(AThisValue).Instantiate(ConstructorArgs);
      finally
        ConstructorArgs.Free;
      end;
    end
    else
    begin
      UseConstructor := False;
      Result := TGocciaArrayValue.Create;
    end;
  end;

begin
  // Step 1: Let C be the this value
  UseConstructor := False;

  if AArgs.Length < 1 then
  begin
    Result := ConstructOrCreate(0);
    Exit;
  end;

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
        ThrowTypeError('Array.from: when provided, the second argument must be a function');
      // Step 3b: Let mapping be true
      Mapping := True;
      if AArgs.Length > 2 then
        ThisArg := AArgs.GetElement(2);
    end;
  end;

  MapArgs := nil;
  if Mapping then
    MapArgs := TGocciaArgumentsCollection.Create([TGocciaUndefinedLiteralValue.UndefinedValue, TGocciaNumberLiteralValue.ZeroValue]);

  try
    // Fast path: source is already an array (avoids iterator overhead)
    if Source is TGocciaArrayValue then
    begin
      SourceArray := TGocciaArrayValue(Source);
      // Step 5a-b: Construct or create result (iterable path uses 0)
      ResultObj := ConstructOrCreate(0);
      if ResultObj is TGocciaArrayValue then
        TGocciaArrayValue(ResultObj).Elements.Capacity := SourceArray.Elements.Count;
      // Step 5e: Iterate and populate
      for K := 0 to SourceArray.Elements.Count - 1 do
      begin
        KValue := SourceArray.Elements[K];
        if not Assigned(KValue) then
          KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        // Step 5e-iv: If mapping, let mappedValue = Call(mapfn, thisArg, « next, k »)
        if Mapping then
        begin
          MapArgs.SetElement(0, KValue);
          MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
          KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
        end;
        // Step 5e-v: CreateDataPropertyOrThrow(A, ToString(k), mappedValue)
        CreateDataProperty(K, KValue);
      end;
      // Step 5e-iii: Set length
      if UseConstructor and not (ResultObj is TGocciaArrayValue) then
        ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(SourceArray.Elements.Count));
      Result := ResultObj;
    end
    // Fast path: source is a string (iterate code points)
    else if Source is TGocciaStringLiteralValue then
    begin
      SourceStr := TGocciaStringLiteralValue(Source).Value;
      ResultObj := ConstructOrCreate(0);
      if ResultObj is TGocciaArrayValue then
        TGocciaArrayValue(ResultObj).Elements.Capacity := Length(SourceStr);
      for K := 1 to Length(SourceStr) do
      begin
        KValue := TGocciaStringLiteralValue.Create(SourceStr[K]);
        if Mapping then
        begin
          MapArgs.SetElement(0, KValue);
          MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K - 1));
          KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
        end;
        CreateDataProperty(K - 1, KValue);
      end;
      if UseConstructor and not (ResultObj is TGocciaArrayValue) then
        ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(Length(SourceStr)));
      Result := ResultObj;
    end
    else
    begin
      // Step 4: Let usingIterator = GetMethod(items, @@iterator)
      IteratorMethod := nil;
      if Source is TGocciaObjectValue then
        IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);

      // Step 5: If usingIterator is not undefined
      if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
      begin
        // Step 5c: Let iteratorRecord = GetIteratorFromMethod(items, usingIterator)
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, Source);
        finally
          CallArgs.Free;
        end;

        if IteratorObj is TGocciaIteratorValue then
          Iterator := TGocciaIteratorValue(IteratorObj)
        else if IteratorObj is TGocciaObjectValue then
        begin
          NextMethod := IteratorObj.GetProperty(PROP_NEXT);
          if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
            Iterator := TGocciaGenericIteratorValue.Create(IteratorObj)
          else
            Iterator := nil;
        end
        else
          Iterator := nil;

        if not Assigned(Iterator) then
          ThrowTypeError('[Symbol.iterator] did not return a valid iterator');

        // Step 5a-b: If IsConstructor(C), Construct(C, « »); else ArrayCreate(0)
        ResultObj := ConstructOrCreate(0);
        TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
        TGocciaGarbageCollector.Instance.AddTempRoot(ResultObj);
        try
          // Step 5d-e: Iterate
          K := 0;
          IterResult := Iterator.AdvanceNext;
          while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
          begin
            KValue := IterResult.GetProperty(PROP_VALUE);
            if Mapping then
            begin
              MapArgs.SetElement(0, KValue);
              MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
              KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
            end;
            // Step 5e-v: CreateDataPropertyOrThrow(A, ToString(k), mappedValue)
            CreateDataProperty(K, KValue);
            Inc(K);
            IterResult := Iterator.AdvanceNext;
          end;
        finally
          TGocciaGarbageCollector.Instance.RemoveTempRoot(ResultObj);
          TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
        end;
        // Step 5e-iii: Set A.[[length]] to k
        if UseConstructor and not (ResultObj is TGocciaArrayValue) then
          ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(K));
        Result := ResultObj;
      end
      else
      begin
        // Steps 7-8: Array-like path — Let arrayLike = ToObject(items), len = LengthOfArrayLike
        LengthVal := Source.GetProperty(PROP_LENGTH);
        if Assigned(LengthVal) and not (LengthVal is TGocciaUndefinedLiteralValue) then
        begin
          Len := Trunc(LengthVal.ToNumberLiteral.Value);
          // Step 9-10: If IsConstructor(C), Construct(C, « len »); else ArrayCreate(len)
          ResultObj := ConstructOrCreate(0);
          if ResultObj is TGocciaArrayValue then
            TGocciaArrayValue(ResultObj).Elements.Capacity := Len;
          // Step 12: Iterate array-like
          for K := 0 to Len - 1 do
          begin
            // Step 12b: Let kValue = Get(arrayLike, Pk)
            KValue := Source.GetProperty(IntToStr(K));
            if not Assigned(KValue) then
              KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
            // Step 12c-d: If mapping, Call(mapfn, thisArg, « kValue, k »)
            if Mapping then
            begin
              MapArgs.SetElement(0, KValue);
              MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
              KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
            end;
            // Step 12e: CreateDataPropertyOrThrow(A, Pk, mappedValue)
            CreateDataProperty(K, KValue);
          end;
          // Step 13: Set A.[[length]] = len
          if UseConstructor and not (ResultObj is TGocciaArrayValue) then
            ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(Len));
          Result := ResultObj;
        end
        else
          Result := ConstructOrCreate(0);
      end;
    end;
  finally
    MapArgs.Free;
  end;
end;

// ES2026 §23.1.2.1 Array.fromAsync(asyncItems [, mapfn [, thisArg]])
function TGocciaGlobalArray.ArrayFromAsync(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  ResultObj: TGocciaValue;
  Source, MapCallback, ThisArg, KValue: TGocciaValue;
  IteratorMethod, IteratorObj, NextMethod, NextResult, DoneValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  AwaitedPromise: TGocciaPromiseValue;
  EmptyArgs, MapArgs, ConstructorArgs: TGocciaArgumentsCollection;
  Mapping, UseConstructor: Boolean;
  K, Len: Integer;
  LengthValue: TGocciaValue;
  GC: TGocciaGarbageCollector;

  function AwaitValue(const AValue: TGocciaValue): TGocciaValue;
  begin
    if AValue is TGocciaPromiseValue then
    begin
      AwaitedPromise := TGocciaPromiseValue(AValue);
      if Assigned(GC) then
        GC.AddTempRoot(AwaitedPromise);
      try
        if AwaitedPromise.State = gpsPending then
        begin
          if Assigned(TGocciaMicrotaskQueue.Instance) then
            TGocciaMicrotaskQueue.Instance.DrainQueue;
        end;
        if AwaitedPromise.State = gpsRejected then
          raise TGocciaThrowValue.Create(AwaitedPromise.PromiseResult);
        Result := AwaitedPromise.PromiseResult;
      finally
        if Assigned(GC) then
          GC.RemoveTempRoot(AwaitedPromise);
      end;
    end
    else
      Result := AValue;
  end;

  procedure AddElement(const AIndex: Integer; const AValue: TGocciaValue);
  begin
    if ResultObj is TGocciaArrayValue then
      TGocciaArrayValue(ResultObj).Elements.Add(AValue)
    else
      ResultObj.SetProperty(IntToStr(AIndex), AValue);
  end;

begin
  GC := TGocciaGarbageCollector.Instance;
  Promise := TGocciaPromiseValue.Create;

  if Assigned(GC) then
    GC.AddTempRoot(Promise);
  try
    Source := AArgs.GetElement(0);

    // ES2026 §23.1.2.1 step 3.c: GetMethod(asyncItems, @@asyncIterator) — ToObject throws on null/undefined
    if (Source is TGocciaNullLiteralValue) or (Source is TGocciaUndefinedLiteralValue) then
    begin
      Promise.Reject(CreateErrorObject(TYPE_ERROR_NAME, 'Cannot convert ' + Source.ToStringLiteral.Value + ' to object'));
      Result := Promise;
      Exit;
    end;

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
          Promise.Reject(CreateErrorObject(TYPE_ERROR_NAME, MapCallback.ToStringLiteral.Value + ' is not a function'));
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

    // ES2026 §23.1.2.1 step 5: If IsConstructor(C), Construct(C, « 0 »); else ArrayCreate(0)
    UseConstructor := AThisValue is TGocciaClassValue;
    if UseConstructor then
    begin
      ConstructorArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.ZeroValue]);
      try
        ResultObj := TGocciaClassValue(AThisValue).Instantiate(ConstructorArgs);
      finally
        ConstructorArgs.Free;
      end;
    end
    else
      ResultObj := TGocciaArrayValue.Create;

    if Assigned(GC) then
      GC.AddTempRoot(ResultObj);
    try
      try
        // ES2026 §23.1.2.1 step 3.c: GetMethod(asyncItems, @@asyncIterator)
        IteratorMethod := nil;
        if Source is TGocciaObjectValue then
          IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownAsyncIterator);

        if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
        begin
          // ES2026 §23.1.2.1 step 4.a: GetIterator(asyncItems, async)
          EmptyArgs := TGocciaArgumentsCollection.Create;
          try
            IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(EmptyArgs, Source);
          finally
            EmptyArgs.Free;
          end;

          if Assigned(GC) then
            GC.AddTempRoot(IteratorObj);
          try
            NextMethod := IteratorObj.GetProperty(PROP_NEXT);
            if not Assigned(NextMethod) or not NextMethod.IsCallable then
              ThrowTypeError('Async iterator .next is not callable');

            K := 0;
            EmptyArgs := TGocciaArgumentsCollection.Create;
            try
              // ES2026 §23.1.2.1 step 4.b: Repeat
              while True do
              begin
                // ES2026 §23.1.2.1 step 4.b.ii: Await(IteratorNext(iteratorRecord))
                NextResult := TGocciaFunctionBase(NextMethod).Call(EmptyArgs, IteratorObj);
                NextResult := AwaitValue(NextResult);

                // ES2026 §7.4.2 step 5: If nextResult is not an Object, throw a TypeError
                if NextResult.IsPrimitive then
                  ThrowTypeError('Iterator result ' + NextResult.ToStringLiteral.Value + ' is not an object');

                // ES2026 §23.1.2.1 step 4.b.iii: IteratorComplete(next)
                DoneValue := NextResult.GetProperty(PROP_DONE);
                if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
                  Break;

                // ES2026 §23.1.2.1 step 4.b.v: Await(IteratorValue(next))
                KValue := NextResult.GetProperty(PROP_VALUE);
                if not Assigned(KValue) then
                  KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
                KValue := AwaitValue(KValue);

                if Mapping then
                begin
                  // ES2026 §23.1.2.1 step 4.b.vii: Await(Call(mapfn, thisArg, « kValue, k »))
                  MapArgs.SetElement(0, KValue);
                  MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
                  KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
                  KValue := AwaitValue(KValue);
                end;

                // ES2026 §23.1.2.1 step 4.b.ix: CreateDataPropertyOrThrow(A, Pk, mappedValue)
                AddElement(K, KValue);
                Inc(K);
              end;
            finally
              EmptyArgs.Free;
            end;

            // ES2026 §23.1.2.1 step 4.b.iv: Set A.[[length]] to k
            if UseConstructor and not (ResultObj is TGocciaArrayValue) then
              ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(K));
          finally
            if Assigned(GC) then
              GC.RemoveTempRoot(IteratorObj);
          end;
        end
        else
        begin
          // ES2026 §23.1.2.1 step 3.d: GetMethod(asyncItems, @@iterator)
          Iterator := nil;
          if Source is TGocciaObjectValue then
          begin
            IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
            if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
            begin
              // ES2026 §23.1.2.1 step 4.a: GetIterator(asyncItems, sync)
              EmptyArgs := TGocciaArgumentsCollection.Create;
              try
                IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(EmptyArgs, Source);
              finally
                EmptyArgs.Free;
              end;

              if IteratorObj is TGocciaIteratorValue then
                Iterator := TGocciaIteratorValue(IteratorObj)
              else if IteratorObj is TGocciaObjectValue then
              begin
                NextMethod := IteratorObj.GetProperty(PROP_NEXT);
                if Assigned(NextMethod) and NextMethod.IsCallable then
                  Iterator := TGocciaGenericIteratorValue.Create(IteratorObj)
                else
                  Iterator := nil;
              end;
            end;
          end
          else if Source is TGocciaStringLiteralValue then
            Iterator := TGocciaStringIteratorValue.Create(Source);

          if Assigned(Iterator) then
          begin
            if Assigned(GC) then
              GC.AddTempRoot(Iterator);
            try
              K := 0;
              // ES2026 §23.1.2.1 step 4.b: Repeat (sync iterator path)
              IterResult := Iterator.AdvanceNext;
              while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
              begin
                // ES2026 §23.1.2.1 step 4.b.v: Await(IteratorValue(next))
                KValue := IterResult.GetProperty(PROP_VALUE);
                KValue := AwaitValue(KValue);

                if Mapping then
                begin
                  MapArgs.SetElement(0, KValue);
                  MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
                  KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
                  KValue := AwaitValue(KValue);
                end;

                AddElement(K, KValue);
                Inc(K);
                IterResult := Iterator.AdvanceNext;
              end;

              if UseConstructor and not (ResultObj is TGocciaArrayValue) then
                ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(K));
            finally
              if Assigned(GC) then
                GC.RemoveTempRoot(Iterator);
            end;
          end
          else
          begin
            // ES2026 §23.1.2.1 step 5: Array-like path — Let len be LengthOfArrayLike(arrayLike)
            LengthValue := Source.GetProperty(PROP_LENGTH);
            if Assigned(LengthValue) and not (LengthValue is TGocciaUndefinedLiteralValue) then
            begin
              Len := Trunc(LengthValue.ToNumberLiteral.Value);
              K := 0;
              // ES2026 §23.1.2.1 step 5.e: Repeat, while k < len
              while K < Len do
              begin
                KValue := Source.GetProperty(IntToStr(K));
                if not Assigned(KValue) then
                  KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
                // ES2026 §23.1.2.1 step 5.e.iii: Await(kValue)
                KValue := AwaitValue(KValue);

                if Mapping then
                begin
                  MapArgs.SetElement(0, KValue);
                  MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
                  KValue := InvokeCallable(MapCallback, MapArgs, ThisArg);
                  KValue := AwaitValue(KValue);
                end;

                AddElement(K, KValue);
                Inc(K);
              end;

              // ES2026 §23.1.2.1 step 5.f: Set A.[[length]] to len
              if UseConstructor and not (ResultObj is TGocciaArrayValue) then
                ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(K));
            end;
          end;
        end;

        Promise.Resolve(ResultObj);
      except
        on E: TGocciaThrowValue do
          Promise.Reject(E.Value);
      end;
    finally
      MapArgs.Free;
      if Assigned(GC) then
        GC.RemoveTempRoot(ResultObj);
    end;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(Promise);
  end;

  Result := Promise;
end;

// ES2026 §23.1.2.3 Array.of(...items)
function TGocciaGlobalArray.ArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultObj: TGocciaValue;
  Arr: TGocciaArrayValue;
  ConstructorArgs: TGocciaArgumentsCollection;
  Len, K: Integer;
begin
  // Step 1: Let len be the number of elements in items
  Len := AArgs.Length;

  // Steps 3-5: Let C be the this value; if IsConstructor(C), Construct(C, « len »); else ArrayCreate(len)
  if (AThisValue is TGocciaClassValue) and (AThisValue <> nil) then
  begin
    // Step 4a: Let A = Construct(C, « len »)
    ConstructorArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.SmallInt(Len)]);
    try
      ResultObj := TGocciaClassValue(AThisValue).Instantiate(ConstructorArgs);
    finally
      ConstructorArgs.Free;
    end;

    // Steps 6-7: CreateDataPropertyOrThrow(A, ToString(k), items[k])
    if ResultObj is TGocciaArrayValue then
    begin
      Arr := TGocciaArrayValue(ResultObj);
      for K := 0 to Len - 1 do
        ArrayCreateDataProperty(Arr, K, AArgs.GetElement(K));
    end
    else
    begin
      for K := 0 to Len - 1 do
        ResultObj.SetProperty(IntToStr(K), AArgs.GetElement(K));
      // Step 8: Set A.[[length]] = len
      ResultObj.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.SmallInt(Len));
    end;

    Result := ResultObj;
  end
  else
  begin
    // Step 5a: Let A = ArrayCreate(len)
    Arr := TGocciaArrayValue.Create;
    for K := 0 to Len - 1 do
      Arr.Elements.Add(AArgs.GetElement(K));
    Result := Arr;
  end;
end;

end.
