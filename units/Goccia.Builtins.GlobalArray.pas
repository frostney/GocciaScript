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
    function ArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.SymbolValue;

constructor TGocciaGlobalArray.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(IsArray, 'isArray', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFrom, 'from', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayOf, 'of', -1));
end;

function TGocciaGlobalArray.IsArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Array.isArray accepts 0 or more arguments; returns false if no argument provided
  if AArgs.Length < 1 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    if AArgs.GetElement(0) is TGocciaArrayValue then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
end;

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

  procedure AddToResult(const AIndex: Integer; const AValue: TGocciaValue);
  begin
    if UseConstructor and not (ResultObj is TGocciaArrayValue) then
      ResultObj.SetProperty(IntToStr(AIndex), AValue)
    else
      TGocciaArrayValue(ResultObj).Elements.Add(AValue);
  end;

  function CreateResult(const ALen: Integer): TGocciaValue;
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
  UseConstructor := False;

  if AArgs.Length < 1 then
  begin
    Result := CreateResult(0);
    Exit;
  end;

  Source := AArgs.GetElement(0);

  Mapping := False;
  MapCallback := nil;
  ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;
  if AArgs.Length > 1 then
  begin
    MapCallback := AArgs.GetElement(1);
    if not (MapCallback is TGocciaUndefinedLiteralValue) then
    begin
      if not MapCallback.IsCallable then
        ThrowTypeError('Array.from: when provided, the second argument must be a function');
      Mapping := True;
      if AArgs.Length > 2 then
        ThisArg := AArgs.GetElement(2);
    end;
  end;

  MapArgs := nil;
  if Mapping then
    MapArgs := TGocciaArgumentsCollection.Create([TGocciaUndefinedLiteralValue.UndefinedValue, TGocciaNumberLiteralValue.ZeroValue]);

  try
    if Source is TGocciaArrayValue then
    begin
      SourceArray := TGocciaArrayValue(Source);
      ResultObj := CreateResult(0);
      if ResultObj is TGocciaArrayValue then
        TGocciaArrayValue(ResultObj).Elements.Capacity := SourceArray.Elements.Count;
      for K := 0 to SourceArray.Elements.Count - 1 do
      begin
        KValue := SourceArray.Elements[K];
        if not Assigned(KValue) then
          KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if Mapping then
        begin
          MapArgs.SetElement(0, KValue);
          MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
          KValue := TGocciaFunctionBase(MapCallback).Call(MapArgs, ThisArg);
        end;
        AddToResult(K, KValue);
      end;
      if UseConstructor and not (ResultObj is TGocciaArrayValue) then
        ResultObj.SetProperty('length', TGocciaNumberLiteralValue.SmallInt(SourceArray.Elements.Count));
      Result := ResultObj;
    end
    else if Source is TGocciaStringLiteralValue then
    begin
      SourceStr := TGocciaStringLiteralValue(Source).Value;
      ResultObj := CreateResult(0);
      if ResultObj is TGocciaArrayValue then
        TGocciaArrayValue(ResultObj).Elements.Capacity := Length(SourceStr);
      for K := 1 to Length(SourceStr) do
      begin
        KValue := TGocciaStringLiteralValue.Create(SourceStr[K]);
        if Mapping then
        begin
          MapArgs.SetElement(0, KValue);
          MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K - 1));
          KValue := TGocciaFunctionBase(MapCallback).Call(MapArgs, ThisArg);
        end;
        AddToResult(K - 1, KValue);
      end;
      if UseConstructor and not (ResultObj is TGocciaArrayValue) then
        ResultObj.SetProperty('length', TGocciaNumberLiteralValue.SmallInt(Length(SourceStr)));
      Result := ResultObj;
    end
    else
    begin
      IteratorMethod := nil;
      if Source is TGocciaObjectValue then
        IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);

      if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
      begin
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
          NextMethod := IteratorObj.GetProperty('next');
          if Assigned(NextMethod) and not (NextMethod is TGocciaUndefinedLiteralValue) and NextMethod.IsCallable then
            Iterator := TGocciaGenericIteratorValue.Create(IteratorObj)
          else
            Iterator := nil;
        end
        else
          Iterator := nil;

        if not Assigned(Iterator) then
          ThrowTypeError('[Symbol.iterator] did not return a valid iterator');

        ResultObj := CreateResult(0);
        TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
        TGocciaGarbageCollector.Instance.AddTempRoot(ResultObj);
        try
          K := 0;
          IterResult := Iterator.AdvanceNext;
          while not IterResult.GetProperty('done').ToBooleanLiteral.Value do
          begin
            KValue := IterResult.GetProperty('value');
            if Mapping then
            begin
              MapArgs.SetElement(0, KValue);
              MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
              KValue := TGocciaFunctionBase(MapCallback).Call(MapArgs, ThisArg);
            end;
            AddToResult(K, KValue);
            Inc(K);
            IterResult := Iterator.AdvanceNext;
          end;
        finally
          TGocciaGarbageCollector.Instance.RemoveTempRoot(ResultObj);
          TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
        end;
        if UseConstructor and not (ResultObj is TGocciaArrayValue) then
          ResultObj.SetProperty('length', TGocciaNumberLiteralValue.SmallInt(K));
        Result := ResultObj;
      end
      else
      begin
        LengthVal := Source.GetProperty('length');
        if Assigned(LengthVal) and not (LengthVal is TGocciaUndefinedLiteralValue) then
        begin
          Len := Trunc(LengthVal.ToNumberLiteral.Value);
          ResultObj := CreateResult(0);
          if ResultObj is TGocciaArrayValue then
            TGocciaArrayValue(ResultObj).Elements.Capacity := Len;
          for K := 0 to Len - 1 do
          begin
            KValue := Source.GetProperty(IntToStr(K));
            if not Assigned(KValue) then
              KValue := TGocciaUndefinedLiteralValue.UndefinedValue;
            if Mapping then
            begin
              MapArgs.SetElement(0, KValue);
              MapArgs.SetElement(1, TGocciaNumberLiteralValue.SmallInt(K));
              KValue := TGocciaFunctionBase(MapCallback).Call(MapArgs, ThisArg);
            end;
            AddToResult(K, KValue);
          end;
          if UseConstructor and not (ResultObj is TGocciaArrayValue) then
            ResultObj.SetProperty('length', TGocciaNumberLiteralValue.SmallInt(Len));
          Result := ResultObj;
        end
        else
          Result := CreateResult(0);
      end;
    end;
  finally
    MapArgs.Free;
  end;
end;

function TGocciaGlobalArray.ArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultObj: TGocciaValue;
  ConstructorArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  if (AThisValue is TGocciaClassValue) and (AThisValue <> nil) then
  begin
    ConstructorArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.SmallInt(AArgs.Length)]);
    try
      ResultObj := TGocciaClassValue(AThisValue).Instantiate(ConstructorArgs);
    finally
      ConstructorArgs.Free;
    end;

    if ResultObj is TGocciaArrayValue then
    begin
      TGocciaArrayValue(ResultObj).Elements.Clear;
      for I := 0 to AArgs.Length - 1 do
        TGocciaArrayValue(ResultObj).Elements.Add(AArgs.GetElement(I));
    end
    else
    begin
      for I := 0 to AArgs.Length - 1 do
        ResultObj.SetProperty(IntToStr(I), AArgs.GetElement(I));
      ResultObj.SetProperty('length', TGocciaNumberLiteralValue.SmallInt(AArgs.Length));
    end;

    Result := ResultObj;
  end
  else
  begin
    ResultObj := TGocciaArrayValue.Create;
    for I := 0 to AArgs.Length - 1 do
      TGocciaArrayValue(ResultObj).Elements.Add(AArgs.GetElement(I));
    Result := ResultObj;
  end;
end;

end.
